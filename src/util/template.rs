use nom::combinator::rest;
use nom::error::ErrorKind;
use nom::{Err, IResult};
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::marker::PhantomData;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Path<'a> {
    head: &'a str,
    tail: Vec<&'a str>,
}

impl<'a> Path<'a> {
    fn new(head: &'a str, tail: Vec<&'a str>) -> Self {
        Path { head, tail }
    }
}

impl<'a> Display for Path<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.head)?;
        for part in &self.tail {
            write!(f, ".{}", part)?;
        }
        Ok(())
    }
}

// named!(path_ident, map_res!(is_not!(".}"), std::str::from_utf8));
fn path_ident<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    take_till!(input, |c| c == '.' || c == '}')
}

fn path<'a>(input: &'a str) -> IResult<&'a str, Path<'a>> {
    map!(
        input,
        tuple!(
            path_ident,
            many0!(complete!(preceded!(char!('.'), path_ident)))
        ),
        |(h, t)| Path::new(h, t)
    )
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TemplateToken<'a> {
    Literal(&'a str),
    Substitution(Path<'a>),
}

fn token_substitution<'a>(
    input: &'a str,
) -> IResult<&'a str, TemplateToken<'a>> {
    map!(
        input,
        delimited!(tag!("{{"), path, tag!("}}")),
        TemplateToken::Substitution
    )
}

fn template_token<'a>(input: &'a str) -> IResult<&'a str, TemplateToken<'a>> {
    alt!(
        input,
        token_substitution
            | map!(
                alt!(complete!(take_until!("{{")) | complete!(rest)),
                TemplateToken::Literal
            )
    )
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Template<'a> {
    tokens: Vec<TemplateToken<'a>>,
}

impl<'a> Template<'a> {
    pub fn new(tokens: Vec<TemplateToken<'a>>) -> Self {
        Template { tokens }
    }
}

pub struct TemplateVisitor<'a> {
    marker: PhantomData<fn() -> Template<'a>>,
}

impl<'a> TemplateVisitor<'a> {
    pub fn new() -> Self {
        TemplateVisitor {
            marker: PhantomData,
        }
    }
}

impl<'a> serde::de::Visitor<'a> for TemplateVisitor<'a> {
    type Value = Template<'a>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a valid template string")
    }

    fn visit_borrowed_str<E: serde::de::Error>(
        self,
        v: &'a str,
    ) -> Result<Self::Value, E> {
        Template::parse(v).map_err(|_| {
            serde::de::Error::invalid_value(
                serde::de::Unexpected::Str(v),
                &"a valid template string",
            )
        })
    }
}

impl<'a> serde::Deserialize<'a> for Template<'a> {
    fn deserialize<D: serde::Deserializer<'a>>(
        deserializer: D,
    ) -> Result<Self, D::Error> {
        deserializer.deserialize_str(TemplateVisitor::new())
    }
}

impl<'a> Template<'a> {
    pub fn parse(
        input: &'a str,
    ) -> Result<Template<'a>, Err<(&'a str, ErrorKind)>> {
        let (remaining, res) = template(input)?;
        if remaining.len() > 0 {
            unreachable!();
        }
        Ok(res)
    }

    pub fn format(
        &self,
        params: &TemplateParams<'a>,
    ) -> Result<String, TemplateError<'a>> {
        use TemplateToken::*;
        let mut res = String::new();
        for token in &self.tokens {
            match token {
                Literal(s) => res.push_str(s),
                Substitution(p) => match params.get(p.clone()) {
                    Some(s) => res.push_str(s),
                    None => return Err(TemplateError::MissingParam(p.clone())),
                },
            }
        }
        Ok(res)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TemplateError<'a> {
    MissingParam(Path<'a>),
}

impl<'a> Display for TemplateError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TemplateError::*;
        match self {
            MissingParam(path) => {
                write!(f, "Missing template parameter: {}", path)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TemplateParams<'a> {
    Direct(&'a str),
    Nested(HashMap<&'a str, TemplateParams<'a>>),
}

impl<'a> TemplateParams<'a> {
    fn get(&self, path: Path<'a>) -> Option<&'a str> {
        use TemplateParams::*;
        match self {
            Direct(_) => None,
            Nested(m) => m.get(path.head).and_then(|next| {
                if path.tail.len() == 0 {
                    match next {
                        Direct(s) => Some(*s),
                        _ => None,
                    }
                } else {
                    next.get(Path {
                        head: path.tail[0],
                        tail: path.tail[1..].to_vec(),
                    })
                }
            }),
        }
    }
}

#[macro_export]
macro_rules! template_params {
    (@count $head: expr => $hv: tt, $($rest:tt)+) => { 1 + template_params!(@count $($rest)+) };
    (@count $one:expr => $($ov: tt)*) => { 1 };
    (@inner $ret: ident, ($key: expr => {$($v:tt)*}, $($r:tt)*)) => {
        $ret.insert($key, template_params!({ $($v)* }));
        template_params!(@inner $ret, ($($r)*));
    };
    (@inner $ret: ident, ($key: expr => $value: expr, $($r:tt)*)) => {
        $ret.insert($key, template_params!($value));
        template_params!(@inner $ret, ($($r)*));
    };
    (@inner $ret: ident, ()) => {};

    ({ $($body: tt)* }) => {{
        let _cap = template_params!(@count $($body)*);
        let mut _m = ::std::collections::HashMap::with_capacity(_cap);
        template_params!(@inner _m, ($($body)*));
        TemplateParams::Nested(_m)
    }};

    ($direct:expr) => { TemplateParams::Direct($direct) };

    () => { TemplateParams::Nested(::std::collections::HashMap::new()) };
}

fn template<'a>(input: &'a str) -> IResult<&'a str, Template<'a>> {
    complete!(
        input,
        map!(many1!(complete!(template_token)), Template::new)
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_path_ident() {
        assert_eq!(path_ident("foo}}"), Ok(("}}", "foo")));
        assert_eq!(path_ident("foo.bar}}"), Ok((".bar}}", "foo")));
    }

    #[test]
    fn test_parse_path() {
        assert_eq!(path("foo}}"), Ok(("}}", Path::new("foo", vec![]))));
        assert_eq!(
            path("foo.bar}}"),
            Ok(("}}", Path::new("foo", vec!["bar"])))
        );
        assert_eq!(
            path("foo.bar.baz}}"),
            Ok(("}}", Path::new("foo", vec!["bar", "baz"])))
        );
    }

    #[test]
    fn test_parse_template_token() {
        assert_eq!(
            template_token("foo bar"),
            Ok(("", TemplateToken::Literal("foo bar")))
        );

        assert_eq!(
            template_token("foo bar {{baz}}"),
            Ok(("{{baz}}", TemplateToken::Literal("foo bar ")))
        );

        assert_eq!(
            template_token("{{baz}}"),
            Ok((
                "",
                TemplateToken::Substitution(Path::new("baz", Vec::new()))
            ))
        );

        assert_eq!(
            template_token("{{baz}} foo bar"),
            Ok((
                " foo bar",
                TemplateToken::Substitution(Path::new("baz", Vec::new()))
            ))
        );
    }

    #[test]
    fn test_parse_template() {
        assert_eq!(
            template("foo bar"),
            Ok((
                "",
                Template {
                    tokens: vec![TemplateToken::Literal("foo bar")]
                }
            ))
        );

        assert_eq!(
            template("foo bar {{baz}} qux"),
            Ok((
                "",
                Template {
                    tokens: vec![
                        TemplateToken::Literal("foo bar "),
                        TemplateToken::Substitution(Path::new(
                            "baz",
                            Vec::new()
                        )),
                        TemplateToken::Literal(" qux"),
                    ]
                }
            ))
        );
    }

    #[test]
    fn test_template_params_literal() {
        // trace_macros!(true);
        let expected = template_params!({
            "direct" => "hi",
            "other" => "here",
            "nested" => {
                "one" => "1",
                "two" => "2",
                "double" => {
                    "three" => "3",
                },
            },
        });
        // trace_macros!(false);
        assert_eq!(
            TemplateParams::Nested(hashmap! {
                "direct" => TemplateParams::Direct("hi"),
                "other" => TemplateParams::Direct("here"),
                "nested" => TemplateParams::Nested(hashmap!{
                    "one" => TemplateParams::Direct("1"),
                    "two" => TemplateParams::Direct("2"),
                    "double" => TemplateParams::Nested(hashmap!{
                        "three" => TemplateParams::Direct("3"),
                    })
                })
            }),
            expected,
        )
    }

    #[test]
    fn test_format_template() {
        assert_eq!(
            "foo bar baz qux",
            Template::parse("foo {{x}} {{y.z}} {{y.w.z}}")
                .unwrap()
                .format(&template_params!({
                    "x" => "bar",
                    "y" => {
                        "z" => "baz",
                        "w" => {
                            "z" => "qux",
                        },
                    },
                }))
                .unwrap()
        )
    }
}
