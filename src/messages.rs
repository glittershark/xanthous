use crate::util::template::Template;
use crate::util::template::TemplateParams;
use rand::seq::SliceRandom;
use rand::Rng;
use std::collections::HashMap;

#[derive(Deserialize, Debug, PartialEq, Eq)]
#[serde(untagged)]
enum Message<'a> {
    #[serde(borrow)]
    Single(Template<'a>),
    Choice(Vec<Template<'a>>),
}

impl<'a> Message<'a> {
    fn resolve<R: Rng + ?Sized>(&self, rng: &mut R) -> Option<&Template<'a>> {
        use Message::*;
        match self {
            Single(msg) => Some(msg),
            Choice(msgs) => msgs.choose(rng),
        }
    }
}

#[derive(Deserialize, Debug, PartialEq, Eq)]
#[serde(untagged)]
enum NestedMap<'a> {
    #[serde(borrow)]
    Direct(Message<'a>),
    #[serde(borrow)]
    Nested(HashMap<&'a str, NestedMap<'a>>),
}

impl<'a> NestedMap<'a> {
    fn lookup(&'a self, path: &str) -> Option<&'a Message<'a>> {
        use NestedMap::*;
        let leaf =
            path.split(".")
                .fold(Some(self), |current, key| match current {
                    Some(Nested(m)) => m.get(key),
                    _ => None,
                });
        match leaf {
            Some(Direct(msg)) => Some(msg),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deserialize_nested_map() {
        let src = r#"
[global]
hello = "Hello World!"

[foo.bar]
single = "Single"
choice = ["Say this", "Or this"]
"#;
        let result = toml::from_str(src);
        assert_eq!(
            result,
            Ok(NestedMap::Nested(hashmap! {
                "global" => NestedMap::Nested(hashmap!{
                    "hello" => NestedMap::Direct(Message::Single(Template::parse("Hello World!").unwrap())),
                }),
                "foo" => NestedMap::Nested(hashmap!{
                    "bar" => NestedMap::Nested(hashmap!{
                        "single" => NestedMap::Direct(Message::Single(
                            Template::parse("Single").unwrap()
                        )),
                        "choice" => NestedMap::Direct(Message::Choice(
                            vec![
                                Template::parse("Say this").unwrap(),
                                Template::parse("Or this").unwrap()
                            ]
                        ))
                    })
                })
            }))
        )
    }

    #[test]
    fn test_lookup() {
        let map: NestedMap<'static> = toml::from_str(
            r#"
[global]
hello = "Hello World!"

[foo.bar]
single = "Single"
choice = ["Say this", "Or this"]
"#,
        )
        .unwrap();

        assert_eq!(
            map.lookup("global.hello"),
            Some(&Message::Single(Template::parse("Hello World!").unwrap()))
        );
        assert_eq!(
            map.lookup("foo.bar.single"),
            Some(&Message::Single(Template::parse("Single").unwrap()))
        );
        assert_eq!(
            map.lookup("foo.bar.choice"),
            Some(&Message::Choice(vec![
                Template::parse("Say this").unwrap(),
                Template::parse("Or this").unwrap()
            ]))
        );
    }
}

// static MESSAGES_RAW: &'static str = include_str!("messages.toml");

static_cfg! {
    static ref MESSAGES: NestedMap<'static> = toml_file("messages.toml");
}

/// Look up and format a game message based on the given (dot-separated) name,
/// with the given random generator used to select from choice-based messages
pub fn message<'a, R: Rng + ?Sized>(
    name: &'static str,
    rng: &mut R,
    params: &TemplateParams<'a>,
) -> String {
    match MESSAGES.lookup(name).and_then(|msg| msg.resolve(rng)) {
        Some(msg) => msg.format(params).unwrap_or_else(|e| {
            error!("Error formatting template: {}", e);
            "Template Error".to_string()
        }),
        None => {
            error!("Message not found: {}", name);
            "Template Not Found".to_string()
        }
    }
}
