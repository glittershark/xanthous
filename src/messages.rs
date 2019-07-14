use rand::seq::SliceRandom;
use rand::Rng;
use serde::de::MapAccess;
use serde::de::SeqAccess;
use serde::de::Visitor;
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;

#[derive(Deserialize, Debug, PartialEq, Eq)]
#[serde(untagged)]
enum Message<'a> {
    Single(&'a str),
    Choice(Vec<&'a str>),
}

impl<'a> Message<'a> {
    fn resolve<R: Rng + ?Sized>(&self, rng: &mut R) -> Option<&'a str> {
        use Message::*;
        match self {
            Single(msg) => Some(*msg),
            Choice(msgs) => msgs.choose(rng).map(|msg| *msg),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum NestedMap<'a> {
    Direct(Message<'a>),
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

struct NestedMapVisitor<'a> {
    marker: PhantomData<fn() -> NestedMap<'a>>,
}

impl<'a> NestedMapVisitor<'a> {
    fn new() -> Self {
        NestedMapVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de> Visitor<'de> for NestedMapVisitor<'de> {
    type Value = NestedMap<'de>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(
            "A message, a list of messages, or a nested map of messages",
        )
    }

    fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E> {
        Ok(NestedMap::Direct(Message::Single(v)))
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut choices = Vec::with_capacity(seq.size_hint().unwrap_or(0));
        while let Some(choice) = seq.next_element()? {
            choices.push(choice);
        }
        Ok(NestedMap::Direct(Message::Choice(choices)))
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut nested = HashMap::with_capacity(map.size_hint().unwrap_or(0));
        while let Some((k, v)) = map.next_entry()? {
            nested.insert(k, v);
        }
        Ok(NestedMap::Nested(nested))
    }
}

impl<'de> serde::Deserialize<'de> for NestedMap<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(NestedMapVisitor::new())
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
                    "hello" => NestedMap::Direct(Message::Single("Hello World!")),
                }),
                "foo" => NestedMap::Nested(hashmap!{
                    "bar" => NestedMap::Nested(hashmap!{
                        "single" => NestedMap::Direct(Message::Single("Single")),
                        "choice" => NestedMap::Direct(Message::Choice(
                            vec!["Say this", "Or this"]
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
            Some(&Message::Single("Hello World!"))
        );
        assert_eq!(
            map.lookup("foo.bar.single"),
            Some(&Message::Single("Single"))
        );
        assert_eq!(
            map.lookup("foo.bar.choice"),
            Some(&Message::Choice(vec!["Say this", "Or this"]))
        );
    }
}

static_cfg! {
    static ref MESSAGES: NestedMap<'static> = toml_file("messages.toml");
}

/// Look up a game message based on the given (dot-separated) name, with the
/// given random generator used to select from choice-based messages
pub fn message<R: Rng + ?Sized>(name: &str, rng: &mut R) -> &'static str {
    MESSAGES
        .lookup(name)
        .and_then(|msg| msg.resolve(rng))
        .unwrap_or_else(|| {
            error!("Message not found: {}", name);
            "Message not found"
        })
}
