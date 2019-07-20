use crate::entities::entity_char::EntityChar;
use crate::messages::Message;
use crate::types::Speed;

#[derive(Debug, Deserialize)]
pub struct CreatureType<'a> {
    /// The name of the creature. Used in raw lookups.
    pub name: &'a str,

    /// A description of the entity, used by the "look" command
    pub description: &'a str,

    #[serde(rename = "char")]
    pub chr: EntityChar,
    pub max_hitpoints: u16,
    pub speed: Speed,
    pub friendly: bool,
}

#[derive(Debug, Deserialize)]
pub struct EdibleItem<'a> {
    #[serde(borrow)]
    pub eat_message: Option<Message<'a>>,

    /// The number of hitpoints that eating this item heals
    pub hitpoints_healed: u16,
}

#[derive(Debug, Deserialize)]
pub struct ItemType<'a> {
    pub name: &'a str,

    /// A description of the item, used by the "look" command
    pub description: &'a str,

    pub edible_item: Option<EdibleItem<'a>>,

    #[serde(rename = "char")]
    pub chr: EntityChar,
}

#[cfg(test)]
mod item_type_tests {
    use super::*;

    #[test]
    fn test_deserialize_item_type() {
        let result = serde_json::from_str(
            r#"{
                "Item": {
                    "name": "noodles",
                    "description": "You know exactly what kind of noodles",
                    "char": { "char": "n" },
                    "edible_item": {
                        "eat_message": "You slurp up the noodles",
                        "hitpoints_healed": 2
                    }
                }
            }"#,
        )
        .unwrap();
        assert_matches!(result, EntityRaw::Item(_));
        if let EntityRaw::Item(item) = result {
            assert_eq!(item.name, "noodles");
        }

        let toml_result = toml::from_str(
            r#"[Item]
name = "noodles"
description = "You know exactly what kind of noodles"
char = { char = "🍜" }
edible_item = { eat_message = "You slurp up the noodles", hitpoints_healed = 2 }
"#,
        )
        .unwrap();

        assert_matches!(toml_result, EntityRaw::Item(_));
        if let EntityRaw::Item(item) = toml_result {
            assert_eq!(item.name, "noodles");
        }
    }
}

impl<'a> ItemType<'a> {
    pub fn is_edible(&self) -> bool {
        self.edible_item.is_some()
    }
}

#[derive(Debug, Deserialize)]
pub enum EntityRaw<'a> {
    Creature(#[serde(borrow)] CreatureType<'a>),
    Item(#[serde(borrow)] ItemType<'a>),
}

impl<'a> EntityRaw<'a> {
    pub fn name(&self) -> &'a str {
        use EntityRaw::*;
        match self {
            Creature(typ) => typ.name,
            Item(typ) => typ.name,
        }
    }
}
