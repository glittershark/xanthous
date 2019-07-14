use crate::entities::entity_char::EntityChar;
use crate::types::Speed;
use std::collections::HashMap;

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
pub enum EntityRaw<'a> {
    Creature(#[serde(borrow)] CreatureType<'a>),
}

impl<'a> EntityRaw<'a> {
    pub fn name(&self) -> &'a str {
        match self {
            EntityRaw::Creature(typ) => typ.name,
        }
    }
}

static_cfg! {
    static ref RAWS: Vec<EntityRaw<'static>> = toml_dir("src/entities/raws");
}

lazy_static! {
    static ref RAWS_BY_NAME: HashMap<&'static str, &'static EntityRaw<'static>> = {
        let mut hm = HashMap::new();
        for er in RAWS.iter() {
            if hm.contains_key(er.name()) {
                panic!("Duplicate entity: {}", er.name())
            }

            hm.insert(er.name(), er);
        }
        hm
    };
}

pub fn raw(name: &'static str) -> &'static EntityRaw<'static> {
    debug!("{:?}", RAWS_BY_NAME.keys().collect::<Vec<&&'static str>>());
    RAWS_BY_NAME
        .get(name)
        .map(|e| *e)
        .expect(format!("Raw not found: {}", name).as_str())
}
