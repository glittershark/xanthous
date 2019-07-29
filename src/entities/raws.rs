pub use crate::entities::raw_types::{CreatureType, EntityRaw, ItemType};
use std::collections::HashMap;

static_cfg! {
    static ref RAWS: Vec<EntityRaw<'static>> = cfg_dir("src/entities/raws");
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
    RAWS_BY_NAME
        .get(name)
        .copied()
        .unwrap_or_else(|| panic!("Raw not found: {}", name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_raws() {
        RAWS_BY_NAME.keys();
        assert_eq!(raw("noodles").name(), "noodles");
    }
}
