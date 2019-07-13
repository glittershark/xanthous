use crate::types::Position;
use crate::types::Positioned;
use crate::types::PositionedMut;
use std::collections::hash_map::HashMap;
use std::collections::BTreeMap;
use std::iter::FromIterator;

pub type EntityID = u32;

#[derive(Debug)]
pub struct EntityMap<A> {
    by_position: BTreeMap<Position, Vec<EntityID>>,
    by_id: HashMap<EntityID, A>,
    last_id: EntityID,
}

// impl<A: Debug> ArbitraryF1<A> for EntityMap<A> {
//     type Parameters = ();
//     fn lift1_with<AS>(base: AS, _: Self::Parameters) -> BoxedStrategy<Self>
//     where
//         AS: Strategy<Value = A> + 'static,
//     {
//         unimplemented!()
//     }
//     // type Strategy = strategy::Just<Self>;
//     // fn arbitrary_with(params : Self::Parameters) -> Self::Strategy;
// }

// impl<A: Arbitrary> Arbitrary for EntityMap<A> {
//     type Parameters = A::Parameters;
//     type Strategy = BoxedStrategy<Self>;
//     fn arbitrary_with(params: Self::Parameters) -> Self::Strategy {
//         let a_strat: A::Strategy = Arbitrary::arbitrary_with(params);
//         ArbitraryF1::lift1::<A::Strategy>(a_strat)
//     }
// }

const BY_POS_INVARIANT: &'static str =
    "Invariant: All references in EntityMap.by_position should point to existent references in by_id";

impl<A> EntityMap<A> {
    pub fn new() -> EntityMap<A> {
        EntityMap {
            by_position: BTreeMap::new(),
            by_id: HashMap::new(),
            last_id: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.by_id.len()
    }

    /// Returns a list of all entities at the given position
    pub fn at<'a>(&'a self, pos: Position) -> Vec<&'a A> {
        // self.by_position.get(&pos).iter().flat_map(|eids| {
        //     eids.iter()
        //         .map(|eid| self.by_id.get(eid).expect(BY_POS_INVARIANT))
        // })
        // gross.
        match self.by_position.get(&pos) {
            None => Vec::new(),
            Some(eids) => {
                let mut res = Vec::new();
                for eid in eids {
                    res.push(self.by_id.get(eid).expect(BY_POS_INVARIANT));
                }
                res
            }
        }
    }

    /// Remove all entities at the given position
    pub fn remove_all_at(&mut self, pos: Position) {
        self.by_position.remove(&pos).map(|eids| {
            for eid in eids {
                self.by_id.remove(&eid).expect(BY_POS_INVARIANT);
            }
        });
    }

    pub fn get<'a>(&'a self, id: EntityID) -> Option<&'a A> {
        self.by_id.get(&id)
    }

    pub fn entities<'a>(&'a self) -> impl Iterator<Item = &'a A> {
        self.by_id.values()
    }

    pub fn entities_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut A> {
        self.by_id.values_mut()
    }

    pub fn ids(&self) -> impl Iterator<Item = &EntityID> {
        self.by_id.keys()
    }

    fn next_id(&mut self) -> EntityID {
        self.last_id += 1;
        self.last_id
    }
}

impl<A: Positioned> EntityMap<A> {
    pub fn insert(&mut self, entity: A) -> EntityID {
        let pos = entity.position();
        let entity_id = self.next_id();
        self.by_id.entry(entity_id).or_insert(entity);
        self.by_position
            .entry(pos)
            .or_insert(Vec::new())
            .push(entity_id);
        entity_id
    }
}

impl<A: Positioned> FromIterator<A> for EntityMap<A> {
    fn from_iter<I: IntoIterator<Item = A>>(iter: I) -> Self {
        let mut em = EntityMap::new();
        for ent in iter {
            em.insert(ent);
        }
        em
    }
}

impl<A: PositionedMut> EntityMap<A> {
    pub fn update_position(
        &mut self,
        entity_id: EntityID,
        new_position: Position,
    ) {
        let mut old_pos = None;
        if let Some(entity) = self.by_id.get_mut(&entity_id) {
            if entity.position() == new_position {
                return;
            }
            old_pos = Some(entity.position());
            entity.set_position(new_position);
        }
        old_pos.map(|p| {
            self.by_position
                .get_mut(&p)
                .map(|es| es.retain(|e| *e != entity_id));

            self.by_position
                .entry(new_position)
                .or_insert(Vec::new())
                .push(entity_id);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::PositionedMut;
    use proptest::prelude::*;
    use proptest_derive::Arbitrary;

    #[derive(Debug, Arbitrary, PartialEq, Eq, Clone)]
    struct TestEntity {
        position: Position,
        name: String,
    }

    impl Positioned for TestEntity {
        fn position(&self) -> Position {
            self.position
        }
    }

    impl PositionedMut for TestEntity {
        fn set_position(&mut self, pos: Position) {
            self.position = pos
        }
    }

    fn gen_entity_map() -> BoxedStrategy<EntityMap<TestEntity>> {
        any::<Vec<TestEntity>>()
            .prop_map(|ents| {
                ents.iter()
                    .map(|e| e.clone())
                    .collect::<EntityMap<TestEntity>>()
            })
            .boxed()
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]

        #[test]
        fn test_entity_map_len(items: Vec<TestEntity>) {
            let mut map = EntityMap::new();
            assert_eq!(map.len(), 0);
            for ent in &items {
                map.insert(ent);
            }
            assert_eq!(map.len(), items.len());
        }

        #[test]
        fn test_entity_map_getset(
            mut em in gen_entity_map(),
            ent: TestEntity
        ) {
            em.insert(ent.clone());
            assert!(em.at(ent.position).iter().any(|e| **e == ent))
        }

        #[test]
        fn test_entity_map_set_iter_contains(
            mut em in gen_entity_map(),
            ent: TestEntity
        ) {
            em.insert(ent.clone());
            assert!(em.entities().any(|e| *e == ent))
        }

        #[test]
        fn test_update_position(
            mut em in gen_entity_map(),
            ent: TestEntity,
            new_position: Position,
        ) {
            let original_position = ent.position();
            let entity_id = em.insert(ent.clone());
            em.update_position(entity_id, new_position);

            if new_position != original_position {
                assert!(em.at(original_position).iter().all(|e| e.name != ent.name));
            }
            assert_eq!(
                em.get(entity_id).map(|e| e.position()),
                Some(new_position)
            );
            assert_eq!(
                em.at(new_position).iter().map(
                    |e| e.name.clone()).collect::<Vec<_>>(),
                vec![ent.name]
            )
        }

        #[test]
        fn test_remove_all_at(
            mut em in gen_entity_map(),
            pos: Position,
        ) {
            em.remove_all_at(pos);
            assert_eq!(em.at(pos).len(), 0);
        }
    }
}
