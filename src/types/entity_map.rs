use crate::entities::entity::Identified;
use crate::entities::EntityID;
use crate::types::Neighbors;
use crate::types::Position;
use crate::types::Positioned;
use crate::types::PositionedMut;
use alga::general::{
    AbstractMagma, AbstractMonoid, AbstractSemigroup, Additive, Identity,
};
use std::collections::{hash_map, BTreeMap, HashMap};
use std::iter::FromIterator;

#[derive(Debug, Clone, Default)]
pub struct EntityMap<A> {
    by_position: BTreeMap<Position, Vec<EntityID>>,
    by_id: HashMap<EntityID, A>,
    last_id: EntityID,
}

impl<A: PartialEq> PartialEq for EntityMap<A> {
    fn eq(&self, other: &Self) -> bool {
        self.by_position == other.by_position && self.by_id == other.by_id
    }
}
impl<A: Eq> Eq for EntityMap<A> {}

const BY_POS_INVARIANT: &str =
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
        self.by_position
            .get(&pos)
            .iter()
            .flat_map(|eids| {
                eids.iter()
                    .map(|eid| self.by_id.get(eid).expect(BY_POS_INVARIANT))
            })
            .collect()
    }

    /// Remove all entities at the given position
    pub fn remove_all_at(&mut self, pos: Position) {
        if let Some(eids) = self.by_position.remove(&pos) {
            for eid in eids {
                self.by_id.remove(&eid).expect(BY_POS_INVARIANT);
            }
        }
    }

    pub fn get(&self, id: EntityID) -> Option<&A> {
        self.by_id.get(&id)
    }

    pub fn get_mut(&mut self, id: EntityID) -> Option<&mut A> {
        self.by_id.get_mut(&id)
    }

    pub fn entities(&self) -> impl Iterator<Item = &A> {
        self.by_id.values()
    }

    pub fn entities_mut(&mut self) -> impl Iterator<Item = &mut A> {
        self.by_id.values_mut()
    }

    pub fn ids(&self) -> hash_map::Keys<'_, EntityID, A> {
        self.by_id.keys()
    }

    pub fn drain(&mut self) -> Drain<'_, A> {
        let ids = self.ids().copied().collect::<Vec<_>>();
        Drain {
            map: self,
            ids_iter: Box::new(ids.into_iter()),
        }
    }

    fn next_id(&mut self) -> EntityID {
        self.last_id += 1;
        self.last_id
    }
}

impl<A: Positioned + Identified<EntityID>> EntityMap<A> {
    pub fn insert(&mut self, mut entity: A) -> EntityID {
        let pos = entity.position();
        let entity_id = self.next_id();
        entity.set_id(entity_id);
        self.by_id.entry(entity_id).or_insert(entity);
        self.by_position
            .entry(pos)
            .or_insert_with(Vec::new)
            .push(entity_id);
        entity_id
    }

    /// Remove the entity with the given ID
    pub fn remove(&mut self, id: EntityID) -> Option<A> {
        self.by_id.remove(&id).map(|e| {
            let mut empty = false;
            let position = e.position();

            if let Some(es) = self.by_position.get_mut(&position) {
                es.retain(|e| *e != id);
                if es.is_empty() {
                    empty = true;
                }
            }

            if empty {
                self.by_position.remove(&position);
            }
            e
        })
    }

    /// Moves all elements from `other` into `Self`, leathing `other` empty.
    pub fn append(&mut self, other: &mut Self) {
        // TODO there's probably some perf opportunities here by calling
        // reserve() on stuff
        for (_, entity) in other.drain() {
            self.insert(entity);
        }
    }

    /// Gets all 8 neighbors of the given position.
    pub fn neighbors<'a>(
        &'a self,
        position: Position,
    ) -> Neighbors<Vec<(EntityID, &'a A)>> {
        Neighbors::of_position(position)
            .map(|pos| self.at(*pos))
            .mapmap(&|e| (e.id(), *e))
    }

    pub fn neighbor_entities<'a>(
        &'a self,
        position: Position,
    ) -> Neighbors<Vec<&'a A>> {
        self.neighbors(position).mapmap(&|(_eid, ent)| *ent)
    }

    pub fn check_invariants(&self) {
        for (id, ent) in &self.by_id {
            assert_eq!(*id, ent.id());
        }

        for (pos, ents) in &self.by_position {
            for eid in ents {
                let ent = self.by_id.get(eid).unwrap();
                assert_eq!(*pos, ent.position())
            }
        }
    }
}

impl<'a, A: Positioned + Identified<EntityID>> IntoIterator
    for &'a EntityMap<A>
{
    type Item = (&'a EntityID, &'a A);
    type IntoIter = std::collections::hash_map::Iter<'a, EntityID, A>;
    fn into_iter(self) -> Self::IntoIter {
        (&self.by_id).iter()
    }
}

impl<A: Positioned + Identified<EntityID>> IntoIterator for EntityMap<A> {
    type Item = (EntityID, A);
    type IntoIter = std::collections::hash_map::IntoIter<EntityID, A>;
    fn into_iter(self) -> Self::IntoIter {
        self.by_id.into_iter()
    }
}

impl<A: Positioned + Identified<EntityID>> FromIterator<A> for EntityMap<A> {
    fn from_iter<I: IntoIterator<Item = A>>(iter: I) -> Self {
        let mut em = EntityMap::new();
        for ent in iter {
            em.insert(ent);
        }
        em
    }
}

impl<A: Positioned + Identified<EntityID> + Eq + Clone> AbstractMagma<Additive>
    for EntityMap<A>
{
    fn operate(&self, right: &Self) -> Self {
        let mut by_position = self.by_position.clone();
        by_position.append(&mut right.by_position.clone());

        let mut by_id = self.by_id.clone();
        for (k, v) in right.by_id.clone() {
            by_id.insert(k, v);
        }

        EntityMap {
            by_position,
            by_id,
            last_id: self.last_id.max(right.last_id),
        }
    }
}

impl<A: Positioned + Identified<EntityID> + Eq + Clone>
    AbstractSemigroup<Additive> for EntityMap<A>
{
}

impl<A: Positioned + Identified<EntityID> + Eq> Identity<Additive>
    for EntityMap<A>
{
    fn identity() -> Self {
        EntityMap::new()
    }
}

impl<A: Positioned + Identified<EntityID> + Eq + Clone> AbstractMonoid<Additive>
    for EntityMap<A>
{
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

        if let Some(p) = old_pos {
            if let Some(es) = self.by_position.get_mut(&p) {
                es.retain(|e| *e != entity_id);
            }

            self.by_position
                .entry(new_position)
                .or_insert_with(Vec::new)
                .push(entity_id);
        }
    }
}

pub struct Drain<'a, A> {
    map: &'a mut EntityMap<A>,
    ids_iter: Box<dyn Iterator<Item = EntityID> + 'a>,
}

impl<A: Positioned + Identified<EntityID>> Iterator for Drain<'_, A> {
    type Item = (EntityID, A);

    fn next(&mut self) -> Option<Self::Item> {
        self.ids_iter
            .next()
            .map(|eid| (eid, self.map.remove(eid).expect(BY_POS_INVARIANT)))
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
        _id: Option<EntityID>,
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

    impl Identified<EntityID> for TestEntity {
        fn opt_id(&self) -> Option<EntityID> {
            self._id
        }

        fn set_id(&mut self, id: EntityID) {
            self._id = Some(id);
        }
    }

    fn gen_entity_map() -> BoxedStrategy<EntityMap<TestEntity>> {
        any::<Vec<TestEntity>>()
            .prop_map(|ents| {
                ents.iter().cloned().collect::<EntityMap<TestEntity>>()
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
                map.insert(ent.clone());
            }
            assert_eq!(map.len(), items.len());
        }

        #[test]
        fn test_entity_map_getset(
            mut em in gen_entity_map(),
            ent: TestEntity
        ) {
            em.insert(ent.clone());
            assert!(em.at(ent.position).iter().any(|e| e.name == ent.name))
        }

        #[test]
        fn test_entity_map_set_iter_contains(
            mut em in gen_entity_map(),
            ent: TestEntity
        ) {
            em.insert(ent.clone());
            assert!(em.entities().any(|e| e.name == ent.name))
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
            assert!(
                em.at(new_position).iter().map(
                    |e| e.name.clone()).any(|en| en == ent.name),
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

        #[test]
        fn test_entity_map_semigroup_laws(
            em1 in gen_entity_map(),
            em2 in gen_entity_map(),
            em3 in gen_entity_map(),
        ) {
            assert!(AbstractSemigroup::prop_is_associative((em1, em2, em3)));
        }

        fn test_entity_map_monoid_laws(
            em in gen_entity_map(),
        ) {
            assert!(
                AbstractMonoid::prop_operating_identity_element_is_noop((em,))
            );
        }

        #[test]
        fn test_entity_map_append(
            mut target in gen_entity_map(),
            mut source in gen_entity_map(),
        ) {
            let orig_target = target.clone();
            let orig_source = source.clone();

            target.append(&mut source);
            target.check_invariants();

            assert_eq!(source, EntityMap::new());

            for ent in orig_source.entities() {
                assert!(
                    target.at(ent.position()).iter().any(|e| e.name == ent.name)
                );
            }

            for ent in orig_target.entities() {
                assert!(
                    target.at(ent.position()).iter().any(|e| e.name == ent.name)
                );
            }
        }
    }
}
