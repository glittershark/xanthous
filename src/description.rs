use crate::entities::Describe;

pub fn list_to_sentence(lst: &[String]) -> String {
    let mut buf = String::with_capacity(
        lst.iter()
            .map(|e| e.len() + 2usize /* ", " */)
            .sum::<usize>()
            + if lst.len() >= 3 {
                3usize /* "and" */
            } else {
                0usize
            },
    );

    match lst.len() {
        0 => {}
        1 => buf.push_str(&lst[0]),
        2 => {
            buf.push_str(&lst[0]);
            buf.push_str(" and ");
            buf.push_str(&lst[1]);
        }
        _ => {
            for desc in &lst[..lst.len() - 1] {
                buf.push_str(desc);
                buf.push_str(", ");
            }
            buf.push_str("and ");
            buf.push_str(&lst[lst.len() - 1]);
        }
    }

    buf
}

pub fn describe_list<A: Describe>(lst: &[A]) -> String {
    list_to_sentence(
        &lst.iter().map(|e| e.description()).collect::<Vec<String>>(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use proptest_derive::Arbitrary;

    #[derive(Debug, Arbitrary)]
    struct Description(String);

    impl Describe for Description {
        fn description(&self) -> String {
            self.0.clone()
        }
    }

    proptest! {
        #[test]
        fn test_describe_list_includes_all_descriptions(
            descriptions: Vec<Description>
        ) {
            let res = describe_list(&descriptions);
            for Description(desc) in descriptions {
                assert!(res.contains(&desc));
            }
        }
    }

    #[test]
    fn test_describe_list() {
        assert_eq!(
            describe_list(&[Description("one".to_string())]),
            "one".to_string()
        );

        assert_eq!(
            describe_list(&[
                Description("one".to_string()),
                Description("two".to_string())
            ]),
            "one and two".to_string()
        );

        assert_eq!(
            describe_list(&[
                Description("one".to_string()),
                Description("two".to_string()),
                Description("three".to_string())
            ]),
            "one, two, and three".to_string()
        );
    }
}
