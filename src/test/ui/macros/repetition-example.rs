// run-pass

#[derive(Debug)]
struct Example<'a> {
    indexes: &'a [(u32, u32)],
    counts: &'a [u32],
    nested: Vec<Example<'a>>,
}

macro_rules! example {
    ( $( [ $( ( $( $x:ident )* ) )* ] )* ) => {
        Example {
            indexes: &[],
            counts: &[${count(x, 1)}, ${count(x, 2)}, ${count(x, 3)}],
            nested: vec![
            $(
                Example {
                    indexes: &[(${index()}, ${length()})],
                    counts: &[${count(x, 1)}, ${count(x, 2)}],
                    nested: vec![
                    $(
                        Example {
                            indexes: &[(${index(1)}, ${length(1)}), (${index()}, ${length()})],
                            counts: &[${count(x)}],
                            nested: vec![
                            $(
                                Example {
                                    indexes: &[
                                        (${index(2)}, ${length(2)}),
                                        (${index(1)}, ${length(1)}),
                                        (${index()}, ${length()})
                                    ],
                                    counts: &[],
                                    nested: vec![],
                                    ${ignore(x)}
                                }
                            ),*
                            ]
                        }
                    ),*
                    ]
                }
            ),*
            ]
        }
    };
}

static EXPECTED: &str = concat!(
    "Example { indexes: [], counts: [2, 4, 13], nested: [",
    concat!(
        "Example { indexes: [(0, 2)], counts: [3, 10], nested: [",
        concat!(
            "Example { indexes: [(0, 2), (0, 3)], counts: [4], nested: [",
            concat!(
                "Example { indexes: [(0, 2), (0, 3), (0, 4)], counts: [], nested: [] }, ",
                "Example { indexes: [(0, 2), (0, 3), (1, 4)], counts: [], nested: [] }, ",
                "Example { indexes: [(0, 2), (0, 3), (2, 4)], counts: [], nested: [] }, ",
                "Example { indexes: [(0, 2), (0, 3), (3, 4)], counts: [], nested: [] }", 
            ),
            "] }, ",
            "Example { indexes: [(0, 2), (1, 3)], counts: [4], nested: [",
            concat!(
                "Example { indexes: [(0, 2), (1, 3), (0, 4)], counts: [], nested: [] }, ",
                "Example { indexes: [(0, 2), (1, 3), (1, 4)], counts: [], nested: [] }, ",
                "Example { indexes: [(0, 2), (1, 3), (2, 4)], counts: [], nested: [] }, ",
                "Example { indexes: [(0, 2), (1, 3), (3, 4)], counts: [], nested: [] }",
            ),
            "] }, ",
            "Example { indexes: [(0, 2), (2, 3)], counts: [2], nested: [",
            concat!(
                "Example { indexes: [(0, 2), (2, 3), (0, 2)], counts: [], nested: [] }, ",
                "Example { indexes: [(0, 2), (2, 3), (1, 2)], counts: [], nested: [] }",
            ),
            "] }",
        ),
        "] }, ",
        "Example { indexes: [(1, 2)], counts: [1, 3], nested: [",
        concat!(
            "Example { indexes: [(1, 2), (0, 1)], counts: [3], nested: [",
            concat!(
                "Example { indexes: [(1, 2), (0, 1), (0, 3)], counts: [], nested: [] }, ",
                "Example { indexes: [(1, 2), (0, 1), (1, 3)], counts: [], nested: [] }, ",
                "Example { indexes: [(1, 2), (0, 1), (2, 3)], counts: [], nested: [] }",
            ),
            "] }",
        ),
        "] }",
    ),
    "] }",
);

fn main() {
    let e = example! {
        [ ( A B C D ) ( E F G H ) ( I J ) ]
        [ ( K L M ) ]
    };
    let debug = format!("{:?}", e);
    assert_eq!(debug, EXPECTED);
}
