#[derive(Debug, PartialEq)]
pub struct Graph(Vec<Edge>);

impl std::str::FromStr for Graph {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(
            s.split('\n')
                .map(|line| line.parse::<Edge>().unwrap())
                .collect(),
        ))
    }
}

#[derive(Debug, PartialEq)]
pub struct Edge(Node, Node);

impl std::str::FromStr for Edge {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let left: Node = s.split('-').nth(0).unwrap().parse().unwrap();
        let right: Node = s.split('-').nth(1).unwrap().parse().unwrap();
        Ok(Self(left, right))
    }
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Start,
    End,
    BigCave(String),
    SmallCave(String),
}

impl std::str::FromStr for Node {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "end" => Ok(Node::End),
            "start" => Ok(Node::Start),
            _ => {
                let first_letter: char = s.chars().next().unwrap();
                if first_letter.is_uppercase() {
                    Ok(Node::BigCave(s.to_string()))
                } else {
                    Ok(Node::SmallCave(s.to_string()))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;
    use Node::*;

    #[rstest]
    #[case("A", BigCave("A".to_string()))]
    #[case("a", SmallCave("a".to_string()))]
    #[case("end", End)]
    #[case("start", Start)]
    fn str_to_node(#[case] s: &str, #[case] expected: Node) {
        assert_eq!(s.parse::<Node>().unwrap(), expected);
    }

    fn str_to_edge() {
        let s: &str = "start-A";
        let actual: Edge = s.parse().unwrap();
        let expected: Edge = Edge(Start, BigCave("A".to_string()));
        assert_eq!(actual, expected);
    }

    fn str_to_graph() {
        let s: &str = "start-A\nA-end";
        let actual: Graph = s.parse().unwrap();
        let expected: Graph = Graph(vec![
            Edge(Start, BigCave("A".to_string())),
            Edge(BigCave("A".to_string()), End),
        ]);
        assert_eq!(actual, expected);
    }
}
