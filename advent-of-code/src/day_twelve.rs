use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Graph {
    edges: Vec<Edge>,
}

impl Graph {
    pub fn from_vec(edges: Vec<Edge>) -> Self {
        Self { edges }
    }
    pub fn find_path(&self) -> Vec<Node> {
        // Build vertex structure
        let mut map: HashMap<Node, Node> = HashMap::new();
        for edge in &self.edges {
            map.insert(edge.0.clone(), edge.1.clone());
        }

        // Traverse mapping
        let mut path: Vec<Node> = vec![];
        let mut node: Node = Node::Start;
        loop {
            match map.get(&node) {
                None => break,
                Some(next_node) => match next_node {
                    Node::End => {
                        path.push(node);
                        path.push(next_node.clone());
                        break;
                    }
                    _ => {
                        path.push(node);
                        node = next_node.clone();
                    }
                },
            }
        }

        path
    }
}

impl std::str::FromStr for Graph {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from_vec(
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
        // Remove whitespace
        let text: String = s.split_whitespace().collect();
        let lhs: &str = text.split('-').nth(0).unwrap();
        let rhs: &str = text.split('-').nth(1).unwrap();
        let left: Node = lhs.parse().unwrap();
        let right: Node = rhs.parse().unwrap();
        Ok(Self(left, right))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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

    #[rstest]
    #[case("start-end", Edge(Start, End))]
    #[case("  start-end", Edge(Start, End))]
    fn str_to_edge(#[case] s: &str, #[case] expected: Edge) {
        let actual: Edge = s.parse().unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn str_to_graph() {
        let s: &str = "start-A
                       A-end";
        let actual: Graph = s.parse().unwrap();
        let expected: Graph = Graph::from_vec(vec![
            Edge(Start, BigCave("A".to_string())),
            Edge(BigCave("A".to_string()), End),
        ]);
        assert_eq!(actual, expected);
    }

    #[test]
    fn find_path() {
        let graph: Graph = "start-a
                            a-end"
            .parse()
            .unwrap();
        let actual = graph.find_path();
        let expected: Vec<Node> = vec![Start, "a".parse().unwrap(), End];
        assert_eq!(actual, expected);
    }
}
