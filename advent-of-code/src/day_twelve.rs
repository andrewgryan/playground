use std::collections::{HashMap, HashSet};

pub fn part_one(content: &str) -> usize {
    let graph: Graph = content.parse().unwrap();
    let paths: Vec<Vec<Node>> = graph.find_all_paths(rule_one).collect();
    paths.len()
}

pub fn part_two(content: &str) -> usize {
    let graph: Graph = content.parse().unwrap();
    let paths: Vec<Vec<Node>> = graph.find_all_paths(rule_two).collect();
    paths.len()
}

pub fn rule_one(current: &Node, _path: &Vec<Node>, visited: &mut HashSet<Node>) {
    match current {
        Node::BigCave(_) => (),
        _ => {
            visited.insert(current.clone());
        }
    };
}

pub fn rule_two(current: &Node, path: &Vec<Node>, visited: &mut HashSet<Node>) {
    match current {
        Node::BigCave(_) => (),
        Node::Start | Node::End => {
            visited.insert(current.clone());
        }
        Node::SmallCave(_) => small_cave_rule(current, path, visited),
    }
}

pub fn small_cave_rule(current: &Node, path: &Vec<Node>, visited: &mut HashSet<Node>) {
    // Allowed visit a single small cave twice
    let mut frequency: HashMap<&Node, u32> = HashMap::new();
    for node in path {
        match node {
            Node::SmallCave(_) => {
                let ptr = frequency.entry(node).or_insert(0);
                *ptr += 1;
            }
            _ => (),
        }
    }

    // Mark as visited only if path contains duplicated SmallCave
    if frequency.iter().map(|(_, v)| v).any(|v| v > &1) {
        // Mark all small caves as visited if any visited twice
        for node in path {
            match node {
                Node::SmallCave(_) => {
                    visited.insert(node.clone());
                }
                _ => (),
            }
        }
    }
}

pub fn to_adjacency(edges: Vec<Edge>) -> HashMap<Node, Vec<Node>> {
    let mut adjacency_list: HashMap<Node, Vec<Node>> = HashMap::new();

    for edge in &edges {
        // Edge 0 -> 1
        let nodes = adjacency_list
            .entry(edge.0.clone())
            .or_insert_with(Vec::new);
        nodes.push(edge.1.clone());

        // Edge 1 -> 0
        let nodes = adjacency_list
            .entry(edge.1.clone())
            .or_insert_with(Vec::new);
        nodes.push(edge.0.clone());
    }

    adjacency_list
}

pub struct IterPath {
    adjacency_list: HashMap<Node, Vec<Node>>,
    stack: Vec<(Node, Vec<Node>, HashSet<Node>)>,
    rule: fn(&Node, &Vec<Node>, &mut HashSet<Node>),
}

impl Iterator for IterPath {
    type Item = Vec<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.stack.len() > 0 {
            let (node, path, mut visited) = self.stack.pop().unwrap();

            if node == Node::End {
                return Some(path);
            }

            if visited.contains(&node) {
                continue;
            }

            // Support visiting BigCave more than once
            (&self.rule)(&node, &path, &mut visited);

            for neighbour in self.adjacency_list.get(&node).unwrap() {
                let mut next_path = path.clone();
                next_path.push(neighbour.clone());
                self.stack
                    .push((neighbour.clone(), next_path, visited.clone()));
            }
        }
        None
    }
}

#[derive(Debug, PartialEq)]
pub struct Graph {
    edges: Vec<Edge>,
}

impl Graph {
    pub fn from_vec(edges: Vec<Edge>) -> Self {
        Self { edges }
    }

    pub fn find_all_paths(&self, rule: fn(&Node, &Vec<Node>, &mut HashSet<Node>)) -> IterPath {
        let adjacency_list = to_adjacency(self.edges.clone());
        let start = Node::Start;
        IterPath {
            adjacency_list: adjacency_list.clone(),
            stack: vec![(start.clone(), vec![start], HashSet::new())],
            rule,
        }
    }

    fn build_map(&self) -> HashMap<Node, Vec<Node>> {
        let mut map: HashMap<Node, Vec<Node>> = HashMap::new();
        for edge in &self.edges {
            let nodes = map.entry(edge.0.clone()).or_insert_with(Vec::new);
            nodes.push(edge.1.clone());
        }
        map
    }

    pub fn find_path(&self) -> Vec<Node> {
        // Build vertex structure
        let map: HashMap<Node, Vec<Node>> = self.build_map();

        // Traverse mapping
        let mut path: Vec<Node> = vec![];
        let mut node: Node = Node::Start;
        loop {
            match map.get(&node) {
                None => break,
                Some(nodes) => {
                    let next_node = nodes.iter().next().unwrap();
                    match next_node {
                        Node::End => {
                            path.push(node);
                            path.push(next_node.clone());
                            break;
                        }
                        _ => {
                            path.push(node);
                            node = next_node.clone();
                        }
                    }
                }
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
                .map(|line| line.parse::<Edge>())
                .filter(|result| result.is_ok())
                .map(|result| result.unwrap())
                .collect(),
        ))
    }
}

#[derive(Debug, PartialEq)]
pub struct Path(Vec<Node>);

impl Path {
    pub fn to_vec(&self) -> Vec<Node> {
        match self {
            Path(v) => v.clone(),
        }
    }
}

impl std::str::FromStr for Path {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let text: String = s.split_whitespace().collect();
        let nodes: Vec<Node> = text.split(',').map(|s| s.parse().unwrap()).collect();
        Ok(Self(nodes))
    }
}

pub fn fib(n: u32) -> u32 {
    fib_recurse(1, n)
}
pub fn fib_recurse(c: u32, n: u32) -> u32 {
    match n {
        0 => c,
        1 => c,
        _ => fib_recurse(n * c, n - 1),
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Edge(Node, Node);

impl std::str::FromStr for Edge {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains('-') {
            // Remove whitespace
            let text: String = s.split_whitespace().collect();
            let lhs: &str = text.split('-').next().unwrap();
            let rhs: &str = text.split('-').nth(1).unwrap();
            let left: Node = lhs.parse().unwrap();
            let right: Node = rhs.parse().unwrap();
            Ok(Self(left, right))
        } else {
            Err(())
        }
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

    #[rstest]
    #[case("start-a
            a-end", vec!["start", "a", "end"])]
    #[case("start-a
            start-b
            a-end
            b-end", vec!["start", "a", "end"])]
    fn find_path(#[case] s: &str, #[case] nodes: Vec<&str>) {
        let graph: Graph = s.parse().unwrap();
        let actual = graph.find_path();
        let expected: Vec<Node> = nodes.iter().map(|s| s.parse().unwrap()).collect();
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case("start-a
            start-b
            a-end
            b-end", 2, vec!["start,a,end", "start,b,end"])]
    #[case("start-A
            start-b
            A-c
            A-b
            b-d
            A-end
            b-end", 10, vec![
            "start,A,b,A,c,A,end",
            "start,A,b,A,end",
            "start,A,b,end",
            "start,A,c,A,b,A,end",
            "start,A,c,A,b,end",
            "start,A,c,A,end",
            "start,A,end",
            "start,b,A,c,A,end",
            "start,b,A,end",
            "start,b,end",
    ])]
    fn find_all_paths(#[case] s: &str, #[case] n: usize, #[case] texts: Vec<&str>) {
        let graph: Graph = s.parse().unwrap();
        let actual: Vec<Vec<Node>> = graph.find_all_paths(rule_one).collect();
        assert_eq!(actual.len(), n);
        for text in texts {
            let path: Path = text.parse().unwrap();
            assert!(actual.contains(&path.to_vec()), "contains: {:?}", path);
        }
    }

    #[rstest]
    #[case("start-A
            start-b
            A-c
            A-b
            b-d
            A-end
            b-end", 36, vec![
            "start,A,b,A,b,A,c,A,end",
            "start,A,b,A,b,A,end",
            "start,A,b,A,b,end",
            "start,A,b,A,c,A,b,A,end",
            "start,A,b,A,c,A,b,end",
            "start,A,b,A,c,A,c,A,end",
            "start,A,b,A,c,A,end",
            "start,A,b,A,end",
            "start,A,b,d,b,A,c,A,end",
            "start,A,b,d,b,A,end",
            "start,A,b,d,b,end",
            "start,A,b,end",
            "start,A,c,A,b,A,b,A,end",
            "start,A,c,A,b,A,b,end",
            "start,A,c,A,b,A,c,A,end",
            "start,A,c,A,b,A,end",
            "start,A,c,A,b,d,b,A,end",
            "start,A,c,A,b,d,b,end",
            "start,A,c,A,b,end",
            "start,A,c,A,c,A,b,A,end",
            "start,A,c,A,c,A,b,end",
            "start,A,c,A,c,A,end",
            "start,A,c,A,end",
            "start,A,end",
            "start,b,A,b,A,c,A,end",
            "start,b,A,b,A,end",
            "start,b,A,b,end",
            "start,b,A,c,A,b,A,end",
            "start,b,A,c,A,b,end",
            "start,b,A,c,A,c,A,end",
            "start,b,A,c,A,end",
            "start,b,A,end",
            "start,b,d,b,A,c,A,end",
            "start,b,d,b,A,end",
            "start,b,d,b,end",
            "start,b,end",
    ])]
    fn find_all_paths_with_duplicate_small_path(
        #[case] s: &str,
        #[case] n: usize,
        #[case] texts: Vec<&str>,
    ) {
        let graph: Graph = s.parse().unwrap();
        let actual: Vec<Vec<Node>> = graph.find_all_paths(rule_two).collect();
        assert_eq!(actual.len(), n);
        for text in texts {
            let path: Path = text.parse().unwrap();
            assert!(actual.contains(&path.to_vec()), "contains: {:?}", path);
        }
    }

    #[test]
    fn adjaceny_list_from_edges() {
        let edges: Vec<Edge> = vec!["a-B", "B-c"]
            .iter()
            .map(|s| s.parse().unwrap())
            .collect();
        let actual: HashMap<Node, Vec<Node>> = to_adjacency(edges);
        let a = SmallCave("a".to_string());
        let b = BigCave("B".to_string());
        let c = SmallCave("c".to_string());
        assert!(actual.get(&a).unwrap().contains(&b));
        assert!(actual.get(&b).unwrap().contains(&a));
        assert!(actual.get(&b).unwrap().contains(&c));
        assert!(actual.get(&c).unwrap().contains(&b));
    }

    #[test]
    fn test_fib() {
        assert_eq!(fib(5), 120);
    }
}
