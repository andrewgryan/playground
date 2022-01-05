use std::collections::HashMap;
use std::str::FromStr;

pub fn part_one(puzzle: &str) -> u32 {
    println!("{}", puzzle);

    let polymer: Polymer = puzzle.split('\n').next().unwrap().parse().unwrap();

    let rules: Vec<Rule> = puzzle
        .split('\n')
        .map(|l| l.parse())
        .filter(|r| r.is_ok())
        .map(|r| r.unwrap())
        .collect();
    println!("{:?}", polymer);
    let mut table: HashMap<String, char> = HashMap::new();
    for rule in rules {
        table.insert(rule.0, rule.1);
    }
    println!("{:?}", table);
    0
}

#[derive(Debug, PartialEq)]
struct Polymer(String);

impl FromStr for Polymer {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() == 0 {
            return Err(());
        }
        let code = s.trim();
        if code.chars().all(|c| c.is_uppercase()) {
            Ok(Polymer(code.to_string()))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, PartialEq)]
struct Rule(String, char);

impl FromStr for Rule {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains("->") {
            let mut parts = s.split("->");
            let left = parts.next().unwrap().trim();
            let right = parts.next().unwrap().trim().chars().next().unwrap();
            Ok(Rule(left.to_string(), right))
        } else {
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case("NNCB", Err(()))]
    #[case("CH -> B", Ok(Rule("CH".to_string(), 'B')))]
    #[case("", Err(()))]
    fn parse_pair_insertion_rule(#[case] s: &str, #[case] expected: Result<Rule, ()>) {
        let actual: Result<Rule, _> = s.parse();
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case("NNCB", Ok(Polymer("NNCB".to_string())))]
    #[case("CH -> B", Err(()))]
    #[case("", Err(()))]
    fn parse_polymer(#[case] s: &str, #[case] expected: Result<Polymer, ()>) {
        let actual: Result<Polymer, _> = s.parse();
        assert_eq!(actual, expected);
    }
}
