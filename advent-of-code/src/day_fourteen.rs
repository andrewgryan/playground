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
pub struct Polymer(String);

impl Polymer {
    pub fn to_string(&self) -> String {
        match &self {
            Polymer(s) => s.to_string(),
        }
    }
}

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
pub struct Rule(String, char);

impl Rule {
    pub fn new(s: &str, c: char) -> Self {
        Self(s.to_string(), c)
    }
}

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

pub fn apply(rules: &Vec<Rule>, polymer: Polymer) -> Polymer {
    // Map rules
    let mut table: HashMap<String, char> = HashMap::new();
    for rule in rules {
        table.insert(rule.0.clone(), rule.1.clone());
    }

    // Perform substitution
    let s = polymer.to_string();
    let left = s.chars();
    let mut right = s.chars();
    right.next();
    let words: Vec<String> = left
        .zip(right)
        .map(|(a, b)| {
            let mut word = String::new();
            word.push(a);
            word.push(b);
            word
        })
        .collect();

    let chars: Vec<char> = words
        .iter()
        .map(|w| table.get(w).unwrap())
        .map(|c| *c)
        .collect();

    let n = s.len() + chars.len();
    let mut t: String = String::with_capacity(n);
    let mut polymer_letters = s.chars();
    let mut insertion_letters = chars.iter();
    for i in 0..n {
        if i % 2 == 0 {
            t.push(polymer_letters.next().unwrap());
        } else {
            t.push(*insertion_letters.next().unwrap());
        }
    }

    Polymer(t)
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

    #[test]
    fn perform_insertion() {
        let polymer: Polymer = "NNCB".parse().unwrap();
        let rules: Vec<Rule> = vec![
            Rule::new("CH", 'B'),
            Rule::new("HH", 'N'),
            Rule::new("CB", 'H'),
            Rule::new("NH", 'C'),
            Rule::new("HB", 'C'),
            Rule::new("HC", 'B'),
            Rule::new("HN", 'C'),
            Rule::new("NN", 'C'),
            Rule::new("BH", 'H'),
            Rule::new("NC", 'B'),
            Rule::new("NB", 'B'),
            Rule::new("BN", 'B'),
            Rule::new("BB", 'N'),
            Rule::new("BC", 'B'),
            Rule::new("CC", 'N'),
            Rule::new("CN", 'C'),
        ];
        let actual = apply(&rules, polymer);
        let expected: Polymer = "NCNBCHB".parse().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn perform_insertion_twice() {
        let polymer: Polymer = "NNCB".parse().unwrap();
        let rules: Vec<Rule> = vec![
            Rule::new("CH", 'B'),
            Rule::new("HH", 'N'),
            Rule::new("CB", 'H'),
            Rule::new("NH", 'C'),
            Rule::new("HB", 'C'),
            Rule::new("HC", 'B'),
            Rule::new("HN", 'C'),
            Rule::new("NN", 'C'),
            Rule::new("BH", 'H'),
            Rule::new("NC", 'B'),
            Rule::new("NB", 'B'),
            Rule::new("BN", 'B'),
            Rule::new("BB", 'N'),
            Rule::new("BC", 'B'),
            Rule::new("CC", 'N'),
            Rule::new("CN", 'C'),
        ];
        let actual = apply(&rules, apply(&rules, polymer));
        let expected: Polymer = "NBCCNBBBCBHCB".parse().unwrap();

        assert_eq!(actual, expected);
    }
}
