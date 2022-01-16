use std::collections::HashMap;
use std::str::FromStr;

pub fn part_two(puzzle: &str) -> u64 {
    let mut letters: HashMap<char, u64> = HashMap::new();
    let mut pairs: HashMap<(char, char), u64> = HashMap::new();

    let polymer: Polymer = puzzle.split('\n').next().unwrap().parse().unwrap();

    // Initialise letters tally
    for c in polymer.to_string().chars() {
        let ptr = letters.entry(c).or_insert(0);
        *ptr += 1;
    }

    // Initialize pairs tally
    let s = polymer.to_string();
    let left = s.chars();
    let mut right = s.chars();
    right.next();
    for pair in left.zip(right) {
        let ptr = pairs.entry(pair).or_insert(0);
        *ptr += 1;
    }

    let rules: Vec<Rule> = puzzle
        .split('\n')
        .map(|l| l.parse())
        .filter(|r| r.is_ok())
        .map(|r| r.unwrap())
        .collect();

    // Map rules
    let mut table: HashMap<(char, char), char> = HashMap::new();
    for rule in rules {
        let pair = rule.0.clone();
        let a = pair.chars().next().unwrap();
        let b = pair.chars().nth(1).unwrap();
        let c = rule.1.clone();
        table.insert((a, b), c);
    }

    for _ in 0..40 {
        // Apply rules to pairs + letters
        let mut next_pairs: HashMap<(char, char), u64> = HashMap::new();
        for ((a, b), c) in &table {
            let factor = pairs.get(&(*a, *b)).unwrap_or(&0);
            if *factor == 0 {
                continue;
            } else {
                // AC
                let ptr = next_pairs.entry((*a, *c)).or_insert(0);
                *ptr += factor;
                // CB
                let ptr = next_pairs.entry((*c, *b)).or_insert(0);
                *ptr += factor;

                // Letters tally
                let ptr = letters.entry(*c).or_insert(0);
                *ptr += factor;
            }
        }
        pairs = next_pairs.clone();
    }

    // Score letter tally
    let most_frequent = letters.values().max().unwrap();
    let least_frequent = letters.values().min().unwrap();
    most_frequent - least_frequent
}

pub fn part_one(puzzle: &str) -> u32 {
    let n = 10; // 10 for part 1

    let mut polymer: Polymer = puzzle.split('\n').next().unwrap().parse().unwrap();

    let rules: Vec<Rule> = puzzle
        .split('\n')
        .map(|l| l.parse())
        .filter(|r| r.is_ok())
        .map(|r| r.unwrap())
        .collect();

    // Apply N steps of rules
    for _ in 0..n {
        polymer = apply(&rules, polymer);
    }

    score(&polymer.to_string())
}

pub fn score(s: &str) -> u32 {
    let mut counts: HashMap<char, u32> = HashMap::new();
    for c in s.chars() {
        let ptr = counts.entry(c).or_insert(0);
        *ptr += 1;
    }
    let most_frequent = counts.values().max().unwrap();
    let least_frequent = counts.values().min().unwrap();
    most_frequent - least_frequent
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

    #[test]
    fn test_score() {
        let actual = score("NNCB");
        let expected = 1;
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_part_one() {
        let puzzle = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C";
        let actual = part_one(puzzle);
        let expected = 1588;
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_part_two() {
        let puzzle = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C";
        let actual = part_two(puzzle);
        let expected = 2188189693529;
        assert_eq!(actual, expected);
    }

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
