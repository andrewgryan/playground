use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;

// 0 -> abcefg
// 1 -> cf
// 2 -> acdeg
// 3 -> acdfg
// 4 -> bcdf
// 5 -> abdfg
// 6 -> abdefg
// 7 -> acf
// 8 -> abcdefg
// 9 -> abcdfg
//
pub fn part_one(input_file: &str) -> usize {
    println!("{}", input_file);
    let content = read_to_string(input_file).unwrap();
    let mut unique_codes = 0;
    for row in content.split('\n') {
        let codes: Vec<&str>;
        match row.split('|').nth(1) {
            None => continue,
            Some(s) => {
                codes = s.split_whitespace().collect();
            }
        }
        unique_codes += codes
            .iter()
            .filter(|s| simple_code(s.len()))
            .collect::<Vec<_>>()
            .len();
        println!("{:?}", codes);
    }
    unique_codes
}

pub fn part_two(input_file: &str) -> u32 {
    let mut result = 0;
    let content = read_to_string(input_file).unwrap();
    for row in content.split('\n') {
        let ciphers: Vec<&str>;
        let digits: Vec<&str>;
        match row.split('|').nth(0) {
            None => continue,
            Some(s) => {
                ciphers = s.split_whitespace().collect();
            }
        }
        match row.split('|').nth(1) {
            None => continue,
            Some(s) => {
                digits = s.split_whitespace().collect();
            }
        }
        if ciphers.len() == 10 {
            println!("solving: {}", row);
            result += solve(ciphers, digits);
        }
    }
    result
}

pub fn simple_code(n: usize) -> bool {
    match n {
        2 | 3 | 4 | 7 => true,
        _ => false,
    }
}

/// Utility to make comparing codes easier
pub fn sort_chars(x: &str) -> String {
    let mut chars = x.chars().collect::<Vec<char>>();
    chars.sort();
    let mut actual = String::new();
    for c in chars {
        actual.push(c);
    }
    actual
}

pub fn remove_char(x: &str, c: char) -> String {
    x.chars().into_iter().filter(|&s| s != c).collect()
}

#[derive(Debug)]
struct PlainText {
    key: HashSet<char>,
    digit: usize,
}
impl PlainText {
    pub fn from(s: &str, digit: usize) -> Self {
        let mut key = HashSet::new();
        for c in s.chars() {
            key.insert(c);
        }
        Self { key, digit }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Code {
    key: HashSet<char>,
}
impl Code {
    pub fn new(s: &str) -> Self {
        let mut key = HashSet::new();
        for c in s.chars() {
            key.insert(c);
        }
        Code { key }
    }
    pub fn len(&self) -> usize {
        self.key.len()
    }
    pub fn union(&self, other: &Self) -> Self {
        let key = self.key.union(&other.key).map(|k| k.clone()).collect();
        Self { key }
    }
}
impl Into<Code> for &str {
    fn into(self) -> Code {
        let mut key = HashSet::new();
        for c in self.chars() {
            key.insert(c);
        }
        Code { key }
    }
}

pub struct Decoder {
    plain_texts: HashMap<usize, PlainText>,
}
impl Decoder {
    pub fn new(cipher_texts: Vec<String>) -> Self {
        let mut plain_texts: HashMap<usize, PlainText> = HashMap::new();
        let mut unused: Vec<String> = vec![];

        // Find 1, 4, 7 and 8
        for s in &cipher_texts {
            match s.len() {
                2 => {
                    // Digit 1 has 2 segments
                    plain_texts.entry(1).or_insert(PlainText::from(s, 1));
                }
                3 => {
                    // Digit 7 has 3 segments
                    plain_texts.entry(7).or_insert(PlainText::from(s, 7));
                }
                4 => {
                    // Digit 4 has 4 segments
                    plain_texts.entry(4).or_insert(PlainText::from(s, 4));
                }
                7 => {
                    // Digit 8 has 7 segments
                    plain_texts.entry(8).or_insert(PlainText::from(s, 8));
                }
                _ => unused.push(s.to_string()),
            }
        }

        // Find 3
        // Take 1 away from length 5 codes and assert length 3
        let one = &plain_texts.get(&1).unwrap();
        let one_key: HashSet<char> = one.key.clone();
        for s in &unused {
            if s.len() == 5 {
                let mut key: HashSet<char> = HashSet::new();
                for c in s.chars() {
                    key.insert(c);
                }
                if key.difference(&one_key).collect::<HashSet<&char>>().len() == 3 {
                    plain_texts.entry(3).or_insert(PlainText::from(&s, 3));
                }
            }
        }

        // Find 9
        // Combine 4 and 7 to closely match 9 shape
        let four = plain_texts.get(&4).unwrap();
        let seven = plain_texts.get(&7).unwrap();
        let four_seven: HashSet<char> = four.key.union(&seven.key).map(|c| c.clone()).collect();
        for s in &unused {
            let mut key: HashSet<char> = HashSet::new();
            for c in s.chars() {
                key.insert(c);
            }
            if (key.len() == 6)
                && (key
                    .difference(&four_seven)
                    .collect::<HashSet<&char>>()
                    .len()
                    == 1)
            {
                plain_texts.entry(9).or_insert(PlainText::from(&s, 9));
            }
        }

        // Find 2 and 5
        let three = &plain_texts.get(&3).unwrap();
        let three_key: HashSet<char> = three.key.clone();

        let nine = &plain_texts.get(&9).unwrap();
        let nine_key: HashSet<char> = nine.key.clone();
        for s in &unused {
            if s.len() == 5 {
                let mut key: HashSet<char> = HashSet::new();
                for c in s.chars() {
                    key.insert(c);
                }

                // Skip 3
                if key == three_key {
                    continue;
                }

                if key.difference(&nine_key).collect::<HashSet<&char>>().len() == 0 {
                    plain_texts.entry(5).or_insert(PlainText::from(&s, 5));
                } else {
                    plain_texts.entry(2).or_insert(PlainText::from(&s, 2));
                }
            }
        }

        // Only 0, 6 left
        for s in &unused {
            if s.len() == 6 {
                let mut key: HashSet<char> = HashSet::new();
                for c in s.chars() {
                    key.insert(c);
                }

                // Skip 9
                if key == nine_key {
                    continue;
                }

                if key.difference(&one_key).collect::<HashSet<&char>>().len() == 4 {
                    plain_texts.entry(0).or_insert(PlainText::from(&s, 0));
                } else {
                    plain_texts.entry(6).or_insert(PlainText::from(&s, 6));
                }
            }
        }
        for (k, v) in plain_texts.iter() {
            println!(
                "{:?} {:?}",
                k,
                sort_chars(v.key.iter().collect::<String>().as_str())
            );
        }
        Self { plain_texts }
    }
    pub fn decode(&self, candidate: Code) -> Option<usize> {
        self.plain_texts
            .iter()
            .find(|p| p.1.key == candidate.key)
            .and_then(|p| Some(p.1.digit))
    }
}

pub fn to_decimal(x: Vec<u32>) -> u32 {
    let mut result = 0;
    let mut place: u32 = 0 as u32;
    let base: u32 = 10;
    for d in x.iter().rev() {
        result += d * (base.pow(place));
        place += 1;
    }
    result
}

pub fn solve(ciphers: Vec<&str>, digits: Vec<&str>) -> u32 {
    let decoder = Decoder::new(ciphers.iter().map(|s| s.to_string()).collect());
    let nums = digits
        .into_iter()
        .map(|s| s.into())
        .map(|d| decoder.decode(d))
        .map(|o| o.unwrap())
        .map(|i| i as u32)
        .collect();
    to_decimal(nums)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[test]
    fn test_solve() {
        let samples = vec![
            "acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab",
        ];
        let digits = vec!["cdfeb", "fcadb", "cdfeb", "cdbaf"];
        let actual = solve(samples, digits);
        let expected = 5353;
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case("dab", Some(7))]
    #[case("acedgfb", Some(8))]
    #[case("cefabd", Some(9))]
    #[case("gcdfa", Some(2))]
    #[case("cdfbe", Some(5))]
    #[case("fbcad", Some(3))]
    #[case("cagedb", Some(0))]
    #[case("cdfgeb", Some(6))]
    fn day_eight_part_two(#[case] digit: &str, #[case] expected: Option<usize>) {
        let samples = vec![
            "acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab",
        ]
        .into_iter()
        .map(|s| s.into())
        .collect();
        let decoder = Decoder::new(samples);
        let actual = decoder.decode(digit.into());
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case("fbgca", Some(5))]
    #[case("deagfc", Some(0))]
    #[case("ec", Some(1))]
    #[case("ce", Some(1))]
    fn day_eight_part_two_thread_panic(#[case] digit: &str, #[case] expected: Option<usize>) {
        // egcafb adgecf caebf fdgbaec fbaed cebg facbgd cfe fgabc ec | fbgca deagfc ce ce
        println!("{}", sort_chars(digit));
        let samples = vec![
            "egcafb", "adgecf", "caebf", "fdgbaec", "fbaed", "cebg", "facbgd", "cfe", "fgabc", "ec",
        ]
        .into_iter()
        .map(|s| s.into())
        .collect();
        let decoder = Decoder::new(samples);
        let actual = decoder.decode(digit.into());
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_to_decimal() {
        let nums = vec![5, 3, 5, 3];
        let actual = to_decimal(nums);
        let expected = 5353;
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_hashset_equality() {
        let mut left = HashSet::new();
        left.insert(1);
        let mut right = HashSet::new();
        right.insert(1);
        assert_eq!(left, right);
    }

    #[test]
    fn test_code_equality() {
        assert_eq!(Code::new("ab"), Code::new("ba"))
    }

    #[test]
    fn test_sort_str() {
        assert_eq!(sort_chars("acedgfb"), "abcdefg");
    }

    #[test]
    fn test_remove_char() {
        assert_eq!(remove_char("abcdefg", 'c'), "abdefg");
    }
}
