/// Advent of Code - Day 10
use std::collections::HashMap;

pub fn part_one(input_file: &str) -> u32 {
    let content = std::fs::read_to_string(input_file).unwrap();
    score(content.as_str())
}

pub fn score(content: &str) -> u32 {
    let mut points: HashMap<u32, u32> = HashMap::new();
    for line in content.split('\n') {
        match parser(line) {
            Corrupt(c) => match c {
                ')' => {
                    let v = points.entry(3).or_insert(0);
                    *v += 1;
                }
                ']' => {
                    let v = points.entry(57).or_insert(0);
                    *v += 1;
                }
                '}' => {
                    let v = points.entry(1197).or_insert(0);
                    *v += 1;
                }
                '>' => {
                    let v = points.entry(25137).or_insert(0);
                    *v += 1;
                }
                _ => (),
            },
            _ => (),
        }
    }
    let mut result: u32 = 0;
    for (value, count) in points {
        println!("{} {}", value, count);
        result += value * count;
    }
    result
}

pub fn parser(_line: &str) -> Syntax {
    InComplete
}

#[derive(Debug, PartialEq)]
pub enum Syntax {
    Corrupt(char),
    InComplete,
    Complete,
}
use Syntax::*;

#[derive(Debug, PartialEq)]
pub enum Token {
    Open(char),
    Close(char),
    Skip,
}
impl From<char> for Token {
    fn from(c: char) -> Self {
        match c {
            '{' | '(' | '[' | '<' => Self::Open(c),
            '}' | ')' | ']' | '>' => Self::Close(c),
            _ => Self::Skip,
        }
    }
}

pub struct Chunk {
    children: Vec<Chunk>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case("{", Token::Open('{'))]
    #[case("(", Token::Open('('))]
    #[case("[", Token::Open('['))]
    #[case("<", Token::Open('<'))]
    #[case("}", Token::Close('}'))]
    #[case(")", Token::Close(')'))]
    #[case("]", Token::Close(']'))]
    #[case(">", Token::Close('>'))]
    fn test_token(#[case] c: char, #[case] expected: Token) {
        let actual = Token::from(c);
        assert_eq!(actual, expected);
    }

    fn test_chunk() {
        let mut root = Chunk { children: vec![] };
        root.children.push(Chunk { children: vec![] });
    }

    #[rstest]
    #[case("[)", Corrupt(')'))]
    #[case("(]", Corrupt(']'))]
    #[case("()]", Corrupt(']'))]
    #[case("()", Complete)]
    #[case("(())", Complete)]
    #[case("{()()()}", Complete)]
    #[case("{()()()>", Corrupt('>'))]
    #[case("(((())))>", Corrupt('>'))]
    #[case("{([(<{}[<>[]}>{[]{[(<()>", Corrupt('}'))]
    #[ignore]
    fn corrupt_chunk(#[case] text: &str, #[case] expected: Syntax) {
        assert_eq!(parser(text), expected);
    }

    #[test]
    #[ignore]
    fn compute_score() {
        let content = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]";
        assert_eq!(score(content), 26397);
    }
}
