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

#[derive(Debug, PartialEq)]
pub enum Syntax {
    Corrupt(char),
    InComplete,
    Complete,
}
use Syntax::*;

pub fn parser(text: &str) -> Syntax {
    let mut round: u32 = 0;
    let mut curly: u32 = 0;
    let mut pointy: u32 = 0;
    let mut square: u32 = 0;
    for c in text.chars() {
        match c {
            '(' => round += 1,
            ')' => {
                if round == 0 {
                    return Corrupt(')');
                } else {
                    round -= 1
                }
            }
            '{' => curly += 1,
            '}' => {
                if curly == 0 {
                    return Corrupt('}');
                } else {
                    curly -= 1
                }
            }
            '<' => pointy += 1,
            '>' => {
                if pointy == 0 {
                    return Corrupt('>');
                } else {
                    pointy -= 1
                }
            }
            '[' => square += 1,
            ']' => {
                if square == 0 {
                    return Corrupt(']');
                } else {
                    square -= 1
                }
            }
            _ => (),
        }
    }
    if (round == 0) && (curly == 0) && (pointy == 0) && (square == 0) {
        Complete
    } else {
        InComplete
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

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
    fn corrupt_chunk(#[case] text: &str, #[case] expected: Syntax) {
        assert_eq!(parser(text), expected);
    }

    #[test]
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
