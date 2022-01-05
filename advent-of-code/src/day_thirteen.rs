use std::str::FromStr;

pub fn part_one(puzzle_input: &str) -> u32 {
    // Parse dots
    let dots: Vec<Dot> = puzzle_input
        .split('\n')
        .map(|l| l.parse())
        .filter(|r| r.is_ok())
        .map(|r| r.unwrap())
        .collect();
    println!("{:?}", dots);

    // Parse folds
    let folds: Vec<Fold> = puzzle_input
        .split('\n')
        .map(|l| l.parse())
        .filter(|r| r.is_ok())
        .map(|r| r.unwrap())
        .collect();
    println!("{:?}", folds);

    0
}

pub fn shape(_: Vec<(u32, u32)>) -> (u32, u32) {
    (0, 0)
}

#[derive(Debug, PartialEq)]
pub struct Dot(u32, u32);

impl FromStr for Dot {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains(',') {
            let mut values = s.split(',').take(2).map(|c| c.parse::<u32>().unwrap());
            let a: u32 = values.next().unwrap();
            let b: u32 = values.next().unwrap();
            Ok(Self(a, b))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Fold {
    X(u32),
    Y(u32),
}
impl FromStr for Fold {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains("fold along") {
            let n: u32 = s.split("=").nth(1).map(|c| c.parse().unwrap()).unwrap();
            if s.contains("x") {
                Ok(Fold::X(n))
            } else {
                Ok(Fold::Y(n))
            }
        } else {
            Err(())
        }
    }
}

pub fn apply(fold: Fold, dot: Dot) -> Dot {
    match fold {
        Fold::Y(n) => Dot(dot.0, reflect(dot.1, n)),
        _ => dot,
    }
}

fn reflect(v: u32, about: u32) -> u32 {
    if v >= about {
        let d = v - about;
        (about - d) - 1
    } else {
        v
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case("6,10", Ok(Dot(6, 10)))]
    #[case("", Err(()))]
    fn parse_dot(#[case] s: &str, #[case] expected: Result<Dot, <Dot as FromStr>::Err>) {
        let actual = s.parse();
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case("fold along y=7", Ok(Fold::Y(7)))]
    #[case("fold along x=5", Ok(Fold::X(5)))]
    #[case("fold along x=123", Ok(Fold::X(123)))]
    #[case("", Err(()))]
    fn parse_fold_instruction(
        #[case] s: &str,
        #[case] expected: Result<Fold, <Fold as FromStr>::Err>,
    ) {
        let actual = s.parse();
        assert_eq!(actual, expected);
    }

    #[test]
    fn shape_transparent_dots() {
        let dots = vec![];
        let actual = shape(dots);
        let expected = (0, 0);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(Fold::Y(7), Dot(0, 0), Dot(0, 0))]
    #[case(Fold::Y(7), Dot(0, 7), Dot(0, 6))]
    #[case(Fold::Y(7), Dot(0, 8), Dot(0, 5))]
    #[case(Fold::Y(7), Dot(0, 13), Dot(0, 0))]
    #[case(Fold::Y(7), Dot(0, 12), Dot(0, 1))]
    fn apply_fold_to_dot(#[case] fold: Fold, #[case] dot: Dot, #[case] expected: Dot) {
        let actual = apply(fold, dot);
        assert_eq!(actual, expected);
    }
}
