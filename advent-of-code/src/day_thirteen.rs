use std::collections::HashSet;
use std::fmt;
use std::fmt::write;
use std::str::FromStr;

pub fn part_one(puzzle_input: &str) -> usize {
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

    let fold = folds.iter().next().unwrap();
    println!("{:?}", fold);
    let mut set = HashSet::new();
    for dot in dots {
        set.insert(apply(fold, dot));
    }

    println!("{:?}", folds);

    set.len()
}

pub fn part_two(puzzle_input: &str) -> usize {
    // Parse dots
    let mut dots: Vec<Dot> = puzzle_input
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

    for fold in folds {
        let mut set = HashSet::new();
        for dot in dots {
            set.insert(apply(&fold, dot));
        }
        dots = set.iter().map(|d| d.clone()).collect();
    }

    // Display Dots
    let paper: Paper = Paper { dots: dots.clone() };
    println!("{}", paper);

    dots.len()
}

struct Paper {
    dots: Vec<Dot>,
}
impl fmt::Display for Paper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut x = self.dots.iter().map(|d| d.0).max().unwrap();
        let mut y = self.dots.iter().map(|d| d.1).max().unwrap();

        // Off by one error
        x += 1;
        y += 1;

        // Build &str
        let mut s: String = String::new();
        for j in 0..y {
            for i in 0..x {
                if self.dots.contains(&Dot(i, j)) {
                    s.push('#');
                } else {
                    s.push(' ');
                }
            }
            s.push('\n');
        }
        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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

pub fn apply(fold: &Fold, dot: Dot) -> Dot {
    match fold {
        Fold::Y(n) => Dot(dot.0, reflect(dot.1, *n)),
        Fold::X(n) => Dot(reflect(dot.0, *n), dot.1),
    }
}

fn reflect(v: u32, about: u32) -> u32 {
    if v > about {
        let d = v - about;
        about - d
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

    #[rstest]
    #[case(Fold::Y(7), Dot(0, 0), Dot(0, 0))]
    #[case(Fold::Y(7), Dot(0, 1), Dot(0, 1))]
    #[case(Fold::Y(7), Dot(0, 2), Dot(0, 2))]
    #[case(Fold::Y(7), Dot(0, 3), Dot(0, 3))]
    #[case(Fold::Y(7), Dot(0, 4), Dot(0, 4))]
    #[case(Fold::Y(7), Dot(0, 5), Dot(0, 5))]
    #[case(Fold::Y(7), Dot(0, 6), Dot(0, 6))]
    #[case(Fold::Y(7), Dot(0, 7), Dot(0, 7))]
    #[case(Fold::Y(7), Dot(0, 8), Dot(0, 6))]
    #[case(Fold::Y(7), Dot(0, 9), Dot(0, 5))]
    #[case(Fold::Y(7), Dot(0, 10), Dot(0, 4))]
    #[case(Fold::Y(7), Dot(0, 11), Dot(0, 3))]
    #[case(Fold::Y(7), Dot(0, 12), Dot(0, 2))]
    #[case(Fold::Y(7), Dot(0, 13), Dot(0, 1))]
    #[case(Fold::Y(7), Dot(0, 14), Dot(0, 0))]
    #[case(Fold::X(7), Dot(0, 0), Dot(0, 0))]
    #[case(Fold::X(7), Dot(1, 0), Dot(1, 0))]
    #[case(Fold::X(7), Dot(2, 0), Dot(2, 0))]
    #[case(Fold::X(7), Dot(3, 0), Dot(3, 0))]
    #[case(Fold::X(7), Dot(4, 0), Dot(4, 0))]
    #[case(Fold::X(7), Dot(5, 0), Dot(5, 0))]
    #[case(Fold::X(7), Dot(6, 0), Dot(6, 0))]
    #[case(Fold::X(7), Dot(7, 0), Dot(7, 0))]
    #[case(Fold::X(7), Dot(8, 0), Dot(6, 0))]
    #[case(Fold::X(7), Dot(9, 0), Dot(5, 0))]
    #[case(Fold::X(7), Dot(10, 0), Dot(4, 0))]
    #[case(Fold::X(7), Dot(11, 0), Dot(3, 0))]
    #[case(Fold::X(7), Dot(12, 0), Dot(2, 0))]
    #[case(Fold::X(7), Dot(13, 0), Dot(1, 0))]
    #[case(Fold::X(7), Dot(14, 0), Dot(0, 0))]
    fn apply_fold_to_dot(#[case] fold: Fold, #[case] dot: Dot, #[case] expected: Dot) {
        let actual = apply(&fold, dot);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_part_one() {
        let puzzle = "
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5";
        assert_eq!(part_one(puzzle), 17);
    }
}
