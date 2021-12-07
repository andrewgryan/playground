use std::fs;
use std::str::FromStr;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Point {
    x: i32,
    y: i32,
}
impl Point {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}
impl FromStr for Point {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let values: Vec<Result<i32, _>> = s.split(',').take(2).map(|s| s.parse()).collect();
        match (&values.get(0), &values.get(1)) {
            (Some(Ok(x)), Some(Ok(y))) => Ok(Point::new(*x, *y)),
            _ => Err("failed to parse".to_string()),
        }
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct Line {
    start: Point,
    end: Point,
}
impl Line {
    pub fn new(start: Point, end: Point) -> Self {
        Self { start, end }
    }
    pub fn from_coords(x1: i32, y1: i32, x2: i32, y2: i32) -> Self {
        Self {
            start: Point::new(x1, y1),
            end: Point::new(x2, y2),
        }
    }
}

impl FromStr for Line {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let values: Vec<Result<Point, _>> = s.split(" -> ").take(2).map(|s| s.parse()).collect();
        match (&values.get(0), &values.get(1)) {
            (Some(Ok(start)), Some(Ok(end))) => Ok(Self::new(start.clone(), end.clone())),
            _ => Err("failed to parse".to_string()),
        }
    }
}

pub fn part_one(input_file: &str) -> i32 {
    let contents = fs::read_to_string(input_file).expect("could not open file");
    for text in contents.split('\n') {
        match text.parse::<Line>() {
            Ok(line) => println!("{:?}", line),
            Err(_) => (),
        }
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_point_from_str() {
        let actual: Point = "1,2".parse().unwrap();
        assert_eq!(actual.x, 1);
        assert_eq!(actual.y, 2);
    }

    #[test]
    fn test_line_from_str() {
        let actual: Line = "1,2 -> 3,4".parse().unwrap();
        assert_eq!(actual, Line::from_coords(1, 2, 3, 4));
    }
}
