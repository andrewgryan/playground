use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::str::FromStr;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, Default, PartialEq)]
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
    fn iter_points(&self) -> HashSet<Point> {
        let mut points = HashSet::new();
        if self.is_horizontal() {
            let start = min(self.start.x, self.end.x);
            let end = max(self.start.x, self.end.x);
            for x in start..(end + 1) {
                points.insert(Point::new(x, self.start.y));
            }
        }
        if self.is_vertical() {
            let start = min(self.start.y, self.end.y);
            let end = max(self.start.y, self.end.y);
            for y in start..(end + 1) {
                points.insert(Point::new(self.start.x, y));
            }
        }
        points
    }
    pub fn is_horizontal(&self) -> bool {
        self.start.y == self.end.y
    }
    pub fn is_vertical(&self) -> bool {
        self.start.x == self.end.x
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

#[derive(Debug)]
pub struct Maze {
    lines: Vec<Line>,
}
impl Maze {
    pub fn new(lines: Vec<Line>) -> Self {
        Self { lines }
    }

    pub fn solve(&self) -> usize {
        let mut counts: HashMap<Point, usize> = HashMap::new();
        for point in self.iter_points() {
            let n = counts.entry(point).or_insert(0);
            *n += 1;
        }

        // Points with 2 or more overlaps
        let mut overlaps: Vec<&Point> = vec![];
        for (key, val) in counts.iter() {
            if val >= &2 {
                overlaps.push(key);
            }
        }
        overlaps.len()
    }

    fn iter_points(&self) -> Vec<Point> {
        self.lines
            .iter()
            .map(|line| line.iter_points())
            .flatten()
            .collect()
    }
}
impl FromStr for Maze {
    type Err = String;
    fn from_str(contents: &str) -> Result<Self, Self::Err> {
        let mut lines = vec![];
        for text in contents.split('\n') {
            match text.parse::<Line>() {
                Ok(line) => lines.push(line),
                Err(_) => continue,
            }
        }
        Ok(Self { lines })
    }
}

pub fn part_one(input_file: &str) -> i32 {
    let full_maze: Maze = fs::read_to_string(input_file)
        .expect("could not open file")
        .parse()
        .unwrap();

    // Only consider horizontal/vertical lines first
    let maze = Maze::new(
        full_maze
            .lines
            .iter()
            .filter(|line| line.is_horizontal() || line.is_vertical())
            .map(|line| line.clone())
            .collect::<Vec<Line>>(),
    );

    maze.solve() as i32
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

    #[test]
    fn test_line_points() {
        let actual = Line::from_coords(1, 1, 1, 3).iter_points();
        let mut expect = HashSet::new();
        expect.insert(Point::new(1, 1));
        expect.insert(Point::new(1, 2));
        expect.insert(Point::new(1, 3));
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_line_points_given_97_to_77() {
        let actual = Line::from_coords(9, 7, 7, 7).iter_points();
        let mut expect = HashSet::new();
        expect.insert(Point::new(9, 7));
        expect.insert(Point::new(8, 7));
        expect.insert(Point::new(7, 7));
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_line_points_given_79_to_77() {
        let actual = Line::from_coords(7, 9, 7, 7).iter_points();
        let mut expect = HashSet::new();
        expect.insert(Point::new(7, 9));
        expect.insert(Point::new(7, 8));
        expect.insert(Point::new(7, 7));
        assert_eq!(actual, expect);
    }
}
