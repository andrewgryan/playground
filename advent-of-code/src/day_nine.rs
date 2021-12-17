use std::fmt;
use std::str::FromStr;

pub struct CaveMap {
    map: String,
    data: Vec<u32>,
    shape: (usize, usize),
}
impl CaveMap {
    pub fn neighbourhood(&self, i: usize, j: usize) -> Option<Neighbourhood> {
        match self.get(i, j) {
            None => None,
            Some(v) => Some(Neighbourhood {
                value: *v,
                neighbours: self.neighbours(i, j),
            }),
        }
    }
    pub fn neighbours(&self, i: usize, j: usize) -> Vec<u32> {
        let mut values = vec![];
        if i > 0 {
            match self.get(i - 1, j) {
                None => (),
                Some(v) => values.push(*v),
            }
        }
        match self.get(i, j + 1) {
            None => (),
            Some(v) => values.push(*v),
        }
        match self.get(i + 1, j) {
            None => (),
            Some(v) => values.push(*v),
        }
        if j > 0 {
            match self.get(i, j - 1) {
                None => (),
                Some(v) => values.push(*v),
            }
        }
        values
    }

    pub fn get(&self, i: usize, j: usize) -> Option<&u32> {
        if &j >= &self.shape.0 {
            None
        } else {
            self.data.get(i * &self.shape.1 + j)
        }
    }
}

impl IntoIterator for CaveMap {
    type Item = Neighbourhood;
    type IntoIter = CaveMapIterator;
    fn into_iter(self) -> Self::IntoIter {
        CaveMapIterator {
            k: 0,
            cave_map: self,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Neighbourhood {
    value: u32,
    neighbours: Vec<u32>,
}

impl Neighbourhood {
    pub fn risk_level(&self) -> Option<u32> {
        match self.neighbours.iter().min() {
            None => None,
            Some(v) => {
                if v > &self.value {
                    Some(self.value + 1)
                } else {
                    None
                }
            }
        }
    }
}

pub struct CaveMapIterator {
    k: usize,
    cave_map: CaveMap,
}

impl Iterator for CaveMapIterator {
    type Item = Neighbourhood;

    fn next(&mut self) -> Option<Self::Item> {
        let shape = self.cave_map.shape;
        let j = self.k % shape.0;
        let i = (self.k - j) / shape.0;
        let result;
        if self.k < (shape.0 * shape.1) {
            result = self.cave_map.neighbourhood(i, j);
        } else {
            result = None;
        }
        self.k += 1;
        result
    }
}

impl FromStr for CaveMap {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rows = s
            .split('\n')
            .filter(|s| s.len() != 0)
            .collect::<Vec<&str>>()
            .len();
        let cols = s.split('\n').nth(0).unwrap().len();

        let mut data: Vec<u32> = vec![];
        for row in s.split('\n') {
            if s.len() == 0 {
                continue;
            }
            for c in row.chars() {
                match c.to_digit(10) {
                    None => continue,
                    Some(i) => data.push(i),
                }
            }
        }

        Ok(CaveMap {
            map: s.to_string(),
            data,
            shape: (rows, cols),
        })
    }
}

impl fmt::Display for CaveMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Map of cave: {:?}\n{}", self.shape, self.map)
    }
}

pub fn part_one(input_file: &str) -> u32 {
    println!("{}", input_file);
    let cave_map: CaveMap = std::fs::read_to_string(input_file)
        .unwrap()
        .parse()
        .unwrap();
    println!("{}", cave_map);
    let mut result = 0;
    for neighbourhood in cave_map.into_iter() {
        match neighbourhood.risk_level() {
            None => continue,
            Some(level) => result += level,
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[test]
    fn cave_map_shape() {
        let cave_map: CaveMap = "12\n34".parse().unwrap();
        assert_eq!(cave_map.shape, (2, 2));
    }

    #[rstest]
    #[case(0, 0, vec![2, 3])]
    #[case(1, 0, vec![1, 4])]
    #[case(1, 1, vec![2, 3])]
    fn cave_map_neighbours_2x2(#[case] i: usize, #[case] j: usize, #[case] expected: Vec<u32>) {
        let cave_map: CaveMap = "12\n34".parse().unwrap();
        let actual = cave_map.neighbours(i, j);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(2, 2, vec![6, 7])]
    #[case(1, 1, vec![2, 4, 6, 7])]
    #[case(0, 0, vec![2, 4])]
    fn cave_map_neighbours_3x3(#[case] i: usize, #[case] j: usize, #[case] mut expected: Vec<u32>) {
        let cave_map: CaveMap = "123\n456\n678\n".parse().unwrap();
        let mut actual = cave_map.neighbours(i, j);
        actual.sort();
        expected.sort();
        assert_eq!(actual, expected);
    }

    #[test]
    fn cave_map_into_iter_neighbourhood() {
        let cave_map: CaveMap = "123\n456\n678\n".parse().unwrap();
        let mut iter = cave_map.into_iter();
        let actual = iter.nth(3);
        let expected = Some(Neighbourhood {
            value: 4,
            neighbours: vec![1, 5, 6],
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn neighbourhood_risk_level() {
        let neighbourhood = Neighbourhood {
            value: 1,
            neighbours: vec![2, 9, 9],
        };
        let actual = neighbourhood.risk_level();
        let expected = Some(2);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(0, 0, Some(&1))]
    #[case(2, 0, None)]
    #[case(0, 2, None)]
    fn cave_map_get(#[case] i: usize, #[case] j: usize, #[case] expected: Option<&u32>) {
        let cave_map: CaveMap = "12\n34".parse().unwrap();
        assert_eq!(cave_map.get(i, j), expected);
    }
}
