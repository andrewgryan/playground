use std::fmt;
use std::str::FromStr;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Cell {
    index: (usize, usize),
    height: u32,
}
impl Cell {
    pub fn new(i: usize, j: usize, height: u32) -> Self {
        Self {
            index: (i, j),
            height,
        }
    }
}

pub struct CaveMap {
    map: String, // TODO remove this property
    data: Vec<u32>,
    shape: (usize, usize),
}
impl CaveMap {
    /// Find region surrounding low point
    pub fn basin(&self, index: (usize, usize)) -> Vec<Cell> {
        let (i, j) = index;
        println!("{} {}", i, j);
        let mut cells: Vec<Cell> = vec![];
        match self.get(i, j) {
            None => (),
            Some(v) => cells.push(Cell::new(i, j, *v)),
        }
        for cell in self.neighbours(i, j) {
            cells.push(cell);
        }
        cells
    }

    pub fn low_points(self) -> Vec<Cell> {
        let mut points = vec![];
        for neighbourhood in self.into_iter() {
            match neighbourhood.low_point() {
                None => continue,
                Some(point) => points.push(*point),
            }
        }
        points
    }
    pub fn neighbourhood(&self, i: usize, j: usize) -> Option<Neighbourhood> {
        match self.get(i, j) {
            None => None,
            Some(v) => Some(Neighbourhood {
                center: Cell::new(i, j, *v),
                neighbours: self.neighbours(i, j),
            }),
        }
    }
    pub fn neighbours(&self, i: usize, j: usize) -> Vec<Cell> {
        let mut values = vec![];
        if i > 0 {
            match self.get(i - 1, j) {
                None => (),
                Some(v) => values.push(Cell::new(i - 1, j, *v)),
            }
        }
        match self.get(i, j + 1) {
            None => (),
            Some(v) => values.push(Cell::new(i, j + 1, *v)),
        }
        match self.get(i + 1, j) {
            None => (),
            Some(v) => values.push(Cell::new(i + 1, j, *v)),
        }
        if j > 0 {
            match self.get(i, j - 1) {
                None => (),
                Some(v) => values.push(Cell::new(i, j - 1, *v)),
            }
        }
        values
    }

    pub fn get_cell(&self, index: (usize, usize)) -> Option<Cell> {
        let (i, j) = index;
        match self.get(i, j) {
            None => None,
            Some(v) => Some(Cell::new(i, j, *v)),
        }
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
    center: Cell,
    neighbours: Vec<Cell>,
}

impl Neighbourhood {
    pub fn risk_level(&self) -> Option<u32> {
        match self
            .neighbours
            .iter()
            .min_by(|x, y| x.height.cmp(&y.height))
        {
            None => None,
            Some(neighbour) => {
                if &neighbour.height > &self.center.height {
                    Some(self.center.height + 1)
                } else {
                    None
                }
            }
        }
    }
    pub fn low_point(&self) -> Option<&Cell> {
        match self
            .neighbours
            .iter()
            .min_by(|x, y| x.height.cmp(&y.height))
        {
            None => None,
            Some(neighbour) => {
                if &neighbour.height > &self.center.height {
                    Some(&self.center)
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

pub fn part_two(input_file: &str) -> u32 {
    println!("{}", input_file);
    let cave_map: CaveMap = std::fs::read_to_string(input_file)
        .unwrap()
        .parse()
        .unwrap();
    println!("{}", cave_map);
    for point in cave_map.low_points() {
        println!("{:?}", point);
        // println!("{:?}", cave_map.basin(point.index));
    }
    0
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
        let actual: Vec<u32> = cave_map.neighbours(i, j).iter().map(|c| c.height).collect();
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(2, 2, vec![6, 7])]
    #[case(1, 1, vec![2, 4, 6, 7])]
    #[case(0, 0, vec![2, 4])]
    fn cave_map_neighbours_3x3(#[case] i: usize, #[case] j: usize, #[case] mut expected: Vec<u32>) {
        let cave_map: CaveMap = "123\n456\n678\n".parse().unwrap();
        let mut actual: Vec<u32> = cave_map.neighbours(i, j).iter().map(|c| c.height).collect();
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
            center: Cell::new(1, 0, 4),
            neighbours: vec![Cell::new(0, 0, 1), Cell::new(1, 1, 5), Cell::new(2, 0, 6)],
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn neighbourhood_risk_level() {
        let neighbourhood = Neighbourhood {
            center: Cell::new(1, 0, 1),
            neighbours: vec![Cell::new(2, 0, 2), Cell::new(1, 1, 9), Cell::new(0, 0, 9)],
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

    #[rstest]
    #[case(
        "929
         202
         929",
         (1, 1),
        5,
        vec![(0,1), (1,0), (1,1), (1,2), (2,1)]
    )]
    #[case(
    "2199943210
     3987894921
     9856789892
     8767896789
     9899965678", (0, 0), 3, vec![(0,0), (1,0), (0,1)]
    )]
    #[case(
    "2199943210
     3987894921
     9856789892
     8767896789
     9899965678", (0, 9), 9, vec![(0,9), (0,8), (0,7), (0,6)]
    )]
    fn cave_map_basin(
        #[case] text: &str,
        #[case] index: (usize, usize),
        #[case] expected: usize,
        #[case] indices: Vec<(usize, usize)>,
    ) {
        let cave_map: CaveMap = text.parse().unwrap();
        let actual = cave_map.basin(index);

        // Check size
        assert_eq!(actual.len(), expected);

        // Check individual cells
        for index in indices {
            let cell = cave_map.get_cell(index).unwrap();
            assert_eq!(actual.iter().find(|c| c.index == cell.index), Some(&cell));
        }
    }

    #[test]
    fn test_cell() {
        let actual = Cell::new(0, 0, 0);
        assert_eq!(actual.index, (0, 0));
        assert_eq!(actual.height, 0);
    }

    #[test]
    fn test_cell_dedup() {
        let mut cells = vec![Cell::new(0, 0, 0), Cell::new(1, 1, 0), Cell::new(0, 0, 0)];
        cells.sort();
        cells.dedup();
        assert_eq!(cells, vec![Cell::new(0, 0, 0), Cell::new(1, 1, 0)]);
    }

    #[test]
    fn test_cave_map_new() {
        let actual: CaveMap = "123
                               456
                               789"
        .parse()
        .unwrap();
        assert_eq!(actual.shape, (3, 3));
        assert_eq!(actual.get(1, 2), Some(&6));
    }
}
