use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
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

#[derive(Clone)]
pub struct CaveMap {
    map: String, // TODO remove this property
    data: Vec<u32>,
    shape: (usize, usize),
}
impl CaveMap {
    /// Find region surrounding low point
    pub fn basin(&self, index: (usize, usize)) -> HashSet<Cell> {
        // Algorithm
        // 1. Set border to cell(index) and interior to empty set
        // 2. Collect neighbourhood of border
        // 3. Remove duplicate neighbours
        // 4. Remove interior
        // 5. Remove peaks (e.g. value == 9)
        // 6. Repeat steps 2 through 5

        let mut interior: HashSet<Cell> = HashSet::new();
        let mut border: HashSet<Cell> = HashSet::new();
        let mut neighbours: HashSet<Cell> = HashSet::new();

        // Initialize border
        border.insert(self.get_cell(index).unwrap());

        while border.len() > 0 {
            // Collect unique neighbours
            neighbours.clear();
            for cell in border.clone() {
                for neighbour in self.neighbours(cell.index.0, cell.index.1) {
                    neighbours.insert(neighbour);
                }
            }

            // Remove interior
            for cell in neighbours.clone() {
                if interior.contains(&cell) {
                    neighbours.remove(&cell);
                }
            }

            // Remove peaks
            for cell in neighbours.clone() {
                if cell.height == 9 {
                    neighbours.remove(&cell);
                }
            }

            // Add border to interior for next iteration
            for cell in border.clone() {
                interior.insert(cell);
            }

            // Set neighbours as border for next iteration
            border.clear();
            for cell in neighbours.clone() {
                border.insert(cell);
            }
        }

        interior
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
        if &j >= &self.shape.1 {
            None
        } else if &i >= &self.shape.0 {
            None
        } else {
            self.data.get((j * &self.shape.0) + i)
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
        let i = self.k % shape.1;
        let j = (self.k - i) / shape.1;
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
            shape: (cols, rows),
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

pub fn part_two(input_file: &str) -> usize {
    let cave_map: CaveMap = std::fs::read_to_string(input_file)
        .unwrap()
        .parse()
        .unwrap();
    let mut sizes: Vec<usize> = vec![];
    for point in cave_map.clone().low_points() {
        let size = cave_map.basin(point.index).len();
        sizes.push(size);
    }
    sizes.sort();
    let i: usize = sizes.len() - 3;
    let result = &sizes[i..].iter().product::<usize>();
    *result
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
    #[case(0, 0, vec![3, 2])]
    #[case(1, 0, vec![1, 4])]
    #[case(1, 1, vec![3, 2])]
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
        let cave_map: CaveMap = "123
                                 456
                                 678"
        .parse()
        .unwrap();
        let mut iter = cave_map.into_iter();
        let actual = iter.nth(3);
        let expected = Some(Neighbourhood {
            center: Cell::new(0, 1, 4),
            neighbours: vec![Cell::new(0, 2, 6), Cell::new(1, 1, 5), Cell::new(0, 0, 1)],
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
        "999
         909
         999",
         (1, 1),
        1,
        vec![(1,1)]
    )]
    #[case(
        "99999
         90909
         99999",
         (3, 1),
        1,
        vec![(3,1)]
    )]
    #[case(
        "999999
         911019
         999999",
         (4, 1),
        4,
        vec![(4,1), (3,1), (2,1), (1,1)]
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
     9899965678", (9, 0), 9, vec![(9,0), (8,0), (7,0), (6,0)]
    )]
    fn cave_map_basin(
        #[case] text: &str,
        #[case] index: (usize, usize),
        #[case] expected: usize,
        #[case] indices: Vec<(usize, usize)>,
    ) {
        let cave_map: CaveMap = text.parse().unwrap();
        let actual = cave_map.basin(index);

        println!("actual: {:?}", actual);

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
    fn test_cell_hashset() {
        let mut cells: HashSet<Cell> = HashSet::new();
        cells.insert(Cell::new(0, 0, 0));
        cells.insert(Cell::new(1, 1, 0));
        cells.insert(Cell::new(0, 0, 0));
        assert_eq!(cells.len(), 2);
    }

    #[rstest]
    #[case(
    "2199943210
     3987894921
     9856789892
     8767896789
     9899965678", (9, 0), (10, 5), Some(&0)
    )]
    #[case(
    "2199943210
     3987894921
     9856789892
     8767896789
     9899965678", (0, 4), (10, 5), Some(&9)
    )]
    fn test_cave_map_new(
        #[case] text: &str,
        #[case] index: (usize, usize),
        #[case] shape: (usize, usize),
        #[case] value: Option<&u32>,
    ) {
        let actual: CaveMap = text.parse().unwrap();
        let (i, j) = index;
        assert_eq!(actual.shape, shape);
        assert_eq!(actual.get(i, j), value);
    }
}
