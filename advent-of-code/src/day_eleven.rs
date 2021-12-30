use ndarray::Array2;
use std::convert::{TryFrom, TryInto};
use std::ops::{Add, AddAssign};

pub fn part_one(puzzle_input: &str) -> u32 {
    let grid = to_octopus_array(puzzle_input, (10, 10));
    println!("{:?}", grid);
    0
}

fn to_octopus_array(puzzle_input: &str, shape: (usize, usize)) -> Array2<Octopus> {
    let values: Vec<Octopus> = puzzle_input
        .split('\n')
        .map(|l| {
            l.chars()
                .filter(|c| c != &' ')
                .map(Octopus::try_from)
                .map(|o| o.unwrap())
        })
        .flatten()
        .collect();
    Array2::from_shape_vec(shape, values).unwrap()
}

pub fn flash(index: [usize; 2], array: &Array2<Octopus>) -> Array2<Octopus> {
    let [i, j] = index;
    let mut copy = array.to_owned();
    // Corners
    copy[[i - 1, j - 1]] += 1;
    copy[[i - 1, j + 1]] += 1;
    copy[[i + 1, j + 1]] += 1;
    copy[[i + 1, j - 1]] += 1;

    // Cross
    copy[[i, j + 1]] += 1;
    copy[[i, j - 1]] += 1;
    copy[[i + 1, j]] += 1;
    copy[[i - 1, j]] += 1;

    copy
}

#[derive(Clone, Debug, PartialEq)]
pub struct Octopus {
    energy_level: u32,
}

impl Octopus {
    pub fn new(energy_level: u32) -> Self {
        Self { energy_level }
    }
}

impl AddAssign<u32> for Octopus {
    fn add_assign(&mut self, other: u32) {
        self.energy_level += 1;
    }
}
impl Add<u32> for Octopus {
    type Output = Self;
    fn add(self, other: u32) -> Self::Output {
        Self {
            energy_level: self.energy_level + 1,
        }
    }
}

impl TryFrom<char> for Octopus {
    type Error = ();

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c.to_digit(10) {
            Some(n) => Ok(Octopus::new(n)),
            None => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn octopus_try_from() {
        let actual: Result<Octopus, _> = '9'.try_into();
        assert_eq!(actual, Ok(Octopus::new(9)));
    }

    #[test]
    fn octopus_grid_indexing() {
        let puzzle_input: &str = "01234
                                  00000
                                  00000
                                  00000
                                  56789";
        let actual: Array2<Octopus> = to_octopus_array(puzzle_input, (5, 5));

        // Big-endian indexing by default, e.g. C index
        assert_eq!(actual[[0, 0]], Octopus::new(0));
        assert_eq!(actual[[0, 1]], Octopus::new(1));
        assert_eq!(actual[[0, 2]], Octopus::new(2));
        assert_eq!(actual[[0, 3]], Octopus::new(3));
        assert_eq!(actual[[0, 4]], Octopus::new(4));
        assert_eq!(actual[[4, 0]], Octopus::new(5));
        assert_eq!(actual[[4, 4]], Octopus::new(9));
    }

    #[test]
    fn octopus_flash() {
        let start: &str = "00000
                           00000
                           00000
                           00000
                           00000";
        let array: Array2<Octopus> = to_octopus_array(start, (5, 5));

        let actual = flash([1, 2], &array);

        let end: &str = "01110
                         01010
                         01110
                         00000
                         00000";
        let expected: Array2<Octopus> = to_octopus_array(end, (5, 5));
        assert_eq!(actual, expected);
    }

    #[test]
    fn octopus_add_one() {
        let start: &str = "00000
                           00000
                           00000
                           00000
                           00000";
        let array: Array2<Octopus> = to_octopus_array(start, (5, 5));

        let mut actual = array.to_owned();
        actual = actual + 1;

        let end: &str = "11111
                         11111
                         11111
                         11111
                         11111";
        let expected: Array2<Octopus> = to_octopus_array(end, (5, 5));
        assert_eq!(actual, expected);
    }
}
