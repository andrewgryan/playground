use ndarray::Array2;
use std::convert::{TryFrom, TryInto};
use std::ops::{Add, AddAssign};

pub fn part_one(puzzle_input: &str) -> u32 {
    let grid = to_octopus_array(puzzle_input, (10, 10));
    println!("{:?}", grid);
    0
}

pub fn turn(array: &Array2<Octopus>) -> Array2<Octopus> {
    let mut copy = array.to_owned();
    turn_mut(&mut copy);
    copy
}

pub fn turn_mut(array: &mut Array2<Octopus>) {
    // Increase energy levels
    array.map_mut(|octopus| *octopus += 1);

    // Find charged octopuses
    let mut indices = find_charged(&array);

    while indices.len() > 0 {
        // Flash charged octopuses
        for index in indices.clone() {
            flash_mut([index.0, index.1], array);
        }

        // Find next batch of octopuses
        indices = find_charged(&array);
    }

    // Reset the spent octopus charges
    array.map_mut(|octopus| octopus.reset_charge());
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

/// Find charged Octopus locations
pub fn find_charged(array: &Array2<Octopus>) -> Vec<(usize, usize)> {
    array
        .indexed_iter()
        .filter(|(_, octopus)| octopus.is_charged())
        .map(|(index, _)| index)
        .collect()
}

/// Flash Octopus return modified copy of array
pub fn flash(index: [usize; 2], array: &Array2<Octopus>) -> Array2<Octopus> {
    let mut copy = array.to_owned();
    flash_mut(index, &mut copy);
    copy
}

/// Flash Octopus array in-place
pub fn flash_mut(index: [usize; 2], array: &mut Array2<Octopus>) {
    let [i, j] = index;

    let octopus = &mut array[[i, j]];
    if !octopus.is_spent() {
        // Flash octopus
        octopus.flash();

        let mut indices: Vec<[usize; 2]> = vec![];

        // Corners
        if i > 0 && j > 0 {
            indices.push([i - 1, j - 1]);
        }
        if i > 0 {
            indices.push([i - 1, j + 1]);
        }
        indices.push([i + 1, j + 1]);
        if j > 0 {
            indices.push([i + 1, j - 1]);
        }

        // Cross
        indices.push([i, j + 1]);
        if j > 0 {
            indices.push([i, j - 1]);
        }
        indices.push([i + 1, j]);
        if i > 0 {
            indices.push([i - 1, j]);
        }

        // Apply += 1 to Octopuses at neighbour indices
        indices
            .iter()
            .for_each(|idx| array.get_mut(*idx).into_iter().for_each(|o| *o += 1));
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Octopus {
    energy_level: u32,
    spent: bool,
}

impl Octopus {
    pub fn new(energy_level: u32) -> Self {
        Self {
            energy_level,
            ..Default::default()
        }
    }

    pub fn is_charged(&self) -> bool {
        !self.spent && self.energy_level > 9
    }

    pub fn is_spent(&self) -> bool {
        self.spent
    }

    pub fn flash(&mut self) {
        self.spent = true
    }

    pub fn reset_charge(&mut self) {
        // Reset charge state
        if self.is_spent() {
            self.energy_level = 0;
        }
        // Reset spent state
        self.reset();
    }

    pub fn reset(&mut self) {
        self.spent = false
    }
}

impl AddAssign<u32> for Octopus {
    fn add_assign(&mut self, other: u32) {
        self.energy_level += other;
    }
}
impl Add<u32> for Octopus {
    type Output = Self;
    fn add(self, other: u32) -> Self::Output {
        Self {
            energy_level: self.energy_level + other,
            ..Default::default()
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
    use rstest::*;

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

    #[rstest]
    #[case("00000
            00000
            00000
            00000
            00000",
            [1, 2],
           "01110
            01010
            01110
            00000
            00000")]
    #[case("00000
            00000
            00000
            00000
            00000",
            [0, 0],
           "01000
            11000
            00000
            00000
            00000")]
    #[case("00000
            00000
            00000
            00000
            00000",
            [1, 0],
           "11000
            01000
            11000
            00000
            00000")]
    #[case("00000
            00000
            00000
            00000
            00000",
            [4, 4],
           "00000
            00000
            00000
            00011
            00010")]
    fn octopus_flash(#[case] start: &str, #[case] index: [usize; 2], #[case] end: &str) {
        let array: Array2<Octopus> = to_octopus_array(start, (5, 5));
        let actual = flash(index, &array);
        let mut expected: Array2<Octopus> = to_octopus_array(end, (5, 5));
        expected[index].flash();
        assert_eq!(actual, expected);
    }

    #[test]
    fn octopus_flash_multiple_times_without_reset() {
        let start: &str = "00000
                           00000
                           00000
                           00000
                           00000";
        let mut actual: Array2<Octopus> = to_octopus_array(start, (5, 5));
        let [i, j] = [1, 2];

        // in-place mutable flash
        flash_mut([i, j], &mut actual);
        flash_mut([i, j], &mut actual);
        flash_mut([i, j], &mut actual);
        flash_mut([i, j], &mut actual);

        let end: &str = "01110
                         01010
                         01110
                         00000
                         00000";
        let mut expected: Array2<Octopus> = to_octopus_array(end, (5, 5));
        expected[[i, j]].flash();
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

    #[test]
    fn octopus_flash_internal_state() {
        let mut octopus: Octopus = Octopus::new(0);
        assert!(!octopus.is_spent());
        octopus.flash();
        assert!(octopus.is_spent());
    }

    #[test]
    fn octopus_reset_internal_state() {
        let mut octopus: Octopus = Octopus::new(0);
        octopus.flash();
        octopus.reset();
        assert!(!octopus.is_spent());
    }

    #[test]
    fn find_charged_indices() {
        let text: &str = "11111
                          11911
                          11111
                          11111
                          11111";
        let mut grid: Array2<Octopus> = to_octopus_array(text, (5, 5));
        grid = grid + 1;
        let actual = find_charged(&grid);
        let expected = vec![(1, 2)];
        assert_eq!(actual, expected);
        assert_eq!(grid[(1, 2)], Octopus::new(10));
    }

    #[rstest]
    #[case("11111
            19991
            19191
            19991
            11111",
            "34543
             40004
             50005
             40004
             34543", (5,5), 1)]
    #[case("11111
            19991
            19191
            19991
            11111",
            "45654
             51115
             61116
             51115
             45654", (5,5), 2)]
    #[case("5483143223
            2745854711
            5264556173
            6141336146
            6357385478
            4167524645
            2176841721
            6882881134
            4846848554
            5283751526",
            "6594254334
             3856965822
             6375667284
             7252447257
             7468496589
             5278635756
             3287952832
             7993992245
             5957959665
             6394862637", (10,10), 1)]
    #[case("5483143223
            2745854711
            5264556173
            6141336146
            6357385478
            4167524645
            2176841721
            6882881134
            4846848554
            5283751526",
            "8807476555
             5089087054
             8597889608
             8485769600
             8700908800
             6600088989
             6800005943
             0000007456
             9000000876
             8700006848", (10,10), 2)]
    #[case("5483143223
            2745854711
            5264556173
            6141336146
            6357385478
            4167524645
            2176841721
            6882881134
            4846848554
            5283751526",
            "0397666866
             0749766918
             0053976933
             0004297822
             0004229892
             0053222877
             0532222966
             9322228966
             7922286866
             6789998766", (10,10), 100)]
    fn play_turns(
        #[case] text: &str,
        #[case] end: &str,
        #[case] shape: (usize, usize),
        #[case] n: u32,
    ) {
        let mut actual: Array2<Octopus> = to_octopus_array(text, shape);
        for _ in 0..n {
            turn_mut(&mut actual);
        }
        let expected: Array2<Octopus> = to_octopus_array(end, shape);
        assert_eq!(actual, expected);
    }
}
