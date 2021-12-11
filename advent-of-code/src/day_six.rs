use std::fs;

#[derive(Clone)]
pub struct Lanternfish {
    pots: [i64; 9],
}

impl Lanternfish {
    pub fn new(ages: Vec<usize>) -> Self {
        // Gather lanternfish by age
        let mut pots: [i64; 9] = [0; 9];
        for age in &ages {
            pots[*age] += 1;
        }
        Self { pots }
    }
    pub fn generation(&self) -> Self {
        // Apply lanternfish reproduction rules
        // [0, 1, 2, 3, 4, 5, 6, 7, 8]
        //  0 -> 6, 8
        //  1 -> 0
        //  2 -> 1
        //  3 -> 2
        //  4 -> 3
        //  5 -> 4
        //  6 -> 5
        //  7 -> 6
        //  8 -> 7
        let mut next_pots: [i64; 9] = [0; 9];
        for i in 0..9 {
            match i {
                0 => {
                    next_pots[6] += self.pots[0];
                    next_pots[8] += self.pots[0];
                }
                _ => {
                    next_pots[i - 1] += self.pots[i];
                }
            }
        }
        Self { pots: next_pots }
    }

    pub fn after(&self, days: i64) -> i64 {
        let mut lanternfish = self.clone();
        for _ in 0..days {
            lanternfish = lanternfish.generation();
        }

        // Sum the number of lanternfish
        let mut count = 0;
        for i in &lanternfish.pots {
            count += i;
        }
        count
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lanternfish_example_1_day() {
        let actual = Lanternfish::new(vec![3, 4, 3, 1, 2]);
        assert_eq!(actual.after(1), 5);
    }

    #[test]
    fn lanternfish_example_2_day() {
        let actual = Lanternfish::new(vec![3, 4, 3, 1, 2]);
        assert_eq!(actual.after(2), 6);
    }

    #[test]
    fn lanternfish_example_18_days() {
        let actual = Lanternfish::new(vec![3, 4, 3, 1, 2]);
        assert_eq!(actual.after(18), 26);
    }
    #[test]
    fn lanternfish_example_80_days() {
        let actual = Lanternfish::new(vec![3, 4, 3, 1, 2]);
        assert_eq!(actual.after(80), 5934);
    }
    #[test]
    fn lanternfish_example_256_days() {
        let actual = Lanternfish::new(vec![3, 4, 3, 1, 2]);
        assert_eq!(actual.after(256), 26984457539);
    }
}

pub fn part_one(input_file: &str) -> i64 {
    // Read data
    let content = fs::read_to_string(input_file).unwrap();
    let row = content.split('\n').next().unwrap();
    let ages: Vec<usize> = row.split(',').map(|s| s.parse().unwrap()).collect();

    let lanternfish = Lanternfish::new(ages);
    lanternfish.after(256) // 80 for part one
}
