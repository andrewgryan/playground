use std::fs;

pub mod day_eight;
pub mod day_five;
pub mod day_four;
pub mod day_nine;
pub mod day_seven;
pub mod day_six;
pub mod day_ten;
pub mod day_three;
pub mod day_two;
pub mod utils;

/// Part 1
pub fn part_one(depths: Vec<i32>) -> i32 {
    count_increases(depths)
}

/// Part 1
pub fn part_two(depths: Vec<i32>) -> i32 {
    count_increases(three_measure_sum(depths))
}

pub fn count_increases(depths: Vec<i32>) -> i32 {
    let mut previous: Option<i32> = None;
    let mut count: i32 = 0;
    for depth in depths {
        match previous {
            Some(previous_value) => {
                if depth > previous_value {
                    count += 1
                }
            }
            None => (),
        }
        previous = Some(depth);
    }
    count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_two_three_measure_sum() {
        let actual = three_measure_sum(vec![1, 2, 3, 4]);
        let expect = vec![6, 9];
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_part_two_three_measure_sum_more_values() {
        let actual = three_measure_sum(vec![1, 2, 3, 4, 5, 6]);
        let expect = vec![6, 9, 12, 15];
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_part_two_three_measure_sum_less_values() {
        let actual = three_measure_sum(vec![1, 2]);
        let expect = vec![];
        assert_eq!(actual, expect);
    }
}

struct Triple {
    index: usize,
    values: Vec<i32>,
}
impl Triple {
    fn from(values: Vec<i32>) -> Self {
        Self { index: 0, values }
    }
}

impl Iterator for Triple {
    type Item = (i32, i32, i32);

    fn next(&mut self) -> Option<Self::Item> {
        let next_item = match (
            self.values.get(self.index),
            self.values.get(self.index + 1),
            self.values.get(self.index + 2),
        ) {
            (Some(a), Some(b), Some(c)) => Some((*a, *b, *c)),
            _ => None,
        };
        self.index += 1;
        next_item
    }
}

fn three_measure_sum(items: Vec<i32>) -> Vec<i32> {
    let mut result = vec![];
    let triple = Triple::from(items);
    for (a, b, c) in triple {
        result.push(a + b + c)
    }
    result
}

pub fn load_depths(input_file: &str) -> Vec<i32> {
    let contents = fs::read_to_string(input_file).expect("Could not read file");
    let mut depths = vec![];
    for line in contents.split("\n") {
        let depth = match line.parse::<i32>() {
            Ok(n) => n,
            Err(_) => continue,
        };
        depths.push(depth);
    }
    depths
}
