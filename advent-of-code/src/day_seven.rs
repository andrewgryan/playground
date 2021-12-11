use std::fs;

pub fn part_one(input_file: &str) -> i32 {
    println!("{}", input_file);
    let content = fs::read_to_string(input_file).unwrap();
    let row = content.split('\n').next().unwrap();
    let positions: Vec<i32> = row.split(',').map(|s| s.parse().unwrap()).collect();
    let cost_fn = quadratic_cost; // linear_cost for part 1
    match minimum_position(positions, cost_fn) {
        None => -1,
        Some(place) => place.cost,
    }
}

#[derive(Clone, Copy)]
pub struct Position {
    cost: i32,
    x: i32,
}
impl Position {
    pub fn new(x: i32, cost: i32) -> Self {
        Position { x, cost }
    }
}

pub fn minimum_fuel(positions: Vec<i32>, cost_fn: fn(&i32, i32) -> i32) -> i32 {
    match minimum_position(positions, cost_fn) {
        None => -1,
        Some(place) => place.x,
    }
}

pub fn minimum_position(positions: Vec<i32>, cost_fn: fn(&i32, i32) -> i32) -> Option<Position> {
    let mut position: Option<Position> = None;
    let start = positions.iter().min().unwrap();
    let end = positions.iter().max().unwrap();
    for x in *start..(*end + 1) {
        println!("{}", x);
        let new_position = Position::new(x, cost(&positions, x, cost_fn));
        match position {
            None => position = Some(new_position),
            Some(old_position) => {
                if old_position.cost >= new_position.cost {
                    position = Some(new_position);
                }
            }
        }
    }
    position
}

pub fn cost(positions: &Vec<i32>, x: i32, cost_fn: fn(&i32, i32) -> i32) -> i32 {
    let mut result = 0;
    for position in positions {
        result += cost_fn(position, x);
    }
    result
}

pub fn linear_cost(start: &i32, end: i32) -> i32 {
    (start - end).abs()
}

pub fn quadratic_cost(start: &i32, end: i32) -> i32 {
    // 4 -> 1 + 2 + 3 + 4 = 10 -> n * (n + 1) / 2
    let n = (start - end).abs();
    n * (n + 1) / 2
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(1, 41)]
    #[case(2, 37)]
    #[case(3, 39)]
    #[case(10, 71)]
    fn day_seven_cost_given_linear_cost(#[case] input: i32, #[case] expected: i32) {
        let data = vec![16, 1, 2, 0, 4, 2, 7, 1, 2, 14];
        let actual = cost(&data, input, linear_cost);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(2, 206)]
    #[case(5, 168)]
    fn day_seven_cost_given_quadratic_cost(#[case] input: i32, #[case] expected: i32) {
        let data = vec![16, 1, 2, 0, 4, 2, 7, 1, 2, 14];
        let actual = cost(&data, input, quadratic_cost);
        assert_eq!(actual, expected);
    }

    #[test]
    fn day_seven_minimum_position() {
        let data = vec![16, 1, 2, 0, 4, 2, 7, 1, 2, 14];
        let actual = minimum_fuel(data, linear_cost);
        assert_eq!(actual, 2);
    }
}
