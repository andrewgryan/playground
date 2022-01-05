use advent_of_code::day_thirteen;
use std::fs::read_to_string;

fn main() {
    let puzzle_input = read_to_string("input-13").unwrap();
    println!(
        "solution: {}",
        day_thirteen::part_two(puzzle_input.as_str())
    );
}
