use advent_of_code::day_fourteen;
use std::fs::read_to_string;

fn main() {
    let puzzle_input = read_to_string("input-14").unwrap();
    println!(
        "solution: {}",
        day_fourteen::part_two(puzzle_input.as_str())
    );
}
