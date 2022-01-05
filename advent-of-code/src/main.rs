use advent_of_code::day_twelve;
use std::fs::read_to_string;

fn main() {
    let puzzle_input = read_to_string("input-12").unwrap();
    println!("solution: {}", day_twelve::part_one(puzzle_input.as_str()));
}
