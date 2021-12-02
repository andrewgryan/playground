use std::{fs, str::FromStr};

pub fn part_one(input_file: &str) -> i32 {
    let lines = read_lines(input_file);

    let mut depth = 0;
    let mut horizontal = 0;

    for line in lines {
        let instruction = match line.parse::<Instruction>() {
            Ok(i) => i,
            Err(_) => continue,
        };

        match instruction {
            Instruction::Up(z) => depth = depth - z,
            Instruction::Down(z) => depth = depth + z,
            Instruction::Forward(x) => horizontal = horizontal + x,
        }
    }
    depth * horizontal
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_two() {
        let instructions = vec![
            Instruction::Forward(5),
            Instruction::Down(5),
            Instruction::Forward(8),
            Instruction::Up(3),
            Instruction::Down(8),
            Instruction::Forward(2),
        ];
        assert_eq!(part_two_calculation(instructions), 900);
    }
}

pub fn part_two(input_file: &str) -> i32 {
    let lines = read_lines(input_file);
    let mut instructions = vec![];
    for line in lines {
        match line.parse::<Instruction>() {
            Ok(i) => instructions.push(i),
            Err(_) => continue,
        };
    }
    part_two_calculation(instructions)
}

fn part_two_calculation(instructions: Vec<Instruction>) -> i32 {
    let mut aim = 0;
    let mut depth = 0;
    let mut horizontal = 0;
    for instruction in instructions {
        match instruction {
            Instruction::Up(x) => aim = aim - x,
            Instruction::Down(x) => aim = aim + x,
            Instruction::Forward(x) => {
                horizontal = horizontal + x;
                depth = depth + (aim * x)
            }
        }
    }
    depth * horizontal
}

#[derive(Debug)]
enum Instruction {
    Up(i32),
    Down(i32),
    Forward(i32),
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(" ");
        let cmd = split.next().unwrap();
        let value = split.next().map(|s| s.parse::<i32>());
        match value {
            Some(result) => match result {
                Ok(n) => match cmd {
                    "forward" => Ok(Instruction::Forward(n)),
                    "down" => Ok(Instruction::Down(n)),
                    "up" => Ok(Instruction::Up(n)),
                    _ => Err(format!("invalid: {}", s)),
                },
                Err(_) => Err(format!("invalid: {}", s)),
            },
            None => Err(format!("invalid: {}", s)),
        }
    }
}

fn read_lines(input_file: &str) -> Vec<String> {
    let contents = fs::read_to_string(input_file).expect("Could not read file");
    let mut lines = vec![];
    for line in contents.split("\n") {
        lines.push(line.to_string());
    }
    lines
}
