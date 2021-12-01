use std::fs;

pub fn part_one(input_file: &str) -> i32 {
    // Perform I/O to load inputs
    let contents = fs::read_to_string(input_file).expect("Could not read file");
    let mut depths = vec![];
    for line in contents.split("\n") {
        let depth = match line.parse::<i32>() {
            Ok(n) => n,
            Err(_) => continue,
        };
        depths.push(depth);
    }

    // Analyse inputs
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
