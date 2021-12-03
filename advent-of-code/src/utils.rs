use std::fs;

pub fn read_lines(input_file: &str) -> Vec<String> {
    let contents = fs::read_to_string(input_file).expect("Could not read file");
    let mut lines = vec![];
    for line in contents.split("\n") {
        lines.push(line.to_string());
    }
    lines
}
