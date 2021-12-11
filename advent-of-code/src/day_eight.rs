use std::fs::read_to_string;

// 0 -> abcefg
// 1 -> cf
// 2 -> acdeg
// 3 -> acdfg
// 4 -> bcdf
// 5 -> abdfg
// 6 -> abdefg
// 7 -> acf
// 8 -> abcdefg
// 9 -> abcdfg
pub fn part_one(input_file: &str) -> usize {
    println!("{}", input_file);
    let content = read_to_string(input_file).unwrap();
    let mut unique_codes = 0;
    for row in content.split('\n') {
        let codes: Vec<&str>;
        match row.split('|').nth(1) {
            None => continue,
            Some(s) => {
                codes = s.split_whitespace().collect();
            }
        }
        unique_codes += codes
            .iter()
            .filter(|s| simple_code(s.len()))
            .collect::<Vec<_>>()
            .len();
        println!("{:?}", codes);
    }
    unique_codes
}

pub fn simple_code(n: usize) -> bool {
    match n {
        2 | 3 | 4 | 7 => true,
        _ => false,
    }
}
