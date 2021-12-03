/// Gamma/Epsilon from binary numbers
use crate::utils::read_lines;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_decimal() {
        assert_eq!(to_decimal(vec![0, 0, 1]), 1);
        assert_eq!(to_decimal(vec![0, 1, 0]), 2);
        assert_eq!(to_decimal(vec![1, 0, 0]), 4);
        assert_eq!(to_decimal(vec![1, 1, 1]), 7);
        assert_eq!(to_decimal(vec![1, 1, 1, 1]), 15);
    }
}

pub fn part_two(input_file: &str) -> i32 {
    let x = find_rate(input_file, most_common);
    let y = find_rate(input_file, least_common);
    x * y
}

pub fn find_rate(input_file: &str, algorithm: fn(Vec<Bit>) -> Bit) -> i32 {
    let mut entries = load(input_file);
    for i in 0..12 {
        let mut column: Vec<Bit> = vec![];
        for entry in &entries {
            column.push(entry[i].clone());
        }
        let bit = algorithm(column);
        entries = entries.into_iter().filter(|e| e[i] == bit).collect();
        if entries.len() == 1 {
            break;
        }
    }
    to_decimal(entries[0].clone().into_iter().map(to_number).collect())
}

fn to_number(b: Bit) -> i32 {
    match b {
        Bit::One => 1,
        Bit::Zero => 0,
    }
}

fn load(input_file: &str) -> Vec<Vec<Bit>> {
    let mut data = vec![];
    for line in read_lines(input_file) {
        let mut binary = vec![];
        for c in line.chars() {
            match c {
                '1' => binary.push(Bit::One),
                '0' => binary.push(Bit::Zero),
                _ => continue,
            }
        }
        if binary.len() > 0 {
            data.push(binary);
        }
    }
    data
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Bit {
    One,
    Zero,
}

fn most_common(bits: Vec<Bit>) -> Bit {
    let mut one: i32 = 0;
    let mut zero: i32 = 0;
    for bit in bits {
        match bit {
            Bit::Zero => zero += 1,
            Bit::One => one += 1,
        }
    }
    if one >= zero {
        Bit::One
    } else {
        Bit::Zero
    }
}

fn least_common(bits: Vec<Bit>) -> Bit {
    match most_common(bits) {
        Bit::One => Bit::Zero,
        Bit::Zero => Bit::One,
    }
}

pub fn part_one(input_file: &str) -> i32 {
    let mut gamma: Vec<i32> = vec![];
    let mut epsilon: Vec<i32> = vec![];
    for i in 0..12 {
        let mut ones: i32 = 0;
        let mut zeros: i32 = 0;
        println!("{}", i);
        for line in read_lines(input_file) {
            let chars: Vec<char> = line.chars().collect();
            match chars.get(i) {
                None => continue,
                Some(symbol) => match symbol {
                    '1' => ones += 1,
                    '0' => zeros += 1,
                    _ => (),
                },
            };
        }
        println!("ones: {} zeros: {}", ones, zeros);

        // Gamma refers to most common digit
        if ones > zeros {
            gamma.push(1);
        } else {
            gamma.push(0);
        }

        // Epsilon refers to least common digit
        if ones < zeros {
            epsilon.push(1);
        } else {
            epsilon.push(0);
        }
    }

    to_decimal(gamma) * to_decimal(epsilon)
}

fn to_decimal(bits: Vec<i32>) -> i32 {
    let mut result: i32 = 0;
    let mut i = bits.len() as u32;
    let base: i32 = 2;
    for bit in bits {
        result += bit * (base.pow(i - 1));
        i -= 1;
    }
    result
}
