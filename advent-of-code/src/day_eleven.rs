use std::convert::{TryFrom, TryInto};

pub fn part_one(puzzle_input: &str) -> u32 {
    println!("{}", puzzle_input);
    puzzle_input.split('\n').for_each(|l| {
        l.chars()
            .map(Octopus::try_from)
            .map(|o| o.unwrap())
            .for_each(|c| println!("{:?}", c))
    });
    0
}

#[derive(Debug, PartialEq)]
struct Octopus {
    energy_level: u32,
}

impl Octopus {
    pub fn new(energy_level: u32) -> Self {
        Self { energy_level }
    }

    pub fn increase_level(&mut self) {
        self.energy_level += 1;
    }
}

impl TryFrom<char> for Octopus {
    type Error = ();

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c.to_digit(10) {
            Some(n) => Ok(Octopus::new(n)),
            None => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn octopuses() {
        let actual: Result<Octopus, _> = '9'.try_into();
        assert_eq!(actual, Ok(Octopus::new(9)));
    }

    #[test]
    fn increase_level() {
        let mut actual = Octopus::new(0);
        actual.increase_level();
        assert_eq!(actual, Octopus::new(1));
    }
}
