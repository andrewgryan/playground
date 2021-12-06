use std::fs;

#[derive(Debug)]
struct Game {
    calls: Vec<i32>,
    boards: Vec<Board>,
}

#[derive(Debug)]
struct Board {
    cells: Vec<i32>,
}
impl Board {
    fn new() -> Self {
        Self { cells: vec![] }
    }
    fn insert_row(&mut self, items: Vec<i32>) {
        self.cells.extend(items);
    }
    fn len(&self) -> usize {
        self.cells.len()
    }
}

impl From<String> for Game {
    fn from(contents: String) -> Self {
        let mut lines = contents.split('\n');

        // Parse bingo calls
        let calls = lines
            .next()
            .unwrap()
            .split(',')
            .map(|c| c.parse().unwrap())
            .collect();

        // Parse bingo boards
        let mut boards = vec![];
        let mut board = Board::new();
        for line in lines {
            if (line.len() == 0) && (board.len() > 0) {
                boards.push(board);
                board = Board::new();
            } else {
                let row: Vec<i32> = line
                    .split_whitespace()
                    .map(|c| c.parse().unwrap())
                    .collect();
                board.insert_row(row);
            }
        }

        Self { calls, boards }
    }
}

pub fn part_one(input_file: &str) -> i32 {
    let contents = fs::read_to_string(input_file).expect("could not open file");
    let game: Game = contents.into();
    println!("{:?}", game);

    0
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert!(false);
    }
}
