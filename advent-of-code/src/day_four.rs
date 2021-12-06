use std::fs;
use std::str::FromStr;

#[derive(Debug)]
struct Game {
    calls: Vec<i32>,
    boards: Vec<Board>,
    previous_call: i32,
}
impl Game {
    fn play(&self) -> Game {
        let mut replacement_boards = vec![];
        let call = self.calls.iter().next().unwrap();
        for board in self.boards.iter() {
            replacement_boards.push(board.mark(call));
        }
        Game {
            boards: replacement_boards,
            calls: (&self.calls[1..]).to_vec(),
            previous_call: *call,
        }
    }

    fn winner(&self) -> Option<&Board> {
        self.boards.iter().find(|board| board.bingo())
    }

    fn winners(&self) -> Vec<Board> {
        self.boards
            .iter()
            .filter(|board| board.bingo())
            .map(|board| board.clone())
            .collect::<Vec<Board>>()
    }

    fn eject_winners(&self) -> Game {
        Game {
            previous_call: self.previous_call,
            boards: self
                .boards
                .iter()
                .filter(|board| !board.bingo())
                .map(|b| b.clone())
                .collect(),
            calls: self.calls.clone(),
        }
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
                let row: Vec<Cell> = line
                    .split_whitespace()
                    .map(|c| c.parse().unwrap())
                    .collect();
                board.insert_row(row);
            }
        }

        Self {
            calls,
            boards,
            previous_call: 0,
        }
    }
}

#[derive(Debug, Clone)]
struct Board {
    cells: Vec<Cell>,
}
impl Board {
    fn new() -> Self {
        Self { cells: vec![] }
    }
    fn insert_row(&mut self, items: Vec<Cell>) {
        self.cells.extend(items);
    }
    fn len(&self) -> usize {
        self.cells.len()
    }
    fn mark(&self, call: &i32) -> Self {
        let mut replacement_cells = vec![];
        for cell in self.cells.iter() {
            replacement_cells.push(cell.mark(call));
        }
        Self {
            cells: replacement_cells,
        }
    }

    fn iter_rows(&self) -> RowIterator {
        RowIterator::new(&self.cells)
    }
    fn iter_cols(&self) -> ColIterator {
        ColIterator::new(&self.cells)
    }

    fn bingo(&self) -> bool {
        self.iter_rows().any(|row| row.bingo()) || self.iter_cols().any(|col| col.bingo())
    }

    fn secret_code(&self, call: i32) -> i32 {
        let mut blank = 0;
        for cell in &self.cells {
            match cell {
                Cell::Blank(i) => blank += i,
                _ => (),
            }
        }
        call * blank
    }
}

struct RowIterator<'a> {
    i: usize,
    cells: &'a Vec<Cell>,
}
impl<'a> RowIterator<'a> {
    fn new(cells: &'a Vec<Cell>) -> Self {
        Self { i: 0, cells }
    }
}
impl<'a> Iterator for RowIterator<'a> {
    type Item = Line<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let result;
        if self.i >= 5 {
            result = None;
        } else {
            let mut cells = vec![];
            for j in 0..5 {
                cells.push(&self.cells[5 * self.i + j]);
            }
            result = Some(Line { cells });
        }
        self.i += 1;
        result
    }
}

struct ColIterator<'a> {
    i: usize,
    cells: &'a Vec<Cell>,
}
impl<'a> ColIterator<'a> {
    fn new(cells: &'a Vec<Cell>) -> Self {
        Self { i: 0, cells }
    }
}
impl<'a> Iterator for ColIterator<'a> {
    type Item = Line<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let result;
        if self.i >= 5 {
            result = None;
        } else {
            let mut cells = vec![];
            for j in 0..5 {
                cells.push(&self.cells[5 * j + self.i]);
            }
            result = Some(Line { cells });
        }
        self.i += 1;
        result
    }
}

struct Line<'a> {
    cells: Vec<&'a Cell>,
}
impl<'a> Line<'a> {
    fn bingo(&self) -> bool {
        self.cells.iter().all(|cell| cell.marked())
    }
}

#[derive(Debug, Clone)]
enum Cell {
    Marked(i32),
    Blank(i32),
}
impl Cell {
    fn mark(&self, call: &i32) -> Self {
        match self {
            Cell::Marked(_) => self.clone(),
            Cell::Blank(n) => {
                if *n == *call {
                    Cell::Marked(*n)
                } else {
                    self.clone()
                }
            }
        }
    }
    fn marked(&self) -> bool {
        match self {
            Cell::Marked(_) => true,
            _ => false,
        }
    }
}
impl From<i32> for Cell {
    fn from(n: i32) -> Cell {
        Cell::Blank(n)
    }
}
impl FromStr for Cell {
    type Err = String;
    fn from_str(s: &str) -> Result<Cell, Self::Err> {
        match s.parse() {
            Err(_) => Err("Could not parse Cell".to_string()),
            Ok(n) => Ok(Cell::Blank(n)),
        }
    }
}

pub fn part_one(input_file: &str) -> i32 {
    let contents = fs::read_to_string(input_file).expect("could not open file");
    let mut game: Game = contents.into();
    let mut solution = 0;
    for _ in 0..100 {
        game = game.play();

        if let Some(winner) = game.winner() {
            solution = winner.secret_code(game.previous_call);
            break;
        }
    }
    solution
}

pub fn part_two(input_file: &str) -> i32 {
    let contents = fs::read_to_string(input_file).expect("could not open file");
    let mut game: Game = contents.into();
    let mut last_winners;
    for _ in 0..100 {
        last_winners = game.winners();
        game = game.eject_winners();
        if game.boards.len() == 0 {
            println!("{:?}", last_winners[0].secret_code(game.previous_call));
            break;
        }
        game = game.play();
    }
    0
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert!(false);
    }
}
