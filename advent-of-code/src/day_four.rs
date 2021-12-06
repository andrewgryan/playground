use crate::utils::read_lines;

#[derive(Debug, Clone)]
enum Cell {
    Checked(i32),
    Blank(i32),
}
impl Cell {
    fn checked(&self) -> bool {
        match self {
            Cell::Checked(_) => true,
            Cell::Blank(_) => false,
        }
    }
}

#[derive(Debug)]
struct Board {
    cells: Vec<Vec<Cell>>,
}
impl Board {
    fn new() -> Self {
        Self { cells: vec![] }
    }
    fn add_row(&mut self, row: Vec<i32>) {
        self.cells.push(row.into_iter().map(Cell::Blank).collect());
    }
    fn len(&self) -> usize {
        self.cells.len()
    }
    fn mark(&self, call: i32) -> Self {
        Self {
            cells: self
                .cells
                .iter()
                .map(|row| {
                    row.iter()
                        .map(|cell| match cell {
                            Cell::Checked(_) => Cell::Checked(call),
                            Cell::Blank(i) => {
                                if i == &call {
                                    Cell::Checked(call)
                                } else {
                                    Cell::Blank(call)
                                }
                            }
                        })
                        .collect()
                })
                .collect(),
        }
    }
    fn complete(&self) -> bool {
        self.check(self.cells.clone()) || self.check(transpose(self.cells.clone()))
    }
    fn check(&self, cells: Vec<Vec<Cell>>) -> bool {
        cells
            .iter()
            .any(|row| row.iter().all(|cell| cell.checked()))
    }
}

fn transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Clone,
{
    assert!(!v.is_empty());
    (0..v[0].len())
        .map(|i| v.iter().map(|inner| inner[i].clone()).collect::<Vec<T>>())
        .collect()
}

pub fn part_one(input_file: &str) -> i32 {
    let mut lines = read_lines(input_file).into_iter();

    // Parse Bingo Calls
    let mut call_line = String::new();
    match lines.next() {
        None => (),
        Some(s) => call_line = s,
    }
    let calls = parse_calls(call_line.as_str());

    // Parse Bingo Boards
    let mut boards = vec![];
    let mut board = Board::new();
    for line in lines {
        if line.len() == 0 {
            continue;
        }
        if board.len() == 5 {
            boards.push(board);
            board = Board::new();
        } else {
            let row = parse_row(line.as_str());
            board.add_row(row);
        }
    }
    let mut winner: Option<Board> = None;
    for call in calls {
        match winner {
            None => (),
            Some(_) => break,
        }
        println!("call: {:?}", call);
        boards = boards
            .iter()
            .map(|board| board.mark(call))
            .collect::<Vec<Board>>()
            .clone();

        for board in boards {
            if board.complete() {
                winner = Some(board);
            }
        }
    }
    for board in boards {
        println!("{:?}", board);
    }
    println!("{:?}", winner);
    0
}

fn parse_row(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|c| c.parse().unwrap())
        .collect()
}

fn parse_calls(line: &str) -> Vec<i32> {
    line.split(',').map(|c| c.parse().unwrap()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_board() {
        let mut board = Board::new();
        board.add_row(parse_row("1 2 3 4 5"));
    }

    #[test]
    fn test_parse_row_given_extra_white_space() {
        let actual = parse_row("26 68  3 95 59");
        assert_eq!(actual, vec![26, 68, 3, 95, 59]);
    }

    #[test]
    fn test_parse_calls() {
        let actual = parse_calls("1,2,3");
        assert_eq!(actual, vec![1, 2, 3]);
    }
}
