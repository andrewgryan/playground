use crossterm::event::{DisableMouseCapture, EnableMouseCapture};
use crossterm::execute;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use std::io;
use std::thread;
use std::time::Duration;
use tui::layout::{Alignment, Constraint, Direction, Layout};
use tui::widgets::{Block, Borders, Paragraph};
use tui::{backend::CrosstermBackend, Terminal};

fn main() -> Result<(), io::Error> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();

    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    terminal.draw(|f| {
        // Layout
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .margin(1)
            .constraints(
                [
                    Constraint::Percentage(10),
                    Constraint::Percentage(80),
                    Constraint::Percentage(10),
                ]
                .as_ref(),
            )
            .split(f.size());

        // Block 1
        let block = Block::default().title("Block I").borders(Borders::ALL);
        f.render_widget(block, chunks[0]);

        // Block 2
        let block = Block::default().title("Block II").borders(Borders::ALL);
        f.render_widget(block, chunks[1]);

        // Block 3
        let block = Paragraph::new("Copyright 2022")
            .alignment(Alignment::Center)
            .block(Block::default().title("Block III").borders(Borders::ALL));
        f.render_widget(block, chunks[2]);
    })?;

    // Wait 5 seconds
    thread::sleep(Duration::from_millis(5000));

    // Restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    Ok(())
}
