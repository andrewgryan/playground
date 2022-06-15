use clap::Parser;
use serde::Serialize;
use serde_json;
use std::str::FromStr;

#[derive(Serialize)]
enum Format {
    Text,
    Json,
}

impl FromStr for Format {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "json" => Ok(Format::Json),
            "text" => Ok(Format::Text),
            &_ => Err("Unrecognized format".to_string()),
        }
    }
}

#[derive(Parser)]
struct Cli {
    name: String,

    #[clap(long)]
    output: Option<Format>,
}

#[derive(Serialize)]
struct Settings {
    name: String,
    output: Format,
}

fn main() {
    let args = Cli::parse();
    let output = args.output.unwrap_or(Format::Text);
    let name = args.name.clone();
    let settings = Settings { name, output };

    println!("Hello, {}!", args.name);

    let serialized = serde_json::to_string(&settings).unwrap();
    println!("{}", serialized);
}
