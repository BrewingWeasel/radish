use crossterm::{
    cursor::MoveToColumn,
    execute,
    style::Print,
    terminal::{disable_raw_mode, enable_raw_mode},
};
use std::{
    collections::HashMap,
    env,
    error::Error,
    fs::{read_dir, File, OpenOptions},
    io::{stderr, stdout, Write},
    path::Path,
    process::{self, Child, Command, Stdio},
};
use tokenizer::CommandPart;

mod input_reader;
mod tokenizer;

#[derive(Debug, Clone)]
pub struct InvalidItemError;

impl Error for InvalidItemError {}

impl std::fmt::Display for InvalidItemError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Oops! Looks like the item you're looking for doesn't exist"
        )
    }
}

pub struct Env {
    // More in the future
    commands: Vec<String>,
    prompt_length: u16,
    lists: HashMap<String, Vec<String>>,
    locations: HashMap<String, String>,
}

pub fn run_radish() {
    enable_raw_mode().unwrap();
    let mut history: Vec<String> = vec![];
    let mut commands = get_all_commands();
    commands.sort_unstable();
    commands.dedup(); // Hacky workaround for when there are multiple of the same file name in path
    let mut env = Env {
        commands,
        prompt_length: 3,
        lists: HashMap::new(),
        locations: HashMap::new(),
    };
    loop {
        execute!(stdout(), MoveToColumn(0)).unwrap();
        print!("~> ");
        stdout().flush().unwrap();
        let input = input_reader::get_input(&mut history, &env);
        if input.is_empty() {
            continue;
        }
        if let Err(e) = run_from_string(&input, &mut env) {
            eprintln!("{}", e);
        }
        history.push(input);
    }
}

fn run_from_string(input: &String, env: &mut Env) -> Result<(), Box<dyn Error>> {
    if input.is_empty() {
        return Ok(());
    }
    let parsed_input = tokenizer::parse_input(&input, &env)?;
    generate_commands(parsed_input, env);
    Ok(())
}

fn generate_commands(
    parsed_input: (Vec<CommandPart>, Vec<Vec<(usize, usize, String)>>),
    env: &mut Env,
) {
    if parsed_input.1.is_empty() {
        if let Err(e) = run_input(parsed_input.0, env) {
            eprintln!("Oops! {e}");
        }
        return;
    }
    for replacement in parsed_input.1 {
        let mut final_tokens = parsed_input.0.clone();
        for (command_part_index, token_index, contents) in replacement {
            if let CommandPart::Command(ref mut cmd) = final_tokens[command_part_index] {
                // cmd[token_index - 1] = contents;
                cmd[token_index - 1] = contents + &cmd[token_index - 1];
            }
        }
        if let Err(e) = run_input(final_tokens, env) {
            eprintln!("Oops! {e}");
        }
    }
}

fn run_input(mut input: Vec<tokenizer::CommandPart>, env: &mut Env) -> crossterm::Result<()> {
    disable_raw_mode().unwrap();
    // let mut input = input.deref().iter().peekable();
    let mut last_command: Option<Child> = None;

    for token_index in 0..input.len() {
        if !matches!(input.get(token_index).unwrap(), CommandPart::Command(_)) {
            continue;
        }
        let mut stdout = match input.get(token_index + 1) {
            Some(part) => match part {
                CommandPart::ToFile((file_name, append)) => {
                    if *append {
                        Stdio::from(
                            OpenOptions::new()
                                .append(true)
                                .create(true)
                                .open(file_name)
                                .unwrap(),
                        )
                    } else {
                        Stdio::from(
                            OpenOptions::new()
                                .write(true)
                                .create(true)
                                .truncate(true)
                                .open(file_name)
                                .unwrap(),
                        )
                    }
                }
                CommandPart::FromFile(_) => Stdio::inherit(),
                _ => Stdio::piped(),
            },
            None => Stdio::inherit(),
        };

        let stdin = if let Some(CommandPart::FromFile(file_name)) = input.get(token_index + 1) {
            // Do in a way so this is no longer necessary
            if let Some(CommandPart::ToFile((file_name, append))) = input.get(token_index + 2) {
                stdout = if *append {
                    Stdio::from(
                        OpenOptions::new()
                            .append(true)
                            .create(true)
                            .open(file_name)
                            .unwrap(),
                    )
                } else {
                    Stdio::from(
                        OpenOptions::new()
                            .write(true)
                            .create(true)
                            .truncate(true)
                            .open(file_name)
                            .unwrap(),
                    )
                }
            }
            Stdio::from(File::open(file_name).unwrap())
        } else {
            if let Some(output) = last_command {
                Stdio::from(output.stdout.unwrap())
            } else {
                Stdio::inherit()
            }
        };

        let tokens = match &mut input[token_index] {
            CommandPart::Command(cmd) => cmd,
            _ => unreachable!(),
        };
        let command = tokens.remove(0);

        last_command = run(&command, tokens.to_vec(), stdout, stdin, env);
    }
    if let Some(mut cmd) = last_command {
        cmd.wait()?;
    }
    enable_raw_mode().unwrap();
    Ok(())
}

fn run(
    command: &str,
    args: Vec<String>,
    stdout: Stdio,
    stdin: Stdio,
    env: &mut Env,
) -> Option<Child> {
    // TODO: probably make there be a
    // struct for this
    match command {
        "cd" => {
            cd(args);
            None
        }
        "mklist" => {
            mklist(&mut env.lists, args);
            None
        }
        "mkloc" => {
            mkloc(&mut env.locations, args);
            None
        }
        "exit" => process::exit(0),
        command => {
            match Command::new(command)
                .args(args)
                .stdin(stdin)
                .stdout(stdout)
                .spawn()
            {
                Ok(output) => Some(output),
                Err(e) => {
                    println!("Error running {command}: {e}");
                    None
                }
            }
        }
    }
}

fn cd(args: Vec<String>) {
    let newdir = match args.iter().next() {
        None => dirs::home_dir().unwrap(),
        Some(dir) => Path::new(dir).into(),
    };
    env::set_current_dir(newdir).expect("Error setting dir");
}

fn mklist(lists: &mut HashMap<String, Vec<String>>, args: Vec<String>) {
    let mut args = args.iter();
    if let Some(first) = args.next() {
        lists.insert(first.to_string(), args.map(|x| x.to_string()).collect());
    } else {
        execute!(stdout(), Print("No list name provided")).unwrap();
    }
}

fn mkloc(locs: &mut HashMap<String, String>, args: Vec<String>) {
    let mut args = args.iter();
    if let Some(name) = args.next() {
        locs.insert(name.to_string(), args.next().unwrap().to_string()); // TODO: clean up
    } else {
        execute!(stderr(), Print("Not enough arguments provided")).unwrap();
        return;
    }
}

pub fn get_all_commands() -> Vec<String> {
    //TODO: filter for only executables
    let path = env::var("PATH").unwrap();
    path.split(':')
        .flat_map(|p| {
            read_dir(p)
                .unwrap()
                .filter(|f| f.as_ref().unwrap().metadata().unwrap().is_file())
                .map(|f| {
                    f.unwrap()
                        .path()
                        .display()
                        .to_string()
                        .strip_prefix(p)
                        .unwrap()
                        .trim_start_matches('/')
                        .to_string()
                })
                .collect::<Vec<String>>()
        })
        .collect()
}
