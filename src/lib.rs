use crossterm::{
    cursor::MoveToColumn,
    execute,
    style::Print,
    terminal::{disable_raw_mode, enable_raw_mode},
};
use std::{
    collections::HashMap,
    env,
    fs::read_dir,
    io::{stdout, Write},
    path::Path,
    process::{self, Child, Command, Stdio},
};
use tokenizer::CommandPart;

mod input_reader;
mod tokenizer;

pub struct Env {
    // More in the future
    commands: Vec<String>,
    prompt_length: u16,
    lists: HashMap<String, Vec<String>>,
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
    };
    loop {
        execute!(stdout(), MoveToColumn(0)).unwrap();
        print!("~> ");
        stdout().flush().unwrap();
        let input = input_reader::get_input(&mut history, &env);
        if input.is_empty() {
            continue;
        }
        let parsed_input = tokenizer::parse_input(&input, &env);
        generate_commands(parsed_input, &mut env);
        history.push(input);
    }
}

fn generate_commands(
    parsed_input: (Vec<CommandPart>, Vec<Vec<(usize, usize, String)>>),
    env: &mut Env,
) {
    if parsed_input.1.is_empty() {
        if let Err(e) = run_input(parsed_input.0, env) {
            println!("Oops! {e}");
        }
        return;
    }
    for replacement in parsed_input.1 {
        let mut final_tokens = parsed_input.0.clone();
        for (command_part_index, token_index, contents) in replacement {
            if let CommandPart::Command(ref mut cmd) = final_tokens[command_part_index] {
                cmd[token_index - 1] = contents;
            }
        }
        if let Err(e) = run_input(final_tokens, env) {
            println!("Oops! {e}");
        }
    }
}

fn run_input(mut input: Vec<tokenizer::CommandPart>, env: &mut Env) -> crossterm::Result<()> {
    disable_raw_mode().unwrap();
    // let mut input = input.deref().iter().peekable();
    let mut last_command: Option<Child> = None;

    for token_index in 0..input.len() {
        let stdout = match input.get(token_index + 1) {
            Some(_) => Stdio::piped(),
            None => Stdio::inherit(),
        };
        let tokens = match &mut input[token_index] {
            CommandPart::Command(cmd) => cmd,
            _ => unreachable!(),
        };
        let command = tokens.remove(0);

        let stdin = if let Some(output) = last_command {
            Stdio::from(output.stdout.unwrap())
        } else {
            Stdio::inherit()
        };
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
