use crossterm::{
    cursor::MoveToColumn,
    execute,
    style::Print,
    terminal::{disable_raw_mode, enable_raw_mode},
};
use std::{
    borrow::Cow,
    collections::HashMap,
    env::{self, args},
    error::Error,
    fs::{read_dir, File, OpenOptions},
    io::{stdout, BufRead, BufReader, Lines, Write},
    path::{Path, PathBuf},
    process::{self, Child, Command, Stdio},
    str::from_utf8,
};
use tokenizer::{CommandPart, TokenizedOutput};

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
    history: Vec<String>,
    commands: Vec<String>,
    prompt_length: u16,
    lists: HashMap<String, Vec<String>>,
    locations: HashMap<String, String>,
    aliases: HashMap<String, String>,
    functions: HashMap<String, String>,
    shell_variables: HashMap<String, String>,
    continue_if: bool,
}
fn run_from_file(path: PathBuf, env: &mut Env) -> Result<(), Box<dyn Error>> {
    let mut lines = BufReader::new(File::open(path)?).lines();
    while let Some(line) = lines.next() {
        run_from_string(Cow::Borrowed(&line.unwrap()), env, true, Some(&mut lines))?;
    }
    Ok(())
}

pub fn run_radish() {
    let mut history = vec![];
    if let Ok(file) = File::open(dirs::home_dir().unwrap().join(".radish_history")) {
        let mut lines = BufReader::new(file).lines();
        while let Some(line) = lines.next() {
            history.push(line.unwrap());
        }
    }
    let mut commands = get_all_commands();
    commands.sort_unstable();
    commands.dedup(); // Hacky workaround for when there are multiple of the same file name in path
    let mut env = Env {
        history,
        commands,
        prompt_length: 3,
        lists: HashMap::new(),
        functions: HashMap::new(),
        locations: HashMap::new(),
        aliases: HashMap::new(),
        shell_variables: HashMap::new(),
        continue_if: false,
    };
    env.shell_variables
        .insert(String::from("?"), String::from("0"));
    let mut args = args().peekable();
    args.next();
    if args.peek() == Some(&String::from("-e")) {
        args.next();
    }
    if let Some(file) = args.next() {
        if let Err(e) = run_from_file(PathBuf::from(file), &mut env) {
            eprintln!("{e}");
        }
        return;
    }
    if let Err(e) = run_from_file(dirs::home_dir().unwrap().join(".radishrc"), &mut env) {
        eprintln!("{e}");
    };
    loop {
        execute!(stdout(), MoveToColumn(0)).unwrap();
        let prompt = unescape(env::var("PS1").unwrap_or("~> ".to_string()));
        let stripped_prompt = strip_ansi_escapes::strip(prompt.lines().last().unwrap());
        env.prompt_length = from_utf8(&stripped_prompt.unwrap())
            .unwrap()
            .len()
            .try_into()
            .unwrap();
        print!("{}", prompt);
        stdout().flush().unwrap();
        enable_raw_mode().unwrap();
        let input = input_reader::get_input(&mut env);
        disable_raw_mode().unwrap();
        if input.is_empty() {
            continue;
        }
        if let Err(e) = run_from_string(Cow::Borrowed(&input), &mut env, true, None) {
            eprintln!("{}", e);
        }
        env.history.push(input);
    }
}

fn run_from_string(
    input: Cow<String>,
    env: &mut Env,
    output: bool,
    extra_lines: Option<&mut Lines<BufReader<File>>>,
) -> Result<Option<Child>, Box<dyn Error>> {
    if input.is_empty() || input.starts_with('#') {
        return Ok(None);
    }

    let mut new_input = input.to_string();
    for alias in env.aliases.keys() {
        if new_input.starts_with(alias) {
            new_input = new_input.replacen(alias, env.aliases.get(alias).unwrap(), 1);
        }
    }
    let parsed_input = tokenizer::parse_input(&new_input, env, extra_lines)?;
    generate_commands(parsed_input, env, output)
}

fn generate_commands(
    parsed_input: TokenizedOutput,
    env: &mut Env,
    output: bool,
) -> Result<Option<Child>, Box<dyn Error>> {
    let reset_value = match &parsed_input.variable_assignment {
        Some((name, value)) => {
            if parsed_input.commands == vec![vec![CommandPart::Command(vec![])]] {
                env::set_var(name, value);
                return Ok(None);
            } else {
                let old_value = env::var(name).unwrap_or(String::new());
                env::set_var(name, value);
                Some(old_value)
            }
        }
        None => None,
    };
    if parsed_input.replacements.first().unwrap().is_empty() {
        let output = run_input(parsed_input.commands, env, output);
        if let Some(old_value) = reset_value {
            env::set_var(parsed_input.variable_assignment.unwrap().0, old_value);
        }
        return output;
    }
    let mut last_command = None;
    for replacement in parsed_input.replacements {
        let mut final_tokens = parsed_input.commands.clone();
        for cur_replacement in replacement {
            if let CommandPart::Command(ref mut cmd) =
                final_tokens[cur_replacement.command_number][cur_replacement.command_part_number]
            {
                cmd[cur_replacement.token_number - 1] =
                    cur_replacement.replacement + &cmd[cur_replacement.token_number - 1];
            }
        }
        last_command = run_input(final_tokens, env, output)?;
    }
    if let Some(old_value) = reset_value {
        env::set_var(parsed_input.variable_assignment.unwrap().0, old_value);
    }
    Ok(last_command)
}

fn run_input(
    mut commands: Vec<Vec<CommandPart>>,
    env: &mut Env,
    output: bool,
) -> Result<Option<Child>, Box<dyn Error>> {
    let mut last_command: Option<Child>;
    let mut commands_iter = commands.iter_mut().peekable();
    while let Some(input) = commands_iter.next() {
        last_command = None;
        for token_index in 0..input.len() {
            match input.get(token_index).unwrap() {
                CommandPart::Command(_) => (),
                CommandPart::Or => {
                    if last_command.unwrap().wait()?.to_string() == "exit status: 0" {
                        last_command = None;
                        break;
                    } else {
                        last_command = None;
                        continue;
                    }
                }
                CommandPart::And => {
                    if last_command.unwrap().wait()?.to_string() == "exit status: 0" {
                        last_command = None;
                        continue;
                    } else {
                        last_command = None;
                        break;
                    }
                }
                _ => continue,
            }
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
                                    .open(file_name)?,
                            )
                        } else {
                            Stdio::from(
                                OpenOptions::new()
                                    .write(true)
                                    .create(true)
                                    .truncate(true)
                                    .open(file_name)?,
                            )
                        }
                    }
                    CommandPart::FromFile(_) | CommandPart::Or | CommandPart::And => {
                        Stdio::inherit()
                    }
                    _ => Stdio::piped(),
                },
                None => {
                    if output {
                        Stdio::inherit()
                    } else {
                        Stdio::piped()
                    }
                }
            };

            let stdin = if let Some(CommandPart::FromFile(file_name)) = input.get(token_index + 1) {
                // Do in a way so this is no longer necessary
                if let Some(CommandPart::ToFile((file_name, append))) = input.get(token_index + 2) {
                    stdout = if *append {
                        Stdio::from(
                            OpenOptions::new()
                                .append(true)
                                .create(true)
                                .open(file_name)?,
                        )
                    } else {
                        Stdio::from(
                            OpenOptions::new()
                                .write(true)
                                .create(true)
                                .truncate(true)
                                .open(file_name)?,
                        )
                    }
                }
                Stdio::from(File::open(file_name)?)
            } else if let Some(output) = last_command {
                Stdio::from(output.stdout.unwrap())
            } else {
                Stdio::inherit()
            };

            let tokens = match &mut input[token_index] {
                CommandPart::Command(cmd) => cmd,
                _ => unreachable!(),
            };
            let command = tokens.remove(0);

            last_command = run(&command, tokens.to_vec(), stdout, stdin, env)?;
        }
        if let Some(mut cmd) = last_command {
            env.shell_variables.insert(
                String::from("?"),
                cmd.wait()?
                    .to_string()
                    .strip_prefix("exit status: ")
                    .unwrap()
                    .to_string(),
            );
            if commands_iter.peek().is_none() {
                return Ok(Some(cmd));
            }
        }
    }
    Ok(None)
}

fn run(
    command: &str,
    args: Vec<String>,
    stdout: Stdio,
    stdin: Stdio,
    env: &mut Env,
) -> Result<Option<Child>, Box<dyn Error>> {
    // TODO: probably make there be a
    // struct for this
    match command {
        "cd" => {
            cd(args);
            Ok(None)
        }
        "" => Ok(None),
        "if" => Ok(if_statement(env, args)?),
        "elif" => Ok(elif_statement(env, args)?),
        "then" => {
            then_statement(env, args.first().unwrap())?;
            Ok(None)
        }
        "else" => {
            else_statement(env, args.first().unwrap())?;
            Ok(None)
        }
        "mklist" => {
            mklist(&mut env.lists, args);
            Ok(None)
        }
        "alias" => {
            alias(&mut env.aliases, args.first().unwrap());
            Ok(None)
        }
        "export" => {
            export(args.first().unwrap());
            Ok(None)
        }
        "mkloc" => {
            mkloc(&mut env.locations, args);
            Ok(None)
        }
        "function" => {
            mkloc(&mut env.functions, args);
            Ok(None)
        }
        "exit" => {
            exit(env);
            unreachable!()
        }
        command => {
            if let Some(contents) = env.functions.get(command) {
                let new_contents = contents
                    .strip_prefix('{')
                    .unwrap()
                    .trim_matches(';')
                    .strip_suffix('}')
                    .unwrap()
                    .to_string();
                let last_cmd = run_from_string(Cow::Borrowed(&new_contents), env, true, None)?;
                Ok(last_cmd)
            } else {
                match Command::new(command)
                    .args(args)
                    .stdin(stdin)
                    .stdout(stdout)
                    .spawn()
                {
                    Ok(output) => Ok(Some(output)),
                    Err(e) => {
                        eprintln!("Error running {command}: {e}");
                        Ok(None)
                    }
                }
            }
        }
    }
}

pub fn exit(env: &mut Env) {
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .open(dirs::home_dir().unwrap().join(".radish_history"))
        .unwrap();
    for i in &env.history {
        file.write(format!("{i}\n").as_bytes())
            .expect("Error writing to history on exit");
    }
    process::exit(0);
}

fn cd(args: Vec<String>) {
    let newdir = match args.first() {
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

fn if_statement(env: &mut Env, mut args: Vec<String>) -> Result<Option<Child>, Box<dyn Error>> {
    let cmd = match args.remove(0).as_str() {
        "[" => {
            if args.last().unwrap() == "]" {
                args.pop();
            } else {
                return Err("No matching ] found".into());
            }
            "test".to_string()
        }
        cmd => cmd.to_string(),
    };
    if let Some(mut child) = run(&cmd, args, Stdio::null(), Stdio::null(), env)? {
        env.continue_if = child.wait()?.success();
        Ok(Some(child))
    } else {
        Ok(None)
    }
}
fn elif_statement(env: &mut Env, mut args: Vec<String>) -> Result<Option<Child>, Box<dyn Error>> {
    let cmd = args.remove(0);
    if env.continue_if {
        env.continue_if = false;
    } else if let Some(mut child) = run(&cmd, args, Stdio::null(), Stdio::null(), env)? {
        env.continue_if = child.wait()?.success();
        return Ok(Some(child));
    }
    Ok(None)
}

fn then_statement(env: &mut Env, commands: &String) -> Result<(), Box<dyn Error>> {
    if env.continue_if {
        run_from_string(Cow::Borrowed(commands), env, true, None)?;
    }
    Ok(())
}
fn else_statement(env: &mut Env, commands: &String) -> Result<(), Box<dyn Error>> {
    if !env.continue_if {
        run_from_string(Cow::Borrowed(commands), env, true, None)?;
    }
    Ok(())
}

fn mkloc(locs: &mut HashMap<String, String>, args: Vec<String>) {
    // TODO: clean up code
    if args.len() != 2 {
        eprintln!("Wrong number of arguments provided");
        return;
    }
    let mut args = args.iter();
    if let Some(name) = args.next() {
        locs.insert(name.to_string(), args.next().unwrap().to_string());
    }
}

fn export(args: &str) {
    let mut args = args.split('=');
    env::set_var(args.next().unwrap(), args.next().unwrap());
}
fn alias(aliases: &mut HashMap<String, String>, args: &str) {
    let mut args = args.split('=');
    aliases.insert(
        args.next().unwrap().to_string(),
        args.next().unwrap().to_string(),
    );
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

fn unescape(mut input: String) -> String {
    let replacements = [
        ("\\t", "\t"),
        ("\\x1b", "\x1b"),
        ("\\033", "\x1b"),
        ("\\n", "\n"),
    ];
    for replacement in replacements {
        input = input.replace(replacement.0, replacement.1);
    }
    input
}
