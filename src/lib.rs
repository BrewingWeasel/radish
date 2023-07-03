use crokey::crossterm::{
    cursor::MoveToColumn,
    event::KeyEvent,
    execute,
    style::Print,
    terminal::{disable_raw_mode, enable_raw_mode},
};
use std::{
    borrow::Cow,
    collections::HashMap,
    env::{self, args, current_dir},
    error::Error,
    fs::{self, read_dir, File, OpenOptions},
    io::{stdout, BufRead, BufReader, Write},
    path::{Path, PathBuf},
    process::{self, Child, Command, Stdio},
    str::from_utf8,
};
use tokenizer::{CommandPart, ExtraLines, TokenizedOutput};
use utils::HashOptions;

mod input_reader;
mod tokenizer;
mod utils;

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

#[derive(Debug)]
pub enum Scope {
    Main,
    Function,
    Workspace(String),
}

#[derive(Debug)]
pub struct EnvValues {
    lists: HashMap<String, Vec<String>>,
    locations: HashMap<String, String>,
    aliases: HashMap<String, String>,
    functions: HashMap<String, String>,
    bindings: HashMap<KeyEvent, (bool, String)>,
    shell_variables: HashMap<String, String>,
}

impl EnvValues {
    pub fn new() -> EnvValues {
        EnvValues {
            lists: HashMap::new(),
            functions: HashMap::new(),
            locations: HashMap::new(),
            aliases: HashMap::new(),
            bindings: HashMap::new(),
            shell_variables: HashMap::new(),
        }
    }
}
impl Default for EnvValues {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Env<'a> {
    history: Vec<String>,
    sorted_history: Vec<Cow<'a, String>>,
    dirs: Vec<String>,
    sorted_dirs: Vec<String>,
    dirs_up_to_date: bool,
    commands: Vec<String>,
    prompt_length: u16,
    continue_if: bool,
    settings: EnvValues,
    scope: Scope,
    cur_workspace: String,
    workspaces: HashMap<String, EnvValues>,
    workspace_locs: HashMap<PathBuf, String>,
}

// TODO: macro
impl Env<'_> {
    pub fn get_lists(&mut self) -> HashOptions<String, Vec<String>> {
        if let Scope::Workspace(workspace_name) = &self.scope {
            HashOptions {
                orig: &mut self.workspaces.get_mut(workspace_name).unwrap().lists,
                secondary: None,
            }
        } else {
            let secondary = self
                .workspaces
                .get(&self.cur_workspace)
                .map(|workspace| &workspace.lists);
            HashOptions {
                orig: &mut self.settings.lists,
                secondary,
            }
        }
    }

    pub fn get_bindings(&self) -> HashOptions<KeyEvent, (bool, String)> {
        if let Scope::Workspace(workspace_name) = &self.scope {
            HashOptions {
                orig: &self.workspaces.get(workspace_name).unwrap().bindings,
                secondary: None,
            }
        } else {
            let secondary = self
                .workspaces
                .get(&self.cur_workspace)
                .map(|workspace| &workspace.bindings);
            HashOptions {
                orig: &self.settings.bindings,
                secondary,
            }
        }
    }

    pub fn get_aliases(&self) -> HashOptions<String, String> {
        if let Scope::Workspace(workspace_name) = &self.scope {
            HashOptions {
                orig: &self.workspaces.get(workspace_name).unwrap().aliases,
                secondary: None,
            }
        } else {
            let secondary = self
                .workspaces
                .get(&self.cur_workspace)
                .map(|workspace| &workspace.aliases);
            HashOptions {
                orig: &self.settings.aliases,
                secondary,
            }
        }
    }
}

fn run_from_file(path: PathBuf, env: &mut Env) -> Result<(), Box<dyn Error>> {
    let mut lines = BufReader::new(File::open(path)?).lines();
    while let Some(line) = lines.next() {
        run_from_string(
            Cow::Borrowed(&line.unwrap()),
            env,
            true,
            Some(&mut ExtraLines::File(&mut lines)),
        )?;
    }
    Ok(())
}

fn get_conts_from_file(dir: PathBuf) -> Vec<String> {
    let mut history = vec![];
    if let Ok(file) = File::open(dir) {
        let lines = BufReader::new(file).lines();
        for line in lines {
            history.push(line.unwrap());
        }
        history
    } else {
        vec![]
    }
}

pub fn run_radish() {
    let mut next_cmd = None;

    let history = get_conts_from_file(dirs::home_dir().unwrap().join(".radish_history"));
    let mut dirs = get_conts_from_file(dirs::home_dir().unwrap().join(".radish_dirs"));
    dirs.push(current_dir().unwrap().display().to_string());
    let mut commands = get_all_commands();
    commands.sort_unstable();
    commands.dedup(); // Hacky workaround for when there are multiple of the same file name in path

    let mut new_history = history.clone();
    new_history.sort_unstable();
    new_history.dedup();
    let sorted_history = new_history.iter().map(Cow::Borrowed).collect();

    let mut sorted_dirs = dirs.clone();
    sorted_dirs.sort_unstable();
    sorted_dirs.dedup();

    let mut env = Env {
        history,
        sorted_history,
        dirs_up_to_date: true,
        prompt_length: 3,
        commands,
        dirs,
        sorted_dirs,
        settings: EnvValues::new(),
        continue_if: false,
        scope: Scope::Main,
        cur_workspace: String::new(),
        workspaces: HashMap::new(),
        workspace_locs: HashMap::new(),
    };

    env.settings
        .shell_variables
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
        if let Some(func) = env.settings.functions.get("radish_eval_on_new") {
            let new_func = func.to_owned();
            if let Err(e) = exec_function(&mut env, &new_func, Scope::Function) {
                eprintln!("{e}");
            };
        };
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
        let (input, new_cmd) = input_reader::get_input(&mut env, next_cmd);
        next_cmd = new_cmd;
        disable_raw_mode().unwrap();
        if input.is_empty() {
            continue;
        }
        if let Err(e) = run_from_string(
            Cow::Borrowed(&input),
            &mut env,
            true,
            Some(&mut ExtraLines::Stdin),
        ) {
            eprintln!("{}", e);
        }
        env.history.push(input);
    }
}

fn run_from_string(
    input: Cow<String>,
    env: &mut Env,
    output: bool,
    extra_lines: Option<&mut ExtraLines>,
) -> Result<Option<Child>, Box<dyn Error>> {
    if input.is_empty() || input.starts_with('#') {
        return Ok(None);
    }

    let mut new_input = input.to_string();
    for alias in env.get_aliases().keys() {
        if new_input.starts_with(alias) {
            new_input = new_input.replacen(alias, env.get_aliases().get(alias).unwrap(), 1);
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
            let mut stderr = Stdio::piped();
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
                    CommandPart::ToFileStderr((file_name, append)) => {
                        stderr = if *append {
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
                        };
                        Stdio::inherit()
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

            last_command = run(&command, tokens.to_vec(), stdout, stdin, stderr, env)?;
        }
        if let Some(mut cmd) = last_command {
            env.settings.shell_variables.insert(
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
    stderr: Stdio,
    env: &mut Env,
) -> Result<Option<Child>, Box<dyn Error>> {
    // TODO: probably make there be a
    // struct for this
    match command {
        "cd" => {
            cd(args, env)?;
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
            mklist(env, args);
            Ok(None)
        }
        "alias" => {
            alias(env, args.first().unwrap())?;
            Ok(None)
        }
        "bind" => {
            bind(env, args)?;
            Ok(None)
        }
        "mkworkspace" => {
            mkworkspace(env, args)?;
            Ok(None)
        }
        "export" => {
            export(args.first().unwrap());
            Ok(None)
        }
        "mkloc" => {
            mkloc(&mut env.settings.locations, args);
            Ok(None)
        }
        "function" => {
            mkloc(&mut env.settings.functions, args);
            Ok(None)
        }
        "dirs" => dirs(env, stdin, stdout, stderr),
        "read" => read(stdin, stdout, stderr),
        "list_workspaces" => {
            fake_stdout(stdin, stdout, stderr, &format!("{:?}", env.workspace_locs))
        }
        "list_workspace_values" => {
            fake_stdout(stdin, stdout, stderr, &format!("{:?}", env.workspaces))
        }
        "cur_workspace" => fake_stdout(stdin, stdout, stderr, &env.cur_workspace),
        "workspace" => exec_function(
            env,
            args.last().ok_or("No arguments provided for workspace")?,
            Scope::Workspace(args.first().unwrap().to_string()),
        ),
        "exit" => {
            exit(env);
            unreachable!()
        }
        "return" => {
            if let Scope::Function = env.scope {
                env.settings
                    .shell_variables
                    .insert(String::from("?"), args.first().unwrap().to_string());

                // Hack to stop executing
                Err("".into())
            } else {
                eprintln!("Can only return from a function");
                Ok(None)
            }
        }
        command => {
            if let Some(contents) = env.settings.functions.get(command) {
                exec_function(env, &contents.to_owned(), Scope::Function)
            } else {
                match Command::new(command)
                    .args(args)
                    .stdin(stdin)
                    .stdout(stdout)
                    .stderr(stderr)
                    // .stderr(Stdio::null())
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

fn fake_stdout(
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
    output: &str,
) -> Result<Option<Child>, Box<dyn Error>> {
    match Command::new("echo") // Hacky workaround so it can be piped
        .args(vec![output])
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr)
        .spawn()
    {
        Ok(output) => Ok(Some(output)),
        Err(e) => {
            eprintln!("Error running dirs: {e}");
            Ok(None)
        }
    }
}

fn dirs(
    env: &Env,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
) -> Result<Option<Child>, Box<dyn Error>> {
    fake_stdout(stdin, stdout, stderr, &env.dirs.join("\n"))
}

fn read(stdin: Stdio, stdout: Stdio, stderr: Stdio) -> Result<Option<Child>, Box<dyn Error>> {
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer)?;
    fake_stdout(stdin, stdout, stderr, &buffer)
}

fn write_to_file(file_name: &str, conts: &Vec<String>) {
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .open(dirs::home_dir().unwrap().join(file_name))
        .unwrap();
    for i in conts {
        file.write_all(format!("{i}\n").as_bytes())
            .expect("Error writing to history on exit");
    }
}

pub fn exit(env: &mut Env) {
    write_to_file(".radish_history", &env.history);
    write_to_file(".radish_dirs", &env.dirs);
    process::exit(0);
}

fn cd(args: Vec<String>, env: &mut Env) -> Result<(), Box<dyn Error>> {
    let newdir = match args.first() {
        None => dirs::home_dir().unwrap(),
        Some(dir) => {
            if dir == "-" {
                Path::new(env.dirs.get(env.dirs.len() - 2).unwrap()).into()
            } else {
                Path::new(dir).into()
            }
        }
    };
    env::set_current_dir(newdir)?;
    if let Some(val) = env.workspace_locs.get(&env::current_dir()?) {
        env.cur_workspace = val.to_string();
    } else {
        env.cur_workspace = String::new();
    }
    let new_dir_name = current_dir().unwrap().display().to_string();
    if !env.sorted_dirs.contains(&new_dir_name) {
        env.dirs_up_to_date = false;
    }
    env.dirs.push(new_dir_name);
    Ok(())
}

fn mklist(env: &mut Env, args: Vec<String>) {
    let mut args = args.iter();
    if let Some(first) = args.next() {
        if let Scope::Workspace(workspace_name) = &env.scope {
            env.workspaces
                .get_mut(workspace_name)
                .unwrap()
                .lists
                .insert(first.to_string(), args.map(|x| x.to_string()).collect());
        } else {
            env.settings
                .lists
                .insert(first.to_string(), args.map(|x| x.to_string()).collect());
        }
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
    if let Some(mut child) = run(
        &cmd,
        args,
        Stdio::null(),
        Stdio::null(),
        Stdio::inherit(),
        env,
    )? {
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
    } else if let Some(mut child) = run(
        &cmd,
        args,
        Stdio::null(),
        Stdio::null(),
        Stdio::inherit(),
        env,
    )? {
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
fn alias(env: &mut Env, args: &str) -> Result<(), Box<dyn Error>> {
    let mut args = args.split('=');
    let (name, value) = (
        args.next().unwrap().to_string(),
        args.next().unwrap().to_string(),
    );

    if let Scope::Workspace(workspace_name) = &env.scope {
        env.workspaces
            .get_mut(workspace_name)
            .ok_or("Workspace does not exist")?
            .aliases
            .insert(name, value);
    } else {
        env.settings.aliases.insert(name, value);
    }
    Ok(())
}

fn bind(env: &mut Env, mut args: Vec<String>) -> Result<(), Box<dyn Error>> {
    let reset = if args[0] == "-x" {
        args.remove(0);
        true
    } else {
        false
    };
    let mut args = args[0].split(':');
    let keycode = args.next().ok_or(InvalidItemError)?;

    let command = args.next().unwrap().to_string();

    if let Scope::Workspace(workspace_name) = &env.scope {
        env.workspaces
            .get_mut(workspace_name)
            .ok_or("Workspace does not exist")?
            .bindings
            .insert(crokey::parse(keycode)?, (reset, command));
    } else {
        env.settings
            .bindings
            .insert(crokey::parse(keycode)?, (reset, command));
    }
    Ok(())
}

fn mkworkspace(env: &mut Env, mut args: Vec<String>) -> Result<(), Box<dyn Error>> {
    if args.len() != 2 {
        return Err("Wrong number of arguments".into());
    }
    let dir = PathBuf::from(&args[0]);
    let workspace_name = args.pop().unwrap();
    env.workspaces
        .insert(workspace_name.clone(), EnvValues::new());
    env.workspace_locs
        .insert(fs::canonicalize(dir)?, workspace_name);
    Ok(())
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

fn exec_function(
    env: &mut Env,
    contents: &str,
    scope: Scope,
) -> Result<Option<Child>, Box<dyn Error>> {
    let new_contents = contents
        .strip_prefix('{')
        .unwrap()
        .trim_matches(';')
        .strip_suffix('}')
        .unwrap()
        .to_string();
    env.scope = scope;
    let last_cmd = run_from_string(Cow::Borrowed(&new_contents), env, true, None)?;
    env.scope = Scope::Main;
    Ok(last_cmd)
}
