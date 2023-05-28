use glob::glob;
use owned_chars::OwnedChars;
use std::{
    borrow::Cow,
    env,
    error::Error,
    fs::File,
    io::{BufReader, Lines, Read},
    iter::Peekable,
};

// TODO: come up with a better name
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CommandPart {
    Command(Vec<String>),
    ToFile((String, bool)),
    ToFileStderr((String, bool)),
    FromFile(String),
    Or,
    And,
}

impl CommandPart {
    fn unwrap_command_mut(&mut self) -> &mut Vec<String> {
        if let CommandPart::Command(args) = self {
            args
        } else {
            panic!("Tried to parse a value other than command")
        }
    }
}

#[derive(Clone, Debug)]
pub struct ReplacementInfo {
    pub command_number: usize,
    pub command_part_number: usize,
    pub token_number: usize,
    pub replacement: String,
}

#[derive(Debug)]
pub struct TokenizedOutput {
    pub commands: Vec<Vec<CommandPart>>,
    pub replacements: Vec<Vec<ReplacementInfo>>,
    pub variable_assignment: Option<(String, String)>,
}

enum ListType {
    Regular,
    GlobbedList,
}

enum CurrentlyTokenizing {
    Arg,
    GlobPattern,
    List(ListType),
    VariableAssignment(usize),
    Function,
}

// TODO: Clean up
pub fn parse_input(
    input: &str,
    env: &mut crate::Env,
    mut extra_lines: Option<&mut Lines<BufReader<File>>>,
) -> Result<TokenizedOutput, Box<dyn Error>> {
    let mut in_quotes = false;
    let mut variable_assignment = None;
    let mut currently_tokenizing = CurrentlyTokenizing::Arg;
    let mut commands: Vec<Vec<CommandPart>> =
        vec![vec![CommandPart::Command(vec![String::from("")])]];
    let mut chars = OwnedChars::from_string(input.trim_start().to_owned()).peekable();
    let mut commandpart_index = 0;
    let mut current_token_index = 1;
    let mut replacement: Vec<Vec<ReplacementInfo>> = vec![vec![]];
    let mut analyze_next = false;

    loop {
        let c = chars.next();
        if (c == Some(' ') || c.is_none() || analyze_next) && !in_quotes {
            match currently_tokenizing {
                CurrentlyTokenizing::GlobPattern => {
                    if let CommandPart::Command(args) = commands.last().unwrap().last().unwrap() {
                        let glob_pattern = args.last().unwrap().clone();
                        if let CommandPart::Command(cmd) =
                            commands.last_mut().unwrap().last_mut().unwrap()
                        {
                            cmd.pop();
                            for entry in glob(&glob_pattern)?.flatten() {
                                cmd.push(entry.display().to_string())
                            }
                        }
                    }
                }
                CurrentlyTokenizing::List(list_type) => {
                    let mut new_replacement = vec![];
                    let last_cmd = commands.last_mut().unwrap().last_mut().unwrap();
                    let last_str = last_cmd.unwrap_command_mut().pop().unwrap();
                    last_cmd.unwrap_command_mut().push(String::new());
                    for pattern in replacement {
                        let list_with_replacements: Vec<String> =
                            if matches!(list_type, ListType::GlobbedList) {
                                let mut paths: Vec<String> = vec![];
                                for i in glob(&last_str)? {
                                    paths.push(i?.display().to_string());
                                }
                                paths
                            } else {
                                env.lists
                                    .get(&last_str)
                                    .ok_or(crate::InvalidItemError)?
                                    .to_vec()
                            };
                        for item in list_with_replacements {
                            let mut replacement_pattern = pattern.clone();
                            replacement_pattern.push(ReplacementInfo {
                                command_number: commands.len() - 1,
                                command_part_number: commandpart_index,
                                token_number: current_token_index,
                                replacement: item.to_string(),
                            });
                            new_replacement.push(replacement_pattern);
                        }
                    }
                    replacement = new_replacement;
                }
                CurrentlyTokenizing::VariableAssignment(place) => {
                    let last_cmd = commands.last_mut().unwrap().last_mut().unwrap();
                    let last_str = last_cmd.unwrap_command_mut().pop().unwrap();
                    let (variable_name, variable_value) = last_str.split_at(place);
                    variable_assignment =
                        Some((variable_name.to_string(), variable_value.to_string()));
                    if chars.peek().is_none() {
                        break;
                    }
                    commands
                        .last_mut()
                        .unwrap()
                        .last_mut()
                        .unwrap()
                        .unwrap_command_mut()
                        .push(String::new());
                    currently_tokenizing = CurrentlyTokenizing::Arg;
                    continue;
                }
                CurrentlyTokenizing::Function => {
                    let (contents, new_chars) =
                        multiline_loop_parsing(chars, &mut extra_lines, func_specific_parsing)?;
                    chars = new_chars;
                    if let CommandPart::Command(cmd) =
                        commands.last_mut().unwrap().last_mut().unwrap()
                    {
                        cmd.insert(0, String::from("function"));
                        cmd.push(contents.first().unwrap().to_string());
                    }
                    for branch in contents[1..].iter() {
                        commands.last_mut().unwrap().push(CommandPart::Command(
                            branch.splitn(2, ' ').map(|x| x.to_string()).collect(),
                        ));
                    }
                    currently_tokenizing = CurrentlyTokenizing::Arg;
                    analyze_next = false;
                    continue;
                }
                _ => (),
            }
            currently_tokenizing = CurrentlyTokenizing::Arg;
        }
        let last_str = match commands.last_mut().unwrap().last_mut().unwrap() {
            CommandPart::Command(args) => args.last_mut().unwrap(),
            CommandPart::ToFile((name, _)) => name,
            CommandPart::ToFileStderr((name, _)) => name,
            CommandPart::FromFile(name) => name,
            _ => unreachable!(),
        };
        match last_str.as_str() {
            "then" => {
                let (contents, new_chars) =
                    multiline_loop_parsing(chars, &mut extra_lines, if_specific_parsing)?;
                chars = new_chars;
                if let CommandPart::Command(cmd) = commands.last_mut().unwrap().last_mut().unwrap()
                {
                    cmd.push(contents.first().unwrap().to_string());
                }
                for branch in contents[1..].iter() {
                    commands.last_mut().unwrap().push(CommandPart::Command(
                        branch.splitn(2, ' ').map(|x| x.to_string()).collect(),
                    ));
                }
                continue;
            }
            "function" => {
                commands
                    .last_mut()
                    .unwrap()
                    .last_mut()
                    .unwrap()
                    .unwrap_command_mut()
                    .pop();
                let mut function_name = String::new();
                while let Some(c) = chars.peek() {
                    if c.is_whitespace() {
                        break;
                    }
                    function_name.push(chars.next().unwrap());
                }
                commands
                    .last_mut()
                    .unwrap()
                    .last_mut()
                    .unwrap()
                    .unwrap_command_mut()
                    .push(function_name);
                currently_tokenizing = CurrentlyTokenizing::Function;
                analyze_next = true;
                continue;
            }
            _ => (),
        }
        if let Some(i) = c {
            match i {
                '"' => {
                    in_quotes = !in_quotes;
                    continue;
                }
                '$' => {
                    if let Some(&'(') = chars.peek() {
                        chars.next();
                        let new_cmd = &chars.by_ref().take_while(|x| *x != ')').collect::<String>();
                        let mut output =
                            crate::run_from_string(Cow::Borrowed(new_cmd), env, false, None)?
                                .ok_or("running command failed")?
                                .stdout
                                .ok_or("No stdout from command")?;
                        let mut buffer = String::new();
                        output.read_to_string(&mut buffer)?;
                        let final_output = buffer.trim_end();
                        last_str.push_str(final_output);
                        continue;
                    } else {
                        let mut variable_name = String::new();
                        while let Some(digit) = chars
                            .by_ref()
                            .next_if(|c| *c != ' ' && *c != '"' && *c != '/' && *c != '\n')
                        {
                            variable_name.push(digit)
                        }
                        let val = match env.shell_variables.get(&variable_name) {
                            Some(v) => v.to_string(),
                            None => env::var(variable_name)?,
                        };
                        last_str.push_str(&val);
                        continue;
                    }
                }
                '\\' => {
                    if let Some(c) = chars.peek() {
                        if ['"', ' ', '$', '~', '*', '@', '%', '&', '\\'].contains(c) {
                            last_str.push(chars.next().unwrap());
                            continue;
                        }
                    } else {
                        let next_line = match extra_lines {
                            Some(ref mut lines) => {
                                lines.next().ok_or("Expected another line")?.unwrap()
                            }
                            None => {
                                return Err("Expected more input".into());
                            }
                        };
                        chars = OwnedChars::from_string(next_line).peekable();
                        continue;
                    }
                }

                _ => (),
            }
            if !in_quotes {
                match i {
                    '~' => last_str.push_str(
                        &dirs::home_dir()
                            .unwrap()
                            .into_os_string()
                            .into_string()
                            .unwrap(),
                    ),
                    '|' => {
                        if chars.peek() == Some(&'|') {
                            commands.last_mut().unwrap().push(CommandPart::Or);
                            logical_operators(&mut chars, &mut commands)?;
                        } else {
                            commands
                                .last_mut()
                                .unwrap()
                                .push(CommandPart::Command(vec![String::from("")]));
                            chars.next();
                        }
                        commandpart_index += 1;
                        current_token_index = 1;
                    }
                    ' ' => {
                        if [
                            Some(&' '),
                            None,
                            Some(&'<'),
                            Some(&'>'),
                            Some(&'|'),
                            Some(&'&'),
                        ]
                        .contains(&chars.peek())
                        {
                            continue;
                        }
                        if let CommandPart::Command(cmd) =
                            commands.last_mut().unwrap().last_mut().unwrap()
                        {
                            cmd.push(String::from(""));
                        }
                        current_token_index += 1;
                    }
                    '@' => {
                        let name = chars
                            .by_ref()
                            .take_while(|x| *x != '\n' && *x != ' ')
                            .collect::<String>();
                        let loc_val = if let Some(l) = env.locations.get(&name) {
                            Ok(l)
                        } else {
                            if !env.dirs_up_to_date {
                                let mut new_dirs = env.dirs.clone();
                                new_dirs.sort_unstable();
                                new_dirs.dedup();
                                env.sorted_dirs = new_dirs;
                            } else {
                            }
                            let mut dir_selected = None;
                            let mut has_selection_been_made = false;
                            for dir_name in &env.sorted_dirs {
                                let ending_name = format!("/{}", name);
                                if dir_name.ends_with(&ending_name) {
                                    if has_selection_been_made {
                                        dir_selected = None;
                                    } else {
                                        dir_selected = Some(dir_name);
                                        has_selection_been_made = true;
                                    }
                                }
                            }
                            if let Some(dir) = dir_selected {
                                Ok(dir)
                            } else {
                                Err(crate::InvalidItemError)
                            }
                        }?;
                        last_str.push_str(loc_val);
                    }
                    '*' => {
                        currently_tokenizing = CurrentlyTokenizing::GlobPattern;
                        last_str.push('*');
                    }
                    '%' => {
                        let is_glob_list = if chars.peek() == Some(&'%') {
                            chars.next();
                            ListType::GlobbedList
                        } else {
                            ListType::Regular
                        };
                        currently_tokenizing = CurrentlyTokenizing::List(is_glob_list);
                        continue;
                    }
                    '&' => {
                        current_token_index += 1;
                        if chars.peek() == Some(&'&') {
                            commands.last_mut().unwrap().push(CommandPart::And);
                            logical_operators(&mut chars, &mut commands)?;
                            commandpart_index += 1;
                            current_token_index = 1;
                            continue;
                        }

                        if let CommandPart::Command(cmd) =
                            commands.last_mut().unwrap().last_mut().unwrap()
                        {
                            cmd.push(String::from(""));
                        }

                        let mut reference_index = String::new();
                        while let Some(digit) = chars.by_ref().next_if(|c| c.is_ascii_digit()) {
                            reference_index.push(digit)
                        }
                        let reference_index: usize = reference_index.parse().unwrap();

                        for old_replacement in replacement.iter_mut() {
                            let original_str = old_replacement[reference_index].replacement.clone();
                            old_replacement.push(ReplacementInfo {
                                command_number: commands.len() - 1,
                                command_part_number: commandpart_index,
                                token_number: current_token_index,
                                replacement: original_str,
                            });
                        }
                    }
                    '>' => {
                        if last_str.ends_with('2') {
                            let cur_commands = commands.last_mut().unwrap();
                            cur_commands
                                .last_mut()
                                .unwrap()
                                .unwrap_command_mut()
                                .last_mut()
                                .unwrap()
                                .pop();
                            cur_commands.push(CommandPart::ToFileStderr((String::new(), false)));
                        } else {
                            commands
                                .last_mut()
                                .unwrap()
                                .push(CommandPart::ToFile((String::new(), false)));
                        }
                        match chars.next() {
                            Some('>') => {
                                if let CommandPart::ToFile(options) =
                                    commands.last_mut().unwrap().last_mut().unwrap()
                                {
                                    options.1 = true;
                                } else if let CommandPart::ToFileStderr(options) =
                                    commands.last_mut().unwrap().last_mut().unwrap()
                                {
                                    options.1 = true;
                                }
                            }
                            Some('!') => {
                                let val = match commands.last_mut().unwrap().last_mut().unwrap() {
                                    CommandPart::ToFile((name, _)) => name,
                                    CommandPart::ToFileStderr((name, _)) => name,
                                    _ => unreachable!(),
                                };
                                *val = String::from("/dev/null");
                                continue;
                            }
                            _ => (),
                        }
                        commandpart_index += 1;
                        current_token_index = 1;
                    }
                    '<' => {
                        commands
                            .last_mut()
                            .unwrap()
                            .push(CommandPart::FromFile(String::new()));
                        chars.next();
                        commandpart_index += 1;
                        current_token_index = 1;
                    }
                    '#' => {
                        if last_str.is_empty() {
                            if let CommandPart::Command(cmd) =
                                commands.last_mut().unwrap().last_mut().unwrap()
                            {
                                cmd.pop();
                            }
                            break;
                        }
                        last_str.push('#')
                    }
                    '\'' => {
                        last_str.push_str(
                            &chars
                                .by_ref()
                                .take_while(|x| *x != '\'')
                                .collect::<String>(),
                        );
                    }
                    ';' => {
                        if chars.peek().is_some() {
                            commands.push(vec![CommandPart::Command(vec![String::from("")])]);
                            if chars.peek().unwrap().is_whitespace() {
                                chars.next();
                            }
                        }
                    }
                    '=' => {
                        if current_token_index == 1 {
                            currently_tokenizing =
                                CurrentlyTokenizing::VariableAssignment(last_str.len())
                        } else {
                            last_str.push('=');
                        }
                    }
                    ')' => {
                        if current_token_index == 2 && last_str == "(" {
                            currently_tokenizing = CurrentlyTokenizing::Function;
                            commands
                                .last_mut()
                                .unwrap()
                                .last_mut()
                                .unwrap()
                                .unwrap_command_mut()
                                .pop();
                        } else {
                            last_str.push(')');
                        }
                    }
                    _ => last_str.push(i),
                }
            } else {
                last_str.push(i);
            }
        } else {
            break;
        }
    }
    // Ok((commands, replacement))
    Ok(TokenizedOutput {
        commands,
        variable_assignment,
        replacements: replacement,
    })
}

fn logical_operators(
    chars: &mut Peekable<OwnedChars>,
    commands: &mut [Vec<CommandPart>],
) -> Result<(), Box<dyn Error>> {
    chars.next();
    let first_cmd = if let CommandPart::Command(args) = commands.last().unwrap().first().unwrap() {
        args.first().unwrap()
    } else {
        return Err("&& or || was used on file".into());
    };
    let new_cmd = if ["if", "elif"].contains(&first_cmd.as_str()) {
        vec![first_cmd.into(), String::new()]
    } else {
        vec![String::new()]
    };
    commands
        .last_mut()
        .unwrap()
        .push(CommandPart::Command(new_cmd));
    chars.next();
    Ok(())
}

fn check_for_new_line(
    chars: &mut Peekable<OwnedChars>,
    extra_lines: &mut Option<&mut Lines<BufReader<File>>>,
    add_semicolon: &bool,
    contents: &mut [String],
) -> Result<Option<Peekable<OwnedChars>>, Box<dyn Error>> {
    if chars.peek().is_none() {
        let next_line = match extra_lines {
            Some(ref mut lines) => lines.next().ok_or("Expected another line")?.unwrap(),
            None => {
                return Err("Expected more input".into());
            }
        };
        if *add_semicolon {
            contents.last_mut().unwrap().push(';');
        } else {
            contents.last_mut().unwrap().push(' ');
        }
        let mut new_chars = OwnedChars::from_string(next_line).peekable();
        loop {
            if new_chars.next_if(|c| c.is_whitespace()).is_none() {
                break;
            }
        }
        Ok(Some(new_chars))
    } else {
        Ok(None)
    }
}

fn trim_block_keyword(keyword: &str, last_contents: &mut str) -> String {
    last_contents
        .strip_suffix(keyword)
        .unwrap()
        .trim_end()
        .strip_suffix(';')
        .unwrap()
        .to_string()
}

enum ActionToTake {
    Break,
    Continue,
    None,
}

struct MultilineVariables {
    block_in_quotes: bool,
    block_in_single_quotes: bool,
    add_semicolon: bool,
    last_cmd: String,
}

fn parse_char_multiline_statement(
    contents: &mut Vec<String>,
    chars: &mut Peekable<OwnedChars>,
    mut parsing_vars: MultilineVariables,
    specific_parsing: fn(
        MultilineVariables,
        &mut Vec<String>,
    ) -> (ActionToTake, MultilineVariables),
) -> (ActionToTake, MultilineVariables) {
    let last_contents = contents.last_mut().unwrap();
    // let mut new_last_cmd = Some(*last_cmd);
    parsing_vars.add_semicolon = true;
    let c = match chars.next() {
        Some(c) => c,
        None => {
            return (ActionToTake::Break, parsing_vars);
        }
    };
    if parsing_vars.block_in_single_quotes && c == '\'' {
        parsing_vars.block_in_single_quotes = false;
        last_contents.push('\'');
        return (ActionToTake::Continue, parsing_vars);
    }
    match c {
        '\'' => parsing_vars.block_in_single_quotes = true,
        '\\' => {
            last_contents.push(chars.next().unwrap());
        }
        '"' => parsing_vars.block_in_quotes = !parsing_vars.block_in_quotes,
        ';' | '\n' => {
            parsing_vars.last_cmd = String::new();
        }
        _ => (),
    };
    last_contents.push(c);
    if !c.is_whitespace() && c != ';' && parsing_vars.last_cmd.len() < 4 {
        parsing_vars.last_cmd.push(c);
    }
    specific_parsing(parsing_vars, contents)
}

fn func_specific_parsing(
    parsing_vars: MultilineVariables,
    _contents: &mut Vec<String>,
) -> (ActionToTake, MultilineVariables) {
    if !parsing_vars.block_in_single_quotes
        && !parsing_vars.block_in_quotes
        && parsing_vars.last_cmd.as_str() == "}"
    {
        (ActionToTake::Break, parsing_vars)
    } else {
        (ActionToTake::None, parsing_vars)
    }
}

fn if_specific_parsing(
    mut parsing_vars: MultilineVariables,
    contents: &mut Vec<String>,
) -> (ActionToTake, MultilineVariables) {
    let last_contents = contents.last_mut().unwrap();
    if !parsing_vars.block_in_single_quotes && !parsing_vars.block_in_quotes {
        match parsing_vars.last_cmd.as_str() {
            "fi" => {
                *last_contents = trim_block_keyword("fi", last_contents);
                return (ActionToTake::Break, parsing_vars);
            }
            "elif" => {
                *last_contents = trim_block_keyword("elif", last_contents);
                contents.push(String::from("elif"));
                return (ActionToTake::Break, parsing_vars);
            }
            "else" => {
                *last_contents = trim_block_keyword("else", last_contents);
                parsing_vars.last_cmd = String::new();
                contents.push(String::from("else"));
                parsing_vars.add_semicolon = false;
            }
            _ => (),
        }
    }
    (ActionToTake::None, parsing_vars)
}

fn multiline_loop_parsing(
    mut chars: Peekable<OwnedChars>,
    extra_lines: &mut Option<&mut Lines<BufReader<File>>>,
    specific_parsing: fn(
        MultilineVariables,
        &mut Vec<String>,
    ) -> (ActionToTake, MultilineVariables),
) -> Result<(Vec<String>, Peekable<OwnedChars>), Box<dyn Error>> {
    let mut contents = vec![String::new()];
    let mut parsing_vars = MultilineVariables {
        add_semicolon: false,
        block_in_single_quotes: false,
        block_in_quotes: false,
        last_cmd: String::new(),
    };
    loop {
        if let Some(new_chars) = check_for_new_line(
            &mut chars,
            extra_lines,
            &parsing_vars.add_semicolon,
            &mut contents,
        )? {
            chars = new_chars;
            parsing_vars.last_cmd = String::new();
        }
        let (action, new_parsing_vars) = parse_char_multiline_statement(
            &mut contents,
            &mut chars,
            parsing_vars,
            specific_parsing,
        );
        parsing_vars = new_parsing_vars;
        match action {
            ActionToTake::Break => break,
            ActionToTake::Continue => continue,
            _ => (),
        }
    }
    Ok((contents, chars))
}
