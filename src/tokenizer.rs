use glob::glob;
use owned_chars::OwnedChars;
use std::{
    env,
    error::Error,
    fs::File,
    io::{BufReader, Lines, Read},
};

// TODO: come up with a better name
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CommandPart {
    Command(Vec<String>),
    ToFile((String, bool)),
    FromFile(String),
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
}

// TODO: Clean up
pub fn parse_input(
    input: &str,
    env: &mut crate::Env,
    mut extra_lines: Option<&mut Lines<BufReader<File>>>,
    // extra_lines: &mut impl Iterator<Item = String>,
    // },
) -> Result<TokenizedOutput, Box<dyn Error>> {
    let mut in_quotes = false;
    let mut in_glob_pattern = false;
    let mut commands: Vec<Vec<CommandPart>> =
        vec![vec![CommandPart::Command(vec![String::from("")])]];
    let mut chars = OwnedChars::from_string(input.trim_start().to_owned()).peekable();
    let mut commandpart_index = 0;
    let mut current_token_index = 1;
    let mut replacement: Vec<Vec<ReplacementInfo>> = vec![vec![]];

    loop {
        let c = chars.next();
        if (c == Some(' ') || c.is_none()) && !in_quotes && in_glob_pattern {
            if let CommandPart::Command(args) = commands.last().unwrap().last().unwrap() {
                let glob_pattern = args.last().unwrap().clone();
                if let CommandPart::Command(cmd) = commands.last_mut().unwrap().last_mut().unwrap()
                {
                    cmd.pop();
                    for entry in glob(&glob_pattern)?.flatten() {
                        cmd.push(entry.display().to_string())
                    }
                }
            }
            in_glob_pattern = false;
        }
        let last_str = match commands.last_mut().unwrap().last_mut().unwrap() {
            CommandPart::Command(args) => args.last_mut().unwrap(),
            CommandPart::ToFile((name, _)) => name,
            CommandPart::FromFile(name) => name,
        };
        if last_str == "then" {
            let mut contents = vec![String::new()];
            let mut last_cmd = String::new();
            let mut block_in_quotes = false;
            let mut block_in_single_quotes = false;
            loop {
                if chars.peek().is_none() {
                    let next_line = match extra_lines {
                        Some(ref mut lines) => {
                            lines.next().ok_or("Expected another line")?.unwrap()
                        }
                        None => {
                            return Err("Expected more input".into());
                        }
                    };
                    chars = OwnedChars::from_string(next_line).peekable();
                }
                let c = match chars.next() {
                    Some(c) => c,
                    None => {
                        break;
                    }
                };
                let last_contents = contents.last_mut().unwrap();
                if block_in_single_quotes && c == '\'' {
                    block_in_single_quotes = false;
                    last_contents.push('\'');
                    continue;
                }
                match c {
                    '\'' => block_in_single_quotes = true,
                    '\\' => {
                        last_contents.push(chars.next().unwrap());
                    }
                    '"' => block_in_quotes = !block_in_quotes,
                    ';' | '\n' => last_cmd = String::new(),
                    _ => (),
                };
                last_contents.push(c);
                if !c.is_whitespace() && c != ';' && last_cmd.len() < 4 {
                    last_cmd.push(c);
                }
                if !block_in_single_quotes && !block_in_quotes {
                    match last_cmd.as_str() {
                        "fi" => {
                            *last_contents = last_contents
                                .strip_suffix("fi")
                                .unwrap()
                                .trim_end()
                                .strip_suffix(";")
                                .unwrap()
                                .to_string();
                            break;
                        }
                        "elif" => {
                            *last_contents = last_contents
                                .strip_suffix("elif")
                                .unwrap()
                                .trim_end()
                                .strip_suffix(";")
                                .unwrap()
                                .to_string();
                            contents.push(String::from("elif"));
                            break;
                        }
                        "else" => {
                            *last_contents = last_contents
                                .strip_suffix("else")
                                .unwrap()
                                .trim_end()
                                .strip_suffix(";")
                                .unwrap()
                                .to_string();
                            last_cmd = String::new();
                            contents.push(String::from("else"));
                        }
                        _ => (),
                    }
                }
            }
            if let CommandPart::Command(cmd) = commands.last_mut().unwrap().last_mut().unwrap() {
                cmd.push(contents.first().unwrap().to_string());
            }
            for branch in contents[1..].into_iter() {
                commands.last_mut().unwrap().push(CommandPart::Command(
                    branch.splitn(2, " ").map(|x| x.to_string()).collect(),
                ));
            }
            continue;
        }
        if c.is_none() {
            break;
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
                        let mut output = crate::run_from_string(new_cmd, env, false, None)?
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
                    if ['"', ' ', '$', '~', '*', '@', '%', '&', '\\']
                        .contains(chars.peek().ok_or("No character after \\!")?)
                    {
                        last_str.push(chars.next().unwrap());
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
                        commands
                            .last_mut()
                            .unwrap()
                            .push(CommandPart::Command(vec![String::from("")]));
                        chars.next(); // Hacky work around to not treat the space after a pipe as a
                                      // command
                        commandpart_index += 1;
                        current_token_index = 1;
                    }
                    ' ' => {
                        if [Some(&' '), None, Some(&'<'), Some(&'>'), Some(&'|')]
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
                        last_str.push_str(env.locations.get(&name).ok_or(crate::InvalidItemError)?)
                    }
                    '*' => {
                        in_glob_pattern = true;
                        last_str.push('*');
                    }
                    '%' => {
                        let mut name = String::new();
                        let is_glob_list = if chars.peek() == Some(&'%') {
                            chars.next();
                            true
                        } else {
                            false
                        };
                        while let Some(digit) = chars.by_ref().next_if(|c| *c != ' ' && *c != '\n')
                        {
                            name.push(digit)
                        }
                        let mut new_replacement = vec![];
                        for pattern in replacement {
                            let list_with_replacements: Vec<String> = if is_glob_list {
                                let mut paths: Vec<String> = vec![];
                                for i in glob(&name)? {
                                    paths.push(i?.display().to_string());
                                }
                                paths
                            } else {
                                env.lists
                                    .get(&name)
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
                        continue;
                    }
                    '&' => {
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
                        continue;
                    }
                    '>' => {
                        commands
                            .last_mut()
                            .unwrap()
                            .push(CommandPart::ToFile((String::new(), false)));
                        if chars.next() == Some('>') {
                            if let CommandPart::ToFile(options) =
                                commands.last_mut().unwrap().last_mut().unwrap()
                            {
                                options.1 = true;
                            }
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
                            chars.next();
                        }
                    }
                    _ => last_str.push(i),
                }
            } else {
                last_str.push(i);
            }
        }
    }
    // Ok((commands, replacement))
    Ok(TokenizedOutput {
        commands,
        replacements: replacement,
    })
}
