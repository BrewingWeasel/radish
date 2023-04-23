use glob::glob;
use std::{env, error::Error, io::Read};

// TODO: come up with a better name
#[derive(Clone, Debug)]
pub enum CommandPart {
    Command(Vec<String>),
    ToFile((String, bool)),
    FromFile(String),
}

// TODO: Clean up
pub fn parse_input(
    input: &str,
    env: &mut crate::Env,
) -> Result<(Vec<CommandPart>, Vec<Vec<(usize, usize, String)>>), Box<dyn Error>> {
    let mut in_quotes = false;
    let mut in_glob_pattern = false;
    let mut commands: Vec<CommandPart> = vec![CommandPart::Command(vec![String::from("")])];
    let mut chars = input.chars().peekable();
    let mut commandpart_index = 0;
    let mut current_token_index = 1;
    let mut replacement: Vec<Vec<(usize, usize, String)>> = vec![vec![]];

    loop {
        let c = chars.next();
        if (c == Some(' ') || c.is_none()) && !in_quotes && in_glob_pattern {
            if let CommandPart::Command(args) = commands.last().unwrap() {
                let glob_pattern = args.last().unwrap().clone();
                if let CommandPart::Command(cmd) = commands.last_mut().unwrap() {
                    cmd.pop();
                    for entry in glob(&glob_pattern)?.flatten() {
                        cmd.push(entry.display().to_string())
                    }
                }
            }
            in_glob_pattern = false;
        }
        if c.is_none() {
            break;
        }
        if let Some(i) = c {
            let last_str = match commands.last_mut().unwrap() {
                CommandPart::Command(args) => args.last_mut().unwrap(),
                CommandPart::ToFile((name, _)) => name,
                CommandPart::FromFile(name) => name,
            };
            match i {
                '"' => {
                    in_quotes = !in_quotes;
                    continue;
                }
                '$' => {
                    if let Some(&'(') = chars.peek() {
                        chars.next();
                        let new_cmd = &chars.by_ref().take_while(|x| *x != ')').collect::<String>();
                        let mut output = crate::run_from_string(new_cmd, env, false)?
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
                        while let Some(digit) = chars.by_ref().next_if(|c| *c != ' ' && *c != '\n')
                        {
                            variable_name.push(digit)
                        }
                        last_str.push_str(&env::var(variable_name).unwrap());
                        continue;
                    }
                }
                '\\' => {
                    last_str.push(chars.next().unwrap());
                    continue;
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
                        commands.push(CommandPart::Command(vec![String::from("")]));
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
                        if let CommandPart::Command(cmd) = commands.last_mut().unwrap() {
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
                        while let Some(digit) = chars.by_ref().next_if(|c| *c != ' ' && *c != '\n')
                        {
                            name.push(digit)
                        }
                        let mut new_replacement = vec![];
                        for pattern in replacement {
                            for item in env.lists.get(&name).ok_or(crate::InvalidItemError)? {
                                let mut replacement_pattern = pattern.clone();
                                replacement_pattern.push((
                                    commandpart_index,
                                    current_token_index,
                                    item.to_string(),
                                ));
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
                            let original_str = old_replacement[reference_index].2.clone();
                            old_replacement.push((
                                commandpart_index,
                                current_token_index,
                                original_str,
                            ))
                        }
                        continue;
                    }
                    '>' => {
                        commands.push(CommandPart::ToFile((String::new(), false)));
                        if chars.next() == Some('>') {
                            if let CommandPart::ToFile(options) = commands.last_mut().unwrap() {
                                options.1 = true;
                            }
                        }
                        commandpart_index += 1;
                        current_token_index = 1;
                    }
                    '<' => {
                        commands.push(CommandPart::FromFile(String::new()));
                        chars.next();
                        commandpart_index += 1;
                        current_token_index = 1;
                    }
                    '#' => {
                        if last_str == "" {
                            if let CommandPart::Command(cmd) = commands.last_mut().unwrap() {
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
                    _ => last_str.push(i),
                }
            } else {
                last_str.push(i);
            }
        }
    }
    Ok((commands, replacement))
}
