use glob::glob;
use std::env;

// TODO: come up with a better name
#[derive(Clone)]
pub enum CommandPart {
    Command(Vec<String>),
    File((String, bool)),
    // TODO: allow sending stdout to a file or possibly somewhere else in
    // the future
}

// TODO: Clean up
pub fn parse_input(
    input: &str,
    env: &crate::Env,
) -> (Vec<CommandPart>, Vec<Vec<(usize, usize, String)>>) {
    let mut in_quotes = false;
    let mut in_glob_pattern = false;
    let mut commands: Vec<CommandPart> = vec![CommandPart::Command(vec![String::from("")])];
    let mut chars = input.chars();
    let mut commandpart_index = 0;
    let mut current_token_index = 0;
    let mut replacement: Vec<Vec<(usize, usize, String)>> = vec![vec![]];

    while let Some(i) = chars.next() {
        let last_str = match commands.last_mut().unwrap() {
            CommandPart::Command(args) => args.last_mut().unwrap(),
            _ => panic!("Took in file instead of argument"),
        };
        match i {
            '"' => {
                in_quotes = !in_quotes;
                continue;
            }
            '$' => {
                let variable_name = &env::var(
                    &chars
                        .by_ref()
                        .take_while(|x| *x != '\n' && *x != ' ')
                        .collect::<String>(),
                );
                last_str.push_str(variable_name.as_ref().unwrap());
                continue;
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
                    current_token_index = 0;
                }
                ' ' => {
                    if in_glob_pattern {
                        let glob_pattern = last_str.clone();
                        if let CommandPart::Command(cmd) = commands.last_mut().unwrap() {
                            cmd.pop();
                            for entry in glob(&glob_pattern).unwrap() {
                                if let Ok(path) = entry {
                                    cmd.push(path.display().to_string())
                                }
                            }
                        }
                    }
                    in_glob_pattern = false;
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
                    last_str.push_str(&env.locations.get(&name).unwrap())
                }
                '*' => {
                    in_glob_pattern = true;
                    last_str.push('*');
                }
                '%' => {
                    if let CommandPart::Command(cmd) = commands.last_mut().unwrap() {
                        cmd.push(String::from(""));
                    }
                    current_token_index += 1;

                    let name = chars
                        .by_ref()
                        .take_while(|x| *x != '\n' && *x != ' ')
                        .collect::<String>();
                    let mut new_replacement = vec![];
                    for index in 0..replacement.len() {
                        for item in env.lists.get(&name).unwrap() {
                            let mut replacement_pattern = replacement[index].clone();
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
                    if let CommandPart::Command(cmd) = commands.last_mut().unwrap() {
                        cmd.push(String::from(""));
                    }
                    current_token_index += 1;

                    let reference_index: usize = chars
                        .by_ref()
                        .take_while(|x| x.is_digit(10))
                        .collect::<String>()
                        .parse()
                        .unwrap();

                    for old_replacement in replacement.iter_mut() {
                        let original_str = old_replacement[reference_index].2.clone();
                        old_replacement.push((commandpart_index, current_token_index, original_str))
                    }
                    continue;
                }
                _ => last_str.push(i),
            }
        } else {
            last_str.push(i);
        }
    }
    (commands, replacement)
}
