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
                    if let CommandPart::Command(cmd) = commands.last_mut().unwrap() {
                        cmd.push(String::from(""));
                    }
                    current_token_index += 1;
                }
                _ => last_str.push(i),
            }
        } else {
            last_str.push(i);
        }
    }
    (commands, replacement)

    //let mut new_commands: Vec<Vec<CommandPart>> = vec![];

    // let mut new_commands_parts = vec![commands];
    // let mut replace_locations = replace_locations.iter().peekable();
    // while let Some(loc) = replace_locations.next() {
    //     let mut new_parts: (Option<Vec<Vec<String>>>, Vec<Vec<usize>>) = (Some(vec![]), vec![]);
    //     for (command, cur_offset) in new_commands_parts.iter().zip(offset.iter()) {
    //         let cur_offset = cur_offset.get(loc.item - 1).unwrap_or(&0);
    //         if replace_locations.peek().is_none() {
    //             for list_item in loc.conts {
    //                 let mut new_cmd = command.clone();
    //                 new_cmd
    //                     .get_mut(loc.item - 1)
    //                     .unwrap()
    //                     .replace_range((loc.start + cur_offset)..(loc.end + cur_offset), list_item);
    //                 new_commands.push(new_cmd);
    //             }
    //         } else {
    //             for list_item in loc.conts {
    //                 let mut new_cmd = command.clone();
    //                 new_cmd
    //                     .get_mut(loc.item - 1)
    //                     .unwrap()
    //                     .replace_range((loc.start + cur_offset)..(loc.end + cur_offset), list_item);
    //                 new_parts.0.as_mut().unwrap().push(new_cmd);
    //                 new_parts.1.push(vec![list_item.len() - 1]);
    //             }
    //         }
    //     }
    //     offset = new_parts.1;
    //     if let Some(replacement) = new_parts.0 {
    //         new_commands_parts = replacement;
    //     }
    // }
    //new_commands
}
