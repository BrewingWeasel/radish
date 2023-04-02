use std::env;

#[derive(Debug)]
struct ReplaceInfo<'a> {
    start: usize,
    end: usize,
    item: usize,
    conts: &'a Vec<String>,
}

pub enum Argument {
    Text(String),
    List(String),
    ListReference(u8),
}

// TODO: come up with a better name
pub enum CommandPart {
    Command(Vec<Argument>),
    File((String, bool)),
    // TODO: allow sending stdout to a file or possibly somewhere else in
    // the future
}

// TODO: Actually tokenize instead of just hacking it with ignoring strings, indexes etc
pub fn parse_input(input: &str, env: &mut crate::Env) -> Vec<Vec<CommandPart>> {
    let mut in_quotes = false;
    let mut commands: Vec<CommandPart> =
        vec![CommandPart::Command(vec![Argument::Text(String::from(""))])];
    let mut chars = input.chars();

    while let Some(i) = chars.next() {
        let last_str = match commands.last_mut().unwrap() {
            CommandPart::Command(args) => match args.last_mut().unwrap() {
                Argument::Text(s) => s,
                _ => panic!("Tokenizer should have finished parsing list"),
            },
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
            // '%' => {
            //     let name = &chars
            //         .by_ref()
            //         .take_while(|x| *x != '\n' && *x != ' ')
            //         .collect::<String>();
            //     replace_locations.push(ReplaceInfo {
            //         start: cur_char,
            //         end: cur_char + 1,
            //         item: commands.len(),
            //         conts: env.lists.get(name).unwrap(),
            //     });
            //     commands.last_mut().unwrap().push('%');
            //     commands.last_mut().unwrap().push(' ');
            //     cur_char += 2;
            //     continue;
            // }
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
                    commands.push(CommandPart::Command(vec![Argument::Text(String::from(""))]));
                }
                ' ' => {
                    if let CommandPart::Command(cmd) = commands.last_mut().unwrap() {
                        cmd.push(Argument::Text(String::from("")));
                    }
                }
                _ => last_str.push(i),
            }
        } else {
            last_str.push(i);
        }
    }
    vec![commands]

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
