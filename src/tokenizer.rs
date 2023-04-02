use std::env;

#[derive(Debug)]
struct ReplaceInfo<'a> {
    start: usize,
    end: usize,
    item: usize,
    conts: &'a Vec<String>,
}

// TODO: Actually tokenize instead of just hacking it with ignoring strings, indexes etc
pub fn parse_input(input: &str, env: &mut crate::Env) -> Vec<Vec<String>> {
    let mut in_quotes = false;
    let mut commands: Vec<String> = vec![String::from("")];
    let mut chars = input.chars();
    let mut replace_locations: Vec<ReplaceInfo> = vec![];
    let mut cur_char = 0;

    while let Some(i) = chars.next() {
        match i {
            '"' => {
                in_quotes = !in_quotes;
                cur_char += 1;
                continue;
            }
            '$' => {
                let variable_name = &env::var(
                    &chars
                        .by_ref()
                        .take_while(|x| *x != '\n' && *x != ' ')
                        .collect::<String>(),
                );
                commands
                    .last_mut()
                    .unwrap()
                    .push_str(variable_name.as_ref().unwrap());
                cur_char += variable_name.as_ref().unwrap().len() + 1;
                continue;
            }
            '\\' => {
                commands.last_mut().unwrap().push(chars.next().unwrap());
                cur_char += 2;
                continue;
            }
            '%' => {
                let name = &chars
                    .by_ref()
                    .take_while(|x| *x != '\n' && *x != ' ')
                    .collect::<String>();
                replace_locations.push(ReplaceInfo {
                    start: cur_char,
                    end: cur_char + 1,
                    item: commands.len(),
                    conts: env.lists.get(name).unwrap(),
                });
                commands.last_mut().unwrap().push('%');
                commands.last_mut().unwrap().push(' ');
                cur_char += 2;
                continue;
            }
            _ => (),
        }
        if !in_quotes {
            match i {
                '~' => commands.last_mut().unwrap().push_str(
                    &dirs::home_dir()
                        .unwrap()
                        .into_os_string()
                        .into_string()
                        .unwrap(),
                ),
                '|' => {
                    commands.push(String::from(""));
                    cur_char = 0;
                }
                _ => commands.last_mut().unwrap().push(i),
            }
        } else {
            commands.last_mut().unwrap().push(i);
        }
        cur_char += 1;
    }
    if replace_locations.is_empty() {
        return vec![commands];
    }
    let mut new_commands: Vec<Vec<String>> = vec![];

    let mut offset: Vec<Vec<usize>> = vec![];
    for _ in 0..commands.len() {
        offset.push(vec![0]);
    }

    let mut new_commands_parts = vec![commands];
    let mut replace_locations = replace_locations.iter().peekable();
    while let Some(loc) = replace_locations.next() {
        let mut new_parts: (Option<Vec<Vec<String>>>, Vec<Vec<usize>>) = (Some(vec![]), vec![]);
        for (command, cur_offset) in new_commands_parts.iter().zip(offset.iter()) {
            let cur_offset = cur_offset.get(loc.item - 1).unwrap_or(&0);
            if replace_locations.peek().is_none() {
                for list_item in loc.conts {
                    let mut new_cmd = command.clone();
                    new_cmd
                        .get_mut(loc.item - 1)
                        .unwrap()
                        .replace_range((loc.start + cur_offset)..(loc.end + cur_offset), list_item);
                    new_commands.push(new_cmd);
                }
            } else {
                for list_item in loc.conts {
                    let mut new_cmd = command.clone();
                    new_cmd
                        .get_mut(loc.item - 1)
                        .unwrap()
                        .replace_range((loc.start + cur_offset)..(loc.end + cur_offset), list_item);
                    new_parts.0.as_mut().unwrap().push(new_cmd);
                    new_parts.1.push(vec![list_item.len() - 1]);
                }
            }
        }
        offset = new_parts.1;
        if let Some(replacement) = new_parts.0 {
            new_commands_parts = replacement;
        }
    }
    new_commands
}
