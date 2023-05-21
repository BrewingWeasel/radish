use crossterm::{
    cursor::{MoveLeft, MoveToColumn},
    event::{read, Event, KeyCode, KeyModifiers},
    execute, queue,
    style::{Color, Print, ResetColor, SetForegroundColor},
    terminal::Clear,
};
use std::{borrow::Cow, cmp::Ordering, fs::read_dir, io::stdout};

use crate::exit;

pub fn get_input(env: &mut crate::Env) -> String {
    #[derive(Debug)]
    enum CompletionType {
        Command,
        List,
        File,
    }

    let mut history_index = env.history.len();
    let mut input = String::new();
    let mut in_quotes = false;
    let mut after_slash = false;
    let mut currently_completing = CompletionType::Command;
    let new_history = env
        .sorted_history
        .iter()
        .map(|x| Cow::Borrowed(x))
        .collect();
    let mut suggested_input: Option<&str> = None;

    loop {
        if let Event::Key(x) = read().unwrap() {
            match x.code {
                KeyCode::Char(c) => {
                    if after_slash {
                        execute!(stdout(), Print(c), ResetColor).unwrap();
                        input.push(c);
                        after_slash = false;
                        continue;
                    }
                    match c {
                        '"' => {
                            in_quotes = !in_quotes;
                            if in_quotes {
                                queue!(stdout(), SetForegroundColor(Color::Blue)).unwrap();
                            } else {
                                queue!(stdout(), ResetColor).unwrap();
                            }
                        }
                        '\\' => {
                            queue!(stdout(), SetForegroundColor(Color::Magenta)).unwrap();
                            after_slash = true;
                        }
                        '%' => {
                            if !in_quotes {
                                currently_completing = CompletionType::List;
                            }
                        }
                        ' ' => {
                            if !in_quotes {
                                currently_completing = CompletionType::File;
                            }
                        }
                        '|' => {
                            if !in_quotes {
                                execute!(
                                    stdout(),
                                    SetForegroundColor(Color::Cyan),
                                    Print("|"),
                                    ResetColor
                                )
                                .unwrap();
                                input.push(c);
                                currently_completing = CompletionType::Command;
                                // TODO: clean up, change name or stmh
                                after_slash = true;
                                continue;
                            }
                        }
                        _ => (),
                    }
                    execute!(stdout(), Print(c)).unwrap();
                    input.push(c);
                }
                KeyCode::Tab => {
                    let completing_values = match currently_completing {
                        CompletionType::Command => (
                            get_backwards_until(&input, ' '),
                            env.commands.iter().map(Cow::Borrowed).collect(),
                        ),
                        CompletionType::List => (
                            get_backwards_until(&input, '%'),
                            env.lists.keys().map(Cow::Borrowed).collect(),
                        ),
                        CompletionType::File => {
                            let file_name = get_backwards_until(&input, ' ');
                            let folder = if let Some((f, _)) = file_name.rsplit_once('/') {
                                f
                            } else {
                                "./"
                            };
                            (
                                get_backwards_until(&input, ' '),
                                get_all_files(folder).into_iter().map(Cow::Owned).collect(),
                            )
                        }
                    };
                    let new_cmd = suggest(&completing_values.0, &completing_values.1);
                    if let Some(completion) = new_cmd {
                        execute!(
                            stdout(),
                            MoveLeft(completing_values.0.len().try_into().unwrap()),
                            Clear(crossterm::terminal::ClearType::UntilNewLine),
                            Print(completion)
                        )
                        .unwrap();
                        input.replace_range(input.len() - completing_values.0.len().., completion);
                    }
                }
                KeyCode::Esc => {
                    exit(env);
                    unreachable!()
                }
                KeyCode::Enter => {
                    execute!(stdout(), Print('\n'), MoveToColumn(0)).unwrap();
                    break;
                }
                KeyCode::Backspace => {
                    if x.modifiers.contains(KeyModifiers::CONTROL) {
                        // TODO: add ctrl delete
                    } else if input.pop().is_some() {
                        execute!(
                            stdout(),
                            MoveLeft(1),
                            Clear(crossterm::terminal::ClearType::UntilNewLine)
                        )
                        .unwrap()
                    }
                }
                KeyCode::Right => {
                    if let Some(mut completion) = suggested_input {
                        completion = completion.strip_prefix(&input).unwrap();
                        execute!(
                            stdout(),
                            ResetColor,
                            MoveToColumn(env.prompt_length + u16::try_from(input.len()).unwrap()),
                            Clear(crossterm::terminal::ClearType::UntilNewLine),
                            Print(completion),
                        )
                        .unwrap();
                        input.push_str(completion)
                    }
                }
                KeyCode::Up => {
                    if history_index == 0 {
                        continue;
                    }
                    history_index -= 1;
                    let new_input = env.history.get(history_index);
                    if let Some(inp) = new_input {
                        execute!(
                            stdout(),
                            MoveToColumn(env.prompt_length),
                            Clear(crossterm::terminal::ClearType::UntilNewLine),
                            Print(inp)
                        )
                        .unwrap();
                        input = inp.to_string();
                    } else {
                        history_index += 1;
                    }
                }
                KeyCode::Down => {
                    history_index += 1;
                    let new_input = env.history.get(history_index);
                    if let Some(inp) = new_input {
                        execute!(
                            stdout(),
                            MoveToColumn(env.prompt_length),
                            Clear(crossterm::terminal::ClearType::UntilNewLine),
                            Print(inp)
                        )
                        .unwrap();
                        input = inp.to_string();
                    } else {
                        history_index -= 1;
                        execute!(
                            stdout(),
                            MoveToColumn(env.prompt_length),
                            Clear(crossterm::terminal::ClearType::UntilNewLine),
                        )
                        .unwrap();
                    }
                }
                _ => (),
            }
        }
        suggested_input = suggest(&input, &new_history);
        if let Some(mut completion) = suggested_input {
            completion = if let Some(completion) = completion.strip_prefix(&input) {
                completion
            } else {
                continue;
            };
            execute!(
                stdout(),
                MoveToColumn(env.prompt_length + u16::try_from(input.len()).unwrap()),
                Clear(crossterm::terminal::ClearType::UntilNewLine),
                SetForegroundColor(Color::Cyan),
                Print(completion),
                MoveToColumn(env.prompt_length + u16::try_from(input.len()).unwrap()),
                ResetColor
            )
            .unwrap();
        } else {
            queue!(
                stdout(),
                Clear(crossterm::terminal::ClearType::UntilNewLine)
            )
            .unwrap();
        }
    }
    input
}

fn get_backwards_until(input: &str, until: char) -> String {
    //TODO: probably make this use traits and clean this up
    input
        .chars()
        .rev()
        .take_while(|x| *x != until)
        .collect::<String>()
        .chars()
        .rev()
        .collect()
}

fn suggest<'a>(input: &str, options: &'a Vec<Cow<String>>) -> Option<&'a str> {
    if options.len() == 1 {
        return Some(&options[0]);
    }
    let mut most_shared = 0;
    let mut number_of_shared = 0;

    for (i, option) in options.iter().enumerate() {
        if option.as_str() == input {
            return None;
        }
        let mut cur_shared = 0;
        let mut input_chars = input.chars();
        let mut option_chars = option.chars();
        while input_chars.next() == option_chars.next() {
            cur_shared += 1;
        }
        match cur_shared.cmp(&most_shared) {
            Ordering::Less => {
                if !options[i - 1].starts_with(input) {
                    return None;
                }
                if number_of_shared == 0 && most_shared != 0 {
                    return Some(&options[i - 1]);
                } else {
                    let orig_to_share = &options[i - number_of_shared];
                    let mut min_shared_count = orig_to_share.chars().count();
                    for shared_index in 1..number_of_shared - 1 {
                        let mut orig_shared_chars = orig_to_share.chars().take(min_shared_count);
                        let mut new_shared_chars = options[i - shared_index].chars();
                        let mut shared_count = 0;
                        while orig_shared_chars.next() == new_shared_chars.next() {
                            shared_count += 1;
                        }
                        if shared_count < min_shared_count {
                            min_shared_count = shared_count;
                        }
                    }
                    if min_shared_count >= most_shared {
                        return Some(&orig_to_share[..min_shared_count]);
                    } else {
                        return None;
                    }
                }
            }
            Ordering::Greater => {
                number_of_shared = 0;
                most_shared = cur_shared;
            }
            Ordering::Equal => {
                number_of_shared += 1;
            }
        }
    }
    None
}

fn get_all_files(dir: &str) -> Vec<String> {
    read_dir(dir)
        .unwrap()
        .map(|p| {
            let path = p.unwrap().path().display().to_string();
            path.strip_prefix("./").unwrap_or(&path).to_owned()
        })
        .collect()
}
