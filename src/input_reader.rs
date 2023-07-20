use crokey::crossterm::{
    self,
    cursor::{MoveLeft, MoveRight, MoveToColumn},
    event::{read, Event, KeyCode, KeyEvent},
    execute, queue,
    style::{Color, Print, ResetColor, SetForegroundColor},
    terminal::{disable_raw_mode, enable_raw_mode, Clear},
};
use std::{borrow::Cow, cmp::Ordering, collections::HashMap, fs::read_dir, io::stdout};

use crate::{exit, run_from_string};

struct WritingEnv {
    input: String,
    chars_from_end: usize,
    prompt_length: u16,
}

pub fn get_input(env: &mut crate::Env, next_cmd: Option<String>) -> (String, Option<String>) {
    #[derive(Debug)]
    enum CompletionType {
        Command,
        List,
        File,
    }
    let mut builtin_modifiers: HashMap<KeyEvent, fn(&mut WritingEnv)> = HashMap::new();

    builtin_modifiers.insert(crokey::parse("alt-backspace").unwrap(), delete_word);
    builtin_modifiers.insert(crokey::parse("alt-left").unwrap(), back_word);
    builtin_modifiers.insert(crokey::parse("alt-right").unwrap(), forward_word);
    builtin_modifiers.insert(crokey::parse("alt-a").unwrap(), start_of_line);
    builtin_modifiers.insert(crokey::parse("alt-e").unwrap(), end_of_line);

    let mut history_index = env.history.len();
    let mut writing_env = WritingEnv {
        input: next_cmd.unwrap_or(String::new()),
        chars_from_end: 0,
        prompt_length: env.prompt_length,
    };
    execute!(stdout(), Print(&writing_env.input),).unwrap();
    let mut in_quotes = false;
    let mut after_slash = false;
    let mut currently_completing = CompletionType::Command;
    let mut suggested_input: Option<&str> = None;

    loop {
        if let Event::Key(x) = read().unwrap() {
            if let Some(func) = builtin_modifiers.get(&x) {
                func(&mut writing_env);
            } else if let Some((reset, cmd)) = env.get_bindings().get(&x) {
                let new_cmd = cmd.to_owned();
                disable_raw_mode().unwrap();
                if *reset {
                    execute!(stdout(), Print('\n')).unwrap();
                    return (new_cmd, Some(writing_env.input));
                }
                if let Err(e) = run_from_string(Cow::Owned(new_cmd), env, true, None) {
                    eprintln!("{e}");
                }
                enable_raw_mode().unwrap();
            } else {
                match x.code {
                    KeyCode::Char(c) => {
                        if after_slash {
                            execute!(stdout(), Print(c), ResetColor).unwrap();
                            writing_env.input.push(c);
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
                                    queue!(stdout(), SetForegroundColor(Color::Cyan)).unwrap();
                                    currently_completing = CompletionType::Command;
                                    // TODO: clean up, change name or stmh
                                    after_slash = true;
                                }
                            }
                            _ => (),
                        }
                        if writing_env.chars_from_end != 0 {
                            writing_env.input.insert(
                                writing_env.input.chars().count() - writing_env.chars_from_end,
                                c,
                            );
                            if writing_env.chars_from_end != 0 {
                                execute!(
                                    stdout(),
                                    Print(c),
                                    Clear(crossterm::terminal::ClearType::UntilNewLine),
                                    Print(
                                        writing_env
                                            .input
                                            .chars()
                                            .rev()
                                            .take(writing_env.chars_from_end)
                                            .collect::<String>()
                                            .chars()
                                            .rev()
                                            .collect::<String>()
                                    ),
                                    MoveLeft(writing_env.chars_from_end.try_into().unwrap())
                                )
                                .unwrap();
                            }
                        } else {
                            execute!(stdout(), Print(c)).unwrap();
                            writing_env.input.push(c);
                        }
                    }

                    KeyCode::Tab => {
                        let completing_values = match currently_completing {
                            CompletionType::Command => (
                                get_backwards_until(&writing_env.input, ' '),
                                env.commands.iter().map(Cow::Borrowed).collect(),
                            ),
                            CompletionType::List => (
                                get_backwards_until(&writing_env.input, '%'),
                                env.settings.lists.keys().map(Cow::Borrowed).collect(),
                            ),
                            CompletionType::File => {
                                let file_name = get_backwards_until(&writing_env.input, ' ');
                                let folder = if let Some((f, _)) = file_name.rsplit_once('/') {
                                    f
                                } else {
                                    "./"
                                };
                                (
                                    get_backwards_until(&writing_env.input, ' '),
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
                            writing_env.input.replace_range(
                                writing_env.input.len() - completing_values.0.len()..,
                                completion,
                            );
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
                        if writing_env.chars_from_end == 0 {
                            if writing_env.input.pop().is_some() {
                                execute!(
                                    stdout(),
                                    MoveLeft(1),
                                    Clear(crossterm::terminal::ClearType::UntilNewLine),
                                )
                                .unwrap();
                            }
                        } else {
                            writing_env
                                .input
                                .remove(writing_env.input.len() - writing_env.chars_from_end);
                            execute!(
                                stdout(),
                                MoveLeft(1),
                                Clear(crossterm::terminal::ClearType::UntilNewLine),
                                Print(
                                    writing_env
                                        .input
                                        .chars()
                                        .rev()
                                        .take(writing_env.chars_from_end)
                                        .collect::<String>()
                                        .chars()
                                        .rev()
                                        .collect::<String>()
                                ),
                                MoveLeft(writing_env.chars_from_end.try_into().unwrap()),
                            )
                            .unwrap();
                        }
                    }
                    KeyCode::Right => {
                        if suggested_input.is_some() && writing_env.chars_from_end == 0 {
                            let completion = suggested_input
                                .unwrap()
                                .strip_prefix(&writing_env.input)
                                .unwrap();
                            execute!(
                                stdout(),
                                ResetColor,
                                MoveToColumn(
                                    env.prompt_length
                                        + u16::try_from(writing_env.input.len()).unwrap()
                                ),
                                Clear(crossterm::terminal::ClearType::UntilNewLine),
                                Print(completion),
                            )
                            .unwrap();
                            writing_env.input.push_str(completion)
                        } else {
                            writing_env.chars_from_end =
                                if let Some(val) = writing_env.chars_from_end.checked_sub(1) {
                                    execute!(stdout(), MoveRight(1)).unwrap();
                                    val
                                } else {
                                    0
                                }
                        }
                    }
                    KeyCode::Left => {
                        if writing_env.chars_from_end < writing_env.input.chars().count() {
                            writing_env.chars_from_end += 1;
                            execute!(stdout(), MoveLeft(1)).unwrap();
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
                            writing_env.input = inp.to_string();
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
                            writing_env.input = inp.to_string();
                        } else {
                            history_index -= 1;
                            execute!(
                                stdout(),
                                MoveToColumn(env.prompt_length),
                                Clear(crossterm::terminal::ClearType::UntilNewLine),
                            )
                            .unwrap();
                            writing_env.input = String::new();
                        }
                    }
                    _ => (),
                }
            }
        }

        suggested_input = suggest(&writing_env.input, &env.sorted_history);
        if let Some(mut completion) = suggested_input {
            completion = if let Some(completion) = completion.strip_prefix(&writing_env.input) {
                completion
            } else {
                continue;
            };
            execute!(
                stdout(),
                MoveToColumn(env.prompt_length + u16::try_from(writing_env.input.len()).unwrap()),
                Clear(crossterm::terminal::ClearType::UntilNewLine),
                SetForegroundColor(Color::Cyan),
                Print(completion),
                MoveToColumn(env.prompt_length + u16::try_from(writing_env.input.len()).unwrap()),
                ResetColor
            )
            .unwrap();
        } else if writing_env.chars_from_end == 0 {
            queue!(
                stdout(),
                Clear(crossterm::terminal::ClearType::UntilNewLine)
            )
            .unwrap();
        }
    }
    (writing_env.input, None)
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

fn get_forward_until(input: &str, until: char) -> String {
    //TODO: probably make this use traits and clean this up
    input
        .chars()
        .rev()
        .take_while(|x| *x != until)
        .collect::<String>()
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

fn delete_word(writing_env: &mut WritingEnv) {
    if writing_env.input.len() != 0 {
        let conts_of_word = get_backwards_until(&writing_env.input, ' ');
        let length = (writing_env.input.len() - conts_of_word.len())
            .checked_sub(1)
            .unwrap_or(0);
        writing_env.input = writing_env.input[..length].to_string();
        execute!(
            stdout(),
            MoveToColumn(writing_env.prompt_length + length as u16),
            Clear(crossterm::terminal::ClearType::UntilNewLine),
        )
        .unwrap();
    }
}

fn back_word(writing_env: &mut WritingEnv) {
    let conts_of_word = get_backwards_until(&writing_env.input, ' ');
    if writing_env.chars_from_end != writing_env.input.len() {
        writing_env.chars_from_end += conts_of_word.len();
        execute!(stdout(), MoveLeft(conts_of_word.len().try_into().unwrap()),).unwrap();
    }
}

fn forward_word(writing_env: &mut WritingEnv) {
    let conts_of_word = get_forward_until(&writing_env.input, ' ');
    if writing_env.chars_from_end != 0 {
        writing_env.chars_from_end -= conts_of_word.len();
        execute!(stdout(), MoveRight(conts_of_word.len().try_into().unwrap()),).unwrap();
    }
}

fn end_of_line(writing_env: &mut WritingEnv) {
    writing_env.chars_from_end = 0;
    execute!(
        stdout(),
        MoveToColumn(writing_env.input.len() as u16 + writing_env.prompt_length),
    )
    .unwrap();
}

fn start_of_line(writing_env: &mut WritingEnv) {
    writing_env.chars_from_end = writing_env.input.len();
    execute!(stdout(), MoveToColumn(writing_env.prompt_length as u16),).unwrap();
}
