pub type ShouldStop = bool;

pub fn repl<R, W, Err, State>(
    input: &mut R,
    output: &mut W,
    initial_state: State,
    step: impl Fn(&str, &mut State) -> Result<ShouldStop, Err>,
) -> Result<(), Err>
where
    R: std::io::BufRead,
    W: std::io::Write,
    Err: From<std::io::Error>,
{
    let mut line = String::new();
    let mut state = initial_state;
    loop {
        write!(output, ">   ")?;
        output.flush().expect("output stream should be flushable");
        line.clear();
        input
            .read_line(&mut line)
            .expect("stdin should be readable");
        let should_stop = step(&line, &mut state)?;
        if should_stop {
            break Ok(());
        }
    }
}
