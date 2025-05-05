use interpreter_rust::repl;

fn main() {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    repl::repl::start_repl(stdin.lock(), stdout.lock());
}
