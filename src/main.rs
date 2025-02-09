use std::env;


fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return ;
    }

    let formula = args.get(1).unwrap();
    dbg!(formula);
}
