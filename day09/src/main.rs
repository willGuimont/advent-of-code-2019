use std::fs::File;
use std::io::Read;
use crate::intcode::IntCode;

mod intcode;


fn main() {
    let mut file = File::open("input.txt").expect("input file not found");
    let mut content = String::new();
    file.read_to_string(&mut content).expect("could not read file");

    let mut machine = IntCode::new(&content);
    machine.run();
}
