use std::fs::File;
use std::io::Read;
use std::thread;

fn get_args(p: &Vec<usize>, i: usize) -> (usize, usize, usize) {
    (p[i + 1], p[i + 2], p[i + 3])
}

fn run(program: &mut Vec<usize>, a: usize, b: usize) -> usize {
    program[1] = a;
    program[2] = b;
    let mut index = 0;
    loop {
        let code = program[index];
        match code {
            1 => {
                let (a, b, c) = get_args(&program, index);
                program[c] = program[a] + program[b];
                index += 4;
            },
            2 => {
                let (a, b, c) = get_args(&program, index);
                program[c] = program[a] * program[b];
                index += 4;
            },
            99 => break,
            _ => break
        }
    }
    return program[0];
}

fn main() {
    let mut file = File::open("input.txt").expect("input file not found");
    let mut content = String::new();
    file.read_to_string(&mut content).expect("could not read file");

    let split : Vec<&str> = content.split(",").collect();
    let program : Vec<usize> = split.iter()
        .map(|x| x.parse().expect("expected int"))
        .collect();

    let mut threads = vec![];
    for i in 0..100 {
        let p = program.clone();
        threads.push(thread::spawn(move || {
            for j in 0..100 {
                let result = run(&mut p.clone(), i, j);
                if result == 19690720 {
                    println!("{}{:02}", i, j);
                }
            }
        }));
    }

    for t in threads {
        let _ = t.join();
    }
}
