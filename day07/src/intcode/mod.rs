use std::io;

use crate::intcode::Instruction::{Add, Equals, Halt, Input, JumpIfFalse, JumpIfTrue, LessThan, Mult, Output};
use crate::intcode::Mode::{Immediate, Position};

pub struct IntCode {
    program: Vec<i64>,
    pc: usize,
}

#[derive(Clone, Copy, Debug)]
enum Mode {
    Position,
    Immediate,
}

impl Mode {
    pub fn from_value(mode: u32) -> Mode {
        match mode {
            0 => Position,
            1 => Immediate,
            _ => panic!("Mode not handler")
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Argument {
    mode: Mode,
    value: i64,
    address: usize,
}

#[derive(Debug)]
enum Instruction {
    Add(Argument, Argument, Argument),
    Mult(Argument, Argument, Argument),
    Input(Argument),
    Output(Argument),
    Halt,
    JumpIfTrue(Argument, Argument),
    JumpIfFalse(Argument, Argument),
    LessThan(Argument, Argument, Argument),
    Equals(Argument, Argument, Argument),
}

impl IntCode {
    pub fn new(program_string: &str) -> IntCode {
        let split: Vec<&str> = program_string.split(",").collect();
        let program: Vec<i64> = split.iter()
            .map(|x| x.parse().expect("expected int"))
            .collect();

        IntCode {
            program,
            pc: 0,
        }
    }

    pub fn run(&mut self) {
        loop {
            let current_instruction = self.parse();
            let halted = self.execute(current_instruction);

            if halted {
                break;
            }
        }
    }

    fn parse(&self) -> Instruction {
        let current = self.program[self.pc as usize];
        let mut current: Vec<char> = format!("{:0>2}", current).chars().collect();
        current.reverse();

        let mut opcode = &mut current[0..2];
        opcode.reverse();

        let opcode: String = opcode.iter().collect();
        let opcode: i64 = opcode.parse().unwrap();

        let modes = &current[2..];
        let modes = modes.iter().map(|x| x.to_digit(10).unwrap()).map(Mode::from_value).collect();

        match opcode {
            1 => {
                let args = self.get_args(3, modes);
                Add(args[0].clone(), args[1].clone(), args[2].clone())
            }
            2 => {
                let args = self.get_args(3, modes);
                Mult(args[0].clone(), args[1].clone(), args[2].clone())
            }
            3 => {
                let args = self.get_args(1, modes);
                Input(args[0].clone())
            }
            4 => {
                // TODO remove clone
                let args = self.get_args(1, modes);
                Output(args[0].clone())
            }
            5 => {
                let args = self.get_args(2, modes);
                JumpIfTrue(args[0], args[1])
            }
            6 => {
                let args = self.get_args(2, modes);
                JumpIfFalse(args[0], args[1])
            }
            7 => {
                let args = self.get_args(3, modes);
                LessThan(args[0], args[1], args[2])
            }
            8 => {
                let args = self.get_args(3, modes);
                Equals(args[0], args[1], args[2])
            }
            99 => {
                Halt
            }
            _ => {
                panic!("Opcode not supported");
            }
        }
    }

    fn get_args(&self, n: i64, modes: Vec<Mode>) -> Vec<Argument> {
        let mut arguments = Vec::new();
        let default_mode = Position;
        for i in 0..n as usize {
            if let Some(mode) = modes.get(i).or_else(|| Some(&default_mode)) {
                let mode = mode.clone();
                let arg = match mode {
                    Position => {
                        let index = self.program[self.pc + i + 1] as usize;
                        Argument {
                            mode,
                            value: self.program[index],
                            address: index,
                        }
                    }
                    Immediate => {
                        let value = self.program[self.pc + i + 1];
                        Argument {
                            mode,
                            value: value,
                            address: 0, // TODO
                        }
                    }
                };
                arguments.push(arg);
            }
        }
        arguments
    }

    fn execute(&mut self, instruction: Instruction) -> bool {
        match instruction {
            Add(a, b, to) => {
                self.program[to.address as usize] = a.value + b.value;
                self.pc += 4;
            }
            Mult(a, b, to) => {
                self.program[to.address as usize] = a.value * b.value;
                self.pc += 4;
            }
            Input(to) => {
//                println!("Input: ");
                let mut input = String::new();
                io::stdin().read_line(&mut input).expect("Could not read from stdin");
                let input = input.trim();
                let input = input.parse().expect("Bad input");
                self.program[to.address as usize] = input;
                self.pc += 2;
            }
            Output(what) => {
                println!("{}", what.value);
                self.pc += 2;
            }
            Halt => {
                return true;
            }
            JumpIfTrue(condition, to) => {
                if condition.value != 0 {
                    self.pc = to.value as usize;
                } else {
                    self.pc += 3;
                }
            }
            JumpIfFalse(condition, to) => {
                if condition.value == 0 {
                    self.pc = to.value as usize;
                } else {
                    self.pc += 3;
                }
            }
            LessThan(a, b, to) => {
                let value = if a.value < b.value {
                    1
                } else {
                    0
                };
                self.program[to.address] = value;
                self.pc += 4;
            }
            Equals(a, b, to) => {
                let value = if a.value == b.value {
                    1
                } else {
                    0
                };
                self.program[to.address] = value;
                self.pc += 4;
            }
        }
        false
    }
}
