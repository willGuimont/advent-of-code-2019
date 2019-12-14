use std::collections::HashMap;
use std::io;
use crate::intcode::Instruction::{Add, AdjustRelativeBase, Equals, Halt, Input, JumpIfFalse, JumpIfTrue, LessThan, Mult, Output};
use crate::intcode::Mode::{Immediate, Position, Relative};

pub struct IntCode {
    memory: HashMap<usize, i64>,
    pc: usize,
    relative_base: i64,
    score: i64,
    paddle_x: i64,
    ball_x: i64,
}

#[derive(Clone, Copy, Debug)]
enum Mode {
    Position,
    Immediate,
    Relative,
}

impl Mode {
    pub fn from_value(mode: u32) -> Mode {
        match mode {
            0 => Position,
            1 => Immediate,
            2 => Relative,
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
    AdjustRelativeBase(Argument),
}

impl IntCode {
    pub fn new(program_string: &str) -> IntCode {
        let split: Vec<&str> = program_string.split(",").collect();
        let program: Vec<i64> = split.iter()
            .map(|x| x.parse().expect("expected int"))
            .collect();

        let mut memory = HashMap::new();
        program.iter().enumerate()
            .for_each(|(i, x)| {
                memory.insert(i, x.clone());
            });
        IntCode {
            memory,
            pc: 0,
            relative_base: 0,
            score: 0,
            paddle_x: 0,
            ball_x: 0,
        }
    }

    pub fn run(&mut self) {
        let mut outputs = Vec::new();
        loop {
            let current_instruction = self.parse();
            let input = if let Input(x) = current_instruction {
                if self.paddle_x > self.ball_x {
                    Some(1)
                } else if self.paddle_x < self.ball_x {
                    Some(-1)
                } else {
                    Some(0)
                }
            } else {
                None
            };

            let (halted, output) = self.execute(current_instruction, input);

            if let Some(output) = output {
                outputs.push(output);
            }

            if outputs.len() == 3 {
                let x = outputs[0];
                let y = outputs[1];
                let z = outputs[2];
                println!("parse: {} {} {}", x, y, z);

                if x == -1 && y == 0 {
                    self.score = z;
                    println!("score: {}", self.score);
                } else if z == 4 {
                    self.paddle_x = x;
                    println!("paddle: {}", self.paddle_x);
                } else if z == 3 {
                    self.ball_x = x;
                    println!("ball: {}", self.ball_x);
                }

                outputs = Vec::new();
            }

            if halted {
                println!("{}", self.score);
                break;
            }
        }
    }

    fn read_memory(&self, index: usize) -> i64 {
        self.memory.get(&index).cloned().unwrap_or(0)
    }

    fn write_memory(&mut self, index: usize, value: i64) {
        self.memory.insert(index, value);
    }

    fn parse(&self) -> Instruction {
        let current = self.read_memory(self.pc);
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
                Add(args[0], args[1], args[2])
            }
            2 => {
                let args = self.get_args(3, modes);
                Mult(args[0], args[1], args[2])
            }
            3 => {
                let args = self.get_args(1, modes);
                Input(args[0])
            }
            4 => {
                let args = self.get_args(1, modes);
                Output(args[0])
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
            9 => {
                let args = self.get_args(1, modes);
                AdjustRelativeBase(args[0])
            }
            99 => {
                Halt
            }
            _ => {
                println!("{}", opcode);
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
                        let index = self.read_memory(self.pc + i + 1) as usize;
                        let value = self.read_memory(index);
                        Argument {
                            mode,
                            value,
                            address: index,
                        }
                    }
                    Immediate => {
                        let value = self.read_memory(self.pc + i + 1);
                        Argument {
                            mode,
                            value,
                            address: 0, // TODO
                        }
                    }
                    Relative => {
                        let index = (self.read_memory(self.pc + i + 1) + self.relative_base) as usize;
                        let value = self.read_memory(index);
                        Argument {
                            mode,
                            value,
                            address: index,
                        }
                    }
                };
                arguments.push(arg);
            }
        }
        arguments
    }

    fn execute(&mut self, instruction: Instruction, input: Option<i64>) -> (bool, Option<i64>) {
        let mut halted = false;
        let mut output = None;
        match instruction {
            Add(a, b, to) => {
                let value = a.value + b.value;
                self.write_memory(to.address as usize, value);
                self.pc += 4;
            }
            Mult(a, b, to) => {
                let value = a.value * b.value;
                self.write_memory(to.address as usize, value);
                self.pc += 4;
            }
            Input(to) => {
                if let Some(input) = input {
                    self.write_memory(to.address as usize, input);
                    self.pc += 2;
                } else {
                    panic!("no input");
                }
            }
            Output(what) => {
                output = Some(what.value);
                self.pc += 2;
            }
            Halt => {
                halted = true;
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
                self.write_memory(to.address, value);
                self.pc += 4;
            }
            Equals(a, b, to) => {
                let value = if a.value == b.value {
                    1
                } else {
                    0
                };
                self.write_memory(to.address, value);
                self.pc += 4;
            }
            AdjustRelativeBase(a) => {
                self.relative_base += a.value;
                self.pc += 2;
            }
        }
        (halted, output)
    }
}
