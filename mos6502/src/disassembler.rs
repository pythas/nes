pub struct Disassembler {
    instructions: Vec<Vec<u8>>,
}

impl Disassembler {
    pub fn new() -> Disassembler {
        Disassembler {
            instructions: Vec::new(),
        }
    }

    pub fn load(&mut self, instruction: &[u8]) {
        self.instructions.push(instruction.to_vec());
    }

    pub fn get_last(&self) -> &[u8] {
        self.instructions.last().unwrap()
    }
}