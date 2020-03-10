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

    pub fn last(&self) -> Option<&Vec<u8>> {
        self.instructions.last()
    }
}