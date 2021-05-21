use std::collections::HashMap;

pub struct Program {
    blks: Vec<BasicBlock>,
    symtab: HashMap<Ident, Symbol>
}

pub enum SymType {
    Func,
    Int,
    Char
}

pub enum Literal {
    Int(i32),
    Char(u8)
}

type Ident = String;

pub struct Symbol {
    ty: SymType,
    ident: Ident
}

pub struct BasicBlock {

}

pub enum Instr {
    
}
