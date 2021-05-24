//! in this model we convert AST into a custom MIR, which is in the form of 4 arity code
//! for example, the assign statement
//! 
//! ```
//! a := 1 + 2 / 3;
//! ```
//! 
//! will be converted into 
//! 
//! ```
//! t1 <- DIV 2, 3
//! a <- ADD 1, t1
//! ```
//！ 
//! and the if statement
//! 
//! ```
//! if (a) then
//!   a := a + 1
//! end
//! ```
//! 
//! will be converted into 
//! 
//! ```
//! CMP a, 0
//! JZ oth
//! a <- ADD a, 1
//! oth: 
//! ...
//! ```
//! 

use std::{collections::HashMap, ops::Deref, usize};
use crate::ast::{self, Expr, Statement, BinOp, Literal};

type InstrTid = usize;
type StrTid = usize;
type Ident = String;

#[derive(Debug, Clone)]
pub struct Program {
    stmts: Vec<Statement>,
    instrs: Vec<Instr>,
    /// symbol id map to symbol name
    strtab: HashMap<StrTid, Ident>,
    /// symbol name map to symbol id
    strmap: HashMap<Ident, StrTid>,
    symtab: HashMap<Ident, Symbol>,
    instr_tid_cnt: InstrTid,
    str_tid_cnt: StrTid
}

impl Program {
    pub fn new(ast: ast::Program) -> Self {
        Program {
            stmts: ast.statements,
            instrs: Vec::new(),
            strtab: HashMap::new(),
            strmap: HashMap::new(),
            symtab: HashMap::new(),
            instr_tid_cnt: 0,
            str_tid_cnt: 0
        }
    }

    pub fn emit(&mut self) {
        for stmt in self.stmts.clone() {
            self.emit_statement(&stmt)
        }
    }

    /// receieve a variable name and initialize the corresponding hashmap entries (if needed).
    /// returns the id of the name in string table
    fn prepare_var(&mut self, ident: &Ident) -> StrTid {
        match self.strmap.get(ident) {
            Some(x) => *x,
            None => {
                let tid = self.get_str_tid();
                self.strtab.insert(tid, ident.clone());
                self.strmap.insert(ident.clone(), tid);
                tid
            }
        }
    }

    fn emit_expr(&mut self, expr: &Expr) -> Oprand {
        match expr {
            Expr::Ident(x) => Oprand::Ident(self.prepare_var(x)),
            Expr::Number(x) => Oprand::Literal(*x),
            Expr::UnaryExpr(x) => {
                let rhs = self.emit_expr(x.oprand.as_ref());

                let op = match x.op {
                    ast::UnOp::NegOp => IrOp::SUB,
                    ast::UnOp::PosOp => {return rhs}
                };

                let lhs = Oprand::Literal(Literal::Int(0));
                let dest = self.get_tmp_var();
                let tid = self.get_instr_tid();
                

                self.instrs.push(Instr {
                    tid,
                    op,
                    dest,
                    src: (Some(lhs), Some(rhs))
                });

                Oprand::Ident(dest)
            },
            Expr::BinExpr(x) => {
                let lhs = self.emit_expr(x.lhs.as_ref());
                let rhs = self.emit_expr(x.rhs.as_ref());

                // note that the type of a expression result is consist with the type of lhs
                let dest = self.get_tmp_var();
                let tid = self.get_instr_tid();

                self.instrs.push(Instr {
                    tid,
                    op: match x.op {
                        BinOp::AddOp => IrOp::ADD,
                        BinOp::DivOp => IrOp::DIV,
                        BinOp::MulOp => IrOp::MUL,
                        BinOp::SubOp => IrOp::SUB,
                        // the rest are comparsion ops
                        BinOp::Eq | BinOp::Ge | BinOp::Gt
                        | BinOp::Le | BinOp::Lt | BinOp::Ne => IrOp::CMP
                    },
                    dest,
                    src: (Some(lhs), Some(rhs))
                });

                Oprand::Ident(dest)
            }
        }
    }

    fn get_tmp_var(&mut self) -> StrTid {
        self.str_tid_cnt += 1;
        self.prepare_var(&format!("tmp_{}", self.str_tid_cnt - 1))
    }

    fn get_instr_tid(&mut self) -> InstrTid {
        self.instr_tid_cnt += 1;
        self.instr_tid_cnt - 1
    }

    fn get_str_tid(&mut self) -> StrTid {
        self.str_tid_cnt += 1;
        self.str_tid_cnt - 1
    }

    fn emit_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::AssignStmt(x) => {
                let tid = self.get_instr_tid();
                let src = self.emit_expr(&x.src);
                let dest = self.prepare_var(&x.dest);

                self.instrs.push(Instr {
                    tid,
                    op: IrOp::MOV,
                    dest,
                    src: (Some(src), None)
                })
            },
            Statement::CallStmt(x) => {},
            Statement::DeclStmt(x) => {
                let tid = self.get_instr_tid();
                let dest = self.prepare_var(&x.ident);
                self.instrs.push(Instr {
                    op: IrOp::DECL,
                    tid,
                    dest,
                    src: (None, None)
                });

                self.symtab.insert(x.ident.clone(), Symbol {
                    ty: match x.ty {
                            ast::DataType::Char => SymType::Char,
                            ast::DataType::Int => SymType::Int
                        },
                    ident: dest
                });
            },
            Statement::IfStmt(x) => {},
            Statement::RepeatStmt(x) => {}
        }
    }
}

#[derive(Debug, Clone)]
pub enum SymType {
    Func,
    Int,
    Char,
    /// this is a type to be discovered with static analysis
    Buttom
}

#[derive(Debug, Clone)]
pub struct Symbol {
    ty: SymType,
    ident: StrTid
}

#[derive(Debug, Clone)]
pub struct Instr {
    tid: InstrTid,
    op: IrOp,
    dest: StrTid,
    src: (Option<Oprand>, Option<Oprand>)
}

#[derive(Debug, Clone)]
pub enum IrOp {
    JZ,
    JNZ,
    JG,
    JGE,
    JL,
    JLE,
    CALL,
    ADD,
    MOV,
    SUB,
    MUL,
    DIV,
    CMP,
    DECL
}

#[derive(Debug, Clone)]
pub enum Oprand {
    Literal(Literal),
    Ident(StrTid)
}
