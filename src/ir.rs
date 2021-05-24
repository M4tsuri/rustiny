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

use std::{collections::HashMap, usize};
use crate::ast::{self, Expr, Statement, BinOp, Literal};

type Tid = usize;
type Ident = String;

#[derive(Debug, Clone)]
pub struct Program {
    stmts: Vec<Statement>,
    instrs: Vec<Instr>,
    symtab: HashMap<Ident, Symbol>,
    tid_cnt: Tid
}

impl Program {
    pub fn new(ast: ast::Program) -> Self {
        Program {
            stmts: ast.statements,
            instrs: Vec::new(),
            symtab: HashMap::new(),
            tid_cnt: 0
        }
    }

    pub fn emit(&mut self) {
        for stmt in self.stmts.clone() {
            self.emit_statement(&stmt)
        }
    }

    fn emit_expr(&mut self, expr: &Expr) -> Oprand {
        match expr {
            Expr::Ident(x) => Oprand::Ident(x.clone()),
            Expr::Number(x) => Oprand::Literal(*x),
            Expr::UnaryExpr(x) => {
                let rhs = self.emit_expr(x.oprand.as_ref());

                let op = match x.op {
                    ast::UnOp::NegOp => IrOp::SUB,
                    ast::UnOp::PosOp => {return rhs}
                };

                let lhs = Oprand::Literal(Literal::Int(0));
                let res = self.get_tmp_var();
                let tid = self.get_tid();
                

                self.instrs.push(Instr {
                    tid,
                    op,
                    oprands: vec![lhs, rhs]
                });

                Oprand::Ident(res.ident)
            },
            Expr::BinExpr(x) => {
                let lhs = self.emit_expr(x.lhs.as_ref());
                let rhs = self.emit_expr(x.rhs.as_ref());

                // note that the type of a expression result is consist with the type of lhs
                let res = self.get_tmp_var();
                let tid = self.get_tid();

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
                    oprands: vec![lhs, rhs]
                });

                Oprand::Ident(res.ident)
            }
        }
    }

    fn get_tmp_var(&self) -> Symbol {
        Symbol {
            ty: SymType::Buttom,
            /// the tid part of the tmp variable equals the tid of the conrresponding instruction
            ident: format!("tmp_{}", self.tid_cnt + 1)
        }
    }

    fn get_tid(&mut self) -> Tid {
        self.tid_cnt += 1;
        self.tid_cnt
    }

    fn emit_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::AssignStmt(x) => {
                
            },
            Statement::CallStmt(x) => {},
            Statement::DeclStmt(x) => {
                let new_tid = self.get_tid();
                self.instrs.push(Instr {
                    op: IrOp::DECL,
                    oprands: vec![Oprand::Ident(x.ident.clone())],
                    tid: new_tid
                });

                self.symtab.insert(x.ident.clone(), Symbol {
                    ty: match x.ty {
                            ast::DataType::Char => SymType::Char,
                            ast::DataType::Int => SymType::Int
                        },
                    ident: x.ident.clone()
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
    ident: Ident
}

#[derive(Debug, Clone)]
pub struct Instr {
    tid: Tid,
    op: IrOp,
    oprands: Vec<Oprand>
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
    SUB,
    MUL,
    DIV,
    CMP,
    DECL
}

#[derive(Debug, Clone)]
pub enum Oprand {
    Literal(Literal),
    Ident(Ident)
}
