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

use std::{collections::{HashMap, HashSet}, fmt::Debug, usize};

use ast::Rule;
use pest::{Span, error::Error};
use crate::ast::{self, Expr, Statement, BinOp, Literal};

pub type InstrTid = usize;
pub type StrTid = usize;
type Ident = String;

#[derive(Debug, Clone)]
pub struct Program<'a> {
    stmts: Vec<Statement<'a>>,
    pub instrs: Vec<Instr>,
    /// symbol id map to symbol name
    strtab: HashMap<StrTid, Ident>,
    /// symbol name map to symbol id
    strmap: HashMap<Ident, StrTid>,
    pub symtab: HashMap<Ident, Symbol>,
    str_tid_cnt: StrTid
}

impl<'a> Program<'a> {
    pub fn get_symbol(&self, tid: StrTid) -> &Symbol {
        let name = self.get_str(tid).unwrap();
        self.symtab.get(name).unwrap()
    }

    #[allow(dead_code)]
    pub fn instrs_pprint(&self, instrs: &Vec<Instr>) -> String {
        instrs.iter().map(|instr| {
            format!("{}  {}{:?} {}", 
                instr.tid, 
                match instr.dest {
                    Some(x) => self.get_str(x).unwrap().clone() + " <- ",
                    _ => String::new(),
                },
                instr.op,
                instr.src.iter().map(|x| {
                    x.pprint(self)
                }).collect::<Vec<String>>().join(", "))
        }).collect::<Vec<String>>().join("\n")
    }

    #[allow(dead_code)]
    pub fn pprint(&self) {
        println!("{}", self.instrs_pprint(&self.instrs));
    }

    pub fn new(ast: ast::Program<'a>) -> Self {
        Program {
            stmts: ast.statements,
            instrs: Vec::new(),
            strtab: HashMap::new(),
            strmap: HashMap::new(),
            symtab: HashMap::new(),
            str_tid_cnt: 0
        }
    }

    pub fn prepare_builtin_funcs(&mut self) {
        let mut prepare_builtin_func = |name: &str, proto: FuncPrototype| {
            self.prepare_var(
                &name.into(), 
                SymType::Func(proto),
                false);
        };

        prepare_builtin_func("write", FuncPrototype {
            args: vec![Box::new(SymType::Int)],
            ret_val: None
        });

        prepare_builtin_func("read", FuncPrototype {
            args: vec![Box::new(SymType::Int)],
            ret_val: None
        });
    }

    pub fn emit(&mut self) -> Result<(), String> {
        self.instrs.push(Instr {
            tid: self.instrs.len(),
            op: IrOp::ENTRY,
            dest: None,
            src: Vec::new(),
        });

        self.prepare_builtin_funcs();
        self.emit_stmts(&self.stmts.clone())
            .or_else(|x| {
                Err(x.to_string())
            })?;

        self.instrs.push(Instr {
            tid: self.instrs.len(),
            op: IrOp::END,
            dest: None,
            src: Vec::new(),
        });

        Ok(())
    }

    fn emit_stmts(&mut self, stmts: &Vec<Statement>) -> Result<(), Error<Rule>> {
        for stmt in stmts {
            self.emit_stmt(stmt)?;
        }
        Ok(())
    }

    pub fn get_str(&self, id: StrTid) -> Option<&String> {
        self.strtab.get(&id)
    }

    /// receieve a variable name and initialize the corresponding hashmap entries (if needed).
    /// returns the id of the name in string table
    fn prepare_var(&mut self, ident: &Ident, ty: SymType, is_tmp: bool) -> StrTid {
        match self.strmap.get(ident) {
            Some(x) => *x,
            None => {
                let tid = self.get_str_tid();
                self.strtab.insert(tid, ident.clone());
                self.strmap.insert(ident.clone(), tid);
                self.symtab.insert(ident.clone(), Symbol {
                    ty,
                    ident: tid,
                    is_tmp,
                });
                tid
            }
        }
    }

    fn get_var(&mut self, ident: &Ident, span: &Span) -> Result<Symbol, Error<Rule>> {
        match self.symtab.get(ident) {
            Some(x) => Ok(x.clone()),
            None => {
                Err(pest::error::Error::new_from_span(
                    pest::error::ErrorVariant::CustomError {
                        message: String::from("variable must be declared before used.")
                    }, 
                    span.clone()))
            }
        }
    }

    fn emit_bool_expr(&mut self, expr: &Expr) -> Result<(), pest::error::Error<Rule>> {
        let may_next = self.emit_expr(expr)?;
        if let Expr::BinExpr(e) = expr {
            match e.op {
                BinOp::Eq | BinOp::Ge | BinOp::Gt
                | BinOp::Le | BinOp::Lt | BinOp::Ne => return Ok(()),
                _ => {}
            }
        }

        // here we need to create a bool expression from a non-bool expression
        // simplely compare the return value of the non-bool expression with 0
        let dest = self.get_tmp_var(SymType::Bool);
        self.instrs.push(Instr {
            tid: self.instrs.len(),
            dest: Some(dest),
            op: IrOp::CMP,
            src: vec![may_next, Oprand::Literal(Literal::Int(0))]
        });
        Ok(())
    }

    fn emit_expr(&mut self, expr: &Expr) -> Result<Oprand, pest::error::Error<Rule>> {
        match expr {
            Expr::Ident(x) => Ok(Oprand::Ident(self.get_var(&x.name, &x.span)?.ident)),
            Expr::Number(x) => Ok(Oprand::Literal(x.content)),
            Expr::UnaryExpr(x) => {
                let rhs = self.emit_expr(x.oprand.as_ref())?;

                let op = match x.op {
                    ast::UnOp::NegOp => IrOp::SUB,
                    ast::UnOp::PosOp => {return Ok(rhs)}
                };

                let lhs = Oprand::Literal(Literal::Int(0));
                let ty = lhs.combine_type(&rhs, &self, &x.span)?;
                
                let dest = self.get_tmp_var(ty);
                

                self.instrs.push(Instr {
                    tid: self.instrs.len(),
                    op,
                    dest: Some(dest),
                    src: vec![lhs, rhs]
                });

                Ok(Oprand::Ident(dest))
            },
            Expr::BinExpr(x) => {
                let lhs = self.emit_expr(x.lhs.as_ref())?;
                let rhs = self.emit_expr(x.rhs.as_ref())?;

                let mut ty = lhs.combine_type(&rhs, &self, &x.span)?;

                if let SymType::Int = ty {}
                else {
                    return Err(pest::error::Error::new_from_span(
                        pest::error::ErrorVariant::CustomError {
                            message: String::from("only int type is allowed in computation.")
                        }, 
                        x.span.clone()))
                }
                    
                let op = match x.op {
                    BinOp::AddOp => IrOp::ADD,
                    BinOp::DivOp => IrOp::DIV,
                    BinOp::MulOp => IrOp::MUL,
                    BinOp::SubOp => IrOp::SUB,
                    // the rest are comparsion ops
                    BinOp::Eq | BinOp::Ge | BinOp::Gt
                    | BinOp::Le | BinOp::Lt | BinOp::Ne => {
                        ty = SymType::Bool;
                        IrOp::CMP
                    }
                };

                // note that the type of a expression result is consist with the type of lhs
                let dest = self.get_tmp_var(ty);

                self.instrs.push(Instr {
                    tid: self.instrs.len(),
                    op,
                    dest: Some(dest),
                    src: vec![lhs, rhs]
                });

                Ok(Oprand::Ident(dest))
            }
        }
    }

    fn get_tmp_var(&mut self, ty: SymType) -> StrTid {
        let name = format!("tmp#{}", self.str_tid_cnt);
        let tid = self.prepare_var(&name, ty, true);
        tid
    }

    fn get_str_tid(&mut self) -> StrTid {
        self.str_tid_cnt += 1;
        self.str_tid_cnt - 1
    }

    fn emit_stmt(&mut self, stmt: &Statement) -> Result<(), pest::error::Error<Rule>> {
        match stmt {
            Statement::AssignStmt(x) => {
                let src = self.emit_expr(&x.src)?;
                let dest = self.get_var(&x.dest, &x.span)?;
                
                if dest.ty != src.to_sym_type(self) {
                    return Err(pest::error::Error::new_from_span(
                        pest::error::ErrorVariant::CustomError {
                            message: format!("conflict type detected: {:?} v.s. {:?}.", 
                                dest.ty, 
                                src.to_sym_type(self))
                        }, 
                        x.span.clone()))
                }

                self.instrs.push(Instr {
                    tid: self.instrs.len(),
                    op: IrOp::MOV,
                    dest: Some(dest.ident),
                    src: vec![src]
                });
                Ok(())
            },
            Statement::CallStmt(x) => {
                let mut src = vec![Oprand::Ident(*self.strmap.get(&x.func)
                    .expect("unexpetced function name."))];

                let func_sym = match self.symtab.get(&x.func).unwrap().ty.clone() {
                    SymType::Func(t) => t,
                    _ => panic!("inconsist built-in function type.")
                };


                match &x.args {
                    None => {},
                    Some(exprs) => {
                        for expr in exprs {
                            src.push(self.emit_expr(expr)?)
                        }
                    }
                };

                if func_sym.args.len() != src.len() - 1 {
                    panic!("inconsist number of function arguments.")
                }
                

                self.instrs.push(Instr {
                    tid: self.instrs.len(),
                    dest: None,
                    op: IrOp::CALL,
                    src
                });

                Ok(())
            },
            Statement::DeclStmt(x) => {
                let ty = match x.ty {
                    ast::DataType::Char => SymType::Char,
                    ast::DataType::Int => SymType::Int
                };

                let dest = self.prepare_var(&x.ident.name, ty, false);

                self.instrs.push(Instr {
                    op: IrOp::DECL,
                    tid: self.instrs.len(),
                    dest: None,
                    src: vec![Oprand::Ident(dest)]
                });

                Ok(())
            },
            Statement::IfStmt(x) => {
                self.emit_bool_expr(&x.condition)?;

                let jmp_idx = self.instrs.len();

                // this jmp instruction acts as a placeholder, we will modify it later
                self.instrs.push(Instr {
                    tid: jmp_idx,
                    op: IrOp::JNZ,
                    dest: None,
                    src: Vec::new()
                });

                if let Some(s) = &x.else_block {
                    self.emit_stmts(s)?;
                }

                let if_tid = self.instrs.len();
                self.emit_stmts(&x.if_block)?;

                let mut jmp = self.instrs.get_mut(jmp_idx).unwrap();
                jmp.src.push(Oprand::Target(if_tid));

                if let Expr::BinExpr(b) = &x.condition {
                    jmp.op = IrOp::comp2jmp(&b.op);
                }

                Ok(())
            },
            Statement::RepeatStmt(x) => {
                let blk_tid = self.instrs.len();

                self.emit_stmts(&x.statements)?;
                self.emit_bool_expr(&x.condition)?;

                let mut jmp_op = IrOp::JNZ;

                if let Expr::BinExpr(b) = &x.condition {
                    jmp_op = IrOp::comp2jmp(&b.op).comp_rev();
                }

                self.instrs.push(Instr {
                    tid: self.instrs.len(),
                    op: jmp_op,
                    dest: None,
                    src: vec![Oprand::Target(blk_tid)]
                });

                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncPrototype {
    args: Vec<Box<SymType>>,
    ret_val: Option<Box<SymType>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymType {
    Func(FuncPrototype),
    Int,
    Char,
    /// this is a type to be discovered with static analysis
    Bool
}

#[derive(Debug, Clone)]
pub struct Symbol {
    ty: SymType,
    pub ident: StrTid,
    pub is_tmp: bool
}

#[derive(Debug, Clone)]
pub struct Instr {
    pub tid: InstrTid,
    pub op: IrOp,
    /// the destination of an instruction can only be a symbol name
    pub dest: Option<StrTid>,
    pub src: Vec<Oprand>
}

pub enum InstrType {
    CondJmp,
    #[allow(dead_code)]
    DirectJmp,
    FuncCall,
    Assign,
    /// instructions with no affects when executing.
    /// these instructions are only useful when analyzing
    Nop
}

impl Instr {
    /// return a set of names of symbols which is used as source in this instruction.
    /// this method is useful when making a live variable analysis
    pub fn symbols_used(&self) -> HashSet<StrTid> {
        let mut res = HashSet::new();
        let mut iter = self.src.iter();
        if let IrOp::CALL = self.op {
            iter.next();
        }

        for op in iter {
            if let Oprand::Ident(id) = op {
                res.insert(*id);
            }
        }
        res
    }

    pub fn get_type(&self) -> InstrType {
        match self.op {
            IrOp::JG | IrOp::JZ | IrOp::JNZ
            | IrOp::JGE | IrOp::JL | IrOp::JLE => InstrType::CondJmp,
            IrOp::CALL => InstrType::FuncCall,
            IrOp::ADD | IrOp::DIV | IrOp::MOV
            | IrOp::MUL | IrOp::SUB | IrOp::CMP => InstrType::Assign,
            IrOp::DECL | IrOp::END | IrOp::ENTRY => InstrType::Nop
        }
    }

    pub fn is_branch(&self) -> bool {
        match self.get_type() {
            InstrType::CondJmp | InstrType::DirectJmp => true,
            _ => false
        }
    }

    pub fn get_branch_target(&self) -> InstrTid {
        if let Oprand::Target(x) = self.src.first().unwrap() {
            *x
        } else {
            panic!("internal error: corrupted branch instruction.")
        }
    }
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
    ENTRY,
    END,
    DECL
}

impl IrOp {
    pub fn comp_rev(self) -> Self {
        match self {
            IrOp::JNZ => IrOp::JZ,
            IrOp::JL => IrOp::JGE,
            IrOp::JLE => IrOp::JG,
            IrOp::JG => IrOp::JLE,
            IrOp::JGE => IrOp::JL,
            IrOp::JZ => IrOp::JNZ,
            _ => panic!("internal error: inconsist comparsion op type.")
        }
    }

    pub fn comp2jmp(binop: &BinOp) -> IrOp {
        match binop {
            BinOp::Eq => IrOp::JZ,
            BinOp::Ge => IrOp::JGE,
            BinOp::Gt => IrOp::JG,
            BinOp::Le => IrOp::JLE,
            BinOp::Lt => IrOp::JL,
            BinOp::Ne => IrOp::JNZ,
            _ => IrOp::JNZ
        }
    }
}

#[derive(Debug, Clone)]
pub enum Oprand {
    Literal(Literal),
    Ident(StrTid),
    Target(InstrTid)
}

impl Oprand {
    fn to_sym_type(&self, ir: &Program) -> SymType {
        match self {
            &Oprand::Ident(x) => ir.get_symbol(x).ty.clone(),
            &Oprand::Literal(x) => x.to_sym_type(),
            &Oprand::Target(_) => panic!("internal error: trying to get symbol type from branch target.")
        }
    }

    fn combine_type(&self, oth: &Oprand, ir: &Program, span: &Span) -> Result<SymType, Error<Rule>> {
        match (self, oth) {
            (Oprand::Target(_), _) | (_, Oprand::Target(_)) => Err(pest::error::Error::new_from_span(
                pest::error::ErrorVariant::CustomError {
                    message: String::from("internal error.")
                },
                span.clone())),
            _ => {
                let lt = self.to_sym_type(ir);
                let rt = oth.to_sym_type(ir);
                if lt == rt {
                    Ok(lt)
                } else {
                    Err(pest::error::Error::new_from_span(
                        pest::error::ErrorVariant::CustomError {
                            message: format!("conflict type detected: {:?} v.s. {:?}.", lt, rt)
                        },
                        span.clone()))
                }
            }
        }

    }

    fn pprint(&self, ir: &Program) -> String {
        match self {
            Oprand::Ident(x) => ir.get_str(*x).unwrap().clone(),
            Oprand::Literal(x) => format!("{:?}", x),
            Oprand::Target(x) => x.to_string()
        }
    }
}
