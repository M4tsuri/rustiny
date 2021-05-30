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

pub type InstrTid = usize;
type StrTid = usize;
type Ident = String;

#[derive(Debug, Clone)]
pub struct Program {
    stmts: Vec<Statement>,
    pub instrs: Vec<Instr>,
    /// symbol id map to symbol name
    strtab: HashMap<StrTid, Ident>,
    /// symbol name map to symbol id
    strmap: HashMap<Ident, StrTid>,
    symtab: HashMap<Ident, Symbol>,
    str_tid_cnt: StrTid
}

macro_rules! prepare_builtin_func {
    ($s:expr, $e:expr, $proto:expr) => {
        let prepare_builtin_idx = $s.prepare_var(&$e.into());
        $s.symtab.insert($e.into(), Symbol {
            ty: SymType::Func($proto),
            ident: prepare_builtin_idx,
            is_tmp: false
        });
    }
}

impl Program {
    pub fn instrs_pprint(&self, instrs: &Vec<Instr>) -> String {
        instrs.iter().map(|instr| {
            format!("{}  {}{:?} {}", 
                instr.tid, 
                match instr.dest {
                    Some(Oprand::Ident(x)) => self.get_str(x).unwrap().clone() + " <- ",
                    _ => String::new(),
                },
                instr.op,
                instr.src.iter().map(|x| {
                    x.pprint(self)
                }).collect::<Vec<String>>().join(", "))
        }).collect::<Vec<String>>().join("\n")
    }

    pub fn pprint(&self) {
        println!("{}", self.instrs_pprint(&self.instrs));
    }

    pub fn new(ast: ast::Program) -> Self {
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
        prepare_builtin_func!(self, "write", FuncPrototype {
            args: vec![Box::new(SymType::Int)],
            ret_val: None
        });
        prepare_builtin_func!(self, "read", FuncPrototype {
            args: vec![Box::new(SymType::Int)],
            ret_val: None
        });
    }

    pub fn emit(&mut self) {
        self.instrs.push(Instr {
            tid: self.instrs.len(),
            op: IrOp::ENTRY,
            dest: None,
            src: Vec::new(),
        });

        self.prepare_builtin_funcs();
        self.emit_stmts(&self.stmts.clone());

        self.instrs.push(Instr {
            tid: self.instrs.len(),
            op: IrOp::END,
            dest: None,
            src: Vec::new(),
        })
    }

    fn emit_stmts(&mut self, stmts: &Vec<Statement>) {
        for stmt in stmts {
            self.emit_stmt(stmt)
        }
    }

    pub fn get_str(&self, id: StrTid) -> Option<&String> {
        self.strtab.get(&id)
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

    fn emit_bool_expr(&mut self, expr: &Expr) {
        let may_next = self.emit_expr(expr);
        if let Expr::BinExpr(e) = expr {
            match e.op {
                BinOp::Eq | BinOp::Ge | BinOp::Gt
                | BinOp::Le | BinOp::Lt | BinOp::Ne => return,
                _ => {}
            }
        }

        // here we need to create a bool expression from a non-bool expression
        // simplely compare the return value of the non-bool expression with 0
        let dest = Oprand::Ident(self.get_tmp_var());
        self.instrs.push(Instr {
            tid: self.instrs.len(),
            dest: Some(dest),
            op: IrOp::CMP,
            src: vec![may_next, Oprand::Literal(Literal::Int(0))]
        })
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
                let dest = Oprand::Ident(self.get_tmp_var());
                

                self.instrs.push(Instr {
                    tid: self.instrs.len(),
                    op,
                    dest: Some(dest.clone()),
                    src: vec![lhs, rhs]
                });

                dest
            },
            Expr::BinExpr(x) => {
                let lhs = self.emit_expr(x.lhs.as_ref());
                let rhs = self.emit_expr(x.rhs.as_ref());

                // note that the type of a expression result is consist with the type of lhs
                let dest = Oprand::Ident(self.get_tmp_var());

                self.instrs.push(Instr {
                    tid: self.instrs.len(),
                    op: match x.op {
                        BinOp::AddOp => IrOp::ADD,
                        BinOp::DivOp => IrOp::DIV,
                        BinOp::MulOp => IrOp::MUL,
                        BinOp::SubOp => IrOp::SUB,
                        // the rest are comparsion ops
                        BinOp::Eq | BinOp::Ge | BinOp::Gt
                        | BinOp::Le | BinOp::Lt | BinOp::Ne => IrOp::CMP
                    },
                    dest: Some(dest.clone()),
                    src: vec![lhs, rhs]
                });

                dest
            }
        }
    }

    fn get_tmp_var(&mut self) -> StrTid {
        let name = format!("tmp#{}", self.str_tid_cnt);
        let tid = self.prepare_var(&name);
        self.symtab.insert(name, Symbol {
            ty: SymType::Buttom,
            ident: tid,
            is_tmp: true
        });
        tid
    }

    fn get_str_tid(&mut self) -> StrTid {
        self.str_tid_cnt += 1;
        self.str_tid_cnt - 1
    }

    fn emit_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::AssignStmt(x) => {
                let src = self.emit_expr(&x.src);
                let dest = Oprand::Ident(self.prepare_var(&x.dest));

                self.instrs.push(Instr {
                    tid: self.instrs.len(),
                    op: IrOp::MOV,
                    dest: Some(dest),
                    src: vec![src]
                })
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
                    Some(x) => src.extend(x.iter().map(|e| {
                        self.emit_expr(e)
                    }))
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
            },
            Statement::DeclStmt(x) => {
                let dest = self.prepare_var(&x.ident);

                self.instrs.push(Instr {
                    op: IrOp::DECL,
                    tid: self.instrs.len(),
                    dest: None,
                    src: vec![Oprand::Ident(dest)]
                });

                self.symtab.insert(x.ident.clone(), Symbol {
                    ty: match x.ty {
                            ast::DataType::Char => SymType::Char,
                            ast::DataType::Int => SymType::Int
                        },
                    ident: dest,
                    is_tmp: false
                });
            },
            Statement::IfStmt(x) => {
                self.emit_bool_expr(&x.condition);

                let jmp_idx = self.instrs.len();

                // this jmp instruction acts as a placeholder, we will modify it later
                self.instrs.push(Instr {
                    tid: jmp_idx,
                    op: IrOp::JNZ,
                    dest: None,
                    src: Vec::new()
                });

                if let Some(s) = &x.else_block {
                    self.emit_stmts(s);
                }

                let if_tid = self.instrs.len();
                self.emit_stmts(&x.if_block);

                let mut jmp = self.instrs.get_mut(jmp_idx).unwrap();
                jmp.src.push(Oprand::Target(if_tid));

                if let Expr::BinExpr(b) = &x.condition {
                    jmp.op = IrOp::comp2jmp(&b.op);
                }
            },
            Statement::RepeatStmt(x) => {
                let blk_tid = self.instrs.len();

                self.emit_stmts(&x.statements);
                self.emit_bool_expr(&x.condition);

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
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncPrototype {
    args: Vec<Box<SymType>>,
    ret_val: Option<Box<SymType>>
}

#[derive(Debug, Clone)]
pub enum SymType {
    Func(FuncPrototype),
    Int,
    Char,
    /// this is a type to be discovered with static analysis
    Buttom
}

#[derive(Debug, Clone)]
pub struct Symbol {
    ty: SymType,
    ident: StrTid,
    is_tmp: bool
}

#[derive(Debug, Clone)]
pub struct Instr {
    pub tid: InstrTid,
    pub op: IrOp,
    pub dest: Option<Oprand>,
    pub src: Vec<Oprand>
}

pub enum InstrType {
    CondJmp,
    DirectJmp,
    FuncCall,
    SeqInstr
}

impl Instr {
    pub fn get_type(&self) -> InstrType {
        match self.op {
            IrOp::JG | IrOp::JZ | IrOp::JNZ
            | IrOp::JGE | IrOp::JL | IrOp::JLE => InstrType::CondJmp,
            IrOp::CALL => InstrType::FuncCall,
            _ => InstrType::SeqInstr
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
    fn pprint(&self, ir: &Program) -> String {
        match self {
            Oprand::Ident(x) => ir.get_str(*x).unwrap().clone(),
            Oprand::Literal(x) => format!("{:?}", x),
            Oprand::Target(x) => x.to_string()
        }
    }
}
