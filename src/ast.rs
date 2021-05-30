//! This file provids method to construct our own AST struct from pest parsing results 


use std::{fs::File, io::Read, iter::Rev};
use pest::{Parser, iterators::Pairs, iterators::Pair};

#[derive(Parser)]
#[grammar = "../resources/grammar.pest"]
struct TinyParser;

/// a program is a sequence of statements,
/// for example, a program like:
/// 
/// ```
/// int a;
/// int b;
/// ```
/// 
/// can be parsed to statements:
/// 
/// ```
/// [DeclStmt(DeclStmt { ty: Int, ident: "a" }), DeclStmt(DeclStmt { ty: Int, ident: "b" })]
/// ```

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>
}

fn parse_stmts(src: Pairs<Rule>) -> Vec<Statement> {
    let mut statements = Vec::new();

    for i in src {
        if let Rule::EOI = i.as_rule() {
            break;
        }
        statements.push(Statement::new(i.into_inner()));
    }

    statements
}


impl Program {
    pub fn parse(file: &str) -> Result<Program, String> {
        let mut src_file = File::open(file)
            .expect("Failed when opening file.");

        let mut src_str = String::new();
        src_file.read_to_string(&mut src_str).unwrap();

        let res = TinyParser::parse(Rule::Program, &src_str)
            .or_else(|x| Err(x.to_string()))?;

        let statements = parse_stmts(res);


        Ok(Program {
            statements
        })
    }
}

/// A statement has five forms
/// 
/// - call statement: `fun_name()` or `fun_name(arg1, arg2, ...)`
/// - assign statement: `dest = source_expression`
/// - if statement: `if (condition) then statements end` or 
///   `if (condition) then statements else statements end`
/// - repeat statement: `repeat statements until (condition)`
/// - declaration: `type identifier`
#[derive(Debug, Clone)]
pub enum Statement {
    CallStmt(CallStmt),
    AssignStmt(AssignStmt),
    IfStmt(IfStmt),
    RepeatStmt(RepeatStmt),
    DeclStmt(DeclStmt)
}

impl Statement {
    fn new(mut src: Pairs<Rule>) -> Self {
        let stmt = src.next().unwrap();
        match stmt.as_rule() {
            Rule::DeclStmt => Self::DeclStmt(DeclStmt::new(stmt)),
            Rule::IfStmt => Self::IfStmt(IfStmt::new(stmt)),
            Rule::AssignStmt => Self::AssignStmt(AssignStmt::new(stmt)),
            Rule::CallStmt => Self::CallStmt(CallStmt::new(stmt)),
            Rule::RepeatStmt => Self::RepeatStmt(RepeatStmt::new(stmt)),
            _ => panic!("not implemented")
        }
    }
}

/// else block is optional, note that we do not support else-if statement now
#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub if_block: Vec<Statement>,
    pub else_block: Option<Vec<Statement>>,
}

impl IfStmt {
    fn new(src: Pair<Rule>) -> Self {
        let mut inner = src.into_inner();
        let mut ifcond = inner.next().unwrap().into_inner();
        let ifblk = inner.next().unwrap().into_inner();
        let elseblk = inner.next();

        IfStmt {
            condition: Expr::new(ifcond.next().unwrap()),
            if_block: parse_stmts(ifblk),
            else_block: match elseblk {
                Some(x) => Some(parse_stmts(x.into_inner())),
                None => None
            }
        }
    }
}

/// now we only support int and char data, note that data in char type cannot occur in program as literal
#[derive(Debug, Clone)]
pub enum DataType {
    Int,
    Char
}

impl DataType {
    fn new(src: Pair<Rule>) -> Self {
        match src.as_str() {
            "int" => Self::Int,
            "char" => Self::Char,
            _ => panic!("unexpected internal error at DataType.")
        }
    }
}

type Ident = String;

#[derive(Debug, Clone)]
pub struct DeclStmt {
    pub ty: DataType,
    pub ident: Ident
}

impl DeclStmt {
    fn new(src: Pair<Rule>) -> Self {
        let mut body = src.into_inner();
        
        DeclStmt {
            ty: DataType::new(body.next().unwrap()),
            ident: body.next().unwrap().as_str().into()
        }
    }
}

#[derive(Debug, Clone)]
pub struct RepeatStmt {
    pub statements: Vec<Statement>,
    pub condition: Expr,
}

impl RepeatStmt {
    fn new(src: Pair<Rule>) -> Self {
        let mut inner = src.into_inner();
        
        RepeatStmt {
            statements: parse_stmts(inner.next().unwrap().into_inner()),
            condition: Expr::new(inner.next().unwrap())
        }
    }
}

/// All binary operators, the precedence is as follows:
/// 
/// ```text
/// | 0 | -, + (unary)         |
/// | 1 | *, /                 |
/// | 2 | >, <, ==, !=, <=, >= |
/// | 3 | -, + (binary)        |
/// ```
#[derive(Debug, Clone)]
pub enum BinOp {
    AddOp,
    SubOp,
    MulOp,
    DivOp,
    Le,     // less than or equal
    Lt,     // less than
    Ge,     // greater t"han or equal
    Gt,     // greater than
    Eq,     // equal
    Ne,     // not equal
}

impl BinOp {
    fn new(src: Pair<Rule>) -> Self {
        match src.as_str() {
            "+" => Self::AddOp,
            "-" => Self::SubOp,
            "*" => Self::MulOp,
            "/" => Self::DivOp,
            "<" => Self::Lt,
            "<=" => Self::Le,
            ">" => Self::Gt,
            ">=" => Self::Ge,
            "==" => Self::Eq,
            "!=" => Self::Ne,
            _ => panic!("{:?} op not supported.", src)
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnOp {
    PosOp,
    NegOp,
}

impl UnOp {
    fn new(src: Pair<Rule>) -> Self {
        match src.as_str() {
            "+" => Self::PosOp,
            "-" => Self::NegOp,
            _ => panic!("Op not supported.")
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Int(i32),
    Char(u8)
}

impl From<&str> for Literal {
    fn from(x: &str) -> Self {
        Self::Int(x.parse().unwrap())
    }
}

/// for example, an expression like this:
/// 
/// ```
/// -(c - 1 * 1 - 1 * 1)
/// ```
/// 
/// the parsing tree is:
/// 
/// ![](/Users/ctsinon/Projects/Compiler/rustiny/resources/expr_example.png)
/// 
/// when turned into our AST form:
/// 
/// ![](/Users/ctsinon/Projects/Compiler/rustiny/resources/expr_ast_example.png)
#[derive(Debug, Clone)]
pub enum Expr {
    UnaryExpr(UnaryExpr),
    BinExpr(BinExpr),
    Number(Literal),
    Ident(Ident)
}

impl Expr {
    fn parse_rec(src: &mut Rev<Pairs<Rule>>) -> Self {
        let may_rhs = src.next().unwrap();

        match src.next() {
            None => Self::new(may_rhs),
            Some(x) => {
                Self::BinExpr(BinExpr {
                    rhs: Box::new(Expr::new(may_rhs)),
                    op: BinOp::new(x),
                    lhs: Box::new(Expr::parse_rec(src))
                })
            },
        }
    }

    fn new(src: Pair<Rule>) -> Self {
        match src.as_rule() {
            Rule::Number => Self::Number(src.as_str().into()),
            Rule::Ident => Self::Ident(src.as_str().into()),
            Rule::UnaryExpr => Self::UnaryExpr(UnaryExpr::new(src)),
            _ => Self::parse_rec(&mut src.into_inner().rev())
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnOp,
    pub oprand: Box<Expr>
}

impl UnaryExpr {
    fn new(src: Pair<Rule>) -> Self {
        let mut inner = src.into_inner();

        UnaryExpr {
            op: UnOp::new(inner.next().unwrap()),
            oprand: Box::new(Expr::new(inner.next().unwrap()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinExpr {
    pub lhs: Box<Expr>,
    pub op: BinOp,
    pub rhs: Box<Expr>
}

#[derive(Debug, Clone)]
pub struct AssignStmt {
    pub dest: String,
    pub src: Expr
}

impl AssignStmt {
    fn new(src: Pair<Rule>) -> Self {
        let mut inner = src.into_inner();
        AssignStmt {
            dest: inner.next().unwrap().as_str().into(),
            src: Expr::new(inner.next().unwrap())
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallStmt {
    pub func: String,
    pub args: Option<Vec<Expr>>
}

impl CallStmt {
    fn new(src: Pair<Rule>) -> Self {
        let mut inner = src.into_inner();

        CallStmt {
            func: inner.next().unwrap().as_str().into(),
            args: match inner.peek().unwrap().into_inner().next() {
                None => None,
                Some(_) => Some(inner.next().unwrap().into_inner().map(|x| {
                    Expr::new(x)
                }).collect())
            }
        }
    }
}

