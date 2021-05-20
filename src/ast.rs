use std::{fs::File, io::Read};
use pest::{Parser, iterators::Pairs, iterators::Pair};

#[derive(Parser)]
#[grammar = "../resources/grammar.pest"]
struct TinyParser;

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct IfStmt {
    condition: Expr,
    if_block: Vec<Statement>,
    else_block: Option<Vec<Statement>>,
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct DeclStmt {
    ty: DataType,
    ident: Ident
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

#[derive(Debug)]
pub struct RepeatStmt {
    statements: Vec<Statement>,
    condition: Expr,
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

#[derive(Debug)]
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
            _ => panic!("Op not supported.")
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Expr {
    UnaryExpr(Box<UnaryExpr>),
    BinExpr(Box<BinExpr>)
}

impl Expr {
    fn new(src: Pair<Rule>) -> Self {
        println!("{:?}\n\n", src);
        let mut check = src.clone().into_inner().take(2);
        check.next().unwrap();

        match check.next() {
            None => {println!("{:?}", 123);Self::UnaryExpr(Box::new(UnaryExpr::new(src)))},
            Some(_) => Self::BinExpr(Box::new(BinExpr::new(src)))
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    op: Option<UnOp>,
    oprand: Expr
}

impl UnaryExpr {
    fn new(src: Pair<Rule>) -> Self {
        let mut inner = src.into_inner();
        let mut first = inner.next().unwrap();
        let mut op: Option<UnOp> = None;

        if let Rule::UnOp = first.as_rule() {
            op = Some(UnOp::new(first));
            first = inner.next().unwrap();
        }

        UnaryExpr {
            op,
            oprand: Expr::new(first)
        }
    }
}

#[derive(Debug)]
pub struct BinExpr {
    lhs: Box<Expr>,
    op: BinOp,
    rhs: Box<Expr>
}

impl BinExpr {
    fn new(src: Pair<Rule>) -> Self {
        let mut inner = src.into_inner();

        BinExpr {
            lhs: Box::new(Expr::new(inner.next().unwrap())),
            op: BinOp::new(inner.next().unwrap()),
            rhs: Box::new(Expr::new(inner.next().unwrap()))
        }
    }
}

#[derive(Debug)]
pub struct AssignStmt {
    dest: String,
    src: Expr
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

#[derive(Debug)]
pub struct CallStmt {
    func: String,
    args: Vec<Expr>
}

impl CallStmt {
    fn new(src: Pair<Rule>) -> Self {
        let mut inner = src.into_inner();
        let func = inner.next().unwrap().as_str().into();
        let mut args = Vec::new();

        for arg in inner.next().unwrap().into_inner() {
            args.push(Expr::new(arg));
        }

        CallStmt {
            func,
            args
        }
    }
}

