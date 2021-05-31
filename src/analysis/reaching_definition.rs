use std::collections::{HashMap, HashSet};

use crepe::crepe;
use crate::ir::{Instr, InstrTid, InstrType, StrTid};
use petgraph::graph::NodeIndex;

use super::cfg::CFG;

crepe! {
    @input
    struct Gen(NodeIndex, StrTid);
    @input
    struct Next(NodeIndex, NodeIndex);
    @input
    struct Kill(NodeIndex, StrTid);
    
    @output
    struct In(NodeIndex, StrTid);
    @output
    struct Out(NodeIndex, StrTid);

    Out(b, s) <- Gen(b, s);
    Out(b, s) <- In(b, s), !Kill(b, s);
    In(b, s) <- Next(d, b), Out(d, s);
}

/// The context to make a reaching-definition analysis on our program.
/// A reaching-definition analysis determains the available span of a 
/// assignment statement.
pub struct RDContext<'a> {
    cfg: &'a CFG,
    /// All assignment statements and their destination
    assigns: HashMap<InstrTid, StrTid>,
    /// Map variables to the statements which assigns value to them
    assigns_rev: HashMap<StrTid, HashSet<InstrTid>>,
    /// Map block id to its generation set
    /// An instruction is in generation set when it assigns a value to a variable
    gens: HashMap<NodeIndex, HashSet<InstrTid>>,
    /// Map block id to its kill set.
    /// An instruction is in kill set when it assigns a value to a variable and 
    /// there exists another instrcution which assigns to the same variable in the program
    kills: HashMap<NodeIndex, HashSet<InstrTid>>,
    pub res: HashMap<NodeIndex, (HashSet<InstrTid>, HashSet<InstrTid>)>,
}

impl<'a> RDContext<'a> {
    pub fn new(cfg: &'a mut CFG) -> Self {
        let mut res = RDContext {
            cfg,
            assigns: HashMap::new(),
            assigns_rev: HashMap::new(),
            gens: HashMap::new(),
            kills: HashMap::new(),
            res: HashMap::new(),
        };

        // We only care of the variables defined by users.
        // Note that it's impossiable for temporary variables to transfer between
        // basic blocks, so we do not make reaching-definition analysis for them.
        let consider_instr = |instr: &Instr| -> bool {
            if let InstrType::Assign = instr.get_type() {
                true
            } else {
                false
            }
        };

        for (_, sym) in &res.cfg.ir.symtab {
            res.assigns_rev.insert(sym.ident, HashSet::new());
        }
        
        for instr in &res.cfg.ir.instrs {
            if consider_instr(instr) {
                let dest = instr.dest.unwrap();
                res.assigns.insert(instr.tid, dest);
                res.assigns_rev.get_mut(&dest).unwrap().insert(instr.tid);
            }
        }

        for blk in res.cfg.graph.node_indices() {
            let mut gen_set = HashSet::new();
            let mut kill_set = HashSet::new();
            for instr in &res.cfg.graph.node_weight(blk).unwrap().instrs {
                if consider_instr(instr) {
                    let dest = instr.dest.unwrap();
                    let mut gen = HashSet::new();
                    gen.insert(instr.tid);
                    gen_set.extend(&gen);
                    kill_set.extend(res.assigns_rev.get(&dest).unwrap().difference(&gen));
                }
            }

            // println!("{:?}: Gen{:?}: Kill{:?}", blk, gen_set, kill_set);
            res.gens.insert(blk, gen_set);
            res.kills.insert(blk, kill_set);
        }
        res
    }

    pub fn analysis(&mut self) {
        let mut runtime = Crepe::new();
        
        runtime.extend::<Vec<Gen>>(self.gens.iter().map(|(x, y)| -> Vec<Gen> {
            y.iter().map(|i| {
                Gen(*x, *i)
            }).collect()
        }).flatten().collect());

        runtime.extend::<Vec<Kill>>(self.kills.iter().map(|(x, y)| -> Vec<Kill> {
            y.iter().map(|i| {
                Kill(*x, *i)
            }).collect()
        }).flatten().collect());

        runtime.extend::<Vec<Next>>(self.cfg.nexts.iter().map(|(x, y)| -> Next {
            // println!("{:?} -> {:?}", x, y);
            Next(*x, *y)
        }).collect());

        let (ins, outs): (HashSet<In>, HashSet<Out>) = runtime.run();

        for out in outs {
            match self.res.get_mut(&out.0) {
                Some(x) => {x.1.insert(out.1);},
                None => {
                    let mut set = HashSet::new();
                    set.insert(out.1);
                    self.res.insert(out.0, (HashSet::new(), set));
                }
            }
        }

        for i in ins {
            match self.res.get_mut(&i.0) {
                Some(x) => {x.0.insert(i.1);},
                None => panic!("what?")
            }
        }
    }

    #[allow(dead_code)]
    #[allow(dead_code)]
    pub fn pprint(&self) {
        for (k, v) in &self.res {
            print!("\nBlk {:?}:\n", k);
            println!("\tIn{:?}\n\tOut{:?}\n", 
                v.0,
                v.1
            );
        }
    }
}
