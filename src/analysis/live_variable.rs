use std::collections::{HashMap, HashSet};

use crepe::crepe;
use crate::ir::{InstrTid, InstrType, StrTid};
use petgraph::graph::NodeIndex;

use super::cfg::CFG;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum DUType {
    Def,
    Use
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct DUInstr {
    ty: DUType,
    sym: StrTid,
    tid: InstrTid
}

crepe! {
    @input
    struct Gen(NodeIndex, DUInstr);
    @input
    struct Next(NodeIndex, NodeIndex);
    @input
    struct Kill(NodeIndex, DUInstr);
    
    struct In(NodeIndex, DUInstr);
    struct Out(NodeIndex, DUInstr);

    @output
    struct DULink(DUInstr, DUInstr);

    In(b, s) <- Gen(b, s);
    In(b, s) <- Out(b, s), !Kill(b, s);

    // def-use link appears between two blocks
    // 1. def in b and use in d
    DULink(p, q) <- Next(b, d), Out(b, p), Gen(d, q), (p.sym == q.sym);
    // 2. both def and use in b, def before use
    DULink(p, q) <- Kill(d, p), Gen(d, q), (p.sym == q.sym), (p.tid >= q.tid);
    // 3. two uses in b
    DULink(p, q) <- Gen(d, p), Gen(d, q), (p.sym == q.sym), (p.tid > q.tid);

    Out(b, s) <- Next(b, d), In(d, s);
}

/// The context to make a live variable analysis.
/// This analysis determines the blocks a variable is used.
/// Later we can build a du chain with the information we got in 
/// live variable analysis and reaching definition analysis
pub struct LVContext{
    /// Map block id to its generation set
    /// A symbol is in generation set when it's used before killed(re-assigned) in this block
    gens: HashMap<NodeIndex, HashSet<DUInstr>>,
    /// A collection of the use information in a block, it's useful when building the du-chain
    pub uses: HashMap<NodeIndex, HashMap<DUInstr, Vec<InstrTid>>>,
    /// Map block id to its kill set.
    /// A symbol is in kill set when it's assigned a new value in this block
    kills: HashMap<NodeIndex, HashSet<DUInstr>>,
    nexts: HashSet<(NodeIndex, NodeIndex)>,
    pub res: HashMap<NodeIndex, (HashSet<DUInstr>, HashSet<DUInstr>)>,
}

impl LVContext {
    pub fn new(cfg: &CFG) -> Self {
        let mut res = LVContext {
            gens: HashMap::new(),
            uses: HashMap::new(),
            kills: HashMap::new(),
            res: HashMap::new(),
            nexts: cfg.nexts.clone()
        };

        for blk in cfg.graph.node_indices() {
            let mut gen_set = HashSet::new();
            let mut kill_set = HashSet::new();
            for instr in &cfg.graph.node_weight(blk).unwrap().instrs {
                // note that we must extend generation set first, for example:
                // a <- add a, b
                // here a is used before assigned
                let used_set = instr.symbols_used();

                gen_set.extend(used_set.iter().map(|x| {
                    DUInstr {
                        ty: DUType::Use,
                        sym: *x,
                        tid: instr.tid
                    }
                }));

                if let InstrType::Assign = instr.get_type() {
                    kill_set.insert(DUInstr {
                        ty: DUType::Def,
                        sym: instr.dest.unwrap(),
                        tid: instr.tid
                    });
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

        runtime.extend::<Vec<Next>>(self.nexts.iter().map(|(x, y)| -> Next {
            // println!("{:?} -> {:?}", x, y);
            Next(*x, *y)
        }).collect());

        let (res,) = runtime.run();

    }

    #[allow(dead_code)]
    pub fn pprint(&self, cfg: &CFG) {
        for (k, v) in &self.res {
            print!("\nBlk {:?}:\n", k);
            println!("\tIn{:?}\n\tOut{:?}\n", 
                v.0.iter().map(|x| {
                    cfg.ir.get_str(*x).unwrap()
                }).collect::<Vec<&String>>(),
                v.1.iter().map(|x| {
                    cfg.ir.get_str(*x).unwrap()
                }).collect::<Vec<&String>>()
            );
        }
    }
}
