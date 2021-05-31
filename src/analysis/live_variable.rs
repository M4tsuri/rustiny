use std::collections::{HashMap, HashSet};

use crepe::crepe;
use crate::ir::{InstrTid, InstrType, StrTid};
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

    In(b, s) <- Gen(b, s);
    In(b, s) <- Out(b, s), !Kill(b, s);
    Out(b, s) <- Next(b, d), In(d, s);
}

/// The context to make a live variable analysis.
/// This analysis determines the blocks a variable is used.
/// Later we can build a du chain with the information we got in 
/// live variable analysis and reaching definition analysis
pub struct LVContext<'a> {
    cfg: &'a CFG<'a>,
    /// Map block id to its generation set
    /// A symbol is in generation set when it's used before killed(re-assigned) in this block
    gens: HashMap<NodeIndex, HashSet<StrTid>>,
    /// A collection of the use information in a block, it's useful when building the du-chain
    uses: HashMap<NodeIndex, HashMap<StrTid, Vec<InstrTid>>>,
    /// Map block id to its kill set.
    /// A symbol is in kill set when it's assigned a new value in this block
    kills: HashMap<NodeIndex, HashSet<StrTid>>,
    pub res: HashMap<NodeIndex, (HashSet<StrTid>, HashSet<StrTid>)>,
}

impl<'a> LVContext<'a> {
    pub fn new(cfg: &'a mut CFG) -> Self {
        let mut res = LVContext {
            cfg,
            gens: HashMap::new(),
            uses: HashMap::new(),
            kills: HashMap::new(),
            res: HashMap::new(),
        };

        for blk in res.cfg.graph.node_indices() {
            let mut gen_set = HashSet::new();
            let mut kill_set = HashSet::new();
            let mut use_map: HashMap<InstrTid, Vec<InstrTid>> = HashMap::new();
            for instr in &res.cfg.graph.node_weight(blk).unwrap().instrs {
                // note that we must extend generation set first, for example:
                // a <- add a, b
                // here a is used before assigned
                let used_set = instr.symbols_used();
                for u in &used_set {
                    match use_map.get_mut(u) {
                        Some(x) => x.push(instr.tid),
                        None => {use_map.insert(*u, vec![instr.tid]);}
                    }
                }
                gen_set.extend(used_set);
                if let InstrType::Assign = instr.get_type() {
                    kill_set.insert(instr.dest.unwrap());
                }
            }

            // println!("{:?}: Gen{:?}: Kill{:?}", blk, gen_set, kill_set);
            res.gens.insert(blk, gen_set);
            res.kills.insert(blk, kill_set);
            res.uses.insert(blk, use_map);
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
    pub fn pprint(&self) {
        for (k, v) in &self.res {
            print!("\nBlk {:?}:\n", k);
            println!("\tIn{:?}\n\tOut{:?}\n", 
                v.0.iter().map(|x| {
                    self.cfg.ir.get_str(*x).unwrap()
                }).collect::<Vec<&String>>(),
                v.1.iter().map(|x| {
                    self.cfg.ir.get_str(*x).unwrap()
                }).collect::<Vec<&String>>()
            );
        }
    }
}
