use std::collections::{HashMap, HashSet};

use crepe::crepe;
use crate::ir::{InstrTid, InstrType, StrTid};
use petgraph::graph::{DiGraph, NodeIndex};

use super::cfg::CFG;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum DUType {
    Def,
    Use
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DUNode {
    ty: DUType,
    sym: StrTid,
    tid: InstrTid
}

crepe! {
    @input
    struct Gen(NodeIndex, DUNode);
    @input
    struct Next(NodeIndex, NodeIndex);
    @input
    struct Kill(NodeIndex, DUNode);
    
    struct In(NodeIndex, DUNode);
    struct Out(NodeIndex, DUNode);

    @output
    struct UDLink(DUNode, DUNode);

    In(b, s) <- Gen(b, s);
    In(b, s) <- Out(b, s), Kill(b, t), (!s.sym == t.sym);

    // def-use link appears between two blocks
    // 1. def/use in b and def/use in d
    UDLink(p, q) <- Next(b, d), In(d, p), Kill(b, q), 
                    (p.sym == q.sym && p.tid >= q.tid);

    UDLink(p, q) <- Next(b, d), In(d, p), Gen(b, q), 
                    (p.sym == q.sym && p.tid > q.tid);

    // 2. both def and use in b, def before use
    UDLink(p, q) <- Gen(d, p), Kill(d, q), (p.sym == q.sym && p.tid >= q.tid);
    // 3. two uses in b
    UDLink(p, q) <- Gen(d, p), Gen(d, q), (p.sym == q.sym && p.tid > q.tid);

    Out(b, s) <- Next(b, d), In(d, s);
}

/// The context to make a live variable analysis.
/// This analysis determines the blocks a variable is used.
/// Later we can build a du chain with the information we got in 
/// live variable analysis and reaching definition analysis
pub struct LVContext{
    /// Map block id to its generation set
    /// A symbol is in generation set when it's used before killed(re-assigned) in this block
    gens: HashMap<NodeIndex, HashSet<DUNode>>,
    /// Map block id to its kill set.
    /// A symbol is in kill set when it's assigned a new value in this block
    kills: HashMap<NodeIndex, HashSet<DUNode>>,
    nexts: HashSet<(NodeIndex, NodeIndex)>,
    du_node_map: HashMap<DUNode, NodeIndex>,
    pub du_web: DiGraph<DUNode, u8>,
}

impl LVContext {
    pub fn new(cfg: &CFG) -> Self {
        let mut res = LVContext {
            gens: HashMap::new(),
            kills: HashMap::new(),
            du_node_map: HashMap::new(),
            du_web: DiGraph::new(),
            nexts: cfg.nexts.clone()
        };

        for blk in cfg.graph.node_indices() {
            let mut gen_set = HashSet::new();
            let mut assign_set = HashSet::new();
            let mut kill_set = HashSet::new();
            for instr in &cfg.graph.node_weight(blk).unwrap().instrs {
                // note that we must extend generation set first, for example:
                // a <- add a, b
                // here a is used before assigned
                let used_set = instr.symbols_used();

                gen_set.extend(used_set.difference(&assign_set).map(|x| {
                    let tmp = DUNode {
                        ty: DUType::Use,
                        sym: *x,
                        tid: instr.tid
                    };
                    let id = res.du_web.add_node(tmp.clone());
                    res.du_node_map.insert(tmp.clone(), id);
                    tmp
                }));

                used_set.intersection(&assign_set).map(|x| {

                });

                if let InstrType::Assign = instr.get_type() {
                    assign_set.insert(instr.dest.unwrap());
                    let tmp = DUNode {
                        ty: DUType::Def,
                        sym: instr.dest.unwrap(),
                        tid: instr.tid
                    };
                    let id = res.du_web.add_node(tmp.clone());
                    res.du_node_map.insert(tmp.clone(), id);
                    kill_set.insert(tmp);
                }
            }

            println!("{:?}: Gen{:?}: Kill{:?}", blk, gen_set, kill_set);
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

        let (res,): (HashSet<UDLink>,) = runtime.run();

        for link in res {
            self.du_web.add_edge(*self.du_node_map.get(&link.0).unwrap(), 
                *self.du_node_map.get(&link.1).unwrap(), 
                0);
        }

    }

    #[allow(dead_code)]
    pub fn pprint(&self, cfg: &CFG) {
        let dot_body = self.du_web.raw_edges().iter().map(|node| {
            let src = self.du_web.node_weight(node.source()).unwrap();
            let dest = self.du_web.node_weight(node.target()).unwrap();
            format!("  \"{}\\l\" -> \"{}\\l\"", 
                format!("{:?}: {}: {}", src.ty, src.tid, cfg.ir.get_str(src.sym).unwrap()),
                format!("{:?}: {}: {}", dest.ty, dest.tid, cfg.ir.get_str(dest.sym).unwrap()))
        }).collect::<Vec<String>>().join(";\n");

        println!("digraph G {{\n{}\n}}", dot_body);
    }
}
