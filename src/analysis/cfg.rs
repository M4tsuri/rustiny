//! This module builds a control-flow graph from the original IR program
//! the original program will be spilited into Basic Blocks with each
//! conatins maxinum number of non-branch instructions and only has branch
//! instruction at its end.
//!
//! Note that each program has two special block: entry and end. entry block
//! represents the entry of the whole program and the end block represents the end.
//! A program can have only one entry block and multiple end blocks (currently our program
//! has only one end block).

use std::{cmp::Ordering, collections::{HashMap, HashSet}};

use petgraph::graph::{DiGraph, NodeIndex};
use super::reaching_definition::RDContext;
use super::live_variable::LVContext;
use crate::ir::{self, InstrTid, StrTid};

/// the type of a program point, this struct contains data we 
pub struct Edge {

}

pub struct BasicBlock {
    pub instrs: Vec<ir::Instr>
}

#[derive(Debug, Clone)]
pub struct DUNode {
    tid: InstrTid, 
    sym: StrTid,
}

/// A CFG is a directed graph with BasicBlocks as its Nodes and 
/// program points as the weigh of its Edges.
/// A CFG can be derived from a IR program.
/// Most of our static analysis and optimizations are based on CFG.
pub struct CFG<'a> {
    /// this field gives us the IR program, note that this field is read-only
    pub ir: ir::Program<'a>,
    pub graph: DiGraph<BasicBlock, Edge>,
    /// this field map each entry of block to its index in CFG
    entry2blk: HashMap<InstrTid, NodeIndex>,
    /// this information is pretty important for our analysises later,
    /// so we prepare it when building CFG
    pub nexts: HashSet<(NodeIndex, NodeIndex)>,
    ranges: Vec<SplitRange>,
    // du-web is a DAG
    pub du_web: DiGraph<DUNode, Edge>,
    pub du_heads: Vec<NodeIndex>
}

/// The struct representing a edge in a CFG.
/// The `to` field is the destination instruction id.
/// The `froms` are instruction ids which can be the successors 
/// of the `to` instruction when executing.
/// Later we will build a vector containing `SplitRange`s to represent
/// the range of each basic block with whose start instruction id and
/// end instruction id.
/// There are two types of `SplitEdge`, for example:
///
/// ```
/// 15  c <- MOV tmp#14
/// 16  tmp#15 <- CMP a, Int(0)
/// 17  JNZ 19
/// 18  CALL write, Int(1)
/// 19  CALL read, a
/// ```
///
/// - `SplitEdge` from `ins#17` to `ins#19`
/// - `SplitEdge` from `ins#17` to `ins#18`
/// - `SplitEdge` from `ins#18` to `ins#19`
///
/// We can see that a explicit edge can be inferred from the jump
/// target of `ins#17`. However, we need extra work to make our 
/// program build edge from `ins#17` to `ins#18`, as well as
/// the edge from `ins#18` to `ins#19`.
///
/// So each time when we meet a branch instruction, we need to build
/// - An edge from the branch instruction and its direct successor
/// - An edge from the direct precursor of the target of the branch to the target of the branch
#[derive(Debug, Clone)]
struct SplitEdge {
    to: InstrTid,
    froms: HashSet<InstrTid>
}

/// A range can be direved from a `SplitEdge`.
/// We only consider the `to` field in our algorithm for building `SplitRange`s
/// because `to` fields point to all blocks which may be executed in our program.
#[derive(Debug)]
struct SplitRange {
    entry: InstrTid,
    end: InstrTid,
    blockid: NodeIndex
}

impl<'a> CFG<'a> {
    /// construct a CFG object
    pub fn new(src: ir::Program<'a>) -> Self {
        CFG {
            ir: src,
            graph: DiGraph::new(),
            entry2blk: HashMap::new(),
            nexts: HashSet::new(),
            ranges: Vec::new(),
            du_heads: Vec::new(),
            du_web: DiGraph::new()
        }
    }

    pub fn get_node(&mut self, id: InstrTid) -> NodeIndex {
        // println!("{:?}, {}", ranges, instr);
        let range_idx = self.ranges.binary_search_by(|x| {
            match (id < x.entry, id > x.end) {
                (true, true) => panic!("what?"),
                (false, false) => Ordering::Equal,
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less
            }
        }).unwrap();

        self.ranges.get(range_idx).unwrap().blockid
    }

    /// build the CFG from the IR program
    pub fn build(&mut self) {
        let mut split_edge: HashMap<InstrTid, SplitEdge> = HashMap::new();
        let first_tid = self.ir.instrs.first().unwrap().tid;
        let last_tid = self.ir.instrs.last().unwrap().tid;

        let mut add_edge = |from: InstrTid, to: InstrTid| {
            if let Some(x) = split_edge.get_mut(&to) {
                x.froms.insert(from);
            } else {
                let mut froms = HashSet::new();
                froms.insert(from);
                split_edge.insert(to, SplitEdge {
                    to,
                    froms
                });
            }
        };

        for instr in &self.ir.instrs {
            if !instr.is_branch() {
                continue;
            }

            let from = instr.tid;
            let to = instr.get_branch_target();

            match instr.get_type() {
                ir::InstrType::CondJmp => add_edge(from, from + 1),
                _ => {}
            }

            add_edge(from, to);
            
            let to_prev = self.ir.instrs.get(to - 1).unwrap();
            match to_prev.get_type() {
                ir::InstrType::DirectJmp => {},
                _ => add_edge(to - 1, to)
            }
        }
        // edge for the entry block and end block
        add_edge(first_tid, first_tid + 1);
        add_edge(last_tid - 1, last_tid);

        let mut worklist: Vec<SplitEdge> = split_edge.values().cloned().collect();

        worklist.sort_by(|x, y| {
            x.to.cmp(&y.to)
        });
        
        let mut workiter = worklist.iter();
        let mut cur = workiter.next().unwrap();

        let mut graph_add_node = |start, end| {
            let id = self.graph.add_node(BasicBlock {
                instrs: self.ir.instrs[start..end].into()
            });
            self.entry2blk.insert(start, id);

            self.ranges.push(SplitRange {
                blockid: id,
                entry: start,
                end: end - 1
            });
        };

        // the entry block
        graph_add_node(first_tid, first_tid + 1);
        for next in workiter {
            graph_add_node(cur.to, next.to);
            cur = next;
        }
        // the end block
        graph_add_node(last_tid, last_tid + 1);

        for (_, point) in split_edge {
            let dest = self.get_node(point.to);
            for in_entry in point.froms {
                let src = self.get_node(in_entry);
                self.nexts.insert((src, dest));
                self.graph.add_edge(src, dest, Edge {});
            }
        }
    }

    
    /// Pretty print the CFG, we use dot graph here
    /// Just copy the output of it to https://dreampuf.github.io/GraphvizOnline/
    /// and then you can see the CFG
    #[allow(dead_code)]
    pub fn pprint(&self) {
        let dot_body = self.graph.raw_edges().iter().map(|node| {
            let src = self.graph.node_weight(node.source()).unwrap();
            let dest = self.graph.node_weight(node.target()).unwrap();
            format!("  \"{:?}\\l{}\\l\" -> \"{:?}\\l{}\\l\"", 
                node.source(),
                self.ir.instrs_pprint(&src.instrs).replace("\n", "\\l"),
                node.target(),
                self.ir.instrs_pprint(&dest.instrs).replace("\n", "\\l"))
        }).collect::<Vec<String>>().join(";\n");

        println!("digraph G {{\n{}\n}}", dot_body);
    }
}
