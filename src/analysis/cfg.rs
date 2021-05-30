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

use crate::ir::{self, InstrTid};

/// the type of a program point, this struct contains data we 
pub struct ProgramPoint {

}

pub struct BasicBlock {
    instrs: Vec<ir::Instr>
}

/// A CFG is a directed graph with BasicBlocks as its Nodes and 
/// program points as the weigh of its Edges.
/// A CFG can be derived from a IR program.
/// Most of our static analysis and optimizations are based on CFG.
pub struct CFG {
    src_ir: ir::Program,
    graph: DiGraph<BasicBlock, ProgramPoint>,
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

impl CFG {
    /// construct a CFG object
    pub fn new(src: ir::Program) -> Self {
        CFG {
            src_ir: src,
            graph: DiGraph::new()
        }
    }

    /// build the CFG from the IR program
    pub fn build(&mut self) {
        let mut split_edge: HashMap<InstrTid, SplitEdge> = HashMap::new();
        let first_tid = self.src_ir.instrs.first().unwrap().tid;
        let last_tid = self.src_ir.instrs.last().unwrap().tid;

        let mut add_edge = |from: InstrTid, to: InstrTid| {
            println!("{} -> {}", from, to);
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

        for instr in &self.src_ir.instrs {
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
            
            let to_prev = self.src_ir.instrs.get(to - 1).unwrap();
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

        let mut ranges: Vec<SplitRange> = Vec::new();
        let mut workiter = worklist.iter();
        let mut cur = workiter.next().unwrap();

        let mut graph_add_node = |start, end| {
            let id = self.graph.add_node(BasicBlock {
                instrs: self.src_ir.instrs[start..end].into()
            });

            ranges.push(SplitRange {
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

        let get_node_in_range = |instr: InstrTid| -> NodeIndex {
            // println!("{:?}, {}", ranges, instr);
            let range_idx = ranges.binary_search_by(|x| {
                match (instr < x.entry, instr > x.end) {
                    (true, true) => panic!("what?"),
                    (false, false) => Ordering::Equal,
                    (true, false) => Ordering::Greater,
                    (false, true) => Ordering::Less
                }
            }).unwrap();

            ranges.get(range_idx).unwrap().blockid
        };

        for (_, point) in split_edge {
            let dest = get_node_in_range(point.to);
            for in_entry in point.froms {
                let src = get_node_in_range(in_entry);
                self.graph.add_edge(src, dest, ProgramPoint {});
            }
        }
    }

    /// Pretty print the CFG, we use dot graph here
    /// Just copy the output of it to https://dreampuf.github.io/GraphvizOnline/
    /// and then you can see the CFG
    pub fn pprint(&self) {
        let dot_body = self.graph.raw_edges().iter().map(|node| {
            let src = self.graph.node_weight(node.source()).unwrap();
            let dest = self.graph.node_weight(node.target()).unwrap();
            format!("  \"{}\\l\" -> \"{}\\l\"", 
                self.src_ir.instrs_pprint(&src.instrs).replace("\n", "\\l"),
                self.src_ir.instrs_pprint(&dest.instrs).replace("\n", "\\l"))
        }).collect::<Vec<String>>().join(";\n");

        println!("digraph G {{\n{}\n}}", dot_body);
    }
}
