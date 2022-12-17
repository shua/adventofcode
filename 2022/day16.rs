const INPUT: [&str;2] = ["Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II", include_str!("day16.txt")];

use std::collections::{HashMap, HashSet};
use std::cmp::{max, min};
#[derive(Clone,Copy,PartialEq,Eq,PartialOrd,Ord,Hash)]
struct ID(u8, u8);
impl From<&'_ str> for ID {
    fn from(s: &str) -> ID { ID(s.as_bytes()[0], s.as_bytes()[1]) }
}
impl std::fmt::Debug for ID { fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}{}", self.0 as char, self.1 as char)
}}
fn edge(a: ID, b: ID) -> [ID;2] { if a <= b { [a,b] } else { [b,a] } }
fn main() {
    let mut nodes : HashMap<ID, u32> = HashMap::new();
    let mut edges : HashSet<[ID;2]> = HashSet::new();
    for l in INPUT[0].lines() {
        let part = l.find(';').expect("bad input, no ';': {l}");
        let id : ID = l["Valve ".len()..].into();
        let rate : u32 = l["Valve XX has flow rate=".len()..part].parse().expect("number");
        nodes.insert(id, rate);
        let part = if &l[part..][.."; tunnels".len()] == "; tunnels" {
            part + "; tunnels lead to valves ".len()
        } else {
            part + "; tunnel leads to valve ".len()
        };
        for eid0 in l[part..].split(", ") {
            assert!(eid0.len() == 2, "something's off {eid0}: {l}");
            let eid : ID = eid0.into();
            //assert!(eid[0] == eid[1], "something's off {eid0}: {l}");
            edges.insert(edge(id, eid));
        }
    }
    println!("nodes {nodes:?}");
    println!("edges {edges:?}");
    
    let start = ID(b'A', b'A');
    let (vnodes, unodes) : (HashMap<_, _>, HashMap<_,_>) = nodes.iter().partition(|(id, r)| **r > 0 || **id == start);
    println!("vnodes {vnodes:?}");
    fn neighbor(n: ID) -> impl Fn((&'_ [ID;2], &'_ u32)) -> Option<(ID, u32)> {
        move |(&[a,b], &d)| if a == n { Some((b, d)) } else if b == n { Some((a, d)) } else { None }
    }
    // contract 0-valued nodes into paths between valuable nodes
    let mut vedges : HashMap<[ID;2], u32> = edges.iter().map(|e| (*e, 1)).collect();
    for &n in unodes.keys() {
        let others : Vec<_> = vedges.iter()
            .filter_map(neighbor(n))
            .collect();
        if others.len() == 0 {
            continue;
        }
        for i in 0..others.len()-1 {
            for j in i..others.len() {
                let (i, j) = (others[i].0, others[j].0);
                let (di, dj) = (vedges[&edge(n, i)], vedges[&edge(n, j)]);
                if i == j {
                    continue;
                }
                let dij = vedges.entry(edge(i, j)).or_insert(di+dj);
                if *dij > di+dj {
                    *dij = di+dj;
                }
            }
        }
        for (i, _) in others {
            vedges.remove(&edge(n, i));
        }
    }
    println!("vedges {vedges:?}");
    let (nodes, edges) = (vnodes, vedges);
    // want a K-n graph such that nodes = nodes, but edges = shortest path between nodes
    let mut kedges = edges.clone();
    let nids : Vec<_> = nodes.keys().collect();
    for i in 0..nids.len()-1 {
        // every node has 0 distance to self
        kedges.insert([*nids[i]; 2], 0);
        for j in i..nids.len() {
            let (i, j) = (*nids[i], *nids[j]);
            // find shortest route between i,j
            // tentative distance is kedges.get(i-j).unwrap_or(MAX)
            if let Some(0|1) = kedges.get(&edge(i, j)) {
                continue; // can't do better than that
            }

            // Mark all nodes unvisited. Create a set of all the unvisited nodes
            // called the unvisited set.
            // Assign to every node a tentative distance value: set it to zero 
            // for our initial node and to infinity for all other nodes.
            let mut unvisited : HashSet<_> = nodes.keys().filter(|k| **k != i).copied().collect();
            let mut a = i;
            while unvisited.len() > 0 {
                let mut next = (ID(0,0), u32::MAX);
                // For the current node, consider all of its unvisited neighbors
                // and calculate their tentative distances through the current node. 
                // Compare the newly calculated tentative distance to the one 
                // currently assigned to the neighbor and assign it the smaller one.
                for b in unvisited.iter() {
                    let dab = *edges.get(&edge(a, *b)).unwrap_or(&u32::MAX);
                    if dab == u32::MAX {
                        continue; // not a neighbor
                    }
                    let va = edges[&edge(i, a)];
                    let tvb = va + dab;
                    let mut vb = edges.entry(&edge(i,b)).or_insert(u32::MAX);
                    *vb = min(*vb, tvb);
                    
                    if dab < next.1 {
                        next = (*b, dab);
                    }
                }
                if next.1 == u32::MAX {
                    // pick a random unvisited
                }
            }
        }
    }

    // TODO: djikstra's with value of each node == rate * (30 - time - dist - 1)
}
