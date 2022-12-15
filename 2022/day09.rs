const INPUT: &str = include_str!("day09.txt");
const EXAMPLE: [&str; 2] = ["R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2", "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"];

use std::collections::HashSet;
use std::cmp::{min,max};

type V2 = [i32;2];
fn sub(a: &V2, b: &V2) -> V2 { [a[0] - b[0], a[1] - b[1]] }
fn add(a: &V2, b: &V2) -> V2 { [a[0] + b[0], a[1] + b[1]] }
fn neg(a: &V2) -> V2 { [-a[0], -a[1]] }
fn del(a: &V2) -> i32 { max(a[0].abs(), a[1].abs()) }
fn pts(a: &V2, b: &V2) -> impl Iterator<Item=V2> {
    assert!(a[0] == b[0] || a[1] == b[1], "pts must be colinear: {a:?} {b:?}");
    let d = sub(b, a);
    let (a, b) = (*a, *b);
    let bds = (min(0,d[0]), min(0,d[1]), max(0,d[0]), max(0,d[1]));
    let mut it = (bds.0..=bds.2).flat_map(move |x| (bds.1..=bds.3).map(move |y| [
            if bds.0 < 0 { b[0] - x } else { a[0] + x }, 
            if bds.1 < 0 { b[1] - y } else { a[1] + y },
        ]));
    it.next();
    it
}

fn plot<'i, T: IntoIterator<Item=&'i V2>>(pts: T) {
    let mut bounds = (i32::MAX, i32::MAX, i32::MIN, i32::MIN);
    let pts : Vec<V2> = pts.into_iter().map(|p| {
        bounds = (min(bounds.0, p[0]), min(bounds.1, p[1]), max(bounds.2, p[0]), max(bounds.3, p[1]));
        *p
    }).collect();
    println!("bounds: {bounds:?}");
    for ny in (-bounds.3)..=(-bounds.1) {
        let y = -ny;
        for x in bounds.0..=bounds.2 {
            print!("{}", if pts.contains(&[x,y]) { '#' } else { '.' });
        }
        println!();
    }
}

fn main() {
    let ops : Vec<V2> = INPUT.lines().map(|l| {
        match &l[0..1] {
            "U" => [0, l[2..].parse::<i32>().expect("number")],
            "D" => [0, -l[2..].parse::<i32>().expect("number")],
            "L" => [-l[2..].parse::<i32>().expect("number"), 0],
            "R" => [l[2..].parse::<i32>().expect("number"), 0],
            _ => panic!("bad input: {l}")
        }
    }).collect();

    let mut h = [0,0];
    let mut t = [[0,0]; 9];
    let mut visited1 = HashSet::new();
    let mut visited9 = HashSet::new();
    visited1.insert(t[0]);
    visited9.insert(t[8]);
    for op in ops.iter() {
        assert!(op[0] == 0 || op[1] == 0);
        let h0 = h;
        h = add(&h, op);
        //println!("{h0:?}->{h:?}: ");
        for pt in pts(&h0, &h) {
            //print!("    {pt:?}");
            let mut t0 = pt;
            for (i, t) in t.iter_mut().enumerate() {
                *t = match sub(&t0, t) {
                    [-1..=1, -1..=1] => break,
                    [x @ -2..=2, y @ -2..=2] => add(t, &[x.signum(), y.signum()]),
                    _ => panic!("unexpected state: T{} {t0:?} T{} {t:?}", i, i+1),
                };

                //print!("{t:?}");
                t0 = *t;
                if i == 0 && visited1.insert(*t) {
                    //print!("+");
                }
                if i == 8 && visited9.insert(*t) {
                    //print!("+");
                }
            }
            //println!(", ");
        }
        //println!();
    }
    //println!("{visited:?}");
    //plot(&visited);
    println!("part 1: {}", visited1.len());
    println!("part 2: {}", visited9.len());
}
