const INPUT: [&str; 2] = ["root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32", include_str!("day21.txt")];

use std::collections::HashMap;
#[derive(Clone,Copy,Debug)]
enum Expr<'s> { N(i64), O(char, &'s str, &'s str) }

fn main() {
    let mut ms = HashMap::new();
    for m in INPUT[1].lines() {
        let mname = &m[0..4];
        let expr = &m[6..];
        let expr = if expr.len() == 11 {
            // expr
            Expr::O(expr.chars().nth(5).unwrap(), &expr[..4], &expr[7..])
        } else {
            Expr::N(expr.parse().expect("number"))
        };
        ms.insert(mname, expr);
    }
    println!("{ms:?}");
    fn resolve<'s>(ms: &mut HashMap<&'s str, Expr<'s>>, root: &'s str) -> Option<i64> {
        match *ms.get(root)? {
            Expr::N(n) => Some(n),
            Expr::O(op, a, b) => {
                let a = resolve(ms, a);
                let b = resolve(ms, b);
                let (a, b) = (a?, b?);
                let n = match op {
                    '*' => a * b,
                    '/' => a / b,
                    '+' => a + b,
                    '-' => a - b,
                    _ => panic!("bad input: {op:?} not a valid operator"),
                };
                ms.insert(root, Expr::N(n));
                Some(n)
            }
        }
    }
    println!("part 1: {}", resolve(&mut ms.clone(), "root").unwrap());
    
    ms.remove("humn");
    fn solveeq<'s>(ms: &mut HashMap<&'s str, Expr<'s>>, name: &'s str, val: i64) {
        println!("solve for: {name} = {val}");
        match ms.get(name) {
            Some(&Expr::N(n)) => {
                assert!(n == val, "unable to unify: {n} == {val}");
            }
            Some(&Expr::O(op, a, b)) => {
                let av = resolve(ms, a);
                let bv = resolve(ms, b);
                /*
                println!("solve for: {} {op} {} = {val}",
                    av.map(|n| format!("{n}")).unwrap_or(String::from(a)),
                    bv.map(|n| format!("{n}")).unwrap_or(String::from(b)));
                */
                match (op, av, bv) {
                    ('*', Some(a), Some(b)) => {
                        ms.insert(name, Expr::N(a * b));
                        solveeq(ms, name, val);
                    }
                    ('/', Some(a), Some(b)) => {
                        ms.insert(name, Expr::N(a / b));
                        solveeq(ms, name, val);
                    }
                    ('+', Some(a), Some(b)) => {
                        ms.insert(name, Expr::N(a + b));
                        solveeq(ms, name, val);
                    }
                    ('-', Some(a), Some(b)) => {
                        ms.insert(name, Expr::N(a - b));
                        solveeq(ms, name, val);
                    }
                    ('*', Some(a), None) => solveeq(ms, b, val / a),
                    ('*', None, Some(b)) => solveeq(ms, a, val / b),
                    ('/', Some(a), None) => solveeq(ms, b, a / val),
                    ('/', None, Some(b)) => solveeq(ms, a, val * b),
                    ('+', Some(a), None) => solveeq(ms, b, val - a),
                    ('+', None, Some(b)) => solveeq(ms, a, val - b),
                    ('-', Some(a), None) => solveeq(ms, b, a - val),
                    ('-', None, Some(b)) => solveeq(ms, a, val + b),
                    (op, _, _) => panic!("don't know how to solve: {a} {op} {b} = {val}"),
                }
            }
            None => {
                ms.insert(name, Expr::N(val));
            }
        }
    }
    
    match ms.remove("root").unwrap() {
        Expr::O(_, a, b) => {
            let av = resolve(&mut ms, a);
            let bv = resolve(&mut ms, b);
            match (av, bv) {
                (Some(n), None) => solveeq(&mut ms, b, n),
                (None, Some(n)) => solveeq(&mut ms, a, n),
                _ => panic!("can't solve: {av:?} = {bv:?}"),
            }
        }
        e => panic!("root is wrong: {e:?}")
    }
    println!("{ms:?}");
    println!("part 2: {}", resolve(&mut ms, "humn").unwrap());
}
