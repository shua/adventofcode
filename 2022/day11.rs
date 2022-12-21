const INPUT: [&str; 2] = ["Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1", include_str!("day11.txt")];

fn main() {
    let input: Vec<_> = INPUT[1].lines().collect();
    let mut ms = vec![];
    for ls in input.chunks(7) {
        let starting: Vec<u32> = ls[1]["  Starting items: ".len()..].split(", ").map(|s| s.parse::<u32>().expect("number")).collect();
        let op: Vec<_> = ls[2]["  Operation: new = ".len()..].split(" ").collect();
        let op = (
            match op[1] {
                "+" => <u32 as std::ops::Add>::add,
                "*" => <u32 as std::ops::Mul>::mul,
                op => panic!("unrecognized op {op:?}: {}", ls[2]),
            },
            match op[0] {
                "old" => None,
                n => Some(n.parse::<u32>().expect("number")),
            },
            match op[2] {
                "old" => None,
                n => Some(n.parse::<u32>().expect("number")),
            },
        );
        let test: u32 = ls[3]["  Test: divisible by ".len()..].parse().expect("number");
        let testt: usize = ls[4]["    If true: throw to monkey ".len()..].parse().expect("number");
        let testf: usize = ls[5]["    If false: throw to monkey ".len()..].parse().expect("number");
        ms.push((starting, op, test, testt, testf));
    }
    println!("{ms:?}");
    let mut insp = vec![0; ms.len()];
    
    for i in 0..(20*ms.len()) {
        let mslen = ms.len();
        let mut starting = std::mem::replace(&mut ms[i%mslen].0, vec![]);
        let (_, op, test, test_then, test_else) = ms[i%ms.len()];
        for mut v in starting {
            v = op.0(op.1.unwrap_or(v), op.2.unwrap_or(v));
            v /= 3;
            insp[i%ms.len()] += 1;
            if v % test == 0 {
                ms[test_then].0.push(v);
            } else {
                ms[test_else].0.push(v);
            }
        }
    }
    println!("{ms:?}");
    println!("{insp:?}");
    let mut vs2 = insp.clone();
    vs2.sort();
    let v = vs2[vs2.len()-1] * vs2[vs2.len()-2];
    println!("part 1: {v}");
    
    // TODO: I think there's a cycle
}
