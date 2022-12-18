const INPUT: [&str; 2] = ["[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]", include_str!("day13.txt")];

#[derive(Debug,PartialEq)]
enum Packet { L(Vec<Packet>), I(u32) }
impl PartialOrd for Packet {
    fn partial_cmp(&self, rhs: &Packet) -> Option<std::cmp::Ordering> {
        match (self, rhs) {
            (Packet::I(i1), Packet::I(i2)) => i1.partial_cmp(i2),
            (Packet::L(l1), Packet::L(l2)) => l1.partial_cmp(l2),
            (Packet::I(i), Packet::L(l)) => [Packet::I(*i)].as_slice().partial_cmp(l.as_slice()),
            (Packet::L(l), Packet::I(i)) => l.as_slice().partial_cmp([Packet::I(*i)].as_slice()),
        }
    }
}

fn parse_num(bs: &[u8], i: &mut usize) -> Packet {
    let start = *i;
    while let Some(b'0'..=b'9') = bs.get(*i) {
        *i += 1;
    }
    Packet::I(std::str::from_utf8(&bs[start..*i]).unwrap().parse().expect("number"))
}
fn parse_list(bs: &[u8], i: &mut usize) -> Packet {
    let mut p = vec![];
    loop {
        match bs.get(*i) {
            Some(b'[') => {
                *i += 1;
                p.push(parse_list(bs, i));
            }
            Some(b']') => {
                *i += 1;
                return Packet::L(p);
            }
            Some(b'0'..=b'9') => p.push(parse_num(bs, i)),
            Some(b',') => { *i += 1; }
            Some(c) => panic!("unexpected {:?}: {p:?} {} ( ... {} )", *c as char, *i, std::str::from_utf8(&bs[*i-4..*i]).unwrap()),
            None => panic!("unexpected EOF {p:?} {}", *i),
        }
    }
}
fn main() {
    let input = INPUT[1].as_bytes();
    let mut i = 1;
    let mut p = vec![];
    // just making sure
    assert!([1].as_slice() < [1,0].as_slice());
    while i < input.len() {
        let p0 = parse_list(input, &mut i);
        assert!(&input[i..i+2] == b"\n[");
        i += 2;
        let p1 = parse_list(input, &mut i);
        p.push((p0, p1));
        if i < input.len() {
            assert!(&input[i..i+3] == b"\n\n[");
            i += 3;
        }
    }
    println!("{p:?}");
    println!("part 1: {}", p.iter().enumerate().filter(|(i, (a,b))| a < b).fold(0, |acc, (i, _)| acc + i + 1));
}
