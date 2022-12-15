const INPUT : [&str; 2] = ["addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop", include_str!("day10.txt") ];

type V2 = [i32; 2];
fn add<'a>(a: &'a mut V2, b: &V2) -> &'a mut V2 { a[0] += b[0]; a[1] += b[1]; a }
fn main() {
    let [cyc, rx] = [0usize, 1];
    let ops : Vec<V2> = INPUT[1]
        .lines()
        .map(|l| match &l[..4] {
            "noop" => [1, 0],
            "addx" => [2, l[5..].parse().expect("number")],
            _ => panic!("bad input: {l}"),
        })
        .collect();
    let mut s = [0, 1];
    let track = [20, 60, 100, 140, 180, 220, i32::MAX];
    let mut track = track.iter().peekable();
    let mut sigstr = 0;
    for op in ops {
        if s[cyc] + op[cyc] >= **track.peek().unwrap() {
            let curstr = *track.next().unwrap() * s[rx];
            //println!("{:?} {:?}: {curstr}", s, op);
            sigstr += curstr;
        }
        add(&mut s, &op);
        if **track.peek().unwrap() == i32::MAX {
            break;
        }
    }
    println!("part 1: {sigstr}");
}
