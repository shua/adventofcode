const INPUT : [&str; 2] = ["Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi", include_str!("day12.txt")];

use std::cmp::{min,max};
fn main() {
    let mut start = (0, 0);
    let mut end = (0, 0);
    let hmap : Vec<Vec<u8>> = INPUT[1].lines().enumerate().map(|(y, s)|
        s.bytes().enumerate().map(|(x, b)| match b {
            b'S' => { start = (x, y); 0 }
            b'E' => { end = (x, y); 25 }
            _ => b - b'a',
        })
        .collect::<Vec<_>>())
        .collect();
    let mut cost = vec![vec![u32::MAX; hmap[0].len()]; hmap.len()];
    cost[start.1][start.0] = 0;
    let mut visited = vec![vec![false; hmap[0].len()]; hmap.len()];
    visited[start.1][start.0] = true;
    let mut cur = start;
    let mut next = vec![];
    while !visited[end.1][end.0] {
        let curh = hmap[cur.1][cur.0];
        let nextcost = cost[cur.1][cur.0];
        assert!(nextcost < u32::MAX-1, "something's off {} {cur:?}", u32::MAX - nextcost);
        let nextcost = nextcost + 1;
        fn can_move(hcur: u8, hnext: u8) -> bool { hcur+1 >= hnext }
        for pos in [
            if cur.0 + 1 < visited[cur.1].len() { Some((cur.0+1, cur.1)) } else { None },
            if cur.0 > 0 { Some((cur.0-1, cur.1)) } else { None },
            if cur.1 + 1 < visited.len() { Some((cur.0, cur.1+1)) } else { None },
            if cur.1 > 0 { Some((cur.0, cur.1-1)) } else { None },
        ].into_iter().flatten() {
            if !visited[pos.1][pos.0] && can_move(curh, hmap[pos.1][pos.0]) {
                cost[pos.1][pos.0] = min(cost[pos.1][pos.0], nextcost);
                if !next.contains(&pos) {
                    next.push(pos);
                }
            }
        }
        visited[cur.1][cur.0] = true;
        if cur == end {
            continue;
        }
        
        let (n, nextpos) = next.iter().enumerate().fold((next.len(), end), |(m, mpos), (i, pos)| {
            if cost[pos.1][pos.0] <= cost[mpos.1][mpos.0] {
                (i, *pos)
            } else {
                (m, mpos)
            }
        });
        if nextcost % 100 == 0 || n >= next.len() {
            println!("cur {cur:?}");
            print!("     ");
            for j in 0..cost[0].len() {
                print!("{j:6} ");
            }
            println!();
            for i in 0..cost.len() {
                print!("{i:3}  ");
                for j in 0..cost[i].len() {
                    if cost[i][j] < u32::MAX {
                        print!("{:2} {:3}|", hmap[i][j], cost[i][j]);
                    } else {
                        print!("{:2}    |", hmap[i][j]);
                    }
                }
                println!();
            }
            println!();
        }
        assert!(n < next.len(), "nowhere to go: {cur:?} {}\n{cost:?}", cost[cur.1][cur.0]);

        next.swap_remove(n);
        cur = nextpos;
    }
    println!("part 1: {}", cost[end.1][end.0]);
}
