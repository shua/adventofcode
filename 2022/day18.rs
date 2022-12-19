const mut INPUT : [&str; 2] = ["2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5", include_str!("day18.txt")];

use std::cmp::{max,min};
fn main() {
    let mut space = [[0u32; 32]; 32];
    let mut nnodes = 0;
    for l in INPUT[1].lines() {
        let mut ns = l.split(",")
            .map(|n| n.parse::<usize>().expect("number"));
        let v = [ns.next().unwrap(), ns.next().unwrap(), ns.next().unwrap()];
        assert!(v[0] < 30 && v[1] < 30 && v[2] < 30);
        nnodes += 1;
        space[v[0]+1][v[1]+1] |= 1 << (v[2]+1);
    }

    /*
    for yz in space {
        for z in yz {
            println!("{z:032b}");
        }
        println!();
    }
    */
    
    let mut touching = 0;
    for x in 1..31 {
        for y in 1..31 {
            let zvec = space[x][y];
            touching += (zvec << 1 & zvec).count_ones();
            touching += (zvec >> 1 & zvec).count_ones();
            touching += (zvec & space[x][y-1]).count_ones();
            touching += (zvec & space[x][y+1]).count_ones();
            touching += (zvec & space[x+1][y]).count_ones();
            touching += (zvec & space[x-1][y]).count_ones();
        }
    }
    println!("part 1: {nnodes} * 6 - {touching} = {}", nnodes * 6 - touching);

    // surround the space in steam
    let mut ext = [[0u32; 32]; 32];
    for x in 0..32 {
        for y in 0..32 {
            if let (0|31, 0|31) = (x, y) {
                // exterior face of space
                ext[x][y] = !0;
            } else {
                // interior of space
                ext[x][y] = 0x8000_0001;
            }
        }
    }
    // simulate flow, any space with no rock and lava neighbor, becomes lava
    for nx in 1..31 {
        for ny in 1..31 {
            let (x, y) = (31-nx, 31-ny);
            let air = !space[x][y];
            let mut steam = (ext[x][y] | ext[x+1][y] | ext[x][y+1]) & air;
            for _ in 0..16 {
                steam = (steam | steam << 1 | steam >> 1) & air;
            }
            ext[x][y] = steam;
        }
    }

    touching = 0;
    
    for x in 1..31 {
        println!("{:032b} {:032b}", 0u32, !0u32);
        for y in 1..31 {
            let lava = space[x][y];
            let air = !lava;
            let mut steam = (ext[x][y] | ext[x-1][y] | ext[x+1][y] | ext[x][y+1] | ext[x][y-1]) & air;
            for _ in 0..16 {
                steam = (steam | steam << 1 | steam >> 1) & air;
            }
            ext[x][y] = steam;
            
            println!("{lava:032b} {steam:032b}");
            
            touching += ((steam >> 1) & lava).count_ones();
            touching += ((steam << 1) & lava).count_ones();
            touching += (steam & space[x][y-1]).count_ones();
            touching += (ext[x][y-1] & lava).count_ones();
            touching += (steam & space[x-1][y]).count_ones();
            touching += (ext[x-1][y] & lava).count_ones();
        }
        println!("{:032b} {:032b}", 0u32, !0u32);
        println!();
    }
    println!("part 2: {touching}");
}
