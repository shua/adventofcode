const INPUT : [&str;2] = [
    ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>",
    include_str!("day17.rs"),
];

const ROCKS : [[u8;4]; 5] = [
    [ 0b00_1111_00
    , 0, 0, 0],
    [ 0b000_1_0000
    , 0b00_111_000
    , 0b000_1_0000
    , 0 ],
    [ 0b00_111_000
    , 0b0000_1_000
    , 0b0000_1_000
    , 0 ],
    [ 0b00_1_00000
    , 0b00_1_00000
    , 0b00_1_00000
    , 0b00_1_00000 ],
    [ 0b00_11_0000
    , 0b00_11_0000
    , 0, 0 ],
];

fn plot(tunnel: &[u8]) {
    for slice in tunnel.iter().rev() {
        let mut mask = 1<<7;
        print!("|");
        while mask > 1 {
            if slice & mask != 0 {
                print!("#");
            } else {
                print!(".");
            }
            mask >>= 1;
        }
        println!("|");
    }
    println!("+-------+");
}
fn main() {
    let mut tunnel = vec![];
    let mut jet = std::iter::repeat(INPUT[1]).flat_map(|s| s.bytes());
    // leftmost 7 bits are in play
    // fall is movement or new rock @ top+3, left+2
    // jet is << or >> UNLESS \E bits. bits.ones() != (bits' & ~0b1).ones()
    for n in 0..2022 {
        tunnel.extend([1u8;7]);
        let mut bot = tunnel.len()-4;
        let mut rock = ROCKS[n%5].to_vec();
        loop {
            let tslice = &tunnel[bot..bot+4];
            // jet
            let jet = jet.next().expect("more jet");
            let isect = if jet == b'>' {
                rock.iter().zip(tslice.iter()).any(|(r, t)| (r >> 1) & t != 0)
            } else {
                rock.iter().zip(tslice.iter()).any(|(r, t)| (r & 0b1000_0000) == 0b1000_0000 || (r << 1) & t != 0)
            };
            /*
            println!("jet {jet} {shift}");
            rock.iter().zip(tslice.iter()).for_each(|(&r, &t)| {
                let pb = |i, n| print!("{}", if (1u8 << 8-i) & n == 0u8 { '.' } else { '#' });
                for i in 1..8 { pb(i, r) }
                print!(" ");
                for i in 1..8 { pb(i, t) }
                print!(" ");
                for i in 1..8 { pb(i, r | t) }
                println!();
            });
            */
            if !isect {
                for b in rock.iter_mut() {
                    if jet == b'>' {
                        *b >>= 1;
                    } else {
                        *b <<= 1;
                    }
                }
            }
            // fall
            if bot == 0 {
                break;
            }
            let isect = rock.iter().zip(tunnel[bot-1..bot+3].iter()).any(|(r, t)| r & t != 0);
            if isect {
                break;
            }
            bot -= 1;
        }
        for i in 0..4 {
            tunnel[bot+i] |= rock[i];
        }
        
        let mut top = tunnel.len()-1;
        while top > 0 && tunnel[top] == 1 {
            top -= 1;
        }
        tunnel.truncate(top+1);
        //println!("tunnel {n}");
        //plot(&tunnel);
    }
    
    println!("part 1: {}", tunnel.len());
}
