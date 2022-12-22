const INPUT: [&str; 2] = ["        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5", include_str!("day22.txt")];
fn main() {
    let mut width = 0;
    let input = INPUT[1];
    for l in input.lines() {
        if l.len() >= 1 && matches!(&l[0..1], "." | " " | "#") {
            width = std::cmp::max(width, l.len());
        }
    }
    width += 2;
    let blank_line = vec![b' '; width];
    let mut board = vec![blank_line.clone()];
    let mut bounds = vec![0..0];
    let mut cmds = vec![];
    for l in input.lines() {
        if l.len() >= 1 && matches!(&l[0..1], "." | "#" | " ") {
            let mut s = Vec::with_capacity(width);
            s.push(b' ');
            s.extend(l.bytes());
            let r = s.len();
            let mut l = 0;
            while s[l] == b' ' { l += 1; }
            bounds.push(l..r);
            while s.len() < width { s.push(b' '); }
            board.push(s);
        } else if l != "" {
            let mut l = l;
            let mut i = 0;
            while i < l.len() {
                while i < l.len() && matches!(l.as_bytes()[i], b'0'..=b'9') {
                    i += 1;
                }
                let mv: usize = l[..i].parse().expect(&format!("number: {}", &l[..i]));
                if let Some(&t) = l.as_bytes().get(i) {
                    cmds.push((mv, if t == b'L' { 3 } else { 1 }));
                    i += 1;
                    l = &l[i..];
                    i = 0;
                } else {
                    cmds.push((mv, 0));
                }
            }
        }
    }
    board.push(blank_line);
    bounds.push(0..0);
    println!("width {width}, cmds {cmds:?}");
    for i in 0..board.len() { println!("{:2} {:2} {}", bounds[i].start, bounds[i].end, std::str::from_utf8(&board[i]).unwrap()); }
    
    let mut pos = (0usize, 1usize);
    while board[pos.1][pos.0] == b' ' { pos.0 += 1; }
    let mut dir = 0;
    for (mut mv, t) in cmds {
        println!("{mv} {t}: {pos:?} {dir}");
        match dir {
            3 => while mv > 0 {
                // north
                let mut next = (pos.0, pos.1 - 1);
                if board[next.1][next.0] == b' ' {
                    next = pos;
                    while bounds[next.1+1].contains(&next.0) {
                        next.1 += 1;
                    }
                }
                if board[next.1][next.0] == b'#' {
                    break;
                }
                mv -= 1;
                pos = next;
            },
            1 => while mv > 0 {
                // south
                let mut next = (pos.0, pos.1 + 1);
                if board[next.1][next.0] == b' ' {
                    next = pos;
                    while bounds[next.1-1].contains(&next.0) {
                        next.1 -= 1;
                    }
                }
                if board[next.1][next.0] == b'#' {
                    break;
                }
                mv -= 1;
                pos = next;
            },
            0 => while mv > 0 {
                // east
                let mut next = (pos.0 + 1, pos.1);
                if board[next.1][next.0] == b' ' {
                    next.0 = bounds[next.1].start;
                }
                if board[next.1][next.0] == b'#' {
                    break;
                }
                mv -= 1;
                pos = next;
            },
            2 => while mv > 0 {
                // west
                let mut next = (pos.0 - 1, pos.1);
                if board[next.1][next.0] == b' ' {
                    next.0 = bounds[next.1].end-1;
                }
                if board[next.1][next.0] == b'#' {
                    break;
                }
                mv -= 1;
                pos = next;
            },
            _ => unreachable!("directions don't go that high"),
        }
        dir = (dir + t) % 4;
    }
    
    println!("part 1: {}", 1000 * pos.1 + 4 * pos.0 + dir);
}
