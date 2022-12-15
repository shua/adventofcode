const INPUT: &str = include_str!("day08.txt");
const EXAMPLE: &str = "30373
25512
65332
33549
35390";

fn printgrid<I: std::fmt::Debug, T: Iterator<Item=I>>(grid: T, width: usize) {
    for (i, v) in grid.enumerate() {
        print!("{v:?}");
        if (i+1) % width == 0 {
            println!();
        }
    }
}

fn printboundedgrid<I: std::fmt::Debug, T: Iterator<Item=I>>(grid: T, width: usize, (x0,y0): (usize, usize), [up,dn,lt,rt]: [u8; 4]) {
    let (xm,xM,ym,yM) = (x0-lt as usize,x0+rt as usize,y0-up as usize,y0+dn as usize);
    for (i, v) in grid.enumerate() {
        let (x, y) = (i % width, i / width);
        if (x,y) == (xm, ym) {
            print!(" ");
            for j in xm..xM {
                print!("{}", if j == x0 { '*' } else { ' ' });
            }
            println!();
        }
        
        if x >= xm && x <= xM && y >= ym && y <= yM {
            if x == xm {
                print!("{}", if y == y0 { '*' } else { ' ' });
            }
            print!("{v:?}");
            if x == xM {
                println!();
            }
        }
    }
}

fn main() {
    let input = INPUT;
    let width = input.lines().next().unwrap_or("").len();
    let map : Vec<_> = input.bytes().filter(|b| *b != b'\n').map(|b| b - b'0').collect();
    let height = map.len() / width;
    let mut vis = vec![true; map.len()];
    printgrid(map.iter(), width);

    let mut colmax = map[0..width].to_vec();
    for y in 1..(height-1) {
        let mut rowmax = map[y*width];
        for x in 1..(width-1) {
            let v = map[y*width+x];
            let mut vv = false;
            if colmax[x] < v {
                colmax[x] = v;
                vv = true;
            }
            if rowmax < v {
                rowmax = v;
                vv = true;
            }
            vis[y*width+x] = vv;
        }
    }
    let mut colmax = map[((height-1)*width)..((height-1)*width+(width-1))].to_vec();
    for ny in 1..(height-1) {
        let mut rowmax = map[(height-1-ny)*width+(width-1)];
        for nx in 1..(width-1) {
            let (x, y) = (width-1-nx, height-1-ny);
            let v = map[y*width+x];
            if colmax[x] < v {
                colmax[x] = v;
                vis[y*width+x] = true;
            }
            if rowmax < v {
                rowmax = v;
                vis[y*width+x] = true;
            }
        }
    }
    printgrid(vis.iter().map(|b| if *b {1} else {0}), width);
    println!("part 1: {}", vis.iter().filter(|b| **b).count());

    let mut scenic = vec![[0u8;4]; width*height];
    let [up,dn,lt,rt] = [0usize,1,2,3];
    macro_rules! scenic { ($x:expr, $y:expr) => { scenic[$y*width+$x] }; }
    macro_rules! map { ($x:expr, $y:expr) => { map[$y*width+$x] }; }
    for y in 1..(height-1) {
        for x in 1..(width-1) {
            let mut sup = 1u8;
            while y > sup as usize as usize && map![x,y-(sup as usize)] < map![x,y] {
                let sup1 = scenic![x,y-(sup as usize)][up];
                if sup1 == 0 {
                    break;
                }
                sup += sup1;
            }
            scenic![x,y][up] = sup;
            let mut slt = 1u8;
            while x > slt as usize && map![x-(slt as usize),y] < map![x,y] {
                let slt1 = scenic![x-(slt as usize),y][lt];
                if slt1 == 0 {
                    break;
                }
                slt += slt1;
            }
            scenic![x,y][lt] = slt;
        }
    }
    let mut max = 0u32;
    let mut max_coords = (0,0);
    for ny in 2..height {
        for nx in 2..width {
            let (x,y) = (width-nx, height-ny);
            let mut sdn = 1;
            while y+(sdn as usize) < height && map![x,y+(sdn as usize)] < map![x,y] {
                let sdn1 = scenic![x,y+(sdn as usize)][dn];
                if sdn1 == 0 {
                    break;
                }
                sdn += sdn1;
            }
            scenic![x,y][dn] = sdn;
            let mut srt = 1u8;
            while x+(srt as usize) < width && map![x+(srt as usize),y] < map![x,y] {
                let srt1 = scenic![x+(srt as usize),y][rt];
                if srt1 == 0 {
                    break;
                }
                srt += srt1;
            }
            scenic![x,y][rt] = srt;
            
            let sv = scenic![x,y];
            let sv = sv.iter().fold(1u32, |acc, b| acc * (*b) as u32);
            if sv > max {
                println!("new max {x},{y} = {sv} ({:?})", scenic![x,y]);
                max = sv;
                max_coords = (x,y);
            }
        }
    }
    println!("part 2: {max}");
    let ms = scenic![max_coords.0, max_coords.1];
    printboundedgrid(map.iter(), width, max_coords, [ms[up]+2,ms[dn]+1,ms[lt]+2,ms[rt]+2]);
}
