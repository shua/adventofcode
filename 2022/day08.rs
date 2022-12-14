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
}
