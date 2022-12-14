const INPUT: &str = include_str!("day14.txt");

const EXAMPLE: &str = "
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
";

#[derive(Clone, Copy, PartialEq, Eq)]
enum Mat {
    Rock,
    Air,
    Sand,
}

fn printmap((minx, miny): (usize, usize), (width, height): (usize, usize), map: &[Mat]) {
    println!("minx {minx} miny {miny} width {width} height {height}");

    for y in miny..height {
        let mut line = String::new();
        line.push('|');
        for x in minx..width {
            match map[x + width * y] {
                Mat::Rock => line.push('#'),
                Mat::Air => line.push(' '),
                Mat::Sand => line.push('O'),
            }
        }
        println!("{line}|");
    }
}

fn step(map: &mut [Mat], (width, height): (usize, usize), (x, y): (usize, usize)) -> bool {
    let row = width*(y+1);
    let left = if x == 0 { None } else { map.get((x-1)+row) };
    let right = if x >= width-1 { None } else { map.get((x+1)+row) };
    match (left, map.get(x + row), right) {
        (_, None, _) => true,
        (_, Some(Mat::Air), _) => step(map, (width, height), (x, y+1)),
        (None, Some(_), _) => true,
        (Some(Mat::Air), Some(_), _) => step(map, (width, height), (x-1, y+1)),
        (Some(_), Some(_), None) => true,
        (Some(_), Some(_), Some(Mat::Air)) => step(map, (width, height), (x+1, y+1)),
        (Some(_), Some(_), Some(_)) => {
            map[x+width*y] = Mat::Sand;
            false
        }
    }
}

fn step2(map: &mut [Mat], (width, height): (usize, usize), (x, y): (usize, usize)) -> usize {
    let row = width*(y+1);
    let left = if x == 0 { None } else { map.get((x-1)+row) };
    let right = if x >= width - 1 { None } else { map.get((x+1)+row) };
    match (left, map.get(x + row), right) {
        (_, None, _) => {
            map[x+width*y] = Mat::Sand;
            1
        },
        (_, Some(Mat::Air), _) => step2(map, (width, height), (x, y+1)),
        (None, Some(_), _) => {
            map[x+width*y] = Mat::Sand;
            height-y
        },
        (Some(Mat::Air), Some(_), _) => step2(map, (width, height), (x-1, y+1)),
        (Some(_), Some(_), None) => {
            map[x+width*y] = Mat::Sand;
            height-y
        },
        (Some(_), Some(_), Some(Mat::Air)) => step2(map, (width, height), (x+1, y+1)),
        (Some(_), Some(_), Some(_)) => {
            map[x+width*y] = Mat::Sand;
            1
        }
    }
}

fn main() {
    let (mut width, mut height) = (0, 0);
    let mut min = (usize::MAX, 0);
    let mut parsed = vec![];
    for line in INPUT.lines() {
        if line == "" {
            continue;
        }
        let mut path = vec![];
        for pair in line.split(" -> ") {
            let i = pair.find(",").expect(&format!("x,y: {pair}"));
            let (x, y) = (
                (&pair[..i]).parse::<usize>().expect("number"),
                (&pair[(i + 1)..]).parse::<usize>().expect("number"),
            );
            path.push((x, y));
            min = (std::cmp::min(min.0, x), std::cmp::min(min.1, y));
            width = std::cmp::max(width, x + 1);
            height = std::cmp::max(height, y + 1);
        }
        if path.len() > 0 {
            parsed.push(path);
        }
    }
    min = (min.0 - height, 0);
    width = (width+1) - min.0;
    let mut map = vec![Mat::Air; width * height];
    map[(500-min.0) + width*0] = Mat::Sand; 
    for path in parsed {
        for pair in path.windows(2) {
            let (a, b) = (pair[0], pair[1]);
            fn rng(base: usize, s: usize, e: usize) -> std::ops::RangeInclusive<usize> {
                if s > e {
                    (e-base)..=(s-base)
                } else {
                    (s-base)..=(e-base)
                }
            }
            for x in rng(min.0, a.0, b.0) {
                for y in rng(min.1, a.1, b.1) {
                    map[x + width * y] = Mat::Rock;
                }
            }
        }
    }

    let mut i = 0;
    while !step(&mut map, (width, height), (500-min.0, 0)) {
        i += 1;
    }
    println!("total units: {i}");
    printmap((height, min.1), (width, height), &map);
    
    map[(500-min.0)+width*0] = Mat::Air;
    map.extend(vec![Mat::Air; width]);
    height += 1;
    
    let mut last = i;
    while map[(500-min.0)+width*0] != Mat::Sand {
        let s = step2(&mut map, (width, height), (500-min.0, 0));
        i += s;
    }
    println!("total units: {i}");
    printmap((height, min.1), (width, height), &map);
    
    let vis = map.iter().filter(|m| **m == Mat::Sand).count();
    let invis = map.iter().enumerate().filter(|(i, m)| (((i+1) % width) == 0) && **m == Mat::Sand).count() - 1;
    let invis = (invis * (invis + 1)) / 2;
    assert!(i == vis + invis);
}
