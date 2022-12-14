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

fn printmap(min: (usize, usize), (width, height): (usize, usize), map: &[Mat]) {
    println!("min {min:?}, width {width} height {height}");

    for y in min.1..height {
        let mut line = String::new();
        line.push('|');
        for x in min.0..width {
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
    match (map.get((x-1)+row), map.get(x + row), map.get((x+1) + row)) {
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
    let mut map = vec![Mat::Air; width * height];
    map[500 + width*0] = Mat::Sand; 
    for path in parsed {
        for pair in path.windows(2) {
            let (a, b) = (pair[0], pair[1]);
            fn rng(s: usize, e: usize) -> std::ops::RangeInclusive<usize> {
                if s > e {
                    e..=s
                } else {
                    s..=e
                }
            }
            for x in rng(a.0, b.0) {
                for y in rng(a.1, b.1) {
                    map[x + width * y] = Mat::Rock;
                }
            }
        }
    }

    printmap(min, (width, height), &map);

    let mut i = 0;
    while !step(&mut map, (width, height), (500, 0)) {
        i += 1;
        if i % 50 == 0 { println!("step: {i} "); printmap(min, (width, height), &map); }
    }
    println!("total units: {i}");
    printmap(min, (width, height), &map);
}
