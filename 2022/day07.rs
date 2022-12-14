const INPUT: &str = include_str!("day07.txt");
const EXAMPLE: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

use std::collections::HashMap;
use std::path::PathBuf;

fn main() {
    let mut dirs = HashMap::new();
    let mut path = PathBuf::new();
    for line in INPUT.lines() {
        if &line[..4] == "$ cd" {
            if line == "$ cd .." {
                path.pop();
            } else {
                path.push(&line[5..]);
            }
        } else if matches!(line.bytes().next().unwrap(), b'0'..=b'9') {
            let mut path = path.clone();
            loop {
                let size : u32 = line.split(" ").next().unwrap().parse().expect("number");
                let cur = *dirs.get(&path).unwrap_or(&0);
                dirs.insert(path.clone(), cur + size);
                if !path.pop() {
                    break;
                }
            }
        }
    }
    println!("{dirs:?}");
    let mut sum = 0;
    for v in dirs.values() {
        if *v < 100000 {
            sum += v;
        }
    }
    println!("part 1: {sum}");
    
    let used = dirs[&PathBuf::from("/")];
    let free = 70000000 - used;
    let want = 30000000 - free;
    println!("used: {used} free: {free} want: {want}");
    let target = *dirs.values().filter(|v| **v >= want).min().unwrap_or(&0);
    println!("part 2: {target}");
}
