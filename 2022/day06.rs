const INPUT: &str = include_str!("day06.txt");
const EXAMPLES: [&str; 5] = [
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",
];

fn main() {
    let mut i = 0;
    for win in INPUT.as_bytes().windows(4) {
        let v = win.iter().map(|b| 1 << *b - b'a').fold(0u32, |acc, b| acc | b);
        if v.count_ones() == 4 {
            println!("part 1: {}", i + 4);
            break;
        }
        i += 1;
    }
    i = 0;
    for win in INPUT.as_bytes().windows(14) {
        let v = win.iter().map(|b| 1 << *b - b'a').fold(0u32, |acc, b| acc | b);
        if v.count_ones() == 14 {
            println!("part 2: {}", i + 14);
            break;
        }
        i += 1;
    }
}
