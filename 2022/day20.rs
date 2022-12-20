const INPUT : [&str;2] = ["1
2
-3
3
-2
0
4", ""];

use std::cmp::{min,max};
fn permute<'v, T>(ind: &[usize], v: &'v [T]) -> Vec<&'v T> {
    let mut p : Vec<_> = ind.into_iter().enumerate().map(|(i, p)| (p, i)).collect();
    p.sort();
    p.into_iter().map(|(_, i)| v.get(i).unwrap()).collect()
}
// can I keep two lists with indexes into each other?
fn main() {
    let next: Vec<_> = INPUT[1].lines().map(|s| s.parse::<i32>().expect("number")).collect();
    let n = next.len();
    let mut pos: Vec<_> = (0..n).collect();
    for i in 0..n {
        let mut newpos = (pos[i] as i32 + next[i] + n as i32) as usize % n;
        if next[i] < 0 { newpos = (newpos + n - 1) % n; }
        if next[i] > 0 && (pos[i] + next[i] as usize) > n { newpos += 1; }
        // rm from prev -> \A j > i, pos[j] -= 1
        // insert new -> \A j >= new, pos[j] += 1
        //print!("{i} {} {newpos} {:?} -> ", pos[i], permute(&pos, &next));
        for j in 0..n {
            if pos[j] > pos[i] {
                pos[j] -= 1;
            }
        }
        for j in 0..n {
            if pos[j] >= newpos {
                pos[j] += 1;
            }
            pos[j] = pos[j] % n;
        }
        pos[i] = newpos;
        //println!("{:?}", permute(&pos, &next));
    }
    println!("done");
    let res: Vec<_> = permute(&pos, &next);
    let (start, _) = res.iter().enumerate().find(|(_, v)| ***v == 0).unwrap();
    let [a, b, c] = [res[(1000+start) % n], res[(2000+start) % n], res[(3000+start) % n]];
    println!("part 1 {res:?} {:?}: {}", [a,b,c], a + b + c);
}
