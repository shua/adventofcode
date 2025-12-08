Ex ← $$ 162,817,812
     $$ 57,618,57
     $$ 906,360,560
     $$ 592,479,940
     $$ 352,342,300
     $$ 466,668,158
     $$ 542,29,236
     $$ 431,825,988
     $$ 739,650,466
     $$ 52,470,668
     $$ 216,146,977
     $$ 819,987,18
     $$ 117,168,530
     $$ 805,96,715
     $$ 346,949,466
     $$ 970,615,88
     $$ 941,993,340
     $$ 862,61,35
     $$ 984,92,344
     $$ 425,690,689

Input  ← Ex
Count  ← 10
Input  ← °(˜⊂ @\n) &fras "day8.txt"
Count  ← 1000
Parsed ← ⊜(⊜⋕ ⊸≠@,) ⊸≠@\n Input
SqDist ← /+ ˙× -

# TODO: probably a better way to do this using `tuples`
˙⊞SqDist Parsed
≡₀(˜⊂ °□) ≡₁□ °⊡
↯[˙× ⧻Parsed 3]
≡(⊏ 1_2) ⍆                      # sort by dist
↯[⧻Parsed] [0]                  # skip first N
♭↯[-⧻Parsed ˙× ⧻Parsed] [[0 1]] # rest are duplicate pairs
▽˜⊂

Step ← +⊃(×|⋅(×¬)) ⊙⊸= °⊟ ⊸⊏

# part 1
# ⊙(⇡ ⧻Parsed)
# ⊏ ⇡Count ?
# ∧(Step)
# /× ⊏ ⇡3 ⇌⍆ ˙⊜⧻ +1 ⍆

# part 2
⍢(⊸˜Step ⊙°⊂ ⊙◌)(¬/×=°⊂) ⇡⧻Parsed [0 0]
⊏ ⊙⋅Parsed ◌
/× ≡(⊏0)
