# Input ← &fras "day1.txt"
Input    ← "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"
WordMask ← ≠ @\n Input
RMask    ← ≠ @R Input
LMask    ← ≠ @L Input
# Numbers  ← ⊜⋕(/× [WordMask RMask LMask]) Input
Numbers ← ⊜⋕ ⊸(¬∊"RL\n") Input
# Sign    ← -(⍥1⊸⧻)× 2♭⊜(= "R")/+¬ [RMask LMask] Input
Sign ← -1 ×2 ♭⊜(= "R") ⊸(∊"RL") Input

# part 1
/+ =0◿100 \+ (⊂ [50] (× Numbers Sign))

# part 2
# [50 51 ¯101 ¯1 1 ¯1 2] # better example
⊂[50] × Numbers Sign
\+ ⟜(>0)        # moving up
⟜(=0◿100)       # eq 0
⌵- ⊸↻¯1 (⌈÷100) # number of times crossing 0
⊙⊃(¯×↻¯1|×)     # add 1 if moving up into 0, -1 if up from 0
⍜⊡(×0)0         # the first value is garbage
/+ ++
