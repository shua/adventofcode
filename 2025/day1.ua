Input ← &fras "day1.txt"
# Input    ←  "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"
WordMask ← ≠ @\n Input
RMask    ← ≠ @R Input
LMask    ← ≠ @L Input
Numbers  ← ⊜⋕(/× [WordMask RMask LMask]) Input
Sign     ← -(⍥1⊸⧻)× 2♭⊜(= "R")/+¬ [RMask LMask] Input
# /+ = 0\+(× Numbers Sign)
# × Numbers Sign
/+ = 0 ◿ 100 \+ (⊂ [50] (× Numbers Sign))
