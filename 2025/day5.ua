Example ← $$ 3-5
          $$ 10-14
          $$ 16-20
          $$ 12-18
          $$ 
          $$ 1
          $$ 5
          $$ 8
          $$ 11
          $$ 17
          $$ 32
# Input   ← Example
Input ← °(˜⊂ "\n") &fras "day5.txt"
Parse ← ≡([⋕ ⊙⋕] °$"_-_")°/$"_\n_" ⊙(⋕°/$"_\n_") °$"_\n\n_"

# part 1
∧(+ ⊙⊸(¬/×/= <) ⊃(⋅∘|∘)) ⊃(⋅∘|0 ∘) ⍉ ⍆≡(+[0 1]) Parse Input
⊙◌

⍆≡(+[0 1]) Parse Input
⊙◌
# cur:[2] top:[2] acc
# either merge cur with top, or add top's range to acc
# either way, we end with [2] acc
# `do` can be `if` if check is "can merge" and after
⊙([0 0] 0)
Merge    ← [⊙(↥⊣⊙⊣) ◡(↧⊢⊙⊢)] # [min max]
CanMerge ← ≥⊢⊙⊣              # sorted-> d>=a, check b>=c

∧(
  ⍢(⊙[0 0] Merge|CanMerge)
  ⊙(+-°⊟)
)
+-°⊟
