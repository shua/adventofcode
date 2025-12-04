Input ← $$ ..@@.@@@@.
        $$ @@@.@.@.@@
        $$ @@@@@.@.@@
        $$ @.@@@@..@.
        $$ @@.@@@@.@@
        $$ .@@@@@@@.@
        $$ .@.@.@.@@@
        $$ @.@@@.@@@@
        $$ .@@@@@@@@.
        $$ @.@.@@@.@.
# Input ← &fras "day4.txt"

PadY  ← ⬚0(˜⊂[[0]] ⊂[[0]])
PadX  ← ˜⊂0 ⊂0
Grid  ← PadY≡PadX ⊜(=@@) ⊸≠@\n Input
Neigh ← PadY ⧈(/+) 3 ≡(PadX ⧈(/+) 3)

# part 1
/+♭ ×Grid ≤4 Neigh Grid

# part 2
Step ← ×⊸(>4 Neigh)
Grid [1] 0 # cur, prev, counter
⍢(⊙(+1◌)   # drop prev, inc counter
  ⊸Step
| ¬≍)
⊙◌◌ # remove prev and counter
/+♭×Grid¬
