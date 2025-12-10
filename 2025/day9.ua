Ex ← $$ 7,1
     $$ 11,1
     $$ 11,7
     $$ 9,7
     $$ 9,5
     $$ 2,5
     $$ 2,3
     $$ 7,3

Input ← Ex
Input ← &fras "day9.txt"

Parsed ← ⊜(⊜⋕ ⊸≠@,) ⊸≠@\n Input
SortedRects ← (
  Parsed
  ⊏ ⧅< 2 ⇡⊸⧻
  ≡([⊃(/× +1 ⌵-°⊟|□)])
  ⇌⍆
)

# part 1
°□⊡0_0 SortedRects

# part 2
# min/max x and y
# ⊙[⊃(⊢|⊣)] [⊃(⊢|⊣)] ⊃(⍆≡(⊏0)|⍆≡(⊏1)) #
# [1681 98242]
# [1750 98267]
#
# check if they're all "move x" followed by "move y"
# /×⧈(≍¬°⊟) 2 =⊸↻1 ## 1

# reduce full range to just sorted x,y coords, then we can map to that discrete space
CoordList ← (
  Parsed
  ≡(˙⊜(⊏0) ⍆) ⍉
)
MapCoord ← ≡⨂ CoordList

# ⊜(⊜⋕ ⊸≠@,) ⊸≠@\n Input
# ≡(MapCoord)

# fill a graph
=∞ °△ +1 [⊃(⊣⍆⊢|⊣⍆⊣)] ⍉≡MapCoord Parsed
∧(⍜⊡(⋅1)) ≡MapCoord Parsed # 1. mark nodes
\≠                         # 2. sweep y coords, on/off based on nodes
≡(\≠)                      # 3. sweep x coords, on/off based on graph/nodes
≡(≠0+ ⊸↻¯1) ≠0+ ⊸↻¯1       # clean up, need to add bottom, and right

# iterate from largest rect to smallest
# checking if all points in the rect are filled
PointsInRect ← ⊞(⊡⊟) ⊙⊙¤ ⊙(⍜-(⇡+1) °⊟) (⍜-(⇡+1) °⊟) °⊟ ≡(⍆) ⍉
≡(⍜(⊡1)(□ ≡MapCoord °□)) SortedRects
⍢(↘1)(¬ /×♭ PointsInRect °□ ⊣ ⊏0)
⊢ ⊙◌
