Ex    ← $$ aaa: you hhh
        $$ you: bbb ccc
        $$ bbb: ddd eee
        $$ ccc: ddd eee fff
        $$ ddd: ggg
        $$ eee: out
        $$ fff: out
        $$ ggg: out
        $$ hhh: ccc fff iii
        $$ iii: out
Input ← &fras "dayB.txt"

Parse ← ⊙(˜⊂□[]) ˜⊂□"out" ⊜(⊙(□ ⊜∘ ⊸≠@  °□) °⊟ ⊜□ ¬⊸⦷": ") ⊸≠@\n

MapEdges ← (
  # names edges
  ⊃(⋅∘|∘)
  ⍜⊙(¤¤) ⊸⍚≡(˜⨂ □) # map names to indices
)

# part 1
RoutesFrom ← (
  # . start names edges
  ⊃(⋅MapEdges|∘)      # . edges names start
  ⊙(⨂⊙□)              # . edges start
  ⊃([[1 ⋅∘]]|×0⇡ ⧻|∘) # . work result edges
  ⍢(
    ◡(◌◌ ⧻ ⊙(/+))
    °⊂       # . work0 workn result edges
    ⊃(⋅⊙∘|∘) # . workn result work0 edges
    ⊙⊙(
      # . work0 edges
      °⊟      # . n_paths i_next edges
      ⊙(°□⊸⊏) # . n_paths next edges
    )

    # . workn result n_paths next edges
    ⊃(⋅⋅⋅∘|⋅⋅∘|⊙∘)
    # . next n_paths workn result edges
    ⊃(∧(⊂[⊃(⋅∘|∘)]) # join [n_paths next] to work
    | ⊙⊙◌           # . next n_paths result ..
      ⬚0+×°⊚        # add
    )
  )(>0 ⧻)
  ⋅⊙◌
)
Parse Ex # . names edges
⊣ RoutesFrom "you"

# RoutesFrom "svr" Parse Input

Exx ← $$ svr: aaa bbb
      $$ aaa: fft
      $$ fft: ccc
      $$ bbb: tty
      $$ tty: ccc
      $$ ccc: ddd eee
      $$ ddd: hub
      $$ hub: fff
      $$ eee: dac
      $$ dac: fff
      $$ fff: ggg hhh
      $$ ggg: out
      $$ hhh: out

# part 2
# works for DAG, but paths from "svr" and "dac" take forever
# I could try topo sorting, and prune anything after dest node from worklist
# maybe they already are topo sorted?
RoutesFromTo ← (
  # . start end names edges
  ⊃(˜⨂□⊙◌|⋅(˜⨂□)|⋅⋅∘) # . i_start i_end names edges
  ⊃(⊏|⊏⊙◌) ⍜-(⇡+1)    # . names edges  (bounded)
  MapEdges            # . edges names
  ⊙◌

  ⊃([[1 0]]|×0⇡+1 ⧻|˜⊂□[]) # . work result edges
  ⍢(
    ◡(◌◌ ⧻ ⊙(/+))
    °⊂       # . work0 workn result edges
    ⊃(⋅⊙∘|∘) # . workn result work0 edges
    ⊙⊙(
      # . work0 edges
      °⊟      # . n_paths i_next edges
      ⊙(°□⊸⊏) # . n_paths next edges
    )

    # . workn result n_paths next edges
    ⊃(⋅⋅⋅∘|⋅⋅∘|⊙∘)
    # . next n_paths workn result edges
    ⊃(∧(⊂[⊃(⋅∘|∘)]) # join [n_paths next] to work
    | ⊙⊙◌           # . next n_paths result ..
      ⬚0+×°⊚        # add
    )
  )(>0 ⧻)
  ⊏-2⊸⧻ ⋅⊙◌
)

# It is not pre-sorted :(
# TODO: topo sort
# Parse Input
# ≡(□ ? > ⊙°□ ?) ⇡ ⊸⧻ MapEdges

# ◡(◌ ? ˜⨂ ≡□ ["svr" "fft" "dac" "out"])

# /× [⊃(
#     RoutesFromTo "svr" "fft"
#   | RoutesFromTo "fft" "dac"
#   | RoutesFromTo "dac" "out"
#   )]
# Parse Input
# /× [⊃(
#     RoutesFromTo "svr" "dac"
#   | RoutesFromTo "dac" "fft"
#   | RoutesFromTo "fft" "out"
#   )]
