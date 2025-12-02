# Input ← "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
Input  ← °(˜⊂ @\n) &fras "day2.txt"
RangeN ← ≡◇(⋕ °/$"_-_") °/$"_,_" Input
FromTo ← +(⊙(⇡+ 1⊣))⟜-(⊸⊢)

EvenDigits  ← ¬◿2⧻⊥10
HalvesMatch ← /× = ↻⊸(⌊÷2⧻) . ⊥10

# Brute force, generate the range of each pair, then filter that range down
/+ ≡(/+ ▽⊸≡HalvesMatch ▽⊸≡EvenDigits FromTo) RangeN

# a nicer approach might be to first filter pairs, generate ranges of the high digits, paste the digits together, and finally filter by the range defined by the pair
SplitRange ← ⊜[⊢⊙⊣.] ⊸≡(⧻⊥10) FromTo
RangeSplit ← ∧(˜⊂ SplitRange) RangeN []
RangeEven  ← ▽ ⊸≡(EvenDigits ⊢) RangeSplit
HighDigits ← ⍜(⊥10) (↘ ÷2⧻ .)
# HighDigits ← ⋕ ↙⊸(÷2⧻) $"_"

# Candidate values, how to compare them in range of pairs?
# ≡(□ ≡(⋕ $"__" .) FromTo) ≡₀HighDigits RangeEven
