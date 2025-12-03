import Aoc2025

-- def substr (s : String) (lower : Nat) (upper : Nat) (h: lower < upper): String :=
--   let a := s.pos! (String.Pos.Raw.mk lower)
--   let b := s.pos! (String.Pos.Raw.mk upper)
--   let slice := String.Slice.mk s a b (by
--     dsimp [a, b]
      
--   )
--   slice.copy

-- def prefix (s : String) (l : Nat) (h : l ≤ s.length) : String :=
  
def print (r : Std.Range) : String :=
  s!"{r.start}-{r.stop}"

def forEach (r : Std.Range) (f : Nat → α) [ToString α] : List α  := Id.run do
  --dbg_trace "Range: {print r}"
  let mut xs := [] 
  for i in r do
     xs := xs.append [f i]
  --dbg_trace "Result: {xs}"
  return xs


def invalid (min : Nat) (max: Nat) (s : Nat) (n : Nat) : Nat :=
  let tmp := n * s + n
  if min <= tmp && tmp <= max then tmp else 0


def isEvenLength (n : Nat) : Bool :=
  let s := toString n
  s.length % 2 == 0

def findInvalid (r : String) : Int :=
  match r.splitOn "-" with
  | [lowS, highS] =>
      dbg_trace "Range:  {lowS} {highS}"
      let halfWidth := (max lowS.length highS.length) / 2
      let halfLowS := lowS.take (lowS.length - halfWidth)
      let halfHighS := highS.take (highS.length - halfWidth)
      let halfLow := halfLowS.toNat!
      let halfHigh := halfHighS.toNat!
      let low := lowS.toNat!
      let high := highS.toNat!
      let scale := 10 ^ halfWidth -- if halfWidth > 0 then 10 ^ halfWidth else 0
      dbg_trace "halfLow {halfLow}, halfHigh {halfHigh}, scale {scale}"
      let ids := forEach [halfLow : halfHigh + 1] (invalid low high scale)
      let ids' := ids.filter isEvenLength
      dbg_trace "Ids found: {ids'}"
      ids'.sum
  | _ => -1

def solve1 (input : String) : String :=
  let ranges := input.splitOn ","
  let counts := ranges.map findInvalid
  toString (counts.sum)

def main : IO Unit := interact solve1


