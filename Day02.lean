import Aoc2025
import Std.Data.HashSet

-- def substr (s : String) (lower : Nat) (upper : Nat) (h: lower < upper): String :=
--   let a := s.pos! (String.Pos.Raw.mk lower)
--   let b := s.pos! (String.Pos.Raw.mk upper)
--   let slice := String.Slice.mk s a b (by
--     dsimp [a, b]
      
--   )
--   slice.copy

-- def prefix (s : String) (l : Nat) (h : l ≤ s.length) : String :=
  

def forEach (r : Std.Range) (f : Nat → α) [ToString α] : List α  := Id.run do
  let mut xs := [] 
  for i in r do
     xs := xs.append [f i]
  return xs


def splitRange (range : String × String) : List (String × String) :=
  let (low, high) := range
  if low.length == high.length
  then [(low, high)]
  else
    let midL := String.join (List.replicate low.length "9")
    let midH := String.join (["1"] ++ List.replicate low.length "0")
    [(low, midL), (midH, high)]

def invalid (min : Nat) (max: Nat) (s : Nat) (n : Nat) : Nat :=
  let tmp := n * s + n
  if min <= tmp && tmp <= max then tmp else 0


def build (digits : Nat) (root : Nat) (reps : Nat) : Nat :=
  match reps with
  | 0 => 0
  | n + 1 => build digits root n * 10 ^ digits + root

def checkForInvalid (range : String × String) (size : Nat) : List Nat :=
  let (lowS, highS) := range
  dbg_trace "Range:  {lowS} {highS} {size}"
  if lowS.length % size != 0
  then []
  else
    let lowPrefix := lowS.take size
    let highPrefix := highS.take size
    let reps := lowS.length / size
    let ids := forEach [lowPrefix.toNat! : highPrefix.toNat! + 1] (λ n =>
      build size n reps
    )
    let check x := lowS.toNat! <= x && x <= highS.toNat!
    let invalidIds := ids.filter check
    dbg_trace "Ids found: {invalidIds}"
    invalidIds

-- def isEvenLength (n : Nat) : Bool :=
--   let s := toString n
--   s.length % 2 == 0

-- def findInvalid (range : String × String) : Int :=
--   let (lowS, highS) := range
--   dbg_trace "Range:  {lowS} {highS}"
--   let halfWidth := (max lowS.length highS.length) / 2
--   let halfLowS := lowS.take (lowS.length - halfWidth)
--   let halfHighS := highS.take (highS.length - halfWidth)
--   let halfLow := halfLowS.toNat!
--   let halfHigh := halfHighS.toNat!
--   let low := lowS.toNat!
--   let high := highS.toNat!
--   let scale := 10 ^ halfWidth -- if halfWidth > 0 then 10 ^ halfWidth else 0
--   dbg_trace "halfLow {halfLow}, halfHigh {halfHigh}, scale {scale}"
--   let ids := forEach [halfLow : halfHigh + 1] (invalid low high scale)
--   let ids' := ids.filter isEvenLength
--   dbg_trace "Ids found: {ids'}"
--   ids'.sum

def parseInput (input : String) : List (String × String) :=
  let rawRanges := input.splitOn ","
  rawRanges.map (λ s => 
    match s.splitOn "-" with
    | [l, h] => (l, h)
    | _ => ("", "")  -- TODO should be add error to parsing?
  )

def ceilDiv (x : Nat) (y : Nat) : Nat :=
  if x % y == 0
  then x / y
  else x / y + 1

def solve1 (input : String) : String :=
  let ranges := parseInput input     
  let splitRanges := ranges.flatMap splitRange
  let splitRanges' := splitRanges.filter (λ r => r.fst.length != 1)  -- just get rid of 1's
  let invalidIds := splitRanges'.map (λ r => checkForInvalid r (ceilDiv r.fst.length 2)) 
  toString invalidIds.flatten.sum

def checkAllSizes (range : String × String) : List Nat :=
  let invalidIds := forEach [1 : range.fst.length / 2 + 1] (λ n => checkForInvalid range n)
  invalidIds.flatten



def uniq (xs : List α) [BEq α] [Hashable α] : List α := 
  let set := Std.HashSet.ofList xs
  set.toList

def solve2 (input : String) : String :=
  let ranges := parseInput input     
  let splitRanges := ranges.flatMap splitRange
  let invalidIds := splitRanges.map checkAllSizes
  let invalidIds' := uniq (invalidIds.flatten)
  toString invalidIds'.sum

def main : IO Unit := interact solve2


