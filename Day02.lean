import Aoc2025
import Std.Data.HashSet

/--
  Copyright (c) 2025 Ian Clement.

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any
  later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with this program. If not,
  see <https://www.gnu.org/licenses/>.
-/

-- TODO move to shared library, since it's likely useful, also re-write in functional style

/--
 Generate the list from applying `f` to each element in the range. 
 -/
def forEach (r : Std.Range) (f : Nat → α) [ToString α] : List α  := Id.run do
  let mut xs := [] 
  for i in r do
     xs := xs.append [f i]
  return xs

/--
 If a range changes by a single digit length, then split into two ranges:
 Ex: 3411-12332 would split to 3411-9999 and 10000-12332
 -/
def splitRange (range : String × String) : List (String × String) :=
  let (low, high) := range
  if low.length == high.length
  then [(low, high)]
  else
    let midL := String.join (List.replicate low.length "9")
    let midH := String.join (["1"] ++ List.replicate low.length "0")
    [(low, midL), (midH, high)]


/--
 Build a number by repeating the `root` a total of `reps` times.
 Ex: build 123 4 is 123123123123
 -/
def build (digits : Nat) (root : Nat) (reps : Nat) : Nat :=
  match reps with
  | 0 => 0
  | n + 1 => build digits root n * 10 ^ digits + root



/--
 Find "invalid" ids in the range by checking all prefixes of a given size. Only loops over valid prefixes to save time.

 Ex:  
  - checkForInvalid ("1023", "1456") 2 == [1111, 1212, 1313, 1414]
  - checkForInvalid ("1023", "1456") 1 == [1111]
 -/
def checkForInvalid (range : String × String) (prefixSize : Nat) : List Nat :=
  let (lowS, highS) := range
  --dbg_trace "Range:  {lowS} {highS} {size}"
  if lowS.length % prefixSize != 0
  then []
  else
    let lowPrefix := lowS.take prefixSize
    let highPrefix := highS.take prefixSize
    let reps := lowS.length / prefixSize
    let ids := forEach [lowPrefix.toNat! : highPrefix.toNat! + 1] (λ n =>
      build prefixSize n reps
    )
    let check x := lowS.toNat! <= x && x <= highS.toNat!
    let invalidIds := ids.filter check
    --dbg_trace "Ids found: {invalidIds}"
    invalidIds

/--
 Parse input
 -/
def parseInput (input : String) : List (String × String) :=
  let rawRanges := input.splitOn ","
  rawRanges.map (λ s => 
    match s.splitOn "-" with
    | [l, h] => (l, h)
    | _ => ("", "")  -- TODO should be add error to parsing?
  )

/--
  Ceiling of integer division
 -/
def ceilDiv (x : Nat) (y : Nat) : Nat :=
  if x % y == 0
  then x / y
  else x / y + 1

/--
 Solution for Part 1
 -/
def solve1 (input : String) : String :=
  let ranges := parseInput input     
  let splitRanges := ranges.flatMap splitRange
  let splitRanges' := splitRanges.filter (λ r => r.fst.length != 1)  -- just get rid of 1's
  let invalidIds := splitRanges'.map (λ r => checkForInvalid r (ceilDiv r.fst.length 2)) 
  toString invalidIds.flatten.sum


/--
 Get "invalid" ids for all prefix size up to the midpoint of the range string.
 -/
def checkAllSizes (range : String × String) : List Nat :=
  let invalidIds := forEach [1 : range.fst.length / 2 + 1] (λ n => checkForInvalid range n)
  invalidIds.flatten


/--
 Get list of unique values.
 -/
def unique (xs : List α) [BEq α] [Hashable α] : List α := 
  let set := Std.HashSet.ofList xs
  set.toList

/--
 Solution for Part 2
 -/
def solve2 (input : String) : String :=
  let ranges := parseInput input     
  let splitRanges := ranges.flatMap splitRange
  let invalidIds := splitRanges.map checkAllSizes
  let invalidIds' := unique (invalidIds.flatten)
  toString invalidIds'.sum


def main : IO Unit := interact solve2


