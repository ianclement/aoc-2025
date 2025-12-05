/-
  Copyright (c) 2025 Ian Clement.

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any
  later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with this program. If not,
  see <https://www.gnu.org/licenses/>.
-/

import Aoc2025
import Mathlib.Data.List.Sort

/- -----------------------------------------------------------------------------
  Parsing & Output
-------------------------------------------------------------------------------/

def parseRanges (s: String) : List Std.Range :=
  let s' := s.splitOn "\n"
  let s'' := s'.map (λ s => 
    match s.splitOn "-" with
    | [lowS, highS] => [lowS.toNat! : highS.toNat! + 1]
    | _ => [0:0]
  )
  s''

def parseIds (s: String) : List ℕ :=
   let s' := s.splitOn "\n"
   s'.map (λ s => s.toNat!)

def parse (input : String) : List Std.Range × List ℕ :=
  match input.splitOn "\n\n" with
  | [ranges, ids] => 
    let ranges' := parseRanges ranges
    let ids' := parseIds ids
    (ranges', ids')
  | _ => ([], [])


-- def output (sol : α) [ToString α] : String := toString sol
def output (sol : ℕ) : String := toString sol

/- ----------------------------------------------------------------------------
  Part 1
------------------------------------------------------------------------------/

def Std.Range.member (r : Std.Range) (x : ℕ) : Bool :=
  r.start <= x && x < r.stop

def solve1 (xs: List Std.Range × List ℕ) : ℕ :=
  let (ranges, ids) := xs
  let fresh := ids.filter (λ id => 
    ranges.any (λ r => r.member id)
  )
  fresh.length

/- ----------------------------------------------------------------------------
  Part 2
------------------------------------------------------------------------------/

/- Define a new property since we'll be sorting by the range's start value -/

def RangeStartLe (r₁ r₂ : Std.Range) : Prop := r₁.start ≤ r₂.start

/- Relation is decidable, total and transitive: -/

instance : DecidableRel RangeStartLe := λ r₁ r₂ => Nat.decLe r₁.start r₂.start 
  
instance : IsTotal Std.Range RangeStartLe where
  total r₁ r₂ := Nat.le_total r₁.start r₂.start

instance : IsTrans Std.Range RangeStartLe where
  trans r₁ r₂ r₃ h₁ h₂ := Nat.le_trans h₁ h₂


/--
  Construct the union of the (possibly overlapping) ranges.
  Optimization: the scan can be linear when the ranges are sorted by start value (ascending), ensured by the proof term `h`
-/
def union (current : Std.Range) (ranges : List Std.Range) (h : List.Sorted RangeStartLe ranges) : List Std.Range := 
  match ranges with
  | [] => [current]
  | r :: rs => 
    -- prove that sub-list is also sorted (needed for the recursive call)
    have h' : List.Sorted RangeStartLe rs := by 
      apply List.Sorted.of_cons h

    -- if the new `r` range overlaps the current one, then expand the current, otherwise
    -- `r` is our new current range
    if r.start < current.stop
    then union [current.start : max r.stop current.stop] rs h'
    else current :: union r rs h'


def solve2 (xs: List Std.Range × List ℕ) : ℕ :=
  let (ranges, _) := xs

  -- sort the list and construct the proof term for the union function
  -- TODO: use a more efficient proof
  let rangesSorted := ranges.insertionSort (λ r₁ r₂ => r₁.start ≤ r₂.start)  
  have h: List.Sorted RangeStartLe rangesSorted:= by
    simp [rangesSorted]
    apply List.sorted_insertionSort RangeStartLe

  let rangesUnion := union [0:0] rangesSorted h
  let sizes := rangesUnion.map (λ r => r.size)
  sizes.sum

/- ----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------/

def main (args : List String) : IO UInt32 := 
  advent args (parse ∣ solve1 ∣ output) (parse ∣ solve2 ∣ output)


