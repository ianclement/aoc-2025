import Aoc2025

 
def maxWithIndex' (xs: List α) (low : Nat) (high : Nat) [Ord α] : Option (α × Nat) :=
  if low == high
  then .none
  else 
    match xs with
      | [] => .none
      | x :: xs' => 
        match maxWithIndex' xs' (low + 1) high with
        | .some (y, j) => 
            if compare x y == .lt then (y, j) else (x, low)
        | .none => (x, low)

def maxWithIndex (xs: List α) (low : Nat) (high : Nat) [Ord α] : Option (α × Nat) :=
  maxWithIndex' (xs.drop low) low high


def digit (c : Char) : Nat := c.toNat - '0'.toNat

def largestJoltage (batteries : List Nat) : Nat := 
  match maxWithIndex batteries 0 (batteries.length - 1) with
  | .none => 0
  | .some (x, i) =>
    match maxWithIndex batteries (i + 1) batteries.length with
    | .none => 0
    | .some (y, _) => 
      x * 10 + y

def largestJoltage2' (left : Nat) (low : Nat) (batteries : List Nat) (acc : Nat) : Nat := 
  match left with
  | 0 => acc
  | left' + 1 => 
    match maxWithIndex batteries low (batteries.length - left + 1) with
    | .none => 0
    | .some (x, i) =>
      largestJoltage2' left' (i + 1) batteries (acc * 10 + x)

def largestJoltage2 (left : Nat) (low : Nat) (batteries : List Nat) : Nat :=
  largestJoltage2' left low batteries 0


def parseLine (input : String) : List Nat :=
  input.toList.map digit
  

def solve1 (input : String) : String :=
  let input' := input.splitOn "\n"
  let banks := input'.map parseLine
  let joltages := banks.map largestJoltage
  toString (joltages.sum)

def solve2 (input : String) : String :=
  let input' := input.splitOn "\n"
  let banks := input'.map parseLine
  let joltages := banks.map (largestJoltage2 12 0)
  toString (joltages.sum)


def main : IO Unit := interact solve2
