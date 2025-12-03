import Aoc2025

inductive Rotation where
  | Left
  | Right

instance : ToString Rotation where
  toString (r: Rotation) :=
    match r with
    | .Left => "L"
    | .Right => "R"

deriving instance BEq for Rotation

open Rotation

def parseLine (s: String) : Rotation × Int :=
  let dir := if s.startsWith "R" then Right else Left
  match (s.drop 1).toNat? with
  | none => (dir, -1)
  | some x => (dir, x)

#eval -(-30) / 100

#eval -20 % 100

def rotate' (rs : List (Rotation × Int)) (pos : Int) : List (Int × Int) :=
  match rs with
  | [] => []
  | (d, amt) :: rs' => 
    let turns := amt / 100
    let amt' := amt % 100
    let pos' := match d with
                | Left => pos - amt'
                | Right => pos + amt'
    let turns' := if pos' < 0 || pos' >= 100 then turns + 1 else turns
    let turns'' := if pos' == 0 && d == Left then turns' + 1 else turns'
    let turns''' := if pos == 0 && d == Left then turns'' - 1 else turns''
    dbg_trace "{pos} {d} {amt} {turns'''}"
    (turns''', pos' % 100) :: rotate' rs' (pos' % 100)


def rotate (rs : List (Rotation × Int)) (pos : Int) : List (Int × Int) :=
  match rs with
  | [] => []
  | (d, amt) :: rs' => 
    let (turns, pos') := 
      match d with
      | Left => 
         let tmp := pos - amt
         let pos' := tmp % 100
         let start_zero := if pos == 0 then -1 else 0
         let end_zero := if pos' == 0 then 1 else 0
         (-tmp / 100 + 1 + start_zero + end_zero, pos')
         -- if tmp > 0
         --   then (0, tmp)
         -- else   
         --   let tmp' := tmp % 100
         --   if tmp' == 0
         --   then (- tmp / 100 + 1, tmp')
         --   else (- tmp / 100, tmp')
      | Right => 
         let tmp := pos + amt
         (tmp / 100, tmp % 100)

    -- let turns' := if pos' == 0 then turns + 1 else turns
    (turns, pos') :: rotate rs' pos'

def solve1 (moves : List (Rotation × Int)) : Int :=
  let start := 50
  let positions := rotate moves start 
  (positions.map Prod.snd).count 0

def solve2 (moves : List (Rotation × Int)) : Int :=
  let start := 50
  let positions := rotate' moves start 
  (positions.map Prod.fst).sum

def main : IO Unit := do
  let stdin <- IO.getStdin
  let lines <- stdin.lines
  let input := lines.toList.map parseLine
  let solution := solve2 input
  IO.println s!"{solution}"


