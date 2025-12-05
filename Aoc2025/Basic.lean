def interact (f : String → String) : IO UInt32 := do
  let stdin ← IO.getStdin
  let contents ← stdin.readToEnd
  let stdout ← IO.getStdout
  stdout.putStr (f contents)
  return 0

def advent (args : List String) (part₁ : String → String) (part₂ : String → String) : IO UInt32 :=
  match args with
  | [] => interact part₁
  | ["1"] => interact part₁
  | ["2"] => interact part₂
  | _ => return 1

-- this is already defined
-- def flip (f : α → β → γ) (x : β) (y : α) : γ := f y x

def pipe {α : Sort u} {β : Sort v} {φ : Sort w} (f : α → β) (g : β → φ) : α → φ := g ∘ f

infixr:90 " ∣ " => pipe 

-- def a (x : String) : String × Nat := (x, 1)
-- def b (x : Nat) : String := toString x ++ "abc"
-- def c (x : Nat) : Nat := x * 5

-- #eval (a ∘ b ∘ c) 2
-- #eval (c ∣ b ∣ a) 2


/--
 Generate the list from applying `f` to each element in the range. 
 -/
def forEach (r : Std.Range) (f : Nat → α) : List α  := Id.run do
  let mut xs := [] 
  for i in r do
     xs := xs.append [f i]
  return xs


