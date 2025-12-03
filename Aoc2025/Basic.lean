def interact (f : String → String) : IO Unit := do
  let stdin ← IO.getStdin
  let contents ← stdin.readToEnd
  let stdout ← IO.getStdout
  stdout.putStr (f contents)

-- this is already defined
-- def flip (f : α → β → γ) (x : β) (y : α) : γ := f y x

def pipe {α : Sort u} {β : Sort v} {φ : Sort w} (f : α → β) (g : β → φ) : α → φ := g ∘ f

infix:90 " ∣ " => pipe 

#eval "abc\ndef".splitOn "̈\n" 
 

def f (xs : List Int) := (List.drop 1 ∣ List.take 1) xs

#eval f [1,2,3]




