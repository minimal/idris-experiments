import data.Vect

||| Vect of numbers from a to (b - 1)
total range2 : (start: Nat) -> (end: Nat) -> Vect (end `minus` start) Nat
range2 start end = range' (end `minus` start)
where total range' : (a: Nat) -> Vect a Nat
      range' Z     = []
      range' (S k) = (end `minus` (S k)) :: (range' k)
