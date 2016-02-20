import data.Vect

total range' : (a: Nat) -> (b: Nat) -> Vect a Nat
range' Z b = []
range' (S k) b = (k + b) :: (range' k b)

||| Vect of numbers from a to (b - 1)
total range2 : (a: Nat) -> (b: Nat) -> Vect (b `minus` a) Nat
range2 a b =  reverse (range2' a b)
  where range2' a b = (range' (minus b a) a)
