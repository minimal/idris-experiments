import Data.Vect

word_lengths : Vect len String -> Vect len Nat
word_lengths [] = []
word_lengths (x :: xs) = length x :: word_lengths xs

-- insertion sort
insert : Ord elem =>
         (x : elem) -> (xs_sorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs


ins_sort : Ord elem => Vect n elem -> Vect n elem
ins_sort [] = []
ins_sort (x :: xs) = let xs_sorted = ins_sort xs in
                         insert x xs_sorted

-- 3.2.4 excercises

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + (my_length xs)

||| length using type var
my_length_vect : Vect n a -> Nat
my_length_vect {n} xs = n

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = (my_reverse xs) ++ [x]

mymap : (a -> b) -> List a -> List b
mymap f [] = []
mymap f (x :: xs) = f x :: mymap f xs

-- this one generated itself!!
mymapv : (a -> b) -> Vect n a -> Vect n b
mymapv f [] = []
mymapv f (x :: xs) = f x :: mymapv f xs


-- 3.3

create_empties : Vect n (Vect 0 elem)
create_empties = replicate _ []

transpose_helper : (x : Vect n elem) -> (xs_trans : Vect n (Vect k elem)) ->
                    Vect n (Vect (S k) elem)
transpose_helper [] [] = []
transpose_helper (x :: xs) (y :: ys) = (x :: y) :: transpose_helper xs ys
-- usage replaced by zipWith

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
                              zipWith (::) x xs_trans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) ->
            Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys
