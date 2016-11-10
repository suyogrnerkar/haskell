import Data.List
import Debug.Trace

-- Problem 1
-- Return pair containing roots of quadratic equation a*x**2 + b*x + c.
-- The first element in the returned pair should use the positive 
-- square-root of the discriminant, the second element should use the 
-- negative square-root of the discriminant.  Need not handle complex
-- roots.
quadraticRoots :: Floating t => t -> t -> t -> (t, t)
quadraticRoots a b c = (p_root, n_root)
  where
    real =  - b / (2 * a)
    delta = b * b - 4 * a * c
    p_root = real + sqrt delta / (2 * a)
    n_root = real - sqrt delta / (2 * a)


-- Problem 2
-- Return infinite list containing [z, f(z), f(f(z)), f(f(f(z))), ...]
-- May use recursion.
iterateFunction :: (a -> a) -> a -> [a]
iterateFunction f a =
  a : iterateFunction f (f a)


-- Problem 3
-- Using iterateFunction return infinite list containing 
-- multiples of n by all the non-negative integers.
-- May NOT use recursion.
multiples n =
  (map (\ y -> y * n) (iterateFunction (\ y -> y + 1) 0))


-- Problem 4
-- Use iterateFunction to return an infinite list containing list 
-- of hailstone numbers starting with n.
-- Specifically, if i is a hailstone number, and i is even, then
-- the next hailstone number is i divided by 2; if i is a hailstone
-- number and i is odd, then the next hailstone number is 3*i + 1.
-- May NOT use recursion.
hailstones :: Integral a => a -> [a]
hailstones n =
  (iterateFunction (\ hsn -> if even hsn then div hsn 2 else 3 * hsn + 1) n)


-- Problem 5
-- Return length of hailstone sequence starting with n terminating
-- at the first 1.
-- May NOT use recursion.  Can use elemIndex from Data.List
hailstonesLen :: Integral a => a -> Int
hailstonesLen n =
  case elemIndex 1 (hailstones n) of
    Just counter -> counter + 1
    Nothing -> 0


-- Problem 6
-- Given a list of numbers, return sum of the absolute difference
-- between consecutive elements of the list.
-- May NOT use recursion.
sumAbsDiffs :: Num a => [a] -> a
sumAbsDiffs numberList =
  foldr (+) 0 (
    map (\ num -> abs num) (
      zipWith (-) numberList (tail numberList)
    )
  )


-- Problem 7
-- The x-y coordinate of a point is represented using the pair (x, y).
-- Return the list containing the distance of each point in list
-- points from point pt.
-- May NOT use recursion.
distances :: Floating b => (b, b) -> [(b, b)] -> [b]
distances pt points =
  [sqrt((x - fst pt)^2 + (y - snd pt)^2) | (x, y) <- points]


-- Problem 8
-- Given a list of coordinate pairs representing points, return the
-- sum of the lengths of all line segments between successive 
-- adjacent points.
-- May NOT use recursion.
sumLengths :: Floating a => [(a, a)] -> a
sumLengths pointsList =
  foldr (+) 0 (
    zipWith (\ x y -> sqrt((fst y - fst x)^2 + (snd y - snd x)^2))
      pointsList
      (tail pointsList))


-- Problem 9
-- Given a string s and char c, return list of indexes in s where c
-- occurs
occurrences s c = elemIndices c s


-- A tree of some type t is either a Leaf containing a value of type t,
-- or it is an internal node (with constructor Tree) with some left
-- sub-tree, a value of type t and a right sub-tree.
data Tree t = Leaf t
            | Tree (Tree t) t (Tree t)

-- Problem 10
-- Fold tree to a single value.

-- If 'tree' is a Tree node, then it's
-- folded value is the result of applying ternary treeFn to the result
-- of folding the left sub-tree, the value stored in the Tree node and
-- the result of folding the right sub-tree;
-- If 'tree' is a Leaf node,
-- then the result of the fold is the result of applying the unary
-- leafFn to the value stored within the Leaf node.
-- May use recursion.

-- For all below: lst: left sub tree , rst: right sub tree, val: node, lf: leaf

-- E.g. >>> foldTree (\ t1 t t2 -> t1+3*t+t2) (\x -> x*2) (Leaf 5)
-- E.g. >>> foldTree (\ t1 t t2 -> t1+3*t+t2) (\x -> x*2) (Tree (Leaf 3) 2 (Leaf 4))
foldTree :: (t1 -> t -> t1 -> t1) -> (t -> t1) -> Tree t -> t1
foldTree treeFn leafFn (Leaf tree) = leafFn tree
foldTree treeFn leafFn (Tree lst val rst) =
  treeFn (foldTree treeFn leafFn lst) val (foldTree treeFn leafFn rst)


-- Problem 11
-- Return list containing flattening of tree.  The elements of the
-- list correspond to the elements stored in the tree ordered as per 
-- an in-order traversal of the tree. Must be implemented using foldTree.
-- May NOT use recursion.
-- E.g. >>> flattenTree  (Tree (Leaf 5) 3 (Tree (Leaf 3) 2 (Leaf 4)))
-- (++) ((++) lst [val]) rst): can be rewritten as lst ++ [val] ++ rst
flattenTree :: Tree a -> [a]
flattenTree tree =
  foldTree (\ lst val rst -> (++) ((++) lst [val]) rst) (\ lf -> [lf]) tree


-- Problem 12
-- Given tree of type (Tree [t]) return list which is concatenation
-- of all lists in tree.
-- Must be implemented using flattenTree.
-- May NOT use recursion.
catenateTreeLists :: Tree [a] -> [a]
catenateTreeLists tree =
  foldl (++) [] (flattenTree tree)


main =  do
    putStr "-- "; print $ quadraticRoots 2 5 2
    putStr "-- "; print $ quadraticRoots 5 6 1
    putStr "-- "; print $ take 10 $ iterateFunction (\x->x+1) 0
    putStr "-- "; print $ take 5 $ iterateFunction (\x->x*x) 2
    putStr "-- "; print $ take 10 $ multiples 3
    putStr "-- "; print $ take 10 $ multiples (-3)
    putStr "-- "; print $ take 15 $ hailstones 3
    putStr "-- "; print $ take 15 $ hailstones 7
    putStr "-- "; print $ hailstonesLen 3
    putStr "-- "; print $ hailstonesLen 7
    putStr "-- "; print $ hailstonesLen 77031
    putStr "-- "; print $ sumAbsDiffs []
    putStr "-- "; print $ sumAbsDiffs [2]
    putStr "-- "; print $ sumAbsDiffs [1..5]
    putStr "-- "; print $ sumAbsDiffs [1, 3, -5, 5]
    putStr "-- "; print $ distances (0, 0) []
    putStr "-- "; print $ distances (0, 0) [(1, 1), (0, 2), (3, 4)]
    putStr "-- "; print $ sumLengths []
    putStr "-- "; print $ sumLengths [(0, 0)]
    putStr "-- "; print $ sumLengths [(0, 0), (0, 2), (3, 6)]
    putStr "-- "; print $ sumLengths [(0, 0), (0, 2), (3, 6), (0, 2)]
    putStr "-- "; print $ occurrences "twas brillig and the slithy toves" 't'
    putStr "-- "; print $ occurrences "twas brillig and the slithy toves" 'x'
    putStr "-- "; print $ foldTree (\t1 t t2->t1 + 3*t + t2) (\x->x*2) (Leaf 5)
    putStr "-- "; print $ foldTree (\t1 t t2->t1 + 3*t + t2) (\x->x*2) (Tree (Leaf 3) 2 (Leaf 4))
    putStr "-- "; print $ foldTree (\t1 t t2->t1 + 3*t + t2) (\x->x*2)(Tree (Leaf 5) 3 (Tree (Leaf 3) 2 (Leaf 4)))
    putStr "-- "; print $ flattenTree (Leaf 5)
    putStr "-- "; print $ flattenTree  (Tree (Leaf 5) 3 (Tree (Leaf 3) 2 (Leaf 4)))
    putStr "-- "; print $ flattenTree  (Tree (Leaf [5]) [3] (Tree (Leaf [3, 2]) [1, 2] (Leaf [4, 5])))
    putStr "-- "; print $ catenateTreeLists  (Tree (Leaf [5]) [3] (Tree (Leaf [3, 2]) [1, 2] (Leaf [4, 5])))
    putStr "-- "; print $ catenateTreeLists  (Tree (Leaf "twas ") "brillig " (Tree (Leaf "and ") "the slithy " (Leaf "toves")))

