**HASKELL PROGRAMMING**

------------------------------------------------------

**Comments: Project contains the following**

- Following are the functions implemented using 
  Haskell expressions syntax
	- quadraticRoots a b c
	- iterateFunction f a
	- multiples n 
	- hailstones n
	- hailstonesLen n
	- sumAbsDiffs numberList
	- distances pt points
	- sumLengths pointsList
	- occurrences s c
	- foldTree treeFn leafFn <tree or leaf>
	- flattenTree tree
	- catenateTreeLists tree

------------------------------------------------------

  Provided file prj3-sol.hs contains the functions
  implementations only, you may need to implement a 
  separate routine to proceed for execution.

------------------------------------------------------

To run a sample program using the test example case

	1. Implement a main function in the file prj3-sol.hs as

		main =  do
		  putStr "-- "; print $ quadraticRoots 2 5 2 

  2. Use the following command to run the sample test 
    program from the linux commandline.

    ghc prj3-sol.hs && ./prj3-sol
