## The following functions are used to create the inverse of a matrix
## If the matrix inversion has already been generated it will look to the cache 
## to find the answer.  If the inversion has not occurred for the given matrix
## the function will use the solve() function to determine the inversion and 
## assign the inverted matrix to the cache.


## This function simply initializes some variables and creates the subfunctions 
## that we will use to actually do the work.
## To call these listed function below we will use the name of the master function
## along with the $ to explicitly ask for the subfunction.  This construct is used
## in the function cacheSolve() below.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL  # sets m to NULL for the function
     set <- function(y) {
          x <<- y
          m <<- NULL  # sets m to NULL for the parent environment
     }
     get <- function() x  # returns the matrix
     setmi <- function(solve) m <<- solve # this solves the inverse of the matrix
     getmi <- function() m  # retrieves the inverse of the matrix
     list(set = set, get = get,
          setmi = setmi,
          getmi = getmi)  # creates the list of objects so they are accessible from the main function
}


## The following function will check for a cached version of the inverse of the matrix
## If it does not find the inverse value it will generate it using solve() and define
## the inverse into the variable m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getmi()  #  Gets the matrix inversion from cache if set
     if(!is.null(m)) {  # checks to see if there is a value for m from the get above
          message("getting cached data")  # if there is a value it will pull from 
          return(m)  # the cache and return the matrix inversion
     }
     data <- x$get()  # if the cache does not contain the inversion
     m <- solve(data, ...) # this line will generate the inversion
     x$setmi(m) # and set it to cached variable
     m
     
}
