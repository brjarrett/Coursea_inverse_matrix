## Caching the inverse of a matrix, note that format for code was provided by
## the coursea "caching the mean of a vector" example and functions
## replaced (mean was replaced with solve)

## First function, makeCachematrix, creates a matrix, really just a list as
## output is list().
## matrix initialized and a value set, (NULL set initially to be able to populate)
## get value of the matrix
## Next set the value of the inverse of the matrix (solve func used)
## Then get the inverse of the matrix
## Output the results as a list
#

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This next function calculates the inverse of the "list" inverse created by
## function above, but first checks to see if inverse already calculated.
## It only does the calculation if the inverse is not already found.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
      if(!is.null(m)) {
          message("getting cached data (inversed matrix)")
          return(m)
      }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
