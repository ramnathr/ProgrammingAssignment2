## The functions below are part of the second programming assignment for "R
## Programming". They are written to speed up the computation of a matrix
## inverse through caching. Both functions use the code for 'makeVector' and
## 'cachemean' as a template (found on
## https://github.com/rdpeng/ProgrammingAssignment2)


## This function will create a special "matrix" - a list containing various
## functions to help with caching the inverse.

makeCacheMatrix <- function(x = matrix()) {
      if (!is.matrix(x)) {
            stop("Argument to 'makeCacheMatrix' is not a matrix!")
      }
      m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function accepts an object created with the 'makeCacheMatrix'
## function and will either compute the inverse using 'solve' or if the object
## has been used in a previous calculation, the function will return the cached
## result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
