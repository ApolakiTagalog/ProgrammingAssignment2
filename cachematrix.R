## Caching the Inverse of a Matrix
##
## Note:  Always assume that the matrix supplied for the functions
##      herewith are invertible.
##
## This script contains two functions:
## makeCacheMatrix and cacheSolve
##
## makeCacheMatrix is a function that creates a special "matrix"
##              object that can cache its inverse. This function
##              performs the following operations:
##              1. set the value of the matrix
##              2. get the value of the matrix
##              3. set the inverse of the matrix
##              4. get the inverse of the matrix
## 
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(solve) inverseMatrix <<- solve
        getInverse <- function() inverseMatrix
        
        list( set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
        
}

## cacheSolve is a function that computes the inverse of the special
##              "matrix" returned by makeCacheMatrix.  If the
##              inverse has already been calculated, the function
##              retrieves the inverse from the cache.  This function uses
##              the solve() function to calculate the inverse of the matrix.
##              The solve() function originally accepts two arguments 'a' and 'b'.
##              if 'b' is left without any input argument, solve() calculates
##              the inverse of the first argument'a'.
##
cacheSolve <- function(x, ...) {
        
        inverseMatrix <- x$getInverse()
        
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
