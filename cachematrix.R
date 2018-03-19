## Put comments here that give an overall description of what your
## functions do

## This creates a matrix and caches it for use later so the inverse can be
## solved

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    make <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getinv <- function() x
    makeInverse <- function(invert) inverse <<- invert
    getInverse <- function() inverse
    list(make = make,
         getinv = getinv,
         makeInverse = makeInverse,
         getinv = getinv)
}


## This function takes the products of the previous function and uses them
## to solve the inverse of the matrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if (!is.null(inverse)){return(inverse)
    }
    matrix <- x$getinv()
    inverse <- solve(matrix, ...)
    x$makeInverse(inverse)
    inverse
}
