## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. My assignment
## is to write a pair of following two functions to cache the inverse of a matrix.

## makeCacheMatrix: This function creates a matrix object that can cache its inverse
## A. set the value of the matrix
## B. get the value of the matrix
## C. set the value of inverse of the matrix
## D. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the matrix. It checks first if
## the inverse has been calculated already.  If not, it calculated the inverse, 
## sets the value in the cache via# setinverse function. If so, it gets the result 
## and skips the calculation.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
