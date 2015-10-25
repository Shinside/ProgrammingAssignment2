## This file contains a pair of functions that cache the inverse of a matrix.
## 1) makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## 2) cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.


## The makeCacheMatrix function creates a special "matrix" object that can cache its 
## inverse, this function represents a list of functions:
## $set: remembers the matrix
## $get: returns the matrix stored
## $setInverse: remembers the inverse of the stored matrix 
## $getInverse: returns the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The cacheSolve function calculates the inverse of the matrix stored with the 
## makeCacheMatrix function:
## 1) first it checks whether the inverse has already been calculated,
## 2.1) if so, it gets the inverse from the cache and skips the computation. 
## 2.2) if not, it calculates the inverse of the matrix and sets the value of
## the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        message("inverse was not cached, calculating the inverse now")
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
