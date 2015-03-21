## These functions enable to caching the inverse of a square matrix rather than 
## computing it repeatedly.
## It is assumed that the square matrix supplied is always invertible.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function returns the inverse of the special "matrix": 
## - retrieves it from cache if it has been already calculated
## - computes it if the matrix has changed

cachesolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)){
                message("retrieved from cache")
                return(s)
        }
        data <- x$get()
        ## The 'solve' function returns a matrix that is the inverse of 'x'
        s <- solve(data, ...)
        message("recomputed")
        x$setsolve(s)
        s
}