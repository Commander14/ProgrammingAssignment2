## This is a set of functions that allows a user to create a matrix object that handles
## the returning of cached results on an inversion operation if the inversion has already
## been done once before

## A function that creates a matrix object with the intent of caching results
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## A solver that will return the inversion of a matrix, and if that particular
## inversion has been seen before it will return a cached result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}

## test function borrowed from Subramanian Kartik on the discussion boards
testFunc <- function()
{
    ## First I create a test matrix to invert...
    a<-matrix(data=1:4,nrow=2,ncol=2)
    print("Matrix to be inverted:")
    print(a)
    ## As cacheSolve() is expecting type makeCacheMatrix, use the constructor to create z from a
    z<-makeCacheMatrix(a)
    ## Now call cacheSolve() 5 times on z - 1st time should have no cached inverse, but subsequent calls should
    ## 5 times is arbitrary..2 should be enough
    print("1...Should not have cached data")
    zinv<-cacheSolve(z)
    print(zinv)
    
    print("2...Cached Data")
    zinv<-cacheSolve(z)
    print(zinv)

    print("3...Cached Data")
    zinv<-cacheSolve(z)
    print(zinv)

    zinv<-cacheSolve(z)
    print("Final result - Inverse Matrix:")
    print(zinv)
}
