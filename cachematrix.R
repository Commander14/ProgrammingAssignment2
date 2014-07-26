## This is a set of functions that allows a user to create a matrix object that handles
## the returning of cached results on an inversion operation if the inversion has already
## been done once before

## A function that creates a matrix object with the intent of caching results
makeCacheMatrix <- function(x = matrix()) {
    if(nrow(x) != ncol(x)) {
        stop("You can only invert a square matrix!")
    }

    ## default the cached value to NULL
    inverse <- NULL

    ## setup functions for the cacheSolve function to use to introspect whether there
    ## are cached results or not and to set/get them
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) inverse <<- solve
    getSolve <- function() inverse 

    ## expose the internal functions to the parent of this object
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## A solver that will return the inversion of a matrix, and if that particular
## inversion has been seen before it will return a cached result
cacheSolve <- function(x, ...) {
    ## fetch what we think might be a cached result, but might be NULL as well
    inverse <- x$getSolve()

    ## if we have a cached result, then by all means return it!
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    ## Haven't done this yet? Calculate the inversion
    data <- x$get()
    inverse <- solve(data, ...)

    ## cache the result in the passed in cacheMatrix cache
    x$setSolve(inverse)

    ## return the result
    inverse
}
