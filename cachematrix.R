## These functions cache the inverse of a matrix

## This function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) z <<- solve
        getsolve <- function() z
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        z <- x$getsolve()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setsolve(z)
        z
}
