## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y){
        x <<- y
        r <<- NULL
    }
    get <- function() x
    setreverse <- function(reverse) r <<- reverse
    getreverse <- function() r
    list(set = set, get = get, setreverse = setreverse, getreverse = getreverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    r <- x$getreverse()
    if(!is.null(r)) {
        message("getting cached data")
        return(r)
    }
    data <- x$get()
    r <- solve(data, ...)
    x$setreverse(r)
    r
}
