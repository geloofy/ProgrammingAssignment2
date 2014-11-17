## Functions to find the inverse of a matrix and cache the result

## List of a set of functions to handle the caching
## eg. a <- makeCacheMatrix(matrix(1:4,2,2))
## a$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

makeCacheMatrix <- function(x = matrix()) {
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

## Tries to obtain matrix inverse from cache, else calculates and caches result
## eg. cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


cacheSolve <- function(x, ...) {
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
