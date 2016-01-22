## These 2 functions allow the user to interactively create a new matrix which 
## can cache its inverse to avoid the computation penalty of recalculating on
## each call.

## example:
## mat <- makeCacheMatrix(matrix(c(c(2,5),c(4,3)), 2, 2))
## cacheSolve(mat)
## cacheSolve(mat)
## mat <- makeCacheMatrix(matrix(c(c(1,2,2),c(2,2,1),c(1,2,3)),3,3))
## cacheSolve(mat)
## cacheSolve(mat)

## makeCacheMatrix returns a list of functions for setting, getting and caching
## the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    ## reset the matrix cache when x is set to a new matrix
    ## invalidating the cached version of the matrix inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks if the matrix inverse is cached
## If it is, return it
## If not, solve for the matrix inverse, cache, and return

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
