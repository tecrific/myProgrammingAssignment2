## An overall description of what these functions do...
## Given the following Vector-mean functions:
# makeVector <- function(x = numeric()) {
#     m <- NULL
#     set <- function(y) {
#         x <<- y
#         m <<- NULL
#     }
#     get <- function() x
#     setmean <- function(mean) m <<- mean
#     getmean <- function() m
#     list(set = set, get = get,
#          setmean = setmean,
#          getmean = getmean)
# }
# cachemean <- function(x, ...) {
#     m <- x$getmean()
#     if(!is.null(m)) {
#         message("getting cached data")
#         return(m)
#     }
#     data <- x$get()
#     m <- mean(data, ...)
#     x$setmean(m)
#     m
# }

## This function creates a matrix object that uses 'solve' to set its inverse
## then caches it...

makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL
    set <- function(y) {
        x <<- y
        mInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) mInverse <<- solve
    getInverse <- function() mInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the matrix returned above.
## If the inverse has already been calculated then cacheSolve retrieves the
## inverse from the cache...

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mInverse <- x$getInverse()
    if (!is.null(mInverse)) {
        message("getting cached data of inverted matrix!")
        return(mInverse)
    }
    matrix <- x$get()
    mInverse <- solve(matrix, ...)
    x$setInverse(mInverse)
    mInverse
}
