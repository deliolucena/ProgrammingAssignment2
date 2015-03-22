## This function is a double function (a pair of functions: makeCacheMatrix and cacheSolve) for 
## calculate and cache the inverse of a matrix
## The function "makeCacheMatrix create a special matrix objet that can cache its inverse
makeCacheMatrix <- function(x=matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(
        set = set,
        get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse)
}
## The function cacheSolve computes the matrix created by the function adove
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of "x"
    i <- x$get_inverse()
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inverse(i)
    i
}
