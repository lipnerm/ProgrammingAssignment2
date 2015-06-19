## Caches a matrix

makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    set <- function(x) {
        matrix <<- x;
        inverse <<- NULL;
    }
    get <- function() return(matrix);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list)
}

## Calculates the inverse of a matrix

cacheSolve <- function(matrix, ...) {
    inverse <- matrix$getinv()
    if(!is.null(inverse)) {
        return(inverse)
    }
    data <- matrix$get()
    inverse <- solve(data, ...)
    matrix$setinv(inverse)
    return(inverse)
}
