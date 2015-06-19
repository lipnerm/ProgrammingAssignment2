## Caches a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() return(x)
  setinv <- function(inverse) m <<- inverse
  getinv <- function() return(m)
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Calculates the inverse of a matrix

cachematrix <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  return(m)
}
