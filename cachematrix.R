# Make a matrix into an object with
# properties required for later
# calculating the inverse

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

# Uses the object created by makeCacheMatrix
# to cache the inverse of the matrix
# if no inverse has already been cached

cachematrix <- function(q, ...) {
  qinv <- q$getinv()
  if(!is.null(qinv)) {
    print("using cache data")
    return(qinv)
  }
  
# If an inverse is not already cached,
# calculate the inverse of the matrix
  
  data <- q$get()
  qinv <- solve(data, ...)
  q$setinv(qinv)
  return(qinv)
}
