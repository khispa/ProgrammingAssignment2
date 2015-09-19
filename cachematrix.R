## Those functions combined calculate the inverse of a given matrix that is invertible and store it in Cache memory.
## If the same matrix inversion is required again, the cache values are used, intead of calculate them again.

## makeCacheMatrix creates an S4 object that is able to store in cache the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve(x)
  getinv <- function() i
  list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve takes the formal class output from previous function and calculates the inverse or 
## returns the cache values if it has been calculated before. 
## Remember: x must be S4 object from previous function.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
