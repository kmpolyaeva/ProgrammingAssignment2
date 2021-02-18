## The following functions are about solving the inverse of a matrix
## by a caching the result within a lexical scope of a function

## The following fuction creates an object that 
## caches the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The following fuction computes the inverse of the matrix 
## returned be the function above.

cacheSolve <- function(x, ...) {
  b <- x$getinv()
  if(!is.null(b)) {
    message("getting cached result")
    return(b)
  }
  data <- x$get()
  b <- solve(data, ...)
  x$setinv(b)
  b
}
