## makeCacheMatrix stores values of matrices and their inverses, while
## cacheSolve computes the inverse of a matrix

## makeCacheMatrix takes a n x n matrix input and caches it, while
## creating a cache storage location for any inverse, and allowing
## for retrieval of both values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks if the makeCacheMatrix function already has an
## inverse, calculates one if it doesn't, and stores it

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
