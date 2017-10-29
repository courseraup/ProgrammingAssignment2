## makeCacheMatrix takes a square invertible matrix as input and returns a list of utilities to set and fetch data, and set and fetch
# the matrix inverse.
## CacheSolve computes the inverse of the matrix returned by makeCacheMatrix, unless this is already cached, and in this
# case the cached version is returned for computational efficiency.

## Set and fetch a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Obtain the inverse of a square matrix. If the inverse is already cached from previous computations, the function prints
## a message and returns the cached version.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
