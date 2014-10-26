## These functions can be used to set up a matrix with a cacheble
## inverse.  makeCacheMatrix constructs the matrix and cacheSolve 
## handles the caching.

## Takes a matrix and returns a list of functions which act as a
## special kind of matrix which has a cacheable inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the cahced inverse of a matrix if possible.  If not,
## it finds the inverse, caches it, and return it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
