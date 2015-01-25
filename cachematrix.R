## These functions can efficiently store a matrix and its inverse by computing the inverse once and
## caching it. It can then call the inverse from the cache instead of computing it every time.

## Using this function will create a list containing a set of functions. 
## Calling set(y) will allow the user to reset the matrix to y, and tell the function that
## a new inverse needs to be cached. Calling get() will return the matrix itself.
## The getIn and setInv allow the cacheSolve() function below to check if an inverse
## has been cached and to cache a inverse if needed.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv) 
}

## This function checks to see if the inverse has already been cached.
## If so, it returns the cached value. Otherwise it will calculate it.

cacheSolve <- function(x, ...) {        
    inv <- x$getInv()
    if(!is.null(inv)){
      message ("getting cached data")
      return (inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
