## Functionality to increase matrix inverse performance by enabling
## caching of the result.

## makeCacheMatrix returns a wrapper around matrix that provides
## functionality to cache the inverse of that matrix to avoid 
## costly recalulcations.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Set new matrix, clear the cached inverse.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Get the matrix.
  get <- function() x
  
  # Set the inverse of the matrix, this is the cached inverse.
  setinverse <- function(value) inverse <<- value
  
  # Get the cached inverse value.
  getinverse <- function() inverse
  
  # Return a list containing the above functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The first time cacheSolve is called for a specific 'x'
## the inverse of x's matrix will be calculated and cached.
## On subsequent calls with the same 'x' the cached value will
## be returned.

cacheSolve <- function(x, ...) {
  # Get inverse of x and check if it contains none null value.
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Calculate the inverse and cache the result on x using setinverse()
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
