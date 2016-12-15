## This function creates a matrix object that can cache its inverse



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse of x
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. If the inverse has already been calculated and has no changed, then the cacheSolve shlould retreive the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()   ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv	
	
}
