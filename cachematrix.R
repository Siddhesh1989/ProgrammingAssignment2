
## this function makeCacheMatrix() allows us to store
## the cached value of the inverse for a given matrix x

makeCacheMatrix <- function(x = matrix()) {
  inverseValue <- NULL
  ## the below function sets the value of the matrix
  ## the <<- operator is used because 
  ## we are explicitly assigning a value to a variable that is in the parent environment
  set <- function(y)
  {
    x <<- y
    inverseValue <<- NULL
  }
  ## getting the value of the matrix
  get <- function() x
  ##setting and getting the inverse
  setInverse <- function(inverseVal) inverseValue <<- inverseVal
  getInverse <- function() inverseValue
  ## create the list that allows the functions within makeCacheMatrix to be called
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## this function cacheSolve() will check the cache for cached values of the matrix inverse
## failing that it will compute the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## First check if the cache contains the inverse
  inverseValue <- x$getInverse()
  if(!is.null(inverseValue)) {
    message("getting inverse from cache")
    return (inverseValue)
  }
  ## get the value of the matrix in order to calculate the inverse
  ## now that we know the inverse is not cached
  data <- x$get()
  ## calculate inverse
  inverseValue <- solve(data)
  ## set the value of the inverse in the cache
  x$setInverse(inverseValue)
  ## return the inverse value
  inverseValue  
}
