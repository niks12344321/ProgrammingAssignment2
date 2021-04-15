## The makecachematrix gets the inverse of the matrix provided if available and 
## returns a null otherwise

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## the cachesolve function calculates the inverse of matrix for new data or 
## otherwise gets it from the cache if available for same previous data

cacheSolve <- function(x, ...) 
{
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("getting cached inverse matrix for similar previous data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}