

makeCacheMatrix <- function(x = matrix()) 
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  
  {
  inv <- NULL
  set <- function(y)
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment.
    
    {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) (inv <<- inverse)
  getInverse <- function() (inv) 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


cacheSolve <- function(x, ...)
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv))
    # get it from the cache and skips the computation.
    {
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculates the inverse 
  mat <- x$get()
  inv <- solve(mat,...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setInverse(invj)
  inv
}