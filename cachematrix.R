## makeCacheMatrix makes the Matrix class
## cacheSolve retrieves the inverse if it is cached and
## calculates the inverse if it is not.

## This is a Cache Matrix Class

makeCacheMatrix <- function(x = matrix()) {
  #Initial value is null
  INV <- NULL
  #Initializes the matrix
  set <- function(y){
    x<<-y
    INV<<- NULL
  }
  #returns the matrix
  get <- function() x
  #sets inverse to be a specific value
  setInverse <- function(inverse) INV <<- inverse
  #returns the inverse
  getInverse <- function() INV
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This retrieves the inverse if cached and calculates the inverse if not

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #gets the inverse if it is cached
  INV <- x$getInverse()
  if(!is.null(INV)){
    message("getting cached data")
    return (INV)
  }
  #if not cached, use the get function to get the matrix
  data <- x$get()
  #Solve for the inverse
  INV <- solve(x,...)
  #and then cache the inverse for later
  x$setInverse(INV)
  #return the inverse
  INV
}