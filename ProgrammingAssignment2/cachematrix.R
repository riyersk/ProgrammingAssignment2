## makeCacheMatrix makes the Matrix class
## cacheSolve retrieves the inverse if it is cached and
## calculates the inverse if it is not.

## This is a Cache Matrix Class

makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y){
    x<<-y
    INV<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) INV <<- inverse
  getInverse <- function() INV
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This retrieves the inverse if cached and calculates the inverse if not

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  INV <- x$getInverse()
  if(!is.null(INV)){
    message("getting cached data")
    return (INV)
  }
  data <- x$get()
  INV <- solve(x,...)
  x$setInverse(INV)
  INV
}