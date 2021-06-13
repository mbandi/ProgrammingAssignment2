## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix stores the original and inverted matrices, while cacheSolve 
## returns the inverse of a matrix, and save it in the cache if it was not 
## previously resolved


## getter and setter functions for the original and inverted matrices
makeCacheMatrix <- function(originalMatrix = matrix()) {
  invertedMatrix <- NULL
  
  get <- function() originalMatrix
  
  set <- function(matrix) {
    originalMatrix <<- matrix
    invertedMatrix <<- NULL
  }
  
  getInverted <- function() invertedMatrix
  
  setInverted <- function(inverted) invertedMatrix <<- inverted
  
  list(get = get, 
       set = set,
       getInverted = getInverted,
       setInverted = setInverted)
}

## Return a matrix that is the inverse of 'x' and stores the result in a cache
cacheSolve <- function(x, ...) {
  inverted <- x$getInverted()
  if (!is.null(inverted)) {
    message("getting cached matrix")
    return (inverted)
  }
  
  message("calculating new matrix")
  inverted <- solve(x$get(), ...)
  x$setInverted(inverted)
  inverted
}