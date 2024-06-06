## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize im for inverse matrix object
  im <- NULL
  ## set matrix object
  setm <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ## get matrix object
  getm <- function() x
  ## set inverse matrix object
  setim <- function(solveM) im <<- solveM
  ## get inverse matrix object
  getim <- function() im
  ## set all getter and setter functions
  list(getm = getm, setm = setm, getim = getim, setim = setim)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getim()
  ## Check for the given matrix object in cache 
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  ## If no cache, get the given matrix object
  data <- x$getm()
  ## Values for inverse matrix for the given matrix object
  invm <- solve(data, ...)
  ## set the result in the given matrix
  x$setim(invm)
  
  invm
}
