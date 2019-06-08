rm(list = ls())

## function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function( mat = matrix()) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## To set the matrix
  set <- function( matrix ) {
    mat <<- matrix
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() mat # Return Matrix
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInverse <- function() inv # Return inverse property
    
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and there is no change in the matrix),
## then the "cachesolve" should retrieve the inverse from cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getInverse()
  
  ## Return the inverse as it is - if its already set
  if( !is.null(mat) ) {
    message("getting cached data")
    return(mat)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Inverse using matrix multiplication
  mat <- solve(data) %*% data
  
  ## Set inverse to the object
  x$setInverse(mat)
  
  ## Return the matrix
  mat
}
