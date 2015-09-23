## This pair of functions support caching of matrix inversion
## which can be an expensive operation.
##
## Usage: create the helper list object, then pass it to the cacheSolve function
## e.g.
## mcm <- makeCacheMatrix(matrix(c(2,4,3,1,5,8,9,9,9), nrow=3, ncol=3))
## inverted <- cacheSolve(mcm)
##   see calculating message
## inverted2 <- cacheSolve(mcm)
##   see retrieving cached message
## inverted equals inverted2

## This is a function that returns of list of functions which
## together support caching the inversion of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## a function to store a matrix, set the inversion to null (haven't calculated it yet)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  ## an accessor function for the original matrix
  get <- function() x
  
  ## a function to store a computed inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## an accessor function for the computed inverse
  getinverse <- function() i
  
  ## return the list of the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function performs matrix inversion using the caching
## functions created by makeCacheMtrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("no cached value, so calculating inverse")
  data <- x$get()
  i <- solve(data)
  
  ## remember the result so that it can be reused
  x$setinverse(i)
  
  ## return the actual inverted matrix
  i
}
