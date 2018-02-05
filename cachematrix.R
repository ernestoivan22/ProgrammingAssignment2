## functions to create a custom matrix object and generate the inverse of said matrix object 

## this function creates the special matrix object
## it was based on the example function 'makeVector'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function returns the inverse of the matrix if it was already saved,
## else it saves it
## it was based on the example function 'cachemean'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse of matrix")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}
