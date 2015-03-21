
## Takes the matrix and return list of functions to get matrix, to get its inverse, and to set both. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Check for cached value, and calculate only if there is no one.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

## You can use following example to test the code:
##
## exampleMatrix <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
## matrixVector <- makeCacheMatrix(exampleMatrix)
## matrixVector$getinverse()
## matrixVector$get()
## 
## cacheSolve(matrixVector)
## cacheSolve(matrixVector)
##
## In the second call cacheSolve() function will use a cached value

