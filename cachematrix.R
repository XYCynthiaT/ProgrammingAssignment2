## Use 'makeCacheMatrix()' to create a matrix and
## cache the inverse matrix, use 'cacheSolve()' to
## return the inverse matrix.

## It creates an empty matrix with 'makeCacheMatrix().
## Once the empty matrix is created, you can set and get the matrix, 
## and the inverse matrix with '.$set()','.$get()' and 
## '.$getinverse()' respectively

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matr) inverse <<- matr
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function return the inverse matrix of the 
## argument 'x'. First, it check if the inverse matrix
## of 'x' was cached. if so, it get the cached matrix,
## or it computes a new matrix and cache the results.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("Getting cached inverse matrix.")
    return(inverse)
  }
  matr <- x$get()
  inverse <- solve(matr)
  x$setinverse(inverse)
  inverse
}
