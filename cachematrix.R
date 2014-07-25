## These functions provide a caching method of calculating 
## the inverse of an invertible matrix.
## Note: invertibility is assumed with no checks or error handlign.

## Function makeCacheMatrix takes a matrix as its 
## argument and constructs a list whose members are 
## functions to:
##  [i] set the value of the input matrix, 
##  [ii] get the value of the input matrix,
##  [iii] set the value of the output matrix, 
##  [iv] get the value of the output  matrix.


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Function cacheSolve takes as first argument  
## a list whose members are 
## functions to:
##  [i] set the value of the matrix, 
##  [ii] get the value of the matrix,
##  [iii] set the value of the inverse matrix, 
##  [iv] get the value of the inverse matrix.
## Note: the matrix is assumed to be invertible.
## The function checks whether the inverse matrix 
## has been calcuted and otherwise proceeds with 
## the calculation.

cacheSolve <- function(x, ...) {

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
