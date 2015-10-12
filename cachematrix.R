## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of a matrix rather than computing it repeatedly 
## This  assignment is to write a pair of functions that cache the inverse of a matrix - 
## This has been cloned from example of makeVector and cacheMean

## Func makeCacheMatrix creates a special matrix
## Func makes use of special matrix to return the inverse from cache
## If the Inverse is not already in cache it stores it in cache.

## Usage: z <- makeCacheMatrix( matrix( c(2,3,2,1,2,1,1,1,2), 3, 3) ) 
## Creates a matrix - assumption is that it can be inverted
## cacheSolve returns the Inverse by using one of the 4 functions in makeCacheMatrix
## Usage: cacheSolve(z) where z is the special matrix created by makeCacheMatrix

makeCacheMatrix <- function(mat = matrix()) {
  

  inv_mat <- NULL

  
  setmat <- function(m) {
      mat <<- m
      inv_mat <<- NULL
  }
  
  
  getmat <- function() mat
  
  
  setinv <- function(m) inv_mat <<- m
  
  
  getinv <- function() inv_mat
  
  
  list (setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_mat <- x$getinv()
  
  if( !is.null(inv_mat)) {
          message("getting cached value of inverse matrix")
          return(inv_mat)
  }
  
  m <- x$getmat()
  inv_mat <- solve(m)
  x$setinv(inv_mat)
  
  inv_mat
  
  
}

