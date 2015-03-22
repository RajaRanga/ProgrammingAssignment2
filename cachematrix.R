## makeCacheMatrix operates on the following data:
##  x - a matrix that persists across multiple calls from the calling environment
##  inv - the inverse of x that persists across multiple calls
## 
## and returns a list of the following functions to operate on the data
##
##  set(y) assigns matrix y to x
##  get() retrieves matrix x
##  setinv(inv_in) assigns inv_in to inv
##  getinv() retrieves inv
##
## makeCacheMatrix returns a list of the above functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL ## the new x no longer has a valid inverse
  }
  get <- function() x
  setinv <- function(inv_in) inv <<- inv_in
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
  

## cacheSolve takes as input the matrices and 
## list of access functions provided by makeCacheMatrix
##
## checks if there is a cached inverse to the matrix and returns that if there is
## 
## else it computes the inverse of the matrix
## and caches it in the makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cached_inv <- x$getinv()
  if(!is.null(cached_inv)) { # return cached inv if exists
    message("getting cached data")
    return(cached_inv) 
  }
  data <- x$get()
  new_inv <- solve(data, ...) #compute new inverse
  x$setinv(new_inv)
  new_inv
  
}
