## The following set of functions create a matrix Object and returns an inverse of the matrix. 
## However, the matrix inverse calculation is cached in case we have to repeat the calculation
## on the same matrix again.

## makeCacheMatrix builds a matrix object with a set of functions set, get, setinv and getinv.
## set - to change the matrix
## get - to get the current matrix
## setinv - to set the inverse of the matrix
## getinv - to get the inverse of the matrix
## please note this function doesn't calculate the inverse, it only holds the current inverse value.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y = matrix()) {
      x <<- y
      x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) x_inv <<- inv
    getinv <- function() x_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes the matrix object as input and calculate the inverse if its not calculated
## or returns the cached inverse if its already calculated for the same matrix.

cacheSolve <- function(x, ...) {
        
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$setinv(x_inv)
  x_inv
  
}
