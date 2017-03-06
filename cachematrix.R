## This function creates a special vector to do the following:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. set the value of the inverse
## 4. Get the value of the inverse
##
## This function was greatly leveraged off of the example
## provided in the instructions for the 2nd programming
## assignment in the R course

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function calculates the inverse of a matrix created
## using the makeCacheMatrix() function. If the inverse has 
## not been calculated, it will calculate it and cache the 
## value. If it has been calculated and cached, it will
## return the cached value instead of recalculating it.


cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
