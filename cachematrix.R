## This file is in response to the PA2 Assignment of the R Programming class
## The file uses caching techniques to limit computation time for certain heavy operations. 
## Objects (here, a matrix) are replaced by lists containing the object and certain "methods" allowing
## programmers to set and get its value, and to set and get certain useful transformations.
## In this instance, we cache the inverse of a matrix (found through the solve() function)

## The makeCacheMatrix() funciton creates the object (a CacheMatrix). 
## As far as I can tell, the <<- assignment
## allows the "subfunction" to "pull in" the variables from the parent 
## environment. That's probably a complete misunderstanding of the way it 
## actually works :-)
## The function is called with the actual matrix and returns a list object that can be
## manipulated further (and that contains NULL as the cached value of the inverse)

makeCacheMatrix <- function(x = matrix()) {
      ## set a CacheMatrix object with a matrix, and methods to manipulate it programmatically
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL 
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve() function accepts a CacheMatrix as an argument. If the inverse has 
## already been solved (i.e. the cached value of the inverse is non-NULL),  
## then we just retrieve that, saving the computation time. 
## Otherwise, just fire up the engine and solve away. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
