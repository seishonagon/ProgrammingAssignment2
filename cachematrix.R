## This function is in response to the PA2 Assignment of the R Programming class
## The file uses caching techniques to limit computation time for certain heavy operations. 
## Objects (here a matrix) are replaced by lists containing the object and certain "methods" allowing
## programmers to set and get it's value, and to set and get certain useful transformations.
## In this instance, we cache the inverse of a matrix (found through the solve function)

## The first function creates the object (a CacheMatrix). As far as I can tell, the <<- assignment
## allows the "subfunction" to "pull in" the variables from the parent 
## environment. That's probably a complete misunderstanding of the way it 
## actually works
## The function is called with the actual matrix and returns a list object that can be
## manipulated further.

makeCacheMatrix <- function(x = matrix()) {
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


## This functions accepts a CacheMatrix as an argument. If the inverse has 
## already been solved, then we know it and it's stored, so we don't need to 
## compute it. 
## otherwise, just fire up the engine and solve away. 

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
