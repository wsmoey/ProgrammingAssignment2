
## This function Creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() {
    x
  }
  
  setinvr  <- function(invr){
    
  i  <<- invr
  }
  
  getinvr  <- function() {
    i
  }
  
  list(set= set, get = get, 
       setinvr = setinvr, 
       getinr = getinvr)
  
}


## Calculate the inverse and if the inverse calculated (and the matrix has not changed),
## then function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  i  <- x$getinverse()
  
  if (!is.null(i)){
    message("caching data")
    return(i)
  }
  
  data  <- x$get()
  
  i  <- solve(data, ...)
  
  x$setinverse(i)
  
  i
}
