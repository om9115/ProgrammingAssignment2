## Functions to cache the inverse of a matrix:
## 1) makeCacheMatrix: function to creates a special "matrix" object 
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  ## Initializing the inverse property
  m<-NULL
  
  ## to set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ##to  get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  ##to set the matrix inverse
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ##to get the matrix inverse
  getInverse <- function() {
    ## Return the inverse property
    m
  }
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## 2) cacheSolve: computes the inverse of the special "matrix".
## If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m      
}
