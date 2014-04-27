## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  ## makeCacheMatrix returns a matrix object 
  ## which can cache its inverse
  ## determine whether the import is a square matrix
  
  if((!is.matrix(x)) || (ncol(x)!=nrow(x))){
    stop("x must be a square matrix")
  }  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x‘
  ## return the inverse of a squared matrix
  ## check for cached value first
  ## check if data from the object is a matrix
  
  if((!is.matrix(x$get()))||(ncol(x$get())!=nrow(x$get()))){
    stop("x must be a square matrix")
  }      
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
