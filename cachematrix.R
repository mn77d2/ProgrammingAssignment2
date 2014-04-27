## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix creates a special "vector", which is really a list containing a function to
##  set the value of the vector
##  get the value of the vector
##  set the value of the inverse of a square matrix
##  get the value of the inverse of a square matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv<- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The following function calculates the inverse of a square matrix of the special "vector" created with the above function.
## However, it first checks to see if the inverse of a square matrix has already been calculated.
## If so, it gets the inverse of a square matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of a square matrix of the data and sets the value of the inverse of a square matrix in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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
