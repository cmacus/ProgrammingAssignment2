## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# A list of four function to construct a matrix, get and set inverse, and list all attribs
# Much like a C++ class

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv= setInv, getInv = getInv)
}


## Write a short comment describing this function
## Takes a 'special' matrix and fetches it  inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInv(inv)
  inv
}
