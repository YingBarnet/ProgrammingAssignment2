## Put comments here that give an overall description of what your
## functions do

## pass a matrix to makeCacheMatrix to save it to cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inv <<- inv
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Check if the cache has got the data. If there is, 
##     get the data from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
