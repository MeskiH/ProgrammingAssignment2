# function creates matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# function computes the inverse matrix returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (is.null(m)) {
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
  }
  m
}