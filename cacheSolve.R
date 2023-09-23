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
