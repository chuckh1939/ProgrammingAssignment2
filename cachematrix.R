## makeCacheMatrix takes matrix and saves inverse of matrix in cache
## cacheSolve retrieves inverse of matrix from first function if it has already been
## saved to cache, and matrix has not changed
## otherwise it recalculate the inverse, if data has changed

## function to save inverse of matrix to cache

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
makeCacheMatrix

## function to get inverse data from cache, otherwise calculates it 

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
