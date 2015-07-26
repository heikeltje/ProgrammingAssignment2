## makeCacheMatrix and cacheSolve can be used to cache the inverse of a matrix. 
## The usage of the functions is as follows; 
##   y <- makeCacheMatrix( x )   creates cache matrix from ordinary matrix x
##   y$get()                     returns original matrix
##   y$set(x1)                   resets y to new matrix x1
##   y$getCache()                returns cached value
##   y$setCache(val)             sets cached value to val
##
##   cacheSolve(y)               given cache matrix y, return it's inverse, 
##                               using cached value if set.

## makeCacheMatrix creates a matrix that can cache its own inverse
## input: a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCache <- function(val) m <<- val
  getCache <- function() m
  list(set = set, get = get, setCache = setCache,
       getCache = getCache)
  
}


## cacheSolve returns the inverse of a cache matrix. If the inverse is 
## already cached, it returns the cache, otherwise it calculates the inverse
## using solve().
## input: a cache Matrix created using makeCacheMatrix
## output: the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getCache()
  if (!is.null(m)) {
    message("getting cashed data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setCache(m)
  m
}
