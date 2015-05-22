## Programming Assignment 2
## Returns inverse of an invertible matrix, using a cached version if available

makeCacheMatrix <- function(x = matrix()) {
## Creates a list of functions
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setIM <- function(IM) m <<- IM
   getIM <- function() m
   
   list(set = set, get = get, setIM = setIM, getIM = getIM)
}

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getIM()
    if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
    data <- x$get()
    m <- solve(data, ...)
    x$setIM(m)
}
