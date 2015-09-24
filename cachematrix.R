## makeCacheMatrix() - creates a special "matrix" object that can cache its inverse
## cacheSolve() - function computes the inverse of the special "matrix" returned by makeCacheMatrix above



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set matrix()
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get matrix()
  get <- function() x
  ##cacheSolve()

  cacheSolve <- function(x, ...) {
      ## if 
      if ( !is.null(inv) ) {
        message("###Inverted matrix has already been calculated### \r\n ###THE RESULT IS:")
        return(inv)
      }
      ## Return a matrix that is the inverse of 'x'
      data <- x$get()
      inv <<- solve(data)
      inv
  }
  list(set = set, get = get,
       cacheSolve = cacheSolve)
}


