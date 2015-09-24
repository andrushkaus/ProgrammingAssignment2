################# ASSIGNMENT_2 ###################################################
## makeCacheMatrix() - creates a special "matrix" object that can cache its inverse
## cacheSolve() - function computes the inverse of the special "matrix" returned by makeCacheMatrix above
###################################################################################

## MakeCacheMatrix() an object that consists of 2 parts: set & get. 
## NOTE: in addition to $set you can also assign matrix by doing "a <- makeCacheMatrix(y)"
makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  list(set = set, get = get)
}

##cacheSolve(x) checks inv variable is empty, if not, returns cached value. 
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



