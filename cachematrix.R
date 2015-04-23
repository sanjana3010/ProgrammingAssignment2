## The function makeCacheMatrix and cacheSolve work together 
## to calculate the inverse of a matrix with caching so as 
## to keep the computational time to the minimum. 
## The input is assumed to always be a square matrix


## This function returns the special cached matrix for reference in the future

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function calculates the inverse of the special vector
## It first checks if this has already been calculated by the function above
## by checking the value of the variable 'm'
##If so, it just returns the cached value; otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
