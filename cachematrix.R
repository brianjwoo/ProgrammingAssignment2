## Put comments here that give an overall description of what your
## functions do

## This function takes as input a matrix that can be assigned to a variable. When initialized, m, which represents the cached varaiable,
## is undefined (NULL). It is calculated when the cacheSolve function is called. makeCache returns a list containing 4 functions which
## can be used to retrive the original matrix or the cached solution.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inverse <- function(cache) m <<- cache
  get.inverse <- function() m
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

## When called on an instance of makeCacheMatrix, the function will look to see if the solution has previously been computed and cached.
## If it exists, then it will be returned. Otherwise, the function will solve the matrix and save it by calling the set.inverse function.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$get.inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set.inverse(m)
  m
}
