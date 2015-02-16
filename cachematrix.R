## Put comments here that give an overall description of what your
## functions do
## These functions are part of Coursera & Johns Hopkins "R Programming Course"
## They are used in the Programming Assignment #2 that asks for 2 functions 
## working together to cache the inverse of a matrix

## makeCacheMatrix receives a matrix and creates the structure needed to
## be able to reference the functions that can manage that particular matrix

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


## cacheSolve receives an object returned by makeCacheMatrix and can
## get the inverse of the matrix that was used to create that object.
## If the inverse of that matrix was already calculated, it will return
## a cached version of that inverse matrix, instead of computing it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
