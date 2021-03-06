## These functions are part of Coursera & Johns Hopkins "R Programming Course"
## They are used in the Programming Assignment #2 that asks for 2 functions 
## working together to cache the inverse of a matrix.
## For example, they can be used like this
## > m<-matrix(1:4,nrow=2,ncol=2)
## > z<-makeCacheMatrix(m)
## > i<-cacheSolve(z)
## > m
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > i
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 

## makeCacheMatrix receives a matrix and returns a list of functions that 
## live in the environment created in this execution of makeCacheMatrix.
## The functions in the returned list can access the following variables
## that are local to that environment:
## The "x" variable contains a matrix that is served as the parameter for
## makeCacheMatrix.
## The "s" variable may contain:
##   * a NULL value (default): Indicating that the inverse of the matrix was 
##     never cached.
##   * a matrix (inverse of x): It only gets calculated if (later) the
##     cacheSolve function was called using the object returned by
##     the current execution of makeCacheMatrix
## "x" and "s" are only available from this function environment,
## though the function returns a list containing a group of functions that
## exist in the same environment as makeCacheMatrix, so through these functions
## you can access indirectly to "x" and "s"
makeCacheMatrix <- function(x = matrix()) {
  ## We initialize "s" with NULL in order to make clear that in the current
  ## environment the inverse of "x" was never calculated.
  s <- NULL

  ## Now we define the functions that are available from the returned object:
  ## "set" is only available from this function (makeCacheMatrix) environment
  ## (or from the list that makeCacheMatrix returns).
  ## "set" will set (or change) the matrix element, 
  ## while reseting to NULL the "s" variable.
  ## The <<- operand is used to assign a value to a variable in the 
  ## calling environment (x and s are local in the "makeCacheMatrix" function,
  ## they are not local in the "set" function)
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## "get" is only available from this function (makeCacheMatrix) environment
  ## (or from the list that makeCacheMatrix returns).
  ## "get" will return the matrix element contained in "x"
  get <- function() x

  ## "setsolve" is only available from this function (makeCacheMatrix) environment
  ## (or from the list that makeCacheMatrix returns).
  ## "setsolve" will store it's "solve" argument in "s".
  ## Keep in mind that the "solve" argument is expected to be the inverse
  ## of the matrix
  ## The <<- operand is used to assign a value to a variable in the 
  ## calling environment (s is local in the "makeCacheMatrix" function,
  ## it's not local in the "setsolve" function)
  setsolve <- function(solve) s <<- solve
  
  ## "getsolve" is only available from this function (makeCacheMatrix) environment
  ## (or from the list that makeCacheMatrix returns).
  ## "getsolve" will return the "s" variable, that is expected to be NULL
  ## or the inverse of the "x" variable (matrix element)
  getsolve <- function() s
  
  ## We return a list with the 4 declared functions, so that you
  ## can call them from the outside.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve receives an object returned by makeCacheMatrix and can
## get the inverse of the matrix that was used to create that object.
## If the inverse of that matrix was already calculated, it will return
## a cached version of that inverse matrix, instead of computing it again.
## The ... parameters (if included) are passed to the "solve" function 
## as additional parameters (only in the inverse of thematrix was not 
## previously cached for "x").

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## We call the "getsolve" function in the "x" list to get the "s" variable
  ## that lives in the environment created for "x". We store the result
  ## in a local "s" variable.
  s <- x$getsolve()
  
  ## If "s" is not NULL, it means that the inverse of the matrix was 
  ## previously calculated (and cached). In that case we simply return "s".
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## If we get to this point, "s" must be NULL, meaning that the inverse
  ## of the matrix was never calculated.
  ## In that case we call the "get" function in the "x" list to get the
  ## "x" variable that lives in the environment created for "x" (it's not
  ## our local "x"!!! So we store it in a local variable called "data").
  data <- x$get()
  ## We call "solve" to calculate the inverse of the matrix that was used 
  ## when we initially called makeCacheMatrix (that matrix is now in our
  ## local "data" variable). The ... parameters are optional in the calling
  ## to cacheSolve, and they are included in the call to "solve".
  s <- solve(data, ...)
  ## As we calculated the inverse of the matrix, we use the "setsolve" 
  ## function in the "x" list, so that inverse matrix is stored in the 
  ## "s" variable that lives in the environmet created for "x".
  ## Note that the "s" variable is local, so this step needs 
  ## to be done in order to store it in the environment created for "x".
  x$setsolve(s)
  
  ## Finally, we return our local "s", that has the inverse of the matrix.
  s
}
