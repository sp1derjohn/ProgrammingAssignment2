## The following functions are written to satisfy the
## requirement of Programming Assignment 2 in "R Programming
## for Data Analysis". The objective of the following
## functions is to cache the inverse of a matrix.

## makeCacheMatrix will create a special "matrix" object
##  that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## create a variable 'm' to hold the inverse matrix
  m <- NULL
  
  ## set - allows us to store/cache the matrix
  set<-function(y) {
    
    ## this sets 'y' which was passed to the function as the
    ## global variable 'x'
    x<<-y
    
    ##We then re-set NULL to the global 'm'
    m <<- NULL
  }
  
  ## This 'get' function essentially returns global/cached 'x'
  get <- function() x
  
  ##setmatrix will save the matrix to the global variable m
  setmatrix <- function(mat1) m <<-mat1
  
  ##getmatrix will return the matrix
  getmatrix <- function() m
  
  ## return cached matrix object as a list
  list(set = set, get = get, 
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## cacheSolve computes the inverse of the special matrix
##  that is returned by makeCacheMatrix.  If the inverse
##  has already been calculated and the (matrix hasn't changed),
##  cacheSolve retrieves the inverse matrix from cache.
cacheSolve <- function(x, ...) {

  ## store the matrix
  m <- x$getmatrix()
  
  ## check to see if the inverse has been calculated
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## NOTE: the code below will not run if 'm' is not null
  ##  that is, nothing has changed and so the cached matrix
  ##  has already been returned out of this function
  
  ## Store the matrix in the data variable
  data <- x$get()
  
  ## Inverse Calculate the inverse of 'data'
  m <- solve(data, ...)
  ## store the inverse matrix as the new 
  x$setmatrix(m)
  
  ## return the inverse matrix
  m
}
