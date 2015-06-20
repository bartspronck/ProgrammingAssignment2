## R Programming Coursera Course (June 2015)
## Assignment 2 by Bart Spronck
## b.spronck@maastrichtuniversity.nl
## 
## The functions defined below allow a user to cache the results of a matrix inversion
## for subsequent use.

## makeCacheMatrix(x) returns a list of four functions:
##   set         set matrix of which the inverse will be calculated.
##   get         get matrix.
##   setInverse  set computed inverse of matrix.
##   getInverse  get stored inverse of matrix.
## x is an optional matrix argument that sets the matrix of which the inverse will be
## calculated.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL          #The inverse is not yet calculated.
  set <- function(y) { #Function for setting the matrix.
    M <<- y
    inv <<- NULL       #If a new matrix is set, remove a (previously calculated) inverse.
  }
  get <- function() M  #Function for getting the matrix.
  setInverse <- function(inverse) inv <<- inverse  #Function for setting the calculated inverse.
  getInverse <- function() inv                     #Function for getting the stored inverse.
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve(x, ...) performs matrix inversion for a matrix stored in an object x created
## by makeCacheMatrix. It takes optional arguments which are passed through to R's solve
## function.
## cacheSolve returns the inverted matrix, and stores the inverted matrix in x.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #Try to get a (previously calculated) inverse.
  if(!is.null(inv)) {
    message("Getting previously calculated inverse...")
    return(inv) #Return the previously calculated inverse.
  }
  
  #If the inverse was not yet calculated...
  M <- x$get()         #...get the matrix to be inverted.
  inv <- solve(M, ...) #Compute the inversion.
  x$setInverse(inv)    #Set the inversion result to the object x.
  inv                  #Return the inversion result.
}