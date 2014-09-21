##Aiming to reduce the computational cost of matrix inversion, 
##these functions are used to cache the inverse of a matrix.

##The following function receives as input one parameter matrix
##and creates a list containing a function that: set the value of
##the matrix, get the value of the matrix, set the value of the inverse
##of the matrix, get the value of inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(x) {
    m <<- x
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##The following function returns the inverse of the matrix. 
##First by checking if the inverse has been computed.
##If it is not calculated, it computes the inverse and puts 
##the value cached via function setinverse

cacheSolve <- function(m, ...) {
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data)
  m$setinverse(inv)
  inv
}

