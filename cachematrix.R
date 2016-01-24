## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.
#Computing the inverse of a square matrix can be done with the solve function

a<-matrix(c(1:8), ncol=3)

makeCacheMatrix <- function(x = matrix()) {   ## takes an empty matrix as default
  inv <- NULL                                 ## resets the local variable
  #  set <- function(y) {                     ## the set function CHANGES THE VECTOR stored in the main function - NO USE HERE
  #    x <<- y
  #    m <<- NULL
  #  }
  get <- function() x                                    ## gets the MATRIX stored in the main function
  setinverse <- function(inverse) inv <<- inverse        ## stores the value of the input in a variable inv (parent env.)
  getinverse <- function() inv                           ## returns the stored value
  list(get = get,                                        ## a list of 4 functions in the function makeCacheMatrix
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {             
  inv <- x$getinverse()                     ## cacheSolve checks for 'inverse' in the cached data.
  if(!is.null(inv)) {                       ## if 'inverse' exists in memory,
    message("getting cached data")          ## it returns a message
    return(inv)                             ## and the existing value
  }
  data <- x$get()                             
  inv <- solve(data, ...)                   ## else it calculates the 'inverse' of the matrix
  x$setinverse(inv)                         ## and sets the value of the inverse in the cache via the setinverse function
  inv                                       
}
