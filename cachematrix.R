## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function has a list of get and set methods for the matrix ...
## ... and its inverse and the cache variable  
## Create a makeCacheMatrix object aMatrix with input matrix mtA as follows:
## aMatrix<- makeCacheMatrix(mtA)
## aMatrix will now have all the methods listed in makeCacheMatrix and the cache object
 
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the cache as an empty placeholder object 
  ## This placeholder object will be invoked from CacheSolve function ...
  ## ... this will eventually store the inverse of the matrix
  m <- NULL
  
  ## set method to provide the input matrix
  ## when a new matrix is provided as input, it also "resets" the cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get method to invoke the input matrix
  get <- function() x
  
  ## set method for the inverse, will be called from CacheSolve
  ## Don't call CacheSolve directly from makeCacheMatrix!
  setInverse <- function(mtInverse) m <<- mtInverse
  
  ## get method for the inverse, this will provide the output of CacheSolve...
  ## ... once CacheSolve provides the inverse and stores in the cache object m
  getInverse <- function() m
  
  ## Calling makeCacheMatrix itself just provides the list of functions within it
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve provides the actual function to invert a matrix
## and checks whether the cache (object m) already has the inverted matrix, ...
## ... in which case it simply returns the cached result
## Call cacheSolve with passing the previously created aMatrix object ...
## .. to invert the matrix or invoke the cached result, as follows
## cacheSolve(aMatrix)
## Now we can call the getInverse method (from makeCacheMatrix) as follows:
## aMatrix$getInverse() to get the inverse of mtA

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## If the result is already cached, return the cached result with a message ...
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## ... otherwise get the matrix, and return the inverted matrix 
  data <- x$get()
  m <- solve(data, ...)
  
  ## Invoke the setInverse method from makeCacheMatrix to return to cache (object m) 
  x$setInverse(m)
  m
}
