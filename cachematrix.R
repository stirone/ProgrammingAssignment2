##
## cachematrix.R
##
## Two functions which aid in performance when using the inverse of a matrix, 
##  by caching the inverse and returning it from the cache, if available.
##
## The two functions are:
##
## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse. 
##
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve the
## inverse from the cache.
##
## Example Usage:
##
##   a <- makeCacheMatrix() ## create the "special matrix object"
##   m <- matrix(1:4,2,2)   ## create a matrix to solve for its inverse
##   a$set(m)               ## set the value of the special matrix object's matrix
##   cacheSolve(a)          ## return the matrix's inverse, possibly from the cache
##


## This is the function that creates a special "matrix" object that can cache
## its inverse.  
makeCacheMatrix <- function(x = matrix()) {

    ## initialize the value of the cache
    i <- NULL

    ## set the value of the matrix to be solved for its inverse
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    ## get the value of the matrix to be solved for its inverse
    get <- function() {
      x
    }

    ## set the value of the inverse (cache it)
    setinverse <- function(inverse) {
      i <<- inverse
    }
    
    ## return the value of the inverse (from the cached value)
    getinverse <- function() {
      i
    }

    ## set the list function return values
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve retrieves the inverse from the
## cache.
cacheSolve <- function(x) {

  ## see if we have the inverse already
  m <- x$getinverse()
  
  ## if not null, we have the inverse, 
  ##  so just return that to the caller.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)   ## return it to the caller
  }
  
  ## inverse is not cached, so calculate it.
  ## first get the matrix to be solved...
  data <- x$get()
  ## ...now get its inverse via solve.
  m <- solve(data)
  
  ## save it, for later retrieval
  x$setinverse(m)

  ## and finally, return it to the caller
  m  
    
}

## end of file
