## The first function creates a list of four functions the serve as
## as setters and getters of a matrix and its inverse.
## The second function checks if there is an inverse of the matrix 
## cached from a previous time. If there is it returns it. If there is
## not then it calculates it and returns it.

## The makeCacheMatrix() function is based on the makeVector() example function
## given in the second programming assignment of the R programming
## course on coursera.com.

makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
      set <- function(y) {
      	x <<- y
      	invx <<- NULL
      }
      get <- function() x
      setinv <- function(inv) invx <<- inv
      getinv <- function() invx
      list(set = set, get = get,
      	setinv = setinv ,
      	getinv = getinv )
}


## The cacheSolve() function is based on the cachemean() example function
## given in the second programming assignment of the R programming
## course on coursera.com.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	invx <- x$getinv()
      if(!is.null(invx)) {
      	message("getting cached data")
      	return(invx)
      }
      data <- x$get()
	## The following line is the main diference of this function and
	## the example. It solves the system represented by a square matrix
	## and a singular matrix of same dimentions. Note that the singular
	## matrix is given explicitly to the slove() function because I
	## am not yet 100% sure how R behaves with the '...' arguments.
      invx <- solve(data, diag(nrow(data)), ...)
      x$setinv(invx)
      invx
}



