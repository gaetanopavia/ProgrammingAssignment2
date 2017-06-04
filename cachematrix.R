## The following functions allow to compute the inverse of a matrix and to
## avoid repeating the same computation by caching it on to memory

## The function allows caching the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

## The input is the matrix of which we want to obtain the inverse.
## The function returns a list of functions to:
## - set the matrix
## - get the matrix
## - set the inverse
## - get the inverse

  	inv = NULL
 	set = function(y) {
    		x <<- y
    		inv <<- NULL
  	}
  	get = function() x
  	setinv = function(inverse) inv <<- inverse
  	getinv = function() inv
  	list	(set = set, get = get, 
		setinv = setinv, 
		getinv=getinv)
}

## Calculates the inverse of the above matrix. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the matrix and sets the inverted matrix
## in the cache via the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  	inv = x$getinv()
    	if (!is.null(inv)) {
    
## Inverse has been already calculated: it is retrieved from the cache.
    
    		message("getting cached data")
    		return (inv)
  	}
  
## calculates the inverse of the matrix and saves data onto cache

  	mat.data = x$get()
  	inv = solve(mat.data, ...)
  	x$setinv(inv)
  
  	return(inv)
}

