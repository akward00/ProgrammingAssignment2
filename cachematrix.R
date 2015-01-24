## These functions implement a CacheMatrix.   A CacheMatrix is a 
## matrix that maintains a cache of its inverse to avoid recomputation.
##
## the makeCacheMatrix() function will create a CacheMatrix.
## the cacheSolve() function will return the inverse of the CacheMatrix.



## This function will take a matrix and return a CacheMatrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	set <- function(y){
		x <<- y
		inv <<- NULL
	}

	get <- function() x
	setinv <- function(pInverse) inv <<- pInverse
	getinv <- function() inv
	list(set = set, get = get, 
		setinv = setinv, 
		getinv = getinv)			
}



## This function calculates the inverse of the CacheMatrix
## If the inverse has already been calculated it will return the cached value
## Otherwise it will compute it and cache it for future use.
cacheSolve <- function(x, ...) {
        
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
		
	message("Calling Solve")
	data <- x$get()
	inv=solve(data, ...)
	x$setinv(inv)

	return(inv)
}
