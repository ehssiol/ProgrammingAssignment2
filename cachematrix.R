## This function creates a special "matrix" object that can cache its inverse
## The function returns a list containing a function to:
##      * set the value of the vector
##      * get the value of the vector
##      * set the value of the mean
##      * get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created 
## with the above function. 
##
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
##
## Otherwise, it calculates the inverse of the matrix and sets the value of 
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data <- x$get()
     
	i <- solve(data, ...)
	x$setinverse (i)
	i
}
