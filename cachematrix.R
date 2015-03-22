## Matrix inversion is a resource intensive computation and
## rather than compute it each time, we will instead try to
## mitigate the extra time spent by caching the inverse for
## future use, rather than solving it again and again.

## This function takes a matrix as an input
## and assigns several methods to it. We will use
## use these methods in the next function.

## We initially set up the inverse to default to NULL
## and x to be our matrix
## When we call setInverse, we change the value from NULL
## to an inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	
	inverse <- NULL

	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	get <- function() {
		x
	}
	setInverse <- function(inv) {
		inverse <<- inv
	}
	getInverse <- function() {
		inverse
	}

	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## cacheSolve attempts to see if there is a non-NULL value
## for the inverse variable, and if so, we return that. If not,
## we continue on to calculate the inverse and cache it for that matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
        	message("Getting cached data.")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
