## These functions are used to solve and store the inverse of
## a given matrix. Storing the inverse allows for faster computation time
## since the value can be pulled from a variable instead of being computed
## everytime it is called.

## The makeCacheMatrix function is used to create an object which both stores
## and returns values of the original vector and its inverse.

makeCacheMatrix <- function(orig_mat = matrix()) {
	inv_mat <- NULL
	set <- function(orig_copy) {
		orig_mat <<- orig_copy
		inv_mat <<- NULL
	}
	get <- function() orig_mat
	setinverse <- function(inverse) inv_mat <<- inverse
	getinverse <- function() inv_mat
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function solves the inverse of the original matrix, but
## first checks to see if the inverse has already been solved for.
## If so, it just gets the previously solved inverse matrix result
## and returns it. If not, it solves for the inverse matrix and 
## stores it, then returns the solved inverse matrix.

cacheSolve <- function(orig_mat, ...) {
	inv_mat <- orig_mat$getinverse()
	if(!is.null(inv_mat)) {
		message ("Getting Cached Data")
		return(inv_mat)
	}
	data <- orig_mat$get()
	inv_mat <- solve(data,...)
	orig_mat$setinverse(inv_mat)
	inv_mat
}
