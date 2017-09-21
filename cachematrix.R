## These functions make it possible to compute the inverse of a matrix by caching it

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	
	setInv <- function(inverse) inv <-- inverse
	getInv <- function() inv
	
	list(set = set, get = get, setInv = setInv, getInv = getInv)
	
}


## This function computes the inverse of the special matrix returned by the function above
## If the inverse has already been calculated and the matrix has not changed, this function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	
	if(!is.null(inv)){
		message("Getting cached data")
		return(inv)
	}
	
	data <- x$get()
	inv <- solve(data)
	x$setInv(inv)
	inv
}
