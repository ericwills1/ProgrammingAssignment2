## creates inversion of a matrix, by creating a special vector

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inversion) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## takes the output of makeCacheMatrix and returns the inverse of the original matrix input into makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
}