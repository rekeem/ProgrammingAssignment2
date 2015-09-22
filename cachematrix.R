## These functions invert a given matrix while caching the result 
## in order to save computation time.

## makeCacheMatrix takes a matrix and converts it into a list of functions 
## which cacheSolve can then act upon

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve takes the output list from makeCacheMatrix and returns the inverse of the original 
## matrix by retrieving it from the cache if possible or computing the inverse if not

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
