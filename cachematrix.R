## Overall goals of the follow functions are to:
## a) become familiar with the superassignment operator <<-
## b) understand variable assignment in local environment and global environment
## c) perform function and store result in global environment for later access
## d) furthermore, reason to store values to save memory/time

## makeCacheMatrix allows user to create a matrix while also solving/inversing
## the matrix and storing in global environment to call later

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve returns inverse of supplied matrix but more importantly
## returns stored inverse if previously calculated by checking x$getInverse
## if x$getInverse is null it pulls in inverse
## otherwise it alerts user it's pulling cached data/inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
