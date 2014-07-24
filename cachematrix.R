## To avoid expensive computations the two functions below cache the inverse
## of a matrix

## makeCacheMatrix returns the functions set and get matrix, set and get
## the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
      	x <<- y
            m <<- NULL
      }
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created by makeCacheMAtrix 
## function if not cached already. Otherwise retrieves the stored one

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
                message("retrieving cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
