
## The two functions - makeCacheMatrix and cacheSolve defined below help Cache inverse of a matrix. 
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather 
## than computing it repeatedly.
## makeCacheMatrix function creates a special matrix object that can cache its inverse. Here are the steps the funciton follows
## 1-Set the value of a matrix
## 2-get the value of the matrix
## 3- set the value of the inverse
## 4- get the  value of the inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
	set <- function(y) {
		   x <<- y
		    m <<- NULL
		  }
		   get <- function() x
		   setinverse <- function(inverse) m <<- inverse
		   getinverse <- function() m
		    list(set = set, get = get,
		    setinverse = setinverse,
		    getinverse = getinverse)
}

## cacheSolve - This function computes the inverse of the special matrix returned bymake CacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed) then cacheSolve 
## should retrieve the inverse from the cache and skips computation. Otherwise, it calculates the 
## mean of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
m <- x$getinverse()
	if(!is.null(m)) {
		  message ("getting cached data")
		  return(m)
		  }
		 data <- x$get()
		 m <- solve(data)
		 x$setinverse(m)
		 m
        ## Return a matrix that is the inverse of 'x'
}
