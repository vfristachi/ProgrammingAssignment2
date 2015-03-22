makeCacheMatrix <- function(x = matrix()) {

# This function will cache a matrix and store the results 

	m <- NULL
      set <- function(y) {
      	x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
      setinverse = setinverse ,
      getinverse = getinverse )

}


cacheSolve <- function(x, ...) {

# If the inverse has been cached, it will be returned, 
# otherwise the matrix will be solved and the inverse will be stored
# Return a matrix that is the inverse of 'x'

 	m <- x$getinverse()
      if(!is.null(m)) {
		if (identical(x$setinverse(solve),x$getinverse())) {
      		message("getting cached data")
            	return(m)
		}
      }
      
   	data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}