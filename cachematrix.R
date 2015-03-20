## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        ## returns a list containing functions to
        ##      a) set the matrix
        ##      b) get the matrix
        ##      c) set the inverse
        ##      d) get the inverse
        
	  m <- NULL
        set <- function(y) {
        
	  ##  assign value to environment not current environment 
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



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
        
	          ## If not null (i.e. if inverse already exists)
       
                message("getting cached data")
	 
                ## Retrieve the cache
       
                return(m)
        }
        
	  ## If inverse not exist, calculate the inverse
	  data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
