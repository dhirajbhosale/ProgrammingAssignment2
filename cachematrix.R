# Using this, the following allows caching the
# inverse of the matrix and returning it from
# cached value rather than computing it again

#----------------------------------------------------
# Creates a special matrix type from a given matrix x
# with handle functions get, set, getinv, setinv that
# encapsulates the access to the underlying matrix and
# the cached inverse matrix.
#----------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
		# When called, empty cache containing inverse:
		    invm <- NULL
		    # Sets the matrix to the given value and cleans cache:
		    set <- function(y) {
			x <<- y
			invm <<- NULL
		    }
		    # Returns the Matrix:
		    get <- function() x
		    # Sets the cached invm value to the given value
		    setinv <- function(inverse) invm <<- inverse
		    # Returns the cached value:
		    getinv <- function() invm
		    # List of the handle functions:
		    list(set = set, get=get, setinv=setinv, getinv=getinv)
}


#------------------------------------------------------------
# Returns the inverse (invm) of a special "matrix" type (x)
# created by makeCacheMatrix either from the cached value 
# or from a computed value if the cach is empty (invm = NULL) 
#-------------------------------------------------------------

cacheSolve <- function(x, ...) {
        # Attempt to get inverse from cached value:
        invm <- x$getinv()
        if (!is.null(invm)) {
            message("getting cached data")
            return(invm)
        }
        # If cached value is empty (NULL) compute the inverse:
        mtrx <- x$get()
        message("Computing inverse, this can take a while...")
        invm <- solve(mtrx,...)
        # Set the cache to the current value
        x$setinv(invm)
        invm        
}
