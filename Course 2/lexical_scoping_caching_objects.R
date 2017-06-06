##########################################################################################
# PART 1: CACHING THE MEAN OF A VECTOR
##########################################################################################

#-----------------------------------------------------------------------------------------
# FUNCTION makeVector
# creates a special "vector", which is really a list containing a function to:
# - set the value of the vector
# - get the value of the vector
# - set the value of the mean
# - get the value of the mean
#-----------------------------------------------------------------------------------------
makeVector <- function(x = numeric()) {
        m <- NULL
        # - set the value of the vector
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # - set the value of the vector
        get <- function() x
        # - set the value of the mean
        setmean <- function(mean) m <<- mean
        # - get the value of the mean
        getmean <- function() m
        
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

#-----------------------------------------------------------------------------------------
# FUNCTION 
# calculates the mean of the special "vector" created with the above function
#-----------------------------------------------------------------------------------------
cachemean <- function(x, ...) {
        
        # first checks to see if the mean has already been calculated
        m <- x$getmean()
        # If so, it gets the mean from the cache and skips the computation
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}



##########################################################################################
# PART 2: CACHING THE INVERSE OF A MATRIX
##########################################################################################
require(MASS)

#-----------------------------------------------------------------------------------------
# FUNCTION makeCacheMatrix
# creates a special "matrix", which is really a list containing a function to:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the solve
# - get the value of the solve
#-----------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # - set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # - set the value of the matrix
        get <- function() x
        # - set the value of the solve
        setsolve <- function(solve) m <<- solve
        # - get the value of the solve
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



#-----------------------------------------------------------------------------------------
# FUNCTION cacheSolve
# Return a matrix that is the solve of 'x'
#-----------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        # first checks to see if the solve has already been calculated
        m <- x$getsolve()
        # If so, it gets the solve from the cache and skips the computation
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Otherwise, it calculates the solve of the data and sets the value of the solve in the cache via the setsolve function
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}