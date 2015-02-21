# The first function creates a special "matrix" object that can cache its inverse.

# This function creates a special "matrix" object that can cache its inverse.
# This function also allows the user to set a new matrix using the set function that is inherited through the 
# makeCacheMatrix function and also get a matrix that has been provided to the makeCahceMatrix funtion by using the
# get function also inheritied by the makeCacheMatrix funtion.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(solve) m <<- solve
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed),then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}

#This checks the code to ensure it works correctly
size = 10
x <- matrix(rnorm(size^2), nrow=size, ncol=size)
xx <- makeCacheMatrix(x)
cacheSolve(xx)
cacheSolve(xx)
xx$get() %*% cacheSolve(xx)
xx$set(matrix(rnorm(size^2), nrow=size, ncol=size))
xx$get()
cacheSolve(xx)
cacheSolve(xx)
xx$get() %*% cacheSolve(xx)

