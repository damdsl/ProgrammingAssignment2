
## makeCacheMatrix caches a matrix in memory
## then cacheSolve looks if the inverse of this specific matrix has been cached. 
##      If yes it returns the inversed matrix
##      If not, it computes the inverse of the matrix and returns it



#  Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setInversedMatrix <- function(solve) m <<- solve
        getInversedMatrix <- function() m
        list(set = set, get = get,
             setInversedMatrix = setInversedMatrix,
             getInversedMatrix = getInversedMatrix)
}


#  Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#  If the inverse has already been calculated (and the matrix has not changed), 
#  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInversedMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInversedMatrix(m)
        m 
}
