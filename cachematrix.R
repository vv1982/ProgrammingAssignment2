## The following couple of functions return the inverse of a matrix if exists.
## In case the inverse natrix is already calculated, it is just returned without
## spending computation time again. The [R] build-in function used, is 'solve'.
## Example of use:
##      1) make a matrix (e.g. mat <- makeCacheMatrix(matrix(1:9, 3)) ) 
##      2) compute the inverse of 'mat' (e.g. invnat <- cacheSolve(mat) )



## Function 'makeCacheMatrix' creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {                         ## stores the given matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x                          ## get the matrix
        setsolve <- function(solve) inv <<-  solve   ## calculate the inverse
        getsolve <- function() inv                   ## get the inverse
        list(set = set, get = get,                   ## return list of the functions
             setsolve = setsolve,
             getsolve = getsolve)       
}


## Function 'cacheSolve' computes the inverse of the special "matrix" 
## returned by 'makeCacheMatrix' above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()     ## gets the existing inverse matrix (or NULL)
        if(!is.null(m)) {     ## if inverse matrix exists, inform and return it
                message("getting cached data")     
                return(m)
        }
        data <- x$get()       ## if inverse==NULL copy the matrix
        m <- solve(data, ...) ## assert the inverse to m 
        x$setsolve(m)         ## assert m to function setsolve
        m                     ## return the inverse
}



