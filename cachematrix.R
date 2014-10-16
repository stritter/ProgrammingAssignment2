## Put comments here that give an overall description of what your
## functions do

# Creating a matrix, Computing the inverse of the matrix, Caching the inverse of a matrix 

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		ma <- NULL
        set <- function(ma_y) {
                x <<- ma_y
                ma <<- NULL
        }
        get <- function() x
        solveinverse <- function(ma_inverse) ma <<- ma_inverse
        getinverse <- function() ma
        list(set = set, get = get,
             solveinverse = solveinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		ma <- x$getinverse()
        if(!is.null(ma)) {
                message("getting cached data")
                return(ma)
        }
        data <- x$get()
        ma <- solve(data, ...)
        x$solveinverse(ma)
        ma
        ## Return a matrix that is the inverse of 'x'
}
