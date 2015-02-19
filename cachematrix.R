## Function makeCacheMatrix creates a vector of functions
## set & get for matrix
## setInverse and getInverse for inverse of a matrix
##
## Function cacheSolve
## Retrieves the inverse of the matrix from the cache,
## if it is not available in the cache then it calculates
## the inverse of the matrix and returns it


## The first function, makeCacheMatrix creates a special
## "vector", which is really a list containing functions 
## to:
## set & get the value of the matrix
## set & get the value of the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL
        set <- function(y) {
            x <<- y
            matrix_inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) matrix_inverse <<- inverse
        getInverse <- function() matrix_inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the special "vector"
## created with the above function makeCacheMatrix. However, it first
## checks to see if the inverse of the matrix has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix using the solve
## function and sets the value of the inverse in the cache via the
## setInverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting inverse data from cache")
                return (inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}

##Output:
## 
##> x=matrix(c(4,2,7,6), 2, 2)
##
##> x
##     [,1] [,2]
##[1,]    4    7
##[2,]    2    6
##
##> inv = makeCacheMatrix(x)
##
##> inv$get()
##     [,1] [,2]
##[1,]    4    7
##[2,]    2    6
##
##> cacheSolve(inv)
##     [,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
##
##> cacheSolve(inv)
##getting inverse data from cache
##     [,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
##> 

