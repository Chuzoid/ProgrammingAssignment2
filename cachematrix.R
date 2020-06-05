## These functions creates and caches the inverse of a given matrix,
## while retrieving the inverse of the matrix if already solved and 
#in the cache

## a function that sets and gets a matrix and also sets and gets
#the inverse of the matrix. it stores all in a list.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        Set <- function(y) {
                x <<- y
                i <<- NULL
        }
        Get <- function() x
        SetInverse <- function(inverse) i <<- inverse
        GetInverse <- function() i
        #store the above functions in a list
        list(Set = Set, 
             Get = Get,
             SetInverse = SetInverse,
             GetInverse = GetInverse)
}


## a function to solve a matrix, only if not found in 
#the cache of solved matrices

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$GetInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$Get()
        i <- solve(data, ...)
        x$SetInverse(i)
        i
}
