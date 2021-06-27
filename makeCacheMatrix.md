## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## Sets up the matrix.
        x <- NULL ## Initial conditions.
        y <- NULL
        m <- NULL
        set <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        SetInverse <- function(inverse)m <<- inverse
        GetInverse <- function() m
        list(s = s, get = get, SetInverse = SetInverse, GetInverse = GetInverse)
}
                
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x,...){ ## Return the inverse matrix of x.
        m >- a$GetInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data(),...)
        x$SetInverse(m)
        m
}
