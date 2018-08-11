## Put comments here that give an overall description of what your
## functions do

# The overall function creates a matrix, takes its inverse and stores it in cache.
# If the inverse is already stored in a cache, it just reads its from the memory.

## My function creats a list to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
            x <<- y
            Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) Inv <<- inverse
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }

## Write a short comment describing this function

# This function calculates the inverse of the special "matrix" created with
# the makeCacheMatrix function. It first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the matrix and sets 
# the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        Inv <- x$getinverse()
        if(!is.null(Inv)) {
            message("getting cached data")
            return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinverse(Inv)
        Inv
    }
