### makeCacheMatrix creates a matrix object
### cacheSolve calculates the inverse of the matrix.
### If the inverse has been calculated 
### it will it in the cache and return it, instead of calculating it again


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,getinverse = getinverse)

}

### cacheSolve returns the inverse of the matrix. 
### It checks if the inverse has already been computed. 
### If so, it returns the inverse and skips the computation. 
### If not, it computes the inverse, sets the value in the cache using setinverse function.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

### example run
###> 
###> ax<-matrix(c(1,2,3,4,23,56,12,9,10),3,3)
###> 
###> a<-makeCacheMatrix(ax)
###> 
###> a$get()
###     [,1] [,2] [,3]
###[1,]    1    4   12
###[2,]    2   23    9
###[3,]    3   56   10
###> 
###> cacheSolve(a)
###            [,1]       [,2]        [,3]
###[1,] -1.01481481  2.3407407 -0.88888889
###[2,]  0.02592593 -0.0962963  0.05555556
###[3,]  0.15925926 -0.1629630  0.05555556


