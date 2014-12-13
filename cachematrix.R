## Computing matrix inversion is often costly. Hence it will be useful to have a functionality that can cache the matrix inverse, where the same inverse is going to be reused many time during the execution of the application. In this task we will write a pair of functions that cache the inverse of a matrix.
## Assumption: The input matrix is invertible.


## This function creates a special "matrix" object, which can;
## Set the matrix object
## Get the matrix object
## Set the matrix inverse
## Get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Creates a special "matrix" object that can cache its inverse.
    xInv <- NULL
    setMat <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    getMat <- function() x
    setMatInv <- function(inv) xInv <<- inv
    getMatInv <- function() xInv
    list(setMat = setMat, getMat = getMat,
         setMatInv = setMatInv,
         getMatInv = getMatInv)

}

# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix`. If the inverse has
# already been calculated (and the matrix has not changed), then
# This function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getMatInv()
    if(!is.null(m)) {
        message("getting cached matrix inverse")
        return(m)
    }
    data <- x$getMat()
    m <- solve(data) # Compute matrix inverse
    x$setMatInv(m)
    m 
}

#a <- matrix(c(2, 3, 5, 7, 11, 13, 17, 19, 23), nrow = 3, ncol = 3)
#b <- solve(a)
#b %*% a
#a <- makeCacheMatrix(matrix(c(2, 3, 5, 7, 11, 13, 17, 19, 23), nrow = 3, ncol = 3))
#b <- a$getMatInv()
#b
#a$getMat()
#b <- cacheSolve(a)
#b
#a$getMatInv()
#b %*% a$getMat()
#a$getMatInv()
#cacheSolve(a)
