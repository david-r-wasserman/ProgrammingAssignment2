## This assignment demonstrates the use of <<- to cache the
## result of a computation. Specifically, makeCacheMatrix() 
## creates a "matrix". cacheSolve() computes its inverse and 
## also causes the "matrix" to remember the inverse. If you 
## call cacheSolve() a second time, it returns the stored value 
## instead of recomputing the inverse.
##
## I could have done this assignment by making just a few small
## changes to the examples makeVector() and cachemean(). I didn't
## do that because makeVector() makes it possible to store an
## incorrect mean; see 
## https://www.coursera.org/learn/r-programming/discussions/forums/_ZerTCj2EeaZ8Apto8QB_w/threads/KcTH39VAEe-3kxJxSS6pBw
## for details.

## makeCacheMatrix(mat) takes a matrix as input, and outputs
## an object that stores the matrix, and also stores the
## inverse when it has been computed.
## Strictly speaking, the object is a list of four functions,
## and the matrix and the inverse aren't stored in the object.
## The execution of makeCacheMatrix() creates an environment in
## which mat and cachedInverse are local variables. The four
## functions are defined in that environment, so they have
## access to those local variables even after makeCacheMatrix()
## exits.

makeCacheMatrix <- function(mat = matrix()) {
        cachedInverse <- NULL
        setMatrix <- function(newMat) {
                mat <<- newMat
                cachedInverse <<- NULL
        }
        getMatrix <- function() mat
        computeInverse <- function(...) cachedInverse <<- solve(mat, ...)
        getInverse <- function() cachedInverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             computeInverse = computeInverse,
             getInverse = getInverse)
}


## cacheSolve(cacheMatrix) returns the inverse of cacheMatrix.
## If the inverse is already stored, it returns the stored value.
## If the inverse isn't already stored, it computes and stores it.

cacheSolve <- function(cacheMatrix, ...) {
        tempInverse <- cacheMatrix$getInverse()
        if(!is.null(tempInverse)) {
                message("getting cached data")
                return(tempInverse)
        }
        cacheMatrix$computeInverse(...)
		return(cacheMatrix$getInverse())
}