## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## 1.set the value of the matrix 
## 2.get the value of the matrix 
## 3.set the inverse of the matrix 
## 4.get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        InvMat <- NULL
        set <- function(y) {
                x <<- y
                InvMat <<- NULL
        }
        get <- function() x
        SetInverse <- function(Inverse) InvMat <<- Inverse
        GetInverse <- function() InvMat
        list(set = set, get = get, SetInverse = SetInverse, GetInverse = GetInverse)
        }        
        


## Write a short comment describing this function

## The following calculates the inverse of the "matrix" created with makeCacheMatrix 
## However first it checks to see if the inverse has already been calculated. 
## If it has, it gets the inverse from the cache and skips the calculation. 
## Otherwise, it calculates the inverse  and sets it in the cache with the SetInverse function.

cacheSolve <- function(x, ...) {
        InvMat <- x$GetInverse()
        if(!is.null(InvMat)) {
                message("getting cached data")
                return(InvMat)
        }
        data <- x$get()
        InvMat <- solve(data)
        x$SetInverse(InvMat)
        InvMat
        }        
        
        ## Returns a matrix that is the inverse of 'x'
