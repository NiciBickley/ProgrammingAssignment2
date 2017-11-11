## The makeCacheMatrix function creates a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

## This function creates the special matrix obeject  and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y){
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix
        list(set=set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)){
                message("getting cached data")
                return(inverse_matrix)
        }
        initial_matrix <- x$get()
        inverse_matrix <- solve(initial_matrix, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
        }
