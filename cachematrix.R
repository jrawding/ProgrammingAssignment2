### This code caches the potentially time consuming inverse matrix calculation, so that if the matrix is
### unchanging, the inverse matrix can be looked up rather than calculated

### NOTE: This code assumes use of square matrices, to code for a non-square matrix, insert a line in the
### makeCacheMatrix function to load the MASS package (library(MASS)) and replace solve with ginv throughout

## This function creates a list of functions to set/get the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
            set <- function(y){
                x <<- y
                m <<- NULL
            }
            get <- function() x
            setMat <- function(solve) m <<- solve
            getMat <-function() m
            list(set = set, get = get, 
                 setMat = setMat, 
                 getMat = getMat)
}


## This function checks to see if the inverse matrix has already been calculated; if yes, it gets the mean from
## the cache instead of calculating; if no, it calculates the inverse of the data and updates the cache via setMat

cacheSolve <- function(x, ...) {
        m <- x$getMat()
            if(!is.null(m)){
                message("getting cached data")
                return(m)
            }
            data <-x$get()
            m <- solve(data,...)
            x$setMat(m)
            m
}
