## Assignment 2 - Week 2
## Function make Cache Matrix line YYY
## Function cacheSolve line XXX

## Create makeCacheMatrix function
## This function creates a list of all the functions used later in cacheSolve
makeCacheMatrix <- function(x = numeric()) {
        
        ## Set cache to default value NULL
        cache <- NULL
        
        ## Create function setMatrix
        ## Assigns the input to x stored in the main function
        setMatrix <- function(newValue) {
                x <<- newValue
                ## Reset cache to default value NULL
                cache <<- NULL
        }
        
        ## Create function getMatrix
        ## Returns the matrix from x stored in the main function
        getMatrix <- function() {
                x
        }
        
        ## Create function cacheInverse
        ## Stores inverse of a matrix in cache
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        
        ## Create function getInverse
        ## Retrieves stored value from cache
        getInverse <- function() {
                cache
        }
        
        ## Return list of generated functions for subsetting
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## The following function calculates the inverse of the matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {
        ## Read out the cached value
        inverse <- y$getInverse()
        ## Check if inverse has already been calculated (cache <> "NULL")
        ## If value is available skip calculation and go to output function
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## Otherwise get matrix, caclulate inverse and store it in cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # # Output the inverse
        inverse
}