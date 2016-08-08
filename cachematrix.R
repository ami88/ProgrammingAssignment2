## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates the special "matrix",
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the solve
## get the value of the solve
## Basically, the only thing that I did ir replacing "mean" by "solve"
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## Calculates the mean of the special "matrix" created
## with the above function. As before, I replaced "mean" by "solve"
## Example of how it works in my Console
## > A = matrix(c(2, 4, 3, 1, 5, 7,-2,0,-4), nrow=3, ncol=3) 
## > Ainv <- makeCacheMatrix(A)
## > cacheSolve(Ainv)
## [,1] [,2]  [,3]
## [1,]  0.40 0.20 -0.20
## [2,] -0.32 0.04  0.16
## [3,] -0.26 0.22 -0.12

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}
