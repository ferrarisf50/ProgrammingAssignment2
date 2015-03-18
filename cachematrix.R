## Put comments here that give an overall description of what your
## functions do


# This function creates a special "matrix" object that can cache its inverse.
# It returns a list containing a function to    
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}



# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
## test the function:
## > a <- matrix(c(1,3,4,5),2,2)
## > a
##      [,1] [,2]
## [1,]    1    4
## [2,]    3    5
## 
## > m_matrix <- makeCacheMatrix(a)
##
## > cacheSolve(m_matrix)
##            [,1]       [,2]
## [1,] -0.7142857  0.5714286
## [2,]  0.4285714 -0.1428571
## > m_matrix <- makeCacheMatrix(a)
## 
## > cacheSolve(m_matrix)
## getting cached data
##            [,1]       [,2]
## [1,] -0.7142857  0.5714286
## [2,]  0.4285714 -0.1428571
## >


