## Programming Assignment 2 ##

## Functions below is a pair of function that cache the inversive of the matrix.


## Write a short comment describing this function
## The function, makeCacheMatrix, creats a special "matrix", which is really
##  a list containing a function to 
##     1) set the value of the matrix
##     2) get the value of the matrix
##     3) set the value of the inverse
##     4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y){
          x <<- y
          invm <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invm <<- inverse
    getinv <- function() invm
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## The following functions calculates the inverse of the special "matrix"
##  created with the above function. However, it first checks to see if the inverse
##  has already been calculated. If so, it retrives the inverse from the cache.
##  Otherwise, it calculates the inverse of the data, sets the value of mean in the
##  cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getinv()
    if(!is.null(invm)){
            message("getting cached data")
            return(invm)
    }
    data <- x$get()
    invm <- solve(data)
    x$setinv(invm)
    invm
}
