## The two functions below cache a matrix inverse.  
## The makeCacheMatrix function defines the interface of the cached inverse matrix
## and the cacheSolve calculates the returns the inverse, either from cache or
## calculation.

##Define cache inverse matrix interface by creating a list containing functions to 
##1.set the value of the matrix
##2. get the value of the matrix, 
##3. set the value of the inverse
##4. get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    

}


## Return the inverse matrix. First check it current inverse is
## cached, if yes return it, otherwise calculate the invesre, update the
## chached value and return it.

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

