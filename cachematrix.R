## These two function cache the inverse of a matrix

## this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    in <- NULL
    set <- function(y){
        x <<- y
        in<<- NULL
    }
    get <- function() x
    setinverse <- function(z) in<<-z
    getinverse <- function() in
    list(set=set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## this function computes the inverse of the matrix 
## it first checks if the inverse is already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(in)
    in
}
