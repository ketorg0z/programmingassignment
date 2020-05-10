##These functions make the inverse matrix
##makeCacheMatrix makes an invertible matrix
##cacheSolve checks if the inverse matrix already exists and if it is not - gives us
##the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

##Some examples:

##> x <- cbind(c(1, 2), c(3, 4))
##> x
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> m <- makeCacheMatrix(x)
##> cacheSolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.53

##> x <- cbind(c(2, 6, 5), c(5, 3, -2), c(7, 4, -3))
##> x
##[,1] [,2] [,3]
##[1,]    2    5    7
##[2,]    6    3    4
##[3,]    5   -2   -3
##> m <- makeCacheMatrix(x)
##> cacheSolve(m)
##[,1] [,2] [,3]
##[1,]    1   -1    1
##[2,]  -38   41  -34
##[3,]   27  -29   24

##> cacheSolve(m)
##getting cached data
##[,1] [,2] [,3]
##[1,]    1   -1    1
##[2,]  -38   41  -34
##[3,]   27  -29   24