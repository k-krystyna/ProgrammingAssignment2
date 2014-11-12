## These functions are able to cache inverse matrix

## Function makeCacheMatrix creates a special "vector", which is a list containing function to 
## - set/get value of a matrix (functions set/get)
## - set/get value of the inverse matrix (functions setinverse/getinverse)

makeCacheMatrix <- function(x = matrix()) {
    
    # cache is empty
    m <- NULL 
    
    # store a matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # return matrix
    get <- function() x
    
    # cache matrix
    setinverse <- function(solve) m <<- solve
    
    # get cached matrix
    getinverse <- function() m
    
    # return list of function
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve computes, caches and returns inverse matrix

cacheSolve <- function(x, ...) {
    
    # return invers matrix
    m <- x$getinverse()
    
    # check if cached value exists and return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # otherwise get the matrix...
    data <- x$get()
    
    # ...get inverse of it...
    m <- solve(data, ...)
    
    # ... and store in cache
    x$setinverse(m)
    
    # return inverse matrix
    m
}

## TEST

#amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#amatrix$get()
#cacheSolve(amatrix)
#cacheSolve(amatrix)
#amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
#cacheSolve(amatrix)
#amatrix$get()
#amatrix$getinverse()

