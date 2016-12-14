## 12.14.2016
## Inverting matrices is expensive. The functions below will ensure the inverse is only calculated once.
## makeCacheMatrix stores the value of the inverse in a cache
## cacheSolve returns the inverse, checking the cache first
## NOTE: assignment assumes matrix 'x' is invertible


## creates a "special" matrix which is really a list of 4 functions used as input to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize m so cache is null first time function is called
    m <- NULL
    
    ## set function enables changes to the matrix, resets cache to null
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get function returns matrix values
    get <- function() x
    
    ## setinverse function stores inverse to the cache
    setinverse <- function(inverse) m <<- inverse
    
    ## getinverse function returns the inverse (will be null if chacheinverse has not been called for matrix)
    getinverse <- function() m
    
    ## create "special" matrix: list of 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
    
}


## Calls on makeCacheMatrix. If the cache is not null, returns cache (inverse).  
## If cache is null, calculates inverse and stores in the cache.
## Input, x, is a "special" matrix created by makeCacheMatrix  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## stores cache value locally to m
    m <- x$getinverse()
    
    ## if cache is not null, return cache value (inverse of 'x') and exit cacheSolve
    ## mesage will appear if inverse came from the cache (rather than calculated)
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if the cache for inverse is null, calculate the inverse
    
    ## stores matrix locally in data
    data <- x$get()
    
    ## compute inverse.  solve(a,b,...) solves a %*% x = b. If b is ommitted, treated as identity matrix
    m <- solve(data, ...)
    
    ## store inverse in cache
    x$setinverse(m)
    
    ## return inverse of 'x'
    m
}


## check functions work???:
# matrix1 <- matrix(c(0,1,1,0),2,2)
# matrix_sp <- makeCacheMatrix(matrix1)
# cacheSolve(matrix_sp)
# cacheSolve(matrix_sp)
# matrix1 %*% matrix_sp$get
## second call to cacheSolve is to show the value came from the cache
## last line is 2x2 identity matrix