## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        set <- function(y) {                             #function to set a new matrix
        
                x <<- y                                  #store new data
                
                m <<- NULL                               #get new cache
        }

        get <- function() x                              #get matrix's data
        
        setinverse <- function(inverse) m <<- inverse    #set a new Matrix without calling the main function
        
        getinverse <- function() m                       #get cache
        
        list(set = set, get = get,                       #create the list
             setinverse = setinverse,
             getinverse = getinverse
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()         #query the cache of the matrix
        
        if(!is.null(m)) {
                
                message("getting cached data")
                
                return(m)           #if there is data in cache, just return it
        }
        
        data <- x$get()             #set data if it doesnt exist
        
        m <- solve(data, ...)       #calculate the inverse
        
        x$setinverse(m)             #save the result
        
        m                           ## Return a matrix that is the inverse of 'x'
}
