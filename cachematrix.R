## The overal function is to cache the results of caiculating inverse matrix. 
## For the first time of calculating a inverse matrix, R will cache the result. 
## Then, when we want to use the inverse matrix again, this function will get the values 
## from cache instead of calculating it again. 


## Creat set, get, setinverse, and get inverse functions. Using the scooping rules and the ervironment 
## of each function to cache the original matrix and inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
        if(!is.matrix(x)) {message("what you enter is not a matrix")} # to warn the input data type 
        m <- NULL
        set <- function(y) {    
                x <<- y
                m <<- NULL
        } ## modify 'x' and 'm' in the parent enviroment,
          ## 'set' fucntion can be used to input a new matrix
        get <- function() x
        setinverse <- function(inverse) m <<- inverse ## to modify 'm' in the parent enviroment
        getinverse <- function() m 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) ## to enable us call each fucntion using $

}

## cacheSolve is to get the cached values from the 'makeCacheMatrix' marix 
## and check if having already beening calculated
## if yes, just get and display the value from cache 
## if no, calculate the inverse and cache it in 'makeCacheMatrix' environment

cacheSolve <- function(x, ...) {
        m <- x$getinverse() #get the cached value and make sure that the x must go through makeCacheMatrix 
        if(is.null(m)) {
                data <- x$get() # to get the data from 'makeCacheMatrix' environment 
                m <- solve(data, ...)
                x$setinverse(m) # return the calculation result to 
                                # 'makeCacheMatrix' environment and cache it
                m
        } else {
               message("getting cached data")
        return(m) 
        }
}

