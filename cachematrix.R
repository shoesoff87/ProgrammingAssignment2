## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function stores arguments to pass on to the cacheSolve() function. 
# The argument "i" is initially set to NULL, but after the first run of the cacheSolve() function, 
# "i" will be set to the inverse of the matrix via the setinverse() function within, 
# and this can later be recalled by cacheSolve() using getinverse()

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# This function first checks whether the inverse has already been calculated by calling x$getinverse() 
# and storing it in "i".

# On the first run of cacheSolve(),"i" will be NULL, and so it gets the matrix via x$get() 
# and stores it in data. It then solves for the inverse and stores this result in "i" which is 
# then passed back to the makeCacheMatrix() result via x$setinverse(i), and the result is printed"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() # tries to get any cached inverse result from previous calculation
        if(!is.null(i)) { # if the result is NOT NULL, print "getting cached data"
                message("getting cached data")
                return(i) # then print out the cached solved matrix
        }
        data <- x$get() # if i was NULL, get the matrix
        i <- solve(data, ...) # then solve the matrix and store it in i
        x$setinverse(i) # store the solved matrix into the makeCacheMatrix object /  next time cacheSolve()                          runs it can then use x$getinverse() to get the inverse from cache
        i # print out the calculated solved matrix
}

# running the code
mm <- matrix(runif(100),10)
nm <- makeCacheMatrix(mm)
cacheSolve(nm)
cacheSolve(nm)
