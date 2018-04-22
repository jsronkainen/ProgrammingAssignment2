#This file contains two functions makeCacheMatrix and cacheSolve. As such, it provides a user dealing with 
#substantially big matrices a way to cache calculated inverses to be used later on. 
#Its sole purpose is therefore to make the code more efficient in runtime

#To use the function, simply do the following
#1. Create a new cache object for your matrices: cacheA<-makeCacheMatrix(yourMatrix)
#2. Cache the inverse of yourMatrix: cacheSolve(cacheA)
#Note the argument of the cacheSolve function!

#This is the one which sets the cache for your matrix. 
#It actually doesn't do anything else. It is just a command center for the cache. 
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        #Variables x (for the matrix) and inv (for the inverse) have now been initialized by a function call
        
        #This is is the matrix 'setter'
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        #This is the matrix 'getter'
        get <- function() x
        
        #This is the inverse 'setter'. This just actually caches the value calculated in the function cacheSolve()
        setinv <- function(inv) i <<- inv
        
        #And this is the inverse 'getter'
        getinv <- function() i
        
        #And this is the cherry on top, which gives names to these functions and allows you to access these
        #setters and getters neatly using the dollar sign
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


#And this is the one doing the actual inverse calculation (if that is needed). 
#As said before, beaware of the argument, as the function doesn't really work with elementary matrices. 
#You must call the first function and make the caching before calling this.
cacheSolve <- function(x, ...) {
        
        #First we go and see if there is a cached inverse available for our matrix
        i <- x$getinv()
        #if there is a cache, the cached inverse is returned
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        #There is something beautiful going on here, but essentially once there is no cached inverse available
        #we must calculate it. And this is what is actually happening in the next two rows.
        data <- x$get()
        i <- solve(data, ...)
        
        #And this. I love this: As a courtesy, we pass this newly calculated inverse to the makeCacheMatrix() to be
        #cached for future use. And finally we just return the inverse. 
        x$setinv(i)
        i
        
}
