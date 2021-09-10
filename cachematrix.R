## Put comments here that give an overall description of what your
## functions do

##Creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        g<-NULL
        set<-function(y) {
                x<<-y
                g<--NULL
        }
        get<-function()x
        setInverse<-function(solve) g<<-solve
        getInverse<-function() g
        list(set = set, get = get,setInverse=setInverse,getInverse=getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## if the inverse has already been calculated (and the matrix hasn't changed), 
## the inverse will be retrieved from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        g<-x$solve()
        if(!is.null(g)) {
                message("getting cached data")
                return(g)
        }
        data<-x$get()
        g<-solve(data,...)
        x$setInverse(g)
        g
}
