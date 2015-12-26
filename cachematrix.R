## makeCacheMatrix creates a list of functions to get and set matrix and inverse as mentioned in the assignment.
## cacheSolve generates the inverse of matrix. If already created then recalls from cache.

## Create list of functions

makeCacheMatrix <- function(x = matrix()) {
        m<-matrix()
        set<-function(y)
        {
                x<<-y
                m<<-matrix()
        }
        get<-function() x
        setinv<-function(inv) m<<-inv
        getinv<-function() m
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Solve and display inverse of matrix or recall from cache if already done

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinv()
        k<-is.na(m)
        d<-colSums(k)
        l<-sum(d)
        if (l==0)
        {
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinv(m)
        m
}
