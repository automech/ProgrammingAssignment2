## The program has two functions makecachematrix and cache solve
## make cachematrix creates a list of 4 functions - set,get,setinv and getinv
## cachesolve computes the inverse if it has not been computed before and saves it using setinv and if it was computed before, it pulls the value from the cache


## make cachematrix creates a list of 4 functions - set,get,setinv and getinv
## set - sets the data
## get - pulls the data
## setinv - sets the inverse matrix
## getinv- pulls the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv<- NULL
        
        set <- function(y)
        {
                x<<- y
                inv= NULL
        }
        
        get<- function() x
        setinv <- function(inverse) inv<<- inverse
        getinv<- function() inv
        
        list(set=set,get=get,setinv=setinv,getinv=getinv)
        
}




## cachesolve gets the variable inv first.
## if its null, it computes the inverse of the matrix, stores using setinv and prints the inverse
## if its not null, it pulls the inverse from cahce using getinv

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
        
        ## Return a matrix that is the inverse of 'x'
}
