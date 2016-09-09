## This two functions cache the inverse of a matrix


## makeCacheMatrix creates a "matrix" object that can cache its inverse,
##it is a list containning a function to: (1) set the matrix, 
##(2)get te matrix, (3) set the Inverse of the marix and (4) get the 
##inverse marix 


makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        
        set<-function(y=matrix,...){
                x<<-y
                i<<-NULL
        }
        
        get<-function()x
        setinv<-function(inv) i<<-inv
        getinv<-function()i
        list(set=set,get=get,setinv=setinv,getinv=getinv)
        

}


## cacheSolve calculates the inverse of the special "matrix" created
## with the above function. It checks to see if the inverse matrix has 
## alredy been calculated. If so, it gets the inverse matrix from the 
## cache an skips the computation, if not, it calculates the inverse
## matrix and sets it in the cache via te setinv function.


cacheSolve <- function(x, ...) {
        
        i<-x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matr<-x$get()
        i<-solve(matr,...)
        x$setinv(i)
        i
}
