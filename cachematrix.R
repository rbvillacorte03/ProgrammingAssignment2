## This file contain a functions on Module 2: R Programming
## caching inverse of a matrix

## makeCacheMatrix and cacheSolve are functions needed to create a special "matrix" object that
## can cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function()x
        setinv<function(inverse)inv<<-inverse
        getinv<function()inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## cacheSolve is a function that computes the inverse of the special "matrix"
##returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached result")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}
