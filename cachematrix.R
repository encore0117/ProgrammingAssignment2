## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creats a special matrix which is a list
## containing the function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
                }
        get<-function()x
        setinv<-function(inv) i<<-inv
        getinv<-function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calculates the inverse of the special matrix
## created with the above function. However, it firstly check 
## if the inverse of the matrix has been stroed in the cache. 
## If so, it will simply return the cached data. Otherwise,
## it will calculate the inverse and set the inverse in the cache.
cacheSolve <- function(x, ...) {
        i<-x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
                }
        data<-x$get()
        i<-solve(data,...)
        x$setinv(i)
        i
}
