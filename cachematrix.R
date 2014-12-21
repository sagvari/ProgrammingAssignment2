## makeCacheMatrix will be the first function. X will be a matrix
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    ## m will be the inverse and it's reset to NULL every 
    ## time the function is called
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x ## returns the value of the original vector
    setmatrix<-function(solve) m<<- solve ## stores the value of the inverse matrix using superassignment, 
    ## it's called during the first cacheSolve access  
    getmatrix<-function() m ## returns the cached value to cacheSolve on subsequent access
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix) ## list of internal functions
}

## cacheSolve will be the second function. X is an object created by makeCacheMatrix
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix() ## accessing object X and the inverse matrix
    if(!is.null(m)){ ## if there is already a cached mean (= it's not NULL)
        message("Getting cached data") ## a message is sent...
        return(m) ## ... and the inverse matrix is returned
    }
    ## This part of the code will be executed when x$getsolve is NULL (=there is no cached result))
    matrix<-x$get() ## calculating inverse of the matrix 
    m<-solve(matrix, ...) ## and storing the calculated values in x 
    x$setmatrix(m)  
    m ## returning the inverse vector
}
