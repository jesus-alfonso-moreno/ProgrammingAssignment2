## The following two functions are part of the second programming assignment for
## Coursera's R programming

## The following functions take advantage of the lexical scoping principles of
## R language, to cache the inverse of matrix. Such functionality allows to avoid
## repeating the same operation over and over.

##This function creates an object that contains the following properties and methods
### x - original matrix, the only input of the function
### mSolve - contains the inverse matrix of x
### get - function returns original matrix
### setSolve - function overrides variable mSolve
### getSolve - returns value of mSolve (inverse matrix)
### set - function overrides original matrix. 
######Waring: function "set" will erase cached inverse matrix (if any) 
makeCacheMatrix <- function(x = matrix()) {
    # initializes values of inverse matrix as null
    mSolve <- NULL
    set <- function(y) {
        
        # overrides value of original matrix with "y".
        x <<- y
        # since there's a new matrix, then mSolve must be initialized to null
        mSolve <<- NULL
    }
    
    get <- function() x
    # overrides value of mSolve with user input mNewSolve
    setsolve <- function(mNewSolve) mSolve <<- mNewSolve
    getsolve <- function() mSolve
    
    # creates list to access functions of the object
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## Returns an inverse matrix of object x (input variable).
## if inverse matrix has already been calculated, then the
## return value is the cached value
## else, inverse matrix is calculated and returned.

cacheSolve <- function(x, ...) {
        
    # gets known inverse matrix
    mSolve <- x$getsolve()
    # verifies if the inverse has already been calculated
    if(!is.null(mSolve)) {
        # if variable is different from null, then it's value is returned
        message("getting cached inverse Matrix")
        return(mSolve)
    }
    # if mSolve is null, then inverse matrix is calculated and then returned
    data <- x$get()
    message("Calculating inverse Matrix")
    mSolve <- solve(data, ...)
    x$setsolve(mSolve)
    mSolve    

}
