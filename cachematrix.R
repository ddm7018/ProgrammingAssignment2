## Put comments here that give an overall description of what your
## functions do

# creates a list that set and gets the value of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initing the inverse
        
        inv <- NULL
        
        ##setter of matrix
        set  <- function(inputMatrix){
                matrix   <<- inputMatrix
                inv <<- NULL
        }
        
        ## getter of matrix
        get     <- function() x
        
        ## setter for matrix inverse
        setInv  <- function( inverse ) inv << - inverse
        
        ## getter for inverse of matrix
        getInv  <- function() inv
        ##returning the two setters and two getters methods
        list( set = set, get = get, setInv = setInv, getInv = getInv)

}

## if matrix inverse is caches returne the value, if not returns the inverse and caches
cacheSolve <- function(x, ...) {
        ## get the matrix inverse
        inverse <- x$getInv()
        ## returns the inverse if already calculated
        if ( !is.null(inverse)){
                message("already cached, retrieving...")
                return ( inverse )
                }
                
        input   <- x$get()
        ## matrix calculation 
        inverse <- solve(input)
        ## setting the cache 
        x$setInv(inverse)
        ## returing the inverse matrix
        inverse
}
