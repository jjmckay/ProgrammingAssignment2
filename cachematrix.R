## These functions allow us to build a special square matrix object which
## not only stores the matrix but also its inverse counterpart.
## A separate, standing function is used to calculate and set the inverse.

## We could have added some error checking and additional functionality, but the
## assignment instructions say "For this assignment, assume that the matrix
## supplied is always invertible." So I did.

## makeCacheMatrix is a function which creates a special matrix object.
## It stores both the passed square matrix and the inverse of that matrix.
## When assigned, the object is represented as a list, each item of which is
## its associated functions.

## Example usage of both functions:
# > m <- matrix(c(4,2,7,6), nrow=2, ncol=2) ## Assigning a matrix
# > t <- makeCacheMatrix(m) ## Building and assigning the matrix object to t
# > t$get() ## Retrieves the original passed matrix, m
# > n <- matrix(c(1,1,1,3,4,3,3,3,4), nrow=3, ncol=3) ## Making a new matrix
# > t$set(n) ## Setting a new matrix for the special matrix object, t
# > cacheSolve(t) ## Solves and stores the inverse of the matrix in t
# > t$getInv() ## Returns the stored inverse matrix we just solved for

makeCacheMatrix <- function(cache = matrix()) {
    ## Initialize a name and memory location for the inverse matrix
    inv <- NULL
    
    ## set(newMatrix) is used to set a new matrix in the object, replacing the
    ## old one
    set <- function(newMatrix) { #newMatrix is a new square matrix
        ## First we test if the new matrix is a different matrix than the old.
        isSameMatrix <- identical( unname(cache), unname(new) )
        ## If they are identical, there's no need to replace the stored matrix
        ## and blow out its stored inverse. If they are not identical, which we
        ## test for here, then we reassign the cache field to the new matrix and
        ## assign the inverse variable to NULL.
        if (!isSameMatrix) {
            cache <<- newMatrix
            inv <<- NULL
        }
    }
    ## get() simply returns the matrix stored in cache.
    get <- function() cache
    
    ## setInv(newInv) is utilized by the cacheSolve function to store the solved
    ## inverse matrix
    setInv <- function(newInv) inv <<- newInv
    
    ## getInv() returns the inverse matrix if it has been solved and cached.
    ## If not, it returns NULL
    getInv <- function() inv
    
    ## Here we build and return a list of the functions we built above.
    ## We name the using the same names as their respective functions.
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}

## The cacheSolve function is standalone and given a special matrix object made
## via the makeCacheMatrix function, it will solve its stored square matrix and
## store that inverse matrix inside the special object, too.

cacheSolve <- function(cm, ...) {
    ## First we get what is stored as the inverse by the special matrix object
    im <- cm$getInv()
    ## If the inverse is *not* NULL, which means it has been solved already,
    ## then return the cached inverse matrix stored in the special matrix object
    ## We use the return() function because it will exit this function, without
    ## proceeding to run the rest of the code after the if statement.
    if (!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    
    ## To solve the stored matrix we first assign it to a variable for easy
    ## recall
    m <- cm$get()
    
    ## And we apply the R built-in solve() function to it assuming it's square
    message("solving inverse matrix")
    im <- solve(m)
    
    ## Call the setInv() function to store the solved inverse matrix in the
    ## special matrix object
    cm$setInv(im)
    
    ## Finally, return the inverse matrix itself
    im
}
