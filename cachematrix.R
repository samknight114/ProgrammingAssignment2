## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function should ostensibly take a 'cache' of a matrix, using this new operator <<- (something, something environments) 
        #It does this by through the Set and Get functions, which respectively define and assign their values. 
        #Then it assigns these functions to a list for later retrieval

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y 
                inverse <<- NULL
        }
        get <- function() {x} 
        setinverse <- function(inv) {inverse <<- inv} 
        getinverse <- function() {setinverse} 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 

}


## Write a short comment describing this function

#Again, closely emulated on the structure of the Mean example. Only significant differences are the variable names and the solve() function instead of mean()
#This function uses the above function to get the Boolean value of the list created above
        #It then checks if it is NULL, and if it is not NULL then it prints a message indicating that it is getting the data that has already been cached
        #Then it does some magic using the solve() function on the matrix and spits out the inverted matrix

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}


