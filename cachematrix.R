## User inputs a matrix (E.g. makeCacheMatrix(matrix(2:5,2,2)) ).
## Program check if prior calculation has been done on this set of matrix.
## If calculation has been done before, program will print the cached matrix.
## If not, progrm will calculate, store the value and print the value.


## Input the matrix, pass it to cacheSolve for calculation.
## Calculated value is stored.
Oldx<-matrix
makeCacheMatrix <- function(x = matrix()) {
        Inv<-cacheSolve(x)
        
         Inv <<- Inv
print(Inv)
        
}


## Check if prior calculation has been carried out on this set of matrix.
## Will retrive cached calculation if so,
## If not, calculation be carried out

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-identical(Oldx,x)
        if(i){
                message("getting cached data")
                return(Inv)
        }
        Inv<-solve(x)
        Oldx<<-x
        Inv
}

