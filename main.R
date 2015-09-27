source("cachematrix.R")

# build 3x3 test matrix
mtx <- matrix( c(1.0,2.0,3.0,2.0,1.0,2.0,3.0,2.0,1.0), nrow=3, ncol=3, byrow=TRUE)

# cache init
cm = makeCacheMatrix(mtx)

inv <- cacheSolve(cm)

# check, shall produce unit 3x3 matrix
print( round( inv %*% cm$get(), 7 ) )

# check, shall produce unit 3x3 matrix
print( round( cm$get() %*% inv, 7 ) )

