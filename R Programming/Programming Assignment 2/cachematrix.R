###################################################################
## Below functions give ability to invert the matrix into the cache.
## It is an important task because inversion of matrix
## is computationally expensive.
##
## @author: Patryk Kiepas (quepas)
###################################################################

## Function creates cacheable matrix object
makeCacheMatrix <- function(base_matrix = matrix()) {
  inverted_matrix <- NULL

  setMatrix <- function(new_matrix) {
    base_matrix <<- new_matrix
    inverted_matrix <<- NULL
  }
  getMatrix <- function() {
    base_matrix
  }
  setInvMatrix <- function(new_matrix) {
    inverted_matrix <<- new_matrix
  }
  getInvMatrix <- function() {
    inverted_matrix
  }

  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## Function computes inverse of the cacheable matrix (only if needed)
cacheSolve <- function(cached_matrix, ...) {
  ## Return a matrix that is the inverse of 'cached_matrix'
  inverted_matrix <- cached_matrix$getInvMatrix()

  ## If inverse matrix already calculated - return it!
  if (!is.null(inverted_matrix)) {
    message("Getting inverse matrix from cache")
    return (inverted_matrix)
  }
  ## Otherwise, compute inverse matrix
  matrix <- cached_matrix$getMatrix()
  inverted_matrix <- solve(matrix, ...)
  cached_matrix$setInvMatrix(inverted_matrix)
  inverted_matrix
}
