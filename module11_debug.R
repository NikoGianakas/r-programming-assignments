corrected_tukey <- function(x) {
  # Defensive check
  if(!is.matrix(x) || !is.numeric(x)) stop("x must be a numeric matrix")
  
  outliers <- array(TRUE, dim = dim(x))
  
  for (j in seq_len(ncol(x))) {
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])
  }
  
  outlier.vec <- logical(nrow(x))
  for (i in seq_len(nrow(x))) {
    outlier.vec[i] <- all(outliers[i, ])
  }
  
  return(outlier.vec)
}

# enahancements:
if(!is.matrix(x) || !is.numeric(x)) stop("x must be a numeric matrix")
if(any(is.na(x))) stop("x contains missing values")
