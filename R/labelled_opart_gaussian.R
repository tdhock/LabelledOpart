#' compute the optimal changepoint model for a vector of real-valued data
#' and a non-negative real-valued penalty,
#' given the square loss (to minimize) / gaussian likelihood (to maximize)
#'
#' @param data A numerical vector for which the changepoint model is to be computed
#' @param labels A data frame containing information of labelled regions and breaks
#' @param penalty A non-negative real number indicating penalty parameter
#' @return A vector of the optimal cost values and a vector of the optimal segment ends
#' @export


labelled_opart_gaussian <- function(data, labels, penalty) {

  #check if given object is a data frame
  if(!is.data.frame(labels)){
    stop("input argument labelled_data must be a data frame")
  }

  #check if the given data frame has correct number of columns
  if(ncol(labels) != 3){
    stop("input argument labelled_data must be a data frame with 4 columns.
         1st and 2nd columns for start and end of labelled region, 3rd column for
         number of change points")
  }

  #check if the penalty value is numeric
  if(!is.numeric(penalty)){
    stop("penalty value should be numeric")
  }

  #check if penalty is finite and of length 1
  if(!(is.finite(penalty) && (length(penalty) == 1))){
    stop("penalty must be a finite numeric value")
  }

  #extract the start of labelled regions and put them in list
  labelled_data <- data.table::as.data.table(labels)

  result <- .C("labelled_opart_gaussian_interface",
               n_data = as.integer(length(data)),
               n_labels = as.integer(nrow(labelled_data)),
               data.vec = as.double(data),
               penalty = as.double(penalty),
               cost.vec = double(length(data) + 1),
               starts = as.integer(labelled_data[[1]]),
               ends = as.integer(labelled_data[[2]]),
               breaks = as.integer(labelled_data[[3]]),
               indicator = integer(length(data) + 1),
               zeros = integer(length(data) + 1),
               closest = integer(length(data) + 1),
               sums = double(length(data) + 1),
               dp = double(length(data) + 1),
               vt = double(length(data) + 1),
               end.vec = integer(length(data) + 1),
               positions = integer(length(data) + 1),
               vt_end = integer(length(data) + 1),
               PACKAGE="LabelledOpart")

  seg_ends <- (result$end.vec)

  #remove -2 placeholders from the output
  result$end.vec <- seg_ends[seg_ends != -2]
  #result$dp <- result$dp[2:length(dp)]
  #result$vt <- result$vt[2:length(dp)]
  result$cost.vec <- result$cost.vec[2:length(result$cost.vec)]
  result$end.vec <- result$end.vec[2:length(result$end.vec)]
  #remove the columns used for internal calculations as they don't need to be displayed
  result <- result[c("zeros", "positions", "vt_end", "dp", "vt", "cost.vec", "end.vec", "indicator", "closest")]

  #display the result
  result
}
