#' @useDynLib RandomSplitter, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import checkmate
NULL

#' Randomly Shuffle Data Frame Rows
#'
#' @description
#' Reorders the rows of a data frame randomly. This is useful for removing any
#' pre-existing ordering bias before performing cross-validation or splitting.
#'
#' @param df A `data.frame` to shuffle.
#'
#' @return A `data.frame` with the same dimensions as `df` but with rows
#'   in a randomized order.
#'
#' @examples
#' # Shuffle the built-in iris dataset
#' shuffled_iris <- shuffle_df(iris)
#' head(shuffled_iris)
#' @export
shuffle_df <- function(df) {
  # Validate input type
  checkmate::assert_data_frame(df)
  
  n <- nrow(df)
  
  # Handle edge cases (empty or single-row DF)
  if (n <= 1) return(df)
  
  indices <- sample_n(n)
  df[indices, , drop = FALSE]
}

#' Split a Data Frame into Multiple Subsets
#'
#' @description
#' Divides a data frame into multiple parts (e.g., train, validation, test)
#' based on provided weights. Supports both simple random splitting and
#' stratified splitting to maintain class balances.
#'
#' @param df A `data.frame` to split.
#' @param weights A numeric vector of weights summing to 1 (e.g., `c(0.7, 0.2, 0.1)`).
#'   If the sum is not 1, weights are normalized automatically.
#' @param names Optional character vector of names for the resulting list.
#'   Defaults to `c("train", "test")` if two weights are provided and names are NULL.
#' @param stratify_by Optional string. The name of the column to use for
#'   stratified splitting (typically a categorical target variable).
#'
#' @return A named `list` of `data.frame` objects.
#'
#' @details
#' This function utilizes underlying Rcpp implementations (`random_split_indices`
#' or `stratified_split_indices`) for high-performance index generation.
#'
#' @examples
#' # 3-way split: 70% Train, 20% Val, 10% Test
#' sets <- split_df(iris, weights = c(0.7, 0.2, 0.1), names = c("tr", "val", "te"))
#' lapply(sets, nrow)
#'
#' # Stratified split to maintain Species proportions
#' stratified_sets <- split_df(iris, weights = c(0.8, 0.2), stratify_by = "Species")
#' @export
split_df <- function(df,
                     weights = c(0.8, 0.2),
                     names = NULL,
                     stratify_by = NULL) {
  
  # 1. Data Frame Validation
  checkmate::assert_data_frame(df, min.rows = 1)
  
  # 2. Weights Validation
  # Ensures weights are numeric, non-negative, finite, and at least length 1
  checkmate::assert_numeric(weights, lower = 0, any.missing = FALSE, 
                            min.len = 1, finite = TRUE)
  
  # Ensure the sum is greater than 0 to avoid division by zero
  if (sum(weights) <= 0) {
    stop("The sum of 'weights' must be greater than 0.", call. = FALSE)
  }
  
  # Normalize weights if they don't sum to 1
  if (abs(sum(weights) - 1) > .Machine$double.eps^0.5) {
    weights <- weights / sum(weights)
  }

  # 3. Stratification Logic
  if (is.null(stratify_by)) {
    idx_list <- random_split_indices(nrow(df), weights)
  } else {
    # Check if column exists
    checkmate::assert_string(stratify_by)
    checkmate::assert_names(stratify_by, subset.of = names(df))
    
    if (any(is.na(df[[stratify_by]]))) {
      warning("Stratification column contains NAs. These will be treated as a distinct level.")
    }
    
    labels <- as.integer(as.factor(df[[stratify_by]]))
    idx_list <- stratified_split_indices(labels, weights)
  }
  
  # 4. Subset Generation
  out <- lapply(idx_list, function(idx) df[idx, , drop = FALSE])
  
  # 5. Naming Logic
  if (!is.null(names)) {
    checkmate::assert_character(names, len = length(weights), any.missing = FALSE)
    names(out) <- names
  } else if (length(weights) == 2) {
    names(out) <- c("train", "test")
  }
  
  return(out)
}

#' Fast Train/Test Split Wrapper
#'
#' @description
#' A convenience wrapper around \code{\link{split_df}} for the most common
#' machine learning use case: partitioning data into a training set and a testing set.
#'
#' @param df A `data.frame` to split.
#' @param train_size Numeric. The proportion of data to be assigned to the
#'   training set (between 0 and 1). Default is 0.75.
#' @param stratify_by Optional string. The name of the column to use for
#'   stratified splitting.
#'
#' @return A named `list` containing two `data.frame` objects: `train` and `test`.
#'
#' @seealso \code{\link{split_df}}
#'
#' @examples
#' # Simple 80/20 split
#' split_data <- train_test_split(mtcars, train_size = 0.8)
#' train_set <- split_data$train
#' test_set  <- split_data$test
#' @export
train_test_split <- function(df,
                             train_size = 0.75,
                             stratify_by = NULL) {
  
  # Specific validation for train_size
  checkmate::assert_number(train_size, lower = 0, upper = 1, finite = TRUE)
  
  # Checkmate 'lower' and 'upper' are inclusive; we check exclusivity here
  if (train_size <= 0 || train_size >= 1) {
    stop("'train_size' must be between 0 and 1 (exclusive).", call. = FALSE)
  }
  
  split_df(
    df = df,
    weights = c(train_size, 1 - train_size),
    names = c("train", "test"),
    stratify_by = stratify_by
  )
}
