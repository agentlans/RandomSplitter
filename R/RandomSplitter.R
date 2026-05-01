#' @useDynLib RandomSplitter, .registration = TRUE
#' @importFrom Rcpp sourceCpp
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
  if (!is.data.frame(df))
    stop("Input must be a data.frame")
  n <- nrow(df)
  if (n <= 1)
    return(df)
  
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
  if (!is.data.frame(df))
    stop("Input must be a data.frame")
  
  if (is.null(stratify_by)) {
    idx_list <- random_split_indices(nrow(df), weights)
  } else {
    if (!stratify_by %in% names(df))
      stop("Stratification column not found.")
    labels <- as.integer(as.factor(df[[stratify_by]]))
    idx_list <- stratified_split_indices(labels, weights)
  }
  
  out <- lapply(idx_list, function(idx)
    df[idx, , drop = FALSE])
  
  if (!is.null(names) && length(names) == length(out)) {
    names(out) <- names
  } else if (is.null(names) && length(weights) == 2) {
    names(out) <- c("train", "test")
  }
  
  out
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
  split_df(
    df = df,
    weights = c(train_size, 1 - train_size),
    names = c("train", "test"),
    stratify_by = stratify_by
  )
}
