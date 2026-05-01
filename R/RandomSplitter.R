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
  # Validate input type
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data.frame or tibble.", call. = FALSE)
  }
  
  n <- nrow(df)
  
  # Handle edge cases (empty or single-row DF)
  if (n <= 1) return(df)
  
  # Ensure sample_n (presumably your Rcpp function) handles the indices
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
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data.frame.", call. = FALSE)
  }
  
  n_rows <- nrow(df)
  if (n_rows == 0) {
    stop("Input 'df' has 0 rows. Cannot split an empty data frame.", call. = FALSE)
  }

  # 2. Weights Validation
  if (!is.numeric(weights) || any(is.na(weights)) || any(weights < 0)) {
    stop("'weights' must be a non-negative numeric vector without NAs.", call. = FALSE)
  }
  
  if (sum(weights) <= 0) {
    stop("The sum of 'weights' must be greater than 0.", call. = FALSE)
  }
  
  # Normalize weights if they don't sum to 1
  if (abs(sum(weights) - 1) > .Machine$double.eps^0.5) {
    weights <- weights / sum(weights)
  }

  # 3. Stratification Logic
  if (is.null(stratify_by)) {
    idx_list <- random_split_indices(n_rows, weights)
  } else {
    if (!(stratify_by %in% names(df))) {
      stop(sprintf("Column '%s' not found in the data frame.", stratify_by), call. = FALSE)
    }
    
    # Ensure stratification column doesn't have NAs (common cause of Rcpp crashes)
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
    if (length(names) != length(weights)) {
      stop("Length of 'names' must match the length of 'weights'.", call. = FALSE)
    }
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
  if (!is.numeric(train_size) || length(train_size) != 1) {
    stop("'train_size' must be a single numeric value.", call. = FALSE)
  }
  
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
