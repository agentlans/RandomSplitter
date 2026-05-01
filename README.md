# RandomSplitter 🎲

`RandomSplitter` is a high-performance R package designed for efficient data partitioning. Built with an **Rcpp** backbone, it provides lightning-fast row shuffling and data splitting, supporting both simple random sampling and stratified splitting for machine learning workflows.

## 🚀 Features

*   **Fast Shuffling**: Quickly reorder data frames to eliminate ordering bias.
*   **Flexible Splitting**: Partition data into any number of subsets (e.g., Train/Validation/Test).
*   **Stratified Sampling**: Maintain class proportions across splits—critical for imbalanced datasets.
*   **High Performance**: Core indexing logic is implemented in C++ via Rcpp.

## 📦 Installation

You can install the development version of `RandomSplitter` from GitHub (requires `devtools` or `remotes`):

```R
# install.packages("devtools")
devtools::install_github("agentlans/RandomSplitter")
```

## 🛠 Usage

### 1. Shuffling Data
Remove any inherent sequence or bias in your dataset before processing.

```R
library(RandomSplitter)

# Randomly reorder the iris dataset
shuffled_iris <- shuffle_df(iris)
head(shuffled_iris)
```

### 2. Basic Train/Test Split
The most common use case: splitting data into two parts.

```R
# Default 75/25 split
split_data <- train_test_split(mtcars, train_size = 0.8)

train_set <- split_data$train
test_set  <- split_data$test
```

### 3. Multi-way & Stratified Splitting
For more complex workflows, use `split_df`. You can define custom weights and ensure your target variable is represented proportionally across all sets.

```R
# 3-way split: 70% Train, 20% Val, 10% Test
sets <- split_df(
  iris, 
  weights = c(0.7, 0.2, 0.1), 
  names = c("train", "val", "test")
)

# Stratified split based on the "Species" column
# This ensures each set has the correct ratio of Setosa, Versicolor, and Virginica
stratified_sets <- split_df(
  iris, 
  weights = c(0.8, 0.2), 
  stratify_by = "Species"
)
```

## 📋 Function Overview

| Function | Description |
| :--- | :--- |
| `shuffle_df(df)` | Randomly reorders rows of a data frame. |
| `split_df(df, weights, names, stratify_by)` | Splits a data frame into $N$ subsets based on weights. |
| `train_test_split(df, train_size, stratify_by)` | A convenience wrapper for standard 2-way partitioning. |

## ⚙️ How it Works

`RandomSplitter` leverages **Rcpp** to handle the heavy lifting:
1.  **Index Generation**: The C++ layer generates shuffled indices using efficient sampling algorithms.
2.  **Stratification**: When `stratify_by` is used, the package groups rows by label and performs sampling within each group to preserve distribution, then merges the indices.
3.  **Memory Efficiency**: By working with row indices before subsetting the data frame in R, it minimizes memory overhead.

## Licence

GNU General Public License v3
