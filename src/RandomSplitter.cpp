#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <numeric>
#include <cmath>

using namespace Rcpp;

/**
 *@struct R_Generator
 *@brief A Uniform Random Number Generator (URNG) for C++11 algorithms.
 *
 * Bridges R's internal RNG (\code{R::unif_rand}) with C++ \code{std::shuffle}.
 * This allows the C++ logic to stay in sync with R's \code{set.seed()}.
 */
struct R_Generator {
  typedef uint32_t result_type;
  
  static constexpr result_type min() {
    return 0;
    
  }
  static constexpr result_type max() {
    return 0xFFFFFFFF;
  }
  result_type operator()() {
    // Map R's [0, 1) double to the full uint32 range
    return static_cast<result_type>(R::unif_rand() * 4294967296.0);
  }
};

/**
 * @brief Apportions a total count into integer buckets based on weights.
 *
 * Uses the Largest Remainder Method (Hare-Niemeyer) to ensure that the sum of
 * returned integers exactly equals \code{total}, even when proportions result
 * in fractional rows.
 *
 * @param total The total number of items to distribute.
 * @param weights A vector of relative weights for each bucket.
 * @return std::vector<size_t> Integer counts for each bucket.
 */
std::vector<size_t> apportion_counts(size_t total, const NumericVector& weights) {
  const int n = weights.size();
  double total_weight = std::accumulate(weights.begin(), weights.end(), 0.0);
  
  if (total_weight <= 0) stop("Sum of weights must be positive.");
  const double scale = total / total_weight;
  
  std::vector<size_t> counts(n);
  std::vector<std::pair<double, int>> remainders(n);
  size_t assigned = 0;
  
  for (int i = 0; i < n; ++i) {
    double exact = weights[i] * scale;
    counts[i] = static_cast<size_t>(std::floor(exact));
    remainders[i] = {exact - counts[i], i};
    assigned += counts[i];
  }
  
  size_t gap = total - assigned;
  if (gap > 0) {
    // Sort indices by descending fractional remainder
    std::nth_element(remainders.begin(), remainders.begin() + gap - 1, remainders.end(),
                     [](const std::pair<double, int>& a, const std::pair<double, int>& b) {
                       return a.first > b.first;
                     });
    for (size_t i = 0; i < gap; ++i) {
      counts[remainders[i].second]++;
    }
  }
  return counts;
}

/**
 * @title Randomly Split Indices
 * @description Generates a list of shuffled 1-based indices split into buckets.
 * @param n Total number of indices (1:n).
 * @param split_weights Numeric vector of proportions for each split.
 */
// [[Rcpp::export]]
List random_split_indices(int n, NumericVector split_weights) {
  if (n <= 0) return List::create();
  RNGScope scope;
  R_Generator g;
  
  IntegerVector indices = no_init(n);
  std::iota(indices.begin(), indices.end(), 1);
  std::shuffle(indices.begin(), indices.end(), g);
  
  const auto counts = apportion_counts(static_cast<size_t>(n), split_weights);
  List out(counts.size());
  
  int offset = 0;
  for (size_t i = 0; i < counts.size(); ++i) {
    IntegerVector split = no_init(counts[i]);
    std::copy(indices.begin() + offset, indices.begin() + offset + counts[i], split.begin());
    // Shuffle the final split to ensure randomness within the bucket
    std::shuffle(split.begin(), split.end(), g);
    out[i] = split;
    offset += counts[i];
  }
  return out;
}

/**
 * @title Stratified Split of Indices
 * @description Splits 1-based indices while maintaining the proportion of
 * values found in the 'labels' vector across all resulting buckets.
 * @param labels An IntegerVector representing classes or strata.
 * @param split_weights Proportions for the final splits.
 */
// [[Rcpp::export]]
List stratified_split_indices(IntegerVector labels, NumericVector split_weights) {
  int n = labels.size();
  if (n == 0) return List::create();
  
  RNGScope scope;
  R_Generator g;
  
  // Grouping indices by label for stratified processing
  std::map<int, std::vector<int>> strata;
  for (int i = 0; i < n; ++i) {
    strata[labels[i]].push_back(i + 1);
  }
  
  int num_splits = split_weights.size();
  std::vector<std::vector<int>> split_acc(num_splits);
  
  for (auto& pair : strata) {
    std::vector<int>& group = pair.second;
    std::shuffle(group.begin(), group.end(), g);
    
    const auto counts = apportion_counts(group.size(), split_weights);
    
    size_t offset = 0;
    for (int s = 0; s < num_splits; ++s) {
      for (size_t k = 0; k < counts[s]; ++k) {
        split_acc[s].push_back(group[offset + k]);
      }
      offset += counts[s];
    }
    R_CheckUserInterrupt(); // Allow user to cancel in long-running stratification
  }
  
  List out(num_splits);
  for (int s = 0; s < num_splits; ++s) {
    // Final shuffle across strata for total randomness
    std::shuffle(split_acc[s].begin(), split_acc[s].end(), g);
    out[s] = wrap(split_acc[s]);
  }
  return out;
}

/**
 * @title Optimized Sequence Sampler
 * @description Creates a random permutation of the sequence 1:n.
 * @param n Maximum value of the sequence.
 */
// [[Rcpp::export]]
IntegerVector sample_n(int n) {
  if (n < 0) stop("n cannot be negative.");
  if (n == 0) return IntegerVector(0);
  RNGScope scope;
  R_Generator g;
  IntegerVector x = no_init(n);
  std::iota(x.begin(), x.end(), 1);
  std::shuffle(x.begin(), x.end(), g);
  return x;
}