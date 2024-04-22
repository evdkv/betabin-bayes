# Beta Binomial Model
This is an R package for estimating probability for binomial data with a beta prior

## Usage
Data needs to be passed as a vector of 1s (successes) or 0s (failures).
Additionally, it can be passed as a vector in a cummarized bersion with 
number of successed in the first position and the number of failures in
the second.

Example 1:

```r
model <-- betabin(data, alpha = 1, beta = 3)
```

Example 2:

```r
model <-- betabin(c(23, 78), alpha = 1, beta = 3, as_obs = FALSE)
```

## Summary of Functions

Below is a summary of the functions available in the package.

```r
# Create a model
betabin(data, alpha, beta, as_obs=TRUE)

# Get a model summary
summary(obj, ci.show = TRUE, ci.level = 0.1)

# Plot the prior, likelihood, and posterior
plot(obj)

# Get the Bayesian estimator for the model
bayes_estimate(obj)

# Get the MLE estimate for the model
mle_estimate(obj)

# Get the credible interval for the Bayesian estimator
ci(obj, ci.level = 0.05)
```