#' Define a theme for the plots
#'
#' @return a ggplot2 theme
#'
#' @export
theme_tufte <- function()
{
  ret <- ggplot2::theme_bw(base_family = "sans", base_size = 11) +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.key        = ggplot2::element_blank(),
      panel.background  = ggplot2::element_blank(),
      panel.border      = ggplot2::element_blank(),
      strip.background  = ggplot2::element_blank(),
      plot.background   = ggplot2::element_blank(),
      axis.line         = ggplot2::element_blank(),
    )
  ret
}

#' Create a beta-binomial model
#'
#' @param data Numeric vector of 1s or 0s if as_obs is TRUE or a vector with
#' length 2 if the summed successes and failures are reported.
#' @param alpha The alpha value of the prior
#' @param beta The beta value of the prior
#' @param as_obs A boolean indicating whether the data is summed or not
#'
#' @return The model object
#'
#' @export
betabin <- function(data, alpha, beta, as_obs = TRUE){

  if (any(is.null(alpha) & is.null(beta))) {
    stop("both alpha and beta args must be non-null")
  } else if (!any(is.numeric(alpha) & is.numeric(beta))) {
    stop("arguments must be numeric")
  }

  if (as_obs) {
    num_heads = sum(data == 1)
    num_tails = sum(data == 0)
  } else {
    num_heads = data[1]
    num_tails = data[2]
  }

  thetas = seq(0, 1, 0.005)

  # P(theta)
  prior = dbeta(thetas, alpha, beta)

  # P(data | theta)
  likelihood = thetas**num_heads * (1 - thetas)**num_tails

  # P(data)
  prob_data = sum(likelihood * prior)

  posterior = (likelihood * prior) / prob_data

  model <- list(prior = prior, likelihood = likelihood,
                posterior = posterior, alpha = alpha,
                beta = beta, thetas = thetas, n = num_heads + num_tails,
                num_heads = num_heads, num_tails = num_tails)

  class(model) <- "betabin"
  model
}

#' Calculate a Bayesian Estimate
#'
#' @param object Object of class betabin
#'
#' @return A Bayesian point estimator of theta
#'
#' @export
bayes_estimate <- function(object) {
  a <- object$alpha + object$num_heads
  b <- object$beta + object$n - object$num_heads

  a / (a + b)
}

#' Calculate an MLE Estimate
#'
#' @param object Object of class betabin
#'
#' @return An MLE point estimator of theta
#'
#' @export
mle_estimate <- function(object) {
  object$num_heads / object$n
}

#' Plot the distributions
#'
#' @param object Object of class betabin
#'
#' @export
plot.betabin <- function(object) {
  library(ggplot2)
  library(ggpubr)

  p_bayes <- bayes_estimate(object)

  theme_set(theme_tufte())

  prior_p <- ggplot() +
    geom_line(aes(x = object$thetas, y = object$prior), color = "red") +
    geom_area(aes(x = object$thetas, y = object$prior), fill = "pink") +
    labs(title = "Prior", x = "", y = "")

  likelihood_p <- ggplot() +
    geom_line(aes(x = object$thetas, y = object$likelihood), color = "darkgreen") +
    geom_area(aes(x = object$thetas, y = object$likelihood), fill = "lightgreen") +
    ylim(0, NA) +
    labs(title = "Likelihood", x = "", y = "")

  posterior_p <- ggplot() +
    geom_line(aes(x = object$thetas, y = object$posterior), color = "blue") +
    geom_area(aes(x = object$thetas, y = object$posterior), fill = "lightblue") +
    geom_vline(aes(xintercept = p_bayes)) +
    labs(title = "Posterior", x = "theta values", y = "")

  ggarrange(prior_p, likelihood_p, posterior_p, ncol = 1, nrow = 3, common.legend = TRUE)
}

#' Calculate credible interval for Bayesian estimator
#'
#' @param object Object of class betabin
#' @param ci.level Confidence level
#'
#' @return Lower and upper bound on the credible interval
#'
#' @export
ci <- function(object, ci.level = 0.05) {

  ci_upper <- 1 - (ci.level / 2)
  ci_lower <- ci.level / 2

  a_post <- object$alpha + object$num_heads
  b_post <- object$beta + object$n - object$num_heads

  round(quantile(rbeta(n = object$n, a_post, b_post), probs = c(ci_lower, ci_upper)), 4)

}

#' Summarize the model
#'
#' @param object Object of class betabin
#' @param ci.show A boolean indicating whether credible interval is shown
#' @param ci.level Confidence level
#'
#' @export
summary.betabin <- function(object, ci.show = TRUE, ci.level = 0.1) {
  cat("--------- Beta-Binomial Model Summary ---------\n")
  cat("Inputs:\n\t")
  cat(paste0("Prior alpha: ", object$alpha),
      paste0("Prior beta: ", object$beta),
      paste0("Number of observations: ", object$n),
      paste0("Number of successes: ", object$num_heads),
      paste0("Number of failures: ", object$num_tails),
      sep = "\n\t")
  cat("\n")
  cat("Posterior (Beta):\n\t")
  cat(paste0("Posterior alpha: ", object$alpha + object$num_heads),
      paste0("Posterior beta: ", object$beta + object$n - object$num_heads),
      sep = "\n\t")
  cat("\n")
  cat("Bayes Point Estimator of Theta:\n")

  if (ci.show) {
    ci_bayes <- ci(object, ci.level)
    cat(paste0("Point Esimator: ", round(bayes_estimate(object), 4)),
        paste0(ci.level * 100 ,"% Credible Interval:\t"),
        sep = "\n")
    print(ci_bayes)
  } else {
    cat(paste0("Point Esimator: ", round(bayes_estimate(object), 4)))
  }

  cat("\n")
  cat("MLE Point Estimator of Theta:\n")
  cat(paste0("Point Esimator: ", round(mle_estimate(object), 4)))

  cat("\n\n------------------------------------------------")
}
