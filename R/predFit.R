#' Predictions from a Fitted Model
#'
#' Generic prediction method for various types of fitted models. \code{predFit}
#' can be used to obtain standard errors of fitted values and
#' adjusted/unadjusted confidence/prediction intervals for objects of class
#' \code{"lm"}, \code{"nls"}.
#'
#' @param object An object that inherits from class \code{"lm"}, \code{"nls"}.
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict. If omitted, the fitted values are used.
#' @param se.fit A logical vaue indicating if standard errors are required.
#'   Default is \code{FALSE}.
#' @param interval Type of interval to be calculated. Can be one of "none"
#'   (default), "confidence", or "prediction". Default is \code{"none"}.
#' @param level A numeric scalar between 0 and 1 giving the confidence level for
#'   the intervals (if any) to be calculated. Default is \code{0.95}.
#' @param adjust A logical value indicating if an adjustment should be made to
#'   the critical value used in calculating the confidence interval. This is
#'   useful for when the calibration curve is to be used multiple, say k, times.
#'   Default is \code{FALSE}.
#' @param k The number times the calibration curve is to be used for computing
#'   a confidence/prediction interval. Only needed when
#'   \code{adjust = "Bonferroni"}.
#' @param ... Additional optional arguments. At present, no optional arguments
#'   are used.
#'
#' @export
#' @return  No return value (called for side effects).
#' @note predFit function is from 'investr' package written by Brandon M. Greenwell.
#'
#' @references
#' Greenwell B. M., and Schubert-Kabban, C. M. (2014)
#' \emph{investr: An R Package for Inverse Estimation}. The R Journal, 6(1), 90-100.
#'
#' @seealso  \code{\link[investr]{predFit}}

predFit <- function(object, ...) {
  UseMethod("predFit")
}

#' @rdname predFit
#' @export
predFit.default <- function(object, ...) {
  stats::predict(object, ...)
}

#' @rdname predFit
#' @export
predFit.nls <- function(object, newdata, se.fit = FALSE,
                        interval = c("none", "confidence", "prediction"),
                        level = 0.95,
                        adjust = c("none", "Bonferroni", "Scheffe"), k,
                        ...) {

  # Match arguments
  interval <- match.arg(interval)
  adjust <- match.arg(adjust)

  # Make sure se.fit is set to TRUE if intervals are requested
  compute.se.fit <- if (se.fit || (interval != "none")) {
    TRUE
  } else {
    FALSE
  }

  # No support for the Golub-Pereyra algorithm for partially linear
  # least-squares models
  if (interval != "none") {
    if (!is.null(object$call$algorithm) && object$call$algorithm == "plinear") {
      stop(paste0("The Golub-Pereyra algorithm for partially linear least-",
                  "squares models is currently not supported."), call. = FALSE)
    }
  }

  # Prediction data
  newdata <- if (missing(newdata)) {
    eval(stats::getCall(object)$data, envir = parent.frame())
  } else {
    as.data.frame(newdata)
  }
  if (is.null(newdata)) {
    stop("No data available for predictions.", call. = FALSE)
  }

  # Name of independent variable
  xname <- intersect(all.vars(stats::formula(object)[[3]]), colnames(newdata))

  # Predicted values
  pred <- object$m$predict(newdata)

  # Compute standard error
  if (compute.se.fit) {

    # Assign values to parameter names in current environment
    param.names <- names(stats::coef(object))
    for (i in 1:length(param.names)) {
      assign(param.names[i], stats::coef(object)[i])
    }

    # Assign values to independent variable name
    assign(xname, newdata[, xname])

    # Calculate gradient (numerically)
    form <- object$m$formula()
    rhs <- eval(form[[3]])
    if (is.null(attr(rhs, "gradient"))) {
      f0 <- attr(stats::numericDeriv(form[[3]], param.names), "gradient")
    } else {  # self start models should have gradient attribute
      f0 <- attr(rhs, "gradient")
    }

    # Calculate standard error
    R1 <- object$m$Rmat()
    # v0 <- diag(f0 %*% solve(t(R1) %*% R1) %*% t(f0))
    v0 <- diag(f0 %*% tcrossprod(solve(crossprod(R1)), f0))  # slightly faster
    se_fit <- sqrt(stats::sigma(object)^2 * v0)

  }

  # Compute results
  if (interval == "none") {

    # Vector of fitted/predicted values
    res <- pred

  } else {

    # Adjustment for simultaneous inference
    crit <- if (adjust == "Bonferroni") {  # Bonferroni adjustment

      stats::qt((level + 2*k - 1) / (2*k), stats::df.residual(object))

    } else if (adjust == "Scheffe") {  # Scheffe adjustment

      if (interval == "confidence") {
        p <- length(stats::coef(object))  # number of regression parameters
        # sqrt(p * stats::qf((level + 1) / 2, p, stats::df.residual(object)))
        sqrt(p * stats::qf(level, p, stats::df.residual(object)))
      } else {
        # sqrt(k * stats::qf((level + 1) / 2, k, stats::df.residual(object)))
        sqrt(k * stats::qf(level, k, stats::df.residual(object)))
      }

    } else {  # no adjustment

      stats::qt((level + 1) / 2, stats::df.residual(object))

    }

    # Interval calculations
    if (interval == "confidence") {  # confidence limits for mean response
      lwr <- pred - crit * se_fit  # lower limits
      upr <- pred + crit * se_fit  # upper limits
    } else {  # prediction limits for individual response
      lwr <- pred - crit * sqrt(stats::sigma(object)^2 + se_fit^2)  # lower limits
      upr <- pred + crit * sqrt(stats::sigma(object)^2 + se_fit^2)  # upper limits
    }

    # Store results in a matrix
    res <- cbind("fit" = pred, "lwr" = lwr, "upr" = upr)

  }

  # If standard errors of fitted values are requested, convert results to a list
  # and store additional information
  if (se.fit) {
    res <- list("fit" = if (interval != "none") res else pred, #res,
                "se.fit" = se_fit,
                "df" = stats::df.residual(object),
                "residual.scale" = stats::sigma(object))
  }

  # Return results
  return(res)

}
