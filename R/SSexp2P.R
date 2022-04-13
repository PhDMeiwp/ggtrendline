#' Self-Starting Nls 'exp2P' Regression Model
#'
#' This selfStart model evaluates the power regression function (formula as: y=a*exp(b*x)). It has an initial attribute that will evaluate initial estimates of the parameters 'a' and 'b' for a given set of data.
#'
#' @usage SSexp2P(predictor, a, b)
#' @param predictor  a numeric vector of values at which to evaluate the model.
#' @param a,b The numeric parameters responding to the exp2P model.
#' @export
#' @examples
#' library(ggtrendline)
#' x<-1:5
#' y<-c(2,4,8,20,25)
#' xy<-data.frame(x,y)
#' getInitial(y ~ SSexp2P(x,a,b), data = xy)
#' ## Initial values are in fact the converged values
#'
#' fitexp2P <- nls(y~SSexp2P(x,a,b), data=xy)
#' summary(fitexp2P)
#'
#' prediction <- predFit(fitexp2P , data.frame(x=x), se.fit = TRUE,
#'                           level = 0.95, interval = "confidence")
#' yfitexp2P <- prediction$fit
#' yfitexp2P  # output a matrix of predictions and bounds with column names fit, lwr, and upr.
#'
#' @seealso  \code{\link{ggtrendline}}, \code{\link{SSexp3P}}, \code{\link{SSpower3P}}, \code{\link[stats]{nls}}, \code{\link[stats]{selfStart}}


# selfStart method for exp2P model (formula as y = a *exp(b*x))
SSexp2P<-selfStart(
  function(predictor,a,b){a*exp(b*predictor)},
  function(mCall,LHS, data, ...) # added '...' to meet the update in 'nls' function in R package 'stats'.
  {
    xy <- sortedXyData(mCall[["predictor"]],LHS, data)

    if (min(xy[,"y"])>0){
    lmFit <- lm(log(xy[,"y"]) ~ xy[,"x"])
    coefs <- coef(lmFit)
    a <- exp(coefs[1])  #intercept
    b <- coefs[2]   #slope
    value <- c(a, b)
    names(value) <- mCall[c("a","b")]
    value
  }else{stop("
>>Try to use other selfStart functions.
Because the 'SSexp2P' function need ALL x values greater than 0.")
  }
  },c("a","b"))

# getInitial(y~SSexp2P(x,a,b),data = xy)
