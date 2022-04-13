#' Self-Starting Nls 'power2P' Regression Model
#'
#' This selfStart model evaluates the power regression function (formula as: y=a*x^b). It has an initial attribute that will evaluate initial estimates of the parameters 'a' and 'b' for a given set of data.
#'
#' @usage SSpower2P(predictor, a, b)
#' @param predictor  a numeric vector of values at which to evaluate the model.
#' @param a,b The numeric parameters responding to the exp2P model.
#' @export
#' @examples
#' library(ggtrendline)
#' x<-1:5
#' y<-c(2,4,8,20,25)
#' xy<-data.frame(x,y)
#' getInitial(y ~ SSpower2P(x,a,b), data = xy)
#' ## Initial values are in fact the converged values
#'
#' fitpower2P <- nls(y~SSpower2P(x,a,b), data=xy)
#' summary(fitpower2P)
#'
#' prediction <- predFit(fitpower2P , data.frame(x=x), se.fit = TRUE,
#'                           level = 0.95, interval = "confidence")
#' yfitpower2P <- prediction$fit
#' yfitpower2P # output a matrix of predictions and bounds with column names fit, lwr, and upr.
#'
#' @seealso  \code{\link{ggtrendline}}, \code{\link{SSexp3P}}, \code{\link{SSpower3P}}, \code{\link[stats]{nls}}, \code{\link[stats]{selfStart}}


# selfStart method for power2P model (formula as y = a *x^b)
SSpower2P<-selfStart(
  function(predictor,a,b){a*predictor^b},
  function(mCall,LHS, data, ...) # added '...'
{
  xy <- sortedXyData(mCall[["predictor"]],LHS, data)

  if (min(xy[,"x"])>0){
  lmFit <- lm(log(xy[,"y"]) ~ log(xy[,"x"])) # both x and adjy values should be greater than 0.
  coefs <- coef(lmFit)
  a <- exp(coefs[1])  #intercept
  b <- coefs[2]   #slope

  value <- c(a,b)
  names(value) <- mCall[c("a","b")]
  value

  }else{stop("
>>Try to use other selfStart functions.
Because the 'SSpower2P' function need ALL x values greater than 0.")
  }
  },c("a","b"))

# getInitial(y~SSpower2P(x,a,b),data = xy)
