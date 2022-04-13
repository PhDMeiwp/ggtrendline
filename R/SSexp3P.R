#' Self-Starting Nls 'exp3P' Regression Model
#'
#' This selfStart model evaluates the exponential regression function (formula as: y=a*exp(b*x)+c). It has an initial attribute that will evaluate initial estimates of the parameters a, b, and c for a given set of data.
#'
#' @usage SSexp3P(predictor, a, b, c)
#' @param predictor  a numeric vector of values at which to evaluate the model.
#' @param a,b,c Three numeric parameters responding to the exp3P model.
#' @export
#' @examples
#' library(ggtrendline)
#' x<-1:5
#' y<-c(2,4,8,16,28)
#' xy<-data.frame(x,y)
#' getInitial(y ~ SSexp3P(x,a,b,c), data = xy)
#' ## Initial values are in fact the converged values
#'
#' fitexp3P <- nls(y~SSexp3P(x,a,b,c), data=xy)
#' summary(fitexp3P)
#'
#' prediction <- predFit(fitexp3P , data.frame(x=x), se.fit = TRUE,
#'                           level = 0.95, interval = "confidence")
#' yfitexp3P <- prediction$fit
#' yfitexp3P  # output a matrix of predictions and bounds with column names fit, lwr, and upr.
#'
#' @seealso  \code{\link{ggtrendline}}, \code{\link{SSexp3P}}, \code{\link{SSpower3P}}, \code{\link[stats]{nls}}, \code{\link[stats]{selfStart}}

# selfStart method for exp3P model (formula as y = a *exp(b*x)+ c)
SSexp3P<-selfStart(
  function(predictor,a,b,c){a*exp(b*predictor)+c},
  function(mCall,LHS, data, ...) # added '...'
  {
    xy <- sortedXyData(mCall[["predictor"]],LHS, data)
    y=xy[,"y"]
    x=xy[,"x"]
    adjy=y-min(y)+1
    xadjy=data.frame(x,adjy)

    lmFit <- lm(log(adjy) ~ x)
    coefs <- coef(lmFit)
    get.b <- coefs[2]   #slope

    nlsFit<-nls(adjy~cbind(1+exp(b*x),exp(b*x)),
                start = list(b=get.b),data = xadjy,algorithm = "plinear",
                nls.control(maxiter = 5000000,minFactor = 10^(-10)))

    coef<-coef(nlsFit)
    b<-coef[1]
    c<-coef[2]+min(y)-1
    a<-coef[3]+coef[2]

    value <- c(a,b,c)
    names(value) <- mCall[c("a","b","c")]
    value
  },c("a","b","c"))

  # getInitial(y~SSexp3P(x,a,b,c),data = z)
