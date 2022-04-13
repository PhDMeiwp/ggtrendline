#' Self-Starting Nls 'power3P' Regression Model
#'
#' This selfStart model evaluates the power regression function (formula as: y=a*x^b+c). It has an initial attribute that will evaluate initial estimates of the parameters a, b, and c for a given set of data.
#'
#' @usage SSpower3P(predictor, a, b, c)
#' @param predictor  a numeric vector of values at which to evaluate the model.
#' @param a,b,c Three numeric parameters responding to the exp3P model.
#' @export
#' @examples
#' library(ggtrendline)
#' x<-1:5
#' y<-c(2,4,8,20,25)
#' xy<-data.frame(x,y)
#' getInitial(y ~ SSpower3P(x,a,b,c), data = xy)
#' ## Initial values are in fact the converged values
#'
#' fitpower3P <- nls(y~SSpower3P(x,a,b,c), data=xy)
#' summary(fitpower3P)
#'
#' prediction <- predFit(fitpower3P , data.frame(x=x), se.fit = TRUE,
#'                           level = 0.95, interval = "confidence")
#' yfitpower3P <- prediction$fit
#' yfitpower3P # output a matrix of predictions and bounds with column names fit, lwr, and upr.
#'
#' @seealso  \code{\link{ggtrendline}}, \code{\link{SSexp3P}}, \code{\link{SSpower3P}}, \code{\link[stats]{nls}}, \code{\link[stats]{selfStart}}

# selfStart method for power3P model (formula as y = a *x^b+ c)
SSpower3P<-selfStart(
  function(predictor,a,b,c){a*predictor^b+c},
  function(mCall,LHS, data, ...) # added '...'
    {
      xy <- sortedXyData(mCall[["predictor"]],LHS, data)
      y=xy[,"y"]
      x=xy[,"x"]

    if (min(x)>0){

      adjy=y-min(y)+1
      xadjy=data.frame(x,adjy)

      lmFit <- lm(log(adjy) ~ log(x)) # both x and adjy values should be greater than 0.
      coefs <- coef(lmFit)
      get.b <- coefs[2]   #slope

      nlsFit<-nls(adjy~cbind(1+x^b,x^b),
                  start = list(b=get.b),data = xadjy,algorithm = "plinear",
                  nls.control(maxiter = 5000000,minFactor = 10^(-10)))

      coef<-coef(nlsFit)
      b<-coef[1]
      c<-coef[2]+min(y)-1
      a<-coef[3]+coef[2]

      value <- c(a,b,c)
      names(value) <- mCall[c("a","b","c")]
      value

      }else{stop("
>>Try to use other selfStart functions.
Because the 'SSpower3P' function need ALL x values greater than 0.")
        }
    },c("a","b","c"))

    # getInitial(y~SSpower3P(x,a,b,c),data = xy)
