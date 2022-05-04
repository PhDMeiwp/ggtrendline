#' Add Trendline and Confidence Interval to 'ggplot'
#'
#' Add trendline and confidence interval of linear or nonlinear regression model to 'ggplot',
#' by using different models built in the 'ggtrendline()' function. \cr The function includes the following models:\cr
#' "line2P" (formula as: y=a*x+b), \cr  "line3P" (y=a*x^2+b*x+c), \cr "log2P" (y=a*ln(x)+b), \cr "exp2P" (y=a*exp(b*x)), \cr  "exp3P" (y=a*exp(b*x)+c), \cr "power2P" (y=a*x^b), \cr and "power3P" (y=a*x^b+c).
#'
#' @param x,y  the x and y arguments provide the x and y coordinates for the 'ggplot'. Any reasonable way of defining the coordinates is acceptable.
#' @param model select which model to fit. Default is "line2P". The "model" should be one of c("line2P", "line3P", "log2P", "exp2P", "exp3P", "power2P", "power3P"), their formulas are as follows:\cr "line2P": y=a*x+b \cr "line3P": y=a*x^2+b*x+c \cr "log2P": y=a*ln(x)+b \cr "exp2P": y=a*exp(b*x) \cr "exp3P": y=a*exp(b*x)+c \cr "power2P": y=a*x^b \cr "power3P": y=a*x^b+c
#'
#' @param linecolor the color of regression line. Default is "black".
#' @param linetype the type of regression line.  Default is 1. Notes: linetype can be specified using either text c("blank","solid","dashed","dotted","dotdash","longdash","twodash") or number c(0, 1, 2, 3, 4, 5, 6).
#' @param linewidth the width of regression line. Default is 0.6.
#'
#' @param CI.level level of confidence interval to use. Default is 0.95.
#' @param CI.fill the color for filling the confidence interval. Default is "grey60".
#' @param CI.alpha alpha value of filling color of confidence interval. Default is 0.3.
#' @param CI.color line color of confidence interval. Default is "black".
#' @param CI.lty line type of confidence interval. Default is 2.
#' @param CI.lwd line width of confidence interval. Default is 0.5.
#'
#' @param summary summarizing the model fits. Default is TRUE.
#'
#' @param show.eq whether to show the regression equation, the value is one of c("TRUE", "FALSE").
#' @param yhat whether to add a hat symbol (^) on the top of "y" in equation. Default is FALSE.
#' @param eq.x,eq.y equation position.
#'
#' @param Pvalue.corrected if P-value corrected or not, the value is one of c("TRUE", "FALSE").
#' @param show.Rsquare whether to show the R-square, the value is one of c("TRUE", "FALSE").
#' @param show.pvalue whether to show the P-value, the value is one of c("TRUE", "FALSE").
#' @param Rname to specify the character of R-square, the value is one of c(0, 1), corresponding to c(r^2, R^2).
#' @param Pname to specify the character of P-value, the value is one of c(0, 1), corresponding to c(p, P).
#' @param rrp.x,rrp.y the position for R square and P value.
#' @param text.col the color used for the equation text.
#' @param eDigit the numbers of digits for R square and P value. Default is 3.
#' @param eSize  font size of R square and P value. Default is 3.
#' @param xlab,ylab labels of x- and y-axis.
#'
#' @import stats
#' @import ggplot2
#' @export
#' @return  No return value (called for side effects).
#' @details The values of each parameter of regression model can be found by typing \code{\link{trendline_sum}} function in this package.\cr\cr The linear models (line2P, line3P, log2P) in this package are estimated by \code{\link[stats]{lm}} function, while the nonlinear models (exp2P, exp3P, power2P, power3P) are estimated by \code{\link[stats]{nls}} function (i.e., least-squares method).
#'
#' @references
#' Ritz C., and Streibig J. C. (2007)
#' \emph{Nonlinear Regression with R}. Springer.
#'
#' Greenwell B. M., and Schubert Kabban C. M. (2014)
#' \emph{investr: An R Package for Inverse Estimation}. The R Journal, 6(1), 90-100.
#'
#' @examples
#' # library(ggplot2)
#' library(ggtrendline)
#' x <- c(1, 3, 6, 9,  13,   17)
#' y <- c(5, 8, 11, 13, 13.2, 13.5)
#'
#' ggtrendline(x, y, model = "line2P")  # default
#' ggtrendline(x, y, model = "log2P", CI.fill = NA)  # CI lines only, without CI filling
#'
#' ggtrendline(x, y, model = "exp2P", linecolor = "blue", linetype = 1, linewidth = 1) # set line
#' ggtrendline(x, y, model = "exp3P", CI.level = 0.99,
#'             CI.fill = "red", CI.alpha = 0.1, CI.color = NA, CI.lty = 2, CI.lwd = 1.5) # set CI
#'
#' @seealso  \code{\link{ggtrendline}}, \code{\link{stat_eq}}, \code{\link{stat_rrp}}, \code{\link{trendline_sum}}, \code{\link[stats]{nls}}, \code{\link[stats]{selfStart}}


ggtrendline <- function(x, y, model="line2P",
                        linecolor = "black", linetype = 1, linewidth = 0.6,
                        CI.level = 0.95, CI.fill = "grey60", CI.alpha = 0.3, CI.color = "black",  CI.lty = 2, CI.lwd = 0.5,
                        summary = TRUE,
                        show.eq = TRUE,
                        yhat = FALSE, eq.x = NULL, eq.y = NULL,
                        show.Rsquare = TRUE, show.pvalue = TRUE, Pvalue.corrected = TRUE,
                        Rname = 0, Pname = 0, rrp.x = NULL, rrp.y = NULL,
                        text.col="black", eDigit = 3, eSize = 3,
                        xlab=NULL, ylab=NULL)
{
  model=model
  if(is.null(xlab))  xlab = deparse(substitute(x)) else xlab = xlab
  if(is.null(ylab))  ylab = deparse(substitute(y)) else ylab = ylab

  OK <- complete.cases(x, y)
  x <- x[OK]
  y <- y[OK]
  z<-data.frame(x,y)

  xxx <- seq(min(x), max(x), len=100)

# 1) model="line2P"
if (model== c("line2P"))
  {formula = 'y = a*x + b'
  fit <- lm(y~x)

  prediction <- stats::predict(fit, data.frame(x=xxx), se.fit = TRUE,
                       level = CI.level, interval = "confidence")
  yfit <- prediction$fit
 }

# 2) model="line3P"
  if (model== c("line3P"))
  { formula = 'y = a*x^2 + b*x + c'
    fit<-lm(y~I(x^2)+x)

    prediction <- stats::predict(fit, data.frame(x=xxx), se.fit = TRUE,
                                 level = CI.level, interval = "confidence")
    yfit <- prediction$fit
  }

# 3) model="log2P"
if (model== c("log2P"))
  { formula = 'y = a*ln(x) + b'

  if (min(x)>0)
  { fit<-lm(y~log(x))

    prediction <- stats::predict(fit, data.frame(x=xxx), se.fit = TRUE,
                                 level = CI.level, interval = "confidence")
    yfit <- prediction$fit
   }else{
    stop("
'log2P' model need ALL x values greater than 0. Try other models.")
    }
  }

# 4.2) model="exp2P"
if (model== c("exp2P"))
  { formula = 'y = a*exp(b*x)'
    fit<-nls(y~SSexp2P(x,a,b),data=z)

    prediction <- predFit(fit, data.frame(x=xxx), se.fit = TRUE,
                            level = CI.level, interval = "confidence")
    yfit <- prediction$fit
}

# 4.3) model="exp3P"
  if (model== c("exp3P"))
  { formula = 'y = a*exp(b*x) + c'
    fit<-nls(y~SSexp3P(x,a,b,c),data=z)

    prediction <- predFit(fit, data.frame(x=xxx), se.fit = TRUE,
                                 level = CI.level, interval = "confidence")
    yfit <- prediction$fit
  }

 # 5.2) model="power2P"
  if (model== c("power2P"))
  { formula = 'y = a*x^b'

    if (min(x)>0){
      fit<-nls(y~SSpower2P(x,a,b),data=z)

      prediction <- predFit(fit, data.frame(x=xxx), se.fit = TRUE,
                                   level = CI.level, interval = "confidence")
      yfit <- prediction$fit
    }else{
      stop("
'power2P' model need ALL x values greater than 0. Try other models.")
    }
  }

# 5.3) model="power3P"
if (model== c("power3P"))
    { formula = 'y = a*x^b + c'

    if (min(x)>0){
      fit<-nls(y~SSpower3P(x,a,b,c),data=z)

      prediction <- predFit(fit, data.frame(x=xxx), se.fit = TRUE,
                                   level = CI.level, interval = "confidence")
      yfit <- prediction$fit
    }else{
    stop("
'power3P' model need ALL x values greater than 0. Try other models.")
    }

# 100) beyond the  built-in models.

}else{
  Check<-c("line2P","line3P","log2P","exp2P","exp3P","power2P","power3P")
  if (!model %in% Check)
  stop("
\"model\" should be one of c(\"lin2P\",\"line3P\",\"log2P\",\"exp2P\",\"exp3P\",\"power2P\",\"power3P\").")
}

  if (summary==TRUE){
    trendline_sum(x=x, y=y, model=model, Pvalue.corrected = Pvalue.corrected, eDigit = eDigit)
  }else{}


  if (requireNamespace("ggplot2", quietly = TRUE)){
    gg1 <- ggplot(NULL, aes(x = xxx))+
      geom_ribbon(aes(x = xxx, ymin = yfit[,2], ymax = yfit[,3]), fill=CI.fill, color= CI.color, alpha = CI.alpha, linetype = CI.lty, size = CI.lwd) +
      geom_line(aes(x = xxx, y = yfit[,1]), color = linecolor, linetype = linetype, size = linewidth)+

      stat_eq(x=x, y=y, model=model,
              show.eq = show.eq, xname = "x", yname = "y", yhat = yhat,
              eq.x = eq.x, eq.y = eq.y, text.col=text.col, eDigit = eDigit, eSize = eSize)+

      stat_rrp(x=x, y=y, model=model, Pvalue.corrected = Pvalue.corrected,
               show.Rsquare = show.Rsquare, show.pvalue = show.pvalue, Rname = Rname, Pname = Pname,
               rrp.x = rrp.x , rrp.y = rrp.y , text.col=text.col, eDigit = eDigit , eSize = eSize)+
      xlab(xlab) + ylab(ylab)
    gg1  # ggtrendline
  }

}
