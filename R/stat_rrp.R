#' Add R square and P-value to 'ggplot'
#'
#' Add R-square and P-value of regression models to 'ggplot',
#' by using models built in the 'ggtrendline()' function. The function includes the following models: \cr
#' "line2P" (formula as: y=a*x+b), \cr  "line3P" (y=a*x^2+b*x+c), \cr "log2P" (y=a*ln(x)+b), \cr "exp2P" (y=a*exp(b*x)), \cr  "exp3P" (y=a*exp(b*x)+c), \cr "power2P" (y=a*x^b), \cr and "power3P" (y=a*x^b+c).
#'
#' @param x,y  the x and y arguments provide the x and y coordinates for the 'ggplot'. Any reasonable way of defining the coordinates is acceptable.
#' @param model select which model to fit. Default is "line2P". The "model" should be one of c("line2P", "line3P", "log2P", "exp2P", "exp3P", "power2P", "power3P"), their formulas are as follows:\cr "line2P": y=a*x+b \cr "line3P": y=a*x^2+b*x+c \cr "log2P": y=a*ln(x)+b \cr "exp2P": y=a*exp(b*x) \cr "exp3P": y=a*exp(b*x)+c \cr "power2P": y=a*x^b \cr "power3P": y=a*x^b+c
#' @param Pvalue.corrected if P-value corrected or not, the value is one of c("TRUE", "FALSE").
#' @param show.Rsquare whether to show the R-square, the value is one of c("TRUE", "FALSE").
#' @param show.pvalue whether to show the P-value, the value is one of c("TRUE", "FALSE").
#' @param Rname to specify the character of R-square, the value is one of c(0, 1), corresponding to c(r^2, R^2).
#' @param Pname to specify the character of P-value, the value is one of c(0, 1), corresponding to c(p, P).
#' @param rrp.x,rrp.y the position for R square and P value.
#' @param text.col the color used for the equation text.
#' @param eDigit the numbers of digits for R square and P value. Default is 3.
#' @param eSize  font size of R square and P value. Default is 3.
#' @import ggplot2
#' @export
#' @return  No return value (called for side effects).
#' @details The values of each parameter of regression model can be found by typing \code{\link{trendline_sum}} function in this package.\cr\cr The linear models (line2P, line3P, log2P) in this package are estimated by \code{\link[stats]{lm}} function, while the nonlinear models (exp2P, exp3P, power2P, power3P) are estimated by \code{\link[stats]{nls}} function (i.e., least-squares method).\cr\cr The argument 'Pvalue.corrected' is only valid for non-linear regression.\cr\cr If "Pvalue.corrected = TRUE", the P-value is calculated by using "Residual Sum of Squares" and "Corrected Total Sum of Squares (i.e. sum((y-mean(y))^2))".\cr\cr If "Pvalue.corrected = FALSE", the P-value is calculated by using "Residual Sum of Squares" and "Uncorrected Total Sum of Squares (i.e. sum(y^2))".
#'
#' @seealso  \code{\link{ggtrendline}}, \code{\link{stat_eq}}, \code{\link{trendline_sum}}

stat_rrp <- function(x, y, model="line2P", Pvalue.corrected = TRUE,
                      show.Rsquare = TRUE, show.pvalue = TRUE,
                      Rname = 0, Pname = 0,
                      rrp.x = NULL, rrp.y = NULL, text.col="black", eDigit = 3, eSize = 3
                      )
{
  model = model
 if(Rname==0)   Rname = "r"  else Rname = "R"
  if(Pname==0)  Pname = "p"  else Pname = "P"

  OK <- complete.cases(x, y)
  x <- x[OK]
  y <- y[OK]
  z <- data.frame(x,y)

  return <- trendline_sum(x=x, y=y, model=model, Pvalue.corrected=Pvalue.corrected, summary = FALSE, eDigit = eDigit)

  if (return$p.value >= 0.0001){
    pval <- return$p.value
    pval <- paste("=" , unname(pval))
    }else{
    pval <- "< 0.0001"
  }
  r2   <- return$R.squared
  adjr2<- return$adj.R.squared

  if (show.Rsquare  == TRUE & show.pvalue  == FALSE) param = substitute(expression(italic(Rname)^2 == r2))[2]
  if (show.Rsquare  == FALSE & show.pvalue  == TRUE) param =substitute(expression(italic(Pname)~~pval))[2]
  if (show.Rsquare  == TRUE & show.pvalue  == TRUE) param = substitute(expression(italic(Rname)^2 == r2*","~~italic(Pname)~~pval))[2]
  if (show.Rsquare  == FALSE & show.pvalue  == FALSE) param =NULL

  Check <- c("line2P","line3P","log2P","exp2P","exp3P","power2P","power3P")
  if (!model %in% Check)
  stop("
\"model\" should be one of c(\"lin2P\",\"line3P\",\"log2P\",\"exp2P\",\"exp3P\",\"power2P\",\"power3P\").")


### add R square and P value to 'ggplot'
  startx <- min(x)+(max(x)-min(x))*0.1
  starty <- max(y)
  if (is.null(rrp.x)) rrp.x = startx else rrp.x = rrp.x
  if (is.null(rrp.y)) rrp.y = starty else rrp.y = rrp.y

   ggplot2::geom_text(data=NULL, aes(x=rrp.x, y=rrp.y, label = as.character(param)), parse = TRUE, color = text.col, size = eSize)

}
