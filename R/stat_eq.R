#' Add Equation to 'ggplot'
#'
#' Add regression equation to ggplot,
#' by using different models built in the 'ggtrendline()' function. The function includes the following models in the latest version:
#' "line2P" (formula as: y=a*x+b), "line3P" (y=a*x^2+b*x+c), "log2P" (y=a*ln(x)+b), "exp2P" (y=a*exp(b*x)),"exp3P" (y=a*exp(b*x)+c), "power2P" (y=a*x^b), and "power3P" (y=a*x^b+c).
#'
#' @param x,y  the x and y arguments provide the x and y coordinates for the ggplot. Any reasonable way of defining the coordinates is acceptable.
#' @param model select which model to fit. Default is "line2P". The "model" should be one of c("line2P", "line3P", "log2P", "exp2P", "exp3P", "power2P", "power3P"), their formulas are as follows:\cr "line2P": y=a*x+b \cr "line3P": y=a*x^2+b*x+c \cr "log2P": y=a*ln(x)+b \cr "exp2P": y=a*exp(b*x) \cr "exp3P": y=a*exp(b*x)+c \cr "power2P": y=a*x^b \cr "power3P": y=a*x^b+c
#' @param show.eq whether to show the regression equation, the value is one of c("TRUE", "FALSE").
#' @param xname to specify the expression of "x" in equation, i.e., expression('x'), see Examples.
#' @param yname to specify the expression of "y" in equation, i.e., expression('y'), see Examples.
#' @param yhat whether to add a hat symbol (^) on the top of "y" in equation. Default is FALSE.
#' @param eq.x,eq.y equation position.
#' @param text.col the color used for the equation text.
#' @param eDigit the numbers of digits for equation parameters. Default is 3.
#' @param eSize  font size of equation. Default is 3.
#' @import ggplot2
#' @export
#' @return NULL
#'
#'
#' @seealso  \code{\link{ggtrendline}}, \code{\link{stat_rrp}}, \code{\link{trendline_sum}}

stat_eq <- function(x, y, model="line2P",
                      show.eq = TRUE, xname = "x", yname = "y", yhat = FALSE,
                      eq.x = NULL, eq.y = NULL, text.col="black", eDigit = 3, eSize = 3
                      )
{
  model = model
  xname = substitute(xname)
  if(yhat == TRUE)  yname = substitute(hat(yname)) else yname = substitute(yname)

  OK <- complete.cases(x, y)
  x <- x[OK]
  y <- y[OK]
  z <- data.frame(x,y)

  return <- trendline_sum(x=x, y=y, model=model, summary = FALSE, eDigit = eDigit)
  a = return$parameter$a
  b = return$parameter$b
  a = format(a, digits = eDigit)
  b = format(b, digits = eDigit)
  if (a==1){a=c("")}
  if (b==1){b=c("")}
  if (b==-1){b=c("-")}

  if (is.null(return$parameter$c)==FALSE){
    c = return$parameter$c
    c = format(c, digits = eDigit)
    if (c==1){c=c("")}
    if (c==-1){c=c("-")}
  }else{}

# 1) model="line2P"
if (model== c("line2P"))
  { formula = 'y = a*x + b'

    if (b>=0)
    {param <- substitute(expression(italic(yname) == a~italic(xname) +b))[2]
    }else{
     param <- substitute(expression(italic(yname) == a~italic(xname) ~b))[2]
    }
 }

# 2) model="line3P"
  if (model== c("line3P"))
  { formula = 'y = a*x^2 + b*x + c'

    if (b>=0)
    {
      if(c>=0)
      {param <- substitute(expression(italic(yname) == a~italic(xname)^2 + b~italic(xname) +c))[2]
      }else{param <- substitute(expression(italic(yname) == a~italic(xname)^2 + b~italic(xname) ~c))[2]
      }
    }else{
      if(c>=0)
      {param <- substitute(expression(italic(yname) == a~italic(xname)^2 ~b~italic(xname) +c))[2]
      }else{param <- substitute(expression(italic(yname) == a~italic(xname)^2 ~b~italic(xname) ~c))[2]
      }
    }
  }

# 3) model="log2P"
if (model== c("log2P"))
  { formula = 'y = a*ln(x) + b'

  if (min(x)>0)
  {
    if (b>=0)
    {param <- substitute(expression(italic(yname) == a~"ln(x)" +b))[2]
    }else{
     param <- substitute(expression(italic(yname) == a~"ln(x)" ~b))[2]
    }

 }else{
    stop("
'log2P' model need ALL x values greater than 0. Try other models.")
 }
}

# 4.2) model="exp2P"
  if (model== "exp2P")
  { formula = 'y = a*exp(b*x)'
    param <- substitute(expression(italic(yname) == a~"e"^{b~italic(xname)}))[2]
  }


# 4.3) model="exp3P"
  if (model== "exp3P")
  { formula = 'y = a*exp(b*x) + c'

      if (c>=0){
      param <- substitute(expression(italic(yname) == a~"e"^{b~italic(xname)}~+c))[2]
      }else{
      param <- substitute(expression(italic(yname) == a~"e"^{b~italic(xname)}~~c))[2]
      }
  }


# 5.2) model="power2P"
if (model== "power2P")
  {formula = 'y = a*x^b'

    if (min(x)>0){
        param <- substitute(expression(italic(yname) == a~italic(xname)^b))[2]
    }else{
      stop("
           'power2P' model need ALL x values greater than 0. Try other models.")
    }
}


# 5.3) model="power3P"
if (model== "power3P")
  {formula = 'y = a*x^b + c'

    if (min(x)>0){

    if (c>=0){
        param <- substitute(expression(italic(yname) == a~italic(xname)^b ~+c))[2]
        }else{
        param <- substitute(expression(italic(yname) == a~italic(xname)^b ~~c))[2]
        }

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

### add equation to ggplot
  startx <- min(x)+(max(x)-min(x))*0.1
  starty <- max(y)+(max(y)-min(y))*0.1
  if (is.null(eq.x)) eq.x = startx else eq.x = eq.x
  if (is.null(eq.y)) eq.y = starty else eq.y = eq.y

  if (show.eq == TRUE) param = param else param = ""

      ggplot2::geom_text(data=NULL, aes(x=eq.x, y=eq.y, label = as.character(param)), parse = TRUE, color = text.col, size = eSize)
}
