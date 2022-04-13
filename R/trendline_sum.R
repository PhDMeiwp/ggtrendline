#' Summarized Results of Each Regression Model
#'
#' Summarizing the results of linear or nonlinear regression model which built in the 'ggtrendline()' function. The function includes the following models in the latest version:
#' "line2P" (formula as: y=a*x+b), "line3P" (y=a*x^2+b*x+c), "log2P" (y=a*ln(x)+b), "exp2P" (y=a*exp(b*x)),"exp3P" (y=a*exp(b*x)+c), "power2P" (y=a*x^b), and "power3P" (y=a*x^b+c).
#'
#' @param x,y  the x and y arguments provide the x and y coordinates for the ggplot. Any reasonable way of defining the coordinates is acceptable.
#' @param model select which model to fit. Default is "line2P". The "model" should be one of c("line2P", "line3P", "log2P", "exp2P", "exp3P", "power2P", "power3P"), their formulas are as follows:\cr "line2P": y=a*x+b \cr "line3P": y=a*x^2+b*x+c \cr "log2P": y=a*ln(x)+b \cr "exp2P": y=a*exp(b*x) \cr "exp3P": y=a*exp(b*x)+c \cr "power2P": y=a*x^b \cr "power3P": y=a*x^b+c
#' @param Pvalue.corrected if P-value corrected or not, the vlaue is one of c("TRUE", "FALSE").
#' @param summary summarizing the model fits. Default is TRUE.
#' @param eDigit the numbers of digits for summarized results. Default is 3.
#' @import stats
#' @import AICcmodavg
#' @export
#' @details The linear models (line2P, line3P, log2P) in this package are estimated by \code{\link[stats]{lm}} function, \cr while the nonlinear models (exp2P, exp3P, power2P, power3P) are estimated by \code{\link[stats]{nls}} function (i.e., least-squares method).\cr\cr The argument 'Pvalue.corrected' is workful for non-linear regression only.\cr\cr If "Pvalue.corrected = TRUE", the P-vlaue is calculated by using "Residual Sum of Squares" and "Corrected Total Sum of Squares (i.e. sum((y-mean(y))^2))".\cr If "Pvalue.corrected = TRUE", the P-vlaue is calculated by using "Residual Sum of Squares" and "Uncorrected Total Sum of Squares (i.e. sum(y^2))".
#' @note If the output of 'AICc' is 'Inf', not an exact number, please try to expand the sample size of your dataset to >=6.
#'
#' @return R^2, indicates the R-Squared value of each regression model.
#' @return p, indicates the p-value of each regression model.
#' @return N, indicates the sample size.
#' @return AIC, AICc, or BIC, indicate the Akaike's Information Criterion (AIC), the second-order AIC (AICc) for small samples, or Bayesian Information Criterion (BIC) for fitted model. Click \code{\link[stats]{AIC}} for details. The smaller the AIC, AICc or BIC, the better the model.
#' @return RSS, indicate the value of "Residual Sum of Squares".
#' 
#' @examples
#' library(ggtrendline)
#' x <- c(1, 3, 6, 9,  13,   17)
#' y <- c(5, 8, 11, 13, 13.2, 13.5)
#'
#' trendline_sum(x, y, model="exp3P", summary=TRUE, eDigit=3)
#'
#' @seealso  \code{\link{ggtrendline}}, \code{\link{SSexp2P}}, \code{\link{SSexp3P}}, \code{\link{SSpower2P}}, \code{\link{SSpower3P}}, \code{\link[stats]{nls}}, \code{\link[stats]{selfStart}}, \code{\link[AICcmodavg]{AICc}}

trendline_sum <- function(x,y,model="line2P", Pvalue.corrected=TRUE, summary=TRUE, eDigit=5)
{
  model=model

  OK <- complete.cases(x, y)
  x <- x[OK]
  y <- y[OK]
  z<-data.frame(x,y)
  z<-na.omit(z)
  nrow = nrow(z)

  # 1) model="line2P"
  if (model== c("line2P"))
  {
    Pvalue.corrected=TRUE

    formula = 'y = a*x + b'

    fit<- lm(y~x)
    sum.line2P <- summary(fit)
    ss.res<-sum((residuals(fit))^2) # Residual Sum of Squares, DF= n-k

    if (summary==TRUE){
      print(sum.line2P,digits=eDigit)
    }else{}

    coeff<-sum.line2P$coefficients
    a<-coeff[2,1]   # slope
    b<-coeff[1,1]   # intercept

    n<-length(x)
    pval <- coeff[2,4]   # p-value of parameter "a", indicates the p-value of whole model.
    pval<-unname(pval)
    r2 <- sum.line2P$r.squared
    adjr2<- sum.line2P$adj.r.squared

    a = format(a, digits = eDigit)
    b = format(b, digits = eDigit)
    r2 = format(r2, digits = eDigit)
    adjr2 = format(adjr2, digits = eDigit)
    pval = format(pval, digits = eDigit)

    a=as.numeric(a)
    b=as.numeric(b)
    param.out<- c(list("a"=a,"b"=b)) # for return values

  }

  # 2) model="line3P"
  if (model== c("line3P"))
  {
    Pvalue.corrected=TRUE

    formula = 'y = a*x^2 + b*x + c'

    fit<-lm(y~I(x^2)+x)

    sum.line3P <- summary(fit)
    ss.res<-sum((residuals(fit))^2) # Residual Sum of Squares, DF= n-k

    if (summary==TRUE){
      print(sum.line3P,digits=eDigit)
    }else{}

    coeff<-coef(sum.line3P)
    a<-coeff[2,1] # slope of x.square
    b<-coeff[3,1] # slope of x
    c<-coeff[1,1] # intercept c

    n<-length(x)
    r2<-sum.line3P$r.squared
    adjr2 <- sum.line3P$adj.r.squared

    fstat<-sum.line3P$fstatistic
    pval<-pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE) #p-value of whole model.
    pval<-unname(pval)

    a = format(a, digits = eDigit)
    b = format(b, digits = eDigit)
    c = format(c, digits = eDigit)
    r2 = format(r2, digits = eDigit)
    adjr2 = format(adjr2, digits = eDigit)
    pval = format(pval, digits = eDigit)

    a=as.numeric(a)
    b=as.numeric(b)
    c=as.numeric(c)
    param.out<- c(list("a"=a,"b"=b,"c"=c))
  }

  # 3) model="log2P"
  if (model== c("log2P"))
  {
    Pvalue.corrected=TRUE

    formula = 'y = a*ln(x) + b'

    yadj<-y-min(y) #adjust

    if (min(x)>0)
    {
      if (summary==TRUE){
        fit0<-lm(y~log(x))
        sum.log0<-summary(fit0)
        ss.res<-sum((residuals(fit0))^2) # Residual Sum of Squares, DF= n-k
        print(sum.log0, digits = eDigit)
      }else{}

      fit<-lm(yadj~log(x))  # adjusted y used
      sum.log<-summary(fit)
      ss.res<-sum((residuals(fit))^2) # Residual Sum of Squares, DF= n-k
      a<-sum.log$coefficients[2,1]  # slope
      b<-sum.log$coefficients[1,1]  # intercept
      b=b+min(y)  #re-adjust

      fstat<-sum.log$fstatistic
      pval<-pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE) #p-value of whole model.
      pval<-unname(pval)

      n<-length(x)
      r2<-sum.log$r.squared
      adjr2 <- sum.log$adj.r.squared

      a = format(a, digits = eDigit)
      b = format(b, digits = eDigit)
      r2 = format(r2, digits = eDigit)
      adjr2 = format(adjr2, digits = eDigit)
      pval= format(pval, digits = eDigit)

      a=as.numeric(a)
      b=as.numeric(b)
      param.out<- c(list("a"=a,"b"=b))

    }else{
      stop("
           'log2P' model need ALL x values greater than 0. Try other models.")
    }
    }


  # 4.2) model="exp2P"
  if (model== c("exp2P"))
  {
    formula = 'y = a*exp(b*x)'

    n=length(x)
    k = 2     # k means the count numbers of parameters(i.e., 'a', 'b' and 'c' in this case)

    fit<-nls(y~SSexp2P(x,a,b),data=z)
    sum.exp2P <- summary(fit)   # Get the exact value of each parameter.

    ### calculate the F-statistic and p-value for model
    ss.res<-sum((residuals(fit))^2) # Residual Sum of Squares, DF= n-k
    ss.total.uncor<-sum(y^2)             # Uncorrected Total Sum of Squares, DF=n
    ss.total.cor<-sum((y-mean(y))^2)     # Corrected Total Sum of Squares, DF=n-1

    if (Pvalue.corrected==TRUE){
      ss.reg <- ss.total.cor - ss.res  # Regression Sum of Squares, DF= (n-1)-(n-k) = k-1 in this case
      dfR= k-1
    }else{
      ss.reg <- ss.total.uncor - ss.res  # Regression Sum of Squares, DF= n-(n-k) = k in this case
      dfR= k
    }

    dfE= n-k  # degrees of freedom for Error (or Residuals)

    Fval=(ss.reg/dfR)/(ss.res/dfE)
    pval=pf(Fval,dfR,dfE,lower.tail = F)
    pval<-unname(pval)

    RSE<-sum.exp2P$sigma  # Residual standard error, type ?summary.nls in R for more detials.
    SSE<-(RSE^2)*(n-1)      # Sum of Squares for Error, not equal to 'ss.res'.

    adjr2 <- 1-SSE/((var(y))*(n-1))
    r2<-1-(1-adjr2)*((n-k)/(n-1))

    if (summary==TRUE){
      ### Start print step by step
      coeff = coef(sum.exp2P)
      # print
      cat("\nNonlinear regression model\n")
      cat("\nFormula: y = a*exp(b*x)","\n")
      df <- sum.exp2P$df
      rdf <- df[2L]
      cat("\nParameters:\n")
      printCoefmat(coeff, digits = eDigit)
      cat("\nResidual standard error:",
          format(sum.exp2P$sigma, digits = eDigit), "on", rdf, "degrees of freedom","\n")

      convInfo = fit$convInfo
      iterations<-convInfo$finIter
      tolerance<-convInfo$finTol

      cat("\nNumber of iterations to convergence:",
          format(iterations, digits = eDigit))
      cat("\nAchieved convergence tolerance:",format(tolerance, digits = eDigit),"\n")

      cat("\nMultiple R-squared:",
          format(r2, digits = eDigit), ", Adjusted R-squared: ",
          format(adjr2, digits = eDigit))
      cat("\nF-statistic:",
          format(Fval, digits = eDigit), "on", dfR, "and", dfE, "DF, ", "p-value:", format(pval, digits = eDigit), "\n")
      ### finished print
    }else{}

    coeffs<-sum.exp2P$coefficients
    a<-coeffs[1,1]
    b<-coeffs[2,1]

    a = format(a, digits = eDigit)
    b = format(b, digits = eDigit)
    r2 = format(r2, digits = eDigit)
    adjr2 = format(adjr2, digits = eDigit)
    pval= format(pval, digits = eDigit)

    a=as.numeric(a)
    b=as.numeric(b)
    param.out<- c(list("a"=a,"b"=b))

  }

  # 4.3) model="exp3P"
  if (model== c("exp3P"))
  {
    formula = 'y = a*exp(b*x) + c'

    yadj<-y-min(y)+1
    zzz<-data.frame(x,yadj)

    n=length(x)
    k = 3     # k means the count numbers of parameters(i.e., 'a', 'b' and 'c' in this case)

    # use selfStart function 'SSexp3P' for y = a *exp(b*x)+ c
    # fit model
    fit<-nls(yadj~SSexp3P(x,a,b,c),data=zzz) # use 'yadj', in case of extreme high y-values with low range, such as y= c(600002,600014,600018,600019,600020).
    sum.exp3P <- summary(fit)   # Get the exact value of each parameter.

    ### calculate the F-statistic and p-value for model
    ss.res<-sum((residuals(fit))^2) # Residual Sum of Squares, DF= n-k
    ss.total.uncor<-sum(y^2)             # Uncorrected Total Sum of Squares, DF=n
    ss.total.cor<-sum((y-mean(y))^2)     # Corrected Total Sum of Squares, DF=n-1

    if (Pvalue.corrected==TRUE){
      ss.reg <- ss.total.cor - ss.res  # Regression Sum of Squares, DF= (n-1)-(n-k) = k-1 in this case
      dfR= k-1
    }else{
      ss.reg <- ss.total.uncor - ss.res  # Regression Sum of Squares, DF= n-(n-k) = k in this case
      dfR= k
    }

    dfE= n-k  # degrees of freedom for Error (or Residuals)

    Fval=(ss.reg/dfR)/(ss.res/dfE)
    pval=pf(Fval,dfR,dfE,lower.tail = F)
    pval<-unname(pval)

    RSE<-sum.exp3P$sigma  # Residual standard error, type ?summary.nls in R for more detials.
    SSE<-(RSE^2)*(n-1)      # Sum of Squares for Error, not equal to 'ss.res'.

    adjr2 <- 1-SSE/((var(y))*(n-1))
    r2<-1-(1-adjr2)*((n-k)/(n-1))

    if (summary==TRUE){
      ### Start print step by step
      #re-adjust the output of coefficients.
      coeffadj = coef(sum.exp3P)
      ab.param<-coeffadj[1:2,]
      # re-adjust the Estimate value of parameter c
      c.param<-coeffadj[3,]
      c.p1<-c.param[1]
      c.p1 = c.p1 + min(y)-1  # re-adjust 'Estimate' value
      c.se<-c.param[2]  # Std.Error value
      c.tval<-c.p1/c.se #re-adjust 't-value'
      c.pval<-2 * pt(abs(c.tval), n-k, lower.tail = FALSE) #re-adjust 'p-value'
      c<-c(c.p1,c.se,c.tval,c.pval) # re-adjust
      coeff.re.adj<- rbind(ab.param,c)

      # print
      cat("\nNonlinear regression model\n")
      cat("\nFormula: y = a*exp(b*x) + c","\n")
      df <- sum.exp3P$df
      rdf <- df[2L]
      cat("\nParameters:\n")
      printCoefmat(coeff.re.adj, digits = eDigit)
      cat("\nResidual standard error:",
          format(sum.exp3P$sigma, digits = eDigit), "on", rdf, "degrees of freedom","\n")

      convInfo = fit$convInfo
      iterations<-convInfo$finIter
      tolerance<-convInfo$finTol

      cat("\nNumber of iterations to convergence:",
          format(iterations, digits = eDigit))
      cat("\nAchieved convergence tolerance:",format(tolerance, digits = eDigit),"\n")

      cat("\nMultiple R-squared:",
          format(r2, digits = eDigit), ", Adjusted R-squared: ",
          format(adjr2, digits = eDigit))
      cat("\nF-statistic:",
          format(Fval, digits = eDigit), "on", dfR, "and", dfE, "DF, ", "p-value:", format(pval, digits = eDigit), "\n")
      ### finished print
    }else{}

    coeff<-sum.exp3P$coefficients
    a<-coeff[1,1]
    b<-coeff[2,1]
    c<-coeff[3,1]+min(y)-1

    a = format(a, digits = eDigit)
    b = format(b, digits = eDigit)
    c = format(c, digits = eDigit)
    r2 = format(r2, digits = eDigit)
    adjr2 = format(adjr2, digits = eDigit)
    pval= format(pval, digits = eDigit)

    a=as.numeric(a)
    b=as.numeric(b)
    c=as.numeric(c)
    param.out<- c(list("a"=a,"b"=b,"c"=c))

  }


  # 5.2) model="power2P"
  if (model== c("power2P"))
  {
    formula = 'y = a*x^b'

    n<-length(x)
    k =  2  # k means the count numbers of parameters (i.e., a, b and c in this case)

    if (min(x)>0){
      # use selfStart function 'SSpower2P' for y = a *x^b
      # trendline model
      fit<-nls(y ~ SSpower2P(x,a,b),data=z)
      sum.power2P <- summary(fit)

      ### calculate the F-statistic and p-value for model
      ss.res<-sum((residuals(fit))^2) # Residual Sum of Squares, DF= n-k
      ss.total.uncor<-sum(y^2)   # Uncorrected Total Sum of Squares, DF=n
      ss.total.cor<-sum((y-mean(y))^2) # Corrected Total Sum of Squares, DF=n-1

      if (Pvalue.corrected==TRUE){
        ss.reg <- ss.total.cor - ss.res  # Regression Sum of Squares, DF= (n-1)-(n-k) = k-1 in this case
        dfR= k-1
      }else{
        ss.reg <- ss.total.uncor - ss.res  # Regression Sum of Squares, DF= n-(n-k) = k in this case
        dfR= k
      }

      dfE = n-k  # degrees of freedom for Error (or Residuals)

      Fval = (ss.reg/dfR)/(ss.res/dfE)
      pval = pf(Fval,dfR,dfE,lower.tail = F)
      pval <- unname(pval)

      RSE<-sum.power2P$sigma  # Residual standard error, type ?summary.nls in R for more detials.
      SSE<-(RSE^2)*(n-1)      # Sum of Squares for Error, not equal to 'ss.res'.

      adjr2 <- 1-SSE/((var(y))*(n-1))
      r2 <- 1-(1-adjr2)*((n-k)/(n-1))

      if (summary==TRUE){

        ### Start print step by step
        coeff = coef(sum.power2P)
        ab.param<-coeff[1:2,]

        # print
        cat("\nNonlinear regression model\n")
        cat("\nFormula:  y = a*x^b","\n")
        df <- sum.power2P$df
        rdf <- df[2L]
        cat("\nParameters:\n")
        printCoefmat(coeff, digits = eDigit)
        cat("\nResidual standard error:",
            format(sum.power2P$sigma, digits = eDigit), "on", rdf, "degrees of freedom","\n")

        convInfo = fit$convInfo
        iterations<-convInfo$finIter
        tolerance<-convInfo$finTol

        cat("\nNumber of iterations to convergence:",
            format(iterations, digits = eDigit))
        cat("\nAchieved convergence tolerance:",format(tolerance, digits = eDigit),"\n")

        cat("\nMultiple R-squared:",
            format(r2, digits = eDigit), ", Adjusted R-squared: ",
            format(adjr2, digits = eDigit))
        cat("\nF-statistic:",
            format(Fval, digits = eDigit), "on", dfR, "and", dfE, "DF, ", "p-value:", format(pval, digits = eDigit), "\n")
        ### finished print
      }else{}

      coeffs<-sum.power2P$coefficients
      a<-coeffs[1,1]
      b<-coeffs[2,1]

      a = format(a, digits = eDigit)
      b = format(b, digits = eDigit)

      r2 = format(r2, digits = eDigit)
      adjr2 = format(adjr2, digits = eDigit)
      pval = format(pval, digits = eDigit)

      a=as.numeric(a)
      b=as.numeric(b)
      param.out<- c(list("a"=a,"b"=b))
    }else{
      stop("
           'power2P' model need ALL x values greater than 0. Try other models.")
    }
    }


  # 5.3) model="power3P"
  if (model== c("power3P"))
  {
    formula = 'y = a*x^b + c'

    yadj<-y-min(y)+1
    zzz<-data.frame(x,yadj)

    n<-length(x)
    k =  3  # k means the count numbers of parameters (i.e., a, b and c in this case)

    if (min(x)>0){
      # use selfStart function 'SSpower3P' for y = a *x^b+ c
      # trendline model
      fit<-nls(yadj~SSpower3P(x,a,b,c),data=zzz)  # use 'yadj', in case of extreme high y-values with low range.
      sum.power3P <- summary(fit)

      ### calculate the F-statistic and p-value for model
      ss.res<-sum((residuals(fit))^2) # Residual Sum of Squares, DF= n-k
      ss.total.uncor<-sum(y^2)   # Uncorrected Total Sum of Squares, DF=n
      ss.total.cor<-sum((y-mean(y))^2) # Corrected Total Sum of Squares, DF=n-1

      if (Pvalue.corrected==TRUE){
        ss.reg <- ss.total.cor - ss.res  # Regression Sum of Squares, DF= (n-1)-(n-k) = k-1 in this case
        dfR= k-1
      }else{
        ss.reg <- ss.total.uncor - ss.res  # Regression Sum of Squares, DF= n-(n-k) = k in this case
        dfR= k
      }

      dfE= n-k  # degrees of freedom for Error (or Residuals)

      Fval=(ss.reg/dfR)/(ss.res/dfE)
      pval=pf(Fval,dfR,dfE,lower.tail = F)
      pval<-unname(pval)

      RSE<-sum.power3P$sigma  # Residual standard error, type ?summary.nls in R for more detials.
      SSE<-(RSE^2)*(n-1)      # Sum of Squares for Error, not equal to 'ss.res'.

      adjr2 <- 1-SSE/((var(y))*(n-1))
      r2<-1-(1-adjr2)*((n-k)/(n-1))

      if (summary==TRUE){

        ### Start print step by step
        #re-adjust the output of coefficients.
        coeffadj = coef(sum.power3P)
        ab.param<-coeffadj[1:2,]
        # re-adjust the 'Estimate\ value of parameter 'c'
        c.param<-coeffadj[3,]
        c.p1<-c.param[1]
        c.p1 = c.p1 + min(y)-1  # re-adjust
        c.se<-c.param[2]  # Std.Error value
        c.tval<-c.p1/c.se #re-adjust 't-value'
        c.pval<-2 * pt(abs(c.tval), n-k, lower.tail = FALSE) #re-adjust 'p-value'
        c<-c(c.p1,c.se,c.tval,c.pval) # re-adjust
        coeff.re.adj<- rbind(ab.param,c)

        # print
        cat("\nNonlinear regression model\n")
        cat("\nFormula:  y = a*x^b + c","\n")
        df <- sum.power3P$df
        rdf <- df[2L]
        cat("\nParameters:\n")
        printCoefmat(coeff.re.adj, digits = eDigit)
        cat("\nResidual standard error:",
            format(sum.power3P$sigma, digits = eDigit), "on", rdf, "degrees of freedom","\n")

        convInfo = fit$convInfo
        iterations<-convInfo$finIter
        tolerance<-convInfo$finTol

        cat("\nNumber of iterations to convergence:",
            format(iterations, digits = eDigit))
        cat("\nAchieved convergence tolerance:",format(tolerance, digits = eDigit),"\n")

        cat("\nMultiple R-squared:",
            format(r2, digits = eDigit), ", Adjusted R-squared: ",
            format(adjr2, digits = eDigit))
        cat("\nF-statistic:",
            format(Fval, digits = eDigit), "on", dfR, "and", dfE, "DF, ", "p-value:", format(pval, digits = eDigit), "\n")
        ### finished print
      }else{}

      coeff<-sum.power3P$coefficients
      a<-coeff[1,1]
      b<-coeff[2,1]
      c<-coeff[3,1]
      c<-c+min(y)-1  #re-adjust

      a = format(a, digits = eDigit)
      b = format(b, digits = eDigit)
      c = format(c, digits = eDigit)
      r2 = format(r2, digits = eDigit)
      adjr2 = format(adjr2, digits = eDigit)
      pval = format(pval, digits = eDigit)

      a=as.numeric(a)
      b=as.numeric(b)
      c=as.numeric(c)
      param.out<- c(list("a"=a,"b"=b,"c"=c))
    }else{
      stop("
           'power3P' model need ALL x values greater than 0. Try other models.")
    }

    # 100) beyond the  built-in models.

    }else{
      Check<-c("line2P","line3P","log2P","exp2P","exp3P","power2P","power3P")
      if (!model %in% Check)
        stop("
             \"model\" should be one of c(\"lin2P\",\"line3P\",\"log2P\",\"exp2P\",\"exp3P\",\"power2P\",\"power3P\".")
    }

  nrow = as.numeric(nrow)
  r2=as.numeric(r2)
  adjr2=as.numeric(adjr2)
  pval=as.numeric(pval)
  AIC = as.numeric(format(AIC(fit), digits = eDigit))
  BIC = as.numeric(format(BIC(fit), digits = eDigit))
  ss.res=as.numeric(format(ss.res, digits = eDigit))
  if (requireNamespace("AICcmodavg", quietly = TRUE)){
    AICc = as.numeric(format(AICcmodavg::AICc(fit), digits = eDigit))}

  if (summary==TRUE){
    ##print N, AIC, AICc, BIC and RSS
    cat("\nN:", nrow, ", AIC:", AIC, ", AICc:", AICc, ", BIC: ", BIC, "\nResidual Sum of Squares: ", ss.res,"\n")
      }else{}

  invisible(list(formula=formula, parameter=param.out, R.squared=r2, adj.R.squared=adjr2, p.value = pval,
                      N = nrow, AIC=AIC, AICc=AICc, BIC=BIC, RSS=ss.res))
  }
