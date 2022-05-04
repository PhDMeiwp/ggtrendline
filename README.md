# ggtrendline: an R package for adding trendline and confidence interval to ggplot

<a href="https://sm.ms/image/JGF8MWVbRwh2X17" target="_blank"><img src="https://s2.loli.net/2022/04/13/JGF8MWVbRwh2X17.png" height="300" align="right"></a>

[![cran version](http://www.r-pkg.org/badges/version/ggtrendline)](http://cran.rstudio.com/web/packages/ggtrendline) 
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/ggtrendline)](https://github.com/metacran/cranlogs.app)

## 1. Installing "ggtrendline" package in R

- Get the released version from CRAN:

		install.packages("ggtrendline")

- Or the development version from Github:

		install.packages("devtools")
		devtools::install_github("PhDMeiwp/ggtrendline@master", force = TRUE)
		library(ggtrendline)


## 2. Using "ggtrendline" package
	
     library(ggplot2)
	 library(ggtrendline)
     x <- c(1, 3, 6, 9,  13,   17)
     y <- c(5, 8, 11, 13, 13.2, 13.5)

### 2.1 default ("line2P")

     ggtrendline(x, y, model = "line2P")  

<a href="https://sm.ms/image/4pICPTDh2gJxuFK" target="_blank"><img src="https://s2.loli.net/2022/04/13/4pICPTDh2gJxuFK.png" ></a>

<a href="https://sm.ms/image/JOrRHYWGy8EMofs" target="_blank"><img src="https://s2.loli.net/2022/04/13/JOrRHYWGy8EMofs.png" ></a>


### 2.2 add geom_point()

     ggtrendline(x, y, model = "line3P") + geom_point(aes(x, y)) + theme_bw()

<a href="https://sm.ms/image/Dp6Lt58jf9rmaNW" target="_blank"><img src="https://s2.loli.net/2022/04/13/Dp6Lt58jf9rmaNW.png" ></a>

### 2.3 CI lines only, without CI filling

     ggtrendline(x, y, model = "log2P", CI.fill = NA) + 
			geom_point(aes(x, y))+ theme_classic() 
	
<a href="https://sm.ms/image/VuDypF3tZWzK9B5" target="_blank"><img src="https://s2.loli.net/2022/04/13/VuDypF3tZWzK9B5.png" ></a>

### 2.4 set the regression line and geom_point()

     ggtrendline(x, y, model = "exp2P", linecolor = "blue", linetype = 1, linewidth = 1) + 
             geom_point(aes(x, y), color = "blue", shape = 1, size = 3)  

<a href="https://sm.ms/image/TF48LAtiIHB1ukd" target="_blank"><img src="https://s2.loli.net/2022/04/13/TF48LAtiIHB1ukd.png" ></a>
		
### 2.5 set confidence interval

     ggtrendline(x, y, model = "exp3P", CI.level = 0.99, 
                CI.fill = "red", CI.alpha = 0.1, CI.color = NA, CI.lty = 2, CI.lwd = 1.5) + 
             geom_point(aes(x, y)) 

<a href="https://sm.ms/image/6ul7toUOWkhcw3s" target="_blank"><img src="https://s2.loli.net/2022/04/13/6ul7toUOWkhcw3s.png" ></a>
		
### 2.6 one trendline with different points belonged to multiple groups.

		library(ggplot2)
		library(ggtrendline)
		data("iris")
		x <- iris$Petal.Width
		y <- iris$Petal.Length
		group <- iris$Species
		ggtrendline(x,y,"exp3P") + geom_point(aes(x,y,color=group))

<a href="https://sm.ms/image/MYa9WHqlALfFXeD" target="_blank"><img src="https://s2.loli.net/2022/05/04/MYa9WHqlALfFXeD.png" ></a>

## 3. Details

### 3.1 Description

The 'ggtrendline' package is developed for adding **trendline and confidence interval** of **linear or nonlinear regression** model, and
    **showing equation, R square, and P value**  to 'ggplot' as simple as possible. 

<br>For a general overview of the methods used in this package, 
	see Ritz and Streibig (2008) <doi:10.1007/978-0-387-09616-2> and 
	Greenwell and Schubert Kabban (2014) <doi:10.32614/RJ-2014-009>.

### 3.2 ggtrendline function

The built-in 'ggtrendline()' function includes the following models:<br>
<br>"line2P", formula as: y = a\*x + b;
<br>"line3P", y = a\*x^2 + b\*x + c;
<br>"log2P" , y = a\*ln(x) + b; 
<br>"exp2P", y = a\*exp(b\*x); 
<br>"exp3P", y = a\*exp(b\*x) + c;
<br>"power2P", y = a\*x^b;
<br>"power3P", y = a\*x^b + c.

### 3.3 stat_eq and stat_rrp functions

**The built-in 'stat_eq()' and 'stat_rrp()' functions can be used separately, i.e., not together with 'ggtrendline()' function.**

To see more details, you can run the following R code if you have the "ggtrendline" package installed:

    library(ggtrendline)
    ?ggtrendline
	?stat_eq
	?stat_rrp

## 4. Contact

- Bugs and feature requests can be filed to https://github.com/PhDMeiwp/ggtrendline/issues. 
- BTW, [Pull requests](https://github.com/PhDMeiwp/ggtrendline/pulls) are also welcome.

## 5. Acknowledgements

We would like to express our special thanks to **Uwe Ligges, Gregor Seyer, and CRAN team** for their valuable comments to the 'ggtrendline' package.
