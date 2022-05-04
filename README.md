# ggtrendline: an R package for adding trendline and confidence interval to ggplot

<a href="https://sm.ms/image/JGF8MWVbRwh2X17" target="_blank"><img src="https://s2.loli.net/2022/04/13/JGF8MWVbRwh2X17.png" height="300" align="right"></a>

[![cran version](http://www.r-pkg.org/badges/version/ggtrendline)](http://cran.rstudio.com/web/packages/ggtrendline) 
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/ggtrendline)](https://github.com/metacran/cranlogs.app)

## 1. Installing "ggtrendline" package in R

- Get the released version from CRAN:

	install.packages("ggtrendline")

- Or the development version from Github:

     install.packages("devtools")
	 library(devtools)  
	 
     install_github("PhDMeiwp/ggtrendline@master", force = TRUE)
	 
	 library(ggtrendline)


## 2. Using "ggtrendline" package
	
     library(ggplot2)
	 library(ggtrendline)
     x <- c(1, 3, 6, 9,  13,   17)
     y <- c(5, 8, 11, 13, 13.2, 13.5)

### [case 1]line2P example,default

     ggtrendline(x, y, model = "line2P")  

<a href="https://sm.ms/image/4pICPTDh2gJxuFK" target="_blank"><img src="https://s2.loli.net/2022/04/13/4pICPTDh2gJxuFK.png" ></a>

<a href="https://sm.ms/image/JOrRHYWGy8EMofs" target="_blank"><img src="https://s2.loli.net/2022/04/13/JOrRHYWGy8EMofs.png" ></a>


### [case 2] line3P example, add geom_point()

     ggtrendline(x, y, model = "line3P") + geom_point(aes(x, y)) + theme_bw()

<a href="https://sm.ms/image/Dp6Lt58jf9rmaNW" target="_blank"><img src="https://s2.loli.net/2022/04/13/Dp6Lt58jf9rmaNW.png" ></a>

### [case 3] log2P example, CI lines only, without CI filling

     ggtrendline(x, y, model = "log2P", CI.fill = NA) + geom_point(aes(x, y))+ theme_classic() 
	
<a href="https://sm.ms/image/VuDypF3tZWzK9B5" target="_blank"><img src="https://s2.loli.net/2022/04/13/VuDypF3tZWzK9B5.png" ></a>

### [case 4]  exp2P example, set the regression line and geom_point()

     ggtrendline(x, y, model = "exp2P", linecolor = "blue", linetype = 1, linewidth = 1) + 
             geom_point(aes(x, y), color = "blue", shape = 1, size = 3)  

<a href="https://sm.ms/image/TF48LAtiIHB1ukd" target="_blank"><img src="https://s2.loli.net/2022/04/13/TF48LAtiIHB1ukd.png" ></a>
		
### [case 5] exp3P example, set confidence interval

     ggtrendline(x, y, model = "exp3P", CI.level = 0.99, 
                CI.fill = "red", CI.alpha = 0.1, CI.color = NA, CI.lty = 2, CI.lwd = 1.5) + 
             geom_point(aes(x, y)) 

<a href="https://sm.ms/image/6ul7toUOWkhcw3s" target="_blank"><img src="https://s2.loli.net/2022/04/13/6ul7toUOWkhcw3s.png" ></a>
		
### [case 6] one trendline with different points belonged to multiple groups.

		library(ggplot2)
		library(ggtrendline)
		data("iris")
		x <- iris$Petal.Width
		y <- iris$Petal.Length
		
		group <- iris$Species
		
		ggtrendline(x,y,"exp3P") + geom_point(aes(x,y,color=group))

<a href="https://sm.ms/image/MYa9WHqlALfFXeD" target="_blank"><img src="https://s2.loli.net/2022/05/04/MYa9WHqlALfFXeD.png" ></a>

## 3. Details
To see more details, you can run the following R code if you have the "ggtrendline" package installed:

    library(ggtrendline)
    ?ggtrendline()

## 4. Contact
- Bugs and feature requests can be filed to https://github.com/PhDMeiwp/ggtrendline/issues. 
- BTW, [Pull requests](https://github.com/PhDMeiwp/ggtrendline/pulls) are also welcome.