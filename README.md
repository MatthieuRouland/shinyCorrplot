Correlation Matrix in Shiny with *corrplot*
===========

This [Shiny](http://shiny.rstudio.com/) application aims to provide an interactive experience to the awesome [corrplot](http://cran.r-project.org/web/packages/corrplot) R package. [Correlation](http://en.wikipedia.org/wiki/Correlation_and_dependence) is one of the most commonly used metrics to depict statistical relationships between two variables. It's often used in exploratory analysis, feature selection and many other modelling steps.

## Overview

### Data Input
This adapted version of Shiny Corrplot correlation matrix by Matthieu Rouland allow the usage of .xlsx excel files. 

### Variable Selection
You can use checkboxes to pick only the variables you want to see. Furthremore, you can drag variables around to manually reorder the columns. This will have an obvious impact if you choose to see correlation in their original order. However it can also affect some reordering algorithms such as AOE.

The reordering by checkbox updates the plot in realtime while the *selectize* style only udpates when you drop the selection. When the server is powerful enough, the realtime update can be very helpful. Otherwise, it will seem visually disruptive.

### Correlation
The `cor` function in `stats` package is used thus you have the freedom to choose from *pearson*, *kendall*, and *spearman* correlation and some control over how to treat NA values.

### Corrplot
We have include abilities to generate most examples in [corrplot vignette](http://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html). In particular, you can make [statstical inference](http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient#Inference) on correlation significance and/or confidence intervals.

## Credits
Big thanks to Saurfang who made the first version of this wonderfull shiny app. As following to the first release of the app by Saurfang, thanks goes to [corrplot](http://cran.r-project.org/web/packages/corrplot) and [sortList example](https://gist.github.com/trestletech/9691459) for inspiration, and [Developing Data Products](https://class.coursera.org/devdataprod-005) on Coursera for motivation and this great opportunity.

## LICENSE
MIT
