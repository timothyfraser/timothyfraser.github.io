library(rmarkdown)
rmarkdown::render("docs/index.Rmd")
browseURL("docs/index.html")

setwd("C:/Users/tmf77/timothyfraser.github.io")
rmarkdown::render("docs/cv/index.Rmd")
browseURL("docs/cv/index.html")
