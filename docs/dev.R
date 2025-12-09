library(rmarkdown)
rmarkdown::render("docs/index.Rmd")
browseURL("docs/index.html")

rmarkdown::render("docs/cv/index.Rmd")
browseURL("docs/cv/index.html")
