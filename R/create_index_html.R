# knitr::knit2html(input = "Crop_production.Rmd", output = "index.html")
#rmarkdown::render(input = "Crop_production.Rmd", 
                  # output_format = bookdown::html_document2(),
                  # output_file = "index")
file.copy("Crop_production.Rmd", "index.html")
