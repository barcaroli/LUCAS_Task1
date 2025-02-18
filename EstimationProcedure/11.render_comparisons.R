# ----------------------------
# Render html and word (plots)
library(rmarkdown)
# ----------------------------

options("knitr.duplicate.label" = "allow")
knitr::opts_chunk$set(warning = FALSE, echo=FALSE)

current_wd <- getwd()
setwd(paste0(current_wd,"./11.Comparisons"))


rmarkdown::render("A.Compare_estimates_tables.Rmd",
                  output_format=html_document(df_print="paged", theme="flatly", highlight="haddock",
                                              fig_width = 8, fig_height = 6))


rmarkdown::render("B.Compare_estimates_tables__by_NUTS0.Rmd",
                  output_format=html_document(df_print="paged", theme="flatly", highlight="haddock",
                                              fig_width = 8, fig_height = 6))



setwd(current_wd)