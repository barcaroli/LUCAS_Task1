# ----------------------------
# Render html and word (plots)
library(rmarkdown)
# ----------------------------

options("knitr.duplicate.label" = "allow")
knitr::opts_chunk$set(warning = FALSE, echo=FALSE)

current_wd <- getwd()
setwd(paste0(current_wd,"/10.Plots"))


rmarkdown::render("A.plot_confidence_EU22.R",
                  output_format=word_document(df_print="paged",
                                              fig_width = 8, fig_height = 6))
rmarkdown::render("A.plot_confidence_EU22.R",
                  output_format=html_document(df_print="paged", theme="flatly", highlight="haddock",
                                              fig_width = 8, fig_height = 6))

rmarkdown::render("B.plot_confidence_EU27.R",
                  output_format=word_document(df_print="paged",
                                              fig_width = 8, fig_height = 6))
rmarkdown::render("B.plot_confidence_EU27.R",
                  output_format=html_document(df_print="paged", theme="flatly", highlight="haddock",
                                              fig_width = 8, fig_height = 6))


rmarkdown::render("C.Compare_estimates_plots.R",
                  output_format=word_document(df_print="paged",
                                              fig_width = 8, fig_height = 6))
rmarkdown::render("C.Compare_estimates_plots.R",
                  output_format=html_document(df_print="paged", theme="flatly", highlight="haddock",
                                              fig_width = 8, fig_height = 6))


rmarkdown::render("D.Check_areas.R",
                  output_format=word_document(df_print="paged",
                                              fig_width = 8, fig_height = 6))
rmarkdown::render("D.Check_areas.R",
                  output_format=html_document(df_print="paged", theme="flatly", highlight="haddock",
                                              fig_width = 8, fig_height = 6))


setwd(current_wd)