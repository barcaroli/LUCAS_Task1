# # Render an html of this script
library(rmarkdown)
options("knitr.duplicate.label" = "allow")
knitr::opts_chunk$set(warning = FALSE, echo=FALSE)
rmarkdown::render("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/10.plot_confidence_EU22.R",
# rmarkdown::render("D:\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES/10.plot_confidence_EU22.R",
                  # output_format=html_document(df_print="paged", theme="flatly", highlight="haddock",
                                              output_format=word_document(df_print="paged",
                                              fig_width = 8, fig_height = 6))
rmarkdown::render("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/10.plot_confidence_EU27.R",
                  # rmarkdown::render("D:\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES/10.plot_confidence_EU27.R",
                  # output_format=html_document(df_print="paged", theme="flatly", highlight="haddock",
                  output_format=word_document(df_print="paged",
                                              fig_width = 8, fig_height = 6))
rmarkdown::render("D:/Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES light/12.Compare_estimates_plots.R",
                  # rmarkdown::render("D:\\Google Drive/LUCAS 2025/Task 1 - ESTIMATES/2.TWO PHASES/10.plot_confidence_EU27.R",
                  # output_format=html_document(df_print="paged", theme="flatly", highlight="haddock",
                  output_format=word_document(df_print="paged",
                                              fig_width = 8, fig_height = 6))


