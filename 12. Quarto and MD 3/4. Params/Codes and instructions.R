# Rendering
library(quarto)

quarto_render("QMD_class_3_4.qmd", execute_params = list(
  year = 2017,
  region = "Asia",
  printcode = FALSE,
  data = "file.csv"
))

# Name change

reg <- "Asia"
y <- 2049

quarto_render("C:/Users/nomin/OneDrive/Desktop/DSBA/4.2. Reproducible Research/Lab 4/RRcourse2023/Quarto and MD 3/4. Params/QMD_class_3_4.qmd", execute_params = list(
  year = y,
  region = reg,
  printcode = FALSE,
  data = "file.csv"
), output_file = paste0("Game of Thrones Report-", "Season-", y, ".html"))


