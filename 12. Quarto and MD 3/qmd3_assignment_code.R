# Rendering

library(quarto)


# Name change

for (i in 1:8) {
  
  quarto_render("C:/Users/nomin/OneDrive/Desktop/DSBA/4.2. Reproducible Research/Lab 4/RRcourse2023/Quarto and MD 3/qmd3_assignment_code.qmd", execute_params = list(
    season = i,
    printcode = TRUE
  ), output_file = paste0("Game of Thrones Report-", "Season-", i, ".html"))
  
}


quarto_render("qmd3_assignment_code.qmd", execute_params = list(
  season = 1,
  printcode = FALSE,
  data = 
), output_file = paste0("Game of Thrones Report-", "Season-", season, ".html"))
