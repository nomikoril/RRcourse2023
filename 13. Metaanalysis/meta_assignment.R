library(readxl)
install.packages("meta")
library(meta)
library(dplyr)

metaanalysis_data <- read_excel("C:/Users/nomin/OneDrive/Desktop/DSBA/4.2. Reproducible Research/Lab 4/RRcourse2023/13. Metaanalysis/data/metaanalysis_data.xlsx")

m.ass <- metacont(data=metaanalysis_data,
                 n.e=metaanalysis_data$N_boys,
                 mean.e=metaanalysis_data$Mean_boys_play_female,
                 sd.e=metaanalysis_data$SD_boys_play_female,
                 n.c=metaanalysis_data$N_boys,
                 mean.c=metaanalysis_data$Mean_boys_play_male,
                 sd.c=metaanalysis_data$SD_boys_play_male,
                  
                  studlab=paste(Study),
                  comb.fixed = TRUE,
                  comb.random = TRUE,
)
m.ass
m.ass %>% forest(sortvar=TE)
m.ass %>% funnel()
contour_levels <- c(0.90, 0.95, 0.99)
contour_colors <- c("darkblue", "blue", "lightblue")
funnel(m.ass, contour = contour_levels, col.contour = contour_colors)
legend("topright", c("p < 0.10", "p < 0.05", "p < 0.01"), bty = "n", fill = contour_colors)

m.ass2 <- metacont(data=metaanalysis_data,
                  n.e=metaanalysis_data$N_girls,
                  mean.e=metaanalysis_data$Mean_girls_play_male,
                  sd.e=metaanalysis_data$SD_girls_play_male,
                  n.c=metaanalysis_data$N_girls,
                  mean.c=metaanalysis_data$Mean_girls_play_female,
                  sd.c=metaanalysis_data$SD_girls_play_female,
                  
                  studlab=paste(Study),
                  comb.fixed = TRUE,
                  comb.random = TRUE,
)
m.ass2
m.ass2 %>% forest(sortvar=TE)
m.ass2 %>% funnel()
contour_levels <- c(0.90, 0.95, 0.99)
contour_colors <- c("darkblue", "blue", "lightblue")
funnel(m.ass2, contour = contour_levels, col.contour = contour_colors)
legend("topright", c("p < 0.10", "p < 0.05", "p < 0.01"), bty = "n", fill = contour_colors)


m.ass %>% metareg(metaanalysis_data$`Female authors`+metaanalysis_data$`Neutral toys`)

m.ass2 %>% metareg(metaanalysis_data$`Female authors` + metaanalysis_data$Setting)


