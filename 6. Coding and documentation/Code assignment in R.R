#library
library(readxl)   
library(purrr)
library(stringr)
library(dplyr)
library(Hmisc)


# Sets the path to the parent directory of RR classes
setwd("C:/Users/nomin/OneDrive/Desktop/DSBA/4.2. Reproducible Research/Lab 4/RRcourse2023/6. Coding and documentation")

#   Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using:  https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data = read.csv("Data/onet_tasks.csv")
# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)
                 


isco_list <- map(set_names(excel_sheets("Data/Eurostat_employment_isco.xlsx")),
               read_excel, path = "Data/Eurostat_employment_isco.xlsx"
)
purrr::map(names(isco_list),
           ~assign(.x, isco_list[[.x]], envir = .GlobalEnv)
)



# We will focus on three countries, but perhaps we could clean this code to allow it
# to easily run for all the countries in the sample?

# Let's merge all these datasets. We'll need a column that stores the occupation categories:

ISCO1$ISCO <- 1
ISCO2$ISCO <- 2
ISCO3$ISCO <- 3
ISCO4$ISCO <- 4
ISCO5$ISCO <- 5
ISCO6$ISCO <- 6
ISCO7$ISCO <- 7
ISCO8$ISCO <- 8
ISCO9$ISCO <- 9

# and this gives us one large file with employment in all occupations.
all_data <- rbind(ISCO1, ISCO2, ISCO3, ISCO4, ISCO5, ISCO6, ISCO7, ISCO8, ISCO9)
library(dplyr)
totals<-all_data %>% group_by(TIME) %>% summarise(total_belgium=sum(Belgium),
                                                  total_czechia=sum(Czechia),
                                                  total_denmark=sum(Denmark),
                                                  total_spain=sum(Spain),
                                                  total_italy=sum(Italy),
                                                  total_lithuania=sum(Lithuania),
                                                  total_poland=sum(Poland),
                                                  total_finland=sum(Finland),
                                                  total_sweden=sum(Sweden))

all_data<-merge(all_data, totals, by = "TIME", all = T)

# And this will give us shares of each occupation among all workers in a period-country
all_data<-all_data %>% mutate(share_belgium=Belgium/total_belgium,
                                    share_czechia=Czechia/total_czechia,
                                    share_denmark=Denmark/total_denmark,
                                    share_spain=Spain/total_spain,
                                    share_italy=Italy/total_italy,
                                    share_lithuania=Lithuania/total_lithuania,
                                    share_poland=Poland/total_poland,
                                    share_finland=Finland/total_finland,
                                    share_sweden=Sweden/total_sweden)






# Now let's look at the task data. We want the first digit of the ISCO variable only


task_data$ISCO08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata <-aggregate(task_data, by=list(task_data$ISCO08_1dig),
                    FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

#These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

#Let's combine the data.


combined <- left_join(all_data, aggdata, by = c("ISCO" = "ISCO08_1dig"))

# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

#install.packages("Hmisc")




# first task item

temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_belgium)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_belgium) %>% sqrt()
combined$std_Belgium_t_4A2a4 = (combined$t_4A2a4-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_poland)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_poland) %>% sqrt()
combined$std_Poland_t_4A2a4 = (combined$t_4A2a4-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_spain)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_spain) %>% sqrt()
combined$std_Spain_t_4A2a4 = (combined$t_4A2a4-temp_mean)/temp_sd

# second task item
temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_belgium)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_belgium) %>% sqrt()
combined$std_Belgium_t_4A2b2 = (combined$t_4A2b2-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_poland)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_poland) %>% sqrt()
combined$std_Poland_t_4A2b2 = (combined$t_4A2b2-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_spain)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_spain) %>% sqrt()
combined$std_Spain_t_4A2b2 = (combined$t_4A2b2-temp_mean)/temp_sd

# third task item
temp_mean <- wtd.mean(combined$t_4A4a1 , combined$share_belgium)
temp_sd <- wtd.var(combined$t_4A4a1 , combined$share_belgium) %>% sqrt()
combined$std_Belgium_t_4A4a1  = (combined$t_4A4a1 -temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A4a1 , combined$share_poland)
temp_sd <- wtd.var(combined$t_4A4a1 , combined$share_poland) %>% sqrt()
combined$std_Poland_t_4A4a1  = (combined$t_4A4a1 -temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A4a1 , combined$share_spain)
temp_sd <- wtd.var(combined$t_4A4a1 , combined$share_spain) %>% sqrt()
combined$std_Spain_t_4A4a1  = (combined$t_4A4a1 -temp_mean)/temp_sd

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

combined$Belgium_NRCA <- combined$std_Belgium_t_4A2a4 + combined$std_Belgium_t_4A2b2 + combined$std_Belgium_t_4A4a1 
combined$Poland_NRCA <- combined$std_Poland_t_4A2a4 + combined$std_Poland_t_4A2b2 + combined$std_Poland_t_4A4a1 
combined$Spain_NRCA <- combined$std_Spain_t_4A2a4 + combined$std_Spain_t_4A2b2 + combined$std_Spain_t_4A4a1 

# And we standardise NRCA in a similar way.
temp_mean <- wtd.mean(combined$Belgium_NRCA, combined$share_belgium)
temp_sd <- wtd.var(combined$Belgium_NRCA, combined$share_belgium) %>% sqrt()
combined$std_Belgium_NRCA = (combined$Belgium_NRCA-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$Poland_NRCA, combined$share_poland)
temp_sd <- wtd.var(combined$Poland_NRCA, combined$share_poland) %>% sqrt()
combined$std_Poland_NRCA = (combined$Poland_NRCA-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$Spain_NRCA, combined$share_spain)
temp_sd <- wtd.var(combined$Spain_NRCA, combined$share_spain) %>% sqrt()
combined$std_Spain_NRCA = (combined$Spain_NRCA-temp_mean)/temp_sd

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.
combined$multip_Spain_NRCA <- (combined$std_Spain_NRCA*combined$share_spain)
combined$multip_Belgium_NRCA <- (combined$std_Belgium_NRCA*combined$share_belgium)
combined$multip_Poland_NRCA <- (combined$std_Poland_NRCA*combined$share_poland)

# Step 2: sum it up (it basically becomes another weighted mean)
agg_Spain <-aggregate(combined$multip_Spain_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)
agg_Belgium <-aggregate(combined$multip_Belgium_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)
agg_Poland <-aggregate(combined$multip_Poland_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)

# We can plot it now!
plot(agg_Poland$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Poland$Group.1[seq(1, 40, 3)])

plot(agg_Spain$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Spain$Group.1[seq(1, 40, 3)])

plot(agg_Belgium$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Belgium$Group.1[seq(1, 40, 3)])


# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment

