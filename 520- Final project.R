# HMS 520 Final Project
# Members: Rebecca Gilbert, Asrat Arja Wolde, Susan Campbell

library(dplyr)
library(ggplot2)
library(usmap)
library(rigr)

# read in 2018 data for county-level social vulnerability indices from CDC (CDC SVI)
# based on documentation, the value "-999" represents missing data, so we recoded as NAs
svi <- read.csv("C:/Users/rebec/OneDrive/UW Graduate School/Programming/SVI_2018_US_county.csv", 
                na.strings = "-999")

# link to data documentation and dictionary
# https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2018.html

############ Data processing ############

# check types of data, number rows, number columns, 
dim(svi)
# see summary statistics for each column
summary(svi)

# rename columns to be all lowercase for simplicity
names(svi) <- tolower(names(svi))

# explore NAs
any_na_in_rows <- apply(svi, 1, function(row) any(is.na(row)))
sum(any_na_in_rows) # just 1 row has any NAs

# discarding data for the single county with significant missing data because we believe the data may not be reliable
fips_to_discard <- svi[which(any_na_in_rows == TRUE), "fips"] # for 2018, Rio Arriba county in NM discarded
svi <- filter(svi, fips != fips_to_discard)

# TODO: fix xthis!!
# select columns that give raw counts (i.e., those that begin with "e_") and in the composite indicator columns (beginning with "spl_")
# select_raw_counts_cols <- function(data) {
#   data <- select("state" | starts_with(c("e_", "spl_")))
#   return(data)
# }

# in reading through documentation, the following variables were of interest to us
cols_of_interest <- c("e_totpop", # total population
                      "e_pov", # people below 1.5 times poverty line
                      "e_unemp", # number unemployed
                      "e_pci", # per capita income
                      "e_nohsdp", # no high school diploma
                      "e_disabl", # non-institutionalized population with a disability
                      "spl_theme1", 
                      "spl_theme2", 
                      "spl_theme3", 
                      "spl_theme4",
                      "spl_themes")

# aggregate by state, summing county data in 
svi_state <- svi %>%
  select(state, cols_of_interest) %>%
  group_by(state) %>%
  summarize_all(sum)

# test an example to make sure aggregation by state is working as expected
svi_alabama <- svi %>%
  filter(state == "ALABAMA") %>%
  select(state, e_totpop) %>%
  summarize(sum = sum(e_totpop))

# create a dataframe with data for just Washington to run additional county-level analyses
svi_wa <- filter(svi, state == "WASHINGTON")


############ Exploratory data analysis ############## 

# make a grid of scatterplots of all variables of interest
pairs(svi_state[, cols_of_interest])

# function that will calculate percent of total population with each feature
calculate_percents <- function(data, variable_names, denominator_col) {

  for (variable in variable_names) {
    # Create a new column with the calculated percentage
    new_col_name <- paste0("percent_", sub("^e_", "", variable))
    data[[new_col_name]] <- data[[variable]] / data[[denominator_col]] * 100
  }

  return(data)
}

# see in final_project_functions.R
svi_state <- calculate_percents(svi_state, 
                                cols_of_interest[2:6], 
                                "e_totpop") %>%
  select("state" | "spl_themes" | starts_with("percent_"))

# choose some relationships to explore
# relationship between lack of HS diploma and percent poverty
# relationship between lack of HS diploma and unemployment


################# Mapping ###################

plot_map <- function(data, map_var, legend_title = "Percent with feature", include_geo = FALSE, var_color = "darkred") {

  if(include_geo == FALSE) {
    map <- plot_usmap(data = data, values = map_var, color = "black") +
      scale_fill_continuous(low = "white", high = var_color, name = legend_title, label = scales::comma) +
      theme(legend.position = "right")
  } else {
      map <- plot_usmap(data = data, values = map_var, include = include_geo, color = "black") +
        scale_fill_continuous(low = "white", high = var_color , name = legend_title, label = scales::comma) +
        theme(legend.position = "right")
  }

  return(map)
}

# plot map of features by US counties
pov_by_county <- plot_map(data = svi, 
                          map_var = "ep_pov", 
                          legend_title = "Percent below 1.5x poverty line (2018)")
nohsdp_by_county <- plot_map(data = svi, 
                             map_var = "ep_nohsdp", 
                             legend_title = "Percent without HS diploma (2018)", 
                             var_color = "blue")
unemp_by_county <- plot_map(data = svi, 
                            map_var = "ep_unemp", 
                            legend_title = "Percent unemployment (2018)", 
                            var_color = "green")

# plot map of features by US states
pov_by_state <- plot_map(data = svi_state, 
                         map_var = "percent_pov", 
                         legend_title = "Percent below 1.5x poverty line (2018)")
nohsdp_by_state <- plot_map(data = svi_state, 
                            map_var = "percent_nohsdp", 
                            legend_title = "Percent without HS diploma (2018)", 
                            var_color = "blue")
unemp_by_state <- plot_map(data = svi_state, 
                           map_var = "percent_unemp", 
                           legend_title = "Percent unemployment (2018)", 
                           var_color = "green")

# plot map of features by counties in Washington state
pov_wa <- plot_map(data = svi_wa, 
                   map_var = "ep_pov", 
                   legend_title = "Percent below 1.5x poverty line (2018)", 
                   include_geo = "WA")
nohsdp_wa <- plot_map(data = svi_wa, 
                      map_var = "ep_nohsdp", 
                      legend_title = "Percent without HS diploma (2018)", 
                      include_geo = "WA", 
                      var_color = "blue")
unemp_wa <- plot_map(data = svi_wa, 
                     map_var = "ep_unemp", 
                     legend_title = "Percent unemployment (2018)", 
                     include_geo = "WA", 
                     var_color = "green")


################## Modeling ##################

#Check the normality of the distribution of the variables

#Histogram

plot_hist_counts <- function(data) {
  par(mfrow = c(2, 3))  # Set up a 2x3 grid for multiple plots
  hist(data$e_pov, main = "Histogram - Number below 1.5x poverty line (2018)")
  hist(data$e_unemp, main = "Histogram - Number unemployed")
  hist(data$e_pci, main = "Histogram - Per capita income")
  hist(data$e_nohsdp, main = "Histogram - Number no high school diploma")
  hist(data$e_disabl, main = "Histogram - Number non-institutionalized population with a disability")
}

plot_hist_percents <- function(data) {
  par(mfrow = c(2, 3))  # Set up a 2x3 grid for multiple plots
  hist(data$percent_pov, main = "Histogram - Number below 1.5x poverty line (2018)")
  hist(data$percent_unemp, main = "Histogram - Number unemployed")
  hist(data$percent_pci, main = "Histogram - Per capita income")
  hist(data$percent_nohsdp, main = "Histogram - Number no high school diploma")
  hist(data$percent_disabl, main = "Histogram - Number non-institutionalized population with a disability")
}

plot_hist_counts(svi)
plot_hist_percents(svi_state)

#Q-Q plot

plot_qq_counts <- function(data) {
  par(mfrow = c(2, 3))  # Set up a 2x3 grid for multiple plots
  qqnorm(data$e_pov, main = "Q-Q Plot - Percent below 1.5x poverty line (2018)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(data$e_pov)
  
  qqnorm(data$e_unemp, main = "Q-Q Plot - Number unemployed", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(data$e_unemp)
  
  qqnorm(data$e_pci, main = "Q-Q Plot - Per capita income", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(data$e_pci)
  
  qqnorm(data$e_nohsdp, main = "Q-Q Plot - No high school diploma", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(data$e_nohsdp)
  
  qqnorm(data$e_disabl, main = "Q-Q Plot - Non-institutionalized population with a disability", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(data$e_disabl)
}

plot_qq_percents <- function(data) {
  par(mfrow = c(2, 3))  # Set up a 2x3 grid for multiple plots
  qqnorm(data$percent_pov, main = "Q-Q Plot - Percent below 1.5x poverty line (2018)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(data$percent_pov)
  
  qqnorm(data$percent_unemp, main = "Q-Q Plot - Percent unemployed", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(data$percent_unemp)
  
  qqnorm(data$percent_pci, main = "Q-Q Plot - Per capita income", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(data$percent_pci)
  
  qqnorm(data$percent_nohsdp, main = "Q-Q Plot - Percent with no high school diploma", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(data$percent_nohsdp)
  
  qqnorm(data$percent_disabl, main = "Q-Q Plot - Percent non-institutionalized population with a disability", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(data$percent_disabl)
}

plot_qq_counts(svi)
plot_qq_percents(svi_state)


plot_scatter_with_lm <- function(data, x_var, y_var, plot_title = NULL) {

  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
    labs(title = plot_title,
         x = x_var,
         y = y_var,
         log = "xy")
  
}


######################### High school education and poverty ######################

plot_scatter_with_lm(data = svi, 
             x_var = "e_nohsdp", 
             y_var = "e_pov", 
             plot_title = "For US counties: relationship between counts of people lacking high school diploma and counts of people below 1.5 times poverty line")
plot_scatter_with_lm(data = svi_state, 
             x_var = "percent_nohsdp", 
             y_var = "percent_pov", 
             plot_title = "For US states: relationship between percent of people who lack of high school diploma and percent below 1.5 times poverty line")
plot_scatter_with_lm(data = svi_wa, 
             x_var = "e_nohsdp", 
             y_var = "e_pov", 
             plot_title = "For WA counties: relationship between counts of people lacking high school diploma and counts of people below 1.5 times poverty line")


# Fit linear regression model for US counties
lm_nohsdp <- lm(e_pov ~ e_nohsdp, data = svi)
# Print the summary of the linear regression model
summary(lm_nohsdp)
lm1_summary <- summary(lm_nohsdp)
print(lm1_summary)


# Fit linear regression model for Washington
lm_nohsdp_washington <- lm(e_pov ~ e_nohsdp, data = svi_wa)
# Print the summary of the linear regression model for Washington
summary(lm_nohsdp_washington)


######################### High school education and unemployment ######################

plot_scatter_with_lm(data = svi, 
             x_var = "e_nohsdp", 
             y_var = "e_unemp", 
             plot_title = "For US counties: relationship between counts of people lacking high school diploma and counts of people unemployed")
plot_scatter_with_lm(data = svi_state, 
             x_var = "percent_nohsdp", 
             y_var = "percent_unemp", 
             plot_title = "For US states: relationship between percent of people who lack of high school diploma and percent unemployed")
plot_scatter_with_lm(data = svi_wa, 
             x_var = "e_nohsdp", 
             y_var = "e_unemp", 
             plot_title = "For WA counties: relationship between counts of people lacking high school diploma and counts of people unemployed")


# Fit linear regression model
lm_unemp <- lm(e_unemp ~ e_nohsdp, data = svi)
# Print the summary of the linear regression model
summary(lm_unemp)
lm2_summary <- summary(lm_unemp)
print(lm2_summary)


# Fit linear regression model for WASHINGTON
lm_nohsdp_washington <- lm(e_unemp ~ e_nohsdp, data = svi_wa)
# Print the summary of the linear regression model for Washington
summary(lm_nohsdp_washington)


################# Mapping ###################

# TODO: consider removing from this script
plot_map <- function(data, map_var, legend_title = "Percent with feature", include_geo = FALSE, var_color = "darkred") {
  
  if(include_geo == FALSE) {
    map <- plot_usmap(data = data, values = map_var, color = "black") +
      scale_fill_continuous(low = "white", high = var_color, name = legend_title, label = scales::comma) +
      theme(legend.position = "right")
  } else {
    map <- plot_usmap(data = data, values = map_var, include = include_geo, color = "black") +
      scale_fill_continuous(low = "white", high = var_color , name = legend_title, label = scales::comma) +
      theme(legend.position = "right")
  }
  
  return(map)
}

# plot map of features by US counties
pov_by_county <- plot_map(data = svi, 
                          map_var = "ep_pov", 
                          legend_title = "Percent below 1.5x poverty line (2018)")
nohsdp_by_county <- plot_map(data = svi, 
                             map_var = "ep_nohsdp", 
                             legend_title = "Percent without HS diploma (2018)", 
                             var_color = "blue")
unemp_by_county <- plot_map(data = svi, 
                            map_var = "ep_unemp", 
                            legend_title = "Percent unemployment (2018)", 
                            var_color = "green")

# plot map of features by US states
pov_by_state <- plot_map(data = svi_state, 
                         map_var = "percent_pov", 
                         legend_title = "Percent below 1.5x poverty line (2018)")
nohsdp_by_state <- plot_map(data = svi_state, 
                            map_var = "percent_nohsdp", 
                            legend_title = "Percent without HS diploma (2018)", 
                            var_color = "blue")
unemp_by_state <- plot_map(data = svi_state, 
                           map_var = "percent_unemp", 
                           legend_title = "Percent unemployment (2018)", 
                           var_color = "green")

# plot map of features by counties in Washington state
pov_wa <- plot_map(data = svi_wa, 
                   map_var = "ep_pov", 
                   legend_title = "Percent below 1.5x poverty line (2018)", 
                   include_geo = "WA")
nohsdp_wa <- plot_map(data = svi_wa, 
                      map_var = "ep_nohsdp", 
                      legend_title = "Percent without HS diploma (2018)", 
                      include_geo = "WA", 
                      var_color = "blue")
unemp_wa <- plot_map(data = svi_wa, 
                     map_var = "ep_unemp", 
                     legend_title = "Percent unemployment (2018)", 
                     include_geo = "WA", 
                     var_color = "green")





