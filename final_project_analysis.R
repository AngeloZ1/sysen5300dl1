# Project: Preliminary Results
# Statistical Process Control

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("readxl")

library(readr)
library(tidyverse)
library(readxl)

# Input Parameters
# Raw Data
all_data <- read_excel("CombinedAllData.xlsx")

# Modifiable Parameters
growth_factor = 0.05 # Used for calculating statistics on new scenario
ci = 0.95 # Desired confidence Interval
alpha = 1-((1-ci)/2) # input for qnorm() based on desired ci

## Determine Opportunity Cost for Unaffordable Housing 

# Unaffordable Housing is defined as the housing to income ratio (HIR) exceeding
# 3.0, which is defined as the affordability ratio (AR)
  # Ex. Mean Income of 100,000 and Home value of 500,000 is a ratio of 5
# In this example, the HIR discrepancy with affordable housing is 2 (5-3 = 2)
# The HIR, AR, and OPCR can be used to calculate an income that would be necessary 
# to achieve the AR, with the given data.
# The formula is as follows:
  # (HIR-AR) = Housing Cost / (Income + )
opportunity_cost = all_data %>%
  mutate(Population = Population,
         HC = `Zillow Home Value (Average)`,
         I = `Mean Income`,
         HIR = HC/I,
         OP_raw = HC/3 - HC/HIR, # Also HC/3 - I
         OP = ifelse(OP_raw < 0, 0, OP_raw),
          `OP Cost (m$)` = (Population*OP)/1000000) # per million $

all_data = all_data %>% mutate(`OP Cost (m$)` = opportunity_cost$`OP Cost (m$)`)

# Cap the increases for a new scenario
all_data_capped <- all_data %>%
  arrange(`Zip Code`, Year) %>%
  group_by(`Zip Code`) %>%
  group_modify(~ {
    df <- .x
    # initialize capped value column
    df$capped_value <- df$`Zillow Home Value (Average)`
    # perform iterative capping
    for (i in 2:nrow(df)) {
      raw_value <- df$`Zillow Home Value (Average)`[i]
      prev_capped <- df$capped_value[i - 1]
      increase <- raw_value / prev_capped - 1
      # apply cap
      if (increase > growth_factor) {
        df$capped_value[i] <- prev_capped * (1 + growth_factor)
      } else {
        df$capped_value[i] <- raw_value
      }
    }
    
    # (optional) replace the Zillow column directly
    df$`Zillow Home Value (Average)` <- df$capped_value
    
    df
  }) %>%
  ungroup()

# Create a new data frame with calculated percent changes and percent differences
all_data_perc_change <- all_data %>%
  arrange(`Zip Code`, Year) %>%
  group_by(`Zip Code`) %>%
  mutate(
    # New column for income percent difference
    Inc_PC = (`Mean Income` - lag(`Mean Income`)) / lag(`Mean Income`) * 100,
    # New column for population percent difference
    Pop_PC = (Population - lag(Population)) / lag(Population) * 100,
    # New column for home cost percent difference
    HC_PC = (`Zillow Home Value (Average)` - lag(`Zillow Home Value (Average)`)) /
      lag(`Zillow Home Value (Average)`) * 100,
    # Calculate difference between home cost and income in terms of percent changes
    Inc_HC_diff = HC_PC-Inc_PC) %>%
  filter(Year != 2011) %>% # Filter out 2011 because it's the first year, so no calculated change
  ungroup() %>%
  select(`Year`,`Zip Code`,County,Inc_PC,Pop_PC,HC_PC,Inc_HC_diff)

# Create a new data frame with calculated percent changes and percent differences
all_data_perc_change_new <- all_data_capped %>%
  arrange(`Zip Code`, Year) %>%
  group_by(`Zip Code`) %>%
  mutate(
    # New column for income percent difference
    Inc_PC = (`Mean Income` - lag(`Mean Income`)) / lag(`Mean Income`) * 100,
    # New column for population percent difference
    Pop_PC = (Population - lag(Population)) / lag(Population) * 100,
    # New column for home cost percent difference
    HC_PC = (`Zillow Home Value (Average)` - lag(`Zillow Home Value (Average)`)) /
      lag(`Zillow Home Value (Average)`) * 100,
    # Calculate difference between home cost and income in terms of percent changes
    Inc_HC_diff = HC_PC-Inc_PC) %>%
  filter(Year != 2011) %>% # Filter out 2011 because it's the first year, so no calculated change
  ungroup() %>%
  select(`Year`,`Zip Code`,County,Inc_PC,Pop_PC,HC_PC,Inc_HC_diff)

# Re-calculate the opportunity costs with the new scenario, where home values are capped
opportunity_cost_new = all_data_capped %>%
  mutate(HC = `Zillow Home Value (Average)`,
         I = `Mean Income`,
         HIR = HC/I,
         OP_raw = HC/3 - HC/HIR,
         OP = ifelse(OP_raw < 0, 0, OP_raw),
         `OP Cost (m$)` = (Population*OP)/1000000) # per million $

all_data_capped = all_data_capped %>% mutate(`OP Cost (m$)` = opportunity_cost_new$`OP Cost (m$)`)

# Counties
county_labels = c("Middlesex","Hudson","Essex","Morris","Bergen","Passaic","Union","Somerset",
                  "Sussex","Monmouth","Warren","Hunterdon","Camden","Ocean","Burlington","Gloucester",
                  "Atlantic","Salem","Cape May","Cumberland","Mercer")
# Parameter Labels
param_options = c("Mean Income","Population","Zillow Home Value (Average)","`Zillow Home Value (Average)`","OP Cost (m$)","Inc_PC",
                  "Pop_PC","HC_PC","Inc_HC_diff")
# Function for given spreadsheet
# alldata -> references spreadsheet from CombinedData
# param_select inputs:
  # 1 for Mean Income
  # 2 for Population
  # 3 for Zillow Home Value (Average)
  # 4 for new HC
  # 5 for Opportunity Cost
  # 6 for Income Percent increase
  # 7 for Population percent increase
  # 8 for Home Value percent increase
  # 9 for Percent Difference between Home Value and Income
# region_select Inputs:
  # 1 to Group by Zip Code
  # 2 to Group by County
spc_subgroup = function(alldata,param_select,region_select){
  if (region_select == 1){
    stats_subgroup <- alldata %>% group_by(Year)
  }
  else if (region_select == 2){
    stats_subgroup <- alldata %>% group_by(Year,County) 
  }
  # Calculate statistics of interest
  stats_subgroup %>% 
    summarize(
      # subgroup mean
      xbar = mean(.data[[param_options[param_select]]]),
      # subgroup range
      r = max(.data[[param_options[param_select]]]) - min(.data[[param_options[param_select]]]),
      # subgroup standard deviation
      sd = sd(.data[[param_options[param_select]]]),
      # subgroup sample size
      nw = n(),
      # Degrees of freedom within groups
      dof = nw - 1) %>%
    # Calculating sigma short (within-group variance)
    mutate(
      sigma_s = sqrt(mean(sd^2)),
      # Calculating Standard error
      se = sigma_s / sqrt(nw),
      # Calculating 6-sigma control limits
      upper = mean(xbar) + 3*se,
      lower = mean(xbar) - 3*se,
      parameter = param_options[param_select])
}

# Function for calculating total stats
# Inputs:
  # alldata -> the all_data spreadsheet data
  # subgroup_stats -> the output from the spc_subgroup function
  # param_select: -> 1 for Mean Income, 2 for Population, 3 for Zillow Home Value
  # region select -> 0 for zip code total data, otherwise 1-21
spc_total = function(alldata,subgroup_stats,region_select){
  if (region_select == 0){
    stats_total <- subgroup_stats
    }
  else if (region_select >= 1 & region_select <= 21){
    stats_total <- subgroup_stats %>% filter(County == county_labels[region_select])
  }
  else {
    stop("Error: region_select option must be either 0 or between 1 and 21")
  }
  # Calculating grand mean and sd_total using all data
  stats_total = subgroup_stats %>%
    summarize(
      xbbar = mean(xbar), #  X-Double bar, or Grand Mean
      rbar = mean(r), # Average Range
      sdbar = mean(sd), # Average Standard Deviation
      # Recalculating Sigma Short
      sigma_s = sqrt( mean(sd^2) ),
      # Calculating overall standard deviation, or total standard deviation
      std_total = sd(alldata[[subgroup_stats$parameter[1]]]),
      se = se[1])
      if (region_select != 0){
      stats_total %>% mutate(County = county_labels[region_select])
      }
  else{
    return(stats_total)}
}

# Calculating Confidence Intervals for Opportunity Cost

# Existing Scenario
stats_subgroup = spc_subgroup(all_data,5,1)
stats_total = spc_total(all_data,stats_subgroup,0)

# New Scenario with Growth Factor
stats_subgroup_new = spc_subgroup(all_data_capped,5,1)
stats_total_new = spc_total(all_data_capped,stats_subgroup_new,0)

# 95% Confidence Intervals

# Existing Scenario
existing_op_int = stats_total %>%
  reframe(
    lower = xbbar - qnorm(alpha)*se,
    upper = xbbar + qnorm(alpha)*se,
  )

# New Scenario
new_op_int = stats_total_new %>%
  reframe(
    lower = xbbar - qnorm(alpha)*se,
    upper = xbbar + qnorm(alpha)*se,
  )

# Determine if this is statistically significant based on if CIs overlap
(stat_sig = tibble(
  if_else(existing_op_int$lower - new_op_int$upper >= 0, TRUE, FALSE),
  diff = existing_op_int$lower - new_op_int$upper
))

# Plotting for any SPC Value

# Create labels_avg data frame for plotting based on subgroup stats
labels_avg = stats_subgroup %>%
  summarize(
    subgroups = 2023, # Max number of subgroups in data set
    type = c("xbbar",  "upper", "lower"), # Vector of labels for data frame
    name = c("Mean", "+3 sigma", "-3 sigma"), # Vector of names for labels
    value = c(mean(xbar), unique(upper), unique(lower)), # Vector of total statistical values
    value = round(value, 2), # Rounding of statistical values
    text = paste(name, value, sep = " = ")) # concatenating vectors into a string for plotting

# Plotting using ggplot2
gg1 = stats_subgroup %>% # defining gg1 to save plotted image
  ggplot(mapping = aes(x = 2012:2023, y = xbar)) + # mapping ggplot to average per subgroup (xbar)
  # Plotting the center line as a red dotted horizontal line
  geom_hline(mapping = aes(yintercept = mean(xbar)), color = "red", linewidth = 1, linetype = "dotted") +
  # Plotting the lower limit (-3 sigma) as a steelblue dashed horizontal line
  geom_hline(mapping = aes(yintercept = lower), color = "steelblue", linewidth = 2, linetype = "dashed") +
  # Plotting the upper limit (+3 sigma) as a steelblue dashed horizontal line
  geom_hline(mapping = aes(yintercept = upper), color = "steelblue", linewidth = 2, linetype = "dashed") +
  geom_line(linewidth = 0.75) +
  geom_point(size = 3) +
  # Plot labels from labels_avg data frame
  geom_label(data = labels_avg, mapping = aes(x = subgroups, y = value, label = text),  hjust = 1)  +
  # Labeling Axes and creating title/subtitle
  labs(x = "Subgroup #", y = "OP (million $)",
       title = "Opportunity Cost with limited Housing Growth",
       subtitle = "Capped at 10% Growth")

# Saving plot as an image with a higher resolution/dpi
ggsave(filename = "op_cost.png", plot = gg1, dpi = 500, width = 8, height = 5)

