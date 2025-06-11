# Code for HarvardX PH125.9x Capstone Project - Education Data
# Elan Weingarten
# 6/10/2025

#   This script is part of the Capstone Project pt 2 for HarvardX Data Science
#
#   The goal of this project is to analyze a large public education dataset
# and pull out trends that may not be obvious at first glance. In this report
# we focus on the relationship between Title I funding and the racial
# demographics of public schools in the past 10 years.
#
# To accomplish this, we take the following steps:
#   1. Load, clean, and process the dataset
#   2. Exploratory data analysis
#   3. Attempt basic linear regression model
#   4. Refine the model using mixed-effects models
#   5. Attempt to make predictions using the mixed-effects models
#   6. Explain the trends

library(broom.mixed)
library(caret)
library(knitr)
library(kableExtra)
library(lme4)
library(naniar)
library(scales)
library(tidyverse)

#############################################
## 1. Load, clean, and process the dataset ##
#############################################

## A. Load
data_dir = file.path(".", "data")

#    NCES data is split into four datasets because ELSi can only output 75 
# columns at a time, and each datapoint takes 10 columns (one per year).
get_elsi_data = function (filename) {
  zip_filename = paste0(filename, ".zip")
  zip_path = file.path(data_dir, zip_filename)
  
  ed_data_filename = paste0(filename, ".csv")
  ed_data_path = file.path(data_dir, ed_data_filename)
  
  if (!file.exists(ed_data_path)) unzip(zip_path, exdir = data_dir)
  read_csv(ed_data_path, skip = 5)
}

filenames = c("1 ElSi School Info",
              "2 ElSi Characteristics",
              "3 ElSi Race",
              "4 ElSi Teacher and Race")

raw_data = lapply(filenames, get_elsi_data)

## B. Merge
make_clean_data = function (raw_data) {
  raw_data %>%
    rename( #easier names to work with
      school_name = `School Name`,
      state = `State Name [Public School] Latest available year`,
      full_id = `School ID (12-digit) - NCES Assigned [Public School] Latest available year`
    ) %>%
    # Restructure to turn school year from column name into a separate column
    pivot_longer(
      cols = -c(school_name, state, full_id) & !contains("Latest available year"),
      values_to = "value",
      names_to = "metric"
    ) %>%
    mutate(  #splits at the last space before the school year
      year = as.numeric(paste0("20", str_sub(metric, nchar(metric) - 1))),
      metric = str_sub(metric, 1, nchar(metric) - 8)
    ) %>%
    pivot_wider(
      names_from = metric,
      values_from = value
    )
}

clean_data = lapply(raw_data, make_clean_data)

# Combine all four datasets into one big one
combined = clean_data[[1]] %>%
  left_join(select(clean_data[[2]], -c(school_name, state)), by = c("full_id", "year")) %>%
  left_join(select(clean_data[[3]], -c(school_name, state)), by = c("full_id", "year")) %>%
  left_join(select(clean_data[[4]], -c(school_name, state)), by = c("full_id", "year")) %>%
  rename_with(~ .x %>% #clean up column names
                str_remove_all("\\s\\[(District|Public School)\\]|\\.| Latest available year| [-–—] NCES Assigned") %>%  # remove [District] or [Public School]
                str_trim() %>%                                         # remove leading/trailing spaces
                str_replace_all("[ /\\-–]", "_") %>%                      # replace space, /, -, or – with _
                str_to_lower() %>%                                         # make all lower case
                str_replace_all("agency", "district") %>%              # replace agency with district
                str_replace_all("\\(sy_2017_18_onward\\)", "2018_onward") %>% #replace note about sy so that all parentheticals can be removed
                str_remove_all("_\\([^\\)]*\\)")                         # remove all parentheticals
  ) %>%
  mutate( #merge school level (since the value-set changed after 2018)
    school_level = str_replace(str_replace(str_replace(
      str_replace(school_level, "1-Primary", "Elementary"),
      "2-Middle", "Middle"),
      "3-High", "High"),
      "4-Other", "Other")
  ) %>%
  mutate(
    school_level = if_else(is.na(school_level), school_level_2018_onward, school_level)
  ) %>%
  select(-school_level_2018_onward) %>%
  # Replace missing data with NA
  mutate(across(everything(), ~ ifelse(. %in% c("†", "–", "‡"), NA, .))) %>%
  filter( # remove all rows where there is no meaningful data
    !if_all(
      -c(school_name, state, full_id, district_id, year),
      ~ is.na(.)
    )
  ) %>%
  # guess type of data
  type.convert(as.is = TRUE) 

## C. Clean
#   Make tidy by converting flags to boolean, and spreading columns with multiple
# possible values into individual columns with 1/-1 values.
flag_to_boolean = function(column) {
  case_when(
    column == "1-Yes" ~ TRUE,
    column == "2-No" ~ FALSE,
    TRUE ~ NA
  )
}

tidy_data = combined %>%
  mutate(
    # convert flags into boolean columns
    magnet_school = flag_to_boolean(magnet_school),
    charter_school = flag_to_boolean(charter_school),
    reconstituted_school = flag_to_boolean(reconstituted_flag),
    title_i = case_when(
      grepl("6-Not", title_i_school_status) ~ FALSE,
      is.na(title_i_school_status) ~ NA,
      TRUE ~ TRUE
    ),
    #recode options to simple terms
    #only keep the categories we plan to investigate
    school_type = case_when(
      grepl("Regular", school_type) ~ "school_type_regular",
      grepl("Special", school_type) ~ "school_type_sped",
      TRUE ~ NA
    ),
    district_type = case_when( 
      grepl("local", district_type, ignore.case = TRUE) ~ "district_type_regular",
      grepl("agency", district_type) ~ "district_type_gov_run",
      grepl("Charter", district_type) ~ "district_type_charter",
      TRUE ~ NA
    ),
    status = case_when(
      grepl("Open", start_of_year_status) ~ "status_open",
      grepl("New", start_of_year_status) ~ "status_new",
      grepl("Changed", start_of_year_status) ~ "status_moved",
      grepl("Reopened", start_of_year_status) ~ "status_reopened",
      TRUE ~ NA
    ),
    school_level = case_when(
      school_level == "Elementary" ~ "school_level_es",
      school_level == "High" ~ "school_level_hs",
      school_level == "Middle" ~ "school_level_ms",
      school_level == "Secondary" ~ "school_level_ms|school_level_hs",
      TRUE ~ NA
    )
  ) %>%
  select(-c(reconstituted_flag, start_of_year_status, title_i_school_status, school_id))

# Create a widened version for each column and reduce into a single data frame
ed_data_full = tidy_data %>%
  separate_rows(school_level, sep = "\\|") %>%
  
  # Create school_type columns
  filter(!is.na(school_type)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = school_type, values_from = value, values_fill = -1) %>%
  
  # Create district_type columns
  filter(!is.na(district_type)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = district_type, values_from = value, values_fill = -1) %>%
  
  # Create status columns
  filter(!is.na(status)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = status, values_from = value, values_fill = -1) %>%
  
  # Create school_level columns
  filter(!is.na(school_level)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = school_level, values_from = value, values_fill = -1) %>%
  
  #turn demographics into percentages
  mutate(
    race_aian = american_indian_alaska_native_students/total_race_ethnicity,
    race_aapi = asian_or_asian_pacific_islander_students/total_race_ethnicity,
    race_hispanic = hispanic_students/total_race_ethnicity,
    race_black = black_or_african_american_students/total_race_ethnicity,
    race_white = white_students/total_race_ethnicity,
    race_nhpi = nat_hawaiian_or_other_pacific_isl_students/total_race_ethnicity,
    race_multi = two_or_more_races_students/total_race_ethnicity
  ) %>%
  select(-c(american_indian_alaska_native_students, 
            asian_or_asian_pacific_islander_students,
            hispanic_students,
            black_or_african_american_students,
            white_students,
            nat_hawaiian_or_other_pacific_isl_students,
            two_or_more_races_students,
            total_race_ethnicity
  )) %>%
  
  #turn genders into percentages
  mutate(
    total_gender = male_students + female_students,
    gender_male = male_students/total_gender,
    gender_female = female_students/total_gender
  ) %>%
  select(-c(total_gender, male_students, female_students)) %>%
  filter(total_students_all_grades > 50)

##################################
## 2. Exploratory data analysis ##
##################################
## A. Schools and Districts by Year
schools_by_year = ed_data_full %>%
  group_by(year) %>%
  summarize(n = n())

schools_by_year_plot = schools_by_year %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "#2c7fb8") +  # clean blue tone
  geom_text(
    aes(label = comma(n)), 
    vjust = -0.3, 
    color = "black",
    size = 3.5
  ) +
  scale_x_continuous(breaks = pretty(schools_by_year$year, n = 10)) +
  scale_y_continuous(label = comma) +
  labs(
    title = "Number of Schools in Dataset by Year",
    x = "Year",
    y = "Number of Schools"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
schools_by_year_plot

n_schools_avg = ed_data_full %>%
  select(full_id, year) %>%
  distinct() %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  mean()

n_districts_avg = ed_data_full %>%
  select(district_id, year) %>%
  distinct() %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  mean()

#    Dataset has:
# About 15k districts each year
# About 84k schools each year

## B. School Student Enrollment
enrollment_geo_mean = exp(mean(log(ed_data_full$total_students_all_grades[ed_data_full$total_students_all_grades > 0]), na.rm = TRUE))

# Plot with geometric mean
enrollment_plot = ed_data_full %>%
  select(total_students_all_grades) %>%
  sample_frac(0.007) %>% #take random sample of .7% of data points to save processing time
  ggplot(aes(x = total_students_all_grades)) +
  geom_density(fill = "blue", aes(y = after_stat(density * nrow(ed_data_full)))) +
  geom_vline(xintercept = enrollment_geo_mean, color = "white", linetype = "dashed", size = 1, alpha = .5) +
  geom_text(
    aes(x = enrollment_geo_mean, y = 50, label = paste0("Geo. Mean: ", comma(round(enrollment_geo_mean)))),
    vjust = -0.5,
    hjust = -0.05,
    color = "white",
    alpha = .5,
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_x_log10(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Student Enrollment Across all Schools",
    x = "Student Enrollment (log scale)",
    y = "Number of Schools"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  )

enrollment_plot

#    Enrollment across schools is normally distributed (on the logarithmic 
# scale) as to be expected with population sizes. The typical school has 447 
# students (geometric mean).

## C. Racial Trends
# Racial trends over time
race_trends_plot = ed_data_full %>%
  group_by(year) %>%
  summarize(across(starts_with("race_"), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-year, names_to = "race", values_to = "share") %>%
  ggplot(aes(x = year, y = share, color = race)) +
  geom_line(size = 1.2) +
  labs(title = "Average Racial Composition of Schools Over Time",
       x = "Year", y = "Average Share", color = "Race") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = pretty(schools_by_year$year, n = 10)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

race_trends_plot

#    There is no racial data for 2017, but otherwise a clear trend is shown for
# all groups form 2015 to 2024:
#   * White is largest group, decreasing steadily.
#   * Hispanic is second most common group, increasing faster than any other 
# group.
#   * Black is the third most common group, fluctuating, but steady over all.
#
#    All other racial groups form a small minority compared to these three.

## D. Explore missing data
gg_miss_var(ed_data_full, show_pct = TRUE)
cat("Magnet School data is spotty, and is thus dropped.")

#   We pursue this racial demographic trend by considering it alongside 
# potential trends in Title I funding. This is a source of funding that is
# intended to help schools educate students from lower-income families, and is
# often used as a proxy for income equity. 
#
#   Sometimes efforts to achieve income equity are perceived as being related to
# efforts to achieve racial equity. This exploratory modeling investigates that
# relationship.

## E. Reduce dataset size
#   Focus on Title I status and the top 3 races: White, Hispanic, & Black
ed_data = ed_data_full %>%
  select(school_name, state, full_id, district_id, year, district_name, 
         county_name, county_number, total_students_all_grades, title_i, 
         race_hispanic, race_black, race_white) %>%
  mutate(
    title_i = case_when(
      is.na(title_i) ~ NA,
      title_i ~ 1,
      !title_i ~ 0
    ),
    year_scaled = year - 2015 # to user for modelling, making year 0 meaningful
  ) %>%
  filter(!is.na(title_i), !is.na(race_hispanic), !is.na(race_black), !is.na(race_white)) %>%
  #remove schools with only 1 year of data
  group_by(full_id) %>%
  filter(n() >= 2) %>%
  ungroup()

## F. Explore racial trends of top three races
race3_trend_plot = ed_data %>%
  group_by(year) %>%
  summarize(across(starts_with("race_"), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-year, names_to = "race", values_to = "share") %>%
  ggplot(aes(x = year, y = share, color = race)) +
  geom_line(size = 1.2) +
  labs(title = "Average Racial Composition of Schools Over Time",
       x = "Year", y = "Average Share", color = "Race") +
  scale_y_continuous(labels = percent) +
  theme_minimal()

race3_trend_plot

#   The trends we saw earlier are present, but zoomed in we see that the extra 
# details makes the trend more complicated.

## G. Explore trends in changes in racial shares over the years
race_trends = ed_data %>%
  group_by(full_id) %>%
  summarize(
    first_year = min(year),
    last_year = max(year),
    race_black_change = race_black[year == last_year] - race_black[year == first_year],
    race_hispanic_change = race_hispanic[year == last_year] - race_hispanic[year == first_year],
    race_white_change = race_white[year == last_year] - race_white[year == first_year],
    avg_enrollment = mean(total_students_all_grades),
    title_i_mode = as.numeric(names(which.max(table(title_i))))
  ) %>%
  filter(last_year > 2020, first_year < 2016) %>% #ensure at least 6 years of data
  select(race_black_change, race_hispanic_change, race_white_change, title_i_mode, avg_enrollment)

#   Title I schools appear to be more likely to have lost Black students 
# proportionally in the last 10 years
black_density_plot = race_trends %>%
  ggplot(aes(x = race_black_change, fill = title_i_mode == 1)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("FALSE" = "#e41a1c", "TRUE" = "#377eb8")) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = .2 * c(-1, 1)
  ) +
  labs(
    title = "Distribution of Black Student Share Change",
    subtitle = "Comparing Title I vs. Non–Title I Schools",
    x = "Change in % Black Students",
    y = "Density",
    fill = "Title I"
  ) +
  theme_minimal(base_size = 12)
black_density_plot

#   Title I schools appear to be more likely to have lost Hispanic students
# proportionally in the last 10 years, moreso than Black students.
hispanic_density_plot = race_trends %>%
  ggplot(aes(x = race_hispanic_change, fill = title_i_mode == 1)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("FALSE" = "#e41a1c", "TRUE" = "#377eb8")) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = .2 * c(-1, 1)
  ) +
  labs(
    title = "Distribution of Hispanic Student Share Change",
    subtitle = "Comparing Title I vs. Non–Title I Schools",
    x = "Change in % Hispanic Students",
    y = "Density",
    fill = "Title I"
  ) +
  theme_minimal(base_size = 12)
hispanic_density_plot

#   Title I schools appear to be LESS likely to have lost White students
# than non-Title I schools in the last 10 years.
white_density_plot = race_trends %>%
  ggplot(aes(x = race_white_change, fill = title_i_mode == 1)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("FALSE" = "#e41a1c", "TRUE" = "#377eb8")) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = .3 * c(-1, 1)
  ) +
  labs(
    title = "Distribution of White Student Share Change",
    subtitle = "Comparing Title I vs. Non–Title I Schools",
    x = "Change in % White Students",
    y = "Density",
    fill = "Title I"
  ) +
  theme_minimal(base_size = 12)
white_density_plot

##############################################
## 3. Attempt basic linear regression model ##
##############################################

#   Finding an interesting relationship between race and Title I status, we
# investigate further using multiple modelling techniques.

# Investigate the CHANGE in race shares with Linear Regression
lm_black = lm(race_black_change ~ title_i_mode + avg_enrollment, data = race_trends)
lm_hispanic = lm(race_hispanic_change ~ title_i_mode + avg_enrollment, data = race_trends)
lm_white = lm(race_white_change ~ title_i_mode + avg_enrollment, data = race_trends)

get_lm_summary = function (model, race) {
  tidy(model) %>%
    select(term, estimate, std.error, statistic) %>%
    rename_with(~paste0(race, "_", .), -term)
}

lm_black_summary = get_lm_summary(lm_black, "black")
lm_hispanic_summary = get_lm_summary(lm_hispanic, "hispanic")
lm_white_summary = get_lm_summary(lm_white, "white")

# Combine into one table
summarize_models = function (black_summary, white_summary, hispanic_summary)
  reduce(
    list(black_summary, white_summary, hispanic_summary),
    full_join,
    by = "term"
    ) %>%
  mutate(
    black = sprintf("%.5f (%.5f) [%.2f]", black_estimate, black_std.error, black_statistic),
    white = sprintf("%.5f (%.5f) [%.2f]", white_estimate, white_std.error, white_statistic),
    hispanic = sprintf("%.5f (%.5f) [%.2f]", hispanic_estimate, hispanic_std.error, hispanic_statistic)
    ) %>%
  select(term, black, white, hispanic)

lm_summary = summarize_models(lm_black_summary, lm_white_summary, lm_hispanic_summary)

suppressWarnings( #warns about a missing text file
  lm_summary %>%
    kable(
      caption = "Linear Regression Estimates (Standard Error) [t-values]",
      col.names = c("Predictor", "% Black", "% White", "% Hispanic")
    ) %>%
    kable_styling(full_width = FALSE)
)
# NOTE: Year 0 is in fact 2015, since it's scaled

#   We see trends supporting our observation, but the t-value is quite low for
# the Hispanic relation to Title I.

####################################################
## 4. Refine the model using mixed-effects models ##
####################################################

#   Model the school (full_id) and state as random effects to control for 
# unobserved, school- or state-specific characteristics.
# lmer_black_state = lmer(race_black ~ title_i * year_scaled + (1 | full_id) + (1 | state), data = ed_data)
# lmer_hispanic_state = lmer(race_hispanic ~ title_i * year_scaled + (1 | full_id) + (1 | state), data = ed_data)
# lmer_white_state = lmer(race_white ~ title_i * year_scaled + (1 | full_id) + (1 | state), data = ed_data)
# 
# # Error! Model could not converge for black or white.

#   Try again, with only the school (full_id) as a random effect, since the school
# already accounts for location and including state is somewhat redundant.
lmer_black = lmer(race_black ~ title_i * year_scaled + (1 | full_id), data = ed_data)
lmer_hispanic = lmer(race_hispanic ~ title_i * year_scaled + (1 | full_id), data = ed_data)
lmer_white = lmer(race_white ~ title_i * year_scaled + (1 | full_id), data = ed_data)

# Converges!

get_lmer_summary = function (model, race) {
  tidy(model, effects = "fixed") %>%
    select(term, estimate, std.error, statistic) %>%
    rename_with(~ paste0(race, "_", .), -term)
}

lmer_black_summary = get_lmer_summary(lmer_black, "black")
lmer_white_summary = get_lmer_summary(lmer_white, "white")
lmer_hispanic_summary = get_lmer_summary(lmer_hispanic, "hispanic")

lmer_summary = summarize_models(lmer_black_summary, lmer_white_summary, lmer_hispanic_summary)

suppressWarnings( #warns about a missing text file
  lmer_summary %>%
    kable(
      caption = "Fixed Effects Estimates (Standard Error) [t-values]",
      col.names = c("Predictor", "% Black", "% White", "% Hispanic")
    ) %>%
    kable_styling(full_width = FALSE)
)
# NOTE: Year 0 is in fact 2015, since it's scaled

#   Trends show that while title i schools are losing black and Hispanic 
# students and gaining white students, Non-Title I schools are diversifying
#
#   All trends with Mixed Effect model are very statistically significant given
# the high t-statistic value
# NOTE: |t| > 2 ~ significant on the 5% level

###################################################################
## 5. Attempt to make predictions using the mixed-effects models ##
###################################################################

#   The Mixed-Effect model (lmer) proved too memory-intensive to reliably
# generate predictions. As a fallback, we used the simpler linear regression 
# model (lm) to estimate trends.

set.seed(42)  # for reproducibility

# Split ed_data into train (80%) and test (20%)
train_indices <- sample(nrow(ed_data), size = 0.8 * nrow(ed_data))
ed_train <- ed_data[train_indices, ]
ed_test <- ed_data[-train_indices, ]

# Train simpler lm model
simple_lm_black = lm(race_black ~ title_i * year_scaled, data = ed_train)
simple_lm_hispanic = lm(race_hispanic ~ title_i * year_scaled, data = ed_train)
simple_lm_white = lm(race_white ~ title_i * year_scaled, data = ed_train)

# Predict on test data
ed_test = ed_test %>%
  mutate(
    pred_black = predict(simple_lm_black, newdata = ed_test),
    pred_white = predict(simple_lm_white, newdata = ed_test),
    pred_hispanic = predict(simple_lm_hispanic, newdata = ed_test)
  )

# Calculate RMSE
rmse_black = RMSE(ed_test$pred_black, ed_test$race_black)
rmse_white = RMSE(ed_test$pred_white, ed_test$race_white)
rmse_hispanic = RMSE(ed_test$pred_hispanic, ed_test$race_hispanic)

# Print results
tibble(
  Race = c("Black", "White", "Hispanic"),
  RMSE = c(rmse_black, rmse_white, rmse_hispanic)
)

###########################
## 6. Explain the trends ##
###########################

#   The linear model captures general trends but performs poorly as a predictor,
# with RMSE too high for practical use. The mixed-effects model was unfeasible,
# leaving us without a reliable forecasting tool. More complex models or richer
# data may be needed for accurate prediction.

#   Still, we found a clear pattern: Title I schools are losing Black and Hispanic
# student share faster (or gaining more slowly) than non–Title I schools, while
# White student share is declining more slowly in Title I schools.


save(lm_summary, lmer_summary, ed_data_full, ed_data, race_trends, 
     schools_by_year_plot, enrollment_plot, race_trends_plot, race3_trend_plot,
     white_density_plot, hispanic_density_plot, black_density_plot,
     n_schools_avg, n_districts_avg,
     file = file.path("data", "r_variables.RData"))