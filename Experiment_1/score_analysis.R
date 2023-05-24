library(readr)
library(dplyr)

pre_data <- read_csv('../TLM_Exp_1_PRE.csv', col_types = cols(
  uid = col_character(),
  group = col_factor()
) )

post_data <- read_csv('../TLM_Exp_1_POST.csv', col_types = cols(
  uid = col_character(),
  group = col_factor()
))


summary(pre_data[pre_data$group=='exp',]$uid)
summary(pre_data[pre_data$group=='cont',]$uid)
summary(post_data[post_data$group=='exp',]$uid)
summary(post_data[post_data$group=='cont',]$uid)

levels(post_data$group)


# Identify rows with missing values   PRE
rows_with_missing <- which(rowSums(is.na(pre_data)) > 0)
rows_with_missing
# Print rows with missing values PRE
print(pre_data[rows_with_missing, ])
# ONLY 5 STUDENTS HAVE MISSING DATA


# Identify rows with missing values POST
rows_with_missing <- which(rowSums(is.na(post_data)) > 0)
# Print rows with missing values POST
print(post_data[rows_with_missing, ])
# ONLY 4 STUDENTS HAVE MISSING DATA

# Remove cases with any missing values
pre_complete <- na.omit(pre_data)
post_complete <- na.omit(post_data)

# size before
length(pre_data$uid)
length(post_data$uid)

# size after omitting the missing values
length(pre_complete$uid)
length(post_complete$uid)


## PRE
# calculate the total duration by summing duration in all subcategories
pre_complete <- pre_complete %>% rowwise() %>%
    mutate(total_time = sum(c(duration_aa, duration_as, duration_dd, duration_in, duration_ii), na.rm = TRUE))

# calcualte total score by summing the score of each subcategory
pre_complete <- pre_complete %>% rowwise() %>%
  mutate(score = sum(c(total_aa, total_as, total_dd, total_in, total_ii), na.rm = TRUE))

# create a new variable containing only these columns
pre_totals <- pre_complete %>% select(uid, group, total_aa, total_as, total_dd, total_in, total_ii, total_time, score)

# show it
pre_totals
# show the mean of both groups at PRE for final total score
pre_totals %>% group_by(group) %>% summarise(mean_total=mean(score), .groups = 'drop')
# show the mean of both groups at PRE for each subcateory, time, and final total score
pre_totals %>% select( group, total_aa, total_as, total_dd, total_in, total_ii, total_time, score) %>% 
              group_by(group) %>% summarise(across(everything(), mean),
                                             .groups = 'drop')  %>% as.data.frame()

## POST
# calculate the total duration by summing duration in all subcategories
post_complete <- post_complete %>% rowwise() %>%
  mutate(total_time = sum(c(duration_aa, duration_as, duration_dd, duration_in, duration_ii), na.rm = TRUE))

# calcualte total score by summing the score of each subcategory
post_complete <- post_complete %>% rowwise() %>%
  mutate(score = sum(c(total_aa, total_as, total_dd, total_in, total_ii), na.rm = TRUE))

# create a new variable containing only these columns
post_totals <- post_complete %>% select(uid, group, total_aa, total_as, total_dd, total_in, total_ii, total_time, score)

# show it
post_totals
# show the mean of both groups at post for final total score
post_totals %>% group_by(group) %>% summarise(mean_total=mean(score), .groups = 'drop')
# show the mean of both groups at post for each subcateory, time, and final total score
post_totals %>% select( group, total_aa, total_as, total_dd, total_in, total_ii, total_time, score) %>% 
  group_by(group) %>% summarise(across(everything(), mean),
                                .groups = 'drop')  %>% as.data.frame()


# Calculate mean and standard deviation of 'score' column grouped by 'group'
result_pre <- pre_totals %>%
  group_by(group) %>%
  summarise(mean_total = mean(score),
            std_dev_total = sd(score),
            .groups = 'drop') %>%
  filter(std_dev_total >= 0.4)

# Print the filtered result
print(result_pre)
# Calculate mean and standard deviation of 'score' column grouped by 'group'
result_post <- post_totals %>%
  group_by(group) %>%
  summarise(mean_total = mean(score),
            std_dev_total = sd(score),
            .groups = 'drop') %>%
  filter(std_dev_total >= 0.4)

# Print the filtered result
print(result_post)
