# Install and load necessary libraries
install.packages("readxl")  # Install the package
install.packages("openxlsx")
install.packages("lubridate")

library(readxl)
library(dplyr)
library(lubridate)
library(splines)  # Needed for natural splines if required

#load("my_workspace.RData")



####################################
####################
##########################


#selecting the file containing the dataset
glmfilepath <- file.choose()
#print(glmfilepath)  # Display the selected file path

# Extract directory path
#glmdirpath <- dirname(glmfilepath)

# Set working directory
#setwd(glmdirpath)

#writeLines(glmfilepath, "glmfilepath.txt")

# Read the saved file path
#glmfilepath <- readLines("glmfilepath.txt")

# Load the data
glmprojectdata <- read.csv(glmfilepath)




# Inspect the first few rows
head(glmprojectdata)
View(glmprojectdata)

# Check the structure of the dataset
str(glmprojectdata)

# Summary statistics
summary(glmprojectdata)

# Check for missing values
colSums(is.na(glmprojectdata))

# Check for duplicates
sum(duplicated(glmprojectdata))
class(glmprojectdata$inverse_data)
tail(glmprojectdata$inverse_data, 10)

# Convert the Date column from character to Date
glmprojectdata$date <- as.Date(glmprojectdata$inverse_data, format = "%Y-%m-%d")

class(glmprojectdata$date)
# Extract the year
glmprojectdata$year <- format(glmprojectdata$date, "%Y")

# Extract the month
glmprojectdata$month <- format(glmprojectdata$date, "%m")

# Extract the day
glmprojectdata$day <- format(glmprojectdata$date, "%d")

class(glmprojectdata$km)

# Remove commas and convert the km column to numeric
glmprojectdata$km <- as.numeric(gsub(",", ".", glmprojectdata$km))
glmprojectdata$km[is.na(glmprojectdata$km)] <- 
  median(glmprojectdata$km, na.rm = TRUE)

sum(is.na(glmprojectdata$km))


# Extract the hour as numeric value
str(glmprojectdata$hour)


# Ensure 'hour' is from lubridate's hms class
library(lubridate)

# Convert the 'hour' column to a proper time format
glmprojectdata$hour <- hms(glmprojectdata$hour)

# Now extract the hour component
glmprojectdata$hour_of_day <- hour(glmprojectdata$hour)

# Quick check
head(glmprojectdata$hour_of_day)

class(glmprojectdata$hour)

class(glmprojectdata$hour_of_day)


library(dplyr)
library(tidyverse)
# Convert categorical columns to factors
str(glmprojectdata)
class(glmprojectdata)
glmprojectdata <- glmprojectdata %>%
  mutate(
    week_day = as.factor(week_day),
    weather_timestamp = as.factor(weather_timestamp),
    road_type = as.factor(road_type),
    road_delineation = as.factor(road_delineation),
    state = as.factor(state),
    city = as.factor(city),
    road_direction = as.factor(road_direction),
    wheather_condition = as.factor(wheather_condition),
    cause_of_accident = as.factor(cause_of_accident),
    type_of_accident = as.factor(type_of_accident),
    regional = as.factor(regional)
  )
glmprojectdata$year <- as.numeric(glmprojectdata$year)
glmprojectdata$month <- as.numeric(glmprojectdata$month)
glmprojectdata$day <- as.numeric(glmprojectdata$day)





class(glmprojectdata)
str(glmprojectdata)

# Select only the variables of concern
varofinterest <- c("year", "month", "day", "week_day", 
                   "hour_of_day", "weather_timestamp", 
                   "wheather_condition", 
                   "road_type", "road_direction", "road_delineation",
                   "km","vehicles_involved","people", "deaths")
names(glmprojectdata)
print(varofinterest)
class(varofinterest)


# Create a new dataframe with the selected variables


glmfiltereddata <- glmprojectdata %>% dplyr::select(all_of(varofinterest))



# View the structure of the new dataframe
str(glmfiltereddata)

View(glmfiltereddata)

summary(glmprojectdata)
summary(glmfiltereddata)


head(glmprojectdata)



set.seed(2025)
n <- nrow(glmprojectdata)
train_index <- sample(seq_len(n), size = 0.7 * n)
train_data <- glmprojectdata[train_index, ]
test_data <- glmprojectdata[-train_index, ]



str(glmprojectdata)



######Exploratory Data Analysis
######################
########################
########################
########################
########################
#########################
#####################
######Exploratory Data Analysis
######################
########################
########################
########################
########################
#########################
#####################
######Exploratory Data Analysis
######################
########################
########################
########################
########################
#########################
#####################
######Exploratory Data Analysis
######################
########################
########################
########################
########################
#########################
########################Perform exploratory data analysis (EDA) to 
#identify trends and patterns in the data.############
########################Perform exploratory data analysis (EDA) to 
#identify trends and patterns in the data.############
########################Perform exploratory data analysis (EDA) to 
#identify trends and patterns in the data.############
########################Perform exploratory data analysis (EDA) to 
#identify trends and patterns in the data.############
# Load ggplot2
library(ggplot2)
library(gridExtra)

# Histogram for numeric variables
# Create the bar plot
ggplot(glmfiltereddata, aes(x = factor(year), y = deaths, fill = week_day)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Deaths by Year and Weekday",
       x = "Year",
       y = "Number of Deaths",
       fill = "Weekday") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summarize number of deaths by year and weekday
death_summary_table <- glmprojectdata %>%
  group_by(year, week_day) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = week_day, values_from = total_deaths, values_fill = 0)

# View the table
print(death_summary_table)

ggplot(glmprojectdata, aes(x = deaths)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Deaths", x = "Deaths", y = "Frequency")

ggplot(glmprojectdata, aes(x = vehicles_involved)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribution of Vehicles Involved", x = "Vehicles Involved", 
       y = "Frequency")

ggplot(glmprojectdata, aes(x = km)) +
  geom_histogram(binwidth = 50, fill = "orange", color = "black") +
  labs(title = "Distribution of Kilometer Markers", x = "Kilometer",
       y = "Frequency")

####################
# Bar plots for categorical variables
ggplot(glmprojectdata, aes(x = week_day)) +
  geom_bar(fill = "navy") +
  labs(title = "Accidents by Day of the Week", x = "Day of the Week", 
       y = "Count")

ggplot(glmprojectdata, aes(x = weather_timestamp)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Accidents by Time of Day", x = "Time of Day", y = "Count")

ggplot(glmprojectdata, aes(x = road_type)) +
  geom_bar(fill = "brown") +
  labs(title = "Accidents by Road Type", x = "Road Type", y = "Count")



####################


ggplot(glmprojectdata, 
       aes(x = km, y = deaths, color = road_type)) +
  geom_smooth(method = "glm", formula = y ~ x) +
  labs(title = "")

ggplot(glmprojectdata, 
       aes(x = people, y = deaths, color = road_type)) +
  geom_smooth(method = "glm", formula = y ~ x) +
  labs(title = "")

ggplot(glmprojectdata, 
       aes(x = people, y = deaths, color = road_delineation)) +
  geom_smooth(method = "glm", formula = y ~ x) +
  labs(title = "")



ggplot(glmprojectdata$hour_of_day, aes(x = Var1, y = Freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Accidents by Hour of the Day", 
       x="Hour of Day", y="Number of Accidents") +
  theme_minimal()




# Scatter plot of distance traveled vs deaths
ggplot(glmprojectdata, aes(x = km, y = deaths)) +
  geom_point() +
  labs(title = "Distance Traveled vs Deaths",
       x = "Distance (km)", y = "Deaths") +
  theme_minimal()


ggplot(glmprojectdata, aes(x = km, y = deaths)) +
  geom_point(alpha = 0.3) +
  labs(title = "Distance Traveled vs Deaths", 
       x = "Distance (km)", y = "Deaths") +
  theme_minimal()



ggplot(glmprojectdata, aes(x = vehicles_involved,
                          y = deaths,
                          color = road_delineation)) +
  geom_smooth(method = "glm", formula = y ~ x) +
  labs(title = "Effect of road_del and Veh Inv on deaths",
       x = "Number of Vehicles Involved", 
       y = "deaths") +
  theme_minimal()



############################
ggplot(glmprojectdata, aes(x = vehicles_involved,
                          y = deaths,
                          color = road_type)) +
  geom_smooth(method = "glm", formula = y ~ x,
              ) +
  labs(title = "Int Effect of Road Type and Veh Involved on deaths",
       x = "Number of Vehicles Involved", 
       y = "Predicted Fatalities") +
  theme_minimal()

ggplot(glmprojectdata, aes(x = vehicles_involved,
                       y = deaths,
                       color = road_type)) +
  geom_smooth(method = "glm", formula = y ~ x) +
  labs(title = "Int Effect of Road Type and Veh Involved on deaths",
       x = "Number of Vehicles Involved", 
       y = "Predicted Fatalities") +
  theme_minimal()

ggplot(glmprojectdata, aes(x = vehicles_involved,
                       y = deaths,
                       color = road_delineation)) +
  geom_smooth(method = "glm", formula = y ~ x) +
  labs(title = "Interaction Effect of road_delineation and Veh Involved on deaths",
       x = "Number of Vehicles Involved", 
       y = "deaths") +
  theme_minimal()





# Group by weekday and summarize total deaths
library(dplyr)

total_deaths_by_weekday <- glmprojectdata %>%
  group_by(week_day) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(perc = round(100 * deaths / sum(deaths), 2))

# Add total row
total_row <- data.frame(
  week_day = "Total",
  deaths = sum(total_deaths_by_weekday$deaths),
  perc = 100
)

final_table <- rbind(total_deaths_by_weekday, total_row)
print(final_table)



ggplot(glmfiltereddata, aes(x = road_type)) +
  geom_bar(fill = "brown") +
  labs(title = "Accidents by Road Type", x = "Road Type", y = "Count")



# Box plot
ggplot(glmfiltereddata, aes(x = road_type, y = deaths)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Deaths by Road Type", x = "Road Type", y = "Deaths")


# Box plot
ggplot(glmfiltereddata, aes(x = wheather_condition, y = deaths)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Deaths by Weather Condition", x = "Weather Condition",
       y = "Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Box plot
ggplot(glmfiltereddata, aes(x = wheather_condition, y = deaths)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Deaths by Weather Condition", x = "Weather Condition", 
       y = "Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Line plot
glmfiltereddata %>%
  group_by(year) %>%
  summarise(accidents = n()) %>%
  ggplot(aes(x = year, y = accidents)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "", x = "Year", y = "Number of Accidents")



# Bar plot
glmfiltereddata %>%
  group_by(month) %>%
  summarise(accidents = n()) %>%
  ggplot(aes(x = month, y = accidents)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Accidents by Month", x = "Month", y = "Number of Accidents")

# Line plot
glmfiltereddata %>%
  group_by(hour_of_day) %>%
  summarise(accidents = n()) %>%
  ggplot(aes(x = hour_of_day, y = accidents)) +
  geom_line(color = "green") +
  geom_point(color = "purple") +
  labs(title = "", x = "Hour of Day", y = "Number of Accidents")


library(vcd)
library(corrplot)
# Select numeric variables
numeric_data <- glmfiltereddata %>%
  select(deaths, vehicles_involved, people, km)

# Compute correlation matrix
cor_matrix <- cor(numeric_data)

# Plot correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", 
         tl.srt = 45)



# Function to compute Cramér’s V
cramers_v <- function(var1, var2, data) {
  tbl <- table(data[[var1]], data[[var2]])
  assocstats(tbl)$cramer
}

# Check associations between categorical variables
cat_vars <- c("road_type", "road_delineation", "week_day", "weather_timestamp")

# Generate Cramér's V matrix for categorical variables
cramers_v_matrix <- matrix(NA, nrow = length(cat_vars), ncol = length(cat_vars),
                           dimnames = list(cat_vars, cat_vars))

for (i in 1:length(cat_vars)) {
  for (j in 1:length(cat_vars)) {
    if (i != j) {
      cramers_v_matrix[i, j] <- cramers_v(cat_vars[i], cat_vars[j],
                                          glmfiltereddata)
    }
  }
}

# Print Cramér’s V matrix
print("Cramér’s V Matrix:")
print(cramers_v_matrix)


library(reshape2)
# Create the Cramér's V matrix as a dataframe
cramers_v_matrix <- matrix(c(
  NA, 0.0993, 0.0246, 0.0173,
  0.0993, NA, 0.0140, 0.0329,
  0.0246, 0.0140, NA, 0.0680,
  0.0173, 0.0329, 0.0680, NA
), nrow = 4, byrow = TRUE)

# Assign row and column names
rownames(cramers_v_matrix) <- colnames(cramers_v_matrix) <- c("road_type", 
                                                              "road_delineation", "week_day", "weather_timestamp")

# Convert matrix to long format
cramers_v_df <- melt(cramers_v_matrix, na.rm = TRUE)

# Create heatmap
ggplot(cramers_v_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 3)), color = "white", size = 5) +  # Add values to tiles
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Cramér's V Heatmap", fill = "Cramér's V", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Count accidents by day of the important features
table(glmfiltereddata$week_day)
table(glmfiltereddata$hour_of_day)
table(glmfiltereddata$month)
table(glmfiltereddata$year)
table(glmfiltereddata$weather_timestamp)
table(glmfiltereddata$wheather_condition)





##############
##################
#########################################
###################################################






# Boxplot: deaths by week_day, grouped by road_type
ggplot(glmprojectdata, aes(x = week_day, y = deaths, fill = road_type)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
  labs(title = "Boxplot of Deaths by Weekday and Road Type",
       x = "Day of the Week",
       y = "Number of Deaths",
       fill = "Road Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####checking of skewness
#Check for Skewness 



ggplot(glmprojectdata, aes(x = km)) +
  geom_density(fill = "lightblue") +
  labs(title = "Density Plot of km", x = "km") +
  theme_minimal()


library(e1071)
skewness(glmprojectdata$km, na.rm = TRUE)

ggplot(glmprojectdata, aes(x = vehicles_involved)) +
  geom_density(fill = "lightblue") +
  labs(title = "Density Plot ofvehicles_involved", x = "vehicles_involved") +
  theme_minimal()



skewness(glmprojectdata$vehicles_involved, na.rm = TRUE)

skewness(glmprojectdata$deaths, na.rm = TRUE)






skewness(glmprojectdata$vehicles_involved, na.rm = TRUE)



##########################START HERE
################
##################CHECKING FOR SIGNIFICANT VARIABLES
###############################

poissonmodelfull <-glm(deaths ~ year+day+week_day + hour_of_day +
                      weather_timestamp + wheather_condition + road_type + 
                      road_direction + road_delineation + km +
                      vehicles_involved + people,
                    family = poisson(link = "log"),
                    data = train_data)


step_model <-step(poissonmodelfull, direction = "both")


summary(step_model)

drop1(poissonmodelfull, test = "Chisq")

stepback_model <-step(poissonmodelfull, direction = "backward")


summary(stepback_model)

drop1(poissonmodelfull, test = "Chisq") 




##############fitting different link functions

# Poisson with log link (refitting full model from stepwise selection)
poisson_log <- glm(deaths ~ week_day + hour_of_day +
                           weather_timestamp + wheather_condition + road_type + 
                           road_direction + road_delineation + km +
                           vehicles_involved + people,
                         family = poisson(link = "log"),
                         data = train_data)
summary(poisson_log)



poisson_identity <- glm(deaths ~ day + week_day + hour_of_day +
                          weather_timestamp + wheather_condition + road_type + 
                          road_direction + road_delineation + km +
                          vehicles_involved + people,
                        family = poisson(link = "identity"),
                        data = train_data,
                        )


summary(poisson_identity)


poisson_sqrt <- glm(deaths ~ day + week_day + hour_of_day +
                      weather_timestamp + wheather_condition + road_type + 
                      road_direction + road_delineation + km +
                      vehicles_involved + people,
                    family = poisson(link = "sqrt"), data =train_data )

summary(poisson_sqrt)


# Extract summary information
summary_log <- summary(poisson_log)
summary_sqrt <- summary(poisson_sqrt)

# Create comparison table
comparison <- data.frame(
  Metric = c("Residual Deviance", "Residual DF", "Fisher Scoring Iterations", "AIC", "Model DF"),
  poisson_log = c(
    summary_log$deviance,
    summary_log$df.residual,
    summary_log$iter,
    AIC(poisson_log),
    length(coef(poisson_log))
  ),
  poisson_sqrt = c(
    summary_sqrt$deviance,
    summary_sqrt$df.residual,
    summary_sqrt$iter,
    AIC(poisson_sqrt),
    length(coef(poisson_sqrt))
  )
)

# Print table
print(comparison)



# Define different models
poisson_logf <- glm(deaths ~ year + month + day + week_day + hour_of_day +
                               weather_timestamp + wheather_condition + 
                               road_type + road_direction + road_delineation +
                               km + vehicles_involved + people,
                   family = poisson(link = "log"),
                   data = train_data)







# Original km
model_km <- glm(deaths ~ km  + week_day +
                  hour_of_day + 
                  road_delineation * vehicles_involved + 
                  road_type + weather_timestamp,
                offset = log(people), 
                family = poisson(link = "log"), 
                data = train_data)

# Log-transformed km
model_logkm <- glm(deaths ~ log(km+1) + week_day +
                     hour_of_day + 
                     road_delineation * vehicles_involved + 
                     road_type + weather_timestamp, 
                   offset = log(people),
                   family = poisson(link = "log"), 
                   data = train_data)


# Log-transformed km
model_logkm2 <- glm(deaths ~ km + week_day +
                      hour_of_day + 
                      road_delineation * vehicles_involved + 
                      road_type + weather_timestamp+road_direction, 
                    offset = log(people),
                    family = poisson(link = "log"), 
                    data = train_data)

model_logkmnew <- glm(deaths ~ log(km+1) + 
                        road_type * vehicles_involved + 
                        weather_timestamp, 
                      offset = log(people),
                      family = poisson(link = "log"), 
                      data = train_data)


po_model <- glm(
  deaths ~ log(km + 1) + week_day + hour_of_day + road_delineation + 
    vehicles_involved + road_type + weather_timestamp + 
    road_direction + road_delineation:vehicles_involved,
  offset = log(people),
  family = poisson(link = "log"), 
  data = train_data)


pois_model <- glm(
  deaths ~ week_day + road_delineation + 
    vehicles_involved + road_type + weather_timestamp + 
    road_delineation:vehicles_involved,
  offset = log(people),
  family = poisson(link = "log"),
  data = train_data
)





summary(po_model)
summary(pois_model)
summary(model_logkmnew)
summary(poisson_logf)
summary(model_logkm)
summary(model_logkm2)
summary(model_logkmnew)

# Named list of models
models <- list(
  po_model = po_model,
  pois_model = pois_model,
  model_logkmnew = model_logkmnew,
  poisson_logf = poisson_logf,
  model_logkm = model_logkm,
  model_logkm2 = model_logkm2
)

# Create comparison table
comparison <- data.frame(
  Metric = c("Residual Deviance", "Residual DF", "Fisher Scoring Iterations", "AIC", "Model DF")
)

# Loop through models and extract info
for (name in names(models)) {
  model <- models[[name]]
  comparison[[name]] <- c(
    deviance(model),
    df.residual(model),
    model$iter,
    AIC(model),
    length(coef(model))
  )
}

# Print comparison table
print(comparison, row.names = FALSE)

anova(model_logkm2, test="Chisq")
anova(po_model, test="Chisq")









#Check Significance of Categorical Predictors

 # Type III test

drop1(model_logkm2, test = "Chisq")  # Type III test




drop1(model_logkmnew, test = "Chisq")  # Type III test
# Load required package
install.packages("statmod"); 
library(statmod)
############################## 
# Diagnostic Plots 
##############################
# Diagnostic Plots 
##############################
# Diagnostic Plots 
par(mfrow = c(2, 2))
library(statmod)
qr <- qresid( po_model)
qqnorm(qr, las=1)
qqline(qr, col = "red")
# To find randomized quantile residuals
plot( qr ~ fitted(po_model), las=1 )
plot( cooks.distance(po_model), type="h", las=1 )
plot( hatvalues(po_model), type="h", las=1 )

maxhat <- which.max( hatvalues(po_model) )
maxqr <- which.max( abs(qr) )
maxinfl <- which.max( cooks.distance(po_model))
# Largest leverage
# Largest abs. residual
# Most influential
c( MaxLeverage=maxhat, MaxResid=maxqr, MaxInfluence=maxinfl)
which(influence.measures(po_model)$is.inf[,"cook.d"] )


#######################NB
######################
####################
library(ggplot2)
library(MASS)

nb_model <- glm.nb(
  deaths ~ log(km + 1) + week_day + hour_of_day + 
    road_delineation + 
    vehicles_involved + road_type + 
    weather_timestamp + 
    road_direction + 
    road_delineation:vehicles_involved + 
    offset(log(people)),
  data = train_data
)

summary(nb_model)
plot(nb_model)




summary(po_model)

plot(po_model)



AIC(po_model, nb_model)

######
##################
############################## 
# Diagnostic Plots 
##############################
# Diagnostic Plots 
##############################
# Diagnostic Plots from
par(mfrow = c(2, 2))
library(statmod)
qr <- qresid( nb_model)
qqnorm(qr, las=1)
qqline(qr, col = "red")
# To find randomized quantile residuals
plot( qr ~ fitted(nb_model), las=1 )
plot( cooks.distance(nb_model), type="h", las=1 )
plot( hatvalues(nb_model), type="h", las=1 )

maxhat <- which.max( hatvalues(nb_model) )
maxqr <- which.max( abs(qr) )
maxinfl <- which.max( cooks.distance(nb_model))
c( MaxLeverage=maxhat, MaxResid=maxqr, MaxInfluence=maxinfl)
which(influence.measures(nb_model)$is.inf[,"cook.d"] )

#######################
######################
# Half-Normal Plot for Residuals

##################Poisson Model
# Plot half-normal plot for deviance residuals
# Install and load the faraway package (if not already installed)
install.packages("faraway")  # run this only once
library(faraway)
# Deviance and Pearson residuals
po_deviance_res <- residuals(po_model, type = "deviance")
po_pearson_res <- residuals(po_model, type = "pearson")

# Plot QQ plot
par(mfrow = c(2, 2))
qqnorm(po_deviance_res)
qqline(po_deviance_res)




# Plot residuals vs. fitted values for PO model
fitted_vals_po <- fitted(po_model)
residuals_po <- residuals(po_model, type = "deviance")

# Residual vs Fitted Plot
plot(fitted_vals_po, residuals_po,
     main = "Residuals vs Fitted (PO)",
     xlab = "Fitted Values", ylab = "Deviance Residuals")
abline(h = 0, col = "red", lwd = 2)




# Half-normal plot with reference line and point labels for largest residuals
halfnorm(po_deviance_res, 
         main = "Half-Normal Plot (PO)",
         ylab = "Deviance Residuals")

# Optional: Identify the top 10 largest residuals
top_res <- order(abs(po_deviance_res), decreasing = TRUE)[1:10]
text(x = 1:10, 
     y = sort(abs(po_deviance_res), decreasing = TRUE)[1:10], 
     labels = top_res, 
     pos = 3, 
     col = "red")




# Observed vs Fitted Plot for Negative Binomial model
pobserved_vals <- train_data$deaths

# Plot observed vs fitted values
plot(pobserved_vals, fitted_vals_po,
     main = "Observed vs Fitted (PO)",
     xlab = "Observed Values", ylab = "Fitted Values")
abline(0, 1, col = "red", lwd = 2)



# Plot histogram of Deviance Residuals
hist(po_deviance_res, main = "Histogram of Deviance Residuals", 
     xlab = "Deviance Residuals", col = "lightblue", breaks = 50)



# Create density plot
plot(density(po_deviance_res, na.rm = TRUE),
     main = "Density Plot (PO)",
     xlab = "Deviance Residuals",
     col = "darkblue",
     lwd = 2)
polygon(density(po_deviance_res, na.rm = TRUE), col = "lightblue", border = "darkblue")
abline(v = 0, col = "red", lwd = 2, lty = 2)  # Add vertical line at 0
####################################
####################################
# Half-Normal Plot for Residuals
##################Negative Binomial
# Half-Normal Plot for Residuals

##################Negative Binomial
# Plot half-normal plot for deviance residuals
# Deviance and Pearson residuals
nb_deviance_res <- residuals(nb_model, type = "deviance")
nb_pearson_res <- residuals(nb_model, type = "pearson")

# Plot QQ plot
par(mfrow = c(2, 2))
qqnorm(nb_deviance_res)
qqline(nb_deviance_res)




# Plot residuals vs. fitted values for Negative Binomial model
fitted_vals_nb <- fitted(nb_model)
residuals_nb <- residuals(nb_model, type = "deviance")

# Residual vs Fitted Plot
plot(fitted_vals_nb, residuals_nb,
     main = "Residuals vs Fitted (NB)",
     xlab = "Fitted Values", ylab = "Deviance Residuals")
abline(h = 0, col = "red", lwd = 2)




# Half-normal plot with reference line and point labels for largest residuals
halfnorm(nb_deviance_res, 
         main = "Half-Normal Plot (NB)",
         ylab = "Deviance Residuals")

# Optional: Identify the top 10 largest residuals
top_res <- order(abs(nb_deviance_res), decreasing = TRUE)[1:10]
text(x = 1:10, 
     y = sort(abs(nb_deviance_res), decreasing = TRUE)[1:10], 
     labels = top_res, 
     pos = 3, 
     col = "red")




# Observed vs Fitted Plot for Negative Binomial model
observed_vals <- train_data$deaths

# Plot observed vs fitted values
plot(observed_vals, fitted_vals_nb,
     main = "Observed vs Fitted (NB)",
     xlab = "Observed Values", ylab = "Fitted Values")
abline(0, 1, col = "red", lwd = 2)



# Plot histogram of Deviance Residuals
hist(nb_deviance_res, main = "Histogram of Deviance Residuals", 
     xlab = "Deviance Residuals", col = "lightblue", breaks = 50)



# Create density plot
plot(density(nb_deviance_res, na.rm = TRUE),
     main = "Density Plot (NB)",
     xlab = "Deviance Residuals",
     col = "darkblue",
     lwd = 2)
polygon(density(nb_deviance_res, na.rm = TRUE), col = "lightblue", border = "darkblue")
abline(v = 0, col = "red", lwd = 2, lty = 2)  # Add vertical line at 0
####################################
#####################Further diagnostics for the outliers
###################
par(mfrow = c(1, 1))

nb_model_clean <- glm.nb(
  deaths ~ log(km + 1) + week_day + hour_of_day + road_delineation + 
    vehicles_involved + road_type + weather_timestamp + 
    road_direction + road_delineation:vehicles_involved+
  offset(log(people)),
  data = train_data[-influential_obs, ]
)

AIC(nb_model, nb_model_clean)




# Deviance and Pearson residuals
nb_deviance_rescl <- residuals(nb_model_clean, type = "deviance")
nb_pearson_rescl <- residuals(nb_model_clean, type = "pearson")




qqnorm(nb_deviance_rescl)
qqline(nb_deviance_rescl)





# Plot residuals vs. fitted values for Negative Binomial model
fitted_vals_nbcl <- fitted(nb_model_clean)
residuals_nbcl <- residuals(nb_model_clean, type = "deviance")

# Residual vs Fitted Plot
plot(fitted_vals_nbcl, residuals_nbcl,
     main = "Residuals vs Fitted (Negative Binomial-outliers)",
     xlab = "Fitted Values", ylab = "Deviance Residuals")
abline(h = 0, col = "red", lwd = 2)


# Now you can use the halfnorm() function
# Compute deviance residuals for your Negative Binomial model
residuals_nbcl <- residuals(nb_model_clean, type = "deviance")

# Half-normal plot with reference line and point labels for largest residuals
halfnorm(residuals_nbcl, 
         main = "Half-Normal Plot (Negative Binomial - without inf out)",
         ylab = "Deviance Residuals")

# Optional: Identify the top 10 largest residuals
top_rescl <- order(abs(residuals_nbcl), decreasing = TRUE)[1:10]
text(x = 1:10, 
     y = sort(abs(residuals_nbcl), decreasing = TRUE)[1:10], 
     labels = top_rescl, 
     pos = 3, 
     col = "red")


# QQ plot of Deviance Residuals
qqnorm(residuals_nbcl, main = "QQ Plot of Deviance Residuals")
qqline(residuals_nbcl, col = "red", lwd = 2)





# Plot histogram of Deviance Residuals
hist(residuals_nbcl, main = "Histogram of Deviance Residuals", 
     xlab = "Deviance Residuals", col = "lightblue", breaks = 50)




# Extract deviance residuals
residuals_nbcl <- residuals(nb_model_clean, type = "deviance")

# Create density plot
plot(density(residuals_nbcl, na.rm = TRUE),
     main = "Density Plot of Deviance Residuals (Negative Binomial)",
     xlab = "Deviance Residuals",
     col = "darkblue",
     lwd = 2)
polygon(density(residuals_nbcl, na.rm = TRUE), col = "lightblue", border = "darkblue")
abline(v = 0, col = "red", lwd = 2, lty = 2)  # Add vertical line at 0
####################################
#####################Further diagnostics for the outliers
###################

cooks_dcl <- cooks.distance(nb_model_clean)

plot(cooks_dcl, 
     type = "h", 
     main = "Cook's Distance for NB Model without outliers", 
     ylab = "Cook's Distance", 
     xlab = "Observation Index")
abline(h = 4/length(cooks_dcl), col = "red", lty = 2)  # Threshold line

fitted_valscl <- fitted(nb_model_clean)



####################
# Reset plotting space to default
par(mfrow = c(1, 1))

##############################


#############






###################
################PREDICTING WITH p and NB under glm
###############po_model
test_predsp <- predict(po_model, newdata = test_data, type = "response")
test_predsw <- predict(nb_model, newdata = test_data, type = "response")
test_predswo <- predict(nb_model_clean, newdata = test_data, type = "response")

# Assuming your test_data has the true deaths count
true_deaths <- test_data$deaths

# Root Mean Squared Error (RMSE)
rmsep <- sqrt(mean((test_predsp - true_deaths)^2))
rmsew <- sqrt(mean((test_predsw - true_deaths)^2))
rmsewo <- sqrt(mean((test_predswo - true_deaths)^2))
# Mean Absolute Error (MAE)
maep <- mean(abs(test_predsp - true_deaths))
maew <- mean(abs(test_predsw - true_deaths))
maewo <- mean(abs(test_predswo - true_deaths))
cat("RMSE:", rmsep, "\nMAE:", maep)
cat("RMSE:", rmsew, "\nMAE:", maew)
cat("RMSE:", rmsewo, "\nMAE:", maew)

######################gamlss
#####################gamlss
#####################gamlss
####################gamlss

library(gamlss)

# Poisson model in gamlss
po_gamlss <- gamlss(
  deaths ~ log(km + 1) + week_day + hour_of_day + road_delineation + 
    vehicles_involved + road_type + weather_timestamp + 
    road_direction + road_delineation:vehicles_involved,
  offset = log(people),
  family = PO,  # Poisson family
  data = na.omit(train_data)
)


summary(po_gamlss)
plot(po_gamlss)  # to assess residuals, fitted values, etc.


# Poisson model in gamlss
po_gamlsspb <- gamlss(
  deaths ~ pb(km) + week_day + pb(hour_of_day) + road_delineation + 
    pb(vehicles_involved) + road_type + weather_timestamp + 
    road_direction + road_delineation:pb(vehicles_involved),
  offset = log(people),
  family = PO,  # Poisson family
  data = na.omit(train_data)
)


summary(po_gamlsspb)
plot(po_gamlsspb)  # to assess residuals, fitted values, etc.

# fit negative binomial model
nb_gamlss <- gamlss(deaths ~log(km +1) + week_day +
                      hour_of_day + road_delineation + 
                      vehicles_involved + road_type + 
                      weather_timestamp + road_direction +
                      road_delineation:vehicles_involved+
                      offset(log(people)),
                    data = na.omit(train_data), family=NBI)


summary(nb_gamlss)
plot(nb_gamlss)



# fit negative pb binomial model
nb_gamlsspb <- gamlss(deaths ~pb(km) + week_day +
                        pb(hour_of_day) + road_delineation + 
                        pb(vehicles_involved) + road_type + 
                        weather_timestamp + road_direction +
                        road_delineation:pb(vehicles_involved)+
                        offset(log(people)),
                      data = na.omit(train_data), family=NBI)


summary(nb_gamlsspb)
plot(nb_gamlsspb)



# fit negative pb binomial model
nb_gamlssmall <- gamlss(deaths ~ week_day + road_delineation + 
                        pb(vehicles_involved) + road_type + 
                        weather_timestamp  +
                        road_delineation:pb(vehicles_involved)+
                        offset(log(people)),
                      data = na.omit(train_data), family=NBI)


summary(nb_gamlssmall)
plot(nb_gamlssmall)
# fit negative binomial model
nb_gamlsspb <- gamlss(deaths ~log(km + 1) + week_day +
                      hour_of_day + road_delineation + 
                      vehicles_involved + road_type + 
                      weather_timestamp + road_direction +
                      road_delineation:vehicles_involved+
                      offset(log(people)),
                    data = na.omit(train_data), family=NBI)


summary(nb_gamlsspb)
plot(nb_gamlsspb)


# fit negative pb binomial model
nb_gamlssmallwoweek <- gamlss(deaths ~ road_delineation + 
                          pb(vehicles_involved) + road_type + 
                          weather_timestamp  +
                          road_delineation:pb(vehicles_involved)+
                          offset(log(people)),
                        data = na.omit(train_data), family=NBI)


summary(nb_gamlssmallwoweek)
plot(nb_gamlssmallwoweek)

# Zero-Inflated Poisson
# Zero-Inflated Poisson

zip_gamlss <- gamlss(
  deaths ~ log(km + 1) + week_day + hour_of_day + road_delineation + 
    vehicles_involved + road_type + weather_timestamp + 
    road_direction + road_delineation:vehicles_involved,
  offset = log(people),
  family = ZIP,  
  data =  na.omit(train_data)
)

summary(zip_gamlss)
plot(zip_gamlss)




###############resolving convergence issues with RS
zip_gamlssCG <- gamlss(
  formula = deaths ~ log(km + 1) + week_day + hour_of_day +
    road_delineation + vehicles_involved + road_type +
    weather_timestamp + road_direction +
    road_delineation:vehicles_involved,
  family = ZIP,
  data = na.omit(train_data),
  offset = log(people),
  method = CG()
)

summary(zip_gamlssCG)

plot(zip_gamlssCG)

###############resolving convergence issues with CG n.cyc = 50
zip_gamlssCG <- gamlss(
  formula = deaths ~ log(km + 1) + week_day + hour_of_day +
    road_delineation + vehicles_involved + road_type +
    weather_timestamp + road_direction +
    road_delineation:vehicles_involved,
  family = ZIP,
  data = na.omit(train_data),
  offset = log(people),
  method = CG(n.cyc = 50)  # try increasing to 50 or more
)



summary(zip_gamlssCG)

plot(zip_gamlssCG)
###############resolving convergence issues with RS
zip_gamlssRS <- gamlss(
  formula = deaths ~ log(km + 1) + week_day + hour_of_day +
    road_delineation + vehicles_involved + road_type +
    weather_timestamp + road_direction +
    road_delineation:vehicles_involved,
  family = ZIP,
  data = na.omit(train_data),
  offset = log(people),
  method = RS()
)

###################


model_smooth <- gamlss(
  deaths ~ pb(km) + pb(vehicles_involved) + road_delineation + 
    week_day + weather_timestamp + pb(hour_of_day) + 
    road_type * vehicles_involved + road_direction,
  offset = log(people),
  family = PO,
  data = na.omit(train_data)
)

summary(model_smooth)

plot(model_smooth)
###################
# Transform and model log(deaths + 1)
train_data$log_deaths <- log(train_data$deaths + 1)

model_cont <- gamlss(
  log_deaths ~ pb(km) + pb(vehicles_involved) + 
    road_delineation + week_day + weather_timestamp + 
    pb(hour_of_day) + road_type * pb(vehicles_involved) + 
    road_direction,
  family = NO,
  data = na.omit(train_data)
)



summary(model_cont)

plot(model_cont)

# Transform and model sqrt(deaths + 1)
train_data$sqrt_deaths <- sqrt(train_data$deaths + 1)

model_sqrt <- gamlss(
  sqrt_deaths ~ pb(km) + pb(vehicles_involved) + 
    road_delineation + week_day + weather_timestamp + 
    pb(hour_of_day) + road_type * vehicles_involved + 
    road_direction,
  family = NO,
  data = na.omit(train_data)
)


summary(model_sqrt)

plot(model_sqrt)


train_data$rate <- train_data$deaths / train_data$people

model_gamma <- gamlss(
  rate ~ pb(km) + pb(vehicles_involved) + 
    road_delineation + week_day + weather_timestamp + 
    pb(hour_of_day) + road_type * vehicles_involved + 
    road_direction,
  family = GA,
  data = na.omit(train_data)
)



summary(model_gamma)

plot(model_gamma)


##################
##############GAMLSS
###################

model_zap <- gamlss(deaths ~ 
                        pb(vehicles_involved) + 
                        road_delineation + 
                        week_day + weather_timestamp + 
                        pb(hour_of_day) + 
                        road_type * vehicles_involved + 
                        road_direction, 
                      family = ZAP, 
                      offset = log(people),  # Adding the offset for people
                      data = na.omit(train_data))

summary(model_zap)

plot(model_zap)




model_zappb <- gamlss(deaths ~ road_delineation + 
                      week_day + weather_timestamp + 
                      pb(hour_of_day) + 
                        road_type + pb(vehicles_involved) +
                        road_type:pb(vehicles_involved) + 
                      road_direction, 
                    family = ZAP, 
                    offset = log(people),  # Adding the offset for people
                    data = na.omit(train_data))

summary(model_zappb)

plot(model_zappb)




model_zap <- gamlss(deaths ~
                      vehicles_involved + 
                      road_delineation + 
                      week_day + weather_timestamp + 
                      road_type * vehicles_involved + 
                      road_direction, 
                    family = ZAP, 
                    offset = log(people),  # Adding the offset for people
                    data = na.omit(train_data))

summary(model_zap)

plot(model_zap)
save.image("my_workspace.RData")  # Saves everything




####################ZERO INFLATED NB


model_zanbi <- gamlss(deaths ~ 
                        pb(vehicles_involved) + 
                        road_delineation + 
                        week_day + 
                        weather_timestamp + 
                        pb(hour_of_day) + 
                        road_type * vehicles_involved + 
                        road_direction +offset(log(people)),
                      family = ZANBI, 
                      data = na.omit(train_data))

summary(model_zanbi)
plot(model_zanbi)



####################
###############new dimension
################
###################


# Load required libraries
library(gamlss)
library(ggplot2)
library(dplyr)
library(MASS)

model_ZIP <- gamlss(deaths ~ pb(vehicles_involved) + 
                      road_delineation + 
                      week_day + weather_timestamp + 
                      road_type * pb(vehicles_involved), 
                    family = ZIP, 
                    offset = log(people),  # Adding the offset for people
                    data = na.omit(train_data))


summary(model_ZIP)

plot(model_ZIP)
model_zap <- gamlss(deaths ~ pb(vehicles_involved) + 
                       road_delineation + 
                       week_day + weather_timestamp + 
                       road_type * pb(vehicles_involved) + 
                       road_direction, 
                     family = ZAP, 
                     offset = log(people),  # Adding the offset for people
                     data = na.omit(train_data))

summary(model_zap)

plot(model_zap)



######TWEEDIE

model_TW <- gamlss(deaths ~ pb(vehicles_involved) + 
                     road_delineation + 
                     week_day + weather_timestamp + 
                     road_type * pb(vehicles_involved),
                   family = TF(),  # Tweedie family in GAMLSS
                   offset = log(people),
                   data = na.omit(train_data))

summary(model_TW)

plot(model_TW)



model_ZINB <- gamlss(
  deaths ~ km +
    road_delineation + 
    week_day + road_direction+
    weather_timestamp + 
    hour_of_day + 
    road_type * vehicles_involved,
  family = ZINBI,
  offset = log(people),
  data = na.omit(train_data)
)




summary(model_ZINB)

plot(model_ZINB)


# fit negative pb binomial model
model_ZINB <- gamlss(deaths ~ road_delineation + 
                       pb(vehicles_involved) + road_type + 
                       weather_timestamp  +
                       road_delineation:pb(vehicles_involved)+
                       offset(log(people)),
                     data = na.omit(train_data), family=NBI)


summary(model_ZINB)

plot(model_ZINB)





# Use cubic splines (cs) instead of pb for speed, 
#and set df to reduce complexity
model_ZINB_light <- gamlss(
  deaths ~ cs(vehicles_involved, df = 3) + 
    road_delineation + 
    week_day + 
    weather_timestamp + 
    road_type * cs(vehicles_involved, df = 3),
  family = ZINBI,
  offset = log(people),
  data = na.omit(train_data),
  trace = FALSE  # suppress iterative fitting messages
)


summary(model_ZINB_light)

plot(model_ZINB_light)

###################################
#########ZAGA
###########################
################




model_zaga <- gamlss(deaths ~ vehicles_involved + 
                       road_delineation + 
                       week_day + weather_timestamp + 
                       road_type * vehicles_involved,
                     family = ZAGA, 
                     offset = log(people), 
                     data = na.omit(train_data))



summary(model_zaga)


plot(model_zaga)



#######################zaga with pb()
###############################
####################



model_zagapb <- gamlss(deaths ~ pb(vehicles_involved) + 
                         road_delineation + 
                         week_day + weather_timestamp + offset(log(people))+
                         road_type * pb(vehicles_involved),
                       family = ZAGA, 
                       data = na.omit(train_data))



summary(model_zagapb)


plot(model_zagapb)






########################
#######################ZAIG
#############
model_ZAIG <- gamlss(deaths ~ pb(vehicles_involved) + 
                       road_delineation + 
                       week_day + weather_timestamp + 
                       road_type * pb(vehicles_involved),
                     family = ZAIG, 
                     offset = log(people),
                     data = na.omit(train_data))



summary(model_ZAIG)


plot(model_ZAIG)



########################
#####################
#########################
##########################
###
##################

summary(pois_model)
summary(po_model)       
AIC(pois_model, po_model)


overdispersion_ratio <- deviance(pois_model) / df.residual(pois_model)
print(overdispersion_ratio)

NBoverdispersion_ratio <- deviance(nb_model) / df.residual(nb_model)
print(NBoverdispersion_ratio)

ZAPoverdispersion_ratio <- deviance(model_zap) / df.residual(model_zap)
print(ZAPoverdispersion_ratio)


AIC(pois_model, nb_model)
GAIC(model_zap, model_ZIP, model_TW, model_ZINB)


AIC(pois_model, nb_model,model_zap, model_ZIP, model_TW, model_ZINB)
GAIC(pois_model, nb_model,model_zap, model_ZIP, model_TW, model_ZINB)
library(pscl)
vuong(pois_model, nb_model,model_zap, model_ZIP, model_TW, model_ZINB)








library(MASS)
##########randomized quantiles

##############################
# Diagnostic Plots
##############################
# Set up a 2x2 plotting space for base R plots
par(mfrow = c(2, 2))

# Plot Pearson Residuals
plot(fitted(pois_model), pearson_res,
     main = "Pearson Residuals", xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "green")

# Plot Deviance Residuals
plot(fitted(pois_model), deviance_res,
     main = "Deviance Residuals", xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "pink")

# Plot Randomized Quantile Residuals
plot(fitted(pois_model), qres,
     main = "Randomized Quantile Residuals", xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "blue")

# QQ Plot of Quantile Residuals
qqnorm(qres, main = "QQ Plot of Quantile Residuals")
qqline(qres, col = "red")

# Reset plotting space to default
par(mfrow = c(1, 1))

##############################
###################
################PREDICTING WITH p and NB under glm
###############po_model
test_predsp <- predict(po_model, newdata = test_data, type = "response")
test_predsw <- predict(nb_model, newdata = test_data, type = "response")
test_predswo <- predict(nb_model_clean, newdata = test_data, type = "response")

# Assuming your test_data has the true deaths count
true_deaths <- test_data$deaths

# Root Mean Squared Error (RMSE)
rmsep <- sqrt(mean((test_predsp - true_deaths)^2))
rmsew <- sqrt(mean((test_predsw - true_deaths)^2))
rmsewo <- sqrt(mean((test_predswo - true_deaths)^2))
# Mean Absolute Error (MAE)
maep <- mean(abs(test_predsp - true_deaths))
maew <- mean(abs(test_predsw - true_deaths))
maewo <- mean(abs(test_predswo - true_deaths))
cat("RMSE:", rmsep, "\nMAE:", maep)
cat("RMSE:", rmsew, "\nMAE:", maew)
cat("RMSE:", rmsewo, "\nMAE:", maew)

######################
library(MASS)



pois_model <- glm(
  deaths ~ week_day + road_delineation + 
    vehicles_involved + road_type + weather_timestamp + 
    road_delineation:vehicles_involved,
  offset = log(people),
  family = poisson(link = "log"),
  data = train_data
)

po_model <- glm(
  deaths ~ week_day + road_delineation + 
    vehicles_involved + road_type + weather_timestamp + 
    road_type:vehicles_involved,
  offset = log(people),
  family = poisson(link = "log"),
  data = train_data
)
AIC( po_model, pois_model)
vif(po_model)
#####################


# Required packages
library(MASS)
# Initialize result storage
model_perf <- data.frame(
  Model = character(),
  AIC = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  MBE = numeric(),
  ROC_AUC = numeric(),
  Zero_Pred_Acc = numeric(),
  McFadden_R2 = numeric(),
  stringsAsFactors = FALSE
)

# Actual responses
actual <- test_data$deaths

# Loop over models
for (model_name in c("po_model", "nb_model")) {
  mod <- get(model_name)
  
  # Predicted response
  pred <- predict(mod, newdata = test_data, type = "response")
  
  # Basic error metrics
  rmse_val <- sqrt(mean((actual - pred)^2))
  mae_val  <- mean(abs(actual - pred))
  mbe_val  <- mean(pred - actual)
  
  # Binary for zero vs non-zero prediction
  actual_binary <- ifelse(actual == 0, 0, 1)
  pred_binary   <- ifelse(pred == 0, 0, 1)
  
  # ROC AUC (optional)
  roc_auc_val <- tryCatch({
    pROC::auc(pROC::roc(actual_binary, pred))
  }, error = function(e) NA)
  
  # Zero prediction accuracy
  zero_acc <- mean((pred < 0.5) == (actual == 0))
  
  # McFadden’s pseudo R²
  mcFadden_r2 <- tryCatch({
    1 - (mod$deviance / mod$null.deviance)
  }, error = function(e) NA)
  
  # Append to results
  model_perf <- rbind(model_perf, data.frame(
    Model = model_name,
    AIC = AIC(mod),
    RMSE = rmse_val,
    MAE = mae_val,
    MBE = mbe_val,
    ROC_AUC = roc_auc_val,
    Zero_Pred_Acc = zero_acc,
    McFadden_R2 = mcFadden_r2
  ))
}

# Print the comparison table
print(model_perf)







library(ggplot2)

# Add predictions to test_data
test_data$pred_poisson <- predict(po_model, newdata = test_data, type = "response")
test_data$pred_nb <- predict(nb_model, newdata = test_data, type = "response")

# Melt the data for ggplot2
library(reshape2)
plot_data <- melt(test_data[, c("deaths", "pred_poisson", "pred_nb")], 
                  id.vars = "deaths", variable.name = "Model", value.name = "Predicted")

# Clean model labels
plot_data$Model <- factor(plot_data$Model, 
                          levels = c("pred_poisson", "pred_nb"),
                          labels = c("Poisson", "Negative Binomial"))

# Plot actual vs predicted
ggplot(plot_data, aes(x = deaths, y = Predicted, color = Model)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Actual vs Predicted Deaths",
       x = "Actual Deaths",
       y = "Predicted Deaths",
       color = "Model") +
  theme_minimal()















#############################################
#############due to non-normal fitted-residual values I try ZAGA and ZAIG too.
#############due to non-normal fitted-residual values I try ZAGA and ZAIG too.
#############due to non-normal fitted-residual values I try ZAGA and ZAIG too.
#############due to non-normal fitted-residual values I try ZAGA and ZAIG too.
#############due to non-normal fitted-residual values I try ZAGA and ZAIG too.
#############due to non-normal fitted-residual values I try ZAGA and ZAIG too.
#############due to non-normal fitted-residual values I try ZAGA and ZAIG too.
#############due to non-normal fitted-residual values I try ZAGA and ZAIG too.
#########################new gamlss





