setwd("/Users/ejcain/Projects/module3/dsc-3-final-project-online-ds-ft-100118/")
library("tidyverse")

list.files(pattern = ".csv")

wp_feats = read.csv("waterpumpfeatures.csv")
train_targets = read.csv("traintargets.csv")

# predict which pumps are functional, non-functional, or functional 
# and in need of repair

# from DrivenData competition Pump it Up

# visualize different values of payment 

glimpse(wp_feats$payment)
glimpse(wp_feats)

# investigating water pressure

range(wp_feats$amount_tsh)
hist(wp_feats$amount_tsh)

summary(wp_feats)

hist(wp_feats$amount_tsh[wp_feats$amount_tsh < 100])

# can also use the pipe
wp_feats$amount_tsh[wp_feats$amount_tsh < 100] %>%
  hist()

# water pressure, most values are 0, mean of 317

# examine categorical variables

# shows each value and counts of each value
table(wp_feats$water_quality)

table(wp_feats$quality_group)

# looking at water quality of wells
wp_feats %>%
  ggplot(aes(water_quality)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Water Quality Groups", y = "Number of Wells")
# result = majority of well observations have soft water quality

# save.image("well_predictions.RData")
# load("well_predictions.RData") to get it back!

# package for plotting locations
install.packages("tmap")

# tmap Hello World
library(tmap)
# library("tmap")
data("World")

tm_shape(World) +
  tm_polygons("HPI")

summary(World)

install.packages("rworldmap")
library(rworldmap)

vignette("rworldmap")

# plot world map, xlim and ylim to zoom in
# plot points using lat and long columns
new_map = getMap(resolution = "low")
plot(new_map,
     xlim = c(30, 50),
     ylim = c(-8, -4))
points(wp_feats$longitude, wp_feats$latitude, col = "red", cex = 0.2)


# combine feature and target variables

# library(tidyverse)
train_targets = read.csv("traintargets.csv")

total_df = left_join(train_targets, wp_feats, by = "id")

head(total_df)

# explore target variables

# tanzania_flag_colors = ['#1eb53a', '#fcd116', '#00a3dd', 'black']

library(scales) # for plot axis formatting

total_df %>%
  # color = outline color
  # fill = fill color
  # scale color = define colors
  # fill color 
  ggplot(aes(status_group, fill = status_group)) +
  geom_bar() +
  scale_fill_manual(values = c('#1eb53a', '#fcd116', '#00a3dd')) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 1, hjust = 1, angle = 30)) +
  # to change the title of the legend, define label for "fill"
  # to have an empty x or y label, use blank quotes
  labs(x = "Labels", y = "Well Counts", fill = "Labels")

# check for na's and zeros

sum(is.na(total_df))
sum(is.null(total_df))

# there ARE NULLS! spaces weren't counted
# grep will find something
# gsub to replace something

# MAKE MY OWN FUNCTION!!!

# take subset of data, make something work, turn into function

install.packages("stringi")

library(stringi)

stri_isempty(total_df)

stri_isempty(total_df$scheme_management)

apply()

function_name = 

# 

empty_space_finder = apply(total_df, 2, function(x){
  stri_isempty(x) %>%
    sum()
})

unique(total_df$scheme_name)

unique(total_df$scheme_management)  

# Drop scheme_name from total_df because there are a million different values
# Drop scheme_management, create new_scheme_management where 
filter.total_df = total_df[, !names(total_df) %in% c("scheme_name", "id",
                                              "scheme_management", "recorded_by",
                                              "quantity_group", "payment_type",
                                              "quality_group", "quantity_group",
                                              "waterpoint_type_group",
                                              "public_meeting", "permit")]

# deleted empty_space_finder object with rm(empty_space_finder) and just
# use apply
apply(total_df, 2, function(x){
  stri_isempty(x) %>%
    sum()
})  

# Change null values in scheme_management to "Unknown" in new column

unique(total_df$permit)
table(total_df$permit)

# Check correlation for funsies
cor.test()
status_group_num = as.numeric(total_df$status_group)
permit_num = as.numeric(total_df$permit)

cor.test(x = status_group_num, y = permit_num)

cor.test(x = status_group_num, y = permit_num, method = "spearman")

# s apply returns a vector and only works on columns
# categoricals is a new df with only columns that are factors
categoricals = total_df[,sapply(total_df, is.factor)]
everything_else = total_df[,!sapply(total_df, is.factor)]

table(categoricals$status_group)

uniques = sapply(categoricals, function(e){
  length(table(e))
})

# which needs a logical argument, defaults to true
small_categoricals = categoricals[which(uniques < 10)]

# when you are giving a function name for apply, you don't need the parentheses
sapply(small_categoricals, table)

# need to decide what to do with public meeting and permit nulls

# apply(total_df, 2, function(x){
#   stri_isempty(x) %>%
#     sum()
# }) 

replaced_categoricals = small_categoricals[replace(small_categoricals$public_meeting,
                                                   stri_isempty(small_categoricals$public_meeting),
                                                   "Unknown"),]

small_categoricals$public_meeting_edited = replace(as.character(small_categoricals$public_meeting), 
                  stri_isempty(small_categoricals$public_meeting),
                  "Unknown") %>%
  as.factor()

small_categoricals$permit_edited = replace(as.character(small_categoricals$permit), 
                                           stri_isempty(small_categoricals$permit),
                                           "Unknown") %>%
  as.factor()

new_all = cbind(small_categoricals, everything_else)

# region_code and district_code are discrete values
# convert to factors



# convert 0's in construction_year to Unknown
# convert year to character, then add the "Unknown," then factor

test_year = new_all$construction_year %>%
  as.character()

new_all$edited_construction_year = replace(test_year,
                                           test_year == "0",
                                           "Unknown") %>%
  as.factor()

new_all$district_code = new_all$district_code %>% as.character()
new_all$region_code = new_all$region_code %>% as.character()

phillip = new_all[, !names(new_all) %in% c("scheme_name", "id", 
                                           "scheme_management", "recorded_by",
                                           "quantity_group", "payment_type",
                                           "quality_group", "quantity_group",
                                           "waterpoint_type_group",
                                           "public_meeting", "permit",
                                           "num_private", "construction_year"
                                           )]

glimpse(phillip)

install.packages("tree")
install.packages("caret")

library("DataExplorer")
create_report(phillip)

plot_bar(phillip)

# standardize continuous values
# what to do with zeros?
# dummify categorical variables
# separate X (features) and y (labels)
# which is the best model?
# lots of zeros for longitutde and latitude
# DataExplorer did not know what to do with construction year and all of those Unknowns
# should construction year be numeric / continuous? - newer wells probably have a higher likelihood
# of being functional
# should we drop some features to run initial vanilla model?
# could go for random forest since there are so many features

library(caret)

# dummify categorical variables

#

# dummies = dummyVars(phillip$status_group ~ ., data = phillip[,!names(phillip) %in% "status_group"])

install.packages("dummies")
library(dummies)

# dummy_df = dummy(phillip$status_group, data = phillip[,!names(phillip) %in% "status_group"], 
                 verbose = TRUE)

dummies = dummyVars(~ ., data = phillip)

dumb_phillip = predict(dummies, newdata = phillip)

set.seed(987)
trainIndex <- createDataPartition(dumb_phillip$, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

#Converting outcome variable to numeric
phillip$status_group = phillip$status_group %>%
  as.numeric()

# id<-train_processed$Loan_ID
# train_processed$Loan_ID<-NULL

dumb_phillip = predict(dummies, newdata = phillip)

#Converting every categorical variable to numerical using dummy variables
# dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))

train_phillip <- data.frame(predict(dumb_phillip, newdata = train_))

# wtf am I doing

dmy <- dummyVars(" ~ .", data = phillip,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = phillip))

train_transformed$status_group<-as.factor(train_transformed$status_group)

glimpse(train_transformed[,1:10])

index <- createDataPartition(train_transformed$status_group, p=0.75, list=FALSE)
trainSet <- train_transformed[ index,]
testSet <- train_transformed[-index,]

str(trainSet)

install.packages("randomForest")
library(randomForest)

control <- rfeControl(functions = NULL,
                      method = "boot",
                      saveDetails = TRUE,
                      verbose = TRUE)
outcomeName<-'status_group'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
Loan_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)

# Homework: run subset of data and figure out the theory of what's going on behind the scenes

