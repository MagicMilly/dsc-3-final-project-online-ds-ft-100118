library(caret)
library(dplyr)

summary(feat_select)
feat_select

# top 5 variables chosen by Random Forest Feature Selection: f
# latitude, longitude, quantity.dry, gps_height, district_code.1
# create new df with latitude, longitude, quantity, gps_height
# will leave district_code out of this sample to reduce dimensionality
# don't forget status_group lol

the_chosen_df = dumb_phillip[, c("latitude", "longitude", "quantity.dry", "quantity.enough",
                                 "quantity.insufficient", "quantity.seasonal", "quantity.unknown",
                                 "gps_height", "status_group")]

# take random sample from the_chosen_df 

the_chosen_sample = the_chosen_df[sample(nrow(the_chosen_df), 10000), ]



the_chosen_sample = as.data.frame(the_chosen_sample)
  
samp = createDataPartition(the_chosen_sample$status_group, p = 0.75, list = FALSE) 
training = the_chosen_sample[samp,]  
testing = the_chosen_sample[-samp,]

training$status_group = as.factor(training$status_group)

# random forest model
rf_model = train(x = training[,-9],
                 y = training$status_group,
                 method = "rf")

# predict on test set
# predict(rf_model)
testing$status_group = as.factor(testing$status_group)
results = predict(rf_model, newdata = testing)
# predict(knnFit, type = "prob")

# sample code for multiple models
# bothModels <- list(
#   knn = knnFit,
#   tree = rpartFit)

# predict(bothModels)


