#library(miceplorer)

#data <- clean_mouse_data("data-raw/mousedata.xlsx")

#for (i in seq_along(data)) {
 # file_name <- paste0("data/", names(data)[i], ".csv")
 # write.csv(data[[i]], file = file_name, row.names = FALSE)
#}


#treatment_info <- create_treatment_info(data$birth)

#weightoutliers <- identifyoutliers(data$weight, "Body Weight", treatment_info)
#outcomeoutliers <- identifyoutliers(data$outcome, "Outcome", treatment_info)

#weightcheck <- weightloss_check(data$weight, "Body Weight")

#plots <- plot_indmeasurement(data$weight, "Body Weight", treatment_info)
#print(plots$treatment)
#print(plots$placebo)
