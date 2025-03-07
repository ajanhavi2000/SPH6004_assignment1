library(VIM)
Samples <- read.csv("sph6004_assignment1_data.csv")

length(unique(Samples$id))

Samples_1 <- Samples[complete.cases(Samples),]
nrow(Samples_1)#No complete cases?????
rm(Samples_1)
Samples <- Samples[,-2] #remove hospital mortality --> not a predictor

###any samples for which we do not have the outcome?
any(is.na(Samples$aki_stage))

###data is sparse --> i would need to impute values
###Some columns are rly sparce and some rows are rly sparse
###Sparse cols = not informative --> even if imputed, not sure the vals will even be reliable cos so few data points
###remove sparse cols first (> 20% missing vals), then remove sparse rows (> 20% missing)
#out of 162 predictor variables, 65 remains
prop_na_col <- colMeans(is.na(Samples))
hist(prop_na_col, main = "Distribution of proportion of NA in columns", xlab = "Proportion of NA",freq = F)
abline(v = 0.2, col = "red")
Sparseness_threshold <- 0.80

Samples_filtered <- Samples[,colMeans(!is.na(Samples)) >= Sparseness_threshold]

prop_na_row <- rowMeans(is.na(Samples_filtered))
hist(prop_na_row, main = "Distribution of proportion of NA in rows\nafter removing sparse cols", 
     xlab = "Proportion of NA",freq = F, xlim = c(0,1), breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
abline(v = 0.2, col = "red")

par(mfrow = c(1,2))
hist(prop_na_col, main = "Distribution of proportion of NA in columns", xlab = "Proportion of NA",freq = F)
abline(v = 0.2, col = "red")
hist(prop_na_row, main = "Distribution of proportion of NA in rows\nafter removing sparse cols", 
     xlab = "Proportion of NA",freq = F, xlim = c(0,1), breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
abline(v = 0.2, col = "red")


Samples_filtered <- Samples_filtered[rowMeans(!is.na(Samples_filtered)) >= Sparseness_threshold,]
nrow(Samples_filtered)
rm(Samples, Sparseness_threshold)
#out of 50920 entries, 50132 remain

###all samples except gender, race, and aki stage are numeric samples
Numeric_samples <- Samples_filtered[,c(1,4,6:ncol(Samples_filtered))]
Numeric_samples <- as.data.frame(apply(Numeric_samples, 2, as.numeric))
ncol(Samples_filtered)
ncol(Numeric_samples)

###check data for weird values
Numeric_samples_min <- apply(Numeric_samples, 2, min, na.rm = T)
Numeric_samples_max <- apply(Numeric_samples, 2, max, na.rm = T)
Numeric_summary <- cbind(Numeric_samples_min, Numeric_samples_max)
Numeric_summary

###odd values spotted in some columns, mainly, glucose cols (min, max, mean) and the weight col (5000 kg or 1kg)
###even people with diabetes have arnd 125g/dL... so this is definitely strange
###for lab glucose, a max of 2440 is definitely out there, but does not seem impossible from googling around
###however, 999999mg/dL is very unlikely for the vital glucose cols
problems <- which(Numeric_samples$glucose_vital_mean == max(Numeric_samples$glucose_vital_mean,na.rm = T))
Numeric_samples$glucose_lab_max[problems]
Numeric_samples$glucose_lab_min[problems]
Numeric_samples$glucose_vital_min[problems]
Numeric_samples$glucose_vital_max[problems]
###lab values are normal but vital values are odd --> likely that its some instrumental error, can remove and impute

###get z scores for all the values in the problematic cols, then get rows that exceed z-score of 3, set them to NA to be imputed
problem_cols <- cbind(Numeric_samples$glucose_vital_max, Numeric_samples$glucose_vital_mean, 
                  Numeric_samples$glucose_vital_min, Numeric_samples$weight_admit)
z_scores_problem_cols <- abs(scale(problem_cols))

which(z_scores_problem_cols[,1] > 3)
Numeric_samples$glucose_vital_max[which(z_scores_problem_cols[,1] > 3)] #gets all the 999999 values
Numeric_samples$glucose_vital_max[which(z_scores_problem_cols[,1] > 3)] <- NA

which(z_scores_problem_cols[,2] > 3)
Numeric_samples$glucose_vital_mean[which(z_scores_problem_cols[,2] > 3)]
Numeric_samples$glucose_vital_mean[which(z_scores_problem_cols[,2] > 3)] <- NA

which(z_scores_problem_cols[,3] > 3)
Numeric_samples$glucose_vital_min[which(z_scores_problem_cols[,3] > 3)]
Numeric_samples$glucose_vital_min[which(z_scores_problem_cols[,3] > 3)] <- NA

which(z_scores_problem_cols[,4] > 3)
Numeric_samples$weight_admit[which(z_scores_problem_cols[,4] > 3)]
###At Z-score of 3, the weights are more "extreme" but still not impossible?
###however, low weigths (1 - 30kg) are still not removed here.
###The use of hard thresholds might be more appropriate
lower_threshold <- 30
upper_threshold <- 300

Problem_weights <- which(Numeric_samples$weight_admit < lower_threshold | Numeric_samples$weight_admit > upper_threshold)
Numeric_samples$weight_admit[Problem_weights] <- NA

###recheck data
Numeric_samples_min <- apply(Numeric_samples, 2, min, na.rm = T)
Numeric_samples_max <- apply(Numeric_samples, 2, max, na.rm = T)
Numeric_summary <- cbind(Numeric_samples_min, Numeric_samples_max)
Numeric_summary

###outliers dealt with, set full subset of samples
Samples_filtered[,c(1,4,6:ncol(Samples_filtered))] <- Numeric_samples
rm(Numeric_samples, Numeric_summary, problem_cols, z_scores_problem_cols, Numeric_samples_max, 
   Numeric_samples_min, Problem_weights, problems, lower_threshold, upper_threshold)


###check data balance --> seems relatively balanced, none of the classes are below 15% of the samples (7871/50132)
###is not severely imbalanced in terms of absolute terms
table(Samples_filtered$aki_stage)/nrow(Samples_filtered)*100

table(Samples_filtered$gender)
length(unique(Samples_filtered$race))
table(Samples_filtered$race)
min(table(Samples_filtered$race))
max(table(Samples_filtered$race))
###so many categories, might be good to combine,since some have very few numbers
new_race <- c()
for (i in 1:nrow(Samples_filtered)){
  print(i)
  if (grepl("UNKNOWN", Samples_filtered$race[i], fixed = TRUE)){
    new_race <- c(new_race, "UNKNOWN")
  } else if (grepl("UNABLE TO OBTAIN", Samples_filtered$race[i], fixed = TRUE)){
    new_race <- c(new_race, "UNKNOWN")
  } else if (grepl("PATIENT DECLINED TO ANSWER", Samples_filtered$race[i], fixed = TRUE)){
    new_race <- c(new_race, "UNKNOWN")
  } else if (grepl("WHITE", Samples_filtered$race[i], fixed = TRUE)){
    new_race <- c(new_race, "WHITE")
  } else if (grepl("PORTUGUESE", Samples_filtered$race[i], fixed = TRUE)){
    new_race <- c(new_race, "WHITE")
  }else if (grepl("BLACK", Samples_filtered$race[i], fixed = TRUE)){
    new_race <- c(new_race, "BLACK")
  } else if (grepl("HISPANIC", Samples_filtered$race[i], fixed = TRUE)){
    new_race <- c(new_race, "HISPANIC")
  } else if (grepl("SOUTH AMERICAN", Samples_filtered$race[i], fixed = TRUE)){
    new_race <- c(new_race, "HISPANIC")
  }else if (grepl("ASIAN", Samples_filtered$race[i], fixed = TRUE)){
    new_race <- c(new_race, "ASIAN")
  } else if (grepl("NATIVE", Samples_filtered$race[i], fixed = TRUE)){
    new_race <- c(new_race, "NATIVE")
  } else {
    new_race <- c(new_race, "OTHER")
  }
  
}
unique(Samples_filtered$race[which(new_race == "UNKNOWN")])
unique(Samples_filtered$race[which(new_race == "WHITE")])
unique(Samples_filtered$race[which(new_race == "BLACK")])
unique(Samples_filtered$race[which(new_race == "HISPANIC")])
unique(Samples_filtered$race[which(new_race == "ASIAN")])
unique(Samples_filtered$race[which(new_race == "NATIVE")])
unique(Samples_filtered$race[which(new_race == "OTHER")])

###NOW! dealing with missing data: can i get away with filtering now?
Samples_1 <- Samples_filtered[complete.cases(Samples_filtered),]
100 - nrow(Samples_1)/nrow(Samples_filtered)*100
#about 24% of samples are discarded if i were to delete all the rows with missing values --> might not be a good idea
rm(Samples_1)

Samples_filtered$race <- as.factor(new_race)
Samples_filtered$gender <- as.factor(Samples_filtered$gender)
Samples_filtered$aki_stage <- as.factor(Samples_filtered$aki_stage)
###TRY KNN
###can try different k and test against model accuracy but we have so many models
###so this increases the computational complexity of the problem
### VIM handles categorical data separately, as long as they are set as factors
chosen_k = round(sqrt(nrow(Samples_filtered))) - 1

###to prevent leakage, need to remove the AKI column for KNN imputation
###remove 
Sample_id <- Samples_filtered$id
Sample_aki <- Samples_filtered$aki_stage

Samples_filtered <- Samples_filtered[,3:ncol(Samples_filtered)]

all_numeric_values <-sapply(Samples_filtered , is.numeric)
Samples_filtered[all_numeric_values] <- scale(Samples_filtered[all_numeric_values])
###double check if cat variables are cat
levels(Samples_filtered$gender)
levels(Samples_filtered$race)
rm(all_numeric_values,new_race,i)

imputed_data <- VIM::kNN(Samples_filtered, k = chosen_k)
imputed_data <- imputed_data[,colnames(Samples_filtered)]
imputed_data$aki <- Sample_aki

####export filled samples to csv
write.csv(imputed_data, file = "full_data.csv", row.names = F)

library(corrplot)
full_data <- read.csv("full_data.csv")
numerical_cols <- full_data[,c(2,4:ncol(full_data))]
cor_matrix <- cor(numerical_cols, use = "pairwise.complete.obs")
corrplot(cor_matrix, order = "hclust", tl.pos = "n")


