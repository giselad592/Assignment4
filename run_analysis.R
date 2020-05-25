library(dplyr) #loads dplyr package
subject_test <- read.table("./Assignment/UCI HAR Dataset/test/subject_test.txt") #data frame containing info about which subjects participated in each observation
X_test <- read.table("./Assignment/UCI HAR Dataset/test/X_test.txt") #data frame contains 561 features for each observation
y_test <- read.table("./Assignment/UCI HAR Dataset/test/y_test.txt") # data frame containing information about which activity was performed for each observation
subject_train <- read.table("./Assignment/UCI HAR Dataset/train/subject_train.txt") #data frame containing info about which subjects participated in each observation
X_train <- read.table("./Assignment/UCI HAR Dataset/train/X_train.txt") #data frame contains 561 features for each observation
y_train <- read.table("./Assignment/UCI HAR Dataset/train/y_train.txt") # data frame containing information about which activity was performed for each observation
features_code <- read.table("./Assignment/UCI HAR Dataset/features.txt") #data frame containing index for each feature
merged_subject <- rbind(subject_test, subject_train) #binds subjects by rows
merged_subject_renamed <- rename(merged_subject, subject_id = V1) #renames the column
merged_features <- rbind(X_test, X_train) #binds features by rows
merged_activity <- rbind(y_test, y_train) #binds activity by rows
merged_activity_renamed <- rename(merged_activity, activity = V1) #renames the column
unified_data <- cbind(merged_subject_renamed, merged_activity_renamed, merged_features) #binds columns together to form one dataset
index <-grep("mean[:(:]|std[:(:]", features_code$V2) #finds index of features that contain mean or std
std_mean_data <- cbind(unified_data[,1:2], unified_data[,index+2]) #combines columns that contain only mean and std of features
std_mean_data_act_labeled <-std_mean_data %>% #substitutes codes for names of activities
        mutate(activity = gsub("1", "walking", activity)) %>%
        mutate(activity = gsub("2", "walking_upstairs", activity)) %>% 
        mutate(activity = gsub("3", "walking_downstairs", activity)) %>% 
        mutate(activity = gsub("4", "sitting", activity)) %>% 
        mutate(activity = gsub("5", "standing", activity)) %>% 
        mutate(activity = gsub("6", "laying", activity))
select_data <- select(std_mean_data_act_labeled,subject_id, activity, V1, V2, V3, V4, V5, V6) #select only one type of observation, in this case, bodu acceleration signals (tBodyAcc-XYZ)
renamed_data <- renamed_data <- rename(select_data, mean_x = V1, #renames columns
                                       mean_y = V2, 
                                       mean_z = V3, 
                                       std_x = V4, 
                                       std_y = V5, 
                                       std_z= V6)
grouped_data <- renamed_data %>% group_by(subject_id, activity) #groups data by activity then by subject
final_data <-summarize(grouped_data, Average_mean_x = mean(mean_x), #summarizes the averages in a tiry dataset
                       Average_mean_y = mean(mean_y), 
                       Average_mean_z = mean(mean_z),
                       Average_std_x = mean(std_x),
                       Average_std_y = mean(std_y),
                       Average_std_z = mean(std_z))
print(final_data)
