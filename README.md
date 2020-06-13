## Working of the script

The script reads the training and testing data and converts them into a data frame by doing multiple manipulations like - converting character strings into numeric data and attaching each line from the text files into a data frame using rbind method, combining it with the testing data, adding column names and making sure each column represents one type of observation only.

Then, the activity labels are combined into the data frame and the numeric values are converted into names giving the activities performed and the subjects list is added into the data frame. Later, the data is subsetted for only mean and standard deviation of the various measurements. Then, the data is grouped by the activity labels and the subjects who performed the activity.

Then, the mean of each variable was taken with respect to each activity and each subject and the dataset obtained was saved into a text file
