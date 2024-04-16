
setwd("C:/Users/wg1075/Downloads/TRU/dasc6820/Winter 2024 - Project 2 Seminar")

print_stats <- function(df){
  cat(paste0(
  'Total Rows: ',nrow(df),'\n',
  'No Frauds ', round(prop.table(table(df$Class))[1] * 100, 2), '% of the dataset\n',
  'Frauds ', round(prop.table(table(df$Class))[2] * 100, 2), '% of the dataset\n',
  'Columns names: ',paste(names(df), collapse = ','),'\n',
  "Quantity of na values in dataset: ",max(colSums(is.na(df))),'\n'))
  knitr::kable(head(df[,c(1:4,(ncol(df)-3):ncol(df))]))
}
colors <- c("cyan", "violet","turquoise")

df <- read.csv('creditcard.csv')
summary(df)
print_stats(df)

ggplot2::ggplot(df, ggplot2::aes(x = Class)) +
  ggplot2::geom_bar(fill = c("blue","red")) +
  ggplot2::labs(title = 'Class Distributions \n (0: No Fraud || 1: Fraud)',
       x = 'Class') +
  ggplot2::geom_text(stat = 'count', ggplot2::aes(label = ..count..), vjust = -0.5) +
  ggplot2::theme_minimal()

plot_data <- data.frame(Amount = df$Amount, Time = df$Time)
plot1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Amount)) +
  ggplot2::geom_density(fill = "red") +
  ggplot2::labs(title = "Distribution of Transaction Amount", x = "Amount") +
  ggplot2::theme_minimal()

plot2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Time)) +
  ggplot2::geom_density(fill = "blue") +
  ggplot2::labs(title = "Distribution of Transaction Time", x = "Time") +
  ggplot2::theme_minimal()

gridExtra::grid.arrange(plot1, plot2, ncol = 2)

robust_scaler <- function(x) {
  (x - median(x)) / IQR(x)
}

df$scaled_amount <- robust_scaler(df$Amount)
df$scaled_time <- robust_scaler(df$Time)

df$Time <- NULL
df$Amount <- NULL

scaled_amount <-df$scaled_amount
scaled_time <-df$scaled_time

df$scaled_amount <- NULL
df$scaled_time <- NULL
df <- cbind(scaled_amount, scaled_time, df)

print_stats(df)

# Split the data using stratified k-fold cross-validation
set.seed(728009)
train_index <- caret::createDataPartition(df$Class, p = 0.8, list = FALSE)
original_Xtrain <- df[train_index, -which(names(df) == 'Class')]
original_Xtest <- df[-train_index, -which(names(df) == 'Class')]
original_ytrain <- df[train_index, 'Class']
original_ytest <- df[-train_index, 'Class']

# Check the distribution of labels in train and test sets
cat('Label Distributions:\n',
    'Train:', table(original_ytrain) / length(original_ytrain), '\n',
    'Test:', table(original_ytest) / length(original_ytest), '\n')


set.seed(728009)
df <- df[sample(nrow(df)), ]

# Select rows with fraud class (Class = 1)
fraud_df <- df[df$Class == 1, ]

# Select a subset of rows with non-fraud class (Class = 0)
non_fraud_df <- df[df$Class == 0, ][1:nrow(fraud_df), ]

# Concatenate fraud and non-fraud dataframes
normal_distributed_df <- rbind(fraud_df, non_fraud_df)

# Shuffle the dataframe rows
set.seed(728009)
new_df <- normal_distributed_df[sample(nrow(normal_distributed_df)), ]

# Display the first few rows of the new dataframe
print_stats(new_df)

ggplot2::ggplot(new_df, ggplot2::aes(x = Class)) +
  ggplot2::geom_bar(fill = c("blue","red")) +
  ggplot2::labs(title = 'Equally Distributed Classes \n (0: No Fraud || 1: Fraud)',
                x = 'Class') +
  ggplot2::geom_text(stat = 'count', ggplot2::aes(label = ..count..), vjust = -0.5) +
  ggplot2::theme_minimal()


sub_sample_corr <- cor(new_df)
sub_sample_corr_melted <- reshape2::melt(sub_sample_corr)
dplyr::glimpse(sub_sample_corr_melted)
sub_sample_corr_melted_filtered = dplyr::filter(
  sub_sample_corr_melted, 
  Var1 != Var2 & abs(value) > 0.4,
  Var1 != "Class",
  Var2 != "Class"
) 
corr_plot2 <- ggplot2::ggplot(sub_sample_corr_melted_filtered, ggplot2::aes(Var1, Var2, fill = value, label = round(value, 2))) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_text(color = "white", size = 3) +
  ggplot2::scale_fill_gradient2(low = "lightblue", mid="orange", high = "black", name = "Correlation") +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Correlation Heatmap - Sub Sample Dataset") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

corr <- cor(df)
corr_melted <- reshape2::melt(corr)
dplyr::glimpse(corr_melted)
corr_melted_filtered = dplyr::filter(
  corr_melted, 
  Var1 != Var2 & abs(value) > 0.4,
  Var1 != "Class",
  Var2 != "Class"
) 
corr_plot1 <- ggplot2::ggplot(corr_melted_filtered, ggplot2::aes(Var1, Var2, fill = value, label = round(value, 2))) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_text(color = "white", size = 3) +
  ggplot2::scale_fill_gradient2(low = "lightblue", mid="orange", high = "black", name = "Correlation") +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Correlation Heatmap - Complete Dataset") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

gridExtra::grid.arrange(corr_plot1, nrow = 1)
gridExtra::grid.arrange(corr_plot2, nrow = 1)

# Boxplot for V17
plot17 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V17, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V17 vs Class Negative Correlation", x = "Class", y = "V17")

# Boxplot for V14
plot14 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V14, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V14 vs Class Negative Correlation", x = "Class", y = "V14")

# Boxplot for V12
plot12 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V12, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V12 vs Class Negative Correlation", x = "Class", y = "V12")

# Boxplot for V10
plot10 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V10, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V10 vs Class Negative Correlation", x = "Class", y = "V10")

# Arrange plots
gridExtra::grid.arrange(plot17, plot14, plot12, plot10, ncol = 2, nrow = 2)



# Boxplot for V11
plot11 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V11, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V11 vs Class Positive Correlation", x = "Class", y = "V11")

# Boxplot for V4
plot4 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V4, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V4 vs Class Positive Correlation", x = "Class", y = "V4")

# Boxplot for V2
plot2 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V2, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V2 vs Class Positive Correlation", x = "Class", y = "V2")

# Boxplot for V19
plot19 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V19, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V19 vs Class Positive Correlation", x = "Class", y = "V19")

# Arrange plots
gridExtra::grid.arrange(plot11, plot4, plot2, plot19, ncol = 2, nrow = 2)


dist14 <- ggplot2::ggplot(new_df[new_df$Class == 1, ], ggplot2::aes(x = V14)) +
  ggplot2::geom_density(fill = colors[1]) +
  ggplot2::labs(title = "V14 Distribution \n (Fraud Transactions)", x = "V14", y = "Density")

dist12 <- ggplot2::ggplot(new_df[new_df$Class == 1, ], ggplot2::aes(x = V12)) +
  ggplot2::geom_density(fill = colors[2]) +
  ggplot2::labs(title = "V12 Distribution \n (Fraud Transactions)", x = "V12", y = "Density")

dist10 <- ggplot2::ggplot(new_df[new_df$Class == 1, ], ggplot2::aes(x = V10)) +
  ggplot2::geom_density(fill = colors[3]) +
  ggplot2::labs(title = "V10 Distribution \n (Fraud Transactions)", x = "V10", y = "Density")

# Arrange plots
gridExtra::grid.arrange(dist14, dist12, dist10, ncol = 3)



# Removing outliers from V14 feature
v14_fraud <- new_df$V14[new_df$Class == 1]
quantiles <- quantile(v14_fraud, c(0.25, 0.75))
q25 <- quantiles[1]
q75 <- quantiles[2]
v14_iqr <- q75 - q25

v14_cut_off <- v14_iqr * 1.5
v14_lower <- q25 - v14_cut_off
v14_upper <- q75 + v14_cut_off

outliers <- new_df$V14[new_df$Class == 1 & (new_df$V14 < v14_lower | new_df$V14 > v14_upper)]
cat('Feature V14 Outliers for Fraud Cases:', length(outliers), '\n')

new_df <- new_df[!(new_df$V14 > v14_upper | new_df$V14 < v14_lower), ]

# Removing outliers from V12 feature
v12_fraud <- new_df$V12[new_df$Class == 1]
quantiles <- quantile(v12_fraud, c(0.25, 0.75))
q25 <- quantiles[1]
q75 <- quantiles[2]
v12_iqr <- q75 - q25

v12_cut_off <- v12_iqr * 1.5
v12_lower <- q25 - v12_cut_off
v12_upper <- q75 + v12_cut_off

outliers <- new_df$V12[new_df$Class == 1 & (new_df$V12 < v12_lower | new_df$V12 > v12_upper)]
cat('Feature V12 Outliers for Fraud Cases:', length(outliers), '\n')

new_df <- new_df[!(new_df$V12 > v12_upper | new_df$V12 < v12_lower), ]

# Removing outliers from V10 feature
v10_fraud <- new_df$V10[new_df$Class == 1]
quantiles <- quantile(v10_fraud, c(0.25, 0.75))
q25 <- quantiles[1]
q75 <- quantiles[2]
v10_iqr <- q75 - q25

v10_cut_off <- v10_iqr * 1.5
v10_lower <- q25 - v10_cut_off
v10_upper <- q75 + v10_cut_off

outliers <- new_df$V10[new_df$Class == 1 & (new_df$V10 < v10_lower | new_df$V10 > v10_upper)]
cat('Feature V10 Outliers for Fraud Cases:', length(outliers), '\n')

new_df <- new_df[!(new_df$V10 > v10_upper | new_df$V10 < v10_lower), ]

print_stats(new_df)





# Define colors
colors <- c("#B3F9C5", "#f9c5b3")

# Create boxplots with outliers removed
# V14 feature
plot1 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V14, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V14 Feature \n Reduction of outliers", x = "Class", y = "V14") +
  ggplot2::theme_minimal()

# V12 feature
plot2 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V12, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V12 Feature \n Reduction of outliers", x = "Class", y = "V12") +
  ggplot2::theme_minimal()

# V10 feature
plot3 <- ggplot2::ggplot(new_df, ggplot2::aes(x = factor(Class), y = V10, fill = factor(Class))) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "V10 Feature \n Reduction of outliers", x = "Class", y = "V10") +
  ggplot2::theme_minimal()

# Arrange plots
gridExtra::grid.arrange(plot1, ncol = 1)
gridExtra::grid.arrange(plot2, ncol = 1)
gridExtra::grid.arrange(plot3, ncol = 1)



# X and y from the random undersample data (fewer instances)
X <- new_df[, !(names(new_df) %in% "Class")]
#X <- X[!duplicated(X), ]
y <- new_df$Class

# T-SNE Implementation
t0 <- Sys.time()
X_reduced_tsne <- Rtsne::Rtsne(X, dims = 2, perplexity = 30, verbose = F, pca = TRUE, check_duplicates = FALSE)
t1 <- Sys.time()
print(paste0("T-SNE took ", round(difftime(t1, t0, units = "secs"), digits = 2), " s"))

# PCA Implementation
t0 <- Sys.time()
X_reduced_pca <- FactoMineR::PCA(X, ncp = 2, graph = FALSE)
t1 <- Sys.time()
print(paste0("PCA took ", round(difftime(t1, t0, units = "secs"), digits = 2), " s"))

# TruncatedSVD
t0 <- Sys.time()
X_reduced_svd <- Rtsne::Rtsne(X, dims = 2, pca = TRUE, check_duplicates = FALSE)
t1 <- Sys.time()
print(paste0("Truncated SVD took ", round(difftime(t1, t0, units = "secs"), digits = 2), " s"))

plot1 <- ggplot2::ggplot(data.frame(v1 = X_reduced_tsne$Y[,1]
                         , v2 = X_reduced_tsne$Y[,2] ), ggplot2::aes(x = v1, y = v2, color = as.factor(round(abs(v2),0) == 0))) +
  ggplot2::geom_point() +
  ggplot2::scale_color_manual(values = c("#0A0AFF", "#AF0000"), labels = c("No Fraud", "Fraud")) +
  ggplot2::labs(title = "t-SNE", color = "Class") +
  ggplot2::theme_minimal()

plot2 <- ggplot2::ggplot(data.frame(v1 = X_reduced_pca$ind$coord[,1]
                           , v2 = X_reduced_pca$ind$coord[,2] ), ggplot2::aes(x = v1, y = v2, color = as.factor(round(abs(v2),0) == 0))) +
  ggplot2::geom_point() +
  ggplot2::scale_color_manual(values = c("#0A0AFF", "#AF0000"), labels = c("No Fraud", "Fraud")) +
  ggplot2::labs(title = "PCA", color = "Class") +
  ggplot2::theme_minimal()


plot3 <- ggplot2::ggplot(data.frame(v1 = X_reduced_svd$Y[,1]
                           , v2 = X_reduced_svd$Y[,2] ), ggplot2::aes(x = v1, y = v2, color = as.factor(round(abs(v2),0) == 0))) +
  ggplot2::geom_point() +
  ggplot2::scale_color_manual(values = c("#0A0AFF", "#AF0000"), labels = c("No Fraud", "Fraud")) +
  ggplot2::labs(title = "SVD", color = "Class") +
  ggplot2::theme_minimal()

# Combine plots
gridExtra::grid.arrange(plot1, plot2, plot3, ncol = 1, nrow=3)


X <- new_df |> dplyr::select(-Class)
y <- new_df$Class

set.seed(728009)
train_indices <- caret::createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_indices, ]
y_train <- as.factor(y[train_indices])
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

dplyr::glimpse(X_train)
dplyr::glimpse(y_train)

dim(X_train)
length(y_train)

dim(X_test)
length(y_test)

# Define classifiers
classifiers <- list(
  "LogisticRegression" = list(method = "glm"),
  "KNearest" = list(method = "knn"),
  #"SupportVectorClassifierRadial" = list(method = "svmRadial"),
  "SupportVectorClassifierLinear" = list(method = "svmLinear"),
  #"SupportVectorClassifierPoly" = list(method = "svmPoly"),
  "DecisionTreeClassifier" = list(method = "rpart"),
  "NaiveBayesClassifier" = list(method = "nb")
)

# Train and evaluate classifiers with cross-validation
for (key in names(classifiers)) {
  #key <- "NaiveBayesClassifier"
  classifier <- classifiers[[key]]
  
  model <- caret::train(
    Class ~ ., 
    data = data.frame(X_train, Class = y_train), 
    method = classifier$method,
    trControl = caret::trainControl(method = "cv", number = 5),
    tuneGrid = NULL,
    preProc = NULL,
    metric = "Accuracy"
  )
  training_score <- model$results$Accuracy[1]
  print(paste("Classifier:", key, "has a cross-validated training score of", round(training_score * 100, 2) , "% accuracy score"))
}


