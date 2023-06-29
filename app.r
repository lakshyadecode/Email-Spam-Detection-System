library(tm)
library(e1071)
dataset <- read.csv("dataset.csv", stringsAsFactors = FALSE)
corpus <- Corpus(VectorSource(dataset$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
dtm <- DocumentTermMatrix(corpus)
dtm_df <- as.data.frame(as.matrix(dtm))
dtm_df$label <- dataset$label
dtm_df$label <- factor(dtm_df$label, levels = c("ham", "spam"))
set.seed(123)
train_idx <- sample(1:nrow(dtm_df), nrow(dtm_df) * 0.8)  # 80% for training
train_data <- dtm_df[train_idx, ]
test_data <- dtm_df[-train_idx, ]
model <- naiveBayes(label ~ ., data = train_data)
predictions <- predict(model, newdata = test_data)
confusion_matrix <- table(predictions, test_data$label)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))
