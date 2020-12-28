library(dplyr)
library(data.table)
library(tidyr)
library(igraph)
library(recommenderlab)
library(openxlsx)

# read data
transaction = read.csv('transactions.csv', header = TRUE)

categories = read.csv('categories.csv')
categories = subset(categories, select = -c(7,8,9,10,11))
categories[!(categories$Item.Sub.Category == 'NULL'),]

book_category = categories[categories$Item.Category == 'BOOKS',]
book_category = book_category[!(book_category$Item.Sub.Category == 'NULL'),]
all_category_ID = c(book_category$Product.Category.ID)
all_category_name = c(book_category$Item.Sub.Category)

# remove rows that no value for category
book_transaction = transaction %>% filter(
  prod_category_id %in% all_category_ID,
  domain_name == "amazon.com"
)

book_transaction = as.data.frame(book_transaction)
length(unique(book_transaction[['machine_id']]))

# create a matrix for recommendation system algorithm input
book_transaction = book_transaction[c(1,3)]
book_transaction = as.data.table(book_transaction)

# order the dataset by machine_id
setorder(book_transaction, machine_id)
book_transaction[, count:= .N, by = c('machine_id', 'prod_category_id')]
book_transaction = unique(book_transaction)

# count number of book categories each customer purchased 
book_transaction[, num_categories := .N, by = 'machine_id']

# take a look at how many different categories are people purchasing
hist(book_transaction$num_categories)

# we will consider poeple who purchase more than 3 categories for the recommendation system 
final = book_transaction %>% filter(
  num_categories >=3
)
final = subset(final, select = -c(4))

# create the matrix 
customers = unique(final$machine_id)

matrix = spread(final, prod_category_id, count)

# create binary matrix
matrix = as.data.frame(matrix)
binary_matrix = matrix
binary_matrix[is.na(binary_matrix)]=0
binary_matrix[,c(2:12)] = binary_matrix[,c(2:12)] %>% mutate_if(is.numeric, ~1 * (. != 0))

write.csv(binary_matrix, 'binary_matrix.csv')
write.csv(matrix, 'matrix.csv')

# recommendation system ------------------------
rating_scores = fread("matrix.csv", head = TRUE)
rating_mat = as.matrix(matrix[,-1])
rownames(rating_mat) = matrix$machine_id

ratings = as(rating_mat,"realRatingMatrix")

# default is cosine but can also specify Jaccard and others in options
# UBCF stands for user based collaborative filtering
rec.model = Recommender(ratings, method = "UBCF")

# what are the top 10 apps recommended for user number 500?
rec.u500 = predict(rec.model, ratings["248378194",], n=3)

# to show
as(rec.u500, "list")

# if wanted just top 3
as(bestN(rec.u500, n = 3), "list")


# let's use the affinity information to predict rating scores for apps that user500 didn't actually rate
predicted.ratings.u500 = predict(rec.model, ratings["248378194",], type="ratings")

# to see the ratings for the apps user500 didn't rate
as(predicted.ratings.u500, "list")

# compared to the real scores from the matrix
as(ratings["248378194",], "list")

# let's train the model on our 1000 users and compare some different similarity types

# create and evaluation scheme using 90% of the ratings data for training and leaving 10% for validation
eval = evaluationScheme(ratings, method="split", train=0.9, given=3)

# creation of recommender model based on ubcf and cosine

# cosine as default
rec.cosine = Recommender(getData(eval, "train"), "UBCF")

# creation of recommender model based on ubcf and jaccard
rec.jaccard = Recommender(getData(eval, "train"), "UBCF", param = list(method = "Jaccard"))

# creation of recommender model based on ubcf and Perason
rec.pearson = Recommender(getData(eval, "train"), "UBCF", param = list(method = "Pearson"))


# making predictions on the test data sets
p.cosine = predict(rec.cosine, getData(eval, "known"), type="ratings")
p.jaccard = predict(rec.jaccard, getData(eval, "known"), type="ratings")
#p.pearson = predict(rec.pearson, getData(eval, "known"), type="ratings")

# obtaining the error metrics
error.cosine = calcPredictionAccuracy(p.cosine, getData(eval, "unknown"))
error.jaccard =calcPredictionAccuracy(p.jaccard, getData(eval, "unknown"))
#error.pearson =calcPredictionAccuracy(p.pearson, getData(eval, "unknown"))
error = rbind(error.cosine,error.jaccard)

rownames(error) = c("cosine","jaccard")
error

# jaccard seems to perform the best out of the two similarity types

# --------------------- user-based collaborative filter using 0-1 data------------------------

binary_matrix = fread("binary_matrix.csv", head = TRUE)
binary_matrix = as.matrix(binary_matrix[,-1])
rownames(binary_matrix) = matrix$machine_id

ratings = as(binary_matrix,"binaryRatingMatrix")

# default is cosine but can also specify Jaccard and others in options
# UBCF stands for user based collaborative filtering
rec.model = Recommender(ratings, method = "UBCF")

# what are the top 10 apps recommended for user number 500?
rec.u500 = predict(rec.model, ratings["248378194",], n=3)

# to show
as(rec.u500, "list")

# if wanted just top 3
as(bestN(rec.u500, n = 3), "list")


# let's use the affinity information to predict rating scores for apps that user500 didn't actually rate
predicted.ratings.u500 = predict(rec.model, ratings["248378194",], type="ratings")

# to see the ratings for the apps user500 didn't rate
as(predicted.ratings.u500, "list")

# compared to the real scores from the matrix
as(ratings["248378194",], "list")

# let's train the model on our 1000 users and compare some different similarity types

# create and evaluation scheme using 90% of the ratings data for training and leaving 10% for validation
eval = evaluationScheme(ratings, method="split", train=0.9, given=3)

# creation of recommender model based on ubcf and cosine

# cosine as default
rec.cosine = Recommender(getData(eval, "train"), "UBCF")

# creation of recommender model based on ubcf and jaccard
rec.jaccard = Recommender(getData(eval, "train"), "UBCF", param = list(method = "Jaccard"))

# creation of recommender model based on ubcf and Perason
rec.pearson = Recommender(getData(eval, "train"), "UBCF", param = list(method = "Pearson"))

# making predictions on the test data sets
p.cosine = predict(rec.cosine, getData(eval, "known"), type="ratings")
p.jaccard = predict(rec.jaccard, getData(eval, "known"), type="ratings")
#p.pearson = predict(rec.pearson, getData(eval, "known"), type="ratings")

# obtaining the error metrics
error.cosine = calcPredictionAccuracy(p.cosine, getData(eval, "unknown"))
error.jaccard =calcPredictionAccuracy(p.jaccard, getData(eval, "unknown"))
#error.pearson =calcPredictionAccuracy(p.pearson, getData(eval, "unknown"))
error = rbind(error.cosine,error.jaccard)

rownames(error) = c("cosine","jaccard")
error