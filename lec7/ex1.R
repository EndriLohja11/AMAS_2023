install.packages(c("tm"))
install.packages(c("SnowballC", "wordcloud"))
# Step 1: Load the required packages
library(tm)
library(SnowballC)
library(wordcloud)


# Step 2: Load the text file using Corpus()
text_file <- system.file("extdata", "C:\\Users\\endri\\amas\\lec7\\book.txt" ,package = "tm")
corpus <- Corpus(VectorSource(readLines("C:\\Users\\endri\\amas\\lec7\\book.txt")))

# Step 3: Preprocess the text data
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Step 4: Create term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Step 5: Get word frequencies
freq_table <- as.data.frame(as.table(tdm))
freq_table <- freq_table[order(freq_table$Freq, decreasing = TRUE), ]
freq_table$Relative_Frequency <- freq_table$Freq / sum(freq_table$Freq)

# Step 6: Sort the frequency table
freq_table <- freq_table[order(freq_table$Freq, decreasing = TRUE), ]

# Step 7: Select top 20 words
top_words <- freq_table[1:20, ]

# Step 8: Generate bar plot for top 20 words with improved aesthetics
barplot(top_words$Freq, las = 2, names.arg = top_words$Var1, main = "Most Frequent Words",
        xlab = "Words", ylab = "Frequency", col = "steelblue", border = "black")

# Step 9: Write frequency table to CSV file
write.csv(freq_table, "word_frequencies.csv", row.names = FALSE)

# Step 10: Generate word cloud
wordcloud(words = top_words$Var1, freq = top_words$Freq, min.freq = 100, random.order = FALSE)

print(top_words)
words <- c("pink", "look", "bobbie")
freq <- c(1215, 1768, 98)

wordcloud(words = top_words$Terms, freq = top_words$Freq, min.freq = 100, random.order = FALSE)
