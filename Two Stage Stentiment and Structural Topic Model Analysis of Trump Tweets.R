
#Sentiment analysis of political tweets
install.packages("syuzhet")
library(syuzhet)
library(ggplot2)
library(ggstance)
library(ggthemes)
library(dplyr)
library(car)

trumptweets<-read.csv("total.csv")

# Filter out everone but trump:
trumptweets <- filter(trumptweets, screenName == "realDonaldTrump")
dim(trumptweets) # data frame with 1,325 rows and 18 columns

# Make sure the dates and times the tweets were created are in the 
# right format.
trumptweets$created<-as.POSIXlt(trumptweets$created)
trumptweets$created<-as.POSIXct(trumptweets$created)

# Make identifiers for whether tweets have been retweeted and favorited.
trumptweets$retweeted<-Recode(trumptweets$retweetCount,"0='FALSE';else='TRUE'")
trumptweets$favorited<-Recode(trumptweets$favoriteCount,"0='FALSE';else='TRUE'")
trumptweets$text <- as.character(trumptweets$text)

# Do sentiment analysis using the NRC Emotion Lexicon:
trumpSentiment <- get_nrc_sentiment(trumptweets$text)

# Combine results with trumptweets data frame:
tweets2 <- cbind(trumptweets, trumpSentiment)

# Prep the data for graphing:
sentimentTotals3 <- data.frame(colSums(tweets2[,c(19:28)]))
names(sentimentTotals3) <- "count"
sentimentTotals3 <- cbind("sentiment" = rownames(sentimentTotals3), sentimentTotals3)
rownames(sentimentTotals3) <- NULL

# Make a multiplot:
e3<-ggplot(data = sentimentTotals3[1:8,], aes(x = reorder(sentiment,count), y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") + coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("") + ylab("") + ggtitle("",
                                                        subtitle="Emotions in Donald Trump's Tweets") +
  geom_text(aes(label=sentiment), hjust=1.1,vjust=.001, colour="white",family="serif") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=-45,hjust=0)) +
  theme(text=element_text(family="serif")) +
  scale_y_continuous(labels=scales::comma_format()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3)) 

s3<-ggplot(data = sentimentTotals3[9:10,], aes(x = reorder(sentiment,count), y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") + coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("") + ylab("") + ggtitle("Sentiment Analysis of Trump's Tweets Based\non the NRC Emotion Lexicon",
                                subtitle="Positive vs. Negative Sentiment in Donald Trump's Tweets") +
  geom_text(aes(label=sentiment,), hjust=1.1,vjust=.1, colour="white",family="serif") +
  theme(plot.title=element_text(face="bold",size=14)) +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=-45,hjust=0)) +
  theme(text=element_text(family="serif")) +
  scale_y_continuous(labels=scales::comma_format()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3)) 

multiplot(s3,e3,layout = matrix(c(1:2),nrow=2,byrow=TRUE)) # plot

# Now for STM analysis
library(stm)
tweets2$valence <- tweets2$positive-tweets2$negative
tweets2$sentiment <- NA
tweets2$sentiment[tweets2$valence > 0] <- "positive"
tweets2$sentiment[tweets2$valence < 0] <- "negative"
tweets2$sentiment[tweets2$valence == 0] <- "neutral"

prostweets<-textProcessor(documents=tweets2$text,
                          metadata=data.frame(tweets2$created,tweets2$sentiment))
  # Convergence in 274 EM iterations
  # 340.53 seconds
tweetsSelect <- selectModel(prostweets$documents, prostweets$vocab, K = 20,
                              prevalence=~tweets2.created + tweets2.sentiment,max.em.its=500,
                              data = prostweets$meta, runs = 20, seed = 8458159)
# plotModels() doesn't give you any flexibility to determine where the legend for the 
# Semantic Coherence/Exclusivity plot. Let's alter that function and create a new one
# called plotModels2() that gives us the ability to specify the legend parameters
# following plotting:
plotModels2<-function (models, xlab = "Semantic Coherence", ylab = "Exclusivity", 
                       labels = 1:length(models$runout),printlegend = T, ...) 
{
  if (length(models$runout[[1]]$beta$logbeta) < 2) {
    plot(0, 0, xlab = xlab, ylab = ylab, col = "white", xlim = c(min(unlist(models$semcoh)), 
                                                                 max(unlist(models$semcoh))), ylim = c(min(unlist(models$exclusivity)), 
                                                                                                       max(unlist(models$exclusivity))), ...)
    col = rainbow(length(models$runout))
    for (i in 1:length(models$runout)) {
      points(models$semcoh[[i]], models$exclusivity[[i]], 
             col = col[i], pch = 16, cex = 0.75)
    }
    
    text(unlist(lapply(models$semcoh, mean)), unlist(lapply(models$exclusivity, 
                                                            mean)), labels, col = col)
  }
  if (length(models$runout[[1]]$beta$logbeta) > 1) {
    avgsc <- unlist(lapply(models$semcoh, mean))
    avgsp <- unlist(lapply(models$sparsity, mean))
    for (i in 1:length(models$runout)) {
      print(paste("Model", i, "has on average", avgsc[i], 
                  "semantic coherence and", avgsp[i], "sparsity"))
    }
  }
}

# Now use plotModels2() instead of plotModels()
par(bty="n")
plotModels2(tweetsSelect,printlegend=F) # Don't print a default legend
legend("bottomleft",legend=c("1","2","3","4"), # Print your own legend that 
                                               # that doesn't overlap with any plotted points.
       bty="n", col = c("red","green","lightblue","purple"), 
       pch = 16)
title(main="Top 4 Models with the Highest Likelihoods after 20 Runs",family="serif",adj=0)
mtext("Semantic Coherence and Exclusivity of Topics",font=3,adj=0)

tweetsSelect$runout[[1]]$convergence$bound
# 300
# 484.46 sec.
# Final Bound: -82677.99
tweetsSelect$runout[[2]]$convergence$bound
# 403
# 623.58 sec.
# Final Bound: -82064.40
tweetsSelect$runout[[3]]$convergence$bound
# 229
# 358.79 sec.
# Final Bound: -82602.25
tweetsSelect$runout[[4]]$convergence$bound
# 336 its
# 523.7 sec.
# Final Bound: -82966.55
par(new=FALSE)
par(family="serif",adj=.5)
plot(tweetsSelect$runout[[1]]$convergence$bound, type = "l",
     ylab = "Approximate Objective",xlab="Iterations",lwd=2,col="red",xlim=c(0,425),
     ylim=c(-96000,-80000))
par(new=TRUE)
plot(tweetsSelect$runout[[2]]$convergence$bound, type = "l",xlim=c(0,425),
     ann=FALSE, axes=FALSE,lwd=2,col="green",
     ylim=c(-96000,-80000))
par(new=TRUE)
plot(tweetsSelect$runout[[3]]$convergence$bound, type = "l",xlim=c(0,425),
     ann=FALSE, axes=FALSE,lwd=2,col="lightblue",
     ylim=c(-96000,-80000))
par(new=TRUE)
plot(tweetsSelect$runout[[4]]$convergence$bound, type = "l",xlim=c(0,425),
     ann=FALSE, axes=FALSE,lwd=2,col="purple",
     ylim=c(-96000,-80000))
title(main="Top 4 Models with the Highest Likelihoods after 20 Runs",adj=0)
mtext("Change in Approximate Objective per Word Bound",adj=0,font=3)
legend("bottom",legend=c("1","2","3","4"), bty="n", col = c("red","green","lightblue","purple"),
       lty=1,lwd=2)
abline(h=c(-82677.99,-82064.40,-82602.25,-82966.55),lty=2,lwd=1,col=c("red","green","lightblue","purple"))
abline(v=c(300,403,229,336),lty=2,lwd=1,col=c("red","green","lightblue","purple"))

# We'll go with the second model:
tweetsmodel<-tweetsSelect$runout[[2]]
esttweets<-estimateEffect(c(1:20)~tweets2.sentiment,tweetsmodel,
                          metadata=prostweets$meta)

# Plot topics in order by expected proportions
topicNames<-c("Topic 1: Rallies",
              "Topic 2: Self-Funding of Campaign",
              "Topic 3: The New York Times and Other Discontents",
              "Topic 4: Bernie Sanders",
              "Topic 5: Ted Cruz",
              "Topic 6: Veterans",
              "Topic 7: Upcoming Interviews",
              "Topic 8: Ratings",
              "Topic 9: John Kaskich",
              "Topic 10: Rigged System",
              "Topic 11: Democratic and Republican Conventions",
              "Topic 12: Hillary Clinton",
              "Topic 13: Thanks for Support",
              "Topic 14: Make America Great Again",
              "Topic 15: Join Me",
              "Topic 16: Killings by Illegal Immigrants",
              "Topic 17: Self-Validation",
              "Topic 18: Support",
              "Topic 19: Elizabeth Warren",
              "Topic 20: Hillary Clinton and Corruption")
par(bty="n",col="grey40",lwd=15,family="serif",adj=0,lend=3)
plot.STM(tweetsmodel,type="summary",
         main="",xlab="",family="serif",custom.labels="",
         topic.names=topicNames)
title(main = "Top STM Identified Topics")
mtext("Expected Topic Proportions",adj=0,font=3,col="black")
par(lwd=1,lend=1,bty="o",col="black") # reset some of the parameters before moving forward.


# Topics
  # Topic 1: Rallies
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[1],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.08))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 1: Rallies",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=1, n=6)$docs[[1]]
par(adj=0.5,bty="n")
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 1: Rallies",adj=0,font=3)


  # Topic 2: Self-Funding of Campaign
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[2],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.08))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 2: Self-Funding of Campaign",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=2, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 2: Self-Funding of Campaign",adj=0,font=3)


  # Topic 3: The New York Times and Other Discontents
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[3],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.15))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 3: The New York Times and Other Discontents",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=3, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 3: The New York Times and Other Discontents",adj=0,font=3)


  # Topic 4: Bernie Sanders
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",stmtweets,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[4],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.15))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 4: Bernie Sanders",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=4, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 4: Bernie Sanders",adj=0,font=3)


  # Topic 5: Ted Cruz
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",stmtweets,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[5],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.1))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 5: Ted Cruz",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=5, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 5: Ted Cruz",adj=0,font=3)


  # Topic 6: Veterans
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",stmtweets,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[6],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.08))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 6: Veterans",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=6, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 6: Veterans",adj=0,font=3)


  # Topic 7: Upcoming Interview
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[7],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,.15))
title(main = "Mean Topic Proportions by Sentiment",col="black")
mtext("Topic 7: Upcoming Interview",adj=0,font=3,col="black")
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=7, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 7: Upcoming Interview",adj=0,font=3)


  # Topic 8: Ratings
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[8],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.08))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 8: Ratings",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=8, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 8: Ratings",adj=0,font=3)

# Topic 9: John Kasich
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[9],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.08))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 9: John Kasich",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=9, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 9: John Kasich",adj=0,font=3)

# Topic 10: Rigged System
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[10],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.08))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 10: Rigged System",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=10, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 10: Rigged System",adj=0,font=3)

# Topic 11: Democratic and Republican Conventions
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[11],labeltype = "custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.08))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 11: Democratic and Republican Conventions",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=11, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 11: Democratic and Republican Conventions",adj=0,font=3)

# Topic 12: Hillary Clinton
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[12],labeltype = "custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,0.12))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 12: Hillary Clinton",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=12, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 12: Hillary Clinton",adj=0,font=3)

# Topic 13: Thanks for Support
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[13],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(-0.001,.25))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 13: Thanks for Support",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=13, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 13: Thanks for Support",adj=0,font=3)

# Topic 14: Make America Great Again
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[14],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,.12))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 14: Make America Great Again",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=14, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 14: Make America Great Again",adj=0,font=3)

# Topic 15: Join Me
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[15],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(-0.01,.16))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 15: Join Me",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=15, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 15: Join Me",adj=0,font=3)

# Topic 16: Killings by Illegal Immigrants and Terrorists/ISIS
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[16],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,.12))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 16: Killings by Illegal Immigrants and Terrorists/ISIS",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=16, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 16: Killings by Illegal Immigrants and Terrorists/ISIS",adj=0,font=3)

# Topic 17: Self-Validation
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[17],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,.06))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 17: Self-Validation",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=17, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 17: Self-Validation",adj=0,font=3)

# Topic 18: Support
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[18],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,.08))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 18: Support",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=18, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 18: Support",adj=0,font=3)

# Topic 19: Elizabeth Warren
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[19],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,.08))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 19: Elizabeth Warren",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=19, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 19: Elizabeth Warren",adj=0,font=3)

# Topic 20: Hillary Clinton and Corruption
par(bty="n",adj=0,family="serif")
plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
     topics=esttweets$topics[20],labeltype="custom",
     custom.labels = c("Neutral","Positive","Negative"),
     xlim=c(0,.12))
title(main = "Mean Topic Proportions by Sentiment")
mtext("Topic 20: Hillary Clinton and Corruption",adj=0,font=3)
thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=20, n=6)$docs[[1]]
par(adj=0.5)
plotQuote(thoughts,width=75)
title(main="Example Tweets",adj=0)
mtext("Topic 20: Hillary Clinton and Corruption",adj=0,font=3)

## tf-idf Analysis Based on Emotions:
# by anger:
library(tidytext)
library(dplyr)
# Cleaning up the text data (there may be an easier way to do this, but at least it works):
library(tm)
tweets2$proText <- tolower(tweets2$text) #make it lower case
tweets2$proText <- gsub('[[:punct:]]', '', tweets2$proText) #remove punctuation
tweets2$proText <- gsub('[[:digit:]]+', '', tweets2$proText) #remove numbers
tweets2$proText <- Corpus(VectorSource(tweets2$proText))
tweets2$proText <- tm_map(tweets2$proText, removeWords, stopwords('english')) #remove stopwords
tweets2$proText <- lapply(tweets2$proText[1:1325], as.character)
tweets2$proText <- unlist(tweets2$proText)

tweets_words2 <- tweets2 %>% unnest_tokens(word, proText) %>%
  count(sentiment, word, sort = TRUE) %>%
  ungroup()

tweets_words2 <- tweets_words2  %>% bind_tf_idf(word,sentiment, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

library(ggplot2)
library(ggstance)
library(ggthemes)
ggplot(tweets_words2 %>% filter(`as.character(anger)`== "4") %>% top_n(5), aes(tf_idf, reorder(word,tf_idf), fill = "red")) +
  geom_barh(stat = "identity", show.legend = FALSE) + theme_classic() +
  ggtitle("Top tf-idf Words",subtitle = "Anger Score = 4") +
  ylab("") + xlab("tf-idf") +
  geom_text(aes(label=word), hjust=1.1,vjust=.001, colour="white",family="serif") +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))

ggplot(tweets_words2 %>% filter(`as.character(anger)`== "3") %>% top_n(5), aes(tf_idf, reorder(word,tf_idf), fill = "red")) +
  geom_barh(stat = "identity", show.legend = FALSE) + theme_classic() +
  ggtitle("Top tf-idf Words",subtitle = "Anger Score = 3") +
  ylab("") + xlab("tf-idf") +
  geom_text(aes(label=word), hjust=1.1,vjust=.001, colour="white",family="serif") +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))

ggplot(tweets_words2 %>% filter(`as.character(anger)`== "2") %>% top_n(5), aes(tf_idf, reorder(word,tf_idf), fill = "red")) +
  geom_barh(stat = "identity", show.legend = FALSE) + theme_classic() +
  ggtitle("Top tf-idf Words",subtitle = "Anger Score = 2") +
  ylab("") + xlab("tf-idf") +
  geom_text(aes(label=word), hjust=1.1,vjust=.001, colour="white",family="serif") +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))

ggplot(tweets_words2 %>% filter(`as.character(anger)`== "1") %>% top_n(5), aes(tf_idf, reorder(word,tf_idf), fill = "red")) +
  geom_barh(stat = "identity", show.legend = FALSE) + theme_classic() +
  ggtitle("Top tf-idf Words",subtitle = "Anger Score = 4") +
  ylab("") + xlab("tf-idf") +
  geom_text(aes(label=word), hjust=1.1,vjust=.001, colour="white",family="serif") +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))

ggplot(tweets_words2[1:20,], aes(tf_idf, reorder(word,tf_idf), fill = sentiment)) +
  geom_barh(stat = "identity", show.legend = FALSE) + theme_classic() +
  ggtitle("Top tf-idf Words",subtitle = "By Sentiment") +
  ylab("") + xlab("tf-idf") +
  geom_text(aes(label=word), hjust=1.1,vjust=.001, colour="white",family="serif") +
  scale_x_continuous(expand=c(0,0)) +
  theme(legend.position = "none") +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))

tweets_words2Pos<-tweets_words2%>%filter(sentiment=="positive")
ggplot(tweets_words2Pos[1:10,], aes(tf_idf, reorder(word,tf_idf),alpha=tf_idf)) +
  geom_barh(stat = "identity",fill="darkblue") + theme_classic() +
  ggtitle("Top tf-idf Words",subtitle = "Positive Sentiment") +
  ylab("") + xlab("tf-idf") +
  geom_text(aes(label=word), hjust=1.1,vjust=.001, colour="white",family="serif") +
  scale_x_continuous(expand=c(0,0)) +
  scale_alpha(range=c(.6,1)) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(hjust=0,vjust=1,angle=-60)) +
  theme(text=element_text(family="serif")) +
  theme(plot.title=element_text(face="bold",size=14)) +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))

tweets_words2Neg<-tweets_words2%>%filter(sentiment=="negative")
ggplot(tweets_words2Neg[1:10,], aes(tf_idf, reorder(word,tf_idf), fill = "red",alpha=tf_idf)) +
  geom_barh(stat = "identity",fill="darkred") + theme_classic() +
  ggtitle("Top tf-idf Words",subtitle = "Negative Sentiment") +
  ylab("") + xlab("tf-idf") +
  geom_text(aes(label=word), hjust=1.1,vjust=.001, colour="white",family="serif") +
  scale_x_continuous(expand=c(0,0)) +
  scale_alpha(range=c(.6,1)) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(hjust=0,vjust=1,angle=-60)) +
  theme(text=element_text(family="serif")) +
  theme(plot.title=element_text(face="bold",size=14)) +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))

tweets_words2Neu<-tweets_words2%>%filter(sentiment=="neutral")
ggplot(tweets_words2Neu[1:10,], aes(tf_idf, reorder(word,tf_idf), fill = "red",alpha=tf_idf)) +
  geom_barh(stat = "identity",fill="darkgreen") + theme_classic() +
  ggtitle("Top tf-idf Words",subtitle = "Neutral Sentiment") +
  ylab("") + xlab("tf-idf") +
  geom_text(aes(label=word), hjust=1.1,vjust=.001, colour="white",family="serif") +
  scale_alpha(range=c(.6,1)) +
  scale_x_continuous(expand=c(0,0)) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(hjust=0,vjust=1,angle=-60)) +
  theme(text=element_text(family="serif")) +
  theme(plot.title=element_text(face="bold",size=14)) +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))
