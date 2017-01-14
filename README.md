# Is Trump an Angry Tweeter?
A Multi-level Sentiment and Structural Topic Model Analysis of Donald Trump's Tweets

## What do I mean by "multi-level?"
Sentiment analysis of textual data is certainly nothing new. Various algorithms and dictionary methods have been designed and implemented, and each has its advantages and drawbacks. Furthermore, supervised and unsupervised methods of textual analysis, increasingly in use by researchers and other professionals due to improvements in computing power, have been around (although without much practical application until the past several years) for decades. 

The proposed multi-level analysis that I outline below is not especially novel, therefore. However, it is an experiment of sorts. I wanted to combine sentiment analysis with topic modeling in a, sort of, two-stage analysis, where in the first stage I utilize, in this particular instance, the NRC Emotion Lexicon to identify the positive-negative valence of text, and where in the second stage I utilize what's called a structural topic model (STM) -- a method of text analysis that identifies a user specified number of topics via a machine learning algorithim where topical prevalence and/or content are determined by document level metadata -- and use the sentiment valence determined in the first stage as metadata in STM estimation. This multi-leveled approach allows one to determine the influence of sentiment expressed in textual data on the distribution of latent topics.

This method, though arguably methodologically consistent and sound, does also present a number of (surmountable) challenges, which will be covered in more detail later. However, even in the face of these challenges, I hope this brief outline will show the usefulness of this two-stage approach.

## The data
Trump's tweets were scraped using a public API, by [Dr. Ryan Burge](https://github.com/ryanburge). The data is a subset of a much larger dataset that also includes the tweets of 88 evangelical leaders (data that Dr. Burge and I have recently used for research for an article I coauthored with him that we hope to have published later this year).

The frequency of Trump's tweets during the period sampled is dipslayed below:

    ggplot(trumptweets, aes(created)) + geom_histogram(bins=215) + 
      ggtitle("Trump's Twitter Activity",
              subtitle="Sample Start Date:  February 6, 2016\nSample End Date:  September 17, 2016") +
      xlab("Date Tweeted") + ylab("Total Number of Tweets per Day") +
      theme_classic() + 
      theme(text=element_text(family="serif")) +
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
      theme(plot.title=element_text(size=14,hjust=0,face="bold",color="black")) +
      theme(panel.grid.major.y=element_line(colour="grey50",linetype=2)) 
![trump twitter activity](https://cloud.githubusercontent.com/assets/23504082/21957311/4f55565c-da59-11e6-8c4a-0eed80829b22.jpeg)

Although the greatest number of tweets in a given day appear more toward the first half of the sample period, Trump appears to have increased his total frequency of tweets closer to the end of the period sampled (perhaps because the November 8 election was approaching?).

## Sentiment analysis
Sentiment analysis with the get_nrc_sentiment() in the syuzhet package is used to identify the positive and negative sentiment, as well as the presence of 8 emotions identified using the NRC Emotion Lexicon, in Donald Trump's tweets.

eMake sure the text is a vector of character strings:

    trumptweets$text <- as.character(trumptweets$text)
    
Do sentiment analysis using the NRC Emotion Lexicon:

    trumpSentiment <- get_nrc_sentiment(trumptweets$text)

Combine the results with trumptweets data frame:

    tweets2 <- cbind(trumptweets, trumpSentiment)

Prep the data for graphing:

    sentimentTotals3 <- data.frame(colSums(tweets2[,c(19:28)]))
    names(sentimentTotals3) <- "count"
    sentimentTotals3 <- cbind("sentiment" = rownames(sentimentTotals3), sentimentTotals3)
    rownames(sentimentTotals3) <- NULL

Now, graph the results:

    library(ggplot2)
    e3<-ggplot(data = sentimentTotals3[1:8,], aes(x = reorder(sentiment,count), y = count)) +
      geom_bar(aes(fill = sentiment), stat = "identity") + coord_flip() +
      theme_classic() +
      theme(legend.position = "none") +
      xlab("") + ylab("") + ggtitle("",subtitle="Emotions in Donald Trump's Tweets") +
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
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +      theme(axis.text.x=element_text(angle=-45,hjust=0)) +
      theme(text=element_text(family="serif")) +
      scale_y_continuous(labels=scales::comma_format()) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      theme(panel.grid.major.x=element_line(colour="grey50",linetype=3)) 

    multiplot(s3,e3,layout = matrix(c(1:2),nrow=2,byrow=TRUE)) # plot
![sentiment analysis](https://cloud.githubusercontent.com/assets/23504082/21957413/209871a8-da5b-11e6-823e-ab0cfea8dc69.jpg)

It seems that there is, overall, more positive sentiment in Trump's tweets; however, there certainly is a good deal of negative sentament as well.

It is also interesting to note that trust is the most prevalent emotion in Trump's tweets. This may suggest that Trump fills his tweets with gestures of trust toward his audience ("believe me").

Anticipation and anger come in second, followed by fear and sadness. Surprise and joy come in third and second to last, with disgust bookending the set.

## Some tf-idf analysis to explore words most unique to positive, negative, and neutral sentiment
Before performing the second stage of the analysis, let's do some basic exploratory analysis using the tf-idf (term frequence-inverse document frequency) method outlined by [Julia Silge](http://juliasilge.com/blog/Term-Frequency-tf-idf/).

The tf-idf method allows the user to display the frequency that a term appears adjusted for how rare it is based on its inverse document frequency:  idf(term)=ln(N of documents/N of documents containing term)

Using these principles it is possible to show how unique a word is to a given set of documents in a larger corpus.

So let's use tf-idf to determine the most unique words of tweets with more positive sentiment than negative, and tweets with neutral (or a net balance of neutral sentiment):

    library(tidytext)
    library(dplyr)

Clean up the text data (there may be an easier way to do this, but at least it works):

    library(tm)
    tweets2$proText <- tolower(tweets2$text) #make it lower case
    tweets2$proText <- gsub('[[:punct:]]', '', tweets2$proText) #remove punctuation
    tweets2$proText <- gsub('[[:digit:]]+', '', tweets2$proText) #remove numbers
    tweets2$proText <- Corpus(VectorSource(tweets2$proText))
    tweets2$proText <- tm_map(tweets2$proText, removeWords, stopwords('english')) #remove stopwords
    tweets2$proText <- lapply(tweets2$proText[1:1325], as.character)
    tweets2$proText <- unlist(tweets2$proText)

Create a vector of sentiment valence, then use that numeric vector to create a new sentiment vector where a valence greater than 0 equals "positive," a valence less than 0 equals "negative," and a valence equal to 0 equals "neutral."

    tweets2$valence <- tweets2$positive-tweets2$negative
    tweets2$sentiment <- NA
    tweets2$sentiment[tweets2$valence > 0] <- "positive"
    tweets2$sentiment[tweets2$valence < 0] <- "negative"
    tweets2$sentiment[tweets2$valence == 0] <- "neutral"

Now for the tf-idf analysis:

    tweets_words2 <- tweets2 %>% unnest_tokens(word, proText) %>%
      count(sentiment, word, sort = TRUE) %>%
       ungroup()

    tweets_words2 <- tweets_words2  %>% bind_tf_idf(word,sentiment, n) %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word))))

    library(ggplot2)
    library(ggstance)
    library(ggthemes)

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

![top tf-idf words positive](https://cloud.githubusercontent.com/assets/23504082/21957657/1616e3c6-da61-11e6-84fa-de1a36193dc9.jpg)
![top tf-idf words negative](https://cloud.githubusercontent.com/assets/23504082/21957660/1b2149c4-da61-11e6-89e6-c55a30ef05cf.jpg)
![top tf-idf words neutral](https://cloud.githubusercontent.com/assets/23504082/21957662/2150f722-da61-11e6-9732-e4acc73ea16b.jpg)

The top tf-idf terms of positive tweets include "enjoy," "join," "honor," "interviewed," and "forward." Meanwhile, top terms for negative tweets include "mess," "hostile," "terrorist," "disgusting," and "tax." Top terms from neutral tweets include "country," "plane," the names of several states, and, oddly, "crookedhillary." I suspect that the latter term is unique to neutral tweets because Trump may often balance out his negative commentary of Hillary Clinton with positive things about himself in individual tweets; however, more research would need to be done to confirm this.

## STM analysis (stage two)
Stage two of the analysis utilizes a structural topic model (STM) to identify a user-specified number of latent topics in whatever corpus of data is being analyzed. The distribution of latent topics will be a function of, for this particular example, the date a tweet by Donald Trump was created and a categorical variable that specifies whether the tweet had positive, negative, or neutral valence.

### Initial Estimation and Model Selection
 First, I'll process the text so that it's in the proper format for STM estimation:

    library(stm)
    prostweets<-textProcessor(documents=tweets2$text,
                              metadata=data.frame(tweets2$created,tweets2$sentiment))

Now that the data is properly processed, rather than just going with the first model we estimate, let's run several models (20) for two EM (expectation-maximization) iterations each. We'll then pick the top 20% of those models (the top 4) and run them until they reach convergence, after which we'll pick one of those models for further analysis based on semantic coherence and exlusivity and final word bound of the approximate objective. Thankfully, the stm package has a function that will do a lot of the work for us:

    # Run 20 models, the top 20% of which with the highest likelihoods will be
    # run until convergence is reached.
    tweetsSelect <- selectModel(prostweets$documents, prostweets$vocab, K = 20,
                                prevalence=~tweets2.created + tweets2.sentiment,max.em.its=500,
                                data = prostweets$meta, runs = 20, seed = 8458159)

I've specified 20 topics (K) for STM estimation. The number of topics is fairly arbitrary, but for a corpus of this size and complexity, 20 topics is a reasonable number to go with. For a much smaller corpus (say 100 or so documents) 3 to 10 topics would be more appropriate, but since our sample of tweets is more than 1,300, 20 topics is more appropriate.

Model summaries for each of the top 4 models estimated are shown below:

![model summaries](https://cloud.githubusercontent.com/assets/23504082/21957873/64a50b62-da66-11e6-9ea1-8bd67da7c515.JPG)

Each model reached convergence at a differing rate, and the total time to convergence also differed between models. Model 2, which took the longest to converge and required the greatest number of EM interations to reach convergence is the model I've selected for further analysis. Model 2, though its topics have on average slightly less exlusivity than models 1, 3, and 4, its semantic coherence per topic is slighly higher, and its approximate objective is noticably lower.

Plot the semantic coherence of each model, and reverse engineer the plotting command used for this purpose in the stm package so that you can control where the legend is displayed:

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

Now use plotModels2() instead of plotModels():

    par(bty="n")
    plotModels2(tweetsSelect,printlegend=F) # Don't print a default legend
    legend("bottomleft",legend=c("1","2","3","4"), # Print your own legend that 
                                               # that doesn't overlap with any plotted points.
           bty="n", col = c("red","green","lightblue","purple"), 
           pch = 16)
    title(main="Top 4 Models with the Highest Likelihoods after 20 Runs",family="serif",adj=0)
    mtext("Semantic Coherence and Exclusivity of Topics",font=3,adj=0)

![semantic coherence and exlcusivity of topics](https://cloud.githubusercontent.com/assets/23504082/21957940/d525ac88-da67-11e6-9175-ef498a869768.jpg)

Let's also compare the change in the approximate objective by iterations and look at the final word bound per model:

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

![change in approximate objective per word bound](https://cloud.githubusercontent.com/assets/23504082/21957964/4ae102ba-da68-11e6-89ea-a3bf7828cd3b.jpg)

### Further analysis of estimated effects
As already stated, model 2 has been selected for further analysis. To keep things brief (I know, this has been a very long, brief analysis already), I'll go straight into some basic analysis with topic names already heuristically identified (I'll save discussion for how I deterimined topic themes for a later time).

Below is a plot of expected topic proportions in Trump's tweets:
![topic 15 point estimates](https://cloud.githubusercontent.com/assets/23504082/21958174/149e7e30-da6d-11e6-9c53-42fa75e40c18.jpg)

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
![top stm identified topics](https://cloud.githubusercontent.com/assets/23504082/21958045/e44a5734-da69-11e6-86ba-e617e14a3793.jpg)

Now, let's go further and estimate the effects of model covariates on topic proportions via further regression analysis:

    tweetsmodel<-tweetsSelect$runout[[2]]
    esttweets<-estimateEffect(c(1:20)~tweets2.sentiment,tweetsmodel,
                              metadata=prostweets$meta)

It would take up a lot of space to go into detail about all 20 topics, so instead, I'll just show two examples below that show the effectiveness of this two-stage method.

The first topic I'll discuss is topic 15, which I have heuristically identified as "Join Me." This topic consists of many words that imply invitations to join Trump at various campaign events. This is fairly obvious by looking at the top words for this topic based on highest probability, FREX score, lift, and score:

    # Topic 15 Top Words:  “Join Me”
 	     Highest Prob: join, tomorrow, today, trump, ticket, look, forward 
 	     FREX: join, ticket, tomorrow, noon, pensacola, forward, wichita 
 	     Lift: denver, excit, invit, joniernst, march, join, admonish 
 	     Score: absolut, accept, act, admit, admonish, afternoon, along 

The stm package will let us plot some of the top tweets associated with this topic so that we can put these lists of words into context:

    thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=15, n=6)$docs[[1]]
    par(adj=0.5)
    plotQuote(thoughts,width=75)
    title(main="Example Tweets",adj=0)
    mtext("Topic 15: Join Me",adj=0,font=3)
![topic 15 examples](https://cloud.githubusercontent.com/assets/23504082/21958161/b8ab5f94-da6c-11e6-800e-71557b06b0f3.jpg)

And we can also show the point estimates of expected topic proportions (with 95% confidence intervals) for this topic based on sentiment:

    par(bty="n",adj=0,family="serif")
    plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
         topics=esttweets$topics[15],labeltype="custom",
         custom.labels = c("Neutral","Positive","Negative"),
         xlim=c(-0.01,.16))
    title(main = "Mean Topic Proportions by Sentiment") 
    mtext("Topic 15: Join Me",adj=0,font=3)
![topic 15 point estimates](https://cloud.githubusercontent.com/assets/23504082/21958174/149e7e30-da6d-11e6-9c53-42fa75e40c18.jpg)

It's pretty clear that whenver Trump invites people to join him, whether at a rally, or online, it's couched in significant positive sentiment. However, compare this topic to another, much more negative topic...

Topic 12, which I've called, "Hillary Clinton," is quite obviously not just about Hillary Clinton, but "Crooked" Hillary Clinton. This is not only suggested by the top words associated with this topic:

    Topic 12 Top Words:  “Hillary Clinton”
 	     Highest Prob: hillari, crook, just, will, said, call, job 
 	     FREX: crook, street, call, hillari, polici, bill, husband 
 	     Lift: awar, caught, consult, experi, knock, sold, street 
 	     Score: ‘widespread’, abandon, abc, abil, abilityzilch, abl, abolish 

It is also clear when one looks at top tweets associated with this topic:

    thoughts<-findThoughts(tweetsmodel, tweets2$text, topics=12, n=6)$docs[[1]]
    par(adj=0.5)
    plotQuote(thoughts,width=75)
    title(main="Example Tweets",adj=0)
    mtext("Topic 12: Hillary Clinton",adj=0,font=3)

![topic 12 examples](https://cloud.githubusercontent.com/assets/23504082/21958199/17cc20ac-da6e-11e6-9ade-ff710189870a.jpg)

And it's easy to see that when Trump talks about Hillary, he uses a significant amount of negative sentiment; however, some neutral sentiment is found in his tweets as well. As previously discussed, this may be because Trump sometimes talks about himself positively in reference to Hillary, which may balance out the negative sentiment in some of his tweets about her. But, again, this is not overtly clear based on this analysis alone.

    par(bty="n",adj=0,family="serif")
    plot(esttweets,"tweets2.sentiment",tweetsmodel,prostweets$meta,method="pointestimate",
         topics=esttweets$topics[12],labeltype = "custom",
         custom.labels = c("Neutral","Positive","Negative"),
         xlim=c(0,0.12))
    title(main = "Mean Topic Proportions by Sentiment")
    mtext("Topic 12: Hillary Clinton",adj=0,font=3)
![topic 12 point estimates](https://cloud.githubusercontent.com/assets/23504082/21958210/99f2ba96-da6e-11e6-9341-aaacb26d368c.jpg)

## Challenges and value of the two-stage method
It has been pointed out by some that dictionary methods, as well as unsupervised machine learning algorithims, may miss important contextual factors associated with text that only humans could reliably identify. Some have proposed using human computation methods akin to Amazon's Mechanical Turk (AMT) in preference to purely computerized methods. However, some place a great deal of emphasis on the computerized element of text analysis and fail to recognize that the human research is meant to be partnered with machine in these sorts of analyses; not replaced by the machine. A critical component of this analysis is the human element, which is required to determine the meaning and themes of topics identified, as well as the overall performance of the model. This is still a challenge, however, because we are often left to rely on the judgments of only one person. But, this challenge could be overcome if more researchers are involved in a project such as this, which would ensure that judgments made about results are more reliable.
