#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

importPackage <- function(){
    library(twitteR)
    library(dplyr)
    library(tidyr)
    library(tidytext)
    library(tidymodels)
    library(stringr)
    library(NLP)
    library(tm)
    library(wordcloud)
    library(RColorBrewer)
    library(ggplot2)
    library(sentimentr)
}

#import package
importPackage()

authenticationTwitterApi <- function(){
    options(httr_oauth_cache=T)
    consumer_key <- "JEnN8yLmglsPQB3KgnNDjiD91"
    consumer_key_secret <- "2E65RCLoZtzv4xvbJJKUW4U5jOeOZSzk4wj9H6Jenq5cydPLKe"
    access_token <- "2261286874-yDWuvQXNTOtzf5SIs9Ho65FVn1JIuAUMPKFGjyT"
    access_token_secret <- "lxRFiq03ShDOGEgjcdUlpntQzlG7DCTCr2Vrq6sLm4xK4"
    
    setup_twitter_oauth(consumer_key, consumer_key_secret, access_token, access_token_secret)
    
}

# run autentication from Twitter
authenticationTwitterApi()



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sentimen terhadap PSBB"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot Klasifikasi Emosi", plotOutput("nrcPlot")), 
                        tabPanel("Plot Klasifikasi Polaritas", plotOutput("bingPlot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Crawling data from twitter
    # psbb <- searchTwitter("psbb",n = 1000)
    # psbb <- twListToDF(psbb)
    # tweetPsbb <- psbb$text
    
    # Data di ekspor terlebih dahulu untuk diterjemahkan ke bahasa Inggris
    # write.csv(tweetPsbb,"tweetIndo.csv",row.names = FALSE)
    
    # Setelah di ekspor lalu file save as .xlsx
    # Terjemahkan data dari Bahasa Indonesia ke bahasa Inggris dengan link https://www.onlinedoctranslator.com/id/translationform
    # Membaca data yang sudah diterjemahkan dalam bentuk csv
    tweetPsbb <- read.csv("tweetIndo.id.en.csv")
    tweetPsbb <- tweetPsbb$tweets.of.Psbb
    
    # Prepocessing data
    # remove url
    tweetPsbb <- gsub("http[^[:space:]]*", "", tweetPsbb)
    
    # remove NewLine
    tweetPsbb <- gsub("\n","",tweetPsbb)
    
    # remove RT
    tweetPsbb = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweetPsbb)
    
    # remove quotes
    tweetPsbb = gsub("'s|'s|[...]","",tweetPsbb)
    
    # remove at people
    tweetPsbb = gsub("@\\w+", "", tweetPsbb)
    
    # remove punctuation
    tweetPsbb = gsub("[[:punct:]]", " ", tweetPsbb)
    
    # remove unnecessary spaces
    tweetPsbb = gsub("[ \t]{2,}", " ", tweetPsbb)
    tweetPsbb = gsub("^\\s+|\\s+$", " ", tweetPsbb)
    tweetPsbb = gsub("note", " ", tweetPsbb)
    
    #remove &amp
    tweetPsbb <- gsub("&amp;","",tweetPsbb)
    
    # remove numbers
    tweetPsbb = gsub("[[:digit:]]", " ", tweetPsbb)
    
    try.error = function(x)
    {
        # create missing value
        y = NA
        # tryCatch error
        try_error = tryCatch(tolower(x), error=function(e) e)
        # if not an error
        if (!inherits(try_error, "error"))
            y = tolower(x)
        # result
        return(y)
    }
    # lower case using try.error with sapply 
    tweetPsbb = sapply(tweetPsbb, try.error)
    # remove NAs in some_txt
    tweetPsbb = tweetPsbb[!is.na(tweetPsbb)]
    names(tweetPsbb) = NULL
    
    tweetPsbb <- as.data.frame(tweetPsbb)
    
    tidy_tweet <- tweetPsbb %>%
        unnest_tokens(word, tweetPsbb) %>%
        anti_join(stop_words)
    
    # Data Clustering with nrc
    nrc_psbb <- tidy_tweet %>%
        inner_join(get_sentiments("nrc")) %>%
        count(sentiment, sort = TRUE)
    
    # Data Clustering with bing
    bing_word_counts <- tidy_tweet %>%
        inner_join(get_sentiments("bing")) %>%
        count(sentiment, word, sort = TRUE)
    
   
    output$nrcPlot <- renderPlot({
        ggplot(data = nrc_psbb) +
            geom_col(aes(n,sentiment, color = sentiment, fill = sentiment)) +
            labs(x = "Jumlah Kata dalam Tweet", y = "Kategori mosi") +
            ggtitle("Sentiment Analisis emosi terhadap PSBB")
    })
    
    # output$bingPlot <- renderPlot({
    #     ggplot(data = bing_word_counts) +
    #         geom_col(aes(n,sentiment,fill = ),show.legend = FALSE) +
    #         labs(y = "Contribution to sentiment", x = NULL) +
    #         coord_flip() +
    #         ggtitle("Sentimen Analisis polaritas terhadap PSBB" )
    # })
    output$bingPlot <- renderPlot({
        ggplot(bing_word_counts, aes(x=sentiment)) +
            geom_bar(aes(y=..count.., fill=sentiment)) +
            scale_fill_brewer(palette = "RdGy") +
            ggtitle("Sentiment Analisis Polaritas terhadap PSBB") +
            labs(x="polaritas", y="banyak Tweets")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
