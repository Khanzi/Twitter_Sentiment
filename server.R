
# Libraries ---------------------------------------------------------------


library(shiny)
library(tidyverse)
library(rtweet)
library(lubridate)
library(NLP)
library(tidytext)
library(magrittr)
library(wordcloud)
library(reshape2)
library(plotly)
library(tm)
library(tidyr)
library(igraph)
library(ggraph)


# Global Vars & Pars ------------------------------------------------------

NUMBER_OF_TWEETS <-  50
TWITTER_BLUE <- "#38A1F3"

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


# Modals -----------------------------------------------------------------

observeEvent(input$smart, {
    showModal(modalDialog(
        title = "Watch out Hadley",
        HTML('<img src="https://i.gifer.com/origin/9c/9c1c439c2bcbdb332dd92ccce88137d2_w200.gif">'),
        easyClose = TRUE,
        footer = NULL
    ))
    
})
showModal(modalDialog(
    title = "Please be patient",
    "We are loading live twitter data, this might take a couple minutes. Once all the data is loaded the app will start producing results"
))    

# Authentication ----------------------------------------------------------

#Sourcing a seperate file with API Keys
source("auth.r")

# Explore Candidates ------------------------------------------------------

# The Embeds
cb_embed <- '<a class="twitter-timeline" href="https://twitter.com/CoryBooker?ref_src=twsrc%5Etfw">Tweets by CoryBooker</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
pb_embed <- '<a class="twitter-timeline" href="https://twitter.com/PeteButtigieg?ref_src=twsrc%5Etfw">Tweets by PeteButtigieg</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
jc_embed <- '<a class="twitter-timeline" href="https://twitter.com/JulianCastro?ref_src=twsrc%5Etfw">Tweets by JulianCastro</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
jd_embed <- '<a class="twitter-timeline" href="https://twitter.com/JohnDelaney?ref_src=twsrc%5Etfw">Tweets by JohnDelaney</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
tg_embed <- '<a class="twitter-timeline" href="https://twitter.com/TulsiGabbard?ref_src=twsrc%5Etfw">Tweets by TulsiGabbard</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
kg_embed <- '<a class="twitter-timeline" href="https://twitter.com/SenGillibrand?ref_src=twsrc%5Etfw">Tweets by SenGillibrand</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
kh_embed <- '<a class="twitter-timeline" href="https://twitter.com/KamalaHarris?ref_src=twsrc%5Etfw">Tweets by KamalaHarris</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
ak_embed <- '<a class="twitter-timeline" href="https://twitter.com/amyklobuchar?ref_src=twsrc%5Etfw">Tweets by amyklobuchar</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
dt_embed <- '<a class="twitter-timeline" href="https://twitter.com/realDonaldTrump?ref_src=twsrc%5Etfw">Tweets by realDonaldTrump</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
ew_embed <- '<a class="twitter-timeline" href="https://twitter.com/SenWarren?ref_src=twsrc%5Etfw">Tweets by SenWarren</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
bs_embed <- '<a class="twitter-timeline" href="https://twitter.com/SenSanders?ref_src=twsrc%5Etfw">Tweets by SenSanders</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
br_embed <- '<a class="twitter-timeline" href="https://twitter.com/BetoORourke?ref_src=twsrc%5Etfw">Tweets by BetoORourke</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'

# HTML EMBED PARSER 
select_candidate_parser <- function(selection){
    if (selection == "Cory Booker") return(cb_embed)
    else if (selection == "Pete Buttigieg") return(pb_embed)
    else if (selection == "Julian Castro") return(jc_embed)
    else if (selection == "John Delaney") return(jd_embed)
    else if (selection == "Tulsi Gabbard") return(tg_embed)
    else if (selection == "Kirsten Gillibrand") return(kg_embed)
    else if (selection == "Kamala Harris") return(kh_embed)
    else if (selection == "Amy Klobuchar") return(ak_embed)
    else if (selection == "Donald Trump") return(dt_embed)
    else if (selection == "Elizabeth Warren") return(ew_embed)
    else if (selection == "Beto O Rourke") return(br_embed)
    else return(bs_embed)
}
#Rendering the HTML EMBED
output$candidate_embed <- renderText({select_candidate_parser(input$select_candidate)})

#Rendering Topic Model Graph
output$candidate_topic_graph <- renderPlot(topic_graph(data_selector_timelines(input$select_candidate)))

#Most Unique Words
output$candidate_unique_words <- renderTable({top_words(data_selector_timelines(input$select_candidate))})

#Sentiment Of Words
output$candidate_sentiment_word <- renderPlotly({most_used_words_sentiment_graph(get_s(data_selector_timelines(input$select_candidate)))})

#Candidate Name
output$candidate_name <- renderText({input$select_candidate})

# General Functions -------------------------------------------------------

get_data <- function(){
    excluded_col = c("mentions_screen_name","mentions_user_id","bbox_coords","coords_coords","geo_coords","ext_media_expanded_url",
                     "ext_media_t.co",
                     "ext_media_url",
                     "media_type",
                     "media_expanded_url",
                     "media_t.co ",
                     "media_url",
                     "urls_expanded_url",
                     "urls_t.co",
                     "urls_url",
                     "symbols",
                     "hashtags",
                     "media_t.co")
    
    
    candidates <- c("@CoryBooker",
                    "@PeteButtigieg",
                    "@JulianCastro",
                    "@JohnDelaney",
                    "@TulsiGabbard",
                    "@SenGillibrand",
                    "@KamalaHarris",
                    "@amyklobuchar",
                    "@realDonaldTrump",
                    "@SenWarren",
                    "@BernieSanders",
                    "@BetoORourke")
    
    namer <- function(name){
        if (name == "@CoryBooker") return("cb")
        else if (name == "@PeteButtigieg") return("pb")
        else if (name == "@JulianCastro") return("jc")
        else if (name == "@JohnDelaney") return("jd")
        else if (name == "@TulsiGabbard") return("tg")
        else if (name == "@SenGillibrand") return("kg")
        else if (name == "@KamalaHarris") return("kh")
        else if (name == "@amyklobuchar") return("ak")
        else if (name == "@realDonaldTrump") return("dt")
        else if (name == "@SenWarren") return("ew")
        else if (name == "@BernieSanders") return("bs")
        else if (name == "@BetoORourke") return("br")
        else return("error")
    }
    
    
    
    
    # MENTIONS
    for (person in candidates) {
        filename <- namer(person)
        assign(filename, (search_tweets(person, n = NUMBER_OF_TWEETS) %>% select(-one_of(excluded_col))), envir = parent.frame())
    }
    # Timeline
    for (person in candidates) {
        filename <- paste0(namer(person),"_timeline")
        assign(filename, (get_timeline(person, n = NUMBER_OF_TWEETS) %>% select(-one_of(excluded_col))), envir = parent.frame())
    }
    
}
get_data()
data_selector <- function(name){
    if (name == "Cory Booker") return(cb)
    else if (name == "Pete Buttigieg") return(pb)
    else if (name == "Julian Castro") return(jc)
    else if (name == "John Delaney") return(jd)
    else if (name == "Tulsi Gabbard") return(tg)
    else if (name == "Kirsten Gillibrand") return(kg)
    else if (name == "Kamala Harris") return(kh)
    else if (name == "Amy Klobuchar") return(ak)
    else if (name == "Donald Trump") return(dt)
    else if (name == "Elizabeth Warren") return(ew)
    else if (name == "Bernie Sanders") return(bs)
    else if (name == "Beto O Rourke") return(br)
    else return("error")
}
data_selector_timelines <- function(name){
    if (name == "Cory Booker") return(cb_timeline)
    else if (name == "Pete Buttigieg") return(pb_timeline)
    else if (name == "Julian Castro") return(jc_timeline)
    else if (name == "John Delaney") return(jd_timeline)
    else if (name == "Tulsi Gabbard") return(tg_timeline)
    else if (name == "Kirsten Gillibrand") return(kg_timeline)
    else if (name == "Kamala Harris") return(kh_timeline)
    else if (name == "Amy Klobuchar") return(ak_timeline)
    else if (name == "Donald Trump") return(dt_timeline)
    else if (name == "Elizabeth Warren") return(ew_timeline)
    else if (name == "Bernie Sanders") return(bs_timeline)
    else if (name == "Beto O Rourke") return(br_timeline)
    else return("error")
}
removeURL <- function(x){
    return(str_replace_all(x, "http[^[:space:]]*", "") %>% str_remove_all("\\bamp\\b"))
}
top_words <- function(data){
    tweets <- data
    tweets$text %<>% removeURL()
    tweets %<>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(status_id, word, sort = TRUE) %>% bind_tf_idf(word, status_id, n) %>% select(word, n, tf, idf, tf_idf) %>% arrange(desc(tf_idf))
    tweets[!duplicated(tweets$word),] %>% head(20)
}

    

# Graphing Functions ------------------------------------------------------

topic_graph <- function(data){
    tweets <- data
    tweets$text %<>% removeURL()
    tweets %<>% select(screen_name, text) 
    tweets %>% head(100) %>%  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% separate(bigram, c("word1", "word2"), sep = " ") %>% count(word1, word2, sort = TRUE) %>% filter(n > 2) %>%  graph_from_data_frame() %>% ggraph(layout = "fr") +
        geom_edge_link(alpha = 0.2) +
        geom_node_point(color = TWITTER_BLUE, size = 3)+
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) + theme_minimal() + labs(title = "Topic Model of Tweets", caption = "Using Bi-Grams", xlab = "", ylab = "")
}
get_s <- function(t){
    summed <- t %>% unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        inner_join(get_sentiments("afinn")) %>% 
        group_by(word) %>%
        summarise(Usage = n(), Sentiment = sum(score))
    return(summed)
}
most_used_words_sentiment_graph <- function(data){
    p <- data %>% arrange(desc(Usage)) %>% head(20) %>% arrange(Sentiment) %>% ggplot() +
        geom_bar(aes(x = word, y = Sentiment, fill = Usage), stat = "identity") +
        coord_flip() + theme_minimal()
    ggplotly(p)
}

favorites_comparison_graph <- function(a,b){
    p <- ggplot() + geom_line(data= a, aes(x = created_at,  y =  favorite_count, color = screen_name)) +  geom_line(data = b, aes(x = created_at,  y =  favorite_count, color = screen_name)) +  theme_minimal() + labs(title = "Comparison of Favorites Over Time", xlab="Date",ylab = "Favorites", caption="Based on tweets from selected candidate")
    return(ggplotly(p))
}

rt_comparison_graph <- function(a,b){
    p <- ggplot() + geom_line(data= a, aes(x = created_at,  y =  retweet_count, color = screen_name)) +  geom_line(data = b, aes(x = created_at,  y =  retweet_count, color = screen_name)) +  theme_minimal() + labs(title = "Comparison of Retweets Over Time", xlab="Date",ylab = "Favorites")
    return(ggplotly(p))
}

# Comparisons -------------------------------------------------------------

output$candidate_name_a <- renderText({input$candidate_a})
output$candidate_name_b <- renderText({input$candidate_b})

output$favorites_comparison <- renderPlotly({
    favorites_comparison_graph(data_selector_timelines(input$candidate_a),data_selector_timelines(input$candidate_b))
    })

output$retweets_comparison <- renderPlotly({
    rt_comparison_graph(data_selector_timelines(input$candidate_a),data_selector_timelines(input$candidate_b))
})

output$candidate_a_mentions_sentiment <- renderPlotly({most_used_words_sentiment_graph(get_s(data_selector(input$candidate_a)))})

output$candidate_b_mentions_sentiment <- renderPlotly({most_used_words_sentiment_graph(get_s(data_selector(input$candidate_b)))})
# Refresh Data ------------------------------------------------------------

observeEvent(input$update_archive,{
    showModal(modalDialog(
        title = "Refreshing Data",
        HTML('<img src="https://thumbs.gfycat.com/CostlyAdmiredHackee-small.gif">'),
        easyClose = TRUE,
        footer = NULL
    ))
    get_data()
})

})
