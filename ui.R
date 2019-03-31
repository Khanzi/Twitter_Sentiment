
# Libraries ---------------------------------------------------------------

require(shiny)
require(tidyverse)
require(magrittr)
require(plotly)
require(shinythemes)

candidates <- c("Cory Booker", "Pete Buttigieg", "Julian Castro", "John Delaney", "Tulsi Gabbard", "Kirsten Gillibrand","Kamala Harris","Amy Klobuchar","Donald Trump","Elizabeth Warren","Bernie Sanders",
                "Beto O Rourke")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("journal"),
    
    navbarPage("Political Sentiment Using Twitter",
               tabPanel("Explore Candidates",
                        selectInput("select_candidate","Select Candidate", choice = candidates),
                        h1(textOutput("candidate_name")),
                        hr(),
                        br(),
                        sidebarPanel(htmlOutput("candidate_embed")),
                        mainPanel(
                            h2("Topic Model"),
                            plotOutput("candidate_topic_graph"),
                            h2("Words Most Unique to Selected Candidate"),
                            tableOutput("candidate_unique_words"),
                            h2("Words used by Candidate"),
                            plotlyOutput("candidate_sentiment_word")
                        )
                        ),
               
               
               
               tabPanel("Comparisons",
                       column(12, h1("This data is from public tweets mentioning the selected candidate.")),
                       column(6, selectInput("candidate_a","Candidate A", choices = candidates)),
                       column(6, selectInput("candidate_b", "Candidate B", choices = candidates,selected = candidates[2])),
                       hr(),
                       column(12,plotlyOutput("favorites_comparison"),
                              hr(),
                              plotlyOutput("retweets_comparison")),
                       column(6,h2(textOutput("candidate_name_a")),
                              plotlyOutput("candidate_a_mentions_sentiment"),
                              plotOutput("candidate_a_mentions_topic_model")),
                       column(6,h2(textOutput("candidate_name_b")),
                              plotlyOutput("candidate_b_mentions_sentiment"),
                              plotOutput("candidate_b_mentions_topic_model"))
                       ),
               
               
               
               
               # DONE
               # For updates edit the respective markdown files.
               navbarMenu("More",
                          tabPanel("About",
                                   includeMarkdown("about.md")),
                          
                          tabPanel("Missing Candidate",
                                   includeMarkdown("missing.md"))))
               )

    
)
