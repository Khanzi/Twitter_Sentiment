
# Libraries ---------------------------------------------------------------

library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)
library(shinythemes)

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
                       column(6, selectInput("candidate_a","Candidate A", choices = candidates)),
                       column(6, selectInput("candidate_b", "Candidate B", choices = candidates,selected = candidates[2])),
                       hr(),
                       column(12,plotlyOutput("favorites_comparison"),
                              hr(),
                              plotlyOutput("retweets_comparison")),
                       column(6,h2(textOutput("candidate_name_a")),
                              plotlyOutput("candidate_a_mentions_sentiment")),
                       column(6,h2(textOutput("candidate_name_b")),
                              plotlyOutput("candidate_b_mentions_sentiment"))
                       ),
               
               
               
               
               # DONE
               # For updates edit the respective markdown files.
               navbarMenu("More",
                          tabPanel("About",
                                   includeMarkdown("about.md")),
                          
                          tabPanel("Missing Candidate",
                                   includeMarkdown("missing.md")),
                          
                          
                          tabPanel("Archive",
                                   h1("So you want more data eh?"),
                                   h2("Well you could press this button but be warned..."),
                                   h3("It will take a considerable amount of time for new data to be downloaded and added to the existing archive"),
                                   br(),
                                   column(4, actionButton("update_archive","Update Archive")),
                                   column(4, tags$a(class="btn btn-default", href="https://www.youtube.com/watch?v=dQw4w9WgXcQ", "Delete Archive")),
                                   column(4, actionButton("smart","Something so intelligble I'm not even allowed to disclose it's purpose")),
                                   br(),
                                   h2("Now that you've pressed all the buttons -  here's some stats about the archive"))))
               )

    
)
