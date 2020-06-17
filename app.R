library(tidyverse)
library(httr)
library(jsonlite)
library(knitr)
library(kableExtra)
library(assertthat)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)

url_base <- "https://newsapi.org/v2/"
apiKey <- "086159d175cc4de1b849c32d2611bd0f"
max_pageSize <- 100

valid_category <- c(All = "default", Business = "business", Entertainment = "entertainment", General = "general", Health = "health", Science = "science", Sports = "sports", Technology = "technology")

valid_sortBy <- c(Featured = "default", Relevancy = "relevancy", Popularity = "popularity", "Published Date" = "publishedAt")


query_sources <- "sources?"

# get category general
url_query_general <- str_c(url_base, query_sources, "&category=general",
                           "&language=en", "&country=us", "&apiKey=", apiKey)
query_general_raw <- fromJSON(url_query_general)

# get category business
url_query_business <- str_c(url_base, query_sources, "&category=business",
                            "&language=en", "&country=us","&apiKey=", apiKey)
query_business_raw <- fromJSON(url_query_business)

df <- rbind(query_general_raw$sources, query_business_raw$sources)

news_name <- c("CNN", "Fox News", "The Wall Street Journal", "Reuters")

id_news <- sapply(news_name, function(x){
  df %>%
    filter(name == x) %>%
    select(id) %>% 
    pull()
})

add_filter <- function(url, filter_name, filter){
  return(str_c(url, "&", filter_name, "=", filter))}

get_news <- function(url){
  
  data_raw <- read_json(url)
  
  if (data_raw$status == "ok"){
    if (data_raw$totalResults > 0){
      names <- data_raw$articles %>% 
        sapply(attributes) %>% 
        unlist() %>% 
        unique
      
      df <- data_raw$articles %>% purrr::flatten()
      df <- data.frame(matrix(df, nrow = length(data_raw$articles), byrow=T))
      colnames(df) <- names
      
      return(df %>%
               mutate(id = map(source, "id")) %>%
               mutate(name = map(source, "name")) %>%
               mutate(date = str_extract(publishedAt, "\\d{4}-\\d{2}-\\d{2}")) %>%
               mutate(time = str_extract(publishedAt, "\\d{2}:\\d{2}:\\d{2}")) %>%
               mutate(author = ifelse(is.na(author), name, author)) %>%
               dplyr::select(-c(source, publishedAt)) %>%
               unique())
    }
  }
  return(data_raw$message)
}

get_sources <- function(apiKey, category = ""){
  
  # input check
  assert_that(is.string(apiKey),
              is.string(category),
              category == "" | category %in% valid_category)
  
  url <- str_c(url_base, "sources?language=en&country=us")
  if (category != ""){url <- add_filter(url, "category", category)}
  url <- add_filter(url, "apiKey", apiKey)
  
  fromJSON(url)$sources %>%
    dplyr::select(name, category, description, url) %>%
    dplyr::rename(Source = name,
                  Category = category,
                  Description = description,
                  URL = url)
}

get_headlines <- function(apiKey, sources = "", q = "", pageSize = "", page = ""){
  
  # input check
  assert_that(is.string(apiKey),
              is.character(sources),
              is.string(q),
              pageSize == "" | is.count(pageSize),
              page == "" | is.count(page))
  
  # auto-adjust inputs
  if (pageSize != "") {pageSize <- min(pageSize, max_pageSize)}
  
  url <- str_c(url_base, "top-headlines")
  
  if (any(sources != "")){
    sources <- sources[sources != ""]
    sources <- paste(sources, collapse = ",")
    url <- add_filter(url, "sources", sources)
  } else {
    url <- add_filter(url, "country", "us")
  }
  if (q != ""){url <- add_filter(url, "q", q)}
  if (pageSize != ""){url <- add_filter(url, "pageSize", pageSize)}
  if (page != ""){url <- add_filter(url, "page", page)}
  
  url %>%
    add_filter("apiKey", apiKey) %>%
    str_replace("&", "?") %>%
    get_news()
  
}

get_historic <- function(apiKey, q = "", qInTitle = "", sources = "",
                         from = "", to = "", sortBy = "", pageSize = "",
                         page = ""){
  
  # input check
  assert_that(is.string(apiKey),
              is.string(q),
              is.string(qInTitle),
              is.character(sources),
              sortBy == "" | sortBy %in% valid_sortBy,
              pageSize == "" | is.count(pageSize),
              page == "" | is.count(page))
  
  # further check date inputs
  if (from != "" & to != ""){assert_that(to >= from)}
  if (from != ""){assert_that(is.date(as.Date(from)))}
  if (to != ""){assert_that(is.date(as.Date(to)))}
  
  url <- str_c(url_base, "everything?language=en")
  if (q != ""){url <- add_filter(url, "q", q)}
  if (qInTitle != ""){url <- add_filter(url, "qInTitle", qInTitle)}
  if (any(sources != "")){
    sources <- sources[sources != ""]
    sources <- paste(sources, collapse = ",")
    url <- add_filter(url, "sources", sources)
  } else {
    url <- add_filter(url, "country", "us")
  }
  if (from != ""){url <- add_filter(url, "from", from)}
  if (to != ""){url <- add_filter(url, "to", to)}
  if (sortBy != ""){url <- add_filter(url, "sortBy", sortBy)}
  if (pageSize != ""){url <- add_filter(url, "pageSize", pageSize)}
  if (page != ""){url <- add_filter(url, "page", page)}
  
  url %>%
    add_filter("apiKey", apiKey) %>%
    get_news()
  
}


# default parameters
default_keyword <- "Enter keywords or phrases"
default_category <- "Enter source category"


# header
news_header <- dashboardHeader(title = "US News Dashboard")

# sidebar
news_sidebar <- dashboardSidebar(
  
  sidebarMenu(id = "sidebarmenu",
              
              menuItem("User Manual", tabName = "usermanual", icon = icon("book-open")),
              
              menuItem("News", tabName = "news", icon = icon("newspaper")),
              conditionalPanel("input.sidebarmenu == 'news'",
                               
                               # Recent or Past
                               radioButtons("select_recent", label = "Customized Search",
                                            choices = list("Recent Headlines" = 1, "Historical Articles" = 2)),
                               
                               # Check box for news sources
                               checkboxGroupInput("check_sources", label = "News Source", choices = id_news),
                               
                               # Keyword search (serve both q and qInTitle)
                               textInput("keyword", label = "Keyword", value = default_keyword),
                               br(),
                               
                               conditionalPanel(
                                 condition = "input.select_recent == 2",
                                 
                                 # Dates input
                                 dateRangeInput("dates", label = "Date range",start = Sys.Date()- 7, 
                                                end = Sys.Date() - 3,min = Sys.Date() - 30, max = Sys.Date(), 
                                                format = "yyyy-mm-dd"),
                                 
                                 # Select box for sortBy
                                 selectInput("select_sortBy", label = "Sort By", choices = valid_sortBy)
                               ),
                               br(),
                               
                               # Action button
                               actionButton("news_button", label = "Get News", icon = icon("sync-alt")),
                               br()
              ),
              
              menuItem("Sources", tabName = "sources", icon = icon("rss")),
              conditionalPanel("input.sidebarmenu === 'sources'",
                               # Category
                               selectInput("select_category", label = "Category", choices = valid_category),
                               # Action button
                               actionButton("sources_button", label = "Get Sources", icon = icon("sync-alt")),
                               br()
              )
  )
)

# body
news_body <- dashboardBody(
  
  useShinyjs(),
  useShinyalert(),
  tabItems(
    
    tabItem(tabName = "usermanual",
            
            h1("Welcome to US News App!", align = "center"),
            HTML('<p>US News App aims to provide users with easy access to breaking news headlines and articles from a variety of US news sources. This is the free trial version of the app. Here you have limited access to news from four sources. Check out <a href="https://newsapi.org/pricing/">our website</a> to subscribe for full access.</p>'),
            
            h2("How to use the App", align = "center"),
            
            fluidRow(
              
              column(6,
                     h3("GET STARTED", align = "center"),
                     p(paste(rep("_",40), collapse = ""), align = "center"),
                     
                     h3("Recent Headlines News"),
                     p("Users can access recent headline news."),
                     tags$ol(
                       tags$li("Go to 'News' tab and select 'Recent Headlines' under 'Customized Search'"), 
                       tags$li("Choose one or more sources under 'News Source'. By default no selection is same as selecting all"), 
                       tags$li("[Optional] Enter any key word or phrases under 'Keyword'"),
                       tags$li("Click 'Get News' button to refresh the news feed"),
                       tags$li("The free trial app gives users access to at most 10 news per source per query. Use the page turn button at the bottom to navigate through all headlines news"),
                       tags$li("If there is no relevant recent news on a particular topic, news feed will be automatically reset to default search results under the selected source(s). At the same time, users will be prompted to change the search criteria")
                     ),
                     
                     h3("Historical Articles"),
                     p("Users can access news articles in the past one month."),
                     tags$ol(
                       tags$li("Go to 'News' tab and select 'Historical Articles' under 'Customized Search'"), 
                       tags$li("Choose one or more sources under 'News Source'. By default no selection is same as selecting all"), 
                       tags$li("[Optional] Enter any key word or phrases under 'Keyword'"),
                       tags$li("Select the dates, or range of dates of interest, with an end date later than the start date"),
                       tags$li("[Optional] Select one of the sorting criteria"),
                       tags$li("Click 'Get News' button to refresh the news feed"),
                       tags$li("The free trial app gives users access to at most 100 news per query. Use the page turn button at the bottom to navigate through all articles"),
                       tags$li("If there is no relevant articles, news feed will be automatically reset to default search results under the selected source(s). At the same time, users will be prompted to change the search criteria"),
                     ),
                     br()
              ),
              
              column(6,
                     h3("AND CHECK THESE OUT", align = "center"),
                     p(paste(rep("_",40), collapse = ""), align = "center"),
                     
                     h3("Sources"),
                     p("Have a look at other news sources in our full version App!"),
                     tags$ol(
                       tags$li("Go to 'Sources' tab"), 
                       tags$li("Select one or all of the news categories of interest"), 
                       tags$li("Click 'Get Sources' button to get a list of news sources available in our full version app"),
                       tags$li("Users can also use the 'Search' function in the page to look for sources of your interest")
                     ),
                     
                     br(), br(), br(), br(), br(), br(),
                     
                     h3("Contact Us"), br(),
                     HTML('<i class="fa fa-comments fa-fw" style="font-size:25px;color:#21618C;margin-right:10px;"></i><a href="mailto:christine.ym.shen@gmail.com">FEEDBACK</a>'), br(), br(),
                     HTML('<i class="fa fa-question fa-fw" style="font-size:25px;color:#21618C;margin-right:10px;"></i><a href="mailto:shawn.santo@duke.edu">HELP</a>'), br(), br(),
                     HTML('<i class="fa fa-shopping-cart fa-fw" style="font-size:25px;color:#21618C;margin-right:10px;"></i><a href="https://newsapi.org/pricing/">SUBSCRIBE</a>'), br(), br(),
                     HTML('<i class="fa fa-laptop-code fa-fw" style="font-size:25px;color:#21618C;margin-right:10px;"></i><a href="https://shawnsanto.com/teaching/duke/sta523/grade-item-schedule/">APP STORY</a>')
                     
              )
            ),
            div(style = "position:fixed;width:100%;background-color:#2E86C1;right:0;bottom:0;",
                div(style = "float: right;margin-right:10px",
                    p("Copyright 2019 Fall STA 523 Inc.")),
                br()
            )
    ),
    
    tabItem(tabName = "news",
            
            fluidRow(
              lapply(1:2, function(i) {
                uiOutput(paste0('b', i))
              })
            ),
            
            fluidRow(
              lapply(3:4, function(i) {
                uiOutput(paste0('b', i))
              })
            ),
            
            fluidRow(
              
              column(4,
                     actionButton("prevBtn", "< Previous")),
              column(4, style = "margin-top: 10px;", align = "center",
                     textOutput("page_num")),
              column(4, align = "right",
                     actionButton("nextBtn", "Next >"))
            ),
    ),
    
    tabItem(tabName = "sources",
            fluidPage(
              titlePanel("A HUGE range of sources"),
              
              h4("Full membership gives you access to articles at over 50 US news sources"),
              
              DT::dataTableOutput("table")
            )
    )
  )
)

# ui
ui <- dashboardPage(
  news_header,
  news_sidebar,
  news_body
)


# server
server <- function(input, output) {
  
  # source tab
  sources <- reactive({
    
    input$sources_button
    
    isolate({
      
      if (input$select_category == "default"){
        category <- ""
      } else {category <- input$select_category}
      
      get_sources(apiKey, category = category)
    })
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- sources()
  }))
  
  # news tab
  news <- reactive({
    
    togetnews$checked
    
    isolate({
      
      if (input$keyword == default_keyword){
        keyword <- ""
      } else {keyword <- str_replace_all(input$keyword, " ", "\\%20")}
      
      if (is.null(input$check_sources)){
        sources <- id_news
      } else {sources <- input$check_sources}
      
      #sources <- ""
      
      if (input$select_recent == 1){
        news_raw <- get_headlines(apiKey, sources = sources,
                                  q = keyword, pageSize = max_pageSize)
        
      } else if (input$select_recent == 2){
        
        if (input$dates[2] < input$dates[1]){news()}
        
        if (input$select_sortBy == "default"){
          sortBy <- ""
        } else {sortBy <- input$select_sortBy}
        
        news_raw <- get_historic(apiKey, q = keyword, sources = sources,
                                 from = as.character(input$dates[1]), to = as.character(input$dates[2]),
                                 sortBy = sortBy, pageSize = max_pageSize)
      }
      
      if (is.null(news_raw)){
        shinyalert("", "No relevant news on this topic. Please start a new search", type = "info")
        get_headlines(apiKey, sources = input$check_sources, pageSize = max_pageSize)
      } else {
        news_raw
      }
      
    })
  })
  
  lapply(1:4, function(i) {
    output[[paste0('b', i)]] <- renderUI({
      box(
        img(src = news()$urlToImage[i + (rv$page-1) * 4], width = "100%"),
        tags$b(tags$h4(str_c(news()$title[i + (rv$page-1) * 4]))),
        p(news()$description[i + (rv$page-1) * 4]),
        div(
          a("Read More", href = news()$url[i + (rv$page-1) * 4]),
          p(str_c("Updated ", news()$time[i + (rv$page-1) * 4], " UTC, ", news()$date[i + (rv$page-1) * 4]),
            style="color:#808B96; font-style:italic; float:right;")
        )
      )
    })
  })
  
  togetnews <- reactiveValues(checked = 1)
  
  # multiple pages
  rv <- reactiveValues(page = 1)
  
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < ceiling(dim(news())[1] / 4))
    hide(selector = ".page")
    output$page_num <- renderText({
      str_c("Page ", rv$page, " / ", ceiling(dim(news())[1] / 4))
    })
  })
  
  
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  
  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, navPage(1))
  observeEvent(input$news_button, {
    rv$page <- 1
    
    if (input$select_recent == 2 & input$dates[2] < input$dates[1]){
      shinyalert("Error", "End date should be same or after the start date", type = "error")
    } else {
      togetnews$checked <- runif(1)
    }
  })
  
  observeEvent(rv$page, {
    
    if (rv$page == ceiling(dim(news())[1] / 4)){
      for (i in (dim(news())[1] %% 4 + 1): 4){
        shinyjs::hide(id = paste0('b', i))
      }
    } else{
      for (i in 1:4){
        shinyjs::show(id = paste0('b', i))
      }
    }
  })
  
}

shinyApp(ui, server, options = list(height = 1000))

