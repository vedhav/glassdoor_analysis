library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(ggalt)
library(textdata)
library(reshape2)
library(lubridate)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)

# Using this function instead of wordcloud2 because there is a render problem in shiny when wordcloud2 is used: https://github.com/rstudio/shinydashboard/issues/281
wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI",
          fontWeight = "bold", color = "random-dark", backgroundColor = "white",
          minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
          rotateRatio = 0.4, shape = "circle", ellipticity = 0.65,
          widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,",
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq,
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color,
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor,
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation,
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape,
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings,
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0,
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}

header_name = c("Glassdoor data analysis", tags$title(tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png"))))
topcompanies <- list.files(path = "www/Best Places to Work", pattern="*.csv",
                           full.names = T) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>%
           mutate(year=substr(basename(x),21,24)))

#drop unnecessary columns and retain those that are needed
topcompanies <-topcompanies %>%
  select(Company = Title,Ranking = h2,Rating = Number,Reviews = subtlelink,Year = year)
#remove anything but alphanumeric
topcompanies$Ranking <- str_replace_all(topcompanies$Ranking, "[^[:alnum:]]","")
topcompanies <- type.convert(topcompanies)

googlereviews_data <- read_csv("www/Google Reviews _ Glassdoor.sg.csv")
googlereviews <- googlereviews_data %>%
  select(c("View5","View7"))
fullgoogle <- cbind(reviews=c(googlereviews$View5,googlereviews$View7)) %>%
  as_tibble()
#remove everything but alphabet, numbers and spaces
fullgoogle$reviews <- gsub("[\r?\n|\r]", "", fullgoogle$reviews)  %>%
  tolower()
fullgoogle$reviews <- gsub("[^0-9A-Za-z ]","",fullgoogle$reviews)
#clean
tidygoogle <- fullgoogle %>%
  # tibble(line=1:576) %>%
  unnest_tokens(word,reviews) %>%
  group_by(word) %>%
  filter(n()>5) %>%
  ungroup()
cleangoogle <- tidygoogle %>%
  anti_join(stop_words)
googlesentiment <- cleangoogle %>%
  filter() %>%
  group_by(word) %>%
  dplyr::summarise(frequency=n())
affin_sentiments <- get_sentiments("afinn")
affin_lexicon_1 <- googlesentiment %>%
      select(word) %>%
      inner_join(affin_sentiments) %>%
      mutate(sentiment=ifelse(value>0,"positive","negative"),score=value)
affin_lexicon_2 <- googlesentiment %>%
      inner_join(affin_sentiments) %>%
      mutate(value=frequency*value) %>%
      mutate(sentiment=ifelse(value>0,"positive","negative"),score=value)
employee_reviews <- read_csv("www/Employee-Reviews-Analysis-master/Data/employee_reviews.csv")
employee_reviews$dates <- as.Date(employee_reviews$dates, format = "%b %d, %Y")

employeereviewclean2 <- employee_reviews %>%
  filter(dates>=2018-01-01) %>%
  select(company,`work-balance-stars`,`culture-values-stars`,`carrer-opportunities-stars`,`comp-benefit-stars`,`senior-mangemnet-stars`)

employeereviewclean2$`work-balance-stars` <- as.numeric(employeereviewclean2$`work-balance-stars`)
employeereviewclean2$`culture-values-stars` <- as.numeric(employeereviewclean2$`culture-values-stars` )
employeereviewclean2$`carrer-opportunities-stars` <- as.numeric(employeereviewclean2$`carrer-opportunities-stars`)
employeereviewclean2$`comp-benefit-stars` <- as.numeric(employeereviewclean2$`comp-benefit-stars`)
employeereviewclean2$`senior-mangemnet-stars` <- as.numeric(employeereviewclean2$`senior-mangemnet-stars`)
employeereviewclean3 <-  employeereviewclean2%>%
      group_by(company) %>%
      summarise_all(list(mean),na.rm = TRUE)

ui <- dashboardPage(
  title = c(header_name[1],header_name[3],header_name[4],header_name[5]),
  dashboardHeader(
    tags$li(class = "dropdown", tags$style(".main-header {padding-top: 4px}")),
    titleWidth = 300,
    title = span(
      tagList(
        tags$a(
          tags$img(src = "logo.png", height = 50, align = "left"),
          tags$b("Glassdoor analysis", style = "color:white"), title = "Title Placeholder",
          href = "https://google.com/", target = "_blank",
          style = "padding-top: 80px"
        )
      )
    ),
    tags$li(class = "dropdown", tags$a(HTML(paste(textOutput("Refresh1")))))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data_ui", icon = icon("table")),
      menuItem("Trend", tabName = "trend_ui", icon = icon("chart-line")),
      menuItem("Ratings", tabName = "ratings_ui", icon = icon("star")),
      menuItem("Review", tabName = "review_ui", icon = icon("comment-alt"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "data_ui",
        tabsetPanel(
          tabPanel(
            "topcompanies",
            DTOutput("topcompanies_table")
          ),
          tabPanel(
            "googlereviews",
            DTOutput("googlereviews_table")
          ),
          tabPanel(
            "employee_reviews",
            DTOutput("employee_reviews_table")
          )
        )
      ),
      tabItem(
        tabName = "trend_ui",
        fluidPage(
          fluidRow(
            column(3, selectInput("analysis_type", "Analysis Type", c("Yearly", "Across the Years"))),
            column(4, selectInput("year_of_interest", "Year of interest", 2015:2020, 2020)),
            column(2, selectInput("colour_in", "Colour", c("Rating", "Company"))),
            column(3, selectInput("number_of_companies", "Number of Companies", 1:100, 10))
          ),
          uiOutput("ranking_plot_ui")
        )
      ),
      tabItem(
        tabName = "ratings_ui",
        fluidPage(
          plotOutput("employee_review_plot_1"),
          plotOutput("employee_review_plot_2"),
          plotOutput("employee_review_plot_3"),
          plotOutput("employee_review_plot_4"),
          plotOutput("employee_review_plot_5"),
          plotOutput("employee_review_plot_6"),
          plotOutput("employee_review_plot_7")
        )
      ),
      tabItem(
        tabName = "review_ui",
        fluidPage(
          wordcloud2Output("google_sentiment_wordcloud"),
          plotOutput("affin_plot_1"),
          plotOutput("affin_plot_2"),
          plotOutput("bing_plot")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  output$topcompanies_table <- renderDT({
    topcompanies
  })
  output$googlereviews_table <- renderDT({
    googlereviews_data
  })
  output$employee_reviews_table <- renderDT({
    topcompanies
  })
  output$ranking_plot_ui <- renderUI({
    plot_height <- as.numeric(input$number_of_companies) * 30 + 40
    if (input$analysis_type != "Yearly") {
      plot_height <- plot_height * 2
    }
    output$ranking_plot <- renderPlot({
      if (input$analysis_type == "Yearly") {
        plot_data <- topcompanies %>%
          filter(Year == input$year_of_interest, Ranking <= as.numeric(input$number_of_companies))
        plot <- plot_data %>%
          ggplot(aes(x = reorder(Company,desc(Ranking)), y = Rating)) +
          geom_bar(stat = "identity",aes(fill=plot_data[[input$colour_in]])) +
          geom_text(aes(label = Ranking), hjust = 1.1, color = "#FFFFFF") +
          scale_y_continuous(labels = scales::comma_format()) +
          coord_flip() +
          labs(x = "Company", y = "Ranking", title = paste0("Best Company to Work in ", input$year_of_interest))
        if (input$colour_in == "Rating") {
          plot <- plot + labs(fill = input$colour_in)
        } else {
          plot <- plot + theme(legend.position="none")
        }
        return(plot)
      } else {
        plot_data <- topcompanies %>%
          filter(Ranking <= as.numeric(input$number_of_companies))
        plot <- plot_data %>%
          ggplot(aes(x = reorder(Company,desc(Ranking)), y = Rating)) +
          facet_wrap(~ Year, scales = "free_y") +
          geom_bar(stat = "identity",aes(fill=plot_data[[input$colour_in]])) +
          geom_text(aes(label = Ranking), hjust = 1.1, color = "#FFFFFF") +
          scale_y_continuous(labels = scales::comma_format()) +
          coord_flip() +
          labs(x = "Company", y = "Ranking", title = "Best Company to Work across the years")
        if (input$colour_in == "Rating") {
          plot <- plot + labs(fill = input$colour_in)
        } else {
          plot <- plot + theme(legend.position="none")
        }
        return(plot)
      }
    })
    plotOutput("ranking_plot", height = plot_height)
  })
  output$google_sentiment_wordcloud <- renderWordcloud2({
    googlesentiment %>% wordcloud2a()
  })
  output$affin_plot_1 <- renderPlot({
    ggplot(affin_lexicon_1,aes(x=reorder(word,score),y=score,colour=sentiment,fill=sentiment))+
      geom_col(alpha=0.5)+
      coord_flip()
  })
  output$affin_plot_2 <- renderPlot({
    ggplot(affin_lexicon_2,aes(x=reorder(word,score),y=score,colour=sentiment,fill=sentiment))+
      geom_col(alpha=0.5)+
      coord_flip()
  })
  output$bing_plot <- renderPlot({
    googlesentiment %>%
      inner_join(get_sentiments("bing")) %>%
      dplyr::count(word,sentiment,frequency,sort=TRUE) %>%
      acast(word~sentiment,value.var="frequency",fill=0) %>%
      comparison.cloud(max.words = 150)
  })
  output$employee_review_plot_1 <- renderPlot({
    employeereviewclean <- employee_reviews %>%
      group_by(dates,company) %>%
      summarise("mean(overall-ratings)" = mean(`overall-ratings`), .groups = "drop") %>%
      rename(date = dates)
    employeereviewclean %>%
      filter(company == "apple") %>%
      ggplot(aes(x=date,y=`mean(overall-ratings)`))+
      geom_line()
  })
  output$employee_review_plot_2 <- renderPlot({
    employeereviewclean <- employee_reviews %>%
      mutate(date=floor_date(dates, "month")) %>%
      group_by(date,company) %>%
      summarise(mean = mean(`overall-ratings`), .groups = "drop") %>%
      filter(!is.na(date))

    employeereviewplot <- employeereviewclean %>%
      #filter(company=="apple") %>%
      ggplot(employeereviewclean,mapping=aes(x=date,y=mean,group=company))+
      geom_line(aes(colour=company))
    employeereviewplot
  })
  output$employee_review_plot_3 <- renderPlot({
    employeereviewclean <- employee_reviews %>%
      mutate(date=floor_date(dates, "month")) %>%
      group_by(date,company) %>%
      summarise(mean = mean(`overall-ratings`), .groups = "drop")

    employeereviewplot <- employeereviewclean %>%
      #filter(company=="apple") %>%
      ggplot(employeereviewclean,mapping=aes(x=date,y=mean,group=company))+
      geom_line(aes(colour=company))
    employeereviewplot+facet_grid(company~.)
  })
  output$employee_review_plot_4 <- renderPlot({
    employeereviewclean <- employee_reviews %>%
      mutate(date=floor_date(dates, "year")) %>%
      group_by(date,company) %>%
      summarise(mean = mean(`overall-ratings`), .groups = "drop")

    employeereviewplot <- employeereviewclean %>%
      ggplot(employeereviewclean,mapping=aes(x=date,y=mean,group=company))+
      geom_line(aes(colour=company))
    employeereviewplot+theme_bw()
  })
  output$employee_review_plot_5 <- renderPlot({
    plot3 <- employeereviewclean3 %>%
      ggplot(aes(x=company,y=`work-balance-stars`,colour=company)) +
      geom_bar(stat='identity',aes(fill=company))+
      labs(x="Company",
           y="Work Balance Rating")
    plot3
  })
  output$employee_review_plot_6 <- renderPlot({
    plot4 <- employeereviewclean3 %>%
      ggplot(aes(x=company,y=`work-balance-stars`,label=`work-balance-stars`)) +
      geom_point(stat="identity",aes(colour="company"))+
      geom_segment(aes(y=mean(`work-balance-stars`),
                       x=company,
                       xend=company,
                       yend=`work-balance-stars`))+
      labs(x="Company",
           y="Work Balance Rating")+
      coord_flip()
    plot4+theme_bw()
  })
  output$employee_review_plot_7 <- renderPlot({
    employeereviewclean3 <-  employeereviewclean2%>%
      group_by(company) %>%
      summarise_all(list(mean),na.rm = TRUE)
    employeereviewclean3$`work-balance-stars` <- round((employeereviewclean3$`work-balance-stars` - mean(employeereviewclean3$`work-balance-stars`))/sd(employeereviewclean3$`work-balance-stars`), 2)
    employeereviewclean3 %>%
      group_by(company)
      employeereviewclean3 <- mutate(employeereviewclean3,
                               belowabove=ifelse(employeereviewclean3$`work-balance-stars` < 0, "below", "above"))
    employeereviewclean3 <- employeereviewclean3[order(employeereviewclean3$`work-balance-stars`), ]  # sort
    employeereviewclean3$company <- factor(employeereviewclean3$company, levels = employeereviewclean3$company)  # convert to factor to retain sorted order in plot.
    ggplot(employeereviewclean3, aes(x=company, y=`work-balance-stars`, label=`work-balance-stars`)) +
      geom_point(stat='identity', aes(col=belowabove), size=6)  +
      geom_segment(aes(y = 0,
                       x = company,
                       yend = `work-balance-stars`,
                       xend = company)) +
      geom_text(color="white", size=2) +
      scale_color_manual(name="Work Balance Stars",
                         labels = c("Above Average", "Below Average"),
                         values = c("above"="#00ba38", "below"="#f8766d")) +
      labs(title="Diverging Lollipop Chart",
           x="Company",
           y="Work Balance Stars")+
      coord_flip()
  })
}

shinyApp(ui = ui, server = server)
