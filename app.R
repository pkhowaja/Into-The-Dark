#
# This is a Shiny web application. 
#
# It includes visualizations of the stops made in California across time of the day and 
#

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(shinythemes)

cadataupdated <- read_csv("cadataupdated.csv")
  #Generating data sets here


#Define UI ----
ui <-  fluidPage(theme = shinytheme("yeti"),
                
  titlePanel("In the Dark: Exploring Racial Disparities in Traffic Stops Before and After Sunset"),

  sidebarLayout(
    sidebarPanel(selectInput("choose_city", "Choose City", choices = c("San Francisco", "Oakland", "San Jose", "Bakersfield", 
                                                                       "Los Angeles", "San Diego")),
                 sliderInput("before_sunset", "Minutes before Sunset", min = 30, max = 600, value = 360),
                 sliderInput("after_sunset", "Minutes after Sunset", min = 30, max = 600, value = 360)
                 ),
    mainPanel(
      tabsetPanel(
      tabPanel("Looking for Bias", plotOutput("histogram"), textOutput("main_page")), 
      tabPanel("Pie Charts", plotOutput("pie")),
      tabPanel("Summary", textOutput("summary"), plotOutput("dayvsnight")))
      
    ))


) #end of Ui


# Define server logic ----
server <- function(input, output) {
  
  output$pie <- renderPlot({
    
    select_city <- switch (input$choose_city,
      "San Francisco" = "sf",
      "Oakland" = "ok",
      "Los Angeles" = "la",
      "San Jose" = "sj",
      "San Diego" = "sd",
      "Bakersfield" = "bf")
    
    stops.day <- cadataupdated %>% filter(city == select_city, daytime == "TRUE") %>%
      filter(minsto.set < input$before_sunset) %>%
      group_by(subject_race) %>%
      tally() %>%
      mutate(proportion = n/ sum(n)) %>%
      mutate_at(vars(proportion), funs(round(., 3))) 
    
    day.plot <- ggplot(stops.day, aes(x = "", y = proportion, fill = subject_race))+
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      theme_minimal() + 
      labs(title = "Daytime Stops")+
      labs(fill = "Race")+
      scale_fill_manual(values = c("deepskyblue2","dodgerblue4", "skyblue1", "skyblue3", "cadetblue1"),
                        labels = c("asian/pacific islander", "black", "hispanic", "other", "white"))
    
    time_after_sunset <- (-1)*(input$after_sunset)
    
    stops.night <- cadataupdated %>% filter(city == select_city, daytime == "FALSE") %>%
      filter(minsto.set > time_after_sunset) %>%
      group_by(subject_race) %>%
      tally() %>%
      mutate(proportion = n/ sum(n)) %>%
      mutate_at(vars(proportion), funs(round(., 3))) 
    
    night.plot <- ggplot(stops.night, aes(x = "", y = proportion, fill = subject_race))+
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      theme_minimal() + 
      labs(title = "Nightime Stops")+
      labs(fill = "Race")+
      scale_fill_manual(values = c("deepskyblue2","dodgerblue4", "skyblue1", "skyblue3", "cadetblue1"),
                        labels = c("asian/pacific islander", "black", "hispanic", "other", "white"))
    
    plot_grid(day.plot, night.plot)
  })  
    
    #Histogram Output
    output$histogram <- renderPlot({
      
      select_city <- switch (input$choose_city,
                             "San Francisco" = "sf",
                             "Oakland" = "ok",
                             "Los Angeles" = "la",
                             "San Jose" = "sj",
                             "San Diego" = "sd",
                             "Bakersfield" = "bf")
      
      stops.day.hist <- cadataupdated %>% filter(city == select_city, daytime == "TRUE") %>%
        filter(minsto.set < input$before_sunset) %>%
        group_by(subject_race) %>%
        tally() %>%
        mutate(proportion = n/ sum(n)) %>%
        mutate_at(vars(proportion), funs(round(., 3))) 
      
     day.hist <- ggplot(stops.day.hist, aes(x = subject_race, y= proportion))+
        geom_bar(stat = "identity", , fill = "steelblue4")+ theme_minimal()+
        ggtitle("Daytime Stops")+ xlab("Race") + ylab("Proportion of total stops")
      
     time_after_sunset <- (-1)*(input$after_sunset)

     stops.night.hist <- cadataupdated %>% filter(city == select_city, daytime == "FALSE") %>%
       filter(minsto.set > time_after_sunset) %>%
       group_by(subject_race) %>%
       tally() %>%
       mutate(proportion = n/ sum(n)) %>%
       mutate_at(vars(proportion), funs(round(., 3)))  
     
     night.hist <- ggplot(stops.night.hist, aes(x = subject_race, y= proportion))+
       geom_bar(stat = "identity", , fill = "steelblue4")+ theme_minimal()+
       ggtitle("Nightime Stops")+ xlab("Race") + ylab("Proportion of total stops")
     
     plot_grid(day.hist, night.hist)
    })
 
  output$main_page <- renderText({
    paste("The Stanford Open Policing Project uses the 'Veil of Darkness' test (2006) to find racial bias in cop decisions to pull over 
          drivers. The method used in the Stanford project looks at time frames immediately before sunset and after dusk. This part of our 
          applet investigates whether their decision to use this limited time frame significantly changes the results we may obtain.
          
          Change the sliders to observe the changes in proportion of stops by race. Adjusting the sliders so that results 
          for 60 minutes before and after sunset are displayed will limit many extraneous factors such as which race is out more during the day.
          However, we notice that in the case of these major Californian cities, we do not obtain significant results even by limiting the time
          frame.")
    
    
  })
   output$summary <- renderText({
     paste("While our results show that using the veil of darkness approach does not provide any evidence of racial bias in traffic stops
           for these cities in California, there is still slight evidence of bias. This is seen in the pie charts below. In almost all these 
           major cities, white people are stopped much more often at night rather than during the day. For people of color, especially black and hispanic
          folks, the split is more even. This indicates that white people are less likely to be stopped during the day as compared 
           to at night.")
   })
   output$dayvsnight <- renderPlot({
     select_city <- switch (input$choose_city,
                            "San Francisco" = "sf",
                            "Oakland" = "ok",
                            "Los Angeles" = "la",
                            "San Jose" = "sj",
                            "San Diego" = "sd",
                            "Bakersfield" = "bf")
     
     stops.white <- filter(cadataupdated, city == select_city, subject_race == "white") %>% 
       group_by(daytime) %>% 
       tally() %>%
       mutate(proportion = n / sum(n)  ) %>%
       mutate_at(vars(proportion), funs(round(., 3)))
     
     plot.white <- ggplot(stops.white, aes(x = "", y = proportion , fill = daytime))+
       geom_bar(width = 1, stat = "identity", color = "white") + 
       coord_polar("y", start = 0)+ 
       theme_minimal() + labs(title = "White folks stopped" ) +
       labs(fill = "Time of Day") +
       scale_fill_manual(values = c("gold1","dodgerblue4"),
                         labels = c("Day", "Night"))
     
     stops.black <- filter(cadataupdated, city == select_city, subject_race == "black") %>% 
       group_by(daytime) %>% 
       tally() %>%
       mutate(proportion = n / sum(n)  ) %>%
       mutate_at(vars(proportion), funs(round(., 3)))
     
     plot.black <- ggplot(stops.black, aes(x = "", y = proportion , fill = daytime))+
       geom_bar(width = 1, stat = "identity", color = "white") + 
       coord_polar("y", start = 0)+ 
       theme_minimal() + labs(title = "Black folks stopped" ) +
       labs(fill = "Time of Day") +
       scale_fill_manual(values = c("gold1","dodgerblue4"),
                         labels = c("Day", "Night"))
     
     stops.hispanic <- filter(cadataupdated, city == select_city, subject_race == "hispanic") %>% 
       group_by(daytime) %>% 
       tally() %>%
       mutate(proportion = n / sum(n)  ) %>%
       mutate_at(vars(proportion), funs(round(., 3)))
     
     plot.hispanic <- ggplot(stops.hispanic, aes(x = "", y = proportion , fill = daytime))+
       geom_bar(width = 1, stat = "identity", color = "white") + 
       coord_polar("y", start = 0)+ 
       theme_minimal() + labs(title = "Hispanic folks stopped" ) +
       labs(fill = "Time of Day") +
       scale_fill_manual(values = c("gold1","dodgerblue4"),
                         labels = c("Day", "Night"))
     plot_grid(plot.white, plot.black, plot.hispanic)
   })
  }


# Run the app ----
shinyApp(ui = ui, server = server)
