library(shiny)
library(tidyverse)
library(tidyr)

# For git-backed deployment to Connet, first rsconnect::writemanifest() in the
# project folder and commit to git.  Then whenever any updates to packages are
# made, rewrite the manifest, re-commit to git.

# rsconnect::writeManifest()

ui <- fluidPage(
    
    titlePanel("NYC Squirrel Actions"), 
    
    sidebarLayout(
        sidebarPanel(radioButtons(inputId = "squirrelChoice", 
                                  label = "Squirrel Primary Color",
                                  choices = c("Gray" = "Gray", 
                                              "Cinnamon" = "Cinnamon", 
                                              "Black" = "Black"))),
        mainPanel(
          plotOutput("squirrelPlot"),
          p("\"Actions\" are defined as follows:"),
          p("chatter = kuks, quaas, and/or moans"),
          p("motions = tail flags, tail twitches, approaches, and/or runs from")
                  )
    )
  
)

server <- function(input, output, session) {
    output$squirrelPlot <- renderPlot({
        
        nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv") %>% 
            select(primary_fur_color, highlight_fur_color, 
                   combination_of_primary_and_highlight_color,
                   kuks, quaas, moans,tail_flags, tail_twitches, 
                   approaches, indifferent, runs_from, other_interactions) %>% 
            filter(!is.na(primary_fur_color))
        
        summaryTable <- nyc_squirrels %>% 
            group_by(primary_fur_color) %>% 
            summarise(count=n(), chatter=sum(kuks, moans, quaas), 
                      motions = sum(tail_flags, tail_twitches, approaches, runs_from), 
                      indifferent = sum(indifferent)) %>% 
            mutate_at(c("chatter", "motions", "indifferent"), ~(./count)) %>% 
            select(-count)
        
        summaryLong <- summaryTable %>% pivot_longer(-primary_fur_color, 
                                                     names_to="action", values_to="count") %>% 
            filter(primary_fur_color == input$squirrelChoice)
        
        ggplot(summaryLong, aes(x=action, y=count)) + 
            geom_col(fill="chartreuse4", colour="black") +
            labs(title=paste(input$squirrelChoice,"Squirrel Observed Actions"), 
                 x= "Action", y="Fraction of Observations") + ylim(c(0,1))
        
    })
  
}

shinyApp(ui, server)