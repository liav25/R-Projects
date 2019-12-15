#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

names <- read.csv ("NationalNames.csv")
head(names)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Names History Graph Maker"),
   
   # Sidebar with a name input
   
          textInput("inputName",
                     "Name:",placeholder = "Name"
                    ),
         plotOutput("distPlot")
      )
   


server <- function(input, output) {

   output$distPlot <- renderPlot({
       name=input$inputName
       
       sansa = names %>% filter(Name == name) %>% group_by(Name, Gender, Year) %>% summarise(sum=sum(Count))
       
      
       #MJ graph
       p = ggplot(data = sansa) + geom_line(data = sansa %>% filter (Gender=="F") ,aes(x=Year, y=sum), color="hotpink1", size=1.3) + 
         geom_line(data = sansa %>% filter (Gender=="M") ,aes(x=Year, y=sum), color="skyblue", size=1.3) + 
         #geom_line(data = wendy %>% group_by(Name, Year) %>% summarise(sum=sum(sum)),aes(x=Year, y=sum), size=0.9) + 
         scale_x_continuous(breaks = scales::pretty_breaks(n = length(names %>% filter(Name == name)))) + 
         scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
         
         
         theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + guides(color = guide_legend(reverse = TRUE)) +
         ylab("Count") + ggtitle(paste0("The name ", name ," over history"))
     
        p
     
     
     })
}

# Run the application 
shinyApp(ui = ui, server = server)


