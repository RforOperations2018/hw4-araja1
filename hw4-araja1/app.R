#
# Name: Ashok Raja
# Project # 1
# Andrew ID: araja1
# Shiny app online link : https://ashokkumarraja.shinyapps.io/hw4-araja1/

# Loading libraries

library(shiny)
library(dplyr)
library(plyr)
library(plotly)
library(tibble)
library(shinydashboard)
library(reshape2)
library(shinythemes)
library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)

ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}
# "<-" not "=" !
id="e03a89dd-134a-4ee8-a2bd-62c40aeebc6f"
neigh <- ckanUniques(id, "INCIDENTNEIGHBORHOOD")
race <- ckanUniques(id, "RACE")
age<- ckanUniques(id, "AGE")


header <- dashboardHeader(title = "Pittsburgh Arrest Records")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot-1", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Plot-2", icon = icon("bar-chart"), tabName = "plot2"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    selectInput("neigh_select",
                "Neighborhood:",
                choices = neigh,
                multiple = FALSE,
                selectize = TRUE,
                selected = 'Manchester'
    ),
    # Selecting the Race
    selectInput("race_select",
                "Race:",
                choices = race,
                multiple = FALSE
                
    ),
    #selecting the Approval Date using a slider
    sliderInput("age_select",
                "Age:",
                min = 10,
                max = 99,
                value = c(10, 99),
                step = 1
    ),
    #selecting the Application Type
    checkboxGroupInput("sex_select",
                       "Sex:",
                       choices = c('M','F'),
                       selected=1
    )
  )
)



body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            # info anf value boxes - note- Applicants os fo the entire data set and is intentionally hnot filtered by selection
            infoBoxOutput("neighborhoods"),
            valueBoxOutput("offences"),
            valueBoxOutput("pk")
          ),
          fluidRow(
            # Rendering the  differnt plots in tabs
           tabBox(title = "Plot1",
                   width = 12,
                   tabPanel("Crimes by Age", plotlyOutput("plot")),
                   tabPanel("Crime Timeline", plotlyOutput("plot2"))

            )
          )
  ),
tabItem("plot2",
        fluidPage(
          # Rendering the  differnt plots in tabs
          tabBox(title = "Plot2",
                 width = 12,
                 tabPanel("Crime Zones", plotlyOutput("plot3"))
          )
        )
), 



 tabItem("table",
          # Rendering the table
          fluidPage(
            box(title = "Crime Data", DT::dataTableOutput("table"), width=300))
  )
)
)

# passing the ui to the dashboard
ui <- dashboardPage(header, sidebar, body)

# Define server logic 
server <- function(input, output, session=session) 
{
  # Caputing the inputs for reactive functions
  swInput <- reactive({
    
    url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%22", id, "%22%20WHERE%20%22INCIDENTNEIGHBORHOOD%22=%20%27",URLencode(input$neigh_select),"%27%20AND%20%22RACE%22%20=%20%27",input$race_select,"%27%20AND%20%22AGE%22%20%3E%27",input$age_select[1],"%27%20AND%20%22AGE%22%20%3C%27",input$age_select[2],"%27")
    crime=ckanSQL(url)
    if(length(input$sex_select)>0){
      crime <- subset(crime, GENDER %in% input$sex_select)  
    }
    return(crime)
  })
 
  
  # Plot for # of crimes by age
  output$plot <- renderPlotly({
    crime=swInput()
    # fill not colour!
    ggplotly(ggplot(data=crime,aes(x=AGE,colour=GENDER))+
               geom_bar()+
               labs(title="Crimes by Age",x="AGE",y="# Crimes",colour="Gender")
    )
  })
  # Plot the upcomgin aptent expiry
  output$plot2 <- renderPlotly({
    crime=swInput()
    ggplotly(ggplot(data=crime,aes(x=as.Date(ARRESTTIME)))+
               # fill not colour! again...
               geom_freqpoly(aes(colour=GENDER))+
               labs(title="OffenceTimeline",x="Timeline",y="# of Offences",colour="Gender")
             
    )
  })
  # Plot the Incident zones and Gender
  output$plot3 <- renderPlotly({
    crime=swInput()
    ggplotly(ggplot(data=crime,aes(x=INCIDENTZONE,colour=GENDER))+
               geom_bar()+ coord_flip()+
               labs(title="Offenses by Incident Zone",x="Zone",y="# of Offenses",colour="Gender")
    )
  })
  # Data table 
  output$table <- DT::renderDataTable(crime <-swInput(), options = list(scrollX = TRUE))

  # Total CCR info box
  output$neighborhoods <- renderInfoBox({
    crime= swInput()
    num <- length(unique(crime$CCR))
    
    infoBox("# CCR", value = num, icon = icon("balance-scale"), color = "purple")
    
  })
  
#   Total offences per selection
  output$offences <- renderValueBox({
    crime=swInput()
    num <- length(unique(crime$OFFENSES))
    valueBox(subtitle = "Unique Offences", value = num, icon = icon("sort-numeric-asc"), color = "green")
  }) 
  
  # Total crimes per selection
  output$pk <- renderValueBox({
    fda=swInput()
    num <- length(fda$PK)
    valueBox(subtitle = "# PK", value = num, icon = icon("sort-numeric-asc"), color = "red")
  }) 
  
}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

