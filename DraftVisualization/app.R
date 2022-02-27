library(shiny)
library(tidyverse)

data = read.csv("https://raw.githubusercontent.com/bbwieland/NBADraft/main/draft-data-20-years.csv")

schools = unique(data$College) %>% sort()
schools = schools[-1]

comp_plot = function(dataframe,xvar,yvar){
    ggplot(dataframe,aes_string(x = xvar,y = yvar)) + geom_col(fill = "lightgrey",color = "black",alpha = 0.5) + theme_minimal()
}

ui <- fluidPage(
    h1("NBA Draft Picks Since 1990"),
    tabsetPanel(type = "tabs",
                tabPanel("Single-School Stats",sidebarLayout(
                    sidebarPanel(
                        selectInput("school","Select a college:",choices = schools,selected = "Virginia")
                    ),
                    
                    mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Players",tableOutput("data")),
                                    tabPanel("Summary",tableOutput("totals"))
                        )
                    )
                )),
                tabPanel("School Comparison",sidebarLayout(
                    sidebarPanel(
                        selectInput("schoolcomp1","Select a college:",choices = schools,selected = "Kansas"),
                        selectInput("schoolcomp2","Select a college:",choices = schools,selected = "Duke"),
                        selectInput("comparevariable","Select a stat to compare:",
                                    choices = c("Players","Points","Rebounds","Assists","Games","VORP","WinShares"),
                                    selected = "WinShares")
                    ),
                    mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Visualize",plotOutput("comparisonplot")),
                                    tabPanel("Table",fluidRow(
                                        column(4,tableOutput("school1"))
                                    ),fluidRow(column(4,tableOutput("school2"))))
                    )
                )),)))



# Define server logic required to draw a histogram
server <- function(input, output) {
    schooldata = reactive(data %>% 
                              filter(College == input$school) %>%
                              select(Player,Tm,College,DraftYear,PPG,RPG,APG,BPM,VORP,WS) %>%
                              arrange(-WS))
    
    schooltotals = reactive(data %>% 
                                filter(College == input$school) %>%
                                summarise(Players = n(),
                                          Games = sum(G,na.rm = T),
                                          Points = sum(TOTPTS,na.rm = T),
                                          Rebounds = sum(TOTTRB,na.rm = T),
                                          Assists = sum(TOTAST,na.rm = T),
                                          VORP = sum(VORP,na.rm = T),
                                          WinShares = sum(WS,na.rm = T)))
    
    school1 = reactive({data %>% 
                           filter(College == input$schoolcomp1) %>%
                           summarise(Players = n(),
                                     Games = sum(G,na.rm = T),
                                     Points = sum(TOTPTS,na.rm = T),
                                     Rebounds = sum(TOTTRB,na.rm = T),
                                     Assists = sum(TOTAST,na.rm = T),
                                     VORP = sum(VORP,na.rm = T),
                                     WinShares = sum(WS,na.rm = T)) %>%
                           mutate(School = input$schoolcomp1)})
    
    school2 = reactive({data %>% 
                           filter(College == input$schoolcomp2) %>%
                           summarise(Players = n(),
                                     Games = sum(G,na.rm = T),
                                     Points = sum(TOTPTS,na.rm = T),
                                     Rebounds = sum(TOTTRB,na.rm = T),
                                     Assists = sum(TOTAST,na.rm = T),
                                     VORP = sum(VORP,na.rm = T),
                                     WinShares = sum(WS,na.rm = T)) %>%
                           mutate(School = input$schoolcomp2)})
    
    schoolcomparison = reactive({rbind(school1(),school2())})
    
    output$school1 = renderTable(school1(),
                                 striped = T,
                                 align = "c",
                                 digits = 1)
    
    
    output$school2 = renderTable(school2(),
                                 striped = T,
                                 align = "c",
                                 digits = 1)
    
    compInputX = reactive({"School"})
    compInputY = reactive(input$comparevariable)
        
    output$comparisonplot = renderPlot({comp_plot(schoolcomparison(),compInputX(),compInputY()) +
            labs(x = "",
                 y = input$comparevariable,
                 title = paste0("Comparing ",input$schoolcomp1," to ",input$schoolcomp2," by NBA ",input$comparevariable))+
            coord_flip()})
                            
    
    output$data = renderTable(schooldata(),
                              striped = T,
                              align = "c",
                              digits = 1)
    
    output$totals = renderTable(schooltotals(),
                                align = "c",
                                digits = 1)
    
}

# Run the application 
shinyApp(ui = ui, server = server)

