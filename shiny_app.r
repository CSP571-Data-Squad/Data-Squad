library('shiny')
load(file = 'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\model.rda')
library(shinyWidgets)
dataframe_x <- data.frame(poverty_gap = numeric(),public_health_exp = numeric(),malnutrition_death_rates = numeric(),Infant_mortality_rate = numeric(),GDP_per_capita = numeric(),annual_health_care_per_capita = numeric())
ui <- fluidPage(
  titlePanel(h1("Homicide Rate", align = "center")),
  tags$h1(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=trirong');
      
      h1 {
        font-family: 'trirong';
        font-size:50px;
        font-style: bold;
        font-weight: 800;
        line-height: 1.5;
        color: black;
      }
    "))),
  setBackgroundImage(
    src = "https://shehabnews.com/thumb/w877/uploads//images/db0af3972b11a550f2ee97fe1abacf8d.jpg"
  ),
  
  
  fluidRow(
    column(4,align = "center",
           tags$style("#Poverty Gap {font-size:50px;height:50px;}"),
           numericInput(inputId = "poverty_g",
                        label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: black;",
                                    "Poverty Gap"),
                        value = ""
           )),
    column(4,align = "center",
           numericInput(inputId = "public_health_e",
                        label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: black;",
                                    "Public Health Expenditure"),
                        value = "")),
    column(4,align = "center",
           numericInput(inputId = "malnutrition_death_rates",
                        label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: black;",
                                    "Malnutrition Death Rate"),
                        value = ""))),
  br(),
  br(),
  br(),
  br(),
  fluidRow(
    column(4,align = "center",
           numericInput(inputId = "Infant_mortality_rates",
                        label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: black;",
                                    "Infant Mortality Rate"),
                        value = "")),
    column(4,align = "center",
           numericInput(inputId = "GDP_per_capitas",
                        label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: black;",
                                    "GDP per capita"),
                        value = "")),
    column(4,align = "center",
           numericInput(inputId = "annual_health_care_per_capitas",
                        label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: black;",
                                    "Annual Health Care per Capita"),
                        value = ""))),
  br(),
  br(),
  
  fluidRow(
    column(6, align="center", offset = 3,
           actionButton("do","Predict"))),
  br(),
  br(),
  br(),
  tags$h3(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=trirong');
      
      h3 {
        font-family: 'trirong';
        font-size:30px;
        font-style: bold;
        font-weight: 500;
        line-height: 1.5;
        color: black;
      }"))),
  h3(textOutput("Pred"),align = "center"),
)
server <- function(input, output){
  values <- reactiveValues()
  values$df <- dataframe_x
  new <-  observe({
    if(input$do>0){
      newline <- isolate(c(input$poverty_g,input$public_health_e,input$malnutrition_death_rates,input$Infant_mortality_rates,input$GDP_per_capitas,input$annual_health_care_per_capitas))
      isolate(values$df[1,] <- newline)
    }
  })
  
  output$Pred <-  renderText({
    paste('Predicted Value = ', predict(model_final2,values$df), 'value/1 million people in a year.')
  })
}
shinyApp(ui = ui, server = server)