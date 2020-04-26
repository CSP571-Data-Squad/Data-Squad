
library('shiny')
load(file = 'C:\\Users\\gouth\\Desktop\\Masters\\Second Semester\\CSP571\\DPA_Project\\DataSets\\Final\\model.rda')
library(shinyWidgets)
dataframe_x <- data.frame(public_health_exp = numeric(),malnutrition_death_rates = numeric(),Infant_mortality_rate = numeric(),GDP_per_capita = numeric(),annual_health_care_per_capita = numeric(),life_exp = numeric(), median_age_2010 = numeric(),fertility = numeric(),gov_exp_per_capita = numeric())
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
        color: white ;
      }
    "))),
  setBackgroundImage(
    src = "https://i.ibb.co/FDws0xB/project-image.jpg"
  ),
  fluidRow(
    column(4,align = "center",
           sliderInput(inputId = "public_health_e",
                       label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: white;",
                                   "Public Health Expenditure"),
                       min = 0, max=100,
                       value = "")),
    column(4,align = "center",
           sliderInput(inputId = "malnutrition_death_rates",
                       label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: white;",
                                   "Malnutrition Death Rate"),
                       min = 0, max=100,
                       value = "")),
    column(4,align = "center",
           sliderInput(inputId = "Infant_mortality_rates",
                       label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: white;",
                                   "Infant Mortality Rate"),
                       min = 0, max=100,
                       value = "")),
  ),
  #
  # br(),
  # br(),
  # br(),
  # br(),
  fluidRow(
    column(4,align = "center",
           sliderInput(inputId = "GDP_per_capitas",
                       label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: white;",
                                   "GDP per capita"),
                       min = 0, max=100,
                       value = "")),
    column(4,align = "center",
           sliderInput(inputId = "annual_health_care_per_capitas",
                       label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: white;",
                                   "Annual Health Care per Capita"),
                       min = 0, max=100,
                       value = "")),
    column(4,align = "center",
           sliderInput(inputId = "life_e",
                       label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: white;",
                                   "Life Expectancy"),
                       min = 0, max=100,
                       value = ""))),
  fluidRow(
    column(4,align = "center",
           sliderInput(inputId = "median_2010",
                       label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: white;",
                                   "Median Age 2010"),
                       min = 0, max=100,
                       value = "")),
    column(4,align = "center",
           sliderInput(inputId = "fert",
                       label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: white;",
                                   "Fertility"),
                       min = 0, max=100,
                       value = "")),
    column(4,align = "center",
           sliderInput(inputId = "gov_exp_per_capitas",
                       label = div(style = "font-family: trirong;
                                              font-size = 20px;
                                              color: white;",
                                   "Government Expenditure per capita"),
                       min = 0, max=100,
                       value = ""))),
  #br(),
  #br(),
  tags$h3(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=trirong');
      h3 {
        font-family: 'trirong';
        font-size:30px;
        font-style: bold;
        font-weight: 500;
        line-height: 1.5;
        color: white;
      }"))),
  h3(textOutput("Pred"),align = "center"),
  br(),
  br(),
  br(),
  fluidRow(
    column(6, align="center", offset = 3,
           actionButton("do","Predict"))),
  br(),
  # br(),
  # br(),
)
server <- function(input, output){
  values <- reactiveValues()
  values$df <- dataframe_x
  new <-  observe({
    if(input$do>0){
      newline <- isolate(c(input$public_health_e,
                           log10(input$malnutrition_death_rates),
                           log10(input$Infant_mortality_rates),
                           log10(input$GDP_per_capitas),
                           log10(input$annual_health_care_per_capitas),
                           input$life_e,
                           input$median_2010,
                           input$fert,
                           log10(input$gov_exp_per_capitas)))
      isolate(values$df[1,] <- newline)
    }
  })
  output$Pred <-  renderText({
    paste('Predicted homicide rate per million people per year is:', round(predict(model_final2,values$df),4),"",sep = '\n')
  })
}
shinyApp(ui = ui, server = server)

