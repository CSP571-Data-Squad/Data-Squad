library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    #renderprint {
                      color: white;
                      background: blue;
                      font-family: 'Times New Roman', Times, serif;
                      font-size: 20px;
                      font-style: italic;
                    }
                    #rendertext {
                      color: blue;
                      background: orange;
                      font-family: 'Times New Roman', Times, serif;
                      font-size: 12px;
                      font-weight: bold;
                    }
                    #rendertext1 {
                      color: red;
                      background: yellow;
                      font-family: Arial, Helvetica, sans-serif;
                      font-size: 19px;
                    }
                    "))
  ),
  
  verbatimTextOutput("renderprint"),
  
  verbatimTextOutput("rendertext"),
  textOutput("rendertext1")
)

server <- function(input, output, session) {
  output$renderprint <- renderPrint({
    print("This is a render Print output")
  })  
  output$rendertext <- renderText({
    "This is a render Text output - with verbatimTextOutput"
  })
  output$rendertext1 <- renderText({
    "This is a render Text output - with textOutput"
  })
}

shinyApp(ui, server)