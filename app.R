library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel(h2("absorbance2colour")),

  # Sidebar with a slider input for number of bins
  fluidRow(
    column(
      6,
      fluidRow(align = "center",
               column(6,
      selectInput("light",
        "Incident light",
        choices = list(
          "Daylight" = "D65", "Incandescent light" = "A",
          "Analytical light" = "E"
        ), selected = 1
      )),
      column(6,
      sliderInput("peak_n", "Number of absorbance maxima",
        min = 1, max = 4, value = 1
      ))),
      fluidRow(column(12, h3("First absorbance peak"))),
      fluidRow(
        column(
          4,
          sliderInput("pos1", "Wavelength [nm]",
            min = 380, max = 780, value = 560
          )
        ),
        column(
          4,
          sliderInput("sd1", "Width",
            min = 1, max = 100, value = 20
          )
        ),
        column(
          4,
          sliderInput("prob1", "Relative intensity",
            min = 0.1, max = 0.9, value = 0.4
          )
        )
      ),
      conditionalPanel(
        condition = "input.peak_n > 1",
        fluidRow(column(12, h3("Second absorbance peak"))),
        fluidRow(
          column(
            4,
            sliderInput("pos2", "Wavelength [nm]",
              min = 380, max = 780, value = 560
            )
          ),
          column(
            4,
            sliderInput("sd2", "Width",
              min = 1, max = 100, value = 20
            )
          ),
          column(
            4,
            sliderInput("prob2", "Relative intensity",
              min = 0.1, max = 0.9, value = 0.4
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.peak_n > 2",
        fluidRow(column(12, h3("Third absorbance peak"))),
        fluidRow(
          column(
            4,
            sliderInput("pos3", "Wavelength [nm]",
                        min = 380, max = 780, value = 560
            )
          ),
          column(
            4,
            sliderInput("sd2", "Width",
                        min = 1, max = 100, value = 20
            )
          ),
          column(
            4,
            sliderInput("prob2", "Relative intensity",
                        min = 0.1, max = 0.9, value = 0.4
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.peak_n > 3",
        fluidRow(column(12, h3("Fourth absorbance peak"))),
        fluidRow(
          column(
            4,
            sliderInput("pos4", "Wavelength [nm]",
                        min = 380, max = 780, value = 560
            )
          ),
          column(
            4,
            sliderInput("sd2", "Width",
                        min = 1, max = 100, value = 20
            )
          ),
          column(
            4,
            sliderInput("prob2", "Relative intensity",
                        min = 0.1, max = 0.9, value = 0.4
            )
          )
        )
      )
    ),
    column(
      6,
      h3("Calculate the perceived colour of a compound from its approximate absorbance spectrum."),
      # plotOutput("lightPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$lightPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
