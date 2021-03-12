library(shiny)
library(distr)
library(tidyverse)

# load required data
colorscience_data <- data(package = "colorscience")[[3]][, 3]
data(list = colorscience_data, package = "colorscience")

# Define UI
ui <- fluidPage(

  # Application title
  titlePanel("absorbance2colour"),

  # Layout based on rows and columns
  fluidRow(
    column(
      6,
      # basic input defining the light source and number of absorbance peaks
      fluidRow(
        align = "center",
        column(
          6,
          selectInput("light",
            "Incident light",
            choices = list(
              "Daylight" = "D65", "Incandescent light" = "A",
              "Analytical light" = "E"
            ), selected = 1
          )
        ),
        column(
          6,
          sliderInput("peak_n", "Number of absorbance maxima",
            min = 1, max = 4, value = 1
          )
        )
      ),
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
            min = 20, max = 100, value = 40
          )
        ),
        column(
          4,
          sliderInput("prob1", "Relative intensity",
            min = 0.1, max = 0.9, value = 0.4
          )
        )
      ),
      # conditional panels for any additional absorbance peaks
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
              min = 20, max = 100, value = 40
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
            sliderInput("sd3", "Width",
              min = 20, max = 100, value = 40
            )
          ),
          column(
            4,
            sliderInput("prob3", "Relative intensity",
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
            sliderInput("sd4", "Width",
              min = 20, max = 100, value = 40
            )
          ),
          column(
            4,
            sliderInput("prob4", "Relative intensity",
              min = 0.1, max = 0.9, value = 0.4
            )
          )
        )
      )
    ),
    # output the incident and absorbance spectra, and the resulting colour 
    column(
      6,
      h3("Calculate the perceived colour of a compound from its approximate absorbance spectrum."),
      plotOutput("lightPlot"),
      plotOutput("absPlot"),
      plotOutput("colourPlot")
    )
  )
)

# Server side calculations
server <- function(input, output) {
  output$lightPlot <- renderPlot({
    # transform incident light spectrum to count data
    inc_light <- as_tibble(illuminants[, c("wlnm", input$light)]) %>%
      rename("intensity" = .data[[input$light]]) %>%
      uncount(intensity)

    # plot incident light distribution
    ggplot(inc_light) +
      geom_density(aes(x = wlnm)) +
      xlim(380, 780) +
      labs(title = "Spectrum of the incident light")
  })

  output$absPlot <- renderPlot({
    # scale distribution probabilities
    probs <- c(input$prob1, 
               if (input$peak_n > 1) input$prob2,
               if (input$peak_n > 2) input$prob3,
               if (input$peak_n > 3) input$prob4
    )
    probs <- probs / sum(probs)

    # create pseudo absorbance spectrum as distribution
    if (input$peak_n == 1) {
      spec_dist <- UnivarMixingDistribution(
        Norm(mean = input$pos1, sd = input$sd1)
      )}
    if (input$peak_n == 2) {
      spec_dist <- UnivarMixingDistribution(
        Norm(mean = input$pos1, sd = input$sd1),
        Norm(mean = input$pos2, sd = input$sd2),
        mixCoeff = probs
      )}
    if (input$peak_n == 3) {
      spec_dist <- UnivarMixingDistribution(
        Norm(mean = input$pos1, sd = input$sd1),
        Norm(mean = input$pos2, sd = input$sd2),
        Norm(mean = input$pos3, sd = input$sd3),
        mixCoeff = probs
      )}
    if (input$peak_n == 4) {
      spec_dist <- UnivarMixingDistribution(
        Norm(mean = input$pos1, sd = input$sd1),
        Norm(mean = input$pos2, sd = input$sd2),
        Norm(mean = input$pos3, sd = input$sd3),
        Norm(mean = input$pos4, sd = input$sd4),
        mixCoeff = probs
      )}

    # create sampler
    spec_dist_sampler <- r(spec_dist)

    # sample from distribution
    spec_dist_sample <- as_tibble(spec_dist_sampler(1e6))

    # plot compound absorbance spectrum (distribution)
    ggplot(spec_dist_sample) +
      geom_density(aes(x = value)) +
      xlim(380, 780) +
      labs(title = "Spectrum of the absorbed light")
  })
  
  output$colourPlot <- renderPlot({
    # repeat calculations, because I haven't figured out how to carry over ojects
    # scale distribution probabilities
    probs <- c(input$prob1, 
               if (input$peak_n > 1) input$prob2,
               if (input$peak_n > 2) input$prob3,
               if (input$peak_n > 3) input$prob4
    )
    probs <- probs / sum(probs)
    
    # create pseudo absorbance spectrum as distribution
    if (input$peak_n == 1) {
      spec_dist <- UnivarMixingDistribution(
        Norm(mean = input$pos1, sd = input$sd1)
      )}
    if (input$peak_n == 2) {
      spec_dist <- UnivarMixingDistribution(
        Norm(mean = input$pos1, sd = input$sd1),
        Norm(mean = input$pos2, sd = input$sd2),
        mixCoeff = probs
      )}
    if (input$peak_n == 3) {
      spec_dist <- UnivarMixingDistribution(
        Norm(mean = input$pos1, sd = input$sd1),
        Norm(mean = input$pos2, sd = input$sd2),
        Norm(mean = input$pos3, sd = input$sd3),
        mixCoeff = probs
      )}
    if (input$peak_n == 4) {
      spec_dist <- UnivarMixingDistribution(
        Norm(mean = input$pos1, sd = input$sd1),
        Norm(mean = input$pos2, sd = input$sd2),
        Norm(mean = input$pos3, sd = input$sd3),
        Norm(mean = input$pos4, sd = input$sd4),
        mixCoeff = probs
      )}
    
    # create sampler
    spec_dist_sampler <- r(spec_dist)
    
    # sample from distribution
    spec_dist_sample <- as_tibble(spec_dist_sampler(1e6))
    
    # transfrom absorbance from a distribution into a spectrum and then into transmittance
    spec <- spec_dist_sample %>%
      filter(value >= 377.5 & value <= 782.5) %>%
      mutate(
        bin = cut_width(value, width = 5, boundary = 377.5, labels = F), # cut into 5 nm bins
        bin = 380 + ((bin - 1) * 5) # name the bins by the central wavelength
      ) %>%
      group_by(bin) %>%
      tally() %>%
      mutate(
        n = (n / max(n)) * 2, # range normalise to pseudo absorbance
        n = (10^-n) * 100 # transform into transmittance
      )
    
    # transform transmittance into a matrix
    spec_mat <- data.matrix(spec)
    
    # transform transmittance spectrum into XYZ colour coordinates and then RGB values
    XYZ_col <- colorscience::spectra2XYZ(spec_mat, illuminantIn = illuminants[, c("wlnm", input$light)])
    RGB_col <- colorscience::XYZ2RGB(XYZ_col, illuminant = input$light)
    
    # correct RGB values < 0 and > 1 (unclear to me why they exist)
    RGB_col <- ifelse(RGB_col < 0, 0, ifelse(RGB_col > 1, 1, RGB_col))
    
    # plot the colour of the compound
    ggplot() + 
      theme_void() +
      annotate("point",
               shape = 21,
               size = 100,
               fill = rgb(RGB_col),
               x = 1,
               y = 1) +
      annotate("text",
               label = rgb(RGB_col),
               x = 1,
               y = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
