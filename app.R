library(shiny)
library(ggplot2)

# Simple letter-to-frequency mapping (A-Z)
letter_freqs <- c(
  A = 440.0, B = 493.9, C = 523.3, D = 587.3, E = 659.3, F = 698.5, G = 783.99,
  H = 880.0, I = 987.8, J = 1046.5, K = 1174.7, L = 1318.5, M = 1396.9,
  N = 1568.0, O = 1760.0, P = 1975.5, Q = 2093.0, R = 2349.3, S = 2637.0,
  T = 2793.8, U = 3136.0, V = 3520.0, W = 3951.1, X = 4186.0, Y = 4698.6, Z = 5274.0
)

# Generate a waveform from text
generate_waveform <- function(text, duration = 0.01, sample_rate = 44100) {
  text <- toupper(gsub("[^A-Z]", "", text))  # Keep only letters A-Z
  t <- seq(0, duration, by = 1 / sample_rate)
  
  if (nchar(text) == 0) return(rep(0, length(t)))  # Empty input fallback
  
  waveform <- numeric(length(t))
  for (char in strsplit(text, "")[[1]]) {
    freq <- letter_freqs[[char]]
    if (!is.null(freq)) {
      waveform <- waveform + sin(2 * pi * freq * t)
    }
  }
  waveform <- waveform / max(abs(waveform))  # Normalize
  data.frame(time = t, amplitude = waveform)
}

# UI
ui <- fluidPage(
  titlePanel("Sound Wave Visualizer"),
  sidebarLayout(
    sidebarPanel(
      textInput("inputText", "Enter a word or phrase:", "Hello")
    ),
    mainPanel(
      plotOutput("wavePlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$wavePlot <- renderPlot({
    waveform <- generate_waveform(input$inputText)
    ggplot(waveform, aes(x = time, y = amplitude)) +
      geom_line(color = "steelblue") +
      labs(title = paste("Waveform for:", input$inputText),
           x = "Time (s)", y = "Amplitude") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
