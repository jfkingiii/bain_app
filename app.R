library(shiny)
library(tibble)

max_seconds <- 60

metadata <- readr::read_csv("bain_metadata.csv")
#metadata <- subset(metadata, set == 1)
rnd <- order(runif(nrow(metadata)))
metadata <- metadata[order(rnd),]
session_data <-
    tibble(
        diagram_id = metadata$diagram_id,
        seconds = NA,
        conclusion = ""
    )

onStop(function() {
    # u <<- session_data[!is.na(session_data$seconds), ]
    # readr::write_csv(u, "session_data.csv")
    # # View(u)
    # cat("You solved ", sum(u$seconds < 30), " tactics problems.\n")
    # cat(round(mean(u$seconds <= 15) * 100, 1), "% within 15 seconds")
})

ui <-
    fluidPage(shinyjs::useShinyjs(),
              titlePanel(h1("Bain Tactics")),
              sidebarLayout(
                  sidebarPanel(
                      actionButton("gotit_button", "Got it!"),
                      actionButton("pause_button", "Pause"),
                      actionButton("restart_button", "Continue"),
                      actionButton("exit_button", "Exit app")
                  ),
                  mainPanel(
                      imageOutput("img", inline = TRUE),
                      h2(textOutput("txt")),
                      h2(textOutput("txt2")),
                      h2(textOutput("currentTime")),
                      h2(span(textOutput("txt3"), style="color:red"))
                  )    
                  
              ))

server <- function(input, output, session) {
    cnt <- reactiveVal(1)
    pause <- FALSE
    global_seconds_passed <- 0
    shinyjs::disable("restart_button")
    start_time <- reactiveVal(as.integer(Sys.time()))
    msg <- reactiveVal("")

    output$img <- renderImage({
        img_path <- paste0("images/", metadata$filename[cnt()])
        list(src = img_path,
             width = 500,
             height = 500)
    }, deleteFile = FALSE)
    output$txt <- renderText({
        ifelse(metadata$to_move[cnt()] == "W", "White to move", "Black to move")
    })
    output$txt2 <- renderText({
      c("Problem ",
        as.character(cnt()),
        " of ",
         as.character(nrow(metadata)))
    })
    output$txt3 <- renderText({
      msg()
    })
    
    output$currentTime <- renderText({
        if (!pause)
            invalidateLater(100, session)
        seconds_passed <- as.integer(Sys.time()) - start_time()
        global_seconds_passed <<- seconds_passed
        if (seconds_passed >= max_seconds & !pause) {
            new_msg <- "Look up the answer, then press Continue"
            msg(new_msg)
            pause <<- TRUE
            shinyjs::disable("gotit_button")
            shinyjs::disable("pause_button")
            shinyjs::enable("restart_button")
         }
        seconds_passed
    })
    
    observeEvent(input$gotit_button, {
        session_data$seconds[cnt()] <<-
            as.integer(Sys.time()) - start_time()
        session_data$conclusion[cnt()] <<- "solved"
        if (cnt() == nrow(metadata))
            stopApp()
        new_cnt <- cnt() + 1
        cnt(new_cnt)
        new_start_time <- as.integer(Sys.time())
        start_time(new_start_time)
    })
    
    observeEvent(input$pause_button, {
        pause <<- TRUE
        shinyjs::disable("gotit_button")
        shinyjs::disable("pause_button")
        shinyjs::enable("restart_button")
    })
    
    observeEvent(input$restart_button, {
        new_msg <- ""
        msg(new_msg)
        pause <<- FALSE
        new_start_time <-
            as.integer(Sys.time()) - global_seconds_passed
        start_time(new_start_time)
        shinyjs::disable("restart_button")
        shinyjs::enable("gotit_button")
        shinyjs::enable("pause_button")
        session_data$seconds[cnt()] <<- global_seconds_passed
        session_data$conclusion[cnt()] <<- "timeout"
        if (global_seconds_passed >= max_seconds){
        new_cnt <- cnt() + 1
        cnt(new_cnt)
        new_start_time <- as.integer(Sys.time())
        start_time(new_start_time)}
    })
    
    observeEvent(input$exit_button, {
      stopApp()
    })
}

shinyApp(ui = ui, server = server)
