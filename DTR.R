library(shiny)

# Define the directory for storing records
save_directory <- "C:\\Users\\CLSD_User\\Documents\\DTR\\DTR Records"
csv_file <- file.path(save_directory, "DTR_records.csv")

if (!dir.exists(save_directory)) {
  dir.create(save_directory, recursive = TRUE)
}

if (!file.exists(csv_file)) {
  empty_df <- data.frame(
    ID = integer(),
    Employee = character(),
    Date = character(),
    AM_ClockIn = character(),
    AM_ClockOut = character(),
    PM_ClockIn = character(),
    PM_ClockOut = character(),
    Total_Hours = numeric(),
    stringsAsFactors = FALSE
  )
  write.csv(empty_df, csv_file, row.names = FALSE, quote = FALSE)
}

ui <- fluidPage(
  tags$head(
    tags$title("Daily-Time Record SYSTEM"),
    tags$style(HTML("
      body { background-color: #f8f9fa; }
      .container { max-width: 600px; margin-top: 50px; }
      h2 { color: #007bff; text-align: center; }
      .btn-primary, .btn-success, .btn-danger { width: 100%; margin-top: 10px; }
      .btn-warning { width: 100%; margin-top: 10px; }
      .table { margin-top: 20px; }
    "))
  ),
  
  div(class = "container",
      h2("Employee Daily-Time Recorder"),
      textInput("employee_name", "Enter Employee Name", ""),
      actionButton("clock_in_btn", "Clock In", class = "btn btn-success"),
      actionButton("clock_out_btn", "Clock Out", class = "btn btn-danger"),
      tableOutput("record_table"),
      downloadButton("download_btn", "Download CSV", class = "btn btn-primary"),
      br(),
      actionButton("shutdown_btn", "Shutdown Computer", class = "btn btn-warning") 
  )
)

server <- function(input, output, session) {
  
  records <- reactiveVal({
    if (file.exists(csv_file) && file.info(csv_file)$size > 0) {
      read.csv(csv_file, stringsAsFactors = FALSE)
    } else {
      data.frame(
        ID = integer(),
        Employee = character(),
        Date = character(),
        AM_ClockIn = character(),
        AM_ClockOut = character(),
        PM_ClockIn = character(),
        PM_ClockOut = character(),
        Total_Hours = numeric(),
        stringsAsFactors = FALSE
      )
    }
  })
  
  append_record <- function(employee_name, action) {
    if (employee_name != "") {
      current_time <- format(Sys.time(), "%I:%M:%S %p")  
      current_date <- format(Sys.Date(), "%m/%d/%Y")
      
      df <- records()
      existing_row <- which(df$Employee == employee_name & df$Date == current_date)
  
      if (length(existing_row) == 0) {
        new_record <- data.frame(
          ID = ifelse(nrow(df) > 0, max(df$ID, na.rm = TRUE) + 1, 1), 
          Employee = employee_name, 
          Date = current_date,
          AM_ClockIn = ifelse(format(Sys.time(), "%p") == "AM", current_time, NA), 
          AM_ClockOut = NA, 
          PM_ClockIn = ifelse(format(Sys.time(), "%p") == "PM", current_time, NA), 
          PM_ClockOut = NA,
          Total_Hours = 0,
          stringsAsFactors = FALSE
        )
        df <- rbind(df, new_record)
      } else {
        df[existing_row, c("AM_ClockIn", "AM_ClockOut", "PM_ClockIn", "PM_ClockOut")][is.na(df[existing_row, c("AM_ClockIn", "AM_ClockOut", "PM_ClockIn", "PM_ClockOut")])] <- ""
  
        if (action == "Clock In") {
          if (format(Sys.time(), "%p") == "AM" && df$AM_ClockIn[existing_row] == "") {
            df$AM_ClockIn[existing_row] <- current_time
          } else if (format(Sys.time(), "%p") == "PM" && df$PM_ClockIn[existing_row] == "") {
            df$PM_ClockIn[existing_row] <- current_time
          }
        } else if (action == "Clock Out") {
          if (format(Sys.time(), "%p") == "AM" && df$AM_ClockOut[existing_row] == "") {
            df$AM_ClockOut[existing_row] <- current_time
          } else if (format(Sys.time(), "%p") == "PM" && df$PM_ClockOut[existing_row] == "") {
            df$PM_ClockOut[existing_row] <- current_time
          }
        }
      }
      
      records(df)
      write.csv(df, csv_file, row.names = FALSE, quote = FALSE)
    }
  }
  
  observeEvent(input$clock_in_btn, { append_record(input$employee_name, "Clock In") })
  observeEvent(input$clock_out_btn, { append_record(input$employee_name, "Clock Out") })
  
  output$record_table <- renderTable({ records() })
  
  output$download_btn <- downloadHandler(
    filename = function() { paste("DTR_records", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(records(), file, row.names = FALSE, quote = FALSE) }
  )
  
  # Manual Shutdown Button
  observeEvent(input$shutdown_btn, {
    system("shutdown /s /t 5", wait = FALSE)
  })
}

shinyApp(ui, server, options = list(port = 8100, launch.browser = TRUE, debug = TRUE, host = "192.168.74.106", quiet = TRUE))
