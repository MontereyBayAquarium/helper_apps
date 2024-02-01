

library(shiny)
library(dplyr)
library(DT)
library(googledrive)
library(googlesheets4)

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "your-app-folder-name/.secrets"
)

# Authenticate tidyverse on Google
# googledrive::drive_auth()
# googlesheets4::gs4_auth()

# Read the sheet
sheet_id <- "1dlFFdawLIUJdLsjuqD_RfLrYGtaTGmpsl-fINAbM52w"

# Define UI
# Define UI
ui <- fluidPage(
  tags$head(
    tags$script("
      $(document).on('wheel', 'input[type=number]', function (e) {
        $(this).blur();
      });
    ")
  ),
  titlePanel("Data Entry App"),
  sidebarLayout(
    sidebarPanel(
      textInput("data_enterer", "Name of Data Enterer"),
      dateInput("date_collected", "Date Collected", value = NULL),
      dateInput("date_processed", "Date Processed", value = NULL),
      textInput("site_number", "Site Number", value = NULL, placeholder = "Enter site number"),
      br(),
      textInput("sample_number", "Sample Number"),
      textInput("observer", "Observer"),
      selectInput("species", "Species", choices = c("purple_urchin", "red_urchin")),
      numericInput("test_height_mm", "Test Height (mm)", value = NULL, step = 0.001),
      numericInput("test_diameter_mm", "Test Diameter (mm)", value = NULL, step = 0.001),
      numericInput("animal_wet_mass_g", "Animal Wet Mass (g)", value = NULL, step = 0.001),
      numericInput("gonad_wet_mass_g", "Gonad Wet Mass (g)", value = NULL, step = 0.001),
      textInput("notes", "Notes"),
      actionButton("submit_sample", "Submit Sample"),
      actionButton("clear_fields", "New Site", class = "btn-primary"), # New Site button
      actionButton("push_to_sheets", "Push to server", class = "btn-primary"),
      br()
    ),
    mainPanel(
      DTOutput("data_table")
    )
  )
)

server <- function(input, output, session) {
  # Initialize data frame to store data
  data <- reactiveVal(data.frame(
    Data_Enterer = character(),
    Date_Collected = character(),
    Date_Processed = character(),
    Site_Number = character(),
    Sample_Number = character(),
    Observer = character(),
    Species = character(),
    Test_Height_mm = numeric(),
    Test_Diameter_mm = numeric(),
    Animal_Wet_Mass_g = numeric(),
    Gonad_Wet_Mass_g = numeric(),
    Notes = character(),
    Date_Entered = character(),
    stringsAsFactors = FALSE
  ))
  
  # Reactive function to update data frame when sample is submitted
  observeEvent(input$submit_sample, {
    isolate({
      new_row <- data.frame(
        Data_Enterer = input$data_enterer,
        Date_Collected = rep(as.character(input$date_collected), length(input$sample_number)),
        Date_Processed = rep(as.character(input$date_processed), length(input$sample_number)),
        Site_Number = toupper(input$site_number), # Convert to uppercase
        Sample_Number = input$sample_number,
        Observer = input$observer,
        Species = input$species,
        Test_Height_mm = input$test_height_mm,
        Test_Diameter_mm = input$test_diameter_mm,
        Animal_Wet_Mass_g = input$animal_wet_mass_g,
        Gonad_Wet_Mass_g = input$gonad_wet_mass_g,
        Notes = input$notes,
        Date_Entered = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      )
      data(bind_rows(data(), new_row))
      
      # Clear input fields (except for metadata)
      updateTextInput(session, "sample_number", value = "")
      updateTextInput(session, "observer", value = "")
      updateTextInput(session, "species", value = "")
      updateNumericInput(session, "test_height_mm", value = NA)
      updateNumericInput(session, "test_diameter_mm", value = NA)
      updateNumericInput(session, "animal_wet_mass_g", value = NA)
      updateNumericInput(session, "gonad_wet_mass_g", value = NA)
      updateTextInput(session, "notes", value = "")
    })
  })
  
  # Reactive function to clear all fields
  observeEvent(input$clear_fields, {
    updateDateInput(session, "date_collected", value = NULL)
    updateDateInput(session, "date_processed", value = NULL)
    updateTextInput(session, "site_number", value = "")
    updateTextInput(session, "sample_number", value = "")
    updateTextInput(session, "observer", value = "")
    updateTextInput(session, "species", value = "")
    updateNumericInput(session, "test_height_mm", value = NA)
    updateNumericInput(session, "test_diameter_mm", value = NA)
    updateNumericInput(session, "animal_wet_mass_g", value = NA)
    updateNumericInput(session, "gonad_wet_mass_g", value = NA)
    updateTextInput(session, "notes", value = "")
  })
  
  # Function to push data to Google Sheets
  push_to_sheets <- function() {
    # Check if data is a data frame
    if (!is.data.frame(data())) {
      showModal(modalDialog(
        title = "Error",
        "Data is not in the correct format.",
        footer = tagList(
          modalButton("Dismiss")
        )
      ))
      return()
    }
    
    # Push data to Google Sheets
    googlesheets4::sheet_append(sheet_id, data())
  }
  
  # Event handler for pushing data to Google Sheets
  observeEvent(input$push_to_sheets, {
    showModal(modalDialog(
      title = "Confirmation",
      "Are you sure? This action will push data to the server.",
      footer = tagList(
        actionButton("yes_push", "Yes", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  })
  
  # Event handler for "Yes" button in the confirmation dialog
  observeEvent(input$yes_push, {
    removeModal()
    push_to_sheets()
    showModal(modalDialog(
      title = "Data Pushed to Google Sheets",
      "Data has been successfully pushed to Google Sheets."
    ))
    # Clear all entries after pushing data
    updateDateInput(session, "date_collected", value = NULL)
    updateDateInput(session, "date_processed", value = NULL)
    updateTextInput(session, "site_number", value = "")
    updateTextInput(session, "sample_number", value = "")
    updateTextInput(session, "observer", value = "")
    updateTextInput(session, "species", value = "")
    updateNumericInput(session, "test_height_mm", value = NA)
    updateNumericInput(session, "test_diameter_mm", value = NA)
    updateNumericInput(session, "animal_wet_mass_g", value = NA)
    updateNumericInput(session, "gonad_wet_mass_g", value = NA)
    updateTextInput(session, "notes", value = "")
  })
  
  # Render data table
  output$data_table <- renderDT({
    datatable(data(),
              rownames = FALSE,
              editable = TRUE,
              options = list(
                columnDefs = list(
                  list(className = "dt-center", targets = "_all")
                )
              ),
              callback = JS(
                "table.on('click', 'button.edit', function () {
                    var data = table.row($(this).parents('tr')).data();
                    $('#data_enterer').val(data[0]);
                    $('#date_collected').val(data[1]);
                    $('#date_processed').val(data[2]);
                    $('#site_number').val(data[3]);
                    $('#sample_number').val(data[4]);
                    $('#observer').val(data[5]);
                    $('#species').val(data[6]);
                    $('#test_height_mm').val(data[7]);
                    $('#test_diameter_mm').val(data[8]);
                    $('#animal_wet_mass_g').val(data[9]);
                    $('#gonad_wet_mass_g').val(data[10]);
                    $('#notes').val(data[11]);
                });"
              )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
