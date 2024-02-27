rm(list=ls())

library(shiny)
library(dplyr)
library(DT)
library(googledrive)
library(googlesheets4)

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "tolkens/.secrets"
)

# Authenticate tidyverse on Google
# googledrive::drive_auth()
# googlesheets4::gs4_auth()

# Read the sheet
sheet_id <- "1Ih-hBXRtfXVMdxw5ibZnXy_dZErdcx5FfeKMSc0HEc4"

ui <- fluidPage(
  tags$head(
    tags$script("
      $(document).on('wheel', 'input[type=number]', function (e) {
        $(this).blur();
      });
    "),
    tags$style(HTML("
  body {
    font-family: 'Open Sans', sans-serif;
    background-color: #f4f5f7;
  }
  #push_to_sheets, .btn-primary {
    margin-top: 20px;
    background-color: #007bff; /* Change this to #ff0000 for red */
    border-color: #007bff; /* Change this to #ff0000 for red */
    border-radius: 20px;
    box-shadow: 0 2px 2px 0 rgba(0,0,0,.1);
  }
  #push_to_sheets:hover, .btn-primary:hover {
    background-color: #0056b3; /* Change this to a darker red for hover */
    border-color: #0056b3; /* Change this to a darker red for hover */
  }
  .form-control {
    border-radius: 20px;
    border: 1px solid #ced4da;
    box-shadow: inset 0 1px 1px rgba(0,0,0,.075);
  }
  .sidebar, .main-panel {
    padding: 20px;
  }
  .sidebar {
    background-color: #ffffff;
    border-radius: 20px;
    box-shadow: 0 4px 6px rgba(0,0,0,.1);
  }
  .main-panel {
    background-color: #ffffff;
    border-radius: 20px;
    box-shadow: 0 4px 6px rgba(0,0,0,.1);
  }
  /* Specific styles for the 'Push to server' button */
  #push_to_sheets {
    background-color: #ff0000; /* Red */
    border-color: #ff0000; /* Red */
  }
  #push_to_sheets:hover {
    background-color: #cc0000; /* Darker shade of red for hover */
    border-color: #cc0000;
  }
"))
  ),
titlePanel("Data entry app for sea urchin dissections", windowTitle = "Sea Urchin Data Entry"),
sidebarLayout(
  sidebarPanel(
    textInput("data_enterer", "Name of Data Enterer"),
    dateInput("date_collected", "Date Collected", value = NULL),
    dateInput("date_fixed", "Date Fixed", value = NULL),
    dateInput("date_processed", "Date Processed", value = NULL),
    textInput("site_number", "Site Number", value = NULL, placeholder = "Enter site number"),
    selectInput("treatment", "Treatment", choices = c("95% EtOH","70% EtOH","40% EtOH", "5 mmol MgCL", "Other (indicate in notes)")),
    br(),
    selectInput("species", "Species", choices = c("purple_urchin", "red_urchin")),
    textInput("sample_number", "Sample Number"),
    numericInput("test_height_mm", "Test Height (mm)", value = NULL, step = 0.001),
    numericInput("test_diameter_mm", "Test Diameter (mm)", value = NULL, step = 0.001),
    numericInput("animal_wet_mass_g", "Animal Wet Mass (g)", value = NULL, step = 0.001),
    numericInput("animal_24hr_mass_g", "Animal 24/hr Mass (g)", value = NULL, step = 0.001),
    numericInput("gonad_wet_mass_g", "Gonad Wet Mass (g)", value = NULL, step = 0.001),
    textInput("notes", "Notes"),
    actionButton("submit_sample", "Submit Sample", class = "btn-primary"),
    actionButton("clear_fields", "New Site", class = "btn-primary"), 
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
    Date_Fixed = character(),
    Date_Processed = character(),
    Site_Number = character(),
    Treatment = character(),
    Species = character(),
    Sample_Number = character(),
    Test_Height_mm = numeric(),
    Test_Diameter_mm = numeric(),
    Animal_Wet_Mass_g = numeric(),
    Animal_24hr_Mass_g = numeric(),
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
        Date_Fixed = rep(as.character(input$date_fixed), length(input$sample_number)),
        Date_Processed = rep(as.character(input$date_processed), length(input$sample_number)),
        Site_Number = toupper(input$site_number), # Convert to uppercase
        Treatment = input$treatment,
        Species = input$species,
        Sample_Number = input$sample_number,
        Test_Height_mm = input$test_height_mm,
        Test_Diameter_mm = input$test_diameter_mm,
        Animal_Wet_Mass_g = input$animal_wet_mass_g,
        Animal_24hr_Mass_g = input$animal_24hr_mass_g,
        Gonad_Wet_Mass_g = input$gonad_wet_mass_g,
        Notes = input$notes,
        Date_Entered = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      )
      data(bind_rows(data(), new_row))
      
      # Clear input fields (except for metadata)
      updateTextInput(session, "species", value = "")
      updateTextInput(session, "sample_number", value = "")
      updateNumericInput(session, "test_height_mm", value = NA)
      updateNumericInput(session, "test_diameter_mm", value = NA)
      updateNumericInput(session, "animal_wet_mass_g", value = NA)
      updateNumericInput(session, "animal_24hr_mass_g", value = NA)
      updateNumericInput(session, "gonad_wet_mass_g", value = NA)
      updateTextInput(session, "notes", value = "")
    })
  })
  
  # Reactive function to clear all fields
  observeEvent(input$clear_fields, {
    updateDateInput(session, "date_collected", value = NULL)
    updateDateInput(session, "date_fixed", value = NULL)
    updateDateInput(session, "date_processed", value = NULL)
    updateTextInput(session, "site_number", value = "")
    updateTextInput(session, "treatment", value = "")
    updateTextInput(session, "species", value = "")
    updateTextInput(session, "sample_number", value = "")
    updateNumericInput(session, "test_height_mm", value = NA)
    updateNumericInput(session, "test_diameter_mm", value = NA)
    updateNumericInput(session, "animal_wet_mass_g", value = NA)
    updateNumericInput(session, "animal_24hr_mass_g", value = NA)
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
  # Event handler for "Yes" button in the confirmation dialog
  observeEvent(input$yes_push, {
    removeModal()
    push_to_sheets() # This function pushes the data
    
    # Show a modal dialog to confirm that data has been pushed
    showModal(modalDialog(
      title = "Data Pushed to Google Sheets",
      "Data has been successfully pushed to Google Sheets."
    ))
    
    # Reset the data reactive variable to its initial empty state
    data(data.frame(
      Data_Enterer = character(),
      Date_Collected = character(),
      Date_Fixed = character(),
      Date_Processed = character(),
      Site_Number = character(),
      Treatment = character(),
      Species = character(),
      Sample_Number = character(),
      Test_Height_mm = numeric(),
      Test_Diameter_mm = numeric(),
      Animal_Wet_Mass_g = numeric(),
      Animal_24hr_Mass_g = numeric(),
      Gonad_Wet_Mass_g = numeric(),
      Notes = character(),
      Date_Entered = character(),
      stringsAsFactors = FALSE
    ))
    
    # Clear all input fields to reflect the reset in the UI
    updateDateInput(session, "date_collected", value = NULL)
    updateDateInput(session, "date_fixed", value = NULL)
    updateDateInput(session, "date_processed", value = NULL)
    updateTextInput(session, "site_number", value = "")
    updateTextInput(session, "treatment", value = "")
    updateTextInput(session, "species", value = "")
    updateTextInput(session, "sample_number", value = "")
    updateNumericInput(session, "test_height_mm", value = NA)
    updateNumericInput(session, "test_diameter_mm", value = NA)
    updateNumericInput(session, "animal_wet_mass_g", value = NA)
    updateNumericInput(session, "animal_24hr_mass_g", value = NA)
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
                    $('#date_fixed').val(data[2]);
                    $('#date_processed').val(data[3]);
                    $('#site_number').val(data[4]);
                    $('#treatment').val(data[5]);
                    $('#species').val(data[6]);
                    $('#sample_number').val(data[7]);
                    $('#test_height_mm').val(data[8]);
                    $('#test_diameter_mm').val(data[9]);
                    $('#animal_wet_mass_g').val(data[10]);
                    $('#animal_24hr_mass_g').val(data[11]);
                    $('#gonad_wet_mass_g').val(data[12]);
                    $('#notes').val(data[13]);
                });"
              )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
