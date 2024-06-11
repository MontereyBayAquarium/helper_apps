rm(list=ls())

library(shiny)
library(dplyr)
library(DT)
library(googledrive)
library(googlesheets4)

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "tokens/.secrets"
)

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
    background-color: #007bff;
    border-color: #007bff;
    border-radius: 20px;
    box-shadow: 0 2px 2px 0 rgba(0,0,0,.1);
  }
  #push_to_sheets:hover, .btn-primary:hover {
    background-color: #0056b3;
    border-color: #0056b3;
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
  #push_to_sheets {
    background-color: #ff0000;
    border-color: #ff0000;
  }
  #push_to_sheets:hover {
    background-color: #cc0000;
    border-color: #cc0000;
  }
"))
  ),
titlePanel("Data entry app for sea urchin dissections", windowTitle = "Sea Urchin Data Entry"),
sidebarLayout(
  sidebarPanel(
    selectInput("institution", "Institution", choices = c("MBA", "UCSC", "Other"), selected = "MBA"),
    conditionalPanel(
      condition = "input.institution == 'Other'",
      textInput("institution_other", "Enter Institution")
    ),
    textInput("observer_initials", "Observer Initials"),
    dateInput("date_collected", "Date Collected", value = NULL),
    dateInput("date_fixed", "Date Fixed", value = NULL),
    dateInput("date_dissected", "Date Dissected", value = NULL),
    textInput("site_number", "Site Number"),
    textInput("transect", "Transect"),
    selectInput("treatment", "Treatment", choices = c("5mmol MgCL", "70% EtOH", "95% EtOH"), selected = "5mmol MgCL"),
    selectInput("species", "Species", choices = c("purple_urchin", "red_urchin")),
    textInput("sample_number", "Sample Number"),
    selectInput("sex", "Sex", choices = c("Female", "Male")),
    numericInput("test_height_mm", "Test Height (mm)", value = NULL, step = 0.001),
    numericInput("test_diameter_mm", "Test Diameter (mm)", value = NULL, step = 0.001),
    numericInput("wet_mass_g", "Wet Mass (g)", value = NULL, step = 0.001),
    numericInput("wet_mass_24hr_g", "24hr Wet Mass (g)", value = NULL, step = 0.001),
    numericInput("gonad_wet_mass_g", "Gonad Wet Mass (g)", value = NULL, step = 0.001),
    numericInput("soft_tissue_mass_g", "Soft Tissue Mass (g)", value = NULL, step = 0.001),
    textInput("notes", "Notes"),
    actionButton("stage_sample", "Stage Sample", class = "btn-primary"),
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
  data <- reactiveVal(data.frame(
    Institution = character(),
    Observer_Initials = character(),
    Date_Collected = character(),
    Date_Fixed = character(),
    Date_Dissected = character(),
    Site_Number = character(),
    Transect = character(),
    Treatment = character(),
    Species = character(),
    Sample_Number = character(),
    Sex = character(),
    Test_Height_mm = numeric(),
    Test_Diameter_mm = numeric(),
    Wet_Mass_g = numeric(),
    Wet_Mass_24hr_g = numeric(),
    Gonad_Wet_Mass_g = numeric(),
    Soft_Tissue_Mass_g = numeric(),
    Notes = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$stage_sample, {
    isolate({
      institution <- if (input$institution == "Other") input$institution_other else input$institution
      new_row <- data.frame(
        Institution = institution,
        Observer_Initials = input$observer_initials,
        Date_Collected = as.character(input$date_collected),
        Date_Fixed = as.character(input$date_fixed),
        Date_Dissected = as.character(input$date_dissected),
        Site_Number = toupper(input$site_number),
        Transect = input$transect,
        Treatment = input$treatment,
        Species = input$species,
        Sample_Number = input$sample_number,
        Sex = input$sex,
        Test_Height_mm = input$test_height_mm,
        Test_Diameter_mm = input$test_diameter_mm,
        Wet_Mass_g = input$wet_mass_g,
        Wet_Mass_24hr_g = input$wet_mass_24hr_g,
        Gonad_Wet_Mass_g = input$gonad_wet_mass_g,
        Soft_Tissue_Mass_g = input$soft_tissue_mass_g,
        Notes = input$notes,
        stringsAsFactors = FALSE
      )
      data(bind_rows(data(), new_row))
      
      # Clear input fields
      updateSelectInput(session, "institution", selected = "MBA")
      updateTextInput(session, "institution_other", value = "")
      updateTextInput(session, "observer_initials", value = "")
      updateDateInput(session, "date_collected", value = NULL)
      updateDateInput(session, "date_fixed", value = NULL)
      updateDateInput(session, "date_dissected", value = NULL)
      updateTextInput(session, "site_number", value = "")
      updateTextInput(session, "transect", value = "")
      updateSelectInput(session, "treatment", selected = "5mmol MgCL")
      updateSelectInput(session, "species", selected = "")
      updateTextInput(session, "sample_number", value = "")
      updateSelectInput(session, "sex", selected = "")
      updateNumericInput(session, "test_height_mm", value = NA)
      updateNumericInput(session, "test_diameter_mm", value = NA)
      updateNumericInput(session, "wet_mass_g", value = NA)
      updateNumericInput(session, "wet_mass_24hr_g", value = NA)
      updateNumericInput(session, "gonad_wet_mass_g", value = NA)
      updateNumericInput(session, "soft_tissue_mass_g", value = NA)
      updateTextInput(session, "notes", value = "")
    })
  })
  
  observeEvent(input$clear_fields, {
    updateSelectInput(session, "institution", selected = "MBA")
    updateTextInput(session, "institution_other", value = "")
    updateTextInput(session, "observer_initials", value = "")
    updateDateInput(session, "date_collected", value = NULL)
    updateDateInput(session, "date_fixed", value = NULL)
    updateDateInput(session, "date_dissected", value = NULL)
    updateTextInput(session, "site_number", value = "")
    updateTextInput(session, "transect", value = "")
    updateSelectInput(session, "treatment", selected = "5mmol MgCL")
    updateSelectInput(session, "species", selected = "")
    updateTextInput(session, "sample_number", value = "")
    updateSelectInput(session, "sex", selected = "")
    updateNumericInput(session, "test_height_mm", value = NA)
    updateNumericInput(session, "test_diameter_mm", value = NA)
    updateNumericInput(session, "wet_mass_g", value = NA)
    updateNumericInput(session, "wet_mass_24hr_g", value = NA)
    updateNumericInput(session, "gonad_wet_mass_g", value = NA)
    updateNumericInput(session, "soft_tissue_mass_g", value = NA)
    updateTextInput(session, "notes", value = "")
  })
  
  push_to_sheets <- function() {
    if (nrow(data()) == 0) {
      showModal(modalDialog(
        title = "No Data",
        "No data to push to Google Sheets.",
        footer = modalButton("OK")
      ))
      return()
    }
    gs4_auth(email = TRUE, cache = "tokens/.secrets")
    sheet_append(sheet_id, data())
  }
  
  observeEvent(input$push_to_sheets, {
    showModal(modalDialog(
      title = "Confirm Push to Google Sheets",
      "Are you sure you want to push the data to Google Sheets?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("yes_push", "Yes")
      )
    ))
  })
  
  observeEvent(input$yes_push, {
    removeModal()
    push_to_sheets()
    showModal(modalDialog(
      title = "Data Pushed to Google Sheets",
      "Data has been successfully pushed to Google Sheets."
    ))
    data(data.frame(
      Institution = character(),
      Observer_Initials = character(),
      Date_Collected = character(),
      Date_Fixed = character(),
      Date_Dissected = character(),
      Site_Number = character(),
      Transect = character(),
      Treatment = character(),
      Species = character(),
      Sample_Number = character(),
      Sex = character(),
      Test_Height_mm = numeric(),
      Test_Diameter_mm = numeric(),
      Wet_Mass_g = numeric(),
      Wet_Mass_24hr_g = numeric(),
      Gonad_Wet_Mass_g = numeric(),
      Soft_Tissue_Mass_g = numeric(),
      Notes = character(),
      stringsAsFactors = FALSE
    ))
    updateSelectInput(session, "institution", selected = "MBA")
    updateTextInput(session, "institution_other", value = "")
    updateTextInput(session, "observer_initials", value = "")
    updateDateInput(session, "date_collected", value = NULL)
    updateDateInput(session, "date_fixed", value = NULL)
    updateDateInput(session, "date_dissected", value = NULL)
    updateTextInput(session, "site_number", value = "")
    updateTextInput(session, "transect", value = "")
    updateSelectInput(session, "treatment", selected = "5mmol MgCL")
    updateSelectInput(session, "species", selected = "")
    updateTextInput(session, "sample_number", value = "")
    updateSelectInput(session, "sex", selected = "")
    updateNumericInput(session, "test_height_mm", value = NA)
    updateNumericInput(session, "test_diameter_mm", value = NA)
    updateNumericInput(session, "wet_mass_g", value = NA)
    updateNumericInput(session, "wet_mass_24hr_g", value = NA)
    updateNumericInput(session, "gonad_wet_mass_g", value = NA)
    updateNumericInput(session, "soft_tissue_mass_g", value = NA)
    updateTextInput(session, "notes", value = "")
  })
  
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
                    $('#institution').val(data[0]);
                    $('#institution_other').val(data[1]);
                    $('#observer_initials').val(data[2]);
                    $('#date_collected').val(data[3]);
                    $('#date_fixed').val(data[4]);
                    $('#date_dissected').val(data[5]);
                    $('#site_number').val(data[6]);
                    $('#transect').val(data[7]);
                    $('#treatment').val(data[8]);
                    $('#species').val(data[9]);
                    $('#sample_number').val(data[10]);
                    $('#sex').val(data[11]);
                    $('#test_height_mm').val(data[12]);
                    $('#test_diameter_mm').val(data[13]);
                    $('#wet_mass_g').val(data[14]);
                    $('#wet_mass_24hr_g').val(data[15]);
                    $('#gonad_wet_mass_g').val(data[16]);
                    $('#soft_tissue_mass_g').val(data[17]);
                    $('#notes').val(data[18]);
                });"
              )
    )
  })
}

shinyApp(ui = ui, server = server)
