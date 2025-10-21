# UI
mod_chatbot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::div(
      class = "chat-wrapper",
      style = "height: calc(100vh - 200px); display: flex; flex-direction: column;",
      
      # Messages container
      shiny::div(
        class = "chat-messages",
        style = "flex: 1; overflow-y: auto; padding: 20px; margin-bottom: 20px;",
        shiny::uiOutput(ns("messages"))
      ),
      
      # Input area
      shiny::div(
        class = "chat-input-area",
        style = "padding: 20px; background: var(--bs-card-bg); border-radius: 12px;",
        shiny::textAreaInput(
          ns("user_input"),
          NULL,
          placeholder = "Ask about your energy data...",
          width = "100%",
          rows = 3
        ),
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-top: 10px;",
          shiny::div(
            shiny::actionButton(ns("send"), "Send", class = "btn-primary"),
            shiny::actionButton(ns("clear"), "Clear Chat", class = "btn-secondary", style = "margin-left: 10px;")
          ),
          shiny::div(
            id = ns("typing_indicator"),
            style = "display: none; color: var(--bs-secondary); font-style: italic;",
            "Assistant is typing..."
          )
        )
      )
    )
  )
}

# Server
mod_chatbot_server <- function(id, dm) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Store conversation history
    messages <- shiny::reactiveVal(list())
    
    # Initialize chat with OpenAI
    chat <- ellmer::chat_openai(
      system_prompt = paste(
        "You are a helpful energy data analyst assistant.",
        "Answer questions about energy consumption and emissions data.",
        "Be concise but informative. Use bullet points when appropriate.",
        "If asked about specific numbers, refer to the data context provided."
      ),
      model = "gpt-5-nano" 
    )
    
    # Generate data context summary
    # Generate data context summary
    get_context <- shiny::reactive({
      shiny::req(dm())
      status <- dm()$status()
      filtered <- dm()$filtered_data()
      
      if (nrow(filtered) == 0) {
        return("No data currently loaded or all filtered out.")
      }
      
      # Get breakdowns by site and type
      by_site <- dm()$compare(by = "site")
      by_type <- dm()$compare(by = "type")
      
      # Format site breakdown
      site_breakdown <- paste(
        sprintf("  - %s: %s (emissions: %.0f kgCO2e)", 
                by_site$site, 
                format(by_site$value, big.mark = ","),
                by_site$emissions),
        collapse = "\n"
      )
      
      # Format type breakdown
      type_breakdown <- paste(
        sprintf("  - %s: %s (emissions: %.0f kgCO2e)", 
                by_type$type, 
                format(by_type$value, big.mark = ","),
                by_type$emissions),
        collapse = "\n"
      )
      
      sprintf(
        paste(
          "=== CURRENT DATA CONTEXT ===",
          "Dataset: %s rows covering %d sites",
          "Date range: %s to %s",
          "",
          "BREAKDOWN BY SITE (sorted by consumption):",
          "%s",
          "",
          "BREAKDOWN BY ENERGY TYPE (sorted by consumption):",
          "%s",
          "",
          "OVERALL TOTALS:",
          "  - Total consumption: %s",
          "  - Total emissions: %s kgCO2e",
          "",
          "Note: The user can filter this data by date, site, and type from the Dashboard tab.",
          sep = "\n"
        ),
        format(status$n_rows, big.mark = ","),
        status$n_sites,
        status$date_min,
        status$date_max,
        site_breakdown,
        type_breakdown,
        format(dm()$kpi_total_consumption(), big.mark = ","),
        format(dm()$kpi_total_emissions(), big.mark = ",")
      )
    })
    # Send message
    shiny::observeEvent(input$send, {
      shiny::req(input$user_input)
      user_msg <- trimws(input$user_input)
      if (nchar(user_msg) == 0) return()
      
      # Show typing indicator
      shinyjs::show("typing_indicator")
      
      # Add user message
      msgs <- messages()
      msgs <- c(msgs, list(
        list(role = "user", content = user_msg, timestamp = Sys.time())
      ))
      messages(msgs)
      
      # Clear input
      shiny::updateTextAreaInput(session, "user_input", value = "")
      
      # Get response from OpenAI
      tryCatch({
        # Combine context with user message
        full_prompt <- paste(
          get_context(),
          "",
          "USER QUESTION:",
          user_msg,
          sep = "\n"
        )
        
        response <- chat$chat(full_prompt)
        
        # Add assistant message
        msgs <- messages()
        msgs <- c(msgs, list(
          list(role = "assistant", content = response, timestamp = Sys.time())
        ))
        messages(msgs)
        
      }, error = function(e) {
        msgs <- messages()
        msgs <- c(msgs, list(
          list(role = "assistant", 
               content = paste("Sorry, I encountered an error:", e$message),
               timestamp = Sys.time(),
               error = TRUE)
        ))
        messages(msgs)
      })
      
      # Hide typing indicator
      shinyjs::hide("typing_indicator")
    })
    
    # Clear conversation
    shiny::observeEvent(input$clear, {
      messages(list())
      # Reset chat context
      chat <<- ellmer::chat_openai(
        system_prompt = paste(
          "You are a helpful energy data analyst assistant.",
          "Answer questions about energy consumption and emissions data.",
          "Be concise but informative. Use bullet points when appropriate.",
          "If asked about specific numbers, refer to the data context provided."
        ),
        model = "gpt-5-nano"
      )
    })
    
    # Render messages
    output$messages <- shiny::renderUI({
      msgs <- messages()
      if (length(msgs) == 0) {
        return(
          shiny::div(
            style = "text-align: center; padding: 40px; color: var(--bs-secondary);",
            shiny::h4("Hello!"),
            shiny::p("Ask me anything about your energy and emissions data."),
            shiny::p(
              style = "font-size: 0.9em;",
              "Try: 'What sites use the most energy?' or 'Summarize my emissions data'"
            )
          )
        )
      }
      
      # Build message divs
      message_divs <- lapply(seq_along(msgs), function(i) {
        msg <- msgs[[i]]
        is_user <- msg$role == "user"
        is_error <- !is.null(msg$error) && msg$error
        
        bg_color <- if (is_user) {
          "var(--bs-primary)"
        } else if (is_error) {
          "#dc3545"
        } else {
          "var(--bs-secondary-bg)"
        }
        
        text_color <- if (is_user || is_error) {
          "#ffffff"
        } else {
          "var(--bs-body-color)"
        }
        
        align <- if (is_user) "flex-end" else "flex-start"
        
        shiny::div(
          style = sprintf(
            "display: flex; justify-content: %s; margin-bottom: 16px;",
            align
          ),
          shiny::div(
            style = sprintf(
              paste(
                "max-width: 80%%;",
                "padding: 12px 16px;",
                "border-radius: 12px;",
                "background: %s;",
                "color: %s;",
                "white-space: pre-wrap;",
                "word-wrap: break-word;"
              ),
              bg_color, text_color
            ),
            shiny::div(
              style = "font-weight: 500; margin-bottom: 4px; font-size: 0.85em;",
              if (is_user) "You" else "Assistant"
            ),
            shiny::markdown(msg$content)
          )
        )
      })
      
      shiny::tagList(message_divs)
    })
    
    # Auto-scroll to bottom when new messages arrive
    shiny::observe({
      messages()
      shinyjs::runjs("
        var container = document.querySelector('.chat-messages');
        if (container) {
          container.scrollTop = container.scrollHeight;
        }
      ")
    })
  })
}