## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Ask Atlantis guidance drawer (optional)    ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

assistant_ui <- function(id) {
  ns <- shiny::NS(id)
  bubble <- shiny::HTML(paste0(
    '<svg class="ra-ai-bubble-icon" viewBox="0 0 24 24" width="26" height="26" aria-hidden="true">',
    '<path fill="currentColor" d="M20 2H4c-1.1 0-2 .9-2 2v18l4-4h14c1.1 0 2-.9 2-2V4c0-1.1-.9-2-2-2zm0 14H6l-2 2V4h16v12z"/>',
    '<circle fill="currentColor" cx="8" cy="10" r="1.2"/>',
    '<circle fill="currentColor" cx="12" cy="10" r="1.2"/>',
    '<circle fill="currentColor" cx="16" cy="10" r="1.2"/>',
    "</svg>"
  ))
  shiny::div(
    class = "ra-ai-shell",
    id = ns("shell"),
    shiny::tags$button(
      id = ns("fab"),
      class = "ra-ai-fab",
      type = "button",
      title = "Ask Atlantis",
      `aria-label` = "Ask Atlantis",
      `aria-expanded` = "false",
      `aria-controls` = ns("drawer"),
      bubble
    ),
    shiny::div(
      id = ns("drawer"),
      class = "ra-ai-drawer",
      role = "dialog",
      `aria-label` = "Ask Atlantis",
      `aria-hidden` = "true",
      shiny::div(
        class = "ra-assistant",
        shiny::div(
          class = "ra-assistant-header",
          shiny::h3("Ask Atlantis"),
          shiny::uiOutput(ns("status_badge"), inline = TRUE),
          shiny::tags$button(
            id = ns("close"),
            class = "ra-ai-close btn-close",
            type = "button",
            title = "Close Ask Atlantis",
            `aria-label` = "Close Ask Atlantis"
          )
        ),
        shiny::p(
          class = "ra-assistant-disclaimer",
          "Guidance only. Answers are not numerical diagnostics or verified diagnoses.",
          "Use the existing tabs to inspect plots and tables."
        ),
        shiny::div(
          class = "ra-assistant-starters",
          shiny::actionLink(ns("starter_what"), "What can Reactive Atlantis do?"),
          shiny::actionLink(ns("starter_pred"), "Where do I inspect predation?"),
          shiny::actionLink(ns("starter_rec"), "What should I check when recruitment is low?")
        ),
        shiny::div(
          class = "ra-assistant-log",
          shiny::uiOutput(ns("messages"))
        ),
        shiny::div(
          class = "ra-assistant-input",
          shiny::textAreaInput(
            ns("question"), NULL,
            placeholder = "Ask about Atlantis processes, parameters, or where to look in this app…",
            rows = 3, width = "100%", resize = "vertical"
          ),
          shiny::div(
            class = "ra-btn-row",
            shiny::actionButton(ns("send"), "Send", class = "btn btn-primary"),
            shiny::actionButton(ns("clear"), "Clear", class = "btn btn-outline-secondary")
          )
        )
      )
    ),
    shiny::tags$script(shiny::HTML(sprintf(
      "(function() {\n  var shellId = '%s', drawerId = '%s', fabId = '%s', closeId = '%s', openInput = '%s';\n  function setOpen(open) {\n    var shell = document.getElementById(shellId);\n    var drawer = document.getElementById(drawerId);\n    var fab = document.getElementById(fabId);\n    if (!drawer || !fab) return;\n    drawer.classList.toggle('is-open', !!open);\n    if (shell) shell.classList.toggle('is-open', !!open);\n    fab.setAttribute('aria-expanded', open ? 'true' : 'false');\n    drawer.setAttribute('aria-hidden', open ? 'false' : 'true');\n    if (window.Shiny && Shiny.setInputValue) {\n      Shiny.setInputValue(openInput, !!open, {priority: 'event'});\n    }\n  }\n  $(document).on('click', '#' + fabId, function(e) {\n    e.preventDefault();\n    var drawer = document.getElementById(drawerId);\n    setOpen(!(drawer && drawer.classList.contains('is-open')));\n  });\n  $(document).on('click', '#' + closeId, function(e) {\n    e.preventDefault();\n    setOpen(false);\n  });\n  $(document).on('keydown', function(e) {\n    if (e.key === 'Escape') setOpen(false);\n  });\n  $(document).on('click', '#' + drawerId + ' .ra-open-tool', function() {\n    Shiny.setInputValue('%s', $(this).attr('data-ra-nav'), {priority: 'event'});\n  });\n  $(document).on('keydown', '#%s textarea', function(e) {\n    if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {\n      e.preventDefault();\n      $('#%s').click();\n    }\n  });\n})();\n",
      ns("shell"), ns("drawer"), ns("fab"), ns("close"), ns("drawer_open"),
      ns("go_nav"), ns("question"), ns("send")
    )))
  )
}

assistant_server <- function(id, project = NULL, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    config <- ra_ai_config()
    history <- shiny::reactiveVal(list())
    display <- shiny::reactiveVal(list())
    busy <- shiny::reactiveVal(FALSE)
    status <- shiny::reactiveVal(ra_ai_panel_state(config))
    pending_q <- shiny::reactiveVal(NULL)
    pending_tick <- shiny::reactiveVal(0L)

    output$status_badge <- shiny::renderUI({
      if (isTRUE(busy())) {
        return(shiny::span(class = "ra-ai-badge is-thinking", "Thinking…"))
      }
      st <- status()
      shiny::span(
        class = paste("ra-ai-badge", paste0("is-", st$code)),
        st$label
      )
    })

    output$messages <- shiny::renderUI({
      items <- display()
      st <- status()
      thinking <- isTRUE(busy())
      if (length(items) == 0 && !thinking) {
        return(shiny::div(
          class = "ra-assistant-empty",
          shiny::p(st$detail %||% "Ask a question about Atlantis or this app."),
          shiny::p(
            class = "ra-muted",
            "Nothing is sent off this machine. Ollama stays local."
          )
        ))
      }
      html <- lapply(items, function(msg) {
        if (identical(msg$role, "user")) {
          shiny::div(
            class = "ra-msg ra-msg-user",
            shiny::HTML(ra_ai_safe_html(msg$content))
          )
        } else {
          nav_btns <- NULL
          if (length(msg$nav_targets) > 0) {
            nav_btns <- shiny::div(
              class = "ra-msg-nav",
              lapply(msg$nav_targets, function(tgt) {
                shiny::tags$button(
                  type = "button",
                  class = "btn btn-sm btn-outline-primary ra-open-tool",
                  `data-ra-nav` = tgt$title,
                  paste("Open", tgt$title)
                )
              })
            )
          }
          src <- NULL
          if (length(msg$citations) > 0) {
            src <- shiny::tags$details(
              class = "ra-msg-sources",
              shiny::tags$summary(paste(length(msg$citations), "sources")),
              shiny::tags$ul(lapply(msg$citations, function(cit) {
                loc <- cit$location %||% ""
                lab <- paste0(
                  cit$title, " — ", cit$section,
                  if (nzchar(loc)) paste0(" (", loc, ")") else "",
                  " [", cit$source_type, "]"
                )
                shiny::tags$li(
                  shiny::span(lab),
                  shiny::span(class = "ra-muted", paste0(" ", cit$path))
                )
              }))
            )
          }
          kinds <- NULL
          if (length(msg$claim_kinds) > 0) {
            kinds <- shiny::p(
              class = "ra-muted ra-msg-kinds",
              paste("Claim type:", paste(msg$claim_kinds, collapse = ", "))
            )
          }
          shiny::div(
            class = "ra-msg ra-msg-assistant",
            shiny::HTML(ra_ai_safe_html(msg$content)),
            nav_btns, src, kinds
          )
        }
      })
      if (thinking) {
        html <- c(html, list(
          shiny::div(
            class = "ra-msg ra-msg-assistant ra-msg-thinking",
            `aria-live` = "polite",
            shiny::span(class = "ra-thinking-dots", "···"),
            shiny::span("Thinking…")
          )
        ))
      }
      shiny::tagList(html)
    })

    parent_input <- function() {
      if (!is.null(parent_session)) parent_session$input else session$input
    }

    enqueue_question <- function(question) {
      question <- trimws(as.character(question)[1])
      if (!nzchar(question)) return(invisible(NULL))
      if (isTRUE(busy())) return(invisible(NULL))
      busy(TRUE)
      status(ra_ai_panel_state(config))
      items <- display()
      items[[length(items) + 1]] <- list(role = "user", content = question)
      display(items)
      pending_q(question)
      pending_tick(pending_tick() + 1L)
      shiny::updateTextAreaInput(session, "question", value = "")
      invisible(NULL)
    }

    ## ExtendedTask keeps the rest of the app usable while Ollama runs.
    use_task <- exists("ExtendedTask", where = asNamespace("shiny"),
                       inherits = FALSE)
    ask_task <- NULL
    if (isTRUE(use_task)) {
      ask_task <- shiny::ExtendedTask$new(function(question, hist, ctx, cfg) {
        ra_ai_answer(question, history = hist, context = ctx, config = cfg)
      })
    }

    shiny::observeEvent(pending_tick(), {
      question <- pending_q()
      if (is.null(question) || pending_tick() < 1) return()
      ctx <- tryCatch(ra_ai_safe_ui_context(parent_input()),
                      error = function(e) list())
      hist_now <- history()
      if (!is.null(ask_task)) {
        ask_task$invoke(question, hist_now, ctx, config)
      } else {
        result <- ra_ai_answer(question, history = hist_now,
                               context = ctx, config = config)
        ra_ai_commit_result(question, result, history, display, busy)
        pending_q(NULL)
      }
    }, ignoreNULL = TRUE)

    if (!is.null(ask_task)) {
      shiny::observeEvent(ask_task$result(), {
        result <- ask_task$result()
        question <- pending_q()
        if (is.null(question)) {
          busy(FALSE)
          return()
        }
        ra_ai_commit_result(question, result, history, display, busy)
        pending_q(NULL)
      }, ignoreNULL = TRUE)
    }

    shiny::observe({
      running <- isTRUE(busy())
      shiny::updateActionButton(
        session, "send",
        label = if (running) "Working…" else "Send"
      )
    })

    shiny::observeEvent(input$send, enqueue_question(input$question),
                        ignoreInit = TRUE)
    shiny::observeEvent(input$starter_what, {
      enqueue_question("What can Reactive Atlantis do?")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$starter_pred, {
      enqueue_question("Where do I inspect predation?")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$starter_rec, {
      enqueue_question("What should I check when recruitment is low?")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$clear, {
      history(list())
      display(list())
      busy(FALSE)
      pending_q(NULL)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$go_nav, {
      ra_ai_open_tab(input$go_nav, parent_session)
    }, ignoreInit = TRUE)
  })
}

ra_ai_commit_result <- function(question, result, history, display, busy) {
  hist_now <- history()
  hist_now[[length(hist_now) + 1]] <- list(role = "user", content = question)
  hist_now[[length(hist_now) + 1]] <- list(
    role = "assistant",
    content = result$answer
  )
  cfg_max <- tryCatch(ra_ai_config()$max_history, error = function(e) 12L)
  if (length(hist_now) > cfg_max) hist_now <- utils::tail(hist_now, cfg_max)
  history(hist_now)
  items <- display()
  items[[length(items) + 1]] <- list(
    role = "assistant",
    content = result$answer,
    citations = result$citations,
    nav_targets = result$nav_targets,
    claim_kinds = result$claim_kinds
  )
  display(items)
  busy(FALSE)
}

ra_ai_open_tab <- function(title, parent_session) {
  title <- as.character(title)[1]
  if (is.null(title) || !nzchar(title)) return(invisible(NULL))
  catalog <- ra_ai_nav_catalog()
  titles <- vapply(catalog, function(x) x$title, character(1))
  if (!title %in% titles) return(invisible(NULL))
  if (!is.null(parent_session)) {
    try(shiny::updateNavbarPage(parent_session, "ra_page", selected = title),
        silent = TRUE)
    try(parent_session$sendCustomMessage("ra-open-tab", title), silent = TRUE)
  }
  invisible(title)
}
