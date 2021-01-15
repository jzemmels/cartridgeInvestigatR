###################################
# compare two signatures
output$sig1_select <- renderUI({
  if(kk() <= dataPar$n) {
    selectInput("sig1_idx","Investigate kth land:", selected = 1,
                choices=1:dataPar$n,
                width = "100%")
  } else {
    NULL
  }
  
})

output$sig2_select <- renderUI({
  if(kk() <= dataPar$n) {
    selectInput("sig2_idx","Investigate kth land:", selected = 1,
                choices=1:dataPar$n,
                width = "100%")
  } else {
    NULL
  }
  
})

observeEvent(input$sig1_display, {
  req(shiny.r$data)
  req(input$sig1_idx)
  
  k <- as.numeric(input$sig1_idx)
  
  tmp <- isolate(shiny.r$data)
  
  output$sig1_display_rgl <- renderRglwidget({
    if(isolate(dataPar$hasname_crosscut)){
      image_x3p(
        tmp$x3p[k][[1]] %>% x3p_add_hline(
          yintercept = tmp$crosscut[k], size = 10)) 
    } else {
      image_x3p(tmp$x3p[k][[1]]) 
    }
  })
})

observeEvent(input$sig2_display, {
  req(shiny.r$data)
  req(input$sig2_idx)
  
  k <- as.numeric(input$sig2_idx)
  
  tmp <- isolate(shiny.r$data)
  
  output$sig2_display_rgl <- renderRglwidget({
    if(isolate(dataPar$hasname_crosscut)){
      image_x3p(
        tmp$x3p[k][[1]] %>% x3p_add_hline(
          yintercept = tmp$crosscut[k], size = 10)) 
    } else {
      image_x3p(tmp$x3p[k][[1]]) 
    }
  })
})

observeEvent(input$sig1_draw, {
  k <- as.numeric(isolate(input$sig1_idx))
  
  # update: compute ccdata if null
  tmp.tt <- shiny.r$data %>% slice(k)
  
  if (!assertthat::has_name(tmp.tt, "ccdata")) {
    tmp.tt <- tmp.tt %>% mutate( 
      ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = bulletxtrctr::x3p_crosscut),
      sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) {
        bulletxtrctr::cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)})
    )
  } else {
    tmp.tt <- tmp.tt %>% mutate(
      sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) {
        bulletxtrctr::cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
      }))
  }
  
  global_sig1 <<- tmp.tt$sigs[[1]]$sig
  
  output$sig1_plot <- renderPlot({
    k <- kk()
    p <- tmp.tt$sigs[[1]] %>% filter(!is.na(sig), !is.na(raw_sig)) %>%
      ggplot(aes(x = x)) +
      geom_line(aes(y = raw_sig), colour = "grey70") +
      geom_line(aes(y = sig), colour = "grey30") +
      ylab("value") + ylim(c(-7.5, 7.5)) + theme_bw()
    p
  })
  
})

observeEvent(input$sig2_draw, {
  k <- as.numeric(isolate(input$sig2_idx))
  
  # update: compute ccdata if null
  tmp.tt <- shiny.r$data %>% slice(k)
  
  if (!assertthat::has_name(tmp.tt, "ccdata")) {
    tmp.tt <- tmp.tt %>% mutate( 
      ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = bulletxtrctr::x3p_crosscut),
      sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) {
        bulletxtrctr::cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)})
    )
  } else {
    tmp.tt <- tmp.tt %>% mutate(
      sigs = purrr::map2(.x = ccdata, .y = grooves, .f = function(x, y) {
        bulletxtrctr::cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
      }))
  }
  
  global_sig2 <<- tmp.tt$sigs[[1]]$sig
  
  output$sig2_plot <- renderPlot({
    k <- kk()
    p <- tmp.tt$sigs[[1]] %>% filter(!is.na(sig), !is.na(raw_sig)) %>%
      ggplot(aes(x = x)) +
      geom_line(aes(y = raw_sig), colour = "grey70") +
      geom_line(aes(y = sig), colour = "grey30") +
      ylab("value") + ylim(c(-7.5, 7.5)) + theme_bw()
    p
  })
  
})

observeEvent(input$sig_align, {
  
  sig1_shift <- isolate(input$sig1_align_num)
  sig2_shift <- isolate(input$sig2_align_num)
  
  if(is.null("global_sig1") | is.null("global_sig2")) {
    p <- NULL
  } else if(!is.null(global_sig1) & !is.null(global_sig2)) {
    aa <- sig_align(global_sig1, global_sig2)
    p <- aa$lands %>% ggplot() +
      geom_line(aes(x = x + sig1_shift, y=sig1), color = "red") +
      geom_line(aes(x = x + sig2_shift, y=sig2), color = "blue") + theme_bw()
  }
  
  
  output$sig_align_plot <- renderPlot({
    p
  })
  

  
})


























