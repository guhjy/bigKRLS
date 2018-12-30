shiny <- function (out, export = FALSE, main.label = "bigKRLS estimates", 
    plot.label = NULL, xlabs = NULL, font_size = 20, hline = 0, 
    shiny.palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
        "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")) 
{
    if (!export) {
        cat("export set to FALSE; set export to TRUE to prepare files for another machine.")
    }
    if (is.null(xlabs)) 
        xlabs = out$xlabs
    colnames(out$X) <- xlabs
    dydxlabs <- if (is.null(out$which.derivatives)) 
        xlabs
    else xlabs[out$which.derivatives]
    colnames(out$derivatives) <- names(out$avgderivatives) <- names(out$var.avgderivatives) <- out$dydxlabs <- dydxlabs
    palette(shiny.palette)
    bigKRLS_server <- shinyServer(function(input, output, session) {
        selectedData <- reactive({
            return(list(x = as.numeric(out$X[, input$xp]), derivatives = as.numeric(out$derivatives[, 
                input$dydxp])))
        })
        output$graph <- renderPlot({
            P = ggplot(NULL)
            P = P + geom_point(aes(x = selectedData()[["x"]], 
                y = selectedData()[["derivatives"]]), alpha = 1, 
                size = 0.1, color = "grey")
            P = P + geom_smooth(aes(x = selectedData()[["x"]], 
                y = selectedData()[["derivatives"]]), method = "loess") + 
                xlab(input$xp)
            P = P + ylab(paste("Log(GOT): marginal effects"))
            P = P + geom_hline(aes(yintercept = hline))
            P = P + theme_minimal(base_size = font_size)
            P = P + theme(panel.background = element_blank(), 
                panel.border = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank())
            P = P + labs(title = plot.label)
            P
        })
    })
    bigKRLS_ui <- shinyUI(fluidPage(titlePanel(main.label), sidebarPanel(selectInput("dydxp", 
        "Marginal Effects (dy/dx)", colnames(out$derivatives)), 
        selectInput("xp", "x", colnames(out$X))), mainPanel(plotOutput("graph"))))
    if (export) {
        output_baseR <- out
        output_baseR$K <- output_baseR$vcov.c <- output_baseR$vcov.fitted <- NULL
        for (i in which(unlist(lapply(output_baseR, is.big.matrix)))) {
            output_baseR[[i]] <- as.matrix(output_baseR[[i]])
        }
        save(output_baseR, file = "shiny_out.RData")
        cat("A re-formatted version of your output has been saved with file name \"shiny_out.RData\" in your current working directory:\n", 
            getwd(), "\nFor a few technical reasons, the big N * N matrices have been removed and the smaller ones converted back to base R;\nthis should make your output small enough for the free version of Shiny's server.\nTo access the Shiny app later or on a different machine, simply execute this script with the following commands:\n", 
            "\nload(\"shiny_out.RData\")\nNext, execute this code:\n\nshiny.bigKRLS(output_baseR)")
    }
    else {
        shinyApp(ui = bigKRLS_ui, server = bigKRLS_server)
    }
}
