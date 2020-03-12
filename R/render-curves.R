
renderCurves <- function(data, outcome, selectedAG, ShowCaseTimeSeries = FALSE, 
                         TimeSeries = CaseTimeSeries) {
    ## --- INFECTED CURVES -------------------------------------------------------
    if (outcome == "Infected") {
    
        mainPlot = renderPlotly({
            if (selectedAG == "All") {
                dataAgg = data[, .(Infected = sum(Infected)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg, aes(x = Time, y = Infected, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(Infected), by = "Time"]
                setnames(dataAgg, "V1", "Infected")
                if(ShowCaseTimeSeries){
                    p = ggplot(dataAgg, aes(x = Time, y = Infected)) +
                        theme_classic() +
                        geom_line() +
                        geom_point(data = TimeSeries[!is.na(Cases),], 
                                   aes(x = Date, y = Cases),
                                   col = "red")
                    p = plotly::ggplotly(p)
                }
                else {
                    p = ggplot(dataAgg, aes(x = Time, y = Infected)) +
                        theme_classic() +
                        geom_line()
                    p = plotly::ggplotly(p)
                }
            } else{
                dataAgg = data[AgeGroup == selectedAG, .(Infected = sum(Infected)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg, aes(x = Time, y = Infected, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            }
            return(p)
        })
        secondPlot = NULL
    } else if (outcome == "symptomatic") {
        #to be updated
        mainPlot = renderPlotly({
            if (selectedAG == "All") {
                dataAgg = data[, .(Infected = sum(Infected)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg, aes(x = Time, y = Infected, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(Infected), by = "Time"]
                setnames(dataAgg, "V1", "Infected")
                p = ggplot(dataAgg, aes(x = Time, y = Infected)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else{
                dataAgg = data[AgeGroup == selectedAG, .(Infected = sum(Infected)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg, aes(x = Time, y = Infected, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            }
            return(p)
        })
    
        ## --- SEVERITY CURVES ---------------------------------------------------
    } else if (outcome == "severity") {
        
        mainPlot = renderPlotly({
            if (selectedAG == "All") {
                dataAgg = data[, .(severe = sum(severe)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg, aes(x = Time, y = severe, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(severe), by = "Time"]
                setnames(dataAgg, "V1", "Severe")
                p = ggplot(dataAgg, aes(x = Time, y = Severe)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else {
                dataAgg = data[AgeGroup == selectedAG, .(severe = sum(severe)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg,
                           aes(x = Time, y = severe, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            }
            return(p)
        })
        secondPlot = renderPlotly({
            if (selectedAG == "All") {
                dataAgg = data[, .(non.severe = sum(non.severe)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg, aes(x = Time, y = non.severe, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(non.severe), by = "Time"]
                setnames(dataAgg, "V1", "Non.Severe")
                p = ggplot(dataAgg, aes(x = Time, y = Non.Severe)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else {
                dataAgg = data[AgeGroup == selectedAG, .(non.severe = sum(non.severe)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg,
                           aes(x = Time, y = non.severe, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            }
            return(p)
        })
        ## ---- ICU CURVES -------------------------------------------------------
    } else if (outcome == "ICU") {
        mainPlot = renderPlotly({
            if (selectedAG == "All") {
                dataAgg = data[, .(ICU = sum(ICU)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg, aes(x = Time, y = ICU, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(ICU), by = "Time"]
                setnames(dataAgg, "V1", "ICU")
                p = ggplot(dataAgg, aes(x = Time, y = ICU)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else {
                dataAgg = data[AgeGroup == selectedAG, .(ICU = sum(ICU)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg,
                           aes(x = Time, y = ICU, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            }
            return(p)
        })
        secondPlot = renderPlotly({
            if (selectedAG == "All") {
                dataAgg = data[, .(non.ICU = sum(non.ICU)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg, aes(x = Time, y = non.ICU, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(non.ICU), by = "Time"]
                setnames(dataAgg, "V1", "Non.ICU")
                p = ggplot(dataAgg, aes(x = Time, y = Non.ICU)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else {
                dataAgg = data[AgeGroup == selectedAG, .(non.ICU = sum(non.ICU)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg,
                           aes(x = Time, y = non.ICU, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            }
            return(p)
        })
        ## --- VENTILATION CURVES ------------------------------------------------
    } else if (outcome == "ventilation") {
        
        mainPlot = renderPlotly({
            if (selectedAG == "All") {
                dataAgg = data[, .(invasive.ventil = sum(invasive.ventil)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg, aes(x = Time, y = invasive.ventil, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(invasive.ventil), by = "Time"]
                setnames(dataAgg, "V1", "invasive.ventil")
                p = ggplot(dataAgg, aes(x = Time, y = invasive.ventil)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else {
                dataAgg = data[AgeGroup == selectedAG, .(invasive.ventil = sum(invasive.ventil)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg,
                           aes(x = Time, y = invasive.ventil, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            }
            return(p)
        })
        secondPlot = renderPlotly({
            if (selectedAG == "All") {
                dataAgg = data[, .(non.invasive.ventil = sum(non.invasive.ventil)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg, aes(x = Time, y = non.invasive.ventil, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(non.invasive.ventil), by = "Time"]
                setnames(dataAgg, "V1", "Non.invasive.ventil")
                p = ggplot(dataAgg, aes(x = Time, y = Non.invasive.ventil)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            } else {
                dataAgg = data[AgeGroup == selectedAG, .(non.invasive.ventil = sum(non.invasive.ventil)), by = c("Time","AgeGroup")]
                p = ggplot(dataAgg,
                           aes(x = Time, y = non.invasive.ventil, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
                p = plotly::ggplotly(p)
            }
            return(p)
        })
    } 
    return(list(mainPlot = mainPlot,
                secondPlot = secondPlot))
    
}
