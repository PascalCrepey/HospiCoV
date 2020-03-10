
renderCurves <- function(data, outcome, selectedAG) {
    ## --- INFECTED CURVES -------------------------------------------------------
    if (outcome == "Infected") {
        
        mainPlot = renderPlotly({
            if (selectedAG == "All") {
                p = ggplot(data, aes(x = Time, y = Infected, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(Infected), by = "Time"]
                setnames(dataAgg, "V1", "Infected")
                p = ggplot(dataAgg, aes(x = Time, y = Infected)) +
                    theme_classic() +
                    geom_line()
            } else{
                p = ggplot(data[AgeGroup == selectedAG,], aes(x = Time, y = Infected, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            }
            return(p)
        })
        secondPlot = NULL
        ## --- SEVERITY CURVES ---------------------------------------------------
    } else if (outcome == "severity") {
        
        mainPlot = renderPlotly({
            if (selectedAG == "All") {
                p = ggplot(data, aes(x = Time, y = severe, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(severe), by = "Time"]
                setnames(dataAgg, "V1", "Severe")
                p = ggplot(dataAgg, aes(x = Time, y = Severe)) +
                    theme_classic() +
                    geom_line()
            } else {
                p = ggplot(data[AgeGroup == selectedAG,],
                           aes(x = Time, y = severe, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            }
            return(p)
        })
        secondPlot = renderPlotly({
            if (selectedAG == "All") {
                p = ggplot(data, aes(x = Time, y = non.severe, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(non.severe), by = "Time"]
                setnames(dataAgg, "V1", "Non.Severe")
                p = ggplot(dataAgg, aes(x = Time, y = Non.Severe)) +
                    theme_classic() +
                    geom_line()
            } else {
                p = ggplot(data[AgeGroup == selectedAG,],
                           aes(x = Time, y = non.severe, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            }
            return(p)
        })
        ## ---- ICU CURVES -------------------------------------------------------
    } else if (outcome == "ICU") {
        mainPlot = renderPlotly({
            if (selectedAG == "All") {
                p = ggplot(data, aes(x = Time, y = ICU, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(ICU), by = "Time"]
                setnames(dataAgg, "V1", "ICU")
                p = ggplot(dataAgg, aes(x = Time, y = ICU)) +
                    theme_classic() +
                    geom_line()
            } else {
                p = ggplot(data[AgeGroup == selectedAG,],
                           aes(x = Time, y = ICU, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            }
            return(p)
        })
        secondPlot = renderPlotly({
            if (selectedAG == "All") {
                p = ggplot(data, aes(x = Time, y = non.ICU, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(non.ICU), by = "Time"]
                setnames(dataAgg, "V1", "Non.ICU")
                p = ggplot(dataAgg, aes(x = Time, y = Non.ICU)) +
                    theme_classic() +
                    geom_line()
            } else {
                p = ggplot(data[AgeGroup == selectedAG,],
                           aes(x = Time, y = non.ICU, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            }
            return(p)
        })
        ## --- VENTILATION CURVES ------------------------------------------------
    } else if (outcome == "ventilation") {
        
        mainPlot = renderPlotly({
            if (selectedAG == "All") {
                p = ggplot(data, aes(x = Time, y = invasive.ventil, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(invasive.ventil), by = "Time"]
                setnames(dataAgg, "V1", "invasive.ventil")
                p = ggplot(dataAgg, aes(x = Time, y = invasive.ventil)) +
                    theme_classic() +
                    geom_line()
            } else {
                p = ggplot(data[AgeGroup == selectedAG,],
                           aes(x = Time, y = invasive.ventil, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            }
            return(p)
        })
        secondPlot = renderPlotly({
            if (selectedAG == "All") {
                p = ggplot(data, aes(x = Time, y = non.invasive.ventil, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            } else if (selectedAG == "Aggregated") {
                dataAgg = data[, sum(non.invasive.ventil), by = "Time"]
                setnames(dataAgg, "V1", "Non.invasive.ventil")
                p = ggplot(dataAgg, aes(x = Time, y = Non.invasive.ventil)) +
                    theme_classic() +
                    geom_line()
            } else {
                p = ggplot(data[AgeGroup == selectedAG,],
                           aes(x = Time, y = non.invasive.ventil, color = AgeGroup)) +
                    theme_classic() +
                    geom_line()
            }
            return(p)
        })

    }
    return(list(mainPlot = mainPlot,
                secondPlot = secondPlot))
    
}