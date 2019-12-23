library(shiny)

function(input, output) {
  
  output$graf1 <- renderPlot({
    graf.kmetije <- ggplot(nov_data_napoved_cela %>% filter(Kmetije == input$Kmetije)) + 
      aes(x = leto, y = stevilo) + geom_bar(stat="identity", position="dodge", fill = "darkgreen") +
      labs(title = "Stolpični graf kmetijskih gospodarstev") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Število kmetijskih gospodarstev") + xlab("Leto")
    print(graf.kmetije)
  })

  output$graf2 <- renderPlot({
    graf.organic <- ggplot(Tabela_svet %>% filter(Area == input$Area)) + 
      aes(x = Year, y = Value) + geom_line(size=1.5, color='red') +
      labs(title = "Delež organičnega kmetijstva na posameznem območju v %") + theme(plot.title = element_text(hjust = 0.5)) +
      ylab("Delež") + xlab("Leto")
    print(graf.organic)
  })
}

