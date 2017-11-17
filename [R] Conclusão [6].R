a <- NULL
a <- data.frame(a)

ano        <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2013", "2014", "2015", "2016", "2012", "2013", "2014", "2015", "2016")
indicador  <- c("CD",   "CD",   "CD",   "CD",   "CD",   "CD",   "CD",   "PPAG", "PPAG", "PPAG", "PPAG", "PNAD", "PNAD", "PNAD", "PNAD", "PNAD")
valor      <- c(0.8389, 0.8509, 0.7987, 0.8279, 0.8220, 0.8256, 0.8960, 0.8803, 0.8741, 0.8779, 0.9528, 1.0926, 1.0042, 1.1174, 1.0121, 1.0420)

a <- data.frame(ano, indicador, valor)

b <- ggplot(a, aes(x = ano, y = valor, group = indicador)) + 
        geom_hline(yintercept = c(0.7, 0.8, 0.9, 1, 1.1, 1.2)) +
        geom_hline(yintercept = 1, size = 1) +
        geom_line(aes(colour = indicador), stat = "identity", size = 2) +
        theme_CFAMGBH + xlab("Ano") + ylab("Percentual da População de\n4 a 5 Anos que Frequenta Pré-Escola") +
        scale_colour_manual(label = c("Dados do Censo Demográfico", "Dados da PNAD Contínua", "Dados do Plano Plurianual de Ação Governamental"),
                            values = c(cor1, cor2, cor3)) +
        scale_y_continuous(breaks = c(0.7, 0.8, 0.9, 1, 1.1, 1.2),
                           labels = c("70%", "80%", "90%", "100%", "110%", "120%"))

Cairo(file="CECDPNAD.png", type="png", units="cm", width=16, height=8, pointsize=12, dpi=96); b ;dev.off()
