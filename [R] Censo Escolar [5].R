setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

CE <- read.csv("DADOS - EdInf.CE.bh.2010.2016.csv")

CE$Etapa_Ensino <- factor(CE$Etapa_Ensino, 
                         levels = c(1, 2), 
                         labels = c("Creche", "Pré-Escola"))

CE$Ano_Censo <- factor(CE$Ano_Censo)

CE$Idade <- factor(CE$Idade)

CE$Cor <- factor(CE$Cor, 
                levels = c(0, 1, 2, 3, 4, 5),
                labels = c("Não Declarada", "Branca", "Preta", "Parda", "Amarela", "Indígena"))

CE$Dependencia_Administrativa <- factor(CE$Dependencia_Administrativa,
                                       levels = c(1, 2, 3, 4),
                                       labels = c("Federal", "Estadual", "Municipal", "Privada"))

CE$Categoria_Privada <- factor(CE$Categoria_Privada,
                              levels = c(1, 2, 3, 4),
                              labels = c("Particular", "Comunitária", "Confessional", "Filantrópica"))

CE$Conveniada_PP <- factor(CE$Conveniada_PP,
                          levels = c(1, 2, 3),
                          labels = c("Municipal", "Estadual", "Estadual e Municipal"))


####################################################################################################
######################################################################################################
########################################################################################################
########################################################                  ################################
########################################################         I        ##################################
########################################################                  ################################
########################################################################################################
######################################################################################################
####################################################################################################

CE1 <-CE %>% group_by(Ano_Censo, Etapa_Ensino) %>% filter(Etapa_Ensino == "Pré-Escola") %>% summarise("Número_Matrículas" = n())

asd <- ggplot(CE1, aes(x = Ano_Censo, y = Número_Matrículas - 40000)) +
        geom_hline(aes(yintercept = c(seq(0, 12000, 2000)))) +
        geom_bar(fill = cor1, stat = "identity", width = 0.5) +
        theme_CFAMGBH + xlab("Ano") + ylab("Número de Crianças Matriculadas\n em Pré-Escola em Belo Horizonte") +
        scale_y_continuous(breaks = seq(0, 12000, 2000),
                           limits = c(0, 12000),
                           label = c("40.000", "42.000", "44.000", "46.000", "48.000", "50.000", "52.000")) +
        theme(axis.text.x = element_text(angle = 90,
                                         vjust = 0.5,
                                         hjust = 1))

Cairo(file="CE.1016.png", type="png", units="cm", width=16, height=8, pointsize=12, dpi=96); asd ;dev.off()
