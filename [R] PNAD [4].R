setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

PNAD <- read.csv("DADOS - bh.PNAD.2012.2016.csv")

PNAD$Ano <- factor(PNAD$Ano)
PNAD$Trimestre <- factor(PNAD$Trimestre)
PNAD$UF <- factor(PNAD$UF, labels = c("Minas Gerais"))
PNAD$Capital <- factor(PNAD$Capital, labels = c("Belo Horizonte"))
PNAD$V1008 <- factor(PNAD$V1008)
PNAD$v1014 <- factor(PNAD$v1014)
PNAD$v1016 <- factor(PNAD$v1016)
PNAD$v2007 <- factor(PNAD$v2007, labels = c("Masculino", "Feminino"))
#PNAD$v2009 <- as.numeric(PNAD$v2009)
PNAD$v2010 <- factor(PNAD$v2010, labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorado"))

PNAD.posest <- read.csv("Estimativas Pos-Estratificadas 2012-2016 com IR e FR06 [CI, SE, VAR].csv", row.names = 1)

####################################################################################################
######################################################################################################
########################################################################################################
########################################################                  ################################
########################################################        I         ##################################
########################################################                  ################################
########################################################################################################
######################################################################################################
####################################################################################################

PNAD.posest %>% group_by(Ano, Trimestre) %>% summarize(sum(Pop_Estimada))
PNAD.posest %>% filter(Idade == "4 e 5")
PNAD.posest$Ano.Trimestre <- paste0(PNAD.posest$Ano, ".",PNAD.posest$Trimestre)

ggplot(PNAD.posest %>% filter(Idade == "4 e 5"), aes(x = Ano.Trimestre, y = Pop_Estimada)) + 
        geom_bar(aes(fill = Idade), fill = cor1, stat = "identity", position = "dodge", width = 0.5) +
        geom_errorbar(aes(ymin = CI_L, ymax = CI_U, group = Idade), position = position_dodge(width = 0.9), width = 0.25, size = 1) +
        theme_CFAMGBH + xlab("Ano / Trimestre") + ylab("População Estimada e\nIntervalo de Confiança") +
        scale_y_continuous(breaks = seq(0, 60000, 15000),
                           label = c("0", "15.000", "30.000", "45.000", "60.000")) +
        theme(axis.text.x = element_text(angle = 90,
                                         vjust = 0.5,
                                         hjust = 1))
       
Cairo(file="PNAD.posest.png", 
      type="png",
      units="cm", 
      width=16, 
      height=8, 
      pointsize=12, 
      dpi=96)

dev.off()

###
###

options(survey.lonely.psu = "adjust")

setwd("D:/Users/joao.medeiros/Documents/PNADc")

bhPNAD.1216 <- PNAD
bhPNAD.1216$v2009b <- 1

bhPNAD.1216$Faixa_Referencia_62[bhPNAD.1216$v2009 < 4] <- "0 a 3"
bhPNAD.1216$Faixa_Referencia_62[bhPNAD.1216$v2009 == 4 | bhPNAD.1216$v2009 == 5] <- "4 e 5"
bhPNAD.1216$Faixa_Referencia_62[bhPNAD.1216$v2009 > 5 ] <- "6 a 104"

aaa <- as.data.frame(NULL)
aab <- as.data.frame(NULL)

for (i in 2012:2016) {
        
        for (j in 1:4) {
                
                spd <- svydesign(ids = ~UPA,
                                 strata = ~Estrato,
                                 weights = ~v1028,
                                 data = bhPNAD.1216[bhPNAD.1216$Ano == i & bhPNAD.1216$Trimestre == j, ],
                                 nest = TRUE)
                
                aaa <- svyby(formula = ~v2009b, 
                             by = ~factor(Ano) + ~factor(Trimestre) + ~factor(Faixa_Referencia_62), 
                             design = spd, 
                             FUN = svytotal, 
                             vartype = c("se","ci","ci","cv","cvpct","var"),
                             level = 0.95)
                
                aaa <- as.data.frame(aaa)
                
                aab <- rbind(aab, aaa)
                
                print(paste(j, i, sep = ", "))
                
        }
}

colnames(aab) <- c("Ano", "Trimestre", "Idade", "Estimativa", colnames(aab)[5:10])
aab %>% filter(factor(Faixa_Referencia_62) == "4 e 5")

write.csv2(aab %>% filter(Idade == "4 e 5"), "estim.csv")

table(bhPNAD.1216$Faixa_Referencia_62[bhPNAD.1216$Ano == 2016])

### Matricula em EI, nao ta pronto

spd <- svydesign(ids = ~UPA,
                 strata = ~Estrato,
                 weights = ~v1028,
                 data = PNAD[PNAD$Ano == 2016 & PNAD$Trimestre == 4, ],
                 nest = TRUE)

aaa <- svyby(formula = ~v2009b, 
             by = ~factor(Ano) + ~factor(Trimestre) + ~factor(Faixa_Referencia_62), 
             design = spd, 
             FUN = svytotal, 
             vartype = c("se","ci","ci","cv","cvpct","var"),
             level = 0.95)

### Agregar por ano
bhPNAD.1216.teste <- bhPNAD.1216
bhPNAD.1216.teste$v1028b <- bhPNAD.1216.teste$v1028/4

aaa <- as.data.frame(NULL)
aab <- as.data.frame(NULL)

for (i in 2012:2016) {
                
        spd <- svydesign(ids = ~UPA,
                         strata = ~Estrato,
                         weights = ~v1028b,
                         data = bhPNAD.1216.teste[bhPNAD.1216.teste$Ano == i,],
                         nest = TRUE)
        
        aaa <- svyby(formula = ~v2009b, 
                     by = ~factor(Ano) + ~factor(Faixa_Referencia_62), 
                     design = spd, 
                     FUN = svytotal, 
                     vartype = c("se","ci","ci","cv","cvpct","var"),
                     level = 0.95)
        
        aaa <- as.data.frame(aaa)
        
        aab <- rbind(aab, aaa)
        
        print(paste(j, i, sep = ", "))
                
}

colnames(aab) <- c("Ano", "Idade", "Estimativa", colnames(aab)[4:9])
write.csv2(aab %>% filter(Idade == "4 e 5"), "estim.pesodividido4.csv")
