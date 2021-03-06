####################
#### Preparando #####
####################

setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

CD2010 <- read.csv2("BH_2010.csv")
CD2000 <- read.csv2("BH_2000.csv")

CD2010$Frequenta_Curso_Bin <- ifelse(is.na(CD2010$Curso_Frequenta), 0, 1)
CD2000$Frequenta_Curso_Bin <- ifelse(is.na(CD2000$Curso_Frequenta), 0, 1)

CD2010$Frequenta_Curso_Bin2 <- factor(CD2010$Frequenta_Curso_Bin, levels = c(0,1), labels = c("0 N�o frequenta", "1 Frequenta"))
CD2000$Frequenta_Curso_Bin2 <- factor(CD2000$Frequenta_Curso_Bin, levels = c(0,1), labels = c("0 N�o frequenta", "1 Frequenta"))

CD2010 <- CD2010 %>%  dplyr::filter(Faixa_Etaria %in% "4 a 5"); CD2000 <- CD2000 %>%  dplyr::filter(Faixa_Etaria %in% "4 a 5")

CD2010 <- CD2010 %>%  dplyr::filter(!Instrucao_Responsavel_UD2 %in% "4 N�o determinado", !Composicao_Familiar_NOVA %in% "2 Casal sem crian�as", !Instrucao_Responsavel_UD2 == is.na(Instrucao_Responsavel_UD))

CD2010 <- droplevels(CD2010); CD2000 <- droplevels(CD2000)

CD2000$Ano <- "2000"
CD2010$Ano <- "2010"

CD2000b <- CD2000 %>% dplyr::select(Ano, Estado, Munic�pio, Controle, Frequenta_Curso_Bin2, Instrucao_Responsavel_UD , Idade_Responsavel_UD, Idade_Responsavel_UD_Quad, N_Hab_UD, Renda_Domiciliar_Per_Capita_SM, Ocupacao_Responsavel_UD_NOVA, Composicao_Familiar_NOVA, Idade, Sexo, Raca_Nova, Regional)
CD2010b <- CD2010 %>% dplyr::select(Ano, Estado, Munic�pio, Controle, Frequenta_Curso_Bin2, Instrucao_Responsavel_UD2, Idade_Responsavel_UD, Idade_Responsavel_UD_Quad, N_Hab_UD, Renda_Domiciliar_Per_Capita_SM, Ocupacao_Responsavel_UD_NOVA, Composicao_Familiar_NOVA, Idade, Sexo, Raca_Nova, Regional)

colnames(CD2010b) <- colnames(CD2000b)

CD <- rbind(CD2000b, CD2010b); rm(CD2000, CD2000b, CD2010, CD2010b)

CD$Munic�pio <- 6200

CD$Idade <- factor(CD$Idade, levels = c(4, 5), labels = c("4 anos", "5 anos"))

write.csv2(CD, "CD_BH_2000_2010.csv", row.names = FALSE); rm(CD)


####################
### Preparando II ###
####################

setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

CD <- read.csv2("CD_BH_2000_2010.csv")

CD$Composicao_Familiar_NOVA2 <- CD$Composicao_Familiar_NOVA
levels(CD$Composicao_Familiar_NOVA2) <- c("3 Outros", "1 Casal com crian�as", "2 Mulher solteira com crian�as", "0 Homem solteiro com crian�as")
CD$Composicao_Familiar_NOVA2 <- factor(CD$Composicao_Familiar_NOVA2, levels(CD$Composicao_Familiar_NOVA2)[c(4, 2, 3, 1)])
CD$Composicao_Familiar_NOVA <- CD$Composicao_Familiar_NOVA2

CD$Regional_NOVA <- CD$Regional
levels(CD$Regional_NOVA) <- c("1 Barreiro", "0 Centro-Sul", "2 Leste", "3 Nordeste", "4 Noroeste", "5 Norte", "6 Oeste", "7 Pampulha", "8 Venda Nova")
CD$Regional_NOVA <- factor(CD$Regional_NOVA, levels(CD$Regional_NOVA)[c(2, 1, 3, 4, 5, 6, 7, 8, 9)])
CD$Regional<- CD$Regional_NOVA

CD <- CD[,1:16]

CD <- CD[complete.cases(CD),]
sum(is.na(CD))

write.csv2(CD, "CD_BH_2000_2010_2.csv", row.names = FALSE); rm(CD)

##################
####  Modelo  #####
##################
 
setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

CD <- read.csv2("CD_BH_2000_2010_2.csv")

glm_2000 <- glm(Frequenta_Curso_Bin2 ~ Instrucao_Responsavel_UD + Idade_Responsavel_UD + Idade_Responsavel_UD_Quad + N_Hab_UD + Renda_Domiciliar_Per_Capita_SM  + Ocupacao_Responsavel_UD_NOVA + Composicao_Familiar_NOVA + Idade + Sexo + Raca_Nova + Regional, 
            data = CD %>% filter(Ano == "2000"), 
            family = binomial(link = "probit"))

glm_2010 <- glm(Frequenta_Curso_Bin2 ~ Instrucao_Responsavel_UD + Idade_Responsavel_UD + Idade_Responsavel_UD_Quad + N_Hab_UD + Renda_Domiciliar_Per_Capita_SM  + Ocupacao_Responsavel_UD_NOVA + Composicao_Familiar_NOVA + Idade + Sexo + Raca_Nova + Regional, 
                data = CD %>% filter(Ano == "2010"), 
                family = binomial(link = "probit"))

summary(glm_2000)
summary(glm_2010)

write.csv2(as.data.frame(summary(glm_2000)$coefficients), "glm_probit_2000.csv")
write.csv2(as.data.frame(summary(glm_2010)$coefficients), "glm_probit_2010.csv")

##

predicted_2000 <- predict(glm_2000, type = "response")
perc_2000 <- data.frame(censo = CD$Frequenta_Curso_Bin2[CD$Ano == "2000"], predicted = round(glm_2000$fitted.values))
xtabs(~ predicted + censo, data = perc_2000)/7539

predicted_2010 <- predict(glm_2010, type = "response")
perc_2010 <- data.frame(censo = CD$Frequenta_Curso_Bin2[CD$Ano == "2010"], predicted = round(glm_2010$fitted.values))
xtabs(~ predicted + censo, data = perc_2010)/2663

library(pscl)
pR2(glm_2000)
pR2(glm_2010)


##################
#### Gr�ficos #####
##################

setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

CD <- read.csv2("CD_BH_2000_2010.csv")

#### Renda per capita familiar em SM

ggplot(data = CD, aes(y = Renda_Domiciliar_Per_Capita_SM, x = factor(Ano), fill = Frequenta_Curso_Bin2)) +
        geom_boxplot(colour = "black", coef = 6) + theme_CFAMGBH + scale_fill_brewer(palette = "Accent") +
        scale_y_continuous(limits = c(0, 8))

ggplot(data = CD, aes(y = Renda_Domiciliar_Per_Capita_SM, x = factor(Ano), fill = Frequenta_Curso_Bin2)) +
        geom_boxplot(outlier.colour = NULL, aes(colour=Frequenta_Curso_Bin2, fill=Frequenta_Curso_Bin2), width = 1, coef = 6) +
        stat_summary(geom = "crossbar", width=0.65, fatten=1.5, color="white", position = position_dodge(width = 0.75),
                     fun.data = function(x){return(c(y=median(x), 
                                                     ymin=median(x), 
                                                     ymax=median(x)))}) + 
        theme_CFAMGBH + scale_fill_manual(values = c(cor1, cor2)) + scale_color_manual(values = c(cor1, cor2)) +
        coord_cartesian(ylim = c(0,5)) + theme(legend.position="right") +
        ylab("Renda Domiciliar Per Capita\nem Sal�rios M�nimos") + xlab(NULL)


#### N_Hab

ggplot(data = CD , aes(y = N_Hab_UD, x = factor(Ano), fill = Frequenta_Curso_Bin2)) +
        geom_boxplot(colour = "black") + theme_CFAMGBH + scale_fill_brewer(palette = "Accent") +
        scale_y_continuous(limits = c(2, 10))

ggplot(data = CD, aes(y = N_Hab_UD, x = factor(Ano), fill = Frequenta_Curso_Bin2)) +
        geom_boxplot(outlier.colour = NULL, aes(colour=Frequenta_Curso_Bin2, fill=Frequenta_Curso_Bin2), width = 1) +
        stat_summary(geom = "crossbar", width=0.65, fatten=2.5, color="white", position = position_dodge(width = 0.75),
                     fun.data = function(x){return(c(y=median(x), 
                                                     ymin=median(x), 
                                                     ymax=median(x)))}) + 
        theme_CFAMGBH + scale_fill_manual(values = c(cor1, cor2)) + scale_color_manual(values = c(cor1, cor2)) +
        coord_cartesian(ylim = c(2, 8)) + theme(legend.position="right") +
        ylab("N�mero de Habitantes da\nUnidade Domiciliar") + xlab(NULL)


#### Idade_Responsavel

ggplot(data = CD, aes(y = Idade_Responsavel_UD, x = factor(Ano), fill = Frequenta_Curso_Bin2)) +
        geom_boxplot(colour = "black") + theme_CFAMGBH + scale_fill_brewer(palette = "Accent") +
        scale_y_continuous(limits = c(15, 65))

ggplot(data = CD, aes(y = Idade_Responsavel_UD, x = factor(Ano), fill = Frequenta_Curso_Bin2)) +
        geom_boxplot(outlier.colour = NULL, aes(colour=Frequenta_Curso_Bin2, fill=Frequenta_Curso_Bin2), width = 1) +
        stat_summary(geom = "crossbar", width=0.65, fatten=1.5, color="white", position = position_dodge(width = 0.75),
                     fun.data = function(x){return(c(y=median(x), 
                                                     ymin=median(x), 
                                                     ymax=median(x)))}) + 
        theme_CFAMGBH + scale_fill_manual(values = c(cor1, cor2)) + scale_color_manual(values = c(cor1, cor2)) +
        coord_cartesian(ylim = c(20,60)) + theme(legend.position="right") +
        ylab("Idade do Respons�vel pela\nUnidade Domiciliar") + xlab(NULL)

#### Regional

Regional <- CD %>% group_by(Ano, Regional) %>% summarize("Total" = n(), "Frequenta" = sum(Frequenta_Curso_Bin2 == "1 Frequenta"), "Propor��o" = Frequenta/Total)

ggplot(data = melt(Regional, id = c("Ano", "Regional")) %>% filter(variable == "Propor��o"), aes(fill = factor(Ano), x = Regional, y = value - 0.5)) +
        geom_bar(position = "dodge", stat = "identity") + theme_CFAMGBH + theme(legend.position="right") +
        scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4), label = c("50%", "60%", "70%", "80%", "90%")) +
        scale_x_discrete(labels = c("Barreiro", "Centro\nSul", "Leste", "Nordeste", "Noroeste", "Norte","Oeste", "Pampulha", "Venda\nNova")) +
        ylab("Propor��o de Crian�as que\nFrequentam Educa��o Infantil") + xlab(NULL) +
        scale_fill_manual(values = c(cor1, cor2)) +
        theme(axis.text.x = element_text(angle = 90,
                                         vjust = 0.5,
                                         hjust = 1))

#### Instrucao

Instrucao_Responsavel_UD <- CD %>% group_by(Ano, Instrucao_Responsavel_UD) %>% summarize("Total" = n(), "Frequenta" = sum(Frequenta_Curso_Bin2 == "1 Frequenta"), "Propor��o" = Frequenta/Total)

ggplot(data = melt(Instrucao_Responsavel_UD, id = c("Ano","Instrucao_Responsavel_UD")) %>% filter(variable == "Propor��o"), aes(fill = Instrucao_Responsavel_UD, y = value - 0.4, x = factor(Ano))) +
        geom_bar(position = "dodge", stat = "identity") + theme_CFAMGBH + theme(legend.position = "right") +
        scale_y_continuous(breaks = c(0, 0.15, 0.3, 0.45, 0.6), limits = c(0, 0.6), label = c("40%", "55%", "70%", "85%", "100%")) +
        ylab("Propor��o de Crian�as que\nFrequentam Educa��o Infantil") + xlab(NULL) +
        scale_fill_manual(values = c(cor1, cor2, cor3, cor5), labels = c("Sem instru��o e fundamental incompleto", 
                                                                         "Fundamental completo e m�dio incompleto",
                                                                         "M�dio completo e superior incompleto",
                                                                         "Superior completo"))


#### Faixa Et�ria

Idade <- CD %>% group_by(Ano, Idade) %>% summarize("Total" = n(), "Frequenta" = sum(Frequenta_Curso_Bin2 == "1 Frequenta"), "Propor��o" = Frequenta/Total)

ggplot(data = melt(Idade, id = c("Ano", "Idade")) %>% filter(variable == "Propor��o"), aes(x = factor(Ano), fill = Idade, y = value - 0.4)) +
        geom_bar(position = "dodge", stat = "identity") + theme_CFAMGBH + theme(legend.position="right") +
        scale_y_continuous(breaks = c(0, 0.15, 0.3, 0.45, 0.6), limits = c(0, 0.6), label = c("40%", "55%", "70%", "85%", "100%")) +
        ylab("Propor��o de Crian�as que\nFrequentam Educa��o Infantil") + xlab(NULL) +
        scale_fill_manual(values = c(cor1, cor2))


#### Ra�a

Raca <- CD %>% group_by(Ano,Raca_Nova) %>% summarize("Total" = n(), "Frequenta" = sum(Frequenta_Curso_Bin2 == "1 Frequenta"), "Propor��o" = Frequenta/Total)

ggplot(data = melt(Raca, id = c("Ano", "Raca_Nova")) %>% filter(variable == "Propor��o"), aes(x = factor(Ano), fill = Raca_Nova, y = value - 0.4)) +
        geom_bar(position = "dodge", stat = "identity") + theme_CFAMGBH + theme(legend.position="right") +
        scale_y_continuous(breaks = c(0, 0.15, 0.3, 0.45, 0.6), limits = c(0, 0.6), label = c("40%", "55%", "70%", "85%", "100%")) +
        ylab("Propor��o de Crian�as que\nFrequentam Educa��o Infantil") + xlab(NULL) +
        scale_fill_manual(values = c(cor1, cor2), labels = c("Preto, pardo, ind�gena\nou ignorado", 
                                                             "Branco ou amarelo"))

#### Ocupa��o_Responsavel_UD


Ocupa��o_Responsavel <- CD %>% group_by(Ano, Ocupacao_Responsavel_UD_NOVA) %>% summarize("Total" = n(), "Frequenta" = sum(Frequenta_Curso_Bin2 == "1 Frequenta"), "Propor��o" = Frequenta/Total)

ggplot(data = melt(Ocupa��o_Responsavel, id = c("Ano","Ocupacao_Responsavel_UD_NOVA")) %>% filter(variable == "Propor��o"), aes(fill = Ocupacao_Responsavel_UD_NOVA, y = value - 0.4, x = factor(Ano))) +
        geom_bar(position = "dodge", stat = "identity") + theme_CFAMGBH + theme(legend.position = "right") +
        scale_y_continuous(breaks = c(0, 0.15, 0.3, 0.45, 0.6), limits = c(0, 0.6), label = c("40%", "55%", "70%", "85%", "100%")) +
        ylab("Propor��o de Crian�as que\nFrequentam Educa��o Infantil") + xlab(NULL) +
        scale_fill_manual(values = c(cor1, cor2, cor3, cor4, cor5), labels = c("Outras modalidades de ocupa��o", 
                                                                               "Empregador", 
                                                                               "Empregado com carteira assinada\nou servidor p�blico", 
                                                                               "Empregado sem carteira assinada", 
                                                                               "Conta pr�pria"))

#### Composicao_Familiar

Composicao_Familiar <- CD %>% group_by(Ano, Composicao_Familiar_NOVA) %>% summarize("Total" = n(), "Frequenta" = sum(Frequenta_Curso_Bin2 == "1 Frequenta"), "Propor��o" = Frequenta/Total)

ggplot(data = melt(Composicao_Familiar, id = c("Ano","Composicao_Familiar_NOVA")) %>% filter(variable == "Propor��o"), aes(fill = Composicao_Familiar_NOVA, y = value - 0.5 , x = factor(Ano))) +
        geom_bar(position = "dodge", stat = "identity") + theme_CFAMGBH + theme(legend.position = "right") +
        scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4), limits = c(0, 0.4), label = c("50%", "60%", "70%", "80%", "90%")) +
        ylab("Propor��o de Crian�as que\nFrequentam Educa��o Infantil") + xlab(NULL) +
        scale_fill_manual(values = c(cor1, cor2, cor3, cor5), labels = c("Outras modalidades de fam�lia", 
                                                                         "Casal com crian�a(s)", 
                                                                         "Mulher solteira com crian�a(s)", 
                                                                         "Homem solteiro com crian�a(s)"))

### Sexo

Sexo <- CD %>% group_by(Ano, Sexo) %>% summarize("Total" = n(), "Frequenta" = sum(Frequenta_Curso_Bin2 == "1 Frequenta"), "Propor��o" = Frequenta/Total)

ggplot(data = melt(Sexo, id = c("Ano", "Sexo")) %>% filter(variable == "Propor��o"), aes(x = factor(Ano), fill = Sexo, y = value - 0.5)) +
        geom_bar(position = "dodge", stat = "identity") + theme_CFAMGBH + theme(legend.position="right") +
        scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4), limits = c(0, 0.4), label = c("50%", "60%", "70%", "80%", "90%")) +
        ylab("Propor��o de Crian�as que\nFrequentam Educa��o Infantil") + xlab(NULL) +
        scale_fill_manual(values = c(cor1, cor2))
