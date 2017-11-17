setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

dic <- openxlsx::read.xlsx("D:\\PDFs escola\\UFMG\\Monografia\\Dados\\Documentacao Censo Demográfico 2010\\Layout_microdados_Amostra.xlsx", 2, colNames = TRUE, startRow = 2)


for (i in 0:245) {
        
        if(i == 0) {
                
                dic <- openxlsx::read.xlsx("D:\\PDFs escola\\UFMG\\Monografia\\Dados\\Documentacao Censo Demográfico 2010\\Layout_microdados_Amostra.xlsx", 2, colNames = TRUE, startRow = 2)
                CD <- readLines("Amostra_Pessoas_31.txt")
                
        }
        
        if(i == 1) {
                
                CD2 <- NULL; data.frame(CD2)
        
                CD1 <- data.frame(substr(CD, dic[i,3], dic[i,4]))
        
                CD2 <- CD1
                print(i)
        }
        
        if(i > 1 & i < 245) {
        
                CD1 <- data.frame(substr(CD, dic[i,3], dic[i,4]))

                CD2 <- cbind(CD2, CD1)
                print(i)

        }
        
        if(i == 245) {
                
                colnames(CD2) <- c(dic$VAR)
                CD <- CD2
                rm(CD1, CD2)
                CD <- CD %>% filter(V0002 == "06200")
                
                CD$Sexo <- factor(CD$V0601, levels = c(1,2), labels = c("Masculino", "Feminino"))
                CD$PesoAmostral <- as.numeric(as.character(CD$V0010))/1e+13
                CD$Raça <- factor(CD$V0606, levels = c(1,2,3,4,5,9), labels = c("Branca", "Preta"," Amarela", "Parda", "Indígena", "Ignorado"))
                
                CD$Idade <- as.numeric(as.character(CD$V6033))
                CD$Idade[CD$Idade > 899] <- 0
                CD$Faixa_Etaria <- cut(CD$Idade, breaks = c(-0.1,3.9,5.9,134), labels = c("0 a 3", "4 a 5", "6 a 135"))
                
                CD$HistEscolarRede <- factor(CD$V0628, levels = c(1,2,3,4), labels = c("Sim, pública", "Sim, particular", "Não, já frequentou", "Não, nunca frequentou"))
                
                CD$Curso_Frequenta <- as.numeric(as.character(CD$V0629))
                CD$Curso_Frequenta <- factor(CD$Curso_Frequenta, levels = c(1:12), labels = c("Creche", "Pré-Escola", "Classe de Alfabetização", "Alfabetização de jovens e adultos", "Regular do Ensino Fundamental", "EJA Fundamental", "Regular do Ensino Médio", "EJA Ensino Médio", "Superior de Graduação", "Especialização de nível Superior (360h)", "Mestrado", "Doutorado"))
                
                CD$Curso_Frequentou <- as.numeric(as.character(CD$V0633))
                CD$Curso_Frequentou <- factor(CD$Curso_Frequentou, levels = c(1:14), labels = c("Educação Infantil", "Alfabetização de Jovens e Adultos", "Antigo Primario", "Antigo Ginasio", "Regular AI Ensino Fundamental", "Regular AM Ensino Fundamental", "Regular AF Ensino Fundamental", "Supletivo do EF", "Antigo Científico", "Regular ou Supletivo do EM", "Graduação", "Especialização de nível Superior (360h)", "Mestrado", "Doutorado"))
                
                CD$Curso_Concluido <- as.numeric(as.character(CD$V0635))
                CD$Curso_Concluido <- factor(CD$Curso_Concluido, levels = c(1:3), labels = c("Graduação", "Mestrado", "Doutorado"))
                
                CD$Nivel_Instrucao <- as.numeric(as.character(CD$V6400))
                CD$Nivel_Instrucao <- factor(CD$Nivel_Instrucao, levels = c(1:5), labels = c("0 Sem instrucao e fundamental incompleto", "1 Fundamental completo e médio incompleto", "2 Médio completo e superior incompleto", "3 Superior completo", "4 Não determinado"))
                
                CD$Tipo_Unidade_Domestica <- as.numeric(as.character(CD$V5030))
                CD$Tipo_Unidade_Domestica <- factor(CD$Tipo_Unidade_Domestica, levels = c(1:3), labels = c("Unipessoal", "Pluripessoal sem parentesco", "Pluripessoal com parentesco"))
                
                CD$Renda_Domiciliar <- as.numeric(as.character(CD$V6529))
                CD$Renda_Domiciliar_SM <- as.numeric(as.character(CD$V6530))/100000
                CD$Renda_Domiciliar_Per_Capita <- as.numeric(as.character(CD$V6531))/100
                CD$Renda_Domiciliar_Per_Capita_SM <- as.numeric(as.character(CD$V6532))/100000
                
                CD$N_Hab_UD <- as.numeric(as.character(CD$V5060))
                
                CD$N_Familia <- as.numeric(as.character(CD$V5020))
                CD$N_Familia <- paste(CD$V0300, CD$N_Familia, sep = ".")
                
                CD$Relacao_com_Responsavel <- as.numeric(as.character(CD$V0502))
                CD$Relacao_com_Responsavel <- factor(CD$Relacao_com_Responsavel, levels = c(1:20), labels = c("Pessoa  responsável  pelo  domicílio", "Cônjuge  ou  companheiro(a)  de  sexo  diferente", "Cônjuge ou companheiro(a) do mesmo sexo", "Filho(a)  do  responsável  e  do  cônjuge", "Filho(a)  somente  do  responsável", "Enteado(a)", "Genro ou nora", "Pai,  mãe,  padrasto  ou  madrasta", "Sogro(a)", "Neto(a)", "Bisneto(a)", "Irmão  ou  irmã", "Avô  ou  avó", "Outro  parente", "Agregado(a)", "Convivente", "Pensionista", "Empregado(a)  doméstico(a)", "Parente  do(a)  empregado(a)  doméstico(a)", "Individual em domicílio coletivo"))
                
                CD$Relacao_com_Responsavel_Neto <- ifelse((CD$Relacao_com_Responsavel == "Filho(a)  do  responsável  e  do  cônjuge" | CD$Relacao_com_Responsavel == "Filho(a)  somente  do  responsável"), "Filho do Responsável", CD$Relacao_com_Responsavel)
                CD$Relacao_com_Responsavel_Neto <- factor(CD$Relacao_com_Responsavel_Neto, levels = c("1", "2", "3", "Filho do Responsável", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"), labels = c("Pessoa  responsável  pelo  domicílio", "Cônjuge  ou  companheiro(a)  de  sexo  diferente", "Cônjuge ou companheiro(a) do mesmo sexo", "Filho(a) do responsável", "Enteado(a)", "Genro ou nora", "Pai,  mãe,  padrasto  ou  madrasta", "Sogro(a)", "Neto(a)", "Bisneto(a)", "Irmão  ou  irmã", "Avô  ou  avó", "Outro  parente", "Agregado(a)", "Convivente", "Pensionista", "Empregado(a)  doméstico(a)", "Parente  do(a)  empregado(a)  doméstico(a)", "Individual em domicílio coletivo"))
                
                CD$Composição_Familiar_Prin <- as.numeric(as.character(CD$V5090))
                CD$Composição_Familiar_Prin <- factor(CD$Composição_Familiar_Prin, levels = c(1:9), labels = c("Casal  sem  filho(s)", "Casal  sem  filho(s)  e  com  parente(s)", "Casal com filho(s)", "Casal  com  filho(s)  e  com  parente(s)", "Mulher  sem  cônjuge  com  filho(s)", "Mulher  sem  cônjuge  com  filho(s)  e  com  parente(s)", "Homem  sem  cônjuge  com  filho(s)", "Homem  sem  cônjuge  com  filho(s)  e  com  parente(s)", "Outro"))
                
                CD$Composição_Familiar_Secund <- as.numeric(as.character(CD$V5100))
                CD$Composição_Familiar_Secund <- factor(CD$Composição_Familiar_Secund, levels = c(1:3), labels = c("Casal  sem  filho(s)", "Casal com filho(s)", "Mulher  sem  cônjuge  com  filho(s)"))
                
                CD$Ocupação_Trabalho_Prin <- as.numeric(as.character(CD$V0648))
                CD$Ocupação_Trabalho_Prin <- factor(CD$Ocupação_Trabalho_Prin, levels = c(1:7), labels = c("Empregado com carteira de trabalho assinada", "Militar do Exército, Marinha, Aeronáutica, Polícia Militar ou Corpo de Bombeiros", "Empregado pelo Regime Jurídico dos Funcionários Públicos", "Empregado sem carteira de trabalho assinada", "Conta própria", "Empregador", "Não remunerado"))
                
                CD1 <- CD %>% dplyr::select(1:4, N_Familia, Sexo, PesoAmostral, Raça, Idade, Faixa_Etaria, N_Hab_UD, Relacao_com_Responsavel, Relacao_com_Responsavel_Neto, Tipo_Unidade_Domestica, HistEscolarRede, Curso_Frequenta, Curso_Frequentou, Curso_Concluido, Nivel_Instrucao, Renda_Domiciliar, Renda_Domiciliar_SM, Renda_Domiciliar_Per_Capita, Renda_Domiciliar_Per_Capita_SM, Composição_Familiar_Prin, Composição_Familiar_Secund, Ocupação_Trabalho_Prin)
                colnames(CD1)[1:4] <- c("Estado", "Município", "Área_de_Ponderação", "Controle")
                
                CD1$Área_de_Ponderação <- droplevels(CD1$Área_de_Ponderação)
                CD1$Município <- droplevels(CD1$Município)
                CD1$Controle <- droplevels(CD1$Controle)
                CD1$Eins <- 1
                
                Controle_FE <- CD1$Controle[CD1$Faixa_Etaria != "6 a 135"]
                CD1 <- CD1 %>% filter(Controle %in% Controle_FE)
                CD1$Controle <- droplevels(CD1$Controle)
                
                print("FIM")
                
        }

}


### treta


Controle_levels <- levels(CD1$Controle)

for(i in 1:6488) { ## instrução do responsável
        
        if(CD1$Relacao_com_Responsavel[CD1$Controle == Controle_levels[i]] == "Individual em domicílio coletivo") {
                
                CD1$Instrucao_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- NA
                print(c(i, "NA"))
                
        }

        else {
                
                CD1$Instrucao_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- CD1$Nivel_Instrucao[CD1$Relacao_com_Responsavel == "Pessoa  responsável  pelo  domicílio" & CD1$Controle == Controle_levels[i]] 
        
                print(i)
                
        }

}


CD1$Instrucao_Responsavel_UD2 <- factor(CD1$Instrucao_Responsavel_UD, levels = c(1:5), labels = c("0 Sem instrucao e fundamental incompleto", "1 Fundamental completo e médio incompleto", "2 Médio completo e superior incompleto", "3 Superior completo", "4 Não determinado"))

CD1$Composição_Familiar_Y <- ifelse(is.na(CD1$Composição_Familiar_Prin), as.numeric(CD1$Composição_Familiar_Secund) + 9, as.numeric(CD1$Composição_Familiar_Prin))
CD1$Composição_Familiar_Y <- factor(CD1$Composição_Familiar_Y, levels = c(1:12), labels = c("Casal  sem  filho(s)", "Casal  sem  filho(s)  e  com  parente(s)", "Casal com filho(s)", "Casal  com  filho(s)  e  com  parente(s)", "Mulher  sem  cônjuge  com  filho(s)", "Mulher  sem  cônjuge  com  filho(s)  e  com  parente(s)", "Homem  sem  cônjuge  com  filho(s)", "Homem  sem  cônjuge  com  filho(s)  e  com  parente(s)", "Outro", "Sec. Casal  sem  filho(s)", "Casal com filho(s)", "Mulher  sem  cônjuge  com  filho(s)"))
CD1$Composição_Familiar_Y <- droplevels(CD1$Composição_Familiar_Y)
###

APCD <- read.dbf("Area_ponderacao_censo_2010.dbf")
APCD <- APCD$dbf
APCD <- APCD %>% arrange(COD_AP)

CD1$Área_de_Ponderação2 <- factor(CD1$Área_de_Ponderação, label = c(levels(APCD$NOME_AP))); rm(APCD)

CD1$Regional <- NA
CD1$Regional[grep("^B", CD1$Área_de_Ponderação2)] <- "Barreiro"
CD1$Regional[grep("^CS", CD1$Área_de_Ponderação2)] <- "Centro-Sul"
CD1$Regional[grep("^L", CD1$Área_de_Ponderação2)] <- "Leste"
CD1$Regional[grep("^N[^OE]", CD1$Área_de_Ponderação2)] <- "Norte"
CD1$Regional[grep("^NE", CD1$Área_de_Ponderação2)] <- "Nordeste"
CD1$Regional[grep("^NO", CD1$Área_de_Ponderação2)] <- "Noroeste"
CD1$Regional[grep("^O", CD1$Área_de_Ponderação2)] <- "Oeste"
CD1$Regional[grep("^P", CD1$Área_de_Ponderação2)] <- "Pampulha"
CD1$Regional[grep("^V", CD1$Área_de_Ponderação2)] <- "Venda Nova"

CD1$Regional <- factor(CD1$Regional)


for(i in 1:6488) {  ## idade do responsável
        
        if(CD1$Relacao_com_Responsavel[CD1$Controle == Controle_levels[i]] == "Individual em domicílio coletivo") {
                
                CD1$Idade_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- NA
                print(c(i, "NA"))
                
        }
        
        else {
                
                CD1$Idade_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- CD1$Idade[CD1$Relacao_com_Responsavel == "Pessoa  responsável  pelo  domicílio" & CD1$Controle == Controle_levels[i]] 
                
                print(i)
                
        }
        
}

for(i in 1:6488) {  ## ocupação do responsável
        
        if(CD1$Relacao_com_Responsavel[CD1$Controle == Controle_levels[i]] == "Individual em domicílio coletivo") {
                
                CD1$Ocupação_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- NA
                print(c(i, "NA"))
                
        }
        
        else {
                
                if(is.na(CD1$Ocupação_Trabalho_Prin[CD1$Controle == Controle_levels[i] & CD1$Relacao_com_Responsavel == "Pessoa  responsável  pelo  domicílio"])) {
                        
                        CD1$Ocupação_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- 8
                        print(c(i, "NA"))
                        
                }
                
                
                else {
                        
                        CD1$Ocupação_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- CD1$Ocupação_Trabalho_Prin[CD1$Relacao_com_Responsavel == "Pessoa  responsável  pelo  domicílio" & CD1$Controle == Controle_levels[i]] 
                        print(i)
                
                }
                
                
        }
        
}

CD1$Ocupação_Responsavel_UD <- factor(CD1$Ocupação_Responsavel_UD, levels = c(1:8), labels = c("Empregado com carteira de trabalho assinada", "Militar do Exército, Marinha, Aeronáutica, Polícia Militar ou Corpo de Bombeiros", "Empregado pelo Regime Jurídico dos Funcionários Públicos", "Empregado sem carteira de trabalho assinada", "Conta própria", "Empregador", "Não remunerado", "Não trabalha"))

##

CD1$Ocupacao_Responsavel_UD_NOVA <- CD1$Ocupação_Responsavel_UD
levels(CD1$Ocupacao_Responsavel_UD_NOVA) <- c("2 Empregado com CLT ou RP", "2 Empregado com CLT ou RP", "2 Empregado com CLT ou RP", "3 Empregado sem CLT", "4 Conta Própria", "1 Empregador", "0 Outros", "0 Outros")
CD1$Ocupacao_Responsavel_UD_NOVA <- factor(CD1$Ocupacao_Responsavel_UD_NOVA, levels(CD1$Ocupacao_Responsavel_UD_NOVA)[c(5,4,1,2,3)])

CD1$Composicao_Familiar_NOVA <- CD1$Composição_Familiar_Y
levels(CD1$Composicao_Familiar_NOVA) <- c("2 Casal sem crianças", "1 Casal com crianças", "1 Casal com crianças", "1 Casal com crianças", "3 Mulher solteira com crianças", "3 Mulher solteira com crianças", "4 Homem solteiro com crianças", "4 Homem solteiro com crianças", "0 Outros", "0 Outros")
CD1$Composicao_Familiar_NOVA <- factor(CD1$Composicao_Familiar_NOVA, levels(CD1$Composicao_Familiar_NOVA)[c(5,2,1,3,4)])

CD1$Idade_Responsavel_UD_Quad <- CD1$Idade_Responsavel_UD^2

CD1$Raca_Nova <- CD1$Raça
levels(CD1$Raca_Nova) <- c("1 Branco ou Amarelo", "0 Preto, pardo, indígena ou ignorado","1 Branco ou Amarelo", "0 Preto, pardo, indígena ou ignorado", "0 Preto, pardo, indígena ou ignorado", "0 Preto, pardo, indígena ou ignorado")
CD1$Raca_Nova <- factor(CD1$Raca_Nova, levels(CD1$Raca_Nova)[c(2,1)])

zzz <- CD1 %>% dplyr::select(Ocupação_Responsavel_UD, Ocupacao_Responsavel_UD_NOVA, Composição_Familiar_Y, Composicao_Familiar_NOVA, Raça, Raca_Nova)

write.csv2(CD1, "BH_2010.csv")

###

setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

CD1 <- read.csv2("BH_2010.csv")

CD1$Frequenta_Curso_Bin <- ifelse(is.na(CD1$Curso_Frequenta), 0, 1)

### lm

CD2 <- CD1 %>% filter(Faixa_Etaria == "4 a 5", Instrucao_Responsavel_UD2 != "4 Não determinado")

lm1 <- lm(Frequenta_Curso_Bin ~ Instrucao_Responsavel_UD2 + N_Hab_UD + Renda_Domiciliar_Per_Capita, data = CD2)
summary(lm1)


CD3 <- CD1 %>% filter(Faixa_Etaria != "6 a 135", Instrucao_Responsavel_UD2 != "4 Não determinado")

lm2 <- lm(Frequenta_Curso_Bin ~ Instrucao_Responsavel_UD2 + Idade_Responsavel_UD + N_Hab_UD + Renda_Domiciliar_Per_Capita_SM + Ocupação_Responsavel_UD + Composição_Familiar_Y + Faixa_Etaria + Raça + Sexo + Regional, data = CD3)
summary(lm2)
write.csv2(as.data.frame(summary(lm2)$coefficients), "lm2.csv")

### probit

CD4 <- CD1 %>% filter(Faixa_Etaria == "4 a 5", Instrucao_Responsavel_UD2 != "4 Não determinado")

CD4$Frequenta_Curso_Bin2 <- factor(CD4$Frequenta_Curso_Bin, levels = c(0,1), labels = c("Não frequenta", "Frequenta"))

glm <- glm(Frequenta_Curso_Bin2 ~ Instrucao_Responsavel_UD2 + Idade_Responsavel_UD + Idade_Responsavel_UD_Quad + N_Hab_UD + Renda_Domiciliar_Per_Capita_SM  + Ocupacao_Responsavel_UD_NOVA + Composicao_Familiar_NOVA + Idade + Sexo + Raca_Nova + Regional, data = CD4, family = binomial(link = "probit"))
summary(glm)
write.csv2(as.data.frame(summary(glm)$coefficients), "glm_probit_2010.csv")

1 - pchisq(glm$deviance, glm$df.residual)

MEglm <- probitmfx(Frequenta_Curso_Bin2 ~ Instrucao_Responsavel_UD2 + Faixa_Etaria + N_Hab_UD + Renda_Domiciliar_Per_Capita_SM + Regional, data = CD4)
MEglm

### logit

glm2 <- glm(Frequenta_Curso_Bin2 ~ Instrucao_Responsavel_UD2 + Idade_Responsavel_UD + Ocupação_Responsavel_UD + Composição_Familiar_X + Faixa_Etaria + Raça + N_Hab_UD + Renda_Domiciliar_Per_Capita_SM + Regional, data = CD4, family = binomial(link = "logit"))
summary(glm2)
write.csv2(as.data.frame(summary(glm2)$coefficients), "glm2.csv")

#####################################
######### Extrapolação ###############
#####################################

setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

CD1 <- read.csv2("BH_2010.csv")

options(survey.lonely.psu = "adjust")

CD.design <- svydesign(data = CD1,
                       ids = ~0,
                       strata = NULL,
                       weights = ~PesoAmostral)

## Pop. Total

svyby(formula = ~Eins,
      by = ~factor(Faixa_Etaria),
      design = CD.design,
      FUN = svytotal,
      vartype = c("se","ci","ci","cv","cvpct","var"),
      level = 0.95)

## Pop. em EI

svyby(formula = ~Frequenta_Curso_Bin,
      by = ~factor(Faixa_Etaria),
      design = CD.design,
      FUN = svytotal,
      vartype = c("se","ci","ci","cv","cvpct","var"),
      level = 0.95)
