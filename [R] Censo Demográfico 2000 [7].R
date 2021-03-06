setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

dic <- openxlsx::read.xlsx("D:\\PDFs escola\\UFMG\\Monografia\\Dados\\Documentacao Censo Demogr�fico 2000\\Dicion�rio_2000.xlsx", 1, colNames = TRUE, startRow = 1)
dic <- dic %>% filter(!is.na(VARI))
dic$TAM <- as.numeric(dic$TAM)


for (i in 0:185) {
        
        if(i == 0) {
                
                dic <- openxlsx::read.xlsx("D:\\PDFs escola\\UFMG\\Monografia\\Dados\\Documentacao Censo Demogr�fico 2000\\Dicion�rio_2000.xlsx", 1, colNames = TRUE, startRow = 1)
                dic <- dic %>% filter(!is.na(VARI))
                dic$TAM <- as.numeric(dic$TAM)
                
                dic2 <- openxlsx::read.xlsx("D:\\PDFs escola\\UFMG\\Monografia\\Dados\\Documentacao Censo Demogr�fico 2000\\Dicion�rio_2000.xlsx", 2, colNames = TRUE, startRow = 1)
                dic2$TAM <- as.numeric(dic2$TAM)
                
                CD <- readLines("D:\\PDFs escola\\UFMG\\Monografia\\Dados\\MG 2000\\Pes31.txt")
                FAMI <- readLines("D:\\PDFs escola\\UFMG\\Monografia\\Dados\\MG 2000\\Fami31.txt")
                
        }
        
        if(i == 1) {
                
                CD2 <- NULL; data.frame(CD2)
                FAMI2 <- NULL; data.frame(FAMI)
                
                CD1 <- data.frame(substr(CD, dic[i,3], dic[i,3] + dic[i,4] - 1))
                FAMI1 <- data.frame(substr(FAMI, dic2[i,3], dic2[i,3] + dic2[i,2] - 1))
                
                CD2 <- CD1
                FAMI2 <- FAMI1
                print(i)
        }
        
        if(i > 1 & i < 27) {
                
                CD1 <- data.frame(substr(CD, dic[i,3], dic[i,3] + dic[i,4] - 1))
                FAMI1 <- data.frame(substr(FAMI, dic2[i,3], dic2[i,3] + dic2[i,2] - 1))
                
                CD2 <- cbind(CD2, CD1)
                FAMI2 <- cbind(FAMI2, FAMI1)
                print(i)
                
        }
        
        if(i > 26 & i < 184) {
                
                CD1 <- data.frame(substr(CD, dic[i,3], dic[i,3] + dic[i,4] - 1))
                
                CD2 <- cbind(CD2, CD1)
                
                print(i)
                
        }
        
        if(i == 184) {
                
                colnames(CD2) <- c(dic$VARI)
                colnames(FAMI2) <- c(dic2$VARI�VEL)
                
                CD <- CD2
                FAMI <- FAMI2
                
                rm(CD1, CD2, FAMI1, FAMI2)
                
                CD <- CD %>% filter(V1103 == "3106200")
                FAMI <- FAMI %>% filter(V0103 == "3106200")
                
                FAMI <- FAMI[, 1:26]
                
                CD$Sexo <- factor(CD$V0401, levels = c(1,2), labels = c("Masculino", "Feminino"))
                #CD$PesoAmostral <- as.numeric(as.character(CD$V0010))/1e+13
                CD$Ra�a <- factor(CD$V0408, levels = c(1,2,3,4,5,9), labels = c("Branca", "Preta"," Amarela", "Parda", "Ind�gena", "Ignorado"))
                
                CD$Idade <- as.numeric(as.character(CD$V4572))
                CD$Faixa_Etaria <- cut(CD$Idade, breaks = c(-0.1,3.9,5.9,134), labels = c("0 a 3", "4 a 5", "6 a 135"))
                
                CD$HistEscolarRede <- factor(CD$V0429, levels = c(1,2,3,4), labels = c("Sim, particular", "Sim, p�blica", "N�o, j� frequentou", "N�o, nunca frequentou"))
                
                CD$Curso_Frequenta <- as.numeric(as.character(CD$V0430))
                CD$Curso_Frequenta <- factor(CD$Curso_Frequenta, levels = c(1:13), labels = c("Creche", "Pr�-Escola", "Classe de Alfabetiza��o", "Alfabetiza��o de adultos", "Regular Seriado do Ensino Fundamental", "Regular N�o-Seriado do Ensino Fundamental", "Supletivo (EF ou 1� Grau)", "Regular Seriado do Ensino M�dio", "Regular N�o-Seriado do Ensino M�dio", "Supletivo (EM ou 2� Grau)", "Pr�-Vestibular","Superior de Gradua��o", "Superior de Mestrado ou Doutorado"))
                
                #CD$Curso_Frequentou <- as.numeric(as.character(CD$V0633))
                #CD$Curso_Frequentou <- factor(CD$Curso_Frequentou, levels = c(1:14), labels = c("Educa��o Infantil", "Alfabetiza��o de Jovens e Adultos", "Antigo Primario", "Antigo Ginasio", "Regular AI Ensino Fundamental", "Regular AM Ensino Fundamental", "Regular AF Ensino Fundamental", "Supletivo do EF", "Antigo Cient�fico", "Regular ou Supletivo do EM", "Gradua��o", "Especializa��o de n�vel Superior (360h)", "Mestrado", "Doutorado"))
                
                #CD$Curso_Concluido <- as.numeric(as.character(CD$V0635))
                #CD$Curso_Concluido <- factor(CD$Curso_Concluido, levels = c(1:3), labels = c("Gradua��o", "Mestrado", "Doutorado"))
                
                CD$Anos_Estudo <- as.numeric(as.character(CD$V4300))
                
                #CD$Tipo_Unidade_Domestica <- as.numeric(as.character(CD$V5030))
                #CD$Tipo_Unidade_Domestica <- factor(CD$Tipo_Unidade_Domestica, levels = c(1:3), labels = c("Unipessoal", "Pluripessoal sem parentesco", "Pluripessoal com parentesco"))
                
                #CD$Renda_Domiciliar <- as.numeric(as.character(CD$V6529))
                #CD$Renda_Domiciliar_SM <- as.numeric(as.character(CD$V6530))/100000
                #CD$Renda_Domiciliar_Per_Capita <- as.numeric(as.character(CD$V6531))/100
                #CD$Renda_Domiciliar_Per_Capita_SM <- as.numeric(as.character(CD$V6532))/100000
                
                CD$Renda_Individual <- as.numeric(as.character(CD$V4614))
                CD$Renda_Individual_SM <- as.numeric(as.character(CD$V4615))/100
                
                #CD$N_Hab_UD <- as.numeric(as.character(CD$V5060))
                
                CD$N_Familia <- as.numeric(as.character(CD$V0404))
                CD$N_Familia <- paste(CD$V0300, CD$N_Familia, sep = ".")
                
                CD$Relacao_com_Responsavel <- as.numeric(as.character(CD$V0402))
                CD$Relacao_com_Responsavel <- factor(CD$Relacao_com_Responsavel, levels = c(1:12), labels = c("Pessoa  respons�vel", "C�njuge  ou  companheiro(a)", "Filho(a) ou enteado(a)", "Pai,  m�e, sogro(a)", "Neto(a), Bisneto(a)", "Irm�o, irm�", "Outro  parente", "Agregado(a)", "Pensionista", "Empregado(a)  dom�stico(a)", "Parente  do(a)  empregado(a)  dom�stico(a)", "Individual em domic�lio coletivo"))
                
                #CD$Relacao_com_Responsavel_Neto <- ifelse((CD$Relacao_com_Responsavel == "Filho(a)  do  respons�vel  e  do  c�njuge" | CD$Relacao_com_Responsavel == "Filho(a)  somente  do  respons�vel"), "Filho do Respons�vel", CD$Relacao_com_Responsavel)
                #CD$Relacao_com_Responsavel_Neto <- factor(CD$Relacao_com_Responsavel_Neto, levels = c("1", "2", "3", "Filho do Respons�vel", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"), labels = c("Pessoa  respons�vel  pelo  domic�lio", "C�njuge  ou  companheiro(a)  de  sexo  diferente", "C�njuge ou companheiro(a) do mesmo sexo", "Filho(a) do respons�vel", "Enteado(a)", "Genro ou nora", "Pai,  m�e,  padrasto  ou  madrasta", "Sogro(a)", "Neto(a)", "Bisneto(a)", "Irm�o  ou  irm�", "Av�  ou  av�", "Outro  parente", "Agregado(a)", "Convivente", "Pensionista", "Empregado(a)  dom�stico(a)", "Parente  do(a)  empregado(a)  dom�stico(a)", "Individual em domic�lio coletivo"))
                
                #CD$Composi��o_Familiar_Prin <- as.numeric(as.character(CD$V5090))
                #CD$Composi��o_Familiar_Prin <- factor(CD$Composi��o_Familiar_Prin, levels = c(1:9), labels = c("Casal  sem  filho(s)", "Casal  sem  filho(s)  e  com  parente(s)", "Casal com filho(s)", "Casal  com  filho(s)  e  com  parente(s)", "Mulher  sem  c�njuge  com  filho(s)", "Mulher  sem  c�njuge  com  filho(s)  e  com  parente(s)", "Homem  sem  c�njuge  com  filho(s)", "Homem  sem  c�njuge  com  filho(s)  e  com  parente(s)", "Outro"))
                
                #CD$Composi��o_Familiar_Secund <- as.numeric(as.character(CD$V5100))
                #CD$Composi��o_Familiar_Secund <- factor(CD$Composi��o_Familiar_Secund, levels = c(1:3), labels = c("Casal  sem  filho(s)", "Casal com filho(s)", "Mulher  sem  c�njuge  com  filho(s)"))
                
                CD$Ocupa��o_Trabalho_Prin <- as.numeric(as.character(CD$V0447))
                CD$Ocupa��o_Trabalho_Prin <- factor(CD$Ocupa��o_Trabalho_Prin, levels = c(1:9), labels = c("Trabalhador dom�stico com carteira de trabalho assinada", "Trabalhador dom�stico sem carteira de trabalho assinada", "Empregado com carteira de trabalho assinada", "Empregado sem carteira de trabalho assinada",  "Empregador", "Conta pr�pria", "Aprendiz ou estagi�rio sem remunera��o", "N�o remunerado em ajuda a membro do domic�lio", "Trabalhador na produ��o para o pr�rio consumo"))
                
                CD1 <- CD %>% dplyr::select(V0102, V1103, AREAP, V0300, N_Familia, Sexo, Ra�a, Idade, Faixa_Etaria, Relacao_com_Responsavel, HistEscolarRede, Curso_Frequenta, Anos_Estudo, Renda_Individual, Renda_Individual_SM, Ocupa��o_Trabalho_Prin)
                colnames(CD1)[1:4] <- c("Estado", "Munic�pio", "�rea_de_Pondera��o", "Controle")
                
                CD1$�rea_de_Pondera��o <- droplevels(CD1$�rea_de_Pondera��o)
                CD1$Munic�pio <- droplevels(CD1$Munic�pio)
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

## Composi��o familiar

FAMI$Composi��o_Familiar_Prin <- as.numeric(as.character(FAMI$CODV0404_2))
FAMI$Composi��o_Familiar_Prin <- factor(FAMI$Composi��o_Familiar_Prin, levels = c(1:12), labels = c("Casal sem filhos", "Casal com filhos menores de 14 anos", "Casal com filhos de 14 anos ou mais", "Casal com filhos de idades variadas", "M�e com filhos menores de 14 anos", "M�e com filhos de 14 anos ou mais", "M�e com filhos de idades variadas", "Pai com filhos menores de 14 anos", "Pai com filhos de 14 anos ou mais", "Pai com filhos de idades variadas", "Outros tipos de fam�lias", "Morador individual"))

FAMI2 <- FAMI %>% dplyr::select(V0300, CODV0404, Composi��o_Familiar_Prin)
FAMI2 <- FAMI2 %>% filter(V0300 %in% Controle_levels)
FAMI2$V0300 <- droplevels(FAMI2$V0300)
FAMI2$CODV0404 <- ifelse(as.numeric(as.character(FAMI2$CODV0404)) == 0, as.numeric(as.character(FAMI2$CODV0404)) + 1, as.numeric(as.character(FAMI2$CODV0404)))
FAMI2$N_Familia <- paste(FAMI2$V0300, FAMI2$CODV0404, sep = ".")

CD1$N_Familia <- as.factor(CD1$N_Familia)
FAMI2$N_Familia <- as.factor(FAMI2$N_Familia)

a <- merge(CD1, FAMI2, by.x = "N_Familia", by.y = "N_Familia", all.y = T)
CD1 <- a[1:78445,]

CD1$Controle <- droplevels(CD1$Controle); CD1$Composi��o_Familiar_Prin <- droplevels(CD1$Composi��o_Familiar_Prin)
Controle_levels <- levels(CD1$Controle)

for(i in 1:17176) { ## instru��o do respons�vel
        
        if(CD1$Relacao_com_Responsavel[CD1$Controle == Controle_levels[i]] == "Individual em domic�lio coletivo") {
                
                CD1$Anos_Estudo_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- NA
                print(c(i, "NA"))
                
        }
        
        else {
                
                CD1$Anos_Estudo_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- CD1$Anos_Estudo[CD1$Relacao_com_Responsavel == "Pessoa  respons�vel" & CD1$Controle == Controle_levels[i]] 
                
                print(i)
                
        }
        
}

APCD <- read.xlsx("Area_P�nteracao_censo_2000.xlsx", colNames = F)

CD1$�rea_de_Pondera��o <- as.character(CD1$�rea_de_Pondera��o)
APCD$X1 <- as.character(APCD$X1)

CD2 <- merge(CD1, APCD, by.x = "�rea_de_Pondera��o", by.y = "X1")
CD1 <- CD2
CD1$Regional <- CD1$X2
CD1$X2 <- NULL

rm(APCD, CD2)

for(i in 1:17176) {  ## idade do respons�vel
        
        if(CD1$Relacao_com_Responsavel[CD1$Controle == Controle_levels[i]] == "Individual em domic�lio coletivo") {
                
                CD1$Instrucao_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- NA
                print(c(i, "NA"))
                
        }
        
        else {
                
                CD1$Idade_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- CD1$Idade[CD1$Relacao_com_Responsavel == "Pessoa  respons�vel" & CD1$Controle == Controle_levels[i]] 
                
                print(i)
                
        }
        
}

for(i in 1:17176) {  ## ocupa��o do respons�vel
        
        if(CD1$Relacao_com_Responsavel[CD1$Controle == Controle_levels[i]] == "Individual em domic�lio coletivo") {
                
                CD1$Ocupa��o_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- NA
                print(c(i, "NA"))
                
        }
        
        else {
                
                if(is.na(CD1$Ocupa��o_Trabalho_Prin[CD1$Controle == Controle_levels[i] & CD1$Relacao_com_Responsavel == "Pessoa  respons�vel"])) {
                        
                        CD1$Ocupa��o_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- 8
                        print(c(i, "NA"))
                        
                }
                
                
                else {
                        
                        CD1$Ocupa��o_Responsavel_UD[CD1$Controle == Controle_levels[i]] <- CD1$Ocupa��o_Trabalho_Prin[CD1$Relacao_com_Responsavel == "Pessoa  respons�vel" & CD1$Controle == Controle_levels[i]] 
                        print(i)
                        
                }
                
                
        }
        
}

CD1$Ocupa��o_Responsavel_UD <- factor(CD1$Ocupa��o_Responsavel_UD, levels = c(1:9), labels = c("Trabalhador dom�stico com carteira de trabalho assinada", "Trabalhador dom�stico sem carteira de trabalho assinada", "Empregado com carteira de trabalho assinada", "Empregado sem carteira de trabalho assinada",  "Empregador", "Conta pr�pria", "Aprendiz ou estagi�rio sem remunera��o", "N�o remunerado em ajuda a membro do domic�lio", "Trabalhador na produ��o para o pr�rio consumo"))

for(i in 1:17176) {  ## N habitantes
        
        CD1$N_Hab_UD[CD1$Controle == Controle_levels[i]] <- length(CD1$Sexo[CD1$Controle == Controle_levels[i]])
        print(i)
}

for(i in 1:17176) {  ## Renda Per Capita UD
        
        CD1$Renda_Domiciliar_SM[CD1$Controle == Controle_levels[i]] <- sum(CD1$Renda_Individual_SM[CD1$Controle == Controle_levels[i]], na.rm = T)
        
        print(i)
}


CD1$Renda_Domiciliar_Per_Capita_SM <- CD1$Renda_Domiciliar_SM/CD1$N_Hab_UD


### ajuste final


CD1$Instrucao_Responsavel_UD <- cut(CD1$Anos_Estudo_Responsavel_UD,
                                    breaks = c(-Inf, 8, 11, 15, Inf),
                                    right = FALSE,
                                    labels = c("0 Sem instrucao e fundamental incompleto", "1 Fundamental completo e m�dio incompleto", "2 M�dio completo e superior incompleto", "3 Superior completo"))

CD1$Ocupacao_Responsavel_UD_NOVA <- CD1$Ocupa��o_Responsavel_UD
levels(CD1$Ocupacao_Responsavel_UD_NOVA) <- c("2 Empregado com CLT ou RP", "3 Empregado sem CLT", "2 Empregado com CLT ou RP", "3 Empregado sem CLT", "1 Empregador", "4 Conta Pr�pria", "0 Outros", "0 Outros", "0 Outros")
CD1$Ocupacao_Responsavel_UD_NOVA <- factor(CD1$Ocupacao_Responsavel_UD_NOVA, levels(CD1$Ocupacao_Responsavel_UD_NOVA)[c(5,3,1,2,4)])

CD1$Composicao_Familiar_NOVA <- CD1$Composi��o_Familiar_Prin
levels(CD1$Composicao_Familiar_NOVA) <- c("2 Casal sem crian�as", "1 Casal com crian�as", "1 Casal com crian�as", "1 Casal com crian�as", "3 Mulher solteira com crian�as", "3 Mulher solteira com crian�as", "3 Mulher solteira com crian�as", "4 Homem solteiro com crian�as", "4 Homem solteiro com crian�as", "4 Homem solteiro com crian�as", "0 Outros")
CD1$Composicao_Familiar_NOVA <- factor(CD1$Composicao_Familiar_NOVA, levels(CD1$Composicao_Familiar_NOVA)[c(5,2,1,3,4)])

CD1$Idade_Responsavel_UD_Quad <- CD1$Idade_Responsavel_UD^2

CD1$Raca_Nova <- CD1$Ra�a
levels(CD1$Raca_Nova) <- c("1 Branco ou Amarelo", "0 Preto, pardo, ind�gena ou ignorado", "1 Branco ou Amarelo", "0 Preto, pardo, ind�gena ou ignorado", "0 Preto, pardo, ind�gena ou ignorado", "0 Preto, pardo, ind�gena ou ignorado")
CD1$Raca_Nova <- factor(CD1$Raca_Nova, levels(CD1$Raca_Nova)[c(2,1)])

zzz <- CD1 %>% dplyr::select(Ocupa��o_Responsavel_UD, Ocupacao_Responsavel_UD_NOVA, Composi��o_Familiar_Prin, Composicao_Familiar_NOVA, Ra�a, Raca_Nova)

write.csv2(CD1, "BH_2000.csv")

##################################################

setwd("D:\\PDFs escola\\UFMG\\Monografia\\Dados")

CD1 <- read.csv2("BH_2000.csv")

CD1$Frequenta_Curso_Bin <- ifelse(is.na(CD1$Curso_Frequenta), 0, 1)

### lm

#CD2 <- CD1 %>% filter(Faixa_Etaria == "4 a 5", Instrucao_Responsavel_UD2 != " 5 N�o determinado")

#lm1 <- lm(Frequenta_Curso_Bin ~ Instrucao_Responsavel_UD2 + N_Hab_UD + Renda_Domiciliar_Per_Capita, data = CD2)
#summary(lm1)


CD3 <- CD1 %>% filter(Faixa_Etaria != "6 a 135")

lm2 <- lm(Frequenta_Curso_Bin ~ Anos_Estudo_Responsavel_UD + Ocupa��o_Responsavel_UD + Composi��o_Familiar_Prin + Idade_Responsavel_UD + N_Hab_UD + Renda_Domiciliar_Per_Capita_SM + Faixa_Etaria + Ra + Regional, data = CD3)
summary(lm2)
write.csv2(as.data.frame(summary(lm2)$coefficients), "lm22000.csv")

### probit

CD4 <- CD1 %>% filter(Faixa_Etaria == "4 a 5")

CD4$Frequenta_Curso_Bin2 <- factor(CD4$Frequenta_Curso_Bin, levels = c(0,1), labels = c("N�o frequenta", "Frequenta"))

glm <- glm(Frequenta_Curso_Bin2 ~ Instrucao_Responsavel_UD + Idade_Responsavel_UD + Idade_Responsavel_UD_Quad + N_Hab_UD + Renda_Domiciliar_Per_Capita_SM  + Ocupacao_Responsavel_UD_NOVA + Composicao_Familiar_NOVA + Idade + Sexo + Raca_Nova + Regional, data = CD4, family = binomial(link = "probit"))
summary(glm)
write.csv2(as.data.frame(summary(glm)$coefficients), "glm_probit_2000.csv")

1 - pchisq(glm$deviance, glm$df.residual)

MEglm <- probitmfx(Frequenta_Curso_Bin2 ~ Instrucao_Responsavel_UD2 + Faixa_Etaria + N_Hab_UD + Renda_Domiciliar_Per_Capita_SM + Regional, data = CD4)
MEglm

### logit

glm2 <- glm(Frequenta_Curso_Bin2 ~ Anos_Estudo_Responsavel_UD + Ocupa��o_Responsavel_UD + Composi��o_Familiar_Prin + Idade_Responsavel_UD + N_Hab_UD + Renda_Domiciliar_Per_Capita_SM + Faixa_Etaria + Ra�a + Regional, data = CD4, family = binomial(link = "logit"))
summary(glm2)
write.csv2(as.data.frame(summary(glm2)$coefficients), "glm_logit_2000.csv")
