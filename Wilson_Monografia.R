# ---------------------------------------------------------------------------
# ANALISE DOS FACTORES ASSOCIADOS A VIOLENCIA
#  DOMESTICA CONTRA MULHERES EM MOCAMBIQUE
# Autor: Wilson Dinis
# Dados: IDS 2022-2023
# --------------------------------------------------------------------------

#  Instalar e carregar pacotes necessários
options(repos = c(CRAN = "https://cloud.r-project.org/"))
install.packages(c("R.oo", "R.methodsS3", "R.utils"))

install.packages("openxlsx")
install.packages("patchwork")
install.packages("haven")
install.packages("dplyr")
install.packages("survey")
install.packages("labelled")
install.packages("ggplot2")
install.packages("pROC")
install.packages("car")
install.packages("car", dependencies = TRUE)
install.packages("performance")

library(performance)
library(car)
library(openxlsx)
library(haven)
library(labelled)
library(survey)
library(dplyr)
library(ggplot2)
library(patchwork) 


setwd("C:/Users/Administrator/Desktop/DHS")

# ===================== BASE DE DADOS ===================== #
IR_data <- read_dta("MZIR81FL.DTA")
IR_data <- IR_data %>% rename_with(toupper)



# DEFINICAO DAS VARIAVEIS DEPENDENTES
IR_data <- IR_data %>%
  filter(V044 == 1) %>%  # apenas mulheres elegiveis ao VPI
  mutate(
    across(c(D103A:D103C, D105A:D105K), ~ifelse(is.na(.x), 0, .x)) 
  ) %>%
  mutate(
    violencia_emocional = ifelse(D103A > 0 | D103B > 0 | D103C > 0, 1, 0),
    
    violencia_fisica = ifelse(D105A > 0 | D105B > 0 | D105C > 0 |
                                D105D > 0 | D105E > 0 | D105F > 0 |
                                D105G > 0 | D105J > 0, 1, 0),
    
    violencia_sexual = ifelse(D105H > 0 | D105I > 0 | D105K > 0, 1, 0),
    
    vd_total = ifelse(violencia_emocional == 1 |
                        violencia_fisica == 1 |
                        violencia_sexual == 1, 1, 0)
  )


#============================================================================#

# RECODIFICACAO DE VARIAVEIS DE ESTUDO

IR_data<- IR_data |>
  mutate(             
    IDADE_ACTUAL_MULHER = case_when(as.numeric(V012) < 18 ~ "1_<18",
                                    between(as.numeric(V012), 18,29) ~ "2_18-29",
                                    between(as.numeric(V012), 30,39) ~ "3_30-39",
                                    as.numeric(V012) > 39 ~ "4_40+" ),  # IDADE ACTUAL DA MULHER
    
    IDADE_DO_PARCEIRO = case_when(as.numeric(V730) < 18 ~ "1_<18",
                                  between(as.numeric(V730), 18,29) ~ "2_18-29",
                                  between(as.numeric(V730), 30,39) ~ "3_30-39",
                                  as.numeric(V730) > 39 ~ "4_40+" ),  # IDADE ACTUAL DO Parceiro
    
    RELIGIAO = case_when(
      V130 == 0 ~ "0_Sem religiao",
      V130 == 1 ~ "1_Catolica",
      V130 == 2 ~ "2_Islamica",
      V130 == 3 ~ "3_Zione",
      V130 == 4 ~ "4_Evangelica/Pentecostal",
      V130 == 5 ~ "5_Anglicana",
      V130 == 6 ~ "6_Outra",
      TRUE ~ NA_character_
    ),
    
    NUMERO_DE_CRIANCAS = case_when(V218 == 0 ~ "1_NENHUM",
                                   V218 == 1 ~ "2_2CRIANCAS",
                                   V218 > 1 ~ "3_3OU_CRIANCAS", # NUMERO DE FILHOS
                                   .default = NA),             #RELIGIAO
    
    
    PROVINCIA = case_when(
      V024 == 1 ~ "Cabo Delgado",
      V024 == 2 ~ "Gaza",
      V024 == 3 ~ "Inhambane",
      V024 == 4 ~ "Manica",
      V024 == 5 ~ "Maputo Cidade",
      V024 == 6 ~ "Maputo Província",
      V024 == 7 ~ "Nampula",
      V024 == 8 ~ "Niassa",
      V024 == 9 ~ "Sofala",
      V024 == 10 ~ "Tete",
      V024 == 11 ~ "Zambézia",
      TRUE ~ NA_character_),
    EDUCACAO_MARIDO = case_when(V701 == 0 ~ "1_NENHUMA",
                                V701 == 1 ~ "2_PRIMARIA",
                                V701 == 2 ~ "3_SECUNDARIA+",
                                V701 == 3 ~ "4_SUPERIOR",
                                .default = NA),      # EDUCACAO DO MARIDO DA MAE
    GRAVIDA = case_when(V213 == 0 ~ "1_NAO",      # ACTUALMENTE GRAVIDA
                        V213 == 1 ~ "2_SIM"), 
    
    RIQUEZA = case_when(V190 %in% c(1,2)  ~ "1_BAIXO",
                        V190 == 3 ~ "2_MEDIO",
                        V190 %in% c(4,5) ~ "3_ALTO",
                        
                        .default = NA
    ),                        # QUINTIL DE RIQUEZA
    ESTADO_MARITAL = case_when(V501 == 0 ~ "1_NUNCA_SE_CASOU",
                               V501 %in% c(1,2) ~ "2_CASADA/VIVE_MARITALMENTE",
                               V501 %in% c(3,4,5) ~ "3_VIUVA/DIVORCIADA/SEPARADA",
                               .default = NA),          # ESTADO MARITAL DA MAE
    
    SITUACAO_LABORAL = case_when(V714 == 0 ~ "1_DESEMPREGADO",
                                 V714 == 1 ~ "2_EMPREGADO",
                                 .default = NA),         # OCUPACAO DA MAE
    
    EDUCACAO_MULHER = case_when(V106 == 0 ~ "1_SEM ESCOLARIDADE",
                                V106 == 1 ~ "2_PRIMARIA",
                                V106 %in% c(2,3) ~ "3_SECUNDARIA+",
                                .default = NA),                  # EDUCACAO DA MAE
    SEXO_CHEFE = case_when(V151 == 1 ~ "1_MASCULINO",
                           V151 == 2 ~ "2_FEMININO",
                           .default = NA),        # SEXO DO AGREGADO FAMILIAR
    RESIDENCIA = case_when(V025 == 1 ~ "2_URBANO",
                           V025 == 2 ~ "1_RURAL",
                           .default = NA),    # ZONA RESIDENCIAL
    AGREGADO = case_when(V136 <= 5 ~ "1_<=5",
                         V136 > 5 ~ "2_>5",
                         .default = NA),          # TAMANHO DO AGREGADO FAMILIAR
    
    COHABITACAO = cut(
      V511,
      breaks = c(0, 15, 20, Inf),
      labels = c("1_0-14anos", "2_15-19anos", "3_20+"),  # COHABITACAO
      right = FALSE
    ),
    
    RANK_COWIVES = case_when(
      V505 == 1 ~ "Primeira esposa",
      V505 == 2 ~ "Segunda esposa",
      V505 > 2 ~ "Terceira esposa ou mais",  # RANKING ENTRE ESPOSAS
      TRUE ~ NA_character_
    ),
    ALCOOL_PARCEIRO = case_when(
      D113 == 1 ~ "Sim",
      D113 == 0 ~ "Não",
      TRUE ~ NA_character_
    ),
    EXPOSICAO_MEDIA = case_when (V157 == 0 & V158 == 0 & V159 == 0 ~ "1_SEM_EXPOSICAO",
                                 V157 == 1 & V158 == 1 & V159 == 1 ~ "2_MENOS_UMAVEZ_SEMANA",
                                 V157 == 2 & V158 == 2 & V159 == 2 ~ "3_ PELO_MENOS_UMA_SEMANA",
                                 V157 == 3 & V158 == 3 & V159 == 3 ~ "4_TODOS OS DIAS",
                                 .default = NA))         # EXPOSICAO AOS MEDIAS  

# DIFERENCA EDUCACIONAL
IR_data <- IR_data %>%
  mutate(EDU_DIF =
           case_when(
             
             (V715==0 & V133==0 & V502==1) ~ 4,
             (V715==V133) & V502==1 ~ 3,
             (V715<V133) & V502==1 ~ 2,
             (V715>V133) & V502==1 ~ 1 )) %>% 
  set_value_labels(EDU_DIF = c("husband better educated"=1, "wife better educated"=2, "equally educated"=3, "neither educated"=4)) %>%
  set_variable_labels(EDU_DIF = "Spousal education difference")

# DIFERENCA DE IDADE
IR_data <- IR_data %>%
  mutate(age_diff_temp= (V730-V012)) %>%
  mutate(IDADE_DIF =
           case_when(
             age_diff_temp >=10   ~ 5,
             age_diff_temp %in% c(5,6,7,8,9)  ~ 4,
             age_diff_temp %in% c(1,2,3,4)  ~ 3,
             age_diff_temp==0   ~ 2,
             age_diff_temp <0   ~ 1,
             V502==0 | V502==2 ~ NA)) %>%
  set_value_labels(IDADE_DIF = c("Wife older"=1, "Same age"=2, "Wife 1-4 yrs younger"=3, "Wife 5-9 yrs younger"=4, "Wife 10+ yrs younger"=5)) %>%
  set_variable_labels(IDADE_DIF = "Spousal age difference")




# VARIAVEIS DE ESTUDOA

vars <- c(
  "IDADE_ACTUAL_MULHER",
  "IDADE_DO_PARCEIRO",
  "RELIGIAO",
  "NUMERO_DE_CRIANCAS",
  "GRAVIDA",
  "RIQUEZA",
  "EXPOSICAO_MEDIA",
  "ESTADO_MARITAL",
  "AGREGADO",
  "SITUACAO_LABORAL",
  "EDUCACAO_MULHER",
  "EDUCACAO_MARIDO",
  "SEXO_CHEFE",
  "RESIDENCIA",
  "AGREGADO",
  "RANK_COWIVES",
  "COHABITACAO",
  "ALCOOL_PARCEIRO",
  "PROVINCIA", "EDU_DIF", "IDADE_DIF"
)

# BASE DE DADOS APENAS COM VARIAVEIS PARA ANALISE

IR_data1 <- IR_data[, c(
  "IDADE_ACTUAL_MULHER",
  "IDADE_DO_PARCEIRO",
  "RELIGIAO",
  "NUMERO_DE_CRIANCAS",
  "GRAVIDA",
  "RIQUEZA",
  "EXPOSICAO_MEDIA",
  "ESTADO_MARITAL",
  "AGREGADO",
  "SITUACAO_LABORAL",
  "EDUCACAO_MULHER",
  "EDUCACAO_MARIDO",
  "SEXO_CHEFE",
  "RESIDENCIA",
  "RANK_COWIVES",
  "COHABITACAO",
  "ALCOOL_PARCEIRO",
  "PROVINCIA", "EDU_DIF", "IDADE_DIF", "V024", "V005", "V022", "D005", "V021", "V023", "violencia_fisica",
  "violencia_sexual", "violencia_emocional", "vd_total"
)]

# DESENHO AMOSTRAL GERAL
wt <- IR_data1$D005/1000000  # peso  
surveydesign <- svydesign(
  ids = ~V021,      # PSU  
  strata = ~V023,   # estratos  
  weights = wt,     # peso ajustado  
  data = IR_data1,
  variance = "HT"
)

# FUNCAO PARA OBTER ESTATISTICAS PONDERADAS
descriptive_pri <- function(data, surveydesign, vector_var, rd) {
  prop_svy <- list()
  tab <- list()
  
  for (i in 1:length(vector_var)) {
    var_name <- vector_var[i]
    var_data <- data[[var_name]]
    prop_svy[[i]] <- svymean(as.formula(paste("~", var_name)), surveydesign, na.rm=TRUE)
    tab[[i]] <- svytable(as.formula(paste("~", var_name)), surveydesign)
  }
  
  conf_int <- lapply(prop_svy, confint)
  pop <- lapply(prop_svy, function(x) { round(x * 100, rd) })
  conf <- lapply(conf_int, function(x) { round(x * 100, rd) })
  vec <- list()
  vec_res <- list()
  
  for (i in 1:length(prop_svy)) {
    vec[[i]] <- paste(pop[[i]][1:length(prop_svy[[i]])], 
                      ' (', 
                      paste(conf[[i]][,1], conf[[i]][,2], sep='-'), 
                      ')', 
                      sep='')
    
    vec_res[[i]] <- data.frame(
      Variavel = rep(vector_var[i], length(names(tab[[i]]))),
      Categoria = names(tab[[i]]),
      N = as.numeric(tab[[i]]),
      Estimativa = vec[[i]]
    )
  }
  
  resultado <- do.call(rbind, vec_res)
  return(resultado)
}
# aplicacao da funcao de estatisticas ponderadas
tab_descr <- descriptive_pri(data = IR_data1,surveydesign = surveydesign, vector_var = vars,rd=1)
save_path <- getwd()
write.xlsx(tab_descr, file = paste(save_path, 'DESCRITIVAS_WILSON.xlsx', sep = '/'))


# FUNCAO PARA OBTER ESTATISTICAS NAO PONDERADAS
descriptive_U5M <- function(data, variables) {
  # tab_list<-list()
  tab_list <- lapply(variables, function(x) {
    t(t(table(data[, x])))
  })
  prop_list <- lapply(tab_list, prop.table)
  variavel_nam <- list()
  for (i in 1:length(tab_list)) {
    variavel_nam[[i]] <- rep(variables[i], length(tab_list[[i]]))
  }
  data_table <- do.call(rbind, tab_list)
  data_prop <- do.call(rbind, prop_list)
  variavel_nam1 <- unlist(variavel_nam)
  data_var <- data.frame(var_nam = variavel_nam1, Names = rownames(data_table), EST = paste(data_table[, 1], " (", round(data_prop[, 1] * 100, 1), ")", sep = ""))
  return(data_var)
}

# aplicar funcao para obter estatisticas nao ponderadas
tab_descr <- descriptive_U5M(data = IR_data1, variables = vars )
save_path <- getwd()
write.xlsx(tab_descr, file = paste(save_path, 'DESCRITIVAS_WILSON_NP.xlsx', sep = '/'))



#============================================================================#
# TABELA CRUZADA: TESTE DE QUI-QUADRADO COM RAO-SCOTT
#============================================================================#

# FUNCAO PARA TABELA CRUZADA PONDERADA COM TESTE DE QUI-QUADRADO
tab_cruz <- function(data, x_var, y_var, dec = 1, margin = 1, desenho, ...) {
  tab <- svytable(as.formula(paste("~", x_var, "+", y_var)), design = desenho, ...)
  tab_prop <- round(prop.table(tab, margin = margin) * 100, digits = dec)
  list_tab <- lapply(1:dim(tab)[2], function(x) {
    paste(tab[, x], " (", tab_prop[, x], "%)", sep = "")
  })
  tabela <- as.data.frame(do.call(cbind, list_tab))
  colnames(tabela) <- attr(tab, "dimnames")[[2]]
  chisq_result <- svychisq(as.formula(paste("~", y_var, "+", x_var)), design = desenho, ...)
  combinations <- expand.grid(CATEGORIA = attr(tab, "dimnames")[[1]])
  tabela_final <- data.frame(
    VAR_NAME = rep(x_var, nrow(combinations)),
    CATEGORIA = combinations$CATEGORIA,
    tabela,
    P_VALUE = round(chisq_result$p.value, 3),
    TEST_STAT = round(chisq_result$statistic, 3)  
  )
  colnames(tabela_final)[length(colnames(tabela_final)) - 1] <- "P_VALUE"
  colnames(tabela_final)[length(colnames(tabela_final))] <- "TEST_STAT"
  
  return(tabela_final)
}

# aplicar funcao da tabela cruzada
list_of_tabs <- list()

for (var in vars) {
  tryCatch({
    tab_result <- tab_cruz(data = IR_data1, 
                           x_var = var, 
                           y_var = "vd_total", 
                           desenho = surveydesign)
    
    list_of_tabs[[var]] <- tab_result
  }, error = function(e) {
    # Exibir erro, se ocorrer
    cat("Variável:", var, "\n")
    cat("Mensagem de erro:", conditionMessage(e), "\n")
  })
}

final_table <- do.call(rbind, list_of_tabs)
excel_file <- "CROSS_VDOMESTICA.xlsx"
write.xlsx(final_table, file = excel_file, rowNames = FALSE)

#===========================================================================#
#                              GRAFICOS
#===========================================================================#

#============================ GRAFICO 1 ===================================#
# Criar tabela ponderada para cada tipo de violência
freq_emocional <- svytable(~violencia_emocional, design = surveydesign)
freq_fisica    <- svytable(~violencia_fisica, design = surveydesign)
freq_sexual    <- svytable(~violencia_sexual, design = surveydesign)
freq_total     <- svytable(~vd_total, design = surveydesign)

df_emocional <- as.data.frame(freq_emocional)
df_fisica    <- as.data.frame(freq_fisica)
df_sexual    <- as.data.frame(freq_sexual)
df_total     <- as.data.frame(freq_total)

df_emocional$violencia_emocional <- factor(df_emocional$violencia_emocional,
                                           levels = c(0,1),
                                           labels = c("Não", "Sim"))

df_fisica$violencia_fisica <- factor(df_fisica$violencia_fisica,
                                     levels = c(0,1),
                                     labels = c("Não", "Sim"))

df_sexual$violencia_sexual <- factor(df_sexual$violencia_sexual,
                                     levels = c(0,1),
                                     labels = c("Não", "Sim"))

df_total$vd_total <- factor(df_total$vd_total,
                            levels = c(0,1),
                            labels = c("Não", "Sim"))

# Criar um único dataframe no mesmo formato que o da residência
df <- data.frame(
  tipo_violencia = c("Emocional", "Física", "Sexual", "Doméstica"),
  prevalencia = c(
    df_emocional$Freq[df_emocional$violencia_emocional == "Sim"] /
      sum(df_emocional$Freq) * 100,
    df_fisica$Freq[df_fisica$violencia_fisica == "Sim"] /
      sum(df_fisica$Freq) * 100,
    df_sexual$Freq[df_sexual$violencia_sexual == "Sim"] /
      sum(df_sexual$Freq) * 100,
    df_total$Freq[df_total$vd_total == "Sim"] /
      sum(df_total$Freq) * 100
  )
)

# Gráfico no mesmo estilo do de residência
plot_violencia <- ggplot(df, aes(x = tipo_violencia, y = prevalencia, fill = tipo_violencia)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(prevalencia, 1), "%")), vjust = -0.5, color = "black", size = 4) +
  labs(title = "", x = "", y = "Percentagem") +
  scale_fill_manual(values = rep("#53868B", 4)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 40)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, color = "#2C3E50"),
    axis.text.y = element_text(size = 10, color = "#2C3E50"),
    axis.title = element_text(size = 12, color = "#2C3E50"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.95, color = "#2C3E50"),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  ) +
  annotate("text", x = Inf, y = Inf, label = "", hjust = 1, vjust = 1, size = 6, fontface = "bold", color = "#2C3E50")

#plot_violencia


# ============================= GRAFICO 2================================#
# 1. Calcular prevalência ponderada por província
prev_fisica <- svyby(
  ~violencia_sexual,
  ~PROVINCIA,
  design = surveydesign,
  FUN = svymean,
  na.rm = TRUE
)

df_plot <- prev_fisica %>%
  as.data.frame() %>%
  mutate(percent = violencia_sexual * 100) %>%
  # Ordenar províncias por percentagem decrescente
  arrange(desc(percent)) %>%
  mutate(PROVINCIA = factor(PROVINCIA, levels = PROVINCIA))

# 3. Criar gráfico de barras horizontais
g3 <- ggplot(df_plot, aes(x = percent, y = PROVINCIA)) +
  geom_bar(stat = "identity", fill = "#53868B") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), hjust = -0.1, size = 3) +
  labs(
    title = "Violência Sexual por Província",
    x = "Percentagem (%)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10)
  ) +
  xlim(0, max(df_plot$percent)*1.1)  # espaço para os labels


# Combinar os gráficos lado a lado
g1 + g2 + g3 + 
  plot_layout(ncol = 3)  # 3 colunas, 1 linha automaticamente



#--------------------------- Graficos de Estatistica Descritivas---------------#

plot_percent <- function(var, var_name){
  
  tab <- svytable(as.formula(paste("~", var)), design = surveydesign)
  
  df <- as.data.frame(tab)
  
  var_col <- names(df)[1]
  
  df <- df %>%
    mutate(percent = Freq / sum(Freq) * 100)
  
  #  Manter como fator 
  df[[var_col]] <- as.factor(df[[var_col]])
  
  # Dicionário de labels
  if(var == "EDUCACAO_MULHER"){
    levels(df[[var_col]]) <- c("Sem escolaridade", "Primária", "Secundária ou mais")
  }
  
  if(var == "SITUACAO_LABORAL"){
    levels(df[[var_col]]) <- c("Desempregada", "Empregada")
  }
  
  if(var == "RIQUEZA"){
    levels(df[[var_col]]) <- c("Baixo", "Medio", "Alto")
  }
  
  if(var == "IDADE_ACTUAL_MULHER"){
    levels(df[[var_col]]) <- c("15-18 Anos", "18–29 Anos", "30–39 Anos", "40+ Anos")
  }
  
  p <- ggplot(df, aes(x = .data[[var_col]], y = percent)) +  
    geom_bar(stat = "identity", fill = "#53868B") +
    geom_text(aes(label = sprintf("%.1f%%", percent)), vjust = -0.3, size = 4) +
    labs(
      title = paste("Distribuição percentual de", var_name),
      x = "",
      y = "Percentagem (%)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    ylim(0, max(df$percent) * 1.15)
  
  print(p)
}



plot_percent("IDADE_ACTUAL_MULHER", "Faixa etária")
plot_percent("EDUCACAO_MULHER", "Escolaridade")
plot_percent("SITUACAO_LABORAL", "Ocupação")
plot_percent("RIQUEZA", "Riqueza")





plot_vd <- function(var, var_name){
  
  tab <- svyby(~vd_total, as.formula(paste("~", var)),
               design = surveydesign,
               svymean, na.rm = TRUE)
  
  df <- as.data.frame(tab)
  
  var_col <- names(df)[1]
  
  df <- df %>%
    mutate(percent = vd_total * 100)
  
  p <- ggplot(df, aes(x = reorder(.data[[var_col]], percent), y = percent)) +
    geom_bar(stat = "identity", fill = "#53868B") +
    geom_text(aes(label = sprintf("%.1f%%", percent)), vjust = -0.3, size = 4) +
    labs(
      title = paste("Violência doméstica por", var_name),
      x = "",
      y = "Percentagem (%)"
    ) +
    theme_minimal() +
    ylim(0, max(df$percent) * 1.15)
  
  print(p)   
}

g_v_educ <- plot_vd("EDUCACAO_MULHER", "escolaridade")

g_v_ocup <- plot_vd("SITUACAO_LABORAL", "ocupação")

g_v_riqueza <- plot_vd("RIQUEZA", "riqueza")


plot_vd("ALCOOL_PARCEIRO", "consumo de álcool pelo parceiro")

plot_vd("IDADE_DIF", "diferença de idade entre parceiros")

plot_vd("IDADE_DO_PARCEIRO", "idade do parceiro")

#=====================================================================#
# Modelos com todos as variaveis 


IR_data <- IR_data %>%
  mutate(across(all_of(vars), ~ as.factor(.))) # FACTORES

# BASE DE DADOS PARA REGRESSAO
IR_data2 <- IR_data[, c(
  "IDADE_ACTUAL_MULHER",
  "IDADE_DO_PARCEIRO",
  "RELIGIAO",
  "NUMERO_DE_CRIANCAS",
  "GRAVIDA",
  "RIQUEZA",
  "EXPOSICAO_MEDIA",
  "ESTADO_MARITAL",
  "AGREGADO",
  "SITUACAO_LABORAL",
  "EDUCACAO_MULHER",
  "EDUCACAO_MARIDO",
  "SEXO_CHEFE",
  "RESIDENCIA",
  "RANK_COWIVES",
  "COHABITACAO",
  "ALCOOL_PARCEIRO",
  "PROVINCIA", "EDU_DIF", "IDADE_DIF", "V024", "V005", "V022", "D005", "V021", "V023", "violencia_fisica",
  "violencia_sexual", "violencia_emocional", "vd_total"
)]

# DESENHO AMOSTRAL
wt <- IR_data2$D005/1000000  # peso  
surveydesign <- svydesign(
  ids = ~V021,      # PSU  
  strata = ~V023,   # estratos  
  weights = wt,     # peso ajustado  
  data = IR_data2,
  variance = "HT"
)

# Seleciona apenas as variáveis do modelo + variável resposta
vars <- c(
  "IDADE_ACTUAL_MULHER",
  "IDADE_DO_PARCEIRO",
  "RELIGIAO",
  "NUMERO_DE_CRIANCAS",
  "GRAVIDA",
  "RIQUEZA",
  "EXPOSICAO_MEDIA",
  "ESTADO_MARITAL",
  "AGREGADO",
  "SITUACAO_LABORAL",
  "EDUCACAO_MULHER",
  "EDUCACAO_MARIDO",
  "SEXO_CHEFE",
  "RESIDENCIA",
  "RANK_COWIVES",
  "COHABITACAO",
  "ALCOOL_PARCEIRO",
  "PROVINCIA", "EDU_DIF", "IDADE_DIF"
)


#========================= Modelo com todos as variaveis ===============================#

vars_svy <- c("V021", "V023", "D005") 

IR_data1_clean <- IR_data2[, c("vd_total", vars, vars_svy)]
IR_data112 <- na.omit(IR_data1_clean)
wt <- IR_data112$D005/1000000  # peso  
surveydesign4 <- svydesign(
  ids = ~V021,      # PSU  
  strata = ~V023,   # estratos  
  weights = wt,     # peso ajustado  
  data = IR_data112,
  variance = "HT"
)
# Modelo completo com todas as variáveis
full_model <- glm(
  vd_total ~ IDADE_ACTUAL_MULHER +
    RELIGIAO +
    NUMERO_DE_CRIANCAS +
    IDADE_DO_PARCEIRO +
    GRAVIDA +
    RIQUEZA +
    EXPOSICAO_MEDIA +
    SITUACAO_LABORAL +
    EDUCACAO_MULHER +
    RELIGIAO +
    EDUCACAO_MARIDO +
    SEXO_CHEFE +
    RESIDENCIA +
    RANK_COWIVES +
    AGREGADO +
    COHABITACAO +
    ALCOOL_PARCEIRO +
    PROVINCIA +
    EDU_DIF +
    IDADE_DIF,
  data = IR_data112,
  family = binomial()
)

step_model <- step(full_model, direction = "both")

options(survey.lonely.psu = "adjust")


modelo_domestico <- svyglm(
  vd_total ~ IDADE_ACTUAL_MULHER + NUMERO_DE_CRIANCAS + 
    IDADE_DO_PARCEIRO + GRAVIDA + EXPOSICAO_MEDIA + 
    SITUACAO_LABORAL + SEXO_CHEFE + 
    ALCOOL_PARCEIRO + PROVINCIA,
  family = quasibinomial(),
  design = surveydesign4
)


summary(modelo_domestico)

#=====================================================================#
# Modelos com Qui-quadrado 


IR_data <- IR_data %>%
  mutate(across(all_of(vars), ~ as.factor(.))) # FACTORES

# BASE DE DADOS PARA REGRESSAO
IR_data3 <- IR_data[, c(
  "IDADE_ACTUAL_MULHER",
  "IDADE_DO_PARCEIRO",
  "RELIGIAO",
  "NUMERO_DE_CRIANCAS",
  "GRAVIDA",
  "RIQUEZA",
  "EXPOSICAO_MEDIA",
  "ESTADO_MARITAL",
  "AGREGADO",
  "SITUACAO_LABORAL",
  "EDUCACAO_MULHER",
  "EDUCACAO_MARIDO",
  "SEXO_CHEFE",
  "RESIDENCIA",
  "RANK_COWIVES",
  "COHABITACAO",
  "ALCOOL_PARCEIRO",
  "PROVINCIA", "EDU_DIF", "IDADE_DIF", "V024", "V005", "V022", "D005", "V021", "V023", "violencia_fisica",
  "violencia_sexual", "violencia_emocional", "vd_total"
)]

# DESENHO AMOSTRAL
wt <- IR_data3$D005/1000000  # peso  
surveydesign <- svydesign(
  ids = ~V021,      # PSU  
  strata = ~V023,   # estratos  
  weights = wt,     # peso ajustado  
  data = IR_data3,
  variance = "HT"
)

# Seleciona apenas as variáveis do modelo + variável resposta
vars <- c(
  "IDADE_ACTUAL_MULHER",
  "IDADE_DO_PARCEIRO",
  "RELIGIAO",
  "NUMERO_DE_CRIANCAS",
  "GRAVIDA",
  "RIQUEZA",
  "EXPOSICAO_MEDIA",
  "ESTADO_MARITAL",
  "AGREGADO",
  "SITUACAO_LABORAL",
  "EDUCACAO_MULHER",
  "EDUCACAO_MARIDO",
  "SEXO_CHEFE",
  "RESIDENCIA",
  "RANK_COWIVES",
  "COHABITACAO",
  "ALCOOL_PARCEIRO",
  "PROVINCIA", "EDU_DIF", "IDADE_DIF"
)


#========================= Modelo Qui-Quadrado ===============================#

vars_svy <- c("V021", "V023", "D005") 

IR_data1_clean <- IR_data3[, c("vd_total", vars, vars_svy)]
IR_data113 <- na.omit(IR_data1_clean)
wt <- IR_data112$D005/1000000  # peso  
surveydesign4 <- svydesign(
  ids = ~V021,      # PSU  
  strata = ~V023,   # estratos  
  weights = wt,     # peso ajustado  
  data = IR_data113,
  variance = "HT"
)
# Modelo completo com todas as variáveis
full_model <- glm(
  vd_total ~ IDADE_ACTUAL_MULHER +
    RELIGIAO +
    NUMERO_DE_CRIANCAS +
    IDADE_DO_PARCEIRO +
    SITUACAO_LABORAL +
    EDUCACAO_MULHER +
    AGREGADO +
    ALCOOL_PARCEIRO +
    PROVINCIA,
  data = IR_data113,
  family = binomial()
)

step_model <- step(full_model, direction = "both")

options(survey.lonely.psu = "adjust")


modelo_qui   <- svyglm(
  vd_total ~ IDADE_ACTUAL_MULHER  
    + IDADE_DO_PARCEIRO  +  ALCOOL_PARCEIRO + PROVINCIA,
  family = quasibinomial(),
  design = surveydesign4
)


summary(modelo_qui)


#===========================================================================#
#         MODELOS DE REGRESSAO LOGISTICA                                    #
#==========================================================================#




IR_data <- IR_data %>%
  mutate(across(all_of(vars), ~ as.factor(.))) # FACTORES

# BASE DE DADOS PARA REGRESSAO
IR_data1 <- IR_data[, c(
  "IDADE_ACTUAL_MULHER",
  "RELIGIAO",
  "NUMERO_DE_CRIANCAS",
  "GRAVIDA",
  "RIQUEZA",
  "ESTADO_MARITAL",
  "SITUACAO_LABORAL",
  "EDUCACAO_MULHER",
  "IDADE_DO_PARCEIRO",
  "SEXO_CHEFE",
  "RESIDENCIA",
  "EXPOSICAO_MEDIA",
  "AGREGADO",
  "COHABITACAO",
  "ALCOOL_PARCEIRO",
  "EDU_DIF", "IDADE_DIF",
  "PROVINCIA", "D005", "V021", "V023", "violencia_fisica",
  "violencia_sexual", "violencia_emocional", "vd_total"
)]

# DESENHO AMOSTRAL
wt <- IR_data1$D005/1000000  # peso  
surveydesign <- svydesign(
  ids = ~V021,      # PSU  
  strata = ~V023,   # estratos  
  weights = wt,     # peso ajustado  
  data = IR_data1,
  variance = "HT"
)

# Seleciona apenas as variáveis do modelo + variável resposta
vars <- c(
  "IDADE_ACTUAL_MULHER",
  "RELIGIAO",
  "NUMERO_DE_CRIANCAS",
  "GRAVIDA",
  "RIQUEZA",
  "ESTADO_MARITAL",
  "EXPOSICAO_MEDIA",
  "SITUACAO_LABORAL",
  "EDUCACAO_MULHER",
  "IDADE_DO_PARCEIRO",
  "SEXO_CHEFE",
  "RESIDENCIA",
  "AGREGADO",
  "COHABITACAO",
  "ALCOOL_PARCEIRO",
  "PROVINCIA", "EDU_DIF", "IDADE_DIF"
)


#========================= Violencia domestica ===============================#

vars_svy <- c("V021", "V023", "D005") 

IR_data1_clean <- IR_data1[, c("vd_total", vars, vars_svy)]
IR_data114 <- na.omit(IR_data1_clean)
wt <- IR_data114$D005/1000000  # peso  
surveydesign4 <- svydesign(
  ids = ~V021,      # PSU  
  strata = ~V023,   # estratos  
  weights = wt,     # peso ajustado  
  data = IR_data114,
  variance = "HT"
)
# Modelo completo com todas as variáveis
full_model <- glm(
  vd_total ~ IDADE_ACTUAL_MULHER +
    RELIGIAO +
    NUMERO_DE_CRIANCAS +
    GRAVIDA +
    RIQUEZA +
    SITUACAO_LABORAL +
    IDADE_DO_PARCEIRO +
    EDUCACAO_MULHER +
    SEXO_CHEFE +
    RESIDENCIA +
    AGREGADO +
    COHABITACAO +
    EXPOSICAO_MEDIA +
    ALCOOL_PARCEIRO +
    PROVINCIA +
    EDU_DIF +
    IDADE_DIF,
  data = IR_data114,
  family = binomial()
)

step_model <- step(full_model, direction = "both")

modelo_domestico <- svyglm (vd_total ~ IDADE_ACTUAL_MULHER + RIQUEZA + SITUACAO_LABORAL + EDUCACAO_MULHER + 
                              SEXO_CHEFE + COHABITACAO + ALCOOL_PARCEIRO + PROVINCIA + 
                              IDADE_DIF, family = quasibinomial, design = surveydesign4)


#===========================  AVALIACAO DOS MODELOS    =============================#

summary(modelo_domestico)


anova(modelo_domestico, update(modelo_domestico, ~1), test="Chisq")

lm_domestico <- glm(vd_total ~ IDADE_ACTUAL_MULHER + RIQUEZA + SITUACAO_LABORAL + EDUCACAO_MULHER + 
                      SEXO_CHEFE + COHABITACAO + ALCOOL_PARCEIRO + PROVINCIA + IDADE_DIF, data = IR_data114)

vif(lm_domestico)
check_collinearity(modelo_domestico)




#==================== TESTES DO MODELO =====================#

# Modelo final e nulo
modelo_final <- glm(
  vd_total ~ IDADE_ACTUAL_MULHER + SITUACAO_LABORAL + EDUCACAO_MULHER + 
    SEXO_CHEFE + AGREGADO + ALCOOL_PARCEIRO + PROVINCIA + IDADE_DIF,
  data = IR_data114,
  family = binomial()
)

modelo_nulo <- glm(
  vd_total ~ 1,
  data = IR_data114,
  family = binomial()
)

# -----------------------------
# 1️⃣ Pseudo-R2 de Nagelkerke
# -----------------------------
logLik_nulo <- logLik(modelo_nulo)
logLik_final <- logLik(modelo_final)
n <- nobs(modelo_final)

R2_nagelkerke <- (1 - exp((2/n) * (logLik_nulo - logLik_final))) /
  (1 - exp((2/n) * logLik_nulo))

R2_nagelkerke


# -----------------------------
# 2️⃣ Teste de razão de verossimilhança
# -----------------------------
anova(modelo_nulo, modelo_final, test = "Chisq")


# -----------------------------
# 3️⃣ Hosmer-Lemeshow
# -----------------------------
library(ResourceSelection)

# Probabilidades previstas
phat <- predict(modelo_final, type = "response")

# Converter variável resposta para 0/1 (ESSENCIAL)
y <- as.numeric(as.character(IR_data114$vd_total))

# Caso acima falhe (se for "Sim/Não"), usa:
# y <- ifelse(IR_data114$vd_total == "Sim", 1, 0)

# Teste HL
hoslem.test(y, phat, g = 10)

#==================== ROC =====================#



# Probabilidades
prob_domestico <- predict(modelo_final, type = "response")

# Curva ROC
roc_domestico <- roc(IR_data114$vd_total, prob_domestico)

# Criar dados para ggplot
roc_df <- data.frame(
  tpr = roc_domestico$sensitivities,
  fpr = 1 - roc_domestico$specificities
)

# AUC
auc_val <- auc(roc_domestico)

# Gráfico estilo da imagem
ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(size = 1.2, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Curva ROC - Violência Doméstica",
    x = "Falso Positivo (1 - Especificidade)",
    y = "Verdadeiro Positivo (Sensibilidade)"
  ) +
  annotate("text", x = 0.6, y = 0.2, 
           label = paste("AUC =", round(auc_val, 3)),
           size = 5) +
  theme_minimal()

# Mostrar AUC no console
cat("AUC Violência Doméstica:", auc_val, "\n")

#=================== EXTRACAO ==============================#

# -----------------------------
# 4️⃣ TESTE DE WALD (coeficientes)
# -----------------------------

# Coeficientes
coefs <- coef(modelo_final)

# Erro padrão
se <- sqrt(diag(vcov(modelo_final)))

# Estatística z (Wald)
z <- coefs / se

# p-values
p_values <- 2 * (1 - pnorm(abs(z)))

# Intervalo de confiança 95%
ic_lower <- coefs - 1.96 * se
ic_upper <- coefs + 1.96 * se

# Odds Ratio
OR <- exp(coefs)
OR_inf <- exp(ic_lower)
OR_sup <- exp(ic_upper)

# Tabela final
resultados_wald <- data.frame(
  Variavel = names(coefs),
  Estimate = coefs,
  Std_Error = se,
  z_value = z,
  p_value = p_values,
  IC_95_Lower = ic_lower,
  IC_95_Upper = ic_upper,
  OR = OR,
  OR_Inf = OR_inf,
  OR_Sup = OR_sup
)

print(resultados_wald)

#==================== MATRIZ DE CONFUSAO =====================#

confusion_matrix <- function(model, data, response_var, cutoff = 0.5){
  probs <- predict(model, newdata = data, type = "response")
  pred <- ifelse(probs >= cutoff, 1, 0)
  tab <- table(Predito = pred, Real = data[[response_var]])
  
  # precisão
  acc <- sum(diag(tab)) / sum(tab)
  
  list(Matriz = tab, Precisao = acc)
}

confusion_matrix(modelo_final, IR_data114, "vd_total")


