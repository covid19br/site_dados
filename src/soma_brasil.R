# Libraries
library(widgetframe)
library(tidyverse)
library(lubridate)
library(optparse)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(withr)
Sys.setlocale(category = "LC_TIME", locale = "pt_BR.UTF-8")

# carrega funcoes----
source("funcoes.R")

data = NULL

estados <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG",
             "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR",
             "RS", "SC", "SE", "SP", "TO")


df.estados <- data.frame(estado=estados, data.dir=NA, data=NA, existe.covid=NA,
                         existe.srag=NA, existe.ob.covid=NA, existe.ob.srag=NA,
                         row.names=estados)
for (estado in estados) {
    # dir para os ler os dados
    data.dir <- paste0("../dados/estado/", estado,
                       "/tabelas_nowcasting_para_grafico/")

    # pegando a data mais recente
    if (is.null(data)) {
        data.e <- get.last.date(data.dir)
    } else {
        data.e <- data
    }

    # testando se existe nowcasting
    existe.covid <- existe.nowcasting2(tipo = "covid",
                                       output.dir = data.dir,
                                       data = data.e)
    existe.srag <- existe.nowcasting2(tipo = "srag",
                                      output.dir = data.dir,
                                      data = data.e)
    existe.ob.covid <- existe.nowcasting2(tipo = "obitos_covid",
                                          output.dir = data.dir,
                                          data = data.e)
    existe.ob.srag <- existe.nowcasting2(tipo = "obitos_srag",
                                         output.dir = data.dir,
                                         data = data.e)
    df.estados[estado,] <- c(estado, data.dir, data.e, existe.covid, existe.srag,
                             existe.ob.covid, existe.ob.srag)
}

# Covid
df.covid.diario <- list()
for (estado in estados) {
    data.dir <- df.estados[estado, "data.dir"]
    df.covid.diario[[estado]] <- read.csv(paste0(data.dir, "nowcasting_diario_covid_", data.e, ".csv"), stringsAsFactors = FALSE)
}
df.covid.diario <- plyr::ldply(df.covid.diario, .id="UF")

# SRAG
df.srag.diario <- list()
for (estado in estados) {
    data.dir <- df.estados[estado, "data.dir"]
    df.srag.diario[[estado]] <- read.csv(paste0(data.dir, "nowcasting_diario_srag_", data.e, ".csv"), stringsAsFactors = FALSE)
}
df.srag.diario <- plyr::ldply(df.srag.diario, .id="UF")

# óbitos Covid
df.ob.covid.diario <- list()
for (estado in estados) {
    data.dir <- df.estados[estado, "data.dir"]
    df.ob.covid.diario[[estado]] <- read.csv(paste0(data.dir, "nowcasting_diario_obitos_covid_", data.e, ".csv"), stringsAsFactors = FALSE)
}
df.ob.covid.diario <- plyr::ldply(df.ob.covid.diario, .id="UF")

# óbitos SRAG
df.ob.srag.diario <- list()
for (estado in estados) {
    data.dir <- df.estados[estado, "data.dir"]
    df.ob.srag.diario[[estado]] <- read.csv(paste0(data.dir, "nowcasting_diario_obitos_srag_", data.e, ".csv"), stringsAsFactors = FALSE)
}
df.ob.srag.diario <- plyr::ldply(df.ob.srag.diario, .id="UF")

# TODO: convert NA into estimate for lower and upper

# óbitos Covid
df.ob.covid.br <- df.ob.covid.diario %>%
    group_by(data) %>%
    summarise(n = sum(estimate.merged, na.rm=T),
              n.lower = sum(lower.merged.pred, na.rm=T),
              n.upper = sum(upper.merged.pred, na.rm=T)) %>%
    mutate(data = as.Date(data)) %>%
    as.data.frame()

ggplot(df.ob.covid.br, aes(x=data)) +
    geom_line(aes(y=n)) +
    geom_ribbon(aes(ymin=n.lower, ymax=n.upper), fill="blue", alpha=0.3) +
    labs(y="óbitos por semana", title="Óbitos confirmados por Covid - Brasil (soma dos estados)") +
    scale_x_date(date_breaks = "2 months", date_labels = "%b")
ggsave("obitos_covid_BR.png", width=12, height=8)

# óbitos SRAG
df.ob.srag.br <- df.ob.srag.diario %>%
    group_by(data) %>%
    summarise(n = sum(estimate.merged, na.rm=T),
              n.lower = sum(lower.merged.pred, na.rm=T),
              n.upper = sum(upper.merged.pred, na.rm=T)) %>%
    mutate(data = as.Date(data)) %>%
    as.data.frame()

ggplot(df.ob.srag.br, aes(x=data)) +
    geom_line(aes(y=n)) +
    geom_ribbon(aes(ymin=n.lower, ymax=n.upper), fill="blue", alpha=0.3) +
    labs(y="óbitos por semana", title="Óbitos SRAG - Brasil (soma dos estados)") +
    scale_x_date(date_breaks = "2 months", date_labels = "%b")
ggsave("obitos_srag_BR.png", width=12, height=8)

# casos Covid
df.covid.br <- df.covid.diario %>%
    group_by(data) %>%
    summarise(n = sum(estimate.merged, na.rm=T),
              n.lower = sum(lower.merged.pred, na.rm=T),
              n.upper = sum(upper.merged.pred, na.rm=T)) %>%
    mutate(data = as.Date(data)) %>%
    as.data.frame()

ggplot(df.covid.br, aes(x=data)) +
    geom_line(aes(y=n)) +
    geom_ribbon(aes(ymin=n.lower, ymax=n.upper), fill="blue", alpha=0.3) +
    labs(y="novas hospitalizações por semana", title="Casos confirmados Covid - Brasil (soma dos estados)") +
    scale_x_date(date_breaks = "2 months", date_labels = "%b")
ggsave("casos_covid_BR.png", width=12, height=8)

# casos SRAG
df.srag.br <- df.srag.diario %>%
    group_by(data) %>%
    summarise(n = sum(estimate.merged, na.rm=T),
              n.lower = sum(lower.merged.pred, na.rm=T),
              n.upper = sum(upper.merged.pred, na.rm=T)) %>%
    mutate(data = as.Date(data)) %>%
    as.data.frame()

ggplot(df.srag.br, aes(x=data)) +
    geom_line(aes(y=n)) +
    geom_ribbon(aes(ymin=n.lower, ymax=n.upper), fill="blue", alpha=0.3) +
    labs(y="novas hospitalizações por semana", title="Casos SRAG - Brasil (soma dos estados)") +
    scale_x_date(date_breaks = "2 months", date_labels = "%b")
ggsave("casos_srag_BR.png", width=12, height=8)
