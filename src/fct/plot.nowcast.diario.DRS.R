library(geofacet)

grid_drs2 <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5),
  col = c(5, 4, 3, 2, 5, 4, 1, 2, 6, 4, 3, 5, 7, 3, 5, 5, 4),
  name = c("Franca", "Barretos", "Sao_Jose_do_Rio_Preto", "Aracatuba", "Ribeirao_Preto", "Araraquara", "Presidente_Prudente", "Marilia", "Sao_Joao_da_Boa_Vista", "Piracicaba", "Bauru", "Campinas", "Taubate", "Sorocaba", "Grande_Sao_Paulo", "Baixada_Santista", "Registro"),
  code = c("DRS 8", "DRS 5", "DRS 15", "DRS 2", "DRS 13", "DRS 3", "DRS 11", "DRS 9", "DRS 14", "DRS 10", "DRS 6", "DRS 7", "DRS 17", "DRS 16", "DRS 1", "DRS 4", "DRS 12"),
  stringsAsFactors = FALSE
)

plot.nowcast.diario.DRS <- function(df) {
    plot <- df %>%
        mutate(data = as.Date(data)) %>%
        ggplot(aes(x = data)) +
        geom_ribbon(aes(ymin = lower.merged.pred, ymax = upper.merged.pred),
                    fill = RColorBrewer::brewer.pal(3, "Set1")[2], alpha = 0.3) +
        geom_line(aes(y = estimate.merged)) +
        #geom_line(aes(y = n.casos, col = "Notificado")) +
        #geom_line(aes(y = estimate, col = "Nowcasting")) +
        facet_geo(~DRS, grid = grid_drs2, scales = "free_y") +
        scale_x_date(date_breaks = "3 months", date_labels = "%b") +
        #scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")[2:1]) +
        xlab("Data do primeiro sintoma") +
        ylab("Número de novos casos") +
        plot.formatos +
        theme(legend.position = "none")
    plot
}
