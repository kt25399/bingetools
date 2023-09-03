make_plot <- function(df, ...) {
  ggplot2::ggplot(df, ...) +
    ggplot2::stat_summary(geom = 'col', fun = 'mean',
                          position = position_dodge(0.9),
                          width = 0.65, color='black') +
    ggplot2::stat_summary(geom = 'errorbar',
                          fun.data = mean_se,
                          position = position_dodge(0.9),
                          width = 0.1) +
    ggbeeswarm::geom_beeswarm(size = 2, cex = 5, dodge.width = 0.9) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggpubr::theme_pubr() +
    ggpubr::labs_pubr(base_family = 'Times New Roman') +
    ggplot2::ylab('BrdU+ cells/section') +
    ggplot2::xlab('') +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(axis.ticks.x = element_blank(), text = element_text(size = 25)) +
    ggplot2::scale_fill_manual(values = c('white', 'grey')) +
    ggplot2::guides(fill = guide_legend(override.aes = list(shape = NA)))
}

