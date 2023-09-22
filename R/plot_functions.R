make_plot <- function(df, mapping, xlab = '', ylab = '', ylim = NA) {
  ggplot2::ggplot(df, mapping) +
    ggplot2::stat_summary(geom = 'col', fun = 'mean',
                          position = ggplot2::position_dodge(0.9),
                          width = 0.65, color='black') +
    ggplot2::stat_summary(geom = 'errorbar',
                          fun.data = mean_se,
                          position = ggplot2::position_dodge(0.9),
                          width = 0.1) +
    ggbeeswarm::geom_beeswarm(size = 2, cex = 5, dodge.width = 0.9) +
    ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,ylim)) +
    ggpubr::theme_pubr() +
    ggpubr::labs_pubr(base_family = 'Times New Roman') +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(axis.ticks.x = element_blank(),
                   text = element_text(size = 25)) +
    ggplot2::scale_fill_manual(values = c('white', 'grey')) +
    ggplot2::guides(fill =
                      ggplot2::guide_legend(override.aes = list(shape = NA)))
}

make_faceted_plot <- function(df, mapping, facet, xlab = '', ylab = '', ylim = NA) {
  make_plot(df, mapping, xlab = '', ylab = '', ylim) +
    ggplot2::facet_wrap(facet, scales = 'free_x') +
    ggplot2::theme(strip.background=
                     ggplot2::element_rect(colour=NA, fill=NA),
                   strip.text =
                     ggplot2::element_text(face = 'bold'),
                   legend.title = ggplot2::element_blank())
}

make_interaction_plot <- function(emmean_obj, formula, facet) {
  emmeans::emmip(emmean_obj, formula, CIs = TRUE, plotit = FALSE) %>%
    ggplot2::ggplot(aes(x = xvar, y = yvar, shape = tvar,
                        group = tvar, fill = tvar)) +
    ggplot2::geom_line(position= ggplot2::position_dodge(width = 0.1),
                       size = 1) +
    ggplot2::geom_errorbar(aes(ymin = LCL, ymax = UCL),
                           width = 0,
                           position= ggplot2::position_dodge(width = 0.1), size = 1) +
    ggplot2::geom_point(position= ggplot2::position_dodge(width = 0.1),
                        size = 5, shape = 21) +
    ggplot2::facet_wrap(facet) +
    ggpubr::theme_pubr() +
    ggpubr::labs_pubr(base_family = "Times New Roman") +
    ggplot2::scale_fill_manual(values = c('grey', 'white')) +
    ggplot2::xlab('') +
    ggplot2::ylab('Linear Prediction') +
    ggplot2::theme(strip.background=
                     ggplot2::element_rect(colour=NA, fill=NA),
                   strip.text =
                     ggplot2::element_text(face = 'bold'),
                   legend.title = ggplot2::element_blank())
}

make_sholl_plot <- function(df, mapping, xlab = '', ylab = '', title = '') {
  ggplot2::ggplot(df, mapping) +
    ggplot2::stat_summary(geom = 'line', fun = 'mean', size = 1) +
    ggplot2::stat_summary(geom = 'errorbar', fun.data = 'mean_se', width = 2) +
    ggplot2::stat_summary(geom = 'point', fun = 'mean', size = 5, shape = 21, color = 'black') +
    ggplot2::scale_fill_manual(values = c('white', 'grey')) +
    ggplot2::ggtitle(title) +
    ggpubr::theme_pubr() +
    ggpubr::labs_pubr(base_family = 'Times New Roman') +
    ggplot2::scale_x_continuous(breaks = seq(0, 300, by = 30), limits = c(0,300)) +
    ggplot2::theme(text = ggplot2::element_text(size = 25),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(title)
}

add_significance <- function(df, model, ..., formula, offset, points) {
  max_point <- df %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(max = max({{ points }}) + offset) %>% .$max


  tmp <- df %>%
    dplyr::group_by(...) %>%
    rstatix::emmeans_test(formula, model = model) %>%
    dplyr::mutate(group1 = 0.8,
                  group2 = 1.2,
                  p = p_format(p),
                  y.position = max_point)
}

add_sholl_significance <- function(df, model, ..., formula, offset, group, x, mean_inter) {
  max_se <- df %>%
    dplyr::group_by(..., {{ group }}) %>%
    dplyr::summarise(mean_se = mean({{ mean_inter }}) +
                       (sd({{ mean_inter }}) / sqrt(n()))) %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(max = max(mean_se) + offset) %>% .$max

  tmp <- df %>%
    dplyr::group_by(...) %>%
    rstatix::emmeans_test(formula, model = model) %>%
    dplyr::mutate(radius = as.numeric(as.character(radius)),
                  p.adj = stats::p.adjust(p,  method = 'bonferroni'),
                  p.adj.signif =
                    dplyr::case_when(p.adj < 0.001 ~ "***",
                                     p.adj < 0.01 ~ "**",
                                     p.adj < 0.05 ~ "*",
                                     p.adj >= 0.05 ~ ''),
                  y.position = max_se)
}

p_format <- function(p) {
  p_formatted = dplyr::if_else(p < 0.001, "p < 0.001",
                               stringr::str_c("p = ",
                                              format(round(p, 3),
                                                     nsmall= 3)))
}

