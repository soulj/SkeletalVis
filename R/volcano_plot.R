# R/volcano_plot.R

#' Volcano Plot of Gene Expression Data
#'
#' @param data A data frame containing gene expression data.
#' @param id_col The column name for the id of the points (genes/experiments).
#' @param pval_col The column name for p-values.
#' @param fc_col The column name for fold changes.
#' @param logFCThreshold The foldchange threshold to define up and down regulated points
#' @param adjPValThreshold The pval threshold to define up and down regulated points
#' @param point.size Size for the points
#' @param lab.size Size for the labels
#' @param top.points Number of top up and down regulated points to label
#' @return A ggplot object representing the volcano plot.
#' @export
#' @examples
#' volcano_plot(foldChanges,"SkeletalVisID","log2FoldChange")

volcano_plot <- function(data, id_col, pval_col, fc_col, logFCThreshold=log2(1.5), adjPValThreshold=0.05,
                         point.size=2, lab.size=4, top.points=5) {

  library(ggplot2)

  if(any(!c(id_col, pval_col, fc_col) %in% colnames(data))) stop("Columns missing from the data")

  data <- data %>%
    mutate(logpval=-log10(!!sym(pval_col))) %>%
      filter(!is.na(!!sym(pval_col))) %>%
     mutate(
      Expression = case_when(
        !!sym(fc_col) >= logFCThreshold & !!sym(pval_col) <= adjPValThreshold ~ "Up-regulated",
        !!sym(fc_col) <= -logFCThreshold & !!sym(pval_col) <= adjPValThreshold ~ "Down-regulated",
        TRUE ~ "Unchanged"
      )
    )

  data$Expression <- factor(data$Expression, levels=c("Down-regulated", "Unchanged", "Up-regulated"))
  myColors <- c("dodgerblue3", "black", "firebrick3")
  names(myColors) <- levels(data$Expression)

  # Find the maximum value in the vector that's not Inf
  max_non_inf <- max(data[is.finite(data$logpval),"logpval"], na.rm = TRUE)

  # Replace Inf values with this maximum non-Inf value
  data[is.infinite(data$logpval),"logpval"] <- max_non_inf


  volcano_plot <- ggplot(data, aes(x = !!sym(fc_col), y = logpval)) +
    geom_point(aes(color = Expression), size = point.size) +
    cowplot::theme_cowplot(font_size = 16) +
    xlab(expression("log"[2]*"FC")) +
    ylab(expression("-log"[10]*"FDR")) +
    scale_color_manual(name = "Expression", values = myColors) +
    geom_hline(yintercept = -log10(adjPValThreshold), linetype = "dashed") +
    geom_vline(xintercept = c(-logFCThreshold, logFCThreshold), linetype = "dashed") +
    theme(legend.position="none")

  if(!is.null(top.points)) {
    top_genes <- bind_rows(
      data %>%
        filter(Expression == 'Up-regulated') %>%
        arrange(!!sym(pval_col), desc(abs(!!sym(fc_col)))) %>%
        head(top.points),
     data %>%
        filter(Expression == 'Down-regulated') %>%
        arrange(!!sym(pval_col), desc(abs(!!sym(fc_col)))) %>%
        head(top.points)
    )


    volcano_plot <- volcano_plot +
      ggrepel::geom_text_repel(data = top_genes, aes(label = !!sym(id_col)),
                      size = lab.size, max.overlaps = Inf, min.segment.length = 0, seed = 42)
  }

  return(volcano_plot)

}
