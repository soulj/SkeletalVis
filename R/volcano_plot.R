#' Volcano Plot of Gene Expression Data
#'
#' Creates a volcano plot showing the log2 fold change and FDR values with optional labelling of points.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @param data A data frame containing gene expression data, containing an ID, log2 foldchange and FDR columns.
#' @param number_points Number of top up and down regulated points to label
#' @param selected_points Character vector of the IDs to label
#' @param interactive Should an interactive plotly graph be made?
#' @param logFC_threshold The foldchange threshold to define up and down regulated points
#' @param FDR_threshold The pval threshold to define up and down regulated points
#' @param point_size Size for the points
#' @param lab_size Size for the labels
#' @return A ggplot object representing the volcano plot.
#' @export
#' @examples
#' skeletalvis <- load_skeletalvis(demo=TRUE)
#'
#' experiment_results <- get_experiment(skeletalvis, "GSE155118_1")
#'
#' volcano_plot(experiment_results)

volcano_plot <- function(data,  number_points=5, selected_points=NULL, interactive= FALSE, logFC_threshold=log2(1.5), FDR_threshold=0.05,
                         point_size=2, lab_size=4) {

  data <- as.data.frame(data)

  if(ncol(data)<3)  stop("The dataset needs ID, foldchange and FDR columns")

  id_col <- colnames(data)[1]
  fc_col <- colnames(data)[2]
  pval_col <- colnames(data)[3]

  if(!all(is.numeric(data[, fc_col]))) stop("The 2nd column should be numeric fold changes")
  if(!all(is.numeric(data[, pval_col]))) stop("The 3rd column should be numeric FDRs")




  data <- data %>%
    dplyr::mutate(logpval=-log10(!!sym(pval_col))) %>%
    dplyr::filter(!is.na(!!sym(pval_col))) %>%
     dplyr::mutate(
      Expression = dplyr::case_when(
        !!sym(fc_col) >= logFC_threshold & !!sym(pval_col) <= FDR_threshold ~ "Up-regulated",
        !!sym(fc_col) <= -logFC_threshold & !!sym(pval_col) <= FDR_threshold ~ "Down-regulated",
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

  if(id_col == "datasetID") {

    text <- paste("datasetID: ", data[,id_col], "<br>",
                "log2FoldChange: ", signif(data[,fc_col],3), "<br>",
                "FDR: ", signif(data[,pval_col],3), "<br>",
                "Description: ", data[,"Description"],
                sep = "")
  } else {
    text= paste("Gene: ", data[,id_col], "<br>",
                "log2FoldChange: ", signif(data[,fc_col],3), "<br>",
                "FDR: ", signif(data[,pval_col],3), "<br>",
                sep = "")
  }


  volcano_plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!sym(fc_col), y = .data$logpval, text=text)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$Expression), size = point_size) +
    cowplot::theme_cowplot(font_size = 16) +
     ggplot2::scale_color_manual(name = "Expression", values = myColors) +
    ggplot2::geom_hline(yintercept = -log10(FDR_threshold), linetype = "dashed") +
    ggplot2::geom_vline(xintercept = c(-logFC_threshold, logFC_threshold), linetype = "dashed") +
    ggplot2::theme(legend.position="none")

  if(!is.null(number_points)) {
    top_genes <- dplyr::bind_rows(
      data %>%
        dplyr::filter(.data$Expression == 'Up-regulated') %>%
        dplyr::arrange(!!sym(pval_col), dplyr::desc(abs(!!sym(fc_col)))) %>%
        head(number_points),
     data %>%
       dplyr::filter(.data$Expression == 'Down-regulated') %>%
       dplyr::arrange(!!sym(pval_col), dplyr::desc(abs(!!sym(fc_col)))) %>%
        head(number_points)
    )
  }

    if(!is.null(selected_points)){

      selected_points <- data %>% dplyr::filter(!!sym(id_col) %in% selected_points)
      top_genes <- dplyr::bind_rows(selected_points, top_genes)
    }

    if(!interactive) {
      if(nrow(top_genes)>0){
        top_genes$text <- ""
        volcano_plot <- volcano_plot +
          ggrepel::geom_text_repel(data = top_genes, ggplot2::aes(label = !!sym(id_col)),
                                   size = lab_size, max.overlaps = Inf, min.segment.length = 0, seed = 42, force = 5) +
          ggplot2::xlab(expression("log"[2]*"FC")) +
          ggplot2::ylab(expression("-log"[10]*"FDR"))

      } else{
        volcano_plot <- volcano_plot +
          ggplot2::xlab(expression("log"[2]*"FC")) +
          ggplot2::ylab(expression("-log"[10]*"FDR"))
      }

    } else{
      volcano_plot <- plotly::ggplotly(volcano_plot, tooltip="text")
    }


  return(volcano_plot)

}
