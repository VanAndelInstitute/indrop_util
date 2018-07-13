#' plotCellCurve
#' 
#' Plot counts vs. barcodes to identify reasonable count cut off to define 
#' which barcodes represent a "cell" vs.noise
#' 
#' @param data A matrix of counts, with barcodes as rows and genes as columns
#' @param threshold Your selected threshold, so curve can be colored nicely
#'   Default: TRUE
#' @param verbose Do you want the cell count to be printed. Default: TRUE
#' @param main Title for plot.  Default is "Cells"
#' @return None.  Called for side-effect of plotting the curve and, optionally,
#'   printing the number of barcodes that are cells based on threshold.
#'   
#' At this point, you must pick the threshold subjectively, ideally high up 
#' in the "knee" region. In the future it would be ideal to add a heuristic 
#' for picking the threshold automatically.
#' @name plotCellCurve
#' @rdname plotCellCurve
#' @examples
#' data <- readRDS("counts.rds")
#' plotCellCurve(data, 300)
#' 
plotCellCurve <- function(data, threshold = 300, verbose=TRUE, main="Cells") 
{
  counts <- apply(data, 1, sum)
  runcounts <- sapply(sort(counts), function(x) { sum(counts > x)})
  Cell <- sort(counts) > threshold
  Cell <- factor(Cell, levels = c(TRUE, FALSE))
  if (verbose)
    print(table(Cell))
  
  pd <- data.frame("bc" = runcounts + 1, "umi" = sort(counts + 1), Cell = Cell)
  ggplot(pd, aes(x = bc, y = umi, color = Cell)) +
    geom_point(pch = 19) +
    scale_x_log10(name = "Log (Barcode Count)", 
                  breaks = c(0, 5, 10, 50, 100, 500, 1000, 5000)) +
    scale_y_log10(name = "Log (UMI Count)", 
                  breaks = c(0, 5, 10, 50, 100, 500, 1000, 5000)) +
    theme_bw() +
    theme( axis.title.x = element_text(face = "bold"),
           axis.title.y = element_text(face = "bold")) +
    labs(title = main) +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0))
}
