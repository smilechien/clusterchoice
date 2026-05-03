# sankey.R
suppressPackageStartupMessages({
  library(htmltools)
})

render_author_sankey <- function(edges_df){
  # edges_df: Leader, follower, WCD
  edges_df <- as.data.frame(edges_df, stringsAsFactors = FALSE)
  if (!all(c("Leader","follower","WCD") %in% names(edges_df))){
    # try Source/Target/WCD
    if (ncol(edges_df) >= 3){
      edges_df <- edges_df[,1:3,drop=FALSE]
      colnames(edges_df) <- c("Leader","follower","WCD")
    } else {
      return(tags$div("Edges not available for Sankey."))
    }
  }
  edges_df$Leader <- as.character(edges_df$Leader)
  edges_df$follower <- as.character(edges_df$follower)
  edges_df$WCD <- suppressWarnings(as.numeric(edges_df$WCD))
  edges_df <- edges_df[is.finite(edges_df$WCD) & edges_df$WCD > 0, , drop=FALSE]
  if (!nrow(edges_df)) return(tags$div("No positive edges for Sankey."))

  # Prefer plotly if available (self-contained in Shiny)
  if (requireNamespace("plotly", quietly = TRUE)) {
    plotly <- asNamespace("plotly")
    nodes <- unique(c(edges_df$Leader, edges_df$follower))
    node_id <- setNames(seq_along(nodes)-1L, nodes)
    p <- plotly$plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(label = nodes, pad = 10, thickness = 15),
      link = list(
        source = unname(node_id[edges_df$Leader]),
        target = unname(node_id[edges_df$follower]),
        value  = edges_df$WCD
      )
    )
    return(plotly$as_widget(p))
  }

  # Fallback: message
  tags$div("plotly not installed; Sankey not available.")
}
