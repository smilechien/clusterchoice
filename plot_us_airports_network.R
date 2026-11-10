# Traditional node-link plot of the full induced U.S. airports network.
source("flca_core.R")
nodes <- utils::read.csv("us_domestic_air_nodes.csv", stringsAsFactors = FALSE)
edges <- utils::read.csv("us_domestic_air_edges.csv", stringsAsFactors = FALSE)
flca <- run_flca(nodes, edges)
selected <- flca$nodes
selected$IATA <- nodes$IATA[match(selected$name, nodes$name)]
stopifnot(nrow(selected) == 20L, nrow(flca$edges) == 188L)

theta <- pi / 2 - 2 * pi * (seq_len(nrow(selected)) - 1) / nrow(selected)
xy <- cbind(cos(theta), sin(theta))
rownames(xy) <- selected$name
value_scale <- (selected$value - min(selected$value)) /
  max(1, diff(range(selected$value)))
node_col <- c("#1f6da8", "#2c9b81")[selected$carac]
output_file <- "us_airports_full_network.png"

grDevices::png(output_file, width = 2600, height = 2600, res = 230,
               type = "cairo-png")
graphics::par(mar = c(3.4, 3.4, 5, 3.4), xpd = NA, family = "sans")
graphics::plot(NA, xlim = c(-1.36, 1.36), ylim = c(-1.36, 1.36),
               asp = 1, axes = FALSE, xlab = "", ylab = "")
for (i in seq_len(nrow(flca$edges))) {
  e <- flca$edges[i, ]
  a <- xy[e$Leader, ]; b <- xy[e$Follower, ]
  alpha <- 0.08 + 0.22 * e$WCD / max(flca$edges$WCD)
  graphics::segments(a[1], a[2], b[1], b[2],
                     col = grDevices::adjustcolor("#5e6d7c", alpha.f = alpha),
                     lwd = 0.35 + 1.5 * e$WCD / max(flca$edges$WCD))
}
for (i in seq_len(nrow(flca$tree_edges))) {
  e <- flca$tree_edges[i, ]
  a <- xy[e$Leader, ]; b <- xy[e$Follower, ]
  graphics::segments(a[1], a[2], b[1], b[2], col = "#ea7438",
                     lwd = 1.3 + 2.3 * e$WCD / max(flca$tree_edges$WCD))
}
graphics::points(xy[, 1], xy[, 2], pch = 21, bg = node_col,
                 col = "white", lwd = 1.5, cex = 2.0 + 1.3 * value_scale)
label_xy <- 1.16 * xy
graphics::text(label_xy[, 1], label_xy[, 2], labels = selected$IATA,
               cex = 0.9, font = 2, col = "#253243")
graphics::title(main = "U.S. Domestic Airports: Full Route Network",
                sub = "20 airports  •  188 weighted routes  •  orange = 19 strongest FLCA parent links",
                cex.main = 1.25, cex.sub = 0.86, col.main = "#1c2d3c")
graphics::legend("bottomleft", inset = c(-0.02, -0.02), bty = "n", cex = 0.78,
                 legend = c("Display group 1", "Display group 2", "All original routes", "Strongest-link tree"),
                 pch = c(21, 21, NA, NA), pt.bg = c("#1f6da8", "#2c9b81", NA, NA),
                 pt.cex = c(1.8, 1.8, NA, NA),
                 lty = c(NA, NA, 1, 1),
                 lwd = c(NA, NA, 1.5, 3),
                 col = c("#1f6da8", "#2c9b81", "#8e9aa5", "#ea7438"))
grDevices::dev.off()
cat(normalizePath(output_file, winslash = "/"), "\n")
