# kano_Astyle_purple_aligned_axes_tangent.R
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggrepel)
})

`%||%` <- function(a,b){ if (!is.null(a)) a else b }


.safe_num <- function(x) suppressWarnings(as.numeric(x))

plot_kano_real <- function(nodes, edges = NULL,
                           title_txt = "Kano-inspired performance plot",
                           visual_ratio = 1/1.5,
                           label_size = 4){

  nodes <- as.data.frame(nodes, stringsAsFactors = FALSE)
  need_cols <- c("name","value","value2","carac")
  miss <- setdiff(need_cols, names(nodes))
  if (length(miss) > 0) stop("nodes missing: ", paste(miss, collapse = ", "))

  nodes$value  <- .safe_num(nodes$value)
  nodes$value2 <- .safe_num(nodes$value2)
  nodes <- nodes[is.finite(nodes$value) & is.finite(nodes$value2), , drop = FALSE]
  if (nrow(nodes) < 2) stop("Not enough valid nodes.")

  nodes$carac <- as.factor(nodes$carac)

  # deterministic cluster colors
  lev <- levels(nodes$carac)
  pal <- grDevices::hcl.colors(max(3, length(lev)), "Dark 3")
  color_mapping <- setNames(pal[seq_along(lev)], lev)
  nodes$fill_col <- unname(color_mapping[as.character(nodes$carac)])

  # bubble size
  nodes$size_plot <- sqrt(pmax(nodes$value, 0))
  min_pos <- suppressWarnings(min(nodes$size_plot[nodes$size_plot > 0], na.rm = TRUE))
  if (!is.finite(min_pos)) min_pos <- 0.05
  nodes$size_plot[nodes$size_plot <= 0] <- min_pos

  # center (mean; keep consistent with your current Kano definition)
  mean_x <- mean(nodes$value2, na.rm = TRUE)
  mean_y <- mean(nodes$value,  na.rm = TRUE)

  # limits
  max_x <- max(nodes$value2, na.rm = TRUE)
  min_x <- min(nodes$value2, na.rm = TRUE)
  max_y <- max(nodes$value,  na.rm = TRUE)
  min_y <- min(nodes$value,  na.rm = TRUE)
  dx <- max_x - min_x; if (!is.finite(dx) || dx == 0) dx <- 1
  dy <- max_y - min_y; if (!is.finite(dy) || dy == 0) dy <- 1
  expand_x <- dx * 0.12
  expand_y <- dy * 0.12

  # ---- wings (same shape as your legacy code) ----
  t <- seq(0, 1, length.out = 400)
  spread_x <- expand_x * 8
  spread_y <- expand_y * 10

  lower_curve <- data.frame(
    x = t * spread_x - spread_x/2 + mean_x,
    y = mean_y - spread_y * (1 - t)^2
  )
  upper_curve <- data.frame(
    x = -t * spread_x + spread_x/2 + mean_x,
    y = mean_y + spread_y * (1 - t)^2
  )

  # wing bounds at a given x (valid only inside wing x-range)
  .wing_bounds_at_x <- function(x){
    t1 <- (x - mean_x + spread_x/2) / spread_x
    t2 <- (spread_x/2 + mean_x - x) / spread_x
    if (!is.finite(t1) || !is.finite(t2)) return(c(NA_real_, NA_real_, FALSE))
    if (t1 < 0 || t1 > 1 || t2 < 0 || t2 > 1) return(c(NA_real_, NA_real_, FALSE))
    ylo <- mean_y - spread_y * (1 - t1)^2
    yhi <- mean_y + spread_y * (1 - t2)^2
    c(ylo, yhi, TRUE)
  }

  # ---- outer circle (pink) ----
  # define a stable outer radius from wing boundary in DISPLAY metric
  wing_dense <- rbind(lower_curve, upper_curve)
  dxv <- wing_dense$x - mean_x
  dyv <- (wing_dense$y - mean_y) * visual_ratio
  wing_outer_radius <- suppressWarnings(max(sqrt(dxv^2 + dyv^2), na.rm = TRUE))
  if (!is.finite(wing_outer_radius) || wing_outer_radius <= 0) wing_outer_radius <- max(dx, dy)

  theta <- seq(0, 2*pi, length.out = 600)
  circle_radius <- wing_outer_radius * 0.55
  circle_data <- data.frame(
    x = mean_x + circle_radius * cos(theta),
    y = mean_y + (circle_radius * sin(theta)) / visual_ratio
  )

  # ---- small hub circle (purple) ----
  # "tangent-like" visually: make it almost touch the two wings at x=mean_x, but not cross
  b0 <- .wing_bounds_at_x(mean_x)
  if (isTRUE(as.logical(b0[3]))) {
    gap_half_y <- min(abs(as.numeric(b0[2]) - mean_y), abs(mean_y - as.numeric(b0[1])))
  } else {
    gap_half_y <- spread_y * 0.25
  }
  # convert y-gap to DISPLAY radius and shrink slightly (0.97) to avoid crossing
  hub_r <- gap_half_y * visual_ratio * 0.97
  hub_r <- max(hub_r, wing_outer_radius * 0.04)
  hub_r <- min(hub_r, wing_outer_radius * 0.35)

  theta_h <- seq(0, 2*pi, length.out = 360)
  hub_circle <- data.frame(
    x = mean_x + hub_r * cos(theta_h),
    y = mean_y + (hub_r * sin(theta_h)) / (visual_ratio*1.25)
  )

  # ---- additional circle (red) : radius = 2 * inner circle ----
  hub_r2 <- hub_r * 2
  hub_r2 <- min(hub_r2, wing_outer_radius * 0.95)
  theta_h2 <- seq(0, 2*pi, length.out = 360)
  hub_circle2 <- data.frame(
    x = mean_x + hub_r2 * cos(theta_h2),
    y = mean_y + (hub_r2 * sin(theta_h2)) / (visual_ratio*1.25)
  )

  wing_poly <- rbind(upper_curve, lower_curve[rev(seq_len(nrow(lower_curve))), ])

  # ---- plot ----
  p <- ggplot(nodes, aes(x=value2, y=value)) +

    # red dotted axes through (0,0)
    geom_vline(xintercept = 0, color = "red", linetype = "dotted", linewidth = 0.9, alpha = 0.85) +
    geom_hline(yintercept = 0, color = "red", linetype = "dotted", linewidth = 0.9, alpha = 0.85) +

    geom_polygon(data=wing_poly, aes(x=x,y=y),
                 inherit.aes=FALSE, fill="lightskyblue1", alpha=.18, color=NA) +

    # purple hub circle (white cut-out + purple ring)
    geom_polygon(data=hub_circle, aes(x=x,y=y),
                 inherit.aes=FALSE, fill="white", color=NA) +
    geom_path(data=hub_circle, aes(x=x,y=y),
              inherit.aes=FALSE, color="purple", linewidth=0.2, linetype="solid") +

    geom_path(data=hub_circle2, aes(x=x,y=y),
              inherit.aes=FALSE, color="hotpink3", linewidth=0.2, linetype="solid") +

    geom_line(data=lower_curve, aes(x=x,y=y),
              inherit.aes=FALSE, color="blue", linewidth=2.3) +
    geom_line(data=upper_curve, aes(x=x,y=y),
              inherit.aes=FALSE, color="blue", linewidth=2.3) +

    geom_path(data=circle_data, aes(x=x,y=y),
              inherit.aes=FALSE, color="hotpink3", linewidth=0.2) +

    geom_point(aes(size=size_plot, fill=fill_col),
               shape=21, color="black", alpha=.9) +
    geom_text_repel(aes(label=name), size=3.2, max.overlaps=200) +

    scale_fill_identity() +
    scale_size(range=c(3,12)) +
    coord_fixed(ratio=visual_ratio, clip="off") +
    scale_x_continuous(limits=c(min_x-3*expand_x, max_x+3*expand_x)) +
    scale_y_continuous(limits=c(min_y-3*expand_y, max_y+3*expand_y)) +
    theme_minimal(base_family = "Helvetica") +
    theme(
      plot.title = element_text(size=16, face="bold", hjust=0.5),
      legend.position = "right"
    ) +
    labs(title=title_txt, x="value2", y="value")

  attr(p, "hub_radius") <- hub_r
  attr(p, "wing_outer_radius") <- wing_outer_radius
  p
}

# ---- DEMO ----
if (interactive()) {
  set.seed(2)
  demo_nodes <- data.frame(
    name  = paste0("A", 1:22),
    value = round(runif(22, -5, 55), 1),
    value2 = round(runif(22, -5, 45), 1),
    carac = sample(1:4, 22, replace = TRUE),
    stringsAsFactors = FALSE
  )
  p <- plot_kano_real(demo_nodes, title_txt = "Kano plot (red dotted axes + near-tangent hub)")
  print(p)
}


# === PATCH: strict dual-circle Kano (SS vs a*), no AAC ===
plot_kano_ss_astar <- function(nd, title_txt = "Kano: SS vs a* [PATCH]") {
  stopifnot(is.data.frame(nd))
  if (!all(c("name","ss","a_star","carac") %in% names(nd))) {
    stop("Required columns missing: name, ss, a_star, carac")
  }
  nd_kano <- nd |>
    dplyr::transmute(
      name   = name,
      value  = a_star,
      value2 = ss,
      carac  = carac
    )
  plot_kano_real(nodes = nd_kano, title_txt = title_txt)
}


# ------------------------------------------------------------------------------
# Wrapper: use plot_kano_real with arbitrary x/y columns (for app integration)
# ------------------------------------------------------------------------------
plot_kano_real_xy <- function(nodes, edges = NULL,
                             xcol = "value2", ycol = "value", sizecol = "value",
                             title_txt = "Kano plot",
                             label_size = 4,
                             xlab = NULL, ylab = NULL,
                             visual_ratio = 1/1.5) {
  df <- as.data.frame(nodes, stringsAsFactors = FALSE)
  if (!("name" %in% names(df)) && ("id" %in% names(df))) df$name <- df$id
  if (!xcol %in% names(df) || !ycol %in% names(df)) stop("plot_kano_real_xy: missing xcol/ycol in nodes")
  df$value2 <- suppressWarnings(as.numeric(df[[xcol]]))
  df$value  <- suppressWarnings(as.numeric(df[[ycol]]))
  df$size_tmp <- suppressWarnings(as.numeric(df[[sizecol]]))
  if (!is.null(xlab)) attr(df, "xlab") <- xlab
  if (!is.null(ylab)) attr(df, "ylab") <- ylab
  # plot_kano_real uses `size` from `value`; we map by setting value accordingly if needed
  df$value_for_size <- df$size_tmp
  # keep original in case
  p <- plot_kano_real(df, edges = edges, title_txt = title_txt, visual_ratio = visual_ratio, label_size = label_size)
  # adjust label size if possible (ggrepel uses fixed in plot_kano_real; we cannot perfectly scale without rewriting)
  p
}


# ------------------------------------------------------------------------------
# Compatibility wrapper (Shiny app)
# - Keeps older app.R calls working:
#     kano_plot(nodes, edges = ..., xlab = ..., ylab = ..., label_size = ...)
# ------------------------------------------------------------------------------
kano_plot <- function(nodes, edges = NULL,
                      xlab = "value2", ylab = "value",
                      label_size = 4,
                      title_txt = "Kano: value vs value2",
                      visual_ratio = 1/1.5,
                      ...) {
  # Accept legacy arguments safely (edges unused in plotting but kept for API stability)
  p <- plot_kano_real(nodes = nodes, edges = edges,
                      title_txt = title_txt,
                      visual_ratio = visual_ratio,
                      label_size = label_size)
  # Allow custom axis labels without breaking older signatures
  p <- p + ggplot2::labs(x = xlab, y = ylab)
  p
}

# ------------------------------------------------------------------------------
# Convenience: SS vs a* Kano that also accepts legacy args (ignored)
# ------------------------------------------------------------------------------
kano_plot_ss_astar <- function(nodes, edges = NULL,
                              xlab = "SS", ylab = "a*",
                              label_size = 4,
                              title_txt = "Kano: SS vs a*",
                              visual_ratio = 1/1.5,
                              ...) {
  # expects nodes contain: name, ss (or ssi/sil_width), a_star (or a_star1), carac
  nd <- as.data.frame(nodes, stringsAsFactors = FALSE)
  if (!("ss" %in% names(nd))) {
    if ("ssi" %in% names(nd)) nd$ss <- nd$ssi
    if ("sil_width" %in% names(nd)) nd$ss <- nd$sil_width
  }
  if (!("a_star" %in% names(nd))) {
    if ("a_star1" %in% names(nd)) nd$a_star <- nd$a_star1
  }
  p <- plot_kano_ss_astar(nd, title_txt = title_txt)
  p <- p + ggplot2::labs(x = xlab, y = ylab)
  p
}
