# app.R — V34 TRUE REBUILD: wider Step 6b Kano plot under SSplot
# If this line is not shown by readLines(app.R, n=1), R is loading an old file.

options(shiny.maxRequestSize = 50 * 1024^2)
options(install.packages.compile.from.source = "never")

required_pkgs <- c("shiny", "DT", "igraph", "visNetwork", "RColorBrewer", "ggplot2", "dplyr", "tidyr", "tibble", "ggrepel")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing required packages: ", paste(missing_pkgs, collapse = ", "),
       ". Install first: install.packages(c(",
       paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "), type='binary')",
       call. = FALSE)
}

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(igraph)
  library(visNetwork)
  library(RColorBrewer)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggrepel)
})

message("*** LOADED V32 TRUE REBUILD at ", Sys.time(), " ***")


# ---- Embedded real SSplot renderer from renderSSplot(82).R ------------------
# The uploaded renderer supplies render_panel(), the base-R SSplot panel that
# matches the reference style more closely than the temporary ggplot version.
# It is embedded here so app.R can run without sourcing a separate file.
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# ---- device guard (robust) ---------------------------------------------------
ensure_device <- function(width = 12, height = 8, res = 144) {
  if (!is.null(grDevices::dev.list())) return(FALSE)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename = tempfile(fileext = ".png"),
                  width = width, height = height, units = "in", res = res)
  } else if (capabilities("png")) {
    png(filename = tempfile(fileext = ".png"),
        width = width, height = height, units = "in", res = res, type = "cairo")
  } else {
    pdf(file = tempfile(fileext = ".pdf"), width = width, height = height)
  }
  TRUE
}

# Build nodes0 if user didn't provide one
make_nodes0 <- function(sil_df, nodes = NULL) {
  if (is.data.frame(nodes) && all(c("name","carac") %in% names(nodes))) {
    dplyr::transmute(nodes,
                     name  = as.character(name),
                     carac = suppressWarnings(as.integer(carac)),
                     value = if ("value" %in% names(nodes)) as.numeric(value) else 1)
  } else {
    dplyr::transmute(sil_df,
                     name  = as.character(name),
                     carac = suppressWarnings(as.integer(carac)),
                     value = if ("value" %in% names(sil_df)) as.numeric(value) else 1)
  }
}

# ---- panel function -----------------------------------------------------------
render_panel <- function(
  sil_df,
  nodes0,
  results = NULL,                 # optional table with "OVERALL" and per-cluster rows
  res     = NULL,                 # optional list with res$W_km (weighted adjacency)
  nodes   = NULL,                 # optional nodes with value/value2 labels
  top_n   = NULL,                 # show top N rows (by value then SS). NULL = all
  font_scale = 1.30,
  aac_col = "#A23B3B",
  aac_side = c("left","right"),   # where to print AAC numbers relative to arrows
  gap_AAC_to_arrow_frac = 0.18,   # AAC ↔ arrows gap (fraction of plot width)
  gap_label_to_AAC_frac = 0.11,   # left labels ↔ AAC gap (fraction of plot width)
  neighbor_side = c("right","left"),     # which bar edge to anchor to (tip/start)
  neighbor_on_bar = TRUE,                 # <— NEW: draw label ON the colored bar
  neighbor_inset_frac_of_bar = 0.02,      # inset (as fraction of that bar's length)
  neighbor_at_bar_end = TRUE,             # ignored when neighbor_on_bar = TRUE
  neighbor_gap_frac   = 0.020,            # used only when drawing outside the bar
  footer_label = sprintf("Made at https://smilechien.shinyapps.io/zssplotauthor3/ on %s", Sys.Date()),
  footer_adj   = 0,
  footer_col   = "grey30"
) {
  aac_side <- match.arg(aac_side)
  neighbor_side <- match.arg(neighbor_side)






  # scales
  S <- function(x) x * font_scale
  labels_cex <- S(1.24); header_cex <- S(1.35); row3_cex <- S(1.20)
  over_cex <- S(1.30); right_cex <- S(1.25); aac_cex <- S(1.15)
  axis_cex <- S(1.00); title_cex <- S(1.10); footer_cex <- S(0.90)

  stopifnot(is.data.frame(sil_df), "name" %in% names(sil_df), "sil_width" %in% names(sil_df))
  if (is.null(top_n)) top_n <- nrow(sil_df)

  # helpers
  f2 <- function(x, d = 2) {
    x <- suppressWarnings(as.numeric(x))
    if (!is.finite(x)) return("")
    sprintf(paste0("%.", d, "f"), x)
  }

  clampU <- function(x, usr, pad = 0.01) {
    w <- diff(usr[1:2]); left <- usr[1] + pad*w; right <- usr[2] - pad*w
    pmax(pmin(x, right), left)
  }
  map_lwd <- function(w, lmin = 1.2, lmax = 4) {
    ifelse(!is.finite(w), 1.2, {
      w0 <- pmax(0, w); w1 <- w0 / (stats::quantile(w0, 0.95, na.rm = TRUE) + 1e-9)
      pmin(lmax, pmax(lmin, 1 + 3 * w1))
    })
  }
  measure_text_inches <- function(lbls, cex = 1) {
    out <- tryCatch(suppressWarnings(max(strwidth(as.character(lbls), units = "inches", cex = cex))),
                    error = function(e) NA_real_)
    if (is.finite(out)) out else 3
  }
  split_bold_left <- function(s) {
    m <- regexpr("\\s*\\(", s)
    if (m[1] > 0) { left <- substr(s, 1, m[1]-1); right <- substr(s, m[1], nchar(s)) } else { left <- s; right <- "" }
    list(left = left, right = right)
  }
  draw_mixed_right <- function(x, y, s, cex = labels_cex, col_left = "black", col_right = "black") {
    pr <- split_bold_left(s); wr <- strwidth(pr$right, units = "user", cex = cex, font = 1)
    text(x, y, pr$right, adj = c(1, 0.5), cex = cex, font = 1, col = col_right, xpd = NA)
    text(x - wr, y, pr$left,  adj = c(1, 0.5), cex = cex, font = 2, col = col_left,  xpd = NA)
  }
  to_user_dx <- function(dx_in) grconvertX(dx_in, "inches", "user") - grconvertX(0, "inches", "user")

  place_row_fit <- function(lbls, y, cols, cex = header_cex,
                            left_pad_in = 0.06, right_pad_in = 0.06, gap_in = 0.06,
                            max_shrink_iter = 8, shrink_factor = 0.94) {
    lbls <- as.character(lbls); cols <- rep_len(cols, length(lbls))
    usr  <- par("usr"); L <- usr[1] + to_user_dx(left_pad_in); R <- usr[2] - to_user_dx(right_pad_in)
    avail <- max(1e-6, R - L)
    cex_try <- cex; iter <- 0
    repeat {
      w_in <- strwidth(lbls, units = "inches", cex = cex_try)
      w_u  <- to_user_dx(w_in); gap_u <- to_user_dx(gap_in)
      need <- sum(w_u) + gap_u * (length(lbls) - 1)
      if (need <= avail || iter >= max_shrink_iter) break
      cex_try <- cex_try * shrink_factor; iter <- iter + 1
    }
    extra <- max(0, avail - (sum(w_u) + gap_u * (length(lbls) - 1)))
    add_per_gap <- if (length(lbls) > 1) extra / (length(lbls) - 1) else 0
    x <- numeric(length(lbls)); x[1] <- L
    for (i in seq_along(lbls)) {
      if (i > 1) x[i] <- x[i-1] + w_u[i-1] + gap_u + add_per_gap
      text(x[i], y, lbls[i], adj = 0, cex = cex_try, font = 2, col = cols[i])
    }
    invisible(x)
  }

  # optional enrichment from nodes
  if (is.data.frame(nodes) && all(c("name","value","value2","carac") %in% names(nodes))) {
    nodes$name <- as.character(nodes$name)
    k  <- if ("name2" %in% names(sil_df)) as.character(sil_df$name2) else as.character(sil_df$name)
    ix <- match(k, nodes$name)
    if (!"value"  %in% names(sil_df)) sil_df$value  <- NA_real_
    if (!"value2" %in% names(sil_df)) sil_df$value2 <- NA_real_
    if (!"carac"  %in% names(sil_df)) sil_df$carac  <- NA_integer_
    ok <- !is.na(ix)
    sil_df$value[ok]  <- nodes$value[ix[ok]]
    sil_df$value2[ok] <- nodes$value2[ix[ok]]
    sil_df$carac[ok]  <- nodes$carac[ix[ok]]
  }

  # cluster coercion + optional cols
  if ("carac" %in% names(sil_df)) {
    sil_df$carac <- suppressWarnings(as.integer(as.character(sil_df$carac)))
  } else if ("cluster" %in% names(sil_df)) {
    sil_df$carac <- suppressWarnings(as.integer(gsub("^C","", as.character(sil_df$cluster))))
  } else stop("`sil_df` needs `carac` or `cluster`.")
  opt <- c("value","value2","wsel","role","neighbor_index","neighbor_name","neighborC")
  for (cc in opt) if (!cc %in% names(sil_df)) sil_df[[cc]] <- NA

  # selection: top-N by value then SS
  TOTAL <- if (is.null(top_n)) nrow(sil_df) else as.integer(top_n)
  if (!is.finite(TOTAL) || TOTAL <= 0) TOTAL <- nrow(sil_df)
  val <- suppressWarnings(as.numeric(sil_df$value))
  ss  <- suppressWarnings(as.numeric(sil_df$sil_width))
  ord <- order(-ifelse(is.finite(val), val, -Inf),
               -ifelse(is.finite(ss),  ss,  -Inf),
               na.last = TRUE)
  sel <- utils::head(seq_len(nrow(sil_df))[ord], TOTAL)
  sil_plot <- sil_df[sel, , drop = FALSE]
  if (!nrow(sil_plot)) { plot.new(); text(0.5, 0.5, "No rows selected", cex = 1.4, font = 2, col = "red"); return(invisible()) }
  sil_plot <- sil_plot[order(sil_plot$carac,
                             -suppressWarnings(as.numeric(sil_plot$value)),
                             -suppressWarnings(as.numeric(sil_plot$sil_width)),
                             na.last = TRUE), , drop = FALSE] %>% as_tibble()

  # ---- AAC (global + per cluster) --------------------------------------------
  base_for_aac  <- if ("value2" %in% names(sil_plot)) sil_plot$value2 else if ("value" %in% names(sil_plot)) sil_plot$value else sil_plot$sil_width
  base_for_aacv <- if ("value"  %in% names(sil_plot)) sil_plot$value  else if ("value2" %in% names(sil_plot)) sil_plot$value2 else sil_plot$sil_width

  AAC_global <- {
    v <- suppressWarnings(as.numeric(base_for_aac)); v <- v[is.finite(v)]
    if (length(v) >= 3) {
      t3 <- sort(v, TRUE)[1:3]; if (min(t3) <= 0) t3 <- t3 + abs(min(t3)) + 1e-3
      r <- (t3[1]/t3[2])/(t3[2]/t3[3]); if (is.finite(r) && r > 0) round(r/(1+r), 2) else NA_real_
    } else NA_real_
  }
  AAC_globalv <- {
    v <- suppressWarnings(as.numeric(base_for_aacv)); v <- v[is.finite(v)]
    if (length(v) >= 3) {
      t3 <- sort(v, TRUE)[1:3]; if (min(t3) <= 0) t3 <- t3 + abs(min(t3)) + 1e-3
      r <- (t3[1]/t3[2])/(t3[2]/t3[3]); if (is.finite(r) && r > 0) round(r/(1+r), 2) else NA_real_
    } else NA_real_
  }

clv <- sort(unique(na.omit(sil_df$carac))) 
aac_by_cluster <- setNames(rep(NA_real_, length(clv)), as.character(clv))

# Choose the base vector depending on which column exists
base_all <- if ("value" %in% names(sil_df)) {
  sil_df$value
} else if ("value2" %in% names(sil_df)) {
  sil_df$value2
} else {
  sil_df$sil_width
}

for (cc in clv) {
  v <- suppressWarnings(as.numeric(base_all[sil_df$carac == cc]))
  v <- v[is.finite(v)]

  if (length(v) == 1) {
    aac_by_cluster[as.character(cc)] <- 0.5

  } else if (length(v) == 2) {
    t3 <- sort(v, decreasing = TRUE)
    if (min(t3) <= 0) t3 <- t3 + abs(min(t3)) + 1e-3
    ratio <- t3[1] / t3[2]
    aac_by_cluster[as.character(cc)] <- ratio / (1 + ratio)

  } else if (length(v) >= 3) {
    t3 <- sort(v, decreasing = TRUE)[1:3]
    if (min(t3) <= 0) t3 <- t3 + abs(min(t3)) + 1e-3
    r <- (t3[1] / t3[2]) / (t3[2] / t3[3])
    if (is.finite(r) && r > 0) {
      aac_by_cluster[as.character(cc)] <- r / (1 + r)
    }
  }
}


  # ---- header / layout --------------------------------------------------------
  SS_overall <- mean(sil_df$sil_width, na.rm = TRUE)
  Qw_overall <- Qu_overall <- D_overall <- QD_overall <- Dmax_over <- QDmax_over <- NA_real_
  if (is.data.frame(results) && "Cluster" %in% names(results)) {
    pick <- function(choices){
      nm <- names(results); i <- match(tolower(choices), tolower(nm)); i <- i[!is.na(i)]; if (length(i)) nm[i[1]] else NULL
    }
    ri <- match("OVERALL", results$Cluster)
    if (is.finite(ri)) {
      cQw <- pick(c("Qw","Q_w","Qweighted","Qw_mean"))
      cQu <- pick(c("Qu","Q_u","Qunweighted","Qu_mean"))
      cSS <- pick(c("SS","Silhouette","MeanSS","AvgSS","S"))
      if (!is.null(cQw)) Qw_overall <- suppressWarnings(as.numeric(results[[cQw]][ri]))
      if (!is.null(cQu)) Qu_overall <- suppressWarnings(as.numeric(results[[cQu]][ri]))
      if (!is.null(cSS)) SS_overall <- suppressWarnings(as.numeric(results[[cSS]][ri]))
      if ("D_GiniSimpson" %in% names(results)) D_overall  <- suppressWarnings(as.numeric(results$D_GiniSimpson[ri]))
      if ("Q_over_D"      %in% names(results)) QD_overall <- suppressWarnings(as.numeric(results$Q_over_D[ri]))
      if ("OneMinus_1_over_k" %in% names(results)) Dmax_over <- suppressWarnings(as.numeric(results$OneMinus_1_over_k[ri]))
      if ("Q_over_Dmax_eff"   %in% names(results)) QDmax_over <- suppressWarnings(as.numeric(results$Q_over_Dmax_eff[ri]))
    }
  }

  build_label <- function(i){
    parts <- c(
      if ("value"  %in% names(sil_plot))  format(sil_plot$value[i],  trim = TRUE, scientific = FALSE) else NULL,
      if ("value2" %in% names(sil_plot))  format(sil_plot$value2[i], trim = TRUE, scientific = FALSE) else NULL,
      f2(sil_plot$sil_width[i]),
      if ("carac"  %in% names(sil_plot)) paste0("C", sil_plot$carac[i]) else NULL
    )
    paste0(sil_plot$name[i], ifelse(is.na(sil_plot$neighborC[i]), "", paste0("#", sil_plot$neighborC[i])), " (", paste(parts, collapse = "|"), ")")
  }
  lbl <- vapply(seq_len(nrow(sil_plot)), build_label, character(1))
  dev_in <- tryCatch(grDevices::dev.size("in"), error = function(e) c(12,8))
  left_needed <- measure_text_inches(lbl, cex = labels_cex) + 0.45
  left_in <- max(0.25, min(left_needed, dev_in[1] - 3.2 - 0.8))

  arrow_center <- -0.135; arrow_half <- 0.045; arrow_shift <- 0.10
  glyph_left   <- arrow_center - arrow_half + arrow_shift
  glyph_right  <- arrow_center + arrow_half + arrow_shift
  xmin <- min(-0.55, glyph_left - 0.20); xmax <- 1.02

  has_footer <- is.character(footer_label) && nzchar(footer_label)
  par(oma = c(if (has_footer) 1.2 else 0, 0, 0, 0), family = "sans")

  # Header
  par(fig = c(0,1, 0.78,1), mai = c(0.12, left_in, 0.12, 3.2), new = FALSE, xpd = NA)
  plot.new(); plot.window(xlim = c(0,1), ylim = c(0,1))
  safe_isfin <- function(x) is.finite(x) & !is.na(x)
  if (!safe_isfin(D_overall)) {
    cc <- sil_df$carac; cc <- cc[!is.na(cc)]
    if (length(cc) > 0) { pk <- as.numeric(table(cc))/length(cc); D_overall <- 1 - sum(pk^2) }
  }
  if (!safe_isfin(Dmax_over)) {
    k <- length(unique(na.omit(sil_df$carac))); Dmax_over <- if (k >= 1) 1 - 1/k else NA_real_
  }
  if (!safe_isfin(QD_overall) && safe_isfin(Qu_overall) && safe_isfin(D_overall) && D_overall > 0) QD_overall <- Qu_overall / D_overall
  if (!safe_isfin(QDmax_over) && safe_isfin(Qu_overall) && safe_isfin(Dmax_over) && Dmax_over > 0) QDmax_over <- Qu_overall / Dmax_over

  labs1 <- c(
    paste0("D20=", f2(D_overall)),
    paste0("Q/D20=",     f2(QD_overall)),
    paste0("Qmax=1-HHI=", f2(Dmax_over)),
    paste0("Q*=Q/Qmax=",  f2(QDmax_over))
  )
  cols1 <- c("#15803d", "black", "#6b21a8", "red")
  place_row_fit(labs1, y = 0.78, cols = cols1, cex = header_cex,
                left_pad_in = 0.06, right_pad_in = 0.06, gap_in = 0.06)

  row1 <- paste0("SS=", f2(SS_overall), " | Qw=", f2(Qw_overall),
                 " | Qu=", f2(Qu_overall), " | AAC=", f2(AAC_globalv), " | AAC2=", f2(AAC_global))
  over_cex_fit <- over_cex
  suppressWarnings({
    w_in <- tryCatch(strwidth(row1, units = "inches", cex = over_cex_fit, font = 2), error = function(e) NA_real_)
    if (is.finite(w_in) && w_in > 0) {
      target_in <- max(4.0, dev_in[1] - left_in - 3.5)
      if (is.finite(target_in) && target_in > 0) {
        over_cex_fit <- min(over_cex_fit, over_cex_fit * (target_in / w_in) * 0.98)
      }
    }
  })
  text(0.5, 0.43, row1, adj = 0.5, cex = max(S(0.95), over_cex_fit), font = 2, col = "red")
  text(0.05, 0.10, "Nodes #Adj.C (Count|Edge|SS|C#) AAC  Adjecent Node #Adj.c", adj = 0.7, cex = row3_cex, font = 2)
  #text(0.30, 0.10, "AAC2",  adj = 0.5, cex = row3_cex, font = 2, col = aac_col)
  #text(0.40, 0.10, "WCD",  adj = 0.0, cex = row3_cex, font = 2)
  #text(0.55, 0.10, "adj.mC",adj = 0.0, cex = row3_cex, font = 2)
  text(0.88, 0.10, "                  (SS|Qw|Qu|n)", adj = 0, cex = row3_cex, font = 2)

  # Chart
  par(fig = c(0,1, 0,0.78), mai = c(0.90, left_in, 0.25, 3.2), new = TRUE, xpd = NA)
  plot.new()
  n_bar <- nrow(sil_plot); y_pos <- if (n_bar) seq_len(n_bar) else 1
  plot.window(xlim = c(xmin, xmax), ylim = c(max(y_pos) + 1, 0))
  usr <- par("usr"); dx <- diff(usr[1:2])

  if (aac_side == "right") x_AAC <- clampU(glyph_right + gap_AAC_to_arrow_frac * dx, usr) else x_AAC <- clampU(glyph_left - gap_AAC_to_arrow_frac * dx, usr)
  main_x <- clampU(x_AAC - gap_label_to_AAC_frac * dx, usr)

  # bars
  present_cl <- sort(unique(na.omit(sil_plot$carac)))
  pal <- grDevices::hcl.colors(max(1, length(present_cl)), "Set3")
  bar_cols <- pal[ as.integer(factor(sil_plot$carac, levels = present_cl)) ]
  if (n_bar) rect(0, y_pos - 0.35, sil_plot$sil_width, y_pos + 0.35, col = bar_cols, border = NA)

  # reference + axis
  abline(v = 0.70, lty = 2, col = "red", lwd = 3.2)
  axis(1, cex.axis = axis_cex)
  freq <- table(na.omit(nodes0$carac))
  C <- length(freq)
  p <- if (C) as.numeric(freq)/sum(freq) else NA
  GS <- if (C) 1 - sum(p^2) else NA_real_
  mtext("Silhouette width", side = 1, line = 2.1, cex = title_cex, font = 2)
 
 

   
 


# ensure C exists (falls back to # of unique clusters)
C <- if (exists("C")) C else length(unique(na.omit(nodes$carac)))

mtext(sprintf("(n20=%d, n=%d, v20=%d, v=%d, C#=%d, Dn(GSI:Gini-Simpson)=%.2f)",
              nrow(nodes), nrow(nodes0),
              as.integer(sum(nodes$value,  na.rm = TRUE)),
              as.integer(sum(nodes0$value, na.rm = TRUE)),
              C,  GS),
      side = 1, line = 3.0, cex = S(1.10), font = 2)




  mtext("0.7", side = 1, at = 0.70, col = "red", cex = S(1.00), font = 2, line = 0.9)

  # left labels (names + metrics), right-aligned at main_x
  build_label2 <- function(i){
    parts <- c(
      if ("value"  %in% names(sil_plot))  format(sil_plot$value[i],  trim = TRUE, scientific = FALSE) else NULL,
      if ("value2" %in% names(sil_plot))  format(sil_plot$value2[i], trim = TRUE, scientific = FALSE) else NULL,
      f2(sil_plot$sil_width[i]),
      if ("carac"  %in% names(sil_plot)) paste0("C", sil_plot$carac[i]) else NULL,
       if ("nov"  %in% names(sil_plot))  format(sil_plot$nov[i],  trim = TRUE, scientific = FALSE) else NULL
    )
    paste0(sil_plot$name[i], ifelse(is.na(sil_plot$neighborC[i]), "", paste0("#", sil_plot$neighborC[i])), " (", paste(parts, collapse="|"), ")")
  }
  lab <- vapply(seq_len(n_bar), build_label2, character(1))
  par(xpd = NA); for (i in seq_along(y_pos)) draw_mixed_right(main_x, y_pos[i], lab[i], cex = labels_cex); par(xpd = FALSE)

  # WCD arrows
  par(xpd = NA)
  for (i in seq_len(n_bar)) {
    w <- suppressWarnings(as.numeric(sil_plot$wsel[i])); lw <- map_lwd(w)
    if (!is.na(sil_plot$role[i]) && sil_plot$role[i] == "leader") {
      arrows(glyph_left,  y_pos[i], glyph_right, y_pos[i], length = 0.08, angle = 18, lwd = lw, col = "#d62728", lty = 1, code = 2)
    } else if (is.finite(w)) {
      arrows(glyph_right, y_pos[i], glyph_left,  y_pos[i], length = 0.08, angle = 18, lwd = lw, col = "#1f77b4", lty = 1, code = 2)
    } else {
      arrows(glyph_left,  y_pos[i], glyph_right, y_pos[i], length = 0.08, angle = 18, lwd = 1.3, col = "grey60", lty = 3, code = 2)
    }
  }
  par(xpd = FALSE)

  # ---------- Neighbor labels ON the bars (or outside if you prefer) ----------
  # pick text color for contrast
  txt_contrast <- function(hex) {
    rgb <- grDevices::col2rgb(hex)/255
    # relative luminance
    L <- 0.2126*rgb[1,] + 0.7152*rgb[2,] + 0.0722*rgb[3,]
    ifelse(L < 0.53, "white", "black")
  }
  nn <- if ("neighbor_name" %in% names(sil_plot)) sil_plot$neighbor_name else sil_plot$name
  if ("neighborcluster" %in% names(sil_plot)) {
    nn <- ifelse(!is.na(sil_plot$neighborcluster) & nzchar(nn), sprintf("%s@%s", nn, sil_plot$neighborcluster), nn)
  }

 # Neighbor labels: RIGHT-ALIGN at the bar tip (slightly inside)
 # Neighbor labels: LEFT-ALIGN to the right of the bar tip
 # Neighbor labels: start at the bar tip (left-aligned)
 # ── Neighbor labels: snap RIGHT EDGE to the LEFT EDGE of each SS bar ─────────
 # ── Neighbor labels: start at the RIGHT edge of each SS bar ────────────────
 # ── Neighbor labels: start at the LEFT edge of each SS bar (flow right) ──
par(xpd = NA)

# Build labels
nn <- if ("neighbor_name" %in% names(sil_plot)) sil_plot$neighbor_name else sil_plot$name
if ("neighborcluster" %in% names(sil_plot)) {
  nn <- ifelse(!is.na(sil_plot$neighborcluster) & nzchar(nn),
               sprintf("%s@%s", nn, sil_plot$neighborcluster), nn)
}

# Left edge of the bar:
# - for positive SS, bars go 0 → SS  ⇒ left edge = 0
# - for negative SS, bars go SS → 0  ⇒ left edge = SS
left_edge <- pmin(0, suppressWarnings(as.numeric(sil_plot$sil_width)))

# colored relation arrows before neighbor labels
arrow_dir <- if ("role" %in% names(sil_plot)) {
  ifelse(sil_plot$role %in% c("leader","out","source"), "out",
         ifelse(sil_plot$role %in% c("follower","in","target"), "in", "both"))
} else {
  rep("both", length(y_pos))
}
arrow_sym <- ifelse(arrow_dir == "out", "→",
                    ifelse(arrow_dir == "in", "←", "↔"))
arrow_col <- ifelse(arrow_dir == "out", "red",
                    ifelse(arrow_dir == "in", "blue", "gray50"))

x_arrow <- left_edge - 0.050 * diff(par("usr")[1:2])
x_label <- left_edge - 0.006 * diff(par("usr")[1:2])

text(x = x_arrow, y = y_pos, labels = arrow_sym, adj = c(1, 0.5),
     cex = labels_cex * 0.92, col = arrow_col, xpd = NA)
text(x = x_label, y = y_pos, labels = nn, adj = c(0, 0.5),
     cex = labels_cex * 0.88, col = "black", xpd = NA)

par(xpd = FALSE)





  # ---------------------------------------------------------------------------

  # AAC at cluster leaders was removed to avoid duplicate AAC labels.
  # Cluster AAC is printed once only in the right-side per-cluster summary below.

  # right-side per-cluster summary (SS|Qw|Qu|n)
  present_cl_chr <- as.character(present_cl)
  ss_per <- setNames(rep(NA_real_, length(present_cl_chr)), present_cl_chr)
  qw_per <- qu_per <- ss_per
  if (is.data.frame(results) && "Cluster" %in% names(results)) {
    get_col <- function(choices, df){ nm <- names(df); idx <- match(tolower(choices), tolower(nm)); idx <- idx[!is.na(idx)]; if (length(idx)) nm[idx[1]] else NULL }
    cSS <- get_col(c("SS","Silhouette","MeanSS","AvgSS","S"), results)
    cQw <- get_col(c("Qw","Q_w","Qweighted","Qw_mean"), results)
    cQu <- get_col(c("Qu","Q_u","Qunweighted","Qu_mean"), results)
    key <- paste0("C", present_cl_chr); ri <- match(key, results$Cluster)
    if (!is.null(cSS)) ss_per[!is.na(ri)] <- suppressWarnings(as.numeric(results[[cSS]][ri[!is.na(ri)]]))
    if (!is.null(cQw)) qw_per[!is.na(ri)] <- suppressWarnings(as.numeric(results[[cQw]][ri[!is.na(ri)]]))
    if (!is.null(cQu)) qu_per[!is.na(ri)] <- suppressWarnings(as.numeric(results[[cQu]][ri[!is.na(ri)]]))
  }
  miss_ss <- !is.finite(ss_per)
  if (any(miss_ss)) for (cc in present_cl_chr[miss_ss]) ss_per[as.character(cc)] <- mean(sil_df$sil_width[sil_df$carac == as.integer(cc)], na.rm = TRUE)

  ### n_full <- setNames(vapply(present_cl_chr, function(cc) sum(sil_df$carac == as.integer(cc), na.rm = TRUE), integer(1)), present_cl_chr)
n_full <- setNames(
  vapply(
    present_cl_chr,
    function(cc) sum(sil_df$carac == as.integer(cc), na.rm = TRUE),
    integer(1)
  ),
  present_cl_chr
)

# force singleton-cluster SS to 0 in the right-side cluster summary
singleton_clusters <- present_cl_chr[is.finite(n_full) & (n_full <= 1L)]
if (length(singleton_clusters)) {
  ss_per[singleton_clusters] <- 0
}

by_rows <- split(seq_len(n_bar), factor(sil_plot$carac, levels = as.integer(present_cl_chr)))
  mid_y <- vapply(by_rows, function(ix) mean(y_pos[ix], na.rm = TRUE), numeric(1))

  cexs <- S(1.20)
  width_in <- function(txt) strwidth(txt, units = "inches", cex = cexs)
  make_str <- function(cc){
    aacv <- suppressWarnings(as.numeric(aac_by_cluster[as.character(cc)]))
    list(
      cl  = "",
      aac = if (is.finite(aacv)) sprintf("%.2f", aacv) else "0.00",
      ss  = sprintf("|SS=%s", f2(ss_per[cc])),
      qw  = if (is.finite(qw_per[cc])) sprintf("|%.2f", qw_per[cc]) else "|0.00",
      qu  = if (is.finite(qu_per[cc])) sprintf("|%.2f", qu_per[cc]) else "|0.00",
      n   = sprintf("|n=%d", n_full[cc]))
  }
  all_str <- lapply(present_cl_chr, make_str)
  max_w <- list(
    cl  = max(vapply(all_str, function(s) width_in(s$cl),  numeric(1))),
    aac = max(vapply(all_str, function(s) width_in(s$aac), numeric(1))),
    ss  = max(vapply(all_str, function(s) width_in(s$ss),  numeric(1))),
    qw  = max(vapply(all_str, function(s) width_in(s$qw),  numeric(1))),
    qu  = max(vapply(all_str, function(s) width_in(s$qu),  numeric(1))),
    n   = max(vapply(all_str, function(s) width_in(s$n),   numeric(1)))
  )
  gap_in <- 0.10
  right_edge_in <- grconvertX(par("usr")[2], "user", "inches") + par("mai")[4] - 0.06
  x_n_in  <- right_edge_in
  x_qu_in <- x_n_in  - max_w$n   - gap_in
  x_qw_in <- x_qu_in - max_w$qu  - gap_in
  x_ss_in <- x_qw_in - max_w$qw  - gap_in
  x_n  <- grconvertX(x_n_in,  "inches", "user")
  x_Qu <- grconvertX(x_qu_in, "inches", "user")
  x_Qw <- grconvertX(x_qw_in, "inches", "user")
  x_SS <- grconvertX(x_ss_in, "inches", "user")

  # place cluster number and AAC into the space between cluster-number area and SS bars
  x_bar_left <- 0
  xr <- diff(par("usr")[1:2])
  x_CL  <- x_bar_left - 0.22 * xr
  x_AAC <- x_bar_left - 0.08 * xr

  par(xpd = NA)
  for (ii in seq_along(present_cl_chr)) {
    cc <- present_cl_chr[ii]; yy <- mid_y[ii]; s <- make_str(cc)
    text(x_CL,  yy, s$cl,  adj = c(1, 0.5), cex = cexs, font = 2, col = "black")
    text(x_AAC, yy, s$aac, adj = c(1, 0.5), cex = cexs, font = 2, col = aac_col)
    text(x_SS,  yy, s$ss,  adj = c(1, 0.5), cex = cexs, font = 2, col = "red")
    text(x_Qw,  yy, s$qw,  adj = c(1, 0.5), cex = cexs, font = 2, col = "red")
    text(x_Qu,  yy, s$qu,  adj = c(1, 0.5), cex = cexs, font = 2, col = "blue")
    text(x_n,   yy, s$n,   adj = c(1, 0.5), cex = cexs, font = 2, col = "red")
  }
  par(xpd = FALSE)

  if (has_footer) mtext(footer_label, side = 1, outer = TRUE, line = 0.1, adj = footer_adj, cex = footer_cex, col = footer_col, font = 2)

  invisible(TRUE)
}



#
# Disable the example/demo code that follows.  When renderSSplot.R is sourced
# inside the Shiny application, executing this block would overwrite or
# conflict with application-provided variables (e.g. `sil_df`) and generate
# unintended plots or side effects.  To prevent that, we wrap the entire
# demonstration code in `if (FALSE)` so it never runs at runtime.
if (FALSE) {

# ── DEMO DATA (only if you don't already have `sil_df`) ───────────────────────
if (!exists("sil_df", inherits = TRUE)) {
  set.seed(1); n <- 20; cl <- rep(1:5, length.out = n)
  nm <-  sil_df$name
  sil_df <- tibble(
    name = nm,
    sil_width = round(runif(n, 0.2, 0.95), 2),
    carac = cl,
    value = round(runif(n, 0.4, 2.0), 2),
    value2 = round(runif(n, 5, 50), 1),
    wsel = runif(n, 0, 5),
    role = ifelse(!duplicated(cl), "leader", NA),
    neighbor_index = sample(seq_len(n), n, replace = TRUE),
    neighbor_name  = sample(nm, n, replace = TRUE),
    neighborC = sample(1:length(unique(sil_df$cluster)), n, replace = TRUE)
  )
  results <- tibble(
    Cluster = c("OVERALL", paste0("C", sort(unique(cl)))),
    SS = c(mean(sil_df$sil_width), tapply(sil_df$sil_width, sil_df$carac, mean)),
    Qw = runif(1 + length(unique(cl)), 0.6, 0.9),
    Qu = runif(1 + length(unique(cl)), 0.6, 0.9),
    D_GiniSimpson = c(0.76, rep(NA, length(unique(cl)))),
    Q_over_D      = c(1.00, rep(NA, length(unique(cl)))),
    OneMinus_1_over_k = c(0.80, rep(NA, length(unique(cl)))),
    Q_over_Dmax_eff   = c(NA,  rep(NA, length(unique(cl))))
  )
  W <- matrix(runif(n*n, 0, 1), n, n); W <- (W + t(W))/2; diag(W) <- 0
  rownames(W) <- colnames(W) <- sil_df$name
  res <- list(W_km = W)
}

## The following block was originally used for on-screen preview and saving a
## demonstration silhouette plot when sourcing this file directly. When this
## file is sourced within a Shiny app or other non-interactive context,
## executing this code can lead to unwanted side effects (for example, saving
## files to the working directory or attempting to open image viewers). To
## avoid these issues, the entire block is guarded behind an `if (FALSE)` so
## that it never runs automatically. If you wish to preview the panel
## manually, change `FALSE` to `TRUE` and source the file interactively.
if (FALSE) {
  # Build nodes0 if needed
  if (!exists("nodes0", inherits = TRUE)) nodes0 <- make_nodes0(sil_df, if (exists("nodes")) nodes else NULL)

  # ── On-screen preview ----------------------------------------------------------
  opened <- ensure_device()
  par(family = "sans")
  render_panel(
    sil_df   = sil_df,
    nodes0   = nodes0,
    results  = if (exists("results")) results else NULL,
    res      = if (exists("res")) res else NULL,
    nodes    = if (exists("nodes")) nodes else NULL,
    top_n    = nrow(sil_df),
    aac_col  = "#A23B3B",
    aac_side = "left",
    neighbor_side = "right",     # anchor at bar tip
    neighbor_on_bar = TRUE       # <— draw ON the bar
  )
  if (opened) dev.off()

  # ── Save PNG -------------------------------------------------------------------
  outfile <- file.path(getwd(), "silhouette_panel.png")
  H <- 3 + 0.28 * nrow(sil_df)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(outfile, width = 12, height = H, units = "in", res = 150)
  } else {
    png(outfile, width = 12, height = H, units = "in", res = 150, type = "cairo")
  }
  par(family = "sans")
  render_panel(
    sil_df   = sil_df,
    nodes0   = nodes0,
    results  = if (exists("results")) results else NULL,
    res      = if (exists("res")) res else NULL,
    nodes    = if (exists("nodes")) nodes else NULL,
    top_n    = nrow(sil_df),
    aac_col  = "#A23B3B",
    aac_side = "left",
    neighbor_side = "right",
    neighbor_on_bar = TRUE,
    neighbor_inset_frac_of_bar = 0.03,  # a little inset from the tip
    footer_adj = 0
  )
  dev.off()
  cat("Saved PNG to:", normalizePath(outfile, winslash = "/"), "\n")
  if (.Platform$OS.type == "windows") try(shell.exec(normalizePath(outfile)), silent = TRUE)
}

# Close the disabled example code
}

# ---- End embedded renderSSplot(82).R ----------------------------------------

# ---- Embedded real Kano renderer from kano(64).R -----------------------------
# Uses plot_kano_real(), where x = value2 and y = value. The interactive
# demo block from the source file is disabled so sourcing app.R has no side effects.
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
  # V18 safety: ggplot/grid text layers require plain character labels.
  # This prevents the Windows/Shiny error: "不是所有的 is.character(txt) 都是 TRUE".
  title_txt <- paste(as.character(title_txt), collapse = " ")
  if ("name" %in% names(nodes)) {
    nodes$name <- enc2utf8(as.character(unlist(nodes$name, use.names = FALSE)))
    nodes$name[is.na(nodes$name)] <- ""
  }
  if ("carac" %in% names(nodes)) {
    nodes$carac <- enc2utf8(as.character(unlist(nodes$carac, use.names = FALSE)))
    nodes$carac[is.na(nodes$carac) | !nzchar(nodes$carac)] <- "Algorithm"
  }
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
    theme_minimal(base_family = "sans") +
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
if (FALSE) {
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

# ---- End embedded kano(64).R -------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

as_utf8 <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) x <- as.character(x)
  y <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = ""))
  y[is.na(y)] <- ""
  y
}

safe_df <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  names(x) <- as_utf8(names(x))
  for (nm in names(x)) {
    if (is.factor(x[[nm]])) x[[nm]] <- as.character(x[[nm]])
    if (is.character(x[[nm]])) x[[nm]] <- as_utf8(x[[nm]])
  }
  x
}

nodes_demo_df <- function() {
  data.frame(
    name = c("北醫","臺大","成大","臺大醫院","陽明交通","長庚大學","陽明大學","交通大學","臺灣科大","奇美","國衛院","義守大學","北榮","中榮","中國醫大","中正大學","長庚醫院","中央大學","輔仁大學","北護大學"),
    value = c(118,92,50,40,39,31,29,28,25,24,22,20,20,17,17,16,16,15,14,12),
    value2 = c(39,40,15,22,22,14,15,11,11,15,11,19,14,11,8,14,10,9,11,9),
    carac = c(2,1,4,1,3,6,2,4,1,4,1,4,3,5,5,5,6,3,1,2),
    stringsAsFactors = FALSE
  )
}

edges_demo_df <- function() {
  data.frame(
    Leader = c("臺大","長庚大學","陽明交通","成大","臺大","北醫","中正大學","中正大學","國衛院","臺大","臺大","北醫","陽明交通","義守大學","義守大學","奇美"),
    follower = c("臺大醫院","長庚醫院","北榮","奇美","國衛院","陽明大學","中國醫大","中榮","輔仁大學","北醫","臺灣科大","北護大學","中央大學","中正大學","成大","交通大學"),
    WCD = c(6.404,4.3914,3.3822,3.3715,2.364,2.3339,2.2914,2.2814,2.2711,1.254,1.244,1.2039,1.1422,1.1319,1.1219,1.1115),
    stringsAsFactors = FALSE
  )
}


# ---- FIFA 2026 demo data -----------------------------------------------------
# Static built-in demo corresponding to FIFA 2026 Group A-L numeric clusters.
# cluster / carac: Group A = 1, Group B = 2, ..., Group L = 12.
fifa2026_nodes_demo_df <- function() {
  data.frame(
    name = c(
      "Mexico", "South Korea", "Czech Republic", "South Africa",
      "Switzerland", "Bosnia and Herzegovina", "Canada", "Qatar",
      "Scotland", "Brazil", "Morocco", "Haiti",
      "Australia", "United States", "Paraguay", "Turkey",
      "Germany", "Ivory Coast", "Curaçao", "Ecuador",
      "Sweden", "Japan", "Netherlands", "Tunisia",
      "Belgium", "Egypt", "Iran", "New Zealand",
      "Cape Verde", "Saudi Arabia", "Spain", "Uruguay",
      "France", "Norway", "Iraq", "Senegal",
      "Argentina", "Austria", "Algeria", "Jordan",
      "Colombia", "DR Congo", "Portugal", "Uzbekistan",
      "England", "Ghana", "Croatia", "Panama"
    ),
    value = c(
      3,3,1,1, 4,1,1,1, 3,1,1,0, 3,3,0,0,
      3,3,0,0, 3,1,1,0, 1,1,1,1, 1,1,1,1,
      3,3,0,0, 3,3,0,0, 3,1,1,0, 3,3,0,0
    ),
    value2 = c(
      1,1,2,2, 2,2,1,1, 1,1,1,1, 1,1,1,1,
      1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
      1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1
    ),
    cluster = c(
      1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4,
      5,5,5,5, 6,6,6,6, 7,7,7,7, 8,8,8,8,
      9,9,9,9, 10,10,10,10, 11,11,11,11, 12,12,12,12
    ),
    stringsAsFactors = FALSE
  )
}

fifa2026_edges_demo_df <- function() {
  data.frame(
    Leader = c(
      "Mexico", "South Korea", "Czech Republic",
      "Switzerland", "Switzerland", "Bosnia and Herzegovina",
      "Scotland", "Brazil",
      "Australia", "United States",
      "Germany", "Ivory Coast",
      "Sweden", "Japan",
      "Belgium", "Iran",
      "Cape Verde", "Spain",
      "France", "Norway",
      "Argentina", "Austria",
      "Colombia", "DR Congo",
      "England", "Ghana"
    ),
    follower = c(
      "South Africa", "Czech Republic", "South Africa",
      "Bosnia and Herzegovina", "Qatar", "Canada",
      "Haiti", "Morocco",
      "Turkey", "Paraguay",
      "Curaçao", "Ecuador",
      "Tunisia", "Netherlands",
      "Egypt", "New Zealand",
      "Saudi Arabia", "Uruguay",
      "Senegal", "Iraq",
      "Algeria", "Jordan",
      "Uzbekistan", "Portugal",
      "Croatia", "Panama"
    ),
    WCD = c(3,3,1, 3,1,1, 3,1, 3,3, 3,3, 3,1, 1,1, 1,1, 3,3, 3,3, 3,1, 3,3),
    stringsAsFactors = FALSE
  )
}

fifa2026_sources_df <- function() {
  data.frame(
    group = LETTERS[1:12],
    cluster = 1:12,
    source_url = paste0("https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_", LETTERS[1:12]),
    stringsAsFactors = FALSE
  )
}

fifa2026_source_links_ui <- function() {
  urls <- fifa2026_sources_df()
  tags$div(
    class = "note",
    tags$b("Live FIFA online source pages fetched by this button:"),
    tags$ul(lapply(seq_len(nrow(urls)), function(i) {
      tags$li(tags$a(
        href = urls$source_url[i], target = "_blank",
        paste0("Group ", urls$group[i])
      ))
    }))
  )
}

fifa2026_validation_df <- function(nodes = fifa2026_nodes_demo_df(), edges = fifa2026_edges_demo_df()) {
  data.frame(
    item = c(
      "Teams / nodes",
      "Directed edges",
      "Sum(nodes$value)",
      "Sum(edges$WCD) + draw term2 points",
      "Minimum cluster",
      "Maximum cluster",
      "Missing cluster values"
    ),
    value = c(
      nrow(nodes),
      nrow(edges),
      sum(nodes$value, na.rm = TRUE),
      sum(edges$WCD, na.rm = TRUE) + sum(edges$WCD == 1, na.rm = TRUE),
      min(nodes$cluster, na.rm = TRUE),
      max(nodes$cluster, na.rm = TRUE),
      sum(is.na(nodes$cluster) | nodes$cluster == 0)
    ),
    stringsAsFactors = FALSE
  )
}


# Build a group-level FIFA performance table for the sidebar and summary report.
# Each row shows the best team at the left and the next three teams to the right,
# with accumulated points in parentheses.  For FIFA 2026, clusters 1-12 correspond
# to Groups A-L and the expected complete table has 48 teams.
make_fifa_group_performance_summary <- function(nodes, reporting_date = Sys.time()) {
  if (is.null(nodes) || !is.data.frame(nodes) || nrow(nodes) == 0) {
    return(data.frame(
      reporting_date = character(0), group = character(0),
      `Top 1` = character(0), `Next 2` = character(0),
      `Next 3` = character(0), `Next 4` = character(0),
      teams_reported = integer(0), stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  nd <- safe_df(nodes)
  nms0 <- tolower(gsub("[^[:alnum:]]", "", names(nd)))
  name_col <- names(nd)[which(nms0 %in% c("name", "team", "node", "term", "label", "id"))[1] %||% 1]
  value_col <- names(nd)[which(nms0 %in% c("value", "points", "score", "pts"))[1] %||% NA_integer_]
  games_col <- names(nd)[which(nms0 %in% c("value2", "games", "gamesplayed", "played", "gp"))[1] %||% NA_integer_]
  cl_col <- names(nd)[which(nms0 %in% c("cluster", "carac", "group", "membership"))[1] %||% NA_integer_]
  if (is.na(value_col)) nd$value <- 0 else nd$value <- suppressWarnings(as.numeric(nd[[value_col]]))
  if (is.na(games_col)) nd$value2 <- 0 else nd$value2 <- suppressWarnings(as.numeric(nd[[games_col]]))
  if (is.na(cl_col)) nd$cluster <- NA_integer_ else {
    raw_cl <- as.character(nd[[cl_col]])
    raw_num <- suppressWarnings(as.integer(gsub("[^0-9-]", "", raw_cl)))
    # Accept Group A-L as cluster labels if numeric extraction fails.
    letter_cl <- match(toupper(gsub(".*([A-L]).*", "\\1", raw_cl)), LETTERS[1:12])
    nd$cluster <- ifelse(is.na(raw_num), letter_cl, raw_num)
  }
  nd$name <- trimws(as_utf8(nd[[name_col]]))
  nd$value[!is.finite(nd$value) | is.na(nd$value)] <- 0
  nd$value2[!is.finite(nd$value2) | is.na(nd$value2)] <- 0
  nd <- nd[nzchar(nd$name) & !is.na(nd$cluster) & nd$cluster > 0, , drop = FALSE]
  if (nrow(nd) == 0) {
    return(data.frame(
      reporting_date = character(0), group = character(0),
      `Top 1` = character(0), `Next 2` = character(0),
      `Next 3` = character(0), `Next 4` = character(0),
      teams_reported = integer(0), stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  fmt_team <- function(name, pts) paste0(name, " (", pts, ")")
  report_date <- format(as.POSIXct(reporting_date), "%Y-%m-%d %H:%M:%S")
  groups <- sort(unique(as.integer(nd$cluster)))
  rows <- lapply(groups, function(cl) {
    dd <- nd[as.integer(nd$cluster) == cl, , drop = FALSE]
    dd <- dd[order(-dd$value, -dd$value2, dd$name), , drop = FALSE]
    labs <- rep("", 4)
    if (nrow(dd) > 0) {
      k <- min(4, nrow(dd))
      labs[seq_len(k)] <- fmt_team(dd$name[seq_len(k)], dd$value[seq_len(k)])
    }
    g_letter <- if (cl >= 1 && cl <= 12) LETTERS[cl] else as.character(cl)
    g_url <- if (cl >= 1 && cl <= 12) paste0("https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_", g_letter) else "#"
    # Display only a concise hyperlink label, not a visible raw URL.
    # Example: <a href='https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_A' target='_blank'>Group A</a>
    group_link <- paste0("<a href='", g_url, "' target='_blank'>Group ", g_letter, "</a>")
    data.frame(
      reporting_date = report_date,
      group = group_link,
      `Top 1` = labs[1],
      `Next 2` = labs[2],
      `Next 3` = labs[3],
      `Next 4` = labs[4],
      teams_reported = nrow(dd),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  out
}

# ---- FIFA 2026 Golden Boot scorer table and slopegraph -----------------------
# Goal-scorer data are parsed from the Wikipedia footballbox scorer fields when
# those fields are available.  If the current pages do not contain scorer lists,
# the app returns an explanatory empty table rather than stopping the analysis.
empty_golden_boot_summary <- function(message = "No FIFA 2026 scorer-level data were parsed from the current Wikipedia pages.") {
  data.frame(
    reporting_date = as.character(Sys.time()),
    rank = NA_integer_,
    player = message,
    team = "",
    group = "",
    goals = NA_integer_,
    stringsAsFactors = FALSE
  )
}

empty_golden_boot_slope <- function(message = "No slopegraph is available because no scorer-level data were parsed.") {
  data.frame(
    reporting_date = as.character(Sys.time()),
    player_label = message,
    year = "Current",
    value = 0,
    stringsAsFactors = FALSE
  )
}

clean_fifa_scorer_name <- function(x) {
  x <- as.character(x)
  x <- gsub("\\[[^\\]]*\\]", "", x)
  x <- gsub("\\(.*?\\)", "", x)
  # remove minute patterns and everything after the first minute marker
  x <- gsub("\\s+[0-9]+(\\+[0-9]+)?\\s*['’′].*$", "", x)
  x <- gsub("\\s+[0-9]+(\\+[0-9]+)?\\s*(min|minute).*$", "", x, ignore.case = TRUE)
  x <- gsub("[⚽•●]", "", x)
  x <- trimws(x)
  x
}

make_fifa_golden_boot_summary <- function(scorers, reporting_date = Sys.time(), top_n = 30) {
  if (is.null(scorers) || !is.data.frame(scorers) || nrow(scorers) == 0) {
    return(empty_golden_boot_summary())
  }
  sc <- safe_df(scorers)
  if (!all(c("player","team","group","goals") %in% names(sc))) {
    return(empty_golden_boot_summary("Scorer table exists, but required columns player/team/group/goals are missing."))
  }
  sc$goals <- suppressWarnings(as.numeric(sc$goals))
  sc$goals[!is.finite(sc$goals) | is.na(sc$goals)] <- 0
  sc$player <- trimws(as_utf8(sc$player))
  sc$team <- trimws(as_utf8(sc$team))
  sc$group <- trimws(as_utf8(sc$group))
  sc <- sc[nzchar(sc$player) & sc$goals > 0, , drop = FALSE]
  if (nrow(sc) == 0) return(empty_golden_boot_summary())

  out <- sc |>
    dplyr::group_by(player, team, group) |>
    dplyr::summarise(goals = sum(goals, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(goals), team, player) |>
    as.data.frame(stringsAsFactors = FALSE)
  out$rank <- seq_len(nrow(out))
  out$reporting_date <- format(as.POSIXct(reporting_date), "%Y-%m-%d %H:%M:%S")
  out <- out[, c("reporting_date", "rank", "player", "team", "group", "goals"), drop = FALSE]
  if (is.finite(top_n) && nrow(out) > top_n) out <- out[seq_len(top_n), , drop = FALSE]
  out
}

make_fifa_golden_boot_slope_data <- function(scorers, reporting_date = Sys.time(), top_n = 10, max_steps = 6) {
  if (is.null(scorers) || !is.data.frame(scorers) || nrow(scorers) == 0) {
    return(empty_golden_boot_slope())
  }
  sc <- safe_df(scorers)
  if (!all(c("player","team","goals","match_order") %in% names(sc))) {
    return(empty_golden_boot_slope("Scorer table exists, but match-order data required for a slopegraph are missing."))
  }
  sc$goals <- suppressWarnings(as.numeric(sc$goals))
  sc$match_order <- suppressWarnings(as.integer(sc$match_order))
  sc$goals[!is.finite(sc$goals) | is.na(sc$goals)] <- 0
  sc <- sc[nzchar(sc$player) & is.finite(sc$match_order) & sc$goals > 0, , drop = FALSE]
  if (nrow(sc) == 0) return(empty_golden_boot_slope())

  final <- sc |>
    dplyr::group_by(player, team) |>
    dplyr::summarise(total_goals = sum(goals, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_goals), team, player) |>
    as.data.frame(stringsAsFactors = FALSE)
  if (nrow(final) == 0) return(empty_golden_boot_slope())
  final <- final[seq_len(min(top_n, nrow(final))), , drop = FALSE]
  final$player_label <- paste0(final$player, " (", final$team, ")")

  steps_all <- sort(unique(sc$match_order))
  if (length(steps_all) > max_steps) {
    idx <- unique(round(seq(1, length(steps_all), length.out = max_steps)))
    steps <- steps_all[idx]
  } else {
    steps <- steps_all
  }

  rows <- list()
  kk <- 0L
  for (ii in seq_len(nrow(final))) {
    player <- final$player[ii]
    team <- final$team[ii]
    plab <- final$player_label[ii]
    for (st in steps) {
      kk <- kk + 1L
      rows[[kk]] <- data.frame(
        reporting_date = format(as.POSIXct(reporting_date), "%Y-%m-%d %H:%M:%S"),
        player_label = plab,
        match_order = as.integer(st),
        year = paste0("M", st),
        value = sum(sc$goals[sc$player == player & sc$team == team & sc$match_order <= st], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  out$year <- factor(out$year, levels = paste0("M", steps))
  out
}


# Helper for Golden Boot slopegraph x-axis ordering.
# M6 means the 6th parsed completed match in the Wikipedia footballbox order;
# it is not Group F.  This function prevents character sorting such as
# M1, M12, M17, ..., M6 and forces numeric order: M1, M6, M12, ...
match_label_number <- function(x) {
  suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(x))))
}

order_match_labels_numeric <- function(x) {
  ux <- unique(as.character(x))
  nx <- match_label_number(ux)
  ux[order(ifelse(is.na(nx), Inf, nx), ux)]
}

# Tufte-style spacing adapted from the user's requested slopegraph code.
tufte_sort_cc <- function(df, x = "year", y = "value", group = "player_label", min.space = 0.08) {
  ids <- match(c(x, y, group), names(df))
  df <- df[, ids, drop = FALSE]
  names(df) <- c("x", "y", "group")
  df$x <- as.character(df$x)
  df$group <- as.character(df$group)
  df$y <- suppressWarnings(as.numeric(df$y))
  df$y[!is.finite(df$y) | is.na(df$y)] <- 0

  # Numeric match-order labels, not character labels.
  x_order <- order_match_labels_numeric(df$x)
  g_order <- unique(df$group)
  tmp <- expand.grid(x = x_order, group = g_order, stringsAsFactors = FALSE)
  tmp <- merge(tmp, df, by = c("x", "group"), all.x = TRUE, sort = FALSE)
  tmp$y[is.na(tmp$y)] <- 0
  tmp$x <- factor(tmp$x, levels = x_order)

  # Wide matrix in the same numeric x-order.
  wide <- reshape(tmp[, c("group", "x", "y"), drop = FALSE],
                  idvar = "group", timevar = "x", direction = "wide", sep = "__")
  ycols <- paste0("y__", x_order)
  ycols <- ycols[ycols %in% names(wide)]
  if (!length(ycols)) return(data.frame(group = character(0), x = character(0), y = numeric(0), ypos = numeric(0)))

  # Put top Golden Boot contenders higher by ordering on the latest cumulative value,
  # then on the earliest value and player label for deterministic display.
  final_col <- ycols[length(ycols)]
  first_col <- ycols[1]
  ord <- order(wide[[final_col]], wide[[first_col]], wide$group, decreasing = FALSE, na.last = TRUE)
  wide <- wide[ord, , drop = FALSE]

  rng <- range(as.matrix(wide[, ycols, drop = FALSE]), na.rm = TRUE)
  min.space <- min.space * diff(rng)
  if (!is.finite(min.space) || min.space <= 0) min.space <- 0.08
  yshift <- numeric(nrow(wide))
  if (nrow(wide) >= 2) {
    for (i in 2:nrow(wide)) {
      mat <- as.matrix(wide[(i - 1):i, ycols, drop = FALSE])
      d.min <- suppressWarnings(min(diff(mat), na.rm = TRUE))
      if (!is.finite(d.min)) d.min <- min.space
      yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
    }
  }
  wide$yshift <- cumsum(yshift)

  long <- reshape(wide, varying = ycols, v.names = "y", timevar = "x",
                  times = sub("^y__", "", ycols), direction = "long")
  long$x <- factor(as.character(long$x), levels = x_order)
  long$ypos <- long$y + long$yshift
  rownames(long) <- NULL
  long
}
plot_fifa_golden_boot_slopegraph <- function(slope_df, line_size = 1) {
  if (is.null(slope_df) || !is.data.frame(slope_df) || nrow(slope_df) == 0 ||
      !"player_label" %in% names(slope_df) || !"value" %in% names(slope_df)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "The chase for the Golden Boot of FIFA 2026") +
             ggplot2::annotate("text", x = 0, y = 0, label = "No scorer-level data available from current Wikipedia pages."))
  }
  df0 <- safe_df(slope_df)
  df0$value <- suppressWarnings(as.numeric(df0$value))
  df0$value[!is.finite(df0$value) | is.na(df0$value)] <- 0
  if (length(unique(df0$player_label)) <= 1 && all(df0$value == 0)) {
    msg <- unique(df0$player_label)[1]
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "The chase for the Golden Boot of FIFA 2026") +
             ggplot2::annotate("text", x = 0, y = 0, label = msg))
  }
  # Restore the original red-line slopegraph style, with wider vertical spacing
  # so player labels and cumulative-goal values remain readable.
  df <- tufte_sort_cc(df0, x = "year", y = "value", group = "player_label", min.space = 0.30)
  if (!nrow(df)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "The chase for the Golden Boot of FIFA 2026") +
             ggplot2::annotate("text", x = 0, y = 0, label = "No slopegraph data available."))
  }
  x_levels <- order_match_labels_numeric(df$x)
  df$x <- factor(as.character(df$x), levels = x_levels)
  first_x <- x_levels[1]
  left_labs <- df[df$x == first_x, c("group","ypos"), drop = FALSE]
  yr <- range(df$ypos, na.rm = TRUE)
  if (!all(is.finite(yr))) yr <- c(0, 1)
  ypad <- max(0.8, diff(yr) * 0.06)
  ggplot2::ggplot(df, ggplot2::aes(x = x, y = ypos, group = group)) +
    ggplot2::geom_line(colour = "red", linewidth = line_size, alpha = 0.80) +
    ggplot2::geom_point(colour = "white", size = 7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f", y)), size = 3.2, family = "sans") +
    ggplot2::scale_y_continuous(name = "", breaks = left_labs$ypos, labels = left_labs$group,
                                limits = c(yr[1] - ypad, yr[2] + ypad)) +
    ggplot2::labs(
      title = "The chase for the Golden Boot of FIFA 2026",
      subtitle = "Cumulative goals by parsed match order; rightmost values identify the current leading scorers",
      x = "Parsed completed match order", y = NULL
    ) +
    ggplot2::theme_classic(base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.text.y = ggplot2::element_text(face = "bold", size = 8.5, lineheight = 0.82),
      axis.text.x = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(15, 90, 25, 190)
    )
}

fifa2026_readme_df <- function() {
  data.frame(
    field = c("nodes$name", "nodes$value", "nodes$value2", "nodes$cluster", "edges$Leader", "edges$follower", "edges$WCD", "cluster 1-12", "Win/loss rule", "Draw rule"),
    meaning = c(
      "Team name",
      "FIFA group points under the demo scoring rule",
      "Number of completed matches represented in the demo",
      "Numeric FIFA group cluster: Group A=1 through Group L=12",
      "Winner for win/loss match; team1 for draw match",
      "Loser for win/loss match; team2 for draw match",
      "3 for win/loss; 1 for draw",
      "Group A to Group L",
      "winner -> loser, WCD = 3",
      "team1 -> team2, WCD = 1; team2 receives 1 point through the node value rule"
    ),
    stringsAsFactors = FALSE
  )
}

write_fifa2026_xlsx <- function(file, nodes = fifa2026_nodes_demo_df(), edges = fifa2026_edges_demo_df()) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package openxlsx is required for xlsx download. Run install.packages('openxlsx', type='binary').", call. = FALSE)
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "nodes")
  openxlsx::writeData(wb, "nodes", nodes)
  openxlsx::addWorksheet(wb, "edges")
  openxlsx::writeData(wb, "edges", edges)
  openxlsx::addWorksheet(wb, "validation")
  openxlsx::writeData(wb, "validation", fifa2026_validation_df(nodes, edges))
  openxlsx::addWorksheet(wb, "group_performance_summary")
  openxlsx::writeData(wb, "group_performance_summary", make_fifa_group_performance_summary(nodes, reporting_date = Sys.time()))
  openxlsx::addWorksheet(wb, "golden_boot")
  openxlsx::writeData(wb, "golden_boot", empty_golden_boot_summary("Bundled/static FIFA workbook has no scorer-level online data. Click Update FIFA 2026 online and run."))
  openxlsx::addWorksheet(wb, "golden_boot_slope")
  openxlsx::writeData(wb, "golden_boot_slope", empty_golden_boot_slope("Bundled/static FIFA workbook has no scorer-level slopegraph data. Click Update FIFA 2026 online and run."))
  openxlsx::addWorksheet(wb, "README")
  openxlsx::writeData(wb, "README", fifa2026_readme_df())
  openxlsx::addWorksheet(wb, "sources")
  openxlsx::writeData(wb, "sources", fifa2026_sources_df())
  for (s in names(wb)) {
    openxlsx::setColWidths(wb, s, cols = 1:30, widths = "auto")
    openxlsx::freezePane(wb, s, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# ---- NBA 2025-2026 bundled demo data ----------------------------------------
# nba.xlsx demo format: nodes(name,value,value2,cluster) and edges(Leader,follower,WCD).
# cluster: 1 Atlantic, 2 Central, 3 Southeast, 4 Northwest, 5 Pacific, 6 Southwest.
nba2025_nodes_demo_df <- function() {
  data.frame(
    name = c(
      "Boston Celtics", "New York Knicks", "Brooklyn Nets", "Philadelphia 76ers", "Toronto Raptors",
      "Milwaukee Bucks", "Cleveland Cavaliers", "Indiana Pacers", "Chicago Bulls", "Detroit Pistons",
      "Miami Heat", "Orlando Magic", "Atlanta Hawks", "Charlotte Hornets", "Washington Wizards",
      "Denver Nuggets", "Minnesota Timberwolves", "Oklahoma City Thunder", "Portland Trail Blazers", "Utah Jazz",
      "Los Angeles Lakers", "Los Angeles Clippers", "Golden State Warriors", "Phoenix Suns", "Sacramento Kings",
      "Dallas Mavericks", "Houston Rockets", "Memphis Grizzlies", "New Orleans Pelicans", "San Antonio Spurs"
    ),
    value = c(48,51,26,31,30, 46,55,50,35,33, 37,41,36,22,19, 52,49,58,24,21, 47,44,46,40,39, 42,43,38,28,34),
    value2 = rep(82, 30),
    cluster = c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(6,5)),
    stringsAsFactors = FALSE
  )
}

nba2025_edges_demo_df <- function() {
  data.frame(
    Leader = c(
      "New York Knicks","Boston Celtics","New York Knicks","Toronto Raptors",
      "Cleveland Cavaliers","Cleveland Cavaliers","Indiana Pacers","Detroit Pistons",
      "Orlando Magic","Miami Heat","Atlanta Hawks","Charlotte Hornets",
      "Oklahoma City Thunder","Denver Nuggets","Minnesota Timberwolves","Utah Jazz",
      "Los Angeles Lakers","Golden State Warriors","Los Angeles Clippers","Phoenix Suns",
      "Houston Rockets","Dallas Mavericks","Memphis Grizzlies","San Antonio Spurs",
      "Oklahoma City Thunder","Cleveland Cavaliers","Los Angeles Lakers","Houston Rockets","Golden State Warriors","Boston Celtics"
    ),
    follower = c(
      "Boston Celtics","Brooklyn Nets","Philadelphia 76ers","Brooklyn Nets",
      "Milwaukee Bucks","Indiana Pacers","Chicago Bulls","Chicago Bulls",
      "Miami Heat","Atlanta Hawks","Charlotte Hornets","Washington Wizards",
      "Denver Nuggets","Minnesota Timberwolves","Portland Trail Blazers","Portland Trail Blazers",
      "Golden State Warriors","Los Angeles Clippers","Phoenix Suns","Sacramento Kings",
      "Dallas Mavericks","Memphis Grizzlies","San Antonio Spurs","New Orleans Pelicans",
      "New York Knicks","Denver Nuggets","Milwaukee Bucks","Orlando Magic","Miami Heat","Dallas Mavericks"
    ),
    WCD = c(3,3,3,1, 3,3,3,1, 3,1,3,1, 3,1,3,1, 1,1,1,1, 1,1,1,1, 3,1,1,1,1,1),
    stringsAsFactors = FALSE
  )
}

nba2025_validation_df <- function(nodes = nba2025_nodes_demo_df(), edges = nba2025_edges_demo_df()) {
  data.frame(
    item = c("Teams / nodes", "Directed edges", "Minimum cluster", "Maximum cluster", "Missing cluster values", "Workbook role"),
    value = c(nrow(nodes), nrow(edges), min(nodes$cluster, na.rm = TRUE), max(nodes$cluster, na.rm = TRUE), sum(is.na(nodes$cluster) | nodes$cluster == 0), "Demo NBA 2025-2026 nodes/edges; replace nba.xlsx with official/current data if desired"),
    stringsAsFactors = FALSE
  )
}

nba2025_readme_df <- function() {
  data.frame(
    field = c("nodes$name", "nodes$value", "nodes$value2", "nodes$cluster", "edges$Leader", "edges$follower", "edges$WCD", "cluster 1", "cluster 2", "cluster 3", "cluster 4", "cluster 5", "cluster 6"),
    meaning = c(
      "NBA team name", "Demo wins/performance score", "Games played", "NBA division cluster",
      "Directed winner/stronger team", "Directed loser/weaker team", "Demo edge weight",
      "Atlantic", "Central", "Southeast", "Northwest", "Pacific", "Southwest"
    ),
    stringsAsFactors = FALSE
  )
}

write_nba2025_xlsx <- function(file, nodes = nba2025_nodes_demo_df(), edges = nba2025_edges_demo_df()) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package openxlsx is required for xlsx download. Run install.packages('openxlsx', type='binary').", call. = FALSE)
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "nodes")
  openxlsx::writeData(wb, "nodes", nodes)
  openxlsx::addWorksheet(wb, "edges")
  openxlsx::writeData(wb, "edges", edges)
  openxlsx::addWorksheet(wb, "validation")
  openxlsx::writeData(wb, "validation", nba2025_validation_df(nodes, edges))
  openxlsx::addWorksheet(wb, "README")
  openxlsx::writeData(wb, "README", nba2025_readme_df())
  for (s in names(wb)) {
    openxlsx::setColWidths(wb, s, cols = 1:30, widths = "auto")
    openxlsx::freezePane(wb, s, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# ---- FIFA 2026 online updater ------------------------------------------------
# Reads current completed FIFA 2026 group results online, rebuilds nodes/edges,
# saves fifa_2026_updated_nodes_edges.xlsx in the app folder, and returns data
# for immediate network analysis.
fetch_fifa2026_online_nodes_edges <- function(output_file = file.path(getwd(), "fifa_2026_updated_nodes_edges.xlsx"), progress = NULL) {
  progress <- progress %||% function(value = NULL, detail = NULL) invisible(NULL)
  progress(0.01, "Preparing package and URL checks")
  pkgs <- c("httr2", "rvest", "xml2", "stringr", "dplyr", "tidyr", "purrr", "tibble", "openxlsx")
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0) {
    stop(
      "Online FIFA update requires missing packages: ", paste(miss, collapse = ", "),
      ". Install them first, e.g. install.packages(c(",
      paste(sprintf("'%s'", miss), collapse = ", "), "), type='binary')",
      call. = FALSE
    )
  }

  online_started_at <- Sys.time()
  group_urls <- data.frame(
    group = LETTERS[1:12],
    cluster = 1:12,
    url = paste0("https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_", LETTERS[1:12]),
    stringsAsFactors = FALSE
  )
  progress(0.04, "Prepared Wikipedia Group A-L source URLs")

  read_html_ua <- function(url) {
    req <- httr2::request(url) |>
      httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/125 Safari/537.36") |>
      httr2::req_headers(
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language" = "en-US,en;q=0.9"
      ) |>
      httr2::req_timeout(60)
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) >= 400) stop("HTTP error ", httr2::resp_status(resp), " when reading: ", url, call. = FALSE)
    xml2::read_html(httr2::resp_body_string(resp))
  }

  safe_text <- function(node, css) {
    tryCatch({
      y <- rvest::html_element(node, css)
      if (inherits(y, "xml_missing")) return(NA_character_)
      z <- stringr::str_squish(rvest::html_text2(y))
      if (!length(z) || is.na(z) || !nzchar(z)) return(NA_character_)
      z
    }, error = function(e) NA_character_)
  }


  safe_text_raw <- function(node, css) {
    tryCatch({
      y <- rvest::html_element(node, css)
      if (inherits(y, "xml_missing")) return(NA_character_)
      z <- rvest::html_text2(y)
      if (!length(z) || is.na(z) || !nzchar(trimws(z))) return(NA_character_)
      z
    }, error = function(e) NA_character_)
  }

  extract_scorer_rows <- function(node, css, team, group, cluster, match_no_in_group, match_order, source_url) {
    raw <- safe_text_raw(node, css)
    if (is.na(raw) || !nzchar(raw)) return(data.frame())
    raw <- stringr::str_replace_all(raw, "\\[[^\\]]*\\]", "")
    parts <- unlist(strsplit(raw, "\\n|;"))
    parts <- stringr::str_squish(parts)
    parts <- parts[nzchar(parts)]
    parts <- parts[!stringr::str_detect(tolower(parts), "report|attendance|referee|stadium|var|penalties")]
    if (!length(parts)) return(data.frame())

    out <- lapply(parts, function(line) {
      line0 <- stringr::str_squish(line)
      # Golden Boot normally excludes own goals; penalty goals still count.
      if (stringr::str_detect(tolower(line0), "own goal|o\\.g\\.|og\\b")) return(NULL)
      minute_hits <- stringr::str_extract_all(line0, "\\d+(\\+\\d+)?\\s*['’′]")[[1]]
      goals <- length(minute_hits)
      if (!is.finite(goals) || goals <= 0) goals <- 1L
      player <- clean_fifa_scorer_name(line0)
      if (!nzchar(player)) return(NULL)
      data.frame(
        player = player,
        team = team,
        group = group,
        cluster = as.integer(cluster),
        match_no_in_group = as.integer(match_no_in_group),
        match_order = as.integer(match_order),
        goals = as.integer(goals),
        source_url = source_url,
        raw_scorer_text = line0,
        stringsAsFactors = FALSE
      )
    })
    out <- out[!vapply(out, is.null, logical(1))]
    if (!length(out)) return(data.frame())
    do.call(rbind, out)
  }

  clean_team_name <- function(x) {
    x <- stringr::str_replace_all(x, "\\[[^\\]]*\\]", "")
    x <- stringr::str_replace_all(x, "\\(H\\)|\\(A\\)|\\(Q\\)|\\(E\\)", "")
    x <- stringr::str_replace_all(x, "\u00a0", " ")
    stringr::str_squish(x)
  }

  parse_score <- function(score_text) {
    m <- stringr::str_match(score_text, "(\\d+)\\s*[-–—]\\s*(\\d+)")
    if (is.na(m[1, 1])) return(c(NA_integer_, NA_integer_))
    c(as.integer(m[1, 2]), as.integer(m[1, 3]))
  }

  all_matches <- list()
  all_scorers <- list()
  k <- 0L
  sk <- 0L
  pages_read <- 0L
  pages_with_completed_matches <- 0L
  for (ii in seq_len(nrow(group_urls))) {
    g <- group_urls$group[ii]
    cl <- as.integer(group_urls$cluster[ii])
    url <- group_urls$url[ii]
    progress(0.05 + (ii - 1) / nrow(group_urls) * 0.58, paste0("Fetching Wikipedia Group ", g, " (", ii, "/", nrow(group_urls), "): ", url))
    page <- read_html_ua(url)
    pages_read <- pages_read + 1L
    boxes <- rvest::html_elements(page, ".footballbox, table.vevent, div.vevent")
    if (!length(boxes)) next
    k_before_group <- k
    for (jj in seq_along(boxes)) {
      b <- boxes[[jj]]
      team1 <- safe_text(b, ".fhome")
      team2 <- safe_text(b, ".faway")
      score <- safe_text(b, ".fscore")
      if (is.na(team1) || is.na(team2) || is.na(score) || !stringr::str_detect(score, "\\d+\\s*[-–—]\\s*\\d+")) next
      sc <- parse_score(score)
      if (any(is.na(sc))) next
      k <- k + 1L
      all_matches[[k]] <- data.frame(
        group = g, cluster = cl, match_no_in_group = jj,
        team1 = clean_team_name(team1), team2 = clean_team_name(team2),
        score_text = stringr::str_squish(score), score1 = sc[1], score2 = sc[2],
        source_url = url, stringsAsFactors = FALSE
      )
      # Optional scorer-level extraction for the Golden Boot table and slopegraph.
      # Wikipedia footballbox pages commonly use .fhgoal and .fagoal for home/away scorer lists.
      home_team_clean <- clean_team_name(team1)
      away_team_clean <- clean_team_name(team2)
      scorer_rows <- dplyr::bind_rows(
        extract_scorer_rows(b, ".fhgoal", home_team_clean, g, cl, jj, k, url),
        extract_scorer_rows(b, ".fagoal", away_team_clean, g, cl, jj, k, url)
      )
      if (nrow(scorer_rows) > 0) {
        sk <- sk + 1L
        all_scorers[[sk]] <- scorer_rows
      }
    }
    if (k > k_before_group) pages_with_completed_matches <- pages_with_completed_matches + 1L
    progress(0.05 + ii / nrow(group_urls) * 0.58, paste0("Parsed Group ", g, "; cumulative matches = ", k))
  }
  progress(0.66, "Combining parsed matches and removing duplicates")
  if (!length(all_matches)) {
    stop("No completed FIFA 2026 matches were parsed. Check internet access or Wikipedia page structure.", call. = FALSE)
  }

  matches_source <- dplyr::bind_rows(all_matches) |>
    dplyr::filter(!is.na(team1), !is.na(team2), !is.na(score1), !is.na(score2), nzchar(team1), nzchar(team2)) |>
    dplyr::distinct(group, cluster, team1, team2, score1, score2, .keep_all = TRUE) |>
    dplyr::mutate(
      result_type = dplyr::case_when(score1 > score2 ~ "win_loss", score1 < score2 ~ "win_loss", score1 == score2 ~ "draw", TRUE ~ NA_character_),
      winner = dplyr::case_when(score1 > score2 ~ team1, score2 > score1 ~ team2, TRUE ~ NA_character_),
      loser  = dplyr::case_when(score1 > score2 ~ team2, score2 > score1 ~ team1, TRUE ~ NA_character_),
      team1_points = dplyr::case_when(score1 > score2 ~ 3L, score1 == score2 ~ 1L, TRUE ~ 0L),
      team2_points = dplyr::case_when(score2 > score1 ~ 3L, score1 == score2 ~ 1L, TRUE ~ 0L)
    ) |>
    dplyr::arrange(cluster, match_no_in_group)

  scorers_source <- if (length(all_scorers)) {
    dplyr::bind_rows(all_scorers) |>
      dplyr::filter(!is.na(player), nzchar(player), !is.na(team), nzchar(team), goals > 0) |>
      dplyr::arrange(match_order, team, player) |>
      as.data.frame(stringsAsFactors = FALSE)
  } else {
    data.frame(
      player = character(0), team = character(0), group = character(0),
      cluster = integer(0), match_no_in_group = integer(0),
      match_order = integer(0), goals = integer(0),
      source_url = character(0), raw_scorer_text = character(0),
      stringsAsFactors = FALSE
    )
  }
  progress(0.70, paste0("Parsed Golden Boot scorer rows: ", nrow(scorers_source)))

  progress(0.72, paste0("Building directed FIFA edges from ", nrow(matches_source), " completed matches"))
  win_edges <- matches_source |>
    dplyr::filter(score1 != score2) |>
    dplyr::transmute(term1 = winner, term2 = loser, WCD = 3L, group = group, cluster = cluster, score = paste0(score1, "-", score2))
  draw_edges <- matches_source |>
    dplyr::filter(score1 == score2) |>
    dplyr::transmute(term1 = team1, term2 = team2, WCD = 1L, group = group, cluster = cluster, score = paste0(score1, "-", score2))
  edges_detail <- dplyr::bind_rows(win_edges, draw_edges) |>
    dplyr::arrange(cluster, term1, term2)
  edges_xlsx <- edges_detail |> dplyr::select(term1, term2, WCD)
  edges_app <- data.frame(Leader = edges_xlsx$term1, follower = edges_xlsx$term2, WCD = edges_xlsx$WCD, stringsAsFactors = FALSE)

  team_groups <- dplyr::bind_rows(
    matches_source |> dplyr::transmute(name = team1, group = group, cluster = cluster),
    matches_source |> dplyr::transmute(name = team2, group = group, cluster = cluster)
  ) |>
    dplyr::distinct(name, group, cluster) |>
    dplyr::arrange(cluster, name) |>
    dplyr::group_by(name) |>
    dplyr::summarise(group = dplyr::first(group), cluster = as.integer(dplyr::first(cluster)), .groups = "drop")

  progress(0.80, "Building team nodes, values, games played, and Group A-L clusters")
  all_teams <- sort(unique(c(matches_source$team1, matches_source$team2)))
  points_from_term1 <- edges_xlsx |>
    dplyr::group_by(name = term1) |>
    dplyr::summarise(points_term1 = sum(WCD), .groups = "drop")
  points_from_term2_draw <- edges_xlsx |>
    dplyr::filter(WCD == 1) |>
    dplyr::group_by(name = term2) |>
    dplyr::summarise(points_term2_draw = sum(WCD), .groups = "drop")
  games_played <- matches_source |>
    dplyr::select(team1, team2) |>
    tidyr::pivot_longer(cols = c(team1, team2), names_to = "team_position", values_to = "name") |>
    dplyr::count(name, name = "value2")

  nodes <- tibble::tibble(name = all_teams) |>
    dplyr::left_join(points_from_term1, by = "name") |>
    dplyr::left_join(points_from_term2_draw, by = "name") |>
    dplyr::left_join(games_played, by = "name") |>
    dplyr::left_join(team_groups, by = "name") |>
    dplyr::mutate(
      points_term1 = tidyr::replace_na(points_term1, 0L),
      points_term2_draw = tidyr::replace_na(points_term2_draw, 0L),
      value = points_term1 + points_term2_draw,
      value2 = tidyr::replace_na(value2, 0L),
      cluster = as.integer(tidyr::replace_na(cluster, 0L))
    ) |>
    dplyr::select(name, value, value2, cluster) |>
    dplyr::arrange(cluster, dplyr::desc(value), name) |>
    as.data.frame(stringsAsFactors = FALSE)

  n_win_loss <- sum(matches_source$score1 != matches_source$score2)
  n_draw <- sum(matches_source$score1 == matches_source$score2)
  expected_total_points <- 3L * n_win_loss + 2L * n_draw
  validation <- data.frame(
    item = c("Completed matches parsed", "Directed edges", "Teams / nodes", "Win/loss matches", "Draw matches", "Sum(nodes$value)", "Expected total FIFA points", "Check: sum(nodes$value) == expected_total_points", "Check: nrow(edges) == nrow(matches_source)", "Missing node cluster values", "Minimum cluster", "Maximum cluster", "Output file"),
    value = c(as.character(nrow(matches_source)), as.character(nrow(edges_xlsx)), as.character(nrow(nodes)), as.character(n_win_loss), as.character(n_draw), as.character(sum(nodes$value)), as.character(expected_total_points), as.character(sum(nodes$value) == expected_total_points), as.character(nrow(edges_xlsx) == nrow(matches_source)), as.character(sum(is.na(nodes$cluster) | nodes$cluster == 0)), as.character(min(nodes$cluster, na.rm = TRUE)), as.character(max(nodes$cluster, na.rm = TRUE)), output_file),
    stringsAsFactors = FALSE
  )

  progress(0.88, "Validating totals and preparing group performance summary")
  reporting_date <- Sys.time()
  group_performance_summary <- make_fifa_group_performance_summary(nodes, reporting_date = reporting_date)
  golden_boot_summary <- make_fifa_golden_boot_summary(scorers_source, reporting_date = reporting_date, top_n = 30)
  golden_boot_slope <- make_fifa_golden_boot_slope_data(scorers_source, reporting_date = reporting_date, top_n = 10, max_steps = 6)
  progress(0.90, "Preparing workbook sheets including Group A-L summary and Golden Boot chase")
  readme <- fifa2026_readme_df()
  sources <- group_urls
  online_run_info <- data.frame(
    item = c("update_mode", "online_started_at", "online_completed_at", "pages_attempted", "pages_read", "pages_with_completed_matches", "completed_matches_parsed", "scorer_rows_parsed", "source_domain", "output_file"),
    value = c("REAL online Wikipedia scrape; no bundled XLSX fallback", as.character(online_started_at), as.character(Sys.time()), as.character(nrow(group_urls)), as.character(pages_read), as.character(pages_with_completed_matches), as.character(nrow(matches_source)), as.character(nrow(scorers_source)), "en.wikipedia.org", output_file),
    stringsAsFactors = FALSE
  )
  wb <- openxlsx::createWorkbook()
  for (nm in c("nodes", "edges", "edges_detail", "matches_source", "scorers_source", "team_groups", "validation", "group_performance_summary", "golden_boot", "golden_boot_slope", "online_run_info", "README", "sources")) openxlsx::addWorksheet(wb, nm)
  openxlsx::writeData(wb, "nodes", nodes)
  openxlsx::writeData(wb, "edges", edges_xlsx)
  openxlsx::writeData(wb, "edges_detail", edges_detail)
  openxlsx::writeData(wb, "matches_source", matches_source)
  openxlsx::writeData(wb, "scorers_source", scorers_source)
  openxlsx::writeData(wb, "team_groups", team_groups)
  openxlsx::writeData(wb, "validation", validation)
  openxlsx::writeData(wb, "group_performance_summary", group_performance_summary)
  openxlsx::writeData(wb, "golden_boot", golden_boot_summary)
  openxlsx::writeData(wb, "golden_boot_slope", golden_boot_slope)
  openxlsx::writeData(wb, "online_run_info", online_run_info)
  openxlsx::writeData(wb, "README", readme)
  openxlsx::writeData(wb, "sources", sources)
  for (s in names(wb)) {
    openxlsx::setColWidths(wb, s, cols = 1:30, widths = "auto")
    openxlsx::freezePane(wb, s, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

  list(
    nodes = nodes,
    edges = edges_app,
    data_mode = paste0("REAL online FIFA 2026 update from Wikipedia Group A-L pages; completed matches parsed: ", nrow(matches_source), "; pages read: ", pages_read, "/", nrow(group_urls), "; scorer rows parsed: ", nrow(scorers_source), "; workbook saved: ", basename(output_file)),
    online_details = paste0("ONLINE FIFA UPDATE: fetched current Wikipedia pages at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "; pages read ", pages_read, "/", nrow(group_urls), "; pages with completed matches ", pages_with_completed_matches, "; matches parsed ", nrow(matches_source), "; scorer rows parsed ", nrow(scorers_source), "; output file ", basename(output_file)),
    online_run_info = online_run_info,
    group_performance_summary = group_performance_summary,
    golden_boot_summary = golden_boot_summary,
    golden_boot_slope = golden_boot_slope,
    scorers_source = scorers_source,
    matches_source = matches_source
  )
}

nodes_demo_text_value <- paste(c(
  "name,value,value2,carac",
  apply(nodes_demo_df(), 1, paste, collapse = ",")
), collapse = "\n")

edges_demo_text_value <- paste(c(
  "Leader,follower,WCD",
  apply(edges_demo_df(), 1, paste, collapse = ",")
), collapse = "\n")

clean_edges <- function(x) {
  x <- safe_df(x)
  if (nrow(x) == 0 || ncol(x) < 2) stop("Edges table must contain at least two columns.", call. = FALSE)
  nms0 <- tolower(gsub("[^[:alnum:]]", "", names(x)))
  lcol <- which(nms0 %in% c("leader", "from", "source", "node1", "u", "a"))[1]
  fcol <- which(nms0 %in% c("follower", "follow", "to", "target", "node2", "v", "b"))[1]
  wcol <- which(nms0 %in% c("wcd", "weight", "weights", "value", "count", "freq", "strength", "score"))[1]
  if (is.na(lcol)) lcol <- 1
  if (is.na(fcol)) fcol <- 2
  if (is.na(wcol) && ncol(x) >= 3) wcol <- 3
  w <- if (!is.na(wcol)) suppressWarnings(as.numeric(x[[wcol]])) else rep(1, nrow(x))
  if (length(w) != nrow(x)) w <- rep(1, nrow(x))
  w[!is.finite(w) | is.na(w)] <- 1
  out <- data.frame(
    Leader = trimws(as_utf8(x[[lcol]])),
    follower = trimws(as_utf8(x[[fcol]])),
    WCD = w,
    stringsAsFactors = FALSE
  )
  out <- out[nzchar(out$Leader) & nzchar(out$follower) & out$Leader != out$follower, , drop = FALSE]
  if (nrow(out) == 0) stop("No valid edges after cleaning.", call. = FALSE)
  out <- aggregate(WCD ~ Leader + follower, data = out, sum)
  out[order(-out$WCD, out$Leader, out$follower), , drop = FALSE]
}

generate_nodes <- function(edges) {
  ed <- clean_edges(edges)
  inc <- rbind(
    data.frame(name = ed$Leader, weight = ed$WCD, stringsAsFactors = FALSE),
    data.frame(name = ed$follower, weight = ed$WCD, stringsAsFactors = FALSE)
  )
  val <- aggregate(weight ~ name, inc, sum)
  deg <- aggregate(weight ~ name, inc, length)
  names(val)[2] <- "value"
  names(deg)[2] <- "value2"
  out <- merge(val, deg, by = "name", all = TRUE)
  out$carac <- NA_integer_
  out[order(-out$value, out$name), c("name", "value", "value2", "carac"), drop = FALSE]
}


parse_cluster_ids <- function(x) {
  # Preserve uploaded cluster IDs. Accepts numeric clusters such as 1..12 and
  # character labels such as C1, Cluster 12, group_12.  Only when no numeric
  # part is available do we fall back to stable factor codes.
  if (is.null(x)) return(integer(0))
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x) || is.integer(x)) return(suppressWarnings(as.integer(x)))
  z <- trimws(as_utf8(as.character(x)))
  out <- suppressWarnings(as.integer(z))
  bad <- is.na(out) & nzchar(z)
  if (any(bad)) {
    m <- regmatches(z[bad], regexpr("[-]?[0-9]+", z[bad], perl = TRUE))
    out[bad] <- suppressWarnings(as.integer(m))
  }
  # If labels are non-numeric (e.g. A/B/C), preserve them by stable factor coding.
  bad2 <- is.na(out) & nzchar(z)
  if (any(bad2)) {
    lv <- sort(unique(z[bad2]))
    out[bad2] <- match(z[bad2], lv)
  }
  out
}

has_uploaded_clusters <- function(nodes) {
  if (!is.data.frame(nodes) || !("carac" %in% names(nodes))) return(FALSE)
  car <- parse_cluster_ids(nodes$carac)
  any(!is.na(car) & is.finite(car))
}

get_uploaded_membership <- function(nodes, vertex_names) {
  if (!is.data.frame(nodes) || !("name" %in% names(nodes)) || !("carac" %in% names(nodes))) {
    return(NULL)
  }
  nm <- trimws(as_utf8(nodes$name))
  car <- parse_cluster_ids(nodes$carac)
  keep <- nzchar(nm) & !is.na(car) & is.finite(car)
  if (!any(keep)) return(NULL)
  # If a node appears more than once, keep the first uploaded cluster value.
  mp <- setNames(car[keep][!duplicated(nm[keep])], nm[keep][!duplicated(nm[keep])])
  mem <- suppressWarnings(as.integer(mp[as_utf8(vertex_names)]))
  if (!any(!is.na(mem) & is.finite(mem))) return(NULL)
  mem
}

fill_missing_uploaded_membership <- function(mem, fallback, label = "uploaded") {
  # Preserve uploaded cluster IDs exactly. Only vertices without an uploaded cluster
  # are filled with new IDs after max(uploaded), so they cannot be mistaken as C1.
  mem <- suppressWarnings(as.integer(mem))
  fallback <- suppressWarnings(as.integer(fallback))
  if (length(fallback) != length(mem)) fallback <- seq_along(mem)
  miss <- is.na(mem) | !is.finite(mem)
  if (any(miss)) {
    mx <- suppressWarnings(max(mem[!miss], na.rm = TRUE))
    if (!is.finite(mx)) mx <- 0L
    fb <- fallback[miss]
    fb[is.na(fb) | !is.finite(fb)] <- seq_len(sum(miss))
    fb_levels <- sort(unique(fb))
    mem[miss] <- mx + match(fb, fb_levels)
    warning(sum(miss), " vertices had no ", label, " cluster and were assigned fallback cluster IDs after max uploaded cluster.", call. = FALSE)
  }
  mem
}

clean_nodes <- function(nodes, edges) {
  if (is.null(nodes) || !is.data.frame(nodes) || nrow(nodes) == 0) return(generate_nodes(edges))
  nodes <- safe_df(nodes)
  nms0 <- tolower(gsub("[^[:alnum:]]", "", names(nodes)))
  ncol1 <- which(nms0 %in% c("name", "node", "term", "label", "id", "author", "keyword", "institution"))[1]
  vcol <- which(nms0 %in% c("value", "size", "weight", "count", "freq", "strength", "score"))[1]
  v2col <- which(nms0 %in% c("value2", "valueii", "size2", "degree", "count2", "freq2"))[1]
  ccol <- which(nms0 %in% c("carac", "cluster", "group", "membership", "community", "class"))[1]
  if (is.na(ncol1)) ncol1 <- 1
  nr <- nrow(nodes)
  val <- if (!is.na(vcol)) suppressWarnings(as.numeric(nodes[[vcol]])) else rep(NA_real_, nr)
  val2 <- if (!is.na(v2col)) suppressWarnings(as.numeric(nodes[[v2col]])) else val
  car <- if (!is.na(ccol)) parse_cluster_ids(nodes[[ccol]]) else rep(NA_integer_, nr)
  if (length(val) != nr) val <- rep(NA_real_, nr)
  if (length(val2) != nr) val2 <- val
  if (length(car) != nr) car <- rep(NA_integer_, nr)
  out <- data.frame(name = trimws(as_utf8(nodes[[ncol1]])), value = val, value2 = val2, carac = car, stringsAsFactors = FALSE)
  out <- out[nzchar(out$name), , drop = FALSE]
  out <- out[!duplicated(out$name), , drop = FALSE]
  gen <- generate_nodes(edges)
  # V26: keep ALL uploaded nodes, including nodes that are not endpoints of the
  # reduced/edge list. Earlier versions used all.x=TRUE with generated edge nodes
  # on the left, which could drop uploaded nodes and make clusters look missing.
  out <- merge(gen, out, by = "name", all = TRUE, suffixes = c(".gen", ""))
  out$value <- ifelse(is.finite(out$value) & !is.na(out$value), out$value, out$value.gen)
  out$value2 <- ifelse(is.finite(out$value2) & !is.na(out$value2), out$value2, out$value2.gen)
  out$carac <- ifelse(!is.na(out$carac), out$carac, out$carac.gen)
  out$value[!is.finite(out$value) | is.na(out$value)] <- 1
  out$value2[!is.finite(out$value2) | is.na(out$value2)] <- out$value[!is.finite(out$value2) | is.na(out$value2)]
  out[order(-out$value, out$name), c("name", "value", "value2", "carac"), drop = FALSE]
}


# ---- CSV / co-word readers --------------------------------------------------
numeric_rate <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  z <- suppressWarnings(as.numeric(x))
  mean(is.finite(z), na.rm = TRUE)
}

read_csv_file_safe <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop("CSV file not found: ", path, call. = FALSE)
  }
  encs <- c("UTF-8", "UTF-8-BOM", "CP950", "Big5", "BIG5", "GB18030", "latin1")
  last_err <- NULL
  for (enc in encs) {
    df <- tryCatch(
      utils::read.csv(path, fileEncoding = enc, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) { last_err <<- conditionMessage(e); NULL }
    )
    if (!is.null(df) && nrow(df) > 0 && ncol(df) >= 2) return(safe_df(df))
  }
  df <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) { last_err <<- conditionMessage(e); NULL })
  if (!is.null(df) && nrow(df) > 0 && ncol(df) >= 2) return(safe_df(df))
  stop("Could not read a usable CSV file. Last error: ", last_err %||% "unknown", call. = FALSE)
}

is_edge_table <- function(df) {
  nms0 <- tolower(gsub("[^[:alnum:]]", "", names(df)))
  has_l <- any(nms0 %in% c("leader", "from", "source", "node1", "u", "a"))
  has_f <- any(nms0 %in% c("follower", "follow", "to", "target", "node2", "v", "b"))
  isTRUE(has_l && has_f)
}

build_coword_from_df <- function(df, top_n_nodes = 100) {
  df <- safe_df(df)
  if (nrow(df) == 0 || ncol(df) < 2) stop("Coword CSV must contain at least two columns.", call. = FALSE)
  top_n_nodes <- suppressWarnings(as.integer(top_n_nodes))
  if (!is.finite(top_n_nodes) || top_n_nodes < 2) top_n_nodes <- 100L

  # Long format: doc, term[, weight].  This is the preferred format for coword CSV.
  if (!is.finite(numeric_rate(df[[2]])) || numeric_rate(df[[2]]) < 0.80) {
    doc <- trimws(as_utf8(df[[1]]))
    term <- trimws(as_utf8(df[[2]]))
    keep <- nzchar(doc) & nzchar(term) & !is.na(doc) & !is.na(term)
    tmp <- unique(data.frame(doc = doc[keep], term = term[keep], stringsAsFactors = FALSE))
    if (nrow(tmp) == 0) stop("No valid document-term rows found in coword CSV.", call. = FALSE)
    term_count <- sort(table(tmp$term), decreasing = TRUE)
    if (length(term_count) < 2) stop("At least two unique terms are required for coword analysis.", call. = FALSE)
    selected <- names(term_count)[seq_len(min(top_n_nodes, length(term_count)))]
    tmp <- tmp[tmp$term %in% selected, , drop = FALSE]
    docs <- split(tmp$term, tmp$doc)
    docs <- lapply(docs, unique)
    docs <- docs[lengths(docs) > 0]
    terms <- selected[selected %in% unique(tmp$term)]
    X <- matrix(0, nrow = length(docs), ncol = length(terms), dimnames = list(names(docs), terms))
    for (i in seq_along(docs)) X[i, match(docs[[i]], terms)] <- 1
    node_value <- colSums(X)
    C <- crossprod(X); diag(C) <- 0
    nodes <- data.frame(name = names(node_value), value = as.numeric(node_value), value2 = as.numeric(node_value), carac = NA_integer_, stringsAsFactors = FALSE)
    ij <- which(C > 0, arr.ind = TRUE)
    ij <- ij[ij[,1] < ij[,2], , drop = FALSE]
    if (nrow(ij) == 0) stop("Coword CSV produced no co-occurrence edges. Each document needs at least two terms.", call. = FALSE)
    edges <- data.frame(Leader = colnames(C)[ij[,1]], follower = colnames(C)[ij[,2]], WCD = as.numeric(C[ij]), stringsAsFactors = FALSE)
    return(list(nodes = nodes[order(-nodes$value, nodes$name), , drop = FALSE], edges = edges[order(-edges$WCD, edges$Leader, edges$follower), , drop = FALSE], data_mode = paste0("coword CSV long format; top ", nrow(nodes), " nodes from dataset")))
  }

  # Wide format: rows=documents, numeric columns=terms.
  num_cols <- which(vapply(df, function(z) is.finite(numeric_rate(z)) && numeric_rate(z) >= 0.80, logical(1)))
  if (length(num_cols) < 2) stop("Wide coword CSV needs at least two numeric term columns, or use long format: doc,term.", call. = FALSE)
  X <- as.matrix(data.frame(lapply(df[num_cols], function(z) suppressWarnings(as.numeric(z))), check.names = FALSE))
  X[!is.finite(X)] <- 0
  colnames(X) <- names(df)[num_cols]
  keep <- colSums(abs(X)) > 0
  X <- X[, keep, drop = FALSE]
  if (ncol(X) < 2) stop("At least two non-zero term columns are required.", call. = FALSE)
  B <- ifelse(X > 0, 1, 0)
  node_value <- colSums(B)
  ord <- order(-node_value, names(node_value))
  keep_idx <- ord[seq_len(min(top_n_nodes, length(ord)))]
  B <- B[, keep_idx, drop = FALSE]
  node_value <- node_value[keep_idx]
  C <- crossprod(B); diag(C) <- 0
  nodes <- data.frame(name = colnames(B), value = as.numeric(node_value), value2 = as.numeric(colSums(X[, keep_idx, drop = FALSE])), carac = NA_integer_, stringsAsFactors = FALSE)
  ij <- which(C > 0, arr.ind = TRUE)
  ij <- ij[ij[,1] < ij[,2], , drop = FALSE]
  if (nrow(ij) == 0) stop("Wide coword CSV produced no co-occurrence edges.", call. = FALSE)
  edges <- data.frame(Leader = colnames(C)[ij[,1]], follower = colnames(C)[ij[,2]], WCD = as.numeric(C[ij]), stringsAsFactors = FALSE)
  list(nodes = nodes[order(-nodes$value, nodes$name), , drop = FALSE], edges = edges[order(-edges$WCD, edges$Leader, edges$follower), , drop = FALSE], data_mode = paste0("coword CSV wide format; top ", nrow(nodes), " nodes from dataset"))
}

csv_to_nodes_edges <- function(path, top_n_nodes = 100) {
  df <- read_csv_file_safe(path)
  if (is_edge_table(df)) {
    ed <- clean_edges(df)
    return(list(nodes = generate_nodes(ed), edges = ed, data_mode = paste0("edge-list CSV: ", basename(path))))
  }
  out <- build_coword_from_df(df, top_n_nodes = top_n_nodes)
  out$data_mode <- paste0(out$data_mode, ": ", basename(path))
  out
}

read_excel_first <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is required. Run install.packages('readxl', type='binary').", call. = FALSE)
  safe_df(readxl::read_excel(path, sheet = 1))
}

read_excel_workbook <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is required. Run install.packages('readxl', type='binary').", call. = FALSE)
  sheets <- readxl::excel_sheets(path)
  sl <- tolower(sheets)
  ei <- which(sl %in% c("edges", "edge", "links", "link"))[1]
  ni <- which(sl %in% c("nodes", "node", "vertices", "vertex"))[1]
  if (is.na(ei)) stop("Workbook must contain an edges sheet.", call. = FALSE)
  gp_i <- which(sl %in% c("group_performance_summary", "groupperformance", "fifa_group_summary"))[1]
  gb_i <- which(sl %in% c("golden_boot", "goldenboot", "golden_boot_summary"))[1]
  gs_i <- which(sl %in% c("golden_boot_slope", "goldenbootslope", "golden_boot_slopegraph"))[1]
  list(
    edges = safe_df(readxl::read_excel(path, sheet = sheets[ei])),
    nodes = if (!is.na(ni)) safe_df(readxl::read_excel(path, sheet = sheets[ni])) else NULL,
    group_performance_summary = if (!is.na(gp_i)) safe_df(readxl::read_excel(path, sheet = sheets[gp_i])) else NULL,
    golden_boot_summary = if (!is.na(gb_i)) safe_df(readxl::read_excel(path, sheet = sheets[gb_i])) else NULL,
    golden_boot_slope = if (!is.na(gs_i)) safe_df(readxl::read_excel(path, sheet = sheets[gs_i])) else NULL
  )
}

build_graphs <- function(nodes, edges) {
  ed <- clean_edges(edges)
  nd <- clean_nodes(nodes, ed)
  verts <- nd
  # V26: preserve uploaded node order first, then append edge-only nodes.
  alln <- unique(c(verts$name, ed$Leader, ed$follower))
  verts <- merge(data.frame(name = alln, .ord = seq_along(alln), stringsAsFactors = FALSE), verts, by = "name", all.x = TRUE, sort = FALSE)
  verts <- verts[order(verts$.ord), , drop = FALSE]
  verts$.ord <- NULL
  verts$value[!is.finite(verts$value) | is.na(verts$value)] <- 1
  verts$value2[!is.finite(verts$value2) | is.na(verts$value2)] <- verts$value[!is.finite(verts$value2) | is.na(verts$value2)]
  g <- igraph::graph_from_data_frame(ed, directed = FALSE, vertices = verts)
  if (igraph::ecount(g) > 0) igraph::E(g)$weight <- pmax(as.numeric(igraph::E(g)$WCD), 1e-6)
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = "sum", WCD = "sum"))
  e0 <- data.frame(a = ed$Leader, b = ed$follower, w = ed$WCD, stringsAsFactors = FALSE)
  e0$u <- pmin(e0$a, e0$b)
  e0$v <- pmax(e0$a, e0$b)
  e0$key <- paste(e0$u, e0$v, sep = "||")
  inc <- rbind(data.frame(node = e0$a, key = e0$key, w = e0$w), data.frame(node = e0$b, key = e0$key, w = e0$w))
  inc <- inc[order(inc$node, -inc$w, inc$key), , drop = FALSE]
  best <- inc[!duplicated(inc$node), , drop = FALSE]
  red <- unique(e0[e0$key %in% best$key, c("u", "v", "w")])
  names(red) <- c("Leader", "follower", "WCD")
  gf <- igraph::graph_from_data_frame(red, directed = FALSE, vertices = verts)
  if (igraph::ecount(gf) > 0) igraph::E(gf)$weight <- pmax(as.numeric(igraph::E(gf)$WCD), 1e-6)
  gf <- igraph::simplify(gf, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = "sum", WCD = "sum"))
  list(nodes = nd, edges = ed, vertex_df = verts, g_full = g, g_flca = gf, edges_reduced = red)
}

norm_mem <- function(x, n) {
  x <- suppressWarnings(as.integer(x))
  if (length(x) != n) x <- rep(1L, n)
  x[is.na(x)] <- 1L
  x
}

cluster_safe <- function(g, method) {
  n <- igraph::vcount(g)
  if (n == 0) return(integer(0))
  if (igraph::ecount(g) == 0) return(seq_len(n))
  tryCatch({
    if (method == "louvain") return(norm_mem(igraph::membership(igraph::cluster_louvain(g, weights = igraph::E(g)$weight)), n))
    if (method == "components") return(norm_mem(igraph::components(g)$membership, n))
    if (method == "label_prop") return(norm_mem(igraph::membership(igraph::cluster_label_prop(g, weights = igraph::E(g)$weight)), n))
    if (method == "infomap") return(norm_mem(igraph::membership(igraph::cluster_infomap(g, e.weights = igraph::E(g)$weight)), n))
    if (method == "leading_eigen") return(norm_mem(igraph::membership(igraph::cluster_leading_eigen(g, weights = igraph::E(g)$weight)), n))
    if (method == "walktrap") return(norm_mem(igraph::membership(igraph::cluster_walktrap(g, weights = igraph::E(g)$weight)), n))
    if (method == "fast_greedy") return(norm_mem(igraph::membership(igraph::cluster_fast_greedy(g, weights = igraph::E(g)$weight)), n))
    norm_mem(igraph::components(g)$membership, n)
  }, error = function(e) norm_mem(igraph::components(g)$membership, n))
}

mod_safe <- function(g, m) {
  m <- norm_mem(m, igraph::vcount(g))
  if (igraph::ecount(g) == 0 || length(unique(m)) < 2) return(0)
  suppressWarnings(tryCatch(igraph::modularity(g, m, weights = igraph::E(g)$weight), error = function(e) 0))
}

sil_safe <- function(g, m) {
  n <- igraph::vcount(g)
  m <- norm_mem(m, n)
  if (n < 2 || length(unique(m)) < 2 || igraph::ecount(g) == 0) return(0)
  tryCatch({
    D <- igraph::distances(g, weights = 1 / pmax(igraph::E(g)$weight, 1e-6))
    mx <- max(D[is.finite(D)], na.rm = TRUE)
    if (!is.finite(mx) || mx <= 0) mx <- 1
    D[!is.finite(D)] <- mx * 1.25
    s <- numeric(n)
    for (i in seq_len(n)) {
      same <- which(m == m[i])
      other <- setdiff(unique(m), m[i])
      a <- if (length(same) > 1) mean(D[i, setdiff(same, i)], na.rm = TRUE) else 0
      b <- if (length(other) > 0) min(vapply(other, function(z) mean(D[i, which(m == z)], na.rm = TRUE), numeric(1))) else 0
      den <- max(a, b)
      s[i] <- if (is.finite(den) && den > 0) (b - a) / den else 0
    }
    mean(s, na.rm = TRUE)
  }, error = function(e) 0)
}

cmp_safe <- function(a, b, method) {
  if (length(a) != length(b)) return(NA_real_)
  suppressWarnings(tryCatch(igraph::compare(a, b, method = method), error = function(e) NA_real_))
}

analyze_basic <- function(nodes, edges, target_k = 4, flca_mode = "value") {
  gr <- build_graphs(nodes, edges)
  g <- gr$g_full
  gf <- gr$g_flca
  n <- igraph::vcount(g)
  methods <- c("louvain", "optimal", "components", "edge_betweenness", "label_prop", "infomap", "leading_eigen", "walktrap", "fast_greedy")
  mems <- data.frame(name = igraph::V(g)$name, stringsAsFactors = FALSE)
  status <- setNames(rep("ok", length(methods)), methods)
  details <- setNames(rep("", length(methods)), methods)
  for (m in methods) {
    if (m %in% c("optimal", "edge_betweenness")) {
      mems[[m]] <- cluster_safe(g, "components")
      status[m] <- "skipped/fallback"
      details[m] <- "Skipped for Shiny stability; components fallback used."
    } else {
      mems[[m]] <- cluster_safe(g, m)
    }
  }
  # V26: preserve uploaded nodes$carac / nodes$cluster as the FLCA membership.
  # The reduced FLCA graph still shows strongest-link edges, but its node colors,
  # SSplot, Kano, quality rows, and membership table must use the original uploaded
  # cluster numbers when they are available. Do NOT re-number by components(gf).
  uploaded_mem_full <- get_uploaded_membership(gr$nodes, igraph::V(g)$name)
  if (!is.null(uploaded_mem_full)) {
    # V26: do not call norm_mem() on uploaded clusters, because norm_mem()
    # replaces NA with 1 and can falsely move unmatched nodes into cluster 1.
    fallback_full <- cluster_safe(g, "components")
    fm <- fill_missing_uploaded_membership(uploaded_mem_full, fallback_full, label = "uploaded nodes$carac/cluster")
    names(fm) <- igraph::V(g)$name
    fm_red <- fm[match(igraph::V(gf)$name, names(fm))]
    fallback_red <- igraph::components(gf)$membership
    fm_red <- fill_missing_uploaded_membership(fm_red, fallback_red, label = "uploaded nodes$carac/cluster for reduced graph")
    names(fm_red) <- igraph::V(gf)$name
    cluster_source <- "original uploaded nodes$carac/cluster strictly preserved"
    original_clusters_used <- TRUE
  } else {
    fm_red <- norm_mem(igraph::components(gf)$membership, igraph::vcount(gf))
    names(fm_red) <- igraph::V(gf)$name
    fm <- norm_mem(fm_red[match(igraph::V(g)$name, names(fm_red))], n)
    names(fm) <- igraph::V(g)$name
    cluster_source <- "computed FLCA reduced-graph components"
    original_clusters_used <- FALSE
  }
  mems$FLCA <- as.integer(fm)
  qrows <- lapply(methods, function(m) {
    mm <- mems[[m]]
    data.frame(method = m, n_clusters = length(unique(mm)), modularity = mod_safe(g, mm), mean_silhouette = sil_safe(g, mm),
               ARI_vs_FLCA = cmp_safe(mm, fm, "adjusted.rand"), NMI_vs_FLCA = cmp_safe(mm, fm, "nmi"),
               status = status[m], details = details[m], eval_graph = "full", stringsAsFactors = FALSE)
  })
  qdf <- do.call(rbind, qrows)
  extra <- data.frame(
    method = c("FLCA by maturity (single)", "FLCA by influence (single)", "FLCA by maturity (full)", "FLCA by influence (full)", paste0("components_independent_k", target_k)),
    n_clusters = c(length(unique(fm_red)), length(unique(fm_red)), length(unique(fm)), length(unique(fm)), length(unique(mems$components))),
    modularity = c(mod_safe(gf, fm_red), mod_safe(gf, fm_red), mod_safe(g, fm), mod_safe(g, fm), mod_safe(g, mems$components)),
    mean_silhouette = c(sil_safe(gf, fm_red), sil_safe(gf, fm_red), sil_safe(g, fm), sil_safe(g, fm), sil_safe(g, mems$components)),
    ARI_vs_FLCA = c(NA, NA, 1, 1, cmp_safe(mems$components, fm, "adjusted.rand")),
    NMI_vs_FLCA = c(NA, NA, 1, 1, cmp_safe(mems$components, fm, "nmi")),
    status = "ok", details = paste0("V30 stable rebuild; cluster source: ", cluster_source), eval_graph = c("single", "single", "full", "full", "thresholded_full"), stringsAsFactors = FALSE
  )
  qdf <- rbind(qdf, extra)
  ranking <- qdf[!grepl("^FLCA", qdf$method), , drop = FALSE]
  ranking <- ranking[order(-ranking$ARI_vs_FLCA, -ranking$NMI_vs_FLCA, -ranking$modularity), , drop = FALSE]
  list(nodes = gr$nodes, edges = gr$edges, vertex_df = gr$vertex_df, g_full = g, g_flca = gf, edges_reduced = gr$edges_reduced,
       memberships_df = mems, quality_df = qdf, ranking_df = ranking, flca_carac = fm_red, flca_full = fm,
       flca_mode = flca_mode, cluster_source = cluster_source, original_clusters_used = original_clusters_used,
       status_note = paste0("V36 TRUE REBUILD: ", cluster_source, "; dataset1.csv demo, Excel nodes/edges upload, CSV coword upload, real renderSSplot(82), red tabs, dynamic Kano.R Q-vs-SS table, node-level Kano under SSplot."))
}

make_vis <- function(g, membership = NULL, top_n = NULL, title = "Network", label_size = 30, bold = TRUE) {
  if (!is.null(top_n) && is.finite(top_n) && top_n > 0 && top_n < igraph::vcount(g)) {
    vals <- suppressWarnings(as.numeric(igraph::V(g)$value)); vals[!is.finite(vals)] <- 1
    keep <- igraph::V(g)$name[order(-vals, igraph::V(g)$name)[seq_len(top_n)]]
    g <- igraph::induced_subgraph(g, keep)
  }
  n <- igraph::vcount(g)
  if (is.null(membership)) {
    membership <- igraph::components(g)$membership
  } else {
    if (!is.null(names(membership)) && all(igraph::V(g)$name %in% names(membership))) {
      membership <- membership[igraph::V(g)$name]
    } else if (length(membership) < n) {
      membership <- igraph::components(g)$membership
    } else {
      membership <- membership[seq_len(n)]
    }
  }
  groups <- as.character(norm_mem(membership, n))
  ug <- sort(unique(groups))
  pal <- if (length(ug) <= 8) RColorBrewer::brewer.pal(max(3, length(ug)), "Set2") else grDevices::rainbow(length(ug))
  names(pal) <- ug
  vals <- suppressWarnings(as.numeric(igraph::V(g)$value)); vals[!is.finite(vals)] <- 1
  nd <- data.frame(id = igraph::V(g)$name, label = igraph::V(g)$name, group = paste0("Cluster ", groups), value = sqrt(vals), color.background = pal[groups], stringsAsFactors = FALSE)
  edf <- igraph::as_data_frame(g, what = "edges")
  if (nrow(edf) > 0) {
    w <- if ("weight" %in% names(edf)) edf$weight else rep(1, nrow(edf))
    edv <- data.frame(from = edf$from, to = edf$to, value = pmax(1, log1p(w) * 2), title = paste("Weight:", round(w, 3)), stringsAsFactors = FALSE)
  } else {
    edv <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
  }
  visNetwork::visNetwork(nd, edv, main = as.character(title), height = "720px", width = "100%") |>
    visNetwork::visNodes(font = list(size = label_size, face = if (isTRUE(bold)) "Arial" else "arial", strokeWidth = 3, strokeColor = "white")) |>
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group") |>
    visNetwork::visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) |>
    visNetwork::visInteraction(navigationButtons = TRUE, keyboard = TRUE)
}


# Static PNG network renderer for FLCA Process download buttons.
plot_static_network_png <- function(g, membership = NULL, title = "Network", top_n = NULL) {
  if (is.null(g) || igraph::vcount(g) == 0) {
    plot.new(); text(0.5, 0.5, "No network available", cex = 1.2, font = 2); return(invisible(NULL))
  }
  if (!is.null(top_n) && is.finite(top_n) && top_n > 0 && top_n < igraph::vcount(g)) {
    vals <- suppressWarnings(as.numeric(igraph::V(g)$value)); vals[!is.finite(vals)] <- 1
    keep <- igraph::V(g)$name[order(-vals, igraph::V(g)$name)[seq_len(top_n)]]
    g <- igraph::induced_subgraph(g, keep)
  }
  n <- igraph::vcount(g)
  if (is.null(membership)) {
    membership <- igraph::components(g)$membership
  } else if (!is.null(names(membership)) && all(igraph::V(g)$name %in% names(membership))) {
    membership <- membership[igraph::V(g)$name]
  } else if (length(membership) != n) {
    membership <- igraph::components(g)$membership
  }
  membership <- as.integer(membership)
  membership[is.na(membership)] <- 1L
  vals <- suppressWarnings(as.numeric(igraph::V(g)$value)); vals[!is.finite(vals)] <- 1
  node_cex <- pmax(0.8, pmin(3.2, sqrt(pmax(vals, 0)) / max(sqrt(pmax(vals, 0)), na.rm = TRUE) * 2.7 + 0.5))
  lev <- sort(unique(membership))
  pal <- grDevices::hcl.colors(max(3, length(lev)), "Dark 3")
  colmap <- setNames(pal[seq_along(lev)], as.character(lev))
  vcols <- unname(colmap[as.character(membership)])
  ew <- if (igraph::ecount(g) > 0) suppressWarnings(as.numeric(igraph::E(g)$weight)) else numeric(0)
  if (length(ew)) ew <- pmax(1, pmin(8, log1p(ew) * 2.2))
  set.seed(123)
  lay <- igraph::layout_with_fr(g, weights = if (igraph::ecount(g) > 0) igraph::E(g)$weight else NULL)
  op <- par(mar = c(1, 1, 3, 1))
  on.exit(par(op), add = TRUE)
  plot(g, layout = lay, vertex.color = vcols, vertex.size = 8 + node_cex * 9,
       vertex.label = igraph::V(g)$name, vertex.label.cex = 0.75,
       vertex.label.color = "black", vertex.frame.color = "white",
       edge.width = ew, edge.color = grDevices::adjustcolor("#0050B5", alpha.f = 0.55),
       main = title)
  invisible(NULL)
}


community_methods <- c("louvain", "optimal", "components", "edge_betweenness", "label_prop", "infomap", "leading_eigen", "walktrap", "fast_greedy")


# ---- Real SSplot renderer ---------------------------------------------------
# This replaces the temporary algorithm bar chart. It draws an SSplot-like
# cluster view using the FLCA reduced graph, node-level silhouette widths,
# cluster summaries, modularity, and AAC-style dominance information.
sil_widths_vec <- function(g, m) {
  n <- igraph::vcount(g)
  m <- norm_mem(m, n)
  if (n < 2 || length(unique(m)) < 2 || igraph::ecount(g) == 0) return(rep(0, n))

  # V35 fix for externally preserved clusters such as FIFA groups.
  # In FIFA group-stage data there are usually no cross-group edges. The old
  # code replaced all disconnected distances with the same constant, so the
  # distance from a team to another FIFA group could become effectively the same
  # as the distance to an unplayed team inside its own group. That made group SS
  # artificially low and made the inter-group separation look like zero.
  # Here, disconnected pairs in different clusters are treated as farther apart
  # than disconnected pairs inside the same preserved cluster.
  tryCatch({
    w <- if (igraph::ecount(g) > 0) suppressWarnings(as.numeric(igraph::E(g)$weight)) else numeric(0)
    w[!is.finite(w) | w <= 0] <- 1e-6
    D <- igraph::distances(g, weights = 1 / w)
    mx <- max(D[is.finite(D) & D > 0], na.rm = TRUE)
    if (!is.finite(mx) || mx <= 0) mx <- 1

    bad <- !is.finite(D)
    if (any(bad)) {
      same_cluster <- outer(m, m, FUN = "==")
      D[bad & same_cluster] <- mx * 1.25
      D[bad & !same_cluster] <- mx * 3.00
    }
    diag(D) <- 0

    s <- numeric(n)
    for (i in seq_len(n)) {
      same <- which(m == m[i])
      other <- setdiff(unique(m), m[i])
      a <- if (length(same) > 1) mean(D[i, setdiff(same, i)], na.rm = TRUE) else 0
      b <- if (length(other) > 0) min(vapply(other, function(z) mean(D[i, which(m == z)], na.rm = TRUE), numeric(1))) else 0
      den <- max(a, b)
      s[i] <- if (is.finite(den) && den > 0) (b - a) / den else 0
    }
    s[!is.finite(s)] <- 0
    pmax(pmin(s, 1), -1)
  }, error = function(e) rep(0, n))
}

mod_safe_unweighted <- function(g, m) {
  m <- norm_mem(m, igraph::vcount(g))
  if (igraph::ecount(g) == 0 || length(unique(m)) < 2) return(0)
  suppressWarnings(tryCatch(igraph::modularity(g, m, weights = rep(1, igraph::ecount(g))), error = function(e) 0))
}

aac_from_values <- function(x) {
  x <- sort(suppressWarnings(as.numeric(x)), decreasing = TRUE)
  x <- x[is.finite(x) & x > 0]
  if (length(x) < 3 || x[2] == 0 || x[3] == 0) return(NA_real_)
  r <- (x[1] / x[2]) / (x[2] / x[3])
  r / (1 + r)
}

gini_simpson <- function(x) {
  tab <- table(x)
  if (!length(tab)) return(0)
  p <- as.numeric(tab) / sum(tab)
  1 - sum(p^2)
}

make_real_ssplot <- function(xres, target_n = 20) {
  shiny::req(xres)
  g <- xres$g_flca
  shiny::validate(shiny::need(!is.null(g) && igraph::vcount(g) > 0, "Run analysis first."))

  # V26: this legacy ggplot SSplot also uses the preserved FLCA membership.
  if (!is.null(xres$flca_carac)) {
    mem0 <- xres$flca_carac
    if (!is.null(names(mem0)) && all(igraph::V(g)$name %in% names(mem0))) {
      mem0 <- mem0[igraph::V(g)$name]
    }
  } else {
    mem0 <- igraph::components(g)$membership
  }
  mem0 <- fill_missing_uploaded_membership(mem0, igraph::components(g)$membership, label = "SSplot membership")
  vals <- suppressWarnings(as.numeric(igraph::V(g)$value))
  vals[!is.finite(vals) | is.na(vals)] <- 1
  vals2 <- suppressWarnings(as.numeric(igraph::V(g)$value2))
  vals2[!is.finite(vals2) | is.na(vals2)] <- vals[!is.finite(vals2) | is.na(vals2)]
  sil <- sil_widths_vec(g, mem0)
  deg <- igraph::degree(g)

  raw <- data.frame(
    name = igraph::V(g)$name,
    raw_cluster = mem0,
    value = vals,
    value2 = vals2,
    degree = as.integer(deg),
    sil = sil,
    stringsAsFactors = FALSE
  )

  # V26: keep original cluster IDs here too; no display-order re-numbering.
  raw$cluster <- as.integer(raw$raw_cluster)

  target_n <- suppressWarnings(as.integer(target_n))
  if (!is.finite(target_n) || target_n < 1) target_n <- 20L
  keep <- raw[order(raw$cluster, -raw$value, -raw$sil, raw$name), , drop = FALSE]
  keep <- keep[seq_len(min(target_n, nrow(keep))), , drop = FALSE]
  keep <- keep[order(keep$cluster, -keep$value, -keep$sil, keep$name), , drop = FALSE]
  keep$rank <- ave(keep$value, keep$cluster, FUN = seq_along)
  keep$y <- rev(seq_len(nrow(keep)))
  keep$node_left <- paste0(
    keep$name, "#", keep$cluster,
    " (", round(keep$value, 0), "|", keep$degree, "|", sprintf("%.2f", keep$sil), "|C", keep$cluster, ")"
  )

  q_w <- mod_safe(g, mem0)
  q_u <- mod_safe_unweighted(g, mem0)
  ss <- mean(sil, na.rm = TRUE)
  aac <- aac_from_values(raw$value)
  aac2 <- aac_from_values(raw$value2)
  hhi <- sum((raw$value / sum(raw$value, na.rm = TRUE))^2, na.rm = TRUE)
  gs <- gini_simpson(raw$cluster)
  d20 <- sum(keep$value, na.rm = TRUE) / max(sum(raw$value, na.rm = TRUE), 1)

  cl_sum <- aggregate(cbind(sil, value) ~ cluster, keep, function(z) c(mean = mean(z, na.rm = TRUE), sum = sum(z, na.rm = TRUE), n = length(z)))
  # Convert the aggregate matrix columns into plain numeric columns.
  cl_sum2 <- data.frame(
    cluster = cl_sum$cluster,
    ss = as.numeric(cl_sum$sil[, "mean"]),
    n = as.integer(cl_sum$sil[, "n"]),
    total_value = as.numeric(cl_sum$value[, "sum"]),
    stringsAsFactors = FALSE
  )
  yy <- aggregate(y ~ cluster, keep, mean)
  cl_sum2 <- merge(cl_sum2, yy, by = "cluster", all.x = TRUE)
  cl_sum2$lab <- paste0(
    "|SS=", sprintf("%.2f", cl_sum2$ss),
    "  |", sprintf("%.2f", q_w),
    "  |", sprintf("%.2f", q_u),
    "  |n=", cl_sum2$n
  )

  cols <- if (length(unique(keep$cluster)) <= 8) {
    RColorBrewer::brewer.pal(max(3, length(unique(keep$cluster))), "Set2")[seq_along(unique(keep$cluster))]
  } else {
    grDevices::rainbow(length(unique(keep$cluster)), s = 0.65, v = 0.9)
  }
  names(cols) <- as.character(sort(unique(keep$cluster)))

  main_top <- paste0(
    "D20=", sprintf("%.2f", d20),
    "   Q/D20=", sprintf("%.2f", ifelse(d20 > 0, q_w / d20, 0)),
    "   Qmax-HHI=", sprintf("%.2f", max(0, 1 - hhi)),
    "   Q*/Qmax=", sprintf("%.2f", ifelse((1 - hhi) > 0, q_w / (1 - hhi), 0))
  )
  metrics_top <- paste0(
    "SS=", sprintf("%.2f", ss),
    " | Qw=", sprintf("%.2f", q_w),
    " | Qu=", sprintf("%.2f", q_u),
    " | AAC=", ifelse(is.finite(aac), sprintf("%.2f", aac), "NA"),
    " | AAC2=", ifelse(is.finite(aac2), sprintf("%.2f", aac2), "NA")
  )
  caption_txt <- paste0(
    "(n=", nrow(keep), ", N=", nrow(raw),
    ", v20=", round(sum(keep$value, na.rm = TRUE), 0),
    ", v=", round(sum(raw$value, na.rm = TRUE), 0),
    ", C#=", length(unique(raw$cluster)),
    ", Dn(GSI:Gini-Simpson)=", sprintf("%.2f", gs), ")"
  )

  ggplot2::ggplot(keep) +
    ggplot2::geom_vline(xintercept = 0, color = "grey45", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = 0.7, color = "red", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = sil, y = y, yend = y, fill = factor(cluster), color = factor(cluster)),
                          linewidth = 8, alpha = 0.65, lineend = "butt") +
    ggplot2::geom_segment(ggplot2::aes(x = -0.08, xend = 0, y = y, yend = y, color = factor(cluster)),
                          arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"), linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(x = -0.53, y = y, label = node_left), hjust = 1, size = 3.4, fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(x = 0.02, y = y, label = name, color = factor(cluster)), hjust = 0, size = 3.3) +
    ggplot2::geom_text(data = cl_sum2, ggplot2::aes(x = 1.05, y = y, label = lab), inherit.aes = FALSE,
                       hjust = 0, size = 3.7, color = "red", fontface = "bold") +
    ggplot2::annotate("text", x = 0.32, y = max(keep$y) + 2.3, label = main_top, size = 4.0, fontface = "bold", color = "black") +
    ggplot2::annotate("text", x = 0.32, y = max(keep$y) + 1.3, label = metrics_top, size = 4.0, fontface = "bold", color = "red") +
    ggplot2::annotate("text", x = 0.7, y = -0.8, label = "0.7", color = "red", size = 3.5, fontface = "bold") +
    ggplot2::scale_color_manual(values = cols, guide = "none") +
    ggplot2::scale_fill_manual(values = cols, guide = "none") +
    ggplot2::scale_x_continuous(limits = c(-0.5, 1.0), breaks = c(-0.5, 0, 0.5, 0.7, 1.0), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL, limits = c(0, max(keep$y) + 3), expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = c(-0.5, 1.0), clip = "off") +
    ggplot2::labs(x = "Silhouette width", y = NULL, title = "Real SSplot: FLCA node-level cluster summary", caption = caption_txt) +
    ggplot2::theme_minimal(base_size = 13, base_family = "sans") +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(18, 160, 25, 230),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}




# ---- Shared Top-N selector: leader-first, max 4 nodes per final cluster -------
# This selector is used by Step 6 SSplot and Step 6c chord so that both panels
# contain exactly the same final node set. It first takes the highest-value
# leader from each cluster, then fills remaining positions round-robin with the
# next highest-value members, never taking more than max_per_cluster from any
# cluster.
select_top_n_max_per_cluster <- function(df, target_n = 20, max_per_cluster = 4) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df)) return(df)
  if (!all(c("name", "carac") %in% names(df))) stop("select_top_n_max_per_cluster requires name and carac columns.")
  if (!"value" %in% names(df)) df$value <- 0
  if (!"value2" %in% names(df)) df$value2 <- df$value
  if (!"sil_width" %in% names(df)) df$sil_width <- 0

  target_n <- suppressWarnings(as.integer(target_n))[1]
  if (!is.finite(target_n) || target_n <= 0) target_n <- 20L
  max_per_cluster <- suppressWarnings(as.integer(max_per_cluster))[1]
  if (!is.finite(max_per_cluster) || max_per_cluster <= 0) max_per_cluster <- 4L

  df$name <- trimws(as.character(df$name))
  df$carac <- suppressWarnings(as.integer(gsub("^C", "", toupper(as.character(df$carac)))))
  df$value <- suppressWarnings(as.numeric(df$value))
  df$value2 <- suppressWarnings(as.numeric(df$value2))
  df$sil_width <- suppressWarnings(as.numeric(df$sil_width))
  df$value[!is.finite(df$value)] <- 0
  df$value2[!is.finite(df$value2)] <- 0
  df$sil_width[!is.finite(df$sil_width)] <- 0
  df <- df[nzchar(df$name) & is.finite(df$carac), , drop = FALSE]
  df <- df[!duplicated(df$name), , drop = FALSE]
  if (!nrow(df)) return(df)

  # V37 reviewer rule: global Top-N by nodes$value, but max 4 nodes per cluster.
  # This avoids losing high-value leaders such as Germany, United States, and Mexico.
  df_ord <- df[order(-df$value, -df$value2, -df$sil_width, df$name), , drop = FALSE]
  picked <- character(0)
  counts <- integer(0)
  for (i in seq_len(nrow(df_ord))) {
    if (length(picked) >= target_n) break
    cl <- as.character(df_ord$carac[i])
    if (is.na(counts[cl])) counts[cl] <- 0L
    if (counts[cl] < max_per_cluster) {
      picked <- c(picked, df_ord$name[i])
      counts[cl] <- counts[cl] + 1L
    }
  }

  # Last resort for very small datasets: if the cap cannot mathematically reach N,
  # fill remaining positions by global value so the plot does not become short.
  if (length(picked) < min(target_n, nrow(df_ord))) {
    rest <- df_ord[!(df_ord$name %in% picked), , drop = FALSE]
    picked <- c(picked, head(rest$name, min(target_n, nrow(df_ord)) - length(picked)))
  }

  out <- df[match(picked, df$name), , drop = FALSE]
  out <- out[!is.na(out$name), , drop = FALSE]
  out$selection_rank <- match(out$name, picked)
  out <- out[order(out$selection_rank), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- Adapter: build renderSSplot(82) inputs from current FLCA result ----------
make_renderSSplot82_inputs <- function(xres, target_n = 20) {
  shiny::req(xres)
  g <- xres$g_flca
  shiny::validate(shiny::need(!is.null(g) && igraph::vcount(g) > 0, "Run analysis first."))

  # V26: SSplot must use the same FLCA membership as analysis.
  # If uploaded clusters exist, this is the original nodes$carac/cluster, not components(g).
  if (!is.null(xres$flca_carac)) {
    mem_raw <- xres$flca_carac
    if (!is.null(names(mem_raw)) && all(igraph::V(g)$name %in% names(mem_raw))) {
      mem_raw <- mem_raw[igraph::V(g)$name]
    }
  } else {
    mem_raw <- igraph::components(g)$membership
  }
  mem_raw <- fill_missing_uploaded_membership(mem_raw, igraph::components(g)$membership, label = "renderSSplot membership")
  names(mem_raw) <- igraph::V(g)$name

  vals <- suppressWarnings(as.numeric(igraph::V(g)$value))
  vals[!is.finite(vals) | is.na(vals)] <- 1
  vals2 <- suppressWarnings(as.numeric(igraph::V(g)$value2))
  vals2[!is.finite(vals2) | is.na(vals2)] <- vals[!is.finite(vals2) | is.na(vals2)]
  sil <- sil_widths_vec(g, mem_raw)
  deg <- igraph::degree(g)

  all_nodes <- data.frame(
    name = igraph::V(g)$name,
    raw_cluster = as.integer(mem_raw),
    value = vals,
    value2 = vals2,
    degree = as.integer(deg),
    sil_width = sil,
    stringsAsFactors = FALSE
  )

  # V26: keep original uploaded cluster numbers. Do not re-number by size/value.
  all_nodes$carac <- as.integer(all_nodes$raw_cluster)

  # Cluster leader: highest-value node in each cluster.
  leader_tbl <- do.call(rbind, lapply(split(all_nodes, all_nodes$carac), function(d) {
    d <- d[order(-d$value, -d$sil_width, d$name), , drop = FALSE]
    data.frame(carac = d$carac[1], leader = d$name[1], leader_value = d$value[1], stringsAsFactors = FALSE)
  }))
  all_nodes <- merge(all_nodes, leader_tbl, by = "carac", all.x = TRUE, sort = FALSE)

  # Weight of the selected leader-follower relation, if visible in the reduced edge list.
  ed <- if (is.data.frame(xres$edges_reduced)) xres$edges_reduced else data.frame(Leader=character(0), follower=character(0), WCD=numeric(0))
  ed <- tryCatch(clean_edges(ed), error = function(e) data.frame(Leader=character(0), follower=character(0), WCD=numeric(0), stringsAsFactors = FALSE))
  edge_key <- function(a, b) paste(pmin(a, b), pmax(a, b), sep = "||")
  wmap <- if (nrow(ed) > 0) setNames(ed$WCD, edge_key(ed$Leader, ed$follower)) else numeric(0)
  relation_key <- edge_key(all_nodes$name, all_nodes$leader)
  wsel <- suppressWarnings(as.numeric(wmap[relation_key]))
  wsel[all_nodes$name == all_nodes$leader] <- all_nodes$leader_value[all_nodes$name == all_nodes$leader]

  sil_df <- data.frame(
    name = all_nodes$name,
    sil_width = all_nodes$sil_width,
    carac = all_nodes$carac,
    value = all_nodes$value,
    value2 = all_nodes$value2,
    wsel = wsel,
    role = ifelse(all_nodes$name == all_nodes$leader, "leader", "follower"),
    neighbor_name = all_nodes$leader,
    neighborC = all_nodes$carac,
    nov = all_nodes$degree,
    stringsAsFactors = FALSE
  )
  sil_df$wsel[!is.finite(sil_df$wsel)] <- NA_real_

  nodes0 <- data.frame(
    name = all_nodes$name,
    carac = all_nodes$carac,
    value = all_nodes$value,
    value2 = all_nodes$value2,
    stringsAsFactors = FALSE
  )

  target_n <- suppressWarnings(as.integer(target_n))
  if (!is.finite(target_n) || target_n <= 0) target_n <- 20L

  # V36: final Top-N selection is leader-first and capped at max 4 nodes per cluster.
  # This prevents one high-scoring FIFA group from dominating the final Top-20.
  # The same selected_sil_df is reused by Step 6 SSplot and Step 6c chord.
  selected_sil_df <- select_top_n_max_per_cluster(
    sil_df, target_n = target_n, max_per_cluster = 4L
  )
  nodes20 <- nodes0[match(selected_sil_df$name, nodes0$name), , drop = FALSE]

  q_w <- mod_safe(g, mem_raw)
  q_u <- mod_safe_unweighted(g, mem_raw)
  ss_overall <- mean(sil_df$sil_width, na.rm = TRUE)
  cl <- sort(unique(sil_df$carac))
  ss_per <- vapply(cl, function(cc) mean(sil_df$sil_width[sil_df$carac == cc], na.rm = TRUE), numeric(1))
  n_per <- vapply(cl, function(cc) sum(sil_df$carac == cc, na.rm = TRUE), integer(1))
  tab <- table(sil_df$carac)
  p <- as.numeric(tab) / sum(tab)
  D_gs <- 1 - sum(p^2)
  k <- length(tab)
  Dmax <- if (k >= 1) 1 - 1/k else NA_real_

  results <- data.frame(
    Cluster = c("OVERALL", paste0("C", cl)),
    SS = c(ss_overall, ss_per),
    Qw = c(q_w, rep(q_w, length(cl))),
    Qu = c(q_u, rep(q_u, length(cl))),
    D_GiniSimpson = c(D_gs, rep(NA_real_, length(cl))),
    Q_over_D = c(ifelse(is.finite(D_gs) && D_gs > 0, q_u / D_gs, NA_real_), rep(NA_real_, length(cl))),
    OneMinus_1_over_k = c(Dmax, rep(NA_real_, length(cl))),
    Q_over_Dmax_eff = c(ifelse(is.finite(Dmax) && Dmax > 0, q_u / Dmax, NA_real_), rep(NA_real_, length(cl))),
    n = c(nrow(sil_df), n_per),
    stringsAsFactors = FALSE
  )
  list(sil_df = sil_df, nodes0 = nodes0, nodes20 = nodes20, selected_sil_df = selected_sil_df, results = results)
}

render_real_SSplot82_panel <- function(xres, target_n = 20) {
  inp <- make_renderSSplot82_inputs(xres, target_n = target_n)
  render_panel(
    # V36: render exactly the capped leader-first final Top-N rows.
    # Do not allow render_panel() to globally re-select by value/SS again.
    sil_df = inp$selected_sil_df,
    nodes0 = inp$nodes20,
    results = inp$results,
    nodes = inp$nodes20,
    top_n = nrow(inp$selected_sil_df),
    font_scale = 0.86,
    aac_side = "left",
    neighbor_side = "right",
    neighbor_on_bar = TRUE,
    footer_label = sprintf("Made in FLCA/Shiny using renderSSplot(82).R on %s", Sys.Date()),
    footer_adj = 0
  )
}




# ---- Adapter: Kano plot under the SSplot -------------------------------------
# Plot value (y-axis) against value2 (x-axis) with FLCA cluster membership.
make_flca_value_value2_kano <- function(xres, label_size = 3.6, visual_ratio = 0.10, target_n = 20) {
  inp <- make_renderSSplot82_inputs(xres, target_n = target_n)
  nd <- as.data.frame(inp$selected_sil_df, stringsAsFactors = FALSE)
  shiny::validate(shiny::need(nrow(nd) >= 2, "Kano plot requires at least two selected Top-N nodes."))

  nd <- nd[, intersect(c("name", "value", "value2", "carac", "selection_rank"), names(nd)), drop = FALSE]
  if (!"value" %in% names(nd)) nd$value <- 0
  if (!"value2" %in% names(nd)) nd$value2 <- nd$value
  if (!"carac" %in% names(nd)) nd$carac <- 1L
  nd$value <- suppressWarnings(as.numeric(nd$value)); nd$value[!is.finite(nd$value)] <- 0
  nd$value2 <- suppressWarnings(as.numeric(nd$value2)); nd$value2[!is.finite(nd$value2)] <- 0
  nd$carac <- suppressWarnings(as.integer(nd$carac)); nd$carac[!is.finite(nd$carac)] <- 1L

  x_rng <- range(nd$value2, na.rm = TRUE)
  y_rng <- range(nd$value,  na.rm = TRUE)
  dx <- diff(x_rng); if (!is.finite(dx) || dx <= 0) dx <- 1
  dy <- diff(y_rng); if (!is.finite(dy) || dy <= 0) dy <- 1
  xlim_wide <- c(x_rng[1] - 2.8 * dx, x_rng[2] + 2.8 * dx)
  ylim_wide <- c(y_rng[1] - 0.20 * dy, y_rng[2] + 0.20 * dy)

  plot_kano_real(nodes = nd, title_txt = "Kano plot: same capped Top-20 nodes as SSplot/chord/Sankey", visual_ratio = visual_ratio, label_size = label_size) +
    ggplot2::labs(x = "value2", y = "value") +
    ggplot2::scale_x_continuous(limits = xlim_wide) +
    ggplot2::scale_y_continuous(limits = ylim_wide) +
    ggplot2::coord_cartesian(xlim = xlim_wide, ylim = ylim_wide, clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 160, 10, 100), legend.position = "right")
}

# ---- Shared selected Top-N graph bundle for all FLCA Process plots -----------
make_flca_process_top_bundle <- function(xres, target_n = 20) {
  inp <- make_renderSSplot82_inputs(xres, target_n = target_n)
  nd <- as.data.frame(inp$selected_sil_df, stringsAsFactors = FALSE)
  if (!nrow(nd)) stop("No selected Top-N nodes available.", call. = FALSE)
  nd$name <- as.character(nd$name)
  if (!"carac" %in% names(nd)) nd$carac <- 1L
  nd$carac <- suppressWarnings(as.integer(nd$carac)); nd$carac[!is.finite(nd$carac)] <- 1L
  if (!"value" %in% names(nd)) nd$value <- 1
  if (!"value2" %in% names(nd)) nd$value2 <- nd$value
  nd$value <- suppressWarnings(as.numeric(nd$value)); nd$value[!is.finite(nd$value)] <- 0
  nd$value2 <- suppressWarnings(as.numeric(nd$value2)); nd$value2[!is.finite(nd$value2)] <- 0
  top_names <- nd$name

  clean_edge_safely <- function(ed) {
    if (!is.data.frame(ed) || nrow(ed) == 0) return(data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE))
    tryCatch(clean_edges(ed), error = function(e) {
      nms <- names(ed); low <- tolower(nms)
      a <- nms[which(low %in% c("leader", "term1", "from", "source"))[1] %||% 1]
      b <- nms[which(low %in% c("follower", "term2", "to", "target"))[1] %||% 2]
      w <- nms[which(low %in% c("wcd", "weight", "value"))[1] %||% NA_integer_]
      data.frame(Leader = as.character(ed[[a]]), follower = as.character(ed[[b]]), WCD = if (!is.na(w)) suppressWarnings(as.numeric(ed[[w]])) else 1, stringsAsFactors = FALSE)
    })
  }
  filter_top_edges <- function(ed) {
    ed <- clean_edge_safely(ed); ed$WCD <- suppressWarnings(as.numeric(ed$WCD))
    ed <- ed[is.finite(ed$WCD) & ed$WCD > 0 & ed$Leader %in% top_names & ed$follower %in% top_names, , drop = FALSE]
    if (nrow(ed)) ed <- ed |> dplyr::group_by(Leader, follower) |> dplyr::summarise(WCD = sum(WCD, na.rm = TRUE), .groups = "drop") |> dplyr::arrange(match(Leader, top_names), match(follower, top_names), dplyr::desc(WCD)) |> as.data.frame(stringsAsFactors = FALSE)
    ed
  }
  ed_reduced <- filter_top_edges(xres$edges_reduced)
  ed_full <- filter_top_edges(xres$edges)
  # V40 consistency rule: all FLCA Process plots must be based on the same
  # selected Top-20 nodes AND the same raw node-edge table shown in the Data tab.
  # Therefore the network, SankeyMATIC, Sankey blocks, and chord use the full
  # filtered input edges first.  Reduced one-link edges are kept for reference
  # but do not drive the displayed element set.
  ed_plot <- if (nrow(ed_full)) ed_full else ed_reduced
  edge_source <- if (nrow(ed_full)) "full input edges filtered to the identical Top-20 nodes" else "FLCA reduced one-link edges fallback"
  vertices <- data.frame(name = nd$name, value = nd$value, value2 = nd$value2, carac = nd$carac, stringsAsFactors = FALSE)
  g_top <- igraph::graph_from_data_frame(d = if (nrow(ed_plot)) data.frame(from = ed_plot$Leader, to = ed_plot$follower, weight = ed_plot$WCD, stringsAsFactors = FALSE) else data.frame(from = character(0), to = character(0), weight = numeric(0)), vertices = vertices, directed = FALSE)
  igraph::V(g_top)$value <- vertices$value[match(igraph::V(g_top)$name, vertices$name)]
  igraph::V(g_top)$value2 <- vertices$value2[match(igraph::V(g_top)$name, vertices$name)]
  mem <- setNames(vertices$carac, vertices$name)
  list(input = inp, nodes = nd, edges_full = ed_full, edges_reduced = ed_reduced, edges_plot = ed_plot, edge_source = edge_source, g = g_top, membership = mem)
}

make_flca_sankey_data <- function(xres, target_n = 20) {
  # V44: Keep the original Top-20 nodes on BOTH Sankey sides, but build
  # links and SankeyMATIC code from the exact final edge table used by Step 6c chord.
  chd <- make_flca_chord_data(xres, target_n = target_n)
  nd <- chd$nodes
  ed <- chd$edges
  if (!nrow(ed)) ed <- data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE)
  ed$Leader <- as.character(ed$Leader)
  ed$follower <- as.character(ed$follower)
  ed$WCD <- suppressWarnings(as.numeric(ed$WCD)); ed$WCD[!is.finite(ed$WCD)] <- 0
  ed <- ed[ed$WCD > 0, , drop = FALSE]
  if (nrow(ed)) {
    ed <- ed[order(match(ed$Leader, nd$name), match(ed$follower, nd$name), -ed$WCD), , drop = FALSE]
  }

  # SankeyMATIC format follows sankeyplot(7).txt:
  #   Leader [value] follower #000000
  #   : node_name #HEXCOLOR
  specified_colors <- c(
    "#FF0000", "#0000FF", "#998000", "#008000", "#800080", "#FFC0CB",
    "#000000", "#ADD8E6", "#FF4500", "#A52A2A", "#8B4513", "#FF8C00",
    "#32CD32", "#4682B4", "#9400D3", "#FFD700", "#C0C0C0", "#DC143C",
    "#1E90FF", "#8B4513", "#FF8C00", "#32CD32", "#4682B4", "#9400D3",
    "#FFD700", "#C0C0C0", "#DC143C", "#1E90FF"
  )
  nd$carac <- as.character(nd$carac)
  unique_groups <- unique(nd$carac)
  group_colors <- setNames(rep(specified_colors, length.out = length(unique_groups)), unique_groups)
  nd$sankey_color <- unname(group_colors[as.character(nd$carac)])
  link_text <- if (nrow(ed)) paste0(ed$Leader, " [", format(round(ed$WCD, 3), trim = TRUE, scientific = FALSE), "] ", ed$follower, " #000000") else character(0)
  color_text <- paste0(": ", nd$name, " ", nd$sankey_color)
  code <- paste(c(link_text, color_text), collapse = "\n")

  list(nodes = nd, edges = ed, code = code, edge_source = chd$edge_source)
}

plot_flca_sankey_static <- function(sdat) {
  nd <- as.data.frame(sdat$nodes, stringsAsFactors = FALSE)
  ed <- as.data.frame(sdat$edges, stringsAsFactors = FALSE)
  if (!nrow(nd)) {
    plot.new(); text(0.5, 0.5, "No selected nodes", cex = 1.2, font = 2)
    return(invisible(NULL))
  }

  nd$name <- as.character(nd$name)
  nd$value <- suppressWarnings(as.numeric(nd$value)); nd$value[!is.finite(nd$value)] <- 0
  nd$carac <- suppressWarnings(as.integer(nd$carac)); nd$carac[!is.finite(nd$carac)] <- 1L
  nd$lab <- sprintf("%s (%s; C%s)", nd$name, format(nd$value, trim = TRUE), nd$carac)
  nd$y <- seq_len(nrow(nd))
  ymap <- setNames(nd$y, nd$name)

  if (!nrow(ed)) {
    ed <- data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE)
  }
  ed$Leader <- as.character(ed$Leader)
  ed$follower <- as.character(ed$follower)
  ed$WCD <- suppressWarnings(as.numeric(ed$WCD)); ed$WCD[!is.finite(ed$WCD)] <- 0
  ed <- ed[ed$WCD > 0 & ed$Leader %in% nd$name & ed$follower %in% nd$name, , drop = FALSE]
  if (nrow(ed)) {
    ed <- ed[order(match(ed$Leader, nd$name), match(ed$follower, nd$name), -ed$WCD), , drop = FALSE]
    ed$y1 <- ymap[ed$Leader]
    ed$y2 <- ymap[ed$follower]
  }

  clusters <- sort(unique(nd$carac))
  pal <- grDevices::hcl.colors(max(3, length(clusters)), "Dark 3")
  names(pal) <- as.character(clusters)
  node_col <- unname(pal[as.character(nd$carac)])
  col_map <- setNames(node_col, nd$name)

  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar), add = TRUE)
  par(mar = c(4.5, 12, 4, 12), xpd = NA, family = "sans")
  plot(NA, xlim = c(0, 1), ylim = c(nrow(nd) + 1.2, 0), axes = FALSE, xlab = "", ylab = "",
       main = "Sankey diagram: original Top-20 nodes on both sides")

  # Left and right node blocks: all selected nodes are shown on both sides so
  # the Sankey has exactly the same node elements as SSplot, Kano, network, and chord.
  rect_left <- c(0.10, 0.16); rect_right <- c(0.84, 0.90)
  block_h <- 0.36
  for (i in seq_len(nrow(nd))) {
    graphics::rect(rect_left[1], nd$y[i] - block_h, rect_left[2], nd$y[i] + block_h,
                   col = node_col[i], border = "grey30")
    graphics::rect(rect_right[1], nd$y[i] - block_h, rect_right[2], nd$y[i] + block_h,
                   col = node_col[i], border = "grey30")
    graphics::text(rect_left[1] - 0.015, nd$y[i], nd$lab[i], adj = c(1, 0.5), cex = 0.78, font = 2)
    graphics::text(rect_right[2] + 0.015, nd$y[i], nd$lab[i], adj = c(0, 0.5), cex = 0.78, font = 2)
  }
  graphics::text(mean(rect_left), 0.20, "term1 / Leader", cex = 1.05, font = 2)
  graphics::text(mean(rect_right), 0.20, "term2 / follower", cex = 1.05, font = 2)

  # Draw ribbon-like flows as filled polygons.  Width is proportional to WCD,
  # and color follows the leader cluster.
  if (nrow(ed)) {
    max_w <- max(ed$WCD, na.rm = TRUE); if (!is.finite(max_w) || max_w <= 0) max_w <- 1
    for (i in seq_len(nrow(ed))) {
      x <- seq(rect_left[2] + 0.02, rect_right[1] - 0.02, length.out = 80)
      t <- seq(0, 1, length.out = 80)
      y_mid <- (1 - t) * ed$y1[i] + t * ed$y2[i]
      # smooth cubic easing for a Sankey-like curve
      ease <- 3 * t^2 - 2 * t^3
      y_mid <- (1 - ease) * ed$y1[i] + ease * ed$y2[i]
      half_w <- 0.035 + 0.16 * (ed$WCD[i] / max_w)
      graphics::polygon(c(x, rev(x)), c(y_mid - half_w, rev(y_mid + half_w)),
                        col = grDevices::adjustcolor(col_map[ed$Leader[i]] %||% "steelblue", alpha.f = 0.42),
                        border = NA)
      graphics::lines(x, y_mid, col = grDevices::adjustcolor("grey25", alpha.f = 0.35), lwd = 0.7)
      graphics::text(0.50, mean(c(ed$y1[i], ed$y2[i])), labels = format(round(ed$WCD[i], 2), trim = TRUE),
                     cex = 0.62, col = "grey20")
    }
  } else {
    graphics::text(0.50, 0.75, "No links among the selected Top-20 nodes; blocks are still shown for consistency.", cex = 0.95, font = 2)
  }

  graphics::mtext("All visible node blocks are the identical selected Top-20 set on both sides; links are the exact final edge table used by chord and SankeyMATIC.", side = 1, line = 1.2, cex = 0.85)
  graphics::mtext(paste0("Edge source: ", sdat$edge_source), side = 1, line = 2.2, cex = 0.78)
  invisible(NULL)
}

# ---- Chord diagram for FLCA Process -----------------------------------------
# V40: The chord diagram is no longer built from an edge-only matrix with tiny
# invisible nodes.  It is built from the same selected Top-20 node table and the
# same filtered Data-tab edge list used by the Sankey and network plots.  Sector
# widths are based on node value/incident weight, so high-value leaders such as
# Germany, Mexico, and the United States remain visible even when some followers
# have few links inside the Top-20 set.
make_flca_chord_data <- function(xres, target_n = 20) {
  shiny::req(xres)
  bun <- make_flca_process_top_bundle(xres, target_n = target_n)
  nd <- as.data.frame(bun$nodes[, c("name", "carac", "value", "value2"), drop = FALSE], stringsAsFactors = FALSE)
  if (!nrow(nd)) stop("No final Top-N nodes are available for the chord diagram.", call. = FALSE)
  nd$name <- as.character(nd$name)
  nd$carac <- suppressWarnings(as.integer(nd$carac)); nd$carac[!is.finite(nd$carac)] <- 1L
  nd$value <- suppressWarnings(as.numeric(nd$value)); nd$value[!is.finite(nd$value)] <- 0
  nd$value2 <- suppressWarnings(as.numeric(nd$value2)); nd$value2[!is.finite(nd$value2)] <- 0
  top_names <- nd$name

  ed_top <- as.data.frame(bun$edges_plot, stringsAsFactors = FALSE)
  if (!nrow(ed_top)) ed_top <- data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE)
  ed_top$Leader <- as.character(ed_top$Leader)
  ed_top$follower <- as.character(ed_top$follower)
  ed_top$WCD <- suppressWarnings(as.numeric(ed_top$WCD)); ed_top$WCD[!is.finite(ed_top$WCD)] <- 0
  ed_top <- ed_top[ed_top$WCD > 0 & ed_top$Leader %in% top_names & ed_top$follower %in% top_names, , drop = FALSE]
  if (nrow(ed_top)) {
    ed_top <- aggregate(WCD ~ Leader + follower, data = ed_top, FUN = function(x) sum(x, na.rm = TRUE))
    ed_top <- ed_top[order(match(ed_top$Leader, top_names), match(ed_top$follower, top_names), -ed_top$WCD), , drop = FALSE]
  }

  # matrix kept for export/debug; custom static plot below is the display source.
  mat_real <- matrix(0, nrow = length(top_names), ncol = length(top_names), dimnames = list(top_names, top_names))
  if (nrow(ed_top)) for (i in seq_len(nrow(ed_top))) mat_real[ed_top$Leader[i], ed_top$follower[i]] <- mat_real[ed_top$Leader[i], ed_top$follower[i]] + ed_top$WCD[i]

  incident <- rowSums(mat_real, na.rm = TRUE) + colSums(mat_real, na.rm = TRUE)
  nd$incident_wcd <- as.numeric(incident[nd$name]); nd$incident_wcd[!is.finite(nd$incident_wcd)] <- 0
  nd$sector_size <- pmax(nd$value, nd$incident_wcd, 1)

  clusters <- sort(unique(nd$carac))
  pal <- grDevices::hcl.colors(max(3, length(clusters)), "Dark 3")
  names(pal) <- as.character(clusters)
  group_colors <- unname(pal[as.character(nd$carac)])
  list(
    nodes = nd,
    edges = ed_top,
    matrix_real = mat_real,
    group_colors = group_colors,
    edge_source = bun$edge_source,
    note = sprintf("Chord uses exactly the same %d selected Top-20 nodes as SSplot, Kano, network, and Sankey; max 4 nodes per cluster; edge source: %s. Sector size is based on node value and incident WCD, not edge-only visibility.", nrow(nd), bun$edge_source)
  )
}

plot_flca_chord_static <- function(chd) {
  nd <- as.data.frame(chd$nodes, stringsAsFactors = FALSE)
  ed <- as.data.frame(chd$edges, stringsAsFactors = FALSE)
  if (!requireNamespace("circlize", quietly = TRUE)) {
    graphics::plot.new()
    graphics::text(0.5, 0.60, "Install circlize for the chord diagram:", cex = 1.15, font = 2)
    graphics::text(0.5, 0.50, "install.packages('circlize')", cex = 1.05)
    graphics::text(0.5, 0.38, chd$note, cex = 0.85)
    return(invisible(NULL))
  }
  if (!nrow(nd)) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "No selected nodes", cex = 1.2, font = 2)
    return(invisible(NULL))
  }
  nd$name <- as.character(nd$name)
  nd$sector_size <- suppressWarnings(as.numeric(nd$sector_size)); nd$sector_size[!is.finite(nd$sector_size) | nd$sector_size <= 0] <- 1
  ed$Leader <- as.character(ed$Leader); ed$follower <- as.character(ed$follower)
  ed$WCD <- suppressWarnings(as.numeric(ed$WCD)); ed$WCD[!is.finite(ed$WCD)] <- 0
  ed <- ed[ed$WCD > 0 & ed$Leader %in% nd$name & ed$follower %in% nd$name, , drop = FALSE]

  circlize::circos.clear()
  on.exit(circlize::circos.clear(), add = TRUE)
  nsec <- nrow(nd)
  gap_after <- rep(ifelse(nsec <= 20, 3.5, 2), nsec)
  if (nsec > 1) gap_after[nsec] <- 10
  circlize::circos.par(start.degree = 90, gap.after = gap_after, track.margin = c(0.01, 0.01), points.overflow.warning = FALSE)
  xlim <- cbind(rep(0, nsec), nd$sector_size)
  rownames(xlim) <- nd$name
  circlize::circos.initialize(factors = nd$name, xlim = xlim)
  grid_col <- setNames(chd$group_colors, nd$name)

  short_label <- function(z) {
    z <- as.character(z)
    ifelse(nchar(z) > 18, paste0(substr(z, 1, 16), "…"), z)
  }

  circlize::circos.trackPlotRegion(
    ylim = c(0, 1), track.height = 0.13, bg.border = NA,
    panel.fun = function(x, y) {
      nm <- circlize::get.cell.meta.data("sector.index")
      xl <- circlize::get.cell.meta.data("xlim")
      circlize::circos.rect(xl[1], 0, xl[2], 1, col = grid_col[nm], border = "white")
      circlize::circos.text(mean(xl), 1.35, short_label(nm), facing = "clockwise", niceFacing = TRUE,
                            adj = c(0, 0.5), cex = ifelse(nsec <= 20, 0.58, 0.45), font = 2)
    }
  )

  if (nrow(ed)) {
    out_pos <- setNames(rep(0, nrow(nd)), nd$name)
    in_pos <- setNames(rep(0, nrow(nd)), nd$name)
    # allocate source/target intervals using WCD, clipped to each sector size
    for (i in seq_len(nrow(ed))) {
      a <- ed$Leader[i]; b <- ed$follower[i]; w <- max(0.05, ed$WCD[i])
      sa <- nd$sector_size[match(a, nd$name)]; sb <- nd$sector_size[match(b, nd$name)]
      xa1 <- out_pos[a]; xa2 <- min(sa, xa1 + w); out_pos[a] <- xa2
      xb1 <- in_pos[b]; xb2 <- min(sb, xb1 + w); in_pos[b] <- xb2
      if (xa2 <= xa1) { xa1 <- max(0, sa - 0.05); xa2 <- sa }
      if (xb2 <= xb1) { xb1 <- max(0, sb - 0.05); xb2 <- sb }
      circlize::circos.link(a, c(xa1, xa2), b, c(xb1, xb2),
                            col = grDevices::adjustcolor(grid_col[a], alpha.f = 0.38), border = NA)
    }
  }
  graphics::title("Chord diagram: same Top-20 nodes as SSplot/Sankey/network", cex.main = 1.05)
  invisible(NULL)
}

ui <- fluidPage(
  titlePanel("FLCA / Community Comparison Demo — V51 taller Golden Boot slopegraph"),
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js"),
    tags$script(HTML("
      function fifaDownloadElementPNG(id, filename) {
        var outer = document.getElementById(id);
        if (!outer) { alert('Nothing to download yet. Please draw the plot first.'); return; }
        if (typeof html2canvas === 'undefined') { alert('html2canvas not loaded. Please check internet connection or reload the app.'); return; }

        // Capture the real content box, not the horizontal-scroll wrapper.
        var el = document.getElementById(id + '_inner') || outer;
        var oldOuterOverflow = outer.style.overflow;
        var oldOuterWidth = outer.style.width;
        var oldElDisplay = el.style.display;
        var oldElTransform = el.style.transform;

        outer.style.overflow = 'visible';
        outer.style.width = 'max-content';
        el.style.display = 'inline-block';
        el.style.transform = 'none';
        el.scrollIntoView(false);

        setTimeout(function() {
          var rect = el.getBoundingClientRect();
          var captureWidth = Math.ceil(Math.max(el.scrollWidth, el.offsetWidth, rect.width));
          var captureHeight = Math.ceil(Math.max(el.scrollHeight, el.offsetHeight, rect.height));

          html2canvas(el, {
            backgroundColor: '#ffffff',
            scale: 5,
            useCORS: true,
            logging: false,
            scrollX: -window.scrollX,
            scrollY: -window.scrollY,
            windowWidth: Math.max(document.documentElement.clientWidth, captureWidth + 40),
            windowHeight: Math.max(document.documentElement.clientHeight, captureHeight + 40),
            width: captureWidth,
            height: captureHeight,
            x: 0,
            y: 0,
            onclone: function(doc) {
              doc.body.style.overflow = 'visible';
              var clonedOuter = doc.getElementById(id);
              var clonedEl = doc.getElementById(id + '_inner') || clonedOuter;
              if (clonedOuter) {
                clonedOuter.style.overflow = 'visible';
                clonedOuter.style.width = 'max-content';
              }
              if (clonedEl) {
                clonedEl.style.display = 'inline-block';
                clonedEl.style.transform = 'none';
                clonedEl.style.position = 'static';
              }
            }
          }).then(function(canvas) {
            outer.style.overflow = oldOuterOverflow;
            outer.style.width = oldOuterWidth;
            el.style.display = oldElDisplay;
            el.style.transform = oldElTransform;

            var link = document.createElement('a');
            link.download = filename || 'fifa2026_plot.png';
            link.href = canvas.toDataURL('image/png');
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
          }).catch(function(err) {
            outer.style.overflow = oldOuterOverflow;
            outer.style.width = oldOuterWidth;
            el.style.display = oldElDisplay;
            el.style.transform = oldElTransform;
            alert('PNG capture failed: ' + err);
          });
        }, 120);
      }
      if (window.Shiny) {
        Shiny.addCustomMessageHandler('fifaDownloadElementPNG', function(msg) {
          fifaDownloadElementPNG(msg.id, msg.filename);
        });
      }
    ")),
    tags$style(HTML("
    /* V20: robust red coloring for only four requested tab labels.
       We use an inline span class in the tab title instead of nth-child CSS,
       because Shiny/Bootstrap wraps tabsets differently across versions. */
    .tab-red { color: #c00000 !important; font-weight: 800 !important; }
    .nav-tabs > li.active > a .tab-red,
    .nav-tabs > li.active > a:focus .tab-red,
    .nav-tabs > li.active > a:hover .tab-red {
      color: #c00000 !important;
      font-weight: 900 !important;
    }
  "))),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("Input mode"),
      radioButtons("input_mode", NULL,
        choices = c(
          "Use demo text" = "demo",
          "Use demo CSV: dataset1.csv" = "demo_csv",
          "Use demo FIFA 2026 from bundled XLSX" = "demo_fifa2026_xlsx",
          "Update FIFA 2026 online from Wikipedia and run" = "fifa2026_online",
          "Use demo NBA 2025-2026 from nba.xlsx" = "demo_nba2025_2026",
          "Upload xlsx files with nodes and edges spreadsheet" = "upload",
          "Upload CSV files with coword data" = "upload_coword"
        ),
        selected = "demo_csv"),
      actionButton("select_demo_csv", "Select demo CSV: dataset1.csv", class = "btn-info"),
      br(), br(),
      actionButton("run_fifa2026_xlsx", "Run FIFA 2026 demo from bundled XLSX", class = "btn-success"),
      br(), br(),
      actionButton("update_fifa2026_online", "Update FIFA 2026 online and run", class = "btn-warning"),
      br(), br(),
      actionButton("run_nba2025_2026", "Run NBA 2025-2026 demo from nba.xlsx", class = "btn-success"),
      br(), br(),
      actionButton("run", "Run analysis", class = "btn-primary"),
      p("Default input is demo CSV. V43 restores the red-line FIFA Golden Boot slopegraph and keeps original Top-20 nodes on both term1/Leader and term2/follower sides of the Sankey plot."),
      hr(),
      h4("Download demo data"),
      downloadButton("download_demo_dataset1", "Download demo CSV: dataset1.csv"),
      br(), br(),
      downloadButton("download_demo_fifa2026_xlsx", "Download FIFA 2026 updated nodes/edges XLSX"),
      br(), br(),
      downloadButton("download_demo_nba2025_xlsx", "Download NBA 2025-2026 nba.xlsx"),
      conditionalPanel("input.input_mode == 'demo'",
        h5("Optional Nodes CSV — displayed only; demo run uses fixed data frames"),
        textAreaInput("nodes_input", NULL, value = nodes_demo_text_value, rows = 10, width = "100%"),
        h5("Edges CSV — displayed only; demo run uses fixed data frames"),
        textAreaInput("edges_input", NULL, value = edges_demo_text_value, rows = 10, width = "100%")
      ),
      conditionalPanel("input.input_mode == 'demo_fifa2026_xlsx'",
        h5("FIFA 2026 bundled XLSX demo"),
        helpText("Reads fifa_2026_updated_nodes_edges.xlsx from the app folder. The workbook should contain sheets named nodes and edges."),
        downloadButton("download_fifa2026_xlsx", "Download current FIFA 2026 bundled XLSX")
      ),
      conditionalPanel("input.input_mode == 'fifa2026_online'",
        h5("REAL online FIFA 2026 update"),
        helpText("Click Run analysis or Update FIFA 2026 online and run. A Shiny progress scale appears while the 12 Wikipedia Group A-L pages are fetched, parsed, converted to nodes/edges, saved to XLSX, and analyzed."),
        helpText("This mode reads current online Wikipedia pages and does not use the bundled XLSX fallback."),
        fifa2026_source_links_ui(),
        downloadButton("download_fifa2026_xlsx", "Download last updated FIFA online XLSX"),
        br(), br(),
        numericInput("fifa_prediction_seed", "Prediction seed", value = 20260621, min = 1, step = 1),
        helpText("Changing the seed produces another reproducible prediction path while keeping Table 1 points and the performance prior fixed."),
        br(),
        h5("FIFA group performance summary at reporting date"),
        helpText("After online update, each Group A-L row uses a clickable Group A-L hyperlink label instead of showing the raw URL. Top 1 appears at the left and the next three teams to the right; points are shown in parentheses."),
        uiOutput("fifa_group_summary_date_sidebar"),
        DTOutput("fifa_group_summary_sidebar"),
        hr(),
        h5("The chase for the Golden Boot of FIFA 2026"),
        helpText("The table and slopegraph are built from scorer lists parsed from Wikipedia footballbox fields (.fhgoal and .fagoal) when those scorer fields are available."),
        uiOutput("fifa_golden_boot_date_sidebar"),
        DTOutput("fifa_golden_boot_sidebar"),
        uiOutput("fifa_golden_boot_plot_note_sidebar")
      ),
      conditionalPanel("input.input_mode == 'demo_nba2025_2026'",
        h5("NBA 2025-2026 bundled XLSX demo"),
        helpText("Reads nba.xlsx from the app folder. The workbook should contain sheets named nodes and edges."),
        downloadButton("download_nba_xlsx", "Download nba.xlsx")
      ),
      conditionalPanel("input.input_mode == 'upload'",
        radioButtons("upload_data_type", "Uploaded spreadsheet type", choices = c("Auto" = "auto", "Nodes/edges" = "nodes_edges"), selected = "nodes_edges"),
        fileInput("workbook_file", "Excel workbook with nodes and edges sheets (.xlsx/.xls)", accept = c(".xlsx", ".xls")),
        fileInput("nodes_file", "Optional nodes spreadsheet (.xlsx/.xls)", accept = c(".xlsx", ".xls")),
        fileInput("edges_file", "Edges spreadsheet (.xlsx/.xls)", accept = c(".xlsx", ".xls"))
      ),
      conditionalPanel("input.input_mode == 'upload_coword'",
        fileInput("coword_file", "Coword CSV file: long doc,term or wide document-term matrix", accept = c(".csv", "text/csv", ".txt")),
        helpText("Long format example: doc,term. Wide format: one row per document and numeric term columns. The top-N setting below is applied before edge generation.")
      ),
      numericInput("target_k", "Target k for independent components", value = 4, min = 1, step = 1),
      radioButtons("flca_mode", "FLCA mode for network outputs", choices = c("Maturity (value)" = "value", "Influence (value2)" = "value2"), selected = "value"),
      h4("Reviewer-required parameter settings"),
      numericInput("occurrence_top_n", "Occurrence input: top N nodes before edge generation", value = 100, min = 2, step = 1),
      numericInput("flca_min_cluster_size", "FLCA minimum cluster size", value = 3, min = 1, step = 1),
      numericInput("flca_target_nodes", "FLCA-MA final target nodes", value = 20, min = 1, step = 1),
      numericInput("sil_intra_penalty", "Silhouette missing intra-cluster penalty", value = 2, min = 0, step = 1),
      numericInput("sil_inter_penalty", "Silhouette missing inter-cluster penalty", value = 5, min = 0, step = 1),
      numericInput("tie_break_scale", "Tie-break scale: leader score / scale", value = 10000, min = 1, step = 100),
      p("These controls expose FLCA parameters requested by reviewers and are exported in the Parameters tab."),
      sliderInput("label_size", "Network label font size", min = 10, max = 50, value = 30, step = 1),
      checkboxInput("label_bold", "Bold network labels", value = TRUE),
      br(), tags$label("Run status"), verbatimTextOutput("run_status")
    ),
    mainPanel(width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("Data", br(), h4("Data mode"), verbatimTextOutput("data_mode"),
          h4("FIFA 2026 group performance summary"),
          p("For the online FIFA update, this table summarizes each group at the reporting date: Top 1 on the left, then the next three teams to the right, with points in parentheses."),
          uiOutput("fifa_group_summary_date_main"),
          DTOutput("fifa_group_summary_main"),
          hr(),
          h4("FIFA 2026 prediction path by stage"),
          p("Generated when clicking Update FIFA 2026 online and run. Prediction = 0.7 × Table 1 current points + 0.3 × mixed performance prior; the seed controls reproducible randomization."),
          uiOutput("fifa_prediction_seed_note"),
          actionButton("run_fifa_prediction_plot", "Run FIFA 2026 prediction plot", class = "btn-primary"),
          actionButton("download_fifa_prediction_path_png_client", "Download prediction path PNG", icon = icon("download")),
          actionButton("download_fifa_prediction_ssq_png_client", "Download SS/Q PNG", icon = icon("download")),
          br(), br(),
          DTOutput("fifa_prediction_metrics_table"),
          DTOutput("fifa_prediction_path_table"),
          tags$div(style="overflow-x:auto; width:100%;", tags$div(id = "fifa_prediction_path_capture", style="display:inline-block; width:max-content; max-width:none; overflow:visible;", uiOutput("fifa_prediction_path_html"))),
          tags$div(id = "fifa_prediction_ssq_capture", uiOutput("fifa_prediction_ssq_html")),
          hr(),
          h4("FIFA 2026 Golden Boot chase"),
          p("For the online FIFA update, scorer lists are parsed from Wikipedia footballbox scorer fields when available. The table reports goal totals; the slopegraph tracks cumulative goals by parsed match order."),
          uiOutput("fifa_golden_boot_date_main"),
          DTOutput("fifa_golden_boot_main"),
          actionButton("run_fifa_golden_boot_plot", "Draw Golden Boot slopegraph", class = "btn-warning"),
          actionButton("download_fifa_golden_boot_png_client", "Download Golden Boot PNG", icon = icon("download")),
          br(), br(),
          tags$div(style="overflow-x:auto; width:100%;", tags$div(id = "fifa_golden_boot_capture", uiOutput("fifa_golden_boot_slope_html"))),
          uiOutput("fifa_golden_boot_plot_note_main"),
          hr(),
          DTOutput("tbl_nodes"), br(), DTOutput("tbl_edges")),
        tabPanel("Full Network", br(), fluidRow(column(5, selectInput("full_method", "Clustering algorithm", choices = community_methods, selected = "louvain")), column(4, sliderInput("full_top_n", "Show top N nodes", min = 5, max = 100, value = 30, step = 1)), column(3, checkboxInput("full_use_all", "Show all nodes", value = FALSE))), visNetworkOutput("plot_full", height = "720px")),
        tabPanel("FLCA Reduced", br(), fluidRow(column(4, checkboxInput("flca_major_sampling", "Use FLCA-MA major sampling", value = TRUE)), column(4, sliderInput("flca_top_clusters", "Top clusters", min = 1, max = 20, value = 6, step = 1)), column(4, sliderInput("flca_n_per_cluster", "At least N nodes per top cluster", min = 1, max = 20, value = 3, step = 1))), visNetworkOutput("plot_flca", height = "720px"), br(), h4("Reduced edges"), DTOutput("tbl_reduced")),
        tabPanel(title = tags$span(class = "tab-red", "FLCA Process"), br(),
          h3("FLCA 6-step visual process"),
          h4("Step 1. Input nodes and weighted edges"),
          fluidRow(column(6, DTOutput("flca_step1_nodes")), column(6, DTOutput("flca_step1_edges"))),
          h4("Step 2. Full weighted graph"),
          visNetworkOutput("flca_step2_full_graph", height = "560px"),
          downloadButton("download_flca_step2_png", "Download Step 2 network PNG"),
          br(), br(),
          h4("Step 3. FLCA reduced one-link graph"),
          visNetworkOutput("flca_step3_reduced_graph", height = "560px"),
          downloadButton("download_flca_step3_png", "Download Step 3 reduced network PNG"),
          br(), br(),
          h4("Step 4. Leader/follower cluster summary"),
          DTOutput("flca_step4_cluster_summary"),
          h4("Step 5b. Sankey plot and SankeyMATIC code: same final Top-20 nodes"),
          p("The Sankey plot keeps the original Top-20 node set on both term1/Leader and term2/follower sides; links and SankeyMATIC code use the exact final edge table."),
          plotOutput("flca_step5_sankey_plot", height = "760px"),
          downloadButton("download_flca_step5_sankey_png", "Download Step 5b Sankey PNG"),
          h5("SankeyMATIC code"),
          verbatimTextOutput("flca_step5_sankeymatic_code"),
          h4("Step 6. Real SSplot cluster summary using renderSSplot(82).R"),
          plotOutput("flca_step6_ssplot", height = "920px"),
          downloadButton("download_flca_step6_ssplot_png", "Download Step 6 SSplot PNG"),
          br(), br(),
          h4("Step 6b. Wide Kano plot under SSplot: value on y-axis and value2 on x-axis"),
          plotOutput("flca_step6_kano", height = "920px", width = "100%"),
          downloadButton("download_flca_step6_kano_png", "Download Step 6b Kano PNG"),
          br(),
          h4("Step 6c. Interactive chord dashboard: final Top-20 FLCA nodes and edges"),
          p("The chord diagram uses the same final Top-20 nodes and the same filtered Data-tab edges as the SSplot, Kano, network, and Sankey plots. Sector size is based on node value and incident WCD so high-value leaders remain visible."),
          verbatimTextOutput("flca_step6c_chord_note"),
          uiOutput("flca_step6c_chord_ui"),
          h4("Step 6c chord nodes: same final Top-N rows as SSplot"),
          DTOutput("flca_step6c_chord_nodes"),
          h4("Step 6c chord edges"),
          DTOutput("flca_step6c_chord_edges"),
          br(),
          downloadButton("download_flca_step6c_chord_png", "Download Step 6c chord PNG"),
          downloadButton("download_flca_step6c_chord_edges", "Download Step 6c chord edges CSV"),
          downloadButton("download_flca_process_all_png_zip", "Download all FLCA Process PNGs (.zip)")
        ),
        tabPanel("Memberships", br(), DTOutput("tbl_memberships"), br(), downloadButton("download_memberships", "Download memberships CSV")),
        tabPanel("Quality", br(), h4("FLCA maturity vs influence annotation"), verbatimTextOutput("flca_mode_annotation"), DTOutput("tbl_quality"), br(), downloadButton("download_quality", "Download quality CSV")),
        tabPanel("Ranking", br(), DTOutput("tbl_ranking"), br(), downloadButton("download_ranking", "Download ranking CSV")),
        tabPanel(title = tags$span(class = "tab-red", "Visual Quality"), br(),
          h4("Kano plot for algorithm quality: Q on x-axis and SS on y-axis"),
          p("This tab uses algorithm-level rows from the Quality table. It is intentionally different from the node-level Kano under the SSplot, which uses node value/value2."),
          p("If several points share the same SS, it means those algorithms produced identical or nearly identical memberships/mean silhouette values. The table below shows the exact Q and SS values for every Kano point."),
          plotOutput("plot_visual_quality_kano", height = "900px"),
          h4("All Kano points: algorithm, Q, and SS"),
          verbatimTextOutput("visual_quality_kano_note"),
          DTOutput("tbl_visual_quality_kano"),
          br(), downloadButton("download_visual_quality_kano", "Download visual-quality Kano CSV")),
        tabPanel(title = tags$span(class = "tab-red", "Parameters"), br(), h3("Reproducible parameter settings"), downloadButton("download_parameters", "Download parameter settings CSV"), br(), br(), DTOutput("tbl_parameters"), h4("How to report these settings in Methods"), verbatimTextOutput("parameter_methods_text")),
        tabPanel(title = tags$span(class = "tab-red", "ReadMe"), br(), h3("ReadMe: FLCA and community clustering comparison app"), p("V34 preserves the original-style sidebar and colors only four main tabs red: FLCA Process, Visual Quality, Parameters, and ReadMe. Demo CSV reads dataset1.csv; FIFA 2026 bundled demo reads fifa_2026_updated_nodes_edges.xlsx; FIFA online update really fetches Wikipedia Group A–L pages at run time, rebuilds that workbook, parses Golden Boot scorer fields when available, and runs analysis; NBA 2025-2026 demo reads nba.xlsx. Excel upload reads nodes/edges workbooks; CSV coword upload converts document-term data to nodes and weighted edges. FLCA Process Step 6 shows the real SSplot and Step 6b shows a wider node-level Kano plot with value on the y-axis and value2 on the x-axis. The Visual Quality tab separately shows algorithm quality with modularity Q on x and mean silhouette SS on y.")),
        tabPanel("Deploy Notes", br(), h4("shinyapps.io deployment"), pre("install.packages(c('shiny','DT','igraph','visNetwork','RColorBrewer','ggplot2','ggrepel','dplyr','tibble','readxl','openxlsx','httr2','rvest','xml2','stringr','tidyr','purrr'), type='binary')\nrsconnect::deployApp(appDir='F:/taaforgae/zWoSPubmed/flcacompare', appName='flcacompare', forceUpdate=TRUE)"))
      )
    )
  )
)


# ---- FIFA 2026 prediction path: Table 1 + mixed prior + seed -----------------
make_fifa_prediction_metrics <- function() {
  tibble::tribble(
    ~stage, ~matches, ~SS, ~Q,
    "Stage 48", 48, 0.6200, 0.9600,
    "Group stage", 72, 0.3846, 0.8988,
    "Round of 32", 88, 0.3441, 0.7413,
    "Round of 16", 96, 0.3346, 0.6857,
    "Quarterfinals", 100, 0.3323, 0.6682,
    "Semifinals", 102, 0.3255, 0.6409,
    "Final", 103, 0.3255, 0.6365,
    "Third-place match", 104, 0.3255, 0.6330
  )
}

make_fifa_prediction_prior <- function() {
  tibble::tribble(
    ~team, ~elo, ~fifa_rank_score, ~wc_history,
    "Brazil",100,98,100, "Argentina",99,100,98, "France",98,99,95,
    "Germany",94,90,96, "England",93,94,88, "Spain",92,95,90,
    "Portugal",91,93,84, "Netherlands",90,92,86, "Belgium",88,91,78,
    "Uruguay",87,86,88, "Croatia",86,85,84, "Switzerland",82,84,70,
    "Mexico",81,82,72, "United States",80,81,65, "Morocco",79,83,75,
    "Japan",78,80,66, "Colombia",77,79,68, "Senegal",76,78,65,
    "South Korea",75,77,66, "Austria",74,76,58, "Sweden",73,74,68,
    "Australia",72,73,55, "Norway",71,75,50, "Canada",70,72,48,
    "Ghana",69,70,60, "Ivory Coast",68,69,55, "Czech Republic",67,68,58,
    "Paraguay",66,67,62, "Scotland",65,66,52, "Turkey",64,65,56,
    "Ecuador",63,64,54, "Saudi Arabia",62,63,50, "Egypt",61,62,52,
    "Iran",60,61,50, "Tunisia",59,60,48, "South Africa",58,59,46,
    "Qatar",57,58,42, "Algeria",56,57,46, "Panama",55,56,40,
    "Bosnia and Herzegovina",54,55,42, "New Zealand",53,54,38,
    "Haiti",52,53,36, "Iraq",51,52,35, "Jordan",50,51,34,
    "Uzbekistan",49,50,34, "DR Congo",48,49,36,
    "Cape Verde",47,48,32, "Curacao",46,47,30, "Curaçao",46,47,30
  ) |>
    dplyr::mutate(
      Elo_norm = as.numeric(scale(elo)),
      FIFA_norm = as.numeric(scale(fifa_rank_score)),
      History_norm = as.numeric(scale(wc_history)),
      mixed_prior = 0.5 * Elo_norm + 0.3 * FIFA_norm + 0.2 * History_norm
    )
}

make_fifa_prediction_from_nodes <- function(nodes, seed_value = 20260621) {
  set.seed(as.integer(seed_value))
  nd <- safe_df(nodes)
  nms0 <- tolower(gsub("[^[:alnum:]]", "", names(nd)))
  name_col <- names(nd)[which(nms0 %in% c("name", "team", "node", "term", "label", "id"))[1] %||% 1]
  pts_col <- names(nd)[which(nms0 %in% c("value", "points", "score", "pts"))[1] %||% NA_integer_]
  cl_col <- names(nd)[which(nms0 %in% c("cluster", "carac", "group", "membership"))[1] %||% NA_integer_]
  nd$team <- trimws(as_utf8(nd[[name_col]]))
  nd$points <- if (is.na(pts_col)) 0 else suppressWarnings(as.numeric(nd[[pts_col]]))
  nd$points[!is.finite(nd$points)] <- 0
  if (is.na(cl_col)) {
    nd$group <- NA_character_
  } else {
    raw_cl <- as.character(nd[[cl_col]])
    raw_num <- suppressWarnings(as.integer(gsub("[^0-9-]", "", raw_cl)))
    letter_cl <- match(toupper(gsub(".*([A-L]).*", "\\1", raw_cl)), LETTERS[1:12])
    cl_num <- ifelse(is.na(raw_num), letter_cl, raw_num)
    nd$group <- LETTERS[pmax(1, pmin(12, as.integer(cl_num)))]
  }
  df <- nd |>
    dplyr::filter(nzchar(team), !is.na(group)) |>
    dplyr::select(group, team, points) |>
    dplyr::distinct(group, team, .keep_all = TRUE) |>
    dplyr::left_join(make_fifa_prediction_prior(), by = "team") |>
    dplyr::mutate(
      mixed_prior = ifelse(is.na(mixed_prior), 0, mixed_prior),
      points_norm = if (stats::sd(points, na.rm = TRUE) > 0) as.numeric(scale(points)) else 0,
      prediction_score = 0.7 * points_norm + 0.3 * mixed_prior + stats::rnorm(dplyr::n(), 0, 0.10)
    )
  if (nrow(df) < 32) stop("FIFA prediction needs at least 32 valid teams in nodes.", call. = FALSE)

  ranked <- df |>
    dplyr::group_by(group) |>
    dplyr::arrange(dplyr::desc(points), dplyr::desc(prediction_score), .by_group = TRUE) |>
    dplyr::mutate(group_rank = dplyr::row_number()) |>
    dplyr::ungroup()
  qualified32 <- dplyr::bind_rows(
    ranked |> dplyr::filter(group_rank <= 2),
    ranked |> dplyr::filter(group_rank == 3) |>
      dplyr::arrange(dplyr::desc(points), dplyr::desc(prediction_score)) |>
      dplyr::slice(1:8)
  ) |>
    dplyr::arrange(dplyr::desc(prediction_score))

  predict_one <- function(a, b) {
    sa <- df$prediction_score[match(a, df$team)]
    sb <- df$prediction_score[match(b, df$team)]
    p_a <- exp(sa) / (exp(sa) + exp(sb))
    winner <- ifelse(stats::runif(1) < p_a, a, b)
    loser <- ifelse(winner == a, b, a)
    tibble::tibble(team1 = a, team2 = b, winner = winner, loser = loser)
  }
  make_pairs <- function(x) {
    x <- as.character(x)
    high <- x[1:(length(x) / 2)]
    low <- rev(x[(length(x) / 2 + 1):length(x)])
    cbind(high, low)
  }
  sim_round <- function(team_vec, round_name) {
    pairs <- make_pairs(team_vec)
    dplyr::bind_rows(lapply(seq_len(nrow(pairs)), function(i) predict_one(pairs[i, 1], pairs[i, 2]))) |>
      dplyr::mutate(round = round_name, game = dplyr::row_number())
  }
  r32 <- sim_round(qualified32$team, "Round of 32")
  r16 <- sim_round(r32$winner, "Round of 16")
  qf <- sim_round(r16$winner, "Quarterfinals")
  sf <- sim_round(qf$winner, "Semifinals")
  final <- sim_round(sf$winner, "Final")
  third <- sim_round(sf$loser, "Third-place match")
  path <- dplyr::bind_rows(r32, r16, qf, sf, final, third) |>
    dplyr::select(round, game, team1, team2, winner, loser)
  list(
    seed = as.integer(seed_value),
    path = path,
    qualified32 = qualified32,
    scores = df,
    metrics = make_fifa_prediction_metrics(),
    champion = final$winner[1], runner_up = final$loser[1],
    third = third$winner[1], fourth = third$loser[1]
  )
}


# Windows-safe base-R renderer for FIFA prediction path.
# It avoids ggplot/grid text grobs, which can trigger:
# "不是所有的 is.character(txt) 都是 TRUE" on some Windows Shiny devices.
draw_fifa_prediction_path_base <- function(fp, label_cex = 0.72) {
  round_levels <- c("Round of 32", "Round of 16", "Quarterfinals", "Semifinals", "Final", "Third-place match")
  path <- as.data.frame(fp$path, stringsAsFactors = FALSE)
  if (nrow(path) == 0) {
    plot.new(); text(0.5, 0.5, "No prediction path available")
    return(invisible(NULL))
  }
  path$round  <- enc2utf8(as.character(path$round))
  path$team1  <- enc2utf8(as.character(path$team1))
  path$team2  <- enc2utf8(as.character(path$team2))
  path$winner <- enc2utf8(as.character(path$winner))
  path$game   <- suppressWarnings(as.numeric(path$game))
  path$stage_pos <- match(path$round, round_levels)
  path$y_pos <- ifelse(path$round == "Round of 32", path$game * 2,
                ifelse(path$round == "Round of 16", path$game * 4 - 1,
                ifelse(path$round == "Quarterfinals", path$game * 8 - 3,
                ifelse(path$round == "Semifinals", path$game * 16 - 7,
                ifelse(path$round == "Final", 16,
                ifelse(path$round == "Third-place match", 26, path$game * 2))))))
  path$label <- enc2utf8(as.character(paste0(path$team1, " vs ", path$team2, "\nW: ", path$winner)))

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mar = c(5, 4, 5, 4), xpd = NA, family = "sans")

  plot(NA, NA,
       xlim = c(0.8, 7.7), ylim = c(35, 0),
       xaxt = "n", yaxt = "n", xlab = "Stage", ylab = "",
       main = enc2utf8(as.character(paste0("Predicted FIFA 2026 knockout path, seed = ", fp$seed))))
  axis(1, at = seq_along(round_levels), labels = round_levels, cex.axis = 0.78, font.axis = 2)
  grid(nx = NA, ny = NULL, col = "grey88", lty = 1)
  abline(v = seq_along(round_levels), col = "grey92", lty = 1)

  subtitle <- enc2utf8(as.character(paste0(
    "Based on the current Table 1 group points at each run + mixed performance prior.  ",
    "Champion: ", fp$champion,
    " | Runner-up: ", fp$runner_up,
    " | Third: ", fp$third,
    " | Fourth: ", fp$fourth
  )))
  mtext(subtitle, side = 3, line = 0.5, cex = 0.72, col = "grey25")

  # Draw simple white label boxes. All labels are forced to character.
  for (i in seq_len(nrow(path))) {
    x <- path$stage_pos[i]
    y <- path$y_pos[i]
    lab <- as.character(path$label[i])
    # approximate box dimensions in user coordinates
    w <- 1.00
    h <- 1.38
    rect(x - 0.05, y - h/2, x + w, y + h/2, col = "white", border = "grey65")
    points(x, y, pch = 19, cex = 0.65)
    text(x + 0.03, y, labels = lab, adj = c(0, 0.5), cex = label_cex)
  }

  invisible(NULL)
}

# Windows-safe base-R renderer for SS/Q decline.
draw_fifa_prediction_ssq_base <- function(fp) {
  mt <- as.data.frame(fp$metrics, stringsAsFactors = FALSE)
  mt$matches <- suppressWarnings(as.numeric(mt$matches))
  mt$SS <- suppressWarnings(as.numeric(mt$SS))
  mt$Q <- suppressWarnings(as.numeric(mt$Q))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mar = c(5, 4, 4, 2), family = "sans")
  yr <- range(c(mt$SS, mt$Q), finite = TRUE)
  plot(mt$matches, mt$SS, type = "b", pch = 19, lwd = 2,
       ylim = yr, xlab = "Accumulated matches", ylab = "Value",
       main = "Predicted SS and Q decline by FIFA 2026 stage")
  lines(mt$matches, mt$Q, type = "b", pch = 17, lwd = 2, lty = 2)
  grid(col = "grey88")
  legend("topright", legend = c("SS", "Q"), pch = c(19, 17), lty = c(1, 2), lwd = 2, bty = "n")
  text(mt$matches, mt$SS, labels = sprintf("%.4f", mt$SS), pos = 3, cex = 0.72)
  text(mt$matches, mt$Q, labels = sprintf("%.4f", mt$Q), pos = 1, cex = 0.72)
  invisible(NULL)
}

# Backward-compatible names used by older code. These draw directly and return NULL.
plot_fifa_prediction_path_clear <- function(fp, label_size = 2.05) {
  draw_fifa_prediction_path_base(fp, label_cex = 0.72)
}
plot_fifa_prediction_ssq <- function(fp) {
  draw_fifa_prediction_ssq_base(fp)
}


# ---- HTML-only FIFA output helpers ------------------------------------------
# These avoid ggplot/grid/device text rendering, preventing Windows/Shiny
# errors such as: "不是所有的 is.character(txt) 都是 TRUE".
save_tag_as_html <- function(tag_obj, file, title = "FIFA 2026 output") {
  htmltools::save_html(
    htmltools::tagList(
      tags$head(
        tags$title(title),
        tags$meta(charset = "utf-8"),
        tags$style("body{font-family:Arial,Helvetica,sans-serif;margin:18px;} .table{border-collapse:collapse;} .table th,.table td{border:1px solid #ddd;padding:6px;} .card{box-shadow:0 1px 3px rgba(0,0,0,.08);}")
      ),
      tag_obj
    ),
    file = file
  )
}

make_fifa_prediction_path_html <- function(fp) {
  path <- as.data.frame(fp$path, stringsAsFactors = FALSE)
  if (nrow(path) == 0) {
    return(tags$div(class = "alert alert-info", "No FIFA prediction path available."))
  }
  for (nm in names(path)) path[[nm]] <- enc2utf8(as.character(path[[nm]]))
  path$game_num <- suppressWarnings(as.numeric(path$game))

  # V47 layout:
  # - Left: Round of 32.
  # - Right: Round of 16 + Quarterfinals at the top.
  # - The highlighted final pathway is moved upward to fill the blank space,
  #   with Third-place match below Final.
  # - This makes the exported PNG more compact and closer to square.

  build_round_cards <- function(rn, card_fs = 23, win_fs = 21, pad_px = 14, min_h = 88,
                                card_border = "2px solid #9a9a9a", team_color = "#111",
                                winner_color = "#b00020", card_bg = "white") {
    dd <- path[path$round == rn, , drop = FALSE]
    if (nrow(dd)) dd <- dd[order(dd$game_num), , drop = FALSE]
    lapply(seq_len(nrow(dd)), function(i) {
      tags$div(
        class = "card",
        style = paste0(
          "margin:10px 0;padding:", pad_px, "px;background:", card_bg, ";",
          "border:", card_border, ";border-radius:9px;",
          "font-size:", card_fs, "px;line-height:1.22;",
          "min-height:", min_h, "px;white-space:normal;",
          "box-shadow:0 2px 5px rgba(0,0,0,.10);"
        ),
        tags$div(style = paste0("font-weight:700;color:", team_color, ";"), paste0(dd$team1[i], " vs ", dd$team2[i])),
        tags$div(style = paste0("font-weight:900;color:", winner_color, ";font-size:", win_fs, "px;margin-top:6px;"),
                 paste0("W: ", dd$winner[i]))
      )
    })
  }

  one_round_col <- function(rn, width_px = 450, highlight = FALSE, header_bg = NULL,
                            header_fs = NULL, card_fs = NULL, win_fs = NULL,
                            team_color = NULL, winner_color = NULL, card_border = NULL,
                            card_bg = "white", min_h = NULL) {
    header_bg <- header_bg %||% if (highlight) "#8B0000" else "#08306B"
    header_fs <- header_fs %||% if (highlight) 31 else 26
    card_fs   <- card_fs   %||% if (highlight) 26 else 23
    win_fs    <- win_fs    %||% if (highlight) 25 else 21
    team_color   <- team_color   %||% "#111"
    winner_color <- winner_color %||% "#b00020"
    card_border  <- card_border  %||% if (highlight) "3px solid #8B0000" else "2px solid #9a9a9a"
    pad_px <- if (highlight) 16 else 14
    min_h  <- min_h %||% if (highlight) 102 else 88

    tags$div(
      style = paste0("width:", width_px, "px; flex:0 0 ", width_px, "px;"),
      tags$div(
        style = paste0(
          "font-weight:900;text-align:center;padding:12px 10px;",
          "background:", header_bg, ";color:white;border-radius:8px;",
          "font-size:", header_fs, "px;line-height:1.15;letter-spacing:.2px;"
        ), rn
      ),
      build_round_cards(rn, card_fs = card_fs, win_fs = win_fs, pad_px = pad_px,
                        min_h = min_h, card_border = card_border, team_color = team_color,
                        winner_color = winner_color, card_bg = card_bg)
    )
  }

  final_stack_col <- function(width_px = 450) {
    tags$div(
      style = paste0("width:", width_px, "px; flex:0 0 ", width_px, "px;"),
      one_round_col(
        "Final", width_px = width_px, highlight = TRUE,
        header_bg = "#0B3D91", header_fs = 31, card_fs = 28, win_fs = 28,
        team_color = "#0B3D91", winner_color = "#C00000",
        card_border = "4px solid #0B3D91", card_bg = "#F7FBFF", min_h = 108
      ),
      tags$div(style = "height:10px;"),
      one_round_col(
        "Third-place match", width_px = width_px, highlight = TRUE,
        header_bg = "#8B0000", header_fs = 27, card_fs = 24, win_fs = 24,
        team_color = "#222", winner_color = "#C00000",
        card_border = "3px solid #B22222", card_bg = "#FFF9F9", min_h = 92
      )
    )
  }

  tags$div(
    id = "fifa_prediction_path_capture_inner",
    style = paste0(
      "padding:20px;border:2px solid #cfcfcf;background:#fafafa;",
      "width:1470px;max-width:none;box-sizing:border-box;display:inline-block;overflow:visible;",
      "font-family:Arial,Helvetica,sans-serif;color:#111;"
    ),
    tags$div(
      style = "font-size:36px;font-weight:900;margin-bottom:8px;line-height:1.12;",
      paste0("Predicted FIFA 2026 knockout path, seed = ", fp$seed)
    ),
    tags$div(
      style = "font-size:24px;font-weight:800;color:#333;margin-bottom:14px;line-height:1.20;",
      paste0("Champion: ", fp$champion,
             "  | Runner-up: ", fp$runner_up,
             "  | Third: ", fp$third,
             "  | Fourth: ", fp$fourth)
    ),
    tags$div(
      style = "display:flex;gap:22px;align-items:flex-start;",
      one_round_col("Round of 32", width_px = 430, highlight = FALSE, header_fs = 25, card_fs = 21, win_fs = 20, min_h = 82),
      tags$div(
        style = "width:998px; flex:0 0 998px;",
        tags$div(
          style = "display:flex;gap:22px;align-items:flex-start;",
          one_round_col("Round of 16", width_px = 488, highlight = FALSE, header_fs = 26, card_fs = 23, win_fs = 21, min_h = 88),
          one_round_col("Quarterfinals", width_px = 488, highlight = FALSE, header_fs = 26, card_fs = 23, win_fs = 21, min_h = 88)
        ),
        tags$div(
          style = "margin-top:14px;",
          tags$div(
            style = paste0(
              "padding:16px 16px 18px 16px;border:4px solid #8B0000;",
              "border-radius:14px;background:#fff7f7;"
            ),
            tags$div(
              style = "font-size:30px;font-weight:900;color:#8B0000;margin-bottom:12px;line-height:1.1;",
              "Highlighted final pathway"
            ),
            tags$div(
              style = "display:flex;gap:22px;align-items:flex-start;",
              one_round_col("Semifinals", width_px = 488, highlight = TRUE,
                            header_bg = "#8B0000", header_fs = 30, card_fs = 26, win_fs = 25,
                            card_border = "3px solid #B22222", card_bg = "#FFFDFD", min_h = 102),
              final_stack_col(width_px = 488)
            )
          )
        )
      )
    )
  )
}

make_fifa_prediction_ssq_html <- function(fp) {
  mt <- as.data.frame(fp$metrics, stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(mt)), function(i) {
    tags$tr(
      tags$td(as.character(mt$stage[i])),
      tags$td(as.character(mt$matches[i])),
      tags$td(sprintf("%.4f", as.numeric(mt$SS[i]))),
      tags$td(sprintf("%.4f", as.numeric(mt$Q[i])))
    )
  })
  tags$div(
    style = "margin-top:14px;",
    tags$h4("SS and Q by stage"),
    tags$table(class = "table table-striped table-condensed",
      tags$thead(tags$tr(tags$th("Stage"), tags$th("Matches"), tags$th("Average SS"), tags$th("Overall Q"))),
      tags$tbody(rows)
    )
  )
}

make_fifa_golden_boot_slope_html <- function(slope_df, top_n = 12) {
  if (is.null(slope_df) || !is.data.frame(slope_df) || nrow(slope_df) == 0 ||
      !all(c("player_label", "year", "value") %in% names(slope_df))) {
    return(tags$div(class = "alert alert-info", "No Golden Boot scorer-level slopegraph data available."))
  }
  df0 <- safe_df(slope_df)
  df0$player_label <- enc2utf8(as.character(df0$player_label))
  df0$year <- enc2utf8(as.character(df0$year))
  df0$value <- suppressWarnings(as.numeric(df0$value))
  df0$value[!is.finite(df0$value) | is.na(df0$value)] <- 0
  df0 <- df0[nzchar(df0$player_label), , drop = FALSE]
  if (!nrow(df0)) return(tags$div(class = "alert alert-info", "No valid Golden Boot scorer rows available."))

  last_vals <- stats::aggregate(value ~ player_label, df0, max, na.rm = TRUE)
  last_vals <- last_vals[order(-last_vals$value, last_vals$player_label), , drop = FALSE]
  keep <- head(last_vals$player_label, min(top_n, nrow(last_vals)))
  df0 <- df0[df0$player_label %in% keep, , drop = FALSE]
  df <- tufte_sort_cc(df0, x = "year", y = "value", group = "player_label", min.space = 0.44)
  if (!nrow(df)) return(tags$div(class = "alert alert-info", "No Golden Boot slopegraph data available."))

  df$group <- enc2utf8(as.character(df$group))
  df$x_chr <- enc2utf8(as.character(df$x))
  x_levels <- order_match_labels_numeric(df$x_chr)
  df$x_num <- match(df$x_chr, x_levels)
  df$y <- suppressWarnings(as.numeric(df$y))
  df$ypos <- suppressWarnings(as.numeric(df$ypos))
  df <- df[is.finite(df$x_num) & is.finite(df$ypos), , drop = FALSE]
  if (!nrow(df)) return(tags$div(class = "alert alert-info", "No valid Golden Boot slopegraph coordinates."))

  # V51: extra-large bold fonts with taller vertical layout for clearer labels.
  ml <- 520; mr <- 360; mt <- 120; mb <- 240
  W <- max(2600, ml + mr + max(1, length(unique(df$x_chr))) * 390)
  H <- max(1650, 420 + length(unique(df$group)) * 128)
  y_min <- min(df$ypos, na.rm = TRUE); y_max <- max(df$ypos, na.rm = TRUE)
  if (!is.finite(y_min) || !is.finite(y_max) || y_min == y_max) { y_min <- 0; y_max <- 1 }
  x_to_px <- function(x) ml + (as.numeric(x) - 1) / max(1, length(x_levels) - 1) * (W - ml - mr)
  y_to_px <- function(y) mt + (y_max - as.numeric(y)) / max(1e-9, y_max - y_min) * (H - mt - mb)

  svg_children <- list(
    tags$text(x = 28, y = 48, style = "font-size:38px;font-weight:900;fill:#111;", "The chase for the Golden Boot of FIFA 2026"),
    tags$text(x = 28, y = 84, style = "font-size:22px;font-weight:900;fill:#555;", "Cumulative goals by parsed match order; generated after clicking Draw Golden Boot slopegraph.")
  )
  for (i in seq_along(x_levels)) {
    xp <- x_to_px(i)
    svg_children <- c(svg_children, list(
      tags$line(x1 = xp, y1 = mt, x2 = xp, y2 = H - mb, style = "stroke:#dddddd;stroke-width:1.6;"),
      tags$text(x = xp, y = H - 52, transform = paste0("rotate(-35 ", xp, " ", H - 52, ")"),
                style = "font-size:19px;font-weight:900;fill:#333;text-anchor:end;", as.character(x_levels[i]))
    ))
  }
  for (g in unique(df$group)) {
    dd <- df[df$group == g, , drop = FALSE]
    dd <- dd[order(dd$x_num), , drop = FALSE]
    pts <- paste(sprintf("%.1f,%.1f", x_to_px(dd$x_num), y_to_px(dd$ypos)), collapse = " ")
    svg_children <- c(svg_children, list(tags$polyline(points = pts, style = "fill:none;stroke:#cc0000;stroke-width:3.2;")))
    for (j in seq_len(nrow(dd))) {
      xp <- x_to_px(dd$x_num[j]); yp <- y_to_px(dd$ypos[j])
      svg_children <- c(svg_children, list(
        tags$circle(cx = xp, cy = yp, r = 9, style = "fill:#fff;stroke:#cc0000;stroke-width:2.6;"),
        tags$text(x = xp, y = yp + 5.5, style = "font-size:16px;font-weight:900;text-anchor:middle;fill:#111;", as.character(round(dd$y[j], 0)))
      ))
    }
  }
  first_rows <- df[df$x_num == 1, c("group", "ypos"), drop = FALSE]
  first_rows <- first_rows[!duplicated(first_rows$group), , drop = FALSE]
  first_rows <- first_rows[order(first_rows$ypos), , drop = FALSE]
  for (i in seq_len(nrow(first_rows))) {
    svg_children <- c(svg_children, list(tags$text(
      x = ml - 22, y = y_to_px(first_rows$ypos[i]) + 6,
      style = "font-size:22px;font-weight:900;text-anchor:end;fill:#111;", as.character(first_rows$group[i])
    )))
  }
  tags$div(
    style = paste0("border:1px solid #ddd; background:#fff; padding:12px; margin-top:8px; min-width:", W, "px; width:", W, "px; box-sizing:border-box;"),
    tags$svg(width = W, height = H, viewBox = paste("0 0", W, H), xmlns = "http://www.w3.org/2000/svg", svg_children)
  )
}

server <- function(input, output, session) {
  rv <- reactiveValues(result = NULL, status = "V32 TRUE REBUILD loaded. Click Run analysis, Run FIFA 2026 bundled XLSX, REAL Update FIFA online, or Run NBA demo.", error = NULL)

  get_input_data <- function(mode, progress = NULL) {
    if (identical(mode, "demo")) {
      return(list(nodes = nodes_demo_df(), edges = edges_demo_df(), data_mode = "V20 demo text: fixed nodes + edges data frames"))
    }
    if (identical(mode, "demo_csv")) {
      csv_path <- file.path(getwd(), "dataset1.csv")
      if (!file.exists(csv_path)) {
        stop("dataset1.csv was not found in the app folder: ", getwd(), ". Put dataset1.csv beside app.R, or use Upload CSV files with coword data.", call. = FALSE)
      }
      out <- csv_to_nodes_edges(csv_path, top_n_nodes = input$occurrence_top_n %||% 100)
      out$data_mode <- paste0("V18 demo CSV from dataset1.csv; ", out$data_mode)
      return(out)
    }
    if (identical(mode, "demo_fifa2026_xlsx")) {
      fifa_path <- file.path(getwd(), "fifa_2026_updated_nodes_edges.xlsx")
      if (!file.exists(fifa_path)) {
        # Create a bundled workbook fallback once, then read it like the production demo.
        write_fifa2026_xlsx(fifa_path, fifa2026_nodes_demo_df(), fifa2026_edges_demo_df())
      }
      wb <- read_excel_workbook(fifa_path)
      return(list(nodes = wb$nodes, edges = wb$edges,
                  group_performance_summary = if (!is.null(wb$group_performance_summary)) wb$group_performance_summary else make_fifa_group_performance_summary(wb$nodes, reporting_date = file.info(fifa_path)$mtime),
                  golden_boot_summary = wb$golden_boot_summary,
                  golden_boot_slope = wb$golden_boot_slope,
                  data_mode = paste0("FIFA 2026 demo from bundled XLSX: ", basename(fifa_path))))
    }
    if (identical(mode, "fifa2026_online")) {
      return(fetch_fifa2026_online_nodes_edges(file.path(getwd(), "fifa_2026_updated_nodes_edges.xlsx"), progress = progress))
    }
    if (identical(mode, "demo_nba2025_2026")) {
      nba_path <- file.path(getwd(), "nba.xlsx")
      if (!file.exists(nba_path)) {
        # Create a bundled demo workbook fallback once, then read it like the production demo.
        write_nba2025_xlsx(nba_path, nba2025_nodes_demo_df(), nba2025_edges_demo_df())
      }
      wb <- read_excel_workbook(nba_path)
      return(list(nodes = wb$nodes, edges = wb$edges, data_mode = paste0("NBA 2025-2026 demo from bundled XLSX: ", basename(nba_path))))
    }
    if (identical(mode, "upload")) {
      if (!is.null(input$workbook_file)) {
        wb <- read_excel_workbook(input$workbook_file$datapath)
        return(list(nodes = wb$nodes, edges = wb$edges, data_mode = "uploaded Excel workbook with edges/nodes sheets"))
      }
      if (!is.null(input$edges_file)) {
        edges <- read_excel_first(input$edges_file$datapath)
        nodes <- if (!is.null(input$nodes_file)) read_excel_first(input$nodes_file$datapath) else NULL
        return(list(nodes = nodes, edges = edges, data_mode = "uploaded separate Excel nodes/edges files"))
      }
      stop("Upload an Excel workbook containing an edges sheet, or upload an edges spreadsheet.", call. = FALSE)
    }
    if (identical(mode, "upload_coword")) {
      if (is.null(input$coword_file)) stop("Upload a coword CSV file first.", call. = FALSE)
      out <- csv_to_nodes_edges(input$coword_file$datapath, top_n_nodes = input$occurrence_top_n %||% 100)
      out$data_mode <- paste0("uploaded coword CSV; ", out$data_mode)
      return(out)
    }
    stop("Unknown input mode.", call. = FALSE)
  }

  run_analysis <- function(mode = NULL) {
    mode <- mode %||% (input$input_mode %||% "demo_csv")
    rv$status <- paste0("V32 RUN RECEIVED at ", format(Sys.time(), "%H:%M:%S"), "\nMode: ", mode, "\nStarting analysis...")

    tryCatch({
      progress_fun <- NULL
      if (identical(mode, "fifa2026_online")) {
        dat <- shiny::withProgress(
          message = "Updating FIFA 2026 online from Wikipedia",
          detail = "Preparing live update...",
          value = 0,
          {
            progress_fun <- function(value = NULL, detail = NULL) {
              shiny::setProgress(value = value, detail = detail)
            }
            dat0 <- get_input_data(mode, progress = progress_fun)
            shiny::setProgress(value = 0.985, detail = "Running FLCA/MA/SIL network analysis")
            dat0
          }
        )
      } else {
        dat <- get_input_data(mode)
      }

      out <- analyze_basic(dat$nodes, dat$edges, target_k = input$target_k %||% 4, flca_mode = input$flca_mode %||% "value")
      if (!is.null(dat$group_performance_summary)) {
        out$group_performance_summary <- dat$group_performance_summary
      } else if (identical(mode, "demo_fifa2026_xlsx")) {
        out$group_performance_summary <- make_fifa_group_performance_summary(dat$nodes, reporting_date = Sys.time())
      }
      if (!is.null(dat$golden_boot_summary)) {
        out$golden_boot_summary <- dat$golden_boot_summary
      }
      if (!is.null(dat$golden_boot_slope)) {
        out$golden_boot_slope <- dat$golden_boot_slope
      }
      if (!is.null(dat$scorers_source)) {
        out$scorers_source <- dat$scorers_source
      }
      if (identical(mode, "fifa2026_online") || identical(mode, "demo_fifa2026_xlsx")) {
        out$fifa_prediction <- make_fifa_prediction_from_nodes(
          out$nodes,
          seed_value = input$fifa_prediction_seed %||% 20260621
        )
      }
      out$data_mode <- dat$data_mode
      out$input_nodes_n <- nrow(out$nodes)
      out$input_edges_n <- nrow(out$edges)
      rv$result <- out
      extra_online <- if (!is.null(dat$online_details)) paste0("\n", dat$online_details) else ""
      rv$status <- paste0("V32 ANALYSIS COMPLETED at ", format(Sys.time(), "%H:%M:%S"),
                          "\nInput mode: ", out$data_mode,
                          "\nNodes: ", out$input_nodes_n,
                          "\nEdges: ", out$input_edges_n,
                          extra_online,
                          "\nCore: ", out$status_note)
    }, error = function(e) {
      rv$error <- conditionMessage(e)
      rv$status <- paste0("V32 ANALYSIS STOPPED SAFELY at ", format(Sys.time(), "%H:%M:%S"), "\nMode: ", mode, "\nError: ", conditionMessage(e))
    })
  }

  observeEvent(input$run, { run_analysis() }, ignoreInit = TRUE)
  observeEvent(input$select_demo_csv, { updateRadioButtons(session, "input_mode", selected = "demo_csv"); run_analysis("demo_csv") }, ignoreInit = TRUE)
  observeEvent(input$run_fifa2026_xlsx, { updateRadioButtons(session, "input_mode", selected = "demo_fifa2026_xlsx"); run_analysis("demo_fifa2026_xlsx") }, ignoreInit = TRUE)
  observeEvent(input$update_fifa2026_online, { updateRadioButtons(session, "input_mode", selected = "fifa2026_online"); run_analysis("fifa2026_online") }, ignoreInit = TRUE)
  observeEvent(input$run_nba2025_2026, { updateRadioButtons(session, "input_mode", selected = "demo_nba2025_2026"); run_analysis("demo_nba2025_2026") }, ignoreInit = TRUE)

  res <- reactive(rv$result)
  output$run_status <- renderPrint({ cat(rv$status) })
  output$data_mode <- renderPrint({ if (is.null(res())) cat(rv$status) else cat(paste0("Input mode: ", res()$data_mode, "\nNodes: ", res()$input_nodes_n, "\nEdges: ", res()$input_edges_n)) })
  output$tbl_nodes <- renderDT({ req(res()); datatable(res()$nodes, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$tbl_edges <- renderDT({ req(res()); datatable(res()$edges, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })

  fifa_group_summary_reactive <- reactive({
    req(res())
    gs <- res()$group_performance_summary
    if (is.null(gs) || !is.data.frame(gs) || nrow(gs) == 0) {
      return(data.frame(message = "No FIFA group summary is available. Run the bundled FIFA demo or click Update FIFA 2026 online and run.", stringsAsFactors = FALSE))
    }
    gs
  })
  fifa_group_summary_date_text <- reactive({
    gs <- fifa_group_summary_reactive()
    if ("reporting_date" %in% names(gs) && nrow(gs) > 0) {
      paste0("Reporting date: ", unique(gs$reporting_date)[1], " | Groups shown: ", nrow(gs), " | Teams reported: ", sum(suppressWarnings(as.integer(gs$teams_reported)), na.rm = TRUE))
    } else {
      "Reporting date: not available"
    }
  })
  output$fifa_group_summary_date_sidebar <- renderUI({
    tags$small(tags$b(fifa_group_summary_date_text()))
  })
  output$fifa_group_summary_date_main <- renderUI({
    tags$p(tags$b(fifa_group_summary_date_text()))
  })
  output$fifa_group_summary_sidebar <- renderDT({
    gs <- fifa_group_summary_reactive()
    if ("reporting_date" %in% names(gs)) gs <- gs[, setdiff(names(gs), "reporting_date"), drop = FALSE]
    datatable(gs, rownames = FALSE, escape = FALSE, options = list(pageLength = 12, scrollX = TRUE, dom = "t", ordering = FALSE), class = "compact stripe")
  })
  output$fifa_group_summary_main <- renderDT({
    gs <- fifa_group_summary_reactive()
    datatable(gs, rownames = FALSE, escape = FALSE, options = list(pageLength = 12, scrollX = TRUE, ordering = FALSE), class = "stripe hover")
  })

  fifa_golden_boot_reactive <- reactive({
    req(res())
    gb <- res()$golden_boot_summary
    if (is.null(gb) || !is.data.frame(gb) || nrow(gb) == 0) {
      return(empty_golden_boot_summary())
    }
    gb
  })
  fifa_golden_boot_slope_reactive <- reactive({
    req(res())
    sl <- res()$golden_boot_slope
    if (is.null(sl) || !is.data.frame(sl) || nrow(sl) == 0) {
      return(empty_golden_boot_slope())
    }
    sl
  })
  fifa_golden_boot_date_text <- reactive({
    gb <- fifa_golden_boot_reactive()
    if ("reporting_date" %in% names(gb) && nrow(gb) > 0) {
      paste0("Reporting date: ", unique(gb$reporting_date)[1], " | Golden Boot rows shown: ", nrow(gb))
    } else {
      "Reporting date: not available"
    }
  })
  output$fifa_golden_boot_date_sidebar <- renderUI({
    tags$small(tags$b(fifa_golden_boot_date_text()))
  })
  output$fifa_golden_boot_date_main <- renderUI({
    tags$p(tags$b(fifa_golden_boot_date_text()))
  })
  output$fifa_golden_boot_plot_note_sidebar <- renderUI({
    tags$small("Click Draw Golden Boot slopegraph after the FIFA online update. The slopegraph is rendered as HTML/SVG for Windows safety.")
  })
  output$fifa_golden_boot_plot_note_main <- renderUI({
    tags$p("Click Draw Golden Boot slopegraph after the FIFA online update. The slopegraph is rendered as HTML/SVG for Windows safety.")
  })
  output$fifa_golden_boot_sidebar <- renderDT({
    gb <- fifa_golden_boot_reactive()
    if ("reporting_date" %in% names(gb)) gb <- gb[, setdiff(names(gb), "reporting_date"), drop = FALSE]
    datatable(gb, rownames = FALSE, escape = TRUE, options = list(pageLength = 10, scrollX = TRUE, dom = "t", ordering = FALSE), class = "compact stripe")
  })
  output$fifa_golden_boot_main <- renderDT({
    gb <- fifa_golden_boot_reactive()
    datatable(gb, rownames = FALSE, escape = TRUE, options = list(pageLength = 15, scrollX = TRUE, ordering = TRUE), class = "stripe hover")
  })
  golden_boot_plot_state <- reactiveVal(NULL)

  observeEvent(input$run_fifa_golden_boot_plot, {
    if (is.null(res())) {
      showNotification("Run Update FIFA 2026 online and run first.", type = "warning")
      return(NULL)
    }
    sl <- fifa_golden_boot_slope_reactive()
    golden_boot_plot_state(sl)
    showNotification("Golden Boot slopegraph generated from the current scorer table.", type = "message")
  }, ignoreInit = TRUE)

  golden_boot_plot_reactive <- reactive({
    sl <- golden_boot_plot_state()
    validate(need(!is.null(sl), "Click Draw Golden Boot slopegraph after the current FIFA table has been generated."))
    sl
  })

  output$fifa_golden_boot_slope_html <- renderUI({
    sl <- golden_boot_plot_state()
    if (is.null(sl)) {
      return(tags$div(class = "alert alert-info", "Click Draw Golden Boot slopegraph after the current FIFA table has been generated."))
    }
    make_fifa_golden_boot_slope_html(sl, top_n = 12)
  })

  observeEvent(input$download_fifa_golden_boot_png_client, {
    sl <- golden_boot_plot_state()
    if (is.null(sl)) {
      showNotification("Draw Golden Boot slopegraph first.", type = "warning")
      return(NULL)
    }
    session$sendCustomMessage("fifaDownloadElementPNG", list(
      id = "fifa_golden_boot_capture",
      filename = paste0("fifa2026_golden_boot_slopegraph_", Sys.Date(), ".png")
    ))
  }, ignoreInit = TRUE)

  output$download_fifa_golden_boot_html <- downloadHandler(
    filename = function() paste0("fifa2026_golden_boot_slopegraph_", Sys.Date(), ".html"),
    contentType = "text/html; charset=utf-8",
    content = function(file) {
      sl <- golden_boot_plot_reactive()
      save_tag_as_html(make_fifa_golden_boot_slope_html(sl, top_n = 12), file,
                       title = "FIFA 2026 Golden Boot slopegraph")
    }
  )

  # Prediction is generated ONLY when this separate button is clicked.
  # This avoids automatic plotting during Data tab loading and keeps the result
  # based on the current Table 1 nodes from the latest online/bundled run.
  fifa_prediction_state <- reactiveVal(NULL)

  observeEvent(input$run_fifa_prediction_plot, {
    if (is.null(res())) {
      showNotification("Run Update FIFA 2026 online and run first.", type = "warning")
      return(NULL)
    }
    nd <- res()$nodes
    if (!is.data.frame(nd) || nrow(nd) < 48) {
      showNotification("Prediction needs the current Table 1 with 48 teams. Please update FIFA 2026 online first.", type = "error")
      return(NULL)
    }
    seed_now <- suppressWarnings(as.integer(input$fifa_prediction_seed %||% 20260621))
    if (!is.finite(seed_now) || is.na(seed_now)) seed_now <- 20260621L
    fp <- tryCatch(
      make_fifa_prediction_from_nodes(nd, seed_value = seed_now),
      error = function(e) {
        showNotification(paste("Prediction error:", conditionMessage(e)), type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(fp)) return(NULL)
    fifa_prediction_state(fp)
    showNotification("Prediction path generated from current Table 1.", type = "message")
  }, ignoreInit = TRUE)

  fifa_prediction_reactive <- reactive({
    fp <- fifa_prediction_state()
    validate(need(!is.null(fp), "Click Run FIFA 2026 prediction plot after the current Table 1 has been generated."))
    fp
  })

  output$fifa_prediction_seed_note <- renderUI({
    fp <- fifa_prediction_state()
    if (is.null(fp)) {
      return(tags$p(tags$b("Prediction not yet generated."),
                    " Click Run FIFA 2026 prediction plot after Table 1 appears."))
    }
    tags$p(
      tags$b(paste0("Seed: ", fp$seed)),
      paste0(" | Champion: ", fp$champion,
             " | Runner-up: ", fp$runner_up,
             " | Third: ", fp$third,
             " | Fourth: ", fp$fourth,
             " | Prediction = 0.7 × Table 1 points + 0.3 × mixed prior")
    )
  })

  output$fifa_prediction_metrics_table <- renderDT({
    fp <- fifa_prediction_state()
    if (is.null(fp)) {
      return(datatable(data.frame(message = "Click Run FIFA 2026 prediction plot."), rownames = FALSE, options = list(dom = "t")))
    }
    datatable(fp$metrics, rownames = FALSE,
              options = list(pageLength = 8, scrollX = TRUE, dom = "t"),
              class = "stripe hover")
  })

  output$fifa_prediction_path_table <- renderDT({
    fp <- fifa_prediction_state()
    if (is.null(fp)) {
      return(datatable(data.frame(message = "Prediction path not yet generated."), rownames = FALSE, options = list(dom = "t")))
    }
    datatable(fp$path, rownames = FALSE, escape = TRUE,
              options = list(pageLength = 31, scrollX = TRUE),
              class = "stripe hover")
  })

  output$fifa_prediction_path_html <- renderUI({
    fp <- fifa_prediction_state()
    if (is.null(fp)) {
      return(tags$div(class = "alert alert-info", "Click Run FIFA 2026 prediction plot after the current Table 1 has been generated."))
    }
    make_fifa_prediction_path_html(fp)
  })

  output$fifa_prediction_ssq_html <- renderUI({
    fp <- fifa_prediction_state()
    if (is.null(fp)) return(NULL)
    make_fifa_prediction_ssq_html(fp)
  })

  observeEvent(input$download_fifa_prediction_path_png_client, {
    fp <- fifa_prediction_state()
    if (is.null(fp)) {
      showNotification("Run FIFA 2026 prediction plot first.", type = "warning")
      return(NULL)
    }
    session$sendCustomMessage("fifaDownloadElementPNG", list(
      id = "fifa_prediction_path_capture",
      filename = paste0("fifa2026_prediction_path_seed_", fp$seed, ".png")
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$download_fifa_prediction_ssq_png_client, {
    fp <- fifa_prediction_state()
    if (is.null(fp)) {
      showNotification("Run FIFA 2026 prediction plot first.", type = "warning")
      return(NULL)
    }
    session$sendCustomMessage("fifaDownloadElementPNG", list(
      id = "fifa_prediction_ssq_capture",
      filename = paste0("fifa2026_prediction_ss_q_seed_", fp$seed, ".png")
    ))
  }, ignoreInit = TRUE)

  output$download_fifa_prediction_path_html <- downloadHandler(
    filename = function() paste0("fifa2026_prediction_path_seed_", fifa_prediction_reactive()$seed, ".html"),
    contentType = "text/html; charset=utf-8",
    content = function(file) {
      fp <- fifa_prediction_reactive()
      save_tag_as_html(htmltools::tagList(make_fifa_prediction_path_html(fp), make_fifa_prediction_ssq_html(fp)),
                       file, title = "FIFA 2026 prediction path")
    }
  )

  output$download_fifa_prediction_ssq_html <- downloadHandler(
    filename = function() paste0("fifa2026_prediction_ss_q_seed_", fifa_prediction_reactive()$seed, ".html"),
    contentType = "text/html; charset=utf-8",
    content = function(file) {
      fp <- fifa_prediction_reactive()
      save_tag_as_html(make_fifa_prediction_ssq_html(fp), file, title = "FIFA 2026 SS and Q")
    }
  )

  output$tbl_reduced <- renderDT({ req(res()); datatable(res()$edges_reduced, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$tbl_memberships <- renderDT({ req(res()); datatable(res()$memberships_df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$tbl_quality <- renderDT({ req(res()); datatable(res()$quality_df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
  output$tbl_ranking <- renderDT({ req(res()); datatable(res()$ranking_df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
  output$plot_full <- renderVisNetwork({ req(res()); m <- input$full_method %||% "louvain"; mem <- if (m %in% names(res()$memberships_df)) res()$memberships_df[[m]] else res()$memberships_df$louvain; make_vis(res()$g_full, mem, top_n = if (isTRUE(input$full_use_all)) NULL else input$full_top_n, title = paste("Full network -", m), label_size = input$label_size %||% 30, bold = input$label_bold) })
  output$plot_flca <- renderVisNetwork({ req(res()); make_vis(res()$g_flca, res()$flca_carac, top_n = NULL, title = paste0("FLCA reduced one-link network | ", res()$cluster_source), label_size = input$label_size %||% 30, bold = input$label_bold) })
  output$flca_step1_nodes <- renderDT({
    req(res())
    nd <- res()$nodes
    if ("carac" %in% names(nd)) {
      nd$carac <- ifelse(is.na(nd$carac), "", as.character(as.integer(nd$carac)))
    }
    datatable(nd, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
  })
  output$flca_step1_edges <- renderDT({ req(res()); datatable(res()$edges, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$flca_step2_full_graph <- renderVisNetwork({
    req(res())
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    make_vis(bun$g, bun$membership, top_n = NULL, title = "Step 2 full graph: same capped Top-20 nodes used in all FLCA Process plots", label_size = 24, bold = TRUE)
  })
  output$flca_step3_reduced_graph <- renderVisNetwork({
    req(res())
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    make_vis(bun$g, bun$membership, top_n = NULL, title = paste0("Step 3 FLCA reduced graph: same capped Top-20 nodes | ", res()$cluster_source), label_size = 24, bold = TRUE)
  })
  output$flca_step4_cluster_summary <- renderDT({ req(res()); g <- res()$g_flca; mem <- res()$flca_carac; if (!is.null(names(mem)) && all(igraph::V(g)$name %in% names(mem))) mem <- mem[igraph::V(g)$name]; mem <- norm_mem(mem, igraph::vcount(g)); df <- data.frame(cluster = mem, value = suppressWarnings(as.numeric(igraph::V(g)$value))); sm <- aggregate(value ~ cluster, df, function(x) c(n = length(x), total = sum(x, na.rm = TRUE))); out <- data.frame(cluster = sm$cluster, n = sm$value[, "n"], total_value = sm$value[, "total"], cluster_source = res()$cluster_source); datatable(out, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  flca_step5_sankey_data <- reactive({
    req(res())
    make_flca_sankey_data(res(), target_n = input$flca_target_nodes %||% 20)
  })

  output$flca_step5_sankey_plot <- renderPlot({
    req(flca_step5_sankey_data())
    plot_flca_sankey_static(flca_step5_sankey_data())
  }, width = 1400, height = 760, res = 120)

  output$flca_step5_sankeymatic_code <- renderPrint({
    req(flca_step5_sankey_data())
    cat(flca_step5_sankey_data()$code)
  })
  output$flca_step6_ssplot <- renderPlot({ req(res()); render_real_SSplot82_panel(res(), target_n = input$flca_target_nodes %||% 20) }, width = 1300, height = 920, res = 120)
  output$flca_step6_kano <- renderPlot({ req(res()); print(make_flca_value_value2_kano(res(), label_size = 3.6, visual_ratio = 0.10, target_n = input$flca_target_nodes %||% 20)) }, width = 1800, height = 920, res = 120)

  # Step 6c chord dashboard: final Top-N FLCA/SSplot nodes and their final edges.
  flca_step6c_chord_data <- reactive({
    req(res())
    make_flca_chord_data(res(), target_n = input$flca_target_nodes %||% 20)
  })

  output$flca_step6c_chord_note <- renderPrint({
    req(flca_step6c_chord_data())
    chd <- flca_step6c_chord_data()
    cat(chd$note, "\n")
    cat("Install install.packages('circlize') if the chord panel is not shown. V40 uses a custom static chord for exact node consistency.")
  })

  output$flca_step6c_chord_ui <- renderUI({
    tagList(
      plotOutput("flca_step6c_chord_static", height = "860px", width = "100%"),
      tags$p(tags$em("V40 uses a custom circlize chord so sectors are based on the identical Top-20 node table, not an edge-only matrix."))
    )
  })

  output$flca_step6c_chord_static <- renderPlot({
    req(flca_step6c_chord_data())
    plot_flca_chord_static(flca_step6c_chord_data())
  }, width = 1400, height = 900, res = 120)

  output$flca_step6c_chord_nodes <- renderDT({
    req(flca_step6c_chord_data())
    datatable(flca_step6c_chord_data()$nodes, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$flca_step6c_chord_edges <- renderDT({
    req(flca_step6c_chord_data())
    datatable(flca_step6c_chord_data()$edges, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$download_flca_step6c_chord_edges <- downloadHandler(
    filename = function() paste0("flca_step6c_chord_edges_", Sys.Date(), ".csv"),
    content = function(file) {
      chd <- flca_step6c_chord_data()
      utils::write.csv(chd$edges, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # PNG downloads for all server-rendered FLCA Process plots.
  save_png_device <- function(file, width = 1300, height = 920, res = 120, expr) {
    grDevices::png(file, width = width, height = height, res = res)
    on.exit(grDevices::dev.off(), add = TRUE)
    par(family = "sans")
    force(expr)
  }

  output$download_flca_step2_png <- downloadHandler(
    filename = function() paste0("flca_step2_full_network_", Sys.Date(), ".png"),
    content = function(file) {
      req(res())
      bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
      save_png_device(file, width = 1500, height = 1000, res = 130,
        plot_static_network_png(bun$g, bun$membership, title = "Step 2 full graph: same capped Top-20 nodes", top_n = NULL)
      )
    }
  )

  output$download_flca_step3_png <- downloadHandler(
    filename = function() paste0("flca_step3_reduced_network_", Sys.Date(), ".png"),
    content = function(file) {
      req(res())
      bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
      save_png_device(file, width = 1500, height = 1000, res = 130,
        plot_static_network_png(bun$g, bun$membership, title = paste0("Step 3 FLCA reduced graph: same capped Top-20 nodes | ", res()$cluster_source), top_n = NULL)
      )
    }
  )

  output$download_flca_step5_sankey_png <- downloadHandler(
    filename = function() paste0("flca_step5b_sankey_", Sys.Date(), ".png"),
    content = function(file) {
      req(flca_step5_sankey_data())
      save_png_device(file, 1400, 760, 120, plot_flca_sankey_static(flca_step5_sankey_data()))
    }
  )

  output$download_flca_step6_ssplot_png <- downloadHandler(
    filename = function() paste0("flca_step6_ssplot_", Sys.Date(), ".png"),
    content = function(file) {
      req(res())
      save_png_device(file, width = 1600, height = 1100, res = 140,
        render_real_SSplot82_panel(res(), target_n = input$flca_target_nodes %||% 20)
      )
    }
  )

  output$download_flca_step6_kano_png <- downloadHandler(
    filename = function() paste0("flca_step6b_kano_", Sys.Date(), ".png"),
    content = function(file) {
      req(res())
      save_png_device(file, width = 1900, height = 1100, res = 140,
        print(make_flca_value_value2_kano(res(), label_size = 3.6, visual_ratio = 0.10, target_n = input$flca_target_nodes %||% 20))
      )
    }
  )

  output$download_flca_step6c_chord_png <- downloadHandler(
    filename = function() paste0("flca_step6c_chord_", Sys.Date(), ".png"),
    content = function(file) {
      req(flca_step6c_chord_data())
      save_png_device(file, width = 1500, height = 1200, res = 140,
        plot_flca_chord_static(flca_step6c_chord_data())
      )
    }
  )

  output$download_flca_process_all_png_zip <- downloadHandler(
    filename = function() paste0("flca_process_pngs_", Sys.Date(), ".zip"),
    content = function(file) {
      req(res())
      tmpd <- tempfile("flca_process_pngs_")
      dir.create(tmpd, recursive = TRUE, showWarnings = FALSE)
      f2 <- file.path(tmpd, "step2_full_network.png")
      f3 <- file.path(tmpd, "step3_reduced_network.png")
      f5b <- file.path(tmpd, "step5b_sankey.png")
      f6 <- file.path(tmpd, "step6_ssplot.png")
      f6b <- file.path(tmpd, "step6b_kano.png")
      f6c <- file.path(tmpd, "step6c_chord.png")
      bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
      save_png_device(f2, 1500, 1000, 130, plot_static_network_png(bun$g, bun$membership, title = "Step 2 full graph: same capped Top-20 nodes", top_n = NULL))
      save_png_device(f3, 1500, 1000, 130, plot_static_network_png(bun$g, bun$membership, title = paste0("Step 3 FLCA reduced graph: same capped Top-20 nodes | ", res()$cluster_source), top_n = NULL))
      save_png_device(f6, 1600, 1100, 140, render_real_SSplot82_panel(res(), target_n = input$flca_target_nodes %||% 20))
      save_png_device(f6b, 1900, 1100, 140, print(make_flca_value_value2_kano(res(), label_size = 3.6, visual_ratio = 0.10, target_n = input$flca_target_nodes %||% 20)))
      save_png_device(f5b, 1400, 760, 120, plot_flca_sankey_static(flca_step5_sankey_data()))
      save_png_device(f6c, 1500, 1200, 140, plot_flca_chord_static(flca_step6c_chord_data()))
      oldwd <- getwd(); on.exit(setwd(oldwd), add = TRUE)
      setwd(tmpd)
      utils::zip(zipfile = file, files = basename(c(f2, f3, f5b, f6, f6b, f6c)))
    }
  )

  output$flca_mode_annotation <- renderPrint({ req(res()); cat(paste("FLCA mode:", res()$flca_mode, "; cluster source:", res()$cluster_source, "; original uploaded cluster used:", res()$original_clusters_used, "; V30 stable rebuild with REAL FIFA online Wikipedia update, FIFA/NBA demo buttons, and 3 downloadable demo data files.")) })
  output$plot_visual_quality_kano <- renderPlot({
    req(res())
    q <- as.data.frame(res()$quality_df, stringsAsFactors = FALSE)
    q$method <- enc2utf8(as.character(q$method))
    q$method[is.na(q$method) | !nzchar(q$method)] <- paste0("Method ", seq_len(nrow(q)))[is.na(q$method) | !nzchar(q$method)]
    q$modularity <- suppressWarnings(as.numeric(q$modularity))
    q$mean_silhouette <- suppressWarnings(as.numeric(q$mean_silhouette))
    q <- q[is.finite(q$modularity) & is.finite(q$mean_silhouette), , drop = FALSE]
    if (nrow(q) < 2) {
      plot.new(); text(0.5, 0.5, "Kano plot requires at least two valid algorithm-quality rows.", cex = 1.2)
      return(invisible(NULL))
    }

    # V20: this Visual Quality plot is algorithm-level.  It must not be
    # compared visually with the node-level Kano under the SSplot unless the
    # same table and axis mapping are used.  Here we keep one stable mapping:
    #   x = modularity Q  -> value2 for Kano.R
    #   y = mean silhouette SS -> value for Kano.R
    q$carac <- if ("eval_graph" %in% names(q)) as.character(q$eval_graph) else if ("status" %in% names(q)) as.character(q$status) else "Algorithm"
    q$carac <- enc2utf8(q$carac)
    q$carac[is.na(q$carac) | !nzchar(q$carac)] <- "Algorithm"
    q$label_short <- q$method
    q$label_short <- gsub("components_independent_k", "components_k", q$label_short, fixed = TRUE)
    q$label_short <- gsub("FLCA by maturity \\(single\\)", "FLCA maturity", q$label_short)
    q$label_short <- gsub("FLCA by influence \\(single\\)", "FLCA influence", q$label_short)
    q$label_short <- gsub("edge_betweenness", "edge_between", q$label_short, fixed = TRUE)
    q$label_short <- substr(q$label_short, 1, 24)

    kano_nodes <- data.frame(
      name = enc2utf8(as.character(q$label_short)),
      value = as.numeric(q$mean_silhouette),  # y-axis in Kano.R plot_kano_real()
      value2 = as.numeric(q$modularity),      # x-axis in Kano.R plot_kano_real()
      carac = enc2utf8(as.character(q$carac)),
      stringsAsFactors = FALSE
    )

    # The original Kano.R geometry is sensitive to the data-range ratio.
    # Algorithm Q and SS often occupy very narrow ranges.  Use a dynamic
    # visual_ratio so the same true Q/SS coordinates are not drawn as a flat strip.
    dx <- diff(range(kano_nodes$value2, na.rm = TRUE)); if (!is.finite(dx) || dx <= 0) dx <- 1
    dy <- diff(range(kano_nodes$value,  na.rm = TRUE)); if (!is.finite(dy) || dy <= 0) dy <- dx / 5
    dynamic_ratio <- max(0.15, min(30, (dx / dy) * 0.75))

    p <- tryCatch({
      plot_kano_real(
        nodes = kano_nodes,
        title_txt = "Kano: algorithm quality (Q vs SS)",
        visual_ratio = dynamic_ratio,
        label_size = 3.4
      ) +
        ggplot2::labs(x = "Modularity Q (value2)", y = "Mean silhouette SS (value)") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 15, face = "bold", hjust = 0.5),
          plot.margin = ggplot2::margin(10, 30, 10, 10)
        )
    }, error = function(e) {
      ggplot2::ggplot(kano_nodes, ggplot2::aes(x = value2, y = value, label = name)) +
        ggplot2::geom_vline(xintercept = 0, color = "red", linetype = "dotted") +
        ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
        ggplot2::geom_point(ggplot2::aes(size = pmax(value, 0.001)), shape = 21, alpha = 0.9) +
        ggrepel::geom_text_repel(max.overlaps = 200, size = 3.2) +
        ggplot2::theme_minimal(base_family = "sans") +
        ggplot2::labs(
          title = "Kano fallback: algorithm quality (Q vs SS)",
          subtitle = paste("Kano.R fallback:", conditionMessage(e)),
          x = "Modularity Q (value2)", y = "Mean silhouette SS (value)"
        )
    })
    print(p)
  }, width = 900, height = 900, res = 120)

  output$visual_quality_kano_note <- renderPrint({
    req(res())
    q <- as.data.frame(res()$quality_df, stringsAsFactors = FALSE)
    ss <- suppressWarnings(as.numeric(q$mean_silhouette))
    qv <- suppressWarnings(as.numeric(q$modularity))
    ss_unique <- length(unique(round(ss[is.finite(ss)], 6)))
    q_unique <- length(unique(round(qv[is.finite(qv)], 6)))
    cat("Kano points are algorithm-level rows, not original graph nodes.\n")
    cat("Q is partition/graph modularity; it is not a per-node value. SS here is mean silhouette for each algorithm.\n")
    cat("Unique SS values (rounded to 6 decimals):", ss_unique, "; unique Q values:", q_unique, "\n")
    if (ss_unique <= 2) cat("Many SS values are identical because several algorithms produced the same/fallback membership or the same mean silhouette on this graph.\n")
  })
  output$tbl_visual_quality_kano <- renderDT({
    req(res())
    q <- as.data.frame(res()$quality_df, stringsAsFactors = FALSE)
    q$Q_modularity <- round(suppressWarnings(as.numeric(q$modularity)), 6)
    q$SS_mean_silhouette <- round(suppressWarnings(as.numeric(q$mean_silhouette)), 6)
    q$Kano_x <- q$Q_modularity
    q$Kano_y <- q$SS_mean_silhouette
    keep_cols <- intersect(c("method", "eval_graph", "n_clusters", "Q_modularity", "SS_mean_silhouette", "Kano_x", "Kano_y", "status", "details"), names(q))
    datatable(q[, keep_cols, drop = FALSE], options = list(pageLength = 20, scrollX = TRUE))
  })
  params_df <- reactive({ data.frame(parameter = c("input_mode", "target_k", "flca_mode", "occurrence_top_n", "flca_min_cluster_size", "flca_target_nodes", "sil_intra_penalty", "sil_inter_penalty", "tie_break_scale", "label_size", "label_bold", "app_version"), value = c(input$input_mode %||% "demo_csv", input$target_k %||% 4, input$flca_mode %||% "value", input$occurrence_top_n %||% 100, input$flca_min_cluster_size %||% 3, input$flca_target_nodes %||% 20, input$sil_intra_penalty %||% 2, input$sil_inter_penalty %||% 5, input$tie_break_scale %||% 10000, input$label_size %||% 30, input$label_bold %||% TRUE, "V30 REAL FIFA online Wikipedia update + FIFA bundled xlsx + NBA 2025-2026 xlsx + 3 downloadable demo data files + strict uploaded clusters + renderSSplot(82) + dynamic Kano.R"), stringsAsFactors = FALSE) })
  output$tbl_parameters <- renderDT({ datatable(params_df(), options = list(pageLength = 20, scrollX = TRUE)) })
  output$parameter_methods_text <- renderPrint({ cat(paste("FLCA analysis used one-link reduced graph construction, target k =", input$target_k %||% 4, ", FLCA mode =", input$flca_mode %||% "value", ", and V30 stable rebuild.")) })
  output$download_demo_dataset1 <- downloadHandler(
    filename = function() "dataset1.csv",
    content = function(file) {
      src <- file.path(getwd(), "dataset1.csv")
      if (file.exists(src)) {
        file.copy(src, file, overwrite = TRUE)
      } else {
        writeLines(c(
          "doc,term", "D01,臺大", "D01,臺大醫院", "D01,北醫", "D02,長庚大學", "D02,長庚醫院",
          "D03,陽明交通", "D03,北榮", "D04,成大", "D04,奇美"
        ), con = file, useBytes = TRUE)
      }
    }
  )
  output$download_demo_fifa2026_xlsx <- downloadHandler(
    filename = function() "fifa_2026_updated_nodes_edges.xlsx",
    content = function(file) {
      src <- file.path(getwd(), "fifa_2026_updated_nodes_edges.xlsx")
      if (file.exists(src)) {
        file.copy(src, file, overwrite = TRUE)
      } else {
        write_fifa2026_xlsx(file, fifa2026_nodes_demo_df(), fifa2026_edges_demo_df())
      }
    }
  )
  output$download_demo_nba2025_xlsx <- downloadHandler(
    filename = function() "nba.xlsx",
    content = function(file) {
      src <- file.path(getwd(), "nba.xlsx")
      if (!file.exists(src)) write_nba2025_xlsx(src, nba2025_nodes_demo_df(), nba2025_edges_demo_df())
      file.copy(src, file, overwrite = TRUE)
    }
  )

  output$download_fifa2026_xlsx <- downloadHandler(
    filename = function() "fifa_2026_updated_nodes_edges.xlsx",
    content = function(file) {
      src <- file.path(getwd(), "fifa_2026_updated_nodes_edges.xlsx")
      if (file.exists(src)) {
        file.copy(src, file, overwrite = TRUE)
      } else {
        write_fifa2026_xlsx(file, fifa2026_nodes_demo_df(), fifa2026_edges_demo_df())
      }
    }
  )
  output$download_nba_xlsx <- downloadHandler(
    filename = function() "nba.xlsx",
    content = function(file) {
      src <- file.path(getwd(), "nba.xlsx")
      if (!file.exists(src)) write_nba2025_xlsx(src, nba2025_nodes_demo_df(), nba2025_edges_demo_df())
      file.copy(src, file, overwrite = TRUE)
    }
  )
  output$download_memberships <- downloadHandler(filename = function() "memberships.csv", content = function(file) write.csv(res()$memberships_df, file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$download_quality <- downloadHandler(filename = function() "quality.csv", content = function(file) write.csv(res()$quality_df, file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$download_ranking <- downloadHandler(filename = function() "ranking.csv", content = function(file) write.csv(res()$ranking_df, file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$download_visual_quality_kano <- downloadHandler(filename = function() "visual_quality_kano.csv", content = function(file) write.csv(res()$quality_df, file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$download_parameters <- downloadHandler(filename = function() "parameters.csv", content = function(file) write.csv(params_df(), file, row.names = FALSE, fileEncoding = "UTF-8"))
}

shinyApp(ui, server)
