# app.R — V40: FIFA prediction remaps knockout results by team pair
# If this line is not shown by readLines(app.R, n=1), R is loading an old file.

options(shiny.maxRequestSize = 50 * 1024^2)
options(install.packages.compile.from.source = "never")
if (dir.exists(".r_libs")) .libPaths(c(normalizePath(".r_libs"), .libPaths()))

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

# Shared deterministic FLCA used by TaipeiMRT.R and edge-list CSV uploads.
if (!file.exists("flca_core.R")) stop("flca_core.R must be beside app.R")
source("flca_core.R", local = TRUE)
if (!file.exists("wgcna_module.R")) stop("wgcna_module.R must be beside app.R")
source("wgcna_module.R", local = TRUE)

message("*** LOADED V40 OFFICIAL BRACKET PAIR-MAP + KNOCKOUT LOCK at ", Sys.time(), " ***")


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


  # Safety: SSplot labels should show a non-self nearest/adjacent node.
  # If incoming sil_df still contains self-neighbors, repair them for display.
  repair_self_neighbors_for_display <- function(df, res = NULL) {
    if (!is.data.frame(df) || !all(c("name", "carac") %in% names(df))) return(df)
    df$name <- as.character(df$name)
    if (!"neighbor_name" %in% names(df)) df$neighbor_name <- NA_character_
    df$neighbor_name <- as.character(df$neighbor_name)
    bad <- is.na(df$neighbor_name) | !nzchar(df$neighbor_name) | df$neighbor_name == df$name
    if (!any(bad)) return(df)

    # Prefer a supplied weighted adjacency matrix if available.
    W <- NULL
    if (is.list(res) && !is.null(res$W_km)) W <- res$W_km
    if (is.matrix(W) && !is.null(rownames(W)) && !is.null(colnames(W))) {
      for (ii in which(bad)) {
        nm <- df$name[ii]
        if (nm %in% rownames(W)) {
          w <- suppressWarnings(as.numeric(W[nm, ]))
          names(w) <- colnames(W)
          w[names(w) == nm] <- NA_real_
          ok <- is.finite(w) & w > 0
          if (any(ok)) {
            cand <- names(w)[ok]
            ord <- order(-w[cand], cand)
            df$neighbor_name[ii] <- cand[ord[1]]
            if ("neighborC" %in% names(df)) {
              jj <- match(df$neighbor_name[ii], df$name)
              if (!is.na(jj)) df$neighborC[ii] <- df$carac[jj]
            }
          }
        }
      }
    }

    # Fallback: same cluster, non-self, highest value; then any non-self node.
    bad <- is.na(df$neighbor_name) | !nzchar(df$neighbor_name) | df$neighbor_name == df$name
    val <- if ("value" %in% names(df)) suppressWarnings(as.numeric(df$value)) else rep(1, nrow(df))
    val[!is.finite(val)] <- 1
    for (ii in which(bad)) {
      cand <- which(df$carac == df$carac[ii] & df$name != df$name[ii])
      if (!length(cand)) cand <- which(df$name != df$name[ii])
      if (length(cand)) {
        ord <- order(-val[cand], df$name[cand])
        jj <- cand[ord[1]]
        df$neighbor_name[ii] <- df$name[jj]
        if ("neighborC" %in% names(df)) df$neighborC[ii] <- df$carac[jj]
      }
    }
    df
  }
  sil_plot <- repair_self_neighbors_for_display(sil_plot, res = res)

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
      if ("value"  %in% names(sil_plot)) f2(sil_plot$value[i]) else NULL,
      if ("value2" %in% names(sil_plot)) f2(sil_plot$value2[i]) else NULL,
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
      if ("value"  %in% names(sil_plot)) f2(sil_plot$value[i]) else NULL,
      if ("value2" %in% names(sil_plot)) f2(sil_plot$value2[i]) else NULL,
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
                           visual_ratio = 1,
                           label_size = 4,
                           origin = c(0, 0)){

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

  # Bubble area is the node value used by the network.  A caller can provide
  # size_plot explicitly, but the uploaded-CSV Kano plot supplies `value`.
  if (!"size_plot" %in% names(nodes)) nodes$size_plot <- pmax(nodes$value, 0)
  nodes$size_plot <- .safe_num(nodes$size_plot)
  min_pos <- suppressWarnings(min(nodes$size_plot[nodes$size_plot > 0], na.rm = TRUE))
  if (!is.finite(min_pos)) min_pos <- 0.05
  nodes$size_plot[!is.finite(nodes$size_plot) | nodes$size_plot <= 0] <- min_pos

  # Kano reference: both wings and the rings share the axis intersection.
  # FLCA passes the data midpoint so the geometry stays visible when raw
  # value/value2 coordinates are far from zero.
  stopifnot(length(origin) == 2L, all(is.finite(origin)))
  origin_x <- origin[1]
  origin_y <- origin[2]

  # limits
  max_x <- max(nodes$value2, na.rm = TRUE)
  min_x <- min(nodes$value2, na.rm = TRUE)
  max_y <- max(nodes$value,  na.rm = TRUE)
  min_y <- min(nodes$value,  na.rm = TRUE)
  dx <- max_x - min_x; if (!is.finite(dx) || dx == 0) dx <- 1
  dy <- max_y - min_y; if (!is.finite(dy) || dy == 0) dy <- 1

  # Use one data span for both axes so reference circles remain circular.
  span_xy <- max(dx, dy, na.rm = TRUE)
  if (!is.finite(span_xy) || span_xy <= 0) span_xy <- 1
  center_x <- (max(max_x, origin_x) + min(min_x, origin_x)) / 2
  center_y <- (max(max_y, origin_y) + min(min_y, origin_y)) / 2
  half_span <- max(span_xy * 1.10, abs(center_x) + span_xy * .10, abs(center_y) + span_xy * .10)
  x_limits_square <- c(center_x - half_span, center_x + half_span)
  y_limits_square <- c(center_y - half_span, center_y + half_span)

  # ---- Kano wings ----
  # The upper wing starts at the second reference circle on the positive
  # y-axis.  The lower wing starts on the negative y-axis and approaches
  # the x-axis from below, as in the author Kano sketch.
  t <- seq(0, 1, length.out = 400)
  upper_length <- 0.48 * span_xy
  upper_height <- 0.75 * span_xy
  lower_length <- 0.76 * span_xy
  lower_height <- 0.27 * span_xy
  upper_curve <- data.frame(
    x = origin_x + upper_length * t^0.72,
    y = origin_y + upper_height * t^1.35
  )
  lower_curve <- data.frame(
    x = origin_x + lower_length * t,
    y = origin_y - lower_height * (0.14 + 0.86 * exp(-5 * t))
  )

  # ---- outer circle (pink) ----
  # define a stable outer radius from wing boundary in DISPLAY metric
  wing_dense <- rbind(lower_curve, upper_curve)
  dxv <- wing_dense$x - origin_x
  dyv <- (wing_dense$y - origin_y) * visual_ratio
  wing_outer_radius <- suppressWarnings(max(sqrt(dxv^2 + dyv^2), na.rm = TRUE))
  if (!is.finite(wing_outer_radius) || wing_outer_radius <= 0) wing_outer_radius <- max(dx, dy)

  theta <- seq(0, 2*pi, length.out = 600)
  circle_radius <- wing_outer_radius * 0.55
  circle_data <- data.frame(
    x = origin_x + circle_radius * cos(theta),
    y = origin_y + (circle_radius * sin(theta)) / visual_ratio
  )

  # ---- small hub circle (purple) ----
  # Keep the small reference rings centered on the common wing hub.
  gap_half_y <- lower_height * 0.50
  # convert y-gap to DISPLAY radius and shrink slightly (0.97) to avoid crossing
  hub_r <- gap_half_y * visual_ratio * 0.97
  hub_r <- max(hub_r, wing_outer_radius * 0.04)
  hub_r <- min(hub_r, wing_outer_radius * 0.35)

  theta_h <- seq(0, 2*pi, length.out = 360)
  hub_circle <- data.frame(
    x = origin_x + hub_r * cos(theta_h),
    y = origin_y + (hub_r * sin(theta_h)) / visual_ratio
  )

  # ---- additional circle (red) : radius = 2 * inner circle ----
  hub_r2 <- hub_r * 2
  hub_r2 <- min(hub_r2, wing_outer_radius * 0.95)
  theta_h2 <- seq(0, 2*pi, length.out = 360)
  hub_circle2 <- data.frame(
    x = origin_x + hub_r2 * cos(theta_h2),
    y = origin_y + (hub_r2 * sin(theta_h2)) / visual_ratio
  )

  # Put the upper wing exactly at the top of the second circle (x = 0).
  upper_start_y <- hub_r2 / visual_ratio
  upper_curve$y <- origin_y + upper_start_y +
    (upper_height - upper_start_y) * t^1.35

  wing_poly <- rbind(upper_curve, lower_curve[rev(seq_len(nrow(lower_curve))), ])

  # ---- plot ----
  p <- ggplot(nodes, aes(x=value2, y=value)) +

    # red dotted axes through the shared Kano hub
    geom_vline(xintercept = origin_x, color = "red", linetype = "dotted", linewidth = 0.9, alpha = 0.85) +
    geom_hline(yintercept = origin_y, color = "red", linetype = "dotted", linewidth = 0.9, alpha = 0.85) +

    geom_polygon(data=wing_poly, aes(x=x,y=y),
                 inherit.aes=FALSE, fill="lightskyblue1", alpha=.18, color=NA) +

    # purple hub circle (white cut-out + purple ring)
    geom_polygon(data=hub_circle, aes(x=x,y=y),
                 inherit.aes=FALSE, fill="white", color=NA) +
    geom_path(data=hub_circle, aes(x=x,y=y),
              inherit.aes=FALSE, color="purple", linewidth=0.7, linetype="solid") +

    geom_path(data=hub_circle2, aes(x=x,y=y),
              inherit.aes=FALSE, color="hotpink3", linewidth=0.7, linetype="solid") +

    geom_line(data=lower_curve, aes(x=x,y=y),
              inherit.aes=FALSE, color="blue", linewidth=2.3) +
    geom_line(data=upper_curve, aes(x=x,y=y),
              inherit.aes=FALSE, color="blue", linewidth=2.3) +

    geom_path(data=circle_data, aes(x=x,y=y),
              inherit.aes=FALSE, color="hotpink3", linewidth=0.7) +

    geom_point(aes(size=size_plot, fill=fill_col),
               shape=21, color="black", alpha=.9) +
    geom_text_repel(aes(label=name), size=label_size, max.overlaps=200) +

    scale_fill_identity() +
    scale_size(range=c(3,12)) +
    coord_fixed(ratio=1, clip="off") +
    scale_x_continuous(limits = x_limits_square) +
    scale_y_continuous(limits = y_limits_square) +
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
                             visual_ratio = 1) {
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
                      visual_ratio = 1,
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
                              visual_ratio = 1,
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
  knockout_url <- "https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_knockout_stage"
  progress(0.04, "Prepared Wikipedia Group A-L plus knockout-stage source URLs")

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
        match_stage = "Group stage", match_number = NA_integer_,
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


  # ---- Knockout online update ------------------------------------------------
  # The 2026 format has 48 teams in the group stage and a 32-team knockout stage.
  # Earlier versions parsed only the first 16 knockout boxes (Round of 32), which
  # allowed stale prediction paths such as Brazil as champion after Brazil had
  # already lost in the Round of 16. V39 parses every completed knockout box
  # through the final, and the prediction module below separately locks completed
  # knockout outcomes so eliminated teams cannot advance.
  knockout_stage_from_match_no <- function(match_no) {
    match_no <- as.integer(match_no)
    dplyr::case_when(
      match_no >= 73L  & match_no <= 88L  ~ "Round of 32",
      match_no >= 89L  & match_no <= 96L  ~ "Round of 16",
      match_no >= 97L  & match_no <= 100L ~ "Quarterfinals",
      match_no >= 101L & match_no <= 102L ~ "Semifinals",
      match_no == 103L                    ~ "Third-place match",
      match_no == 104L                    ~ "Final",
      TRUE                                ~ "Knockout"
    )
  }
  knockout_group_from_stage <- function(stage) {
    dplyr::case_when(
      stage == "Round of 32"       ~ "R32",
      stage == "Round of 16"       ~ "R16",
      stage == "Quarterfinals"     ~ "QF",
      stage == "Semifinals"        ~ "SF",
      stage == "Third-place match" ~ "3P",
      stage == "Final"             ~ "F",
      TRUE                          ~ "KO"
    )
  }
  knockout_cluster_from_stage <- function(stage) {
    dplyr::case_when(
      stage == "Round of 32"       ~ 13L,
      stage == "Round of 16"       ~ 14L,
      stage == "Quarterfinals"     ~ 15L,
      stage == "Semifinals"        ~ 16L,
      stage == "Third-place match" ~ 17L,
      stage == "Final"             ~ 18L,
      TRUE                          ~ 19L
    )
  }

  knockout_matches_parsed <- 0L
  progress(0.64, paste0("Fetching FIFA knockout stage completed matches: ", knockout_url))
  knockout_page <- tryCatch(read_html_ua(knockout_url), error = function(e) NULL)
  if (!is.null(knockout_page)) {
    pages_read <- pages_read + 1L
    boxes_k <- rvest::html_elements(knockout_page, ".footballbox, table.vevent, div.vevent")
    if (length(boxes_k)) {
      k_before_knockout <- k
      for (jj in seq_along(boxes_k)) {
        match_no <- 72L + as.integer(jj)
        if (match_no > 104L) next
        stage <- knockout_stage_from_match_no(match_no)
        stage_group <- knockout_group_from_stage(stage)
        stage_cluster <- knockout_cluster_from_stage(stage)

        b <- boxes_k[[jj]]
        team1 <- safe_text(b, ".fhome")
        team2 <- safe_text(b, ".faway")
        score <- safe_text(b, ".fscore")
        if (is.na(team1) || is.na(team2) || is.na(score) || !stringr::str_detect(score, "\\d+\\s*[-–—]\\s*\\d+")) next
        sc <- parse_score(score)
        if (any(is.na(sc))) next
        k <- k + 1L
        all_matches[[k]] <- data.frame(
          match_stage = stage, match_number = match_no,
          group = stage_group, cluster = stage_cluster, match_no_in_group = match_no,
          team1 = clean_team_name(team1), team2 = clean_team_name(team2),
          score_text = stringr::str_squish(score), score1 = sc[1], score2 = sc[2],
          source_url = knockout_url, stringsAsFactors = FALSE
        )
        home_team_clean <- clean_team_name(team1)
        away_team_clean <- clean_team_name(team2)
        scorer_rows <- dplyr::bind_rows(
          extract_scorer_rows(b, ".fhgoal", home_team_clean, stage_group, stage_cluster, match_no, k, knockout_url),
          extract_scorer_rows(b, ".fagoal", away_team_clean, stage_group, stage_cluster, match_no, k, knockout_url)
        )
        if (nrow(scorer_rows) > 0) {
          sk <- sk + 1L
          all_scorers[[sk]] <- scorer_rows
        }
      }
      knockout_matches_parsed <- k - k_before_knockout
      if (knockout_matches_parsed > 0L) pages_with_completed_matches <- pages_with_completed_matches + 1L
    }
  }
  progress(0.66, paste0("Combining group-stage and completed knockout matches; knockout parsed = ", knockout_matches_parsed))
  if (!length(all_matches)) {
    stop("No completed FIFA 2026 matches were parsed. Check internet access or Wikipedia page structure.", call. = FALSE)
  }

  matches_source <- dplyr::bind_rows(all_matches) |>
    dplyr::filter(!is.na(team1), !is.na(team2), !is.na(score1), !is.na(score2), nzchar(team1), nzchar(team2)) |>
    dplyr::distinct(match_stage, match_number, group, cluster, team1, team2, score1, score2, .keep_all = TRUE) |>
    dplyr::mutate(
      result_type = dplyr::case_when(score1 > score2 ~ "win_loss", score1 < score2 ~ "win_loss", score1 == score2 ~ "draw", TRUE ~ NA_character_),
      winner = dplyr::case_when(score1 > score2 ~ team1, score2 > score1 ~ team2, TRUE ~ NA_character_),
      loser  = dplyr::case_when(score1 > score2 ~ team2, score2 > score1 ~ team1, TRUE ~ NA_character_),
      team1_points = dplyr::case_when(score1 > score2 ~ 3L, score1 == score2 ~ 1L, TRUE ~ 0L),
      team2_points = dplyr::case_when(score2 > score1 ~ 3L, score1 == score2 ~ 1L, TRUE ~ 0L)
    ) |>
    dplyr::arrange(cluster, match_no_in_group, match_number)

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
    dplyr::transmute(term1 = winner, term2 = loser, WCD = 3L, group = group, cluster = cluster, match_stage = match_stage, match_number = match_number, score = paste0(score1, "-", score2))
  draw_edges <- matches_source |>
    dplyr::filter(score1 == score2) |>
    dplyr::transmute(term1 = team1, term2 = team2, WCD = 1L, group = group, cluster = cluster, match_stage = match_stage, match_number = match_number, score = paste0(score1, "-", score2))
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
  n_group_stage <- sum(matches_source$match_stage == "Group stage", na.rm = TRUE)
  n_round32 <- sum(matches_source$match_stage == "Round of 32", na.rm = TRUE)
  n_round16 <- sum(matches_source$match_stage == "Round of 16", na.rm = TRUE)
  n_knockout <- sum(matches_source$match_stage != "Group stage", na.rm = TRUE)
  validation <- data.frame(
    item = c("Completed matches parsed", "Group-stage matches parsed", "Knockout matches parsed", "Round-of-32 matches parsed", "Round-of-16 matches parsed", "Directed edges", "Teams / nodes", "Win/loss matches", "Draw matches", "Sum(nodes$value)", "Expected total FIFA points", "Check: sum(nodes$value) == expected_total_points", "Check: nrow(edges) == nrow(matches_source)", "Missing node cluster values", "Minimum cluster", "Maximum cluster", "Output file"),
    value = c(as.character(nrow(matches_source)), as.character(n_group_stage), as.character(n_knockout), as.character(n_round32), as.character(n_round16), as.character(nrow(edges_xlsx)), as.character(nrow(nodes)), as.character(n_win_loss), as.character(n_draw), as.character(sum(nodes$value)), as.character(expected_total_points), as.character(sum(nodes$value) == expected_total_points), as.character(nrow(edges_xlsx) == nrow(matches_source)), as.character(sum(is.na(nodes$cluster) | nodes$cluster == 0)), as.character(min(nodes$cluster, na.rm = TRUE)), as.character(max(nodes$cluster, na.rm = TRUE)), output_file),
    stringsAsFactors = FALSE
  )

  progress(0.88, "Validating totals and preparing group performance summary")
  reporting_date <- Sys.time()
  group_performance_summary <- make_fifa_group_performance_summary(nodes, reporting_date = reporting_date)
  golden_boot_summary <- make_fifa_golden_boot_summary(scorers_source, reporting_date = reporting_date, top_n = 30)
  golden_boot_slope <- make_fifa_golden_boot_slope_data(scorers_source, reporting_date = reporting_date, top_n = 10, max_steps = 6)
  progress(0.90, "Preparing workbook sheets including Group A-L summary and Golden Boot chase")
  readme <- fifa2026_readme_df()
  sources <- rbind(
    group_urls,
    data.frame(group = c("R32", "R16", "QF", "SF", "3P", "F"),
               cluster = 13:18, url = knockout_url, stringsAsFactors = FALSE)
  )
  online_run_info <- data.frame(
    item = c("update_mode", "online_started_at", "online_completed_at", "pages_attempted", "pages_read", "pages_with_completed_matches", "completed_matches_parsed", "group_stage_matches_parsed", "knockout_matches_parsed", "round_of_32_matches_parsed", "round_of_16_matches_parsed", "scorer_rows_parsed", "source_domain", "output_file"),
    value = c("REAL online Wikipedia scrape; group stage plus completed knockout matches; no bundled XLSX fallback", as.character(online_started_at), as.character(Sys.time()), as.character(nrow(group_urls) + 1L), as.character(pages_read), as.character(pages_with_completed_matches), as.character(nrow(matches_source)), as.character(n_group_stage), as.character(n_knockout), as.character(n_round32), as.character(n_round16), as.character(nrow(scorers_source)), "en.wikipedia.org", output_file),
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
    data_mode = paste0("REAL online FIFA 2026 update from Wikipedia Group A-L plus completed knockout matches; group matches parsed: ", n_group_stage, "; knockout matches parsed: ", n_knockout, "; Round-of-32 matches parsed: ", n_round32, "; Round-of-16 matches parsed: ", n_round16, "; total completed matches parsed: ", nrow(matches_source), "; pages read: ", pages_read, "/", nrow(group_urls) + 1L, "; scorer rows parsed: ", nrow(scorers_source), "; workbook saved: ", basename(output_file)),
    online_details = paste0("ONLINE FIFA UPDATE: fetched current Wikipedia Group A-L plus knockout page at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "; pages read ", pages_read, "/", nrow(group_urls) + 1L, "; pages with completed matches ", pages_with_completed_matches, "; group matches parsed ", n_group_stage, "; knockout matches parsed ", n_knockout, "; Round-of-32 matches parsed ", n_round32, "; Round-of-16 matches parsed ", n_round16, "; total matches parsed ", nrow(matches_source), "; scorer rows parsed ", nrow(scorers_source), "; output file ", basename(output_file)),
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
  lcol <- which(nms0 %in% c("leader", "from", "source", "node1", "term1", "u", "a"))[1]
  fcol <- which(nms0 %in% c("follower", "follow", "to", "target", "node2", "term2", "v", "b"))[1]
  wcol <- which(nms0 %in% c("wcd", "weight", "weights", "value", "count", "freq", "strength", "score"))[1]
  if (is.na(lcol)) lcol <- 1
  if (is.na(fcol)) fcol <- 2
  if (is.na(wcol) && ncol(x) >= 3) wcol <- 3
  if (is.na(wcol)) {
    w <- rep(1, nrow(x))
  } else {
    raw_w <- trimws(as.character(x[[wcol]]))
    missing_w <- is.na(raw_w) | !nzchar(raw_w) | toupper(raw_w) == "NA"
    w <- suppressWarnings(as.numeric(raw_w))
    if (any(!missing_w & !is.finite(w)))
      stop("WCD must be numeric when supplied.", call. = FALSE)
    w[missing_w] <- 1
  }
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
  out <- out[order(-out$value, out$name), c("name", "value", "value2", "carac"), drop = FALSE]
  condition_col <- which(tolower(names(nodes)) == "condition")[1L]
  if (!is.na(condition_col)) {
    source_names <- trimws(as_utf8(nodes[[ncol1]]))
    out$Condition <- as.character(nodes[[condition_col]])[match(out$name, source_names)]
  }
  out
}


# ---- CSV / co-word readers --------------------------------------------------
numeric_rate <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  z <- suppressWarnings(as.numeric(x))
  mean(is.finite(z), na.rm = TRUE)
}

read_csv_file_safe <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop("CSV/TXT/TSV file not found: ", path, call. = FALSE)
  }
  encs <- c("UTF-8", "UTF-8-BOM", "CP950", "Big5", "BIG5", "GB18030", "latin1")
  seps <- c(",", "\t", ";", "|")
  last_err <- NULL

  # Try common separators. This is important for Excel-copied A1-A8 coword data,
  # which is usually tab-separated even when the file extension is .txt or .csv.
  for (enc in encs) {
    for (sep in seps) {
      df <- tryCatch(
        utils::read.table(
          path,
          header = TRUE,
          sep = sep,
          fileEncoding = enc,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          quote = "\"",
          comment.char = "",
          fill = TRUE,
          blank.lines.skip = TRUE
        ),
        error = function(e) { last_err <<- conditionMessage(e); NULL }
      )
      if (!is.null(df) && nrow(df) > 0 && ncol(df) >= 2) return(safe_df(df))
    }
  }

  # Fallback: let R infer whitespace-separated tables.
  for (enc in encs) {
    df <- tryCatch(
      utils::read.table(
        path,
        header = TRUE,
        fileEncoding = enc,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        quote = "\"",
        comment.char = "",
        fill = TRUE,
        blank.lines.skip = TRUE
      ),
      error = function(e) { last_err <<- conditionMessage(e); NULL }
    )
    if (!is.null(df) && nrow(df) > 0 && ncol(df) >= 2) return(safe_df(df))
  }

  # Some Windows R locales reject valid CP950/Big5 bytes on a file connection.
  # Decode the complete file first, then parse the resulting UTF-8 text.
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  raw_text <- rawToChar(bytes)
  for (enc in encs) {
    utf8_text <- tryCatch(suppressWarnings(iconv(raw_text, from = enc,
                                                 to = "UTF-8", sub = NA_character_)),
                          error = function(e) NA_character_)
    if (is.na(utf8_text)) next
    for (sep in seps) {
      df <- tryCatch(utils::read.table(
        text = utf8_text, header = TRUE, sep = sep,
        stringsAsFactors = FALSE, check.names = FALSE, quote = "\"",
        comment.char = "", fill = TRUE, blank.lines.skip = TRUE
      ), error = function(e) { last_err <<- conditionMessage(e); NULL })
      if (!is.null(df) && nrow(df) > 0 && ncol(df) >= 2) return(safe_df(df))
    }
  }

  stop(
    "Could not read a usable co-word file. Save the A1-A8 table as tab-delimited .txt/.tsv or comma-separated .csv. Last error: ",
    last_err %||% "unknown",
    call. = FALSE
  )
}


is_edge_table <- function(df) {
  nms0 <- tolower(gsub("[^[:alnum:]]", "", names(df)))
  # Two-column A1/A2 files such as TaipeiMRT.csv are station-pair edge lists.
  # Reading them as doc/term rows replaces the supplied links with unrelated
  # co-occurrence links and creates artificial isolated components.
  if (ncol(df) == 2L && identical(nms0, c("a1", "a2"))) return(TRUE)
  has_l <- any(nms0 %in% c("leader", "from", "source", "node1", "term1", "u", "a"))
  has_f <- any(nms0 %in% c("follower", "follow", "to", "target", "node2", "term2", "v", "b"))
  isTRUE(has_l && has_f)
}

# ---- TAAA / row-wise co-word helpers ----------------------------------------
# Row-wise TAAA input means one article/record per row, usually with columns such
# as 主題名稱, 主題詞頻, and Cluster.  The parser below converts those rows into:
#   1) a full ToT co-occurrence edge table among all terms in the same row, and
#   2) a one-edge leader -> follower table using the row's representative theme.
locate_named_col <- function(df, exact = character(0), pattern = NULL) {
  nms <- trimws(as_utf8(names(df)))
  low <- tolower(nms)
  exact_low <- tolower(as_utf8(exact))
  hit <- which(nms %in% exact | low %in% exact_low)
  if (length(hit)) return(hit[1])
  if (!is.null(pattern)) {
    hit <- which(grepl(pattern, nms, ignore.case = TRUE, perl = TRUE) |
                   grepl(pattern, low, ignore.case = TRUE, perl = TRUE))
    if (length(hit)) return(hit[1])
  }
  NA_integer_
}

parse_taaa_terms <- function(x) {
  z <- paste(as_utf8(x), collapse = " ")
  z <- trimws(z)
  if (!nzchar(z) || is.na(z)) {
    return(data.frame(term = character(0), freq = numeric(0), stringsAsFactors = FALSE))
  }
  zl <- tolower(z)
  # In TAAA output, these rows explicitly mean no usable Top-20 topic assignment.
  if (grepl("^no\\s+(keyword|top20|top-20|top 20)", zl, perl = TRUE) ||
      grepl("^no keyword/profile terms", zl, perl = TRUE)) {
    return(data.frame(term = character(0), freq = numeric(0), stringsAsFactors = FALSE))
  }
  # If the message includes a colon, use the term list after the final colon.
  if (grepl("no\\s+top", zl, perl = TRUE) && grepl(":", z, fixed = TRUE)) {
    z <- sub("^.*:", "", z)
  }
  z <- gsub("[\r\n]+", ";", z, perl = TRUE)
  z <- gsub("，|、|；", ";", z, perl = TRUE)
  # Split on list separators only; spaces and hyphens are kept inside terms.
  parts <- unlist(strsplit(z, "\\s*(;|,|\\|)\\s*", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return(data.frame(term = character(0), freq = numeric(0), stringsAsFactors = FALSE))
  }
  freq <- rep(1, length(parts))
  m <- regexec("^(.*?)(?:\\s*\\(([0-9]+(?:\\.[0-9]+)?)\\)\\s*)$", parts, perl = TRUE)
  rx <- regmatches(parts, m)
  for (i in seq_along(rx)) {
    if (length(rx[[i]]) == 3) {
      parts[i] <- trimws(rx[[i]][2])
      ff <- suppressWarnings(as.numeric(rx[[i]][3]))
      if (is.finite(ff) && ff > 0) freq[i] <- ff
    }
  }
  parts <- gsub('^["\']+|["\']+$', "", parts, perl = TRUE)
  parts <- trimws(parts)
  keep <- nzchar(parts) & !tolower(parts) %in% c("na", "nan", "none", "unclassified")
  if (!any(keep)) {
    return(data.frame(term = character(0), freq = numeric(0), stringsAsFactors = FALSE))
  }
  out <- data.frame(term = parts[keep], freq = freq[keep], stringsAsFactors = FALSE)
  out <- aggregate(freq ~ term, out, sum)
  out[order(-out$freq, out$term), , drop = FALSE]
}

rowwise_term_list_col <- function(df) {
  nms <- trimws(as_utf8(names(df)))
  # Prefer the explicit TAAA column names first.
  hit <- locate_named_col(
    df,
    exact = c("主題詞頻", "topic_freq", "topic_frequency", "topic_terms", "theme_terms", "terms_freq", "term_frequency", "keywords", "keyword", "mesh_terms", "MeSH", "terms", "coword", "co_word"),
    pattern = "主題詞頻|topic.*(freq|term)|theme.*term|term.*freq|keyword|mesh|coword|co.word"
  )
  if (!is.na(hit)) return(hit)

  # Fallback: choose the text column that most looks like a row-wise term list.
  rates <- vapply(df, function(z) {
    zz <- as_utf8(z)
    mean(grepl("[;,|]|\\([0-9]+(?:\\.[0-9]+)?\\)", zz, perl = TRUE), na.rm = TRUE)
  }, numeric(1))
  if (length(rates) && max(rates, na.rm = TRUE) >= 0.20) return(which.max(rates))
  NA_integer_
}

rowwise_term_matrix_cols <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) < 3) return(integer(0))
  nms <- trimws(as_utf8(names(df)))
  low <- tolower(nms)
  meta_pat <- "^(row|id|doc|document|paper|article|pmid|profile|group|cluster|class|membership|carac|theme|topic|主題名稱|主題詞頻)$"
  cand <- which(!grepl(meta_pat, low, perl = TRUE))
  if (!length(cand)) return(integer(0))
  keep <- vapply(cand, function(j) {
    x <- trimws(as_utf8(df[[j]]))
    nz <- mean(nzchar(x) & !tolower(x) %in% c("na", "nan", "none"), na.rm = TRUE)
    nr <- numeric_rate(x)
    is.finite(nz) && nz >= 0.10 && (!is.finite(nr) || nr < 0.30)
  }, logical(1))
  cand <- cand[keep]
  if (length(cand) < 3) return(integer(0))
  row_n_terms <- vapply(seq_len(nrow(df)), function(i) {
    x <- trimws(as_utf8(unlist(df[i, cand, drop = TRUE], use.names = FALSE)))
    x <- x[nzchar(x) & !tolower(x) %in% c("na", "nan", "none")]
    length(x)
  }, integer(1))
  if (mean(row_n_terms >= 2, na.rm = TRUE) < 0.20) return(integer(0))
  cand
}

is_rowwise_term_matrix <- function(df) {
  length(rowwise_term_matrix_cols(df)) >= 3
}

extract_terms_from_row_cells <- function(vals) {
  parts <- list(); kk <- 0L
  for (v in vals) {
    td <- parse_taaa_terms(v)
    if (nrow(td)) {
      kk <- kk + 1L
      parts[[kk]] <- td
    }
  }
  if (!length(parts)) {
    return(data.frame(term = character(0), freq = numeric(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, parts)
  out <- aggregate(freq ~ term, out, sum)
  out[order(-out$freq, out$term), , drop = FALSE]
}


is_rowwise_taaa_coword <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) < 3) return(FALSE)
  nms <- trimws(as_utf8(names(df)))
  if (any(nms %in% c("主題詞頻", "主題名稱"))) return(TRUE)
  tc <- rowwise_term_list_col(df)
  if (!is.na(tc)) {
    vals <- as_utf8(df[[tc]])
    list_rate <- mean(grepl("[;,|]|\\([0-9]+(?:\\.[0-9]+)?\\)", vals, perl = TRUE), na.rm = TRUE)
    if (is.finite(list_rate) && list_rate >= 0.20) return(TRUE)
  }
  if (is_rowwise_term_matrix(df)) return(TRUE)
  FALSE
}


weighted_mode_cluster <- function(cluster, weight) {
  cc <- parse_cluster_ids(cluster)
  ww <- suppressWarnings(as.numeric(weight))
  ww[!is.finite(ww) | ww <= 0] <- 1
  keep <- !is.na(cc) & is.finite(cc)
  if (!any(keep)) return(NA_integer_)
  sm <- tapply(ww[keep], cc[keep], sum, na.rm = TRUE)
  suppressWarnings(as.integer(names(sm)[which.max(sm)]))
}

build_taaa_tot_from_rows <- function(df, top_n_nodes = 100) {
  df <- safe_df(df)
  top_n_nodes <- suppressWarnings(as.integer(top_n_nodes))
  if (!is.finite(top_n_nodes) || top_n_nodes < 2) top_n_nodes <- 100L

  term_col <- rowwise_term_list_col(df)
  term_matrix_cols <- integer(0)
  if (is.na(term_col)) {
    term_matrix_cols <- rowwise_term_matrix_cols(df)
    if (length(term_matrix_cols) < 3) {
      stop("No row-wise term-list column or A1/A2/... term matrix was detected. Use a column such as 主題詞頻, keywords, MeSH, or a row-wise term matrix with columns like A1-A8.", call. = FALSE)
    }
  }

  theme_col <- locate_named_col(
    df,
    exact = c("主題名稱", "theme", "topic", "topic_name", "representative", "leader", "leader_term"),
    pattern = "主題名稱|theme|topic|representative|leader"
  )
  cluster_col <- locate_named_col(
    df,
    exact = c("Cluster", "cluster", "carac", "group", "profile", "membership", "community", "class"),
    pattern = "^cluster$|carac|group|profile|membership|community|class"
  )

  node_rows <- list()
  full_edges <- list()
  one_edges <- list()
  rr <- 0L; ee <- 0L; oo <- 0L

  for (i in seq_len(nrow(df))) {
    if (!is.na(term_col)) {
      td <- parse_taaa_terms(df[[term_col]][i])
    } else {
      td <- extract_terms_from_row_cells(df[i, term_matrix_cols, drop = TRUE])
    }
    if (!nrow(td)) next
    row_cluster <- if (!is.na(cluster_col)) df[[cluster_col]][i] else NA

    leader <- NA_character_
    if (!is.na(theme_col)) {
      cand <- parse_taaa_terms(df[[theme_col]][i])
      if (nrow(cand)) leader <- cand$term[1]
      if (!nzchar(leader %||% "") || tolower(leader) %in% c("unclassified", "na", "none")) leader <- NA_character_
    }
    if ((is.na(leader) || !nzchar(leader)) && length(term_matrix_cols)) {
      row_vals <- trimws(as_utf8(unlist(df[i, term_matrix_cols, drop = TRUE], use.names = FALSE)))
      row_vals <- row_vals[nzchar(row_vals) & !tolower(row_vals) %in% c("na", "nan", "none")]
      if (length(row_vals)) {
        cand <- parse_taaa_terms(row_vals[1])
        if (nrow(cand)) leader <- cand$term[1]
      }
    }
    if (is.na(leader) || !nzchar(leader)) leader <- td$term[which.max(td$freq)]

    # Ensure the representative theme exists as a node even when 主題名稱 is not
    # repeated in 主題詞頻.
    if (nzchar(leader) && !(leader %in% td$term)) {
      td <- rbind(data.frame(term = leader, freq = max(td$freq, na.rm = TRUE), stringsAsFactors = FALSE), td)
      td <- aggregate(freq ~ term, td, sum)
      td <- td[order(-td$freq, td$term), , drop = FALSE]
    }

    rr <- rr + 1L
    node_rows[[rr]] <- data.frame(term = td$term, freq = td$freq, cluster = as.character(row_cluster), row_id = i, stringsAsFactors = FALSE)

    if (nrow(td) >= 2) {
      cb <- utils::combn(seq_len(nrow(td)), 2)
      tmp <- data.frame(
        Leader = td$term[cb[1,]],
        follower = td$term[cb[2,]],
        WCD = pmin(td$freq[cb[1,]], td$freq[cb[2,]]),
        stringsAsFactors = FALSE
      )
      ee <- ee + 1L
      full_edges[[ee]] <- tmp

      fol <- td[td$term != leader, , drop = FALSE]
      if (nrow(fol)) {
        oo <- oo + 1L
        one_edges[[oo]] <- data.frame(
          Leader = leader,
          follower = fol$term,
          WCD = fol$freq,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (!length(node_rows)) {
    stop("The row-wise TAAA/coword CSV contained no usable term rows after parsing.", call. = FALSE)
  }
  nr <- do.call(rbind, node_rows)
  nodes <- aggregate(freq ~ term, nr, sum)
  names(nodes) <- c("name", "value")
  occ <- aggregate(row_id ~ term, nr, function(x) length(unique(x)))
  names(occ) <- c("name", "value2")
  nodes <- merge(nodes, occ, by = "name", all.x = TRUE, sort = FALSE)

  # A1-A8 coword rows usually have no Cluster column.  Using aggregate()
  # with an all-NA cluster field causes the Chinese R error
  # "沒有可用來聚合的列" (no rows available to aggregate).  Therefore, only
  # compute uploaded carac/cluster labels when at least one usable cluster value
  # is present; otherwise leave carac as NA so the ordinary FLCA pipeline can
  # compute clusters from the graph.
  cl_src <- nr
  cl_src$cluster <- trimws(as_utf8(cl_src$cluster))
  usable_cluster <- nzchar(cl_src$cluster) & !tolower(cl_src$cluster) %in% c("na", "nan", "none", "unclassified")
  if (any(usable_cluster, na.rm = TRUE)) {
    cl_src <- cl_src[usable_cluster, , drop = FALSE]
    if (nrow(cl_src) > 0) {
      cl <- aggregate(freq ~ term + cluster, cl_src, sum)
      car <- vapply(split(cl, cl$term), function(d) weighted_mode_cluster(d$cluster, d$freq), integer(1))
      nodes$carac <- suppressWarnings(as.integer(car[nodes$name]))
    } else {
      nodes$carac <- NA_integer_
    }
  } else {
    nodes$carac <- NA_integer_
  }
  nodes <- nodes[order(-nodes$value, -nodes$value2, nodes$name), , drop = FALSE]
  selected <- nodes$name[seq_len(min(top_n_nodes, nrow(nodes)))]
  nodes <- nodes[nodes$name %in% selected, , drop = FALSE]

  full <- if (length(full_edges)) do.call(rbind, full_edges) else data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE)
  one <- if (length(one_edges)) do.call(rbind, one_edges) else data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE)

  keep_edges <- function(ed) {
    if (!nrow(ed)) return(ed)
    ed$Leader <- as.character(ed$Leader); ed$follower <- as.character(ed$follower)
    ed$WCD <- suppressWarnings(as.numeric(ed$WCD)); ed$WCD[!is.finite(ed$WCD) | ed$WCD <= 0] <- 1
    ed <- ed[ed$Leader %in% selected & ed$follower %in% selected & ed$Leader != ed$follower, , drop = FALSE]
    if (!nrow(ed)) return(ed)
    ed <- aggregate(WCD ~ Leader + follower, ed, sum)
    ed[order(-ed$WCD, ed$Leader, ed$follower), , drop = FALSE]
  }
  full <- keep_edges(full)
  one <- keep_edges(one)
  if (!nrow(full)) stop("TAAA ToT row-wise parsing produced nodes but no full co-occurrence edges. Each usable row needs at least two terms.", call. = FALSE)
  if (!nrow(one)) one <- full

  list(
    nodes = nodes[, c("name", "value", "value2", "carac"), drop = FALSE],
    # Retain the original co-word full-edge network as the main app input.
    # This keeps Data / Full Network / FLCA Reduced behavior unchanged.
    edges = full,
    # Store TAAA ToT networks as ADDITIONAL FLCA Process-only outputs.
    # Do not pass the one-edge table as edges_reduced here, otherwise it replaces
    # the ordinary FLCA reduced graph throughout the app.
    taaa_tot = TRUE,
    taaa_tot_edges_full = full,
    taaa_tot_edges_one = one,
    data_mode = paste0("row-wise coword CSV; original coword network retained; additional TAAA ToT full edges=", nrow(full), "; TAAA ToT one-edge leader/follower edges=", nrow(one), "; top ", nrow(nodes), " nodes")
  )
}

build_coword_from_df <- function(df, top_n_nodes = 100) {
  df <- safe_df(df)
  if (nrow(df) == 0 || ncol(df) < 2) stop("Coword CSV must contain at least two columns.", call. = FALSE)
  top_n_nodes <- suppressWarnings(as.integer(top_n_nodes))
  if (!is.finite(top_n_nodes) || top_n_nodes < 2) top_n_nodes <- 100L

  # Row-wise format: one article row contains a term list such as
  # 主題詞頻 = "Needles(4), Collagen(4), COVID-19(1)" and often 主題名稱 / Cluster.
  # The original co-word full-edge network is retained as `edges` for the normal
  # Data / Full Network / FLCA Reduced tabs. TAAA ToT full-edge and one-edge
  # tables are attached only as extra FLCA Process outputs.
  if (is_rowwise_taaa_coword(df)) {
    return(build_taaa_tot_from_rows(df, top_n_nodes = top_n_nodes))
  }

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
    # Missing WCD in a two-column edge list defaults to 1.0.
    return(list(nodes = generate_nodes(ed), edges = ed, data_mode = paste0("edge-list CSV: ", basename(path))))
  }
  out <- build_coword_from_df(df, top_n_nodes = top_n_nodes)
  out$data_mode <- paste0(out$data_mode, ": ", basename(path))
  out
}

taipei_mrt_default_inputs <- function(base_dir = getwd()) {
  edge_file <- file.path(base_dir, "TaipeiMRT.csv")
  volume_file <- file.path(base_dir, "TaipeiMRT_daily_average_2026_07.csv")
  if (!file.exists(edge_file) || !file.exists(volume_file))
    stop("TaipeiMRT.csv and TaipeiMRT_daily_average_2026_07.csv must be beside app.R.", call. = FALSE)
  dat <- csv_to_nodes_edges(edge_file)
  volume <- utils::read.csv(text = readLines(volume_file, encoding = "UTF-8"),
                            encoding = "UTF-8", stringsAsFactors = FALSE)
  if (!identical(names(volume), c("name", "value")) ||
      anyDuplicated(volume$name) || any(!is.finite(volume$value)))
    stop("Invalid Taipei MRT daily-average snapshot.", call. = FALSE)
  nodes <- dat$nodes[order(dat$nodes$name, method = "radix"),
                     c("name", "value", "value2"), drop = FALSE]
  hit <- match(nodes$name, volume$name)
  nodes$value[!is.na(hit)] <- volume$value[hit[!is.na(hit)]]
  edges <- dat$edges
  list(
    nodes = paste(c("name,value,value2",
                    paste(nodes$name, nodes$value, nodes$value2, sep = ",")), collapse = "\n"),
    edges = paste(c("follower,leader,WCD",
                    paste(edges$follower, edges$Leader, edges$WCD, sep = ",")), collapse = "\n")
  )
}

read_us_airports_demo <- function(base_dir = getwd()) {
  node_file <- file.path(base_dir, "us_domestic_air_nodes.csv")
  edge_file <- file.path(base_dir, "us_domestic_air_edges.csv")
  if (!file.exists(node_file) || !file.exists(edge_file))
    stop("The bundled U.S. airports nodes and edges CSV files must be beside app.R.", call. = FALSE)
  nodes <- read_csv_file_safe(node_file)
  edges <- read_csv_file_safe(edge_file)
  if (!all(c("name", "value") %in% names(nodes)) ||
      !all(c("Leader", "Follower", "WCD") %in% names(edges)))
    stop("Invalid U.S. airports demo columns.", call. = FALSE)
  nodes$name <- trimws(as_utf8(nodes$name))
  nodes$value <- suppressWarnings(as.numeric(nodes$value))
  if (anyNA(nodes$name) || any(!nzchar(nodes$name)) || anyDuplicated(nodes$name) ||
      any(!is.finite(nodes$value)))
    stop("Invalid U.S. airports demo nodes.", call. = FALSE)
  if ("value2" %in% names(nodes)) {
    nodes$value2 <- suppressWarnings(as.numeric(nodes$value2))
    if (any(!is.finite(nodes$value2)))
      stop("Invalid U.S. airports demo value2.", call. = FALSE)
  }
  edges$Leader <- trimws(as_utf8(edges$Leader))
  edges$Follower <- trimws(as_utf8(edges$Follower))
  edges$WCD <- suppressWarnings(as.numeric(edges$WCD))
  if (anyNA(edges$Leader) || anyNA(edges$Follower) ||
      any(!is.finite(edges$WCD)) || any(edges$WCD <= 0) ||
      any(!edges$Leader %in% nodes$name) || any(!edges$Follower %in% nodes$name))
    stop("Invalid U.S. airports demo routes or WCD values.", call. = FALSE)
  list(nodes = nodes[, intersect(c("name", "value", "value2"), names(nodes)), drop = FALSE],
       edges = edges[, c("Leader", "Follower", "WCD"), drop = FALSE],
       data_mode = "U.S. domestic airports demo from bundled nodes and route edges CSVs")
}

read_taipei_mrt_pasted_input <- function(node_text, edge_text, use_nodes = TRUE) {
  raw_edges <- read_app_pasted_table(edge_text, "edge")
  edges <- clean_edges(raw_edges)
  nodes <- if (isTRUE(use_nodes)) read_app_pasted_table(node_text, "node") else NULL
  list(nodes = nodes, edges = edges,
       data_mode = if (isTRUE(use_nodes))
         "Taipei MRT pasted edges and node values" else
         "Taipei MRT pasted edges; node values derived from edges")
}

read_app_pasted_table <- function(value, label) {
  value <- as.character(value %||% "")
  Encoding(value) <- "UTF-8"
  value <- trimws(value)
  if (!nzchar(value)) stop("Paste ", label, " data first.", call. = FALSE)
  first <- strsplit(value, "\n", fixed = TRUE)[[1L]][1L]
  if (grepl(",", first, fixed = TRUE))
    return(utils::read.csv(text = value, check.names = FALSE,
                           stringsAsFactors = FALSE))
  lines <- trimws(strsplit(value, "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(lines) & !grepl("^#", lines)]
  header <- strsplit(lines[1L], "[[:space:]]+")[[1L]]
  rows <- strsplit(lines[-1L], "[[:space:]]+")
  if (!length(rows)) stop("Paste at least one ", label, " row.", call. = FALSE)
  width <- max(lengths(rows), length(header))
  if (width > 3L || width < 2L || any(lengths(rows) < 2L))
    stop("Each pasted ", label, " row needs two or three fields.", call. = FALSE)
  if (length(header) < width)
    header <- c(header, if (identical(label, "edge")) "WCD" else "value2")
  mat <- do.call(rbind, lapply(rows, function(row)
    c(row, rep(NA_character_, width - length(row)))))
  out <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(out) <- header
  out
}

# Hospital time series determine edge strength by Lin's concordance correlation.
nurseratio_github_url <- "https://github.com/smilechien/clusterchoice/blob/main/nurseratio.csv"
nurseratio_raw_url <- "https://raw.githubusercontent.com/smilechien/clusterchoice/main/nurseratio.csv"

read_nurseratio_ccc <- function(path) {
  if (!file.exists(path)) stop("Nurse ratios CSV was not found: ", path, call. = FALSE)
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  csv_text <- rawToChar(bytes)
  utf8_text <- iconv(csv_text, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  if (is.na(utf8_text))
    utf8_text <- iconv(csv_text, from = "CP950", to = "UTF-8", sub = NA_character_)
  if (is.na(utf8_text))
    stop("Nurse ratios CSV must be encoded as UTF-8 or CP950/Big5.", call. = FALSE)
  utf8_text <- sub("^\ufeff", "", utf8_text)
  df <- utils::read.csv(text = utf8_text, check.names = FALSE,
                        stringsAsFactors = FALSE)
  months <- as.character(seq_len(12))
  period_cols <- names(df)[-1L]
  period_cols <- period_cols[!grepl("^[[:space:]]*profile[[:punct:][:space:]]*$",
                                    period_cols, ignore.case = TRUE)]
  if (length(period_cols) < 2L)
    stop("Nurse ratios CSV needs a hospital-name column and at least two observation columns.", call. = FALSE)
  hospital <- trimws(as.character(df[[1]]))
  keep <- !is.na(hospital) & nzchar(hospital)
  df <- df[keep, , drop = FALSE]
  hospital <- hospital[keep]
  if (anyDuplicated(hospital)) stop("Hospital names must be unique.", call. = FALSE)
  x <- as.matrix(data.frame(lapply(df[period_cols], function(z) suppressWarnings(as.numeric(z))),
                            check.names = FALSE))
  rownames(x) <- hospital
  invalid <- which(!is.finite(x), arr.ind = TRUE)
  if (nrow(invalid))
    stop("Nonnumeric or missing observation in column ", period_cols[invalid[1, 2]],
         " for ", hospital[invalid[1, 1]], ".", call. = FALSE)
  if (nrow(x) < 2) stop("At least two hospitals are required.", call. = FALSE)
  pairs <- utils::combn(seq_len(nrow(x)), 2)
  ccc <- apply(pairs, 2, function(ij) {
    a <- x[ij[1], ]; b <- x[ij[2], ]
    ma <- mean(a); mb <- mean(b)
    den <- mean((a - ma)^2) + mean((b - mb)^2) + (ma - mb)^2
    if (den <= 0) return(NA_real_)
    2 * mean((a - ma) * (b - mb)) / den
  })
  positive <- is.finite(ccc) & ccc > 0
  if (!any(positive)) stop("No positive Lin's CCC links were found.", call. = FALSE)
  edges <- data.frame(Leader = hospital[pairs[1, positive]],
                      follower = hospital[pairs[2, positive]],
                      WCD = ccc[positive], stringsAsFactors = FALSE)
  nodes <- data.frame(name = hospital, value = rowMeans(x),
                      value2 = rowMeans(x), carac = NA_integer_,
                      stringsAsFactors = FALSE)
  period_label <- if (identical(period_cols, months)) "12-month" else if (all(grepl("^[0-9]{2,4}Q[1-4]$", period_cols))) paste0(length(period_cols), "-quarter") else paste0(length(period_cols), "-period")
  list(nodes = nodes, edges = edges,
       value_matrix = t(x),
       data_mode = paste0("nurse ratios CSV: ", period_label, " Lin's CCC; Profile ignored; FLCA clusters computed; Step 2 positive pairwise CCC; Step 3 each hospital's maximum CCC"),
       nurseratio_ccc = TRUE, max_per_cluster = 20L)
}

# GSM column headers are node labels. Genes within a log10P group are the
# matched observations used for Lin's CCC between GSM columns.
read_genecluster_ccc_data <- function(path) {
  if (!file.exists(path)) stop("Gene cluster CSV was not found: ", path, call. = FALSE)
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8-BOM")
  required <- c("Gene", "logFC", "log10P")
  if (!all(required %in% names(df)) || ncol(df) < 9L)
    stop("Gene cluster CSV needs Gene, logFC, log10P, at least two observation columns, and four trailing summary columns.", call. = FALSE)
  if (!identical(names(df)[1:3], required))
    stop("Gene cluster CSV columns must begin with Gene, logFC, log10P.", call. = FALSE)
  value_cols <- names(df)[seq.int(4L, ncol(df) - 4L)]
  if (anyDuplicated(value_cols) || length(value_cols) < 2L)
    stop("Gene observation column names must be unique.", call. = FALSE)
  group <- suppressWarnings(as.numeric(df$log10P))
  if (any(!is.finite(group))) stop("log10P must contain numeric group labels.", call. = FALSE)
  list(df = df, groups = sort(unique(group)), value_cols = value_cols)
}

genecluster_sample_condition_default <- paste(
  c("SampleID\tCondition",
    paste0("GSM", 207929:208011, "\t",
           ifelse(207929:208011 <= 207959, "Control", "Case"))),
  collapse = "\n"
)

resolve_genecluster_ccc_columns <- function(data, specification) {
  specification <- as.character(specification)[1L]
  if (is.na(specification) || !nzchar(trimws(specification)))
    stop("Enter a tab-separated SampleID and Condition table.", call. = FALSE)
  lines <- strsplit(gsub("\r\n?", "\n", specification), "\n", fixed = TRUE)[[1L]]
  lines <- lines[nzchar(trimws(lines))]
  header <- trimws(strsplit(lines[1L], "\t", fixed = TRUE)[[1L]])
  if (!identical(header, c("SampleID", "Condition")))
    stop("The first row must be SampleID<TAB>Condition.", call. = FALSE)
  rows <- strsplit(lines[-1L], "\t", fixed = TRUE)
  if (!length(rows) || any(lengths(rows) != 2L))
    stop("Each sample row needs one SampleID and one Condition separated by a tab.", call. = FALSE)
  sample_id <- trimws(vapply(rows, `[[`, character(1), 1L))
  condition <- trimws(vapply(rows, `[[`, character(1), 2L))
  if (any(!nzchar(sample_id)) || anyDuplicated(sample_id) ||
      any(!sample_id %in% data$value_cols))
    stop("SampleID values must be unique observation headers between log10P and the final four summary columns.", call. = FALSE)
  condition <- ifelse(tolower(condition) == "control", "Control",
                      ifelse(tolower(condition) == "case", "Case", NA_character_))
  if (anyNA(condition))
    stop("Condition must be Control or Case for every SampleID.", call. = FALSE)
  if (length(sample_id) < 20L)
    stop("List at least 20 SampleID rows to build a Top-20 GSM network.", call. = FALSE)
  data.frame(SampleID = sample_id, Condition = condition, stringsAsFactors = FALSE)
}

make_genecluster_ccc_group <- function(data, group, top_n = 20L,
                                       value_columns = data$value_cols,
                                       sample_conditions = NULL) {
  group <- suppressWarnings(as.numeric(group))[1]
  if (!is.finite(group) || !group %in% data$groups)
    stop("Select an available log10P group.", call. = FALSE)
  df <- data$df[as.numeric(data$df$log10P) == group, , drop = FALSE]
  x <- as.matrix(data.frame(lapply(df[value_columns], function(z) suppressWarnings(as.numeric(z))),
                            check.names = FALSE))
  if (any(!is.finite(x)))
    stop("The selected log10P group has a missing or nonnumeric observation.", call. = FALSE)
  if (nrow(x) < 2L) stop("At least two genes are required in each log10P group.", call. = FALSE)
  x <- t(x)
  labels <- rownames(x)
  # Keep the same value definition as nurse-ratio mode: mean observation.
  # Ties are resolved by GSM column label so the Top-20 is reproducible.
  rank <- order(-rowMeans(x), labels)
  keep <- rank[seq_len(min(as.integer(top_n), length(rank)))]
  x <- x[keep, , drop = FALSE]
  labels <- labels[keep]
  pairs <- utils::combn(seq_along(labels), 2L)
  ccc <- apply(pairs, 2L, function(ij) {
    a <- x[ij[1L], ]; b <- x[ij[2L], ]
    ma <- mean(a); mb <- mean(b)
    den <- mean((a - ma)^2) + mean((b - mb)^2) + (ma - mb)^2
    if (den <= 0) return(NA_real_)
    2 * mean((a - ma) * (b - mb)) / den
  })
  positive <- is.finite(ccc) & ccc > 0
  if (!any(positive)) stop("No positive Lin's CCC links were found in log10P group ", group, ".", call. = FALSE)
  edges <- data.frame(Leader = labels[pairs[1L, positive]],
                      follower = labels[pairs[2L, positive]], WCD = ccc[positive],
                      stringsAsFactors = FALSE)
  nodes <- data.frame(name = labels, value = rowMeans(x), value2 = rowMeans(x),
                      carac = NA_integer_, stringsAsFactors = FALSE)
  if (!is.null(sample_conditions))
    nodes$Condition <- unname(sample_conditions[labels])
  list(nodes = nodes, edges = edges, nurseratio_ccc = TRUE,
       value_matrix = t(x),
       genecluster_ccc = TRUE, gene_group = group, max_per_cluster = 20L,
       data_mode = paste0("genecluster5.csv: log10P group ", group,
                          "; Top ", length(labels), " GSM columns by mean across ", ncol(x),
                          " genes; ", length(value_columns), " validated observation columns; four trailing columns excluded; Lin's CCC + FLCA"))
}

# Companion view: each gene row is a node, with the selected SampleID columns
# acting as paired observations. This is kept separate from the GSM-node view.
make_genecluster_gene_ccc_group <- function(data, group, value_columns,
                                            top_n = 20L) {
  group <- suppressWarnings(as.numeric(group))[1L]
  if (!is.finite(group) || !group %in% data$groups)
    stop("Select an available log10P group.", call. = FALSE)
  df <- data$df[as.numeric(data$df$log10P) == group, , drop = FALSE]
  genes <- trimws(as.character(df$Gene))
  if (anyNA(genes) || any(!nzchar(genes)) || anyDuplicated(genes))
    stop("Gene names within a log10P group must be present and unique.", call. = FALSE)
  x <- as.matrix(data.frame(lapply(df[value_columns], function(z) suppressWarnings(as.numeric(z))),
                            check.names = FALSE))
  if (any(!is.finite(x)))
    stop("The selected log10P group has a missing or nonnumeric observation.", call. = FALSE)
  if (nrow(x) < 2L || ncol(x) < 2L)
    stop("Gene-row CCC needs at least two genes and two sample columns.", call. = FALSE)
  keep <- order(-rowMeans(x), genes)[seq_len(min(as.integer(top_n), nrow(x)))]
  x <- x[keep, , drop = FALSE]
  genes <- genes[keep]
  rownames(x) <- genes
  pairs <- utils::combn(seq_along(genes), 2L)
  ccc <- apply(pairs, 2L, function(ij) {
    a <- x[ij[1L], ]; b <- x[ij[2L], ]
    ma <- mean(a); mb <- mean(b)
    den <- mean((a - ma)^2) + mean((b - mb)^2) + (ma - mb)^2
    if (den <= 0) return(NA_real_)
    2 * mean((a - ma) * (b - mb)) / den
  })
  positive <- is.finite(ccc) & ccc > 0
  if (!any(positive))
    stop("No positive gene-row Lin's CCC links in log10P group ", group, ".", call. = FALSE)
  list(
    nodes = data.frame(name = genes, value = rowMeans(x), value2 = rowMeans(x),
                       carac = NA_integer_, stringsAsFactors = FALSE),
    value_matrix = t(x),
    edges = data.frame(Leader = genes[pairs[1L, positive]],
                       follower = genes[pairs[2L, positive]], WCD = ccc[positive],
                       stringsAsFactors = FALSE),
    gene_group = group, max_per_cluster = 20L,
    data_mode = paste0("gene-row Lin's CCC: log10P group ", group,
                       "; Top ", length(genes), " genes by mean across ",
                       length(value_columns), " validated sample columns")
  )
}

# Convert a GitHub file-page link into a direct CSV download URL.
github_csv_raw_url <- function(source_url) {
  source_url <- trimws(as.character(source_url)[1L])
  if (is.na(source_url) || !nzchar(source_url)) stop("Provide a GitHub CSV URL.", call. = FALSE)
  source_url <- sub("#.*$", "", source_url)
  blob <- regexec("^https://github\\.com/([^/]+)/([^/]+)/blob/([^/]+)/(.+\\.csv)(?:\\?.*)?$", source_url, perl = TRUE)
  parts <- regmatches(source_url, blob)[[1L]]
  if (length(parts)) return(paste0("https://raw.githubusercontent.com/", parts[2L], "/", parts[3L], "/", parts[4L], "/", parts[5L]))
  if (grepl("^https://raw\\.githubusercontent\\.com/[^/]+/[^/]+/[^/]+/.+\\.csv(?:\\?.*)?$", source_url, perl = TRUE)) return(source_url)
  stop("Use an HTTPS GitHub CSV link, either github.com/.../blob/.../file.csv or raw.githubusercontent.com/.../file.csv.", call. = FALSE)
}

read_demo_nurseratio_ccc <- function(source_url = NULL) {
  explicit_url <- !is.null(source_url) && nzchar(trimws(as.character(source_url)[1L]))
  remote_url <- if (explicit_url) github_csv_raw_url(source_url) else nurseratio_raw_url
  downloaded <- tempfile(fileext = ".csv")
  on.exit(unlink(downloaded), add = TRUE)
  previous_timeout <- getOption("timeout"); options(timeout = 15)
  on.exit(options(timeout = previous_timeout), add = TRUE)
  remote <- tryCatch({
    status <- suppressWarnings(utils::download.file(remote_url, downloaded, mode = "wb", quiet = TRUE, method = "libcurl"))
    if (!identical(status, 0L) || !file.exists(downloaded) || file.size(downloaded) == 0) stop("The GitHub CSV could not be downloaded.", call. = FALSE)
    read_nurseratio_ccc(downloaded)
  }, error = function(e) e)
  if (!inherits(remote, "error")) {
    remote$data_mode <- paste0(remote$data_mode, "; source: ", remote_url)
    return(remote)
  }
  if (explicit_url) stop("The requested online nurse-ratio CSV could not be loaded: ", conditionMessage(remote), call. = FALSE)
  local_path <- file.path(getwd(), "nurseratio.csv")
  if (!file.exists(local_path)) stop("The GitHub nurse ratios CSV could not be loaded (", conditionMessage(remote), "), and nurseratio.csv was not found beside app.R.", call. = FALSE)
  local <- read_nurseratio_ccc(local_path)
  local$data_mode <- paste0(local$data_mode, "; source: bundled CSV (GitHub unavailable)")
  local$online_details <- paste0("GitHub source unavailable: ", conditionMessage(remote), ". Used bundled nurseratio.csv.")
  local
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

# Direct each edge from the higher-value candidate leader to its follower,
# then retain only the strongest incoming link for each follower.
select_flca_follower_links <- function(edges, nodes, node_tie = FALSE) {
  if (!nrow(edges)) {
    return(data.frame(Leader = character(0), follower = character(0),
                      WCD = numeric(0), stringsAsFactors = FALSE))
  }
  values <- stats::setNames(as.numeric(nodes$value), as.character(nodes$name))
  a <- as.character(edges$Leader)
  b <- as.character(edges$follower)
  va <- values[a]
  vb <- values[b]
  va[!is.finite(va)] <- 0
  vb[!is.finite(vb)] <- 0
  # Radix ranks provide a stable name tie-break even on Windows installations
  # whose locale cannot compare some UTF-8 hospital names.
  labels <- sort(unique(c(a, b)), method = "radix")
  label_rank <- stats::setNames(seq_along(labels), labels)
  a_leads <- va > vb | (va == vb & label_rank[a] < label_rank[b])
  candidate <- data.frame(
    Leader = ifelse(a_leads, a, b),
    follower = ifelse(a_leads, b, a),
    WCD = as.numeric(edges$WCD),
    stringsAsFactors = FALSE
  )
  candidate$leader_value <- unname(values[candidate$Leader])
  candidate$leader_value[!is.finite(candidate$leader_value)] <- 0
  if (isTRUE(node_tie)) {
    candidate <- candidate[order(candidate$follower, -candidate$WCD,
                                 -candidate$leader_value, candidate$Leader), , drop = FALSE]
  } else {
    candidate <- candidate[order(candidate$follower, -candidate$WCD,
                                 candidate$Leader), , drop = FALSE]
  }
  candidate <- candidate[!duplicated(candidate$follower),
                         c("Leader", "follower", "WCD"), drop = FALSE]
  rownames(candidate) <- NULL
  candidate
}

# A junction with two or more direct followers starts its own FLCA cluster.
# Followers inherit the nearest junction leader above them; graph connectivity
# alone does not define FLCA membership. Optional min_members only adjusts
# displayed junction groups, preserving every strongest-link parent edge.
junction_flca_membership <- function(nodes, edges, min_members = 1L) {
  labels <- as.character(nodes$name)
  if (!length(labels)) return(stats::setNames(integer(0), character(0)))
  min_members <- suppressWarnings(as.integer(min_members))[1L]
  if (!is.finite(min_members) || min_members < 1L)
    stop("min_members must be a positive integer.", call. = FALSE)
  ed <- edges[edges$Leader %in% labels & edges$follower %in% labels &
                edges$Leader != edges$follower, , drop = FALSE]
  if (anyDuplicated(ed$follower)) ed <- select_flca_follower_links(ed, nodes)
  parent <- stats::setNames(as.character(ed$Leader), as.character(ed$follower))
  child_count <- table(ed$Leader)
  junctions <- names(child_count[child_count >= 2L])
  roots <- setdiff(labels, ed$follower)
  leaders <- union(junctions, roots)
  chosen <- vapply(labels, function(name) {
    current <- name
    seen <- character(0)
    while (!(current %in% leaders) && current %in% names(parent) &&
           !(current %in% seen)) {
      seen <- c(seen, current)
      current <- unname(parent[[current]])
    }
    if (current %in% leaders) current else name
  }, character(1))
  if (min_members > 1L) {
    depth <- function(name) {
      current <- name
      seen <- character(0)
      level <- 0L
      while (current %in% names(parent)) {
        if (current %in% seen)
          stop("Cycle in the FLCA strongest-link parent tree.", call. = FALSE)
        seen <- c(seen, current)
        current <- unname(parent[[current]])
        level <- level + 1L
      }
      level
    }
    seeds <- unique(chosen)
    values <- stats::setNames(as.numeric(nodes$value), labels)
    edge_weight <- stats::setNames(as.numeric(ed$WCD),
                                   as.character(ed$follower))
    seeds <- seeds[order(vapply(seeds, depth, integer(1)),
                         -values[seeds], seeds, method = "radix")]
    for (seed in seeds) {
      if (!sum(chosen == seed) || sum(chosen == seed) >= min_members ||
          !(seed %in% names(parent))) next
      repeat {
        if (sum(chosen == seed) >= min_members) break
        children <- seeds[vapply(seeds, function(child) {
          if (child == seed || !sum(chosen == child) ||
              !(child %in% names(parent))) return(FALSE)
          ancestor <- unname(parent[[child]])
          target <- chosen[match(ancestor, labels)]
          length(target) && !is.na(target) && target == seed
        }, logical(1))]
        if (!length(children)) break
        child <- children[order(-edge_weight[children],
                                -values[children], children,
                                method = "radix")[1L]]
        chosen[chosen == child] <- seed
      }
      if (sum(chosen == seed) >= min_members) next
      ancestor <- unname(parent[[seed]])
      target <- chosen[match(ancestor, labels)]
      if (length(target) && !is.na(target) && target != seed)
        chosen[chosen == seed] <- target
    }
    final_seeds <- unique(chosen)
    undersized <- final_seeds[
      vapply(final_seeds, function(seed)
        seed %in% names(parent) && sum(chosen == seed) < min_members,
        logical(1))]
    if (length(undersized))
      stop("Undersized non-root FLCA display clusters remain: ",
           paste(undersized, collapse = ", "), call. = FALSE)
  }
  leader_names <- unique(chosen)
  values <- stats::setNames(as.numeric(nodes$value), labels)
  leader_values <- unname(values[leader_names])
  leader_values[!is.finite(leader_values)] <- 0
  leader_names <- leader_names[order(-leader_values, leader_names, method = "radix")]
  stats::setNames(match(chosen, leader_names), labels)
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
  red <- select_flca_follower_links(ed, verts, node_tie = FALSE)
  gf <- igraph::graph_from_data_frame(red, directed = FALSE, vertices = verts)
  if (igraph::ecount(gf) > 0) igraph::E(gf)$weight <- pmax(as.numeric(igraph::E(gf)$WCD), 1e-6)
  gf <- igraph::simplify(gf, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = "sum", WCD = "sum"))
  # Node mode differs only in ties: prefer the higher-value candidate leader.
  node_edges <- select_flca_follower_links(ed, verts, node_tie = TRUE)
  gn <- igraph::graph_from_data_frame(node_edges, directed = FALSE, vertices = verts)
  if (igraph::ecount(gn)) igraph::E(gn)$weight <- as.numeric(igraph::E(gn)$WCD)
  list(nodes = nd, edges = ed, vertex_df = verts, g_full = g, g_flca = gf,
       g_flca_node = gn, edges_reduced = red, edges_node = node_edges)
}

norm_mem <- function(x, n) {
  x <- suppressWarnings(as.integer(x))
  if (length(x) != n) x <- rep(1L, n)
  x[is.na(x)] <- 1L
  x
}

# Shared cluster-color map used by Network, SSplot-compatible panels, Sankey, and chord.
# This fixes the earlier mismatch where visNetwork used Set2/rainbow, static
# network used Dark 3, but SSplot used Set3.  Cluster ID 1, 2, ..., 12 now
# receives the same color family across panels.
make_cluster_color_map <- function(clusters, palette = "Set3") {
  z <- suppressWarnings(as.integer(as.character(clusters)))
  if (all(is.na(z))) {
    lev <- sort(unique(as.character(clusters)))
  } else {
    lev <- as.character(sort(unique(z[!is.na(z)])))
  }
  if (!length(lev)) return(stats::setNames(character(0), character(0)))
  pal <- grDevices::hcl.colors(max(1, length(lev)), palette)
  stats::setNames(pal[seq_along(lev)], lev)
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

analyze_basic <- function(nodes, edges, target_k = 4, flca_mode = "edge", edges_reduced_override = NULL) {
  gr <- build_graphs(nodes, edges)

  # Optional override for special workflows that explicitly supply a reduced
  # edge table. Row-wise TAAA ToT no longer uses this override, because its
  # one-edge graph is an additional FLCA Process-only output and must not replace
  # the ordinary FLCA reduced graph used by the rest of the app.
  if (!is.null(edges_reduced_override) && is.data.frame(edges_reduced_override) && nrow(edges_reduced_override) > 0) {
    er <- tryCatch(clean_edges(edges_reduced_override), error = function(e) NULL)
    if (!is.null(er) && nrow(er) > 0) {
      verts <- gr$vertex_df
      er <- er[er$Leader %in% verts$name & er$follower %in% verts$name & er$Leader != er$follower, , drop = FALSE]
      if (nrow(er) > 0) {
        er <- aggregate(WCD ~ Leader + follower, er, sum)
        gf2 <- igraph::graph_from_data_frame(er, directed = FALSE, vertices = verts)
        if (igraph::ecount(gf2) > 0) igraph::E(gf2)$weight <- pmax(as.numeric(igraph::E(gf2)$WCD), 1e-6)
        gf2 <- igraph::simplify(gf2, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = "sum", WCD = "sum"))
        gr$edges_reduced <- er[order(-er$WCD, er$Leader, er$follower), , drop = FALSE]
        gr$g_flca <- gf2
      }
    }
  }

  g <- gr$g_full
  gf_edge <- gr$g_flca
  gf_node <- gr$g_flca_node
  # The selected mode controls displayed FLCA networks when no uploaded
  # membership is supplied. Both computed partitions are always retained.
  use_node <- identical(flca_mode, "node")
  gf <- if (use_node) gf_node else gf_edge
  n <- igraph::vcount(g)
  edge_red <- junction_flca_membership(gr$vertex_df, gr$edges_reduced)
  edge_red <- edge_red[igraph::V(gf_edge)$name]
  node_red <- junction_flca_membership(gr$vertex_df, gr$edges_node)
  node_red <- node_red[igraph::V(gf_node)$name]
  edge_full <- edge_red[match(igraph::V(g)$name, names(edge_red))]
  node_full <- node_red[match(igraph::V(g)$name, names(node_red))]
  names(edge_full) <- names(node_full) <- igraph::V(g)$name
  selected_full <- if (use_node) node_full else edge_full
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
    fm_red <- if (use_node) node_red else edge_red
    fm <- norm_mem(fm_red[match(igraph::V(g)$name, names(fm_red))], n)
    names(fm) <- igraph::V(g)$name
    cluster_source <- if (use_node) "junction FLCA clusters with node-value tie-break" else "junction FLCA clusters from strongest follower links"
    original_clusters_used <- FALSE
  }
  mems$FLCA <- as.integer(fm)
  mems$FLCA_edge <- as.integer(edge_full)
  mems$FLCA_node_tie <- as.integer(node_full)
  # Comparisons use the same full-network node order, regardless of which
  # graph was used to calculate each FLCA partition.
  quality_row <- function(method, mm, eval_g, eval_graph, status_value = "ok", details_value = "") {
    data.frame(method = method, n_nodes = igraph::vcount(eval_g),
               n_clusters = length(unique(mm)),
               modularity = mod_safe(eval_g, mm), mean_silhouette = sil_safe(eval_g, mm),
               ARI_vs_FLCA = cmp_safe(mm, selected_full, "adjusted.rand"),
               NMI_vs_FLCA = cmp_safe(mm, selected_full, "nmi"),
               ARI_vs_FLCA_edge = cmp_safe(mm, edge_full, "adjusted.rand"),
               NMI_vs_FLCA_edge = cmp_safe(mm, edge_full, "nmi"),
               ARI_vs_FLCA_node_tie = cmp_safe(mm, node_full, "adjusted.rand"),
               NMI_vs_FLCA_node_tie = cmp_safe(mm, node_full, "nmi"),
               status = status_value, details = details_value,
               eval_graph = eval_graph, stringsAsFactors = FALSE)
  }
  qrows <- lapply(methods, function(m) quality_row(m, mems[[m]], g, "full", status[m], details[m]))
  qdf <- do.call(rbind, qrows)
  edge_single <- quality_row("FLCA edge weight (single)", edge_full, gf_edge, "single")
  node_single <- quality_row("FLCA edge + node tie (single)", node_full, gf_node, "single")
  extra <- rbind(edge_single, node_single,
                 quality_row("FLCA edge weight (full)", edge_full, g, "full"),
                 quality_row("FLCA edge + node tie (full)", node_full, g, "full"),
                 quality_row(paste0("components_independent_k", target_k),
                             mems$components, g, "thresholded_full"))
  qdf <- rbind(qdf, extra)
  ranking <- qdf[!grepl("^FLCA", qdf$method), , drop = FALSE]
  ranking <- ranking[order(-ranking$ARI_vs_FLCA, -ranking$NMI_vs_FLCA, -ranking$modularity), , drop = FALSE]
  list(nodes = gr$nodes, edges = gr$edges, vertex_df = gr$vertex_df, g_full = g, g_flca = gf,
       g_flca_edge = gf_edge, g_flca_node = gf_node,
       edges_reduced = if (use_node) gr$edges_node else gr$edges_reduced,
       memberships_df = mems, quality_df = qdf, ranking_df = ranking, flca_carac = fm_red, flca_full = fm,
       flca_mode = flca_mode, cluster_source = cluster_source, original_clusters_used = original_clusters_used,
       status_note = paste0("V36 TRUE REBUILD: ", cluster_source, "; dataset1.csv demo, Excel nodes/edges upload, CSV coword upload, real renderSSplot(82), red tabs, dynamic Kano.R Q-vs-SS table, node-level Kano under SSplot."))
}

# Fair Quality comparison: every method partitions the same FLCA-selected
# nodes, and every partition is scored on the same full induced edge network.
make_selected_quality_comparison <- function(core) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(1L)
  nd <- core$nodes
  ed <- core$edges
  g <- igraph::graph_from_data_frame(ed, directed = FALSE, vertices = nd)
  if (igraph::ecount(g)) igraph::E(g)$weight <- as.numeric(igraph::E(g)$WCD)
  methods <- c("louvain", "optimal", "components", "edge_betweenness",
               "label_prop", "infomap", "leading_eigen", "walktrap",
               "fast_greedy", "FLCA shared Top-50 selected")
  flca_mem <- nd$carac[match(igraph::V(g)$name, nd$name)]
  partition <- function(method) {
    w <- igraph::E(g)$weight
    switch(method,
      louvain = igraph::membership(igraph::cluster_louvain(g, weights = w)),
      components = igraph::components(g)$membership,
      edge_betweenness = igraph::membership(igraph::cluster_edge_betweenness(
        g, weights = if (length(w)) 1 / w else NULL, directed = FALSE)),
      label_prop = igraph::membership(igraph::cluster_label_prop(g, weights = w)),
      infomap = igraph::membership(igraph::cluster_infomap(g, e.weights = w)),
      leading_eigen = igraph::membership(igraph::cluster_leading_eigen(g, weights = w)),
      walktrap = igraph::membership(igraph::cluster_walktrap(g, weights = w)),
      fast_greedy = igraph::membership(igraph::cluster_fast_greedy(g, weights = w)),
      stop("Unsupported method"))
  }
  rows <- lapply(methods, function(method) {
    row <- data.frame(method = method, n_nodes = nrow(nd), n_clusters = NA_integer_,
                      modularity = NA_real_, mean_silhouette = NA_real_,
                      ARI_vs_FLCA = NA_real_, NMI_vs_FLCA = NA_real_,
                      ARI_vs_FLCA_edge = NA_real_, NMI_vs_FLCA_edge = NA_real_,
                      ARI_vs_FLCA_node_tie = NA_real_,
                      NMI_vs_FLCA_node_tie = NA_real_, status = "ok",
                      details = "Same FLCA-selected nodes and full induced edges",
                      eval_graph = "selected full induced", stringsAsFactors = FALSE)
    if (method == "optimal") {
      row$status <- "skipped"
      row$details <- "Optimal modularity skipped; exhaustive search can be impractical at 20-23 nodes"
      return(row)
    }
    mem <- if (method == "FLCA shared Top-50 selected") flca_mem else
      tryCatch(partition(method), error = function(e) e)
    if (inherits(mem, "error")) {
      row$status <- "failed"
      row$details <- conditionMessage(mem)
      return(row)
    }
    mem <- as.integer(mem)
    scored <- nd
    scored$carac <- mem[match(scored$name, igraph::V(g)$name)]
    quality <- flca_quality(scored, ed)
    row$n_clusters <- length(unique(mem))
    row$modularity <- quality$modularity$modularity_Q
    row$mean_silhouette <- quality$silhouette$mean_silhouette
    row$ARI_vs_FLCA <- row$ARI_vs_FLCA_edge <-
      cmp_safe(mem, flca_mem, "adjusted.rand")
    row$NMI_vs_FLCA <- row$NMI_vs_FLCA_edge <-
      cmp_safe(mem, flca_mem, "nmi")
    row
  })
  comparable <- do.call(rbind, rows)
  reference <- comparable[comparable$method == "FLCA shared Top-50 selected", , drop = FALSE]
  reduced_quality <- flca_quality(nd, core$tree_edges)
  reference$method <- "FLCA selected reduced tree (reference)"
  reference$modularity <- reduced_quality$modularity$modularity_Q
  reference$mean_silhouette <- reduced_quality$silhouette$mean_silhouette
  reference$status <- "reference"
  reference$details <- "Same selected nodes; strongest-link edges only; excluded from fair ranking"
  reference$eval_graph <- "selected reduced tree"
  rbind(comparable, reference)
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
  pal <- make_cluster_color_map(ug, "Set3")
  vals <- suppressWarnings(as.numeric(igraph::V(g)$value)); vals[!is.finite(vals)] <- 1
  nd <- data.frame(id = igraph::V(g)$name, label = igraph::V(g)$name, group = paste0("Cluster ", groups), value = sqrt(vals), color.background = unname(pal[groups]), stringsAsFactors = FALSE)
  nd$title <- paste0("Node: ", htmltools::htmlEscape(nd$label), "<br>", nd$group)
  if ("Condition" %in% igraph::vertex_attr_names(g)) {
    condition <- as.character(igraph::V(g)$Condition)
    nd$title <- paste0("SampleID: ", htmltools::htmlEscape(nd$label),
                       "<br>Condition: ", htmltools::htmlEscape(condition),
                       "<br>", nd$group)
  }
  edf <- igraph::as_data_frame(g, what = "edges")
  if (nrow(edf) > 0) {
    w <- if ("weight" %in% names(edf)) edf$weight else rep(1, nrow(edf))
    edv <- data.frame(from = edf$from, to = edf$to, value = pmax(1, log1p(w) * 2), title = paste("Weight:", sprintf("%.2f", w)), stringsAsFactors = FALSE)
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
  colmap <- make_cluster_color_map(lev, "Set3")
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


# ---- SSplot nearest-neighbor helper: never use the node itself --------------
# Returns one nearest non-self neighbor per node.  The first choice is the
# strongest directly connected node (largest edge weight/WCD).  If a node has no
# direct edge, the shortest weighted path is used.  If the graph is disconnected,
# the best same-cluster non-self node is used as a final display fallback.
ssplot_nearest_nonself_tbl <- function(g, mem, values = NULL) {
  n <- igraph::vcount(g)
  nms <- as.character(igraph::V(g)$name)
  if (is.null(nms) || any(!nzchar(nms))) nms <- as.character(seq_len(n))

  mem <- as.integer(mem)
  if (length(mem) != n) mem <- rep(1L, n)
  names(mem) <- nms

  values <- suppressWarnings(as.numeric(values))
  if (length(values) != n) values <- rep(1, n)
  values[!is.finite(values)] <- 1
  names(values) <- nms

  out_name <- rep(NA_character_, n)
  out_w    <- rep(NA_real_, n)

  if (n <= 1) {
    return(data.frame(
      name = nms,
      neighbor_name = out_name,
      neighborC = as.integer(NA),
      wsel = out_w,
      stringsAsFactors = FALSE
    ))
  }

  gu <- g
  if (igraph::is.directed(gu)) {
    gu <- igraph::as.undirected(
      gu,
      mode = "collapse",
      edge.attr.comb = list(weight = "sum", WCD = "sum", value = "sum", "ignore")
    )
  }

  ew <- if (igraph::ecount(gu) > 0) {
    if (!is.null(igraph::E(gu)$weight)) suppressWarnings(as.numeric(igraph::E(gu)$weight))
    else if (!is.null(igraph::E(gu)$WCD)) suppressWarnings(as.numeric(igraph::E(gu)$WCD))
    else rep(1, igraph::ecount(gu))
  } else numeric(0)
  if (length(ew) > 0) {
    ew[!is.finite(ew) | ew <= 0] <- 1e-6
    igraph::E(gu)$weight <- ew

    ee <- igraph::ends(gu, igraph::E(gu), names = TRUE)
    for (i in seq_len(n)) {
      nm <- nms[i]
      idx <- which(ee[, 1] == nm | ee[, 2] == nm)
      if (length(idx) > 0) {
        cand <- ifelse(ee[idx, 1] == nm, ee[idx, 2], ee[idx, 1])
        ok <- !is.na(cand) & nzchar(cand) & cand != nm
        cand <- cand[ok]
        ww <- ew[idx][ok]
        if (length(cand) > 0) {
          # collapse duplicate undirected edges by summed/maximum visible weight
          agg <- tapply(ww, cand, sum, na.rm = TRUE)
          cand2 <- names(agg)
          ord <- order(-as.numeric(agg), -values[cand2], cand2)
          out_name[i] <- cand2[ord[1]]
          out_w[i] <- as.numeric(agg[ord[1]])
        }
      }
    }

    # Shortest weighted-path fallback, still excluding self.
    miss <- which(is.na(out_name) | out_name == nms)
    if (length(miss) > 0) {
      D <- tryCatch(igraph::distances(gu, weights = 1 / igraph::E(gu)$weight),
                    error = function(e) NULL)
      if (is.matrix(D)) {
        diag(D) <- Inf
        for (i in miss) {
          d <- D[i, ]
          d[i] <- Inf
          ok <- is.finite(d) & names(d) != nms[i]
          if (any(ok)) {
            cand <- names(d)[ok]
            # distance first; for ties prefer same cluster, then higher value
            same_bonus <- as.integer(mem[cand] == mem[nms[i]])
            ord <- order(as.numeric(d[cand]), -same_bonus, -values[cand], cand)
            out_name[i] <- cand[ord[1]]
            out_w[i] <- if (is.finite(d[cand[ord[1]]]) && d[cand[ord[1]]] > 0) 1 / d[cand[ord[1]]] else NA_real_
          }
        }
      }
    }
  }

  # Disconnected fallback: same preserved cluster, non-self, highest value.
  miss <- which(is.na(out_name) | out_name == nms)
  for (i in miss) {
    cand <- which(mem == mem[i] & seq_len(n) != i)
    if (!length(cand)) cand <- setdiff(seq_len(n), i)
    if (length(cand)) {
      ord <- order(-values[cand], nms[cand])
      out_name[i] <- nms[cand[ord[1]]]
      out_w[i] <- NA_real_
    }
  }

  bad <- !is.na(out_name) & out_name == nms
  out_name[bad] <- NA_character_
  out_w[bad] <- NA_real_

  data.frame(
    name = nms,
    neighbor_name = out_name,
    neighborC = as.integer(unname(mem[out_name])),
    wsel = out_w,
    stringsAsFactors = FALSE
  )
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
  shiny::validate(shiny::need(!is.null(g) && igraph::vcount(g) > 0, "Run FLCA first."))

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
  if (length(vals2) != length(vals)) vals2 <- vals
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




# ---- Shared Top-N selector: qualifying clusters first ------------------------
# The same node set is used by the FLCA network, SSplot, chord, and Sankey.
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
  # Ordinary inputs use the four-member default. CCC modes explicitly request
  # a larger cap so a single dense hospital/sample cluster can fill Top-N.
  first_pass_cap <- max_per_cluster

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

  # With one cluster, keep the top target_n before splitting it. Otherwise,
  # select Top-1's cluster first, then complete capped clusters supplying
  # 3-4 nodes, then smaller clusters until the target is reached.
  df_ord <- df[order(-df$value, df$name, method = "radix"), , drop = FALSE]
  cluster_members <- split(df_ord$name, as.character(df_ord$carac))
  cluster_ids <- unique(as.character(df_ord$carac))
  cluster_sizes <- pmin(lengths(cluster_members), first_pass_cap)
  top_cluster <- as.character(df_ord$carac[1L])
  other_ids <- cluster_ids[cluster_ids != top_cluster]
  cluster_ids <- c(top_cluster,
                   other_ids[cluster_sizes[other_ids] >= 3L],
                   other_ids[cluster_sizes[other_ids] < 3L])
  picked <- character(0)
  if (length(cluster_ids) == 1L) {
    picked <- head(cluster_members[[top_cluster]], target_n)
  } else {
    for (cid in cluster_ids) {
      picked <- c(picked, head(cluster_members[[cid]], first_pass_cap))
      if (length(picked) >= target_n) break
    }
  }
  # CCC workflows explicitly raise the cap above four to obtain an exact
  # hospital/sample count; ordinary FLCA keeps the complete final cluster.
  if (max_per_cluster > 4L) picked <- head(picked, target_n)

  out <- df[match(picked, df$name), , drop = FALSE]
  out <- out[!is.na(out$name), , drop = FALSE]
  if (length(unique(out$carac)) == 1L && nrow(out) >= 2L) {
    out <- out[order(-out$value, out$name, method = "radix"), , drop = FALSE]
    out$carac <- ifelse(seq_len(nrow(out)) <= ceiling(nrow(out) / 2), 1L, 2L)
  }
  out$selection_rank <- match(out$name, picked)
  out <- out[order(out$selection_rank), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Each cluster contributes one term to the graph's modularity sum. These
# terms, rather than the overall sum, belong on SSplot's cluster rows.
ssplot_modularity_contributions <- function(g, membership, weights = NULL) {
  n <- igraph::vcount(g)
  cl <- as.character(membership)
  ids <- unique(cl)
  out <- stats::setNames(rep(NA_real_, length(ids)), ids)
  if (!n || length(cl) != n) return(out)
  ends <- igraph::as_edgelist(g, names = FALSE)
  if (is.null(weights)) weights <- rep(1, nrow(ends))
  weights <- suppressWarnings(as.numeric(weights))
  if (length(weights) != nrow(ends) || any(!is.finite(weights) | weights < 0))
    return(out)
  m <- sum(weights)
  if (m <= 0) return(out)
  strength <- numeric(n)
  internal <- stats::setNames(numeric(length(ids)), ids)
  for (i in seq_len(nrow(ends))) {
    a <- ends[i, 1]; b <- ends[i, 2]; w <- weights[i]
    strength[a] <- strength[a] + w
    strength[b] <- strength[b] + w
    if (cl[a] == cl[b]) internal[cl[a]] <- internal[cl[a]] + w
  }
  for (id in ids) out[id] <- internal[id] / m -
    (sum(strength[cl == id]) / (2 * m))^2
  out
}

# ---- Adapter: build renderSSplot(82) inputs from current FLCA result ----------
make_renderSSplot82_inputs <- function(xres, target_n = 20) {
  shiny::req(xres)
  g <- xres$g_flca
  shiny::validate(shiny::need(!is.null(g) && igraph::vcount(g) > 0, "Run FLCA first."))

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
  if (length(vals2) != length(vals)) vals2 <- vals
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

  # SSplot adjacent node must be a real non-self nearest neighbor.
  # The previous leader-based display used all_nodes$leader as the neighbor;
  # therefore cluster leaders were printed as self-neighbors (for example,
  # Brazil -> Brazil).  Use the strongest/nearest non-self neighbor instead.
  nn_tbl <- ssplot_nearest_nonself_tbl(g, mem_raw, values = vals)
  nn_tbl <- nn_tbl[match(all_nodes$name, nn_tbl$name), , drop = FALSE]
  wsel <- suppressWarnings(as.numeric(nn_tbl$wsel))

  sil_df <- data.frame(
    name = all_nodes$name,
    sil_width = all_nodes$sil_width,
    carac = all_nodes$carac,
    value = all_nodes$value,
    value2 = all_nodes$value2,
    wsel = wsel,
    role = ifelse(all_nodes$name == all_nodes$leader, "leader", "follower"),
    neighbor_name = nn_tbl$neighbor_name,
    neighborC = nn_tbl$neighborC,
    nov = all_nodes$degree,
    stringsAsFactors = FALSE
  )
  sil_df$wsel[!is.finite(sil_df$wsel)] <- NA_real_
  # Final safety check: an SSplot neighbor label must never equal its own node.
  self_nn <- !is.na(sil_df$neighbor_name) & sil_df$neighbor_name == sil_df$name
  sil_df$neighbor_name[self_nn] <- NA_character_
  sil_df$neighborC[self_nn] <- NA_integer_

  nodes0 <- data.frame(
    name = all_nodes$name,
    carac = all_nodes$carac,
    value = all_nodes$value,
    value2 = all_nodes$value2,
    stringsAsFactors = FALSE
  )

  target_n <- suppressWarnings(as.integer(target_n))
  if (!is.finite(target_n) || target_n <= 0) target_n <- 20L

  # Final Top-N selection is leader-first and capped at four per cluster,
  # except when a single cluster supplies the entire Top-N.
  # The same selected_sil_df is reused by Step 6 SSplot and Step 6c chord.
  standard <- NULL
  if (isTRUE(xres$standard_flca_input)) {
    standard_edges <- xres$edges
    names(standard_edges)[names(standard_edges) == "follower"] <- "Follower"
    standard <- run_flca(xres$nodes, standard_edges, top50 = 50L,
                         target_n = target_n,
                         max_cluster_size = xres$max_per_cluster %||% 4L)
    reduced_quality <- flca_quality(standard$nodes, standard$tree_edges)
    selected_sil_df <- sil_df[match(standard$nodes$name, sil_df$name), , drop = FALSE]
    selected_sil_df$carac <- standard$nodes$carac
    selected_sil_df$sil_width <- reduced_quality$silhouette$individual$SS[
      match(selected_sil_df$name, reduced_quality$silhouette$individual$name)]
    selected_sil_df$selection_rank <- seq_len(nrow(selected_sil_df))
  } else {
    selected_sil_df <- select_top_n_max_per_cluster(
      sil_df, target_n = target_n, max_per_cluster = xres$max_per_cluster %||% 4L
    )
  }
  nodes20 <- nodes0[match(selected_sil_df$name, nodes0$name), , drop = FALSE]
  nodes20$carac <- selected_sil_df$carac

  q_w <- mod_safe(g, mem_raw)
  q_u <- mod_safe_unweighted(g, mem_raw)
  q_w_by <- ssplot_modularity_contributions(g, mem_raw, igraph::E(g)$weight)
  q_u_by <- ssplot_modularity_contributions(g, mem_raw)
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
    Qw = c(q_w, unname(q_w_by[as.character(cl)])),
    Qu = c(q_u, unname(q_u_by[as.character(cl)])),
    D_GiniSimpson = c(D_gs, rep(NA_real_, length(cl))),
    Q_over_D = c(ifelse(is.finite(D_gs) && D_gs > 0, q_u / D_gs, NA_real_), rep(NA_real_, length(cl))),
    OneMinus_1_over_k = c(Dmax, rep(NA_real_, length(cl))),
    Q_over_Dmax_eff = c(ifelse(is.finite(Dmax) && Dmax > 0, q_u / Dmax, NA_real_), rep(NA_real_, length(cl))),
    n = c(nrow(sil_df), n_per),
    stringsAsFactors = FALSE
  )
  if (!is.null(standard)) {
    selected_ids <- unique(selected_sil_df$carac)
    tree_metrics <- flca_tree_full_metrics(standard$nodes,
                                           standard$tree_edges, standard$edges)
    cluster_metrics <- tree_metrics$clusters
    cluster_idx <- match(as.character(selected_ids),
                         as.character(cluster_metrics$carac))
    results <- data.frame(
      Cluster = c("OVERALL", paste0("C", selected_ids)),
      SS = c(tree_metrics$mean_SS, cluster_metrics$SS[cluster_idx]),
      Qw = c(tree_metrics$Qw, cluster_metrics$Qw[cluster_idx]),
      Qu = c(tree_metrics$Qu, cluster_metrics$Qu[cluster_idx]),
      D_GiniSimpson = NA_real_, Q_over_D = NA_real_,
      OneMinus_1_over_k = NA_real_, Q_over_Dmax_eff = NA_real_,
      n = c(nrow(selected_sil_df), as.integer(table(factor(selected_sil_df$carac,
                                                           levels = selected_ids)))),
      stringsAsFactors = FALSE)
  }
  list(sil_df = sil_df, nodes0 = nodes0, nodes20 = nodes20,
       selected_sil_df = selected_sil_df, results = results,
       standard_flca = standard)
}

render_real_SSplot82_panel <- function(xres, target_n = 20) {
  # Share the final selected tree and its metrics with the Process dendrogram.
  # make_renderSSplot82_inputs() alone still holds candidate-graph summaries.
  inp <- make_flca_process_top_bundle(xres, target_n = target_n)$input
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
flca_aac_network_values <- function(nodes, edges, mode = c("count", "value")) {
  mode <- match.arg(mode)
  names <- as.character(nodes$name)
  score <- stats::setNames(numeric(length(names)), names)
  ed <- tryCatch(clean_edges(edges), error = function(e) NULL)
  if (is.null(ed) || !nrow(ed)) return(score)
  ed <- ed[ed$Leader %in% names & ed$follower %in% names &
             ed$Leader != ed$follower & is.finite(ed$WCD) & ed$WCD > 0,
           , drop = FALSE]
  if (!nrow(ed)) return(score)
  ed$a <- pmin(ed$Leader, ed$follower)
  ed$b <- pmax(ed$Leader, ed$follower)
  ed <- stats::aggregate(WCD ~ a + b, ed, FUN = max)
  increment <- if (mode == "count") rep(1, nrow(ed)) else ed$WCD
  for (i in seq_len(nrow(ed))) {
    score[ed$a[i]] <- score[ed$a[i]] + increment[i]
    score[ed$b[i]] <- score[ed$b[i]] + increment[i]
  }
  score
}

flca_aac_top3 <- function(scores, mode = c("count", "value"),
                          rank_names = NULL) {
  mode <- match.arg(mode)
  ranked <- if (is.null(rank_names))
    sort(suppressWarnings(as.numeric(scores)), decreasing = TRUE) else
    suppressWarnings(as.numeric(scores[head(rank_names, 3L)]))
  ranked <- ranked[is.finite(ranked)]
  if (length(ranked) < 3L) return(NA_real_)
  v <- ranked[1:3]
  if (mode == "count") {
    if (any(v <= 0)) return(NA_real_)
    ratio <- (v[1] / v[2]) / (v[2] / v[3])
    return(ratio / (1 + ratio))
  }
  if (v[1] < v[2] || v[2] < v[3] || v[1] == v[3]) NA_real_
  else (v[1] - v[2]) / (v[1] - v[3])
}

make_flca_value_value2_kano <- function(xres, label_size = 3.6, visual_ratio = 1, target_n = 20) {
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

  # Bubble area uses the actual structural-openness proportion: 1 - Burt's
  # constraint, expressed as a percentage. Unlike min-max normalization, the
  # Top-1 node is not automatically assigned 100%.
  # A 0.01% floor keeps zero-openness and isolated displayed nodes visible
  # and permits the requested Top-3 1-C AAC calculation.
  nd$One_minus_C_percent <- 0.01
  ed <- tryCatch(clean_edges(xres$edges), error = function(e) NULL)
  if (!is.null(ed) && nrow(ed)) {
    ed <- ed[ed$Leader %in% nd$name & ed$follower %in% nd$name & ed$Leader != ed$follower, , drop = FALSE]
    if (nrow(ed)) {
      ed <- stats::aggregate(WCD ~ Leader + follower, data = ed, FUN = sum)
      w <- matrix(0, nrow(nd), nrow(nd), dimnames = list(nd$name, nd$name))
      for (i in seq_len(nrow(ed))) {
        a <- ed$Leader[i]; b <- ed$follower[i]
        w[a, b] <- w[a, b] + ed$WCD[i]
        w[b, a] <- w[b, a] + ed$WCD[i]
      }
      strength <- rowSums(w)
      degree <- rowSums(w > 0)
      p <- sweep(w, 1, pmax(strength, 1), "/")
      indirect <- p %*% p
      diag(indirect) <- 0
      constraint <- rowSums((p + indirect)^2)
      constraint[degree == 0] <- NA_real_
      valid <- is.finite(constraint)
      if (any(valid)) {
        nd$One_minus_C_percent[valid] <- pmax(0.01, 100 * pmin(1, pmax(0, 1 - constraint[valid])))
      }
    }
  }

  x_rng <- range(nd$value2, na.rm = TRUE)
  y_rng <- range(nd$value, na.rm = TRUE)
  center_x <- mean(x_rng)
  center_y <- mean(y_rng)
  half_x <- if (diff(x_rng) > 0) diff(x_rng) / 2 else max(1, abs(center_x) * .05)
  half_y <- if (diff(y_rng) > 0) diff(y_rng) / 2 else max(1, abs(center_y) * .05)
  half_x <- half_x * 1.15
  half_y <- half_y * 1.15
  x_ticks <- pretty(x_rng, n = 5)
  x_ticks <- x_ticks[x_ticks >= x_rng[1] & x_ticks <= x_rng[2]]
  if (!length(x_ticks)) x_ticks <- center_x
  y_ticks <- pretty(y_rng, n = 5)
  y_ticks <- y_ticks[y_ticks >= y_rng[1] & y_ticks <= y_rng[2]]
  if (!length(y_ticks)) y_ticks <- center_y

  # Kano reference circles need comparable display units. Scale each axis
  # independently for drawing, then label ticks with the original values.
  # Keep value and value2 untouched in the returned plot data and node table.
  display_nodes <- nd
  display_nodes$value2 <- (nd$value2 - center_x) / half_x
  display_nodes$value <- (nd$value - center_y) / half_y
  p <- plot_kano_real(nodes = display_nodes,
                      title_txt = "Kano plot: same capped Top-20 nodes as SSplot/chord/Sankey",
                      visual_ratio = 1, label_size = label_size,
                      origin = c(0, 0))
  p$data$x_display <- p$data$value2
  p$data$y_display <- p$data$value
  p$data$value <- nd$value[match(p$data$name, nd$name)]
  p$data$value2 <- nd$value2[match(p$data$name, nd$name)]
  p$mapping <- ggplot2::aes(x = x_display, y = y_display)
  size_1mc <- suppressWarnings(as.numeric(nd$One_minus_C_percent))
  size_1mc[!is.finite(size_1mc) | size_1mc < 0] <- 0
  p$data$size_plot <- size_1mc[match(p$data$name, nd$name)]
  aac_mode <- if (isTRUE(xres$nurseratio_ccc) || isTRUE(xres$genecluster_ccc) ||
                  isTRUE(xres$gene_row_ccc)) "value" else "count"
  aac_scores <- flca_aac_network_values(nd, xres$edges, aac_mode)
  value_rank <- order(-nd$value, nd$name, method = "radix")
  aac <- flca_aac_top3(aac_scores, aac_mode,
                       rank_names = nd$name[value_rank])
  # The Top-3 identities always follow node value rank, including 1-C AAC.
  openness_top3 <- nd$One_minus_C_percent[head(value_rank, 3L)]
  aac_1mc_top3 <- if (length(openness_top3) >= 3 &&
                        openness_top3[2] > 0 && openness_top3[3] > 0) {
    ratio_1mc <- openness_top3[1] * openness_top3[3] / openness_top3[2]^2
    ratio_1mc / (1 + ratio_1mc)
  } else {
    NA_real_
  }
  top <- value_rank[1L]
  aac_source <- if (aac_mode == "count") "AAC(count; links of value-ranked Top 3)" else
    "AAC(value; CCC of value-ranked Top 3)"
  annotation <- sprintf(
    "Top 1 by node value: %s | 1-C: %.1f%% | %s: %s | AAC(value-ranked Top-3 1-C): %s",
    nd$name[top], nd$One_minus_C_percent[top],
    aac_source,
    if (is.finite(aac)) sprintf("%.2f", aac) else "not estimable",
    if (is.finite(aac_1mc_top3)) sprintf("%.2f", aac_1mc_top3) else "not estimable"
  )

  p +
    ggplot2::labs(x = "value2", y = "value") +
    ggplot2::scale_size_area(max_size = 11, limits = c(0, 100),
                              breaks = c(0, 25, 50, 75, 100),
                              name = "1-C (%)") +
    ggplot2::labs(subtitle = annotation) +
    ggplot2::scale_x_continuous(limits = c(-1.45, 1.45),
      breaks = (x_ticks - center_x) / half_x,
      labels = format(x_ticks, trim = TRUE, scientific = FALSE, big.mark = ",")) +
    ggplot2::scale_y_continuous(limits = c(-1.45, 1.45),
      breaks = (y_ticks - center_y) / half_y,
      labels = format(y_ticks, trim = TRUE, scientific = FALSE, big.mark = ",")) +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 130, 10, 130), legend.position = "right",
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "bold", size = 10))
}

# Give each selected reduced-network component one display color and root its
# links at the highest-value node. WCD values are unchanged.
orient_reduced_edges_by_component <- function(nodes, edges) {
  labels <- as.character(nodes$name)
  values <- stats::setNames(suppressWarnings(as.numeric(nodes$value)), labels)
  values[!is.finite(values)] <- 0
  adjacency <- stats::setNames(vector("list", length(labels)), labels)
  for (i in seq_len(nrow(edges))) {
    a <- as.character(edges$Leader[i]); b <- as.character(edges$follower[i])
    if (!(a %in% labels && b %in% labels) || a == b) next
    adjacency[[a]] <- unique(c(adjacency[[a]], b))
    adjacency[[b]] <- unique(c(adjacency[[b]], a))
  }
  membership <- stats::setNames(rep(NA_integer_, length(labels)), labels)
  leaders <- character()
  distance <- stats::setNames(rep(Inf, length(labels)), labels)
  component <- 0L
  for (start in labels) {
    if (!is.na(membership[[start]])) next
    component <- component + 1L
    queue <- start; members <- character()
    membership[[start]] <- component
    while (length(queue)) {
      node <- queue[1]; queue <- queue[-1]
      members <- c(members, node)
      for (neighbor in adjacency[[node]]) {
        if (is.na(membership[[neighbor]])) {
          membership[[neighbor]] <- component
          queue <- c(queue, neighbor)
        }
      }
    }
    leader <- members[order(-values[members], members)][1]
    leaders <- c(leaders, leader)
    distance[[leader]] <- 0
    queue <- leader
    while (length(queue)) {
      node <- queue[1]; queue <- queue[-1]
      for (neighbor in adjacency[[node]]) {
        if (is.infinite(distance[[neighbor]])) {
          distance[[neighbor]] <- distance[[node]] + 1
          queue <- c(queue, neighbor)
        }
      }
    }
  }
  out <- edges
  for (i in seq_len(nrow(out))) {
    a <- as.character(edges$Leader[i]); b <- as.character(edges$follower[i])
    if (!(a %in% labels && b %in% labels) || a == b) next
    a_leads <- distance[[a]] < distance[[b]] ||
      (distance[[a]] == distance[[b]] &&
         (values[[a]] > values[[b]] || (values[[a]] == values[[b]] && a < b)))
    out$Leader[i] <- if (a_leads) a else b
    out$follower[i] <- if (a_leads) b else a
  }
  if (nrow(out)) {
    key <- paste(out$Leader, out$follower, sep = "\r")
    keep <- !duplicated(key)
    totals <- tapply(as.numeric(out$WCD), key, sum)
    out <- out[keep, , drop = FALSE]
    out$WCD <- unname(as.numeric(totals[key[keep]]))
    rownames(out) <- NULL
  }
  list(edges = out, membership = membership, leaders = leaders)
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
    if (nrow(ed)) ed <- ed |>
      dplyr::group_by(Leader, follower) |>
      dplyr::summarise(WCD = sum(WCD, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(match(Leader, top_names), match(follower, top_names), dplyr::desc(WCD)) |>
      as.data.frame(stringsAsFactors = FALSE)
    ed
  }
  ed_flca_reduced <- filter_top_edges(xres$edges_reduced)
  ed_original_full <- filter_top_edges(xres$edges)
  if (!is.null(inp$standard_flca)) {
    # Display exactly the tree and the full induced edge set returned by the
    # shared FLCA core; do not recompute parents after sampling.
    ed_flca_reduced <- inp$standard_flca$tree_edges
    names(ed_flca_reduced)[names(ed_flca_reduced) == "Follower"] <- "follower"
    ed_original_full <- inp$standard_flca$edges
    names(ed_original_full)[names(ed_original_full) == "Follower"] <- "follower"
  }
  if (isTRUE(xres$nurseratio_ccc) && nrow(ed_original_full)) {
    # Recompute each follower's strongest incoming leader link among the
    # displayed nodes when its full-data leader was outside Top-N.
    ed_flca_reduced <- select_flca_follower_links(ed_original_full, nd)
  }

  # Optional row-wise TAAA ToT edge tables are additive.  They are used only by
  # FLCA Process Step 2/3, not by the app-wide original co-word network plots.
  ed_taaa_full <- if (isTRUE(xres$taaa_tot) && !is.null(xres$taaa_tot_edges_full)) {
    filter_top_edges(xres$taaa_tot_edges_full)
  } else {
    data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE)
  }
  ed_taaa_one <- if (isTRUE(xres$taaa_tot) && !is.null(xres$taaa_tot_edges_one)) {
    filter_top_edges(xres$taaa_tot_edges_one)
  } else {
    data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE)
  }
  vertices <- data.frame(name = nd$name, value = nd$value, value2 = nd$value2, carac = nd$carac, stringsAsFactors = FALSE)
  if ("Condition" %in% names(xres$nodes))
    vertices$Condition <- xres$nodes$Condition[match(vertices$name, xres$nodes$name)]

  graph_from_top_edges <- function(ed) {
    g0 <- igraph::graph_from_data_frame(
      d = if (nrow(ed)) data.frame(from = ed$Leader, to = ed$follower, weight = ed$WCD, WCD = ed$WCD, stringsAsFactors = FALSE) else data.frame(from = character(0), to = character(0), weight = numeric(0), WCD = numeric(0)),
      vertices = vertices,
      directed = FALSE
    )
    igraph::V(g0)$value <- vertices$value[match(igraph::V(g0)$name, vertices$name)]
    igraph::V(g0)$value2 <- vertices$value2[match(igraph::V(g0)$name, vertices$name)]
    g0
  }

  # Step 2/3 selection keeps the original analysis pipeline.
  # For row-wise coword/TAAA uploads, TAAA ToT networks are ADDITIONAL bottom
  # outputs in the FLCA Process tab and must not replace Step 2/3.
  ed_full_plot <- if (nrow(ed_original_full)) ed_original_full else ed_flca_reduced
  ed_one_plot  <- if (nrow(ed_flca_reduced)) ed_flca_reduced else ed_original_full
  if (is.null(inp$standard_flca)) {
    display <- orient_reduced_edges_by_component(nd, ed_one_plot)
    ed_one_plot <- display$edges
  }
  nd$display_cluster <- nd$carac
  is_fifa_input <- grepl("FIFA 2026|fifa2026|completed knockout|Round-of-32|Group A-L", xres$data_mode %||% "", ignore.case = TRUE)
  edge_source <- if (isTRUE(xres$taaa_tot)) {
    paste0(
      "row-wise coword/TAAA upload: original coword full-edge network is retained in Step 2 and ordinary FLCA reduced network is retained in Step 3; ",
      "additional TAAA ToT full-edge and leader -> follower one-edge networks appear at the bottom of this tab"
    )
  } else if (isTRUE(xres$gene_row_ccc)) {
    paste0("genecluster5.csv log10P group ", xres$gene_group,
           ": Step 2 shows positive pairwise Lin's CCC among the Top-20 gene rows; Step 3 shows each gene's maximum CCC link; FLCA computes clusters")
  } else if (isTRUE(xres$genecluster_ccc)) {
    paste0("genecluster5.csv log10P group ", xres$gene_group,
           ": Step 2 shows positive pairwise Lin's CCC among the Top-20 GSM columns; Step 3 shows each GSM node's maximum CCC link; FLCA computes clusters")
  } else if (isTRUE(xres$nurseratio_ccc)) {
    "nurseratio.csv: Step 2 shows positive pairwise Lin's CCC; Step 3 shows each hospital's maximum CCC link; both use the selected 20 hospitals"
  } else if (isTRUE(is_fifa_input)) {
    "FIFA 2026 match network: Step 2 uses the 48-team group-stage + completed knockout full match edges filtered to identical Top-N nodes; Step 3 uses ordinary FLCA reduced one-link edges"
  } else if (nrow(ed_original_full)) {
    "original full input edges filtered to the same selected 20-23 nodes; Step 3 uses the shared strongest-link FLCA tree"
  } else {
    "FLCA reduced one-link edges fallback"
  }

  g_full_top <- graph_from_top_edges(ed_full_plot)
  g_one_top  <- graph_from_top_edges(ed_one_plot)
  g_taaa_full_top <- graph_from_top_edges(ed_taaa_full)
  g_taaa_one_top  <- graph_from_top_edges(ed_taaa_one)

  mem <- setNames(vertices$carac, vertices$name)
  display_leaders <- vapply(split(nd, nd$carac), function(group) {
    group$name[order(-group$value, group$name, method = "radix")][1L]
  }, character(1))
  out <- list(
    input = inp,
    nodes = nd,
    edges_original_full = ed_original_full,
    edges_flca_reduced = ed_flca_reduced,
    edges_taaa_full = ed_taaa_full,
    edges_taaa_one = ed_taaa_one,
    edges_full = ed_full_plot,
    edges_reduced = ed_one_plot,
    edges_plot = ed_full_plot,
    edges_one_plot = ed_one_plot,
    edge_source = edge_source,
    g = g_full_top,
    g_full = g_full_top,
    g_one = g_one_top,
    g_taaa_full = g_taaa_full_top,
    g_taaa_one = g_taaa_one_top,
    membership = mem,
    display_membership = mem,
    display_leaders = unname(display_leaders),
    data_type = if (isTRUE(xres$nurseratio_ccc) || isTRUE(xres$gene_row_ccc) ||
                    isTRUE(xres$genecluster_ccc)) "value" else "count"
  )
  # The SSplot rows and tree labels must describe the same selected nodes,
  # membership, and strongest-link edges. Candidate-graph scores above are
  # useful during selection but must not be displayed as selected-tree scores.
  tree <- make_flca_process_dendrogram(out)
  if (!is.null(tree)) {
    selected <- out$input$selected_sil_df
    if (nrow(tree$edges)) {
      individual <- flca_quality(tree$nodes, tree$edges)$silhouette$individual
      selected$sil_width <- individual$SS[match(selected$name, individual$name)]
      singleton <- table(as.character(selected$carac))
      selected$sil_width[as.character(selected$carac) %in%
                           names(singleton[singleton == 1L])] <- 0
    } else {
      selected$sil_width <- rep(NA_real_, nrow(selected))
    }
    out$nodes$sil_width <- selected$sil_width[match(out$nodes$name, selected$name)]
    out$input$selected_sil_df <- selected
    ids <- as.character(tree$metrics$clusters$carac)
    counts <- table(as.character(selected$carac))
    out$input$results <- data.frame(
      Cluster = c("OVERALL", paste0("C", ids)),
      SS = c(tree$metrics$mean_SS, tree$metrics$clusters$SS),
      Qw = c(tree$metrics$Qw, tree$metrics$clusters$Qw),
      Qu = c(tree$metrics$Qu, tree$metrics$clusters$Qu),
      D_GiniSimpson = NA_real_, Q_over_D = NA_real_,
      OneMinus_1_over_k = NA_real_, Q_over_Dmax_eff = NA_real_,
      n = c(nrow(selected), as.integer(counts[ids])),
      stringsAsFactors = FALSE)
  }
  out
}

# The Process dendrograph uses the exact reduced Leader/follower/WCD table
# behind Step 3's network. The author plot still derives a one-link tree from
# its full record graph, which has no reduced edge table.
flca_tree_effect_category <- function(x, cutoffs) {
  if (length(x) != 1L || !is.finite(x)) return("unavailable")
  magnitude <- abs(x)
  if (magnitude >= cutoffs[3]) "Large" else if (magnitude >= cutoffs[2]) "Medium"
  else if (magnitude >= cutoffs[1]) "Small" else "Trivial"
}

flca_tree_top1_annotation <- function(nodes, count_data = NULL, aac_values = NULL) {
  node_values <- suppressWarnings(as.numeric(nodes$value))
  node_names <- as.character(nodes$name)
  node_rank <- order(-node_values, node_names, na.last = TRUE,
                     method = "radix")
  node_top <- node_rank[1L]
  node_top_name <- node_names[node_top]
  values <- if (is.null(aac_values)) node_values else
    suppressWarnings(as.numeric(aac_values[node_names]))
  ranked <- values[node_rank]
  if (is.null(count_data)) count_data <- length(ranked) > 0L &&
    all(is.finite(ranked) & ranked == round(ranked))
  if (length(ranked) < 3L || any(!is.finite(ranked[1:3])))
    return(c(sprintf("Top 1: %s (node value=%.2f) | AAC unavailable: three finite value-ranked scores are needed",
                     node_top_name, node_values[node_top]),
             if (isTRUE(count_data)) "Equivalent effect size unavailable" else "Cohen's d unavailable"))
  v1 <- ranked[1L]; v2 <- ranked[2L]; v3 <- ranked[3L]
  if (isTRUE(count_data)) {
    ratio <- if (all(c(v1, v2, v3) > 0)) (v1 / v2) / (v2 / v3) else NA_real_
    aac <- if (is.finite(ratio)) ratio / (1 + ratio) else NA_real_
    line1 <- if (is.finite(aac) && !is.null(aac_values))
      sprintf("Top 1: %s (node value=%.2f) | AAC(count; links of value-ranked Top 3) = %.3f",
              node_top_name, node_values[node_top], aac)
    else if (is.finite(aac))
      sprintf("Top 1: %s | AAC(count) = %.3f (v1=%.2f, v2=%.2f, v3=%.2f)",
              node_top_name, aac, v1, v2, v3)
    else sprintf("Top 1: %s (node value=%.2f) | AAC(count) = NA",
                 node_top_name, node_values[node_top])
    line2 <- if (is.finite(aac) && !is.null(aac_values))
      sprintf("Top-1 link count: %.0f | Equivalent effect size = %s",
              v1,
              flca_tree_effect_category(aac, c(.6, .7, .8)))
    else if (is.finite(aac))
      sprintf("Equivalent effect size = %s", flca_tree_effect_category(aac, c(.6, .7, .8)))
    else "Equivalent effect size unavailable"
    return(c(line1, line2))
  }
  top20_sd <- stats::sd(head(ranked[is.finite(ranked)], 20L))
  d <- if (is.finite(top20_sd) && top20_sd > 0) (v1 - v2) / top20_sd else NA_real_
  aac <- if (v1 >= v2 && v2 >= v3 && v1 != v3)
    (v1 - v2) / (v1 - v3) else NA_real_
  line1 <- if (is.finite(aac) && !is.null(aac_values))
    sprintf("Top 1: %s (node value=%.2f) | AAC(value; CCC of value-ranked Top 3) = %.3f",
            node_top_name, node_values[node_top], aac)
  else if (is.finite(aac))
    sprintf("Top 1: %s | AAC(value) = %.3f (v1=%.2f, v2=%.2f, v3=%.2f)",
            node_top_name, aac, v1, v2, v3)
  else sprintf("Top 1: %s (node value=%.2f) | AAC(value; CCC of value-ranked Top 3) = NA",
               node_top_name, node_values[node_top])
  d_category <- flca_tree_effect_category(d, c(.2, .5, .8))
  line2 <- if (is.finite(d) && !is.null(aac_values))
    sprintf("Top-1 CCC strength: %.2f | Cohen's d = %.3f (%s effect)",
            v1, d, d_category)
  else if (is.finite(d)) sprintf("Cohen's d = %.3f (%s effect)", d, d_category)
           else "Cohen's d unavailable (Top-20 SD is zero)"
  c(line1, line2)
}

flca_tree_top3_style <- function(nodes, base_colors) {
  values <- suppressWarnings(as.numeric(nodes$value))
  values[!is.finite(values)] <- -Inf
  ranked <- order(-values, as.character(nodes$name), method = "radix")
  ranked <- ranked[is.finite(values[ranked])]
  colors <- c("#C00000", "#0645D9", "#8B4513")
  fill <- as.character(base_colors)
  border <- rep("grey20", nrow(nodes))
  border_lwd <- rep(1, nrow(nodes))
  label <- rep("black", nrow(nodes))
  for (rank in seq_len(min(3L, length(ranked)))) {
    border[ranked[rank]] <- colors[rank]
    border_lwd[ranked[rank]] <- 2.5
    label[ranked[rank]] <- colors[rank]
  }
  list(fill = fill, border = border, border_lwd = border_lwd,
       label = label, ranked = ranked)
}

# Tree SS and Q use the reduced strongest-link tree; Top-1 SHC uses the full
# original network induced by displayed nodes. Positive WCD is strength.
# Missing paths use the same within/across-cluster penalties as sil_widths_vec.
flca_tree_full_metrics <- function(nodes, full_edges, shc_edges = full_edges) {
  nd <- as.data.frame(nodes, stringsAsFactors = FALSE)
  nm <- as.character(nd$name)
  cl <- as.character(nd$carac)
  value <- suppressWarnings(as.numeric(nd$value))
  top <- nm[order(-value, nm, na.last = TRUE, method = "radix")[1L]]
  ids <- unique(cl)
  ids <- ids[order(suppressWarnings(as.numeric(ids)), ids,
                   na.last = TRUE, method = "radix")]
  empty_cluster <- data.frame(carac = ids, SS = NA_real_, Qw = NA_real_,
                              Qu = NA_real_, stringsAsFactors = FALSE)
  empty <- list(mean_SS = NA_real_, Qw = NA_real_, Qu = NA_real_,
                top1_name = top, top1_constraint = NA_real_,
                top1_SHC = NA_real_, clusters = empty_cluster)
  if (!length(nm)) return(empty)

  edge_matrix <- function(input_edges) {
    adj <- matrix(0, length(nm), length(nm), dimnames = list(nm, nm))
    if (!is.data.frame(input_edges) || !nrow(input_edges)) return(adj)
    ed <- as.data.frame(input_edges, stringsAsFactors = FALSE)
    if ("follower" %in% names(ed) && !"Follower" %in% names(ed))
      names(ed)[names(ed) == "follower"] <- "Follower"
    if (!all(c("Leader", "Follower", "WCD") %in% names(ed))) return(adj)
    ed$Leader <- as.character(ed$Leader); ed$Follower <- as.character(ed$Follower)
    ed$WCD <- suppressWarnings(as.numeric(ed$WCD))
    ed <- ed[is.finite(ed$WCD) & ed$WCD > 0 & ed$Leader %in% nm &
               ed$Follower %in% nm & ed$Leader != ed$Follower, , drop = FALSE]
    if (!nrow(ed)) return(adj)
    ed$a <- pmin(ed$Leader, ed$Follower)
    ed$b <- pmax(ed$Leader, ed$Follower)
    ed <- stats::aggregate(WCD ~ a + b, data = ed, FUN = max)
    for (i in seq_len(nrow(ed))) {
      adj[ed$a[i], ed$b[i]] <- ed$WCD[i]
      adj[ed$b[i], ed$a[i]] <- ed$WCD[i]
    }
    adj
  }
  w <- edge_matrix(full_edges)
  w_shc <- edge_matrix(shc_edges)
  shc_strength <- rowSums(w_shc)
  p <- w_shc
  connected <- shc_strength > 0
  p[connected, ] <- w_shc[connected, , drop = FALSE] / shc_strength[connected]
  indirect <- p %*% p
  diag(indirect) <- 0
  constraint <- rowSums((p + indirect)^2)
  constraint[!connected] <- NA_real_
  shi <- 1 - constraint
  empty$top1_constraint <- unname(constraint[match(top, nm)])
  empty$top1_SHC <- unname(shi[match(top, nm)])
  if (!any(w > 0)) return(empty)

  m <- sum(w) / 2
  q_contrib <- function(adj, total) vapply(ids, function(cid) {
    idx <- which(cl == cid)
    (sum(adj[idx, idx, drop = FALSE]) / 2) / total -
      (sum(rowSums(adj)[idx]) / (2 * total))^2
  }, numeric(1))
  qw <- q_contrib(w, m)
  qu <- if (length(ids) > 1L) qw / (1 - 1 / length(ids)) else rep(NA_real_, length(ids))

  # All-pairs shortest paths using 1/WCD. Unconnected pairs are penalized
  # after path calculation; singleton silhouettes are exactly zero.
  d <- matrix(Inf, length(nm), length(nm))
  d[w > 0] <- 1 / w[w > 0]
  diag(d) <- 0
  for (k in seq_along(nm)) d <- pmin(d, outer(d[, k], d[k, ], "+"))
  finite_d <- d[is.finite(d) & d > 0]
  longest <- if (length(finite_d)) max(finite_d) else 1
  missing <- !is.finite(d)
  same <- outer(cl, cl, "==")
  d[missing & same] <- longest * 1.25
  d[missing & !same] <- longest * 3
  ss <- rep(NA_real_, length(nm))
  if (length(ids) > 1L) for (i in seq_along(nm)) {
    peers <- which(cl == cl[i] & seq_along(nm) != i)
    if (!length(peers)) { ss[i] <- 0; next }
    a <- mean(d[i, peers])
    b <- min(vapply(ids[ids != cl[i]], function(cid) mean(d[i, cl == cid]), numeric(1)))
    ss[i] <- if (max(a, b) > 0) (b - a) / max(a, b) else 0
  }
  ss_cluster <- vapply(ids, function(cid) {
    z <- ss[cl == cid]
    if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
  }, numeric(1))
  list(mean_SS = if (all(is.na(ss))) NA_real_ else mean(ss, na.rm = TRUE),
       Qw = sum(qw), Qu = if (all(is.na(qu))) NA_real_ else sum(qu),
       top1_name = top, top1_constraint = unname(constraint[match(top, nm)]),
       top1_SHC = unname(shi[match(top, nm)]),
       clusters = data.frame(carac = ids, SS = ss_cluster, Qw = qw,
                             Qu = qu, stringsAsFactors = FALSE))
}

make_flca_process_dendrogram <- function(bun) {
  nd <- bun$nodes
  labels <- as.character(nd$name)
  if (!is.null(bun$edges_reduced)) {
    ed <- bun$edges_reduced
    if (!length(labels)) return(NULL)
    links <- if (nrow(ed)) data.frame(Leader = as.character(ed$Leader),
                                     Follower = as.character(ed$follower),
                                     WCD = as.numeric(ed$WCD), stringsAsFactors = FALSE)
             else data.frame(Leader = character(), Follower = character(), WCD = numeric())
    links <- links[is.finite(links$WCD) & links$WCD > 0 &
                   links$Leader %in% labels & links$Follower %in% labels &
                   links$Leader != links$Follower, , drop = FALSE]
    # Step 3 can contain both orientations or several links into a follower.
    # Use the authoritative value/name rank and keep the strongest eligible
    # higher-ranked parent, with parent rank breaking equal-WCD ties.
    value_rank <- suppressWarnings(as.numeric(nd$value))
    value_rank[!is.finite(value_rank)] <- -Inf
    ranked_names <- labels[order(-value_rank, labels, method = "radix")]
    ranks <- stats::setNames(seq_along(ranked_names), ranked_names)
    swap <- ranks[links$Leader] > ranks[links$Follower]
    old_leader <- links$Leader[swap]
    links$Leader[swap] <- links$Follower[swap]
    links$Follower[swap] <- old_leader
    links <- links[order(ranks[links$Follower], -links$WCD,
                         ranks[links$Leader], links$Leader, method = "radix"), , drop = FALSE]
    links <- links[!duplicated(links$Follower), , drop = FALSE]
    children <- split(links$Follower, links$Leader)
    state <- stats::setNames(rep(0L, length(labels)), labels)
    check_cycle <- function(node) {
      if (state[[node]] == 1L)
        stop("The reduced network contains a directed cycle; it cannot be drawn as a dendrograph.", call. = FALSE)
      if (state[[node]] == 2L) return(invisible(NULL))
      state[[node]] <<- 1L
      for (child in children[[node]]) check_cycle(child)
      state[[node]] <<- 2L
      invisible(NULL)
    }
    for (node in labels) check_cycle(node)
    rownames(links) <- NULL
    value_source <- if (is.list(bun$input) && is.data.frame(bun$input$sil_df))
      bun$input$sil_df$value else nd$value
    value_source <- suppressWarnings(as.numeric(value_source))
    value_source <- value_source[is.finite(value_source)]
    count_data <- length(value_source) > 0L && all(value_source == round(value_source))
    if (!is.null(bun$data_type)) count_data <- identical(bun$data_type, "count")
    full_edges <- if (!is.null(bun$edges_original_full)) bun$edges_original_full else bun$edges_full
    aac_mode <- if (count_data) "count" else "value"
    aac_values <- flca_aac_network_values(nd, full_edges, aac_mode)
    return(list(nodes = nd, edges = links, leaders = unique(links$Leader),
                count_data = count_data, aac_values = aac_values,
                full_edges = full_edges,
                metrics = flca_tree_full_metrics(nd, links, full_edges)))
  }
  ed <- bun$edges_full
  if (!length(labels) || !nrow(ed)) return(NULL)
  values <- stats::setNames(as.numeric(nd$value), labels)
  a <- as.character(ed$Leader); b <- as.character(ed$follower)
  wa <- values[a]; wb <- values[b]
  keep <- is.finite(ed$WCD) & ed$WCD > 0 & is.finite(wa) & is.finite(wb) & a != b
  if (!any(keep)) return(NULL)
  a <- a[keep]; b <- b[keep]; w <- as.numeric(ed$WCD[keep])
  wa <- wa[keep]; wb <- wb[keep]
  a_leads <- wa > wb | (wa == wb & a < b)
  links <- data.frame(Leader = ifelse(a_leads, a, b),
                      Follower = ifelse(a_leads, b, a), WCD = w,
                      stringsAsFactors = FALSE)
  leader_value <- values[links$Leader]
  # Match preprocess_edges() in the reference FLCA module.
  links <- links[order(links$Follower,
                       -(links$WCD + leader_value / 10000),
                       -leader_value, links$Leader), , drop = FALSE]
  links <- links[!duplicated(links$Follower), , drop = FALSE]
  rownames(links) <- NULL
  value_source <- suppressWarnings(as.numeric(nd$value))
  value_source <- value_source[is.finite(value_source)]
  count_data <- length(value_source) > 0L && all(value_source == round(value_source))
  if (!is.null(bun$data_type)) count_data <- identical(bun$data_type, "count")
  aac_mode <- if (count_data) "count" else "value"
  aac_values <- flca_aac_network_values(nd, ed, aac_mode)
  list(nodes = nd, edges = links, leaders = unique(links$Leader),
       count_data = count_data, aac_values = aac_values,
       full_edges = ed, metrics = flca_tree_full_metrics(nd, links, ed))
}

flca_dendrogram_positions <- function(nodes, edges, gap = .55) {
  labels <- as.character(nodes$name)
  clusters <- if ("display_cluster" %in% names(nodes)) nodes$display_cluster else nodes$carac
  clusters <- as.character(clusters)
  clusters[is.na(clusters) | !nzchar(clusters)] <- "1"
  values <- suppressWarnings(as.numeric(nodes$value))
  values[!is.finite(values)] <- -Inf
  cluster_num <- suppressWarnings(as.numeric(clusters))
  ord <- order(is.na(cluster_num), cluster_num, clusters, -values, labels,
               method = "radix")
  stats::setNames(as.numeric(match(seq_along(labels), ord)), labels)
}

# FIFA workbook cluster IDs are original group labels (A=1, ..., L=12).
# Reindex only the tree's display copy by the highest-value member so the
# Top-1 cluster appears first and displayed IDs run C1, C2, ... without gaps.
flca_ranked_display_tree <- function(tree) {
  if (is.null(tree) || !nrow(tree$nodes)) return(tree)
  nd <- tree$nodes
  ids <- unique(as.character(nd$carac))
  top_values <- vapply(ids, function(id) max(nd$value[as.character(nd$carac) == id]), numeric(1))
  top_names <- vapply(ids, function(id) {
    x <- nd[as.character(nd$carac) == id, , drop = FALSE]
    x$name[order(-x$value, x$name, method = "radix")[1L]]
  }, character(1))
  ordered_ids <- ids[order(-top_values, top_names, method = "radix")]
  display_id <- stats::setNames(seq_along(ordered_ids), ordered_ids)
  nd$source_carac <- nd$carac
  nd$carac <- unname(display_id[as.character(nd$source_carac)])
  nd$display_cluster <- nd$carac
  tree$nodes <- nd
  if (!is.null(tree$metrics$clusters) && nrow(tree$metrics$clusters)) {
    tree$metrics$clusters$carac <- as.character(
      display_id[as.character(tree$metrics$clusters$carac)])
    tree$metrics$clusters <- tree$metrics$clusters[
      order(as.integer(tree$metrics$clusters$carac)), , drop = FALSE]
  }
  tree
}

flca_tree_cluster_leaders <- function(nodes, edges) {
  if (!nrow(edges)) return(character(0))
  direct_parents <- unique(as.character(edges$Leader))
  cluster_roots <- vapply(split(seq_len(nrow(nodes)), as.character(nodes$carac)), function(idx) {
    value <- suppressWarnings(as.numeric(nodes$value[idx]))
    value[!is.finite(value)] <- -Inf
    as.character(nodes$name[idx[order(-value, as.character(nodes$name[idx]), method = "radix")[1L]]])
  }, character(1))
  intersect(unname(cluster_roots), direct_parents)
}

flca_tree_highlight_leaders <- function(nodes, edges) {
  if (!nrow(edges)) return(character(0))
  direct_count <- table(as.character(edges$Leader))
  qualifying <- names(direct_count[direct_count >= 2L])
  union(qualifying, flca_tree_cluster_leaders(nodes, edges))
}

plot_flca_process_dendrogram <- function(tree, main_title = NULL, weight_label = "WCD",
                                         annotation_override = NULL,
                                         footer_override = NULL,
                                         show_cluster_dividers = FALSE,
                                         highlight_leaders_override = NULL) {
  nd <- tree$nodes; edges <- tree$edges
  labels <- as.character(nd$name)
  roots <- setdiff(labels, edges$Follower)
  children <- split(as.character(edges$Follower), as.character(edges$Leader))
  positions <- flca_dendrogram_positions(nd, edges)
  if (any(!is.finite(positions))) {
    plot.new(); text(.5, .5, "The FLCA links contain a cycle.")
    return(invisible(NULL))
  }
  leader_level <- stats::setNames(rep(0, length(labels)), labels)
  set_level <- function(node) {
    offspring <- children[[node]]
    if (!length(offspring)) return(0)
    leader_level[node] <<- 1 + max(vapply(offspring, set_level, numeric(1)))
    leader_level[node]
  }
  for (root in roots) set_level(root)
  top_height <- max(c(1, leader_level))
  values <- as.numeric(nd$value)
  values[!is.finite(values) | values <= 0] <- 1
  bubble_cex <- 3 * sqrt(values / max(values))
  count_labels <- sprintf("%.2f", as.numeric(nd$value))
  annotation <- if (is.null(annotation_override))
    flca_tree_top1_annotation(nd, tree$count_data, tree$aac_values) else
    as.character(annotation_override)
  metrics <- tree$metrics
  if (is.null(metrics))
    metrics <- flca_tree_full_metrics(nd, edges, tree$full_edges %||% edges)
  fmt_metric <- function(x) if (length(x) && is.finite(x[1])) sprintf("%.3f", x[1]) else "NA"
  top_root <- roots[order(-values[match(roots, labels)], roots,
                          method = "radix")[1L]]
  other_roots <- setdiff(roots, top_root)
  top_reference <- top_height + if (length(other_roots)) .6 else 0
  plot_cluster <- if ("display_cluster" %in% names(nd)) nd$display_cluster else nd$carac
  cluster_ids <- sort(unique(as.character(plot_cluster)))
  colours <- make_cluster_color_map(cluster_ids, "Set3")
  style <- flca_tree_top3_style(nd, unname(colours[as.character(plot_cluster)]))
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mar = c(4, 4, 8, 2), xpd = NA)
  name_cex <- if (length(labels) >= 15L) .68 else .78
  longest_name_in <- max(strwidth(labels, units = "inches", cex = name_cex, font = 2))
  bottom_in <- longest_name_in + 3.05
  upper_y <- top_reference + .5
  units_per_in <- upper_y / max(.5, par("pin")[2] - bottom_in)
  lower_y <- -bottom_in * units_per_in
  name_y <- -.16 * units_per_in
  count_y <- -(longest_name_in + .85) * units_per_in
  shi_y <- count_y - .62 * units_per_in
  cluster_bubble_y <- count_y - .88 * units_per_in
  cluster_ss_y <- count_y - 1.14 * units_per_in
  cluster_q_y <- count_y - 1.40 * units_per_in
  cluster_qu_y <- count_y - 1.66 * units_per_in
  plot(NA, xlim = c(.15, max(positions) + .85), ylim = c(lower_y, upper_y),
       axes = FALSE, xlab = "", ylab = "")
  if (isTRUE(show_cluster_dividers) && length(labels) > 1L) {
    ordered <- names(sort(positions))
    ordered_cluster <- as.character(plot_cluster[match(ordered, labels)])
    boundaries <- which(ordered_cluster[-length(ordered_cluster)] !=
                          ordered_cluster[-1L])
    for (i in boundaries) {
      x <- mean(positions[ordered[c(i, i + 1L)]])
      segments(x, 0, x, top_reference, col = "grey80", lty = 2, lwd = 1)
    }
  }
  mtext(if (is.null(main_title)) sprintf("FLCA leader-follower tree of %d nodes", length(labels)) else main_title,
        side = 3, line = 6.4, cex = 1.2, font = 2)
  mtext(sprintf("Mean SS = %s  |  Overall Qw = %s  |  Qu = %s",
                fmt_metric(metrics$mean_SS), fmt_metric(metrics$Qw), fmt_metric(metrics$Qu)),
        side = 3, line = 4.7, cex = .85, font = 2, col = "black")
  mtext(annotation[1], side = 3, line = 3.0, cex = .84, font = 2, col = "#C00000")
  if (length(annotation) > 1L)
    mtext(annotation[2], side = 3, line = 1.3, cex = .84, font = 2, col = "#C00000")
  axis(2, at = seq_len(top_height), labels = seq_len(top_height), las = 1,
       cex.axis = .75, col.axis = "#555555", lwd = 0)
  # The top red stem is a graphical framework, never an observed WCD edge.
  stem_x <- range(positions)
  if (diff(stem_x) == 0) stem_x <- stem_x + c(-.35, .35)
  segments(stem_x[1], top_reference, stem_x[2], top_reference,
           col = "#C00000", lty = 1, lwd = 2.5)
  highlighted_leaders <- if (is.null(highlight_leaders_override))
    flca_tree_highlight_leaders(nd, edges) else
    intersect(as.character(highlight_leaders_override), labels)
  red_leaders <- flca_tree_cluster_leaders(nd, edges)
  if (nrow(edges)) {
    leader_names <- if (length(tree$leaders)) as.character(tree$leaders) else unique(as.character(edges$Leader))
    for (leader in leader_names) {
      followers <- as.character(edges$Follower[edges$Leader == leader])
      level <- leader_level[leader]
      segments(positions[leader], 0, positions[leader], level,
               col = if (leader %in% red_leaders) "#C00000" else "grey55",
               lwd = if (leader %in% red_leaders) 2.5 else 1.5)
    }
    for (leader in unique(as.character(edges$Leader))) {
      followers <- as.character(edges$Follower[edges$Leader == leader])
      level <- leader_level[leader]
      segments(min(positions[c(leader, followers)]), level,
               max(positions[c(leader, followers)]), level,
               col = "grey55", lwd = 1.5)
    }
    max_wcd <- max(edges$WCD)
    # One follower stem can carry several incoming network links. Keep a
    # separate WCD annotation for each link without drawing parallel stems.
    for (follower in unique(as.character(edges$Follower))) {
      incoming <- which(edges$Follower == follower)
      segments(positions[follower], leader_level[follower], positions[follower],
               max(leader_level[as.character(edges$Leader[incoming])]),
               col = "grey55", lwd = 1 + 5 * max(edges$WCD[incoming]) / max_wcd)
    }
    for (i in seq_len(nrow(edges))) {
      leader <- as.character(edges$Leader[i]); follower <- as.character(edges$Follower[i])
      incoming <- which(edges$Follower == follower)
      offset <- if (length(incoming) > 1L)
        (match(i, incoming) - (length(incoming) + 1) / 2) * .55 else 0
      label <- sprintf("%.2f", edges$WCD[i])
      x <- positions[follower] + offset
      y <- (leader_level[follower] + leader_level[leader]) / 2
      half_w <- strwidth(label, cex = .72, font = 2) / 2 + strwidth(" ", cex = .72)
      half_h <- strheight(label, cex = .72, font = 2) / 2 + strheight(" ", cex = .72) / 3
      rect(x - half_w, y - half_h, x + half_w, y + half_h, col = "white", border = NA)
      text(x, y, label, cex = .72, font = 2, col = "#333333")
    }
  }
  if (!nrow(edges)) leader_names <- character(0)
  for (root in roots)
    segments(positions[root], leader_level[root], positions[root], top_reference,
             col = if (root == top_root) "#C00000" else "grey55",
             lty = if (root == top_root) 1 else 2,
             lwd = if (root == top_root) 2.5 else 1.5)
  # Purple X marks qualifying junctions and one-child cluster-root leaders.
  junction_x <- unname(positions[highlighted_leaders])
  junction_y <- unname(leader_level[highlighted_leaders])
  keep_junction <- !duplicated(paste(junction_x, junction_y))
  if (length(junction_x)) {
    points(junction_x[keep_junction], junction_y[keep_junction],
           pch = 4, col = "white", cex = 1.45, lwd = 5)
    points(junction_x[keep_junction], junction_y[keep_junction],
           pch = 4, col = "#6420C8", cex = 1.35, lwd = 2.5)
  }
  points(unname(positions[labels]), rep(0, length(labels)), pch = 21,
         cex = bubble_cex, bg = style$fill, col = style$border,
         lwd = style$border_lwd)
  text(unname(positions[labels]), rep(name_y, length(labels)), labels,
       srt = 90, adj = c(1, .5), cex = name_cex, font = 2,
       col = style$label)
  text(unname(positions[labels]), rep(count_y, length(labels)), count_labels,
       srt = 90, adj = c(1, .5), cex = .78, font = 2,
       col = style$label)
  # Cluster summaries sit beneath the node-value row. SHI is the full-network
  # Top-1 structural-hole value and is displayed at the far left for clarity.
  cluster_for_plot <- as.character(nd$carac)
  for (i in seq_len(nrow(metrics$clusters))) {
    stat <- metrics$clusters[i, , drop = FALSE]
    members <- labels[cluster_for_plot == as.character(stat$carac)]
    if (!length(members)) next
    x <- mean(unname(positions[members]))
    points(x, cluster_bubble_y, pch = 21, cex = 1.9,
           bg = unname(colours[as.character(stat$carac)]), col = "grey30", lwd = 1)
    text(x, cluster_bubble_y, sprintf("C%s", stat$carac), cex = .58, font = 2)
    text(x, cluster_ss_y, sprintf("SS=%s", fmt_metric(stat$SS)),
         cex = .7, font = 2, col = "#25364A")
    text(x, cluster_q_y, sprintf("Qw=%s", fmt_metric(stat$Qw)),
         cex = .7, font = 2, col = "#25364A")
    text(x, cluster_qu_y, sprintf("Qu=%s", fmt_metric(stat$Qu)),
         cex = .7, font = 2, col = "#25364A")
  }
  if (metrics$top1_name %in% labels)
    text(min(positions), shi_y,
         sprintf("Top-1 SHI=%s", fmt_metric(metrics$top1_SHC)),
         adj = c(0, .5), cex = .75, font = 2, col = "#C00000")
  footer <- if (is.null(footer_override))
    sprintf("Red: Top-1 stem and cluster leaders | grey links: %s | purple X: leader junction | dashed grey: disconnected from stem", weight_label) else
    footer_override
  mtext(footer,
        side = 1, line = .5, cex = .82)
}

# Vertical junction view for AAC(value) inputs. It reuses the same selected
# strongest-link edges as the horizontal Process dendrograph.
plot_flca_value_vertical_tree <- function(tree, weight_label = "WCD") {
  nd <- as.data.frame(tree$nodes, stringsAsFactors = FALSE)
  ed <- as.data.frame(tree$edges, stringsAsFactors = FALSE)
  if ("follower" %in% names(ed) && !"Follower" %in% names(ed))
    names(ed)[names(ed) == "follower"] <- "Follower"
  if (!all(c("Leader", "Follower", "WCD") %in% names(ed)))
    stop("The FLCA value tree needs Leader, Follower, and WCD edges.", call. = FALSE)
  junction_edges <- ed
  names(junction_edges)[names(junction_edges) == "Follower"] <- "follower"
  nd$carac <- unname(junction_flca_membership(nd, junction_edges)[nd$name])
  g <- igraph::graph_from_data_frame(
    data.frame(from = ed$Leader, to = ed$Follower, WCD = ed$WCD),
    directed = TRUE, vertices = data.frame(name = nd$name))
  labels <- igraph::V(g)$name
  values <- nd$value[match(labels, nd$name)]
  clusters <- as.character(nd$carac[match(labels, nd$name)])
  colors <- make_cluster_color_map(sort(unique(clusters)), "Set3")
  roots <- setdiff(labels, ed$Follower)
  layout <- igraph::layout_as_tree(g, root = match(roots, labels), mode = "out")
  top <- nd$name[order(-nd$value, nd$name, method = "radix")[1L]]
  scaled <- pmax(values - min(values) + 1, 1)
  igraph::plot.igraph(
    g, layout = layout, vertex.label = labels,
    vertex.color = unname(colors[clusters]),
    vertex.frame.color = ifelse(labels == top, "red", "grey30"),
    vertex.frame.width = ifelse(labels == top, 3, 1),
    vertex.size = 10 + 20 * sqrt(scaled / max(scaled)),
    vertex.label.cex = if (length(labels) > 30L) .55 else .7,
    edge.arrow.size = .35, edge.width = 1,
    edge.color = "grey55", edge.label = sprintf("%.2f", ed$WCD),
    edge.label.cex = .6, edge.label.color = "#274C91",
    main = sprintf("AAC(value): FLCA strongest-link tree of %d selected nodes",
                   length(labels)))
  graphics::mtext(sprintf("Node colors: junction clusters | edge labels: %s strength | red border: highest nodes$value",
                          weight_label), side = 1, line = 1.2, cex = .75,
                  col = "grey25")
  invisible(NULL)
}

make_flca_sankey_data <- function(xres, target_n = 20) {
  # Keep all selected nodes on both sides; use Step 3's reduced edge table
  # for both the Sankey plot and the SankeyMATIC code.
  bun <- make_flca_process_top_bundle(xres, target_n = target_n)
  nd <- as.data.frame(bun$nodes[, c("name", "carac", "value", "value2"), drop = FALSE],
                      stringsAsFactors = FALSE)
  nd$display_cluster <- unname(bun$display_membership[nd$name])
  ed <- bun$edges_reduced
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
  group_colors <- make_cluster_color_map(nd$display_cluster, "Set3")
  nd$sankey_color <- unname(group_colors[as.character(nd$display_cluster)])
  link_text <- if (nrow(ed)) paste0(ed$Leader, " [", sprintf("%.2f", ed$WCD), "] ", ed$follower, " #000000") else character(0)
  color_text <- paste0(": ", nd$name, " ", nd$sankey_color)
  code <- paste(c(link_text, color_text), collapse = "\n")

  list(nodes = nd, edges = ed, code = code,
       edge_source = "Step 3 reduced Leader/follower/WCD edges")
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
  nd$lab <- sprintf("%s (%.2f; group %s)", nd$name, nd$value, nd$display_cluster)
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

  clusters <- sort(unique(nd$display_cluster))
  pal <- make_cluster_color_map(clusters, "Set3")
  node_col <- unname(pal[as.character(nd$display_cluster)])
  col_map <- setNames(node_col, nd$name)

  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar), add = TRUE)
  par(mar = c(4.5, 12, 4, 12), xpd = NA, family = "sans")
  plot(NA, xlim = c(0, 1), ylim = c(nrow(nd) + 1.2, 0), axes = FALSE, xlab = "", ylab = "",
       main = "Sankey diagram: selected Top-20 nodes and reduced links")

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
      graphics::text(0.50, mean(c(ed$y1[i], ed$y2[i])), labels = sprintf("%.2f", ed$WCD[i]),
                     cex = 0.62, col = "grey20")
    }
  } else {
    graphics::text(0.50, 0.75, "No links among the selected Top-20 nodes; blocks are still shown for consistency.", cex = 0.95, font = 2)
  }

  graphics::mtext("Both sides show the selected Top-20 nodes; links match the Step 3 reduced network and SankeyMATIC code.", side = 1, line = 1.2, cex = 0.85)
  graphics::mtext(paste0("Edge source: ", sdat$edge_source), side = 1, line = 2.2, cex = 0.78)
  invisible(NULL)
}

# ---- Chord diagram for FLCA Process -----------------------------------------
# The chord keeps the selected Top-N node table and the Step 3 reduced edge
# table, so its links agree with the network, dendrograph, and Sankey. Sector
# widths are based on node value/incident weight, so high-value leaders such as
# Germany, Mexico, and the United States remain visible even when some followers
# have few links inside the Top-20 set.
make_flca_chord_data <- function(xres, target_n = 20) {
  shiny::req(xres)
  bun <- make_flca_process_top_bundle(xres, target_n = target_n)
  nd <- as.data.frame(bun$nodes[, c("name", "carac", "value", "value2"), drop = FALSE], stringsAsFactors = FALSE)
  nd$display_cluster <- unname(bun$display_membership[nd$name])
  if (!nrow(nd)) stop("No final Top-N nodes are available for the chord diagram.", call. = FALSE)
  nd$name <- as.character(nd$name)
  nd$carac <- suppressWarnings(as.integer(nd$carac)); nd$carac[!is.finite(nd$carac)] <- 1L
  nd$value <- suppressWarnings(as.numeric(nd$value)); nd$value[!is.finite(nd$value)] <- 0
  nd$value2 <- suppressWarnings(as.numeric(nd$value2)); nd$value2[!is.finite(nd$value2)] <- 0
  top_names <- nd$name

  ed_top <- as.data.frame(bun$edges_reduced, stringsAsFactors = FALSE)
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
  # Chord widths must share the WCD scale with the ribbons. Using raw node
  # totals here (often thousands) makes 0-1 CCC links visually disappear.
  positive_incident <- nd$incident_wcd[nd$incident_wcd > 0]
  min_sector <- if (length(positive_incident))
    0.25 * stats::median(positive_incident) else 1
  nd$sector_size <- pmax(nd$incident_wcd, min_sector)

  clusters <- sort(unique(nd$display_cluster))
  pal <- make_cluster_color_map(clusters, "Set3")
  group_colors <- unname(pal[as.character(nd$display_cluster)])
  all_nodes <- as.data.frame(bun$input$sil_df, stringsAsFactors = FALSE)
  target_n <- suppressWarnings(as.integer(target_n))[1]
  if (!is.finite(target_n) || target_n <= 0L) target_n <- 20L
  max_per_cluster <- suppressWarnings(as.integer(xres$max_per_cluster %||% 4L))[1]
  if (!is.finite(max_per_cluster) || max_per_cluster <= 0L) max_per_cluster <- 4L
  selection_note <- sprintf("Showing %d of %d analysis nodes (Top-N limit %d; usual cluster cap %d).",
                            nrow(nd), nrow(all_nodes), target_n, max_per_cluster)
  target_name <- intersect(c("臺大醫院", "台大醫院"), as.character(all_nodes$name))[1]
  if (!is.na(target_name)) {
    if (target_name %in% nd$name) {
      selection_note <- paste(selection_note, target_name, "is included.")
    } else {
      candidate <- all_nodes[match(target_name, all_nodes$name), , drop = FALSE]
      rank_order <- order(-as.numeric(all_nodes$value), -as.numeric(all_nodes$value2),
                          -as.numeric(all_nodes$sil_width), as.character(all_nodes$name))
      rank <- match(target_name, as.character(all_nodes$name[rank_order]))
      shown_in_cluster <- sum(nd$carac == candidate$carac[1], na.rm = TRUE)
      selection_note <- paste(selection_note,
        sprintf("%s is in the analysis (value %.2f, overall value rank %d, cluster %s), but was excluded by Top-N selection; %d nodes from its cluster are shown.",
                target_name, as.numeric(candidate$value[1]), rank,
                as.character(candidate$carac[1]), shown_in_cluster))
    }
  } else {
    selection_note <- paste(selection_note,
      "台大醫院/臺大醫院 is absent from the current analysis input.")
  }
  list(
    nodes = nd,
    edges = ed_top,
    matrix_real = mat_real,
    group_colors = group_colors,
    edge_source = "Step 3 reduced Leader/follower/WCD edges",
    selection_note = selection_note,
    note = sprintf("Chord uses the same %d selected nodes as SSplot, Kano, network, and Sankey; ribbons are the Step 3 reduced WCD edges. Sector width follows incident WCD; FLCA leadership still follows nodes$value.", nrow(nd))
  )
}

make_flca_chord_widget <- function(chd) {
  if (!requireNamespace("chorddiag", quietly = TRUE)) return(NULL)
  ed <- chd$edges
  if (!nrow(ed)) return(NULL)
  m <- chd$matrix_real
  if (!is.matrix(m) || !any(m > 0)) return(NULL)
  dimnames(m) <- list(have = rownames(m), prefer = colnames(m))
  chorddiag::chorddiag(m, type = "directional",
                       groupColors = chd$group_colors,
                       groupnamePadding = 20,
                       groupnameFontsize = 12,
                       groupThickness = 0.12,
                       showTicks = FALSE,
                       showTooltips = TRUE,
                       fadeLevel = 0.12,
                       margin = 110)
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
    sector_pos <- setNames(rep(0, nrow(nd)), nd$name)
    # Each endpoint gets a nonoverlapping interval on the same WCD scale.
    for (i in seq_len(nrow(ed))) {
      a <- ed$Leader[i]; b <- ed$follower[i]; w <- ed$WCD[i]
      xa1 <- sector_pos[a]; xa2 <- xa1 + w; sector_pos[a] <- xa2
      xb1 <- sector_pos[b]; xb2 <- xb1 + w; sector_pos[b] <- xb2
      circlize::circos.link(a, c(xa1, xa2), b, c(xb1, xb2),
                            col = grDevices::adjustcolor(grid_col[a], alpha.f = 0.62), border = NA)
    }
  }
  graphics::title("Chord diagram: selected nodes and Step 3 reduced links", cex.main = 1.05)
  invisible(NULL)
}

nurseratio_flow_diagram_ui <- function() {
  tags$div(class = "ccc-flow",
    tags$div(class = "ccc-flow-step",
      tags$strong("1 · CSV time series"),
      tags$span("Each row is a hospital or other node; numeric columns are aligned periods. Profile is ignored.")),
    tags$div(class = "ccc-flow-arrow", "↓"),
    tags$div(class = "ccc-flow-step",
      tags$strong("2 · Compare every pair"),
      tags$span("Lin's CCC measures agreement in both pattern and level across the same periods (−1 to +1).")),
    tags$div(class = "ccc-flow-arrow", "↓"),
    tags$div(class = "ccc-flow-step",
      tags$strong("3 · Build the network"),
      tags$span("Keep pairs with CCC > 0; the CCC value is the link weight. FLCA computes clusters from this network.")),
    tags$div(class = "ccc-flow-arrow", "↓"),
    tags$div(class = "ccc-flow-step",
      tags$strong("4 · Inspect the selected nodes"),
      tags$span("Step 2 shows all positive pairwise links. Step 3 keeps each displayed node's strongest CCC link."))
  )
}

taipei_mrt_inputs <- taipei_mrt_default_inputs()

ui <- fluidPage(
  titlePanel("FLCA / Community Comparison Demo — V55 quick-glance Golden Boot zoom"),
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
    input[name='input_mode'][value='demo_nurseratio'] + span {
      color: #c00000 !important;
      font-weight: 800 !important;
    }
    .ccc-flow { margin: 12px 0 16px; padding: 11px; background: #f2f7fc;
                border: 1px solid #c4d5e5; border-radius: 9px; }
    .ccc-flow-step { background: #fff; border-left: 4px solid #286a9d;
                     border-radius: 5px; padding: 8px 10px; }
    .ccc-flow-step strong { display: block; color: #174d75; font-size: 13px; }
    .ccc-flow-step span { display: block; margin-top: 3px; font-size: 12px;
                          line-height: 1.4; color: #263746; }
    .ccc-flow-arrow { text-align: center; color: #286a9d; font-size: 20px;
                      line-height: 1.2; font-weight: bold; }
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
          "Taipei MRT pasted nodes and edges" = "taipei_mrt",
          "U.S. airports demo" = "demo_us_airports",
          "Use demo CSV: dataset1.csv" = "demo_csv",
          "Use demo nurse ratios: nurseratio.csv (Lin's CCC)" = "demo_nurseratio",
          "Use demo gene clusters: genecluster5.csv (Lin's CCC)" = "demo_genecluster_ccc",
          "Use demo FIFA 2026 from bundled XLSX" = "demo_fifa2026_xlsx",
          "Update FIFA 2026 online from Wikipedia and run" = "fifa2026_online",
          "Use demo NBA 2025-2026 from nba.xlsx" = "demo_nba2025_2026",
          "Upload xlsx files with nodes and edges spreadsheet" = "upload",
          "Upload CSV with coword or edge-list data" = "upload_coword"
        ),
        selected = "demo_csv"),
      actionButton("select_demo_csv", "Select demo CSV: dataset1.csv", class = "btn-info"),
      br(), br(),
      actionButton("run_nurseratio_ccc", "Run selected Lin's CCC data", class = "btn-success"),
      br(), br(),
      actionButton("run_genecluster_ccc", "Run gene cluster network (Lin's CCC)", class = "btn-success"),
      br(), br(),
      actionButton("run_fifa2026_xlsx", "Run FIFA 2026 demo from bundled XLSX", class = "btn-success"),
      br(), br(),
      actionButton("update_fifa2026_online", "Update FIFA 2026 online and run", class = "btn-warning"),
      br(), br(),
      actionButton("run_nba2025_2026", "Run NBA 2025-2026 demo from nba.xlsx", class = "btn-success"),
      br(), br(),
      radioButtons("flca_value_source", "FLCA nodes$value for repeated value columns",
                   choices = c("Total score (sum of observations)" = "total",
                               "Derived total (incident CCC or WGCNA adjacency)" = "derived",
                               "Cluster by Edge (weighted Louvain)" = "edge"),
                   selected = "total"),
      helpText("All three choices use the same FLCA parent-link and sampling rules. Total score ranks nodes by summed observations; Derived total ranks repeated-value nodes by incident CCC or WGCNA adjacency; Cluster by Edge uses total score for FLCA and adds a separate weighted Louvain result on the full edge network. Ordinary node/edge inputs keep their supplied node values. Choose the input data separately above; for bundled TaipeiMRT.csv, click Run TaipeiMRT below."),
      actionButton("run", "Run FLCA", class = "btn-primary"),
      br(), br(),
      actionButton("run_wgcna", "Run WGCNA", class = "btn-success"),
      numericInput("wgcna_power", "WGCNA soft-threshold power", value = 6, min = 1, max = 30, step = 1),
      fileInput("wgcna_values_file",
                tags$span(style = "color:#c00000;font-weight:700;",
                          "1. Optional WGCNA value-matrix CSV"),
                accept = c(".csv", "text/csv")),
      helpText("This optional file is only for WGCNA repeated values; it does not supply FLCA edges. Nurse-ratio and gene modes use their selected source CSV automatically. For other modes, upload a CSV with a node-name first column and at least four numeric observation columns."),
      p("Default input is demo CSV. V43 restores the red-line FIFA Golden Boot slopegraph and keeps original Top-20 nodes on both term1/Leader and term2/follower sides of the Sankey plot."),
      hr(),
      h4("Download demo data"),
      downloadButton("download_demo_dataset1", "Download demo CSV: dataset1.csv"),
      br(), br(),
      downloadButton("download_demo_nurseratio", "Download nurse ratios CSV"),
      br(), br(),
      downloadButton("download_demo_genecluster", "Download gene cluster CSV"),
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
      conditionalPanel("input.input_mode == 'demo_nurseratio'",
        radioButtons("ccc_data_type", "Lin's CCC data type",
                     choices = c("Nurse ratios: nurseratio.csv" = "nurse",
                                 "Gene clusters: genecluster5.csv" = "gene"),
                     selected = "nurse")
      ),
      conditionalPanel("input.input_mode == 'demo_nurseratio' && input.ccc_data_type == 'nurse'",
        fileInput("nurseratio_file", "Browse for a nurse ratios CSV",
                  accept = c(".csv", "text/csv")),
        textInput("nurseratio_url", "Online GitHub CSV URL", value = "",
                  placeholder = "https://github.com/owner/repo/blob/main/data.csv"),
        actionButton("run_nurseratio_url", "Run online GitHub CSV", class = "btn-info"),
        helpText("A GitHub file-page URL is converted to its raw CSV automatically. Open the app with ?url=<URL-encoded GitHub CSV URL> for immediate analysis."),
        helpText("Hospitals are nodes; the remaining numeric columns are paired monthly or quarterly observations. Step 2 uses positive Lin's CCC as edge weight. Step 3 retains each hospital's strongest CCC link among displayed nodes. The Profile column is ignored; clusters are computed from the CCC network."),
        h5("How Lin's CCC mode works"),
        nurseratio_flow_diagram_ui(),
        helpText("Choose a CSV above, then click Run FLCA, Run WGCNA, or Run nurse ratio demo. The uploaded file takes priority. Without an upload, the GitHub demo is used; if it is unavailable, the bundled nurseratio.csv is used. The source appears in the run status."),
        tags$a(href = paste0("?url=", utils::URLencode(nurseratio_github_url, reserved = TRUE)),
               "Open this nurse-ratio example and run it")
      ),
      conditionalPanel("input.input_mode == 'demo_genecluster_ccc' || (input.input_mode == 'demo_nurseratio' && input.ccc_data_type == 'gene')",
        fileInput("genecluster_file", "Optional gene cluster CSV", accept = c(".csv", "text/csv")),
        radioButtons("genecluster_node_type", "Nodes for the main FLCA Process",
                     choices = c("Gene rows (top 20)" = "genes",
                                 "GSM sample columns (top 20)" = "samples"),
                     selected = "genes"),
        textAreaInput("genecluster_value_columns", "SampleID and Condition for Lin's CCC columns",
                      value = genecluster_sample_condition_default, rows = 12, width = "100%"),
        helpText("Edit the tab-separated SampleID / Condition table. Every SampleID must match a GSM observation column; Condition must be Control or Case. At least 20 samples are required. Gene, logFC, log10P, and the final four summary columns are excluded."),
        selectInput("gene_log10p_group", "log10P group", choices = "1"),
        actionButton("gene_previous_group", "Previous group"),
        actionButton("gene_next_group", "Next group"),
        helpText("Click Run FLCA to generate both node types for every log10P group, or Run WGCNA for the selected group and node type. Gene rows use validated sample columns as paired observations; GSM sample columns use gene rows as observations. Choose a group or use Previous/Next to view its completed network.")
      ),
      conditionalPanel("input.input_mode == 'demo_fifa2026_xlsx'",
        h5("FIFA 2026 bundled XLSX demo"),
        helpText("Reads fifa_2026_updated_nodes_edges.xlsx from the app folder. The workbook should contain sheets named nodes and edges."),
        downloadButton("download_fifa2026_xlsx", "Download current FIFA 2026 bundled XLSX")
      ),
      conditionalPanel("input.input_mode == 'fifa2026_online'",
        h5("REAL online FIFA 2026 update"),
        helpText("Click Run FLCA or Update FIFA 2026 online and run. A Shiny progress scale appears while the 12 Wikipedia Group A-L pages plus the knockout-stage page are fetched, parsed, converted to nodes/edges, saved to XLSX, and analyzed."),
        helpText("This mode reads current online Wikipedia pages and does not use the bundled XLSX fallback. It includes 48-team group-stage results plus all completed knockout matches when available online."),
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
        fileInput("coword_file",
                  tags$span(style = "color:#c00000;font-weight:700;",
                            "2. CSV/TXT/TSV: edge list (A1,A2 or Leader,Follower,WCD), coword, or TAAA"),
                  accept = c(".csv", "text/csv", ".txt", ".tsv", "text/tab-separated-values")),
        helpText("TaipeiMRT.csv is an A1/A2 edge list; routes without WCD receive 1.0. Row-wise TAAA examples are Row/主題名稱/主題詞頻/Cluster or A1-A8 term cells. Long doc,term and wide numeric matrices are also supported."),
        tags$div(style = "background:#e7f7e8; border:1px solid #83c985; border-radius:7px; padding:10px; margin-top:12px;",
          h5("Coword Kano: two-column author CSV"),
          fileInput("coword_kano_file", "First author / corresponding author CSV", accept = c(".csv", "text/csv")),
          actionButton("run_coword_kano", "Run Coword Kano", class = "btn-primary"),
          helpText("Uses exactly two columns: first author, then corresponding author. Column names are optional. This does not run the main FLCA analysis; its dendrograph uses strongest links from the two-column author graph.")
        )
      ),
      numericInput("target_k", "Target k for independent components", value = 4, min = 1, step = 1),
      h4("FLCA sampling order"),
      tags$ol(
        tags$li(tags$strong("Top-1 cluster first. "),
                "Rank nodes by descending value, then ascending name. Keep its highest-ranked one to four members; always retain the Top-1 node."),
        tags$li(tags$strong("Qualified leader clusters next. "),
                "A candidate leader needs at least two direct followers (at least three nodes in its group). Prefer other clusters supplying three or four nodes, ordered by their highest-ranked member."),
        tags$li(tags$strong("Small root components last. "),
                "One- or two-node components do not have a qualified leader. Use them only if the sample remains below the target; keep the final chosen component whole.")
      ),
      helpText("A single-cluster network keeps the top target number of nodes before splitting them into two display groups. The target is normally 20; multiple clusters can yield 20 to 23 selected nodes."),
      tags$details(
        tags$summary("Advanced parent-link tie-break for alternate analyses"),
        radioButtons("flca_mode", "Equal-WCD leader choice",
                     choices = c("Alphabetical leader name" = "edge",
                                 "Higher leader node value, then name" = "node"),
                     selected = "edge"),
        helpText("Only equal-WCD links are affected. Ordinary node/edge inputs use the shared FLCA core, which chooses the higher-ranked leader and does not use this advanced setting.")
      ),
      h4("Reviewer-required parameter settings"),
      numericInput("occurrence_top_n", "Occurrence input: top N nodes before edge generation", value = 100, min = 2, step = 1),
      numericInput("flca_target_nodes", "FLCA-MA final target nodes", value = 20, min = 1, step = 1),
      numericInput("sil_intra_penalty", "Silhouette missing intra-cluster penalty", value = 2, min = 0, step = 1),
      numericInput("sil_inter_penalty", "Silhouette missing inter-cluster penalty", value = 5, min = 0, step = 1),
      numericInput("tie_break_scale", "Tie-break scale: leader score / scale", value = 10000, min = 1, step = 100),
      p("These controls expose FLCA parameters requested by reviewers and are exported in the Parameters tab."),
      sliderInput("label_size", "Network label font size", min = 10, max = 50, value = 30, step = 1),
      checkboxInput("label_bold", "Bold network labels", value = TRUE),
      br(), tags$label("Run status"), verbatimTextOutput("run_status"),
      hr(),
      tags$div(class = "taipei-mrt-input-panel",
        style = "background:#e9f7ec; border:2px solid #78b88a; border-radius:10px; padding:14px; margin:12px 0 18px;",
        h4(style = "margin-top:0;", "Taipei MRT input"),
        checkboxInput("taipei_use_nodes", "Use node values from box (July 2026 defaults)",
                      value = TRUE),
        textAreaInput("taipei_nodes_input", "Node data: name value [value2]",
                      value = taipei_mrt_inputs$nodes, rows = 9, width = "100%"),
        textAreaInput("taipei_edges_input", "Edge data: follower leader [WCD]",
                      value = taipei_mrt_inputs$edges, rows = 9, width = "100%"),
        helpText("Uncheck node values to derive them from the current edges. Edit either box before running."),
        actionButton("run_taipei_mrt", "Run TaipeiMRT", class = "btn-primary"),
        br(), br(),
        actionButton("run_us_airports", "Run U.S. airports demo", class = "btn-success")
      )
    ),
    mainPanel(width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("FIFA 2026", value = "fifa2026", br(),
          h3("FIFA 2026 analysis and visuals"),
          p("Run the bundled workbook or update from Wikipedia, then review group performance, prediction path, SS/Q, and Golden Boot results here."),
          actionButton("run_fifa2026_tab", "Run bundled FIFA 2026", class = "btn-success"),
          actionButton("update_fifa2026_tab", "Update FIFA 2026 online and run", class = "btn-warning"),
          br(), br(),
          h4("FIFA 2026 group performance summary"),
          p("For the online FIFA update, this table summarizes each group at the reporting date: Top 1 on the left, then the next three teams to the right, with points in parentheses."),
          uiOutput("fifa_group_summary_date_main"),
          DTOutput("fifa_group_summary_main"),
          hr(),
          h4("FIFA 2026 prediction path by stage"),
          p("Generated when clicking Update FIFA 2026 online and run. Round of 32 match-pair labels follow the official/current Wikipedia slot template exactly; prediction mode is used only to resolve slot-based teams internally and to predict winners. Prediction = 0.7 × Table 1 current points + 0.3 × mixed performance prior; the seed controls reproducible randomization."),
          uiOutput("fifa_prediction_seed_note"),
          actionButton("run_fifa_prediction_plot", "Run FIFA 2026 prediction plot", class = "btn-primary"),
          actionButton("download_fifa_prediction_path_png_client", "Download prediction path PNG", icon = icon("download")),
          actionButton("download_fifa_prediction_ssq_png_client", "Download SS/Q PNG", icon = icon("download")),
          downloadButton("download_fifa_prediction_ssq_plot_png", "Download SS/Q plot PNG"),
          br(), br(),
          DTOutput("fifa_prediction_metrics_table"),
          h4("Round of 32 official fixture check"),
          DTOutput("fifa_r32_official_check_table"),
          DTOutput("fifa_prediction_path_table"),
          tags$div(style="overflow-x:auto; width:100%;", tags$div(id = "fifa_prediction_path_capture", style="display:inline-block; width:max-content; max-width:none; overflow:visible;", uiOutput("fifa_prediction_path_html"))),
          h4("FIFA 2026 vertical knockout tree"),
          p("The Top-1 team sits at the top. Follow the quarterfinalists upward through the semifinals to the final; the footer shows third and fourth place."),
          plotOutput("fifa_prediction_vertical_tree", height = "620px"),
          downloadButton("download_fifa_prediction_vertical_tree_png", "Download vertical tree PNG"),
          h4("FIFA 2026 FLCA strongest-link tree"),
          p("The same FLCA dendrograph shown in FLCA Process, using the current FIFA node values and weighted match edges. Displayed clusters run C1, C2, ... from left to right by their highest-value member; nodes within each cluster descend by value. The workbook's original group IDs are retained in the analysis. This network leader is separate from the knockout champion shown above."),
          plotOutput("fifa_prediction_flca_tree", height = "900px"),
          h4("FIFA 2026 Kano plot"),
          p("The same node-level Kano plot shown in FLCA Process: value2 on the horizontal axis, value on the vertical axis, and bubble size from structural openness (1-C) on the selected FIFA network. Its Top-1 follows node value, which may differ from the knockout champion."),
          plotOutput("fifa_prediction_kano", height = "900px", width = "100%"),
          downloadButton("download_fifa_prediction_kano_png", "Download FIFA Kano PNG"),
          tags$div(id = "fifa_prediction_ssq_capture",
                   plotOutput("fifa_prediction_ssq_plot", height = "520px"),
                   uiOutput("fifa_prediction_ssq_html")),
          hr(),
          h4("FIFA 2026 Golden Boot chase"),
          p("For the online FIFA update, scorer lists are parsed from Wikipedia footballbox scorer fields when available. The table reports goal totals; the slopegraph tracks cumulative goals by parsed match order."),
          uiOutput("fifa_golden_boot_date_main"),
          DTOutput("fifa_golden_boot_main"),
          actionButton("run_fifa_golden_boot_plot", "Draw Golden Boot slopegraph", class = "btn-warning"),
          actionButton("download_fifa_golden_boot_png_client", "Download Golden Boot PNG", icon = icon("download")),
          br(), br(),
          tags$div(style="overflow-x:hidden; width:100%; max-width:100%;", tags$div(id = "fifa_golden_boot_capture", style="display:block; width:100%; max-width:100%; overflow:hidden;", uiOutput("fifa_golden_boot_slope_html"))),
          uiOutput("fifa_golden_boot_plot_note_main"),
          hr()
        ),
        tabPanel("Data", br(), h4("Data mode"), verbatimTextOutput("data_mode"),
          uiOutput("genecluster_data_networks_ui"),
          DTOutput("tbl_nodes"), br(), DTOutput("tbl_edges")),
        tabPanel("Coword Kano", br(),
          h4("Coword author Kano plot"),
          p("Upload a two-column first-author/corresponding-author CSV in the sidebar and click Run Coword Kano. The plots use the 20 most frequent authors."),
          verbatimTextOutput("coword_kano_status"),
          plotOutput("coword_kano_plot", height = "900px", width = "100%"),
          h4("First-author/corresponding-author FLCA dendrograph"),
          p("Each author follows the strongest coauthorship link among the displayed authors; link labels show the number of shared records."),
          plotOutput("coword_kano_dendrogram", height = "900px", width = "100%"),
          DTOutput("coword_kano_table")),        tabPanel("Full Network", br(), fluidRow(column(5, selectInput("full_method", "Clustering algorithm", choices = community_methods, selected = "louvain")), column(4, sliderInput("full_top_n", "Show top N nodes", min = 5, max = 100, value = 30, step = 1)), column(3, checkboxInput("full_use_all", "Show all nodes", value = FALSE))), visNetworkOutput("plot_full", height = "720px")),
        tabPanel("FLCA Reduced", br(), fluidRow(column(4, checkboxInput("flca_major_sampling", "Use FLCA-MA major sampling", value = TRUE)), column(4, sliderInput("flca_top_clusters", "Top clusters", min = 1, max = 20, value = 6, step = 1)), column(4, sliderInput("flca_n_per_cluster", "At least N nodes per top cluster", min = 1, max = 20, value = 3, step = 1))), visNetworkOutput("plot_flca", height = "720px"), br(), h4("Selected reduced edges used by network and tree"), p("Leader is the highest-value component root or a node downstream from that root; follower is the next node along the link. WCD is the original edge weight."), DTOutput("tbl_reduced")),
        tabPanel(title = tags$span(class = "tab-red", "FLCA Process"), value = "flca_process", br(),
          h3("FLCA process with optional TAAA ToT add-on"),
          p("The input network appears in Data and Full Network. FLCA Process displays the same selected nodes in Step 2 and the strongest-link network in Step 3. Row-wise coword/TAAA uploads also show TAAA ToT networks below."),
          verbatimTextOutput("flca_process_edge_source"),
          h4("Step 1. Original input nodes and weighted edges"),
          fluidRow(column(6, DTOutput("flca_step1_nodes")), column(6, DTOutput("flca_step1_edges"))),
          h4("Step 2. Weighted input network"),
          visNetworkOutput("flca_step2_full_graph", height = "560px"),
          downloadButton("download_flca_step2_png", "Download Step 2 network PNG"),
          br(), br(),
          h4("Step 3. FLCA strongest-link network"),
          visNetworkOutput("flca_step3_reduced_graph", height = "560px"),
          downloadButton("download_flca_step3_png", "Download Step 3 one-edge network PNG"),
          h5("Step 3 reduced edges used by the network and tree"),
          p("The first column is Leader, the second is follower, and WCD is unchanged. Directions follow the highest-value node in each connected component."),
          DTOutput("flca_step3_reduced_edges"),
          uiOutput("gene_row_flca_networks_ui"),
          br(), br(),
          h4("FLCA leader–follower dendrograph"),
          p("Step 3 reduced links determine tree relationships. Nodes run left to right by cluster, descending value, then name. The top solid red line is a graphical stem. Only the Top-1 cluster joins it with a solid red connector. Dashed grey connectors show that other clusters are disconnected from the stem and carry no WCD. Red vertical lines within each cluster mark cluster leaders, grey horizontal lines mark subbranches, and grey vertical lines mark leaves. Purple X marks qualifying junctions and cluster leaders with one child. Top-1 and Top-3 identities follow nodes$value, with name breaking ties. AAC(count) uses those nodes' incident-link frequency; for Lin's CCC data, AAC(value) uses their summed incident CCC weights. Mean SS and overall Qw/Qu appear above the tree and are calculated from reduced FLCA links. Cluster SS, Qw, and Qu appear beneath node values. Top-1 SHI uses the selected full original-edge network and appears at the far left beneath the node values."),
          plotOutput("flca_process_dendrogram", height = "900px"),
          uiOutput("flca_value_vertical_tree_ui"),
          br(), br(),
          h4("Step 4. Connected-component leader/follower summary"),
          DTOutput("flca_step4_cluster_summary"),
          uiOutput("edge_clustering_flca_ui"),
          h4("Step 5b. Sankey plot and SankeyMATIC code: same final Top-20 nodes"),
          p("The Sankey plot keeps the selected Top-N nodes on both sides. Each SankeyMATIC row is Leader [WCD] follower #000000; reduced links are directed away from the highest-value node in their connected component."),
          plotOutput("flca_step5_sankey_plot", height = "760px"),
          downloadButton("download_flca_step5_sankey_png", "Download Step 5b Sankey PNG"),
          h5("SankeyMATIC code"),
          verbatimTextOutput("flca_step5_sankeymatic_code"),
          h4("Step 6. Real SSplot cluster summary using renderSSplot(82).R"),
          plotOutput("flca_step6_ssplot", height = "920px"),
          downloadButton("download_flca_step6_ssplot_png", "Download Step 6 SSplot PNG"),
          br(), br(),
          h4("Step 6b. Wide Kano plot under SSplot: value on y-axis and value2 on x-axis"),
          p("Kano Top-1 and Top-3 identities follow nodes$value, with name breaking ties. AAC(count) uses their incident-link frequency. For Lin's CCC inputs, AAC(value) uses their summed incident CCC edge weights. Node value and value2 remain the plot axes."),
          plotOutput("flca_step6_kano", height = "1100px", width = "100%"),
          downloadButton("download_flca_step6_kano_png", "Download Step 6b Kano PNG"),
          h4("Step 6b Kano nodes: bubble metric for each displayed element"),
          p("Actual 1-C (%) is mapped to Kano bubble area; it is not min-max normalized, and these are the exact same Top-N rows used by the plot."),
          DTOutput("flca_step6_kano_nodes"),
          br(),
          h4("Step 6c. Interactive chord dashboard: final Top-20 FLCA nodes and edges"),
          p("The chord diagram uses the selected Top-N nodes and Step 3 reduced edges. Colors show connected components; ribbon and sector widths use WCD, while FLCA leader rank uses node value. Hover over a ribbon in the interactive view to inspect its link."),
          verbatimTextOutput("flca_step6c_chord_note"),
          uiOutput("flca_step6c_chord_ui"),
          h4("Step 6c chord nodes: same final Top-N rows as SSplot"),
          DTOutput("flca_step6c_chord_nodes"),
          h4("Step 6c chord edges"),
          DTOutput("flca_step6c_chord_edges"),
          br(),
          downloadButton("download_flca_step6c_chord_png", "Download Step 6c chord PNG"),
          downloadButton("download_flca_step6c_chord_edges", "Download Step 6c chord edges CSV"),
          hr(),
          h3("Additional coword TAAA ToT networks"),
          p("These two plots appear only when the uploaded coword CSV is row-wise TAAA format, for example with 主題名稱, 主題詞頻, and Cluster columns. They are added here without replacing the original coword Step 2 graph or the ordinary FLCA Step 3 graph."),
          verbatimTextOutput("flca_taaa_tot_bottom_note"),
          h4("Coword TAAA ToT full-edge network"),
          visNetworkOutput("flca_taaa_tot_full_graph", height = "560px"),
          br(),
          h4("Coword TAAA ToT one-edge leader→follower network"),
          visNetworkOutput("flca_taaa_tot_one_graph", height = "560px"),
          br(),
          downloadButton("download_flca_taaa_tot_full_png", "Download TAAA ToT full-edge PNG"),
          downloadButton("download_flca_taaa_tot_one_png", "Download TAAA ToT one-edge PNG"),
          br(), br(),
          downloadButton("download_flca_process_all_png_zip", "Download all FLCA Process PNGs (.zip)")
        ),
        tabPanel("Memberships", br(), DTOutput("tbl_memberships"), br(), downloadButton("download_memberships", "Download memberships CSV")),
        tabPanel("WGCNA Results", value = "wgcna_results", br(),
          h3("WGCNA module results"),
          p("WGCNA runs on repeated value columns separately from FLCA. Module 0 is unassigned. SS, Qw, and Qu below use the WGCNA weighted adjacency and its module assignments."),
          verbatimTextOutput("wgcna_status"),
          h4("Summary"), tableOutput("wgcna_metrics"),
          h4("Module sizes"), DTOutput("wgcna_modules"),
          uiOutput("edge_clustering_wgcna_ui"),
          h4("Node membership and connectivity"), DTOutput("wgcna_nodes"),
          h4("WGCNA weighted network"),
          p("The network displays the strongest adjacency links among the highest-value nodes; node colors show WGCNA modules."),
          plotOutput("wgcna_network", height = "760px"),
          h4("WGCNA clustering tree"),
          p("The tree uses dissimilarity 1 − TOM. Leaf colors correspond to WGCNA modules."),
          plotOutput("wgcna_tree", height = "700px"),
          h4("Topological overlap"), plotOutput("wgcna_heatmap", height = "680px"),
          h4("TOM distance map (MDS)"),
          p("Each point is a node, colored by its WGCNA module. WGCNA's yellow module is displayed in dark purple for contrast on white. The map places nodes using 1 − TOM, so close points have similar network neighborhoods. Labels mark the 20 highest-value nodes; the map is a view of the same modules, not another clustering run."),
          plotOutput("wgcna_tom_mds", height = "720px"),
          h4("Soft-threshold power diagnostic"),
          p("The left panel shows signed scale-free topology fit (R²; a negative slope is expected), and the right panel shows mean connectivity as power changes. The red line marks the power used for this run. For small networks, these diagnostics are exploratory and should not be treated as proof of scale-free structure."),
          plotOutput("wgcna_power_diagnostic", height = "550px"),
          h3("FLCA strongest links from WGCNA adjacency"),
          p("The following plots apply FLCA to WGCNA adjacency: WCD = |bicor|^power, so a larger WCD is a stronger link. Total score or Derived total determines FLCA leader rank; Cluster by Edge uses Total score for its companion FLCA plots and reports separate Louvain communities above. Each follower keeps its highest-WCD link to a higher-ranked node. A raw candidate junction has at least two direct followers. A displayed junction group with fewer than three members first absorbs its strongest child-junction group; if none exists, it merges upward along its parent chain. The top root remains a leader. These display merges do not alter the strongest-link tree. The clusters are distinct from WGCNA modules. The WGCNA dendrogram uses distance 1 − TOM, which is not passed to FLCA as WCD. The plots show up to the Top-50 FLCA candidate nodes."),
          h4("Qualified junction-based FLCA clusters"), tableOutput("wgcna_flca_clusters"),
          h4("Reduced-edge FLCA network"),
          plotOutput("wgcna_flca_network", height = "760px"),
          h4("FLCA strongest-link tree"),
          plotOutput("wgcna_flca_tree", height = "760px"),
          h4("FLCA junction dendrograph"),
          p("Horizontal leader-follower tree with value-ranked nodes, colored junction clusters, WGCNA adjacency labels, and reduced-tree SS/Q. This follows the FLCA Process dendrograph layout."),
          plotOutput("wgcna_flca_dendrograph", height = "940px"),
          downloadButton("download_wgcna_nodes", "Download WGCNA nodes CSV"),
          hr(),
          h3("WGCNA adjacency chord diagram"),
          p("Interactive WGCNA connections among up to 30 highest-value nodes. The diagram retains each node's two strongest adjacency links within that set; colors show WGCNA modules. WGCNA links are undirected: the matrix's upper-triangle orientation is only for display, not a leader-follower direction."),
          verbatimTextOutput("wgcna_chord_note"),
          uiOutput("wgcna_chord_ui")),
        tabPanel("Quality", br(), h4("Clustering comparison on identical selected nodes"),
          p("For ordinary FLCA input, all comparable methods run on the same FLCA-selected 20–23 nodes and their Q and SS use all original induced edges. The additional FLCA reduced-tree row is a plot reference and is excluded from the fair ranking and Visual Quality plot. Skipped methods have no score."),
          verbatimTextOutput("flca_mode_annotation"),
          uiOutput("quality_method_explanation"),
          DTOutput("tbl_quality"), br(),
          downloadButton("download_quality", "Download quality CSV")),
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
        tabPanel(title = tags$span(class = "tab-red", "ReadMe"), br(), h3("ReadMe: FLCA and community clustering comparison app"), p("V34 preserves the original-style sidebar and colors only four main tabs red: FLCA Process, Visual Quality, Parameters, and ReadMe. Demo CSV reads dataset1.csv; FIFA 2026 bundled demo reads fifa_2026_updated_nodes_edges.xlsx; FIFA online update really fetches Wikipedia Group A–L pages at run time, rebuilds that workbook, parses Golden Boot scorer fields when available, and runs analysis; NBA 2025-2026 demo reads nba.xlsx. Excel upload reads nodes/edges workbooks; CSV coword upload converts document-term data to nodes and weighted edges. Row-wise TAAA tables with 主題詞頻 retain the original coword network for the ordinary network tabs and add TAAA ToT full-edge plus one-edge networks only under FLCA Process. FLCA Process Step 6 shows the real SSplot and Step 6b shows a wider node-level Kano plot with value on the y-axis and value2 on the x-axis. The Visual Quality tab separately shows algorithm quality with modularity Q on x and mean silhouette SS on y.")),
        tabPanel("Deploy Notes", br(), h4("shinyapps.io deployment"), pre("install.packages(c('shiny','DT','igraph','visNetwork','RColorBrewer','ggplot2','ggrepel','dplyr','tibble','readxl','openxlsx','httr2','rvest','xml2','stringr','tidyr','purrr'), type='binary')\nrsconnect::deployApp(appDir='F:/taaforgae/zWoSPubmed/flcacompare', appName='flcacompare', forceUpdate=TRUE)"))
      )
    )
  )
)


# ---- FIFA 2026 prediction path: Table 1 + mixed prior + seed -----------------
# ---- FIFA 2026 prediction path: Table 1 + mixed prior + seed -----------------
make_fifa_prediction_metrics <- function() {
  # The matches column follows the official knockout match numbers/cumulative
  # match count: semifinal matches are 101-102, third-place match is 103,
  # and the final is 104.
  tibble::tribble(
    ~stage, ~matches, ~SS, ~Q,
    "Stage 48", 48, 0.6200, 0.9600,
    "Group stage", 72, 0.3846, 0.8988,
    "Round of 32", 88, 0.3441, 0.7413,
    "Round of 16", 96, 0.3346, 0.6857,
    "Quarterfinals", 100, 0.3323, 0.6682,
    "Semifinals", 102, 0.3255, 0.6409,
    "Third-place match", 103, 0.3255, 0.6365,
    "Final", 104, 0.3255, 0.6330
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
    "Cape Verde",47,48,32, "Cabo Verde",47,48,32,
    "Curacao",46,47,30, "Curaçao",46,47,30
  ) |>
    dplyr::mutate(
      Elo_norm = as.numeric(scale(elo)),
      fifa_norm = as.numeric(scale(fifa_rank_score)),
      history_norm = as.numeric(scale(wc_history)),
      mixed_prior = 0.5 * Elo_norm + 0.3 * fifa_norm + 0.2 * history_norm
    ) |>
    dplyr::select(team, mixed_prior)
}

# Official bracket template from the current Wikipedia Round of 32 page and
# knockout-stage bracket.  Prediction mode predicts winners only; it must not
# re-seed teams and must not invent unofficial Round-of-32 pairings.
make_fifa_official_bracket_template <- function() {
  list(
    r32 = tibble::tribble(
      ~game, ~match_no, ~team1_slot, ~team2_slot,
       1L, 73L, "South Africa",        "Canada",
       2L, 76L, "Brazil",              "Japan",
       3L, 74L, "Germany",             "Paraguay",
       4L, 75L, "Netherlands",         "Morocco",
       5L, 78L, "Ivory Coast",         "Norway",
       6L, 77L, "France",              "Sweden",
       7L, 79L, "Mexico",              "3rd Group C/E",
       8L, 80L, "Winner Group L",      "3rd Group I/J/K",
       9L, 82L, "Belgium",             "3rd Group A/I/J",
      10L, 81L, "United States",       "Bosnia and Herzegovina",
      11L, 84L, "Spain",               "Runner-up Group J",
      12L, 83L, "Runner-up Group K",   "Runner-up Group L",
      13L, 85L, "Switzerland",         "3rd Group G/J",
      14L, 88L, "Australia",           "Egypt",
      15L, 86L, "Argentina",           "Cape Verde",
      16L, 87L, "Winner Group K",      "3rd Group E/I/L"
    ),
    r16 = tibble::tribble(
      ~game, ~match_no, ~source1, ~source2,
      1L, 90L, 73L, 75L,
      2L, 89L, 74L, 77L,
      3L, 91L, 76L, 78L,
      4L, 92L, 79L, 80L,
      5L, 93L, 83L, 84L,
      6L, 94L, 81L, 82L,
      7L, 95L, 86L, 88L,
      8L, 96L, 85L, 87L
    ),
    qf = tibble::tribble(
      ~game, ~match_no, ~source1, ~source2,
      1L, 97L, 89L, 90L,
      2L, 98L, 93L, 94L,
      3L, 99L, 91L, 92L,
      4L,100L, 95L, 96L
    ),
    sf = tibble::tribble(
      ~game, ~match_no, ~source1, ~source2,
      1L,101L, 97L, 98L,
      2L,102L, 99L,100L
    ),
    third = tibble::tribble(
      ~game, ~match_no, ~source1, ~source2,
      1L,103L,101L,102L
    ),
    final = tibble::tribble(
      ~game, ~match_no, ~source1, ~source2,
      1L,104L,101L,102L
    )
  )
}


# Completed knockout results are treated as authoritative for prediction.
# This prevents eliminated teams from being projected into later rounds.
make_fifa_completed_knockout_results <- function(matches_source = NULL) {
  hardcoded_results <- tibble::tribble(
    ~match_no, ~round, ~team1, ~team2, ~winner, ~loser, ~score, ~result_source,
    73L, "Round of 32", "South Africa", "Canada", "Canada", "South Africa", "0-1", "completed knockout result",
    74L, "Round of 32", "Germany", "Paraguay", "Paraguay", "Germany", "1-1; Paraguay won on penalties", "completed knockout result",
    75L, "Round of 32", "Netherlands", "Morocco", "Morocco", "Netherlands", "1-1; Morocco won on penalties", "completed knockout result",
    76L, "Round of 32", "Brazil", "Japan", "Brazil", "Japan", "2-1", "completed knockout result",
    77L, "Round of 32", "France", "Sweden", "France", "Sweden", "3-0", "completed knockout result",
    78L, "Round of 32", "Ivory Coast", "Norway", "Norway", "Ivory Coast", "1-2", "completed knockout result",
    79L, "Round of 32", "Mexico", "Ecuador", "Mexico", "Ecuador", "2-0", "completed knockout result",
    80L, "Round of 32", "England", "DR Congo", "England", "DR Congo", "2-1", "completed knockout result",
    81L, "Round of 32", "United States", "Bosnia and Herzegovina", "United States", "Bosnia and Herzegovina", "2-0", "completed knockout result",
    82L, "Round of 32", "Belgium", "Senegal", "Belgium", "Senegal", "3-2", "completed knockout result",
    83L, "Round of 32", "Portugal", "Croatia", "Portugal", "Croatia", "2-1", "completed knockout result",
    84L, "Round of 32", "Spain", "Austria", "Spain", "Austria", "3-0", "completed knockout result",
    85L, "Round of 32", "Switzerland", "Algeria", "Switzerland", "Algeria", "2-0", "completed knockout result",
    86L, "Round of 32", "Argentina", "Cape Verde", "Argentina", "Cape Verde", "3-2", "completed knockout result",
    87L, "Round of 32", "Colombia", "Ghana", "Colombia", "Ghana", "1-0", "completed knockout result",
    88L, "Round of 32", "Australia", "Egypt", "Egypt", "Australia", "1-1; Egypt won on penalties", "completed knockout result",
    89L, "Round of 16", "Paraguay", "France", "France", "Paraguay", "0-1", "completed knockout result",
    90L, "Round of 16", "Canada", "Morocco", "Morocco", "Canada", "0-3", "completed knockout result",
    91L, "Round of 16", "Brazil", "Norway", "Norway", "Brazil", "1-2", "completed knockout result; Brazil eliminated",
    92L, "Round of 16", "Mexico", "England", "England", "Mexico", "2-3", "completed knockout result",
    93L, "Round of 16", "Portugal", "Spain", "Spain", "Portugal", "0-1", "completed knockout result",
    94L, "Round of 16", "United States", "Belgium", "Belgium", "United States", "1-4", "completed knockout result",
    95L, "Round of 16", "Argentina", "Egypt", "Argentina", "Egypt", "3-2", "FIFA completed knockout result",
    96L, "Round of 16", "Switzerland", "Colombia", "Switzerland", "Colombia", "0-0; Switzerland won 4-3 on penalties", "FIFA completed knockout result",
    97L, "Quarterfinals", "France", "Morocco", "France", "Morocco", "2-0", "FIFA completed knockout result",
    98L, "Quarterfinals", "Spain", "Belgium", "Spain", "Belgium", "2-1", "FIFA completed knockout result",
    99L, "Quarterfinals", "Norway", "England", "England", "Norway", "1-2", "FIFA completed knockout result",
    100L, "Quarterfinals", "Argentina", "Switzerland", "Argentina", "Switzerland", "3-1", "FIFA completed knockout result",
    101L, "Semifinals", "France", "Spain", "Spain", "France", "0-2", "FIFA completed knockout result",
    102L, "Semifinals", "England", "Argentina", "Argentina", "England", "1-2", "FIFA completed knockout result",
    103L, "Third-place match", "France", "England", "England", "France", "4-6", "FIFA completed knockout result",
    104L, "Final", "Spain", "Argentina", "Spain", "Argentina", "1-0", "FIFA completed knockout result"
  )

  if (is.data.frame(matches_source) &&
      all(c("match_stage", "match_number", "team1", "team2", "score1", "score2") %in% names(matches_source))) {
    ms <- safe_df(matches_source)
    ms$match_number <- suppressWarnings(as.integer(ms$match_number))
    ms$score1 <- suppressWarnings(as.numeric(ms$score1))
    ms$score2 <- suppressWarnings(as.numeric(ms$score2))

    # Online football boxes may be returned in page/chronological order, while
    # the official 2026 knockout bracket uses non-sequential match numbers
    # (for example Brazil-Japan is Match 76, not necessarily the second box).
    # Therefore, remap any parsed completed knockout result by the actual team
    # pair before merging it with the authoritative bracket template.  This
    # prevents stale false conflicts such as Japan appearing after "Match 74".
    norm_team_local <- function(x) {
      tolower(gsub("[^[:alnum:]]", "", iconv(as.character(x), from = "", to = "ASCII//TRANSLIT", sub = "")))
    }
    pair_key_local <- function(a, b) {
      vapply(seq_along(a), function(i) paste(sort(norm_team_local(c(a[i], b[i]))), collapse = "__"), character(1))
    }
    canonical_pairs <- hardcoded_results |>
      dplyr::transmute(
        pair_key = pair_key_local(team1, team2),
        canonical_match_no = as.integer(match_no),
        canonical_round = as.character(round)
      ) |>
      dplyr::distinct(pair_key, .keep_all = TRUE)

    dynamic_results <- ms |>
      dplyr::filter(!is.na(match_number), match_number >= 73L, match_number <= 104L,
                    is.finite(score1), is.finite(score2), score1 != score2) |>
      dplyr::transmute(
        parsed_match_no = as.integer(match_number),
        round = as.character(match_stage),
        team1 = as.character(team1), team2 = as.character(team2),
        winner = ifelse(score1 > score2, as.character(team1), as.character(team2)),
        loser  = ifelse(score1 > score2, as.character(team2), as.character(team1)),
        score = paste0(score1, "-", score2),
        result_source = "online completed knockout result",
        pair_key = pair_key_local(team1, team2)
      ) |>
      dplyr::left_join(canonical_pairs, by = "pair_key") |>
      dplyr::mutate(
        match_no = dplyr::if_else(!is.na(canonical_match_no), canonical_match_no, parsed_match_no),
        round = dplyr::if_else(!is.na(canonical_round), canonical_round, round),
        result_source = dplyr::if_else(!is.na(canonical_match_no),
                                       paste0(result_source, "; match number remapped by team pair"),
                                       result_source)
      ) |>
      dplyr::select(match_no, round, team1, team2, winner, loser, score, result_source)

    if (nrow(dynamic_results) > 0) {
      hardcoded_results <- dplyr::bind_rows(dynamic_results, hardcoded_results) |>
        dplyr::arrange(match_no, dplyr::desc(grepl("^online", result_source))) |>
        dplyr::distinct(match_no, .keep_all = TRUE)
    }
  }

  hardcoded_results
}

make_fifa_prediction_from_nodes <- function(nodes, seed_value = 20260621, matches_source = NULL) {
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

  template <- make_fifa_official_bracket_template()
  completed_results <- make_fifa_completed_knockout_results(matches_source = matches_source)

  norm_team <- function(x) tolower(gsub("[^[:alnum:]]", "", iconv(as.character(x), from = "", to = "ASCII//TRANSLIT", sub = "")))
  used_teams <- character(0)
  used_third_teams <- character(0)

  add_used <- function(x) {
    x <- as.character(x[1])
    if (!is.na(x) && nzchar(x)) used_teams <<- unique(c(used_teams, x))
    x
  }

  get_named_team <- function(slot) {
    key <- norm_team(slot)
    hit <- which(norm_team(df$team) == key)
    if (length(hit) > 0) return(add_used(df$team[hit[1]]))
    # If an announced team is not yet present in an uploaded dataset, keep the
    # official announced name instead of silently substituting another team.
    add_used(slot)
  }

  get_group_rank_team <- function(group_letter, rank_num) {
    cand <- ranked |>
      dplyr::filter(group == group_letter, group_rank == rank_num) |>
      dplyr::arrange(dplyr::desc(points), dplyr::desc(prediction_score))
    if (nrow(cand) == 0) {
      stop(sprintf("Cannot resolve %s place in Group %s from the current nodes.", rank_num, group_letter), call. = FALSE)
    }
    add_used(cand$team[1])
  }

  choose_third_team <- function(group_letters) {
    cand <- ranked |>
      dplyr::filter(group_rank == 3, group %in% group_letters) |>
      dplyr::mutate(is_qualified_third = team %in% qualified32$team) |>
      dplyr::filter(!(team %in% used_teams), !(team %in% used_third_teams)) |>
      dplyr::arrange(dplyr::desc(is_qualified_third), dplyr::desc(points), dplyr::desc(prediction_score))

    # Fallback: if all candidates were already used or absent, pick the best
    # remaining predicted third-place qualifier. This keeps the app runnable
    # with incomplete or partially updated datasets while preserving the
    # official slot label in the fixture template.
    if (nrow(cand) == 0) {
      cand <- ranked |>
        dplyr::filter(group_rank == 3, team %in% qualified32$team) |>
        dplyr::filter(!(team %in% used_teams), !(team %in% used_third_teams)) |>
        dplyr::arrange(dplyr::desc(points), dplyr::desc(prediction_score))
    }
    if (nrow(cand) == 0) {
      stop("Cannot resolve a third-place Round-of-32 slot from the current nodes.", call. = FALSE)
    }
    chosen <- cand$team[1]
    used_third_teams <<- unique(c(used_third_teams, chosen))
    add_used(chosen)
  }

  resolve_official_slot <- function(slot) {
    slot <- trimws(as.character(slot))
    if (grepl("^Winner Group [A-L]$", slot)) {
      return(get_group_rank_team(sub("^Winner Group ([A-L])$", "\\1", slot), 1L))
    }
    if (grepl("^Runner-up Group [A-L]$", slot)) {
      return(get_group_rank_team(sub("^Runner-up Group ([A-L])$", "\\1", slot), 2L))
    }
    if (startsWith(slot, "3rd Group ")) {
      groups <- strsplit(sub("^3rd Group ", "", slot), "/", fixed = TRUE)[[1]]
      return(choose_third_team(groups))
    }
    get_named_team(slot)
  }

  r32_pairs <- template$r32
  r32_pairs$team1 <- NA_character_
  r32_pairs$team2 <- NA_character_
  for (ii in seq_len(nrow(r32_pairs))) {
    r32_pairs$team1[ii] <- resolve_official_slot(r32_pairs$team1_slot[ii])
    r32_pairs$team2[ii] <- resolve_official_slot(r32_pairs$team2_slot[ii])
  }
  # Critical distinction:
  # - team1/team2 are the internally resolved teams used for prediction.
  # - display_team1/display_team2 are the official announced Round-of-32
  #   slot labels shown in the app.  This keeps the displayed R32 fixtures
  #   identical to the official/current announcement, including unresolved
  #   slots such as "3rd Group C/E".
  r32_pairs$display_team1 <- r32_pairs$team1_slot
  r32_pairs$display_team2 <- r32_pairs$team2_slot

  predict_one <- function(a, b, match_no = NA_integer_, round_name = NA_character_) {
    # Lock completed knockout matches before any probabilistic prediction.
    # Example: Match 91 Brazil 1-2 Norway, so Brazil is a loser and cannot
    # be carried into the quarterfinals, semifinals, or final.
    off <- completed_results[completed_results$match_no == as.integer(match_no), , drop = FALSE]
    if (nrow(off) > 0) {
      pair_now <- sort(norm_team(c(a, b)))
      pair_off <- sort(norm_team(c(off$winner[1], off$loser[1])))
      if (identical(pair_now, pair_off)) {
        return(tibble::tibble(
          team1 = a, team2 = b,
          winner = off$winner[1], loser = off$loser[1],
          official_result = TRUE,
          score = off$score[1],
          result_source = off$result_source[1]
        ))
      }
    }

    sa <- df$prediction_score[match(a, df$team)]
    sb <- df$prediction_score[match(b, df$team)]
    if (!is.finite(sa)) sa <- 0
    if (!is.finite(sb)) sb <- 0
    delta <- max(min(sa - sb, 30), -30)
    p_a <- 1 / (1 + exp(-delta))
    winner <- ifelse(stats::runif(1) < p_a, a, b)
    loser <- ifelse(winner == a, b, a)
    tibble::tibble(
      team1 = a, team2 = b, winner = winner, loser = loser,
      official_result = FALSE,
      score = NA_character_,
      result_source = "model prediction"
    )
  }

  sim_from_pairs <- function(pairs_df, round_name, keep_slots = FALSE) {
    dplyr::bind_rows(lapply(seq_len(nrow(pairs_df)), function(i) {
      z <- predict_one(pairs_df$team1[i], pairs_df$team2[i], match_no = pairs_df$match_no[i], round_name = round_name)
      z$round <- round_name
      z$game <- as.integer(pairs_df$game[i])
      z$match_no <- as.integer(pairs_df$match_no[i])
      if (all(c("display_team1", "display_team2") %in% names(pairs_df))) {
        z$display_team1 <- pairs_df$display_team1[i]
        z$display_team2 <- pairs_df$display_team2[i]
      } else {
        z$display_team1 <- z$team1
        z$display_team2 <- z$team2
      }
      if (keep_slots && all(c("team1_slot", "team2_slot") %in% names(pairs_df))) {
        z$team1_slot <- pairs_df$team1_slot[i]
        z$team2_slot <- pairs_df$team2_slot[i]
      }
      z
    })) |>
      dplyr::select(round, game, match_no, dplyr::everything()) |>
      dplyr::arrange(game)
  }

  make_pairs_from_sources <- function(spec_df, prev_df, use_losers = FALSE) {
    prev_key <- stats::setNames(seq_len(nrow(prev_df)), as.character(prev_df$match_no))
    out <- spec_df
    out$team1 <- NA_character_
    out$team2 <- NA_character_
    out$display_team1 <- NA_character_
    out$display_team2 <- NA_character_
    for (ii in seq_len(nrow(out))) {
      i1 <- prev_key[as.character(out$source1[ii])]
      i2 <- prev_key[as.character(out$source2[ii])]
      if (is.na(i1) || is.na(i2)) {
        stop(sprintf("Cannot build Match %s from source matches %s and %s.", out$match_no[ii], out$source1[ii], out$source2[ii]), call. = FALSE)
      }
      col <- if (use_losers) "loser" else "winner"
      out$team1[ii] <- prev_df[[col]][i1]
      out$team2[ii] <- prev_df[[col]][i2]
      out$display_team1[ii] <- out$team1[ii]
      out$display_team2[ii] <- out$team2[ii]
    }
    out
  }

  r32 <- sim_from_pairs(r32_pairs, "Round of 32", keep_slots = TRUE)
  r16 <- sim_from_pairs(make_pairs_from_sources(template$r16, r32), "Round of 16")
  qf  <- sim_from_pairs(make_pairs_from_sources(template$qf,  r16), "Quarterfinals")
  sf  <- sim_from_pairs(make_pairs_from_sources(template$sf,  qf),  "Semifinals")
  third <- sim_from_pairs(make_pairs_from_sources(template$third, sf, use_losers = TRUE), "Third-place match")
  final <- sim_from_pairs(make_pairs_from_sources(template$final, sf, use_losers = FALSE), "Final")

  path <- dplyr::bind_rows(r32, r16, qf, sf, third, final) |>
    dplyr::select(round, game, match_no, display_team1, display_team2, team1, team2, winner, loser, dplyr::everything()) |>
    dplyr::arrange(match_no)

  # Final stale-prediction guard: a team that lost an official completed match
  # must not appear in any later match. This is intentionally strict because a
  # knockout bracket has no second chance.
  completed_losses <- completed_results |>
    dplyr::transmute(
      team = loser,
      lost_match_no = as.integer(match_no),
      allowed_reentry_match_no = dplyr::case_when(
        as.integer(match_no) %in% c(101L, 102L) ~ 103L,
        TRUE ~ NA_integer_
      )
    )
  stale <- dplyr::inner_join(
    path |> dplyr::select(match_no, team1, team2, winner) |>
      tidyr::pivot_longer(c(team1, team2, winner), names_to = "slot", values_to = "team") |>
      dplyr::filter(!is.na(team), nzchar(team)),
    completed_losses,
    by = "team", relationship = "many-to-many"
  ) |>
    dplyr::filter(
      as.integer(match_no) > as.integer(lost_match_no),
      is.na(allowed_reentry_match_no) | as.integer(match_no) != as.integer(allowed_reentry_match_no)
    )
  if (nrow(stale) > 0) {
    stop(
      paste0(
        "Official knockout-result conflict: ",
        paste(unique(paste0(stale$team, " appears after losing Match ", stale$lost_match_no)), collapse = "; ")
      ),
      call. = FALSE
    )
  }

  r32_check <- r32 |>
    dplyr::transmute(
      game, match_no,
      displayed_pair = paste(display_team1, "vs", display_team2),
      official_slot_pair = paste(team1_slot, "vs", team2_slot),
      internally_resolved_pair = paste(team1, "vs", team2),
      identical_to_official_slot = displayed_pair == official_slot_pair
    )

  list(
    seed = as.integer(seed_value),
    path = path,
    r32_validation = r32_check,
    official_bracket_template = template,
    completed_results = completed_results,
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
  if (!"display_team1" %in% names(path)) path$display_team1 <- path$team1
  if (!"display_team2" %in% names(path)) path$display_team2 <- path$team2
  path$display_team1 <- enc2utf8(as.character(path$display_team1))
  path$display_team2 <- enc2utf8(as.character(path$display_team2))
  path$winner <- enc2utf8(as.character(path$winner))
  path$game   <- suppressWarnings(as.numeric(path$game))
  path$stage_pos <- match(path$round, round_levels)
  path$y_pos <- ifelse(path$round == "Round of 32", path$game * 2,
                ifelse(path$round == "Round of 16", path$game * 4 - 1,
                ifelse(path$round == "Quarterfinals", path$game * 8 - 3,
                ifelse(path$round == "Semifinals", path$game * 16 - 7,
                ifelse(path$round == "Final", 16,
                ifelse(path$round == "Third-place match", 26, path$game * 2))))))
  if (!"official_result" %in% names(path)) path$official_result <- FALSE
  if (!"score" %in% names(path)) path$score <- NA_character_
  path$status_label <- ifelse(tolower(as.character(path$official_result)) %in% c("true", "1"),
                              paste0("Official ", ifelse(is.na(path$score) | !nzchar(path$score), "", paste0("(", path$score, ")"))),
                              "Predicted")
  path$label <- enc2utf8(as.character(paste0(path$team1, " vs ", path$team2, "\nW: ", path$winner, "\n", path$status_label)))

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
    "Official completed knockout results are locked; remaining matches use current Table 1 group points + mixed performance prior.  ",
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

# Compact top-down knockout tree. The final winner is the visible Top-1 node;
# each lower level shows the teams that advanced from the preceding matches.
draw_fifa_prediction_vertical_tree_base <- function(fp) {
  path <- as.data.frame(fp$path, stringsAsFactors = FALSE)
  qf <- path[order(path$match_no), , drop = FALSE]
  qf <- qf[qf$match_no %in% 97:100, , drop = FALSE]
  sf <- path[order(path$match_no), , drop = FALSE]
  sf <- sf[sf$match_no %in% 101:102, , drop = FALSE]
  final <- path[path$match_no == 104L, , drop = FALSE]
  third <- path[path$match_no == 103L, , drop = FALSE]
  if (nrow(qf) != 4L || nrow(sf) != 2L || nrow(final) != 1L) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Run FIFA 2026 prediction to build the vertical tree")
    return(invisible(NULL))
  }
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)
  graphics::par(mar = c(4.5, 1.5, 4.5, 1.5), family = "sans", xpd = NA)
  graphics::plot(NA, xlim = c(0.3, 8.7), ylim = c(0, 4.25),
                 axes = FALSE, xlab = "", ylab = "",
                 main = "FIFA 2026: Top-1 leader and knockout path")
  bottom_x <- seq_len(8L)
  qf_x <- c(1.5, 3.5, 5.5, 7.5)
  sf_x <- c(2.5, 6.5)
  final_x <- 4.5
  y <- c(quarterfinalists = 0.65, quarterfinal_winners = 1.65,
         semifinal_winners = 2.65, champion = 3.65)
  box <- function(x, yy, label, width, fill, colour = "#17365D", cex = 0.83) {
    graphics::rect(x - width / 2, yy - 0.18, x + width / 2, yy + 0.18,
                   col = fill, border = colour, lwd = 1.4)
    graphics::text(x, yy, labels = as.character(label), col = colour,
                   font = 2, cex = cex)
  }
  join <- function(child_x, child_y, parent_x, parent_y, official) {
    graphics::segments(child_x, child_y + 0.18, parent_x, parent_y - 0.18,
                       col = if (official) "#64829D" else "#B08042",
                       lty = if (official) 1 else 2, lwd = 1.6)
  }
  for (i in seq_len(4L)) {
    join(bottom_x[2L * i - 1L], y[1L], qf_x[i], y[2L],
         isTRUE(qf$official_result[i]))
    join(bottom_x[2L * i], y[1L], qf_x[i], y[2L],
         isTRUE(qf$official_result[i]))
  }
  for (i in seq_len(2L)) for (j in 1:2)
    join(qf_x[2L * i - 2L + j], y[2L], sf_x[i], y[3L],
         isTRUE(sf$official_result[i]))
  for (i in seq_len(2L))
    join(sf_x[i], y[3L], final_x, y[4L],
         isTRUE(final$official_result[1L]))
  leaf_names <- as.vector(t(qf[, c("team1", "team2")]))
  for (i in seq_along(leaf_names))
    box(bottom_x[i], y[1L], leaf_names[i], 0.98, "#F5F8FB", cex = 0.72)
  for (i in seq_len(4L))
    box(qf_x[i], y[2L], qf$winner[i], 1.55, "#DDEBF7", cex = 0.8)
  for (i in seq_len(2L))
    box(sf_x[i], y[3L], sf$winner[i], 1.9, "#B8D5F0", cex = 0.88)
  top_label <- paste("TOP 1:", final$winner[1L])
  box(final_x, y[4L], top_label, 2.55, "#A40000", "white", cex = 1.12)
  status <- if (isTRUE(final$official_result[1L])) "Official champion" else "Projected leader"
  graphics::text(final_x, 4.05, status, cex = 0.9, font = 2,
                 col = if (isTRUE(final$official_result[1L])) "#A40000" else "#8A5B1B")
  if (nrow(third) == 1L)
    graphics::mtext(paste0("Third: ", third$winner[1L], "   Fourth: ", third$loser[1L],
                           "   |   Solid links: official result; dashed links: prediction"),
                    side = 1, line = 1.5, cex = 0.78, col = "#33465A")
  invisible(list(top1 = final$winner[1L], quarterfinalists = leaf_names,
                 quarterfinal_winners = qf$winner, semifinal_winners = sf$winner))
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
  if (!"display_team1" %in% names(path)) path$display_team1 <- path$team1
  if (!"display_team2" %in% names(path)) path$display_team2 <- path$team2
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
        tags$div(style = paste0("font-weight:700;color:", team_color, ";"), paste0(dd$display_team1[i], " vs ", dd$display_team2[i])),
        tags$div(style = paste0("font-weight:900;color:", winner_color, ";font-size:", win_fs, "px;margin-top:6px;"),
                 paste0("W: ", dd$winner[i])),
        tags$div(style = "font-weight:700;color:#555;font-size:16px;margin-top:4px;",
                 ifelse("official_result" %in% names(dd) && tolower(as.character(dd$official_result[i])) %in% c("true", "1"),
                        paste0("Official result", ifelse("score" %in% names(dd) && !is.na(dd$score[i]) && nzchar(dd$score[i]), paste0(" (", dd$score[i], ")"), "")),
                        "Predicted"))
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
  df <- tufte_sort_cc(df0, x = "year", y = "value", group = "player_label", min.space = 0.42)
  if (!nrow(df)) return(tags$div(class = "alert alert-info", "No Golden Boot slopegraph data available."))

  df$group <- enc2utf8(as.character(df$group))
  df$x_chr <- enc2utf8(as.character(df$x))
  x_levels <- order_match_labels_numeric(df$x_chr)
  df$x_num <- match(df$x_chr, x_levels)
  df$y <- suppressWarnings(as.numeric(df$y))
  df$ypos <- suppressWarnings(as.numeric(df$ypos))
  df <- df[is.finite(df$x_num) & is.finite(df$ypos), , drop = FALSE]
  if (!nrow(df)) return(tags$div(class = "alert alert-info", "No valid Golden Boot slopegraph coordinates."))

  # V54: adjustable title/subtitle zoom for easier clipping into an image file.
  # Increase these values to enlarge the top title block, or decrease them to shrink it.
  title_zoom    <- 1.00   # examples: 0.80 smaller, 1.20 larger
  subtitle_zoom <- 1.00   # examples: 0.80 smaller, 1.20 larger
  label_zoom    <- 1.00   # axis/player label zoom

  title_fs    <- round(30 * title_zoom)
  subtitle_fs <- round(28 * subtitle_zoom)
  axis_fs     <- round(26 * label_zoom)
  player_fs   <- round(40 * label_zoom)
  point_fs    <- round(35 * label_zoom)

  ml <- 420; mr <- 220; mt <- max(120, round(95 + 25 * max(title_zoom, subtitle_zoom))); mb <- 210
  W <- max(1750, ml + mr + max(1, length(unique(df$x_chr))) * 220)
  H <- max(1500, 420 + length(unique(df$group)) * 125)
  y_min <- min(df$ypos, na.rm = TRUE); y_max <- max(df$ypos, na.rm = TRUE)
  if (!is.finite(y_min) || !is.finite(y_max) || y_min == y_max) { y_min <- 0; y_max <- 1 }
  x_to_px <- function(x) ml + (as.numeric(x) - 1) / max(1, length(x_levels) - 1) * (W - ml - mr)
  y_to_px <- function(y) mt + (y_max - as.numeric(y)) / max(1e-9, y_max - y_min) * (H - mt - mb)

  title_y <- 48
  subtitle_y <- title_y + round(36 * max(1, title_zoom))

  svg_children <- list(
    tags$text(x = 24, y = title_y, style = paste0("font-size:", title_fs, "px;font-weight:900;fill:#111;"),
              "The chase for the Golden Boot of FIFA 2026"),
    tags$text(x = 24, y = subtitle_y, style = paste0("font-size:", subtitle_fs, "px;font-weight:800;fill:#555;"),
              "Cumulative goals by parsed match order; generated after clicking Draw Golden Boot slopegraph.")
  )
  for (i in seq_along(x_levels)) {
    xp <- x_to_px(i)
    svg_children <- c(svg_children, list(
      tags$line(x1 = xp, y1 = mt, x2 = xp, y2 = H - mb, style = "stroke:#dfdfdf;stroke-width:1.5;"),
      tags$text(x = xp, y = H - 48, transform = paste0("rotate(-35 ", xp, " ", H - 48, ")"),
                style = paste0("font-size:", axis_fs, "px;font-weight:900;fill:#333;text-anchor:end;"), as.character(x_levels[i]))
    ))
  }
  for (g in unique(df$group)) {
    dd <- df[df$group == g, , drop = FALSE]
    dd <- dd[order(dd$x_num), , drop = FALSE]
    pts <- paste(sprintf("%.1f,%.1f", x_to_px(dd$x_num), y_to_px(dd$ypos)), collapse = " ")
    svg_children <- c(svg_children, list(tags$polyline(points = pts, style = "fill:none;stroke:#cc0000;stroke-width:3.0;stroke-linecap:round;stroke-linejoin:round;")))
    for (j in seq_len(nrow(dd))) {
      xp <- x_to_px(dd$x_num[j]); yp <- y_to_px(dd$ypos[j])
      svg_children <- c(svg_children, list(
        tags$circle(cx = xp, cy = yp, r = 8.5, style = "fill:#fff;stroke:#cc0000;stroke-width:2.4;"),
        tags$text(x = xp, y = yp, style = paste0("font-size:", point_fs, "px;font-weight:900;text-anchor:middle;dominant-baseline:middle;fill:#111;"), as.character(round(dd$y[j], 0)))
      ))
    }
  }
  first_rows <- df[df$x_num == 1, c("group", "ypos"), drop = FALSE]
  first_rows <- first_rows[!duplicated(first_rows$group), , drop = FALSE]
  first_rows <- first_rows[order(first_rows$ypos), , drop = FALSE]
  for (i in seq_len(nrow(first_rows))) {
    svg_children <- c(svg_children, list(tags$text(
      x = ml - 20, y = y_to_px(first_rows$ypos[i]) + 6,
      style = paste0("font-size:", player_fs, "px;font-weight:900;text-anchor:end;fill:#111;"), as.character(first_rows$group[i])
    )))
  }
  # Quick-glance web view: scale the whole SVG into one browser panel.
  # Increase display_width_px for a larger web preview; decrease it for a smaller clipped figure.
  display_width_px <- 1150

  tags$div(
    id = "fifa_golden_boot_capture_inner",
    style = paste0(
      "border:1px solid #d9d9d9;background:#fff;padding:14px 16px 12px 16px;",
      "margin-top:8px;width:", display_width_px, "px;max-width:none;box-sizing:border-box;",
      "display:block;overflow:hidden;"
    ),
    tags$svg(
      width = "100%",
      height = "auto",
      viewBox = paste("0 0", W, H),
      preserveAspectRatio = "xMinYMin meet",
      xmlns = "http://www.w3.org/2000/svg",
      style = "display:block;width:100%;height:auto;",
      svg_children
    )
  )
}

server <- function(input, output, session) {
  rv <- reactiveValues(result = NULL, genecluster_results = NULL,
                       genecluster_gene_results = NULL,
                       wgcna_result = NULL, wgcna_error = NULL,
                       status = "V32 TRUE REBUILD loaded. Click Run FLCA, Run FIFA 2026 bundled XLSX, REAL Update FIFA online, or Run NBA demo.", error = NULL)
  active_genecluster_result <- function(group) {
    group <- as.character(group)
    if (identical(input$genecluster_node_type %||% "genes", "genes"))
      rv$genecluster_gene_results[[group]]
    else
      rv$genecluster_results[[group]]
  }
  nurseratio_source_url <- reactiveVal(NULL)
  genecluster_data <- reactive({
    path <- if (!is.null(input$genecluster_file)) input$genecluster_file$datapath else file.path(getwd(), "genecluster5.csv")
    read_genecluster_ccc_data(path)
  })
  observeEvent(genecluster_data(), {
    groups <- genecluster_data()$groups
    selected <- suppressWarnings(as.numeric(isolate(input$gene_log10p_group)))
    if (length(selected) != 1L || !is.finite(selected) ||
        !selected %in% groups) selected <- groups[1L]
    updateSelectInput(session, "gene_log10p_group", choices = as.character(groups),
                      selected = as.character(selected))
  })
  observeEvent(list(input$genecluster_file, input$genecluster_value_columns,
                    input$target_k, input$flca_mode), {
    rv$genecluster_results <- NULL
    rv$genecluster_gene_results <- NULL
    if (isTRUE(rv$result$genecluster_ccc)) rv$result <- NULL
  }, ignoreInit = TRUE)

  get_input_data <- function(mode, progress = NULL, gene_group_override = NULL) {
    if (identical(mode, "taipei_mrt"))
      return(read_taipei_mrt_pasted_input(
        input$taipei_nodes_input, input$taipei_edges_input,
        use_nodes = isTRUE(input$taipei_use_nodes)))
    if (identical(mode, "demo_us_airports"))
      return(read_us_airports_demo())
    if (identical(mode, "demo")) {
      nodes <- read_app_pasted_table(input$nodes_input, "node")
      raw_edges <- read_app_pasted_table(input$edges_input, "edge")
      edges <- clean_edges(raw_edges)
      return(list(nodes = nodes, edges = edges,
                  data_mode = "pasted nodes and edges"))
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
    if (identical(mode, "demo_nurseratio")) {
      # A shared URL has priority over an earlier browser upload.
      if (!is.null(nurseratio_source_url()))
        return(read_demo_nurseratio_ccc(nurseratio_source_url()))
      if (!is.null(input$nurseratio_file)) {
        out <- read_nurseratio_ccc(input$nurseratio_file$datapath)
        out$data_mode <- paste0(out$data_mode, "; source: uploaded ", basename(input$nurseratio_file$name))
        return(out)
      }
      return(read_demo_nurseratio_ccc())
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
      if (is.null(input$coword_file))
        stop("No CSV was uploaded. Upload a file in the coword CSV field, or choose Taipei MRT pasted nodes and edges (or click Run TaipeiMRT) to use the bundled TaipeiMRT.csv data.", call. = FALSE)
      out <- csv_to_nodes_edges(input$coword_file$datapath, top_n_nodes = input$occurrence_top_n %||% 100)
      out$data_mode <- paste0("uploaded coword CSV; ", out$data_mode)
      return(out)
    }
    stop("Unknown input mode.", call. = FALSE)
  }

  run_analysis <- function(mode = NULL, gene_group_override = NULL) {
    mode <- mode %||% (input$input_mode %||% "demo_csv")
    value_source <- input$flca_value_source %||% "total"
    if (identical(mode, "demo_nurseratio") && identical(input$ccc_data_type, "gene"))
      mode <- "demo_genecluster_ccc"
    rv$error <- NULL
    rv$status <- paste0("V32 RUN RECEIVED at ", format(Sys.time(), "%H:%M:%S"), "\nMode: ", mode, "\nStarting analysis...")

    tryCatch({
      if (identical(mode, "demo_genecluster_ccc")) {
        rv$genecluster_results <- NULL
        rv$genecluster_gene_results <- NULL
        rv$result <- NULL
        data <- genecluster_data()
        sample_map <- resolve_genecluster_ccc_columns(data, input$genecluster_value_columns)
        columns <- sample_map$SampleID
        sample_conditions <- stats::setNames(sample_map$Condition, sample_map$SampleID)
        groups <- data$groups
        source <- if (!is.null(input$genecluster_file)) paste0("uploaded ", basename(input$genecluster_file$name)) else "bundled genecluster5.csv"
        batch <- shiny::withProgress(message = "Generating sample and gene-row CCC/FLCA networks",
                                     value = 0, {
          results <- lapply(groups, function(group) {
            dat <- make_genecluster_ccc_group(data, group, top_n = 20L,
                                              value_columns = columns,
                                              sample_conditions = sample_conditions)
            dat <- apply_flca_value_source(dat, value_source)
            out <- analyze_basic(dat$nodes, dat$edges,
                                 target_k = input$target_k %||% 4,
                                 flca_mode = input$flca_mode %||% "edge")
            if (identical(value_source, "edge"))
              out$edge_clustering <- cluster_by_edge(dat$nodes, dat$edges)
            out$data_mode <- paste0(dat$data_mode, "; source: ", source,
                                    "; FLCA nodes$value: ", dat$node_value_source)
            out$node_value_source <- dat$node_value_source
            out$nurseratio_ccc <- TRUE
            out$genecluster_ccc <- TRUE
            out$gene_group <- group
            out$max_per_cluster <- 20L
            out$input_nodes_n <- nrow(out$nodes)
            out$input_edges_n <- nrow(out$edges)
            shiny::incProgress(0.5 / length(groups),
                               detail = paste0("Sample-node network: log10P ", group))
            gene_dat <- make_genecluster_gene_ccc_group(data, group,
                                                        value_columns = columns,
                                                        top_n = 20L)
            gene_dat <- apply_flca_value_source(gene_dat, value_source)
            gene_out <- analyze_basic(gene_dat$nodes, gene_dat$edges,
                                      target_k = input$target_k %||% 4,
                                      flca_mode = input$flca_mode %||% "edge")
            if (identical(value_source, "edge"))
              gene_out$edge_clustering <- cluster_by_edge(gene_dat$nodes, gene_dat$edges)
            gene_out$data_mode <- paste0(gene_dat$data_mode, "; source: ", source,
                                         "; FLCA nodes$value: ", gene_dat$node_value_source)
            gene_out$node_value_source <- gene_dat$node_value_source
            gene_out$nurseratio_ccc <- TRUE
            gene_out$genecluster_ccc <- TRUE
            gene_out$gene_row_ccc <- TRUE
            gene_out$gene_group <- group
            gene_out$max_per_cluster <- 20L
            gene_out$input_nodes_n <- nrow(gene_out$nodes)
            gene_out$input_edges_n <- nrow(gene_out$edges)
            shiny::incProgress(0.5 / length(groups),
                               detail = paste0("Gene-row network: log10P ", group))
            list(samples = out, genes = gene_out)
          })
          names(results) <- as.character(groups)
          results
        })
        rv$genecluster_results <- lapply(batch, `[[`, "samples")
        rv$genecluster_gene_results <- lapply(batch, `[[`, "genes")
        selected <- suppressWarnings(as.numeric(gene_group_override %||% input$gene_log10p_group))
        if (!is.finite(selected) || !selected %in% groups) selected <- groups[1L]
        rv$result <- active_genecluster_result(selected)
        rv$status <- paste0("Gene-cluster analysis completed at ", format(Sys.time(), "%H:%M:%S"),
                            "\nSource: ", source,
                            "\nValidated CCC columns: ", length(columns),
                            " (", sum(sample_map$Condition == "Control"), " Control, ",
                            sum(sample_map$Condition == "Case"), " Case)",
                            "\nCCC + FLCA networks generated: ", length(batch),
                            " sample-node and ", length(batch),
                            " gene-row groups\nShowing group: ", selected,
                            "\nMain FLCA nodes: ",
                            if (isTRUE(rv$result$gene_row_ccc)) "gene rows" else "GSM sample columns",
                            "\nNodes: ", rv$result$input_nodes_n,
                            "\nEdges: ", rv$result$input_edges_n)
        shiny::showNotification(paste(length(batch), "sample-node and", length(batch),
                                      "gene-row networks generated. Choose a log10P group to view."),
                                type = "message", duration = 6)
        return(invisible(NULL))
      }
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
        dat <- get_input_data(mode, gene_group_override = gene_group_override)
      }

      dat <- apply_flca_value_source(dat, value_source)

      out <- analyze_basic(
        dat$nodes,
        dat$edges,
        target_k = input$target_k %||% 4,
        flca_mode = input$flca_mode %||% "edge",
        edges_reduced_override = dat$edges_reduced %||% NULL
      )
      if (identical(value_source, "edge"))
        out$edge_clustering <- cluster_by_edge(out$nodes, out$edges)
      # Ordinary pasted, CSV, and workbook nodes/edges share TaipeiMRT.R's
      # Top-50 root-chain FLCA path when no external clusters/tree are supplied.
      out$standard_flca_input <- mode %in% c("demo", "demo_csv", "upload", "upload_coword", "taipei_mrt", "demo_us_airports") &&
        !isTRUE(out$original_clusters_used) && is.null(dat$edges_reduced) &&
        !isTRUE(dat$taaa_tot)
      if (isTRUE(out$standard_flca_input)) {
        out$cluster_source <- "shared Top-50 strongest-WCD root-chain FLCA (flca_core.R)"
        out$flca_mode <- "edge (shared FLCA core)"
        standard_edges <- out$edges
        names(standard_edges)[names(standard_edges) == "follower"] <- "Follower"
        core <- run_flca(out$nodes, standard_edges, top50 = 50L,
                         target_n = input$flca_target_nodes %||% 20L)
        candidate <- core$candidate_nodes
        candidate$value2 <- out$nodes$value2[match(candidate$name, out$nodes$name)]
        candidate$value2[!is.finite(candidate$value2)] <-
          candidate$value[!is.finite(candidate$value2)]
        candidate_links <- core$candidate_tree_edges
        out$edges_reduced <- candidate_links
        names(out$edges_reduced)[names(out$edges_reduced) == "Follower"] <- "follower"
        out$g_flca <- igraph::graph_from_data_frame(
          out$edges_reduced, directed = FALSE, vertices = candidate)
        if (igraph::ecount(out$g_flca))
          igraph::E(out$g_flca)$weight <- igraph::E(out$g_flca)$WCD
        out$flca_carac <- stats::setNames(candidate$carac, candidate$name)
        full_names <- igraph::V(out$g_full)$name
        full_membership <- unname(out$flca_carac[full_names])
        missing_members <- is.na(full_membership)
        full_membership[missing_members] <- max(candidate$carac) + seq_len(sum(missing_members))
        out$flca_full <- stats::setNames(full_membership, full_names)
        out$memberships_df$FLCA <- unname(out$flca_full[out$memberships_df$name])
        out$quality_df <- make_selected_quality_comparison(core)
        fair_rank <- out$quality_df[
          out$quality_df$status == "ok" & !grepl("^FLCA", out$quality_df$method),
          , drop = FALSE]
        out$ranking_df <- fair_rank[order(-fair_rank$ARI_vs_FLCA,
                                         -fair_rank$NMI_vs_FLCA,
                                         -fair_rank$modularity), , drop = FALSE]
      }
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
      if (!is.null(dat$matches_source)) {
        out$matches_source <- dat$matches_source
      }
      if (identical(mode, "fifa2026_online") || identical(mode, "demo_fifa2026_xlsx")) {
        out$fifa_prediction <- make_fifa_prediction_from_nodes(
          out$nodes,
          seed_value = input$fifa_prediction_seed %||% 20260621,
          matches_source = out$matches_source %||% NULL
        )
      }
      if (!is.null(dat$taaa_tot)) {
        out$taaa_tot <- dat$taaa_tot
      }
      # Preserve additional row-wise TAAA ToT edge tables for FLCA Process only;
      # the app-wide original co-word network remains out$edges.
      if (!is.null(dat$taaa_tot_edges_full)) {
        out$taaa_tot_edges_full <- dat$taaa_tot_edges_full
      }
      if (!is.null(dat$taaa_tot_edges_one)) {
        out$taaa_tot_edges_one <- dat$taaa_tot_edges_one
      }
      out$data_mode <- if (!is.null(dat$node_value_source))
        paste0(dat$data_mode, "; FLCA nodes$value: ", dat$node_value_source) else
        dat$data_mode
      out$node_value_source <- dat$node_value_source
      if (isTRUE(dat$nurseratio_ccc)) out$nurseratio_ccc <- TRUE
      if (isTRUE(dat$genecluster_ccc)) {
        out$genecluster_ccc <- TRUE
        out$gene_group <- dat$gene_group
      }
      if (!is.null(dat$max_per_cluster)) out$max_per_cluster <- dat$max_per_cluster
      out$input_nodes_n <- nrow(out$nodes)
      out$input_edges_n <- nrow(out$edges)
      rv$result <- out
      if (mode %in% c("fifa2026_online", "demo_fifa2026_xlsx"))
        updateTabsetPanel(session, "main_tabs", selected = "fifa2026")
      extra_online <- if (!is.null(dat$online_details)) paste0("\n", dat$online_details) else ""
      rv$status <- paste0("V32 ANALYSIS COMPLETED at ", format(Sys.time(), "%H:%M:%S"),
                          "\nInput mode: ", out$data_mode,
                          "\nNodes: ", out$input_nodes_n,
                          "\nEdges: ", out$input_edges_n,
                          extra_online,
                          "\nCore: ", out$status_note)
      shiny::showNotification("Analysis completed. Check Data and FLCA Process tabs.", type = "message", duration = 5)
    }, error = function(e) {
      rv$error <- conditionMessage(e)
      rv$status <- paste0("V32 ANALYSIS STOPPED SAFELY at ", format(Sys.time(), "%H:%M:%S"), "\nMode: ", mode, "\nError: ", conditionMessage(e))
      shiny::showNotification(paste("Analysis stopped:", conditionMessage(e)), type = "error", duration = 12)
    })
  }

  observeEvent(input$nurseratio_file, {
    if (!is.null(input$nurseratio_file)) nurseratio_source_url(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$run_nurseratio_url, {
    source_url <- trimws(input$nurseratio_url %||% "")
    if (!nzchar(source_url)) {
      shiny::showNotification("Enter a GitHub CSV URL first.", type = "error", duration = 8)
      return(invisible(NULL))
    }
    nurseratio_source_url(source_url)
    updateRadioButtons(session, "input_mode", selected = "demo_nurseratio")
    run_analysis("demo_nurseratio")
  }, ignoreInit = TRUE)

  observeEvent(session$clientData$url_search, {
    query <- shiny::parseQueryString(session$clientData$url_search)
    if (is.null(query$url)) return(invisible(NULL))
    nurseratio_source_url(query$url)
    updateTextInput(session, "nurseratio_url", value = query$url)
    updateRadioButtons(session, "input_mode", selected = "demo_nurseratio")
    run_analysis("demo_nurseratio")
  }, once = TRUE, ignoreNULL = TRUE)

  coword_kano_result <- reactiveVal(NULL)
  coword_kano_tree <- reactiveVal(NULL)
  coword_kano_error <- reactiveVal("Upload a two-column CSV and click Run Coword Kano.")

  observeEvent(input$run_coword_kano, {
    updateTabsetPanel(session, "main_tabs", selected = "Coword Kano")
    tryCatch({
      if (is.null(input$coword_kano_file)) stop("Choose a CSV file first.", call. = FALSE)
      raw <- as.data.frame(read_csv_file_safe(input$coword_kano_file$datapath), stringsAsFactors = FALSE)
      if (ncol(raw) != 2) stop("The selected file has ", ncol(raw), " columns; Coword Kano requires exactly two.", call. = FALSE)
      dat <- data.frame(PT = trimws(as.character(raw[[1]])),
                        LA = trimws(as.character(raw[[2]])), stringsAsFactors = FALSE)
      dat <- dat[!is.na(dat$PT) & nzchar(dat$PT) & !is.na(dat$LA) & nzchar(dat$LA), , drop = FALSE]
      if (nrow(dat) < 3) stop("At least three rows with values in both columns are required.", call. = FALSE)
      authors <- sort(unique(c(dat$PT, dat$LA)))
      first_n <- as.integer(table(factor(dat$PT, levels = authors)))
      corresponding_n <- as.integer(table(factor(dat$LA, levels = authors)))
      simple_n <- as.integer(table(factor(dat$PT[dat$PT == dat$LA], levels = authors)))
      multi <- dat[dat$PT != dat$LA, , drop = FALSE]
      first_multi <- as.integer(table(factor(multi$PT, levels = authors)))
      corresponding_multi <- as.integer(table(factor(multi$LA, levels = authors)))
      cli <- ifelse(first_multi + corresponding_multi > 0, corresponding_multi / (first_multi + corresponding_multi), NA_real_)
      links <- if (nrow(multi)) {
        z <- stats::aggregate(rep(1, nrow(multi)), by = list(from = multi$PT, to = multi$LA), FUN = sum)
        names(z)[3] <- "weight"; z
      } else data.frame(from = character(0), to = character(0), weight = numeric(0))
      w <- matrix(0, length(authors), length(authors), dimnames = list(authors, authors))
      if (nrow(links)) for (i in seq_len(nrow(links))) { w[links$from[i], links$to[i]] <- links$weight[i]; w[links$to[i], links$from[i]] <- links$weight[i] }
      strength <- rowSums(w); degree <- rowSums(w > 0)
      pr <- sweep(w, 1, pmax(strength, 1), "/"); pr[strength == 0, ] <- 0
      indirect <- pr %*% pr; diag(indirect) <- 0
      shc <- pmax(0, pmin(1, 1 - rowSums((pr + indirect)^2))); shc[strength == 0] <- NA_real_
      role_count <- first_n + corresponding_n
      aac <- aac_from_values(role_count)
      mat <- vapply(authors, function(a) mean(dat$PT != a & dat$LA != a), numeric(1))
      out <- data.frame(name = authors, value = role_count, value2 = as.numeric(degree),
                        first_n = first_n, corresponding_n = corresponding_n, simple_n = simple_n,
                        CLI = cli, SHC = shc, MAT = mat, AAC = aac, n = nrow(dat),
                        n_star = first_n + corresponding_n + simple_n, stringsAsFactors = FALSE)
      # Match the dendrograph's deterministic value-then-name Top-1 ranking.
      out <- out[order(-out$value, out$name, method = "radix"), , drop = FALSE]
      out <- utils::head(out, 20)
      if (nrow(out) < 2) stop("The CSV produces fewer than two distinct authors.", call. = FALSE)
      out$size_plot <- pmax(0.01, out$value); out$size_plot[!is.finite(out$size_plot)] <- 0.01
      out$fill_col <- ifelse(out$CLI >= .8, "red", ifelse(out$CLI >= .6, "blue", ifelse(out$CLI >= .4, "yellow", ifelse(out$CLI >= .2, "black", "green"))))
      out$carac <- "Coword authors"
      shown_links <- links[links$from %in% out$name & links$to %in% out$name, , drop = FALSE]
      tree_edges <- data.frame(Leader = shown_links$from, follower = shown_links$to,
                               WCD = shown_links$weight, stringsAsFactors = FALSE)
      tree <- make_flca_process_dendrogram(list(nodes = out, edges_full = tree_edges))
      coword_kano_result(out)
      coword_kano_tree(tree)
      coword_kano_error(NULL)
    }, error = function(e) {
      coword_kano_result(NULL)
      coword_kano_tree(NULL)
      msg <- conditionMessage(e)
      if (!nzchar(msg)) msg <- "The CSV could not be read. Please upload a plain two-column CSV."
      coword_kano_error(msg)
    })
  }, ignoreInit = TRUE)

  coword_kano_data <- reactive(coword_kano_result())

  output$coword_kano_status <- renderPrint({
    err <- coword_kano_error()
    if (!is.null(err)) {
      cat("Coword Kano could not run:", err, "\n")
    } else {
      nd <- coword_kano_data()
      cat("Coword Kano loaded", nrow(nd), "authors from the two-column CSV.\n")
    }
  })
  output$coword_kano_plot <- renderPlot({
    nd <- coword_kano_data(); req(nrow(nd) >= 2)
    top <- nd[order(-nd$value, nd$name, method = "radix")[1L], , drop = FALSE]
    kano_origin <- c(mean(nd$value2), mean(nd$value))
    span <- max(nd$value, nd$value2, 1, na.rm = TRUE)
    # Reserve a wide header strip; edge counts alone are too narrow to space six cards.
    cards <- data.frame(x = seq(-span * .05, span * .75, length.out = 6),
                        y = rep(max(nd$value, na.rm = TRUE) + span * .22, 6),
                        label = c(sprintf("AAC\n%.3f", top$AAC), sprintf("SHC\n%.4f", top$SHC), sprintf("CLI\n%.2f", top$CLI), sprintf("MAT\n%.1f%%", 100 * top$MAT), paste0("n\n", top$n), paste0("n*\n", top$n_star)),
                        fill = c("#B7D8F5", "#F7C6D9", "#CBEBC7", "#FFE89A", "#DEC9F7", "#B9EEF5"))
    subtitle <- sprintf("AAC=%.3f  |  SHC (= 1 - Burt constraint)=%.3f (%.1f%%)  |  CLI=%.3f\nMAT=%.3f  |  n=%d  |  n*=%d (1st=%d + corresponding=%d + simple_n=%d)", top$AAC, top$SHC, 100 * top$SHC, top$CLI, top$MAT, top$n, top$n_star, top$first_n, top$corresponding_n, top$simple_n)
    print(plot_kano_real(nd, title_txt = paste0("Kano plot: ", top$name),
                         label_size = 3.2, origin = kano_origin) +
      ggplot2::labs(subtitle = subtitle, x = "X: first/last-author edge count", y = "Y: document count") +
      # Keep the maximum-degree bubbles and the Top-1 callout inside the plot.
      ggplot2::scale_x_continuous(limits = c(-span * .15, max(nd$value2, na.rm = TRUE) + span * .50)) +
      ggplot2::scale_y_continuous(limits = c(-span * .30, max(nd$value, na.rm = TRUE) + span * .42)) +
      ggplot2::scale_size_area(max_size = 14, name = "Document count") +
      ggplot2::geom_label(data = cards, ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE, fill = cards$fill, fontface = "bold", size = 3.6) +
      ggplot2::geom_point(data = top, ggplot2::aes(x = value2, y = value, size = size_plot), inherit.aes = FALSE, shape = 21, fill = "red", colour = "black", show.legend = FALSE) +
      ggplot2::annotate("label", x = top$value2 + span * .18, y = top$value + span * .10, label = paste0("Top 1 author: ", top$name), fontface = "bold") +
      ggplot2::annotate("segment", x = top$value2 + span * .15, y = top$value + span * .08, xend = top$value2, yend = top$value, colour = "red", linewidth = 1, arrow = grid::arrow(length = grid::unit(.16, "inches"))))
  }, width = 1300, height = 900, res = 120)
  output$coword_kano_dendrogram <- renderPlot({
    req(coword_kano_data())
    tree <- coword_kano_tree()
    validate(need(!is.null(tree), "No links join the 20 displayed authors, so an FLCA dendrograph cannot be drawn."))
    plot_flca_process_dendrogram(tree,
      main_title = sprintf("First/corresponding-author FLCA dendrograph of %d authors", nrow(tree$nodes)),
      weight_label = "shared records")
  }, width = 1100, height = 900, res = 110)
  output$coword_kano_table <- renderDT({
    nd <- coword_kano_data()
    datatable(nd[, c("name", "value", "value2"), drop = FALSE], rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE))
  })
  observeEvent(input$run, { run_analysis() }, ignoreInit = TRUE)
  observeEvent(input$run_wgcna, {
    rv$wgcna_result <- NULL
    rv$wgcna_error <- NULL
    tryCatch({
      mode <- input$input_mode %||% "demo_csv"
      if (!is.null(input$wgcna_values_file)) {
        matrix_values <- read_wgcna_matrix_csv(input$wgcna_values_file$datapath)
        source_label <- paste0("uploaded value matrix: ", input$wgcna_values_file$name)
      } else if (identical(mode, "demo_nurseratio") &&
                 !identical(input$ccc_data_type, "gene")) {
        dat <- get_input_data("demo_nurseratio")
        matrix_values <- dat$value_matrix
        source_label <- dat$data_mode
      } else if (identical(mode, "demo_genecluster_ccc") ||
                 (identical(mode, "demo_nurseratio") &&
                  identical(input$ccc_data_type, "gene"))) {
        data <- genecluster_data()
        sample_map <- resolve_genecluster_ccc_columns(
          data, input$genecluster_value_columns)
        node_type <- input$genecluster_node_type %||% "genes"
        group <- input$gene_log10p_group %||% as.character(data$groups[1L])
        matrix_values <- wgcna_gene_matrix(
          data, group, sample_map$SampleID, node_type)
        source_label <- paste0("genecluster log10P ", group, "; ", node_type)
      } else {
        stop("This input has node values and edges, but no repeated value columns. Upload a WGCNA value-matrix CSV with node names in the first column.", call. = FALSE)
      }
      rv$wgcna_result <- shiny::withProgress(
        message = "Running WGCNA module detection", value = 0,
        run_wgcna_values(matrix_values, power = input$wgcna_power %||% 6,
                         source = source_label,
                         node_value_mode = input$flca_value_source %||% "total"))
      updateTabsetPanel(session, "main_tabs", selected = "wgcna_results")
      showNotification("WGCNA completed. Open WGCNA Results.", type = "message")
    }, error = function(e) {
      rv$wgcna_error <- conditionMessage(e)
      updateTabsetPanel(session, "main_tabs", selected = "wgcna_results")
      showNotification(rv$wgcna_error, type = "error", duration = 10)
    })
  }, ignoreInit = TRUE)
  output$wgcna_status <- renderPrint({
    if (!is.null(rv$wgcna_error)) return(cat(rv$wgcna_error))
    z <- rv$wgcna_result
    if (is.null(z)) return(cat("Click Run WGCNA to analyze repeated numeric value columns."))
    cat("Source:", z$source, "\nObservations:", z$observations,
        "\nNodes:", nrow(z$nodes), "\nModules:", nrow(z$modules),
        "\nSoft-threshold power:", z$power,
        "\nFLCA node value:", if (identical(z$node_value_mode, "derived"))
          "derived incident WGCNA adjacency total" else "total observation score",
        if (identical(z$node_value_mode, "edge"))
          "\nSeparate edge clustering: weighted Louvain on full WGCNA adjacency" else "")
    if (length(z$excluded)) cat("\nExcluded constant nodes:", paste(z$excluded, collapse = ", "))
  })
  output$wgcna_metrics <- renderTable({ req(rv$wgcna_result); rv$wgcna_result$metrics }, digits = 3)
  output$edge_clustering_wgcna_ui <- renderUI({
    req(rv$wgcna_result)
    if (is.null(rv$wgcna_result$edge_clustering)) return(NULL)
    tagList(h3("Cluster by Edge: weighted Louvain"),
            p("Communities use all positive WGCNA adjacency edges. These memberships are separate from WGCNA modules and FLCA clusters."),
            tableOutput("edge_clustering_wgcna_metrics"),
            DTOutput("edge_clustering_wgcna_clusters"),
            DTOutput("edge_clustering_wgcna_nodes"))
  })
  output$edge_clustering_wgcna_metrics <- renderTable({
    req(rv$wgcna_result$edge_clustering)
    rv$wgcna_result$edge_clustering$metrics
  }, digits = 3)
  output$edge_clustering_wgcna_clusters <- renderDT({
    req(rv$wgcna_result$edge_clustering)
    datatable(rv$wgcna_result$edge_clustering$clusters, rownames = FALSE)
  })
  output$edge_clustering_wgcna_nodes <- renderDT({
    req(rv$wgcna_result$edge_clustering)
    datatable(rv$wgcna_result$edge_clustering$nodes, rownames = FALSE)
  })
  output$wgcna_modules <- renderDT({
    req(rv$wgcna_result)
    datatable(rv$wgcna_result$modules, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE))
  })
  output$wgcna_nodes <- renderDT({
    req(rv$wgcna_result)
    datatable(rv$wgcna_result$nodes, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE))
  })
  output$wgcna_network <- renderPlot({
    req(rv$wgcna_result)
    z <- rv$wgcna_result
    keep <- head(z$nodes$name, 50L)
    w <- z$adjacency[keep, keep, drop = FALSE]
    g <- igraph::graph_from_adjacency_matrix(w, mode = "undirected",
                                             weighted = TRUE, diag = FALSE)
    if (igraph::ecount(g) > 150L) {
      ord <- order(-igraph::E(g)$weight, method = "radix")
      g <- igraph::delete_edges(g, ord[-seq_len(150L)])
    }
    nd <- z$nodes[match(igraph::V(g)$name, z$nodes$name), , drop = FALSE]
    layout <- igraph::layout_with_kk(g)
    igraph::plot.igraph(g, layout = layout, vertex.label = nd$name,
                       vertex.color = nd$module_color,
                       vertex.size = 7 + 16 * sqrt(nd$connectivity /
                         max(nd$connectivity, 1)),
                       vertex.label.cex = 0.7,
                       edge.width = 0.5 + 3 * igraph::E(g)$weight,
                       edge.color = grDevices::adjustcolor("grey40", alpha.f = 0.55),
                       main = "WGCNA strongest weighted adjacency links")
  }, width = 1000, height = 760, res = 100)
  output$wgcna_tree <- renderPlot({
    req(rv$wgcna_result)
    z <- rv$wgcna_result
    keep <- head(z$nodes$name, 80L)
    tom <- z$tom[keep, keep, drop = FALSE]
    tree <- stats::hclust(stats::as.dist(1 - tom), method = "average")
    old <- graphics::par(mar = c(12, 4, 4, 1))
    on.exit(graphics::par(old), add = TRUE)
    graphics::plot(tree, labels = FALSE, hang = -1,
                   main = "WGCNA TOM clustering tree", xlab = "", sub = "",
                   ylab = "1 - TOM")
    leaf_names <- tree$labels[tree$order]
    colors <- z$nodes$module_color[match(leaf_names, z$nodes$name)]
    graphics::text(seq_along(leaf_names), -0.025 * max(tree$height),
                   labels = leaf_names, srt = 90, adj = 1, xpd = TRUE,
                   cex = 0.65, col = colors)
  }, width = 1100, height = 700, res = 100)
  output$wgcna_heatmap <- renderPlot({
    req(rv$wgcna_result)
    z <- rv$wgcna_result
    keep <- head(z$nodes$name, 80L)
    tom <- z$tom[keep, keep, drop = FALSE]
    dend <- as.dendrogram(stats::hclust(stats::as.dist(1 - tom)))
    stats::heatmap(tom, Rowv = dend, Colv = dend, symm = TRUE,
                   scale = "none", margins = c(9, 9),
                   main = "WGCNA topological overlap (highest node values)")
  }, width = 900, height = 680, res = 100)
  output$wgcna_tom_mds <- renderPlot({
    req(rv$wgcna_result)
    z <- rv$wgcna_result
    d <- z$tom_mds
    labels <- head(d, 20L)
    palette <- stats::setNames(
      d$module_color[match(unique(d$module), d$module)],
      unique(d$module))
    print(ggplot2::ggplot(d, ggplot2::aes(x = MDS1, y = MDS2,
                                         colour = module)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey85") +
      ggplot2::geom_vline(xintercept = 0, colour = "grey85") +
      ggplot2::geom_point(size = 3.5, alpha = 0.85) +
      ggrepel::geom_text_repel(data = labels,
                              ggplot2::aes(label = name),
                              size = 3.3, max.overlaps = Inf,
                              show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = palette) +
      ggplot2::coord_equal() +
      ggplot2::labs(title = "WGCNA nodes in TOM distance space",
                    subtitle = sprintf("%d nodes; distance = 1 - TOM; top 20 values labeled",
                                       nrow(d)),
                    colour = "Module", x = "MDS dimension 1",
                    y = "MDS dimension 2") +
      ggplot2::theme_minimal(base_size = 13))
  }, width = 1000, height = 720, res = 100)
  output$wgcna_power_diagnostic <- renderPlot({
    req(rv$wgcna_result)
    z <- rv$wgcna_result
    d <- z$soft_threshold
    if (!nrow(d)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Power diagnostic unavailable for this input.")
      return(invisible(NULL))
    }
    old <- graphics::par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
    on.exit(graphics::par(old), add = TRUE)
    fit <- d$signed_R2
    fit_lim <- range(c(0, 0.85, fit[is.finite(fit)]))
    graphics::plot(d$Power, fit, type = "b", pch = 16,
                   col = "#2467A7", ylim = fit_lim,
                   xlab = "Soft-threshold power", ylab = "Signed scale-free fit (R²)",
                   main = "Scale-free topology fit")
    graphics::abline(h = 0.85, col = "grey55", lty = 2)
    graphics::abline(v = z$power, col = "red", lty = 2, lwd = 2)
    selected <- which(abs(d$Power - z$power) < 1e-9)
    if (length(selected) && is.finite(fit[selected[1L]]))
      graphics::points(d$Power[selected[1L]], fit[selected[1L]],
                       pch = 21, bg = "red", cex = 1.5)
    graphics::plot(d$Power, d$mean.k., type = "b", pch = 16,
                   col = "#2467A7", xlab = "Soft-threshold power",
                   ylab = "Mean connectivity", main = "Network connectivity")
    graphics::abline(v = z$power, col = "red", lty = 2, lwd = 2)
    if (length(selected) && is.finite(d$mean.k.[selected[1L]]))
      graphics::points(d$Power[selected[1L]], d$mean.k.[selected[1L]],
                       pch = 21, bg = "red", cex = 1.5)
  }, width = 1100, height = 550, res = 100)
  plot_wgcna_flca <- function(z, tree_layout = FALSE) {
    nd <- z$flca_junction_nodes
    ed <- z$flca_tree_edges
    g <- igraph::graph_from_data_frame(
      data.frame(from = ed$Leader, to = ed$Follower, WCD = ed$WCD),
      directed = TRUE, vertices = data.frame(name = nd$name))
    ids <- sort(unique(nd$carac))
    palette <- stats::setNames(grDevices::rainbow(length(ids), s = 0.6, v = 0.85),
                               as.character(ids))
    ordered_nodes <- igraph::V(g)$name
    values <- nd$value[match(ordered_nodes, nd$name)]
    cl <- as.character(nd$carac[match(ordered_nodes, nd$name)])
    top <- nd$name[order(-nd$value, nd$name, method = "radix")[1L]]
    if (tree_layout) {
      roots <- setdiff(ordered_nodes, ed$Follower)
      layout <- igraph::layout_as_tree(g,
                                      root = match(roots, ordered_nodes),
                                      mode = "out")
    } else {
      layout <- igraph::layout_with_kk(g)
    }
    scaled <- values - min(values) + 1
    igraph::plot.igraph(
      g, layout = layout, vertex.label = ordered_nodes,
      vertex.color = unname(palette[cl]),
      vertex.frame.color = ifelse(ordered_nodes == top, "red", "grey30"),
      vertex.frame.width = ifelse(ordered_nodes == top, 3, 1),
      vertex.size = 8 + 16 * sqrt(scaled / max(scaled)),
      vertex.label.cex = if (length(ordered_nodes) > 35L) 0.52 else 0.7,
      edge.arrow.size = 0.35,
      edge.width = 1 + 3 * ed$WCD,
      edge.color = "grey50",
      edge.label = sprintf("%.2f", ed$WCD),
      edge.label.cex = 0.52,
      main = if (tree_layout) "Junction FLCA strongest-link tree on WGCNA adjacency"
             else "Junction FLCA reduced-edge network on WGCNA adjacency")
    graphics::mtext(
      "Edge labels: WGCNA adjacency similarity (higher = stronger); bubbles: scaled selected nodes$value; leader rank: selected nodes$value",
      side = 1, line = 1.2, cex = 0.72, col = "grey25")
  }
  output$wgcna_flca_clusters <- renderTable({
    req(rv$wgcna_result)
    rv$wgcna_result$flca_junction_summary
  })
  output$wgcna_flca_network <- renderPlot({
    req(rv$wgcna_result)
    plot_wgcna_flca(rv$wgcna_result)
  }, width = 1100, height = 760, res = 100)
  output$wgcna_flca_tree <- renderPlot({
    req(rv$wgcna_result)
    plot_wgcna_flca(rv$wgcna_result, tree_layout = TRUE)
  }, width = 1100, height = 760, res = 100)
  output$wgcna_flca_dendrograph <- renderPlot({
    req(rv$wgcna_result)
    z <- rv$wgcna_result
    reduced <- z$flca_tree_edges
    names(reduced)[names(reduced) == "Follower"] <- "follower"
    bun <- list(nodes = z$flca_junction_nodes,
                edges_reduced = reduced,
                edges_original_full = z$flca_full_edges,
                data_type = "value")
    tree <- make_flca_process_dendrogram(bun)
    top <- z$flca_junction_nodes$name[
      order(-z$flca_junction_nodes$value, z$flca_junction_nodes$name,
            method = "radix")[1L]]
    plot_flca_process_dendrogram(
      tree,
      main_title = sprintf("WGCNA adjacency: junction FLCA dendrograph of %d nodes",
                           nrow(z$flca_junction_nodes)),
      weight_label = "WGCNA adjacency",
      annotation_override = c(
        sprintf("Top-1 leader by node value: %s", top),
        sprintf("%d qualified junction clusters | WCD = |bicor|^%.0f (higher is stronger)",
                nrow(z$flca_junction_summary), z$power)),
      footer_override = "Red: Top-1 and qualified junction leaders | grey links: WGCNA adjacency | purple X: junction | dashed dividers: qualified clusters",
      show_cluster_dividers = TRUE,
      highlight_leaders_override =
        z$flca_junction_summary$junction_leader[
          z$flca_junction_summary$candidate_leader])
  }, width = 1400, height = 940, res = 120)
  output$wgcna_chord_note <- renderPrint({
    req(rv$wgcna_result)
    z <- rv$wgcna_result
    cat(length(z$chord$node_names), "nodes,",
        z$chord$edge_count, "displayed WGCNA adjacency links.")
    if (!requireNamespace("chorddiag", quietly = TRUE))
      cat("\nInstall chorddiag to display the interactive plot: install.packages('chorddiag')")
  })
  output$wgcna_chord_ui <- renderUI({
    req(rv$wgcna_result)
    if (!requireNamespace("chorddiag", quietly = TRUE) ||
        !rv$wgcna_result$chord$edge_count)
      return(tags$p("The interactive chord plot requires chorddiag and positive WGCNA adjacency links."))
    chorddiag::chorddiagOutput("wgcna_chord_interactive",
                               width = "100%", height = "900px")
  })
  if (requireNamespace("chorddiag", quietly = TRUE))
    output$wgcna_chord_interactive <- chorddiag::renderChorddiag({
      req(rv$wgcna_result)
      make_wgcna_chord_widget(rv$wgcna_result$chord)
    })
  output$download_wgcna_nodes <- downloadHandler(
    filename = function() "WGCNA_nodes.csv",
    content = function(file) {
      req(rv$wgcna_result)
      utils::write.csv(rv$wgcna_result$nodes, file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    })
  observeEvent(input$run_taipei_mrt, {
    updateRadioButtons(session, "input_mode", selected = "taipei_mrt")
    run_analysis("taipei_mrt")
    if (is.null(rv$error)) updateTabsetPanel(session, "main_tabs", selected = "flca_process")
  }, ignoreInit = TRUE)
  observeEvent(input$run_us_airports, {
    updateRadioButtons(session, "input_mode", selected = "demo_us_airports")
    run_analysis("demo_us_airports")
    if (is.null(rv$error)) updateTabsetPanel(session, "main_tabs", selected = "flca_process")
  }, ignoreInit = TRUE)
  observeEvent(input$select_demo_csv, { updateRadioButtons(session, "input_mode", selected = "demo_csv"); run_analysis("demo_csv") }, ignoreInit = TRUE)
  observeEvent(input$run_nurseratio_ccc, { updateRadioButtons(session, "input_mode", selected = "demo_nurseratio"); run_analysis("demo_nurseratio") }, ignoreInit = TRUE)
  observeEvent(input$run_genecluster_ccc, { updateRadioButtons(session, "input_mode", selected = "demo_genecluster_ccc"); run_analysis("demo_genecluster_ccc") }, ignoreInit = TRUE)
  observeEvent(input$genecluster_node_type, {
    if (is.null(rv$genecluster_results) || is.null(rv$genecluster_gene_results))
      return(invisible(NULL))
    group <- as.character(input$gene_log10p_group %||% names(rv$genecluster_results)[1L])
    if (!group %in% names(rv$genecluster_results)) group <- names(rv$genecluster_results)[1L]
    rv$result <- active_genecluster_result(group)
    rv$status <- paste0("Gene-cluster networks ready for ",
                        length(rv$genecluster_results), " log10P groups.\nShowing ",
                        if (isTRUE(rv$result$gene_row_ccc)) "gene rows" else "GSM samples",
                        " as nodes in group ", group, ".")
  }, ignoreInit = TRUE)
  observeEvent(input$gene_log10p_group, {
    gene_mode <- identical(input$input_mode, "demo_genecluster_ccc") ||
      (identical(input$input_mode, "demo_nurseratio") && identical(input$ccc_data_type, "gene"))
    if (!gene_mode || is.null(rv$genecluster_results))
      return(invisible(NULL))
    group <- as.character(input$gene_log10p_group)
    if (!group %in% names(rv$genecluster_results)) return(invisible(NULL))
    rv$result <- active_genecluster_result(group)
    rv$status <- paste0("Gene-cluster CCC + FLCA networks generated for ",
                        length(rv$genecluster_results), " log10P groups.\nShowing group: ", group,
                        "\nNodes: ", rv$result$input_nodes_n,
                        "\nEdges: ", rv$result$input_edges_n)
  }, ignoreInit = TRUE)
  observeEvent(input$gene_previous_group, {
    groups <- genecluster_data()$groups
    current <- match(suppressWarnings(as.numeric(input$gene_log10p_group)), groups)
    if (is.na(current) || current <= 1L) return(invisible(NULL))
    updateSelectInput(session, "gene_log10p_group", selected = as.character(groups[current - 1L]))
    if (is.null(rv$genecluster_results))
      run_analysis("demo_genecluster_ccc", gene_group_override = groups[current - 1L])
  }, ignoreInit = TRUE)
  observeEvent(input$gene_next_group, {
    groups <- genecluster_data()$groups
    current <- match(suppressWarnings(as.numeric(input$gene_log10p_group)), groups)
    if (is.na(current) || current >= length(groups)) return(invisible(NULL))
    updateSelectInput(session, "gene_log10p_group", selected = as.character(groups[current + 1L]))
    if (is.null(rv$genecluster_results))
      run_analysis("demo_genecluster_ccc", gene_group_override = groups[current + 1L])
  }, ignoreInit = TRUE)
  observeEvent(input$run_fifa2026_xlsx, { updateRadioButtons(session, "input_mode", selected = "demo_fifa2026_xlsx"); run_analysis("demo_fifa2026_xlsx") }, ignoreInit = TRUE)
  observeEvent(input$update_fifa2026_online, { updateRadioButtons(session, "input_mode", selected = "fifa2026_online"); run_analysis("fifa2026_online") }, ignoreInit = TRUE)
  observeEvent(input$run_fifa2026_tab, { updateRadioButtons(session, "input_mode", selected = "demo_fifa2026_xlsx"); run_analysis("demo_fifa2026_xlsx") }, ignoreInit = TRUE)
  observeEvent(input$update_fifa2026_tab, { updateRadioButtons(session, "input_mode", selected = "fifa2026_online"); run_analysis("fifa2026_online") }, ignoreInit = TRUE)
  observeEvent(input$run_nba2025_2026, { updateRadioButtons(session, "input_mode", selected = "demo_nba2025_2026"); run_analysis("demo_nba2025_2026") }, ignoreInit = TRUE)

  res <- reactive(rv$result)
  output$edge_clustering_flca_ui <- renderUI({
    req(res())
    if (is.null(res()$edge_clustering)) return(NULL)
    tagList(h3("Cluster by Edge: weighted Louvain"),
            p("Communities use the full positive weighted edge network. These memberships are separate from the FLCA strongest-link clusters above."),
            tableOutput("edge_clustering_flca_metrics"),
            DTOutput("edge_clustering_flca_clusters"),
            DTOutput("edge_clustering_flca_nodes"))
  })
  output$edge_clustering_flca_metrics <- renderTable({
    req(res()$edge_clustering)
    res()$edge_clustering$metrics
  }, digits = 3)
  output$edge_clustering_flca_clusters <- renderDT({
    req(res()$edge_clustering)
    datatable(res()$edge_clustering$clusters, rownames = FALSE)
  })
  output$edge_clustering_flca_nodes <- renderDT({
    req(res()$edge_clustering)
    datatable(res()$edge_clustering$nodes, rownames = FALSE)
  })
  current_quality_df <- reactive({
    req(res())
    if (!isTRUE(res()$standard_flca_input)) return(res()$quality_df)
    edges <- res()$edges
    names(edges)[names(edges) == "follower"] <- "Follower"
    core <- run_flca(res()$nodes, edges, top50 = 50L,
                     target_n = input$flca_target_nodes %||% 20L)
    make_selected_quality_comparison(core)
  })
  output$quality_method_explanation <- renderUI({
    req(res())
    if (!isTRUE(res()$standard_flca_input)) return(NULL)
    q <- current_quality_df()
    lou <- q[q$method == "louvain" & q$status == "ok", , drop = FALSE]
    flca <- q[q$method == "FLCA shared Top-50 selected" & q$status == "ok",
              , drop = FALSE]
    comparison <- if (nrow(lou) && nrow(flca))
      sprintf("On these %d selected nodes, Louvain has %d clusters (Q = %.4f), while FLCA has %d clusters (Q = %.4f). Their ARI is %.3f and NMI is %.3f; matching cluster counts do not imply matching memberships.",
              lou$n_nodes[1L], lou$n_clusters[1L], lou$modularity[1L],
              flca$n_clusters[1L], flca$modularity[1L],
              lou$ARI_vs_FLCA[1L], lou$NMI_vs_FLCA[1L]) else NULL
    full_w <- suppressWarnings(as.numeric(res()$edges$WCD))
    uniform_note <- if (length(full_w) && all(is.finite(full_w)) &&
                        length(unique(full_w)) == 1L)
      sprintf("All input routes have WCD = %s, so the weighted Louvain run has no differences in route strength to use.",
              format(full_w[1L], trim = TRUE)) else NULL
    companion <- if (identical(input$flca_value_source, "edge") &&
                     !is.null(res()$edge_clustering))
      sprintf("The Cluster by Edge option also reports Louvain on all %d input nodes. The Quality table reruns Louvain on the %d FLCA-selected nodes for a like-for-like comparison.",
              nrow(res()$edge_clustering$nodes), if (nrow(lou)) lou$n_nodes[1L] else NA_integer_) else NULL
    tags$div(
      style = "background:#f4f8fc;border-left:4px solid #3276a8;padding:10px 14px;margin:10px 0 16px;",
      tags$strong("Why FLCA and Louvain differ"),
      p("FLCA ranks nodes by their supplied values, gives each follower its strongest WCD link to a higher-ranked neighbor, and forms clusters from parent chains. Louvain seeks a partition with high weighted modularity. Both Quality rows use the same selected nodes and full induced original edges for Q and silhouette."),
      if (!is.null(companion)) p(companion),
      if (!is.null(comparison)) p(comparison),
      if (!is.null(uniform_note)) p(uniform_note),
      p("The reduced-tree reference row evaluates only strongest-link edges, so its Q and silhouette describe a different graph and are excluded from the fair ranking.")
    )
  })
  current_ranking_df <- reactive({
    q <- current_quality_df()
    q <- q[q$status == "ok" & !grepl("^FLCA", q$method), , drop = FALSE]
    q[order(-q$ARI_vs_FLCA, -q$NMI_vs_FLCA, -q$modularity), , drop = FALSE]
  })
  output$run_status <- renderPrint({ cat(rv$status) })
  output$data_mode <- renderPrint({ if (is.null(res())) cat(rv$status) else cat(paste0("Input mode: ", res()$data_mode, "\nNodes: ", res()$input_nodes_n, "\nEdges: ", res()$input_edges_n)) })
  output$tbl_nodes <- renderDT({
    req(res())
    nd <- res()$nodes
    if (isTRUE(res()$genecluster_ccc)) nd$log10P <- res()$gene_group
    datatable(nd, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$tbl_edges <- renderDT({
    req(res())
    ed <- res()$edges
    if (isTRUE(res()$genecluster_ccc)) ed <- cbind(log10P = res()$gene_group, ed)
    datatable(ed, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$genecluster_data_networks_ui <- renderUI({
    req(res())
    if (!isTRUE(res()$genecluster_ccc) || is.null(rv$genecluster_results)) return(NULL)
    tagList(
      hr(),
      h3("Gene-cluster networks by log10P group"),
      p(paste0("Run FLCA generates all groups. The graphs below show ",
               if (isTRUE(res()$gene_row_ccc)) "gene rows" else "GSM sample columns",
               " as nodes for the selected group.")),
      selectInput("data_gene_group", "Network group (log10P)",
                  choices = names(rv$genecluster_results),
                  selected = as.character(res()$gene_group)),
      DTOutput("genecluster_group_summary"),
      h4("Positive pairwise Lin's CCC network"),
      visNetworkOutput("data_gene_ccc_network", height = "560px"),
      h4("FLCA strongest-link network"),
      visNetworkOutput("data_gene_flca_network", height = "560px")
    )
  })
  output$genecluster_group_summary <- renderDT({
    req(rv$genecluster_results)
    batch <- if (isTRUE(res()$gene_row_ccc)) rv$genecluster_gene_results else rv$genecluster_results
    req(batch)
    summary <- do.call(rbind, lapply(names(batch), function(group) {
      x <- batch[[group]]
      condition <- if ("Condition" %in% names(x$nodes)) x$nodes$Condition else character(0)
      row <- data.frame(log10P = as.numeric(group),
                        selected = identical(as.character(res()$gene_group), group),
                        nodes = nrow(x$nodes),
                        CCC_edges = nrow(x$edges),
                        FLCA_edges = nrow(x$edges_reduced),
                        FLCA_clusters = length(unique(x$flca_carac)))
      if (!isTRUE(res()$gene_row_ccc)) {
        row$Control <- sum(condition == "Control", na.rm = TRUE)
        row$Case <- sum(condition == "Case", na.rm = TRUE)
      }
      row
    }))
    datatable(summary, rownames = FALSE, options = list(pageLength = 24, paging = FALSE,
                                                         searching = FALSE, scrollX = TRUE))
  })
  output$data_gene_ccc_network <- renderVisNetwork({
    req(res(), isTRUE(res()$genecluster_ccc))
    make_vis(res()$g_full, res()$flca_full, top_n = NULL,
             title = paste0("log10P ", res()$gene_group, ": ",
                            if (isTRUE(res()$gene_row_ccc)) "gene-row" else "GSM-sample",
                            " positive pairwise Lin's CCC"),
             label_size = input$label_size %||% 30, bold = input$label_bold)
  })
  output$data_gene_flca_network <- renderVisNetwork({
    req(res(), isTRUE(res()$genecluster_ccc))
    make_vis(res()$g_flca, res()$flca_carac, top_n = NULL,
             title = paste0("log10P ", res()$gene_group, ": ",
                            if (isTRUE(res()$gene_row_ccc)) "gene-row" else "GSM-sample",
                            " FLCA strongest links"),
             label_size = input$label_size %||% 30, bold = input$label_bold)
  })
  observeEvent(input$data_gene_group, {
    group <- as.character(input$data_gene_group)
    if (is.null(rv$genecluster_results) || !group %in% names(rv$genecluster_results))
      return(invisible(NULL))
    if (!isTRUE(rv$result$genecluster_ccc) ||
        !identical(as.character(rv$result$gene_group), group)) {
      rv$result <- active_genecluster_result(group)
      rv$status <- paste0("Gene-cluster CCC + FLCA networks generated for ",
                          length(rv$genecluster_results), " log10P groups.\nShowing group: ", group,
                          "\nNodes: ", rv$result$input_nodes_n,
                          "\nEdges: ", rv$result$input_edges_n)
    }
    updateSelectInput(session, "gene_log10p_group", selected = group)
  }, ignoreInit = TRUE)
  output$gene_row_flca_networks_ui <- renderUI({
    req(res())
    if (!isTRUE(res()$genecluster_ccc) || isTRUE(res()$gene_row_ccc) ||
        is.null(rv$genecluster_gene_results))
      return(NULL)
    tagList(
      hr(),
      h3("Additional networks with genes as nodes"),
      p("Each log10P group uses gene rows as nodes. Lin's CCC compares each gene across the validated SampleID columns; the 20 genes with the highest mean value are shown. These networks have their own FLCA clusters."),
      selectInput("flca_gene_row_group", "Gene-row network group (log10P)",
                  choices = names(rv$genecluster_gene_results),
                  selected = as.character(res()$gene_group)),
      DTOutput("gene_row_group_summary"),
      h4("Gene-row positive pairwise Lin's CCC network"),
      visNetworkOutput("gene_row_ccc_network", height = "560px"),
      h4("Gene-row FLCA strongest-link network"),
      visNetworkOutput("gene_row_flca_network", height = "560px"),
      h4("Gene-row nodes and CCC edges"),
      fluidRow(column(6, DTOutput("gene_row_nodes")),
               column(6, DTOutput("gene_row_edges")))
    )
  })
  gene_row_result <- reactive({
    req(res(), isTRUE(res()$genecluster_ccc), rv$genecluster_gene_results)
    rv$genecluster_gene_results[[as.character(res()$gene_group)]]
  })
  output$gene_row_group_summary <- renderDT({
    req(rv$genecluster_gene_results)
    batch <- rv$genecluster_gene_results
    summary <- do.call(rbind, lapply(names(batch), function(group) {
      x <- batch[[group]]
      data.frame(log10P = as.numeric(group),
                 selected = identical(as.character(res()$gene_group), group),
                 genes = nrow(x$nodes),
                 CCC_edges = nrow(x$edges),
                 FLCA_edges = nrow(x$edges_reduced),
                 FLCA_clusters = length(unique(x$flca_carac)))
    }))
    datatable(summary, rownames = FALSE,
              options = list(pageLength = 24, paging = FALSE,
                             searching = FALSE, scrollX = TRUE))
  })
  output$gene_row_ccc_network <- renderVisNetwork({
    x <- gene_row_result(); req(x)
    make_vis(x$g_full, x$flca_full, top_n = NULL,
             title = paste0("log10P ", x$gene_group, ": gene-row Lin's CCC"),
             label_size = 22, bold = TRUE)
  })
  output$gene_row_flca_network <- renderVisNetwork({
    x <- gene_row_result(); req(x)
    make_vis(x$g_flca, x$flca_carac, top_n = NULL,
             title = paste0("log10P ", x$gene_group, ": gene-row FLCA"),
             label_size = 22, bold = TRUE)
  })
  output$gene_row_nodes <- renderDT({
    x <- gene_row_result(); req(x)
    datatable(cbind(log10P = x$gene_group, x$nodes), rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })
  output$gene_row_edges <- renderDT({
    x <- gene_row_result(); req(x)
    datatable(cbind(log10P = x$gene_group, x$edges), rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })
  observeEvent(input$flca_gene_row_group, {
    group <- as.character(input$flca_gene_row_group)
    if (is.null(rv$genecluster_gene_results) ||
        !group %in% names(rv$genecluster_gene_results)) return(invisible(NULL))
    if (!isTRUE(rv$result$genecluster_ccc) ||
        !identical(as.character(rv$result$gene_group), group))
      rv$result <- active_genecluster_result(group)
    updateSelectInput(session, "gene_log10p_group", selected = group)
  }, ignoreInit = TRUE)

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
      make_fifa_prediction_from_nodes(nd, seed_value = seed_now, matches_source = res()$matches_source %||% NULL),
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
             if (all(fp$path$official_result))
               " | all knockout results are completed results"
             else
               " | completed knockout results locked; remaining winners predicted by 0.7 × Table 1 points + 0.3 × mixed prior")
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

  output$fifa_r32_official_check_table <- renderDT({
    fp <- fifa_prediction_state()
    if (is.null(fp)) {
      return(datatable(data.frame(message = "Click Run FIFA 2026 prediction plot."), rownames = FALSE, options = list(dom = "t")))
    }
    datatable(fp$r32_validation, rownames = FALSE, escape = TRUE,
              options = list(pageLength = 16, scrollX = TRUE, dom = "t"),
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

  # Visible base-R SS/Q decline plot shown under Data > FIFA 2026 prediction path by stage.
  # This uses draw_fifa_prediction_ssq_base(), the same renderer that produces
  # "Predicted SS and Q decline by FIFA 2026 stage".
  output$fifa_prediction_ssq_plot <- renderPlot({
    fp <- fifa_prediction_state()
    if (is.null(fp)) {
      plot.new()
      text(0.5, 0.5, "Click Run FIFA 2026 prediction plot first.", cex = 1.2)
      return(invisible(NULL))
    }
    draw_fifa_prediction_ssq_base(fp)
  }, res = 120)

  output$fifa_prediction_vertical_tree <- renderPlot({
    fp <- fifa_prediction_state()
    if (is.null(fp)) {
      plot.new()
      text(0.5, 0.5, "Click Run FIFA 2026 prediction plot to see the Top-1 tree")
      return(invisible(NULL))
    }
    draw_fifa_prediction_vertical_tree_base(fp)
  }, width = 1200, height = 620, res = 110)
  output$download_fifa_prediction_vertical_tree_png <- downloadHandler(
    filename = function() paste0("fifa2026_top1_vertical_tree_seed_",
                                 fifa_prediction_reactive()$seed, ".png"),
    contentType = "image/png",
    content = function(file) {
      fp <- fifa_prediction_reactive()
      grDevices::png(file, width = 1600, height = 850, res = 150)
      on.exit(grDevices::dev.off(), add = TRUE)
      draw_fifa_prediction_vertical_tree_base(fp)
    }
  )

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

  # Server-side PNG export for the base-R SS/Q decline plot. This is more robust
  # than browser screenshot export and works on shinyapps.io / RStudio alike.
  output$download_fifa_prediction_ssq_plot_png <- downloadHandler(
    filename = function() {
      fp <- fifa_prediction_state()
      seed <- if (!is.null(fp) && !is.null(fp$seed)) fp$seed else "current"
      paste0("fifa2026_prediction_ss_q_plot_seed_", seed, ".png")
    },
    contentType = "image/png",
    content = function(file) {
      fp <- fifa_prediction_state()
      req(fp)
      grDevices::png(file, width = 1200, height = 800, res = 150)
      on.exit(grDevices::dev.off(), add = TRUE)
      draw_fifa_prediction_ssq_base(fp)
    }
  )

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

  output$tbl_reduced <- renderDT({
    req(res())
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    datatable(bun$edges_reduced, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })
  output$tbl_memberships <- renderDT({ req(res()); datatable(res()$memberships_df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$tbl_quality <- renderDT({
    req(res())
    datatable(current_quality_df(), rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE))
  })
  output$tbl_ranking <- renderDT({ req(res()); datatable(current_ranking_df(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) })
  output$plot_full <- renderVisNetwork({ req(res()); m <- input$full_method %||% "louvain"; mem <- if (m %in% names(res()$memberships_df)) res()$memberships_df[[m]] else res()$memberships_df$louvain; make_vis(res()$g_full, mem, top_n = if (isTRUE(input$full_use_all)) NULL else input$full_top_n, title = paste("Full network -", m), label_size = input$label_size %||% 30, bold = input$label_bold) })
  output$plot_flca <- renderVisNetwork({
    req(res())
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    make_vis(bun$g_one, bun$display_membership, top_n = NULL,
             title = "FLCA reduced network | selected nodes and connected-component colors",
             label_size = input$label_size %||% 30, bold = input$label_bold)
  })
  output$flca_step1_nodes <- renderDT({
    req(res())
    nd <- res()$nodes
    if ("carac" %in% names(nd)) {
      nd$carac <- ifelse(is.na(nd$carac), "", as.character(as.integer(nd$carac)))
    }
    datatable(nd, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
  })
  output$flca_step1_edges <- renderDT({ req(res()); datatable(res()$edges, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$flca_process_edge_source <- renderPrint({
    req(res())
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    cat(bun$edge_source, "\n")
    cat("Full input edge count for selected nodes:", nrow(bun$edges_original_full), "\n")
    cat("Ordinary FLCA reduced top-edge count:", nrow(bun$edges_flca_reduced), "\n")
    if (isTRUE(res()$taaa_tot)) {
      cat("Additional TAAA ToT full-edge count used in Step 2:", nrow(bun$edges_taaa_full), "\n")
      cat("Additional TAAA ToT one-edge count used in Step 3:", nrow(bun$edges_taaa_one), "\n")
    }
    cat("Step 2 plotted edge count:", nrow(bun$edges_full), "\n")
    cat("Step 3 plotted edge count:", nrow(bun$edges_reduced), "\n")
    leader_values <- bun$nodes$value[match(bun$display_leaders, bun$nodes$name)]
    cat("Step 3 leaders (highest value in each connected component):",
        paste(sprintf("%s (%.2f)", bun$display_leaders, leader_values), collapse = ", "), "\n")
  })
  output$flca_step2_full_graph <- renderVisNetwork({
    req(res())
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    ttl <- if (isTRUE(res()$gene_row_ccc)) paste0("Step 2 gene rows: log10P ", res()$gene_group, " pairwise Lin's CCC network") else if (isTRUE(res()$genecluster_ccc)) paste0("Step 2 GSM samples: log10P ", res()$gene_group, " pairwise Lin's CCC network") else if (isTRUE(res()$nurseratio_ccc)) "Step 2 nurse ratios: pairwise Lin's CCC network" else if (grepl("FIFA 2026|fifa2026|completed knockout|Round-of-32|Group A-L", res()$data_mode %||% "", ignore.case = TRUE)) "Step 2 FIFA 2026 full match network: 48-team group stage + completed knockout" else "Step 2 original coword/full-edge graph: same capped Top-N nodes"
    make_vis(bun$g_full, bun$membership, top_n = NULL, title = ttl, label_size = 24, bold = TRUE)
  })
  output$flca_step3_reduced_graph <- renderVisNetwork({
    req(res())
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    ttl <- if (isTRUE(res()$gene_row_ccc)) paste0("Step 3 gene rows: log10P ", res()$gene_group, " maximum Lin's CCC link per gene") else if (isTRUE(res()$genecluster_ccc)) paste0("Step 3 GSM samples: log10P ", res()$gene_group, " maximum Lin's CCC link per GSM sample") else if (isTRUE(res()$nurseratio_ccc)) "Step 3 nurse ratios: maximum Lin's CCC link per hospital" else paste0("Step 3 ordinary FLCA reduced graph: same capped Top-N nodes | ", res()$cluster_source)
    make_vis(bun$g_one, bun$display_membership, top_n = NULL, title = ttl, label_size = 24, bold = TRUE)
  })
  output$flca_step3_reduced_edges <- renderDT({
    req(res())
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    datatable(bun$edges_reduced, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE))
  })

  flca_process_dendrogram_bundle <- reactive({
    req(res())
    make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
  })
  output$flca_process_dendrogram <- renderPlot({
    bun <- flca_process_dendrogram_bundle()
    tree <- make_flca_process_dendrogram(bun)
    validate(need(!is.null(tree), "At least one selected node is needed for an FLCA tree."))
    if (!is.null(res()$fifa_prediction)) tree <- flca_ranked_display_tree(tree)
    plot_flca_process_dendrogram(tree)
  }, width = 1100, height = 900, res = 110)
  output$fifa_prediction_flca_tree <- renderPlot({
    req(fifa_prediction_state(), res()$fifa_prediction)
    bun <- flca_process_dendrogram_bundle()
    tree <- make_flca_process_dendrogram(bun)
    validate(need(!is.null(tree), "At least one selected FIFA node is needed for the FLCA tree."))
    tree <- flca_ranked_display_tree(tree)
    plot_flca_process_dendrogram(tree)
  }, width = 1100, height = 900, res = 110)
  output$fifa_prediction_kano <- renderPlot({
    req(fifa_prediction_state(), res()$fifa_prediction)
    print(make_flca_value_value2_kano(
      res(), label_size = 3.6, visual_ratio = 1,
      target_n = input$flca_target_nodes %||% 20))
  }, width = 1300, height = 900, res = 110)
  output$download_fifa_prediction_kano_png <- downloadHandler(
    filename = function() paste0("fifa2026_flca_kano_", Sys.Date(), ".png"),
    contentType = "image/png",
    content = function(file) {
      req(fifa_prediction_state(), res()$fifa_prediction)
      grDevices::png(file, width = 1600, height = 1100, res = 140)
      on.exit(grDevices::dev.off(), add = TRUE)
      print(make_flca_value_value2_kano(
        res(), label_size = 3.6, visual_ratio = 1,
        target_n = input$flca_target_nodes %||% 20))
    }
  )
  output$flca_value_vertical_tree_ui <- renderUI({
    bun <- flca_process_dendrogram_bundle()
    tree <- make_flca_process_dendrogram(bun)
    if (is.null(tree) || isTRUE(tree$count_data)) return(NULL)
    tagList(
      h4("AAC(value) FLCA junction tree"),
      p("Vertical view of the same selected strongest-link edges. Each node follows its highest-WCD eligible leader; junction leaders with at least two direct followers define the colored clusters. Edge labels show WCD strength, and node size follows nodes$value."),
      plotOutput("flca_value_vertical_tree", height = "760px"),
      br(), br())
  })
  output$flca_value_vertical_tree <- renderPlot({
    bun <- flca_process_dendrogram_bundle()
    tree <- make_flca_process_dendrogram(bun)
    validate(need(!is.null(tree) && !isTRUE(tree$count_data),
                  "The vertical tree is available for AAC(value) input."))
    weight_label <- if (isTRUE(res()$nurseratio_ccc) ||
                        isTRUE(res()$genecluster_ccc) ||
                        isTRUE(res()$gene_row_ccc)) "Lin's CCC" else "WCD"
    plot_flca_value_vertical_tree(tree, weight_label = weight_label)
  }, width = 1100, height = 760, res = 110)

  output$flca_taaa_tot_bottom_note <- renderPrint({
    req(res())
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    if (!isTRUE(res()$taaa_tot)) {
      cat("No row-wise coword/TAAA upload detected. Bottom TAAA ToT networks are shown only for CSV files with 主題名稱 / 主題詞頻 / Cluster-style rows or A1/A2/... term-per-cell rows.\n")
    } else {
      cat("Bottom add-on active: coword TAAA ToT networks are added here without replacing Step 2 or Step 3.\n")
      cat("TAAA ToT full-edge count:", nrow(bun$edges_taaa_full), "\n")
      cat("TAAA ToT one-edge leader/follower count:", nrow(bun$edges_taaa_one), "\n")
    }
  })

  output$flca_taaa_tot_full_graph <- renderVisNetwork({
    req(res())
    validate(need(isTRUE(res()$taaa_tot), "No row-wise coword/TAAA upload detected."))
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    validate(need(nrow(bun$edges_taaa_full) > 0, "No TAAA ToT full-edge network available for the selected Top-N nodes."))
    make_vis(bun$g_taaa_full, bun$membership, top_n = NULL,
             title = "Bottom add-on: coword TAAA ToT full-edge network", label_size = 24, bold = TRUE)
  })

  output$flca_taaa_tot_one_graph <- renderVisNetwork({
    req(res())
    validate(need(isTRUE(res()$taaa_tot), "No row-wise coword/TAAA upload detected."))
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    validate(need(nrow(bun$edges_taaa_one) > 0, "No TAAA ToT one-edge network available for the selected Top-N nodes."))
    make_vis(bun$g_taaa_one, bun$membership, top_n = NULL,
             title = "Bottom add-on: coword TAAA ToT one-edge leader→follower network", label_size = 24, bold = TRUE)
  })
  output$flca_step4_cluster_summary <- renderDT({
    req(res())
    bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    mem <- bun$display_membership
    groups <- sort(unique(unname(mem)))
    out <- do.call(rbind, lapply(groups, function(group_id) {
      members <- names(mem)[mem == group_id]
      leader <- bun$display_leaders[unname(mem[bun$display_leaders]) == group_id][1]
      data.frame(cluster = group_id, leader = leader, n = length(members),
                 total_value = sum(bun$nodes$value[match(members, bun$nodes$name)], na.rm = TRUE),
                 stringsAsFactors = FALSE)
    }))
    datatable(out, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })
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
  output$flca_step6_kano <- renderPlot({ req(res()); print(make_flca_value_value2_kano(res(), label_size = 3.6, visual_ratio = 1, target_n = input$flca_target_nodes %||% 20)) }, width = 1300, height = 1300, res = 120)
  output$flca_step6_kano_nodes <- renderDT({
    req(res())
    p <- make_flca_value_value2_kano(res(), label_size = 3.6, visual_ratio = 1,
                                      target_n = input$flca_target_nodes %||% 20)
    nd <- as.data.frame(p$data, stringsAsFactors = FALSE)
    keep <- intersect(c("name", "value", "value2", "carac", "One_minus_C_percent"), names(nd))
    nd <- nd[, keep, drop = FALSE]
    if ("One_minus_C_percent" %in% names(nd)) {
      names(nd)[names(nd) == "One_minus_C_percent"] <- "1-C (%)"
      nd[["1-C (%)"]] <- round(suppressWarnings(as.numeric(nd[["1-C (%)"]])), 2)
    }
    datatable(nd, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  # Step 6c chord dashboard: final Top-N FLCA/SSplot nodes and their final edges.
  flca_step6c_chord_data <- reactive({
    req(res())
    make_flca_chord_data(res(), target_n = input$flca_target_nodes %||% 20)
  })

  output$flca_step6c_chord_note <- renderPrint({
    req(flca_step6c_chord_data())
    chd <- flca_step6c_chord_data()
    cat(chd$note, "\n")
    cat(chd$selection_note, "\n")
    if (!requireNamespace("chorddiag", quietly = TRUE))
      cat("Install chorddiag for interactive ribbons: install.packages('chorddiag'). The static PNG view remains available.")
  })

  output$flca_step6c_chord_ui <- renderUI({
    if (requireNamespace("chorddiag", quietly = TRUE) &&
        nrow(flca_step6c_chord_data()$edges))
      chorddiag::chorddiagOutput("flca_step6c_chord_interactive",
                                 width = "100%", height = "860px")
    else
      plotOutput("flca_step6c_chord_static", height = "860px", width = "100%")
  })

  if (requireNamespace("chorddiag", quietly = TRUE))
    output$flca_step6c_chord_interactive <- chorddiag::renderChorddiag({
      req(flca_step6c_chord_data())
      make_flca_chord_widget(flca_step6c_chord_data())
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
    filename = function() paste0("flca_step2_original_coword_full_network_", Sys.Date(), ".png"),
    content = function(file) {
      req(res())
      bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    ttl <- if (isTRUE(res()$gene_row_ccc)) paste0("Step 2 gene rows: log10P ", res()$gene_group, " pairwise Lin's CCC network") else if (isTRUE(res()$genecluster_ccc)) paste0("Step 2 GSM samples: log10P ", res()$gene_group, " pairwise Lin's CCC network") else if (isTRUE(res()$nurseratio_ccc)) "Step 2 nurse ratios: pairwise Lin's CCC network" else if (grepl("FIFA 2026|fifa2026|completed knockout|Round-of-32|Group A-L", res()$data_mode %||% "", ignore.case = TRUE)) "Step 2 FIFA 2026 full match network: 48-team group stage + completed knockout" else "Step 2 original coword/full-edge graph: same capped Top-N nodes"
      save_png_device(file, width = 1500, height = 1000, res = 130,
        plot_static_network_png(bun$g_full, bun$membership, title = ttl, top_n = NULL)
      )
    }
  )

  output$download_flca_step3_png <- downloadHandler(
    filename = function() paste0("flca_step3_ordinary_flca_reduced_network_", Sys.Date(), ".png"),
    content = function(file) {
      req(res())
      bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
    ttl <- if (isTRUE(res()$gene_row_ccc)) paste0("Step 3 gene rows: log10P ", res()$gene_group, " maximum Lin's CCC link per gene") else if (isTRUE(res()$genecluster_ccc)) paste0("Step 3 GSM samples: log10P ", res()$gene_group, " maximum Lin's CCC link per GSM sample") else if (isTRUE(res()$nurseratio_ccc)) "Step 3 nurse ratios: maximum Lin's CCC link per hospital" else paste0("Step 3 ordinary FLCA reduced graph: same capped Top-N nodes | ", res()$cluster_source)
      save_png_device(file, width = 1500, height = 1000, res = 130,
        plot_static_network_png(bun$g_one, bun$display_membership, title = ttl, top_n = NULL)
      )
    }
  )

  output$download_flca_taaa_tot_full_png <- downloadHandler(
    filename = function() paste0("flca_bottom_taaa_tot_full_edge_network_", Sys.Date(), ".png"),
    content = function(file) {
      req(res())
      validate(need(isTRUE(res()$taaa_tot), "No row-wise coword/TAAA upload detected."))
      bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
      validate(need(nrow(bun$edges_taaa_full) > 0, "No TAAA ToT full-edge network available."))
      save_png_device(file, width = 1500, height = 1000, res = 130,
        plot_static_network_png(bun$g_taaa_full, bun$membership,
                                title = "Bottom add-on: coword TAAA ToT full-edge network", top_n = NULL)
      )
    }
  )

  output$download_flca_taaa_tot_one_png <- downloadHandler(
    filename = function() paste0("flca_bottom_taaa_tot_one_edge_network_", Sys.Date(), ".png"),
    content = function(file) {
      req(res())
      validate(need(isTRUE(res()$taaa_tot), "No row-wise coword/TAAA upload detected."))
      bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
      validate(need(nrow(bun$edges_taaa_one) > 0, "No TAAA ToT one-edge network available."))
      save_png_device(file, width = 1500, height = 1000, res = 130,
        plot_static_network_png(bun$g_taaa_one, bun$membership,
                                title = "Bottom add-on: coword TAAA ToT one-edge leader→follower network", top_n = NULL)
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
      save_png_device(file, width = 1600, height = 1600, res = 140,
        print(make_flca_value_value2_kano(res(), label_size = 3.6, visual_ratio = 1, target_n = input$flca_target_nodes %||% 20))
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
      f_taaa_full <- file.path(tmpd, "bottom_taaa_tot_full_edge_network.png")
      f_taaa_one  <- file.path(tmpd, "bottom_taaa_tot_one_edge_network.png")
      bun <- make_flca_process_top_bundle(res(), target_n = input$flca_target_nodes %||% 20)
      ttl2 <- if (isTRUE(res()$gene_row_ccc)) paste0("Step 2 gene rows: log10P ", res()$gene_group, " pairwise Lin's CCC network") else if (isTRUE(res()$genecluster_ccc)) paste0("Step 2 GSM samples: log10P ", res()$gene_group, " pairwise Lin's CCC network") else if (isTRUE(res()$nurseratio_ccc)) "Step 2 nurse ratios: pairwise Lin's CCC network" else "Step 2 original coword full-edge graph: same capped Top-N nodes"
      ttl3 <- if (isTRUE(res()$gene_row_ccc)) paste0("Step 3 gene rows: log10P ", res()$gene_group, " maximum Lin's CCC link per gene") else if (isTRUE(res()$genecluster_ccc)) paste0("Step 3 GSM samples: log10P ", res()$gene_group, " maximum Lin's CCC link per GSM sample") else if (isTRUE(res()$nurseratio_ccc)) "Step 3 nurse ratios: maximum Lin's CCC link per hospital" else paste0("Step 3 ordinary FLCA reduced graph: same capped Top-N nodes | ", res()$cluster_source)
      save_png_device(f2, 1500, 1000, 130, plot_static_network_png(bun$g_full, bun$membership, title = ttl2, top_n = NULL))
      save_png_device(f3, 1500, 1000, 130, plot_static_network_png(bun$g_one, bun$display_membership, title = ttl3, top_n = NULL))
      save_png_device(f6, 1600, 1100, 140, render_real_SSplot82_panel(res(), target_n = input$flca_target_nodes %||% 20))
      save_png_device(f6b, 1600, 1600, 140, print(make_flca_value_value2_kano(res(), label_size = 3.6, visual_ratio = 1, target_n = input$flca_target_nodes %||% 20)))
      save_png_device(f5b, 1400, 760, 120, plot_flca_sankey_static(flca_step5_sankey_data()))
      save_png_device(f6c, 1500, 1200, 140, plot_flca_chord_static(flca_step6c_chord_data()))
      zip_files <- c(f2, f3, f5b, f6, f6b, f6c)
      if (isTRUE(res()$taaa_tot) && nrow(bun$edges_taaa_full) > 0) {
        save_png_device(f_taaa_full, 1500, 1000, 130,
          plot_static_network_png(bun$g_taaa_full, bun$membership, title = "Bottom add-on: coword TAAA ToT full-edge network", top_n = NULL))
        zip_files <- c(zip_files, f_taaa_full)
      }
      if (isTRUE(res()$taaa_tot) && nrow(bun$edges_taaa_one) > 0) {
        save_png_device(f_taaa_one, 1500, 1000, 130,
          plot_static_network_png(bun$g_taaa_one, bun$membership, title = "Bottom add-on: coword TAAA ToT one-edge leader→follower network", top_n = NULL))
        zip_files <- c(zip_files, f_taaa_one)
      }
      oldwd <- getwd(); on.exit(setwd(oldwd), add = TRUE)
      setwd(tmpd)
      utils::zip(zipfile = file, files = basename(zip_files))
    }
  )

  output$flca_mode_annotation <- renderPrint({ req(res()); cat(paste("FLCA mode:", res()$flca_mode, "; cluster source:", res()$cluster_source, "; original uploaded cluster used:", res()$original_clusters_used, "; V30 stable rebuild with REAL FIFA online Wikipedia update, FIFA/NBA demo buttons, and 3 downloadable demo data files.")) })
  output$plot_visual_quality_kano <- renderPlot({
    req(res())
    q <- as.data.frame(current_quality_df(), stringsAsFactors = FALSE)
    if (isTRUE(res()$standard_flca_input)) q <- q[q$status == "ok", , drop = FALSE]
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
    q$label_short <- gsub("FLCA edge weight \\(single\\)", "FLCA edge", q$label_short)
    q$label_short <- gsub("FLCA edge + node tie (single)", "FLCA node tie", q$label_short, fixed = TRUE)
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
    q <- as.data.frame(current_quality_df(), stringsAsFactors = FALSE)
    if (isTRUE(res()$standard_flca_input)) q <- q[q$status == "ok", , drop = FALSE]
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
    q <- as.data.frame(current_quality_df(), stringsAsFactors = FALSE)
    if (isTRUE(res()$standard_flca_input)) q <- q[q$status == "ok", , drop = FALSE]
    q$Q_modularity <- round(suppressWarnings(as.numeric(q$modularity)), 6)
    q$SS_mean_silhouette <- round(suppressWarnings(as.numeric(q$mean_silhouette)), 6)
    q$Kano_x <- q$Q_modularity
    q$Kano_y <- q$SS_mean_silhouette
    keep_cols <- intersect(c("method", "eval_graph", "n_nodes", "n_clusters", "Q_modularity", "SS_mean_silhouette", "Kano_x", "Kano_y", "status", "details"), names(q))
    datatable(q[, keep_cols, drop = FALSE], options = list(pageLength = 20, scrollX = TRUE))
  })
  params_df <- reactive({ data.frame(
    parameter = c("input_mode", "target_k", "flca_mode", "flca_value_source", "occurrence_top_n",
                  "candidate_leader_min_direct_followers", "flca_target_nodes",
                  "sil_intra_penalty", "sil_inter_penalty", "tie_break_scale",
                  "label_size", "label_bold", "app_version"),
    value = c(input$input_mode %||% "demo_csv", input$target_k %||% 4,
              input$flca_mode %||% "edge", input$flca_value_source %||% "total",
              input$occurrence_top_n %||% 100,
              2, input$flca_target_nodes %||% 20,
              input$sil_intra_penalty %||% 2, input$sil_inter_penalty %||% 5,
              input$tie_break_scale %||% 10000, input$label_size %||% 30,
              input$label_bold %||% TRUE,
              "V30 REAL FIFA online Wikipedia update + FIFA bundled xlsx + NBA 2025-2026 xlsx + 3 downloadable demo data files + strict uploaded clusters + renderSSplot(82) + dynamic Kano.R"),
    stringsAsFactors = FALSE) })
  output$tbl_parameters <- renderDT({ datatable(params_df(), options = list(pageLength = 20, scrollX = TRUE)) })
  output$parameter_methods_text <- renderPrint({
    cat("FLCA selects each follower's strongest eligible parent link and samples the Top-1 cluster first, then qualified leader clusters, then small fallback components. A candidate leader has at least two direct followers. This sampling order applies to all three node-value choices.\n")
    cat("Node-value choice:", input$flca_value_source %||% "total",
        "| advanced equal-WCD tie-break:", input$flca_mode %||% "edge",
        "| independent target k:", input$target_k %||% 4)
  })
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
  output$download_demo_nurseratio <- downloadHandler(
    filename = function() "nurseratio.csv",
    content = function(file) {
      src <- file.path(getwd(), "nurseratio.csv")
      if (!file.copy(src, file, overwrite = TRUE)) stop("Could not copy nurseratio.csv.")
    }
  )
  output$download_demo_genecluster <- downloadHandler(
    filename = function() "genecluster5.csv",
    content = function(file) {
      src <- file.path(getwd(), "genecluster5.csv")
      if (!file.copy(src, file, overwrite = TRUE)) stop("Could not copy genecluster5.csv.")
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
  output$download_quality <- downloadHandler(filename = function() "quality.csv", content = function(file) write.csv(current_quality_df(), file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$download_ranking <- downloadHandler(filename = function() "ranking.csv", content = function(file) write.csv(current_ranking_df(), file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$download_visual_quality_kano <- downloadHandler(filename = function() "visual_quality_kano.csv", content = function(file) write.csv(current_quality_df(), file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$download_parameters <- downloadHandler(filename = function() "parameters.csv", content = function(file) write.csv(params_df(), file, row.names = FALSE, fileEncoding = "UTF-8"))
}

shinyApp(ui, server)
