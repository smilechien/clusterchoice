# Taipei MRT FLCA leader-follower tree with the same metrics and visual grammar as app.R.
# Run: shiny::runApp("TaipeiMRT.R") to view the generated tree.
# Default station values are the July 2026 daily averages from trtcvol.

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_dir <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
module_file <- file.path(script_dir, "flca_core.R")
if (!file.exists(module_file)) stop("Cannot find flca_core.R beside TaipeiMRT.R")
if (!requireNamespace("visNetwork", quietly = TRUE)) stop("The reduced network plot requires the visNetwork package")
source(module_file, local = TRUE)

# Copy/paste input: follower leader [WCD]. The third column is optional.
coword_text <- '
follower leader
小碧潭 七張
新店區公所 七張
大坪林 七張
蘆洲 三民高中
徐匯中學 三民高中
徐匯中學 三和國中
先嗇宮 三重
菜寮 三重
三和國中 三重國小
永寧 土城
海山 土城
芝山 士林
忠孝復興 大安
科技大樓 大安
信義安和 大安
大安森林公園 大安
景美 大坪林
松山機場 大直
葫洲 大湖公園
三重國小 大橋頭
民權西路 大橋頭
台北橋 大橋頭
雙連 中山
行天宮 中山國小
松山機場 中山國中
古亭 中正紀念堂
小南門 中正紀念堂
台大醫院 中正紀念堂
東門 中正紀念堂
迴龍 丹鳳
輔大 丹鳳
文德 內湖
大湖公園 內湖
萬隆 公館
台電大樓 公館
科技大樓 六張犁
麟光 六張犁
港墘 文德
萬芳社區 木柵
奇岩 北投
新北投 北投
台電大樓 古亭
頂溪 古亭
東門 古亭
象山 台北101/世貿
台大醫院 台北車站
中山 台北車站
西門 台北車站
善導寺 台北車站
菜寮 台北橋
永春 市政府
雙連 民權西路
圓山 民權西路
中山國小 民權西路
頂溪 永安市場
項埔 永寧
頭前庄 先嗇宮
龍山寺 江子翠
關渡 竹圍
紅樹林 竹圍
小南門 西門
龍山寺 西門
北門 西門
麟光 辛亥
海山 亞東醫院
府中 亞東醫院
板橋 府中
忠孝新生 忠孝復興
忠孝敦化 忠孝復興
善導寺 忠孝新生
松江南京 忠孝新生
東門 忠孝新生
關渡 忠義
芝山 明德
石牌 明德
大安森林公園 東門
葫洲 東湖
行天宮 松江南京
台北101/世貿 信義安和
台北小巨蛋 南京三民
松山 南京三民
中山國中 南京復興
忠孝復興 南京復興
松江南京 南京復興
台北小巨蛋 南京復興
南港展覽館 南港
昆陽 南港
南港軟體園區 南港展覽館
東湖 南港軟體園區
永春 後山埤
昆陽 後山埤
貓空站 指南宮站
淡水 紅樹林
木柵 動物園
動物園站 動物園
動物園站 動物園內站
指南宮站 動物園內站
石牌 唭哩岸
奇岩 唭哩岸
忠孝敦化 國父紀念館
市政府 國父紀念館
新北投 復興崗
忠義 復興崗
永安市場 景安
南勢角 景安
萬隆 景美
西湖 港墘
新店 新店區公所
板橋 新埔
江子翠 新埔
輔大 新莊
頭前庄 新莊
萬芳醫院 萬芳社區
辛亥 萬芳醫院
大直 劍南路
西湖 劍南路
圓山 劍潭
士林 劍潭
貓空纜車 貓空站
'

parse_coword <- function(input, default_wcd = 1.0) {
  lines <- trimws(strsplit(input, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines) & !grepl("^#", lines)]
  if (!length(lines)) stop("Enter at least one follower leader [WCD] row in Edge data")
  parts <- strsplit(lines, "[,[:space:]]+")
  first <- tolower(parts[[1]])
  if (length(first) >= 2 && first[1] %in% c("follower", "from", "source") &&
      first[2] %in% c("leader", "to", "target")) parts <- parts[-1]
  if (!length(parts) || any(!lengths(parts) %in% c(2L, 3L)))
    stop("Each co-word row must contain follower leader [WCD]")
  follower <- vapply(parts, `[[`, character(1), 1)
  leader <- vapply(parts, `[[`, character(1), 2)
  Encoding(follower) <- Encoding(leader) <- "UTF-8"
  wcd <- vapply(parts, function(row)
    if (length(row) == 3L && nzchar(row[3]) && toupper(row[3]) != "NA")
      suppressWarnings(as.numeric(row[3])) else default_wcd, numeric(1))
  if (any(!is.finite(wcd) | wcd <= 0)) stop("WCD values must be positive numbers")
  out <- data.frame(follower = follower, leader = leader, WCD = wcd,
                    stringsAsFactors = FALSE)
  out <- out[out$follower != out$leader, , drop = FALSE]
  if (!nrow(out)) stop("No non-self links were found")
  out
}

parse_nodes <- function(input) {
  if (!nzchar(trimws(input)))
    return(data.frame(name = character(), value = numeric(), value2 = numeric()))
  lines <- trimws(strsplit(input, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines) & !grepl("^#", lines)]
  parts <- strsplit(lines, "[,[:space:]]+")
  if (length(parts) && length(parts[[1]]) >= 2L &&
      tolower(parts[[1]][1]) == "name" && tolower(parts[[1]][2]) == "value")
    parts <- parts[-1]
  if (!length(parts) || any(!lengths(parts) %in% c(2L, 3L)))
    stop("Each Node data row must contain name value [value2]")
  name <- vapply(parts, `[[`, character(1), 1)
  Encoding(name) <- "UTF-8"
  value <- suppressWarnings(as.numeric(vapply(parts, `[[`, character(1), 2)))
  value2 <- vapply(parts, function(row)
    if (length(row) == 3L) suppressWarnings(as.numeric(row[3])) else NA_real_, numeric(1))
  if (any(!is.finite(value) | value < 0))
    stop("Node value must be a nonnegative number")
  if (any(!is.na(value2) & (!is.finite(value2) | value2 < 0)))
    stop("Node value2 must be a nonnegative number")
  if (anyDuplicated(name)) stop("Node names must be unique")
  data.frame(name = name, value = value, value2 = value2,
             stringsAsFactors = FALSE)
}

edge_node_summary_text <- function(edge_text) {
  edges <- parse_coword(edge_text)
  edges <- stats::aggregate(WCD ~ follower + leader, data = edges, FUN = sum)
  names <- sort(unique(c(edges$follower, edges$leader)), method = "radix")
  values <- vapply(names, function(name)
    sum(edges$WCD[edges$follower == name | edges$leader == name]), numeric(1))
  paste(c("name,value", paste(names, values, sep = ",")), collapse = "\n")
}
default_node_text <- local({
  source_file <- file.path(script_dir, "TaipeiMRT_daily_average_2026_07.csv")
  if (!file.exists(source_file)) stop("Missing Taipei MRT daily-average snapshot: ", source_file)
  # read.csv(fileEncoding=) drops UTF-8 rows under Windows R's C locale.
  volume <- utils::read.csv(text = readLines(source_file, encoding = "UTF-8"),
                            encoding = "UTF-8", stringsAsFactors = FALSE)
  if (!identical(names(volume), c("name", "value")) ||
      anyDuplicated(volume$name) || any(!is.finite(volume$value)))
    stop("Invalid Taipei MRT daily-average snapshot")
  fallback <- parse_nodes(edge_node_summary_text(coword_text))
  matched <- match(fallback$name, volume$name)
  fallback$value[!is.na(matched)] <- volume$value[matched[!is.na(matched)]]
  paste(c("name,value", paste(fallback$name, fallback$value, sep = ",")), collapse = "\n")
})

.normalize_chord_edges <- function(edges) {
  if (is.null(edges) || !is.data.frame(edges) || nrow(edges) == 0) {
    return(data.frame(from=character(0), to=character(0), weight=numeric(0), stringsAsFactors=FALSE))
  }
  nms <- names(edges)
  from_col <- intersect(nms, c("from","From","Leader","leader","Source","source","node1","Node1"))[1]
  to_col   <- intersect(nms, c("to","To","Follower","follower","Target","target","node2","Node2"))[1]
  w_col    <- intersect(nms, c("weight","Weight","WCD","value","Value","n","count","Count"))[1]
  if (is.na(from_col) || is.na(to_col)) {
    if (ncol(edges) >= 2) { from_col <- names(edges)[1]; to_col <- names(edges)[2] } else {
      return(data.frame(from=character(0), to=character(0), weight=numeric(0), stringsAsFactors=FALSE))
    }
  }
  if (is.na(w_col)) {
    w <- rep(1, nrow(edges))
  } else {
    w <- suppressWarnings(as.numeric(edges[[w_col]]))
  }
  out <- data.frame(
    from = trimws(as.character(edges[[from_col]])),
    to = trimws(as.character(edges[[to_col]])),
    weight = w,
    stringsAsFactors = FALSE
  )
  out$weight[!is.finite(out$weight)] <- 0
  out <- out[nzchar(out$from) & nzchar(out$to) & out$from != out$to & out$weight > 0, , drop=FALSE]
  if (!nrow(out)) return(out)
  stats::aggregate(weight ~ from + to, data=out, FUN=max)
}

.flca_tree_effect_category <- function(x, cutoffs) {
  if (length(x) != 1L || !is.finite(x)) return("unavailable")
  magnitude <- abs(x)
  if (magnitude >= cutoffs[3]) "Large" else if (magnitude >= cutoffs[2]) "Medium"
  else if (magnitude >= cutoffs[1]) "Small" else "Trivial"
}

.flca_tree_top1_annotation <- function(nodes, count_data = NULL, aac_values = NULL) {
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
              .flca_tree_effect_category(aac, c(.6, .7, .8)))
    else if (is.finite(aac))
      sprintf("Equivalent effect size = %s", .flca_tree_effect_category(aac, c(.6, .7, .8)))
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
  d_category <- .flca_tree_effect_category(d, c(.2, .5, .8))
  line2 <- if (is.finite(d) && !is.null(aac_values))
    sprintf("Top-1 CCC strength: %.2f | Cohen's d = %.3f (%s effect)",
            v1, d, d_category)
  else if (is.finite(d)) sprintf("Cohen's d = %.3f (%s effect)", d, d_category)
           else "Cohen's d unavailable (Top-20 SD is zero)"
  c(line1, line2)
}

# Tree SS and Q use the reduced strongest-link tree; Top-1 SHC uses the full
# original network induced by displayed nodes. Positive WCD is strength.
# Missing paths use the same within/across-cluster penalties as sil_widths_vec.
.flca_tree_full_metrics <- function(nodes, full_edges, shc_edges = full_edges) {
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
  # Normalized weighted modularity is defined only for at least two clusters.
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


# Draw the same reduced leader-follower relationships as the FLCA Process
# dendrograph in flcacompare, using this app's final FLCA-MA node and edge set.
.tree_plot_data <- function(nodes, edges, full_edges = edges, data_type = NULL) {
  nd <- as.data.frame(nodes, stringsAsFactors = FALSE)
  ee <- .normalize_chord_edges(edges)
  if (!nrow(nd)) return(NULL)
  if (!"name" %in% names(nd)) names(nd)[1] <- "name"
  nd$name <- as.character(nd$name)
  nd <- nd[!is.na(nd$name) & nzchar(nd$name) & !duplicated(nd$name), , drop = FALSE]
  if (!nrow(nd)) return(NULL)
  if (!"value" %in% names(nd)) nd$value <- 1
  if (!"carac" %in% names(nd)) nd$carac <- seq_len(nrow(nd))
  nd$value <- suppressWarnings(as.numeric(nd$value))
  nd$value[!is.finite(nd$value)] <- 0
  links <- data.frame(Leader = as.character(ee$from), Follower = as.character(ee$to),
                      WCD = as.numeric(ee$weight), stringsAsFactors = FALSE)
  links <- links[is.finite(links$WCD) & links$WCD > 0 &
                   links$Leader %in% nd$name & links$Follower %in% nd$name &
                   links$Leader != links$Follower, , drop = FALSE]
  # FLCA keeps one incoming leader link per follower. Retain that rule if a
  # helper returns duplicate links, then check that the result is a forest.
  links <- links[order(links$Follower, -links$WCD, links$Leader,
                       method = "radix"), , drop = FALSE]
  links <- links[!duplicated(links$Follower), , drop = FALSE]
  children <- split(links$Follower, links$Leader)
  state <- stats::setNames(rep(0L, nrow(nd)), nd$name)
  visit <- function(node) {
    if (state[[node]] == 1L) stop("FLCA links contain a directed cycle.", call. = FALSE)
    if (state[[node]] == 2L) return(invisible(NULL))
    state[[node]] <<- 1L
    for (child in children[[node]]) visit(child)
    state[[node]] <<- 2L
    invisible(NULL)
  }
  for (node in nd$name) visit(node)
  # Keep each FLCA cluster together, ordered by its leader's value. Within a
  # cluster, the x positions follow node value rather than link/name order.
  roots <- setdiff(nd$name, links$Follower)
  cluster <- as.character(nd$carac)
  cluster_names <- unique(cluster)
  leader_value <- vapply(cluster_names, function(cc) {
    z <- which(cluster == cc & nd$name %in% roots)
    if (!length(z)) z <- which(cluster == cc)
    max(nd$value[z])
  }, numeric(1))
  cluster_names <- cluster_names[order(-leader_value, cluster_names,
                                        method = "radix")]
  nd <- nd[order(match(cluster, cluster_names), -nd$value, nd$name,
                 method = "radix"), , drop = FALSE]
  rownames(nd) <- NULL
  rownames(links) <- NULL
  full <- .normalize_chord_edges(full_edges)
  full_links <- data.frame(Leader = as.character(full$from),
                           Follower = as.character(full$to),
                           WCD = as.numeric(full$weight), stringsAsFactors = FALSE)
  count_data <- if (is.null(data_type)) TRUE else identical(data_type, "count")
  aac_values <- stats::setNames(numeric(nrow(nd)), nd$name)
  if (nrow(full_links)) {
    pairs <- full_links[full_links$Leader %in% nd$name &
                          full_links$Follower %in% nd$name &
                          full_links$Leader != full_links$Follower, , drop = FALSE]
    if (nrow(pairs)) {
      pairs$a <- pmin(pairs$Leader, pairs$Follower)
      pairs$b <- pmax(pairs$Leader, pairs$Follower)
      pairs <- unique(pairs[, c("a", "b"), drop = FALSE])
      for (i in seq_len(nrow(pairs))) {
        aac_values[pairs$a[i]] <- aac_values[pairs$a[i]] + 1
        aac_values[pairs$b[i]] <- aac_values[pairs$b[i]] + 1
      }
    }
  }
  list(nodes = nd, edges = links,
       count_data = count_data, aac_values = aac_values,
       metrics = .flca_tree_full_metrics(nd, links, full_links))
}

.draw_tree_plot <- function(tree, domain) {
  nd <- tree$nodes; links <- tree$edges
  labels <- nd$name
  children <- split(links$Follower, links$Leader)
  roots <- setdiff(labels, links$Follower)
  cluster <- as.character(nd$carac)
  cluster_gap <- c(0, as.integer(cluster[-1L] != cluster[-length(cluster)]))
  positions <- stats::setNames(seq_along(labels) + .55 * cumsum(cluster_gap), labels)
  levels <- stats::setNames(rep(0, length(labels)), labels)
  set_level <- function(node) {
    offspring <- children[[node]]
    if (!length(offspring)) return(0)
    levels[node] <<- 1 + max(vapply(offspring, set_level, numeric(1)))
    levels[node]
  }
  for (root in roots) set_level(root)
  top_height <- max(c(1, levels))
  values <- stats::setNames(nd$value, labels)
  rank <- order(-nd$value, labels, method = "radix")
  top_colors <- c("#C00000", "#0645D9", "#8B4513")
  border <- rep("grey20", length(labels)); label_col <- rep("black", length(labels))
  border[rank[seq_len(min(3L, length(rank)))]] <- top_colors[seq_len(min(3L, length(rank)))]
  label_col[rank[seq_len(min(3L, length(rank)))]] <- top_colors[seq_len(min(3L, length(rank)))]
  cluster <- as.character(nd$carac)
  cluster_levels <- unique(cluster)
  palette <- grDevices::hcl.colors(max(3L, length(cluster_levels)), "Dark 3")
  fill <- palette[match(cluster, cluster_levels)]
  bubble <- 1.1 + 2.3 * sqrt(pmax(0, nd$value) / max(1, nd$value))
  # Only the highest-ranked component joins the display stem. The other
  # components have no FLCA path to it, so their connectors are dashed.
  top_root <- roots[order(-values[roots], roots, method = "radix")[1L]]
  other_roots <- setdiff(roots, top_root)
  reference_y <- top_height + if (length(other_roots)) .6 else 0
  annotation <- .flca_tree_top1_annotation(nd, tree$count_data, tree$aac_values)
  metrics <- tree$metrics
  fmt_metric <- function(x) if (length(x) && is.finite(x[1])) sprintf("%.3f", x[1]) else "NA"
  old_par <- par(no.readonly = TRUE); on.exit(par(old_par), add = TRUE)
  par(mar = c(4, 4, 8, 2), xpd = NA)
  name_cex <- if (length(labels) >= 15L) .68 else .78
  longest_name_in <- max(strwidth(labels, units = "inches", cex = name_cex, font = 2))
  bottom_in <- longest_name_in + 3.05
  upper_y <- reference_y + .5
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
  mtext(sprintf("FLCA leader-follower tree plot: %s (%d nodes)", domain, length(labels)),
        side = 3, line = 6.4, cex = 1.2, font = 2)
  mtext(sprintf("Mean SS = %s  |  Overall Qw = %s  |  Overall Qu = %s",
                fmt_metric(metrics$mean_SS), fmt_metric(metrics$Qw), fmt_metric(metrics$Qu)),
        side = 3, line = 4.7, cex = .85, font = 2)
  mtext(annotation[1], side = 3, line = 3, cex = .84, font = 2, col = "#C00000")
  if (length(annotation) > 1L)
    mtext(annotation[2], side = 3, line = 1.3, cex = .84, font = 2, col = "#C00000")
  axis(2, at = seq_len(top_height), labels = seq_len(top_height), las = 1,
       cex.axis = .75, col.axis = "#555555", lwd = 0)
  segments(min(positions), reference_y, max(positions), reference_y, col = "#C00000", lwd = 2.5)
  leaders <- unique(links$Leader)
  cluster_roots <- vapply(split(seq_len(nrow(nd)), as.character(nd$carac)), function(idx)
    nd$name[idx[order(-nd$value[idx], nd$name[idx],
                      method = "radix")[1L]]], character(1))
  red_leaders <- intersect(unname(cluster_roots), leaders)
  child_counts <- table(links$Leader)
  junctions <- union(names(child_counts[child_counts >= 2L]), red_leaders)
  for (leader in leaders) {
    offspring <- children[[leader]]
    segments(positions[leader], 0, positions[leader], levels[leader],
             col = if (leader %in% red_leaders) "#C00000" else "grey55",
             lwd = if (leader %in% red_leaders) 2.5 else 1.5)
    segments(min(positions[c(leader, offspring)]), levels[leader],
             max(positions[c(leader, offspring)]), levels[leader], col = "grey55", lwd = 1.5)
  }
  max_wcd <- if (nrow(links)) max(links$WCD) else 1
  for (i in seq_len(nrow(links))) {
    leader <- links$Leader[i]; follower <- links$Follower[i]
    segments(positions[follower], levels[follower], positions[follower], levels[leader],
             col = "grey55", lwd = 1 + 5 * links$WCD[i] / max_wcd)
    x <- positions[follower]; y <- (levels[follower] + levels[leader]) / 2
    w <- sprintf("%.2f", links$WCD[i])
    half_w <- strwidth(w, cex = .72, font = 2) / 2 + strwidth(" ", cex = .72)
    half_h <- strheight(w, cex = .72, font = 2) / 2 + strheight(" ", cex = .72) / 3
    rect(x - half_w, y - half_h, x + half_w, y + half_h, col = "white", border = NA)
    text(x, y, w, cex = .72, font = 2, col = "#333333")
  }
  if (length(junctions)) {
    points(unname(positions[junctions]), levels[junctions], pch = 4, cex = 1.45, lwd = 5, col = "white")
    points(unname(positions[junctions]), levels[junctions], pch = 4, cex = 1.35, lwd = 2.5, col = "#6420C8")
  }
  for (root in roots)
    segments(positions[root], levels[root], positions[root], reference_y,
             col = if (root == top_root) "#C00000" else "grey55",
             lty = if (root == top_root) 1 else 2,
             lwd = if (root == top_root) 2.5 else 1.5)
  points(unname(positions[labels]), rep(0, length(labels)), pch = 21,
         cex = bubble, bg = fill, col = border, lwd = ifelse(seq_along(labels) %in% rank[1:min(3L, length(rank))], 2.5, 1))
  text(unname(positions[labels]), rep(name_y, length(labels)), labels,
       srt = 90, adj = c(1, .5), cex = name_cex, font = 2, col = label_col)
  text(unname(positions[labels]), rep(count_y, length(labels)),
       sprintf("%.2f", nd$value), srt = 90, adj = c(1, .5), cex = .78, font = 2, col = label_col)
  for (i in seq_len(nrow(metrics$clusters))) {
    stat <- metrics$clusters[i, , drop = FALSE]
    members <- labels[cluster == as.character(stat$carac)]
    if (!length(members)) next
    x <- mean(unname(positions[members]))
    points(x, cluster_bubble_y, pch = 21, cex = 1.9,
           bg = palette[match(as.character(stat$carac), cluster_levels)], col = "grey30", lwd = 1)
    text(x, cluster_bubble_y, sprintf("C%s", stat$carac), cex = .58, font = 2)
    text(x, cluster_ss_y, sprintf("SS=%s", fmt_metric(stat$SS)), cex = .7, font = 2, col = "#25364A")
    text(x, cluster_q_y, sprintf("Qw=%s", fmt_metric(stat$Qw)), cex = .7, font = 2, col = "#25364A")
    text(x, cluster_qu_y, sprintf("Qu=%s", fmt_metric(stat$Qu)), cex = .7, font = 2, col = "#25364A")
  }
  if (metrics$top1_name %in% labels)
    text(min(positions), shi_y, sprintf("Top-1 SHI=%s", fmt_metric(metrics$top1_SHC)),
         adj = c(0, .5), cex = .75, font = 2, col = "#C00000")
  mtext("Red: Top-1 stem and cluster leaders | Grey links: WCD | Purple X: leader junction | Dashed grey: disconnected from stem",
        side = 1, line = .5, cex = .82)
  invisible(NULL)
}

# Top-1 cluster first, then larger eligible clusters by highest member rank.
# Each chosen cluster contributes up to four members, except that one sole
# cluster retains the top target_n nodes before the two-group display split.
.reduce_flca_network <- function(nodes, edges, target_n = 20L, max_per_cluster = 4L) {
  nd <- as.data.frame(nodes, stringsAsFactors = FALSE)
  ee <- as.data.frame(edges, stringsAsFactors = FALSE)
  if ("Follower" %in% names(ee) && !"follower" %in% names(ee))
    names(ee)[names(ee) == "Follower"] <- "follower"
  nd$carac <- as.character(nd$carac)
  nd$value <- as.numeric(nd$value)
  follower_names <- as.character(ee$follower)
  cluster_ids <- unique(nd$carac)
  cluster_info <- do.call(rbind, lapply(cluster_ids, function(cid) {
    members <- nd[nd$carac == cid, , drop = FALSE]
    roots <- members[!members$name %in% follower_names, , drop = FALSE]
    if (!nrow(roots)) roots <- members
    roots <- roots[order(-roots$value, roots$name, method = "radix"), , drop = FALSE]
    highest <- members[order(-members$value, members$name, method = "radix"), , drop = FALSE]
    data.frame(carac = cid, leader = roots$name[1],
               leader_value = highest$value[1],
               highest_name = highest$name[1], size = nrow(members))
  }))
  cluster_info <- cluster_info[order(-cluster_info$leader_value,
                                     cluster_info$highest_name, method = "radix"), , drop = FALSE]
  top1 <- nd$name[order(-nd$value, nd$name, method = "radix")[1L]]
  top_cluster <- nd$carac[match(top1, nd$name)]
  others <- cluster_info[cluster_info$carac != top_cluster, , drop = FALSE]
  major <- others[others$size >= 3L, , drop = FALSE]
  minor <- others[others$size < 3L, , drop = FALSE]
  ordered_clusters <- c(top_cluster, major$carac, minor$carac)
  picked <- character(0)
  if (length(ordered_clusters) == 1L) {
    members <- nd[order(-nd$value, nd$name, method = "radix"), , drop = FALSE]
    picked <- head(members$name, target_n)
  } else {
    for (cid in ordered_clusters) {
      members <- nd[nd$carac == cid, , drop = FALSE]
      members <- members[order(-members$value, members$name, method = "radix"), , drop = FALSE]
      picked <- c(picked, head(members$name, max_per_cluster))
      if (length(picked) >= target_n) break
    }
  }
  nd <- nd[nd$name %in% picked, , drop = FALSE]
  if (length(unique(nd$carac)) == 1L && nrow(nd) >= 2L) {
    nd <- nd[order(-nd$value, nd$name, method = "radix"), , drop = FALSE]
    nd$carac <- ifelse(seq_len(nrow(nd)) <= ceiling(nrow(nd) / 2), 1L, 2L)
  }
  nd <- nd[order(match(nd$carac, unique(nd$carac)), -nd$value, nd$name,
                 method = "radix"), , drop = FALSE]
  ee <- ee[ee$Leader %in% nd$name & ee$follower %in% nd$name, , drop = FALSE]
  list(nodes = nd, edges = ee)
}

# Compute from the current text boxes. Provided node values replace the
# edge-derived strength for matching names; unlisted names remain edge-derived.
.analyze_mrt <- function(edge_text, node_text) {
  coword <- parse_coword(edge_text)
  supplied <- parse_nodes(node_text)
  # app.R aggregates repeated directed pairs before building node values.
  normalized <- .normalize_chord_edges(data.frame(
    from = coword$leader, to = coword$follower, weight = coword$WCD,
    stringsAsFactors = FALSE))
  # Endpoint columns in uploaded data are undirected. Use one canonical pair
  # and its strongest original WCD; reversing a row cannot change the result.
  canonical_from <- pmin(normalized$from, normalized$to)
  canonical_to <- pmax(normalized$from, normalized$to)
  normalized <- stats::aggregate(normalized$weight,
                                  by = list(from = canonical_from, to = canonical_to),
                                  FUN = max)
  names(normalized)[3L] <- "weight"
  full_edges <- data.frame(Leader = normalized$from, Follower = normalized$to,
                           WCD = normalized$weight, stringsAsFactors = FALSE)
  station_names <- sort(unique(c(full_edges$Follower, full_edges$Leader,
                                 supplied$name)), method = "radix")
  strength <- vapply(station_names, function(x)
    sum(full_edges$WCD[full_edges$Follower == x | full_edges$Leader == x]), numeric(1))
  degree <- vapply(station_names, function(x)
    sum(full_edges$Follower == x | full_edges$Leader == x), numeric(1))
  nodes0 <- data.frame(name = station_names, value = strength, value2 = degree,
                       stringsAsFactors = FALSE)
  hit <- match(nodes0$name, supplied$name)
  has_value <- !is.na(hit)
  nodes0$value[has_value] <- supplied$value[hit[has_value]]
  has_value2 <- has_value & !is.na(supplied$value2[hit])
  nodes0$value2[has_value2] <- supplied$value2[hit[has_value2]]

  flca <- run_flca(nodes0, full_edges)
  tree <- .tree_plot_data(flca$candidate_nodes, flca$candidate_tree_edges,
                          full_edges = full_edges)
  if (is.null(tree)) stop("FLCA did not produce a drawable tree")
  reduced_tree <- .tree_plot_data(flca$nodes, flca$tree_edges,
                                  full_edges = full_edges)
  if (is.null(reduced_tree)) stop("FLCA major sampling did not produce a drawable reduced tree")

  output_dir <- file.path(script_dir, "TaipeiMRT_output")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(nodes0, file.path(output_dir, "TaipeiMRT_input_nodes.csv"),
            row.names = FALSE)
  write.csv(full_edges, file.path(output_dir, "TaipeiMRT_input_edges.csv"),
            row.names = FALSE)
  write.csv(tree$nodes, file.path(output_dir, "TaipeiMRT_FLCA_nodes.csv"),
            row.names = FALSE)
  write.csv(tree$edges, file.path(output_dir, "TaipeiMRT_FLCA_links.csv"),
            row.names = FALSE)
  write.csv(tree$metrics$clusters,
            file.path(output_dir, "TaipeiMRT_FLCA_cluster_metrics.csv"),
            row.names = FALSE)
  annotation <- .flca_tree_top1_annotation(tree$nodes, tree$count_data,
                                           tree$aac_values)
  summary <- data.frame(Mean_SS = tree$metrics$mean_SS,
                        Overall_Qw = tree$metrics$Qw,
                        Overall_Qu = tree$metrics$Qu,
                        Top1 = tree$metrics$top1_name,
                        Top1_SHI = tree$metrics$top1_SHC,
                        AAC_annotation = annotation[1],
                        Effect_annotation = annotation[2],
                        stringsAsFactors = FALSE)
  write.csv(summary, file.path(output_dir, "TaipeiMRT_FLCA_summary.csv"),
            row.names = FALSE)
  write.csv(reduced_tree$nodes,
            file.path(output_dir, "TaipeiMRT_FLCA_reduced_nodes.csv"),
            row.names = FALSE)
  write.csv(reduced_tree$edges,
            file.path(output_dir, "TaipeiMRT_FLCA_reduced_links.csv"),
            row.names = FALSE)
  selected_full_edges <- full_edges[
    full_edges$Leader %in% reduced_tree$nodes$name &
      full_edges$Follower %in% reduced_tree$nodes$name, , drop = FALSE]
  candidate_full_edges <- full_edges[
    full_edges$Leader %in% tree$nodes$name &
      full_edges$Follower %in% tree$nodes$name, , drop = FALSE]
  write.csv(selected_full_edges,
            file.path(output_dir, "TaipeiMRT_FLCA_reduced_full_induced_edges.csv"),
            row.names = FALSE)

  save_tree <- function(data, path, width, height, title) {
    grDevices::png(path, width = width, height = height, res = 110,
                   type = if (capabilities("cairo")) "cairo" else if (.Platform$OS.type == "windows") "windows" else "Xlib",
                   family = if (.Platform$OS.type == "windows") "Microsoft JhengHei" else "sans")
    tryCatch(.draw_tree_plot(data, title), finally = grDevices::dev.off())
  }
  plot_width <- max(1800L, as.integer(nrow(tree$nodes) * 55L))
  reduced_width <- max(1500L, as.integer(nrow(reduced_tree$nodes) * 65L))
  png_file <- file.path(output_dir, "TaipeiMRT_FLCA_tree.png")
  reduced_png_file <- file.path(output_dir, "TaipeiMRT_FLCA_reduced_tree.png")
  save_tree(tree, png_file, plot_width, 1700, "Taipei MRT")
  save_tree(reduced_tree, reduced_png_file, reduced_width, 1100,
            "Taipei MRT: FLCA-MA Top20+")

  reduced_clusters <- unique(as.character(reduced_tree$nodes$carac))
  reduced_palette <- grDevices::hcl.colors(max(3L, length(reduced_clusters)), "Dark 3")
  names(reduced_palette) <- reduced_clusters
  comparison <- data.frame(
    network = c("Top-50 FLCA candidate", "Reduced FLCA-MA"),
    nodes = c(nrow(tree$nodes), nrow(reduced_tree$nodes)),
    tree_links = c(nrow(tree$edges), nrow(reduced_tree$edges)),
    full_induced_edges = c(nrow(candidate_full_edges), nrow(selected_full_edges)),
    mean_SS = c(tree$metrics$mean_SS, reduced_tree$metrics$mean_SS),
    weighted_Q = c(tree$metrics$Qw, reduced_tree$metrics$Qw),
    top1_SHC = c(tree$metrics$top1_SHC, reduced_tree$metrics$top1_SHC))
  write.csv(comparison, file.path(output_dir, "TaipeiMRT_network_comparison.csv"),
            row.names = FALSE)
  list(tree = tree, reduced_tree = reduced_tree, summary = summary,
       comparison = comparison,
       png_file = png_file, plot_width = plot_width,
       reduced_png_file = reduced_png_file, reduced_width = reduced_width,
       reduced_palette = reduced_palette, reduced_clusters = reduced_clusters)
}

ui <- shiny::fluidPage(
  shiny::titlePanel("FLCA leader-follower network and tree"),
  shiny::fluidRow(
    shiny::column(6,
      shiny::textAreaInput("node_text", "Node data: name value [value2] (clear to derive from edges)",
                           value = default_node_text, rows = 12, width = "100%"),
      shiny::actionButton("clear_nodes", "Clear nodes")),
    shiny::column(6,
      shiny::textAreaInput("edge_text", "Edge data: follower leader [WCD]",
                           value = coword_text, rows = 12, width = "100%"),
      shiny::actionButton("clear_edges", "Clear edges"))
  ),
  shiny::actionButton("run_analysis", "Run analysis", class = "btn-primary"),
  shiny::verbatimTextOutput("analysis_status"),
  shiny::tags$hr(),
  shiny::div(class = "well",
             shiny::h3("Reduced leader-follower network"),
             shiny::uiOutput("analysis_header"),
             visNetwork::visNetworkOutput("reduced_network", height = "720px")),
  shiny::div(class = "well",
             shiny::h3("Reduced leader-follower tree"),
             shiny::uiOutput("reduced_tree_ui")),
  shiny::tags$details(
    shiny::tags$summary("Show full FLCA tree and summary"),
    shiny::p("The reduced view samples clusters in highest-value order, keeping up to four nodes per cluster. When there is only one cluster, it keeps the top 20 nodes before splitting them into two display groups. SS, weighted Q (Qw), and normalized weighted Q (Qu) use its strongest-link tree. Top-1 SHC uses every original edge between the displayed nodes. SS uses shortest-path distances 1/WCD, with documented penalties for unreachable pairs."),
    shiny::tableOutput("network_comparison"),
    shiny::tableOutput("flca_summary"),
    shiny::uiOutput("full_tree_ui"))
)

server <- function(input, output, session) {
  shiny::observeEvent(input$clear_nodes,
    shiny::updateTextAreaInput(session, "node_text", value = ""), ignoreInit = TRUE)
  shiny::observeEvent(input$clear_edges,
    shiny::updateTextAreaInput(session, "edge_text", value = ""), ignoreInit = TRUE)

  analysis <- shiny::eventReactive(input$run_analysis, {
    tryCatch(.analyze_mrt(input$edge_text, input$node_text),
             error = function(e) list(error = conditionMessage(e)))
  }, ignoreInit = TRUE)

  result <- shiny::reactive({
    x <- analysis()
    shiny::validate(shiny::need(is.null(x$error), x$error))
    x
  })

  output$analysis_status <- shiny::renderText({
    if (input$run_analysis == 0) return("Enter data, then click Run analysis.")
    x <- analysis()
    if (!is.null(x$error)) return(paste("Analysis error:", x$error))
    node_source <- if (!nzchar(trimws(input$node_text))) "edge-derived" else if (
      identical(input$node_text, edge_node_summary_text(input$edge_text)))
      "edge-derived summary" else if (identical(input$node_text, default_node_text))
      "July 2026 daily averages (six unmatched stations use edge strength)" else "custom node values"
    sprintf("Node values: %s. Reduced: %d nodes, %d links, %d clusters. Full: %d nodes. Files saved in TaipeiMRT_output.",
            node_source,
            nrow(x$reduced_tree$nodes), nrow(x$reduced_tree$edges),
            length(x$reduced_clusters), nrow(x$tree$nodes))
  })

  output$analysis_header <- shiny::renderUI({
    x <- result()
    m <- x$reduced_tree$metrics
    a <- .flca_tree_top1_annotation(x$reduced_tree$nodes,
                                    x$reduced_tree$count_data,
                                    x$reduced_tree$aac_values)
    shiny::tagList(
      shiny::p(shiny::strong(sprintf("Mean SS = %.3f | Overall Qw = %.3f | Overall Qu = %.3f",
                                     m$mean_SS, m$Qw, m$Qu))),
      shiny::p(style = "color: #C00000; font-weight: bold;", a[1]),
      shiny::p(style = "color: #C00000; font-weight: bold;", a[2]),
      shiny::div(style = "display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 12px;",
                 lapply(x$reduced_clusters, function(cid)
                   shiny::tags$span(style = paste0("background:", x$reduced_palette[[cid]],
                                                    "; padding: 3px 8px; border-radius: 12px;"),
                                    paste0("C", cid))))
    )
  })

  output$reduced_network <- visNetwork::renderVisNetwork({
    x <- result()
    nd <- x$reduced_tree$nodes
    ee <- x$reduced_tree$edges
    network_nodes <- data.frame(
      id = nd$name, label = nd$name, value = pmax(1, nd$value),
      group = as.character(nd$carac),
      color = unname(x$reduced_palette[as.character(nd$carac)]),
      title = paste0(nd$name, " | C", nd$carac, " | value=", nd$value),
      stringsAsFactors = FALSE
    )
    network_edges <- data.frame(
      from = ee$Leader, to = ee$Follower, value = pmax(1, ee$WCD),
      title = paste0(ee$Leader, " → ", ee$Follower, " | WCD=", ee$WCD),
      stringsAsFactors = FALSE
    )
    visNetwork::visNetwork(network_nodes, network_edges,
                           width = "100%", height = "720px") |>
      visNetwork::visEdges(smooth = TRUE, arrows = "to",
                           color = list(color = "#808080", highlight = "#C00000")) |>
      visNetwork::visNodes(shape = "dot", scaling = list(min = 12, max = 38),
                           font = list(size = 20, face = "bold", color = "black")) |>
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
      visNetwork::visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) |>
      visNetwork::visInteraction(navigationButtons = TRUE, keyboard = TRUE)
  })

  output$reduced_tree_ui <- shiny::renderUI({
    x <- result()
    shiny::div(style = "max-width: 100%; overflow-x: auto;",
               shiny::imageOutput("reduced_tree", width = paste0(x$reduced_width, "px"),
                                  height = "1100px"))
  })
  output$reduced_tree <- shiny::renderImage({
    x <- result()
    list(src = x$reduced_png_file, contentType = "image/png",
         width = x$reduced_width, height = 1100,
         alt = "FLCA-MA reduced tree")
  }, deleteFile = FALSE)
  output$full_tree_ui <- shiny::renderUI({
    x <- result()
    shiny::div(style = "max-width: 100%; overflow-x: auto;",
               shiny::imageOutput("flca_tree", width = paste0(x$plot_width, "px"),
                                  height = "1700px"))
  })
  output$flca_tree <- shiny::renderImage({
    x <- result()
    list(src = x$png_file, contentType = "image/png",
         width = x$plot_width, height = 1700, alt = "Full FLCA tree")
  }, deleteFile = FALSE)
  output$flca_summary <- shiny::renderTable(result()$summary, rownames = FALSE)
  output$network_comparison <- shiny::renderTable(result()$comparison, rownames = FALSE)
}

shiny::shinyApp(ui, server)
