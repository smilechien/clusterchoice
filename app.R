# app.R — shinyapps.io-ready FLCA / community comparison demo
# Converted from standalone R script: no local Windows paths, no remote readLines dependency.

options(shiny.maxRequestSize = 50 * 1024^2)
options(install.packages.compile.from.source = "never")

required_pkgs <- c("shiny", "DT", "igraph", "dplyr", "tibble", "readr", "visNetwork", "RColorBrewer", "ggplot2", "ggrepel", "circlize")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing required packages: ", paste(missing_pkgs, collapse = ", "),
    ". Install before deployment, e.g. install.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "), type = 'binary')",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(igraph)
  library(dplyr)
  library(tibble)
  library(readr)
  library(visNetwork)
  library(RColorBrewer)
  library(ggplot2)
  library(ggrepel)
  library(circlize)
})

set.seed(123)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

# Optional: source FLCA-MA-SIL module when deployed with app.R.
for (.pp in c("flca-ma-sil-module.R", "flca-sil-ma-module.R", "flca_ms_sil_module.R", "flca_ms_sil_module(62).R")) {
  if (file.exists(.pp)) try(source(.pp, local = FALSE, encoding = "UTF-8"), silent = TRUE)
}


# Optional: source real Kano renderer when deployed with app.R.
for (.kp in c("kano(61).R", "kano.R")) {
  if (file.exists(.kp)) try(source(.kp, local = FALSE, encoding = "UTF-8"), silent = TRUE)
}

# Optional: source real SSplot and Sankey renderers for the FLCA 6-step process tab.
for (.rp in c("renderSSplot(77).R", "renderSSplot.R")) {
  if (file.exists(.rp)) try(source(.rp, local = FALSE, encoding = "UTF-8"), silent = TRUE)
}
for (.sp in c("sankey(58).R", "sankey.R")) {
  if (file.exists(.sp)) try(source(.sp, local = FALSE, encoding = "UTF-8"), silent = TRUE)
}

make_algorithm_kano_nodes <- function(quality_df) {
  q <- as.data.frame(quality_df, stringsAsFactors = FALSE)
  validate(need(all(c("method", "modularity", "mean_silhouette") %in% names(q)),
                "Quality table must contain method, modularity, and mean_silhouette."))
  q$modularity <- suppressWarnings(as.numeric(q$modularity))
  q$mean_silhouette <- suppressWarnings(as.numeric(q$mean_silhouette))
  q <- q[is.finite(q$modularity) & is.finite(q$mean_silhouette), , drop = FALSE]
  validate(need(nrow(q) >= 2, "At least two valid algorithms are needed for the visual quality Kano plot."))
  q$status[is.na(q$status) | !nzchar(q$status)] <- "ok"
  q$score_sum <- q$modularity + q$mean_silhouette
  q <- q[order(-q$score_sum, q$method), , drop = FALSE]
  q$carac <- ifelse(q$method == "FLCA", "FLCA",
                    ifelse(grepl("components", q$method), "Components", "Benchmark"))
  data.frame(
    name = q$method,
    value = q$mean_silhouette,   # y-axis in plot_kano_real()
    value2 = q$modularity,       # x-axis in plot_kano_real()
    carac = q$carac,
    n_clusters = q$n_clusters,
    ARI_vs_FLCA = q$ARI_vs_FLCA,
    NMI_vs_FLCA = q$NMI_vs_FLCA,
    status = q$status,
    stringsAsFactors = FALSE
  )
}

plot_algorithm_quality_kano <- function(quality_df) {
  nd <- make_algorithm_kano_nodes(quality_df)
  if (exists("plot_kano_real", mode = "function")) {
    p <- plot_kano_real(
      nodes = nd,
      title_txt = "Visual quality of clustering algorithms: modularity Q vs mean silhouette score",
      label_size = 4
    ) + ggplot2::labs(x = "Modularity Q", y = "Mean silhouette score")
    return(p)
  }
  ggplot2::ggplot(nd, ggplot2::aes(x = value2, y = value, label = name)) +
    ggplot2::geom_hline(yintercept = mean(nd$value, na.rm = TRUE), linetype = "dashed") +
    ggplot2::geom_vline(xintercept = mean(nd$value2, na.rm = TRUE), linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(size = pmax(value + value2, 0.01), fill = carac), shape = 21, alpha = 0.9) +
    ggrepel::geom_text_repel(max.overlaps = 200) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(title = "Visual quality of clustering algorithms", x = "Modularity Q", y = "Mean silhouette score", fill = "Algorithm class")
}


# -----------------------------
# Demo data
# -----------------------------
default_nodes_txt <- "name,value,value2,carac
北醫,118,39,2
臺大,92,40,1
成大,50,15,4
臺大醫院,40,22,1
陽明交通,39,22,3
長庚大學,31,14,6
陽明大學,29,15,2
交通大學,28,11,4
臺灣科大,25,11,1
奇美,24,15,4
國衛院,22,11,1
義守大學,20,19,4
北榮,20,14,3
中榮,17,11,5
中國醫大,17,8,5
中正大學,16,14,5
長庚醫院,16,10,6
中央大學,15,9,3
輔仁大學,14,11,1
北護大學,12,9,2"

default_edges_txt <- "Leader,follower,WCD
臺大,臺大醫院,6.404
長庚大學,長庚醫院,4.3914
陽明交通,北榮,3.3822
成大,奇美,3.3715
臺大,國衛院,2.364
北醫,陽明大學,2.3339
中正大學,中國醫大,2.2914
中正大學,中榮,2.2814
國衛院,輔仁大學,2.2711
臺大,北醫,1.254
臺大,臺灣科大,1.244
北醫,北護大學,1.2039
陽明交通,中央大學,1.1422
義守大學,中正大學,1.1319
義守大學,成大,1.1219
奇美,交通大學,1.1115"

# -----------------------------
# Helpers
# -----------------------------
read_table_flexible_text <- function(txt) {
  txt <- paste(txt, collapse = "\n")
  txt <- trimws(txt)
  validate(need(nzchar(txt), "Input text is empty."))
  df <- suppressWarnings(tryCatch(
    readr::read_delim(I(txt), delim = NULL, col_names = FALSE, trim_ws = TRUE,
                      show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  ))
  if (is.null(df) || ncol(df) < 2) {
    df <- suppressWarnings(readr::read_table(I(txt), col_names = FALSE, trim_ws = TRUE,
                                             show_col_types = FALSE, progress = FALSE))
  }
  as.data.frame(df, stringsAsFactors = FALSE)
}

read_table_flexible_file <- function(path) {
  txt <- paste(readr::read_lines(path), collapse = "\n")
  read_table_flexible_text(txt)
}

canon <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[[:space:]_\\.-]+", "", x)
  x <- gsub("[^[:alnum:]]", "", x)
  x
}

looks_like_header <- function(row, type = c("edges", "nodes")) {
  type <- match.arg(type)
  cc <- canon(unlist(row, use.names = FALSE))
  if (type == "edges") {
    any(cc %in% c("leader", "from", "source", "node1", "sender", "u", "a")) &&
      any(cc %in% c("follower", "follow", "to", "target", "node2", "receiver", "v", "b"))
  } else {
    any(cc %in% c("name", "node", "term", "label", "institution", "author", "keyword", "id"))
  }
}

apply_header_if_present <- function(df, type = c("edges", "nodes")) {
  type <- match.arg(type)
  if (nrow(df) > 0 && looks_like_header(df[1, , drop = FALSE], type = type)) {
    nm <- make.names(as.character(unlist(df[1, ], use.names = FALSE)), unique = TRUE)
    df <- df[-1, , drop = FALSE]
    names(df) <- nm
  } else {
    names(df) <- paste0("V", seq_len(ncol(df)))
  }
  df
}

first_matching_col <- function(nms, candidates) {
  cn <- canon(nms)
  hit <- match(candidates, cn, nomatch = 0)
  if (any(hit > 0)) return(nms[hit[hit > 0][1]])
  NA_character_
}

clean_edges <- function(edges) {
  edges <- as.data.frame(edges, stringsAsFactors = FALSE)
  if (!all(grepl("^V[0-9]+$", names(edges)))) names(edges) <- trimws(names(edges))
  nms <- names(edges)
  leader_col <- first_matching_col(nms, c("leader", "from", "source", "node1", "sender", "parent", "u", "a"))
  follower_col <- first_matching_col(nms, c("follower", "follow", "to", "target", "node2", "receiver", "child", "v", "b"))
  weight_col <- first_matching_col(nms, c("wcd", "weight", "weights", "value", "count", "freq", "frequency", "strength", "score", "similarity", "correlation", "r"))
  if (is.na(leader_col) || is.na(follower_col)) {
    validate(need(ncol(edges) >= 2, "Edges data must have at least two columns: Leader/follower."))
    leader_col <- names(edges)[1]
    follower_col <- names(edges)[2]
  }
  if (is.na(weight_col) && ncol(edges) >= 3) weight_col <- names(edges)[3]
  out <- tibble::tibble(
    Leader = trimws(as.character(edges[[leader_col]])),
    follower = trimws(as.character(edges[[follower_col]])),
    WCD = if (!is.na(weight_col)) suppressWarnings(as.numeric(edges[[weight_col]])) else 1
  )
  out$WCD[is.na(out$WCD)] <- 1
  out %>%
    filter(!is.na(Leader), !is.na(follower), nzchar(Leader), nzchar(follower), Leader != follower) %>%
    group_by(Leader, follower) %>% summarise(WCD = sum(WCD, na.rm = TRUE), .groups = "drop")
}

generate_nodes_from_edges <- function(edges) {
  node_names <- sort(unique(c(edges$Leader, edges$follower)))
  deg_tbl <- table(c(edges$Leader, edges$follower))
  strength_tbl <- tapply(c(edges$WCD, edges$WCD), c(edges$Leader, edges$follower), sum, na.rm = TRUE)
  tibble::tibble(
    name = node_names,
    value = as.numeric(strength_tbl[node_names]),
    value2 = as.numeric(deg_tbl[node_names]),
    carac = NA_integer_
  )
}

clean_nodes <- function(nodes, edges = NULL) {
  if (is.null(nodes) || nrow(nodes) == 0) {
    validate(need(!is.null(edges), "Nodes are missing and edges are unavailable; cannot generate nodes."))
    return(generate_nodes_from_edges(edges))
  }
  nodes <- as.data.frame(nodes, stringsAsFactors = FALSE)
  if (!all(grepl("^V[0-9]+$", names(nodes)))) names(nodes) <- trimws(names(nodes))
  nms <- names(nodes)
  name_col <- first_matching_col(nms, c("name", "node", "term", "label", "institution", "author", "keyword", "id"))
  value_col <- first_matching_col(nms, c("value", "size", "weight", "score", "count", "freq", "frequency", "strength"))
  value2_col <- first_matching_col(nms, c("value2", "valueii", "size2", "degree", "freq2", "count2"))
  carac_col <- first_matching_col(nms, c("carac", "cluster", "group", "membership", "class", "community"))
  if (is.na(name_col)) name_col <- names(nodes)[1]
  out <- tibble::tibble(name = trimws(as.character(nodes[[name_col]])))
  out$value <- if (!is.na(value_col)) suppressWarnings(as.numeric(nodes[[value_col]])) else NA_real_
  out$value2 <- if (!is.na(value2_col)) suppressWarnings(as.numeric(nodes[[value2_col]])) else out$value
  out$carac <- if (!is.na(carac_col)) suppressWarnings(as.integer(nodes[[carac_col]])) else NA_integer_
  out <- out %>% filter(!is.na(name), nzchar(name)) %>% distinct(name, .keep_all = TRUE)
  if (!is.null(edges)) {
    gen <- generate_nodes_from_edges(edges)
    out <- full_join(out, gen, by = "name", suffix = c("", ".gen"))
    out$value <- ifelse(is.na(out$value), out$value.gen, out$value)
    out$value2 <- ifelse(is.na(out$value2), out$value2.gen, out$value2)
    out$carac <- ifelse(is.na(out$carac), out$carac.gen, out$carac)
    out <- out %>% select(name, value, value2, carac)
  }
  out$value[is.na(out$value)] <- 1
  out$value2[is.na(out$value2)] <- out$value[is.na(out$value2)]
  out
}

# -----------------------------
# Co-word occurrence CSV -> nodes/edges
# -----------------------------
read_occurrence_file <- function(path) {
  out <- suppressWarnings(tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE, trim_ws = TRUE),
    error = function(e) NULL
  ))
  if (is.null(out) || ncol(out) < 2) {
    out <- suppressWarnings(tryCatch(
      readr::read_delim(path, delim = NULL, show_col_types = FALSE, progress = FALSE, trim_ws = TRUE),
      error = function(e) NULL
    ))
  }
  validate(need(!is.null(out) && nrow(out) > 0 && ncol(out) >= 2,
                "Occurrence CSV must contain at least two columns."))
  as.data.frame(out, stringsAsFactors = FALSE)
}

.numeric_rate <- function(x) {
  z <- suppressWarnings(as.numeric(as.character(x)))
  mean(is.finite(z), na.rm = TRUE)
}

.clean_term <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", "<NA>")] <- NA_character_
  x
}

build_coword_from_term_lists <- function(doc_terms, doc_weights = NULL, top_n_nodes = 100) {
  doc_terms <- lapply(doc_terms, function(z) unique(.clean_term(z)[!is.na(.clean_term(z))]))
  doc_terms <- doc_terms[lengths(doc_terms) > 0]
  validate(need(length(doc_terms) > 0, "No valid terms were detected in the occurrence CSV."))

  # Performance-safe rule for occurrence input:
  # first identify the top N terms by node occurrence count, then build the
  # co-word matrix and edges only from those selected nodes. This avoids
  # crossprod() over thousands of low-frequency terms and keeps FLCA/community
  # clustering lightweight in the R session.
  top_n_nodes <- suppressWarnings(as.integer(top_n_nodes))
  if (!is.finite(top_n_nodes) || top_n_nodes <= 0) top_n_nodes <- 100L

  all_terms <- unlist(doc_terms, use.names = FALSE)
  node_count <- sort(table(all_terms), decreasing = TRUE)
  validate(need(length(node_count) >= 2, "At least two unique terms are required to build co-word edges."))

  selected_terms <- names(node_count)[seq_len(min(top_n_nodes, length(node_count)))]
  selected_terms <- selected_terms[order(-as.numeric(node_count[selected_terms]), selected_terms)]

  doc_terms <- lapply(doc_terms, function(z) intersect(z, selected_terms))
  doc_terms <- doc_terms[lengths(doc_terms) > 0]
  validate(need(length(doc_terms) > 0, "No documents contained the selected top occurrence terms."))

  terms <- selected_terms
  n_docs <- length(doc_terms)
  X <- matrix(0, nrow = n_docs, ncol = length(terms), dimnames = list(NULL, terms))
  for (i in seq_along(doc_terms)) X[i, match(doc_terms[[i]], terms)] <- 1

  C <- crossprod(X)
  diag(C) <- 0
  node_count_selected <- colSums(X)
  nodes <- tibble::tibble(
    name = names(node_count_selected),
    value = as.numeric(node_count_selected),
    value2 = as.numeric(node_count_selected),
    carac = NA_integer_
  ) %>% arrange(desc(value), name)

  ij <- which(C > 0, arr.ind = TRUE)
  ij <- ij[ij[, 1] < ij[, 2], , drop = FALSE]
  if (nrow(ij) > 0) {
    edges <- tibble::tibble(
      Leader = colnames(C)[ij[, 1]],
      follower = colnames(C)[ij[, 2]],
      WCD = as.numeric(C[ij])
    ) %>% arrange(desc(WCD), Leader, follower)
  } else {
    edges <- tibble::tibble(Leader = character(0), follower = character(0), WCD = numeric(0))
  }
  list(
    nodes = nodes,
    edges = edges,
    top_n_nodes = top_n_nodes,
    original_node_count = length(node_count),
    selected_node_count = nrow(nodes)
  )
}

build_coword_from_occurrence <- function(df, top_n_nodes = 100) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- make.names(names(df), unique = TRUE)
  validate(need(ncol(df) >= 2, "Occurrence data needs at least two columns."))

  top_n_nodes <- suppressWarnings(as.integer(top_n_nodes))
  if (!is.finite(top_n_nodes) || top_n_nodes <= 0) top_n_nodes <- 100L

  # Case A: long occurrence table, e.g. ArticleID, Term/Phrase[, weight].
  # This is triggered when column 2 is not numeric, so it will not be mistaken for nodes$name/nodes$value.
  col2_numeric_rate <- .numeric_rate(df[[2]])
  if (!is.finite(col2_numeric_rate) || col2_numeric_rate < 0.80) {
    doc_id <- .clean_term(df[[1]])
    term <- .clean_term(df[[2]])
    keep <- !is.na(doc_id) & !is.na(term)
    validate(need(any(keep), "Long occurrence data should contain document IDs in column 1 and terms in column 2."))
    tmp <- tibble::tibble(doc = doc_id[keep], term = term[keep]) %>% distinct(doc, term)
    doc_terms <- split(tmp$term, tmp$doc)
    out <- build_coword_from_term_lists(doc_terms, top_n_nodes = top_n_nodes)
    out$mode <- paste0(
      "long occurrence: document × term; top ", out$selected_node_count,
      " of ", out$original_node_count,
      " nodes retained before edge generation"
    )
    return(out)
  }

  # Case B: wide document-term matrix. First column may be document ID; numeric columns are terms.
  numeric_cols <- which(vapply(df, function(z) .numeric_rate(z) >= 0.80, logical(1)))
  validate(need(length(numeric_cols) >= 2,
                "Wide occurrence data should have at least two numeric term columns, or use long format: document, term."))
  term_cols <- numeric_cols
  X <- as.matrix(data.frame(lapply(df[term_cols], function(z) suppressWarnings(as.numeric(as.character(z))))))
  X[!is.finite(X)] <- 0
  colnames(X) <- names(df)[term_cols]
  keep_col <- colSums(abs(X), na.rm = TRUE) > 0
  X <- X[, keep_col, drop = FALSE]
  validate(need(ncol(X) >= 2, "At least two non-zero term columns are required to build co-word edges."))

  B <- ifelse(X > 0, 1, 0)
  node_value <- colSums(B)
  node_value2 <- colSums(X)
  original_node_count <- length(node_value)

  # Keep only the top N nodes before crossprod() and all later clustering.
  ord <- order(-node_value, names(node_value))
  keep_idx <- ord[seq_len(min(top_n_nodes, length(ord)))]
  X <- X[, keep_idx, drop = FALSE]
  B <- B[, keep_idx, drop = FALSE]
  node_value <- node_value[keep_idx]
  node_value2 <- node_value2[keep_idx]

  validate(need(ncol(B) >= 2, "At least two selected top occurrence terms are required to build co-word edges."))
  C <- crossprod(B)
  diag(C) <- 0
  nodes <- tibble::tibble(
    name = colnames(B),
    value = as.numeric(node_value),
    value2 = as.numeric(node_value2),
    carac = NA_integer_
  ) %>% arrange(desc(value), name)

  ij <- which(C > 0, arr.ind = TRUE)
  ij <- ij[ij[, 1] < ij[, 2], , drop = FALSE]
  edges <- if (nrow(ij) > 0) {
    tibble::tibble(Leader = colnames(C)[ij[, 1]], follower = colnames(C)[ij[, 2]], WCD = as.numeric(C[ij])) %>%
      arrange(desc(WCD), Leader, follower)
  } else {
    tibble::tibble(Leader = character(0), follower = character(0), WCD = numeric(0))
  }
  list(
    nodes = nodes,
    edges = edges,
    mode = paste0(
      "wide occurrence: document × term matrix; top ", nrow(nodes),
      " of ", original_node_count,
      " nodes retained before edge generation"
    ),
    top_n_nodes = top_n_nodes,
    original_node_count = original_node_count,
    selected_node_count = nrow(nodes)
  )
}

build_graphs <- function(nodes, data, eps = 1e-6) {
  all_names <- sort(unique(c(nodes$name, data$Leader, data$follower)))
  vertex_df <- tibble(name = all_names) %>% left_join(nodes, by = "name")
  vertex_df$value[is.na(vertex_df$value)] <- 1
  vertex_df$value2[is.na(vertex_df$value2)] <- vertex_df$value[is.na(vertex_df$value2)]

  g_full <- igraph::graph_from_data_frame(d = data, vertices = vertex_df, directed = FALSE)
  E(g_full)$weight <- E(g_full)$WCD
  g_full <- igraph::simplify(
    g_full, remove.multiple = TRUE, remove.loops = TRUE,
    edge.attr.comb = list(weight = "sum", WCD = "sum")
  )
  E(g_full)$weight <- pmax(E(g_full)$weight, eps)

  edges0 <- data %>%
    transmute(a = Leader, b = follower, w = WCD) %>%
    mutate(u = pmin(a, b), v = pmax(a, b), eid = paste(u, v, sep = "||"))
  long_inc <- bind_rows(
    edges0 %>% transmute(node = a, eid, w),
    edges0 %>% transmute(node = b, eid, w)
  )
  best_by_node <- long_inc %>%
    group_by(node) %>% arrange(desc(w), eid, .by_group = TRUE) %>% slice(1) %>% ungroup()
  edges_reduced <- edges0 %>%
    filter(eid %in% unique(best_by_node$eid)) %>%
    transmute(Leader = u, follower = v, WCD = w)

  g_flca <- igraph::graph_from_data_frame(d = edges_reduced, vertices = vertex_df, directed = FALSE)
  E(g_flca)$weight <- E(g_flca)$WCD
  g_flca <- igraph::simplify(
    g_flca, remove.multiple = TRUE, remove.loops = TRUE,
    edge.attr.comb = list(weight = "sum", WCD = "sum")
  )
  E(g_flca)$weight <- pmax(E(g_flca)$weight, eps)

  list(vertex_df = vertex_df, g_full = g_full, g_flca = g_flca, edges_reduced = edges_reduced)
}

# Deploy-safe FLCA fallback: strongest incident edge for each node, then components.
get_flca_labels_on_graph <- function(g) {
  if (vcount(g) == 0) return(integer(0))
  if (ecount(g) == 0) return(seq_len(vcount(g)))
  best_map <- lapply(seq_len(vcount(g)), function(i) {
    nbrs <- neighbors(g, i)
    if (length(nbrs) == 0) return(NA_character_)
    ws <- sapply(as.integer(nbrs), function(j) {
      eid <- get.edge.ids(g, c(i, j), directed = FALSE, error = FALSE)
      if (eid == 0) NA_real_ else E(g)[eid]$weight
    })
    igraph::V(g)$name[as.integer(nbrs)[which.max(ws)]]
  })
  arcs <- data.frame(from = V(g)$name, to = unlist(best_map), stringsAsFactors = FALSE)
  arcs <- arcs[!is.na(arcs$to), , drop = FALSE]
  if (!nrow(arcs)) return(seq_len(vcount(g)))
  gg <- igraph::graph_from_data_frame(arcs, directed = TRUE, vertices = data.frame(name = V(g)$name))
  as.integer(igraph::components(igraph::as.undirected(gg, mode = "collapse"))$membership[V(g)$name])
}

as_int_membership <- function(x) as.integer(unname(x))

safe_modularity <- function(g, memb) {
  if (is.null(memb) || all(is.na(memb))) return(NA_real_)
  if (length(unique(memb[!is.na(memb)])) < 2L) return(0)
  suppressWarnings(tryCatch(
    igraph::modularity(g, memb, weights = E(g)$weight),
    error = function(e) NA_real_
  ))
}

silhouette_from_membership <- function(g, memb, unreachable = c("ignore", "penalize"), alpha = 1.05) {
  unreachable <- match.arg(unreachable)
  n <- vcount(g)
  if (is.null(memb) || length(memb) != n) return(list(per_vertex = rep(NA_real_, n), mean_sil = NA_real_))
  labs <- memb
  if (length(unique(labs[!is.na(labs)])) < 2L) return(list(per_vertex = rep(0, n), mean_sil = 0))

  w <- E(g)$weight; w[is.na(w)] <- 0; w <- pmax(w, .Machine$double.eps)
  E(g)$length <- 1 / w
  comp <- igraph::components(g)$membership
  sil <- rep(NA_real_, n)

  global_Dmax <- 0
  if (unreachable == "penalize") {
    for (cid in unique(comp)) {
      idx <- which(comp == cid)
      if (length(idx) < 2L) next
      sg <- igraph::induced_subgraph(g, idx)
      D <- suppressWarnings(igraph::distances(sg, weights = E(sg)$length, mode = "all"))
      finite_vals <- D[is.finite(D) & D > 0]
      if (length(finite_vals)) global_Dmax <- max(global_Dmax, max(finite_vals))
    }
    if (!is.finite(global_Dmax) || global_Dmax <= 0) global_Dmax <- 1
    global_Dmax <- global_Dmax * alpha
  }

  for (cid in sort(unique(comp))) {
    idx <- which(comp == cid & !is.na(labs))
    if (length(idx) == 0L) next
    sg <- igraph::induced_subgraph(g, idx)
    subm <- labs[idx]
    D <- suppressWarnings(igraph::distances(sg, weights = E(sg)$length, mode = "all"))
    for (j in seq_along(idx)) {
      cj <- subm[j]
      same <- which(subm == cj)
      a_i <- if (length(same) > 1L) mean(D[j, setdiff(same, j)], na.rm = TRUE) else 0
      others <- setdiff(unique(subm), cj)
      if (length(others) > 0L) {
        b_i <- min(sapply(others, function(oc) mean(D[j, which(subm == oc)], na.rm = TRUE)))
        if (!is.finite(b_i)) b_i <- if (unreachable == "penalize") global_Dmax else a_i
      } else {
        b_i <- if (unreachable == "penalize") global_Dmax else a_i
      }
      denom <- max(a_i, b_i)
      s_ij <- if (denom > 0) (b_i - a_i) / denom else 0
      if (!is.finite(s_ij) || abs(b_i - a_i) < .Machine$double.eps) s_ij <- 0
      sil[idx[j]] <- s_ij
    }
  }
  list(per_vertex = sil, mean_sil = mean(sil, na.rm = TRUE))
}

safe_compare <- function(a, b, method) {
  if (is.null(a) || is.null(b) || length(a) != length(b)) return(NA_real_)
  keep <- !(is.na(a) | is.na(b))
  if (!any(keep)) return(NA_real_)
  suppressWarnings(tryCatch(igraph::compare(a[keep], b[keep], method = method), error = function(e) NA_real_))
}

make_flca_consistent_graph <- function(g_reduced, memb) {
  if (ecount(g_reduced) == 0) return(g_reduced)
  ep <- igraph::ends(g_reduced, E(g_reduced), names = FALSE)
  keep <- memb[ep[, 1]] == memb[ep[, 2]]
  if (all(!keep)) return(g_reduced)
  igraph::delete_edges(g_reduced, E(g_reduced)[!keep])
}

make_components_independent <- function(g, target_k = 4, start_qtl = 0.40) {
  if (ecount(g) == 0) return(list(graph = g, membership = rep(1L, vcount(g))))
  w <- E(g)$weight
  q <- start_qtl
  g_cut <- g
  comp <- igraph::components(g_cut)
  while (length(unique(comp$membership)) < target_k && ecount(g_cut) > 0 && q < 0.99) {
    thr <- as.numeric(quantile(w, probs = q, names = FALSE, type = 7))
    g_cut <- igraph::delete_edges(g, E(g)[weight < thr])
    comp <- igraph::components(g_cut)
    q <- q + 0.05
  }
  list(graph = g_cut, membership = as.integer(comp$membership))
}

safe_scores <- function(g, memb, unreachable = c("ignore", "penalize"), alpha = 1.05) {
  unreachable <- match.arg(unreachable)
  keepV <- which(igraph::degree(g) > 0)
  if (length(keepV) >= 2) {
    g_eval <- igraph::induced_subgraph(g, keepV)
    memb_eval <- memb[keepV]
  } else {
    g_eval <- g
    memb_eval <- memb
  }
  k <- length(unique(memb_eval[!is.na(memb_eval)]))
  if (k < 2) return(list(Q = 1e-9, SS = 1e-9))
  Q <- safe_modularity(g_eval, memb_eval)
  SS <- silhouette_from_membership(g_eval, memb_eval, unreachable = unreachable, alpha = alpha)$mean_sil
  if (!is.na(Q) && Q <= 0) Q <- 1e-9
  if (!is.na(SS) && SS <= 0) SS <- 1e-9
  list(Q = Q, SS = SS)
}

run_method <- function(g, method) {
  tryCatch({
    if (method == "louvain") out <- cluster_louvain(g, weights = E(g)$weight)
    if (method == "components") out <- components(g)
    if (method == "edge_betweenness") out <- cluster_edge_betweenness(g, weights = E(g)$weight)
    if (method == "label_prop") out <- cluster_label_prop(g, weights = E(g)$weight)
    if (method == "infomap") out <- cluster_infomap(g, e.weights = E(g)$weight)
    if (method == "leading_eigen") out <- cluster_leading_eigen(g, weights = E(g)$weight)
    if (method == "walktrap") out <- cluster_walktrap(g, weights = E(g)$weight)
    if (method == "fast_greedy") out <- cluster_fast_greedy(g, weights = E(g)$weight)
    if (method == "optimal") out <- cluster_optimal(g)
    if (inherits(out, "communities")) {
      memb <- as_int_membership(membership(out))
    } else if (is.list(out) && !is.null(out$membership)) {
      memb <- as_int_membership(out$membership)
    } else {
      stop("No membership returned.")
    }
    list(membership = memb, status = "ok", details = "")
  }, error = function(e) {
    list(membership = rep(NA_integer_, vcount(g)), status = "failed", details = conditionMessage(e))
  })
}

analyze_all <- function(nodes, edges, target_k = 4) {
  gr <- build_graphs(nodes, edges)
  g_full <- gr$g_full
  g_flca <- gr$g_flca
  all_vertices <- V(g_full)$name

  flca_carac <- get_flca_labels_on_graph(g_flca)
  if (length(flca_carac) != vcount(g_flca)) flca_carac <- components(g_flca)$membership
  names(flca_carac) <- V(g_flca)$name
  flca_full <- as.integer(flca_carac[match(V(g_full)$name, names(flca_carac))])

  methods <- c("louvain", "optimal", "components", "edge_betweenness", "label_prop", "infomap", "leading_eigen", "walktrap", "fast_greedy")
  res_full <- lapply(methods, function(m) run_method(g_full, m))
  names(res_full) <- methods

  memberships_df <- tibble(name = all_vertices)
  for (m in methods) memberships_df[[m]] <- res_full[[m]]$membership
  memberships_df$FLCA <- flca_full

  quality_df <- tibble(
    method = character(), n_clusters = integer(), modularity = double(), mean_silhouette = double(),
    ARI_vs_FLCA = double(), NMI_vs_FLCA = double(), status = character(), details = character(), eval_graph = character()
  )

  for (m in methods) {
    memb <- res_full[[m]]$membership
    if (all(is.na(memb))) {
      quality_df <- add_row(quality_df, method = m, n_clusters = NA_integer_, modularity = NA_real_,
                            mean_silhouette = NA_real_, ARI_vs_FLCA = NA_real_, NMI_vs_FLCA = NA_real_,
                            status = res_full[[m]]$status, details = res_full[[m]]$details, eval_graph = "full")
      next
    }
    sil <- if (m == "components") safe_scores(g_full, memb, unreachable = "penalize", alpha = 1.10)$SS else
      silhouette_from_membership(g_full, memb, unreachable = "ignore")$mean_sil
    quality_df <- add_row(
      quality_df,
      method = m,
      n_clusters = length(unique(memb[!is.na(memb)])),
      modularity = safe_modularity(g_full, memb),
      mean_silhouette = sil,
      ARI_vs_FLCA = safe_compare(memb, flca_full, "adjusted.rand"),
      NMI_vs_FLCA = safe_compare(memb, flca_full, "nmi"),
      status = res_full[[m]]$status,
      details = res_full[[m]]$details,
      eval_graph = "full"
    )
  }

  g_flca_consistent <- make_flca_consistent_graph(g_flca, as.integer(flca_carac[V(g_flca)$name]))
  quality_df <- add_row(
    quality_df,
    method = "FLCA",
    n_clusters = length(unique(flca_full[!is.na(flca_full)])),
    modularity = safe_modularity(g_flca_consistent, as.integer(flca_carac[V(g_flca)$name])),
    mean_silhouette = silhouette_from_membership(g_flca_consistent, as.integer(flca_carac[V(g_flca)$name]), unreachable = "penalize")$mean_sil,
    ARI_vs_FLCA = NA_real_, NMI_vs_FLCA = NA_real_, status = "ok",
    details = "Deploy-safe max-link FLCA fallback; evaluated on FLCA-consistent reduced graph.",
    eval_graph = "reduced_consistent"
  )

  comp_ind <- make_components_independent(g_full, target_k = target_k, start_qtl = 0.40)
  sc_comp <- safe_scores(comp_ind$graph, comp_ind$membership, unreachable = "penalize", alpha = 1.05)
  comp_full <- rep(NA_integer_, vcount(g_full))
  comp_full[match(V(comp_ind$graph)$name, V(g_full)$name)] <- comp_ind$membership
  quality_df <- add_row(
    quality_df,
    method = paste0("components_independent_k", target_k),
    n_clusters = length(unique(comp_ind$membership)),
    modularity = sc_comp$Q,
    mean_silhouette = sc_comp$SS,
    ARI_vs_FLCA = safe_compare(comp_full, flca_full, "adjusted.rand"),
    NMI_vs_FLCA = safe_compare(comp_full, flca_full, "nmi"),
    status = "ok", details = "Weak edges dropped until target number of components is reached.", eval_graph = "thresholded_full"
  )

  ranking_df <- quality_df %>% filter(method != "FLCA") %>% arrange(desc(ARI_vs_FLCA), desc(NMI_vs_FLCA), desc(modularity))

  list(
    nodes = nodes, edges = edges, vertex_df = gr$vertex_df,
    g_full = g_full, g_flca = g_flca, edges_reduced = gr$edges_reduced,
    memberships_df = memberships_df, quality_df = quality_df, ranking_df = ranking_df,
    flca_carac = flca_carac
  )
}

make_cluster_palette <- function(groups) {
  groups <- sort(unique(as.character(groups)))
  n <- length(groups)
  base <- if (n <= 8) RColorBrewer::brewer.pal(max(3, n), "Set2") else grDevices::rainbow(n, s = 0.65, v = 0.9)
  stats::setNames(base[seq_len(n)], groups)
}

rescale_numeric <- function(x, to = c(18, 58)) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- NA_real_
  if (all(is.na(x))) return(rep(mean(to), length(x)))
  x[is.na(x)] <- min(x, na.rm = TRUE)
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(mean(to), length(x)))
  to[1] + (x - rng[1]) / diff(rng) * diff(to)
}


# ---- Interactive-dashboard graph filters ------------------------------------
community_methods <- c(
  "louvain", "optimal", "components", "edge_betweenness", "label_prop",
  "infomap", "leading_eigen", "walktrap", "fast_greedy", "components_independent_k"
)

get_membership_for_method <- function(res, method) {
  validate(need(!is.null(res) && is.data.frame(res$memberships_df), "Run analysis first."))
  if (identical(method, "components_independent_k")) {
    qrow <- res$quality_df[grepl("^components_independent_k", res$quality_df$method), , drop = FALSE]
    if (nrow(qrow) > 0) method <- qrow$method[1]
    # If the independent k membership is not stored in memberships_df, fall back to components.
    if (!method %in% names(res$memberships_df)) method <- "components"
  }
  validate(need(method %in% names(res$memberships_df), paste("No membership column for", method)))
  memb <- suppressWarnings(as.integer(res$memberships_df[[method]]))
  names(memb) <- res$memberships_df$name
  memb
}

subset_graph_by_top_nodes <- function(g, memb_named = NULL, top_n = 30) {
  validate(need(igraph::vcount(g) > 0, "No graph available."))
  top_n <- suppressWarnings(as.integer(top_n))
  if (!is.finite(top_n) || top_n <= 0) top_n <- igraph::vcount(g)
  vals <- igraph::V(g)$value
  if (is.null(vals)) vals <- rep(1, igraph::vcount(g))
  vals <- suppressWarnings(as.numeric(vals)); vals[!is.finite(vals)] <- 0
  ord <- order(-vals, igraph::V(g)$name)
  keep_names <- igraph::V(g)$name[utils::head(ord, min(top_n, igraph::vcount(g)))]
  sg <- igraph::induced_subgraph(g, vids = keep_names)
  if (!is.null(memb_named)) {
    mm <- as.integer(memb_named[match(igraph::V(sg)$name, names(memb_named))])
  } else {
    mm <- rep(1L, igraph::vcount(sg))
  }
  list(graph = sg, membership = mm)
}

subset_flca_major_sampling <- function(g, memb_named, top_clusters = 6, n_per_cluster = 3, major_sampling = TRUE) {
  validate(need(igraph::vcount(g) > 0, "No FLCA graph available."))
  mm_all <- as.integer(memb_named[match(igraph::V(g)$name, names(memb_named))])
  vals <- igraph::V(g)$value
  if (is.null(vals)) vals <- rep(1, igraph::vcount(g))
  vals <- suppressWarnings(as.numeric(vals)); vals[!is.finite(vals)] <- 0

  if (!isTRUE(major_sampling)) {
    top_n <- max(1L, suppressWarnings(as.integer(top_clusters)) * suppressWarnings(as.integer(n_per_cluster)))
    return(subset_graph_by_top_nodes(g, memb_named, top_n = top_n))
  }

  top_clusters <- suppressWarnings(as.integer(top_clusters)); if (!is.finite(top_clusters) || top_clusters <= 0) top_clusters <- 6L
  n_per_cluster <- suppressWarnings(as.integer(n_per_cluster)); if (!is.finite(n_per_cluster) || n_per_cluster <= 0) n_per_cluster <- 3L

  cl <- sort(unique(mm_all[!is.na(mm_all)]))
  validate(need(length(cl) > 0, "No FLCA cluster membership available."))
  cl_score <- sapply(cl, function(cc) sum(vals[mm_all == cc], na.rm = TRUE))
  top_cl <- cl[order(cl_score, decreasing = TRUE)][seq_len(min(top_clusters, length(cl)))]

  keep <- character(0)
  for (cc in top_cl) {
    idx <- which(mm_all == cc)
    idx <- idx[order(-vals[idx], igraph::V(g)$name[idx])]
    # Major sampling: keep at least n nodes in each selected top cluster, limited by available nodes.
    keep <- c(keep, igraph::V(g)$name[utils::head(idx, min(n_per_cluster, length(idx)))])
  }
  keep <- unique(keep)
  sg <- igraph::induced_subgraph(g, vids = keep)
  list(graph = sg, membership = as.integer(memb_named[match(igraph::V(sg)$name, names(memb_named))]))
}

make_visnetwork <- function(g, memb = NULL, title = "Network", label_font_size = 26, label_bold = TRUE) {
  validate(need(vcount(g) > 0, "No graph available."))

  vals <- V(g)$value
  if (is.null(vals)) vals <- rep(1, vcount(g))
  vals[is.na(vals)] <- 1
  sizes <- rescale_numeric(sqrt(vals), to = c(18, 55))

  if (is.null(memb) || length(memb) != vcount(g)) memb <- rep(1L, vcount(g))
  groups <- as.character(memb)
  pal <- make_cluster_palette(groups)

  nodes <- data.frame(
    id = V(g)$name,
    label = V(g)$name,
    group = paste0("Cluster ", groups),
    value = sizes,
    color.background = unname(pal[groups]),
    color.border = "#34495e",
    font.size = label_font_size,
    font.face = if (isTRUE(label_bold)) "Arial Black" else "arial",
    title = paste0(
      "<b>", V(g)$name, "</b><br>",
      "Cluster: ", groups, "<br>",
      "Value: ", round(vals, 3)
    ),
    stringsAsFactors = FALSE
  )

  ed <- igraph::as_data_frame(g, what = "edges")
  if (nrow(ed) > 0) {
    w <- if ("weight" %in% names(ed)) ed$weight else rep(1, nrow(ed))
    edges <- data.frame(
      from = ed$from,
      to = ed$to,
      value = rescale_numeric(log1p(w), to = c(1, 8)),
      width = rescale_numeric(log1p(w), to = c(1, 7)),
      title = paste0("Weight: ", round(w, 4)),
      color = "rgba(120,120,120,0.45)",
      smooth = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    edges <- data.frame(from = character(0), to = character(0))
  }

  visNetwork(nodes, edges, main = title, height = "720px", width = "100%") %>%
    visGroups(groupname = unique(nodes$group)) %>%
    visNodes(
      font = list(
        size = label_font_size,
        face = if (isTRUE(label_bold)) "Arial Black" else "arial",
        color = "#111111",
        strokeWidth = 3,
        strokeColor = "#ffffff"
      )
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = TRUE,
      selectedBy = "group"
    ) %>%
    visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(gravitationalConstant = -80, centralGravity = 0.01, springLength = 180, springConstant = 0.08),
      stabilization = list(enabled = TRUE, iterations = 1200)
    ) %>%
    visInteraction(navigationButtons = TRUE, keyboard = TRUE, hover = TRUE) %>%
    visLayout(randomSeed = 123)
}


# -----------------------------
# FLCA 6-step process helpers
# -----------------------------
make_flca_step_nodes <- function(xres, top_clusters = 6, n_per_cluster = 3, major_sampling = TRUE) {
  req(xres)
  g <- xres$g_flca
  memb <- as.integer(xres$flca_carac[igraph::V(g)$name])
  if (length(memb) != igraph::vcount(g) || any(is.na(memb))) memb <- igraph::components(g)$membership

  nd <- data.frame(
    name = igraph::V(g)$name,
    value = suppressWarnings(as.numeric(igraph::V(g)$value)),
    value2 = suppressWarnings(as.numeric(igraph::V(g)$value2)),
    carac = as.integer(memb),
    stringsAsFactors = FALSE
  )
  nd$value[!is.finite(nd$value)] <- 1
  nd$value2[!is.finite(nd$value2)] <- nd$value[!is.finite(nd$value2)]

  # Per-node silhouette on the FLCA reduced graph.
  sil <- silhouette_from_membership(g, memb, unreachable = "penalize")$per_vertex
  nd$sil_width <- suppressWarnings(as.numeric(sil))
  nd$sil_width[!is.finite(nd$sil_width)] <- 0

  ed <- igraph::as_data_frame(g, what = "edges")
  if (nrow(ed)) {
    names(ed)[1:2] <- c("Leader", "follower")
    if (!"weight" %in% names(ed)) ed$weight <- 1
    ed$WCD <- suppressWarnings(as.numeric(ed$weight))
  } else {
    ed <- data.frame(Leader=character(0), follower=character(0), WCD=numeric(0), stringsAsFactors=FALSE)
  }

  leader_tbl <- nd |>
    dplyr::group_by(carac) |>
    dplyr::arrange(dplyr::desc(value), .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(carac, leader = name, leader_value = value)

  nd <- nd |> dplyr::left_join(leader_tbl, by = "carac")
  nd$role <- ifelse(nd$name == nd$leader, "leader", "follower")
  nd$neighbor_name <- nd$leader
  nd$neighborC <- nd$carac

  if (nrow(ed)) {
    ed2 <- ed
    ed2$a <- pmin(ed2$Leader, ed2$follower)
    ed2$b <- pmax(ed2$Leader, ed2$follower)
    nd$wsel <- NA_real_
    for (i in seq_len(nrow(nd))) {
      a <- min(nd$name[i], nd$leader[i]); b <- max(nd$name[i], nd$leader[i])
      hit <- ed2[ed2$a == a & ed2$b == b, , drop=FALSE]
      if (nrow(hit)) nd$wsel[i] <- max(hit$WCD, na.rm=TRUE)
    }
  } else {
    nd$wsel <- NA_real_
  }

  target_n <- 20L
  if (isTRUE(major_sampling)) {
    # FLCA-MA rule: first sample top clusters, then force-fill to target_n.
    # Therefore, if at least 20 FLCA nodes exist, the selected Top20 must contain 20 members.
    top_clusters <- suppressWarnings(as.integer(top_clusters)); if (!is.finite(top_clusters) || top_clusters <= 0) top_clusters <- 6L
    n_per_cluster <- suppressWarnings(as.integer(n_per_cluster)); if (!is.finite(n_per_cluster) || n_per_cluster <= 0) n_per_cluster <- 3L

    cl_tbl <- nd |>
      dplyr::group_by(carac) |>
      dplyr::summarise(n = dplyr::n(), cluster_score = sum(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(n), dplyr::desc(cluster_score), carac)
    keep_carac <- utils::head(cl_tbl$carac, min(top_clusters, nrow(cl_tbl)))

    picked <- character(0)
    remain <- list()
    for (cc in keep_carac) {
      sub <- nd[nd$carac == cc, , drop = FALSE]
      sub <- sub[order(-sub$value, -sub$sil_width, sub$name), , drop = FALSE]
      take <- utils::head(sub$name, min(n_per_cluster, nrow(sub)))
      picked <- unique(c(picked, take))
      remain[[as.character(cc)]] <- sub$name[!(sub$name %in% take)]
    }

    # Fill remaining slots from selected top clusters by round-robin, starting from C1/top clusters.
    if (length(picked) < min(target_n, nrow(nd)) && length(keep_carac) > 0) {
      need <- min(target_n, nrow(nd)) - length(picked)
      cids <- as.character(keep_carac)
      i <- 1L
      while (need > 0 && any(vapply(remain[cids], length, integer(1)) > 0)) {
        cid <- cids[((i - 1L) %% length(cids)) + 1L]
        if (length(remain[[cid]]) > 0) {
          nxt <- remain[[cid]][1]
          remain[[cid]] <- remain[[cid]][-1]
          if (!(nxt %in% picked)) {
            picked <- c(picked, nxt)
            need <- need - 1L
          }
        }
        i <- i + 1L
        if (i > 100000L) break
      }
    }

    # Still short: add highest-value nodes globally until Top20 is complete.
    if (length(picked) < min(target_n, nrow(nd))) {
      rest <- nd[!(nd$name %in% picked), , drop = FALSE]
      rest <- rest[order(-rest$value, -rest$sil_width, rest$name), , drop = FALSE]
      picked <- unique(c(picked, utils::head(rest$name, min(target_n, nrow(nd)) - length(picked))))
    }
    keep <- nd[nd$name %in% picked, , drop = FALSE]
    keep <- keep[order(keep$carac, -keep$value, -keep$sil_width, keep$name), , drop = FALSE]
  } else {
    keep <- nd[order(-nd$value, -nd$sil_width, nd$name), , drop = FALSE]
    keep <- utils::head(keep, min(target_n, nrow(keep)))
  }

  # Edges for the selected graph: keep only visible selected endpoints.
  if (nrow(ed)) {
    ed_keep <- ed[ed$Leader %in% keep$name & ed$follower %in% keep$name, c("Leader","follower","WCD"), drop=FALSE]
  } else ed_keep <- ed[, c("Leader","follower","WCD"), drop=FALSE]

  list(nodes = keep, all_nodes = nd, edges = ed_keep, all_edges = ed[, c("Leader","follower","WCD"), drop=FALSE])
}

make_flca_cluster_summary <- function(step) {
  nd <- step$nodes
  validate(need(nrow(nd) > 0, "No FLCA nodes available."))
  nd |>
    dplyr::group_by(carac) |>
    dplyr::summarise(
      leader = dplyr::first(leader[order(-value)]),
      n = dplyr::n(),
      total_value = sum(value, na.rm = TRUE),
      mean_silhouette = mean(sil_width, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_value))
}

make_flca_results_for_ssplot <- function(step) {
  sm <- make_flca_cluster_summary(step)
  overall <- data.frame(
    Cluster = "OVERALL",
    SS = mean(step$nodes$sil_width, na.rm = TRUE),
    Qw = NA_real_, Qu = NA_real_, stringsAsFactors = FALSE
  )
  per <- data.frame(
    Cluster = paste0("C", sm$carac),
    SS = sm$mean_silhouette,
    Qw = NA_real_, Qu = NA_real_, stringsAsFactors = FALSE
  )
  rbind(overall, per)
}


.compute_aac3 <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  v <- v[is.finite(v) & v > 0]
  if (length(v) < 2) return(NA_real_)
  v <- sort(v, decreasing = TRUE)
  if (length(v) == 2) {
    r <- v[1] / v[2]
    return(ifelse(is.finite(r) && r > 0, r / (1 + r), NA_real_))
  }
  t3 <- v[1:3]
  r <- (t3[1] / t3[2]) / (t3[2] / t3[3])
  ifelse(is.finite(r) && r > 0, r / (1 + r), NA_real_)
}

make_flca_kano_aac_summary <- function(step) {
  nd <- as.data.frame(step$nodes, stringsAsFactors = FALSE)
  validate(need(nrow(nd) > 0, "No selected FLCA nodes available for AAC summary."))
  nd$value <- suppressWarnings(as.numeric(nd$value))
  nd$value2 <- suppressWarnings(as.numeric(nd$value2))
  nd$carac_chr <- paste0("C", as.character(nd$carac))
  per <- nd |>
    dplyr::group_by(carac_chr) |>
    dplyr::summarise(
      n = dplyr::n(),
      AAC_value = round(.compute_aac3(value), 3),
      AAC_value2 = round(.compute_aac3(value2), 3),
      top_value_node = name[which.max(value)],
      top_value2_node = name[which.max(value2)],
      .groups = "drop"
    ) |>
    dplyr::rename(carac = carac_chr) |>
    dplyr::arrange(carac)
  overall <- data.frame(
    carac = "OVERALL",
    n = nrow(nd),
    AAC_value = round(.compute_aac3(nd$value), 3),
    AAC_value2 = round(.compute_aac3(nd$value2), 3),
    top_value_node = nd$name[which.max(nd$value)],
    top_value2_node = nd$name[which.max(nd$value2)],
    stringsAsFactors = FALSE
  )
  dplyr::bind_rows(overall, as.data.frame(per, stringsAsFactors = FALSE))
}

make_flca_kano_nodes <- function(step) {
  nd <- as.data.frame(step$nodes, stringsAsFactors = FALSE)
  nd$value <- suppressWarnings(as.numeric(nd$value))
  nd$value2 <- suppressWarnings(as.numeric(nd$value2))
  nd$value[!is.finite(nd$value)] <- 1
  nd$value2[!is.finite(nd$value2)] <- nd$value[!is.finite(nd$value2)]
  nd$carac <- as.factor(nd$carac)
  nd[, c("name", "value", "value2", "carac"), drop = FALSE]
}

make_interactive_chord_dashboard <- function(step, top_links = 60) {
  lf <- make_flca_leader_follower_edges(step)
  validate(need(nrow(lf) > 0, "No leader-follower links available."))
  lf$WCD <- suppressWarnings(as.numeric(lf$WCD))
  lf <- lf[is.finite(lf$WCD) & lf$WCD > 0, , drop = FALSE]
  lf <- lf[order(-lf$WCD, lf$Leader, lf$follower), , drop = FALSE]
  top_links <- suppressWarnings(as.integer(top_links))
  if (!is.finite(top_links) || top_links <= 0) top_links <- nrow(lf)
  lf <- utils::head(lf, min(top_links, nrow(lf)))

  nd <- as.data.frame(step$nodes, stringsAsFactors = FALSE)
  ids <- unique(c(lf$Leader, lf$follower))
  nd2 <- nd[match(ids, nd$name), , drop = FALSE]
  missing <- is.na(nd2$name)
  if (any(missing)) {
    nd2[missing, "name"] <- ids[missing]
    nd2[missing, "carac"] <- 0
    nd2[missing, "value"] <- 1
  }
  nd2$value <- suppressWarnings(as.numeric(nd2$value)); nd2$value[!is.finite(nd2$value)] <- 1
  groups <- as.character(nd2$carac)
  pal <- make_cluster_palette(groups)
  nodes <- data.frame(
    id = nd2$name,
    label = nd2$name,
    group = paste0("Cluster ", groups),
    value = rescale_numeric(sqrt(pmax(nd2$value, 0)), to = c(18, 52)),
    color.background = unname(pal[groups]),
    font.size = 24,
    font.face = "Arial Black",
    title = paste0("<b>", nd2$name, "</b><br>Cluster: ", groups, "<br>value=", round(nd2$value, 3), "<br>value2=", round(suppressWarnings(as.numeric(nd2$value2)), 3)),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    from = lf$Leader,
    to = lf$follower,
    value = rescale_numeric(log1p(lf$WCD), to = c(1, 8)),
    width = rescale_numeric(log1p(lf$WCD), to = c(1, 7)),
    arrows = "to",
    title = paste0("WCD: ", round(lf$WCD, 4)),
    smooth = TRUE,
    stringsAsFactors = FALSE
  )
  visNetwork(nodes, edges, main = "Interactive FLCA leader-follower chord dashboard", height = "760px", width = "100%") |>
    visNodes(font = list(size = 24, face = "Arial Black", strokeWidth = 3, strokeColor = "#ffffff")) |>
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE, selectedBy = "group") |>
    visEdges(smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.35)) |>
    visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -90, centralGravity = 0.005, springLength = 220, springConstant = 0.05), stabilization = list(enabled = TRUE, iterations = 1600)) |>
    visInteraction(navigationButtons = TRUE, keyboard = TRUE, hover = TRUE) |>
    visLayout(randomSeed = 123)
}

make_flca_leader_follower_edges <- function(step, add_missing_selected = TRUE) {
  nd <- as.data.frame(step$nodes, stringsAsFactors = FALSE)
  if (!nrow(nd)) return(data.frame(Leader=character(0), follower=character(0), WCD=numeric(0), link_type=character(0), stringsAsFactors=FALSE))
  nd$name <- as.character(nd$name)
  nd$carac <- as.character(nd$carac)
  if (!"value" %in% names(nd)) nd$value <- 1
  if (!"value2" %in% names(nd)) nd$value2 <- nd$value
  nd$value <- suppressWarnings(as.numeric(nd$value)); nd$value[!is.finite(nd$value)] <- 1
  nd$value2 <- suppressWarnings(as.numeric(nd$value2)); nd$value2[!is.finite(nd$value2)] <- nd$value

  # Main links: one visible leader-follower link per selected follower.
  lf <- nd |>
    dplyr::filter(!is.na(leader), nzchar(as.character(leader)), name != leader) |>
    dplyr::transmute(
      Leader = as.character(leader),
      follower = as.character(name),
      WCD = suppressWarnings(as.numeric(wsel)),
      link_type = "leader-follower"
    )
  if (nrow(lf)) {
    lf$WCD[!is.finite(lf$WCD) | lf$WCD <= 0] <- suppressWarnings(as.numeric(nd$value2[match(lf$follower, nd$name)]))
    lf$WCD[!is.finite(lf$WCD) | lf$WCD <= 0] <- 1
  }

  # Important: Sankey/chord plots only display nodes that occur in links.
  # Therefore, add low-weight cluster-anchor links for selected FLCA nodes
  # that have no visible leader-follower relation, so Top20 does not shrink.
  if (isTRUE(add_missing_selected)) {
    present <- unique(c(lf$Leader, lf$follower))
    miss <- nd[!(nd$name %in% present), , drop = FALSE]
    if (nrow(miss)) {
      anchor <- paste0("Cluster C", miss$carac, " selected")
      w0 <- suppressWarnings(min(lf$WCD[is.finite(lf$WCD) & lf$WCD > 0], na.rm = TRUE))
      if (!is.finite(w0)) w0 <- 1
      filler <- data.frame(
        Leader = anchor,
        follower = miss$name,
        WCD = pmax(0.10 * w0, 0.01),
        link_type = "selected-node-anchor",
        stringsAsFactors = FALSE
      )
      lf <- dplyr::bind_rows(lf, filler)
    }
  }

  if (!nrow(lf) && is.data.frame(step$edges) && nrow(step$edges)) {
    lf <- step$edges[, c("Leader", "follower", "WCD"), drop = FALSE]
    lf$link_type <- "visible-edge"
  }
  lf <- lf |>
    dplyr::filter(!is.na(Leader), !is.na(follower), nzchar(Leader), nzchar(follower), Leader != follower) |>
    dplyr::group_by(Leader, follower, link_type) |>
    dplyr::summarise(WCD = sum(WCD, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(WCD), Leader, follower)
  as.data.frame(lf, stringsAsFactors = FALSE)
}



# ---- SankeyMATIC export for FLCA process ------------------------------------
make_flca_sankeymatic_bundle <- function(step, digits = 3) {
  st <- step
  nd <- as.data.frame(st$nodes, stringsAsFactors = FALSE)
  lf <- make_flca_leader_follower_edges(st)

  if (!nrow(lf)) {
    return(list(
      text = "No leader-follower links available for SankeyMATIC.",
      nodes = data.frame(name = character(0), group = character(0), color = character(0), stringsAsFactors = FALSE),
      edges = data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), link_type = character(0), stringsAsFactors = FALSE)
    ))
  }

  lf$Leader <- trimws(as.character(lf$Leader))
  lf$follower <- trimws(as.character(lf$follower))
  lf$WCD <- suppressWarnings(as.numeric(lf$WCD))
  lf <- lf[is.finite(lf$WCD) & lf$WCD > 0 & nzchar(lf$Leader) & nzchar(lf$follower) & lf$Leader != lf$follower, , drop = FALSE]
  if (!nrow(lf)) {
    return(list(
      text = "No positive leader-follower links available for SankeyMATIC.",
      nodes = data.frame(name = character(0), group = character(0), color = character(0), stringsAsFactors = FALSE),
      edges = lf
    ))
  }

  nd$name <- trimws(as.character(nd$name))
  nd$group <- if ("carac" %in% names(nd)) paste0("C", as.character(nd$carac)) else "C1"
  nd_sm <- nd[, c("name", "group"), drop = FALSE]
  nd_sm <- nd_sm[!is.na(nd_sm$name) & nzchar(nd_sm$name), , drop = FALSE]

  all_link_nodes <- unique(c(lf$Leader, lf$follower))
  missing_nodes <- setdiff(all_link_nodes, nd_sm$name)
  if (length(missing_nodes)) {
    missing_group <- ifelse(grepl("^Cluster C", missing_nodes),
                            sub("^Cluster (C[^ ]+).*", "\\1", missing_nodes),
                            "Anchor")
    nd_sm <- dplyr::bind_rows(
      nd_sm,
      data.frame(name = missing_nodes, group = missing_group, stringsAsFactors = FALSE)
    )
  }
  nd_sm <- nd_sm[!duplicated(nd_sm$name), , drop = FALSE]

  specified_colors <- c(
    "#FF0000", "#0000FF", "#998000", "#008000", "#800080", "#FFC0CB", "#000000",
    "#ADD8E6", "#FF4500", "#A52A2A", "#8B4513", "#FF8C00", "#32CD32", "#4682B4",
    "#9400D3", "#FFD700", "#C0C0C0", "#DC143C", "#1E90FF", "#20B2AA", "#7B68EE",
    "#2E8B57", "#B8860B", "#708090", "#DA70D6", "#66CDAA", "#CD5C5C", "#4169E1"
  )
  groups <- unique(as.character(nd_sm$group))
  group_colors <- setNames(rep(specified_colors, length.out = length(groups)), groups)
  nd_sm$color <- unname(group_colors[as.character(nd_sm$group)])

  wtxt <- format(round(lf$WCD, digits), trim = TRUE, scientific = FALSE)
  link_text <- paste0(lf$Leader, " [", wtxt, "] ", lf$follower, " #000000")
  color_text <- paste0(": ", nd_sm$name, " ", nd_sm$color)

  list(
    text = paste(c(link_text, color_text), collapse = "\n"),
    nodes = nd_sm,
    edges = as.data.frame(lf, stringsAsFactors = FALSE)
  )
}

plot_flca_chord_base <- function(step) {
  lf <- make_flca_leader_follower_edges(step)
  validate(need(nrow(lf) > 0, "No leader-follower links available for Chord diagram."))
  lf$Leader <- as.character(lf$Leader)
  lf$follower <- as.character(lf$follower)
  lf$WCD <- suppressWarnings(as.numeric(lf$WCD))
  lf <- lf[is.finite(lf$WCD) & lf$WCD > 0 & nzchar(lf$Leader) & nzchar(lf$follower), , drop = FALSE]
  validate(need(nrow(lf) > 0, "No positive leader-follower links available for Chord diagram."))

  # Real circular chord diagram. Rows are leaders; columns are followers.
  mat <- xtabs(WCD ~ Leader + follower, data = lf)
  labs <- union(rownames(mat), colnames(mat))
  mat2 <- matrix(0, nrow = length(labs), ncol = length(labs), dimnames = list(labs, labs))
  mat2[rownames(mat), colnames(mat)] <- as.matrix(mat)

  cl <- step$nodes$carac[match(labs, step$nodes$name)]
  cl[!is.finite(cl)] <- 0
  pal <- make_cluster_palette(cl)
  grid_col <- setNames(unname(pal[as.character(cl)]), labs)

  op <- par(mar = c(1, 1, 3, 1), xpd = NA)
  on.exit({ try(circlize::circos.clear(), silent = TRUE); par(op) }, add = TRUE)
  circlize::circos.clear()
  circlize::chordDiagram(
    mat2,
    grid.col = grid_col,
    directional = 1,
    direction.type = c("arrows", "diffHeight"),
    annotationTrack = c("grid"),
    preAllocateTracks = 1,
    transparency = 0.28,
    link.sort = TRUE,
    link.largest.ontop = TRUE
  )
  circlize::circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
      sector <- circlize::get.cell.meta.data("sector.index")
      xlim <- circlize::get.cell.meta.data("xlim")
      ylim <- circlize::get.cell.meta.data("ylim")
      circlize::circos.text(mean(xlim), ylim[1] + .1, sector,
                            facing = "clockwise", niceFacing = TRUE,
                            adj = c(0, 0.5), cex = 0.68, font = 2)
    },
    bg.border = NA
  )
  title("FLCA leader-follower Chord diagram", font.main = 2)
}


# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("\n    body { background:#fafafa; }\n    .box { background:white; border:1px solid #ddd; border-radius:12px; padding:14px; margin-bottom:14px; }\n    .note { color:#666; font-size:13px; }\n    textarea { font-family: Consolas, 'Courier New', monospace; }
    .nav-tabs > li > a[data-value='FLCA Process'], .nav-tabs > li > a[data-value='Visual Quality'], .nav-tabs > li > a[data-value='ReadMe'] { color:#d62728 !important; font-weight:800; }
    .nav-tabs > li.active > a[data-value='FLCA Process'], .nav-tabs > li.active > a[data-value='Visual Quality'], .nav-tabs > li.active > a[data-value='ReadMe'] { border-top:4px solid #d62728 !important; }\n  "))),
  titlePanel("FLCA and Community Clustering Comparison"),
  sidebarLayout(
    sidebarPanel(
      div(class = "box",
          radioButtons("input_mode", "Input mode", c("Use demo text" = "demo", "Upload CSV files" = "upload"), selected = "demo"),
          conditionalPanel("input.input_mode == 'upload'",
                           radioButtons("upload_data_type", "Uploaded CSV data type",
                                        c("Auto detect" = "auto",
                                          "Nodes + edges" = "nodes_edges",
                                          "Co-word occurrence" = "coword"),
                                        selected = "auto"),
                           conditionalPanel("input.upload_data_type != 'coword'",
                                            fileInput("nodes_file", "Optional nodes CSV: name,value,value2,carac", accept = ".csv"),
                                            fileInput("edges_file", "Edges CSV: Leader,follower,WCD or equivalent", accept = ".csv")),
                           conditionalPanel("input.upload_data_type != 'nodes_edges'",
                                            fileInput("occurrence_file", "Co-word occurrence CSV: long format document,term OR wide document-term matrix", accept = ".csv"))),
          conditionalPanel("input.input_mode == 'demo'",
                           tags$label("Optional Nodes CSV — leave blank to auto-generate from edges"),
                           textAreaInput("nodes_txt", NULL, value = default_nodes_txt, rows = 10, width = "100%"),
                           tags$label("Edges CSV — headers optional; first 2 columns=endpoints, 3rd=weight"),
                           textAreaInput("edges_txt", NULL, value = default_edges_txt, rows = 10, width = "100%")),
          numericInput("target_k", "Target k for independent components", value = 4, min = 2, max = 20, step = 1),
          numericInput("label_font_size", "Network label font size", value = 30, min = 12, max = 60, step = 2),
          checkboxInput("label_bold", "Bold network labels", value = TRUE),
          actionButton("run", "Run analysis", class = "btn-primary")
      ),
      div(class = "box note",
          "Deploy-safe revision: edges-only input is allowed; co-word occurrence CSV can be transformed directly into generated nodes and edges; flexible column names are transformed to Leader/follower/WCD; no F:/ path, no browser-opening call, no shell execution.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", br(), div(class = "box", h4("Generated / cleaned data"), verbatimTextOutput("data_mode")), div(class = "box", h4("Nodes used in analysis"), downloadButton("download_nodes", "Download generated/cleaned nodes CSV"), DTOutput("tbl_nodes")), div(class = "box", h4("Edges used in analysis"), downloadButton("download_edges_clean", "Download generated/cleaned edges CSV"), DTOutput("tbl_edges"))),
        tabPanel("Full Network", br(),
                 div(class = "box",
                     fluidRow(
                       column(5, selectInput("full_method", "Clustering algorithm", choices = community_methods[community_methods != "components_independent_k"], selected = "louvain")),
                       column(4, sliderInput("full_top_n", "Show top N nodes", min = 5, max = 100, value = 30, step = 1)),
                       column(3, checkboxInput("full_use_all", "Show all nodes", value = FALSE))
                     ),
                     visNetworkOutput("plot_full", height = "720px"))),
        tabPanel("FLCA Reduced", br(),
                 div(class = "box",
                     fluidRow(
                       column(4, checkboxInput("flca_major_sampling", "Use FLCA-MA major sampling", value = TRUE)),
                       column(4, sliderInput("flca_top_clusters", "Top clusters", min = 1, max = 20, value = 6, step = 1)),
                       column(4, sliderInput("flca_n_per_cluster", "At least N nodes per top cluster", min = 1, max = 20, value = 3, step = 1))
                     ),
                     visNetworkOutput("plot_flca", height = "720px")),
                 div(class = "box", h4("Reduced edges"), DTOutput("tbl_reduced"))),
        tabPanel("FLCA Process", br(),
                 div(class = "box",
                     h3("FLCA 6-step visual process"),
                     p(class = "note", "This red tab shows the FLCA pipeline from source graph to leader-follower interpretation. Major sampling follows the FLCA controls in the sidebar / FLCA Reduced tab.")),
                 div(class = "box", h4("Step 1. Input nodes and weighted co-word/collaboration edges"),
                     fluidRow(column(6, DTOutput("flca_step1_nodes")), column(6, DTOutput("flca_step1_edges")))),
                 div(class = "box", h4("Step 2. Full weighted graph before FLCA"),
                     visNetworkOutput("flca_step2_full_graph", height = "560px")),
                 div(class = "box", h4("Step 3. FLCA strongest-link reduced graph"),
                     visNetworkOutput("flca_step3_reduced_graph", height = "560px")),
                 div(class = "box", h4("Step 4. Cluster leaders and selected followers"),
                     DTOutput("flca_step4_cluster_summary"), br(), DTOutput("flca_step4_selected_nodes")),
                 div(class = "box", h4("Step 5. Leader–follower Sankey"),
                     uiOutput("flca_step5_sankey")),
                 div(class = "box", h4("Step 5b. SankeyMATIC code for sankeymatic.com"),
                     p(class = "note", "Copy this code into sankeymatic.com. Links use FLCA selected leader-follower edges; color definitions use the selected FLCA nodes and cluster colors."),
                     fluidRow(
                       column(4, downloadButton("download_flca_sankeymatic_txt", "Download SankeyMATIC TXT")),
                       column(4, downloadButton("download_flca_sankeymatic_nodes", "Download SankeyMATIC nodes CSV")),
                       column(4, downloadButton("download_flca_sankeymatic_edges", "Download SankeyMATIC edges CSV"))
                     ),
                     br(), uiOutput("flca_step5_sankeymatic_code")),
                 div(class = "box", h4("Step 6. Real SSplot: cohesion, AAC, and cluster summary"),
                     plotOutput("flca_step6_ssplot", height = "920px", width = "100%")),
                 div(class = "box", h4("Step 7. Kano plot for selected FLCA nodes"),
                     p(class = "note", "Kano plot uses selected FLCA nodes: x = edge/connectivity value2, y = node value, grouped by FLCA cluster. The table reports AAC for node value and value2, respectively."),
                     plotOutput("flca_step7_kano", height = "700px"),
                     br(), h4("Step 7 AAC summary"), DTOutput("flca_step7_aac")),
                 div(class = "box", h4("Step 8. Interactive FLCA leader-follower chord dashboard"),
                     p(class = "note", "Interactive dashboard for selected FLCA nodes. Low-weight cluster-anchor links are added only to keep every selected Top20 node visible when a node has no leader-follower edge."),
                     sliderInput("flca_chord_top_links", "Show top leader-follower links", min = 5, max = 200, value = 60, step = 5),
                     visNetworkOutput("flca_step8_chord_interactive", height = "760px"),
                     br(), DTOutput("flca_step8_chord_edges")),
                 div(class = "box", h4("Step 9. Real circular chord diagram for selected FLCA nodes"),
                     p(class = "note", "Publication-style circlize chord diagram rebuilt from selected FLCA leader-follower links only; selected-node anchors keep Top20 nodes visible."),
                     plotOutput("flca_step9_chord", height = "820px", width = "100%"))),
        tabPanel("Memberships", br(), div(class = "box", DTOutput("tbl_memberships")),
                 div(class = "box", downloadButton("download_memberships", "Download memberships CSV"))),
        tabPanel("Quality", br(), div(class = "box", DTOutput("tbl_quality")),
                 div(class = "box", downloadButton("download_quality", "Download quality CSV"))),
        tabPanel("Ranking", br(), div(class = "box", DTOutput("tbl_ranking")),
                 div(class = "box", downloadButton("download_ranking", "Download ranking CSV"))),
        tabPanel("Visual Quality", br(),
                 div(class = "box",
                     h4("Kano plot for algorithm quality"),
                     p(class = "note", "Each bubble is one clustering algorithm. The x-axis is modularity Q and the y-axis is mean silhouette score. The upper-right region indicates better combined separation and cohesion."),
                     plotOutput("plot_visual_quality_kano", height = "700px")),
                 div(class = "box", h4("Kano input table"), DTOutput("tbl_visual_quality_kano")),
                 div(class = "box", downloadButton("download_visual_quality_kano", "Download visual-quality Kano CSV"))),
        tabPanel("ReadMe", br(),
                 div(class = "box",
                     h3("ReadMe: FLCA and community clustering comparison app"),
                     p("This Shiny app compares FLCA with common igraph community-detection algorithms using either demo data, uploaded nodes/edges CSV files, or uploaded co-word occurrence data."),
                     tags$ul(
                       tags$li(tags$b("Input modes: "), "demo nodes/edges, uploaded nodes+edges, or co-word occurrence CSV in long document-term format or wide document-term matrix format."),
                       tags$li(tags$b("Generated datasets: "), "co-word occurrence data are converted into a nodes dataframe and an edges dataframe, both downloadable for downstream FLCA, network, SSplot, or bibliometric analysis."),
                       tags$li(tags$b("Algorithms: "), "benchmark algorithms are edge-driven; FLCA uses strongest leader-follower links and may additionally use node priority for major sampling."),
                       tags$li(tags$b("Visual dashboards: "), "full network, FLCA reduced network, membership table, quality metrics, ranking table, and visual-quality Kano plot."),
                       tags$li(tags$b("FLCA Process tab: "), "workflow visuals: input data, full graph, FLCA reduced graph, leader/follower summary, Sankey, real SSplot, Kano plot with AAC(value)/AAC(value2), interactive chord dashboard, and real circular chord diagram."),
                       tags$li(tags$b("Visual Quality tab: "), "modularity Q is placed on the x-axis and mean silhouette score on the y-axis, making the quality trade-off readable at a glance.")
                     ),
                     p(class = "note", "For deployment, place app.R and kano(61).R or kano.R in the same app folder to use the real Kano renderer; otherwise the app falls back to a ggplot version."))),
        tabPanel("Deploy Notes", br(), div(class = "box",
          h4("shinyapps.io deployment"),
          tags$pre("install.packages(c('shiny','DT','igraph','dplyr','tibble','readr','visNetwork','RColorBrewer'), type='binary')\nrsconnect::setAccountInfo(name='YOUR_ACCOUNT', token='YOUR_TOKEN', secret='YOUR_SECRET')\nrsconnect::deployApp(appDir='F:/taaforgae/zwospubmed/woscited', appName='woscited', account='YOUR_ACCOUNT', server='shinyapps.io', forceUpdate=TRUE)"),
          p("Use rsconnect::accounts() to confirm the exact account name before deployment.")
        ))
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  input_data <- reactive({
    data_mode <- "demo nodes + edges"
    if (identical(input$input_mode, "upload")) {
      dtype <- input$upload_data_type %||% "auto"
      use_coword <- identical(dtype, "coword") || (identical(dtype, "auto") && !is.null(input$occurrence_file) && is.null(input$edges_file))
      if (isTRUE(use_coword)) {
        validate(need(!is.null(input$occurrence_file), "Upload a co-word occurrence CSV first."))
        occ_raw <- read_occurrence_file(input$occurrence_file$datapath)
        built <- build_coword_from_occurrence(occ_raw)
        nodes <- built$nodes
        edges <- built$edges
        data_mode <- paste0("co-word occurrence converted to nodes/edges (", built$mode, ")")
      } else {
        validate(need(!is.null(input$edges_file), "Upload edges CSV first, or choose Co-word occurrence and upload an occurrence CSV."))
        edges_raw <- apply_header_if_present(read_table_flexible_file(input$edges_file$datapath), type = "edges")
        edges <- clean_edges(edges_raw)
        if (!is.null(input$nodes_file)) {
          nodes_raw <- apply_header_if_present(read_table_flexible_file(input$nodes_file$datapath), type = "nodes")
          nodes <- clean_nodes(nodes_raw, edges = edges)
          data_mode <- "uploaded nodes + edges"
        } else {
          nodes <- clean_nodes(NULL, edges = edges)
          data_mode <- "uploaded edges only; nodes generated from edge strength"
        }
      }
    } else {
      edges_raw <- apply_header_if_present(read_table_flexible_text(input$edges_txt), type = "edges")
      edges <- clean_edges(edges_raw)
      nodes_txt <- trimws(input$nodes_txt)
      if (nzchar(nodes_txt)) {
        nodes_raw <- apply_header_if_present(read_table_flexible_text(nodes_txt), type = "nodes")
        nodes <- clean_nodes(nodes_raw, edges = edges)
      } else {
        nodes <- clean_nodes(NULL, edges = edges)
      }
    }
    validate(need(nrow(edges) > 0, "No valid edges were found after column transformation."))
    validate(need(nrow(nodes) > 0, "No valid nodes were available or generated."))
    list(nodes = nodes, edges = edges, data_mode = data_mode)
  })

  res <- eventReactive(input$run, {
    dat <- input_data()
    withProgress(message = "Running clustering analysis...", value = 0, {
      incProgress(0.2, detail = "Building full and reduced graphs")
      out <- analyze_all(dat$nodes, dat$edges, target_k = input$target_k)
      incProgress(1, detail = "Done")
      out
    })
  }, ignoreInit = FALSE)

  output$data_mode <- renderText({ req(input_data()); paste0("Input mode: ", input_data()$data_mode, "
Nodes: ", nrow(input_data()$nodes), "
Edges: ", nrow(input_data()$edges)) })
  output$tbl_nodes <- renderDT({ req(res()); datatable(res()$nodes, options = list(scrollX = TRUE, pageLength = 20)) })
  output$tbl_edges <- renderDT({ req(res()); datatable(res()$edges, options = list(scrollX = TRUE, pageLength = 20)) })
  output$tbl_reduced <- renderDT({ req(res()); datatable(res()$edges_reduced, options = list(scrollX = TRUE, pageLength = 20)) })

  output$download_nodes <- downloadHandler(
    filename = function() paste0("generated_nodes_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(res()$nodes, file),
    contentType = "text/csv"
  )
  output$download_edges_clean <- downloadHandler(
    filename = function() paste0("cleaned_edges_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(res()$edges, file),
    contentType = "text/csv"
  )

  observeEvent(res(), {
    nmax <- max(5L, igraph::vcount(res()$g_full))
    updateSliderInput(session, "full_top_n", max = nmax, value = min(30L, nmax))
    kmax <- max(1L, length(unique(res()$memberships_df$FLCA[!is.na(res()$memberships_df$FLCA)])))
    updateSliderInput(session, "flca_top_clusters", max = kmax, value = min(6L, kmax))
  }, ignoreInit = FALSE)

  output$plot_full <- renderVisNetwork({
    req(res())
    method <- input$full_method
    memb_named <- get_membership_for_method(res(), method)
    top_n <- if (isTRUE(input$full_use_all)) igraph::vcount(res()$g_full) else input$full_top_n
    ss <- subset_graph_by_top_nodes(res()$g_full, memb_named, top_n = top_n)
    make_visnetwork(
      ss$graph, ss$membership,
      paste0("Interactive full graph | ", method, " | top N=", igraph::vcount(ss$graph)),
      label_font_size = input$label_font_size, label_bold = input$label_bold
    )
  })

  output$plot_flca <- renderVisNetwork({
    req(res())
    memb_named <- as.integer(res()$flca_carac)
    names(memb_named) <- names(res()$flca_carac)
    ss <- subset_flca_major_sampling(
      res()$g_flca, memb_named,
      top_clusters = input$flca_top_clusters,
      n_per_cluster = input$flca_n_per_cluster,
      major_sampling = input$flca_major_sampling
    )
    make_visnetwork(
      ss$graph, ss$membership,
      paste0("Interactive FLCA dashboard | major sampling=", input$flca_major_sampling,
             " | nodes=", igraph::vcount(ss$graph)),
      label_font_size = input$label_font_size, label_bold = input$label_bold
    )
  })


  flca_step <- reactive({
    req(res())
    make_flca_step_nodes(
      res(),
      top_clusters = input$flca_top_clusters %||% 6,
      n_per_cluster = input$flca_n_per_cluster %||% 3,
      major_sampling = isTRUE(input$flca_major_sampling)
    )
  })

  output$flca_step1_nodes <- renderDT({
    req(res())
    datatable(res()$nodes, options = list(scrollX = TRUE, pageLength = 10))
  })
  output$flca_step1_edges <- renderDT({
    req(res())
    datatable(res()$edges, options = list(scrollX = TRUE, pageLength = 10))
  })
  output$flca_step2_full_graph <- renderVisNetwork({
    req(res())
    make_visnetwork(res()$g_full, res()$memberships_df$FLCA,
                    "Step 2. Full graph colored by FLCA membership",
                    label_font_size = input$label_font_size, label_bold = input$label_bold)
  })
  output$flca_step3_reduced_graph <- renderVisNetwork({
    req(flca_step())
    st <- flca_step()
    g <- igraph::graph_from_data_frame(st$edges, directed = FALSE, vertices = st$nodes)
    if (igraph::ecount(g) > 0) igraph::E(g)$weight <- igraph::E(g)$WCD
    make_visnetwork(g, st$nodes$carac,
                    "Step 3. FLCA strongest-link graph after major sampling",
                    label_font_size = input$label_font_size, label_bold = input$label_bold)
  })
  output$flca_step4_cluster_summary <- renderDT({
    req(flca_step())
    datatable(make_flca_cluster_summary(flca_step()), options = list(scrollX = TRUE, pageLength = 10))
  })
  output$flca_step4_selected_nodes <- renderDT({
    req(flca_step())
    datatable(flca_step()$nodes |> dplyr::arrange(carac, dplyr::desc(value)), options = list(scrollX = TRUE, pageLength = 20))
  })
  output$flca_step5_sankey <- renderUI({
    req(flca_step())
    st <- flca_step()
    if (exists("render_author_sankey", mode = "function")) {
      render_author_sankey(make_flca_leader_follower_edges(st))
    } else {
      tags$div(class = "note", "Place sankey(58).R or sankey.R in the app folder to enable the Sankey renderer.")
    }
  })

  flca_sankeymatic_bundle <- reactive({
    req(flca_step())
    make_flca_sankeymatic_bundle(flca_step())
  })

  output$flca_step5_sankeymatic_code <- renderUI({
    sm <- flca_sankeymatic_bundle()
    tags$textarea(
      readonly = NA,
      style = "width:100%; height:360px; font-family:Consolas, 'Courier New', monospace; font-size:13px; white-space:pre;",
      sm$text
    )
  })

  output$download_flca_sankeymatic_txt <- downloadHandler(
    filename = function() paste0("flca_sankeymatic_code_", Sys.Date(), ".txt"),
    content = function(file) writeLines(flca_sankeymatic_bundle()$text, file, useBytes = TRUE),
    contentType = "text/plain"
  )
  output$download_flca_sankeymatic_nodes <- downloadHandler(
    filename = function() paste0("flca_sankeymatic_nodes_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(flca_sankeymatic_bundle()$nodes, file),
    contentType = "text/csv"
  )
  output$download_flca_sankeymatic_edges <- downloadHandler(
    filename = function() paste0("flca_sankeymatic_edges_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(flca_sankeymatic_bundle()$edges, file),
    contentType = "text/csv"
  )
  output$flca_step6_ssplot <- renderPlot({
    req(flca_step())
    st <- flca_step()
    validate(need(nrow(st$nodes) > 0, "No selected FLCA nodes for SSplot."))
    if (exists("render_panel", mode = "function")) {
      render_panel(
        sil_df = st$nodes,
        nodes0 = st$all_nodes,
        results = make_flca_results_for_ssplot(st),
        nodes = st$nodes,
        top_n = nrow(st$nodes),
        font_scale = 0.70,
        aac_side = "left",
        footer_adj = 0
      )
    } else {
      ggplot2::ggplot(st$nodes, ggplot2::aes(x = sil_width, y = reorder(name, sil_width), fill = factor(carac))) +
        ggplot2::geom_col() + ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(x = "Silhouette width", y = NULL, fill = "Cluster")
    }
  }, width = 1150, height = function() { max(760, 90 + 34 * nrow(flca_step()$nodes)) }, res = 120)


  output$flca_step7_kano <- renderPlot({
    req(flca_step())
    st <- flca_step()
    validate(need(nrow(st$nodes) >= 2, "At least two selected FLCA nodes are required for the Kano plot."))
    nd <- make_flca_kano_nodes(st)
    aac <- make_flca_kano_aac_summary(st)
    title_txt <- sprintf(
      "Step 7. FLCA Kano plot | AAC(value)=%.2f | AAC(value2)=%.2f",
      suppressWarnings(as.numeric(aac$AAC_value[as.character(aac$carac) == "OVERALL"])),
      suppressWarnings(as.numeric(aac$AAC_value2[as.character(aac$carac) == "OVERALL"]))
    )
    if (exists("plot_kano_real", mode = "function")) {
      print(plot_kano_real(nd, title_txt = title_txt, visual_ratio = 1/2.2, label_size = 3.6) + ggplot2::labs(x = "Edge/connectivity value2 (normalized display; original tick labels)", y = "Node value"))
    } else {
      print(ggplot2::ggplot(nd, ggplot2::aes(x = value2, y = value, label = name, color = factor(carac))) +
              ggplot2::geom_point(size = 4) +
              ggrepel::geom_text_repel(max.overlaps = 200) +
              ggplot2::theme_minimal(base_size = 13) +
              ggplot2::labs(title = title_txt, x = "Edge/connectivity value2", y = "Node value", color = "FLCA cluster"))
    }
  }, width = 1350, height = 760, res = 120)

  output$flca_step7_aac <- renderDT({
    req(flca_step())
    datatable(make_flca_kano_aac_summary(flca_step()), options = list(scrollX = TRUE, pageLength = 20))
  })

  output$flca_step8_chord_interactive <- renderVisNetwork({
    req(flca_step())
    make_interactive_chord_dashboard(flca_step(), top_links = input$flca_chord_top_links %||% 60)
  })

  output$flca_step8_chord_edges <- renderDT({
    req(flca_step())
    lf <- make_flca_leader_follower_edges(flca_step())
    lf <- lf[order(-suppressWarnings(as.numeric(lf$WCD)), lf$Leader, lf$follower), , drop = FALSE]
    datatable(lf, options = list(scrollX = TRUE, pageLength = 20))
  })

  output$flca_step9_chord <- renderPlot({
    req(flca_step())
    plot_flca_chord_base(flca_step())
  }, width = 1050, height = 820, res = 120)

  output$tbl_memberships <- renderDT({ req(res()); datatable(res()$memberships_df, options = list(scrollX = TRUE, pageLength = 20)) })
  output$tbl_quality <- renderDT({ req(res()); datatable(res()$quality_df, options = list(scrollX = TRUE, pageLength = 20)) })
  output$tbl_ranking <- renderDT({ req(res()); datatable(res()$ranking_df, options = list(scrollX = TRUE, pageLength = 20)) })

  visual_quality_kano_data <- reactive({
    req(res())
    make_algorithm_kano_nodes(res()$quality_df)
  })

  output$plot_visual_quality_kano <- renderPlot({
    req(res())
    print(plot_algorithm_quality_kano(res()$quality_df))
  })

  output$tbl_visual_quality_kano <- renderDT({
    datatable(visual_quality_kano_data(), options = list(scrollX = TRUE, pageLength = 20))
  })

  output$download_visual_quality_kano <- downloadHandler(
    filename = function() paste0("visual_quality_kano_algorithms_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(visual_quality_kano_data(), file),
    contentType = "text/csv"
  )

  output$download_memberships <- downloadHandler(
    filename = function() paste0("clustering_memberships_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(res()$memberships_df, file),
    contentType = "text/csv"
  )
  output$download_quality <- downloadHandler(
    filename = function() paste0("clustering_quality_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(res()$quality_df, file),
    contentType = "text/csv"
  )
  output$download_ranking <- downloadHandler(
    filename = function() paste0("clustering_ranking_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(res()$ranking_df, file),
    contentType = "text/csv"
  )
}

shinyApp(ui, server)
