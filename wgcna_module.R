# WGCNA runs on repeated observations: rows are observations, columns are nodes.
# WGCNA module detection is independent of the FLCA leader-value choice.
read_wgcna_matrix_csv <- function(path) {
  if (is.null(path) || !file.exists(path)) stop("Upload a value-matrix CSV.", call. = FALSE)
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  csv_text <- rawToChar(bytes)
  utf8_text <- iconv(csv_text, from = "UTF-8", to = "UTF-8",
                    sub = NA_character_)
  if (is.na(utf8_text))
    utf8_text <- iconv(csv_text, from = "CP950", to = "UTF-8",
                      sub = NA_character_)
  if (is.na(utf8_text))
    stop("The value-matrix CSV must be UTF-8 or CP950/Big5 encoded.", call. = FALSE)
  utf8_text <- sub("^\ufeff", "", utf8_text)
  raw <- utils::read.csv(text = utf8_text, check.names = FALSE,
                         stringsAsFactors = FALSE)
  if (ncol(raw) < 5L) stop("The matrix needs a name column and at least four numeric value columns.", call. = FALSE)
  labels <- trimws(as.character(raw[[1L]]))
  keep <- !is.na(labels) & nzchar(labels)
  raw <- raw[keep, , drop = FALSE]
  labels <- labels[keep]
  if (!length(labels)) stop("The first CSV column has no node names.", call. = FALSE)
  if (anyDuplicated(labels))
    stop("The first CSV column contains duplicate node names.", call. = FALSE)
  value_columns <- setdiff(names(raw)[-1L], names(raw)[tolower(names(raw)) == "profile"])
  x <- as.matrix(data.frame(lapply(raw[value_columns], function(z)
    suppressWarnings(as.numeric(z))), check.names = FALSE))
  if (any(!is.finite(x))) stop("All selected value columns must be finite numeric observations.", call. = FALSE)
  rownames(x) <- labels
  colnames(x) <- value_columns
  t(x)
}

wgcna_gene_matrix <- function(data, group, value_columns, node_type = "genes") {
  group <- suppressWarnings(as.numeric(group))[1L]
  if (!is.finite(group) || !group %in% data$groups)
    stop("Select an available log10P group.", call. = FALSE)
  df <- data$df[as.numeric(data$df$log10P) == group, , drop = FALSE]
  if (!length(value_columns) || any(!value_columns %in% data$value_cols))
    stop("Choose validated GSM value columns.", call. = FALSE)
  x <- as.matrix(data.frame(lapply(df[value_columns], function(z)
    suppressWarnings(as.numeric(z))), check.names = FALSE))
  if (any(!is.finite(x))) stop("The selected gene value columns must be finite numeric observations.", call. = FALSE)
  genes <- trimws(as.character(df$Gene))
  if (anyNA(genes) || any(!nzchar(genes)) || anyDuplicated(genes))
    stop("Gene names in the selected group must be unique and nonempty.", call. = FALSE)
  rownames(x) <- genes
  if (identical(node_type, "samples")) x else t(x)
}

flca_incident_total <- function(node_names, edges) {
  out <- stats::setNames(numeric(length(node_names)), node_names)
  if (!is.data.frame(edges) || !nrow(edges)) return(out)
  ed <- as.data.frame(edges, stringsAsFactors = FALSE)
  if ("follower" %in% names(ed) && !"Follower" %in% names(ed))
    names(ed)[names(ed) == "follower"] <- "Follower"
  if (!all(c("Leader", "Follower", "WCD") %in% names(ed)))
    stop("Derived total requires Leader, Follower, and WCD edges.", call. = FALSE)
  ed <- ed[ed$Leader %in% node_names & ed$Follower %in% node_names &
             ed$Leader != ed$Follower & is.finite(ed$WCD) & ed$WCD > 0,
           , drop = FALSE]
  if (!nrow(ed)) return(out)
  ed$a <- pmin(as.character(ed$Leader), as.character(ed$Follower))
  ed$b <- pmax(as.character(ed$Leader), as.character(ed$Follower))
  ed <- stats::aggregate(WCD ~ a + b, ed, max)
  for (i in seq_len(nrow(ed))) {
    out[ed$a[i]] <- out[ed$a[i]] + ed$WCD[i]
    out[ed$b[i]] <- out[ed$b[i]] + ed$WCD[i]
  }
  out
}

apply_flca_value_source <- function(data, mode = c("total", "derived", "edge")) {
  mode <- match.arg(mode)
  if (is.null(data$value_matrix)) return(data)
  x <- as.matrix(data$value_matrix)
  if (is.null(colnames(x)) || any(!is.finite(x)))
    stop("Repeated value columns require named nodes and finite observations.",
         call. = FALSE)
  names <- as.character(data$nodes$name)
  if (!all(names %in% colnames(x)))
    stop("The repeated value columns do not match node names.", call. = FALSE)
  total <- colSums(x)[names]
  derived <- flca_incident_total(names, data$edges)
  data$nodes$value <- if (!identical(mode, "derived"))
    unname(total) else unname(derived[names])
  data$node_value_source <- switch(mode,
    total = "total observation score",
    derived = "sum of incident positive Lin's CCC",
    edge = "total observation score (FLCA companion to edge-based Louvain)")
  data
}

# Traditional weighted community clustering is a separate comparison result.
# It never changes FLCA's parent links, root chains, or sampled membership.
cluster_by_edge <- function(nodes, edges) {
  if ("follower" %in% names(edges) && !"Follower" %in% names(edges))
    names(edges)[names(edges) == "follower"] <- "Follower"
  v <- flca_validate(nodes, edges)
  nd <- v$nodes
  ed <- v$data[v$data$WCD > 0, , drop = FALSE]
  g <- igraph::graph_from_data_frame(ed, directed = FALSE, vertices = nd)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(1L)
  membership <- if (nrow(ed))
    igraph::membership(igraph::cluster_louvain(g, weights = igraph::E(g)$WCD))
  else seq_len(nrow(nd))
  nd$carac <- as.integer(membership[match(nd$name, igraph::V(g)$name)])
  nd <- nd[order(-nd$value, nd$name, method = "radix"), , drop = FALSE]
  nd$carac <- match(nd$carac, unique(nd$carac))
  rownames(nd) <- NULL
  ids <- unique(nd$carac)
  summary <- do.call(rbind, lapply(ids, function(id) {
    x <- nd[nd$carac == id, , drop = FALSE]
    data.frame(cluster_id = id, leader_name = x$name[1L],
               member_count = nrow(x), highest_value = x$value[1L],
               member_names = paste(x$name, collapse = ", "))
  }))
  q <- flca_quality(nd, v$data)
  list(nodes = nd, edges = v$data, clusters = summary,
       metrics = data.frame(mean_silhouette = q$silhouette$mean_silhouette,
                            modularity_Q = q$modularity$modularity_Q,
                            normalized_Q = q$modularity$normalized_Q),
       diagnostics = q$diagnostics, method = "weighted Louvain")
}

wgcna_tom_mds <- function(tom, nodes, top_n = 80L) {
  keep <- head(as.character(nodes$name), top_n)
  d <- 1 - tom[keep, keep, drop = FALSE]
  diag(d) <- 0
  fit <- stats::cmdscale(stats::as.dist(d), k = 2L, eig = TRUE, add = TRUE)
  xy <- as.matrix(fit$points)
  if (ncol(xy) < 2L) xy <- cbind(xy, 0)
  data.frame(name = keep, MDS1 = xy[, 1L], MDS2 = xy[, 2L],
             module = as.character(nodes$carac[match(keep, nodes$name)]),
             module_color = nodes$module_color[match(keep, nodes$name)],
             value = nodes$value[match(keep, nodes$name)],
             stringsAsFactors = FALSE)
}

wgcna_chord_data <- function(adjacency, nodes, top_n = 30L,
                             links_per_node = 2L) {
  keep <- head(as.character(nodes$name), top_n)
  a <- adjacency[keep, keep, drop = FALSE]
  a[!is.finite(a) | a < 0] <- 0
  diag(a) <- 0
  chosen <- matrix(FALSE, nrow(a), ncol(a))
  for (i in seq_along(keep)) {
    candidates <- which(a[i, ] > 0)
    if (!length(candidates)) next
    candidates <- candidates[order(-a[i, candidates], keep[candidates],
                                   method = "radix")]
    for (j in head(candidates, links_per_node))
      chosen[min(i, j), max(i, j)] <- TRUE
  }
  m <- matrix(0, nrow(a), ncol(a),
              dimnames = list(have = keep, prefer = keep))
  upper <- upper.tri(a) & chosen
  m[upper] <- a[upper]
  list(matrix = m,
       group_colors = nodes$module_color[match(keep, nodes$name)],
       node_names = keep, edge_count = sum(upper))
}

make_wgcna_chord_widget <- function(chord) {
  if (!requireNamespace("chorddiag", quietly = TRUE) ||
      !chord$edge_count) return(NULL)
  chorddiag::chorddiag(chord$matrix, type = "directional",
                       groupColors = chord$group_colors,
                       groupnamePadding = 20,
                       groupnameFontsize = 12,
                       groupThickness = 0.12,
                       showTicks = FALSE,
                       showTooltips = TRUE,
                       showZeroTooltips = FALSE,
                       fadeLevel = 0.12,
                       margin = 110)
}

run_wgcna_values <- function(datExpr, power = 6L, source = "value matrix",
                             node_value_mode = c("total", "derived", "edge")) {
  node_value_mode <- match.arg(node_value_mode)
  if (!requireNamespace("WGCNA", quietly = TRUE))
    stop("WGCNA is not installed. Install WGCNA in this app's R library, then click Run WGCNA again.", call. = FALSE)
  # WGCNA 1.73 resolves its internal bicor function by name while detecting
  # modules, so its namespace must also be attached for this call.
  attached_before <- "package:WGCNA" %in% search()
  if (!attached_before) suppressPackageStartupMessages(library(WGCNA))
  on.exit(if (!attached_before && "package:WGCNA" %in% search())
    detach("package:WGCNA", unload = FALSE), add = TRUE)
  x <- as.matrix(datExpr)
  storage.mode(x) <- "double"
  if (is.null(colnames(x)) || anyNA(colnames(x)) || any(!nzchar(colnames(x))) ||
      anyDuplicated(colnames(x)))
    stop("WGCNA variables need unique, nonempty node names.", call. = FALSE)
  if (nrow(x) < 4L || ncol(x) < 4L || any(!is.finite(x)))
    stop("WGCNA needs at least four observations, four nodes, and finite numeric values.", call. = FALSE)
  power <- suppressWarnings(as.numeric(power))[1L]
  if (!is.finite(power) || power < 1 || power > 30)
    stop("Soft-threshold power must be between 1 and 30.", call. = FALSE)
  variable <- apply(x, 2L, stats::sd) > 0
  excluded <- colnames(x)[!variable]
  x <- x[, variable, drop = FALSE]
  if (ncol(x) < 4L) stop("Fewer than four nonconstant nodes remain.", call. = FALSE)
  fit <- WGCNA::blockwiseModules(x, power = power, networkType = "unsigned",
                                corType = "bicor",
                                TOMType = "unsigned", minModuleSize = 2L,
                                deepSplit = 2L, mergeCutHeight = 0.25,
                                pamRespectsDendro = FALSE, numericLabels = TRUE,
                                maxBlockSize = max(5000L, ncol(x)),
                                saveTOMs = FALSE, verbose = 0)
  membership <- as.integer(fit$colors)
  if (length(membership) != ncol(x)) stop("WGCNA returned an incomplete module assignment.", call. = FALSE)
  # Match blockwiseModules' unsigned bicor adjacency without relying on its
  # string-based corFnc lookup (which can fail when WGCNA is not attached).
  similarity <- abs(WGCNA::bicor(x, use = "p"))
  dimnames(similarity) <- list(colnames(x), colnames(x))
  diag(similarity) <- 1
  diagnostic_powers <- sort(unique(c(1:10, seq(12, 20, by = 2),
                                     power)))
  soft_threshold <- tryCatch({
    diagnostic <- WGCNA::pickSoftThreshold.fromSimilarity(
      similarity, powerVector = diagnostic_powers, verbose = 0)
    as.data.frame(diagnostic$fitIndices)
  }, error = function(e) {
    warning("Soft-threshold diagnostic unavailable: ", conditionMessage(e),
            call. = FALSE)
    data.frame()
  })
  if (nrow(soft_threshold))
    soft_threshold$signed_R2 <-
      -sign(soft_threshold$slope) * soft_threshold$SFT.R.sq
  adjacency <- similarity^power
  dimnames(adjacency) <- list(colnames(x), colnames(x))
  diag(adjacency) <- 0
  tom <- WGCNA::TOMsimilarity(adjacency, TOMType = "unsigned", verbose = 0)
  dimnames(tom) <- dimnames(adjacency)
  diag(tom) <- 1
  indices <- which(upper.tri(adjacency) & adjacency > 0, arr.ind = TRUE)
  edges <- data.frame(Leader = colnames(x)[indices[, 1L]],
                      Follower = colnames(x)[indices[, 2L]],
                      WCD = adjacency[indices], stringsAsFactors = FALSE)
  total_score <- colSums(x)
  derived_total <- rowSums(adjacency)
  chosen_value <- if (!identical(node_value_mode, "derived"))
    total_score else derived_total
  nodes <- data.frame(name = colnames(x), value = unname(chosen_value),
                      total_score = unname(total_score),
                      derived_total = unname(derived_total),
                      carac = membership, stringsAsFactors = FALSE)
  q <- flca_quality(nodes, edges)
  edge_clustering <- if (identical(node_value_mode, "edge"))
    cluster_by_edge(nodes, edges) else NULL
  # Use the WGCNA adjacency as FLCA's WCD input. The Top-50 candidate tree
  # retains all strongest-link parent chains for the companion plots; the
  # Top-20/23 sample can be much smaller in a dense, single-root network.
  flca <- run_flca(nodes[, c("name", "value")], edges,
                   top50 = 50L, target_n = 20L, max_cluster_size = 4L)
  junction_edges <- flca$candidate_tree_edges
  names(junction_edges)[names(junction_edges) == "Follower"] <- "follower"
  junction_membership <- junction_flca_membership(flca$candidate_nodes,
                                                  junction_edges,
                                                  min_members = 3L)
  junction_nodes <- flca$candidate_nodes
  junction_nodes$carac <- unname(junction_membership[junction_nodes$name])
  junction_ids <- sort(unique(junction_nodes$carac))
  junction_summary <- do.call(rbind, lapply(junction_ids, function(id) {
    members <- junction_nodes[junction_nodes$carac == id, , drop = FALSE]
    leader <- members$name[order(-members$value, members$name,
                                 method = "radix")[1L]]
    direct_followers <- sum(junction_edges$Leader == leader)
    data.frame(cluster = id, junction_leader = leader,
               members = nrow(members),
               direct_followers = direct_followers,
               candidate_leader = direct_followers >= 2L &&
                 nrow(members) >= 3L,
               stringsAsFactors = FALSE)
  }))
  module_ids <- sort(unique(membership))
  modules <- data.frame(module = module_ids,
                        members = as.integer(table(factor(membership, levels = module_ids))),
                        mean_node_value = vapply(module_ids, function(id)
                          mean(nodes$value[nodes$carac == id]), numeric(1)),
                        stringsAsFactors = FALSE)
  module_colors <- WGCNA::labels2colors(membership)
  module_colors[module_colors == "yellow"] <- "#6A3D9A"
  nodes$module_color <- module_colors
  nodes$connectivity <- rowSums(adjacency)
  nodes <- nodes[order(-nodes$value, nodes$name, method = "radix"), , drop = FALSE]
  rownames(nodes) <- NULL
  tom_mds <- wgcna_tom_mds(tom, nodes)
  chord <- wgcna_chord_data(adjacency, nodes)
  list(nodes = nodes, modules = modules, adjacency = adjacency, tom = tom,
       tom_mds = tom_mds, soft_threshold = soft_threshold, chord = chord,
       flca_nodes = flca$candidate_nodes,
       flca_junction_nodes = junction_nodes,
       flca_junction_summary = junction_summary,
       flca_tree_edges = flca$candidate_tree_edges,
       flca_full_edges = edges[edges$Leader %in% junction_nodes$name &
                               edges$Follower %in% junction_nodes$name,
                             , drop = FALSE],
       metrics = data.frame(mean_SS = q$silhouette$mean_silhouette,
                            Qw = q$modularity$modularity_Q,
                            Qu = q$modularity$normalized_Q),
       source = source, observations = nrow(x), power = power,
       node_value_mode = node_value_mode, edge_clustering = edge_clustering,
       excluded = excluded)
}
