# Deterministic, dependency-free Follower-Leading Clustering Algorithm.
flca_validate <- function(nodes, data) {
  if (!is.data.frame(nodes) || !all(c("name", "value") %in% names(nodes)))
    stop("nodes requires name and value")
  nodes$name <- as.character(nodes$name)
  if (!nrow(nodes) || anyNA(nodes$name) || any(!nzchar(nodes$name)) ||
      anyDuplicated(nodes$name) || !is.numeric(nodes$value) ||
      any(!is.finite(nodes$value))) stop("Invalid node names or values")
  if (!is.data.frame(data) || !all(c("Leader", "Follower") %in% names(data)))
    stop("data requires Leader and Follower")
  if (!"WCD" %in% names(data)) data$WCD <- rep(1, nrow(data))
  data$Leader <- as.character(data$Leader)
  data$Follower <- as.character(data$Follower)
  if (!is.numeric(data$WCD)) {
    raw_wcd <- trimws(as.character(data$WCD))
    missing_wcd <- is.na(raw_wcd) | !nzchar(raw_wcd) | toupper(raw_wcd) == "NA"
    parsed_wcd <- suppressWarnings(as.numeric(raw_wcd))
    if (any(!missing_wcd & !is.finite(parsed_wcd)))
      stop("Invalid WCD value")
    data$WCD <- parsed_wcd
  }
  data$WCD[is.na(data$WCD)] <- 1
  if (anyNA(data$Leader) || anyNA(data$Follower) ||
      any(!is.finite(data$WCD)) ||
      any(data$WCD < 0) ||
      any(!data$Leader %in% nodes$name) ||
      any(!data$Follower %in% nodes$name)) stop("Invalid edge endpoints or WCD")
  data <- data[data$Leader != data$Follower, c("Leader", "Follower", "WCD"), drop = FALSE]
  if (nrow(data)) {
    a <- pmin(data$Leader, data$Follower)
    b <- pmax(data$Leader, data$Follower)
    data <- stats::aggregate(data$WCD, list(Leader = a, Follower = b), max)
    names(data)[3L] <- "WCD"
  }
  rownames(data) <- NULL
  list(nodes = nodes[, c("name", "value"), drop = FALSE], data = data)
}

flca_rank <- function(nodes) {
  nodes <- nodes[order(-nodes$value, nodes$name, method = "radix"), , drop = FALSE]
  rownames(nodes) <- NULL
  nodes$rank <- seq_len(nrow(nodes))
  nodes$decimal <- round(nodes$value, 3) +
    ((nrow(nodes) - nodes$rank + 1) / nrow(nodes)) * 0.0001
  nodes$cluster <- nodes$rank
  nodes
}

flca_orient <- function(nodes, data) {
  if (!nrow(data)) return(data)
  r1 <- nodes$rank[match(data$Leader, nodes$name)]
  r2 <- nodes$rank[match(data$Follower, nodes$name)]
  swap <- r1 > r2
  leader <- ifelse(swap, data$Follower, data$Leader)
  follower <- ifelse(swap, data$Leader, data$Follower)
  data.frame(Leader = leader, Follower = follower, WCD = data$WCD,
             stringsAsFactors = FALSE)
}

flca_strongest <- function(nodes, edges) {
  if (!nrow(edges)) return(edges)
  leader_rank <- nodes$rank[match(edges$Leader, nodes$name)]
  follower_rank <- nodes$rank[match(edges$Follower, nodes$name)]
  edges <- edges[order(follower_rank, -edges$WCD, leader_rank,
                       method = "radix"), , drop = FALSE]
  edges <- edges[!duplicated(edges$Follower), , drop = FALSE]
  rownames(edges) <- NULL
  edges
}

flca_clusters <- function(nodes, tree_edges) {
  parent <- stats::setNames(tree_edges$Leader, tree_edges$Follower)
  root <- stats::setNames(rep(NA_character_, nrow(nodes)), nodes$name)
  visiting <- stats::setNames(rep(FALSE, nrow(nodes)), nodes$name)
  trace <- function(name) {
    if (!is.na(root[[name]])) return(root[[name]])
    if (visiting[[name]]) stop("Cycle in FLCA parent links")
    visiting[[name]] <<- TRUE
    p <- parent[name]
    if (is.na(p)) ans <- name else {
      if (!p %in% nodes$name) stop("Invalid FLCA parent reference")
      ans <- trace(p)
    }
    visiting[[name]] <<- FALSE
    root[[name]] <<- ans
    ans
  }
  roots <- vapply(nodes$name, trace, character(1))
  root_rank <- nodes$rank[match(unique(roots), nodes$name)]
  ordered_roots <- unique(roots)[order(root_rank)]
  nodes$carac <- match(roots, ordered_roots)
  nodes
}

flca_sample <- function(nodes, target_n = 20L, max_cluster_size = 4L) {
  # A single FLCA root leaves no other clusters to fill the target. Keep the
  # highest-ranked target_n nodes, then apply the two-group display exception.
  if (length(unique(nodes$carac)) == 1L) {
    out <- head(nodes[order(nodes$rank), , drop = FALSE], target_n)
    if (nrow(out) >= 2L)
      out$carac <- ifelse(seq_len(nrow(out)) <= ceiling(nrow(out) / 2), 1L, 2L)
    return(out)
  }
  top_cluster <- nodes$carac[which.min(nodes$rank)]
  ids <- unique(nodes$carac)
  ids <- ids[order(vapply(ids, function(id) min(nodes$rank[nodes$carac == id]), integer(1)))]
  ids <- ids[ids != top_cluster]
  sizes <- vapply(ids, function(id) min(sum(nodes$carac == id), max_cluster_size), integer(1))
  ids <- c(top_cluster, ids[sizes >= 3L], ids[sizes < 3L])
  selected <- integer()
  for (id in ids) {
    members <- which(nodes$carac == id)
    members <- members[order(nodes$rank[members])]
    selected <- c(selected, head(members, max_cluster_size))
    if (length(selected) >= target_n) break
  }
  out <- nodes[selected, , drop = FALSE]
  if (length(unique(out$carac)) == 1L && nrow(out) >= 2L) {
    out <- out[order(out$rank), , drop = FALSE]
    out$carac <- ifelse(seq_len(nrow(out)) <= ceiling(nrow(out) / 2), 1L, 2L)
  } else {
    ids <- unique(out$carac[order(out$rank)])
    out$carac <- match(out$carac, ids)
  }
  out[order(out$rank), , drop = FALSE]
}

flca_quality <- function(nodes, edges) {
  n <- nrow(nodes); names <- nodes$name; cl <- nodes$carac
  a <- matrix(0, n, n, dimnames = list(names, names))
  for (i in seq_len(nrow(edges))) {
    a[edges$Leader[i], edges$Follower[i]] <- edges$WCD[i]
    a[edges$Follower[i], edges$Leader[i]] <- edges$WCD[i]
  }
  k <- length(unique(cl)); strength <- rowSums(a); m <- sum(a) / 2
  diag_msg <- character()
  q <- if (m > 0) sum(vapply(unique(cl), function(id) {
    ix <- which(cl == id)
    sum(a[ix, ix, drop = FALSE]) / (2 * m) - (sum(strength[ix]) / (2 * m))^2
  }, numeric(1))) else NA_real_
  if (is.na(q)) diag_msg <- c(diag_msg, "Modularity undefined: no positive induced edge weight")
  qnorm <- if (k > 1L && is.finite(q)) q / (1 - 1 / k) else NA_real_
  if (k == 1L) diag_msg <- c(diag_msg, "Normalized Q and SS undefined: one cluster")
  d <- matrix(Inf, n, n); d[a > 0] <- 1 / a[a > 0]; diag(d) <- 0
  for (z in seq_len(n)) d <- pmin(d, outer(d[, z], d[z, ], "+"))
  longest <- d[is.finite(d) & d > 0]
  longest <- if (length(longest)) max(longest) else 1
  absent <- !is.finite(d)
  d[absent & outer(cl, cl, "==")] <- 1.25 * longest
  d[absent & !outer(cl, cl, "==")] <- 3 * longest
  ss <- rep(NA_real_, n)
  if (k > 1L) for (i in seq_len(n)) {
    peers <- which(cl == cl[i] & seq_len(n) != i)
    if (!length(peers)) { ss[i] <- 0; next }
    intra <- mean(d[i, peers])
    other <- min(vapply(setdiff(unique(cl), cl[i]),
                        function(id) mean(d[i, cl == id]), numeric(1)))
    ss[i] <- if (max(intra, other) == 0) 0 else (other - intra) / max(intra, other)
  }
  top <- which.max(nodes$value)
  if (strength[top] > 0) {
    p <- a; nonzero <- strength > 0
    p[nonzero, ] <- a[nonzero, , drop = FALSE] / strength[nonzero]
    indirect <- p %*% p
    diag(indirect) <- 0
    constraint <- sum((p[top, ] + indirect[top, ])^2)
    shc <- 1 - constraint
  } else {
    constraint <- shc <- NA_real_
    diag_msg <- c(diag_msg, "Top-1 SHC undefined: Top-1 is isolated")
  }
  list(silhouette = list(individual = data.frame(name = names, carac = cl, SS = ss),
                         mean_by_cluster = data.frame(cluster_id = unique(cl),
                           mean_SS = vapply(unique(cl), function(id) {
                             x <- ss[cl == id]; if (all(is.na(x))) NA_real_ else mean(x)
                           }, numeric(1))),
                         mean_silhouette = if (all(is.na(ss))) NA_real_ else mean(ss)),
       modularity = list(modularity_Q = q, normalized_Q = qnorm),
       top1_shc = list(name = names[top], value = nodes$value[top],
                       top1_constraint = constraint, top1_SHC = shc),
       diagnostics = diag_msg)
}

run_flca <- function(nodes, data, top50 = 50, target_n = 20,
                     max_cluster_size = 4, seed = NULL) {
  if (length(top50) != 1L || !is.finite(top50) || top50 < 1 ||
      length(target_n) != 1L || !is.finite(target_n) || target_n < 1 ||
      length(max_cluster_size) != 1L || !is.finite(max_cluster_size) ||
      max_cluster_size < 1) stop("top50, target_n, and max_cluster_size must be positive")
  v <- flca_validate(nodes, data)
  ranked <- flca_rank(v$nodes)
  candidate <- head(ranked, as.integer(top50))
  candidate_edges <- v$data[v$data$Leader %in% candidate$name &
                              v$data$Follower %in% candidate$name, , drop = FALSE]
  oriented <- flca_orient(candidate, candidate_edges)
  tree <- flca_strongest(candidate, oriented)
  if (nrow(tree)) {
    descending_follower_rank <- order(candidate$rank[match(tree$Follower, candidate$name)],
                                      decreasing = TRUE)
    for (i in descending_follower_rank) {
      follower <- match(tree$Follower[i], candidate$name)
      leader <- match(tree$Leader[i], candidate$name)
      candidate$cluster[follower] <- candidate$cluster[leader]
    }
  }
  candidate <- flca_clusters(candidate, tree)
  direct_count <- table(tree$Leader)
  candidate_leaders <- names(direct_count[direct_count >= 2L])
  selected <- flca_sample(candidate, as.integer(target_n), as.integer(max_cluster_size))
  selected_edges <- v$data[v$data$Leader %in% selected$name &
                             v$data$Follower %in% selected$name, , drop = FALSE]
  selected_tree <- tree[tree$Leader %in% selected$name &
                          tree$Follower %in% selected$name, , drop = FALSE]
  # Splitting one selected cluster changes membership only, never parent links.
  q <- flca_quality(selected, selected_edges)
  ids <- unique(selected$carac)
  clusters <- do.call(rbind, lapply(ids, function(id) {
    x <- selected[selected$carac == id, , drop = FALSE]
    x <- x[order(x$rank), , drop = FALSE]
    data.frame(cluster_id = id, leader_name = x$name[1L],
               member_count = nrow(x), highest_value = x$value[1L],
               member_names = paste(x$name, collapse = ", "))
  }))
  final_nodes <- selected[, c("name", "value", "carac"), drop = FALSE]
  rownames(final_nodes) <- NULL
  rownames(selected_edges) <- rownames(selected_tree) <- NULL
  list(nodes = final_nodes, edges = selected_edges, tree_edges = selected_tree,
       clusters = clusters, silhouette = q$silhouette,
       modularity = q$modularity, top1_shc = q$top1_shc,
       diagnostics = c(q$diagnostics,
         sprintf("Candidate leaders with >=2 direct followers: %s",
                 paste(candidate_leaders, collapse = ", "))),
       candidate_leaders = candidate_leaders,
       candidate_nodes = candidate[, c("name", "value", "carac")],
       candidate_tree_edges = tree)
}
