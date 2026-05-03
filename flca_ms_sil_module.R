# ------------------------------------------------------------
# Utility: pretty print for each stage
# ------------------------------------------------------------
print_stage <- function(label, obj, verbose = TRUE) {
  if (!isTRUE(verbose)) return(invisible(NULL))
  cat("\n---", label, "---\n")
  if (is.data.frame(obj)) {
    cat("[nrow]", nrow(obj), " [ncol]", ncol(obj), "\n")
    print(utils::head(obj, 3))
  } else {
    print(obj)
  }
  invisible(NULL)
}

normalize_nodes <- function(nodes){
  nodes$name <- as.character(nodes$name)
  for (v in intersect(c("value","value2","SSi","a_star","a_star1"), names(nodes))){
    nodes[[v]] <- suppressWarnings(as.numeric(nodes[[v]]))
    nodes[[v]][is.na(nodes[[v]])] <- 0
  }
  nodes
}

normalize_edges <- function(edges){
  names(edges)[1:2] <- c("Source","Target")
  edges$Source <- as.character(edges$Source)
  edges$Target <- as.character(edges$Target)

  wcol <- intersect(c("WCD","weight","value"), names(edges))
  if (length(wcol) == 1){
    edges[[wcol]] <- as.numeric(edges[[wcol]])
    edges <- edges[!is.na(edges[[wcol]]) & edges[[wcol]] > 0, ]
  }
  edges
}

fix_edge_cols <- function(ed){
  stopifnot(is.data.frame(ed))

  cn <- names(ed)

  # 1) 先處理 Source/Target
  if (all(c("Source","Target") %in% cn) && !all(c("Leader","Follower") %in% cn)) {
    names(ed)[match(c("Source","Target"), cn)] <- c("Leader","Follower")
    cn <- names(ed)
  }

  # 2) Follower / follower 統一成 Follower(給 major sampling 用)
  if ("follower" %in% cn && !("Follower" %in% cn)) {
    names(ed)[match("follower", cn)] <- "Follower"
    cn <- names(ed)
  }

  # 3) 如果是 from/to
  if (all(c("from","to") %in% cn) && !all(c("Leader","Follower") %in% cn)) {
    names(ed)[match(c("from","to"), cn)] <- c("Leader","Follower")
    cn <- names(ed)
  }

  # 4) 補 WCD
  if (!("WCD" %in% cn)) {
    if ("weight" %in% cn) ed$WCD <- ed$weight
    else if ("value" %in% cn) ed$WCD <- ed$value
    else if ("W" %in% cn) ed$WCD <- ed$W
    else ed$WCD <- 1
  }

  # 5) 清理型別與無效列
  if ("Leader" %in% names(ed))   ed$Leader   <- trimws(as.character(ed$Leader))
  if ("Follower" %in% names(ed)) ed$Follower <- trimws(as.character(ed$Follower))
  ed$WCD <- suppressWarnings(as.numeric(ed$WCD))
  ed <- ed[is.finite(ed$WCD) & ed$WCD > 0, , drop=FALSE]

  # 6) 最後保證存在 Leader/Follower
  if (!all(c("Leader","Follower") %in% names(ed))) {
    stop("fix_edge_cols(): cannot find Leader/Follower columns after normalization.")
  }

  ed[, c("Leader","Follower","WCD"), drop=FALSE]
}


# ------------------------------------------------------------
# Stage B: edges -> Leader/follower one-link (preprocess)
# ------------------------------------------------------------
# ============================================================
# preprocess_edges.R  (fully working)
# - Leader/follower direction decided by nodes$value (higher = Leader)
# - For each follower: keep only the strongest incoming edge
#   (tie-break by adding Leader's value / 10000 to WCD for ranking only)
# - Aggregate duplicated (Leader,follower) by sum(WCD)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
})

# Optional pretty printer (safe even if you don't care)
print_stage <- function(title, x, n = 10) {
  cat("\n==============================\n")
  cat(title, "\n")
  cat("==============================\n")
  if (is.data.frame(x)) {
    print(utils::head(x, n))
    cat(sprintf("[nrow=%d, ncol=%d]\n", nrow(x), ncol(x)))
  } else {
    print(x)
  }
  invisible(x)
}

preprocess_edges <- function(nodes, edges, verbose = TRUE) {

  df <- as.data.frame(edges, stringsAsFactors = FALSE)
  if (ncol(df) < 3) stop("edges must have at least 3 columns")

  # ---- 1) Standardize edges to columns: t1, t2, WCD ----
  cn <- names(df)

  has_LFW <- ("Leader" %in% cn) && (("follower" %in% cn) || ("Follower" %in% cn)) && ("WCD" %in% cn)

  if (isTRUE(has_LFW)) {
    # edges already in Leader/follower/WCD form
    if ("Follower" %in% cn && !"follower" %in% cn) {
      names(df)[names(df) == "Follower"] <- "follower"
    }
    df <- df[, c("Leader", "follower", "WCD"), drop = FALSE]
    df$t1 <- df$Leader
    df$t2 <- df$follower
  } else {
    # assume first 3 cols are endpoints + weight
    df <- df[, 1:3, drop = FALSE]
    colnames(df) <- c("t1", "t2", "WCD")
  }

  df$t1  <- trimws(as.character(df$t1))
  df$t2  <- trimws(as.character(df$t2))
  df$WCD <- suppressWarnings(as.numeric(df$WCD))
  df <- df[is.finite(df$WCD) & df$WCD > 0 & nzchar(df$t1) & nzchar(df$t2), , drop = FALSE]
  df <- df[df$t1 != df$t2, , drop = FALSE]   # remove self-loops

  # ---- 2) Build name -> value lookup from nodes$value ----
  if (!all(c("name", "value") %in% names(nodes))) {
    stop("nodes must contain columns: name, value")
  }
  nd <- as.data.frame(nodes[, c("name", "value")], stringsAsFactors = FALSE)
  nd$name  <- trimws(as.character(nd$name))
  nd$value <- suppressWarnings(as.numeric(nd$value))
  val_map  <- setNames(nd$value, nd$name)

  # ---- 3) Map node values onto edges endpoints ----
  df$v1 <- suppressWarnings(as.numeric(val_map[df$t1]))
  df$v2 <- suppressWarnings(as.numeric(val_map[df$t2]))

  # keep only edges whose endpoints exist in nodes
  df <- df[is.finite(df$v1) & is.finite(df$v2), , drop = FALSE]
  if (!nrow(df)) {
    out <- data.frame(Leader=character(0), follower=character(0), WCD=numeric(0))
    if (verbose) print_stage("Preprocess Leader/follower edges", out)
    return(out)
  }

  # ---- 4) Decide Leader/follower by value (tie handled deterministically) ----
  # If v1 == v2, break tie alphabetically so direction is stable
  eq <- (df$v1 == df$v2)
  if (any(eq)) {
    swap <- df$t1[eq] > df$t2[eq]
    if (any(swap)) {
      tmp <- df$t1[eq][swap]
      df$t1[eq][swap] <- df$t2[eq][swap]
      df$t2[eq][swap] <- tmp
      tmpv <- df$v1[eq][swap]
      df$v1[eq][swap] <- df$v2[eq][swap]
      df$v2[eq][swap] <- tmpv
    }
  }

  cond <- (df$v1 >= df$v2)
  cond[is.na(cond)] <- FALSE

  df$Leader   <- ifelse(cond, df$t1, df$t2)
  df$follower <- ifelse(cond, df$t2, df$t1)

  df <- df[, c("Leader", "follower", "WCD"), drop = FALSE]
  df$Leader   <- trimws(as.character(df$Leader))
  df$follower <- trimws(as.character(df$follower))
  df <- df[nzchar(df$Leader) & nzchar(df$follower) & df$Leader != df$follower, , drop = FALSE]

  # ---- 5) Add node_wcd from nodes$value for tie-break (ranking only) ----
  node_df <- as.data.frame(nodes[, c("name", "value")], stringsAsFactors = FALSE)
  node_df$name  <- trimws(as.character(node_df$name))
  node_df$value <- suppressWarnings(as.numeric(node_df$value))
  node_wcd <- setNames(node_df$value, node_df$name)

  # base tie-break uses Leader's node value; tiny perturbation keeps ordering stable
  df$source_WCD <- suppressWarnings(as.numeric(node_wcd[df$Leader]))
  df$source_WCD[!is.finite(df$source_WCD)] <- 0
  df$WCD_adj <- as.numeric(df$WCD) + df$source_WCD / 10000

  # ---- 6) EXACTLY ONE edge per follower (NO ties) ----
  # Deterministic: highest WCD_adj, then highest source_WCD, then Leader name
  relation_set_one <- df %>%
    group_by(follower) %>%
    arrange(desc(WCD_adj), desc(source_WCD), Leader) %>%
    slice(1) %>%
    ungroup()

  # Keep only original WCD in the final relation set
  df2 <- relation_set_one[, c("Leader", "follower", "WCD"), drop = FALSE]

  # ---- 7) (Optional) aggregate duplicates (rare now; harmless) ----
  agg <- aggregate(WCD ~ Leader + follower, data = df2, FUN = function(x) sum(x, na.rm = TRUE))
  agg <- agg[order(agg$WCD, decreasing = TRUE), , drop = FALSE]
  rownames(agg) <- NULL

  if (verbose) print_stage("Preprocess Leader/follower edges (single per follower)", agg)
  agg
}

# --------------------------
# Example usage (optional)
# --------------------------
# nodes <- data.frame(name=c("A","B","C"), value=c(10, 5, 8))
# edges <- data.frame(x=c("A","B","C","B"), y=c("B","C","A","A"), w=c(3,2,5,5))
# preprocess_edges(nodes, edges)


#----------------------------------------------------------#
# 主體:直接吃 nodes + edges 的 FLCA(對應你 Python 那版) #
#   nodes: 至少 name, value(可選 value2)                 #
#   edges: 至少三欄,前兩欄節點名,第三欄 WCD              #
#----------------------------------------------------------#
FLCA_nodes_edges <- function(nodes, edges, verbose = TRUE) {
    
    nd <- nodes
    
    if (!"value2" %in% names(nd)) nd$value2 <- nd$value
    
    nd <- nd %>%
        dplyr::select(name, value, value2) %>%
        dplyr::arrange(dplyr::desc(value), name) %>%
        dplyr::mutate(
            cluster    = dplyr::row_number(),
            dummygroup = cluster,
            Leader     = FALSE
        )
    
    print_stage("Stage A initial nodes", nd, verbose = verbose)
    
    # Stage B: Leader/follower 邊 + 每個 follower 保留一個 leader
    ed_all <- preprocess_edges(nd, edges, verbose = TRUE)
    
    ed_all <- ed_all %>%
        dplyr::filter(Leader   %in% nd$name,
                      follower %in% nd$name)
    
    nd_rank <- nd %>%
        dplyr::mutate(rank = dplyr::row_number())
    
    ed_sorted <- ed_all %>%
        dplyr::left_join(nd_rank %>% dplyr::select(name, rank),
                         by = c("Leader" = "name")) %>%
        dplyr::arrange(follower, dplyr::desc(WCD), rank)
    
    ed_one <- ed_sorted %>%
        dplyr::group_by(follower) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(Leader, follower, WCD)
    
    print_stage("Stage B one-link edges (max WCD per follower)",
                ed_one, verbose = verbose)
    
    # Stage C:true leaders + chain merge
    leader_counts <- table(ed_one$Leader)
    true_leaders <- names(leader_counts[leader_counts >= 2])
    
    # 一定把「value 最大的節點」列為 leader
    if (nrow(nd) > 0) {
        top1_name <- nd$name[1]
        true_leaders <- union(true_leaders, top1_name)
    }
    
    nd$Leader <- nd$name %in% true_leaders
    
    if (verbose) {
        cat("True leaders:", paste(true_leaders, collapse = ", "), "\n")
        print_stage("Stage C  nodes with Leader flag", nd, verbose = verbose)
    }
    
    # chain merge:從 WCD 小的開始,把 follower 往 leader 所在 cluster 拉
    ed_chain <- ed_one %>%
        dplyr::arrange(WCD)
    
    changed   <- TRUE
    iteration <- 1L
    
    while (changed) {
        changed <- FALSE
        for (i in seq_len(nrow(ed_chain))) {
            L <- ed_chain$Leader[i]
            F <- ed_chain$follower[i]
            
            # follower 本身是 Leader,就不要再往上拉
            if (nd$Leader[nd$name == F]) next
            
            cL <- nd$cluster[nd$name == L]
            cF <- nd$cluster[nd$name == F]
            
            if (cL != cF) {
                nd$cluster[nd$name == F] <- cL
                changed <- TRUE
                if (verbose) {
                    cat(sprintf("[Chain %d] %s -> %s (cluster %d)\n",
                                iteration, F, L, cL))
                }
            }
        }
        iteration <- iteration + 1L
    }
    
    print_stage("Stage D after chain merge", nd, verbose = verbose)
    
    # Stage E:小群 (<3) 向上併(bubble-up),孤島不併
    max_iter <- 20L
    iter_no  <- 0L
    
    repeat {
        iter_no <- iter_no + 1L
        
        cluster_sizes <- sort(table(nd$cluster))
        small_clusters <- as.numeric(names(cluster_sizes[cluster_sizes < 3]))
        small_clusters <- small_clusters[small_clusters != 1]
        
        if (length(small_clusters) == 0L) break
        
        changed <- FALSE
        
        for (sc in small_clusters) {
            members <- nd$name[nd$cluster == sc]
            parent_cluster <- NA_integer_
            
            for (m in members) {
                rel <- ed_one %>% dplyr::filter(follower == m)
                if (nrow(rel) == 0L) next  # 沒有往上的邊 -> 試下一個 member
                
                L <- rel$Leader[1]
                parent_cluster <- nd$cluster[nd$name == L]
                
                # 如果 leader 跟自己同群,再往上一層找一次
                if (parent_cluster == sc) {
                    rel2 <- ed_one %>% dplyr::filter(follower == L)
                    if (nrow(rel2) > 0L) {
                        L2 <- rel2$Leader[1]
                        parent_cluster <- nd$cluster[nd$name == L2]
                    }
                }
                # 找到一個候選 parent 就先停
                break
            }
            
            # 找不到 parent(完全孤島),或 parent 就是自己 -> 不併
            if (is.na(parent_cluster) || parent_cluster == sc) next
            
            nd$cluster[nd$cluster == sc] <- parent_cluster
            changed <- TRUE
            if (verbose) {
                cat(sprintf("[Bubble-up] cluster %d -> %d\n", sc, parent_cluster))
            }
        }
        
        if (!changed) {
            if (verbose) cat("[Bubble-up] no more mergable small clusters, stop.\n")
            break
        }
        if (iter_no >= max_iter) {
            if (verbose) cat(sprintf("[Bubble-up] reached max_iter=%d, stop.\n", max_iter))
            break
        }
    }
    
    print_stage("Stage E after small-cluster bubble-up", nd, verbose = verbose)
    
    # 最後: cluster 重新編號成 1,2,3,...（穩定規則：以每群最高 value2/value 由大到小）
    score_vec <- if ("value2" %in% names(nd)) nd$value2 else nd$value
    # per-cluster score = max(score_vec)
    cs <- tapply(score_vec, nd$cluster, function(x) suppressWarnings(max(as.numeric(x), na.rm = TRUE)))
    cs[is.na(cs)] <- 0
    # tie-breaker: smaller original cluster id first (when possible)
    ord_names <- names(cs)[order(-cs, suppressWarnings(as.numeric(names(cs))), na.last = TRUE)]
    unique_clusters <- ord_names
    mapping <- setNames(seq_along(unique_clusters), unique_clusters)

    nd$cluster <- unname(mapping[as.character(nd$cluster)])
    # 保證下游欄位一致：carac 與 membership 都等於 cluster
    nd$carac <- nd$cluster
    nd$membership <- nd$cluster

    print_stage("Stage F final clusters", nd, verbose = verbose)
    
    nodes_out <- nd %>% dplyr::select(name, value, value2, carac,cluster )
    edges_out <- ed_one %>% dplyr::arrange(dplyr::desc(WCD))
    
    list(nodes = nodes_out, data = edges_out)
}
# flca_core.R
# Contains your original FLCA() code (unchanged) sourced from flca.txt,
# plus a thin wrapper to convert its output into list(nodes=..., data=...).

# ---- Original FLCA (UNCHANGED) ----
# NOTE: The original FLCA relies on global variables `nodes` and `data`.
# The wrapper below assigns them from `network` before calling FLCA().
# flca_core.R
# Contains your original FLCA() code (unchanged) sourced from flca.txt,
# plus a thin wrapper to convert its output into list(nodes=..., data=...).

# ---- Original FLCA (UNCHANGED) ----
# NOTE: The original FLCA relies on global variables `nodes` and `data`.
# The wrapper below assigns them from `network` before calling FLCA().

FLCA<-function(network){
  # >>> PATCH MARKER: DATA_SCOPE_FIX_V9 <<<
  # Shiny-safe: use inputs from `network` instead of relying on global `nodes`/`data`
  if (!is.list(network) || is.null(network$nodes) || is.null(network$data)) {
    stop("FLCA(network) requires a list with $nodes and $data")
  }
  nodes <- network$nodes
  data  <- network$data
  # --------------------------------------------
  
#########################################
# Check if nodes has exactly 2 columns
names(nodes)[1] <- "name"
if (ncol(nodes) == 2) {
    colnames(nodes) <- c("name", "value")
} else {
   nodes <- nodes[, c("name", "value")]
   nodes<-nodes[,1:2]
    colnames(nodes) <- c("name", "value")
    #warning("The dataset does not have exactly 2 columns. Column names were not changed.")
}
nodes <- nodes[, c("name", "value")]
nodes<- nodes[,1:2]
nodes <- nodes[order(-nodes$value, nodes$name), ]
data <- data[, !is.na(colnames(data))]
if (ncol(data)>3){
data <- data %>%
    mutate(max_value = apply(select(., -c(1, 2)), 1, max)) %>%
    select(1, 2, max_value)
data <- data %>% 
    rename(WCD = max_value)

} else{
data<- data[,1:3]
} 
 WCD_is_integer <- (data$WCD == floor(data$WCD))

data <- data[order(-data$WCD), ]

if (all(WCD_is_integer)) {
    
    # decimal tie-break using sorted row index
    idx <- seq_len(nrow(data))
    data$WCD <- data$WCD + (nrow(data) - idx) / 100
     
    data <- data[, c(1,2,3)]
}


data<- data[,1:3]
colnames(nodes) <- c("name", "value")
# Rename columns in the data dataframe
 
# Check if the first row, first column of `data` exists in `nodes$name`
if (nodes[1, 1] %in% data[,1]) {
    colnames(data) <- c("Leader", "Follower", "WCD")
} else {
    colnames(data) <- c("Follower", "Leader", "WCD")
}

# Print result
# print(data)
 

# Check the first few rows
head(nodes)
head(data)
library(dplyr)

# Sorting nodes by value (descending) and name (ascending)
#nodes <- nodes %>%
 #   arrange(desc(value), name)
nodes <- nodes[order(-nodes$value, nodes$name), ]


# View the sorted nodes
head(nodes)
library(dplyr)

# Merge data with nodes to get the value for sorting
data <- data %>%
    left_join(nodes, by = c("Leader" = "name")) %>%
    rename(Leader_value = value) %>%
    left_join(nodes, by = c("Follower" = "name")) %>%
    rename(Follower_value = value)

# Sort by WCD (descending) and Leader's value (descending)
data <- data %>%
    arrange(desc(WCD), desc(Leader_value))
data <- data[order(-data$WCD,-data$Leader_value), ]
# Remove the extra columns if not needed

data <- data %>% select(Leader, Follower, WCD)

# View sorted data
head(data)
library(dplyr)

# Merge data with nodes to get the value for sorting
data <- data %>%
    mutate(
        Leader = as.character(Leader),
        Follower = as.character(Follower)
    ) %>%
    left_join(nodes, by = c("Leader" = "name")) %>%
    rename(Leader_value = value) %>%
    left_join(nodes, by = c("Follower" = "name")) %>%
    rename(Follower_value = value) %>%
    mutate(
        Leader_temp = ifelse(Follower_value > Leader_value, Follower, Leader),
        Follower_temp = ifelse(Follower_value > Leader_value, Leader, Follower)
    ) %>%
    arrange(desc(WCD), desc(Leader_value)) %>%
    select(Leader = Leader_temp, Follower = Follower_temp, WCD,Leader_value)

data <- data[order(-data$WCD,-data$Leader_value), ]

# Remove extra columns if not needed
data <- data %>% select(Leader, Follower, WCD)

# View sorted data
head(data)
library(dplyr)

# Assign group numbers based on descending value and ascending name

nodes <- nodes[order(-nodes$value, nodes$name), ]
nodes <- nodes %>%
    mutate(carac = row_number())  # Assign group numbers from 1 to n
nodes$dummygroup<-nodes$carac
# View updated nodes dataframe
head(nodes)


################FLCA+++++++++++++++++++
library(dplyr)

# IMPORTANT: run FLCA on the FULL node set (do NOT truncate to Top20 here).
# Top-N selection must happen AFTER FLCA (e.g., major sampling in app).
ncount <- nrow(nodes)

# Step 1: Ensure nodes are ordered and have an initial unique carac (1..n)
nodes <- nodes[order(-nodes$value, nodes$name), ]
nodes <- nodes %>%
  mutate(carac = row_number())  # Assign initial group numbers for all nodes

# Step 2: Keep only relations where both endpoints exist in nodes (full set)
data <- data %>%
  filter(Leader %in% nodes$name & Follower %in% nodes$name) %>%
  left_join(nodes, by = c("Leader" = "name")) %>%
  rename(Leader_value = value, Leader_carac = carac) %>%
  left_join(nodes, by = c("Follower" = "name")) %>%
  rename(Follower_value = value, Follower_carac = carac)
# Step 3: Retain only the maximum WCD for each Follower
data <- data %>%
    group_by(Follower) %>%
    filter(WCD == max(WCD, na.rm = TRUE)) %>%
    ungroup()

# Step 4: If multiple Leaders have the same WCD for a Follower, retain the Leader with the highest ranking in nodes
data <- data %>%
    arrange(Follower, Leader_carac) %>%  # Prioritize the Leader with the lowest carac (higher rank)
    group_by(Follower) %>%
    slice(1) %>%  # Retain only the top-ranked Leader per Follower
    ungroup() %>%
    arrange(desc(WCD))  # Sort data by descending WCD
data <- data[order(-data$WCD, data$Leader), ]

# Step 5: Assign cluster numbers iteratively
changed <- TRUE  # Flag to track changes in cluster assignment

while (changed) {
    changed <- FALSE
    
    for (i in seq_len(nrow(data))) {
        leader <- data$Leader[i]
        follower <- data$Follower[i]

        # Guard against NA / missing endpoints (prevents 'replacement length zero')
        if (is.na(leader) || is.na(follower)) next
        li <- match(leader, nodes$name)
        fi <- match(follower, nodes$name)
        if (is.na(li) || is.na(fi)) next

        leader_carac <- nodes$carac[li]
        follower_carac <- nodes$carac[fi]

        if (is.na(leader_carac) || is.na(follower_carac)) next

        # If Follower's carac is greater than Leader's, update it
        if (follower_carac > leader_carac) {
            nodes$carac[fi] <- leader_carac
            changed <- TRUE  # Track that an update has occurred
        }
    }
}

# Step 6: View final clustered nodes
print("Final clustered nodes:")
print(head(nodes, 20))

print("Filtered relation set with maximum WCD and prioritized Leader:")
print(data, n = 21)

#####################################2nd run for FLCA++++++++
library(dplyr)

# Step 1: Identify potential cluster leaders (leaders with multiple followers)
leaders_with_multiple_followers <- data %>%
    group_by(Leader) %>%
    filter(n() > 1) %>%
    pull(Leader) %>% unique()

# Step 2: Restore the initial cluster number for these leaders
nodes <- nodes %>%
    mutate(
        carac = ifelse(name %in% leaders_with_multiple_followers, dummygroup, carac),
        Leader = name %in% leaders_with_multiple_followers  # Add Leader column with TRUE/FALSE
    )
# Step 3: Filter `data` for relevant leaders
relevant_data <- data %>%
    filter(Leader %in% leaders_with_multiple_followers)

# Step 4: Ensure `Leader_carac` column does not already exist before renaming
if ("Leader_carac" %in% names(relevant_data)) {
    relevant_data <- relevant_data %>% select(-Leader_carac)  # Remove if it exists
}

# Step 5: Join `Leader_carac` from `nodes`
relevant_data <- relevant_data %>%
    left_join(nodes %>% select(name, carac), by = c("Leader" = "name")) %>%
    rename(Leader_carac = carac)

# Step 6: Add `Follower_carac` to `nodes` by matching `nodes$name` to `data$Follower`
nodes <- nodes %>%
    left_join(relevant_data %>% select(Follower, Leader_carac), 
              by = c("name" = "Follower")) %>%
    rename(Follower_carac = Leader_carac)

# Step 7: Iteratively update `nodes$carac` based on `Follower_carac`
changed <- TRUE  # Flag to track updates
######################3 RUN######################
nodes <- nodes %>%
    mutate(
        carac = ifelse(Leader == FALSE & !is.na(Follower_carac), Follower_carac, carac)
    )

##########Loop for convergency setting clusters to minimal member size#######################

# Function to get cluster sizes
# Function to get cluster sizes
get_cluster_sizes <- function(nodes) {
    table(nodes$carac) %>%
        as.data.frame() %>%
        rename(cluster = Var1, size = Freq)
}
cat("Cluster size at least number of member:",sep="\n")
 

table(nodes$carac)
tababc<-table(nodes$carac)
sorted_tab <- sort(tababc, decreasing = TRUE)
# Get the second highest frequency (name and count)
second_highest <- if (length(sorted_tab) >= 2) sorted_tab[2] else 0

# Extract just the numeric frequency (optional)
second_highest_num <- as.numeric(second_highest)
size_of_cluster<-3
if (length(unique(nodes$carac))>1){
if (second_highest_num<=3){
    size_of_cluster<-second_highest_num 
} 
}else{
  size_of_cluster<-1
}


# Function to process nodes iteratively until full convergence
process_nodes_until_full_convergence <- function(nodes, data) {
    repeat {
        changes_made <- FALSE  # Track if updates occur
        
        for (i in rev(seq_len(nrow(nodes)))) {
            if (nodes$dummygroup[i] == 1 || nodes$Leader[i] == TRUE) next  # Skip fixed nodes
            
            current_name <- nodes$name[i]
            matched_rows <- data %>% filter(Follower == current_name)  # Ensure data argument is passed
            
            if (nrow(matched_rows) > 0) {
                for (j in seq_len(nrow(matched_rows))) {
                    leader_name <- matched_rows$Leader[j]
                    leader_carac <- matched_rows$Leader_carac[j]
                    leader_row <- which(nodes$name == leader_name)
                    
                    if (length(leader_row) > 0) {
                        if (!is.na(nodes$carac[leader_row]) || nodes$Leader[leader_row] == TRUE) {
                            if (is.na(nodes$carac[i]) || nodes$carac[i] != nodes$carac[leader_row]) {  
                                nodes$carac[i] <- nodes$carac[leader_row]  # Merge into leader cluster
                                changes_made <- TRUE
                            }
                        }
                    }
                }
            }
        }
        
        if (!changes_made) break  # Stop if no changes occurred
    }
    return(nodes)
}




# **Step 1: Apply full convergence first**
nodes <- process_nodes_until_full_convergence(nodes, data)
nodes
# **Step 2: Merge small clusters before reprocessing**
# Function to mark small clusters (size < 5) for merging
size_of_cluster


 mark_small_clusters <- function(nodes, data, size_of_cluster, max_iterations = 10) {
    iteration <- 0  # Initialize iteration counter
    
    repeat {
        iteration <- iteration + 1  # Increment iteration counter
        
        cluster_sizes <- get_cluster_sizes(nodes)
        
        # Identify the smallest cluster size (excluding carac == 1)
        tmp_sizes <- cluster_sizes %>%
            filter(cluster != 1) %>%  # Exclude carac == 1
            pull(size)
        if (length(tmp_sizes) == 0) break
        min_cluster_size <- min(tmp_sizes)
        
        # Stop if all clusters meet the size criterion or max iterations reached
        if (min_cluster_size >= size_of_cluster || iteration >= max_iterations) {
            break
        }
        
        # Filter clusters that have this smallest size, excluding carac == 1
        smallest_clusters <- cluster_sizes %>%
            filter(size == min_cluster_size, cluster != 1) %>%  # Ensure carac != 1
            arrange(desc(as.numeric(cluster))) %>%  # Select the highest cluster number
            slice(1) %>%  # Keep only the first (largest cluster number)
            pull(cluster)  # Extract cluster ID
        
        # If all smallest clusters are `carac == 1`, stop the loop
        if (length(smallest_clusters) == 0) {
            break  # No changes needed
        }
        
        # **Apply condition only if `carac != 1`**
        nodes <- nodes %>%
            mutate(Leader = ifelse(carac != 1 & carac == smallest_clusters, FALSE, Leader))
        
        # Apply `process_nodes_until_full_convergence` after each update
        nodes <- process_nodes_until_full_convergence(nodes, data)
    }
     if (!('Leader' %in% names(nodes))) nodes$Leader <- FALSE
     if (nrow(nodes) >= 1) nodes$Leader[1] <- TRUE 
    return(nodes)
}
enforce_minimum_cluster_size <- function(nodes, min_size = 3) {
  csize <- table(nodes$carac)
  while (min(csize) < min_size) {
    small_cluster <- as.numeric(names(which.min(csize)))
    small_members <- nodes[nodes$carac == small_cluster, ]
    ref_clusters <- setdiff(unique(nodes$carac), small_cluster)
    dist_to_ref <- sapply(ref_clusters, function(g) {
      abs(mean(small_members$value) - mean(nodes$value[nodes$carac == g]))
    })
    merge_target <- ref_clusters[which.min(dist_to_ref)]
    nodes$carac[nodes$carac == small_cluster] <- merge_target
    csize <- table(nodes$carac)
  }
  return(nodes)
}

nodes <- enforce_minimum_cluster_size(nodes, min_size = 3)

cat("Balanced clusters ensured:\n")
print(table(nodes$carac))

nodes <- nodes[, colSums(is.na(nodes)) == 0]
if (size_of_cluster>1){
   nodes <- mark_small_clusters(nodes,data,size_of_cluster)
}
 

# Print the final optimized nodes
print(nodes)
##############if one component found by FLCA###########################
# Function to check cluster count and split if only one exists
 


##############if one component found by FLCA###########################
# Function to check cluster count and split if only one exists
split_cluster_if_needed <- function(nodes) {
    # Count unique clusters
    unique_clusters <- unique(nodes$carac)
    
    if (length(unique_clusters) == 1) {  # If only one cluster exists
        single_cluster <- unique_clusters[1]
        
        # Get all members of the single cluster and sort from top to bottom
        clustered_nodes <- nodes %>%
            filter(carac == single_cluster) %>%
            arrange(desc(value))  # Assuming name order represents top-to-bottom hierarchy
        
        # Determine split point (halfway)
        split_index <- ceiling(nrow(clustered_nodes) / 2)
        
        # Assign two new clusters
        clustered_nodes$carac[1:split_index] <- paste0(single_cluster, "_A")
        clustered_nodes$carac[(split_index + 1):nrow(clustered_nodes)] <- paste0(single_cluster, "_B")
        
        # Merge back with original nodes
        nodes <- nodes %>%
            left_join(clustered_nodes %>% select(name, carac), by = "name") %>%
            mutate(carac = ifelse(is.na(carac.y), carac.x, carac.y)) %>%
            select(-carac.x, -carac.y)
    }
    
    return(nodes)
}
 




if (!exists("FLCAcluster")) FLCAcluster <- 0
FLCAcluster <- 0

if (FLCAcluster==0){
# **Step 3: Split cluster if only one exists**
nodes <- split_cluster_if_needed(nodes)
}

######################################
# Subset rows where Leader is NOT equal to Follower
# Ensure that databk and nodesbk contain the expected columns
if (!all(c("Leader", "follower") %in% colnames(data))) {
   # stop("data does not contain expected columns 'Leader' and 'follower'")
colnames(data)[1:2] <- c("Leader", "Follower")

}
data <- subset(data, Leader != Follower)
nLeader<-length(unique(data$Leader))
# Count how many unique Leader names also appear in follower
common_leader_count <- sum(data$Leader %in% data$Follower)

# If you want to count how many unique Leader names overlap
unique_leader_in_follower <- sum(unique(data$Leader) %in% unique(data$Follower))

# Print the results
cat("Total overlapping (row-wise):", common_leader_count, "\n")
cat("Unique overlapping Leader names:", unique_leader_in_follower, "\n")
# Find the overlapping members
overlapping_members <- unique(data$Leader[data$Leader %in% data$Follower])

# Show unique overlapping members
unique_overlapping <- unique(overlapping_members)

# Print the result

NoAdvantage<-(nLeader-1) - unique_leader_in_follower
 Top1type<-"AA"
if (NoAdvantage==0){
   Top1type<-"NA"
 }
cat("NoAdvantage:", NoAdvantage, "\n")
cat("nLeader-1:", nLeader-1, "\n")
cat("unique_leader_in_follower:", unique_leader_in_follower, "\n")
print(unique_overlapping) 
#####################Updata cluster with sequential number from 1 to length(cluster)###############

 

unique_sorted <- sort(unique(nodes$carac))
# Use match() to replace each value in Cluster with its corresponding position in unique_sorted
nodes$carac <- match(nodes$carac, unique_sorted)
csize <- table(nodes$carac)
csize
print("Final clustered nodes after 3rd FLCA run:")
head(nodes,20)
print(data,3)
nodes <- nodes[, c("name", "value", "carac")]
nodes<-nodes[,1:3]
nodes$value2<-nodes$value
nodes <- nodes[, c("name", "value", "carac","value2")]
    nodes$membership<-nodes$carac
# Rename columns in the data dataframe
colnames(data) <- c("Leader", "follower", "WCD")


 # 1. Sort nodes by value2 descending
nodes <- nodes[order(-nodes$value2), ]

# 2. Extract top two nodes
top1 <- nodes[1, ]
top2 <- nodes[2, ]

# 3. If they are in different carac groups
if (top1$carac != top2$carac) {

  # 4. Find the single top row in data by WCD
  data_sorted <- data[order(-data$WCD), ]
  top_data <- data_sorted[1, ]  # highest WCD row

  # 5. Check if this top WCD row involves one (or both) of the top 2 nodes
  name1 <- top1$name
  name2 <- top2$name

 
    # Case A: Top node1 appears in the top WCD row
    if (top_data$Leader == name1 && top_data$follower  == name2|| top_data$Leader  == name2 &&top_data$follower == name1) {
        # Then give top1 the carac of top2
        nodes$carac[1] <- top2$carac
      
    }
}




nodes$carac<-nodes$membership


print(nodes,3)


 

 if (NoAdvantage==0 & length(unique(nodes$carac))>1 ){
   Top1type<-"RA"
 }

top1 <- nodes$name[1]
Top1type <- paste(top1, Top1type, sep = ",")

cat("Top 1 Leadership Type:", Top1type, "\n")
 result <- list(Leader=data$Leader, follower=data$follower, WCD=data$WCD, name=nodes$name, value=nodes$value, ncount=length(nodes$carac),membership =nodes$carac,carac =nodes$carac,  csize = csize,Top1type = Top1type)
  return(result)
} #function(result)
# ---- Wrapper: standardize output for the app ----
FLCA_run <- function(network) {
  # network: list(nodes=..., data=...)
  # In the original implementation FLCA() relies on global variables
  # `nodes` and `data`. We assign them here for backward compatibility.
  # Detect trivial cases where the network has too few nodes for meaningful
  # FLCA processing and return a simple clustering instead of throwing an error.
  if (!is.list(network) || is.null(network$nodes) || is.null(network$data)) {
    stop("FLCA_run(network) requires a list with $nodes and $data")
  }
  n_nodes <- nrow(network$nodes)
  # If fewer than 2 nodes, assign each node to its own cluster and return
  # If fewer than 3 nodes, skip the full FLCA logic. For 2 or fewer nodes
  # there isn't a meaningful cluster structure, so assign each node to its own
  # cluster and return the data as-is.
  if (is.null(n_nodes) || n_nodes < 3) {
    # Ensure name/value columns exist; fall back to first two columns
    nn <- network$nodes
    coln <- names(nn)
    nm_col <- if ("name" %in% coln) "name" else coln[1]
    val_col <- if ("value" %in% coln) "value" else coln[min(2, ncol(nn))]
    out_nodes <- data.frame(
      name  = as.character(nn[[nm_col]]),
      value = suppressWarnings(as.numeric(nn[[val_col]])),
      carac = seq_len(nrow(nn)),
      stringsAsFactors = FALSE
    )
    # Standardize data columns if present
    d <- network$data
    if (is.null(d) || nrow(d) == 0) {
      out_data <- data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE)
    } else {
      # Use first three columns as Leader, follower, WCD if available
      L <- as.character(d[[1]]); F <- as.character(d[[2]]); W <- suppressWarnings(as.numeric(d[[3]]))
      W[!is.finite(W)] <- 1
      out_data <- data.frame(Leader = L, follower = F, WCD = W, stringsAsFactors = FALSE)
    }
    return(list(nodes = out_nodes, data = out_data, raw = NULL))
  }
  # Assign globals for the original FLCA() call
# nodes <<- network$nodes  # patched: removed global assignment (Shiny-safe)
# data  <<- network$data  # patched: removed global assignment (Shiny-safe)
  # Run original FLCA algorithm inside tryCatch to guard against errors
  res <- tryCatch(
    {
      FLCA(network)
    },
    error = function(e) {
      # Fallback: if FLCA fails (e.g., replacement length zero), assign each node to its own cluster
      # and return an empty data relation. This prevents crashes in the Shiny app and downstream logic.
      nn <- network$nodes
      coln <- names(nn)
      nm_col <- if ("name" %in% coln) "name" else coln[1]
      val_col <- if ("value" %in% coln) "value" else coln[min(2, length(coln))]
      fallback_nodes <- data.frame(
        name      = as.character(nn[[nm_col]]),
        value     = suppressWarnings(as.numeric(nn[[val_col]])),
        membership= seq_len(nrow(nn)),
        stringsAsFactors = FALSE
      )
      fallback_data <- network$data
      if (is.null(fallback_data) || nrow(fallback_data) == 0) {
        fallback_data <- data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE)
      } else {
        # Standardize columns: use first three as Leader/follower/WCD
        L <- as.character(fallback_data[[1]])
        F <- as.character(fallback_data[[2]])
        W <- suppressWarnings(as.numeric(fallback_data[[3]]))
        W[!is.finite(W)] <- 1
        fallback_data <- data.frame(Leader = L, follower = F, WCD = W, stringsAsFactors = FALSE)
      }
      list(Leader = fallback_data$Leader,
           follower = fallback_data$follower,
           WCD = fallback_data$WCD,
           name = fallback_nodes$name,
           value = fallback_nodes$value,
           membership = fallback_nodes$membership)
    }
  )
  # Convert to standardized output.
  # Required columns:
  #   nodes: name, value, carac
  #   data : Leader, follower, WCD
  out_nodes <- data.frame(
    name  = as.character(res$name),
    value = suppressWarnings(as.numeric(res$value)),
    carac = as.integer(res$membership),
    stringsAsFactors = FALSE
  )
  out_data <- data.frame(
    Leader   = as.character(res$Leader),
    follower = as.character(res$follower),
    WCD      = suppressWarnings(as.numeric(res$WCD)),
    stringsAsFactors = FALSE
  )
  # Defensive cleanup
  out_nodes$value[!is.finite(out_nodes$value)] <- 0
  out_data$WCD[!is.finite(out_data$WCD)] <- 1
  list(nodes = out_nodes, data = out_data, raw = res)
}
# >>> PATCH MARKER: DATA_SCOPE_FIX_V9_END <<<

# --------------------------
# 2) Major sampling (Top-20 balanced)
# --------------------------
major_sample_flca_top20 <- function(nodes, edges,
                                    top_clusters = 5,
                                    base_per_cluster = 4,
                                    target_n = 20){
    
    stopifnot(is.data.frame(nodes), is.data.frame(edges))
    stopifnot("name" %in% names(nodes), "carac" %in% names(nodes))
    if (!("value" %in% names(nodes))) nodes$value <- 0
    if (!("value2" %in% names(nodes))) nodes$value2 <- nodes$value
    
    nodes$name   <- as.character(nodes$name)
    nodes$value  <- suppressWarnings(as.numeric(nodes$value))
    nodes$value2 <- suppressWarnings(as.numeric(nodes$value2))
    # keep cluster id numeric even if carac is like "C1"
    car0 <- trimws(as.character(nodes$carac))
    car_int <- suppressWarnings(as.integer(gsub("^C","", toupper(car0))))
    if (all(is.na(car_int))) car_int <- as.integer(factor(car0))
    nodes$carac <- car_int
    nodes$value[is.na(nodes$value)] <- 0
    nodes$value2[is.na(nodes$value2)] <- nodes$value[is.na(nodes$value2)]
    nodes$carac[is.na(nodes$carac)] <- 1L
    
    edges$Leader   <- as.character(edges$Leader)
    edges$Follower <- as.character(edges$Follower)
    edges$WCD      <- suppressWarnings(as.numeric(edges$WCD))
    edges <- edges[is.finite(edges$WCD) & edges$WCD > 0, , drop=FALSE]
    
    # rank clusters
    cl_size <- tapply(nodes$name, nodes$carac, length)
    cl_sumv <- tapply(nodes$value, nodes$carac, sum)
    cl_tbl <- data.frame(
        carac = as.integer(names(cl_size)),
        n = as.integer(cl_size),
        sumv = as.numeric(cl_sumv[names(cl_size)]),
        stringsAsFactors = FALSE
    )
    cl_tbl <- cl_tbl[order(-cl_tbl$n, -cl_tbl$sumv), , drop=FALSE]
    keep_carac <- head(cl_tbl$carac, top_clusters)
    
    picked <- character()
    remaining <- list()
    
    for (cid in keep_carac) {
        sub <- nodes[nodes$carac == cid, , drop=FALSE]
        sub <- sub[order(-sub$value, sub$name), , drop=FALSE]
        take <- head(sub$name, base_per_cluster)
        picked <- c(picked, take)
        remaining[[as.character(cid)]] <- sub$name[!(sub$name %in% take)]
    }
    picked <- unique(picked)
    
    # fill round-robin
    if (length(picked) < target_n) {
        need <- target_n - length(picked)
        cids <- as.character(keep_carac)
        i <- 1
        while (need > 0 && any(vapply(remaining[cids], length, integer(1)) > 0)) {
            cid <- cids[((i - 1) %% length(cids)) + 1]
            if (length(remaining[[cid]]) > 0) {
                nxt <- remaining[[cid]][1]
                remaining[[cid]] <- remaining[[cid]][-1]
                if (!(nxt %in% picked)) {
                    picked <- c(picked, nxt)
                    need <- need - 1
                }
            }
            i <- i + 1
            if (i > 100000) break
        }
    }
    
    # still short: fill by global value
    if (length(picked) < target_n) {
        rest <- nodes[!(nodes$name %in% picked), , drop=FALSE]
        rest <- rest[order(-rest$value, rest$name), , drop=FALSE]
        picked <- unique(c(picked, head(rest$name, target_n - length(picked))))
    }
    
    sampled_nodes <- nodes[nodes$name %in% picked, , drop=FALSE]
    sampled_nodes <- sampled_nodes[order(-sampled_nodes$value, sampled_nodes$name), , drop=FALSE]
    
    sampled_edges <- edges[edges$Leader %in% picked & edges$Follower %in% picked, , drop=FALSE]
    sampled_edges <- sampled_edges[, c("Leader","Follower","WCD"), drop=FALSE]
    
    list(nodes = sampled_nodes, data = sampled_edges)
}


# --------------------------
# 2.5) Modularity Q (by cluster) on final Top-20 after major sampling
#   - returns per-cluster contribution Q_cluster and global Q_total
#   - uses undirected aggregation of Leader/Follower with weight WCD
# --------------------------
# --------------------------
# 2.5) Modularity Q (by cluster) on final Top-20 after major sampling
#   - returns per-cluster contribution for:
#       * Qw_cluster (weighted, using WCD)
#       * Qu_cluster (unweighted, each undirected edge = 1)
#   - and global totals as attributes:
#       * attr(out, "Qw_total"), attr(out, "Qu_total")
#   - uses undirected aggregation of Leader/Follower (unique unordered pairs)
# --------------------------
compute_modularity_by_cluster <- function(nodes20, edges20, eps = 1e-12){
  stopifnot(is.data.frame(nodes20), is.data.frame(edges20))
  stopifnot(all(c("name","carac") %in% names(nodes20)))

  nm <- trimws(as.character(nodes20$name))
  car0 <- trimws(as.character(nodes20$carac))
  car <- suppressWarnings(as.integer(gsub("^C","", toupper(car0))))
  if (all(is.na(car))) car <- as.integer(factor(car0))
  ok <- nzchar(nm) & is.finite(car)
  nm <- nm[ok]; car <- car[ok]
  nodes <- data.frame(name = nm, carac = car, stringsAsFactors = FALSE)

  cl_ids <- sort(unique(nodes$carac))
  n_nodes <- tapply(nodes$name, nodes$carac, length)

  # edges
  ed <- as_LFW(edges20)
  ed$Leader   <- trimws(as.character(ed$Leader))
  ed$Follower <- trimws(as.character(ed$Follower))
  ed$WCD      <- suppressWarnings(as.numeric(ed$WCD))
  ed <- ed[is.finite(ed$WCD) & ed$WCD > 0 & nzchar(ed$Leader) & nzchar(ed$Follower), , drop=FALSE]
  ed <- ed[ed$Leader %in% nodes$name & ed$Follower %in% nodes$name, , drop=FALSE]

  if (nrow(ed) == 0) {
    out0 <- data.frame(
      carac = cl_ids,
      n_nodes = as.integer(n_nodes[as.character(cl_ids)]),
      Qw_cluster = 0,
      Qu_cluster = 0,
      stringsAsFactors = FALSE
    )
    out0 <- out0[order(out0$carac), , drop=FALSE]
    attr(out0, "Qw_total") <- 0
    attr(out0, "Qu_total") <- 0
    return(out0)
  }

  # undirected aggregate
  a <- pmin(ed$Leader, ed$Follower)
  b <- pmax(ed$Leader, ed$Follower)
  key <- paste(a, b, sep = "
")
  wsum <- tapply(ed$WCD, key, sum)
  ab <- do.call(rbind, strsplit(names(wsum), "
", fixed = TRUE))
  a_u <- ab[,1]; b_u <- ab[,2]
  w_u <- as.numeric(wsum)
  w_u[!is.finite(w_u)] <- 0
  w_u_unw <- rep(1, length(w_u))  # unweighted: each unique undirected edge counts as 1

  # map node -> cluster
  mem <- setNames(nodes$carac, nodes$name)

  compute_Q <- function(w_vec){
    m <- sum(w_vec, na.rm = TRUE)
    if (!is.finite(m) || m <= 0) m <- eps

    w_in <- setNames(rep(0, length(cl_ids)), cl_ids)
    w_tot <- setNames(rep(0, length(cl_ids)), cl_ids)

    for (k in seq_along(w_vec)) {
      u <- a_u[k]; v <- b_u[k]; w <- w_vec[k]
      if (!is.finite(w) || w <= 0) next
      cu <- mem[[u]]; cv <- mem[[v]]
      if (is.na(cu) || is.na(cv)) next

      # total incident weight per cluster (each endpoint counts once for this undirected edge)
      w_tot[as.character(cu)] <- w_tot[as.character(cu)] + w
      w_tot[as.character(cv)] <- w_tot[as.character(cv)] + w

      # internal weight
      if (cu == cv) w_in[as.character(cu)] <- w_in[as.character(cu)] + w
    }

    Qc <- (w_in / m) - ( (w_tot / (2*m))^2 )
    Qc[!is.finite(Qc)] <- 0
    list(Qc = Qc, Q_total = sum(Qc, na.rm = TRUE))
  }

  qw <- compute_Q(w_u)
  qu <- compute_Q(w_u_unw)

  out <- data.frame(
    carac = as.integer(names(qw$Qc)),
    n_nodes = as.integer(n_nodes[names(qw$Qc)]),
    Qw_cluster = as.numeric(qw$Qc),
    Qu_cluster = as.numeric(qu$Qc[names(qw$Qc)]),
    stringsAsFactors = FALSE
  )
  out$Qw_cluster[!is.finite(out$Qw_cluster)] <- 0
  out$Qu_cluster[!is.finite(out$Qu_cluster)] <- 0

  out <- out[order(out$carac), , drop=FALSE]
  attr(out, "Qw_total") <- as.numeric(qw$Q_total)
  attr(out, "Qu_total") <- as.numeric(qu$Q_total)
  out
}

# --------------------------
# 3) Silhouette (SS) + a* + nearest-neighbor cluster + nearest-neighbor node
#    with penalties (intra=2, inter=5)
# -------------------------- (SS) + a* + nearest-neighbor cluster + nearest-neighbor node
#    with penalties (intra=2, inter=5)
# --------------------------
compute_silhouette_df <- function(nodes, edges,
                                  intra_delta = 2,
                                  inter_delta = 5,
                                  eps = 1e-9){

  stopifnot(is.data.frame(nodes), nrow(nodes) > 0)
  stopifnot(is.data.frame(edges), nrow(edges) > 0)

  nm <- trimws(as.character(nodes$name))
  car0 <- trimws(as.character(nodes$carac))
  cl_raw <- suppressWarnings(as.integer(gsub("^C","", toupper(car0))))
  if (all(is.na(cl_raw))) cl_raw <- as.integer(factor(car0))
  ok <- nzchar(nm) & is.finite(cl_raw)
  nm <- nm[ok]; cl_raw <- cl_raw[ok]

  # distinct name
  keep <- !duplicated(nm)
  nm <- nm[keep]; cl_raw <- cl_raw[keep]

  # if only 1 cluster -> trivial
  if (length(unique(cl_raw)) < 2) {
    return(data.frame(
      name = nm,
      ssi = 0, a_i = 0, b_i = 0, a_star1 = 1,
      nn_cluster = cl_raw,         # same cluster (only one)
      nn_name = NA_character_,
      neighborC = paste0("C", as.character(cl_raw)),
      stringsAsFactors = FALSE
    ))
  }

  Leader   <- trimws(as.character(edges$Leader))
  Follower <- trimws(as.character(edges$Follower))
  WCD      <- suppressWarnings(as.numeric(edges$WCD))
  ok2 <- nzchar(Leader) & nzchar(Follower) & is.finite(WCD) & (WCD > 0)
  Leader <- Leader[ok2]; Follower <- Follower[ok2]; WCD <- WCD[ok2]

  in_nm <- (Leader %in% nm) & (Follower %in% nm)
  Leader <- Leader[in_nm]; Follower <- Follower[in_nm]; WCD <- WCD[in_nm]
  if (!length(WCD)) {
    return(data.frame(
      name = nm,
      ssi = 0, a_i = 0, b_i = 0, a_star1 = 1,
      nn_cluster = NA_integer_,
      nn_name = NA_character_,
      neighborC = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  # undirected merge sum by unordered pair
  a <- pmin(Leader, Follower)
  b <- pmax(Leader, Follower)
  key <- paste(a, b, sep = "\r")
  Wsum <- tapply(WCD, key, sum)
  ab <- do.call(rbind, strsplit(names(Wsum), "\r", fixed = TRUE))
  a_u <- ab[,1]; b_u <- ab[,2]; w_u <- as.numeric(Wsum)

  n <- length(nm)
  idx <- setNames(seq_len(n), nm)

  W <- matrix(0, n, n, dimnames = list(nm, nm))
  ia <- idx[a_u]; ib <- idx[b_u]
  W[cbind(ia, ib)] <- w_u
  W[cbind(ib, ia)] <- w_u
  diag(W) <- 0

  has_edge <- (W > 0)
  cost <- matrix(NA_real_, n, n, dimnames = dimnames(W))
  cost[has_edge] <- 1 / (W[has_edge] + eps)
  max_cost <- if (any(has_edge)) max(cost[has_edge], na.rm = TRUE) else 1

  same_cluster <- outer(cl_raw, cl_raw, "==")

  D <- matrix(NA_real_, n, n, dimnames = dimnames(W))
  D[has_edge] <- cost[has_edge]
  D[!has_edge &  same_cluster] <- max_cost * (1 + intra_delta)
  D[!has_edge & !same_cluster] <- max_cost * (1 + inter_delta)
  diag(D) <- 0

  D_sym <- pmin(D, t(D))
  diag(D_sym) <- 0

  # a(i), b(i), ssi, and NN cluster/name
  a_i <- rep(0, n)
  b_i <- rep(0, n)
  ssi <- rep(0, n)
  nn_cluster <- rep(NA_character_, n)
  nn_name    <- rep(NA_character_, n)

  # integer clusters for silhouette + mapping back to original label
  cl_levels <- sort(unique(cl_raw))
  cl_int <- as.integer(factor(cl_raw, levels = cl_levels))

  if (requireNamespace("cluster", quietly = TRUE)) {
    sil <- cluster::silhouette(cl_int, as.dist(D_sym))
    ssi_vec <- as.numeric(sil[, "sil_width"])
    names(ssi_vec) <- rownames(sil)
    ssi <- ssi_vec  # aligned by silhouette output order (matches nm order in practice)

    for (i in seq_len(n)) {
      # a(i)
      same_idx <- which(cl_int == cl_int[i])
      same_idx <- setdiff(same_idx, i)
      a_i[i] <- if (length(same_idx) > 0) mean(D_sym[i, same_idx]) else NA_real_

      # b(i) + nearest-neighbor cluster (the cluster achieving b(i))
      other <- setdiff(unique(cl_int), cl_int[i])
      if (length(other) > 0) {
        d_to <- vapply(other, function(k) mean(D_sym[i, which(cl_int == k)]), numeric(1))
        k_star <- other[which.min(d_to)]
        b_i[i] <- min(d_to)

        # nearest node *within* that nearest cluster
        cand <- which(cl_int == k_star)
        j_star <- cand[which.min(D_sym[i, cand])]
        nn_cluster[i] <- cl_levels[k_star]  # map back to original carac label
        nn_name[i] <- nm[j_star]
      } else {
        b_i[i] <- NA_real_
      }
    }
  } else {
    # manual fallback
    for (i in seq_len(n)) {
      same <- which(cl_int == cl_int[i])
      other_clusters <- setdiff(unique(cl_int), cl_int[i])
      if (length(same) <= 1 || length(other_clusters) == 0) next

      a_ <- mean(D_sym[i, same[same != i]], na.rm = TRUE)

      d_to <- vapply(other_clusters, function(k){
        idxc <- which(cl_int == k)
        mean(D_sym[i, idxc], na.rm = TRUE)
      }, numeric(1))

      k_star <- other_clusters[which.min(d_to)]
      b_ <- min(d_to, na.rm = TRUE)

      if (!is.finite(a_)) a_ <- 0
      if (!is.finite(b_)) b_ <- 0

      a_i[i] <- a_
      b_i[i] <- b_
      ssi[i] <- if (max(a_, b_) == 0) 0 else (b_ - a_) / max(a_, b_)

      cand <- which(cl_int == k_star)
      j_star <- cand[which.min(D_sym[i, cand])]
      nn_cluster[i] <- cl_levels[k_star]
      nn_name[i] <- nm[j_star]
    }
  }

  a_star1 <- 1 / (1 + a_i + eps)

  out <- data.frame(
    name = nm,
    ssi = as.numeric(ssi),
    a_i = as.numeric(a_i),
    b_i = as.numeric(b_i),
    a_star1 = as.numeric(a_star1),
    nn_cluster = as.character(nn_cluster),     # <- 不要 as.integer
    neighborC  = ifelse(is.na(nn_cluster) | nn_cluster=="", paste0("C", as.character(cl_raw)), ifelse(grepl("^C", toupper(as.character(nn_cluster))), as.character(nn_cluster), paste0("C", as.character(nn_cluster)))),
    nn_name = as.character(nn_name),      # nearest node name in that cluster
    stringsAsFactors = FALSE
  )

  out$ssi[!is.finite(out$ssi)] <- 0
  out$a_star1[!is.finite(out$a_star1)] <- 0
  out
}

as_LFW <- function(ed){
  ed <- as.data.frame(ed, stringsAsFactors = FALSE)

  # allow Source/Target / from/to / Leader/Follower / follower
  cn <- names(ed)

  if (all(c("Source","Target") %in% cn) && !all(c("Leader","Follower") %in% cn)) {
    names(ed)[match(c("Source","Target"), cn)] <- c("Leader","Follower")
    cn <- names(ed)
  }
  if (all(c("from","to") %in% cn) && !all(c("Leader","Follower") %in% cn)) {
    names(ed)[match(c("from","to"), cn)] <- c("Leader","Follower")
    cn <- names(ed)
  }
  if ("follower" %in% cn && !("Follower" %in% cn)) {
    names(ed)[match("follower", cn)] <- "Follower"
    cn <- names(ed)
  }

  # weight column -> WCD
  if (!("WCD" %in% cn)) {
    wcol <- intersect(c("weight","value","W"), cn)
    if (length(wcol) >= 1) ed$WCD <- ed[[wcol[1]]] else ed$WCD <- 1
  }

  ed$Leader   <- trimws(as.character(ed$Leader))
  ed$Follower <- trimws(as.character(ed$Follower))
  ed$WCD      <- suppressWarnings(as.numeric(ed$WCD))
  ed$WCD[!is.finite(ed$WCD)] <- 0
  ed <- ed[ed$WCD > 0 & nzchar(ed$Leader) & nzchar(ed$Follower),
           c("Leader","Follower","WCD"), drop=FALSE]
  ed
}
compute_node_values_from_edges <- function(nodes, edges, keep_diag = TRUE){

  ed <- as.data.frame(edges, stringsAsFactors = FALSE)
  if (ncol(ed) < 2) stop("edges need at least 2 columns")

  # --- standardize endpoints to Source/Target ---
  cn <- names(ed)
  if (all(c("Leader","Follower") %in% cn)) {
    ed$Source <- ed$Leader
    ed$Target <- ed$Follower
  } else if (all(c("Source","Target") %in% cn)) {
    # ok
  } else if (all(c("from","to") %in% cn)) {
    ed$Source <- ed$from
    ed$Target <- ed$to
  } else {
    names(ed)[1:2] <- c("Source","Target")
  }

  ed$Source <- trimws(as.character(ed$Source))
  ed$Target <- trimws(as.character(ed$Target))

  # --- pick weight column ---
  wcol <- intersect(c("WCD","weight","value","W"), names(ed))
  if (!length(wcol)) {
    ed$w <- 1
  } else {
    ed$w <- suppressWarnings(as.numeric(ed[[wcol[1]]]))
  }

  ed <- ed[is.finite(ed$w) & ed$w > 0 & nzchar(ed$Source) & nzchar(ed$Target), , drop=FALSE]

  # --- diag/off-diag split ---
  is_diag <- ed$Source == ed$Target
  diag_sum <- numeric(0)
  if (keep_diag && any(is_diag)) {
    diag_sum <- tapply(ed$w[is_diag], ed$Source[is_diag], sum)
  }

  off <- ed[!is_diag, , drop=FALSE]

  # off-diag node total = outgoing + incoming (total strength excluding diag)
  off_out <- if (nrow(off)) tapply(off$w, off$Source, sum) else numeric(0)
  off_in  <- if (nrow(off)) tapply(off$w, off$Target, sum) else numeric(0)

  value2 <- off_out
  if (length(off_in)) {
    nms <- names(off_in)
    value2[nms] <- (value2[nms] %||% 0) + off_in
  }

  # align to nodes$name
  nodes$name <- trimws(as.character(nodes$name))

  nodes$value2 <- as.numeric(value2[nodes$name]); nodes$value2[is.na(nodes$value2)] <- 0
  nodes$diag   <- as.numeric(diag_sum[nodes$name]); nodes$diag[is.na(nodes$diag)] <- 0

  nodes$value  <- nodes$value2 + nodes$diag
  nodes
}

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a

run_flca_ms_sil_runner <- function(nodes, edges0, cfg = list(), verbose = TRUE) {
  stopifnot(is.data.frame(nodes), is.data.frame(edges0))

  # defaults
  cfg <- modifyList(
    list(
      top_clusters = 5, base_per_cluster = 4, target_n = 20,
      intra_delta = 2, inter_delta = 5, eps = 1e-9
    ),
    cfg
  )

  # A) normalize inputs
  nodes  <- normalize_nodes(nodes)
  edges0 <- normalize_edges(edges0)

  nodes_in <- nodes[, intersect(c("name", "value", "value2"), names(nodes)), drop = FALSE]
  if (!all(c("name", "value") %in% names(nodes_in))) stop("nodes must have name,value")

  # edges -> Leader/Follower/WCD
  edges0 <- as_LFW(edges0)  # 若你 module 沒有 as_LFW,改用 fix_edge_cols(edges0)
  if (isTRUE(verbose)) cat("[OK] raw nodes n=", nrow(nodes_in), " raw edges n=", nrow(edges0), "\n")
# --- ensure data_in exists (L/F/WCD) ---
data0 <- as.data.frame(edges0, stringsAsFactors = FALSE)

# harmonize common column names
if ("follower" %in% names(data0) && !"Follower" %in% names(data0)) {
  names(data0)[names(data0) == "follower"] <- "Follower"
}
if ("source" %in% names(data0) && !"Leader" %in% names(data0)) {
  names(data0)[names(data0) == "source"] <- "Leader"
}
if ("target" %in% names(data0) && !"Follower" %in% names(data0)) {
  names(data0)[names(data0) == "target"] <- "Follower"
}

need <- c("Leader","Follower","WCD")
if (!all(need %in% names(data0))) {
  stop("[INPUT ERROR] edges need Leader/Follower/WCD; got: ",
       paste(names(data0), collapse=", "), call.=FALSE)
}

data0$Leader   <- trimws(as.character(data0$Leader))
data0$Follower <- trimws(as.character(data0$Follower))
data0$WCD      <- suppressWarnings(as.numeric(data0$WCD))
data0 <- data0[!is.na(data0$Leader) & nzchar(data0$Leader) &
                   !is.na(data0$Follower) & nzchar(data0$Follower) &
                   !is.na(data0$WCD), ]
# Source/Target -> Leader/Follower
if (all(c("Source","Target") %in% names(data0)) && !("Leader" %in% names(data0))) {
  names(data0)[names(data0)=="Source"] <- "Leader"
}
if ("Target" %in% names(data0) && !("Follower" %in% names(data0))) {
  names(data0)[names(data0)=="Target"] <- "Follower"
}




  fl  <- tryCatch(FLCA_run(list(nodes=nodes, data=data0)), error=function(e) e)
 
# ---- Robust guard (minimal) ----
if (inherits(fl, "error")) {
  stop("[FLCA_run ERROR] ", fl$message, call. = FALSE)
}
if (!is.list(fl)) {
  stop("[FLCA_run BAD OUTPUT] not a list; class=", paste(class(fl), collapse=","), call. = FALSE)
}

# 有些版本可能不是叫 nodes/data(例如 node/edges, vertices/edges, etc.)
pick1 <- function(x, keys){
  for (k in keys) if (!is.null(x[[k]])) return(x[[k]])
  NULL
}
nodes_full <- pick1(fl, c("nodes","node","Vertices","vertices","V","Nodes"))
edges_raw  <- pick1(fl, c("data","edges","edge","E","Edges"))
nodes_full$carac<-nodes_full$membership

if (is.null(nodes_full) || is.null(edges_raw)) {
  stop(
    "[FLCA_run OUTPUT MISMATCH] expected nodes+data but got: ",
    paste(names(fl), collapse=", "),
    call. = FALSE
  )
}

edges_fl <- as_LFW(edges_raw)


  
  
  
  
  #network <- graph_from_data_frame(d=data, vertices=nodes, directed=F) 
  #fl <- FLCA(network)
  # B) FLCA
  # fl <- FLCA_nodes_edges(nodes_in[, c("name", "value"), drop = FALSE], edges0_LFW, verbose = verbose)
  stopifnot(is.list(fl), !is.null(fl$nodes), !is.null(fl$data))
  nodes_full <- fl$nodes
  edges_fl   <- as_LFW(fl$data)

  # C) Major sampling on ORIGINAL edges
  ms <- major_sample_flca_top20(
    nodes_full, edges0,
    top_clusters = cfg$top_clusters,
    base_per_cluster = cfg$base_per_cluster,
    target_n = cfg$target_n
  )
  nodes20 <- ms$nodes
  edges20 <- as_LFW(ms$data)
  # ensure nodes20$value matches edge sums on the SAME edge set used later
  nodes20 <- compute_node_values_from_edges(nodes20, edges20, keep_diag = TRUE)


  
  # D) direction by value for SS stage (Leader/Follower/WCD) - single link per follower (NO ties)
  edges20_dir <- preprocess_edges(nodes20, edges20, verbose = FALSE)
  # preprocess_edges returns columns: Leader, follower, WCD  -> harmonize to Leader, Follower, WCD
  edges20_dir <- as.data.frame(edges20_dir, stringsAsFactors = FALSE)
  if ("follower" %in% names(edges20_dir) && !("Follower" %in% names(edges20_dir))) {
    names(edges20_dir)[names(edges20_dir) == "follower"] <- "Follower"
  }
  edges20_dir <- edges20_dir[, intersect(c("Leader","Follower","WCD"), names(edges20_dir)), drop = FALSE]


  # E) Silhouette + a*
  sil_df <- compute_silhouette_df(
    nodes20, edges20_dir,
    intra_delta = cfg$intra_delta,
    inter_delta = cfg$inter_delta,
    eps = cfg$eps
  )

  modes <- merge(nodes20, sil_df, by = "name", all.x = TRUE)

  # force non-NA
  if ("ssi" %in% names(modes))     modes$ssi[!is.finite(modes$ssi)] <- 0
  if ("a_star1" %in% names(modes)) modes$a_star1[!is.finite(modes$a_star1)] <- 0

  # back-compat aliases
  if (!("SSi" %in% names(modes)) && "ssi" %in% names(modes)) modes$SSi <- modes$ssi
  if (!("a_star" %in% names(modes)) && "a_star1" %in% names(modes)) modes$a_star <- modes$a_star1

# F) Modularity Q on final Top-20 (by cluster + global)
#    - Qw: weighted by WCD
#    - Qu: unweighted (each unique undirected edge = 1)
mod_tbl <- tryCatch(
  compute_modularity_by_cluster(modes, edges20_dir, eps = cfg$eps),
  error = function(e) NULL
)

if (is.data.frame(mod_tbl) && nrow(mod_tbl) > 0) {
  Qw_total <- attr(mod_tbl, "Qw_total")
  Qu_total <- attr(mod_tbl, "Qu_total")
  if (!is.finite(Qw_total)) Qw_total <- sum(mod_tbl$Qw_cluster, na.rm = TRUE)
  if (!is.finite(Qu_total)) Qu_total <- sum(mod_tbl$Qu_cluster, na.rm = TRUE)

  mm <- match(modes$carac, mod_tbl$carac)

  modes$Qw_cluster <- ifelse(is.na(mm), 0, mod_tbl$Qw_cluster[mm])
  modes$Qu_cluster <- ifelse(is.na(mm), 0, mod_tbl$Qu_cluster[mm])
  modes$Qw_cluster[!is.finite(modes$Qw_cluster)] <- 0
  modes$Qu_cluster[!is.finite(modes$Qu_cluster)] <- 0

  modes$Qw_total <- as.numeric(Qw_total); modes$Qw_total[!is.finite(modes$Qw_total)] <- 0
  modes$Qu_total <- as.numeric(Qu_total); modes$Qu_total[!is.finite(modes$Qu_total)] <- 0

  # backward-compat: keep old names as WEIGHTED Q
  modes$Q_cluster <- modes$Qw_cluster
  modes$Q_total   <- modes$Qw_total
} else {
  modes$Qw_cluster <- 0
  modes$Qu_cluster <- 0
  modes$Qw_total <- 0
  modes$Qu_total <- 0
  # backward-compat
  modes$Q_cluster <- 0
  modes$Q_total <- 0
}



  if (isTRUE(verbose)) {
    cat("\n[CHECK] Top-3 nodes20:\n")
    keep <- intersect(c("name","value","value2","carac","ssi","a_star1","SSi","a_star"), names(modes))
    print(utils::head(modes[, keep, drop = FALSE], 3))
  }


  # --- expose neighborC for ssplot label (#neighborC) --------------------------
  if (!("neighborC" %in% names(modes)) && "nn_cluster" %in% names(modes)) {
    modes$neighborC <- ifelse(is.na(modes$nn_cluster) | modes$nn_cluster=="", paste0("C", as.integer(modes$carac)), paste0("C", suppressWarnings(as.integer(gsub("^C","", toupper(as.character(modes$nn_cluster)))))))
  }
  if ("neighborC" %in% names(modes)) {
    modes$neighborC[!is.finite(modes$neighborC)] <- NA
  }

  list(modes = modes, data = edges20_dir, nodes_full = nodes_full, edges_fl = edges_fl,
       modularity_by_cluster = mod_tbl)
}
