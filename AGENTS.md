# FLCA Algorithm Instructions

## Scope

Implement the Follower-Leading Clustering Algorithm (FLCA) in R for weighted
network clustering, leader-follower identification, strongest-link tree
construction, Top-20/23 node sampling, and network quality evaluation.

Preserve these methodological distinctions:

1. Node values and deterministic rank determine leadership.
2. The strongest WCD among eligible higher-ranked neighbors determines each
   follower's parent.
3. Strongest-link parent chains determine FLCA clusters and the tree.
4. Highest node value rank determines sampling priority; 20 is a target, and
   the normal selected count is 20 to 23.
5. Silhouette Score (SS), weighted modularity Q, and Top-1 Structural Hole
   Constraint (SHC) use the **full induced original edge network**, not the
   reduced FLCA tree.

Do not replace FLCA with Louvain, Leiden, K-means, or conventional hierarchical
clustering. Document ambiguous implementation choices instead of silently
substituting another method.

## Inputs and validation

`nodes` is a data frame with unique, nonmissing `name` and finite numeric
`value`. Initialize `decimal <- value`; use `cluster` only as a temporary
cluster ID and `carac` as the final cluster ID. Never change `value`.

`data` is a data frame with `Leader`, `Follower`, and finite numeric `WCD`.
Initially the endpoint names have no directional meaning. Every endpoint must
exist in `nodes$name`. Remove self-links. Consolidate duplicate undirected
pairs by retaining maximum WCD, unless a project-specific rule explicitly
supersedes this. Preserve the full original edge data for quality metrics.

## Ranking and Top-50 candidate network

1. Sort node names ascending, then sort values descending with ascending name
   as the tie-breaker. Use a deterministic sort, such as radix ordering.
2. Record integer rank `1, ..., n`, with rank 1 highest. Compute
   `decimal = round(value, 3) + ((n - rank + 1) / n) * 0.0001` without changing
   `value`. Integer rank is authoritative whenever floating-point adjustment
   cannot uniquely or correctly distinguish nodes. Do not assume four decimal
   places suffice for an arbitrary network.
3. Initialize `cluster <- seq_len(n)` in rank order.
4. Keep the highest-ranked `min(50, n)` nodes (or the caller's configured
   `top50`) and edges whose endpoints are both retained. Keep the full edge
   dataset separately.

## Leaders, strongest links, and clusters

1. Orient every candidate edge from higher-ranked `Leader` to lower-ranked
   `Follower`. Preserve WCD. Equal authoritative ranks must never occur; the
   directed graph must be acyclic.
2. For each follower, select exactly one eligible higher-ranked neighbor with
   maximum WCD. Break equal-WCD ties by choosing the higher-ranked leader.
   Nodes without eligible higher-ranked neighbors remain roots. Process
   followers from lowest to highest rank when updating temporary clusters.
   Save the selected parent links as `flca_tree_edges`; never overwrite the
   original full edge data.
3. A candidate leader has at least two **direct** followers in
   `flca_tree_edges`. Keep candidate leaders identifiable even after cluster
   propagation.
4. Trace every parent chain to a stable ancestor/root to assign `carac`.
   Detect cycles, invalid parents, and disconnected references. Every node
   receives exactly one final cluster.
5. Renumber final clusters sequentially `1, ..., k`, ordered by their
   highest-ranked members. Preserve node rank.

## Top-20/23 sampling

Select the cluster containing rank 1 first. Keep its highest-ranked one to
four members: retain its sole node if it is a singleton, and cap a larger
cluster at its four highest-ranked members. Never discard the Top-1 node.

Then consider other clusters by the rank of their highest-ranked member.
Prefer clusters supplying three or four nodes, including their leader;
preserve each eligible cluster in full. If still below the target, include
smaller one- or two-node clusters in rank order. Do not skip a higher-ranked
eligible cluster to obtain a count closer to exactly 20. Add the next cluster
in full and stop as soon as the cumulative count reaches or exceeds 20.
Thus, a normal final size can be 20, 21, 22, or 23. When fewer than 20
eligible nodes are available, return the actual count without duplication or
fabrication. Never truncate the final cluster merely to hit 20.

If the selected network has only one cluster and at least two nodes, sort
nodes by descending value and ascending name, then split into two
approximately equal groups; the higher-ranked group receives the extra node
when the count is odd. A one-node network remains one cluster, and its
two-cluster quality assessment is not applicable.

Remove temporary `cluster`, `decimal`, rank, and tie-break columns from the
final nodes. Preserve `name`, original `value`, and `carac`.

## Quality metrics on the full induced network

After sampling, retain every original full-network edge whose endpoints are
both selected. Use this induced network for all metrics below.

* **SS:** Document a dissimilarity transformation because WCD is connection
  strength. A normalized similarity in `[0,1]` can use `d = 1 - s`.
  Document how missing pairs in incomplete networks receive distances, such
  as weighted shortest paths or a complete dissimilarity matrix. Report
  individual SS, mean SS per cluster, and overall mean SS. Define singleton
  SS as zero.
* **Weighted modularity:** Use WCD in the undirected weighted adjacency
  matrix and final `carac` memberships. Compute
  `Q = (1/(2m)) * sum_ij (A_ij - k_i*k_j/(2m)) * I(c_i == c_j)`, where `m`
  is total edge weight and `k_i` is weighted node strength. If normalized Q
  is requested, calculate `Q* = Q/(1 - 1/k)` only when `k > 1`.
* **Top-1 SHC:** Compute the structural constraint of the highest-ranked
  selected node on the full induced weighted network; `SHC = 1 - constraint`.
  Report its name, value, constraint, and SHC. If Top-1 is isolated, SHC is
  undefined unless an explicit project convention says otherwise.

Return undefined metrics as `NA` and explain each in diagnostics. Do not
silently invent missing WCD values or change a metric's mathematical
definition.

## R implementation and outputs

Use modular functions for validation, ranking, Top-50 filtering, orientation,
strongest-link reduction, candidate leader identification, cluster
propagation, renumbering, sampling, single-cluster splitting, SS, Q, and SHC.
Expose:

```r
run_flca(nodes, data, top50 = 50, target_n = 20,
         max_cluster_size = 4, seed = NULL)
```

Standard FLCA is deterministic. `seed` may be reserved for simulations but
must not influence standard results. Return a named list with `nodes`,
`edges` (full selected induced network), `tree_edges`, `clusters`,
`silhouette`, `modularity`, `top1_shc`, and `diagnostics`. Final nodes require
`name`, `value`, `carac`; both edge tables require `Leader`, `Follower`,
`WCD`. Every non-root tree node has at most one parent. The cluster summary
requires `cluster_id`, `leader_name`, `member_count`, `highest_value`, and
`member_names`. The metric results require `mean_silhouette`,
`modularity_Q`, `normalized_Q`, `top1_constraint`, and `top1_SHC`.

Construct the FLCA tree visualization from strongest-link relationships,
showing member nodes and WCD edge labels.

## Validation

Automate tests for all of the following:

1. Unique deterministic ranking and alphabetical name tie-breaking.
2. Original value preservation and correct leader-follower orientation.
3. Maximum-WCD parent selection, deterministic WCD ties, and no tree cycles.
4. Correct cluster propagation and sequential final IDs.
5. Top-1 retention, including singleton and larger-than-four Top-1 clusters.
6. Top-20, Top-21, Top-22, and Top-23 outcomes; highest-value priority over
   exact sample size; complete eligible cluster preservation.
7. One-cluster splitting and networks with fewer than 20 nodes.
8. SS, weighted Q, and Top-1 SHC on the **full** induced network.
9. Temporary-column removal and repeated-run reproducibility.

## Execution order

Validate inputs → rank nodes → initialize temporary clusters → select Top-50
candidate network → orient edges → choose each follower's strongest eligible
leader → identify candidate leaders → trace and propagate clusters →
renumber → sample Top-20/23 → apply one-cluster exception → construct the
full induced selected network → compute SS, Q/Q*, and Top-1 SHC → remove
temporary columns → return results.
