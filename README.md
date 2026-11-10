# CowordKano

`cowordkano.R` is a Shiny application for examining authorship-position patterns with a Kano-style author plot. It combines publication-role counts and PT–LA collaboration structure to summarize leadership concentration, collaboration position, and structural-hole capacity.

## Study summary

### Evaluating Whether Middle-Author Participation Dilutes Leadership Concentration Among Five 2026 Physics Citation Laureates

Citation counts alone overlook authorship position, leadership, and collaboration structure. We evaluated mentoring-potential profiles of five physics scientists recognized as Clarivate Citation Laureates using career publications indexed in the Web of Science Core Collection. Kano plots assessed the Absolute Advantage Coefficient (AAC), structural-hole capacity (SHC), collaboration leadership index (CLI), middle-author rate (MAR), total unique articles (`n`), and unique first- or corresponding-author articles (`n*`).

Chihaya Adachi (MAR = 53.4%; AAC = 0.897), Stephen R. Forrest (46.7%; 0.861), and Mark E. Thompson (55.3%; 0.718) demonstrated relatively high leadership concentration. Nicola A. Spaldin’s substantially higher MAR (90.5%) corresponded to lower AAC (0.561). In Qikun Xue’s document profile, extensive middle authorship allowed Jinling Yu, who had more first- or corresponding-author articles, to replace Xue as the Top-1 Kano author; the resulting profile similarly showed high MAR (87.0%) and low AAC (0.400).

These comparisons suggest a descriptive inverse relationship between MAR and AAC: greater middle-author participation may dilute the focal scientist’s relative leadership concentration. However, the sample included only five selected scientists, authorship conventions may differ across physics subfields, and corresponding-authorship metadata may be incomplete. Therefore, this observed pattern is exploratory and cannot establish causality or confirm actual mentoring relationships.

## Input data

Upload a CSV file containing these two required columns:

| Column | Meaning |
| --- | --- |
| `PT` | First author |
| `LA` | Corresponding author |

Each valid row represents one publication. Rows with an empty `PT` or `LA` value are excluded.

By default, the app removes parenthetical text from names. For example, `Xue, Qikun(Hong Kong)` becomes `Xue, Qikun`. The sidebar option can retain parenthetical text when that distinction is needed.

## Kano plot

The plot displays the 20 authors with the highest author-role count. It uses:

- X-axis: first/last-author edge count.
- Y-axis: document/author-role count.
- Bubble area: SHC (%).
- Bubble colour: CLI category.
- Header cards: AAC, SHC, CLI, MAR/MAT, `n`, and `n*` for the Top-1 author.

The highlighted red bubble and callout identify the Top-1 Kano author.

## Metrics

| Metric | Interpretation |
| --- | --- |
| AAC | Absolute Advantage Coefficient calculated from the three highest author-role counts. |
| SHC | Structural-hole capacity, defined as `1 − Burt constraint` in the undirected PT–LA author-link network. |
| CLI | Collaboration Leadership Index: corresponding-author roles divided by first- plus corresponding-author roles, excluding self-pairs. |
| MAR/MAT | Middle-author rate/proportion: the share of retained publications in which an author appears in neither the first- nor corresponding-author role. |
| `n` | Number of retained valid publication rows. |
| `n*` | `first_n + corresponding_n + simple_n`, where `simple_n` counts rows for which PT and LA are the same author. |

## Run locally

Install the required packages in R:

```r
install.packages(c("shiny", "dplyr", "readr", "tidyr", "ggplot2", "ggrepel"))
```

Then run:

```r
shiny::runApp("cowordkano.R")
```

Or open `cowordkano.R` in RStudio and click **Run App**.

## Interpretation and limitations

CowordKano is a descriptive visualization and should not be used to infer causality, mentorship, contribution quality, or researcher value. Interpretation should account for incomplete corresponding-author metadata, field-specific authorship conventions, name disambiguation, publication coverage, and the selected population.

## License

Add a license appropriate for your repository before public release.
# Taipei MRT FLCA comparison

From this folder, run `shiny::runApp("TaipeiMRT.R")` after installing
`shiny` and `visNetwork`. The analysis uses `flca_core.R`; its standard
`run_flca()` path requires no additional R package. The FLCA candidate network
contains the highest-ranked 50 nodes, and the reduced view samples 20 to 23
nodes by cluster. Both views use the same original edge input and strongest-WCD
parent rule. Their node sets differ, so their edge counts and quality scores
normally differ.

The Taipei app preloads `nodes$value` from the July 2026 daily average station
entries in [ButTaiwan's Taipei MRT volume table](https://buttaiwan.github.io/trtcvol/)
(the `Taipei` sheet behind that page). The fixed, reviewable values are in
`TaipeiMRT_daily_average_2026_07.csv`. Six edge-list names have no matching
entry in that sheet: 動物園內站, 動物園站, 指南宮站, 貓空站, 貓空纜車, and 項埔.
Their default values retain the old edge-strength derivation (sum of adjacent
WCD). `value2` is still derived from edge degree. The Clear nodes button
empties the node box; a run with that box empty derives all node values from
the current edges. Clear edges empties only the edge box.

When `TaipeiMRT.csv` is uploaded as an edge-list CSV in `app.R`, that app now
uses the same `flca_core.R` result for its reduced and FLCA Process views. The
two-column A1/A2 file receives the same default WCD of 1.0 per edge as
`TaipeiMRT.R`. A plain CSV upload derives node values from the edges, so it
will now differ from the Taipei app's ridership defaults. Pasting the Taipei
app's node and edge boxes into `app.R` produces identical FLCA input,
Top-50 candidates, parent links, clusters, and 20-23 selected nodes. The
Full Network tab still displays the complete uploaded network.
The red **1. Optional WGCNA value-matrix CSV** upload is for repeated
observations and does not supply FLCA edges. The red **2. CSV/TXT/TSV: edge
list...** upload accepts `TaipeiMRT.csv`. Choosing **Cluster by Edge (weighted
Louvain)** adds a Louvain result on the full input network. The Quality table
reruns Louvain on the FLCA-selected nodes and their full induced edges for a
fair comparison. FLCA retains its strongest-link parent-chain memberships,
which can differ from Louvain even when both report the same cluster count.
All three FLCA node-value choices use the same sampling order: keep the Top-1
cluster first, prefer groups with a qualified leader (at least two direct
followers), then use small root components only if needed to reach the target.
The Top-1 node is retained even when its root component is small. The advanced
equal-WCD tie-break affects alternate analyses; ordinary node/edge inputs use
the shared FLCA core's higher-ranked-leader tie-break.
Both tree plots order clusters by their highest-ranked member and order nodes
within each cluster by descending value, breaking ties with ascending UTF-8
name using radix ordering. The parent links and red leader branches therefore
remain consistent across system collation settings.
Only the Top-1 cluster joins the graphical top stem with a solid red line.
Other clusters have grey dashed connectors to indicate that no FLCA link
joins them to that stem; their internal strongest-link branches remain solid.
The "Use demo text" input now reads the pasted node and edge boxes. Pasting
`TaipeiMRT.R`'s default node values and edge pairs there follows the same
shared FLCA path. If explicit node clusters are supplied, `app.R` preserves
those uploaded clusters as a separate mode.
At the bottom of `app.R`'s left panel, the Taipei MRT node and edge boxes are
preloaded with the same station values and 119 links. The node-values checkbox
is checked by default; uncheck it to derive values from the current edges.
Click **Run TaipeiMRT** to analyze those boxes and open the **FLCA Process**
tab when the run succeeds. Its tree uses dashed grey connectors from all
disconnected components to the display stem, with a solid red connector only
for the Top-1 component.

Click **Run U.S. airports demo** below the Taipei MRT button to analyze the
bundled `us_domestic_air_nodes.csv` (20 airports, including original node
values) and `us_domestic_air_edges.csv` (188 weighted routes). The button opens
the FLCA Process tab; the original route network stays available for network
quality metrics while FLCA uses strongest-link parent edges for its tree. When
all candidate airports form one FLCA cluster, the app retains the top 20
airports before dividing them into two display groups.

`TaipeiMRT_output/TaipeiMRT_network_comparison.csv` compares the candidate
and reduced views. `TaipeiMRT_FLCA_reduced_links.csv` is the selected FLCA
tree; `TaipeiMRT_FLCA_reduced_full_induced_edges.csv` contains every original
edge between the selected nodes. Both tree plots calculate SS, weighted Q,
and normalized weighted Q from the reduced strongest-link links. Top-1 SHC
uses the full induced original network. SS uses shortest-path distance
`1/WCD`; unreachable same-cluster pairs receive 1.25 times the longest finite
path and unreachable cross-cluster pairs receive 3 times that path. Singleton
SS is zero. The standalone `run_flca()` output retains its full-induced
quality metrics; the two apps' tree labels use the reduced-link convention
requested for their plots. Undefined metrics are returned as `NA` with a
diagnostic message.
In `app.R` Step 6, SSplot's per-cluster Qw and Qu are contributions to the
overall reduced-tree modularity, matching the tree's cluster labels. Their
sums equal the overall Qw and Qu.
For ordinary inputs in `app.R`, the Quality and Visual Quality tabs recompute
all comparable methods on the exact FLCA-selected 20-23 node set. Their Q and
SS rows use the same full original edge network induced by those nodes;
`n_nodes` and `eval_graph` show this scope. Quality also includes one FLCA
reduced-tree reference row, excluded from the fair ranking and Visual Quality
plot. The tree and SSplot show the same reduced strongest-link metrics.
In the FLCA Process tab, the SSplot's bars, cluster labels, and overall SS/Qw/Qu
are recalculated on the same selected reduced-link tree as its dendrogram.
The Quality tab separately evaluates the full induced original-edge network.
For AAC(value) inputs, FLCA Process also shows a vertical junction-colored
strongest-link tree beneath the horizontal dendrograph. It uses the same
selected nodes and reduced WCD links, with node sizes following `nodes$value`.
Optimal modularity is explicitly marked skipped with undefined scores; it is
not represented by a components fallback. Edge betweenness runs on the
selected graph using inverse WCD as distance.
AAC(count) in the app tree and Kano subtitle uses the number of distinct
incident links among selected nodes. For Lin's CCC inputs, AAC(value) uses
each selected node's sum of incident positive CCC weights. These AAC scores
do not replace the original node `value` or `value2` plot axes. The standalone
Taipei tree uses the same link-count source for AAC(count). Top-1 and Top-3
identities always follow descending `nodes$value`, with ascending name as the
tie-breaker. If CCC strengths of those value-ranked nodes are not descending,
AAC(value) is undefined rather than re-ranking the nodes by CCC strength.
The Step 6b Kano plot scales the displayed x and y positions independently,
so a small `value2` range (such as degree 2-4) remains readable beside large
station volumes. Axis tick labels and the node table retain the original
unscaled values; the Kano reference rings remain circular.

Step 6c displays the selected Step 3 strongest links as an interactive
`chorddiag` widget when that package is installed. Hovering a ribbon shows
its connection. Ribbon and sector widths use WCD on the same scale, so links
remain visible even when node totals are much larger than WCD. The PNG export
uses the same selected links and WCD scale.

The separate WGCNA Results tab uses repeated numeric observations, with rows
as observations and columns as nodes. Nurse-ratio and gene-cluster modes use
their source value columns; other modes can upload a matrix CSV with node names
in the first column and at least four numeric observation columns. Run FLCA
does not depend on WGCNA. Run WGCNA requires the optional R package `WGCNA`
(and its dependencies) installed in the app's R library. The tab reports module
membership, node connectivity, module sizes, adjacency-based SS/Qw/Qu, and a
weighted network plot, a TOM-based clustering tree, and a topological-overlap
heatmap. Under Topological overlap, an MDS map places nodes using `1 - TOM`
and colors them by WGCNA module; the top 20 value-ranked nodes are labeled.
A two-panel power diagnostic shows signed scale-free topology fit and mean
connectivity across candidate powers, marking the selected power. Both plots
reuse the same bicor similarity and module assignments as the run. The power
diagnostic is exploratory for small networks. Soft-threshold power is set
beside the button. The network displays
the 150 strongest adjacency links among up to 50 highest-value nodes, while
the tree and heatmap show up to 80 highest-value nodes; module detection uses
all nonconstant nodes in the selected value matrix. The default yellow module
color is displayed as dark purple for legibility on white. At the bottom of
WGCNA Results, an interactive chord diagram shows the two strongest WGCNA
adjacency links per node among up to 30 highest-value nodes. WGCNA adjacency
is undirected; the chord matrix stores each pair once in its upper triangle
only to avoid duplicate ribbons.
Below those WGCNA views, the tab also shows an FLCA reduced-edge network and
its strongest-link tree. The selector above both run buttons sets FLCA
`nodes$value` for repeated-value inputs. Its default, **Total score**, is the
sum of each node's observations. **Derived total** instead sums that node's
positive incident Lin's CCC edges for Run FLCA, or its incident
`|bicor|^power` adjacency for the companion FLCA results under Run WGCNA.
**Cluster by Edge** adds a separate weighted Louvain partition on the full
positive CCC or WGCNA adjacency network. It reports community memberships,
cluster summaries, weighted modularity, and silhouette on the full edge
network. The accompanying FLCA result still ranks leaders by total observation
score and follows strongest eligible links; Louvain never replaces FLCA.
The selected value determines leader rank; supplied node values in ordinary
node/edge inputs are preserved. FLCA uses the WGCNA weighted adjacency as WCD
and displays the Top-50 candidate
network before the separate FLCA Top-20/23 sampling step. Here
`WCD = |bicor|^power` is a similarity strength, so a larger value is a
stronger edge. The WGCNA dendrogram's `1 - TOM` is a distance and is never
used as FLCA WCD. Node value determines leader eligibility independently of
the edge-strength comparison. In the WGCNA Results companion FLCA plots,
clusters are displayed by the app's junction rule: nodes with at least two
direct followers are initial junctions. A displayed junction group with fewer
than three members first absorbs its strongest child-junction group; if none
exists, it merges upward along the unchanged strongest-link parent chain.
The top root remains a leader. The table
marks a displayed candidate leader only when it has at least two direct
followers and at least three members. These displayed FLCA
clusters can differ in number from WGCNA modules and from FLCA root-chain
clusters. The cluster summary beside the plots reports the actual count. A
third, horizontal FLCA junction dendrograph appears at the bottom of WGCNA
Results in the same style as the FLCA Process tree, with colored node groups,
WGCNA adjacency edge labels, dashed cluster dividers, reduced-tree SS/Q, and
Top-1 SHI from the full WGCNA adjacency.
The **FIFA 2026** tab is first in the main tab bar. Its buttons run the bundled
workbook or the online update, and the tab contains group performance,
prediction path, Round of 32 fixture check, SS/Q, and Golden Boot visuals.
Below the prediction path, a vertical knockout tree highlights the Top-1
champion or projected leader. The FLCA strongest-link dendrograph directly
below it reuses the same FIFA network view as the FLCA Process tab; its
network leader is distinct from the knockout champion.
The FIFA tab also shows the FLCA Process node-level Kano plot on the selected
FIFA network, with a PNG download button.
For FIFA tree display, selected clusters are ordered by their highest-value
member and labeled C1, C2, ...; members within each cluster descend by
`nodes$value`. The source workbook group IDs are not changed.
Launch the web app with `shiny::runApp("F:/taaforgae/zWoSPubmed/flcacompare")`.
`wgcna_module.R` is sourced by `app.R`; it is a helper file, not a separate
Shiny app entry point.

Run the dependency-free checks with `Rscript tests/test_flca_core.R`,
`Rscript tests/test_taipei_mrt_comparison.R`, and
`Rscript tests/test_taipei_mrt_pipeline.R` from this folder.

