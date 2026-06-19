ClusterChoice 2.0

**A web-based R/Shiny tool for interpretable clustering, algorithm comparison, and reproducible network demonstrations**

ClusterChoice is an R/Shiny application for network-based clustering analysis. It was originally designed for bibliometric co-word, document-term, and node–edge data, and has been updated to include reproducible sports-network demonstrations using **FIFA 2026 online results** and **NBA 2025–2026 workbook data**.

The updated app focuses on:

- interpretable leader–follower clustering using FLCA;
- preservation of user-supplied cluster labels;
- reproducible demo datasets;
- live online FIFA result updating from Wikipedia;
- bundled FIFA and NBA workbook demonstrations;
- visual comparison of clustering quality using modularity Q and silhouette score.

---

## 🔍 Overview

ClusterChoice integrates the following functions into one browser-based workflow:

- **Top-N node filtering** to reduce dense network complexity before clustering.
- **FLCA**: Following Leader Clustering Algorithm for interpretable leader–follower structures.
- **MA/SIL module**: major-sampling and silhouette-based visual diagnostics.
- **Multi-algorithm comparison** across common graph-clustering methods.
- **Quality metrics** including modularity Q, silhouette score, ARI, and NMI.
- **Interactive and exportable visualizations**, including full networks, FLCA-reduced networks, SSplots, Kano plots, Sankey-style outputs, ranking tables, and downloadable node/edge data.

---

## 🆕 What is new in ClusterChoice v2.0?

Version 2.0 extends the original bibliometric workflow by adding **dynamic and bundled demonstration data paths**:

1. **Default co-word demo** using `dataset1.csv`.
2. **Node–edge XLSX upload**, with strict preservation of original `cluster` or `carac` labels.
3. **Co-word CSV upload**, converting occurrence or document-term data into a weighted network.
4. **Bundled FIFA 2026 XLSX demo**, using `fifa_2026_updated_nodes_edges.xlsx`.
5. **Live FIFA 2026 online update**, retrieving the latest completed group-stage match results from Wikipedia, rebuilding nodes and edges, and running analysis.
6. **NBA 2025–2026 demo**, using `nba.xlsx`.

---

## ⚽ FIFA 2026 demo

ClusterChoice v2.0 includes two FIFA workflows.

### 1. Run FIFA 2026 demo from bundled XLSX

This mode reads the local workbook:

```text
fifa_2026_updated_nodes_edges.xlsx
```

It is stable and reproducible. The network is computed from the workbook already stored with the app.

### 2. Update FIFA 2026 online and run

This mode performs a live online update. When the user presses **Update FIFA 2026 online and run**, the app:

1. fetches current Wikipedia pages for FIFA 2026 Groups A–L;
2. extracts completed match results;
3. converts match outcomes into directed weighted edges;
4. rebuilds team-level node data;
5. validates the generated network;
6. saves an updated `fifa_2026_updated_nodes_edges.xlsx`;
7. runs the full ClusterChoice analysis workflow.

The online update reads the following Wikipedia pages:

- [Group A](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_A)
- [Group B](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_B)
- [Group C](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_C)
- [Group D](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_D)
- [Group E](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_E)
- [Group F](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_F)
- [Group G](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_G)
- [Group H](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_H)
- [Group I](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_I)
- [Group J](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_J)
- [Group K](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_K)
- [Group L](https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_Group_L)

The app displays a progress scale during this operation so users can see which group page is being fetched and parsed.

---

## ⚽ FIFA network coding rule

In the FIFA workflow:

| Field | Meaning |
|---|---|
| `name` | Team name |
| `value` | Accumulated point-like performance |
| `value2` | Number of completed games played |
| `cluster` | FIFA group membership, Group A = 1 through Group L = 12 |
| `Leader` / `term1` | Winner in a win/loss match; team 1 in a draw |
| `follower` / `term2` | Loser in a win/loss match; team 2 in a draw |
| `WCD` | Directed weighted match result |

Match rules:

```text
Win/loss: winner -> loser, WCD = 3
Draw: team1 -> team2, WCD = 1
```

Node value rule:

```text
value = SUM(WCD where team appears in term1)
      + SUM(WCD where team appears in term2 and WCD == 1)
```

Node value2 rule:

```text
value2 = number of completed matches played
```

---

## 🏀 NBA 2025–2026 demo

ClusterChoice v2.0 also includes a bundled NBA demonstration using:

```text
nba.xlsx
```

The NBA demo allows users to test the same node–edge workflow on a different sports-network dataset. This helps demonstrate that ClusterChoice is not limited to bibliometric data. The NBA workbook should contain node and edge sheets compatible with the app’s network format.

Recommended NBA workbook sheets:

```text
nodes: name, value, value2, cluster or carac
edges: Leader, follower, WCD
```

or:

```text
nodes: name, value, value2, cluster
edges: term1, term2, WCD
```

---

## 📥 Demo data files

The app provides downloadable demo files for users:

| File | Purpose |
|---|---|
| `dataset1.csv` | Default co-word demo |
| `fifa_2026_updated_nodes_edges.xlsx` | FIFA 2026 bundled and online-updated workbook |
| `nba.xlsx` | NBA 2025–2026 bundled demo workbook |

These files should be stored in the same folder as `app.R`, or in a data folder if the app paths are modified accordingly.

---

## 📥 Supported input formats

ClusterChoice accepts several data types.

### 1. Co-word occurrence CSV

Long format example:

```text
DocumentID,Term
D1,machine learning
D1,python
D2,network analysis
D2,bibliometrics
```

### 2. Document-term matrix

Wide matrix example:

```text
DocumentID,machine learning,python,bibliometrics
D1,1,1,0
D2,0,1,1
```

### 3. Node–edge network data

Nodes:

```text
name,value,value2,cluster
Mexico,3,1,1
South Korea,3,1,1
Switzerland,4,2,2
```

Edges:

```text
Leader,follower,WCD
Mexico,South Korea,3
Switzerland,Qatar,3
```

Alternative edge headers are also accepted:

```text
term1,term2,WCD
```

---

## ⚙️ Workflow

ClusterChoice follows a six-stage analytical pipeline:

1. **Data acquisition and normalization**
2. **Node and edge construction**
3. **Top-node filtering**, when applicable
4. **FLCA / MA / SIL analysis**
5. **Algorithm comparison**
6. **Visualization and export**

The main output tabs include:

- **Data**
- **Full Network**
- **FLCA Reduced**
- **FLCA Process**
- **Memberships**
- **Quality**
- **Ranking**
- **Visual Quality**
- **Parameters**
- **ReadMe**
- **Deploy Notes**

---

## 📊 Evaluation metrics

| Metric | Purpose |
|---|---|
| Q, modularity | Measures cluster partition strength |
| SS, silhouette score | Measures cohesion and separation |
| ARI, adjusted Rand index | Measures agreement between clustering results |
| NMI, normalized mutual information | Measures shared information between clustering solutions |
| AAC, absolute advantage coefficient | Measures dominance concentration among leading nodes or clusters |

The **Visual Quality** tab plots:

```text
x-axis = modularity Q
y-axis = mean silhouette score
```

The **FLCA Process** tab provides:

- stepwise network inspection;
- FLCA-reduced leader–follower structure;
- SSplot;
- Kano plot;
- node and edge downloads;
- cluster summary tables.

---

## 🚀 Key features

- 📉 **Complexity reduction**: Pre-filter top nodes before dense network construction.
- 🧠 **Interpretable clustering**: FLCA produces leader–follower structures.
- 🧩 **Cluster-label preservation**: Uploaded `cluster` or `carac` values are preserved when supplied.
- ⚖️ **Algorithm comparison**: Louvain, Walktrap, Infomap, label propagation, edge betweenness, fast greedy, components, and FLCA-based variants.
- 📊 **Quality evaluation**: Q, SS, ARI, NMI, AAC.
- 🎨 **Visualization outputs**: Full network, FLCA-reduced network, SSplot, Kano plot, Sankey-style output.
- 📦 **Export**: PNG figures, node/edge tables, group summaries, and updated XLSX files.
- 🌐 **Live FIFA update**: Online extraction from Wikipedia Group A–L pages.
- 🏀 **Sports-network demos**: FIFA 2026 and NBA 2025–2026.

---

## 🖥️ Installation requirements

R version:

```text
R >= 4.2 recommended
```

Core packages:

```r
install.packages(c(
  "shiny",
  "DT",
  "igraph",
  "dplyr",
  "tibble",
  "readr",
  "visNetwork",
  "RColorBrewer",
  "ggplot2",
  "ggrepel",
  "circlize"
), type = "binary")
```

Additional packages for XLSX and online FIFA updating:

```r
install.packages(c(
  "readxl",
  "openxlsx",
  "httr2",
  "rvest",
  "xml2",
  "stringr",
  "tidyr",
  "purrr"
), type = "binary")
```

---

## ▶️ Run the app

```r
shiny::runApp("path/to/ClusterChoice")
```

Example:

```r
shiny::runApp("F:/taaforgae/zWoSPubmed/flcacompare")
```

Or open the folder in RStudio and run:

```r
shiny::runApp()
```

---

## 🌐 Deploy

The app can be deployed through shinyapps.io:

```r
install.packages("rsconnect")
library(rsconnect)

rsconnect::deployApp()
```

When deploying, make sure these files are included:

```text
app.R
dataset1.csv
fifa_2026_updated_nodes_edges.xlsx
nba.xlsx
```

If online FIFA updating is enabled on the deployed server, the server must allow internet access to Wikipedia.

---

## 🧪 Example results

In the original bibliometric demonstration, conventional algorithms showed acceptable modularity but weak silhouette values, while FLCA improved both interpretability and silhouette-based cohesion.

Example pattern:

| Method type | Modularity Q | Mean silhouette SS |
|---|---:|---:|
| Conventional clustering | approximately 0.65 | near 0.00 |
| Components-based methods | approximately 0.65 | approximately 0.60 |
| FLCA | approximately 0.80 | approximately 0.78–0.88 |

These results suggest that modularity alone may not be sufficient for judging network-clustering quality. ClusterChoice therefore reports both global partition quality and local cohesion/separation.

---

## 🧩 Use cases

- Bibliometric co-word analysis
- Author collaboration networks
- Keyword co-occurrence networks
- Knowledge-structure mapping
- Algorithm benchmarking
- Sports-result network analysis
- FIFA 2026 group-performance networks
- NBA 2025–2026 workbook-based demonstrations
- Teaching examples for graph clustering and network visualization

---

## ⚠️ Limitations

- Top-N filtering may omit low-frequency but meaningful nodes.
- FLCA assumes a leader–follower structure, which may not suit all networks.
- Online FIFA updating depends on the availability and structure of Wikipedia pages.
- If Wikipedia changes its HTML structure, the online parser may need revision.
- Live online results require internet access and the required web-scraping packages.
- NBA demo results depend on the structure and quality of the supplied `nba.xlsx` workbook.
- Sports examples are demonstration datasets and should be interpreted as network-analysis examples, not official standings.

---

## 📁 Repository structure

```text
ClusterChoice/
│── app.R
│── README.md
│── dataset1.csv
│── fifa_2026_updated_nodes_edges.xlsx
│── nba.xlsx
│── renderSSplot.R
│── kano.R
│── sankey.R
│── data/
│── www/
```

---

## 📖 Citation

If you use ClusterChoice, please cite:

```text
ClusterChoice: A web-based tool for clustering comparison and interpretable network analysis in bibliometrics. SoftwareX, under review.
```

For the v2.0 update, please cite:

```text
ClusterChoice v2.0: Live FIFA 2026 online results as reproducible network demos for interpretable FLCA-based clustering. SoftwareX update manuscript, under preparation.
```

---

## 👨‍💻 Author

Developed by **Smile Chien** and collaborators.

Support email:

```text
rasch.smile@gmail.com
```

---

## 📜 License

MIT License, unless otherwise specified in the repository.

---

## ⭐ Why ClusterChoice?

Unlike traditional tools that primarily visualize dense networks after construction, ClusterChoice emphasizes interpretable and reproducible clustering by:

- reducing clutter before analysis;
- preserving original cluster labels when supplied;
- enforcing interpretable leader–follower structures;
- comparing multiple algorithms objectively;
- reporting both Q and SS;
- producing publication-ready visuals;
- supporting live and bundled reproducible demos, including FIFA 2026 and NBA 2025–2026.

✔ Enforces interpretable cluster structure
✔ Compares multiple algorithms objectively
✔ Produces publication-ready visuals
