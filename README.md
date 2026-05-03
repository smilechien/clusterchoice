ClusterChoice

A web-based tool for interpretable clustering and algorithm comparison in bibliometric networks

🔍 Overview

ClusterChoice is an R Shiny application designed for clustering analysis in bibliometric and network data. It focuses on improving interpretability, computational efficiency, and reproducibility by integrating:

Top-N node filtering (default: 100) to reduce complexity
FLCA (Following Leader Clustering Algorithm) for leader–follower structures
Multi-algorithm comparison (11 methods)
Standardized evaluation metrics (Modularity Q, Silhouette Score, ARI, NMI)
Interactive and exportable visualizations
🚀 Key Features
📉 Complexity reduction: Pre-filter nodes before clustering
🧠 Interpretable clustering: Leader–follower structure (FLCA)
⚖️ Algorithm comparison: Louvain, Walktrap, Infomap, Label Propagation, etc.
📊 Quality evaluation:
Modularity (Q)
Silhouette score (SS)
Adjusted Rand Index (ARI)
Normalized Mutual Information (NMI)
🎨 Visualization outputs:
Network graphs
SSplots
Kano plots
Sankey diagrams
📦 Export:
PNG figures
Node/edge tables (CSV)
Clustering summaries
📥 Supported Input Formats

ClusterChoice accepts multiple data types:

Co-word occurrence (long format)
Example: DocumentID, Term
Document-term matrix (wide format)
Binary or weighted matrix
Node–edge network data
Columns: Leader, follower, WCD
⚙️ Workflow

ClusterChoice follows a 5-step pipeline:

Data acquisition & normalization
Node importance ranking & top-N filtering
Co-word network construction
Clustering & comparative evaluation
Visualization & export
🧪 Example Results
Conventional algorithms:
Modularity ≈ 0.65
Silhouette ≈ 0.00
FLCA (ClusterChoice):
Modularity ≈ 0.80
Silhouette ≈ 0.88

👉 Indicates stronger cohesion and clearer cluster structure.

🖥️ Installation
Requirements
R (≥ 4.2 recommended)
R packages:
install.packages(c(
  "shiny", "DT", "igraph", "dplyr", "tibble",
  "readr", "visNetwork", "RColorBrewer",
  "ggplot2", "ggrepel", "circlize"
))
▶️ Run the App
shiny::runApp("path/to/ClusterChoice")

Or deploy via shinyapps.io:

rsconnect::deployApp()
🌐 Demo

You can test the app using:

Built-in demo dataset
Bibliometric datasets (e.g., SoftwareX from WoSCC)
📊 Evaluation Metrics
Metric	Purpose
Q (Modularity)	Measures cluster partition strength
SS (Silhouette)	Measures cohesion and separation
ARI	Agreement between clustering results
NMI	Shared information between clusterings
⚠️ Limitations
Top-N filtering may omit low-frequency but meaningful nodes
FLCA assumes hierarchical structure (not suitable for all networks)
Static analysis (no temporal modeling yet)
Performance depends on input data quality
🧩 Use Cases
Bibliometric analysis
Author collaboration networks
Co-word analysis
Knowledge structure mapping
Algorithm benchmarking
📖 Citation

If you use ClusterChoice, please cite:

ClusterChoice: A web-based tool for clustering comparison and interpretable network analysis in bibliometrics. (SoftwareX, under review)

👨‍💻 Author

Developed by Smile Chien

📜 License

MIT License (recommended — adjust if needed)

🔗 Repository Structure
ClusterChoice/
│── app.R
│── renderSSplot.R
│── sankey.R
│── kano.R
│── README.md
│── data/
│── www/
💡 Future Work
Dynamic/temporal network analysis
Automatic cluster naming
Integration with OpenAI semantic extraction
Enhanced interactive dashboards
⭐ Why ClusterChoice?

Unlike traditional tools (e.g., VOSviewer, CiteSpace, bibliometrix), ClusterChoice:

✔ Reduces clutter before analysis
✔ Enforces interpretable cluster structure
✔ Compares multiple algorithms objectively
✔ Produces publication-ready visuals
