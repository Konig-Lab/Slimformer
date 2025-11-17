# Slimformer – User Documentation

Slimformer is an interactive Shiny web application designed to annotate, classify, visualize, and explore GO-term–based gene set enrichment results.\
It supports outputs from *g:Profiler2*, custom enrichment tools, or any table containing GO terms, and (optional) gene intersections.

Slimformer provides:

* Automatic group and subcluster assignment

* High-level GO term category visualization

* Pie chart and scatter plot projections

* Export of annotated results and SVG visualizations

***

# 1. Getting Started

## 1.1 Upload Supported Files

Slimformer accepts:

* **CSV** (`.csv`)

* **TSV** (`.tsv`)

* **Excel** (`.xls`, `.xlsx`)

To upload:

1. Click **“Upload CSV, TSV or Excel File”**.
2. Select your enrichment file.
3. Slimformer automatically processes and annotates the data.

***

# 2. Configuring Input Columns

Slimformer needs to know where GO term IDs and (optionally) gene intersections are stored.

## 2.1 Term ID Column (Required)

Specify the column containing **GO term identifiers**.

Default for g:Profiler2: **term\_id**

Example:

```
term_id
GO:0006955
GO:0045087

```

## 2.2 Intersect Column (Optional)

If your file contains **gene lists** associated with each GO term, specify that column.

Default: **intersection**

Example:

```
intersection
STAT1,IRF1,MX1
CXCL10,IFIT3

```

Slimformer will use this column to:

* Compute refined **cluster names**

* Enable **gene-weighted pie charts**

If the column is missing, Slimformer disables gene weighting automatically.

***

# 3. Input Validation

The **Check Input** box reports whether required or optional columns are present.

Possible outputs:

* **✅ All specified columns are present!**

* **❌ Mandatory column missing:** **`<column>`**

* **⚠️ Optional column missing:** **`<column>`**

* **❌ Gene Weight disabled!**

This helps ensure that Slimformer can process your data correctly.

***

# 4. Visualization Panels

Slimformer provides three main panels accessible through tabs.

***

## 4.1 Data Table

Displays the annotated dataset with:

* All user-provided columns

* Added **Group**, **Class**, **Cluster**, and **Subcluster** annotations

* Hoverable truncated text for long entries

* Column visibility controls

* Scrollable, responsive layout

Use this panel to inspect your enriched and cleaned data.

***

## 4.2 Pie Chart

Shows the distribution of GO terms across **18 biological categories**, each assigned a specific color.

### Features:

* Clean, interpretable overview of enrichment categories

* Interactive hover labels

* Optional gene-weighted slice sizes

* Colors aligned with Slimformer’s predefined palette

### Gene Weight Option

* If **enabled**, slice sizes reflect the weighted contribution of each term using gene intersections.

* If the intersection column is missing, the checkbox is disabled.

***

## 4.3 Scatter Plot

Displays each GO term as a point on a **2D SOM-based map**, allowing intuitive comparison of biological processes.

### Features:

* Category-based coloring

* Optional **subcluster outlines** derived from pretrained SOM maps

* Adjustable outline width

* Rich hover tooltips, including:

  * Category (Class)

  * Term name

  * Subcluster (if enabled)

***

# 5. Downloading Results

Slimformer provides two export options.

***

## 5.1 Download Annotated TSV

Click **“Download Annotated TSV”** to save:

* Original data

* Category (Group), Class, Cluster, Subcluster annotations

* Fully mapped and standardized term IDs

Filename format:

```

SlimformerAnnotated_<yourfile>_YYYY-MM-DD.tsv

```

***

## 5.2 Download SVG Plots

Publication-quality vector graphics.

### How to download:

1. Open **Pie Chart** or **Scatter Plot** tab.
2. Click **“Download SVG”**.

Slimformer automatically:

* Collects Plotly SVG layers

* Assembles them into a single SVG

* Assigns a descriptive filename

Examples:

```

SlimformerPieChart_2025-05-12_14_21.svg
SlimformerScatterPlot_2025-05-12_14_22.svg

```

***

# 6. Annotation Pipeline Overview

Slimformer processes uploaded data through these steps:

1. **Remove previous columns**

   * `Group`, `QC`, `Subcluster` (to avoid conflicts)

2. **Join reference tables**

   * Category (Group) mapping

   * Class annotations

   * SOM coordinates

3. **Compute subcluster names**\
   Based on term set sizes

4. **Rename Subclusters**\
   Using inferred cluster representative terms.

This ensures consistent annotation across datasets.

***

<br />

# 7. Troubleshooting

### Pie Chart is empty

* Only one category present

* Group mapping failed due to non-standard term IDs

### Scatter Plot shows fewer points

* Only GO terms with pretrained SOM coordinates can be plotted

### Gene Weights checkbox remains disabled

* Intersection column not found

### Annotated file has fewer rows

* Duplicate term IDs in input cause merging behavior

***

# 8. Citation

If you use Slimformer in research or publications, please cite:

*To be announced (publication in pre-proof)*

# 9. Example Data

Provided are two example data sets.

[Upregulated gene sets](https://github.com/Konig-Lab/Slimformer/blob/main/RSV_Example/upregulated_gene_sets.tsv) and [downregulated gene sets](https://github.com/Konig-Lab/Slimformer/blob/main/RSV_Example/downregulated_gene_sets.tsv).

RSV example from:
Xu X, Qiao D, Mann M, Garofalo RP et al. Respiratory Syncytial Virus Infection Induces Chromatin Remodeling to Activate Growth Factor and Extracellular Matrix Secretion Pathways. Viruses 2020 Jul 26;12(8). PMID: 32722537
