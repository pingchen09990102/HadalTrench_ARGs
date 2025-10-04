library(networkD3)
library(htmlwidgets)
library(tidyverse)

setwd("$HOME/HadalTrench_ARGs/Fig5a")
data <- read.csv("Fig1a.sourceData.csv", header = 1)

links <- data %>%
  transmute(source = Hadal_trench, target = ARG_host, edge) %>%
  bind_rows(data %>% transmute(source = ARG_host, target = ARG_type, edge)) %>%
  group_by(source, target) %>%
  summarise(Value = sum(edge), .group = "drop")

links <- links %>%
  mutate(
    source = as.character(source),
    target = as.character(target)
  )

nodes <- data.frame(
  name = unique(c(links$source, links$target))
)


links <- links %>%
  mutate(
    source = match(source, nodes$name) - 1,
    target = match(target, nodes$name) - 1
  )

# Manually specify the colors of some nodes. 
fixed_colors <- list(
  "4484-113" = "#83B1D0",
  "Acidobacteriota" = "#8DD3C7",
  "Actinomycetota" = "#92A9C4",
  "Aerophobota" = "#99B1BC",
  "Armatimonadota" = "#A0DAC3",
  "Bacillota" = "#A7A1B3",
  "Bacteroidota" = "#AEB2A8",
  "Bdellovibrionota" = "#B4E2C0",
  "CG03" = "#B6DB68",
  "Chloroflexota" = "#BCDB78",
  "Desulfobacterota" = "#BD98A2",
  "Desulfobacterota_B" = "#C2BFD7",
  "Desulfobacterota_D" = "#C3D467",
  "GCA-001730085" = "#C4B294",
  "Gemmatimonadota" = "#C4B4CF",
  "Halobacteriota" = "#C8D88E",
  "Hydrogenedentota" = "#C8EABC",
  "JADFOP01" = "#CEAABD",
  "JdFR-76" = "#CECBD0",
  "Krumholzibacteriota" = "#D0CD66",
  "KSB1" = "#D38F91",
  "Latescibacterota" = "#D5D5A3",
  "Marinisomatota" = "#D99FAB",
  "Methylomirabilota" = "#D9D7C9",
  "Myxococcota" = "#DAB381",
  "Myxococcota_A" = "#DCF1B9",
  "Nanoarchaeota" = "#DDC564",
  "Nitrospinota" = "#E2D2B9",
  "Nitrospirota" = "#E49599",
  "unclassified" = "#E4E3C2",
  "Omnitrophota" = "#E88780",
  "Patescibacteria" = "#EABE63",
  "Planctomycetota" = "#EE8B86",
  "Poribacteria" = "#EFCFCF",
  "Pseudomonadota" = "#F0B36D",
  "SAR324" = "#F0EFBB",
  "SM23-31" = "#F0F9B5",
  "Thermoproteota" = "#F7B762",
  "UBA8248" = "#F98174",
  "Verrucomicrobiota" = "#FBFBB4",
  "Zixibacteria" = "#FCCDE5",
  "MT" = "#FB8072",
  "DT" = "#FDB462",
  "KT" = "#FFED6F",
  "MST" = "#FCCDE5",
  "NBT" = "#8DD3C7",
  "Yap" = "#BC80BD",
  "novobiocin" = "#8DD3C7",
  "bacitracin" = "#FFFFB3",
  "beta-lactam" = "#BEBADA",
  "MLS" = "#FB8072",
  "multidrug" = "#80B1D3",
  "mupirocin" = "#FDB462",
  "tetracycline" = "#B3DE69",
  "trimethoprim" = "#FCCDE5",
  "polymyxin" = "#D9D9D9",
  "vancomycin" = "#BC80BD",
  "sulfonamide" = "#CCEBC5",
  "chloramphenicol" = "#FFED6F",
  "aminoglycoside" = "#E41A1C",
  "fosfomycin" = "#377EB8",
  "puromycin" = "#4DAF4A",
  "pleuromutilin tiamulin" = "#FF7F00",
  "rifamycin" = "#FFFF33",
  "other peptide antibiotics" = "#A65628",
  "antibacterial fatty acid" = "#F781BF",
  "bicyclomycin" = "#999999"
  
)

# Dynamic color logic
colourScale <- JS(sprintf(
  'd3.scaleOrdinal()
    .domain(%s)
    .range(%s.concat(d3.schemeCategory10))',
  jsonlite::toJSON(names(fixed_colors), auto_unbox = TRUE),   # Fixed color nodes
  jsonlite::toJSON(unname(fixed_colors), auto_unbox = TRUE)   # Fixed color
))

customJS <- JS("
  function(el) {
    var svg = d3.select(el).select('svg');

    // 先获取所有节点的颜色映射表
    var nodeColors = {};
    d3.select(el).selectAll('.node').each(function(d, i) {
      var nodeName = d.name;
      var color = d3.select(this).select('rect').style('fill');
      nodeColors[nodeName] = color;
    });

    // 处理连线颜色，确保所有连线颜色与来源节点颜色一致
    d3.select(el).selectAll('.link')
      .each(function(d, i) {
        var sourceColor = nodeColors[d.source.name] || '#cccccc'; // 获取来源节点颜色

        // 应用颜色到连线
        d3.select(this)
          .style('stroke', sourceColor) // 让连线颜色与来源节点一致
          .style('stroke-opacity', 1.0);  // 让连线颜色更清晰
      });
  }
")


# Draw a sankey diagram
sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "Value",
  NodeID = "name",
  fontSize = 15,
  fontFamily = "Arial",
  nodeWidth = 12,
  nodePadding = 8,
  margin = list(top = 70, right = 100, bottom = 100, left = 120),
  colourScale = colourScale,
  width = 700,  
  height = 1200  
) %>%
  htmlwidgets::prependContent(
    htmltools::tags$style("
        /* 右移文本 - 修改为右侧对齐 */
      .node text {
        text-anchor: start !important;  /* 改为end实现右侧对齐 */
        transform: translateX(18px); /* 改为负值向左偏移 */
        fill: black !important;
     }
      
      /* 设置连线默认颜色和透明度 */
      .link {
        stroke-opacity: 0.6 !important;
      }
    ")
  ) %>%
  htmlwidgets::onRender(customJS)

sankey

# Copy the URL of the html window you get
# load webshot library
library(webshot)

#install phantom:
#webshot::install_phantomjs(force = TRUE) , vwidth = 1200,vheight=1200
# Make a webshot in pdf : high quality but can not choose printed zone
saveNetwork(sankey,file = "Sankey_plot.html",selfcontained = TRUE)
webshot::webshot("Sankey_plot.html", file="Sankey_plot.pdf", 
                 delay = 2,zoom = 1, vwidth = 700, vheight=1200)




