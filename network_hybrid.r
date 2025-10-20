library(visNetwork)
library(readr)
library(dplyr)
library(htmlwidgets)
# library(webshot2)
library(igraph)

# 加载收集的杂交种数据
data <- read_csv("Sobralia_hybrid.csv")
# 创建nodes数据框
nodes <- data.frame(
  id = unique(c(data$Grex, data$Parentage)),
  label = unique(c(data$Grex, data$Parentage))
)
nodes$id <- 1:nrow(nodes)
nodes$title <- nodes$label
# 创建edges数据框
# 母本edges数据框
seed_edges <- data %>%
  filter(interaction == "as seed parent") %>%
  left_join(nodes, by = c("Parentage" = "label")) %>%
  rename(from = id) %>%
  left_join(nodes, by = c("Grex" = "label")) %>%
  rename(to = id) %>%
  select(from, to, year, Registrant) %>%
  mutate(color = "blue", title = "Seed Parent")
# 父本edges数据框
pollen_edges <- data %>%
  filter(interaction == "as pollen parent") %>%
  left_join(nodes, by = c("Parentage" = "label")) %>%
  rename(from = id) %>%
  left_join(nodes, by = c("Grex" = "label")) %>%
  rename(to = id) %>%
  select(from, to, year, Registrant) %>%
  mutate(color = "red", title = "Pollen Parent")
edges <- rbind(seed_edges, pollen_edges)
# 创建网络图 
g <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
# 计算circle布局坐标
coords <- layout_in_circle(g)
nodes$x <- coords[, 1] * 800
nodes$y <- coords[, 2] * 800
# 创建可视化网络图
network <- visNetwork(nodes, edges, width = "100%", height = "1000px") %>%
  visNodes(shape = "dot", size = 12, font = list(size = 9)) %>%
  visEdges(arrows = "to", smooth = TRUE) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 2
    # ,algorithm = "hierarchical"
    ),
    nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                 background: #f8f8f8;
                                 color: darkblue;
                                 border:none;
                                 outline:none;')
  ) %>%
  visInteraction(navigationButtons = TRUE,
                 zoomView = TRUE,
                 dragView = TRUE,
                 selectConnectedEdges = TRUE) %>%
  visPhysics(enabled = FALSE) %>%
  visLegend(
    useGroups = FALSE,
    addEdges = data.frame(
      label = c("Seed Parent", "Pollen Parent"),
      color = c("blue", "red"),
      arrows = c("to", "to"),
      font.align = "top"
    ),
    position = "right",
    width = 0.15,
    ncol = 1,
    stepY = 50,
    zoom = FALSE
  )
saveWidget(network, "Sobralia_network(Circular_layout).html", selfcontained = TRUE)

#创建Kamada-Kawai布局网络图
# 计算Kamada-Kawai布局坐标
g <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
coords <- layout_with_kk(g)
nodes$x <- coords[, 1] * 150
nodes$y <- coords[, 2] * 150
# 创建可视化网络图
network <- visNetwork(nodes, edges, width = "100%", height = "1000px") %>%
    visNodes(shape = "dot", size = 12,font = list(size=10)) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.3)),
             smooth = FALSE,width = 1) %>%
    visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1
 # ,algorithm = "hierarchical"
),
        nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                 background: #f8f8f8;
                                 color: darkblue;
                                 border:none;
                                 outline:none;')
    ) %>%
  visInteraction(navigationButtons = TRUE,
                 zoomView = TRUE,
                 dragView = TRUE,
                 selectConnectedEdges = TRUE) %>%
    visPhysics(enabled = FALSE) %>%
    visLegend(
    useGroups = FALSE,
    addEdges = data.frame(
      label = c("Seed Parent", "Pollen Parent"),
      color = c("blue", "red"),
      arrows = c("to", "to"),
      font.align = "top"
    ),
    position = "right",
    width = 0.15,
    ncol = 1,
    stepY = 50,
    zoom = FALSE
  )
saveWidget(network, "Sobralia_network(Kamada-Kawai_layout).html", selfcontained = TRUE)


# Repulsion layout with legend
# network <- visNetwork(nodes, edges, width = "100%", height = "1000px") %>%
#   visNodes(shape = "dot", size = 15, font = list(size = 15)) %>%
#   visEdges(arrows = "to", smooth = FALSE) %>%
#   visOptions(
#     highlightNearest = list(enabled = TRUE, degree = 2),
#     nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
#                                  background: #f8f8f8;
#                                  color: darkblue;
#                                  border:none;
#                                  outline:none;')
#   ) %>%
#   visInteraction(navigationButtons = TRUE,
#                  zoomView = TRUE,
#                  dragView = TRUE,
#                  selectConnectedEdges = TRUE) %>%
#   visPhysics(
#     solver = "repulsion",
#     repulsion = list(
#       nodeDistance = 300,
#       centralGravity = 0.2,
#       springLength = 300,
#       springConstant = 0.05
#     )
#   ) %>%
#   visLegend(
#     useGroups = FALSE,
#     addEdges = data.frame(
#       label = c("Seed Parent", "Pollen Parent"),
#       color = c("blue", "red"),
#       arrows = c("to", "to"),
#       font.align = "top"
#     ),
#     position = "right",
#     width = 0.15,
#     ncol = 1,
#     stepY = 50,
#     zoom = FALSE
#   )

# saveWidget(network, "angraecum_network(Repulsion_layout).html", selfcontained = TRUE)
