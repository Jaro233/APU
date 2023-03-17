library(ahp)
library(data.tree)
setwd("C:/Users/jaro9/Downloads/APU-master/Zad2")
treeAhp <- Load("tree_design.yml")

print(treeAhp, filterFun = isNotLeaf)

Calculate(treeAhp)
print(treeAhp, priority = function(x) x$parent$priority["Total", x$name])
Visualize(treeAhp)
Analyze(treeAhp)
AnalyzeTable(treeAhp)
