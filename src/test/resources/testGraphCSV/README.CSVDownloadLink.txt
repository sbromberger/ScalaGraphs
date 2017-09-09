Download and unzip this archive to ~projectRoot/test/resources/testGraphCSV/ for standard test graphs.
https://www.dropbox.com/s/oxtd8iowv1xhfnh/ER.testGraphCSV.zip?dl=0

CSV files generated with Julia LightGraphs and GraphIO:
g = erdos_renyi(n, p)
savegraph("/Users/aether/ER.n.p.csv", g, EdgeListFormat(); compress=false)
