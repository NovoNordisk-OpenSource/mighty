
new <- data.table::fread("/Users/mewp/Desktop/adlb_new.csv")
old <- data.table::fread("/Users/mewp/Desktop/adlb_old.csv")

# waldo::compare(new_rds, old_rds)
data.table::setorder(new, USUBJID, VISITNUM, LBSEQ)
data.table::setorder(old, USUBJID, VISITNUM, LBSEQ)
waldo::compare(old, new)

edges_devtools <- data.table::fread("/Users/mewp/Desktop/edges_devtools.csv") |> setorder(node_id, parent_node)
edges_interactive <- data.table::fread("/Users/mewp/Desktop/edges_interactive.csv") |> setorder(node_id, parent_node)

edges_interactive[, node_id := gsub("tests/testthat", ".", node_id)]
edges_interactive[, parent_node := gsub("tests/testthat", ".", parent_node)]
waldo::compare(edges_devtools, edges_interactive, x_arg = "devtools", y_arg = "interactive")


new <- data.table::fread("/Users/mewp/Desktop/prog_seq_new.csv")
old <- data.table::fread("/Users/mewp/Desktop/prog_seq_old.csv")
data.table(new=new, old=old)
