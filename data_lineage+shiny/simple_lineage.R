library(dplyr)
library(DiagrammeR)
# DEMO case scenario
# Create a master table which contains all the records of original table,
# its operation and output
# (In progress)user, modification time etc metadata which describe the table
# will be shown on click in a CRUD shiny app

name <- c("ken", "tim", "ben")
master <- tibble(id = uuid::UUIDgenerate(use.time = TRUE) , #implementation for further expansion?
                 table_name = c("a","a","b","c","d","e","f","b", "b","h","g"), 
                 transformation = c("filter", "subset","filter", "subset", "select", "group_by", "filter", "subset", "reverse", "reverse", "sum"), 
                 output = c("f","b","g","h","i","j","g", "c", "a", "a", "z"), 
                 created_by = sample(name, 11, replace = TRUE),
                 script_path = NA,
                 last_modified = Sys.time())


#n = rows of tables
#input_name = table name/origin table column
#output_name = table name/output column after transformation
#transformation = what kind of operation/transformation the table has gone through

dm_graph <- function(df, input_name, output_name,
                     transformation = NULL,
                     input_type = NULL, output_type = NULL){
  require(DiagrammeR)
  
  #set up a table of nodes for every observation in the table
  input_nodes <- create_node_df(n = nrow(df), label = input_name, type = input_type)
  output_nodes <- create_node_df(n = nrow(df), label = output_name, type = output_type)
  all_nodes <- combine_ndfs(input_nodes, output_nodes)
  nodes_name <- c(input_name, output_name)
  
  #set up their linkage e.g from A to B, could also specify transformation
  if (missing(transformation)){
    
    edges <- create_edge_df(from = match(input_name, nodes_name),
                            to = match(output_name, nodes_name))
  } else {
    edges <- create_edge_df(from = match(input_name, nodes_name),
                          to = match(output_name, nodes_name),
                          label = transformation)
  }

  #remove duplicates
  unique_nodes <- unique(c(edges$from, edges$to))
  #create a graph object 
  diagram <- create_graph(all_nodes[unique_nodes,], edges)
}


test <- dm_graph(master, master$table_name, master$output)
test_2 <- dm_graph(master, master$table_name, master$output, master$transformation)

DiagrammeR::render_graph(test, layout = "tree")
DiagrammeR::render_graph(test_2, layout = "nicely")

#search all the previous relation and return a filtered table
locate_nodes <- function(df, output_target){
  t1 <- data.frame()
  for (i in 1:nrow(df)){
    if(df$output[i] == output_target){
      t1 <- bind_rows(t1, df[i, ])
    }
  }
  t1 #t1 is the anchor table with target output

  for (i in 1:nrow(df)){
    sub_t <- get(paste0("t", i)) 
    vector <- sub_t[,2] #create a vector of input
    new_val <- data.frame() 
    for(j in 1:nrow(df)){ #simple logic to include all the tables with duplicates
      if(any(vector %in% df$output[j])){
        #check whether these input exist in other output rows and repeat itself
        new_val <- bind_rows(new_val, df[j,])
      }
      new_val
    }
    assign(paste0("t", i+1), new_val)
  }
  #rbind these duplicate rows hence extract only the unique ones
  final <-unique(do.call(rbind, 
                         mget(ls(pattern="^t"))))
}


subset_node <- locate_nodes(master, "z")
output_nodes_attr <- create_node_df(nrow(subset_node), label = subset_node$output, 
                 value = subset_node$created_by,
                 time = subset_node$last_modified,
                 path = subset_node$script_path)
test_4 <- subset_node %>%
  dm_graph(.$table_name, .$output, .$transformation)

DiagrammeR::render_graph(test_4, layout = "lr")





