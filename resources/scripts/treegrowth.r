trees <- list() #list of trees
n <- 0 #number of trees in the list of trees
#start collecting information on current tree
current.ID <- treeg$tree.ID[1]
current.age <- treeg$age[1]
current.dbh <- treeg$dbh.in[1]
current.height <- treeg$height.ft[1]
for (i in 2:dim(treeg)[1]) {
  if (treeg$tree.ID[i] == current.ID) {
    #continue collecting information on current tree
    current.age <- c(treeg$age[i], current.age)
    current.dbh <- c(treeg$dbh.in[i], current.dbh)
    current.height <- c(treeg$height.ft[i], current.height)
  } else {
    #add previous tree to list of trees
    n <- n + 1
    trees[[n]] <- list(tree.ID = current.ID, 
                      forest = treeg$forest[i-1],
                      habitat = treeg$habitat[i-1],
                      age = current.age, 
                      dbh.in = current.dbh, 
                      height.ft = current.height)
    #start collecting information on current tree
    current.ID <- treeg$tree.ID[i]
    current.age <- treeg$age[i]
    current.dbh <- treeg$dbh.in[i]
    current.height <- treeg$height.ft[i]
  }
}
#add final tree to list of trees
n <- n + 1
trees[[n]] <- list(tree.ID = current.ID, 
                  forest = treeg$forest[i-1],
                  habitat = treeg$habitat[i-1],
                  age = current.age, 
                  dbh.in = current.dbh, 
                  height.ft = current.height)

  