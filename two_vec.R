
library(rTableICC)

### Example dataset (using iris):
contin.table <- with(data=iris, prop.table(table(cut(Petal.Length, 2), Species)))

joint.vec <- rDiscrete(n=200, pf=contin.table)$rDiscrete

contin.table[rDiscrete]



make.two.vec <- function(contin.table, joint.vec){

    n.cat.1 <- nrow(contin.table)
    n.cat.2 <- ncol(contin.table)

}
