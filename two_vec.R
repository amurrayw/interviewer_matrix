
require(rTableICC)



generate.distribution <- function(dataset=iris, category.names=c("Petal.Length", "Species"), sample.size=150){

    contin.table <- with(data=dataset, prop.table(table(get(category.names[1]), get(category.names[2]))))
    
    joint.vec <- rDiscrete(n=sample.size, pf=contin.table)$rDiscrete

    result <- make.two.vec(contin.table=contin.table, joint.vec=joint.vec)

    # Need to post process to make sure .
    ## if(sum(contin.table>0 & table(result)==0)>0){

    ##     must.be.greater <- which(contin.table>0)

    ##     print(table(result))
    ##     print(table(result)[which(table(result)[must.be.greater]==0)])


    ##     return(result)

    ## }
    else{return(result)}

}


make.two.vec <- function(contin.table, joint.vec){

    
    
    p.table <- prop.table(contin.table)
     
    p.table <- data.frame(p.table)
   
    return(p.table[joint.vec,1:2])
    
}


### Example dataset (using iris):







all.equal(target=with(data=iris, table(Petal.Length, Species))>0, current=table(generate.distribution(dataset=iris, category.names=c("Petal.Length", "Species")))>0)
