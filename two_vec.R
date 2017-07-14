
require(rTableICC)



generate.distribution <- function(dataset=iris, category.names=c("Petal.Length", "Species"), sample.size=150){

    contin.table <- with(data=dataset, prop.table(table(get(category.names[1]), get(category.names[2]))))
    
    joint.vec <- rDiscrete(n=sample.size, pf=contin.table)$rDiscrete

    result <- make.two.vec(contin.table=contin.table, joint.vec=joint.vec)

    return(result)

}


make.two.vec <- function(contin.table, joint.vec){

    joint.vec <- ensure.no.false.zero(joint.vec, contin.table,
                                      samp.size=length(joint.vec))
    
    p.table <- prop.table(contin.table)
     
    p.table <- data.frame(p.table)
   
    return(p.table[joint.vec, 1:2])
    
}

ensure.no.false.zero <- function(joint.vec, contin.table, samp.size){

    nonzero.indx <- which(contin.table>0)

    ## If everything that is supposed to have at least one observation
    ## has an observation, return.
    if(sum(nonzero.indx%in%joint.vec)==length(nonzero.indx)){
        return(joint.vec)
    }## Otherwise resample until have such.
    else{
        joint.vec <- c(joint.vec, nonzero.indx[!(nonzero.indx%in%joint.vec)])
        return(ensure.no.false.zero(sample(joint.vec, size=samp.size), contin.table, samp.size))
    }
}


### Example dataset (using iris):

generate.distribution(dataset=iris, category.names=c("Petal.Length", "Species"))


## Test case: Should only differ in the variable names.
all.equal(target=with(data=iris, table(Petal.Length, Species))>0, current=table(generate.distribution(dataset=iris, category.names=c("Petal.Length", "Species")))>0)
