
## This is the intial version of the function. Assumes consistent overlap.
create.matrix <- function(n.interviewers=6, n.areas=6, overlap=6, n.interview.per.area){

    ## Note, a column isnt the same as an interviewer, though a row is the same as an area.
    result.mat <- c()


    
    for(current.area in 1:n.areas){


        result.mat <- rbind(result.mat, c(rep.int(x=0, times=current.area-1), 1:n.interviewers, rep.int(x=0, times=n.areas-current.area)))

    }
    

    return(result.mat)
}

# Can handle shift of 1, symetric.
create.matrix(n.interviewers=6, n.areas=6, overlap=6, n.interview.per.area)



