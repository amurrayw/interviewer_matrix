
## This is the intial version of the function. Assumes consistent overlap.
create.matrix <- function(n.interviewers=6, n.areas=6, overlap=6, n.interview.per.area=6){

    ## Note, a column isnt the same as an interviewer, though a row is the same as an area.
    result.mat <- c()

    
     for(current.area in 1:n.areas){


         left.zeros <- rep.int(x=0, times=current.area*(n.interviewers-overlap))

         interview.vec <- 1:n.interviewers


         right.zeros <- rep.int(x=0, times=n.interviewers*2-sum(length(left.zeros), length(interview.vec)))


         result.mat <- rbind(result.mat, c(left.zeros, interview.vec, right.zeros))
         
     }    

    return(result.mat)
}

# Can handle shift of 1, symetric.
create.matrix(n.interviewers=6, n.areas=6, overlap=6)




create.matrix(n.interviewers=6, n.areas=6, overlap=5)

