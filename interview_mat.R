
## This is the intial version of the function. Assumes consistent overlap.
create.matrix <- function(n.interviewers=6, n.areas=6, overlap=6, n.interview.per.area=6, max.area.depth=1){

    ## Note, a column isnt the same as an interviewer, though a row is the same as an area.
    result.mat <- c()
    
     for(current.area in 1:n.areas){
         left.zeros <- rep.int(x=0, times=ifelse(n.interviewers-overlap==0,
                                                yes=current.area*(n.interviewers-overlap),
                                                 no=current.area*(n.interviewers-overlap)-(n.interviewers-overlap)))

         interview.vec <- 1:n.interviewers

         right.zeros <- rep.int(x=0, times=(n.interviewers*n.areas)-sum(length(left.zeros), length(interview.vec)))

         for(depth in 1:abs(max.area.depth)){
             result.mat <- rbind(result.mat, c(left.zeros, interview.vec, right.zeros))
         }

         
     }    

    return(result.mat)
}

# Can handle shift of 0 (all overlap), symetric.
all.equal(target=rbind(c(1,2,3,0,0,0,0,0,0),
                       c(1,2,3,0,0,0,0,0,0),
                       c(1,2,3,0,0,0,0,0,0)), current=create.matrix(n.interviewers=3, n.areas=3, overlap=3))

# Can handle shift of 1, symetric.
all.equal(target=rbind(c(1,2,3,0,0,0,0,0,0),
                       c(0,1,2,3,0,0,0,0,0),
                       c(0,0,1,2,3,0,0,0,0)), current=create.matrix(n.interviewers=3, n.areas=3, overlap=2))

# Can handle shift of 2, symetric.
all.equal(target=rbind(c(1,2,3,0,0,0,0,0,0),
                       c(0,0,1,2,3,0,0,0,0),
                       c(0,0,0,0,1,2,3,0,0)), current=create.matrix(n.interviewers=3, n.areas=3, overlap=1))

# Can handle shift of 3 (no overlap), symetric.
all.equal(target=rbind(c(1,2,3,0,0,0,0,0,0),
                       c(0,0,0,1,2,3,0,0,0),
                       c(0,0,0,0,0,0,1,2,3)), current=create.matrix(n.interviewers=3, n.areas=3, overlap=0))

# Tests depth flag.
all.equal(target=rbind(c(1,2,3,0,0,0,0,0,0),
                       c(1,2,3,0,0,0,0,0,0),
                       c(0,1,2,3,0,0,0,0,0),
                       c(0,1,2,3,0,0,0,0,0),
                       c(0,0,1,2,3,0,0,0,0),
                       c(0,0,1,2,3,0,0,0,0)), current=create.matrix(n.interviewers=3, n.areas=3, overlap=2, max.area.depth=2))

#create.matrix(n.interviewers=3, n.areas=3, overlap=2, max.area.depth=2)
