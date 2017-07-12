
## This is the intial version of the function. Assumes consistent overlap.
create.matrix <- function(n.interviewers=6, n.areas=6, overlap=6, n.interview.per.area=6, max.area.depth=1){

    ## Note, a column isnt the same as an interviewer, though a row is the same as an area.
    result.mat <- c()

    interview.counter <- 1
   
    starting.interviewer <- 1

    interview.times <- n.interviewers/n.interview.per.area
    
    for(current.area in 1:n.areas){
        
        left.zeros <- vec.left.zeros(n.interview.per.area, overlap, current.area)

        
        if(interview.counter>interview.times){
            interview.counter <- 1
        }
        
              
        interview.vec <- starting.interviewer:(n.interview.per.area*interview.counter)
        
        right.zeros <- vec.right.zeros(n.interviewers, n.areas, left.zeros, interview.vec)
        
        for(depth in 1:abs(max.area.depth)){
            result.mat <- rbind(result.mat, c(left.zeros, interview.vec, right.zeros))
        }
        
        interview.counter <- interview.counter+1

        starting.interviewer <- starting.interviewer+n.interview.per.area

        if(starting.interviewer>n.interviewers){starting.interviewer <- 1}
       
    }    
    
    return(result.mat)
}


####Helper functions####
vec.left.zeros <- function(n.interviewers, overlap, current.area){
    return(rep.int(x=0, times=ifelse(n.interviewers-overlap==0,
                                                yes=current.area*(n.interviewers-overlap),
                                                 no=current.area*(n.interviewers-overlap)-(n.interviewers-overlap))))
}

vec.right.zeros <- function(n.interviewers, n.areas, left.zeros, interview.vec){
    return(rep.int(x=0, times=(n.interviewers*n.areas)-sum(length(left.zeros), length(interview.vec))))
}
####End helper functions####




####BEGIN TEST CASES####


# Can handle shift of 0 (all overlap), symetric.
all.equal(target=rbind(c(1,2,3,0,0,0,0,0,0),
                       c(1,2,3,0,0,0,0,0,0),
                       c(1,2,3,0,0,0,0,0,0)), current=create.matrix(n.interviewers=3, n.areas=3,
                                                                    overlap=3, n.interview.per.area=3))

# Can handle shift of 1, symetric.
all.equal(target=rbind(c(1,2,3,0,0,0,0,0,0),
                       c(0,1,2,3,0,0,0,0,0),
                       c(0,0,1,2,3,0,0,0,0)), current=create.matrix(n.interviewers=3, n.areas=3,
                                                                    overlap=2, n.interview.per.area=3))

# Can handle shift of 2, symetric.
all.equal(target=rbind(c(1,2,3,0,0,0,0,0,0),
                       c(0,0,1,2,3,0,0,0,0),
                       c(0,0,0,0,1,2,3,0,0)), current=create.matrix(n.interviewers=3, n.areas=3,
                                                                    overlap=1, n.interview.per.area=3))

# Can handle shift of 3 (no overlap), symetric.
all.equal(target=rbind(c(1,2,3,0,0,0,0,0,0),
                       c(0,0,0,1,2,3,0,0,0),
                       c(0,0,0,0,0,0,1,2,3)), current=create.matrix(n.interviewers=3, n.areas=3
                                                                  , overlap=0, n.interview.per.area=3))

# Tests depth flag.
all.equal(target=rbind(c(1,2,3,0,0,0,0,0,0),
                       c(1,2,3,0,0,0,0,0,0),
                       c(0,1,2,3,0,0,0,0,0),
                       c(0,1,2,3,0,0,0,0,0),
                       c(0,0,1,2,3,0,0,0,0),
                       c(0,0,1,2,3,0,0,0,0)), current=create.matrix(n.interviewers=3, n.areas=3,
                                                                    overlap=2, n.interview.per.area=3,
                                                                    max.area.depth=2))

all.equal(target=rbind(c(1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       c(1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       c(0,0,3,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       c(0,0,3,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                       c(0,0,0,0,5,6,0,0,0,0,0,0,0,0,0,0,0,0),
                       c(0,0,0,0,5,6,0,0,0,0,0,0,0,0,0,0,0,0)), current=create.matrix(n.interviewers=6,
                                                                                      n.areas=3,
                                                                                      overlap=0,
                                                                                      n.interview.per.area=2,
                                                                                      max.area.depth=2))



####END TEST CASES####


