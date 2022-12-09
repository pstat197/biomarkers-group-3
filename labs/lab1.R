4.5
str(4.5)
is.double(4.5)
4L
str(4L)
TRUE
str(TRUE)
TRUE+FALSE
str(FALSE+FALSE)
'yay'
str('yay')
'4'+'1'
as.numeric('4')+as.numeric('1')
factor(1,levels=c(1,2),labels=c('blue','red'))
factor('blue',levels=c('blue','red'))
str(factor('blue',levels=c('blue','red')))
c(1,4,7)
c('blue','red')
c(1,4,7)[1]
c(1,4,7)[2]
c(1,4,7)[3]
c(1,4,7)[2:3]
c(1,4,7)[c(1,3)]
my_vec <- c(1,4,7)
str(my_vec)
my_ary <- array(data=c(1,2,3,4,5,6,7,8),
                dim=c(2,4))
my_ary
str(my_ary)
my_other_ary <- array(data=c(1,2,3,4,5,6,7,8),
                dim=c(2,2,2))
my_other_ary
str(my_other_ary)
my_other2_ary <- array(data=1:24,
                      dim=c(2,3,4))
my_other2_ary
my_ary[1,2]
my_other_ary[1,2,1]
my_ary[2, ]
my_other_ary[, , 1]
list('cat',c(1,4,7), TRUE)
list(animal='cat',
     numbers=c(1,4,7),
     short = TRUE)
my_lst <- list(animal='cat',
               numbers=c(1,4,7),
               short = TRUE)
str(my_lst)
my_lst[[1]]
my_lst$animal
my_df <- data.frame(animal = c('cat','hare','tortoise'),
                    has.fur = c(TRUE,TRUE,FALSE),
                    weight.lbs = c(9.1,8.2,22.7))
str(my_df)
my_df