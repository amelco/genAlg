program genAlg
implicit none

! Declaration of variables
character(100) :: phrase
integer :: length
integer :: seed
integer :: pop_size
integer :: i,j
real :: rDNA(19)
integer :: iDNA(19)
character(19), dimension(20) :: sDNA    ! Array of 20 strings with length 19
integer, dimension(20) :: fitness       ! It must have the same size of population
real, dimension(20) :: norm_fit      ! It must have the same size of population


! Variables initialization
phrase = "To be or not to be."
pop_size = 20
seed = 12345
length = len(trim(phrase))
fitness = 0


!!!!!!!!!!!!!!!!! Creation of random population !!!!!!!!!!!!!!!!!!!!!!!!!
! Firstly, we need to create a population of random generated phrases
!  with the same length of the original one (ASCII code 32 to 125)
! We will call each element of the population DNA (sDNA for string DNA)
! The population will have the size of 20

! Maximum length of phrase is 100. Change to allocatable arrays later
do j=1,pop_size
call random_number(rDNA)
iDNA = 32 + floor((125+1-32)*rDNA)
  do i=1,length
    if (i==1) then
      sDNA(j) = char(iDNA(i))
    else
      sDNA(j) = trim(sDNA(j)) // char(iDNA(i))
    endif
  enddo
  print*, j, sDNA(j)
enddo

!!! HERE THE LOOP BEGINS

!!!!!!!!!!!!!!!!! Selction of the fittest !!!!!!!!!!!!!!!!!!!!!!!!!
! Now we have to calculate the fitness of each element of the population (DNA)
! It is generally a mathematical function, but in this case it will be
!  the number of correct characters that are in the correct position.

do j=1,pop_size
  do i=1,length
    if (sDNA(j)(i:i) .eq. phrase(i:i)) then
      fitness(j) = fitness(j) + 1
    endif
  enddo
  print*, j, fitness(j)
enddo

!!! Choosing the parents !!!
! We will create a so called 'mating pool' from which we will select the 2 best 
!  parents based on a probabilistic method. First, we normalize the fitness score
!  to have a relative fitness to each element. Then, this relative fitness will
!  give us the probability to chose the parents.

! Normalizing the fitness score
do j=1,pop_size
  norm_fit(j) = real(fitness(j))/sum(fitness)
  if (norm_fit(j) .ne. 0.0) then
    print*, j, norm_fit(j)
  endif
enddo

end program
