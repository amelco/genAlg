program genAlg
implicit none

! Declaration of variables
character(100) :: phrase
integer :: length
real :: seed
integer :: pop_size
integer :: i,j,k
real :: rDNA(19)
integer :: iDNA(19)
character(19), dimension(20) :: sDNA    ! Array of 20 strings with length 19
integer, dimension(20) :: fitness       ! It must have the same size of population
real, dimension(20) :: norm_fit         ! It must have the same size of population
integer, dimension(100) :: pool         ! Mating pool with size 100 to represent 100%
integer :: parentA, parentB
character(100) :: child
integer :: generation


! Variables initialization
phrase = "To be or not to be."
pop_size = 20
seed = 12345.6
length = len(trim(phrase))
fitness = 0
pool = 0
generation = 1


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
do
  ! Verify if, from the previous step, there is a phrase with fitness 1 (100% correct)
  do j=1,pop_size
    if (norm_fit(j) == 1.0) then
      exit
    endif
  enddo

  ! SHOW ON SCREEN
  !call system('clear')
  !print*, generation, sDNA(maxloc(fitness)), maxval(fitness)


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
  
  ! Normalizing the fitness score and populating the probabilistic mating pool
  k=1
  i=1
  do j=1,pop_size
    norm_fit(j) = real(fitness(j))/sum(fitness)
    if (norm_fit(j) .ne. 0.0) then
      !print*, j, norm_fit(j)
      do while (k <= floor(norm_fit(j)*100))
        pool(i) = j
        k = k+1
        i = i+1
      enddo
      k = 1
    endif
  enddo
  !print*, pool
  
  
  !!!!!!!!!!!!!!!!!!!!!!! Reproduction !!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Half of the genetic material from each parent will generate the child
  
  !!!!!!!!!!!!!!!!!!!!!!! Mutation !!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! The child will have a probability to suffer mutation in order to
  !  increase variety.
  ! Each character will have a chance (1%, for example) to mutate to
  !  a new random character.
  
  ! The processes of selecting two parents, reproduce and mutate will be
  !  repeated until a whole new population is achieved.
  
  
  do j=1,pop_size
    ! Chosing randomly 2 elements of the mating pool
    call random_number(seed)
    parentA = floor(seed*100)+1
    call random_number(seed)
    parentB = floor(seed*100)+1
    !print*, parentA, parentB
    !print*, j,parentA, pool(parentA), sDNA(pool(parentA))
    !print*, j,parentB, pool(parentB), sDNA(pool(parentB))
    
    ! Reproducing
    child = sDNA(pool(parentA))(1:length/2) // sDNA(pool(parentB))(length/2+1:length)
    !print*, j,child
  
    ! Mutating
    do i=1,length
      call random_number(seed)
      if (seed < 0.01) then
        call random_number(seed)
        child(i:i) = char(32 + floor((125+1-32)*seed))
      endif
    enddo
  
    ! Adding the child to the population
    sDNA(j) = child
  
    ! printing new population
    !print*, j, sDNA(j)
  enddo

  generation = generation + 1
  fitness = 0
 
enddo  ! END OF MAIN LOOP

end program
