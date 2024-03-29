program genAlg
implicit none

! Declaration of variables
integer, parameter :: maxLen = 100    ! maximum length of the phrase
character(maxLen) :: phrase   ! Phrase to be found
integer :: length             ! Length of the phrase
real :: seed                  ! seed to be used in some random generation
integer :: pop_size           ! size of the population
real :: mutation_rate         ! mutation rate
integer :: i,j,k              ! counters
real, allocatable :: rDNA(:)      ! real DNAs
integer, allocatable :: iDNA(:)   ! integer DNAs
character(:), dimension(:), allocatable :: sDNA    ! string DNAs (Population)
integer :: score              ! score to calculate fitness
real, dimension(:), allocatable :: fitness        
real, dimension(:), allocatable :: norm_fit      ! normalized fitness
integer, dimension(:), allocatable :: pool       ! Mating pool with size 100 to represent 100%
integer :: parentA, parentB   ! parents
character(maxLen) :: child    ! child
integer :: generation         ! generation
real :: start, finish         ! to compute processing time

call cpu_time(start)

! Variables initialization
phrase = "To be or not to be. That is the question! (William Shakespeare, book Hamlet - act III, scene I)"
pop_size = 100
mutation_rate = 0.0002
seed = 12345.6
length = len(trim(phrase))
fitness = 0.0
pool = 0
generation = 1
score = 0

allocate(character(length) :: sDNA(pop_size))
allocate(fitness(pop_size))
allocate(norm_fit(pop_size))
allocate(pool(pop_size))
allocate(rDNA(length))
allocate(iDNA(length))

! generates different random numbers at each execution
call random_seed()

!!!!!!!!!!!!!!!!! Creation of random population !!!!!!!!!!!!!!!!!!!!!!!!!
! Firstly, we need to create a population of random generated phrases
!  with the same length of the original one (ASCII code 32 to 125)
! We will call each element of the population DNA (sDNA for string DNA)
! The population will have the size of 20

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
  !!!!!!!!!!!!!!!!! Selection of the fittest !!!!!!!!!!!!!!!!!!!!!!!!!
  ! Now we have to calculate the fitness of each element of the population (DNA)
  ! It is generally a mathematical function, but in this case it will be
  !  the number of correct characters that are in the correct position.
  
  do j=1,pop_size
    score = 0
    do i=1,length
      if (sDNA(j)(i:i) .eq. phrase(i:i)) then
        score = score + 1
      endif
    enddo
    fitness(j) = (1.0*score)/(1.0*length)
    !print*, j, fitness(j), score, length
    ! Verify if there is a phrase with fitness 1 (100% correct)
    if (fitness(j) == 1.0 .or. trim(sDNA(j)) == trim(phrase)) then
      print*
      write(*,'(A12,A100,A8)') "generation", "phrase", "fitness"
      write(*,'(I12,A100,F8.2)') generation, trim(sDNA(j)), fitness(j)
      call cpu_time(finish)
      print*
      write(*,'(A20,F8.2,A2)') 'Time elapsed:', finish-start, " s"
      print*
      ! Exit program
      stop 
    endif
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
    norm_fit(j) = fitness(j)/sum(fitness)
!    print*, j, norm_fit(j), floor(norm_fit(j)*100)
    if (norm_fit(j) .ne. 0.0) then
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
    !print*, j,parentA, pool(parentA), sDNA(pool(parentA))
    !print*, j,parentB, pool(parentB), sDNA(pool(parentB))
    !print*, j, parentA, sDNA(pool(parentA)), fitness(pool(parentA))
    !print*, j, parentB, sDNA(pool(parentB)), fitness(pool(parentB))
    
    ! Reproducing
    child = sDNA(pool(parentA))(1:length/2) // sDNA(pool(parentB))(length/2+1:length)
    !print*, trim(child)
  
    ! Mutating
    do i=1,length
      call random_number(seed)
      if (seed < mutation_rate) then
        call random_number(seed)
        child(i:i) = char(32 + floor((125+1-32)*seed))
      endif
    enddo
  
    ! Adding the child to the population
    sDNA(j) = trim(child)
  
    ! printing new population
    !print*, j, sDNA(j), fitness(j)
  enddo

  write(*,'(A100,F8.2)') sDNA(maxloc(fitness)), maxval(fitness)

  generation = generation + 1
  fitness = 0.0

enddo  ! END OF MAIN LOOP


end program
