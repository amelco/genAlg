program genAlg
implicit none

! Declaration of variables
character(100) :: phrase
integer :: length
integer :: seed
integer :: i,j
real :: rDNA(19)
integer :: iDNA(19)
character(19), dimension(20) :: sDNA    ! Array of 20 strings with length 19


! Variables initialization
phrase = "To be or not to be."
seed = 12345
length = len(trim(phrase))


! Firstly, we need to create a population of random generated phrases
!  with the same length of the original one (ASCII code 32 to 125)
! We will call each element of the population DNA

call random_number(rDNA)

! 19 is the length. Change to allocatable arrays later
do j=1,20
iDNA = 32 + floor((125+1-32)*rDNA)
  do i=1,19
    if (i==1) then
      sDNA(j) = char(iDNA(i))
    else
      sDNA(j) = trim(sDNA(j)) // char(iDNA(i))
    endif
  enddo
  print*, sDNA(j)
enddo

end program
