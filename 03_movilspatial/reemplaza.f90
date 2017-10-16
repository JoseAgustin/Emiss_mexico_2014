!
! Programa  Reemplaza
!
!	Created by Agustin on 14/08/12.
!	Copyright 2017 CCA-UNAM. All rights reserved.
!
! proposito:  busca los lugares vacion y los rellena
!
module terra

end module terra

program rellena
use terra

   call leeyguarda
contains
subroutine leeyguarda
implicit none
    integer i,j,k
    real ft
    character(len=20):: cdum,fname='grid_terraceria2.csv'
!
     open(unit=10,file=fname,status='old')
     read (10,'(A20)')cdum
     write(20,'(A20)')cdum
     do
     read(10,*,end=120, err=130)i,k,ft
     if(i.eq.0) then; i=j;else ;j=i ;end if
      write(20,'(I5,",",I5,",",f11.9)')i,k,ft
     end do
120  continue
     Stop "Termina bien2"
130  Stop "Error"
end subroutine leeyguarda
end program