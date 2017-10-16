!
!  Programa suma Vialidades
!
!  Creado por Jose Agustin Garcia Reynoso el 12/05/2012
!
! ifort -O2 -axAVX suma_vialidades.f90 -o vial.exe
!
! Proposito:
! Este programa identifica las diferentes vialidades en la celda y las suma.
! y obtiene la fraccion de vialidad en la celda con respecto al municipio
!
!  Modificaciones
!
!   2/Ago/2012  se considera la vialidad en todo el municipio rlm
!
module vars
integer ::nm
integer,allocatable :: grid(:),icve(:),grid2(:),icve2(:),icve3(:)
real,allocatable ::rc(:),rlm(:),rlc(:)
real,allocatable :: sm(:),sc(:),sum(:)

common /vars1/ nm

end module vars
program suma

    call lee

    call calculos

    call guarda
contains
subroutine lee
use vars
implicit none
    integer i
    character(len=30) ::fname
    character (len=10) ::cdum

    fname= "VIALIDADES.csv"
    open (unit=10,file=fname,status='OLD')
    read(10,*) cdum
    i=0
    do
        read(10,*,END=100) cdum
        i=i+1
    end do
100 continue
    nm=i
    print *,'Number of lines in file',nm
    allocate(grid(nm),icve(nm),rlm(nm),rlc(nm))
    rewind(10)
    read(10,*) cdum
    do i=1,nm
        read (10,*) grid(i),icve(i),rlm(i),rlc(i)
    end do
    print *,'Done reading file ',fname
    close(10)
end subroutine

subroutine calculos
use vars
implicit none
    integer i,j,l    
    call count
!    do i=1,nm
!        do j=1,size(icve2)
!     if(icve(i).eq.icve2(j)) then
!        sc(j)= sc(j)+rlc(i)
!     end if
!        end do
!    end do
	rc=0
    do i=1,nm
        do j=1,size(grid2)
        if(grid(i).eq.grid2(j).and.icve(i).eq.icve3(j)) then
          do l=1,size(icve2)
            if(icve(i).eq.icve2(l)) then
                rc(j)=rlc(i)/rlm(i)+rc(j)
                sum(j)=rlm(i)
            end if
          end do
        end if
        end do
    end do
end subroutine calculos

subroutine guarda
use vars
implicit none
integer i,j
open(unit=11,file='salida2.csv',action='write')
    write(11, *)"GRID, CVE_ENT_MUN, frac, suma"
    do i=1,size(grid2)
     write(11, '(I8,",",I6,2(",",ES))') grid2(i),icve3(i),rc(i),sum(i)
    end do
close (11)
end subroutine guarda
subroutine count
    use vars
    logical,allocatable::xl(:)
    allocate(xl(size(icve)))
    xl=.true.
    do i=1,nm-1
        do j=i+1,nm
        if(icve(j).eq.icve(i).and.xl(j)) xl(j)=.false.
        end do
    end do
    j=0
    do i=1,nm
    if (xl(i)) j=j+1
    end do
    allocate(icve2(j),sm(j),sc(j))
    sm=0
    sc=0
    j=0
    do i=1,nm
        if(xl(i)) then
            j=j+1
            icve2(j) = icve(i)
        end if
    end do

  print *,'Number of different municipalities',j
    deallocate(xl)
    allocate(xl(size(grid)))
    xl=.true.   
    do i=1,nm-1
        do j=i+1,nm
        if(grid(j).eq.grid(i).and.xl(j).and.icve(j).eq.icve(i)) xl(j)=.false.
        end do
    end do
    j=0
    do i=1,nm
        if (xl(i)) j=j+1
    end do
    allocate(grid2(j),rc(j),icve3(j),sum(j))
    j=0
    do i=1,nm
        if(xl(i)) then 
            j=j+1
            icve3(j) = icve(i)
            grid2(j) = grid(i)
        end if
    end do
    print *,'Number of different grids',j
    deallocate(xl)
end subroutine count
end program suma