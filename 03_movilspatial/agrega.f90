!   Programa agrega.f90
!
!  Creado por Jose Agustin Garcia Reynoso el 12/05/2012
!
!  ifort -O2 -axAVX agrega.f90 -o agrega.exe
! Proposito:
!  Lee las fracciones de vialidad y carretera y genera un archivo
!  combinado de ambas
!
!  Modificaciones
!
!   9/Sep/2014  se actualiza salida para varios municipios
!   2/Ago/2012  se incluye en la salida la clave del municipio
!
module vars2
integer nm,nm2,nm3
integer,allocatable :: grid(:),icve(:)
integer,allocatable :: grid2(:),icve2(:)
integer,allocatable :: grid3(:),icve3(:)
real,allocatable ::fcc(:),smc(:)
real,allocatable ::fcv(:),smv(:)
real,allocatable ::fc3(:),fv3(:)

common /dims/ nm,nm2,nm3

end module vars2

program agrega
use vars2
    call lee

    call calcula

    call guarda

contains
	subroutine lee
	implicit none
    integer ::i
    character (len=12):: fname,cdum
    fname='salida.csv'
    open (unit=10,file=fname,status='old',action='READ')
    read(10,*) cdum
    i=0
    do
        read(10,*,END=100) cdum
        i=i+1
    end do
100 continue
    nm=i
    print *,'Number of lines in file',nm
    allocate(grid(nm),icve(nm),fcc(nm),smc(nm))
    rewind(10)
    read(10,*) cdum
    do i=1,nm
    read (10,*) grid(i),icve(i),fcc(i),smc(i)
    end do
    print *,'Done reading file ',fname
    close(10)
!
    fname='salida2.csv'
    open (unit=10,file=fname,status='old',action='READ')
    read(10,*) cdum
    i=0
    do
    read(10,*,END=110) cdum
    i=i+1
    end do
110 continue
    nm2=i
    print *,'Number of lines in file',nm2
    allocate(grid2(nm2),icve2(nm2),fcv(nm2),smv(nm2))
    rewind(10)
    read(10,*) cdum
    do i=1,nm2
    read (10,*) grid2(i),icve2(i),fcv(i),smv(i)
    end do
    print *,'Done reading file ',fname
    close(10)
    nm3=nm+nm2
     print *,'Max number of lines ',nm3
	end subroutine lee
!
	subroutine calcula
	implicit none
    integer imax,imin
    integer i,j,k,l,m
    real, allocatable::  suma(:)
    logical, allocatable:: xl(:)
     allocate(xl(nm2))
     allocate(grid3(nm3))
     allocate(suma(nm3),fc3(nm3),fv3(nm3),icve3(nm3))
    print *,">>>>>>>>>   Computations  <<<<<<<<<<<"
    xl=.false.
    l=1
    m=nm+1
    do i  =1,nm
    icve3(i)=icve(i )
    grid3(i)=grid(i)
    fc3(i)=fcc(i )
    fv3(i)=0
        do j =1,nm2
         if(grid(i).eq.grid2(j)) then
            do k=j,nm2
           if(grid(i).eq.grid2(k).and.icve(i).eq.icve2(k).and.not(xl(k))) then
             fv3(i)=fcv(k)
             xl(k)=.true.
            !print *,grid3(i),icve3(i),fc3(i),fv3(i),i
            end if
           end do! k
end if
end do ! j
end do !i
!
     do i  =1,nm
       do j =1,nm2
        if(grid(i).eq.grid2(j)) then
         do k=j,nm2
          if(grid(i).eq.grid2(k).and.icve(i).ne.icve2(k).and.not(xl(k))) then
              icve3(m )=icve2(k)
              grid3(m)=grid2(k)
              fc3(m)= 0
              fv3(m )=fcv(k)
              xl(k)=.true.
             ! print *,grid3(m),icve3(m),fc3(m),fv3(m),m
               m=m+1
             end if
             end do! k
          end if
        end do !j
     end do !i
     do i=1,nm2
      if(not(xl(i))) then
        icve3(m)=icve2(i)
        grid3(m)=grid2(i)
        fc3(m)=0
        fv3(m)=fcv(i)
        m=m+1
       end if
     end do
       nm3=m-1
	end subroutine calcula
!
	subroutine guarda
	implicit none
    integer i,var
    print *," Max number of lines after combining",nm3
    open (unit=10,file='salida3.csv',action='write')
    write(10,*)'GRID, CVE_ENT_MUN, Fv, Fc'
    write(10,*)nm3
    do i=1,nm3
    write(10,*)grid3(i),",",icve3(i),",",fv3(i),",",fc3(i)
    end do
    end subroutine guarda
    end program agrega
