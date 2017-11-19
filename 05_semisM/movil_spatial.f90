!  movil_spatial.f90
!
!  ifort -O3 -axAVX -o MSpatial.exe movil_spatial.f90
!
!  Creado por Jose Agustin Garcia Reynoso el 1/11/2017
! 
!
! Proposito
!          Distribución espacial de las emisiones de fuentes moviles
!          Program that reads MOVES2015 and spatial allocation
!
!   Usa emiss_2015,csv, fracc_2008.csv, salida.csv, gri_movil.csv
!
!  Cambios
!       Se incluye Carbono Negro
!       18/11/2017  Se incluyen GSO4, OTHER, POA
!
module vars
integer nl   !  Number of lines in salida.csv
integer nl2  !  Number of lines in gri_movil.csv
integer,parameter :: npol=14 !  Number of pollutants
integer,parameter :: nstates=32
integer,parameter :: fracp=10 ! Number of
integer,allocatable :: iest(:)  ! estados
integer,allocatable :: cventmun(:) !estado_municipio

integer,allocatable :: id(:),id2(:) !State Mun code in emis and grid files
integer,allocatable ::grid(:),grid2(:) ! gridcode in gri_pob
integer,allocatable :: im(:),im2(:)  ! time lag emis and grid files
! scc code in emis and subset of different scc codes.
integer*8,allocatable ::iscc(:),jscc(:),emid(:) ! emid edo mun id
character (len= 5),dimension(npol) :: pol ! pollutant name
character (len= 5),dimension(fracp) :: polf ! pollutant name fraction file
! ei emission in emissfile (nl dimension)
! uf, rf urban and rural population fraction
! pemi emission in grid cell,pollutan,scc category
real,allocatable:: ei(:,:),uf(:),rf(:),pemi(:,:,:),frac(:,:)
common /vari/ nl,nl2,pol
end module vars
program movil_spatial
use vars

    call lee_e

    call guarda

    call lee

    call computations

    call imprime
	
contains
subroutine imprime
    integer i,j,k
	character(len=15) ::name
	do i=1,npol
	name='M_'//trim(pol(i))//'.csv'
	open(10,file=name)
	write(10,*)'GRIDCODE emissions in g per year'
	write(10,210)size(jscc),(jscc(j),j=1,size(jscc))
	do k=1,size(grid2)
	  write(10,220) grid2(k),(1000*pemi(k,i,j),j=1,size(jscc)),im2(k)
	end do
	close(10)
	end do
    print *,"+++++   DONE SPATIAL MOVIL +++++"
210 format(i6,",",<size(jscc)>(I11,","))
220 format(i6,",",<size(jscc)>(ES12.4,","),I2)
end subroutine imprime
!
subroutine computations
implicit none
  integer i,j,ii,l,k
	print *,' Start doing computations'
	print *,(pol(i),i=1,npol)
	call count  ! counts grids and scc different values
	print *,'end count'
	ii=1
   do k=1, size(grid2)
	do i=1,nl2 ! gri_movil
       if(grid2(k).eq.grid(i))then
		do j=1,nl ! F_movil
		  if (id2(i).eq.emid(j)) then
			  do l=1,size(jscc)
			   if(iscc(j).eq.jscc(l))then
                 do ii=1,npol
                pemi(k,ii,l)=pemi(k,ii,l) &
				+(uf(i)+rf(i))*ei(j,ii)
                 end do !ii
			   end if!scc
			  end do! l
           end if!  id2
		end do!j
      end if! grid
	end do !i
end do! k
end subroutine computations
!
subroutine lee
	implicit none
	integer:: i,j,iedo
    integer:: anio,cint
    character(len=1):: st
	character(len=10):: cdum
	print *,'Starts reading files'
	open(10,file='salida.csv',status='old',action='read')
	read(10,*) cdum,cdum,(pol(i),i=1,npol) !read header
	i=0
	do 
	 read(10,*,END=100)cdum
	 i=1+i
	end do
100 continue
    print *,'number of lines',i
	rewind(10)
	read(10,'(A)') cdum ! read header
	allocate(emid(i),iscc(i),ei(i,npol))
	nl=i
	do i=1,nl
      read(10,*,ERR=140)emid(i),iscc(i),(ei(i,j),j=1,npol)
!    print *,emid(i),iscc(i),ei(i,7)
	end do
	print *,'End reading file salida.csv '
	close(10)
!
	open(10,file='gri_movil.csv',status='old',action='read')
	read(10,'(A)') cdum !read header line 1
	read(10,'(A)') cdum !read header line 2
	i=0
	do 
	 read(10,*,END=110)cdum
	 i=1+i
	end do
110 continue
    !print *,'number of lines',i
	rewind(10)
	read(10,'(A)') cdum !read header line 1
	read(10,'(A)') cdum !read header line 2
	allocate(grid(i),id2(i),uf(i),rf(i),im(i))
	nl2=i
  im=6
  do i=1,nl2
	read(10,*,ERR=160) grid(i),id2(i),uf(i),rf(i)
    iedo=int(id2(i)/1000)
    if(iedo.eq.2.or.iedo.eq.3) im(i)=8
    if(iedo.eq.8.or.iedo.eq.18.and.iedo.eq.25.and.iedo.eq.26)im(i)=7
	!print *,i,grid(i),id2(i),uf(i),rf(i)
	end do
	print *,'End reading file gri_movil.csv',nl2
	close(10)
!
!	Se considera que el 10 % va en carretera 
!   y el 90% en ciudad
!
	uf=uf*0.90
	rf=rf*0.10
	return
140 print *,"Error in reading file salida.csv",i
    stop
160 print *,"Error in reading file gri_movil.csv",i
end subroutine lee
!
!  COUNTING grids, scc
!
subroutine count
  integer i,j
  logical,allocatable::xl(:)
  allocate(xl(size(grid)))
  xl=.true.
  do i=1,nl2-1
   do j=i+1,nl2
   if(grid(j).eq.grid(i).and.xl(j)) xl(j)=.false.
   end do
  end do
  j=0
  do i=1,nl2
    if(xl(i)) j=j+1
  end do
  allocate(grid2(j),im2(j))
  j=0
  do i=1,nl2
    if(xl(i)) then
	j=j+1
	grid2(j)=grid(i)
    im2(j)=im(i)
	end if
  end do

  !print *,'Number of different cells',j
  deallocate(xl)
!
! From emissions file F_moviles.csv
  allocate(xl(size(iscc)))

  xl=.true.
  
  do ii=1,nl-1
    do i=ii+1,nl
    if(iscc(ii).eq.iscc(i).and.xl(i)) xl(i)=.false.
	end do
  end do
  ii=0
  do i=1,nl
   if(xl(i)) then
   ii=ii+1
   end if
  end do
  !print *,'different SCC ',ii
  allocate(jscc(ii))
  allocate(pemi(j,npol,ii))
  pemi=0
   ii=0
    do i=1,nl
     if(xl(i)) then
	 ii=ii+1
	 jscc(ii)=iscc(i)
	 end if
  end do
!  print *,(jscc(i),i=1,ii)
  deallocate(xl)
end subroutine count
subroutine lee_e
integer i,j,k
character (len=10) cdum

  print *,'Start reading files'

  open (unit=10,file="emiss_2015.csv",status='OLD',ACTION='read')
  read (10,*) cdum,cdum,(pol(i),i=1,npol)
  i=0
  do
    read(10,*,END=100)cdum
    i=1+i
  end do
  100 continue
  print *,'number of lines',i
  rewind(10)
  read(10,*) cdum ! read header
  allocate(iest(i),iscc(i),ei(i,npol))
  do j=1,i
    read(10,*)iest(j),iscc(j),(ei(j,k),k=1,npol)
    end do
  print *,'End reading file emiss_2015.csv '
  close(10)

  open (unit=10,file="fracc_2008.csv",status='OLD',ACTION='read')
  read (10,*) cdum,(polf(i),i=1,fracp)
  i=0
  do
    read(10,*,END=200)cdum
    i=1+i
  end do
  200 continue
  print *,'number of lines',i
  rewind(10)
  read(10,*) cdum ! read header
  allocate(cventmun(i),frac(i,npol))
  do j=1,i
    read(10,*)cventmun(j),(frac(j,k),k=1,fracp)
  end do
  print *,'End reading file fracc_2008.csv '
  close(10)
end subroutine lee_e

subroutine guarda
integer i,j,k,l
real,dimension(npol):: mm
  open (unit=11,file='salida.csv')
  write(11,110) "CVENTMUN","SCC",(pol(i),i=1,npol)
  do i=1,nstates
    do j= 1,size(iest)
      do k=i,size(cventmun)
        if(iest(j).eq.i .and. cventmun(k)/1000.eq.i) then
          do l=1,npol
            do m=1,fracp
              if(trim(pol(l)).eq.trim(polf(m))) mm(l)=ei(j,l)*frac(k,m)
              if(trim(pol(l)).eq."NO" .and.trim(polf(m)).eq."NOx") mm(l)=ei(j,l)*frac(k,m)
              if(trim(pol(l)).eq."NO2".and.trim(polf(m)).eq."NOx") mm(l)=ei(j,l)*frac(k,m)
              if(trim(pol(l)).eq."GSO4".and.trim(polf(m)).eq."PM25") mm(l)=ei(j,l)*frac(k,m)
              if(trim(pol(l)).eq."OTHER".and.trim(polf(m)).eq."PM25") mm(l)=ei(j,l)*frac(k,m)
              if(trim(pol(l)).eq."POA".and.trim(polf(m)).eq."PM25") mm(l)=ei(j,l)*frac(k,m)
            end do
          end do
          write(11,120)cventmun(k),iscc(j),(mm(l),l=1,npol)
        end if
      end do
    end do
  end do
  deallocate(iest,iscc,ei,cventmun,frac)
  close(11)
  110 format(A,14(",",A),",",A)
  120 format(I6,",",I10,",",<npol-1>(F12.1,","),F12.1)
end subroutine guarda
end program
