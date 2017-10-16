!
!	agg_m.f90
!	
!
!  Creado por Jose Agustin Garcia Reynoso el 31/05/12.
!
! Proposito:
!               Especiacion y agreacion en diferenes especies y
!               clases de un mecanismo especifico
!
!				to do speciation anad aggregation to the differnt
! species and Classes of an specific mechanism
!
!  compile: ifort -O2 -axAVX agg_m.f90 -o spm.exe
!
module var_agg
integer :: nh     !number of hours in a day
integer :: nclass !number of clasess in profiles_spc.txt
integer :: nspecies ! max number species in profile 0 (292)
integer lfa  ! line number in area file TCOV_2014.txt
integer,allocatable ::grid(:)   ! grid id from emissions file
integer,allocatable ::grid2(:)   ! different grid id from emissions file
integer,allocatable :: isp(:)   ! number of chemical species in profile j
integer,allocatable ::profile(:),prof2(:) ! profile ID from file scc-profiles
integer*8, allocatable:: iscc(:) !SCC from emissions file
real,allocatable :: ea(:,:)      ! emissions in TCOV file grid , nh
real,allocatable :: emis(:,:,:)  ! emissions id cel, category, hours
real,allocatable :: fclass(:,:,:)! aggregation factor by size(prof2), species, nclass
character(len=4),allocatable::cname(:)
character (len=19) :: current_date,cprof

parameter (nspecies=292,nh=24)
common /date/ current_date,cprof
end module var_agg

program agg_m
use var_agg

	call lee
	
	call calculos
	
	call guarda

contains

subroutine lee
	implicit none
	integer :: i,j,id,idum,l
	integer*8::isccf
	real,dimension(40)::fagg ! aggregation factor for 30 species
	character(len=10)::cdum
	logical :: lfil
	print *,"TMCOV_2014.csv"
	open (unit=10,file='TMCOV_2014.csv',status='old',action='read')
	read(10,*) cdum  ! header
	print *, cdum
	read(10,*) lfa,current_date  ! header
	print *,lfa,current_date
	i=0
	do 
	read(10,*,end=100) cdum 
	i=i+1
	end do
100 continue
	print *,'Number in TMCOV_2014',i
	lfa=i
	allocate(grid(lfa),iscc(lfa),ea(lfa,nh),profile(lfa))
	rewind(10)
	read (10,*) cdum  ! header 1
	read (10,*) cdum  ! header 2
	do i=1,lfa
	read (10,*)grid(i),iscc(i),(ea(i,j),j=1,nh)
	end do
	close(10)
! READING  and findign profiles
	open(unit=15,file='scc-profiles.txt',status='old',action='read')
	do
		read(15,*,END=200) isccf,cdum,j
		do i=1,lfa
		 if (isccf.eq.iscc(i)) profile(i)=j
		end do
	end do
200 continue
    close(15)
	!print '(15I5)',(profile(i),i=1,lfa)
	print *,'Start count'
	call count  ! counts the number of different profiles
	print *,'Finishing count'
! READING  and findign speciation for profiles
	open(unit=16,file='profile_mech.csv',status='old',action='read')
	read(16,*)cdum,cprof
	read(16,*) nclass
	print *,'Speciation for Mechanism: ',trim(cprof)
	if(nclass.gt.40) stop "Change size in fagg dimension"
	rewind(16)
	allocate(cname(nclass))
	read(16,*)cdum
	read(16,*) nclass,cdum,(cname(i),i=1,nclass)
	!print *,nclass
	!print '(<nclass>(A,x))',cname
	j=0
        isp=0
	do
		read(16,*,end=300,ERR=300)id
		do i=1,size(prof2)
		if(id.eq.prof2(i)) isp(i)=isp(i)+1
		end do
		j=j+1
	end do
300 continue
	!print *,isp,maxval(isp)
	allocate(fclass(size(prof2),maxval(isp),nclass))
	rewind(16)
	read(16,*)cdum  ! Header 1
	read(16,*)cdum	! Header 2
	isp=0
	do
		read(16,*,end=400,ERR=400)id,idum,(fagg(i),i=1,nclass)
		do i=1,size(prof2)
		if(id.eq.prof2(i)) then
			isp(i)=isp(i)+1
			do l=1,nclass
				fclass(i,isp(i),l)=fagg(l)
			end do
		end if
		end do
	end do
400 continue
!	i=1
!	do j=1,isp(i)			
!		print '(2i,<nclass>F)',prof2(i),j,(fclass(i,j,l),l=1,nclass)
!	end do

	close(16)
end subroutine lee

subroutine calculos
	implicit none
	integer i,j,k,l,ih
	integer ns,ng,ii
	print *,'Starting computations'
	allocate (emis(size(grid2),nclass,nh))
	emis=0
	ng =size(grid2)
	ns =size(prof2)
	do ii=1,lfa
	  do k=1,ng		! grid
		if(grid(ii).eq.grid2(k)) then
			do i=1,ns  !profiles
			if(prof2(i).eq.profile(ii)) then
				do j=1,isp(i)  ! species in profile isp(i)
					do l=1,nclass ! mechanism classes
					 do ih=1,nh   ! hours
	if(fclass(i,j,l).ne.0) emis(k,l,ih)=emis(k,l,ih)+fclass(i,j,l)*ea(ii,ih)
					 end do
					end do
				end do
			end if
			end do
		end if
	  end do
	end do
end subroutine calculos
subroutine guarda
	implicit none
	integer i,j,k
   real suma
	character(len=20)::fname
	print *,maxval(emis),'Valor maximo'
	do j=1,size(emis,dim=2)
    suma=0
	fname=trim(cprof)//'_'//trim(cname(j))//'_M.txt'
	open(unit=20,file=fname,action='write')
	write(20,'(4A)')cname(j),',',trim(cprof),', Emissions'
	write(20,*) size(emis,dim=1),current_date
		do k=1,size(emis,dim=1)
			write(20,'(I7,",",24(ES12.5,","))')grid2(k),(emis(k,j,i),i=1,size(emis,dim=3))
        do i=1,size(emis,dim=3)
            suma=suma+emis(k,j,i)
        end do
		end do
	close(20)
    write (6,*)cname(j),',',suma
	end do
    print *,"*****  DONE MOVIL SPECIATION *****"
end subroutine guarda
subroutine count
  integer i,j,nn
  logical,allocatable::xl(:)
  nn=size(profile)
  allocate(xl(nn))
  xl=.true.
  do i=1,60!nn-1
    do j=i+1,61!nn
      if(profile(j).eq.profile(i).and.xl(j)) 	xl(j)=.false.
    end do
  end do
  j=0
  do i=1,61!nn
    if(xl(i)) j=j+1
  end do
  allocate(prof2(j),isp(j))
  j=0
  do i=1,61!nn
    if(xl(i)) then
      j=j+1
      prof2(j)=profile(i)
    end if
  end do
  !
  print *,'Number different profiles',j !,prof2
  !
  deallocate(xl)
  allocate(xl(size(iscc)))

  xl=.true.
  do i=1,lfa-1
    do j=i+1,lfa
      if(grid(j).eq.grid(i).and.xl(j)) xl(j)=.false.
    end do
  end do
  j=0
  do i=1,lfa
    if(xl(i)) j=j+1
  end do
  allocate(grid2(j))
  j=0
  do i=1,lfa
    if(xl(i)) then
      j=j+1
      grid2(j)=grid(i)
    end if
  end do

  print *,'Number of different cells',j
  deallocate(xl)
end subroutine count
end program agg_m
