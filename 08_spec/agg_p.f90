!
!	aggp.f90
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
!  compile:
!  ifort -O2 -axAVX  agg_p.f90 -o spp.exe
!
!  Modification
!  18/07/2017    for lower and upper capa and layers
!
module var_aggp
integer :: nh     !number of hours in a day
integer :: nclass !number of clasess in profiles_spc.txt
integer :: nspecies ! max number species in profile 0 (292)
integer lfa  ! line number in emissions file T_ANNVOC.csv
integer,allocatable :: capa(:,:),layer(:,:)
integer,allocatable :: grid(:)   ! grid id from emissions file
integer,allocatable :: grid2(:)   ! different grid id from emissions file
integer,allocatable :: isp(:)   ! number of chemical species in profile j
integer,allocatable ::profile(:),prof2(:) ! profile ID from file scc-profiles
integer*8,allocatable:: iscc(:) !SCC from emissions file
real,allocatable :: ea(:,:)      ! emissions in TCOV file grid , nh
real,allocatable :: emis(:,:,:)  ! emissions id cel, category, hours
real,allocatable :: fclass(:,:,:)! aggregation factor by size(profile), species, nclass

character(len=4),allocatable::cname(:)
character (len=19) :: current_date,cprof
character(len=3) :: cday

parameter (nspecies=292,nh=24,ncat=40)
common /date/ lfa,current_date,cday,cprof

end module var_aggp

program agg_m
use var_aggp

	call lee
	
	call calculos
	
	call guarda

contains
subroutine lee
	implicit none
	integer :: i,j,l,id,idum
	integer*8::isccf
	real,dimension(ncat)::fagg ! aggregation factor for 30 species
	character(len=10) :: cdum
	open (unit=10,file='T_ANNVOC.csv',status='old',action='read')
	lfa=0
	read(10,*) cdum
	read(10,*) i,current_date,cday
	do
		read(10,*,end=100)cdum
		lfa=lfa+1
	end do
100 continue
	print *,'Line number in T_ANNVOC.csv',lfa,i,current_date,' ',cday
	allocate(grid(lfa),iscc(lfa),ea(lfa,nh),profile(lfa))
	allocate(capa(lfa,2))
	profile=0
	isp=0
	rewind(10)
	read (10,*) cdum  ! header 1
	read (10,*) cdum  ! header 2
	do i=1,lfa
		read (10,*)iscc(i),grid(i),capa(i,1),(ea(i,j),j=1,nh),capa(i,2)
        !print *, i,iscc(i)
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
!   do i=1,lfa
!     if(profile(i).eq.0) print *,'No profile: ',iscc(i),' ',i
!   end do
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
	if(nclass.gt.ncat) stop "Change size in fagg dimension ncat"
	rewind(16)
	allocate(cname(nclass))
	read(16,*)cdum
	read(16,*) nclass,cdum,(cname(i),i=1,nclass)
	print *,"Number of profiles",nclass
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
       ! print *,isp,maxval(isp)
       ! print *, size(prof2),maxval(isp),nclass
	allocate(fclass(size(prof2),maxval(isp),nclass))
	rewind(16)
	read(16,*)cdum  ! Header 1
	read(16,*)cdum  ! Header 2
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
     print *,'done reading profiles'
	i=1
	!do j=1,isp(i) 
!	print '(2i,<nclass>ES)',profile(i),j,(fclass(i,j,l),l=1,nclass)
	!end do
	close(16)
      print *,'done lee'
end subroutine lee
subroutine calculos
	implicit none
	integer i,j,k,l,ih,m
	integer ns,ng,ii
	print *,'Starting computations'
	allocate (emis(size(grid2),nclass,nh))
	emis=0
	ng =size(grid2)
	ns =size(prof2)
	do ii=1,lfa
	  do k=1,ng		! grid
		if(grid(ii).eq.grid2(k).and. capa(ii,1).eq.layer(k,1)) then
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
	fname=trim(cprof)//'_'//trim(cname(j))//'_P.txt'
	open(unit=20,file=fname,action='write')
	write(20,'(4A)')cname(j),',',trim(cprof),', Emissions'
	write(20,*) size(emis,dim=1),',',current_date,',',cday
		do k=1,size(emis,dim=1)
			write(20,200)grid2(k),layer(k,1),(emis(k,j,i),i=1,size(emis,dim=3)),layer(k,2)
            do i=1,size(emis,dim=3)
                suma=suma+emis(k,j,i)
            end do
        end do
	close(20)
    write (6,*)cname(j),',',suma
	end do
200 format (I8,",",I3,",",24(ES12.5,","),I3)
    print *,"*****  DONE POINT SPATIAL *****"
end subroutine guarda
subroutine count
	integer i,j,nn
	logical,allocatable::xl(:)
    nn=size(profile)
    allocate(xl(nn))
    xl=.true.
    do i=1,nn-1
        do j=i+1,nn
            if(profile(j).eq.profile(i).and.xl(j)) 	xl(j)=.false.
        end do
    end do
    j=0
    do i=1,nn
        if(xl(i)) j=j+1
    end do
    allocate(prof2(j),isp(j))
    j=0
    do i=1,nn
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
   if(grid(j).eq.grid(i).and.xl(j).and. capa(i,1).eq.capa(j,1)) xl(j)=.false.
   end do
  end do
  j=0
  do i=1,lfa
    if(xl(i)) j=j+1
  end do
  allocate(grid2(j),layer(j,2))
  j=0
  do i=1,lfa
    if(xl(i)) then
	j=j+1
	grid2(j)=grid(i)
    layer(j,1)=capa(i,1)
    layer(j,2)=capa(i,2)
	end if
  end do

  print *,'Number of different cells',j
  deallocate(xl)
end subroutine count

end program agg_m
