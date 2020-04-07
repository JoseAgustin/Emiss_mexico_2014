!
!	movil_temp.f90
!	
!
!  Creado por Jose Agustin Garcia Reynoso el 25/05/12.
!
! Proposito
!          Distribución temporal de las emisiones de fuentes moviles
!   ifort -O3 movil_temp.f90 -o Mtemporal.exe
!
!   modificado
!   02/10/2012  Ajuste en horas dia previo subroutina lee
!   10/02/2015  Se indica que esta en g/h las emisiones.
!   10/07/2017  Para 2014 nnscc 57 a 58, inclusion del 76946 y BC, CO2 y hEST
!   21/07/2017  Incluye CO2 y CH4
!   01/11/2017  Incluye NO y NO2
!   18/11/2017  Incluye GSO4, POA and OTHE
!   06/04/2020  Incluye Horario de verano
!
module variables
integer :: month
integer :: daytype ! tipo de dia 1 lun a 7 dom
integer :: perfil  ! perfil temporal horario
integer,parameter :: nf=14 !number of emission files
integer,parameter :: nh=24 ! number of hour per day
integer,parameter :: nnscc=36 !max number of scc descriptors in input files
integer,parameter ::juliano=365
integer :: nm ! line number in emissions file
integer :: iverano  ! si es en periodo de verano
integer,dimension(nf) :: nscc ! number of scc codes per file
integer*8,dimension(nnscc) ::iscc 
integer, allocatable :: idcel(:),idcel2(:)
integer, allocatable :: mst(:)  ! Difference in number of hours (CST, PST, MST)
real :: fweek                   ! weeks per month
real,allocatable ::emiM(:,:,:) !Mobile emisions from files cel,ssc,file
real,allocatable :: emis(:,:,:) ! Emission by cel,file and hour (inorganic)
real,allocatable :: evoc(:,:,:) ! VOC emissions cel,scc and hour
real,allocatable :: epm2(:,:,:) ! PM2.5 emissions cel,scc and hour
real,dimension(nnscc,nf) :: mes,dia, diap
real,dimension(nnscc,nf,nh):: hCST,hMST,hPST,hEST
integer,dimension(3,nnscc,nf):: profile  ! 1=mon 2=weekday 3=hourly
character (len=19) :: current_date

character(len=14),dimension(nf) ::efile,casn

 data efile / 'M_CO.csv' ,'M_NH3.csv' ,'M_NO2.csv','M_NO.csv',&
              'M_SO2.csv','M_CN.csv' ,'M_CO2.csv','M_CH4.csv',&
              'M_PM10.csv','M_GSO4.csv','M_POA.csv','M_OTHER.csv',&
              'M_PM25.csv','M_VOC.csv'/

 data casn /'TMCO__2014.csv','TMNH3_2014.csv','TMNO2_2014.csv', &
            'TMNO_2014.csv ','TMSO2_2014.csv','TMCN__2014.csv', &
            'TMCO2_2014.csv','TMCH4_2014.csv','TMPM102014.csv', &
            'GSO4_M.csv','POA_M.csv','OTHE_M.csv','TMPM2_2014.csv','TMCOV_2014.csv'/

common /vars/ fweek,nscc,nm,month,daytype,perfil,mes,dia,hora,current_date
end module
!
!  Progran  atemporal.f90
!
!  Make the temporal distribution of emissions
!

program atemporal 
   use variables
   
   call lee
   
   call compute
   
   call storage
   
contains

subroutine lee
	implicit none 
	integer i,j,k,l,m
	integer idum,imon,iwk,ipdy,idia
	integer*8 jscc ! scc code from temporal file
	integer,dimension(25) :: itfrc  !montly,weekely and hourly values and total
	integer,dimension(12) :: daym ! days in a month
	real rdum
	logical fil1,fil2
	character(len=4):: cdum
	character(len=18):: nfile,nfilep
	! number of day in a month 
	!          jan feb mar apr may jun jul aug sep oct nov dec
	data daym /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

	print *,"READING fecha.txt file"
	open (unit=10,file='fecha.txt',status='OLD',action='read')
	read (10,*)month  
	read (10,*)idia
	month=abs(month)
	idia=abs(idia)
	if (month.lt.1 .or. month.gt.12) then
	print '(A,I3)','Error in month (from 1 to 12) month= ',month
	stop
	end if
	if (idia.gt.daym(month))then
	print '(A,I2,A,I2)','Error in day value: ',idia,' larger than days in month ',daym(month)
	Stop
	end if
	close(10)
!Horario de verano Abril 6 a octubre 26
    iverano=kverano(idia,month)
!
	if(month.lt.10) then
	write(current_date,'(A6,I1,A12)')'2014-0',month,'-01_00:00:00'
	else
	write(current_date,'(A5,I2,A12)')'2014-0',month,'-01_00:00:00'	
	end if
    if(idia.lt.10) then
        write(current_date(10:10),'(I1)') idia
        else 
        write(current_date( 9:10),'(I2)') idia
    end if
    fweek=7./daym(month) !weeks per month
    print *,'Done fecha.txt : ',current_date,month,idia,fweek

!
!   Days in 2014 year
!
    print *,"READING anio2014.csv file"
    open (unit=10,file='anio2014.csv',status='OLD',action='read')
    daytype=0
    read(10,*)cdum
        do
        read(10,*,end=95)imon,ipdy,idum,cdum
		if(imon.eq.month.and. ipdy.eq.idia) then
		 daytype=idum
		 print *,'Day type :',daytype,cdum
		 exit
		 end if
        end do
95  continue
    close(10)
	if(daytype.eq.0) STOP 'Error in daytype=0'
!
	do k=1,nf
	open (unit=10,file=efile(k),status='OLD',action='read')
	read (10,'(A)') cdum
	read (10,*) nscc(k),(iscc(i),i=1,nscc(k))
	!print '(5(I10,x))',(iscc(i),i=1,nscc(k))
	!print *,cdum,nscc(k)
	nm=0
	do
	   read(10,*,end=100) cdum
	   nm=nm+1
	end do
100	 continue
     !print *,nm
	 rewind(10)
	 if(k.eq.1) then
	allocate(idcel(nm),idcel2(nm),mst(nm))
	allocate(emiM(nm,nnscc,nf))
	end if
	read (10,'(A)') cdum
	read (10,'(A)') cdum
	do i=1,nm
		read(10,*) idcel(i),(emiM(i,j,k),j=1,nscc(k)),mst(i)
		!print *,i,idcel(i),(emiM(i,j,k),j=1,nscc(k)),mst(i)
	end do
	close(10)
	print *,"Done reading: ",efile(k)
!  REading and findig monthly, week and houry code profiles
    inquire(15,opened=fil1)
    if(.not.fil1) then
	  open(unit=15,file='temporal_01.txt',status='OLD',action='read')
	else
	  rewind(15)
	end if
	read (15,'(A)') cdum
      do
	  read(15,*,END=200)jscc,imon,iwk,ipdy
	    do i=1,nscc(k)
		  if(iscc(i).eq.jscc) then
		    profile(1,i,k)=imon
		    profile(2,i,k)=iwk
		    profile(3,i,k)=ipdy
		   end if
		end do
	  end do
 200 continue
     !print '(A3,<nscc(k)>(I5))','mon',(profile(1,i,k),i=1,nscc(k))      
     !print '(A3,<nscc(k)>(I4,x))','day',(profile(2,i,k),i=1,nscc(k))      
	 !print '(A3,<nscc(k)>(I4,x))','hr ',(profile(3,i,k),i=1,nscc(k))
	 print *,'   Done Temporal_01'
!  REading and findig monthly  profile
    inquire(16,opened=fil1)
    if(.not.fil1) then
	  open(unit=16,file='temporal_mon.txt',status='OLD',action='read')
	else
	  rewind(16)
	end if
	read (16,'(A)') cdum
     do
	    read(16,*,END=210)jscc,(itfrc(l),l=1,13)
	    do i=1,nscc(k)
	      if(jscc.eq.profile(1,i,k)) then
	        mes(i,k)=real(itfrc(month))/real(itfrc(13))
	      end if
		end do !i
	 end do
 210 continue
    ! print '(A3,<nscc(k)>(f6.3))','mon',(mes(i,k),i=1,nscc(k))
	 print *,'   Done Temporal_mon'
!  REading and findig weekely  profile
    inquire(17,opened=fil1)
    if(.not.fil1) then
	  open(unit=17,file='temporal_week.txt',status='OLD',action='read')
	else
	  rewind(17)
	end if
	read (17,'(A)') cdum
     do
	    read(17,*,END=220)jscc,(itfrc(l),l=1,8)
    week: do i=1,nscc(k)
	      if(jscc.eq.profile(2,i,k)) then
	        dia(i,k)=real(itfrc(daytype))/real(itfrc(8))
            if(daytype.eq.1) then
              diap(i,k)=real(itfrc(daytype+6))/real(itfrc(8))
            else
              diap(i,k)=real(itfrc(daytype-1))/real(itfrc(8))
            end if
	      end if
		end do week!i
	 end do
 220 continue
    ! print '(A5,<nscc(k)>(f6.3))','day',(dia(i,k),i=1,nscc(k))
    ! print '(A5,<nscc(k)>(f6.3))','day p',(diap(i,k),i=1,nscc(k))
	 print *,'   Done Temporal_week'
	 nfilep='temporal_wkend.txt'
	 nfile='temporal_wkday.txt'
!  REading and findig houlry  profile
    inquire(18,opened=fil1)
    if(.not.fil1) then
	  open(unit=18,file=nfile,status='OLD',action='read')
	else
	  rewind(18)
	end if
	read (18,'(A)') cdum
     do
	    read(18,*,END=230)jscc,(itfrc(l),l=1,25)
	   dias: do i=1,nscc(k)
           call adecua(profile(3,i,k),daytype,perfil)
	      if(jscc.eq.perfil) then
            m=4-iverano
            do l=1,nh
            if(m+l.gt.nh) then
              hEST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
              hEST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
            end do
		    m=5-iverano
		    do l=1,nh
            if(m+l.gt.nh) then
                hCST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
                hCST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
			end do
		    m=6-iverano
            do l=1,nh
            if(m+l.gt.nh) then
                hMST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
                hMST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
			end do
		    m=7-iverano
            do l=1,nh
            if(m+l.gt.nh) then
                hPST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
                hPST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
            end do
	      end if
		end do dias!i
	 end do  !  Files 18
 230 continue
!   do l=1,nh
!    print '(A3,x,I3,x,<nscc(k)>(f7.4))','hr',l,(hCST(i,k,l),i=1,nscc(k))
!   end do

     if(daytype.eq.1 .or. daytype.ge.6) then !lunes, Sabado y Domingo
        inquire(19,opened=fil2)
        if(.not.fil2) then
            open(unit=19,file=nfilep,status='OLD',action='read')
        else
            rewind(19)
        end if
       read (19,'(A)') cdum
      do
          read(19,*,END=240)jscc,(itfrc(l),l=1,25)
        fds:  do i=1,nscc(k)
            call adecua(profile(3,i,k),daytype,perfil)
            if(jscc.eq.perfil) then
            m=4-iverano
            do l=1,nh
            if(daytype.eq.1 )then
                if(m+l.gt.nh) then
                 hEST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                end if
            else
                if(m+l.gt.nh) then
                 hEST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                else
                 hEST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
                end if
            end if  ! daytype
            end do
            m=5-iverano
            do l=1,nh
            if(daytype.eq.1) then
                if(m+l.gt.nh) then
                 hCST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                end if
            else
                if(m+l.gt.nh) then
                 hCST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                else
                 hCST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
                end if
            end if !daytype
            end do
            m=6-iverano
            do l=1,nh
            if(daytype.eq.1) then
                if(m+l.gt.nh) then
                 hMST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                end if
            else
                if(m+l.gt.nh) then
                 hMST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                else
                 hMST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
                end if
            end if !daytype
            end do
            m=7-iverano
            do l=1,nh
            if(daytype.eq.1 )then
                if(m+l.gt.nh) then
                 hPST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                end if
            else
                if(m+l.gt.nh) then
                 hPST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                else
                 hPST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
                end if
            end if ! daytype
            end do
            !exit fds
            end if
          end do fds!i
        end do ! File 19
    240 continue
!
!      do l=1,nh
!        print '("Hr",x,I3,x,<nscc(k)>(f7.4))', l,(hCST(i,k,l),i=1,nscc(k))
!      end do
!
    end if
	 print *,'   Done ',nfile,daytype
	end do ! K
	close(15)
	close(16)
	close(17)
	close(18)
    close(19)
end subroutine lee
!
subroutine compute
	implicit none
	integer i,j,k,l,ival,ii
!
    call count ! computes the number of different cells
!
! For inorganics
!
     mes=mes*fweek! weeks per month

	do k=1,nf-2
	  do i=1,nm
		  do l=1,nh
	        do j=1,nscc(k)
    if(mst(i).eq.5) emis(i,k,l)=emis(i,k,l)+emiM(i,j,k)*mes(j,k)*hEST(j,k,l)
    if(mst(i).eq.6) emis(i,k,l)=emis(i,k,l)+emiM(i,j,k)*mes(j,k)*hCST(j,k,l)
    if(mst(i).eq.7) emis(i,k,l)=emis(i,k,l)+emiM(i,j,k)*mes(j,k)*hMST(j,k,l)
    if(mst(i).eq.8) emis(i,k,l)=emis(i,k,l)+emiM(i,j,k)*mes(j,k)*hPST(j,k,l)
		    end do
		  end do
	  end do
	end do
!  For PM2.5
!  
	k=nf-1
   do i=1,nm
		  do l=1,nh
	        do j=1,nscc(k)
    if(mst(i).eq.5) epm2(i,j,l)=epm2(i,j,l)+emiM(i,j,k)*mes(j,k)*hEST(j,k,l)
    if(mst(i).eq.6) epm2(i,j,l)=epm2(i,j,l)+emiM(i,j,k)*mes(j,k)*hCST(j,k,l)
    if(mst(i).eq.7) epm2(i,j,l)=epm2(i,j,l)+emiM(i,j,k)*mes(j,k)*hMST(j,k,l)
    if(mst(i).eq.8) epm2(i,j,l)=epm2(i,j,l)+emiM(i,j,k)*mes(j,k)*hPST(j,k,l)
		    end do
		  end do
	  end do
!  For VOCs
!  
	k=nf
   do i=1,nm
		  do l=1,nh
	        do j=1,nscc(k)
    if(mst(i).eq.5) evoc(i,j,l)=evoc(i,j,l)+emiM(i,j,nf)*mes(j,nf)*hEST(j,nf,l)
    if(mst(i).eq.6) evoc(i,j,l)=evoc(i,j,l)+emiM(i,j,nf)*mes(j,nf)*hCST(j,nf,l)
    if(mst(i).eq.7) evoc(i,j,l)=evoc(i,j,l)+emiM(i,j,nf)*mes(j,nf)*hMST(j,nf,l)
    if(mst(i).eq.8) evoc(i,j,l)=evoc(i,j,l)+emiM(i,j,nf)*mes(j,nf)*hPST(j,nf,l)
		    end do
		  end do
	  end do
	end subroutine compute
!
subroutine storage
  implicit none
  integer i,j,k,l
  character(len=3):: cdia(7)
  data cdia/'MON','TUE','WND','THR','FRD','SAT','SUN'/

  do k=1,nf-2
   print *,'Storing: ',casn(k),' ',efile(k)
   open(unit=10,file=casn(k),action='write')
   write(10,*)casn(k),',ID, Hr to Hr24,g/h'
   write(10,'(I8,4A)')size(emis,dim=1),",",current_date,', ',cdia(daytype)
   do i=1,size(emis,dim=1)
     write(10,100)idcel2(i),(emis(i,k,l),l=1,nh)
   end do
   close(unit=10)
  end do
100 format(I7,",",23(ES12.4,","),ES12.4)
   k=nf-1
! WARNING iscc voc must be the last one to be read.
    print *,casn(k),efile(k)
   open(unit=10,file=casn(k),action='write')
   write(10,*)casn(k),'ID, SCC,  Hr to Hr24'
   write(10,'(I8,4A)')size(epm2,dim=1)*nscc(k),', ',current_date,', ',cdia(daytype)
   do i=1,size(epm2,dim=1)
     do j=1,nscc(k)
     write(10,110)idcel2(i),iscc(j),(epm2(i,j,l),l=1,nh)
     end do
   end do
	close(10)
! WARNING iscc voc must be the last one to be read.
   k=nf
    print *,casn(k),efile(k)
   open(unit=10,file=casn(k),action='write')
   write(10,*)casn(k),'ID, SCC,  Hr to Hr24'
   write(10,'(I8,4A)')size(evoc,dim=1)*nscc(k),', ',current_date,', ',cdia(daytype)
   do i=1,size(evoc,dim=1)
     do j=1,nscc(k)
     write(10,110)idcel2(i),iscc(j),(evoc(i,j,l),l=1,nh)
     end do
   end do
	close(10)
    print*,"*****  DONE MOBILE TEMPORAL *****"
110 format(I7,",",I10,",",23(ES12.4,","),ES12.4)
end subroutine storage
subroutine count
  integer i,j
  idcel2(1)=idcel(1)
  j=1
  do i=2,nm
   if(idcel2(j).ne.idcel(i)) then
    j=j+1
	idcel2(j)=idcel(i)
	end if
	 
  end do
  print *,'Number of different cells',j
  allocate(emis(j,nf-2,nh))
  allocate(evoc(j,nscc(nf),nh))
  allocate(epm2(j,nscc(nf-1),nh))
   emis=0
   evoc=0
   emp2=0
end subroutine count
subroutine adecua(perfili,idia,perfilo)
implicit none
integer, INTENT(IN)  :: perfili,idia
integer, INTENT(OUT) :: perfilo

if (perfili.eq.2013) then; perfilo=perfili+(idia-1)*100
else;perfilo=perfili;end if

end subroutine adecua
!  _  ____   _____ ___    _   _  _  ___
! | |/ /\ \ / / __| _ \  /_\ | \| |/ _ \
! | ' <  \ V /| _||   / / _ \| .` | (_) |
! |_|\_\  \_/ |___|_|_\/_/ \_\_|\_|\___/
!
integer function kverano(ida,mes)
    implicit none
    integer, intent(in):: ida,mes

    if (mes.lt.4  .or. mes .gt.10)then
      kverano = 0
      return
    end if
    if (mes.gt.4 .and. mes .lt.10) then
      kverano = 1
      write(6, 233)
      return
    end if
    if (mes.eq.4 .and. ida .ge. 6) then
      kverano = 1
      write(6, 233)
      return
      elseif (mes.eq.10 .and. ida .le. 26) then
        kverano = 1
        write(6, 233)
        return
      else
        kverano =0
      return
    end if
233 format("******  HORARIO de VERANO *******")
end function
end program atemporal
