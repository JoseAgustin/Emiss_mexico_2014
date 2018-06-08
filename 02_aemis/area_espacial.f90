!
!	area_espacial.f90
!	
!
!	Created by Agustin on 14/08/12.
!	Copyright 2012 CCA-UNAM. All rights reserved.
!
!  Reads lan use fracction per cell and land use tyepe and converts
!  to a one line.
!  ifort -o ASpatial.exe -O3 area_espacial.f90
!
!  4/03/2015  Correction in Terminasl 2801500002 and agricultural fires 2801500250
!  8/07/2017  For 2014 from 57 to 58 categories (ladrilleras), 2457 Municipalidades
!
module land
    integer nl,nf,nm,nnscc,edo, mun
    parameter (nm=2457,nf=10,nnscc=58)
    integer,allocatable :: grib(:),idb(:)  ! Bosque
    integer,allocatable :: gria(:),ida(:)  ! Agricola
    integer,allocatable :: grip(:),idp(:)  ! Poblacion
    integer,allocatable :: grie(:),ide(:)  ! Aeropuertos
    integer,allocatable :: griu(:),idu(:)  ! Centrales Autobuses
    integer,allocatable :: grim(:),idm(:)  ! Puertos Maritimos
    integer,allocatable :: grit(:),idt(:)  ! Ferrocarriles
    integer,allocatable :: grir(:),idr(:)  ! Terraceria
    integer,allocatable :: griv(:),idv(:)  ! Vialidades

    integer,dimension(nf) :: nscc
    integer,dimension (nf,nm):: iem
    real,allocatable ::fb(:),fa(:)! Fracciones Bosque Agricola
    real,allocatable ::fe(:),fu(:),fm(:),ft(:)! Fracciones Aerop, C.A., P.M FFCC
    real,allocatable ::fr(:),fv(:)    !  Fracciones de terraceria y vialidades
    real,allocatable ::fp1(:),fp2(:),fp3(:)!Fracc Urbana1, Rural2 y total3
!   Emisiones fuentes agricolas, bosques y poblacion grid, n, nnscc
    real,allocatable :: eagr(:,:,:), ebos(:,:,:), epob(:,:,:)
!   Emisiones fuentes aeropuertos, central, puertos grid, n, nnscc
    real,allocatable :: eaer(:,:,:),ecen(:,:,:), epue(:,:,:),etre(:,:,:)
    real,allocatable :: eter(:,:,:),evia(:,:,:)
    real,dimension(nm,nnscc,nf):: emiss
    character(len=10),dimension(nf,nnscc) ::scc
    character(len=25), allocatable :: desc(:)
    character(len=14),dimension(nf) ::efile,ofile
!   Emissions Inventory files
    data efile /'INH3_2014.csv','INOx_2014.csv','ISO2_2014.csv',&
&           'IVOC_2014.csv','ICO__2014.csv','IPM10_2014.csv',&
&           'IPM25_2014.csv','ICO2_2014.csv','IBC__2014.csv',&
&           'imet__2014.csv'/
!            NH3          NO2         SO2    VOC    CO PM10 PM25
    data ofile /'ANH3_2014.csv','ANOx_2014.csv','ASO2_2014.csv',&
&           'AVOC_2014.csv','ACO__2014.csv','APM10_2014.csv',&
&          'APM25_2014.csv','ACO2_2014.csv','ACN__2014.csv', &
&          'ACH4_2014.csv'/
end module land

program area_espacial
use land
       call lee

       call calculos

       call guarda

contains

subroutine lee
implicit none
    integer i,j,k
    character(len=14):: cdum,fname
    fname='bosque.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    nl=0
    do
        read(10,*,end=100) cdum
        nl=nl+1
    end do
100 print *,'numero de lineas',nl
    allocate(grib(nl),idb(nl),fb(nl))
    rewind(10)
    read (10,*) cdum
    do i=1,nl
        read(10,*)grib(i),idb(i),fb(i)
    end do
    close(10)
!
    fname='agricola.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    nl=0
    do
    read(10,*,end=110) cdum
    nl=nl+1
    end do
110 print *,'numero de lineas',nl
    allocate(gria(nl),ida(nl),fa(nl))
    rewind(10)
    read (10,*) cdum
    do i=1,nl
    read(10,*)gria(i),ida(i),fa(i)
    end do
    close(10)
!
    fname='gri_pob.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    read (10,*) cdum
    nl=0
    do
        read(10,*,end=120) cdum
        nl=nl+1
    end do
120 print *,'numero de lineas',nl
!    Population fraction fp1 furb, fp2 frural, fp3 fpob
    allocate(grip(nl),idp(nl),fp1(nl),fp2(nl),fp3(nl))
    rewind(10)
    read (10,*) cdum
    read (10,*) cdum
    do i=1,nl
        read(10,*)grip(i),idp(i),fp1(i),fp2(i),fp3(i)
        ! GRIDCODE ID urb,frural,fpob
    end do
    close(10)
!
    fname='aeropuerto.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    nl=0
    do
    read(10,*,end=130) cdum
    nl=nl+1
    end do
130 print *,'numero de lineas',nl
    allocate(grie(nl),ide(nl),fe(nl))
    rewind(10)
    read (10,*) cdum
    do i=1,nl
    read(10,*)grie(i),ide(i),fe(i)
    end do
    close(10)
!
    fname='centrales.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    nl=0
    do
        read(10,*,end=140) cdum
        nl=nl+1
    end do
140 print *,'numero de lineas',nl
    allocate(griu(nl),idu(nl),fu(nl))
    rewind(10)
    read (10,*) cdum
    do i=1,nl
        read(10,*)griu(i),idu(i),fu(i)
    end do
    close(10)
!
    fname='puertos.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    nl=0
    do
        read(10,*,end=150) cdum
        nl=nl+1
    end do
150 print *,'numero de lineas',nl
    allocate(grim(nl),idm(nl),fm(nl))
    rewind(10)
    read (10,*) cdum
    do i=1,nl
        read(10,*)grim(i),idm(i),fm(i)
    end do
    close(10)
!
    fname='ffcc.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    nl=0
    do
        read(10,*,end=160) cdum
    nl=nl+1
    end do
160 print *,'numero de lineas',nl
    allocate(grit(nl),idt(nl),ft(nl))
    rewind(10)
    read (10,*) cdum
    do i=1,nl
        read(10,*)grit(i),idt(i),ft(i)
    end do
    close(10)
!
    fname='gri_ter.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    nl=0
    do
    read(10,*,end=170) cdum
    nl=nl+1
    end do
170 print *,'numero de lineas',nl
    allocate(grir(nl),idr(nl),fr(nl))
    rewind(10)
    read (10,*) cdum
    do i=1,nl
    read(10,*)grir(i),idr(i),fr(i)
    end do
    close(10)
    !
    fname='gri_pav.csv'
    print *,'Lee ',fname
    open(unit=10,file=fname,status='OLD',action='read')
    read (10,*) cdum
    nl=0
    do
    read(10,*,end=180) cdum
    nl=nl+1
    end do
180 print *,'numero de lineas',nl
    allocate(griv(nl),idv(nl),fv(nl))
    rewind(10)
    read (10,*) cdum
    do i=1,nl
    read(10,*)griv(i),idv(i),fv(i)
    end do
    close(10)
!
    do k=1,nf
        open (unit=10,file=efile(k),status='OLD',action='read')
        read (10,'(A)') cdum
        read (10,'(A)') cdum
        print *,efile(k)
        read (10,*) nscc(k),cdum,(scc(k,i),i=1,nscc(k))
        print '(5(A10,x))',(scc(k,i),i=1,nscc(k))
        print *,k,nscc(k)
        do i=1,nm
          read(10,*) edo,mun,iem(k,i),(emiss(i,j,k),j=1,nscc(k))
       end do ! i
        close(10)
    end do! k
end subroutine lee
subroutine calculos
    implicit none
    integer i,j,k,l,m
	allocate(eagr(size(gria),nf,nnscc))
	allocate(ebos(size(grib),nf,nnscc))
    allocate(epob(size(grip),nf,nnscc))
    allocate(eaer(size(grie),nf,nnscc))
    allocate(ecen(size(griu),nf,nnscc))
    allocate(epue(size(grim),nf,nnscc))
    allocate(etre(size(grit),nf,nnscc))
    allocate(eter(size(grir),nf,nnscc))
    allocate(evia(size(griv),nf,nnscc))
    eagr=0.0
    ebos=0.0
    epob=0.0
    eaer=0.0
    ecen=0.0
    epue=0.0
    etre=0.0
    eter=0.0
    evia=0.0
    print *," Inicia Calculos"
    Clase: do k=1,nf
    print *,"     Agricola  ", efile(k)
    agricola: do j=1,size(fa) ! grid
    inven: do i=1,nm          ! municipality
        if(ida(j).eq.iem(k,i)) then
           do l=1,nscc(k)     ! SCC
    if(scc(k,l).eq.'2801500100') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Qmas_agricolas
    if(scc(k,l).eq.'2801000002') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Labranza Siemb
    if(scc(k,l).eq.'2267005000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Comb_agricola_LPG
    if(scc(k,l).eq.'2801700000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Fertilizantes
    if(scc(k,l).eq.'2270005000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Comb_agricola_Diesel
    if(scc(k,l).eq.'2805020000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Ganaderas
    if(scc(k,l).eq.'2461850000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Pesticidas
    if(scc(k,l).eq.'2801000005') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Labranza_Cose
           end do
           exit inven
        end if
    end do inven
    end do agricola
!   Aeropuertos
    print *,"     Aeropuertos"
   aerop: do j=1,size(fe) !grid
    invee: do i=1,nm          ! municipality
       if(ide(j).eq.iem(k,i)) then
           do l=1,nscc(k)
            if(scc(k,l).eq.'2275000000') eaer(j,k,l)=emiss(i,l,k)*fe(j)*1e6 !Aviacion
            if(scc(k,l).eq.'2275050000') eaer(j,k,l)=emiss(i,l,k)*fe(j)*1e6 !Equipo basico aeropouertos
           end do
          exit invee
       end if
    end do invee
   end do aerop
!   C Caminoneras
    print *,"     Central Caminoneras"
    ccam: do j=1,size(fu) !grid
        invcc: do i=1,nm          ! municipality
        if(idu(j).eq.iem(k,i)) then
            do l=1,nscc(k)
                if(scc(k,l).eq.'2230070310') ecen(j,k,l)=emiss(i,l,k)*fu(j)*1e6 !Terminal Buses
                if(scc(k,l).eq.'2270008010') ecen(j,k,l)=emiss(i,l,k)*fu(j)*1e6 !Terminales de autobuses
            end do
            exit invcc
            end if
        end do invcc
    end do ccam
!   Puertos Maritimos
    print *,"     Puertos Maritimos"
    pmar: do j=1,size(fm) !grid
        invpm: do i=1,nm          ! municipality
        if(idm(j).eq.iem(k,i)) then
            do l=1,nscc(k)
                if(scc(k,l).eq.'2280000000') epue(j,k,l)=emiss(i,l,k)*fm(j)*1e6 !Embarcaciones marinas
            end do
            exit invpm
        end if
        end do invpm
    end do pmar
!   Puertos Maritimos
    print *,"     Ferrocarriles"
    pffc: do j=1,size(ft) !grid
        invfcc: do i=1,nm          ! municipality
        if(idm(j).eq.iem(k,i)) then
        do l=1,nscc(k)
            if(scc(k,l).eq.'2285000000') etre(j,k,l)=emiss(i,l,k)*ft(j)*1e6 !Locomotoras de arrastre
            if(scc(k,l).eq.'2285002010') etre(j,k,l)=emiss(i,l,k)*ft(j)*1e6 !Locomotoras de patio
        end do
        exit invfcc
        end if
        end do invfcc
    end do pffc
    print *,"     Bosque"
    Bosque: do j=1,size(fb) ! grid
    invenb: do i=1,nm       ! municipality
        if(idb(j).eq.iem(k,i)) then
           do l=1,nscc(k)      ! SCC
             if(scc(k,l).eq.'2810001000') ebos(j,k,l)=emiss(i,l,k)*fb(j)*1e6 ! conversion de Mg to g.
           end do
           exit invenb
        end if
    end do invenb
    end do Bosque
    print *,"     Terraceria"
    Terrac: do j=1,size(fr) ! grid
    invenr: do i=1,nm       ! municipality
        if(idr(j).eq.iem(k,i)) then
        do l=1,nscc(k)      ! SCC
   !         if(scc(k,l).eq.'2296000000') eter(j,k,l)=emiss(i,l,k)*fr(j)*1e6  ! Caminos Terra
        end do
        exit invenr
        end if
    end do invenr
    end do Terrac
    print *,"     Vialidades"
    Vialid: do j=1,size(fv) ! grid
    invenv: do i=1,nm       ! municipality
        if(idv(j).eq.iem(k,i)) then
        do l=1,nscc(k)      ! SCC
            if(scc(k,l).eq.'2461020000') evia(j,k,l)=emiss(i,l,k)*fv(j)*1e6  ! Asfaltado
            if(scc(k,l).eq.'2401008000') evia(j,k,l)=emiss(i,l,k)*fv(j)*1e6  ! Senializacion
        end do
        exit invenv
        end if
    end do invenv
    end do Vialid
        print *,"     Poblacion"
    poblacion: do j=1,size(grip)! grid
    invenp: do i=1,nm       ! municipality
        if(idp(j).eq.iem(k,i)) then
            do l=1,nscc(k)
    if(scc(k,l).eq.'2104007000') epob(j,k,l)=emiss(i,l,k)*(fp2(j)*0.2+fp1(j)*0.8)*1e6!Comb_res_LPG
    if(scc(k,l).eq.'30500302') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1e6  ! Ladrillera
    if(scc(k,l).eq.'2102004000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_ind_Diesel
    if(scc(k,l).eq.'2102006000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_ind_NG
    if(scc(k,l).eq.'2102007000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_ind_LPG
    if(scc(k,l).eq.'2103006000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_comer_NG
    if(scc(k,l).eq.'2103007000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_comer_LPG
    if(scc(k,l).eq.'2104006000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_res_GN
    if(scc(k,l).eq.'2104008000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1e6  ! Comb_res_lena
    if(scc(k,l).eq.'2104011000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1e6  ! Comb_res_keroseno
    if(scc(k,l).eq.'2222222222') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Cruces_front
    if(scc(k,l).eq.'2302002000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Asados al C
    if(scc(k,l).eq.'2302050000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Panificacion
    if(scc(k,l).eq.'2311010000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Construccion
    if(scc(k,l).eq.'2401001000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Recub Arq
    if(scc(k,l).eq.'2401005000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Pintado automotriz
    if(scc(k,l).eq.'2401065000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Fab Acceso Elec
    if(scc(k,l).eq.'2401080000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Fab eq transport
    if(scc(k,l).eq.'2401020000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Muebles Colc
    if(scc(k,l).eq.'2401990000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Otr Ind Manuf
    if(scc(k,l).eq.'2415000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! limpieza_Sup_ind
    if(scc(k,l).eq.'2415010000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Ind met basc
    if(scc(k,l).eq.'2420000055') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Lavado en seco
    if(scc(k,l).eq.'2425000000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Serigrafia
    if(scc(k,l).eq.'2425010000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Offset
    if(scc(k,l).eq.'2425030000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Rotograbado
    if(scc(k,l).eq.'2425040000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Flexografia
    if(scc(k,l).eq.'2465000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Productos en aerosol
    if(scc(k,l).eq.'2465100000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Productos_personal
    if(scc(k,l).eq.'2465200000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Productos dom_sticos
    if(scc(k,l).eq.'2465400000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Productos de cuidado automotriz
    if(scc(k,l).eq.'2465600000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Adhesivos y selladores
    if(scc(k,l).eq.'2465800000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Pesticidas comerciales y domesticos
    if(scc(k,l).eq.'2465900000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Productos miscelaneos
    if(scc(k,l).eq.'2501060000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Dist Comb
    if(scc(k,l).eq.'2610000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Qmas Cielo Ab
    if(scc(k,l).eq.'2630030000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Aguas Resd
    if(scc(k,l).eq.'2810030000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Incend_cosntruc
    if(scc(k,l).eq.'2850000010') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Esteri Hosp
    if(scc(k,l).eq.'3333333333') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Dist_LPG
    if(scc(k,l).eq.'5555555555') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Uso_domestico
    if(scc(k,l).eq.'2296000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6
     end do
            exit invenp
        end if
    end do invenp
    end do poblacion
    end do Clase
end subroutine calculos
subroutine guarda
    implicit none
    integer i,k,l
    real suma
    Print *,"   ***   Guarda   ***"
    do k=1,nf
        open(unit=10,file=ofile(k),ACTION='write')
        write(10,*)'grid,CID,Furb,Frural,SCCs'
        write(10,300)nscc(k),(scc(k,i),i=1,nscc(k))
        print *,"   Agricola ",ofile(k)
        do i=1,size(fa)
            suma=0
            do l=1,nscc(k)
               suma=suma+eagr(i,k,l)
            end do
            if (suma.gt.0) &
&           write(10,310) gria(i),ida(i),0,fa(i),(eagr(i,k,l),l=1,nscc(k))
        end do
        print *,"   Bosque"
        do i=1,size(fb)
            suma=0
            do l=1,nscc(k)
            suma=suma+ebos(i,k,l)
            end do
            if (suma.gt.0) &
&            write(10,310) grib(i),idb(i),0,fb(i),(ebos(i,k,l),l=1,nscc(k))
        end do
        print *,"   Poblacion"
        do i=1,size(fp1)
           suma=0
            do l=1,nscc(k)
            suma=suma+epob(i,k,l)
            end do
            if (suma.gt.0) &
&            write(10,310) grip(i),idp(i),fp1(i),fp2(i),(epob(i,k,l),l=1,nscc(k))
        end do
        print *,"   Aeropuerto"
        do i=1,size(fe)
            suma=0
            do l=1,nscc(k)
            suma=suma+eaer(i,k,l)
            end do
            if (suma.gt.0) &
&        write(10,310) grie(i),ide(i),fe(i),0,(eaer(i,k,l),l=1,nscc(k))
        end do
        print *,"   Centrales Autobuses"
        do i=1,size(fu)
            suma=0
            do l=1,nscc(k)
            suma=suma+ecen(i,k,l)
            end do
            if (suma.gt.0) &
&        write(10,310) griu(i),idu(i),fu(i),0,(ecen(i,k,l),l=1,nscc(k))
        end do
        print *,"   Puertos Maritimos"
        do i=1,size(fm)
            suma=0
            do l=1,nscc(k)
            suma=suma+epue(i,k,l)
            end do
            if (suma.gt.0) &
&        write(10,310) grim(i),idm(i),fm(i),0,(epue(i,k,l),l=1,nscc(k))
        end do
        print *,"   Ferrocarriles"
        do i=1,size(ft)
            suma=0
            do l=1,nscc(k)
            suma=suma+etre(i,k,l)
            end do
            if (suma.gt.0) &
&          write(10,310) grit(i),idt(i),ft(i),0,(etre(i,k,l),l=1,nscc(k))
        end do
    print *,"   Terraceria"
    do i=1,size(fr)
        suma=0
        do l=1,nscc(k)
        suma=suma+eter(i,k,l)
        end do
        if (suma.gt.0) &
&        write(10,310) grir(i),idr(i),fr(i),0,(eter(i,k,l),l=1,nscc(k))
        end do
    print *,"   Vialidades"
    do i=1,size(fv)
        suma=0
        do l=1,nscc(k)
        suma=suma+evia(i,k,l)
        end do
        if (suma.gt.0) &
&        write(10,310) griv(i),idv(i),fv(i),0,(evia(i,k,l),l=1,nscc(k))
        end do
    close(10)
    end do
300 format(I3,", g_per_year",<nnscc>(",",A10))
310 format(I9,",",I6,",",F,",",F,<nnscc>(",",ES12.5))
end subroutine guarda
end program area_espacial

