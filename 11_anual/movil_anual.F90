!  Creado por Jose Agustin Garcia Reynoso el 25/05/12.
!
! Proposito
!          Lectura de emisiones de fuentes moviles y creacion de archivo netcdf
!   ifort -O3 movil_anual.F90 -o Mtemporal.exe
!
!> @brief For movil_anual.F90 program. Annual mobile emissions storage in CAMS
!> categories
!> Currently uses EPA temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/26/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
module movil_annual_mod
!> type day (1=Mon, 2= Tue, ... 7=Sun)
integer :: daytype ! tipo de dia 1 lun a 7 dom
!> Hourly temporal profile
integer :: perfil  ;!> number of pollutant emission files
integer,parameter :: nf=11    ;!> max number of SCC IDs in input files
integer,parameter :: nnscc=36 ;!> CAMS categories number
integer,parameter :: ncams=16  ; !> Pollutant ID in temporl profile  CO,NO,VOC, VOC Diesel, SO2
integer,parameter :: ispc=5  ; !> Type number of vehicle  1 all, 2 automobile gas
integer,parameter :: v_type=9 ;;!> stores gricode value from localiza.csv
integer*8,allocatable :: idcg(:);!> Emission by cel,file (pollutant) and idCAMS
real,allocatable :: emis(:,:,:);!> longitudes in output file from localiza
real,allocatable :: xlon(:,:)  ;!> latitudes in output file from localiza
real,allocatable :: xlat(:,:) ; !> population in output file from localiza
real,allocatable :: pob(:,:) ;!> UTMx coordinates in output file from localiza
real,allocatable :: utmxd(:,:) ;!> UTMy coordinates in output file from localiza
real,allocatable :: utmyd(:,:) ;!> temporal array for storage
real,allocatable:: aguardar(:,:) ;!> UTM Z zone
integer, allocatable:: utmzd(:,:) ;!> cell dimension CDIM m
real :: CDIM ;!> grid 1/area  (m^-2)
real :: SUPF1 ;!> longitude values in grid
integer :: nx    ;!> latitude values in grid
integer :: ny    ;
!> number of emissions file
integer :: nm ! grid number in emissions file
!> number of scc codes per file
integer,dimension(nf) :: nscc    ;!> GRIDID in emissions
integer*8, allocatable :: idcel(:) ;!> Difference in number of hours (CST, PST, MST)
integer, allocatable :: mst(:)   ;!> fraction = weeks/month days
real :: fweek                    ;!> Mobile emisions from files cel,ssc,file
real,allocatable :: emiM(:,:,:)   ;!> Emission by cel,file and hour (inorganic)
!> SCC codes per file
character (len=10),dimension(nnscc,nf) ::iscc ;!> SCC codes from Temporal Profiles files
character(len=10),dimension(v_type,1)::cscc
!> Initial date of the emissions period
character (len=19) :: current_date='2014-01-01_00:00:00';!> output file name
character(len=14),dimension(nf) :: efile
character(len=27),dimension(nf) :: casn  ;!> Categories in CAMS
character(len=3),dimension(ncams) :: idCAMS;!> IPCC classification
character(len=4),dimension(ncams) :: idIPCC;!> Pollutant abreviation
character(len=4),dimension(nf)    :: ename ;!> Long name description
character(len=77),dimension(ncams):: cname ;!> Variable description
character(len=66),dimension(nf):: long_nm

character (len=40) :: titulo

 data efile / 'M_CO.csv' ,'M_NH3.csv','M_NO2.csv','M_NO.csv',&
              'M_SO2.csv','M_CN.csv' ,'M_CO2.csv','M_CH4.csv',&
              'M_PM10.csv',&
              'M_PM25.csv','M_VOC.csv'/

data casn /'MEX_MOBILE_9km_2014_CO.nc  ','MEX_MOBILE_9km_2014_NH3.nc ',&
           'MEX_MOBILE_9km_2014_NO2.nc ','MEX_MOBILE_9km_2014_NO.nc  ',&
           'MEX_MOBILE_9km_2014_SO2.nc ','MEX_MOBILE_9km_2014_BC.nc  ',&
           'MEX_MOBILE_9km_2014_CO2.nc ','MEX_MOBILE_9km_2014_CH4.nc ',&
           'MEX_MOBILE_9km_2014_PM10.nc','MEX_MOBILE_9km_2014_PM25.nc',&
           'MEX_MOBILE_9km_2014_VOC.nc '/
data idCAMS/'AGL','AGS','AWB','COM','ENE','FEF','IND','NAT','REF',&
'RES','SHP','SLV','SWD','TNR','TRO','WFR'/
data idIPCC/'3A ','3C  ','3C1 ','1A4a','1A1 ','1B2a','1A2 ','9999','1A2 ',&
'1A4 ','1A3d','2D3 ','4   ','1A4 ','1A3b','4A1b'/
data ename/'CO  ','NH3 ','NO2 ','NO  ','SO2 ','BC  ','CO2 ','CH4 ','PM10',&
           'PM25','VOC '/
data cname / &
'AGL: Agriculture_livestock                                                   ',&
'AGS: Agricultural_soils (without fires)                                      ',&
'AWB: Agricultural waste burning                                              ',&
'COM: Commercial buildings                                                    ',&
'ENE: Power_generation                                                        ',&
'FEF: Fugitive_emissions_from_fuels                                           ',&
'IND: Industrial_process (Energy consumption of manufacture industry+ process)',&
'NAT: Natural emissions                                                       ',&
'REF: refineries                                                              ',&
'RES: Residential_commercial_and_other_combustion                             ',&
'SHP: Navigation                                                              ',&
'SLV: Solvents                                                                ',&
'SWD: Solid_waste_and_waste_water                                             ',&
'TNR: Non-road_transportation                                                 ',&
'TRO: Road_transportation                                                     ',&
'WRF: Wildfires                                                               '/
data long_nm/&
'surface_upward_mass_flux_of_carbon_monoxide                       ',&
'surface_upward_mass_flux_of_ammonia                               ',&
'surface_upward_mass_flux_of_nitrogen_dioxide                      ',&
'surface_upward_mass_flux_of_nitrogen_monoxide                     ',&
'surface_upward_mass_flux_of_sulfur_dioxide                        ',&
'surface_upward_mass_flux_of_black_carbon                          ',&
'surface_upward_mass_flux_of_carbon_dioxide                        ',&
'surface_upward_mass_flux_of_methane                               ',&
'surface_upward_mass_flux_of_pm10_ambient_aerosol_particles_in_air ',&
'surface_upward_mass_flux_of_pm2p5_ambient_aerosol_particles_in_air',&
'surface_upward_mass_flux_of_nmvoc                                 '/
common /vars/ fweek,nscc,nm,daytype,perfil,current_date
common /emi_vars/ efile,casn,iscc
common /domain/ nx,ny,CDIM,SUPF1
common /leenetcdf/profile,tlon,tlat,t_prof_m,cscc
common /texto/ titulo,idCAMS,idIPCC,ename,cname,long_nm

end module movil_annual_mod
!                        _ _    _                                       _
!  _ __ ___   _____   _(_) |  | |_ ___ _ __ ___  _ __   ___  _ __ __ _| |
! | '_ ` _ \ / _ \ \ / / | |  | __/ _ \ '_ ` _ \| '_ \ / _ \| '__/ _` | |
! | | | | | | (_) \ V /| | |  | ||  __/ | | | | | |_) | (_) | | | (_| | |
! |_| |_| |_|\___/ \_/ |_|_|___\__\___|_| |_| |_| .__/ \___/|_|  \__,_|_|
!                        |_____|               |_|
!  Progran  movil_anual.F90
!
!  Make the temporal distribution of mobile emissions
!
!>  @brief Make temporal distribution of mobile emissions using profiles based on SCC.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program movil_annual
   use master
   use movil_annual_mod

    call lee_namelist

    call lee_localiza

    call mobile_annual_reading

    call mobile_sorting

    call movil_annual_storing

    call libera_memoria

contains
!                  _     _ _                                       _
!  _ __ ___   ___ | |__ (_) | ___     __ _ _ __  _ __  _   _  __ _| |
! | '_ ` _ \ / _ \| '_ \| | |/ _ \   / _` | '_ \| '_ \| | | |/ _` | |
! | | | | | | (_) | |_) | | |  __/  | (_| | | | | | | | |_| | (_| | |
! |_| |_| |_|\___/|_.__/|_|_|\___|___\__,_|_| |_|_| |_|\__,_|\__,_|_|
!       _ __ ___  __ _  __| (_)_|_____|_ _
!      | '__/ _ \/ _` |/ _` | | '_ \ / _` |
!      | | |  __/ (_| | (_| | | | | | (_| |
!  ____|_|  \___|\__,_|\__,_|_|_| |_|\__, |
! |_____|                            |___/
!>  @brief Read annual emissions files.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/26/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine mobile_annual_reading
    implicit none
    integer i,j,k
    character(len=4):: cdum
!
    do k=1,nf  ! per pollutant
        open (unit=10,file="../05_semisM/"//efile(k),status='OLD',action='read')
        read (10,'(A)') cdum
        read (10,*) nscc(k),(iscc(i,k),i=1,nscc(k))
        !print '(5(I10,x))',(iscc(i,k),i=1,nscc(k))
        !print *,cdum,nscc(k)
        nm=0
        do
            read(10,*,end=100) cdum
            nm=nm+1
        end do
        100     continue
        !print *,nm
        rewind(10)
        if(k.eq.1) then
            allocate(idcel(nm),mst(nm))
            allocate(emiM(nm,nnscc,nf))
            allocate(emis(nm,ncams,nf))
        end if
        read (10,'(A)') cdum
        read (10,'(A)') cdum
        do i=1,nm
            read(10,*) idcel(i),(emiM(i,j,k),j=1,nscc(k)),mst(i)
            !print *,i,idcel(i),(emiM(i,j,k),j=1,nscc(k)),mst(i)
        end do !i
        close(10)
    end do !k
    print *,"Done reading: ",efile(k)
end subroutine mobile_annual_reading
!
!                  _     _ _                         _   _
!  _ __ ___   ___ | |__ (_) | ___     ___  ___  _ __| |_(_)_ __   __ _
! | '_ ` _ \ / _ \| '_ \| | |/ _ \   / __|/ _ \| '__| __| | '_ \ / _` |
! | | | | | | (_) | |_) | | |  __/   \__ \ (_) | |  | |_| | | | | (_| |
! |_| |_| |_|\___/|_.__/|_|_|\___|___|___/\___/|_|   \__|_|_| |_|\__, |
!                               |_____|                          |___/
!>  @brief maps the SCC to the CAMS ids.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  06/01/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine mobile_sorting
implicit none
integer j,k,m
character(len=10):: tscc
emis=0
  do k=1,nf
        do m=1,size(emiM,1)
            do j=1,nscc(k)
            if(iscc(j,k).eq.'2230001330') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2230060330') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2201020330') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2230074330') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2201070330') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2201001330') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2230075330') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2201040330') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2230073330') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2201080330') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'220100133V') emis(m, 6,k)=emis(m, 6,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'220108033V') emis(m, 6,k)=emis(m, 6,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'220102033V') emis(m, 6,k)=emis(m, 6,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'220104033V') emis(m, 6,k)=emis(m, 6,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'220107033V') emis(m, 6,k)=emis(m, 6,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2203210080') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2204210080') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2203310080') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2204310080') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2230060234') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2204320080') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2230001000') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2203420080') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2201070270') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2230070270') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2204430080') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2203410080') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2204420080') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2204530080') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
            if(iscc(j,k).eq.'2201070214') emis(m,15,k)=emis(m,15,k)+emiM(m,j,k)
      end do !j
    end do !m
  end do !k
end subroutine mobile_sorting
!  _ _ _                                                            _
! | (_) |__   ___ _ __ __ _     _ __ ___   ___ _ __ ___   ___  _ __(_) __ _
! | | | '_ \ / _ \ '__/ _` |   | '_ ` _ \ / _ \ '_ ` _ \ / _ \| '__| |/ _` |
! | | | |_) |  __/ | | (_| |   | | | | | |  __/ | | | | | (_) | |  | | (_| |
! |_|_|_.__/ \___|_|  \__,_|___|_| |_| |_|\___|_| |_| |_|\___/|_|  |_|\__,_|
!                         |_____|
!>  @brief Deallocates allocated arrays.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine libera_memoria

    if(allocated(idcel))  deallocate(idcel)
    if(allocated(mst))    deallocate(mst)
    if(allocated(emis))   deallocate(emis)
    if(allocated(emiM))   deallocate(emiM)
    if(allocated(idcg))   deallocate(idcg)
    if(allocated(pob))    deallocate(pob)
    if(allocated(xlon))   deallocate(xlon)
    if(allocated(xlat))   deallocate(xlat)
    if(allocated(utmxd))  deallocate(utmxd)
    if(allocated(utmyd))  deallocate(utmyd)
    if(allocated(utmzd))  deallocate(utmzd)
    if(allocated(aguardar))   deallocate(aguardar)
    write(6,170)
    write(6,180)
170 format(7x,"XXXXXX  Released memory    XXXXXX")
180 format(7x,"*****  DONE MOBILE TEMPORAL *****")
end subroutine libera_memoria

!  _               _                 _ _
! | | ___  ___    | | ___   ___ __ _| (_)______ _
! | |/ _ \/ _ \   | |/ _ \ / __/ _` | | |_  / _` |
! | |  __/  __/   | | (_) | (_| (_| | | |/ / (_| |
! |_|\___|\___|___|_|\___/ \___\__,_|_|_/___\__,_|
!            |_____|
!>  @brief Reada the lon,lat and utm coordinates for the output grid
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine lee_localiza
    implicit NONE
    character(len=40) :: flocaliza,cdum
    integer i,j,k,idum,ncel
    flocaliza='../07_puntual/'//'localiza.csv'
    write(6,*)' >>>> Reading file -',flocaliza,' ---------'
    open (unit=10,file=flocaliza,status='old',action='read')
    read (10,*) cdum  !Header
    read (10,*) nx,ny,titulo  ! Dimensions and Title
    ncel=nx*ny
    allocate(idcg(ncel))
    allocate(xlon(nx,ny),xlat(nx,ny),pob(nx,ny))
    allocate(utmxd(nx,ny),utmyd(nx,ny),utmzd(nx,ny))
    allocate(aguardar(nx,ny))
    do j=1,ny
        do i=1,nx
            k=i+(j-1)*nx
            read(10,*) idcg(k),xlon(i,j),xlat(i,j),idum,pob(i,j),utmxd(i,j),utmyd(i,j),utmzd(i,j)
        end do
    end do
    close(10)
    CDIM=(utmxd(2,1)-utmxd(1,1))  ! in meters
    SUPF1=1./(CDIM*CDIM)  !computes  grid area in m^-1
    write(6,'(F8.2,F8.4," ",A30)') CDIM,SUPF1,trim(titulo)
end subroutine lee_localiza


!                       _ _                                  _
!  _ __ ___   _____   _(_) |    __ _ _ __  _ __  _   _  __ _| |
! | '_ ` _ \ / _ \ \ / / | |   / _` | '_ \| '_ \| | | |/ _` | |
! | | | | | | (_) \ V /| | |  | (_| | | | | | | | |_| | (_| | |
! |_| |_| |_|\___/ \_/ |_|_|___\__,_|_| |_|_| |_|\__,_|\__,_|_|
!            _             |_____|
!       ___| |_ ___  _ __(_)_ __   __ _
!      / __| __/ _ \| '__| | '_ \ / _` |
!      \__ \ || (_) | |  | | | | | (_| |
!  ____|___/\__\___/|_|  |_|_| |_|\__, |
! |_____|                         |___/
!>  @brief Saves mobile annual emission in a netcdf.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/26/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine movil_annual_storing
use netcdf
implicit none
integer i,j,k,l,m,iun
integer  ncid
integer, parameter :: NDIMS=6
integer :: dimids2(2),dimids3(3),dimids(2)
integer,dimension(NDIMS):: dim,id_dim
integer :: ncol,pren,ren0
integer :: pcol,col0
;!> array of variables ID
integer :: id_var(ncams)  ;!>netcdf longitude ID in netcdf file
integer :: id_varlong  ;!>netcdf latitude ID in netcdf file
integer :: id_varlat  ;!>netcdf population ID in netcdf file
integer :: id_varpop  ;!>netcdf unlimited variable ID in netcdf file
integer :: id_unlimit ;!>netcdf UTMx coordinate variable ID in netcdf file
integer :: id_utmx ;!>netcdf UTMy coordinate variable ID in netcdf file
integer :: id_utmy ;!>netcdf UTMz coordinate variable ID in netcdf file
integer :: id_utmz
real,dimension(ncams) :: suma
character (len=19),dimension(NDIMS) ::sdim
character (len=83) :: geospatial_bounds
character (len=8)  :: fecha,varname
character (len=13) :: ccdim
character (len=10) :: time
character (len=24) :: hoy

data sdim /"Time               ","DateStrLen         ","west_east          ",&
&            "south_north        ","bottom_top         ","emissions_zdim_stag"/
call date_and_time(fecha,time)
hoy=fecha(7:8)//'-'//fecha(5:6)//'-'//fecha(1:4)//'T'//time(1:2)//':'//time(3:4)&
//':'//time(5:10)//'Z'

print *,"Mobile Emissions Annual saving"
do k=1,nf
    print *,"Output File Initialization ",casn(k)
    call check( nf90_create(path =casn(k),cmode = NF90_NETCDF4,ncid = ncid) )
    dim=(/1,19,nx,ny,1,1/)
    call check( nf90_def_dim(ncid,sdim(1), NF90_UNLIMITED, id_dim(1)) )
    do i=2,NDIMS
        call check( nf90_def_dim(ncid, sdim(i), dim(i), id_dim(i)) )
    end do
    dimids2 = (/id_dim(2),id_dim(1)/)
    dimids3 = (/id_dim(3),id_dim(4),id_dim(1) /)
    dimids  = (/id_dim(3),id_dim(4)/)
!Atributos Globales NF90_GLOBAL
!print *,"Globales"
write(geospatial_bounds,100)"POLYGON ((",minval(xlon),minval(xlat),&
&minval(xlon),maxval(xlat),maxval(xlon), maxval(xlat),maxval(xlon),minval(xlat),"))"
write(ccdim,110) CDIM
call check( nf90_put_att(ncid, NF90_GLOBAL, "id","ME_2014_"//trim(ename(k))))
call check( nf90_put_att(ncid, NF90_GLOBAL, "title","Emissions from criteria pollutants and GHG for 2014"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_bounds",geospatial_bounds))
call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_bounds_crs","EPSG:4326"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_lon_resolution",ccdim))
call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_lat_resolution",ccdim))
call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_lat_min",minval(xlat)))
call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_lon_min",minval(xlon)))
call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_lat_max",maxval(xlat)))
call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_lon_max",maxval(xlon)))
call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_lat_units","degrees_north"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_lon_units","degrees_east"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "standard_parallel",'17.5,29.5'))
call check( nf90_put_att(ncid, NF90_GLOBAL, "grid_mapping_name","lambert_conformal_conic"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "processing_level","Level 1"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "time_coverage_duration","P1Y"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "time_coverage_resolution","P1Y"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "time_coverage_start","2014-01-01T06:00:00Z"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "time_coverage_end","2015-01-01T05:00:00Z"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "license","Freely Distributed"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "naming_authority","atmosfera.unam.mx"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "publisher_name","Agustin Garcia"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "publisher_email","agustin@atmosfera.unam.mx"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "publisher_type","institution"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "publisher_institution","Instituto de Ciencias de la Atmosfera y &
&Cambio Climatico, UNAM, Mexico"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "publisher_url","https://atmosfera.unam.mx"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "creator_url","https://atmosfera.unam.mx"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "standard_name_vocabulary","CF Standard Name Table"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "creator_type","person"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "creator_email","agustin@atmosfera.unam.mx"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "creator_name","Jose Agustin Garcia Reynoso"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "creator_institution","Instituto de Ciencias de la Atmosfera y &
&Cambio Climatico, UNAM, Mexico"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "Conventions","CF-1.6, Standard Name Table v19,ACDD-1.3"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "contributor_role","Investigador"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "contributor_name","Agustin Garcia"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "acknowledgment","ICAyCC-UNAM"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "keywords_vocabulary","CF:NetCDF COARDS Climate and Forecast Standard Names"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "keywords","SO2,NOx,PM10,NH3,NOx,BC,CO,PM2.5"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "summary","National emissions inventory for Mexico 2014 &
produced by SEMARNAT it is converted by DiETE v2 to a model ready EI, this file only contains annual &
emissions for mobile sources"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "program","PAPILA,ICAyCC"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "project","PAPILA"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "institution","SEMARNAT,UNAM"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "source","DiETE v2"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "references","https://doi.org/10.20937/RICA.2018.34.04.07, &
https://www.gob.mx/semarnat/documentos/documentos-del-inventario-nacional-de-emisiones"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "cdm_data_type","Grid"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "date_issued",hoy))
call check( nf90_put_att(ncid, NF90_GLOBAL, "date_created",hoy))
call check( nf90_put_att(ncid, NF90_GLOBAL, "date_modified",hoy))
call check( nf90_put_att(ncid, NF90_GLOBAL, "product_version",1))
call check( nf90_put_att(ncid, NF90_GLOBAL, "date_modified",hoy))
call check( nf90_put_att(ncid, NF90_GLOBAL, "comment","Created with movil_anual.F90 v1.0"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "history","Creation "//fecha))
!  Define las variables
call check( nf90_def_var(ncid, "time", NF90_CHAR, dimids2,id_unlimit ) )
call check( nf90_put_att(ncid, id_unlimit, "units", "years since 2014-01-01T06:00:00Z"))
call check( nf90_put_att(ncid, id_unlimit, "long_name", "Format YYYY-MM-DDTHH:MN:SS"))
call check( nf90_put_att(ncid, id_unlimit, "standard_name", "time"))
call check( nf90_put_att(ncid, id_unlimit, "calendar", "standard"))
call check( nf90_put_att(ncid, id_unlimit, "axis", "T"))
call check( nf90_put_att(ncid, id_unlimit, "comment", "Time dimension in years"))

call check( nf90_def_var(ncid, "lon", NF90_REAL,dimids,id_varlong) )
call check( nf90_def_var(ncid, "lat", NF90_REAL,dimids,id_varlat) )
call check( nf90_put_att(ncid, id_varlong, "long_name", "Longitude"))
call check( nf90_put_att(ncid, id_varlong, "standard_name","longitude") )
call check( nf90_put_att(ncid, id_varlong, "units", "degrees_east"))
call check( nf90_put_att(ncid, id_varlong, "axis", "X"))
call check( nf90_put_att(ncid, id_varlong, "comment", "Grid values for the domain LAMBERT CONFORMAL"))
call check( nf90_put_att(ncid, id_varlat, "long_name", "Latitude"))
call check( nf90_put_att(ncid, id_varlat, "standard_name","latitude") )
call check( nf90_put_att(ncid, id_varlat, "units", "degrees_north"))
call check( nf90_put_att(ncid, id_varlat, "axis", "Y"))
call check( nf90_put_att(ncid, id_varlat, "comment", "Grid values for the domain LAMBERT CONFORMAL"))
!print *," Pob"
call check( nf90_def_var(ncid,"POB",NF90_REAL,dimids ,id_varpop ) )
! Assign  attributes
call check( nf90_put_att(ncid, id_varpop, "FieldType", 104 ) )
call check( nf90_put_att(ncid, id_varpop, "long_name", "Population_in_each_grid") )
call check( nf90_put_att(ncid, id_varpop,"description","Number of people in each grid in the domain"))
call check( nf90_put_att(ncid, id_varpop, "units", ""))
call check( nf90_put_att(ncid, id_varpop, "coordinates", "lon lat" ) )
call check( nf90_put_att(ncid, id_varpop, "standard_name","population") )
call check( nf90_put_att(ncid, id_varpop, "coverage_content_type","auxiliaryInformation") )

! Para Mercator
call check( nf90_def_var(ncid, "UTMx", NF90_REAL,dimids,id_utmx ) )
! Assign  attributes
call check( nf90_put_att(ncid, id_utmx, "FieldType", 104 ) )
call check( nf90_put_att(ncid, id_utmx, "long_name", "UTM_coordinate_west-east") )
call check( nf90_put_att(ncid, id_utmx, "standard_name","UTMx") )
call check( nf90_put_att(ncid, id_utmx, "units", "km"))
call check( nf90_put_att(ncid, id_utmx, "coordinates", "lon lat" ) )
call check( nf90_put_att(ncid, id_utmx, "axis", "X"))

call check( nf90_def_var(ncid, "UTMy", NF90_REAL,dimids,id_utmy ) )
! Assign  attributes
call check( nf90_put_att(ncid, id_utmy, "FieldType", 104 ) )
call check( nf90_put_att(ncid, id_utmy, "long_name", "UTM_coordinate_sotuth-north") )
call check( nf90_put_att(ncid, id_utmy, "standard_name","UTMy") )
call check( nf90_put_att(ncid, id_utmy, "units", "km"))
call check( nf90_put_att(ncid, id_utmy, "coordinates", "lon lat" ) )
call check( nf90_put_att(ncid, id_utmy, "axis", "Y"))

call check( nf90_def_var(ncid, "UTMz", NF90_INT,dimids,id_utmz ) )
! Assign  attributes
call check( nf90_put_att(ncid, id_utmz, "FieldType", 104 ) )
call check( nf90_put_att(ncid, id_utmz, "long_name", "UTM_Zone") )
call check( nf90_put_att(ncid, id_utmz, "standard_name","UTMz") )
call check( nf90_put_att(ncid, id_utmz, "units", ""))
call check( nf90_put_att(ncid, id_utmz, "coverage_content_type","coordinate") )
call check( nf90_put_att(ncid, id_utmz, "coordinates", "lon lat" ) )
!print *,"Termina definiciones"
!   Terminan definiciones
    call check( nf90_enddef(ncid) )
!  Coordenadas Mercator UTM
    call check( nf90_put_var(ncid, id_utmx,utmxd,start=(/1,1/)) )
    call check( nf90_put_var(ncid, id_utmy,utmyd,start=(/1,1/)) )
    call check( nf90_put_var(ncid, id_utmz,utmzd,start=(/1,1/)) )
    ! Coordenadas lat lon
    call check( nf90_put_var(ncid, id_varlat,xlat,start=(/1,1/)) )
    call check( nf90_put_var(ncid, id_varlong,xlon,start=(/1,1/)) )
    ! Poblacion
    call check( nf90_put_var(ncid, id_varpop,pob,start=(/1,1/)) )
    !Tiempo
    call check( nf90_put_var(ncid, id_dim(1),"2014-01-01_06:00:00"))! one year output
! Busca el numero de columnas totales
    ncol=idcg(nx+1)-idcg(1)
! Busca columna y renglon del primer valor del dominio
    call get_position(idcg(1),ncol, ren0,col0)
    ren0=ren0-1
    col0=col0-1
    do l=6,15,9
      aguardar=0.
      if(l.eq.15 .or. (l.eq.6 .and.k.eq.nf)) then
        do m=1,size(emis,dim=1)
            call get_position(idcel(m),ncol, pren,pcol)
            j=pren-ren0
            i=pcol-col0
            !if(m.eq.1) print *,i,j
        !  Actualiza la posicion en i,j a partir de m
            aguardar(i,j)=aguardar(i,j)+emis(m,l,k)*3.17098E-05*SUPF1!conversion: kg s-1 m-2
        end do
        varname="        "
        varname=trim(ename(k))//"_"//trim(idCAMS(l)) ! For TRO
        call crea_attr(ncid,2,dimids,varname,long_nm(k),cname(l),idIPCC(l),"kg m-2 s-1",id_var(l))
        call check( nf90_put_var(ncid, id_var(l),aguardar,start=(/1,1/)) )
      end if
    end do
    call check( nf90_close(ncid) )
end do !k
write(6,180)
100 format(A10,3(f9.4,f8.4,","),f9.4,f8.4,A2)
110 format(f5.0," meters")
180 format(7x,"*****  DONE SAVING DATAFILES *****")
end subroutine movil_annual_storing
!                              _   _
!   ___ _ __ ___  __ _     __ _| |_| |_ _ __
!  / __| '__/ _ \/ _` |   / _` | __| __| '__|
! | (__| | |  __/ (_| |  | (_| | |_| |_| |
!  \___|_|  \___|\__,_|___\__,_|\__|\__|_|
!                    |_____|
!>  @brief Creates attributes for each variable in the netcdf file
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.5
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param ncid netcdf file ID
!>   @param idm number of items in dimids array
!>   @param dimids ID dimensons array
!>   @param svar variable name
!>   @param cname description variable name
!>   @param cunits units of the variable
!>   @param id_var variable ID
subroutine crea_attr(ncid,idm,dimids,svar,name,desc,tipcc,cunits,id_var)
use netcdf
implicit none
integer , INTENT(IN) ::ncid,idm
integer, INTENT(out) :: id_var
integer, INTENT(IN),dimension(idm):: dimids
character(len=*), INTENT(IN)::svar,name,desc,tipcc,cunits
character(len=50) :: cvar
cvar="mass_flux_of_"//trim(svar)

call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
! Assign  attributes
call check( nf90_put_att(ncid, id_var, "coordinates", "lon lat" ) )
call check( nf90_put_att(ncid, id_var, "description",desc) )
call check( nf90_put_att(ncid, id_var, "coverage_content_type","modelResult") )
call check( nf90_put_att(ncid, id_var, "long_name",name) )
call check( nf90_put_att(ncid, id_var, "standard_name",name) )
call check( nf90_put_att(ncid, id_var, "ipcc_id",trim(tipcc)) )
call check( nf90_put_att(ncid, id_var, "units",cunits))
return
end subroutine crea_attr
end program movil_annual
