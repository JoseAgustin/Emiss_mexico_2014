!
! Proposito:
!      Realiza la lectura de las emisiones anuales de area y guarda en netcdf
!   ifort -O3 -axAVX areaanual.F90 -o Aanual.exe
!   -c -parallel -guide
!  -parallel -Dtest_gap -opt-report=1 -opt-report-phase=par -opt-report-file=stdout atemporal.F90
! gfortran -DPGI  -fopenmp -O2 atemporal.F90 -o Atemporal.exe
!
!> @brief For areaanual.F90 program. Read annual area emissions and save in netcdf file per pollutant.
!>
!>   Currently uses EPA temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/24/2023
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
module area_anual_mod
!> type day (1=Mon, 2= Tue, ... 7=Sun)
integer :: daytype ;!> number of emission files
integer,parameter :: nf=10    ;!> max number of scc descriptors in input files
integer,parameter :: nnscc=59 ;!> CAMS categories number
integer,parameter :: ncams=16  ;!> number of max lines in emiA
integer :: nmax
!> Number of lines in emissions file
integer :: nm    ;!> number of cell in the grid
integer :: ncel  ;!>  number of lines in files
integer :: nl    ;!> longitude values in grid
integer :: nx    ;!> latitude values in grid
integer :: ny    ;!> netcdf unit ID for file1  in file (24 or 12 hrs)
integer ::ncid   ;!>emissions by nx,ny,ncams
real,allocatable:: eft(:,:,:),aguardar(:,:)
!> Number of lines in Time zone file
integer :: lh ; !> if is dayligth time saving period
integer :: iverano  ! si es en periodo de verano
!> number of scc codes per file
integer,dimension(nf) :: nscc ; !> GRIDID in emissions
integer, allocatable :: idcel(:) ;!> GRIDID not duplictes
integer*8, allocatable ::idcel2(:) ;!> GRIDID fir identification of not duplicate
integer, allocatable ::idcel3(:) ;!> state municipality IDs emiss and time zone
integer, allocatable :: idsm(:,:);!> GRIDCODE in emission input files
integer*8 :: idcf;!> GRIDCODE in output grid obtaines from localiza
integer*8,allocatable :: idcg(:) ;!> UTM Z zone
integer, allocatable:: utmzd(:,:);!> cell dimension CDIM m
real :: CDIM ;!> grid 1/area  (m^-2)
real :: SUPF1
!> Fraction of weeks per days in the month
real ::fweek
!>Area emisions from files cel,ssc,file
real,allocatable ::emiA(:,:,:)
!> Emission by cel,file (pollutant) and idCAMS
real,allocatable :: emis(:,:,:);!> longitudes in output file from localiza
real,allocatable :: xlon(:,:); !> latitudes in output file from localiza
real,allocatable :: xlat(:,:); !> population in output file from localiza
real,allocatable :: pob(:,:) ;!> UTMx coordinates in output file from localiza
real,allocatable :: utmxd(:,:) ;!> UTMy coordinates in output file from localiza
real,allocatable :: utmyd(:,:)

!> index per file
integer,allocatable :: id5(:,:)
!> SCC codes per file
character(len=10),dimension(nnscc,nf) ::iscc
!> Initial date of the emissions period
character (len=19) :: current_date='2014-01-01_00:00:00'
 !> Input file name
character(len=14),dimension(nf)   :: efile ;!> output file name
character(len=25),dimension(nf)   :: casn  ;!> Categories in CAMS
character(len=3),dimension(ncams) :: idCAMS;!> IPCC classification
character(len=4),dimension(ncams) :: idIPCC;!> Pollutant abreviation
character(len=4),dimension(nf)    :: ename ;!> Long name description
character(len=77),dimension(ncams):: cname ;!> Variable description
character(len=66),dimension(nf):: long_nm

character (len=40) :: titulo

 data efile/'ASO2_2014.csv','ANOx_2014.csv','ANH3_2014.csv',&
&           'ACO__2014.csv','APM10_2014.csv','ACO2_2014.csv',&
&           'ACN__2014.csv','ACH4_2014.csv','APM25_2014.csv',&
&           'AVOC_2014.csv'/
 data casn /'MEX_AREA_9km_2014_SO2.nc ','MEX_AREA_9km_2014_NOx.nc ',&
&           'MEX_AREA_9km_2014_NH3.nc ','MEX_AREA_9km_2014_CO.nc  ',&
&           'MEX_AREA_9km_2014_PM10.nc','MEX_AREA_9km_2014_CO2.nc ',&
&           'MEX_AREA_9km_2014_BC.nc  ','MEX_AREA_9km_2014_CH4.nc ',&
&           'MEX_AREA_9km_2014_PM25.nc','MEX_AREA_9km_2014_VOC.nc '/
 data idCAMS/'AGL','AGS','AWB','COM','TOT','FEF','IND','NAT','REF',&
             'RES','SHP','SLV','SWD','TNR','TRO','WFR'/
 data idIPCC/'3A ','3C  ','3C1 ','1A4a','1A1 ','1B2a','1A2 ','9999','1A2 ',&
            '1A4 ','1A3d','2D3 ','4   ','1A4 ','1A3b','4A1b'/
 data ename/'SO2 ','NOx ','NH3 ','CO  ','PM10','CO2 ','BC  ','CH4 ','PM25','VOC '/
 data cname / &
'AGL: Agriculture_livestock                                                   ',&
'AGS: Agricultural_soils (without fires)                                      ',&
'AWB: Agricultural waste burning                                              ',&
'COM: Commercial buildings                                                    ',&
'TOT: Total Emissions                                                         ',&
'FEF: Fugitive_emissions_from_fuels                                           ',&
'IND: Industrial_process (Energy consumption of manufacture industry+ process)',&
'NAT: Natural emissions                                                       ',&
'REF: Refineries                                                              ',&
'RES: Residential_commercial_and_other_combustion                             ',&
'SHP: Navigation                                                              ',&
'SLV: Solvents                                                                ',&
'SWD: Solid_waste_and_waste_water                                             ',&
'TNR: Non-road_transportation                                                 ',&
'TRO: Road_transportation                                                     ',&
'WRF: Wildfires                                                               '/
data long_nm/&
'surface_upward_mass_flux_of_sulfur_dioxide                        ',&
'surface_upward_mass_flux_of_nox                                   ',&
'surface_upward_mass_flux_of_ammonia                               ',&
'surface_upward_mass_flux_of_carbon_monoxide                       ',&
'surface_upward_mass_flux_of_pm10_ambient_aerosol_particles_in_air ',&
'surface_upward_mass_flux_of_carbon_dioxide                        ',&
'surface_upward_mass_flux_of_black_carbon                          ',&
'surface_upward_mass_flux_of_methane                               ',&
'surface_upward_mass_flux_of_pm2p5_ambient_aerosol_particles_in_air',&
'surface_upward_mass_flux_of_nmvoc                                 '/

common /vars/ fweek,nscc,nm,lh,daytype,dia
common /texto/ idCAMS,idIPCC,ename,cname,long_nm,iscc
common /domain/ ncel,nl,nx,ny,CDIM,SUPF1
common /fileout/id_varlong,id_varlat,id_varpop,&
id_unlimit,id_utmx,id_utmy,id_utmz,ncid
common /date/ current_date,titulo
end module area_anual_mod
!
!  Progran  atemporal.F90
!
!>   @brief Make the area emissions temporal distribution using profiles based on SCC.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program area_temporal
   use master
   use area_anual_mod

    call lee_namelist

    call area_spatial_reading

    call area_sorting

    call lee_localiza

    call area_anual_storing

contains
!
!   __ _ _ __ ___  __ _
!  / _` | '__/ _ \/ _` |
! | (_| | | |  __/ (_| |
!  \__,_|_|  \___|\__,_|       _                        _ _
!  ___ _ __   __ _| |_(_) __ _| |    _ __ ___  __ _  __| (_)_ __   __ _
! / __| '_ \ / _` | __| |/ _` | |   | '__/ _ \/ _` |/ _` | | '_ \ / _` |
! \__ \ |_) | (_| | |_| | (_| | |   | | |  __/ (_| | (_| | | | | | (_| |
! |___/ .__/ \__,_|\__|_|\__,_|_|___|_|  \___|\__,_|\__,_|_|_| |_|\__, |
!     |_|                      |_____|                            |___/
!>  @brief Reads spatial area emissions and stores in netcdf for CAMS.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/24/2023
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine area_spatial_reading
	implicit none
	integer i,j,k,l,m
	integer idum,iun
	real rdum
	character(len=4):: cdum
	character(len=18):: nfile,nfilep
!
   call maxline(nmax)
!
	do k=1,nf
	open (newunit=iun,file="../02_aemis/"//efile(k),status='OLD',action='read')
	read (iun,'(A)') cdum
	read (iun,*) nscc(k),cdum,(iscc(i,k),i=1,nscc(k))
    !print *,nscc(8)
    !print "(7(A10,x))",(iscc(i,8),i=1,nscc(8))
	!print *,cdum,nscc(k)
	nm=0
	do
	   read(iun,*,end=100) cdum
	   nm=nm+1
	end do
100	 continue
     !write(6,134)"  mn=",nm,"nmax=",nmax
    if (nm.gt.nmax) STOP "*** ERROR: nm larger than nmax edit code line 140"
	 rewind(iun)
	 if(k.eq.1) then
        allocate(idcel(nm),idcel2(nm),idcel3(nm))
        allocate(emiA(nf,nmax,nnscc),id5(nf,nmax),idsm(nf,nmax))
        emiA=0
        idsm=0
        id5=0
    else
        deallocate(idcel,idcel2,idcel3)
        allocate(idcel(nm),idcel2(nm),idcel3(nm))
	end if
	read (iun,'(A)') cdum
	read (iun,'(A)') cdum
	do i=1,nm
		read(iun,*) idcel(i),idsm(k,i),rdum,rdum,(emiA(k,i,j),j=1,nscc(k))
      id5(k,i)=idcel(i)
	        !print *,idcel(i),idsm(i),(emiA(k,i,j),j=1,16),nscc(k),k
	end do
    idcel3=idcel
	close(iun)
    print *,"Done reading: ",efile(k),size(idcel3)
	end do ! K
    close(15)
    close(16)
    close(17)
    close(18)
    close(19)
134 FORMAT(4x,A5,x,I6,x,A5,I6)
end subroutine area_spatial_reading
!                                          _   _
!   __ _ _ __ ___  __ _     ___  ___  _ __| |_(_)_ __   __ _
!  / _` | '__/ _ \/ _` |   / __|/ _ \| '__| __| | '_ \ / _` |
! | (_| | | |  __/ (_| |   \__ \ (_) | |  | |_| | | | | (_| |
!  \__,_|_|  \___|\__,_|___|___/\___/|_|   \__|_|_| |_|\__, |
!                     |_____|                          |___/
!>  @brief Maps the SCC with the CAMS ids.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/24/2023
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
!
subroutine area_sorting
  implicit none
  integer i,j,k,l,ival,ii
!
  call area_grids_count ! computes the number of different cells
  print *,"   Annual emissions"
  emis=0
  do k=1,nf
    do ii=1,size(idcel2)
        do i=1,size(id5,2)
        if(idcel2(ii).eq.id5(k,i))then
            do j=1,nscc(k)
            emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j,k).eq.'2102007000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2102004000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2103006000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2103007000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2104011000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2104006000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2104008000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2104007000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2267005000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2270005000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2302002000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2610000000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2801500100') emis(ii,k,3)=emis(ii,k,3)+emiA(k,i,j)
            if(iscc(j,k).eq.'2810030000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2810001000') emis(ii,k,16)=emis(ii,k,16)+emiA(k,i,j)
            if(trim(iscc(j,k)).eq.'30500304') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2280000000') emis(ii,k,11)=emis(ii,k,11)+emiA(k,i,j)
            if(iscc(j,k).eq.'2285000000') emis(ii,k,14)=emis(ii,k,14)+emiA(k,i,j)
            if(iscc(j,k).eq.'2275000000') emis(ii,k,14)=emis(ii,k,14)+emiA(k,i,j)
            if(iscc(j,k).eq.'2275050000') emis(ii,k,14)=emis(ii,k,14)+emiA(k,i,j)
            if(iscc(j,k).eq.'2222222222') emis(ii,k,15)=emis(ii,k,15)+emiA(k,i,j)
            if(iscc(j,k).eq.'2620030000') emis(ii,k,13)=emis(ii,k,13)+emiA(k,i,j)
            if(iscc(j,k).eq.'2230070310') emis(ii,k,15)=emis(ii,k,15)+emiA(k,i,j)
            if(iscc(j,k).eq.'2285002010') emis(ii,k,14)=emis(ii,k,14)+emiA(k,i,j)
            if(iscc(j,k).eq.'2265005000') emis(ii,k,14)=emis(ii,k,14)+emiA(k,i,j)
            if(iscc(j,k).eq.'2801700000') emis(ii,k,2)=emis(ii,k,2)+emiA(k,i,j)
            if(iscc(j,k).eq.'2805020000') emis(ii,k,1)=emis(ii,k,1)+emiA(k,i,j)
            if(iscc(j,k).eq.'5555555555') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2311010000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2801000002') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j,k).eq.'2801000005') emis(ii,k,2)=emis(ii,k,2)+emiA(k,i,j)
            if(iscc(j,k).eq.'2294000000') emis(ii,k,15)=emis(ii,k,15)+emiA(k,i,j)
            if(iscc(j,k).eq.'2296000000') emis(ii,k,15)=emis(ii,k,15)+emiA(k,i,j)
            if(iscc(j,k).eq.'2425010000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2425030000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2425040000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2425000000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2461020000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2420000055') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2415000000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2401005000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2401008000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2415010000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2401990000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2401080000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2401065000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2401020000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j,k).eq.'2401001000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2465900000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2465200000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2465100000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2465400000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j,k).eq.'2465600000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j,k).eq.'2465800000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'2465000000') emis(ii,k,10)=emis(ii,k,10)+emiA(k,i,j)
            if(iscc(j,k).eq.'3333333333') emis(ii,k,6)=emis(ii,k,6)+emiA(k,i,j)
            if(iscc(j,k).eq.'2501060000') emis(ii,k,6)=emis(ii,k,6)+emiA(k,i,j)
            if(iscc(j,k).eq.'2302050000') emis(ii,k,1)=emis(ii,k,1)+emiA(k,i,j)
            if(iscc(j,k).eq.'2461850000') emis(ii,k,2)=emis(ii,k,2)+emiA(k,i,j)
            if(iscc(j,k).eq.'2630030000') emis(ii,k,13)=emis(ii,k,13)+emiA(k,i,j)
            if(iscc(j,k).eq.'2850000010') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            end do
        end if
        end do
    end do
end do
!  end do
end subroutine area_sorting
!                                                  _
!   __ _ _ __ ___  __ _     __ _ _ __  _   _  __ _| |
!  / _` | '__/ _ \/ _` |   / _` | '_ \| | | |/ _` | |
! | (_| | | |  __/ (_| |  | (_| | | | | |_| | (_| | |
!  \__,_|_|  \___|\__,_|___\__,_|_| |_|\__,_|\__,_|_|
!           _         |_____|
!       ___| |_ ___  _ __(_)_ __   __ _
!      / __| __/ _ \| '__| | '_ \ / _` |
!      \__ \ || (_) | |  | | | | | (_| |
!  ____|___/\__\___/|_|  |_|_| |_|\__, |
! |_____|                         |___/
!>  @brief Saves area emission for CAMS.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/24/2023
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine area_anual_storing
use netcdf
  implicit none
  integer i,j,k,l,m,iun
  integer :: Layer
  integer, parameter :: NDIMS=6;!> Number of layers in emission
  integer,parameter :: zlev=1
  integer :: dimids2(2),dimids3(3),dimids(2)
  integer,dimension(NDIMS):: dim,id_dim
  integer :: ncol,pren,ren0
  integer :: pcol,col0
!> GRIDCODE in emission input files
  integer,allocatable :: isp(:) ;!> array of variables ID
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
  character (len=83)::geospatial_bounds
  character(8)  :: fecha,varname
  character (13):: ccdim
  character(10) :: time
  character(24) :: hoy
  data sdim /"Time               ","DateStrLen         ","west_east          ",&
&            "south_north        ","bottom_top         ","emissions_zdim_stag"/
    call date_and_time(fecha,time)
    hoy=fecha(7:8)//'-'//fecha(5:6)//'-'//fecha(1:4)//'T'//time(1:2)//':'//time(3:4)&
        //':'//time(5:10)//'Z'
!
 print *,"Area Emissions Annual saving"
  do k=1,nf
  print *,"Inicializa archivo de salida ",casn(k)
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
call check( nf90_put_att(ncid, NF90_GLOBAL, "id","AE_2014_"//trim(ename(k))))
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
call check( nf90_put_att(ncid, NF90_GLOBAL, "summary","National emissions inventory for Mexico 2014&
produced by SEMARNAT it is converted by DiETE v2 to a model ready EI, this file only contains annual &
emissions for area sources"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "program","PAPILA,ICAyCC"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "project","PAPILA"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "institution","SEMARNAT,UNAM"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "source","DiETE v2"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "references","https://doi.org/10.20937/RICA.2018.34.04.07,&
https://www.gob.mx/semarnat/documentos/documentos-del-inventario-nacional-de-emisiones"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "cdm_data_type","Grid"))
call check( nf90_put_att(ncid, NF90_GLOBAL, "date_issued",hoy))
call check( nf90_put_att(ncid, NF90_GLOBAL, "date_created",hoy))
call check( nf90_put_att(ncid, NF90_GLOBAL, "date_modified",hoy))
call check( nf90_put_att(ncid, NF90_GLOBAL, "product_version",1))
call check( nf90_put_att(ncid, NF90_GLOBAL, "date_modified",hoy))
call check( nf90_put_att(ncid, NF90_GLOBAL, "comment","Created with areaanual.F90 v1.0"))
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
  eft=0.0
    do m=1,size(emis,dim=1)
    call get_position(idcel2(m),ncol, pren,pcol)
    j=pren-ren0
    i=pcol-col0
!    if(m.eq.1) print *,i,j
     suma=0.0
        do l=1,ncams
            suma(l)=suma(l)+emis(m,k,l)
            eft(i,j,l)=eft(i,j,l)+emis(m,k,l)*3.17098E-05*SUPF1!conversion: kg s-1 m-2
        end do  !l
    end do !m
    do l=1,ncams
        if(suma(l).gt.0.) then
            varname="        "
            varname=trim(ename(k))//"_"//trim(idCAMS(l))
            aguardar=0.
            do i=1,nx
              do j=1,ny
              aguardar(i,j)=eft(i,j,l)
              end do
            end do
            call crea_attr(ncid,2,dimids,varname,long_nm(k),cname(l),idIPCC(l),"kg m-2 s-1",id_var(l))
            call check( nf90_put_var(ncid, id_var(l),aguardar,start=(/1,1/)) )
        end if
    end do
call check( nf90_close(ncid) )

 end do
    print *,"*****  DONE Annual Output Area *****"
    deallocate(idcel,id5,idcel2,idsm,emiA,emis)
100 format(A10,3(f9.4,f8.4,","),f9.4,f8.4,A2)
110 format(f5.0," meters")
end subroutine area_anual_storing
!   __ _ _ __ ___  __ _
!  / _` | '__/ _ \/ _` |
! | (_| | | |  __/ (_| |
!  \__,_|_|  \___|\__,_|                           _
!   __ _ _ __(_) __| |___     ___ ___  _   _ _ __ | |_
!  / _` | '__| |/ _` / __|   / __/ _ \| | | | '_ \| __|
! | (_| | |  | | (_| \__ \  | (_| (_) | |_| | | | | |_
!  \__, |_|  |_|\__,_|___/___\___\___/ \__,_|_| |_|\__|
!  |___/                |_____|
!>  @brief Counts the number of different cells in file and stores in index.csv file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/24/2023
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine area_grids_count
  integer i,j
  integer idum
!  Se ordenan los indices
  call hpsort(size(idcel3))

  idcel2(1)=idcel3(1)
  j=1
  do i=2,nm
   if(idcel2(j).ne.idcel3(i)) then
    j=j+1
	idcel2(j)=idcel3(i)
	end if
  end do
  deallocate(idcel3)
  allocate(idcel3(j))
  print *,'Number of different cells',j

  open(unit=123,file="index.csv")
  write(123,*)j,"Index"
  do i=1,j
   write(123,'(I8)')idcel2(i)
   idcel3(i)=idcel2(i)
  end do
  deallocate(idcel2)
  allocate(idcel2(j))
  idcel2=idcel3
  close(123)
  allocate(emis(j,nf,ncams))
   emis=0
   evoc=0
  deallocate(idcel3)
end subroutine area_grids_count
!  _                          _
! | |__  _ __  ___  ___  _ __| |_
! | '_ \| '_ \/ __|/ _ \| '__| __|
! | | | | |_) \__ \ (_) | |  | |_
! |_| |_| .__/|___/\___/|_|   \__|
!       |_|
!>  @brief Sorts an array from minumun to max value.
subroutine hpsort(n)
    implicit none
    integer n
    integer i,ir,j,l
    real rra
    if (n.lt.2) return
    l=n/2+1
    ir=n
10 continue
    if(l.gt.1)then
        l=l-1
        rra=idcel3(l)
    else
        rra=idcel3(ir)
        idcel3(ir)=idcel3(1)
        ir=ir-1
        if(ir.eq.1)then
          idcel3(1)=rra
          return
        endif
    endif
    i=l
    j=l+l
20 if(j.le.ir)then
     if(j.lt.ir)then
        if(idcel3(j).lt.idcel3(j+1))j=j+1
     end if
     if(rra.lt.idcel3(j))then
        idcel3(i)=idcel3(j)
        i=j
        j=j+j
     else
        j=ir+1
     endif
     goto 20
   endif
      idcel3(i)=rra
    goto 10
end subroutine hpsort
!                       _ _
!  _ __ ___   __ ___  _| (_)_ __   ___
! | '_ ` _ \ / _` \ \/ / | | '_ \ / _ \
! | | | | | | (_| |>  <| | | | | |  __/
! |_| |_| |_|\__,_/_/\_\_|_|_| |_|\___|
!
!>  @brief Obtains the number of lines in *efile* file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine maxline(entero)
    implicit none
    integer,intent(out):: entero
    integer:: k,inum
    character(len=14):: cdum
    entero=-1
    do k=1,nf
      open(unit=14,file="../02_aemis/"//efile(k),status='OLD')
       inum=0
      do
        read(14,*,end=100) cdum
        inum=inum+1
      end do
100   close(14)
       inum=inum-2
      entero=max(inum,entero)
      !print '(I6)',entero
    end do
end subroutine maxline
!  _               _                 _ _
! | | ___  ___    | | ___   ___ __ _| (_)______ _
! | |/ _ \/ _ \   | |/ _ \ / __/ _` | | |_  / _` |
! | |  __/  __/   | | (_) | (_| (_| | | |/ / (_| |
! |_|\___|\___|___|_|\___/ \___\__,_|_|_/___\__,_|
!            |_____|
!>  @brief Reada the lon,lat and utm coordinates for the output grid
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine lee_localiza
    implicit NONE
    character(len=39) :: flocaliza,cdum
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
    do j=1,ny
     do i=1,nx
      k=i+(j-1)*nx
      read(10,*) idcg(k),xlon(i,j),xlat(i,j),idum,pob(i,j),utmxd(i,j),utmyd(i,j),utmzd(i,j)
     end do
    end do
    CDIM=(utmxd(2,1)-utmxd(1,1))  ! in meters
    write(6,'(F8.2,A30)') CDIM,trim(titulo)
    SUPF1=1./(CDIM*CDIM)  !computes  grid area in m^-2
    close(10)
   allocate(eft(nx,ny,ncams),aguardar(nx,ny))
end subroutine lee_localiza
!
!  _ __ ___   ___  ___
! | '_ ` _ \ / _ \/ __|
! | | | | | |  __/\__ \
! |_| |_| |_|\___||___/
!
!>  @brief Returns the month in characters from month number
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param  num number of the month
   function mes(num)
    character(len=3) :: mes
    character(len=2),intent(in) :: num
    select case (num)
    case('01');mes='Jan'
    case('02');mes='Feb'
    case('03');mes='Mar'
    case('04');mes='Apr'
    case('05');mes='May'
    case('06');mes='Jun'
    case('07');mes='Jul'
    case('08');mes='Aug'
    case('09');mes='Sep'
    case('10');mes='Oct'
    case('11');mes='Nov'
    case('12');mes='Dec'
    case default
        print *,"   **************************"
        print *,"Month:",num," does not exists!!"
        print *,"   **************************"
        stop  "End program, review namelist_emis.nml"
    end select
    return
end function
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
end program area_temporal
