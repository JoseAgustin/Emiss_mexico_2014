!  Creado por Jose Agustin Garcia Reynoso el 26/06/23
! Proposito
!          Almacenamiento emisioens anuales de fuentes puntuales
!
! ifort -O2 -axAVX2 f_anual.F90 -o Panual.exe
!
!>  @brief for f_anual.F90 program. Emissions storage in grid annually.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
module point_vars_mod
!>   number of pollutants
integer, parameter::nsp=10 !number of pollutants
!> CAMS categories number
integer,parameter :: ncams=16 ; !> Vertical layers in emissions
integer,parameter :: nlevels=8 ; !> Layers where emissions are each day and night
integer,allocatable :: capa(:,:);!> **i** index in grid to allocate a point emission
integer,allocatable :: ict(:) ; !> **j** index in grid to allocate a point emission
integer,allocatable :: jct(:) ;!> _GRIDCODE_ in grid domain
integer*8,allocatable :: idcg(:,:);!> longitudes in output file from localiza
real,allocatable :: xlon(:,:)  ;!> latitudes in output file from localiza
real,allocatable :: xlat(:,:) ; !> population in output file from localiza
real,allocatable :: pob(:,:) ;!> UTMx coordinates in output file from localiza
real,allocatable :: utmxd(:,:) ;!> UTMy coordinates in output file from localiza
real,allocatable :: utmyd(:,:) ;!> temporal array for storage
real,allocatable:: eft(:,:,:,:) ;!> array to storing variable in netcdf
real,allocatable:: aguardar(:,:,:) !> UTM Z zone
integer, allocatable:: utmzd(:,:) ;!> cell dimension CDIM m
real :: CDIM ;!> grid 1/area  (m^-2)
real :: SUPF1 ;!> Number of lines in input file
integer :: nl ;!> Number of longitudes (columns) in localiza file
integer :: nx ;!> Number of latitudes (rows( in localiza file
integer :: ny ;;!> Time zone integer
integer,allocatable :: mcst(:,:)
!> Latitude point source
real,allocatable :: lat(:) ;!> Longitude point source
real,allocatable :: lon(:) ;!> point source emission input
real,allocatable :: e_mis(:,:) ;!> point source emission for each pollutant
real,allocatable :: emis(:,:,:,:,:);!> output file name
character(len=27),dimension(nsp)  :: casn  ;!> Categories in CAMS
character(len=3),dimension(ncams) :: idCAMS;!> IPCC classification
character(len=4),dimension(ncams) :: idIPCC;!> Pollutant abreviation
character(len=4),dimension(nsp)   :: ename ;!> Long name description
character(len=77),dimension(ncams):: cname ;!> Variable description
character(len=66),dimension(nsp)  :: long_nm

!> Source clasification code array
character(len=10),allocatable::iscc(:)
!> Pollutant name
character (len=7) :: cvar(nsp)
!> Initial date of the emissions period
character (len=19) :: current_date
data casn /'MEX_POINT_9km_2014_PM10.nc','MEX_POINT_9km_2014_PM25.nc',&
'MEX_POINT_9km_2014_SO2.nc ','MEX_POINT_9km_2014_CO.nc  ',&
'MEX_POINT_9km_2014_NOx.nc ','MEX_POINT_9km_2014_VOC.nc ',&
'MEX_POINT_9km_2014_CO2.nc ','MEX_POINT_9km_2014_CH4.nc ',&
'MEX_POINT_9km_2014_NH3.nc ','MEX_POINT_9km_2014_BC.nc  '/
data idCAMS/'AGL','AGS','AWB','COM','ENE','FEF','IND','NAT','REF',&
'TOT','SHP','SLV','SWD','TNR','TRO','WFR'/
data idIPCC/'3A ','3C  ','3C1 ','1A4a','1A1 ','1B2a','1A2 ','9999','1A2 ',&
'tot ','1A3d','2D3 ','4   ','1A4 ','1A3b','4A1b'/
data ename/ 'PM10','PM25','SO2 ','CO  ','NOx ',&
            'VOC ','CO2 ','CH4 ','NH3 ','BC  '/
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
'TOT: Total_emissions                                                         ',&
'SHP: Navigation                                                              ',&
'SLV: Solvents                                                                ',&
'SWD: Solid_waste_and_waste_water                                             ',&
'TNR: Non-road_transportation                                                 ',&
'TRO: Road_transportation                                                     ',&
'WRF: Wildfires                                                               '/
data long_nm/&
'surface_upward_mass_flux_of_pm10_ambient_aerosol_particles_in_air ',&
'surface_upward_mass_flux_of_pm2p5_ambient_aerosol_particles_in_air',&
'surface_upward_mass_flux_of_sulfur_dioxide                        ',&
'surface_upward_mass_flux_of_carbon_monoxide                       ',&
'surface_upward_mass_flux_of_nitrogen_oxides                       ',&
'surface_upward_mass_flux_of_nmvoc                                 ',&
'surface_upward_mass_flux_of_carbon_dioxide                        ',&
'surface_upward_mass_flux_of_methane                               ',&
'surface_upward_mass_flux_of_ammonia                               ',&
'surface_upward_mass_flux_of_black_carbon                          '/
common /dat/ nl,nx,ny,daytype,cvar,current_date

end module point_vars_mod
!              _       _      _                                       _
!  _ __   ___ (_)_ __ | |_   | |_ ___ _ __ ___  _ __   ___  _ __ __ _| |
! | '_ \ / _ \| | '_ \| __|  | __/ _ \ '_ ` _ \| '_ \ / _ \| '__/ _` | |
! | |_) | (_) | | | | | |_   | ||  __/ | | | | | |_) | (_) | | | (_| | |
! | .__/ \___/|_|_| |_|\__|___\__\___|_| |_| |_| .__/ \___/|_|  \__,_|_|
! |_|                    |_____|               |_|
!
!>  @brief Temporal distribution of point sources emissions.
!>
!>  using EPA temporal profiles based on SCC
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program point_temporal
#ifdef _OPENMP
    use omp_lib
#endif
use point_vars_mod
use master

    call lee_namelist

    call point_emis_reading

    call point_cams_distribution

    call point_emissions_storage

    call free_memory

contains
!              _       _
!  _ __   ___ (_)_ __ | |_
! | '_ \ / _ \| | '_ \| __|
! | |_) | (_) | | | | | |_
! | .__/ \___/|_|_| |_|\__|
! |_|             _                            _ _
!   ___ _ __ ___ (_)___     _ __ ___  __ _  __| (_)_ __   __ _
!  / _ \ '_ ` _ \| / __|   | '__/ _ \/ _` |/ _` | | '_ \ / _` |
! |  __/ | | | | | \__ \   | | |  __/ (_| | (_| | | | | | (_| |
!  \___|_| |_| |_|_|___/___|_|  \___|\__,_|\__,_|_|_| |_|\__, |
!                    |_____|                            |___/
!>  @brief Reads emissions file and temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine point_emis_reading
implicit none
	integer :: i,j,k,l,m,iun
	integer :: idum, imon,iwk,ipdy
	integer,dimension(25) :: itfrc  !montly,weekely and hourly values and total
	real rdum
	logical fil1,fil2
  character(len=10)::jscc
	character(len=40)::cdum,canio
	character(len=18):: nfile,nfilep

   write(current_date,'(I4,"-",I2.2,"-",I2.2,A9)')anio,month,idia,'_00:00:00'
!$omp section
	open(newunit=iun,file='../07_puntual/puntual.csv',status='old',action='read')
	i=0
	read (iun,*) cdum
	do
		read(iun,*,end=100) cdum
		i=i+1
	end do
100 continue
    nl=i
!$omp end parallel sections
    print *,'**  Numero de lineas ',nl
    allocate(iscc(nl),capa(nl,2),lat(nl),lon(nl),e_mis(nl,nsp))
    allocate(ict(nl),jct(nl))
    e_mis=0

    rewind(iun)
    read (iun,*) cdum,cdum,cdum,(cvar(i),i=1,nsp)
    do i=1,nl
      read(iun,*,err=110)lat(i),lon(i),iscc(i),(e_mis(i,j),j=1,nsp),capa(i,1),capa(i,2)
    end do
    close(iun)
    e_mis=e_mis  !en (Mg) Tonne
    print *,'Done puntual.csv '!,cvar,maxval(e_mis)
!
!	localiza.csv
!
   cdum="../07_puntual/"//"localiza.csv"
	write(6,*)' >>>> Reading file - ',cdum,' ----'
	open (newunit=iun,file=cdum,status='old',action='read')
	read (iun,*) cdum  !Header
	read (iun,*) nx,ny  !Header
	allocate(idcg(nx,ny),xlon(nx,ny),xlat(nx,ny),mcst(nx,ny))
   allocate(emis(nx,ny,nlevels,nsp,ncams))
   allocate(aguardar(nx,ny,nlevels))
   allocate(eft(nx,ny,nlevels,ncams))
   allocate(utmxd(nx,ny),utmyd(nx,ny),utmzd(nx,ny),pob(nx,ny))

	do j=1,ny
		do i=1,nx
			read(iun,*) idcg(i,j),xlon(i,j),xlat(i,j),mcst(i,j),pob(i,j),utmxd(i,j),utmyd(i,j),utmzd(i,j)
		end do
	end do
	!print *,ncel
    close(iun)
    CDIM=(utmxd(2,1)-utmxd(1,1))  ! in meters
    write(6,'(F8.2,A30)') CDIM
    SUPF1=1./(CDIM*CDIM)  !computes grid area in m^-2
    print *,'   >>>>>  Finding emissions in grid'
    call localiza(xlat,xlon,nx,ny,lat,lon,ict,jct,1,nl)   ! Point Sources
     print *,'   Done '
	return
110	print *,'Error en ',i
    STOP
end subroutine point_emis_reading
!              _       _
!  _ __   ___ (_)_ __ | |_     ___ __ _ _ __ ___  ___
! | '_ \ / _ \| | '_ \| __|   / __/ _` | '_ ` _ \/ __|
! | |_) | (_) | | | | | |_   | (_| (_| | | | | | \__ \
! | .__/ \___/|_|_| |_|\__|___\___\__,_|_| |_| |_|___/
! |_|      _ _     _     |_____|          _   _
!       __| (_)___| |_ _ __(_) |__  _   _| |_(_) ___  _ __
!      / _` | / __| __| '__| | '_ \| | | | __| |/ _ \| '_ \
!     | (_| | \__ \ |_| |  | | |_) | |_| | |_| | (_) | | | |
! ____\__,_|_|___/\__|_|  |_|_.__/ \__,_|\__|_|\___/|_| |_|
!|_____|
!>  @brief Distributes point annual emissions from SCC to CAMS categories
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/26/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine point_cams_distribution
	implicit none
	integer i,j,kk,l,ival,ii
   real,dimension(nsp) :: bmasa=0
   real,dimension(nsp):: suma=0
	print *,'CAMS distribution'
  emis=0.
  do i=1,nl
    if(ict(i).gt.0 .and.jct(i).gt.0) then
      do j=1,nsp
         bmasa(j)=bmasa(j)+ e_mis(i,j)
         emis(ict(i),jct(i),capa(i,1),j,10) = e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,10)
if(trim(iscc(i)).eq.'0') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10100401') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10100601') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10100901') emis(ict(i),jct(i),capa(i,1),j,5)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,5)
if(trim(iscc(i)).eq.'10101208') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200221') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200229') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200401') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200404') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200501') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200503') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200504') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200601') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200707') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200802') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10200903') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10201001') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10201002') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10201101') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10201201') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10201302') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10201701') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'10500105') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'20100101') emis(ict(i),jct(i),capa(i,1),j,5)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,5)
if(trim(iscc(i)).eq.'20100107') emis(ict(i),jct(i),capa(i,1),j,5)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,5)
if(trim(iscc(i)).eq.'20200101') emis(ict(i),jct(i),capa(i,1),j,5)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,5)
if(trim(iscc(i)).eq.'20200102') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'20200107') emis(ict(i),jct(i),capa(i,1),j,5)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,5)
if(trim(iscc(i)).eq.'20200109') emis(ict(i),jct(i),capa(i,1),j,5)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,5)
if(trim(iscc(i)).eq.'20200201') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30101453') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30101822') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30101847') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30101872') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30102201') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30102826') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30106013') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30112007') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30125805') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30125815') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30187007') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30200705') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30200746') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30200778') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30200911') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30201421') emis(ict(i),jct(i),capa(i,1),j,6)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,6)
if(trim(iscc(i)).eq.'30201520') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30201903') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30203201') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30203204') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30300317') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30300931') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30300935') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30400723') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30400736') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30400737') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30400812') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30402201') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30500504') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30500619') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30500718') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30501114') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30502599') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30504030') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30504034') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30800126') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30800901') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30801006') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30901078') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'30903901') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'31613001') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'33000102') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'33000202') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'36000101') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'39090007') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'39990024') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'40100101') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40100301') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40100307') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40100308') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40200101') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40200110') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40200301') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40200701') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40200801') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40200820') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40200861') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40200870') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40201403') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40201601') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40202520') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40204240') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40204435') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'40301008') emis(ict(i),jct(i),capa(i,1),j,6)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,6)
if(trim(iscc(i)).eq.'40301153') emis(ict(i),jct(i),capa(i,1),j,6)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,6)
if(trim(iscc(i)).eq.'40400121') emis(ict(i),jct(i),capa(i,1),j,6)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,6)
if(trim(iscc(i)).eq.'40400403') emis(ict(i),jct(i),capa(i,1),j,6)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,6)
if(trim(iscc(i)).eq.'40500506') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40600651') emis(ict(i),jct(i),capa(i,1),j,6)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,6)
if(trim(iscc(i)).eq.'40688801') emis(ict(i),jct(i),capa(i,1),j,6)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,6)
if(trim(iscc(i)).eq.'40700801') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40700809') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40700811') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40700813') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40703601') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40703615') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40703623') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40704001') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40704403') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40704405') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40704411') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40704419') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40706801') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40706803') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40707603') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40717601') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40717602') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40717603') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40722009') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'40722803') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'50200507') emis(ict(i),jct(i),capa(i,1),j,13)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,13)
if(trim(iscc(i)).eq.'2101006000') emis(ict(i),jct(i),capa(i,1),j,5)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,5)
if(trim(iscc(i)).eq.'2102007000') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2102008000') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2102010000') emis(ict(i),jct(i),capa(i,1),j,5)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,5)
if(trim(iscc(i)).eq.'2280002020') emis(ict(i),jct(i),capa(i,1),j,11)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,11)
if(trim(iscc(i)).eq.'2301050001') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2303020000') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2308000000') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2309100000') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2310010000') emis(ict(i),jct(i),capa(i,1),j,9)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,9)
if(trim(iscc(i)).eq.'2401005600') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'2401100001') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'2415000000') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2415000350') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2415300000') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'2440000250') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2440000260') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2440020000') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2465000250') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2495000000') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'2495000035') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2495000165') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'2495000255') emis(ict(i),jct(i),capa(i,1),j,12)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,12)
if(trim(iscc(i)).eq.'2501000120') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2510000165') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2510000195') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2510000250') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2530000080') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2730001000') emis(ict(i),jct(i),capa(i,1),j,8)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,8)
if(trim(iscc(i)).eq.'2841010000') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
if(trim(iscc(i)).eq.'2850000010') emis(ict(i),jct(i),capa(i,1),j,7)= e_mis(i,j)+emis(ict(i),jct(i),capa(i,1),j,7)
    end do !j
    end if
  end do  ! i
    print *,bmasa
    print *,suma
    if(allocated(e_mis))  deallocate(e_mis)
end subroutine point_cams_distribution
!               _       _                     _         _
!  _ __   ___ (_)_ __ | |_     ___ _ __ ___ (_)___ ___(_) ___  _ __  ___
! | '_ \ / _ \| | '_ \| __|   / _ \ '_ ` _ \| / __/ __| |/ _ \| '_ \/ __|
! | |_) | (_) | | | | | |_   |  __/ | | | | | \__ \__ \ | (_) | | | \__ \
! | .__/ \___/|_|_| |_|\__|___\___|_| |_| |_|_|___/___/_|\___/|_| |_|___/
! | |     ___| |_ ___  _ __|_____|__ _  ___
!       / __| __/ _ \| '__/ _` |/ _` |/ _ \
!       \__ \ || (_) | | | (_| | (_| |  __/
!   ____|___/\__\___/|_|  \__,_|\__, |\___|
!  |_____|                      |___/
!>  @brief Stores houry emission by pollutant
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine point_emissions_storage
    use netcdf
	implicit none
	integer:: i,j,k,l,m
integer  ncid
integer, parameter :: NDIMS=6
integer :: dimids2(2),dimids3(3),dimids(2)
integer,dimension(NDIMS):: dim,id_dim
integer :: ncol,pren,ren0
integer :: pcol,col0
;!> array of variables ID
integer :: id_var(ncams) ;!>netcdf longitude ID in netcdf file
integer :: id_varlong ;!>netcdf latitude ID in netcdf file
integer :: id_varlat  ;!>netcdf population ID in netcdf file
integer :: id_varpop  ;!>netcdf unlimited variable ID in netcdf file
integer :: id_unlimit ;!>netcdf UTMx coordinate variable ID in netcdf file
integer :: id_utmx    ;!>netcdf UTMy coordinate variable ID in netcdf file
integer :: id_utmy    ;!>netcdf UTMz coordinate variable ID in netcdf file
integer :: id_utmz
real    :: suma,suma2
character (len=19),dimension(NDIMS) ::sdim
character (len=83)::geospatial_bounds
character (13):: ccdim
character(8)  :: fecha,varname
character(10) :: time
character(24) :: hoy

   Write(6,*)"Guarda"
data sdim /"Time               ","DateStrLen         ","west_east          ",&
&            "south_north        ","bottom_top         ","emissions_zdim_stag"/
call date_and_time(fecha,time)
hoy=fecha(7:8)//'-'//fecha(5:6)//'-'//fecha(1:4)//'T'//time(1:2)//':'//time(3:4)&
//':'//time(5:10)//'Z'

print *,"Point Emissions Annual saving"

    do k=1,nsp
        print *,"Output File Initialization ",casn(k)
    call check( nf90_create(path =casn(k),cmode = NF90_NETCDF4,ncid = ncid) )
    dim=(/1,19,nx,ny,nlevels,1/)
    call check( nf90_def_dim(ncid,sdim(1), NF90_UNLIMITED, id_dim(1)) )
    do i=2,NDIMS
    call check( nf90_def_dim(ncid, sdim(i), dim(i), id_dim(i)) )
    end do
    dimids2 = (/id_dim(2),id_dim(1)/)
    dimids3 = (/id_dim(3),id_dim(4),id_dim(5) /)
    dimids  = (/id_dim(3),id_dim(4)/)
!    print *,"Globales"
    write(geospatial_bounds,100)"POLYGON ((",minval(xlon),minval(xlat),&
    &minval(xlon),maxval(xlat),maxval(xlon), maxval(xlat),maxval(xlon),minval(xlat),"))"
    write(ccdim,110) CDIM
    call check( nf90_put_att(ncid, NF90_GLOBAL, "id","PE_2014_"//trim(ename(k))))
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
    call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_vertical_min","20 m"))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_vertical_max","545 m"))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "geospatial_vertical_positive","up"))
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
    emissions for point sources"))
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
    call check( nf90_put_att(ncid, NF90_GLOBAL, "comment","Created with f_anual.F90 v1.0"))
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
!
    call check( nf90_def_var(ncid, "UTMy", NF90_REAL,dimids,id_utmy ) )
    ! Assign  attributes
    call check( nf90_put_att(ncid, id_utmy, "FieldType", 104 ) )
    call check( nf90_put_att(ncid, id_utmy, "long_name", "UTM_coordinate_sotuth-north") )
    call check( nf90_put_att(ncid, id_utmy, "standard_name","UTMy") )
    call check( nf90_put_att(ncid, id_utmy, "units", "km"))
    call check( nf90_put_att(ncid, id_utmy, "coordinates", "lon lat" ) )
    call check( nf90_put_att(ncid, id_utmy, "axis", "Y"))
!
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
   do l=5,13 !ncams  ! CAMS ids
    eft=0
    suma= 0
    suma2=0
      do i=1,nl
        if (ict(i).gt.0 .and.jct(i).gt.0) then
          suma =suma +emis(ict(i),jct(i),capa(i,1) ,k,l) !conversion: kg s-1 m-2
          eft(ict(i),jct(i),capa(i,1),l)= emis(ict(i),jct(i),capa(i,1),k,l)*3.17098E-05*SUPF1
          suma2=eft(ict(i),jct(i),capa(i,1),l)+suma2
        end if
      end do !i
      if(suma.gt.0.) then
!        print *,l,idCAMS(l),suma,suma2
        varname="        "
        varname=trim(ename(k))//"_"//trim(idCAMS(l))
        aguardar=0.0
        do i=1,nx
            do j=1,ny
                do m=1,nlevels
                    aguardar(i,j,m)=eft(i,j,m,l)
                end do
            end do
        end do
        call crea_attr(ncid,3,dimids3,varname,long_nm(k),cname(l),idIPCC(l),"kg m-2 s-1",id_var(l))
        call check( nf90_put_var(ncid, id_var(l),aguardar,start=(/1,1,1/)) )
      end if
  end do! l
call check( nf90_close(ncid) )

	end do
!$omp end parallel do
write(6,120)

100 format(A10,3(f9.4,f8.4,","),f9.4,f8.4,A2)
110 format(f5.0," meters")
120 format(7x,"*****  DONE SAVING DATAFILES *****")

end subroutine point_emissions_storage
!  _                 _ _
! | | ___   ___ __ _| (_)______ _
! | |/ _ \ / __/ _` | | |_  / _` |
! | | (_) | (_| (_| | | |/ / (_| |
! |_|\___/ \___\__,_|_|_/___\__,_|
!
!>   @brief Identifies the i,j index in the grid for the spatial
!>   allocation of point source emissions
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param xlat two dimensional array with latitudes
!>   @param xlon two dimensional array with longitudes
!>   @param mi x dimension in arrays xlat and xlon
!>   @param mj y dimension in arrays xlat and xlon
!>   @param clat array of nst-inst+1 elemets of latitudes
!>   @param clon array of nst-inst+1 elemets of longitudes
!>   @param inst start value of clat and clon arrays
!>   @param nst end  value of clat and clon arrays
!>   @param ist column index value
!>   @param jst row index value
  Subroutine localiza(xlat,xlon,mi,mj,clat,clon,ist,jst,inst,nst)
  implicit none
  integer,intent(IN) :: mi,mj,nst,inst
  integer ::i,j,l
  integer,dimension(:),intent(out):: ist,jst
  real,dimension(:,:),intent(IN):: xlat,xlon
  real,dimension(:),intent(IN):: clat,clon
  real:: tlon,tlat,minlon,minlat
     minlon=minval(xlon)
     minlat=minval(xlat)
    tlon=maxval(xlon)-minlon
    tlat=maxval(xlat)-minlat
   do l=inst,nst
		! Out of the region
   ist(l)=0
   jst(l)=0
	  do i = 1,mi-1
	    do j= 1,mj-1
        if(clon(l) .ge. xlon(i,j) .and. clon(l) .le. xlon(i+1,j).and.&
		  &  clat(l) .ge. xlat(i,j) .and. clat(l) .le. xlat(i,j+1))then
		   ist(l)= i
		   jst(l)= j
          goto 987
		end if
		end do
	  end do
     if ((ist(l).eq.0 .or.jst(l).eq.0).and.(clat(l).gt. minlat.and. clat(l).le.&
       &maxval(xlat)).and.(clon(l).gt.minlon.and.clon(l).le.maxval(xlon))) then
           i=(clon(l)-minlon)/tlon*mi
           j=(clat(l)-minlat)/tlat*mj
        if(j.le.0 .or.i.le.0 ) goto 987
        !print *,l,i,j
           i=(clon(l)-xlon(1,j))/(xlon(mi,j)-xlon(1,j))*mi
           j=(clat(l)-xlat(i,1))/(xlat(i,mj)-xlat(i,1))*mj
        if(j.le.0 .or.i.le.0 ) goto 987
        !print *,i,j,xlon(mi,j),xlon(1,j),xlat(i,mj),xlat(i,1)
          ist(l)=(clon(l)-xlon(1,j))/(xlon(mi,j)-xlon(1,j))*mi
          jst(l)=(clat(l)-xlat(i,1))/(xlat(i,mj)-xlat(i,1))*mj
         !print *,i,j,ist(l),jst(l)
      end if
987 continue
   end do
   RETURN
   end subroutine localiza
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
!    __
!  / _|_ __ ___  ___     _ __ ___   ___ _ __ ___   ___  _ __ _   _
! | |_| '__/ _ \/ _ \   | '_ ` _ \ / _ \ '_ ` _ \ / _ \| '__| | | |
! |  _| | |  __/  __/   | | | | | |  __/ | | | | | (_) | |  | |_| |
! |_| |_|  \___|\___|___|_| |_| |_|\___|_| |_| |_|\___/|_|   \__, |
!                  |_____|                                   |___/
!>  @brief Deallocates allocated arrays.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/29/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine free_memory
        write(6,170)
        if(allocated(iscc))   deallocate(iscc)
        if(allocated(mcst))   deallocate(mcst)
        if(allocated(capa))   deallocate(capa)
        if(allocated(idcg))   deallocate(idcg)
        if(allocated(pob))    deallocate(pob)
        if(allocated(xlon))   deallocate(xlon)
        if(allocated(xlat))   deallocate(xlat)
        if(allocated(utmxd))  deallocate(utmxd)
        if(allocated(utmyd))  deallocate(utmyd)
        if(allocated(utmzd))  deallocate(utmzd)
        if(allocated(aguardar))deallocate(aguardar)
        if(allocated(lat))    deallocate(lat)
        if(allocated(lon))    deallocate(lon)
        if(allocated(emis))   deallocate(emis)
        if(allocated(ict))    deallocate(ict)
        if(allocated(jct))    deallocate(jct)
        if(allocated(eft))    deallocate(eft)
        write(6,180)

170 format(7x,"XXXXXX  Released memory    XXXXXX")
180 format(7x,"*****  DONE POINT ANNUAL *****")
end subroutine free_memory

end program point_temporal
