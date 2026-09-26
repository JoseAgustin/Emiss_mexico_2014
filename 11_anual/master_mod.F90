!>  @brief Module master
!>
!>   contains variables, subroutines and functions used in many programs
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2021
!                      _
!  _ __ ___   __ _ ___| |_ ___ _ __
! | '_ ` _ \ / _` / __| __/ _ \ '__|
! | | | | | | (_| \__ \ ||  __/ |
! |_| |_| |_|\__,_|___/\__\___|_|
!
module master
;!> model ID for output 0=WRF 1=CHIMERE 2=CMAQ
integer :: model!> day in emissions output file
integer :: idia   ;!> month in emissions output file
integer :: month  ;!> year in emissions output file
integer :: anio   ;!!> If =1  one file with 24 hr , =2  two files of 12hrs each one
integer ::periodo ;!> geographical area selected in namelist_emis.nml
!> Days per month
integer,dimension(12) :: daym ! days in a month
!> start day for summer time period for years 2014 to 2020
integer,dimension(2014:2022) :: inicia ! dia inicial del horario de verano
!> end day for summer time period for years 2014 to 2020
integer,dimension(2014:2022) :: termina  ! dia fin del horario de verano
!> emissions area selected
character(len=12):: zona   ! area de emisiones a estimar
!> Photochemical mechanism selected in namelist_emis.nml
character (len=19) ::  mecha  ! mecanismo quimico seleccionado
!> during summer period  consider timasvaing .true. or not .false.
logical :: lsummer
! number of day in a month
!          jan feb mar apr may jun jul aug sep oct nov dec
    data daym /31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
!              2014 2015 2016 2017 2018 2019 2020 2021 2022
    data inicia  /6,  5,   3,   2,   1,  7,   5,  4, 3/
    data termina /26,25,  30,  29,  28, 27,  25, 31, 30/

common /nlm/daym,mecha,zona
contains
!  _                                          _ _     _
! | | ___  ___     _ __   __ _ _ __ ___   ___| (_)___| |_
! | |/ _ \/ _ \   | '_ \ / _` | '_ ` _ \ / _ \ | / __| __|
! | |  __/  __/   | | | | (_| | | | | | |  __/ | \__ \ |_
! |_|\___|\___|___|_| |_|\__,_|_| |_| |_|\___|_|_|___/\__|
!            |_____|
!>  @brief Reads global namelist input file
!>
!>   for setting up geographical, temporal and chemical mechamism settings
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2021
subroutine lee_namelist
implicit none
  NAMELIST /region_nml/ zona
  NAMELIST /fecha_nml/ idia,month,anio,periodo
  NAMELIST /verano_nml/ lsummer
  NAMELIST /chem_nml/ mecha,model
  integer:: unit_nml=9
  logical existe
  existe = .FALSE.
  write(6,*)' >>>> Reading file - ./namelist_emis.nml'
  inquire ( FILE = './namelist_emis.nml' , EXIST = existe )
  if ( existe ) then
  !  Opening the file.
    open ( FILE   = './namelist_emis.nml' ,      &
    UNIT   =  unit_nml        ,      &
    STATUS = 'OLD'            ,      &
    FORM   = 'FORMATTED'      ,      &
    ACTION = 'READ'           ,      &
    ACCESS = 'SEQUENTIAL'     )
    !  Reading the file
    READ (unit_nml , NML = region_nml )
    READ (unit_nml , NML = fecha_nml)
    READ (unit_nml , NML = verano_nml )
    READ (unit_nml , NML = chem_nml )
    close(unit_nml)
    !WRITE (6    , NML = region_nml )
    !WRITE (6    , NML = fecha_nml )
    !WRITE (6    , NML = verano_nml )
    !WRITE (6    , NML = chem_nml )
    if (trim(mecha).ne."saprc07") model=0
  else
    stop '***** No namelist_emis.nml in .. directory'
  end if
  if (month.lt.1 .or. month.gt.12) then
    print '(A,I3)','Error in month (from 1 to 12) month= ',month
    stop
  end if
  if (idia.gt.daym(month))then
    print '(A,I2,A,I2)','Error in day value: ',idia,' larger than days in month ',daym(month)
    stop
  end if
if (lsummer .and. (anio.gt.2022 .or. anio.lt.2014) )then
  print '(A,I2,A,I2)','Error in anio value: ',anio,' not between 2014 to 2022 ',daym(month)
  stop
end if

end subroutine lee_namelist
!  _
! | | ____   _____ _ __ __ _ _ __   ___
! | |/ /\ \ / / _ \ '__/ _` | '_ \ / _ \
! |   <  \ V /  __/ | | (_| | | | | (_) |
! |_|\_\  \_/ \___|_|  \__,_|_| |_|\___/
!
!>  @brief Identifies if it is summer time period and if it is considered  or not.
!>   Returns 1 if the date is within daysaving time period
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param ida  day in the month
!>   @param mes  month of the year
 function kverano(ida,mes)
    implicit none
    integer :: kverano
    integer, intent(in):: ida,mes

    if (mes.lt.4  .or. mes .gt.10)then
      kverano = 0
      return
    end if
    if (mes.gt.4 .and. mes .lt.10) then
      kverano = 1
#ifndef PGI
      write(6, 233) inicia(anio),termina(anio)
#endif
      return
    end if
    if (mes.eq.4 .and. ida .ge. inicia(anio)) then
      kverano = 1
#ifndef PGI
      write(6, 233) inicia(anio),termina(anio)
#endif

      return
      elseif (mes.eq.10 .and. ida .le. termina(anio)) then
        kverano = 1
#ifndef PGI
        write(6, 233) inicia(anio),termina(anio)
#endif
        return
      else
        kverano =0
        return
    end if
233 format("******  HORARIO de VERANO *******",/,3x,"Abril ",I2,x,"a Octubre ",I2)
end function
!            _                        _ _   _
!  __ _  ___| |_      _ __   ___  ___(_) |_(_) ___  _ __
! / _` |/ _ \ __|    | '_ \ / _ \/ __| | __| |/ _ \| '_ \
!| (_| |  __/ |_     | |_) | (_) \__ \ | |_| | (_) | | | |
! \__, |\___|\__|____| .__/ \___/|___/_|\__|_|\___/|_| |_|
! |___/        |_____|_|
!>  @brief Seach the position by column and row in the grid by cell number the grid contains colum x row ids
!>   @author  D. Herrera Moro
!>   @date  10/12/2022
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param cell   grid id number
!>   @param ncol   grid column number
!>   @param row    grid row id
!>   @param col    grid column id
subroutine get_position( cell,ncol, row, col)
    implicit none
    integer*8, intent(in) :: cell !>  column total number in the grid
    integer  ,intent(in)  :: ncol !>  row in the grid corresponding to cell
    integer, intent(out):: row    !>  column in the grid corresponding to cell
    integer, intent(out):: col
    row = cell/ncol
    col = mod(cell,ncol)
    if (col.eq.0 .and. row.ne.0) col=ncol
    if (col.ne.ncol.or. row.eq.0) then
         row=row+1
    end if

end subroutine

end module master
