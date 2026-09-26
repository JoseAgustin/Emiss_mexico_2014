!      _               _
!  ___| |__   ___  ___| | __
! / __| '_ \ / _ \/ __| |/ /
!| (__| | | |  __/ (__|   <
! \___|_| |_|\___|\___|_|\_\
!
!>  @brief Verifies no error in netcdf function call
!>  @param status NetCDF functions return a non-zero status codes on error.
!>  @copyright 1993-2020 University Corporation for Atmospheric Research/Unidata
! Copyright 1993-2020 University Corporation for Atmospheric Research/Unidata
!
!Portions of this software were developed by the Unidata Program at the
!University Corporation for Atmospheric Research.
!
!Redistribution and use in source and binary forms, with or without
!modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice,
! this list of conditions and the following disclaimer.
! 2. Redistributions in binary form must reproduce the above copyright notice,
! this list of conditions and the following disclaimer in the documentation
! and/or other materials provided with the distribution.
! 3. Neither the name of the copyright holder nor the names of its contributors
! may be used to endorse or promote products derived from this software without
! specific prior written permission.
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
! THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
! PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
! CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
! EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
! PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
! OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
! WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
! OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
! OF THE POSSIBILITY OF SUCH DAMAGE.
subroutine check(status)
  use netcdf
  integer, intent ( in) :: status
    if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    stop "Stopped"
  end if
  return
end subroutine check

