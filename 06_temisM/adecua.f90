!
!   adecua.f90
!   
!
!   Created by Agustin on 13/07/17.
!   Copyright 2017 __MyCompanyName__. All rights reserved.
!
subroutine adecua(perfili,idia,perfilo)
integer, INTENT(IN)  :: perfili,idia
integer, INTENT(OUT) :: perfilo

if (perfili.eq.2012) then; perfilo=perfili+(idia-1)*100
else;perfilo=perfili;end if

end subroutine adecua