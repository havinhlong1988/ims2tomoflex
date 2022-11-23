       subroutine TIMSEC (YEAR,MTH,DAY,HR,MIN,SECS,MSECS)                         
!
       implicit none                                                                               
!-- Total seconds to be returned   
       double precision MSECS                                              
       real             SECS                   
!-- Input date                     
       integer          YR,YEAR,MTH,DAY,HR,MIN      
!-- Flag for leap-year             
       integer          DYR,leap                    
!-- Number of leap-years since 0000
       integer          IYR                    
!-- Number of days in current year 
       integer          YDY
       integer          i                    
!                                    
       yr=year                                                                                     
       DYR = 0
       call leap_year(yr,dyr) ! dyr=1 for leap year                                                                 
!
!   find days since 0000, do not count 0000 as a leap year
!
       msecs=0
       do i=1,yr
          call leap_year(i,leap)
          if(leap.eq.0) then
             msecs=msecs+365
          else
             msecs=msecs+366
          endif
       enddo
!
!   if the last year counted was a leap year, subtract one day since only
!   whole year should count. like for middle of year 4, there were 4 years without
!   leap years (0 1 2 3). leap day is counted for curren tyear below  

       msecs=msecs-leap     
                                                               
!-- Seconds to beginning of     
!-- current year                
       MSECS = MSECS*86400.0                      
!-- January                     
       if (MTH .eq. 1) YDY = DAY                  
!-- February                    
       if (MTH .eq. 2) YDY = DAY + 31             
!-- ....                        
       if (MTH .eq. 3) YDY = DAY + DYR + 59       
       if (MTH .eq. 4) YDY = DAY + DYR + 90                                     
       if (MTH .eq. 5) YDY = DAY + DYR + 120                                    
       if (MTH .eq. 6) YDY = DAY + DYR + 151                                    
       if (MTH .eq. 7) YDY = DAY + DYR + 181                                    
       if (MTH .eq. 8) YDY = DAY + DYR + 212                                    
       if (MTH .eq. 9) YDY = DAY + DYR + 243                                    
       if (MTH .eq.10) YDY = DAY + DYR + 273                                    
       if (MTH .eq.11) YDY = DAY + DYR + 304                                    
       if (MTH .eq.12) YDY = DAY + DYR + 334          
                                 
       MSECS = MSECS + real((YDY-1)*86400 + HR*3600 + MIN*60) + SECS                
       return                                                                   
       end subroutine
! ====================================================================
       subroutine leap_year(year,leap)
!
!   input year, if leap year leap=1, else 0
!   calculates since 0000
!
!   jh nov 2012
!
       implicit none
       integer year,leap

       leap=0
       if(year.lt.0) return
       if(mod(year,400).eq.0) then
          leap=1
       elseif(mod(year,100).eq.0) then
          leap=0
       elseif(mod(year,4).eq.0) then
          leap=1
       endif    
       return
       end subroutine         