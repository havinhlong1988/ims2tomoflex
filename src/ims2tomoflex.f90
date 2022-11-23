       program main
!      Read inventory - ims format and export to tomo_joint format
!      
       include "params.inc"
!      Define the emptyline
       do i=1,180
        blank(i:i)=' '
       enddo
!      
       open(1,file="input",status="old") 
       print*,"reading the input file:"
       inpl=0
       do
        read(1,'(a)', iostat=ios) inpline
        if (ios.lt.0) exit
        if(inpline(1:1).eq.'*') cycle
        inpl=inpl+1
        if(inpl==1) read(inpline,*) filename
        if(inpl==2) read(inpline,*) idata
        if(idata.eq.2) then
          goto 111
        elseif (idata.eq.1) then
          print*,"Attention! Reading the data for local earthquakes!!!"
          goto 112
        endif
 112    if(inpl==3) read(inpline,*) latmininp, latmaxinp
        if(inpl==4) read(inpline,*) lonmininp, lonmaxinp
        if(inpl==5) read(inpline,*) depmininp, depmaxinp
       enddo
 111   close(1) 
       print*,trim(filename), idata,latmininp, latmaxinp, lonmininp, lonmaxinp, depmininp, depmaxinp
       print*,"Reading the buletin file: ", trim(filename)

       stationsfile='Hi-Knet_stations.txt'
       stationsfile1='permanent_west_station.txt'
       open(1, file=trim(filename),status="old")
       open(2,file="evt_flag.dat",status="unknown")
       l=0
       evid=1
       nbl=0
       nlev=0
       nhead = 0
       do
         read(1,'(a)',iostat=ios)line
         if(ios.lt.0) exit
         l=l+1
         nlev=nlev+1
         if(line.eq.blank) then
           nbl = nbl+1
           if(mod(nbl,3).eq.0) then
             lend = l; ! end line of wraped event
             lstat = lend - nlev +1! start line of wraped event
             nlev = 0 ! reset the line counter
             lendi(evid)=lend
             lstati(evid)=lstat
             nlevi(evid)=evid
             evid = evid + 1
           endif
         else 
           read(line,'(a)') datal(l)
          !  print*,datal(l)
         endif
        enddo
! last event flag correction
        nlevi(evid)=evid
        lendi(evid)=l+1
        lstati(evid)=lendi(evid-1)+1
!
       evt_count = evid
       total_line = l
       print*,"Buletin file have ",total_line, "line; with ", evt_count, "event"
       close(1)

       do ii=1,evt_count
         write(2,'(i5,2x,i8,2x,i8)')nlevi(ii), lstati(ii), lendi(ii)
        !  write(0,'(i5,2x,i8,2x,i8)')nlevi(ii), lstati(ii), lendi(ii)
       enddo 
       close(2)

       ! Re-open the file
       open(1, file=trim(filename),status="old") ! input ims file  
       open(2, file="00_evt",status='unknown') ! output catalog
       open(3, file="00_sta",status='unknown') ! output station informations
       open(4, file="00_tt",status='unknown') ! output absolute traveltime informations
       open(5, file="00_report.txt",status='unknown')
       ! now call out the data based on the linestart and linestop
       k = 1 ! start event
!  11    print*,"putin: ",lstati(k),lendi(k),k
 11    call ims2tomoflex(1,datal,lstati(k),lendi(k),nstas,sta_list_count,l,k,&
            idata,latmininp, latmaxinp, lonmininp, lonmaxinp, depmininp, depmaxinp)
       
!  11    print*,"reading the event number: #",k
       k = k +1 ! increase the event count
       if (k.gt.evt_count) then
      !  if (k.gt.1) then
         goto 99
       else
         goto 11
       endif
 99    continue
       call write_stations_output(stationsfile,stationsfile1,3)

       print*,"finish create the traveltime table"
       do ii = 1,5
         close(ii)
       enddo
! Tried to run the python script
!      print*,"run the python file:"
!      call system('sh plot_cata.sh')

      print*,"done!!!"


       end program

      subroutine ims2tomoflex &
      (funit,data_array,linestat,linestop,nstas,sta_list_count,total_line,nevent,&
      idata,latmininp, latmaxinp, lonmininp, lonmaxinp, depmininp, depmaxinp)
         include "params.inc"
!     add clause for the global earthquake select
        if (idata.ne.1) then
        latmininp=0
        latmaxinp=90
        lonmininp=0
        lonmaxinp=180
        depmininp=0
        depmaxinp=6400
        endif
!
        ! print*,"subr:", idata,latmininp, latmaxinp, lonmininp, lonmaxinp, depmininp, depmaxinp
!        now collect the event information from header
         read(data_array(linestat)(7:17),'(a11)') eqid
         read(data_array(linestat+2),'(i4.4,1x,i2.2,1x,i2.2,1x,i2.2,1x,i2.2,1x,f5.2)') &
         year,month,day,hour,min,head_sec
         read(data_array(linestat+2)(33:35),'(f4.2)') rms
         read(data_array(linestat+2)(38:44),'(f7.4)') lat
         read(data_array(linestat+2)(47:54),'(f8.4)') long
         read(data_array(linestat+2)(58:60),'(f3.1)') smaj
         read(data_array(linestat+2)(64:66),'(f3.1)') smin
        !  read(data_array(linestat+2)(68:70),*) az
         read(data_array(linestat+2)(72:76),'(f5.1)') depth
         read(data_array(linestat+5)(8:10),'(f3.1)') mag
         eh=0.0
         ez=0.0
        !  print*,depmininp,depmaxinp,depth
         ! write to evt file
         if (idata.eq.1) then ! clause for local earthquake
           if ((lat.ge.latmininp).and.(lat.le.latmaxinp).and.(long.le.lonmaxinp).and.(long.ge.lonmininp).and.&
           (depth.le.depmaxinp).and.(depth.ge.depmininp)) then
             write(2,102)year,month,day,hour,min,int(head_sec*100),lat,long,depth,mag,eh,ez,rms,nevent
           endif
         else
          write(2,102)year,month,day,hour,min,int(head_sec*100),lat,long,depth,mag,eh,ez,rms,nevent
         endif
         ! write to traveltime file and report file
         write(4,101) nevent
         write(5,104) nevent," # ",year,month,day,hour,min,int(head_sec*100),lat,long,depth,mag,eh,ez,rms 
!   get header abs time
!
      call timsec(year,month,day,hour,min,head_sec,head_time)
      ! write(*,'(f8.2,2x,f50.10)'),head_sec,head_time
!          
         do ic=linestat+8,linestop-1
         
           if(data_array(ic)(20:20).eq.'P'.or.data_array(ic)(20:20).eq.'S') then
             read(data_array(ic)(29:30),'(i2.2)') p_hour
             read(data_array(ic)(32:33),'(i2.2)') p_min
             read(data_array(ic)(35:40),'(f6.3))') p_sec
             read(data_array(ic)(9:12),'(f4.1))') dist
             read(data_array(ic)(48:52),'(f4.1))') az
             
             if ((hour.eq.23).and.(p_hour.eq.0)) then
                print*,"leap day correction!!! increase 1 day for arrival", day
                ! set the pre-correction date
                i_day = day
                i_month = month
                i_year = year
                day = day +1
                if (((month.eq.1).and.(day.eq.32))&
                .or.((month.eq.3).and.(day.eq.32))&
                .or.((month.eq.5).and.(day.eq.32))&
                .or.((month.eq.7).and.(day.eq.32))&
                .or.((month.eq.8).and.(day.eq.32))&
                .or.((month.eq.10).and.(day.eq.32))&
                )then           
                  day = 1
                  month = month+1 
                elseif  ((month.eq.12).and.(day.eq.32)) then
                  day = 1
                  month = 1
                  year = year +1
                elseif ( &
                ((month.eq.4).and.(day.eq.31)) &
                .or.((month.eq.6).and.(day.eq.31)) &
                .or.((month.eq.9).and.(day.eq.31)) &
                .or.(month.eq.11).and.(day.eq.31)) then
                  day = 1
                  month = month +1
                elseif(month.eq.2) then
                  print*,month
                   dleap = 0
                   call leap_year(year,dleap)
                  !  print*,"dleap: ",dleap
                   if ((dleap.eq.1).and.(day.eq.30)) then
                     day = 1
                     month = month+1
                   elseif ((dleap.eq.0).and.(day.eq.29)) then
                     day = 1
                     month = month+1
                   endif
                   call timsec(year,month,day,p_hour,p_min,p_sec,p_time)
                else
                    print*,"what the month is this? ",month
                    print*,"what the day is this? ",day
                    print*, "so it no need fix the month! hehe!"
                endif
                call timsec(year,month,day,p_hour,p_min,p_sec,p_time)
                day = i_day
                month = i_month
                year = i_year
              else 
                call timsec(year,month,day,p_hour,p_min,p_sec,p_time)
              endif

             p_diff=p_time-head_time ! phase time relative to header time
             p_sec0 =head_sec+p_diff   ! phase second relative to header second
             ! return the date for next round
             
             write(4,103) data_array(ic)(1:4),p_diff,1.0,data_array(ic)(20:20)
             write(5,105) data_array(ic)(1:4),p_diff,1.0,data_array(ic)(20:20),dist*111, az
           endif
          enddo
!
 101    format('#',i11)
 102    format(i4.4,i2.2,i2.2,2x,2i2.2,i4.4,2f9.3,f8.2,f5.1,2x,3f5.1,i10)
 103    format(a4,9x,f10.3,2x,f3.1,2x,a1)
 104    format('#',i5,a3,i4.4,i2.2,i2.2,2x,2i2.2,i4.4,2f9.3,f8.2,f5.1,2x,3f5.1)
 105    format(a4,9x,f7.3,2x,f3.1,2x,a1,2(2x,f7.3))
       
      end

      subroutine write_stations_output(stationsfile,stationsfile1,sunit)
        include "params.inc"

        open(303,file=trim(stationsfile),status="old")
        sl = 0
        ista=0
        do
          read(303,'(a)',iostat=iosf) sfline
          if(iosf.lt.0) exit
          read(sfline,301)snet,stanm,sloc,slat,slong,selv,scor
          ista = ista + 1
          ! print*,snet,stanm,sloc,slat,slong,selv,scor,sunit
          if((slat.ne.0).and.(slong.ne.0).and.(selv.ne.0)) then
            write(sunit,302)stanm,slat,slong,selv,ista
            ! write(0,302)stanm,slat,slong,selv,ista
          endif
        enddo
        close(303)

        open(304,file=trim(stationsfile1),status="old")
        do
          read(304,'(a)',iostat=iosf) sfline
          if(iosf.lt.0) exit
          read(sfline,303)stanm,slong,slat,selv
          ista = ista + 1
          if((slat.ne.0).and.(slong.ne.0).and.(selv.ne.0)) then
            write(sunit,302)stanm,slat,slong,selv,ista
            ! write(0,302)stanm,slat,slong,selv,ista
          endif
        enddo
        close(304)
 301    format(a2,1x,a4,1x,a2,1x,f7.4,1x,f7.4,1x,f6.1,1x,f3.1)    
 302    format(a4,2x,3f10.3,i10) 
 303    format(a4,3x,f6.3,3x,f6.3,3x,f4.0)  
      end subroutine
