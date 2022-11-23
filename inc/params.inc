       parameter (maxline=10000000)
       parameter (maxevt=100000)
       parameter (maxsta=1000)
       character*180 line, blank, datal(maxline), inpline
       character*180 header_array(3), data_array(maxline),filename
       character*180 stationsfile, stationsfile1, sfline
       character*90 junk
       character*11 eqid
       character*4 sta, stanm
       character*2 snet, sloc
       character*1 phase
       integer linestat, linestop, nevent, sunit
       integer nstat,nphase,nhead,nrecord, nsta, nevt
       integer sta_list_count, evt_count, total_line
       integer lines,l, nbl, evid, lstat, lend, nlev
       integer lstati(maxevt), lendi(maxevt), nlevi(maxevt)
       integer funit, f_line_count, sl, iosf, ista
       integer year,month,day,hour,min, p_hour, p_min
       integer dleap, inpl, idata
       integer i_year, i_month, i_day
       logical station_in_list
       real mag, err, dist
       real lat, long, rms, smaj, smin, az, depth
       real head_sec, p_diff
       real p_sec, p_sec0 
       real slat,slong,selv,scor
       real latmininp, latmaxinp, lonmininp, lonmaxinp, depmininp, depmaxinp
       double precision head_time,p_time 
