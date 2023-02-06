! csubich

! read_flowinit_fst -- reads flow initialization from a specified .fst 
! file, optionally (if >0) using the given iyear/imonth/iday/ihour to 
! specify the appropriate time index within the .fst file

! This procedure depends upon (and calls) read_fst several times, for
! the following variables:

! qi1 -> qi1, qi2
! qo1 -> qo1, qo2
! qo1 (sim) -> qo2sim
! qo1 (rem) -> qo2rem
! qo1 (remirr) -> qo2remirr
! stor[e1] -> store1, store2
! over -> over
! lzs -> lzs

module read_flowinit_fst_mod

contains

subroutine read_flowinit_fst(unitNum,iflname,iyear,imonth,iday,ihour)

   use area_watflood
   use read_fst_mod

   ! Variable declarations
   integer ::  unitNum, & ! Unit number
               iyear, imonth, iday, ihour, & ! Initial times
               jj ! array index

   character iflname*(*)


   ! and a flag for whether the given date is actually in our .fst file.
   ! This is not a fatal error condition because flow_init(.r2c) has
   ! traditionally only contained one set of fields; allowing multiple
   ! initial conditions is an extension permitted by the .fst format.  If
   ! the date is not found, we'll simply read in the first index seen
   ! and output a warning
   logical :: date_error = .false. 

   !print *, 'got ', unitnum, '"', iflname, '"', iyear, imonth, iday, ihour

   ! debug

   ! qi1 gets stored in the global variables qi1 and qi2
   call read_fst(unitNum,iflname,'0','qi1 ',iyear,imonth,iday,ihour,0,'            ')
   if (found_data_end .eqv. .true.) then
      ! This signals that the variable isn't in the given file for
      ! the specified date, but it -is- in the file for some other
      ! set of parameters.  So, we'll read in a wildcard.
      call read_fst(unitNum,iflname,'0','qi1 ',-1,-1,-1,-1,0,'            ')
      date_error = .true.
      print *, 'Warning: the supplied date ', iyear*10000+imonth*100+iday, 'at hour', ihour
      print *, 'was not found in the initial-conditions file'
      write(*,*) trim(iflname)
      found_data_end = .false. ! reset found_data_end, since it's a signal for rte_sub
      stop
   endif

   ! Copy inarray over to the local variable qi1, which is a 1D vector
   forall (jj = 1:ubound(xxx,1)) qi1(jj) = inarray(xxx(jj),yyy(jj))
   ! and qi2 is a copy of qi1 upon initialization
   qi2 = qi1
   
   ! Repeat this logic for qo1
   ! qo1 gets stored in the global variables qo1 and qo2
   call read_fst(unitNum,iflname,'0','qo1 ',iyear,imonth,iday,ihour,0,'            ')
   if (found_data_end .eqv. .true.) then
      call read_fst(unitNum,iflname,'0','qo1 ',-1,-1,-1,-1,0,'            ')
      if (date_error .eqv. .false.) then
         date_error = .true.
         print *, 'Warning: the supplied date ', iyear*10000+imonth*100+iday, 'at hour', ihour
         print *, 'was not found in the initial-conditions file "', iflname, '"'
         stop
      endif
      found_data_end = .false. ! reset found_data_end, since it's a signal for rte_sub
   endif
   forall (jj = 1:ubound(xxx,1)) qo1(jj) = inarray(xxx(jj),yyy(jj))
   qo2 = qo1

   ! Repeat this logic for qo2sim
   ! qo1 (simulated) gets stored in the global variable qo2sim
   ! qo1 (ip3=10: simulated), qo2sim are the flow values simulated at gauge locations; they are stored in case of flow insertion
   call read_fst(unitNum,iflname,'0','qo1 ',iyear,imonth,iday,ihour,10,'            ')
   if (found_data_end .eqv. .true.) then
      call read_fst(unitNum,iflname,'0','qo1 ',-1,-1,-1,-1,10,'            ')
      if (date_error .eqv. .false.) then
         date_error = .true.
         print *, 'Warning: the supplied date ', iyear*10000+imonth*100+iday, 'at hour', ihour
         print *, 'was not found in the initial-conditions file "', iflname, '"'
         stop
      endif
      found_data_end = .false. ! reset found_data_end, since it's a signal for rte_sub
   endif
   forall (jj = 1:ubound(xxx,1)) qo2sim(jj) = inarray(xxx(jj),yyy(jj))

   ! Repeat this logic for qo2rem
   ! qo1 (removed) gets stored in the global variable qo2rem
   ! qo1 (ip3=20: removed through diversion), qo2rem are the flow values removed at diversion locations; they are stored in case of internal diversion where
   ! no more flow can be added than was removed, to preserve water balance
   call read_fst(unitNum,iflname,'0','qo1 ',iyear,imonth,iday,ihour,20,'            ')
   if (found_data_end .eqv. .true.) then
      call read_fst(unitNum,iflname,'0','qo1 ',-1,-1,-1,-1,20,'            ')
      if (date_error .eqv. .false.) then
         date_error = .true.
         print *, 'Warning: the supplied date ', iyear*10000+imonth*100+iday, 'at hour', ihour
         print *, 'was not found in the initial-conditions file "', iflname, '"'
         stop
      endif
      found_data_end = .false. ! reset found_data_end, since it's a signal for rte_sub
   endif
   forall (jj = 1:ubound(xxx,1)) qo2rem(jj) = inarray(xxx(jj),yyy(jj))

   ! Repeat this logic for qo2remirr
   ! qo1 (removed by irrigation) gets stored in the global variable qo2remirr
   ! qo1 (ip3=30: removed through irrigation), qo2remirr are the flow values removed at irrigation locations; 
   ! they are stored to know how much was actually removed by irrigation 
   call read_fst(unitNum,iflname,'0','qo1 ',iyear,imonth,iday,ihour,30,'            ')
   if (found_data_end .eqv. .true.) then
      call read_fst(unitNum,iflname,'0','qo1 ',-1,-1,-1,-1,30,'            ')
      if (date_error .eqv. .false.) then
         date_error = .true.
         print *, 'Warning: the supplied date ', iyear*10000+imonth*100+iday, 'at hour', ihour
         print *, 'was not found in the initial-conditions file "', iflname, '"'
         stop
      endif
      found_data_end = .false. ! reset found_data_end, since it's a signal for rte_sub
   endif
   forall (jj = 1:ubound(xxx,1)) qo2remirr(jj) = inarray(xxx(jj),yyy(jj))
   
   ! And now for stor, originally store1 in the .r2c files
   call read_fst(unitNum,iflname,'0','stor',iyear,imonth,iday,ihour,0,'            ')
   if (found_data_end .eqv. .true.) then
      call read_fst(unitNum,iflname,'0','stor',-1,-1,-1,-1,0,'            ')
      if (date_error .eqv. .false.) then
         date_error = .true.
         print *, 'Warning: the supplied date ', iyear*10000+imonth*100+iday, 'at hour', ihour
         print *, 'was not found in the initial-conditions file "', iflname, '"'
         stop
      endif
      found_data_end = .false. ! reset found_data_end, since it's a signal for rte_sub
   endif
   forall (jj = 1:ubound(xxx,1)) store1(jj) = inarray(xxx(jj),yyy(jj))
   store2 = store1

   ! and over, which is stored in the variable of the same name
   call read_fst(unitNum,iflname,'0','over',iyear,imonth,iday,ihour,0,'            ')
   if (found_data_end .eqv. .true.) then
      call read_fst(unitNum,iflname,'0','over',-1,-1,-1,-1,0,'            ')
      if (date_error .eqv. .false.) then
         date_error = .true.
         print *, 'Warning: the supplied date ', iyear*10000+imonth*100+iday, 'at hour', ihour
         print *, 'was not found in the initial-conditions file "', iflname, '"'
         stop
      endif
      found_data_end = .false. ! reset found_data_end, since it's a signal for rte_sub
   endif
   forall (jj = 1:ubound(xxx,1)) over(jj) = inarray(xxx(jj),yyy(jj))

   ! and finally lzs
   call read_fst(unitNum,iflname,'0','lzs ',iyear,imonth,iday,ihour,0,'            ')
   if (found_data_end .eqv. .true.) then
      call read_fst(unitNum,iflname,'0','lzs ',-1,-1,-1,-1,0,'            ')
      if (date_error .eqv. .false.) then
         date_error = .true.
         print *, 'Warning: the supplied date ', iyear*10000+imonth*100+iday, 'at hour', ihour
         print *, 'was not found in the initial-conditions file "', iflname, '"'
         stop
      endif
      found_data_end = .false. ! reset found_data_end, since it's a signal for rte_sub
   endif
   forall (jj = 1:ubound(xxx,1)) lzs(jj) = inarray(xxx(jj),yyy(jj))

end subroutine

end module read_flowinit_fst_mod
