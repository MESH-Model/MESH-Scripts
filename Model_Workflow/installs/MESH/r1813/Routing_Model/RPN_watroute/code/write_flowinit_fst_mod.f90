! csubich: write_flowinit_fst is a subroutine to output the new set
! of initial conditions to a file of '.fst' format, creating the file
! if necessary.  It is designed as a straight-up replacement for the
! existing write_flowinit subroutine, which outputs to a text-based
! r2c file format -- this has obvious scalability problems for large
! grids.

! By watroute's design, the arrays written out to disk are global
! variables, structured as very long, 1D arrays.  This code will
! re-two-dimensionalize them to the global array 'outarray', which
! is in turn used by write_fst for disk I/O.

module write_flowinit_fst_mod

contains 

subroutine write_flowinit_fst(unitNum, flname, iyear, imonth, iday, ihour)

   use area_watflood
   use write_fst_mod

   integer :: unitNum, & ! Unit number
              iyear, imonth, iday, ihour, & ! Date variables
              jj ! Loop index

   character :: flname*(*) ! Filename


   ! Initial grid inflow: qi1
    do i=1,ycount          ! Reinitialize the output array
      do j=1,xcount
        outarray(i,j)=0.0
      end do
    end do
   forall (jj=1:ubound(qi1,1)) outarray(yyy(jj),xxx(jj))=qi2(jj)
   call write_fst(unitNum,flname,'qi1 ',iyear,imonth,iday,ihour,0)

   ! Initial grid outflow: qo1
    do i=1,ycount          ! Reinitialize the output array
      do j=1,xcount
        outarray(i,j)=0.0
      end do
    end do
   forall (jj=1:ubound(qo1,1)) outarray(yyy(jj),xxx(jj))=qo2(jj)
   call write_fst(unitNum,flname,'qo1 ',iyear,imonth,iday,ihour,0)

   ! Initial grid outflow (purely simulated, not inserted): qo2sim
    do i=1,ycount          ! Reinitialize the output array
      do j=1,xcount
        outarray(i,j)=0.0
      end do
    end do
   forall (jj=1:ubound(qo1,1)) outarray(yyy(jj),xxx(jj))=qo2sim(jj)
   call write_fst(unitNum,flname,'qo1 ',iyear,imonth,iday,ihour,10)

   ! Initial grid diverted flow (removed): qo2rem
    do i=1,ycount          ! Reinitialize the output array
      do j=1,xcount
        outarray(i,j)=0.0
      end do
    end do
   forall (jj=1:ubound(qo1,1)) outarray(yyy(jj),xxx(jj))=qo2rem(jj)
   call write_fst(unitNum,flname,'qo1 ',iyear,imonth,iday,ihour,20)

   ! Initial grid diverted flow (removed): qo2remirr
    do i=1,ycount          ! Reinitialize the output array
      do j=1,xcount
        outarray(i,j)=0.0
      end do
    end do
   forall (jj=1:ubound(qo1,1)) outarray(yyy(jj),xxx(jj))=qo2remirr(jj)
   call write_fst(unitNum,flname,'qo1 ',iyear,imonth,iday,ihour,30)

   ! Grid channel storage: store1 -> 'stor'
    do i=1,ycount          ! Reinitialize the output array
      do j=1,xcount
        outarray(i,j)=0.0
      end do
    end do
   forall (jj=1:ubound(store1,1)) outarray(yyy(jj),xxx(jj))=store2(jj)
   call write_fst(unitNum,flname,'stor',iyear,imonth,iday,ihour,0)

   ! Overbank storage: over
    do i=1,ycount          ! Reinitialize the output array
      do j=1,xcount
        outarray(i,j)=0.0
      end do
    end do
   forall (jj=1:ubound(over,1)) outarray(yyy(jj),xxx(jj))=over(jj)
   call write_fst(unitNum,flname,'over',iyear,imonth,iday,ihour,0)

   ! Lower zone storage: lzs
    do i=1,ycount          ! Reinitialize the output array
      do j=1,xcount
        outarray(i,j)=0.0
      end do
    end do
   forall (jj=1:ubound(lzs,1)) outarray(yyy(jj),xxx(jj))=lzs(jj)
   call write_fst(unitNum,flname,'lzs ',iyear,imonth,iday,ihour,0)

end subroutine

end module write_flowinit_fst_mod
