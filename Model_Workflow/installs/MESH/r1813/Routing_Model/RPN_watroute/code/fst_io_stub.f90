! For systems without access to rmnlib, used for the binary fst-IO,
! this module implements stub functions for all of the procedures
! called by the remainder of watroute.  Each of these procedures will
! error out with a 'STOP' if actually called, since obviously FST-I/O
! can't work if it isn't linked in.

module fst_io

   contains

   subroutine read_fst(unitNum,iflname,header_only,var_name,iyear,imonth,iday,ihour,ip3,etiket)
      integer :: unitNum, iyear, imonth, iday, ihour, ip3
      character iflname*(*)
      character*1 header_only
      character*4 var_name
      character(len = 12) etiket
      STOP 'read_fst is not implemented in this build!'
   end subroutine
   
   subroutine write_fst(unitNum,infname,var_name,iyear,imonth,iday,ihour,ip31)
      integer :: unitNum, iyear, imonth, iday, ihour, ip31
      character infname*(*)
      character*4 var_name
      STOP 'write_fst is not implemented in this build!'
   end subroutine
  
   subroutine read_shed_fst(unitNum, iflname)
      integer :: unitNum
      character iflname*(*)
      STOP 'read_shed_st is not implemented in this build!'
   end subroutine
   
   subroutine read_flowinit_fst(unitNum,iflname,iyear,imonth,iday,ihour)
      integer :: unitNum, iyear, imonth, iday, ihour
      character iflname*(*)
      STOP 'read_flowinit_fst is not implemented in this build!'
   end subroutine
   
   subroutine write_flowinit_fst(unitNum, flname, iyear, imonth, iday, ihour)
      integer :: unitNum, iyear, imonth, iday, ihour
      character flname*(*)
      STOP 'write_flowinit_fst is not implemented in this build!'
   end subroutine

end module fst_io
