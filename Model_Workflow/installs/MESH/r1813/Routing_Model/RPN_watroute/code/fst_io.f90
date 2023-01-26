! Dummy file to lump all of the RPN-standard I/O utilities in one
! single module for inclusion.  This way, a separate 'stub' module
! implementing the same interface can be used on systems that do
! not have rmnlib available. 

module fst_io

   use read_fst_mod
   use write_fst_mod
   use read_shed_fst_mod
   use read_flowinit_fst_mod
   use write_flowinit_fst_mod

end module fst_io
