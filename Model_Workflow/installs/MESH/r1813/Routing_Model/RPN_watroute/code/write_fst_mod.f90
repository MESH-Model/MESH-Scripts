! write_fst.f90 -- function to write out an array (defined, alas, in the global
! variable 'outarray') to a given .fst file.  If such variables are not present,
! then write_fst will also generate appropriate grid variables ('>>' and '^^')
! to properly register the array on-grid.

! On call: writes the data from global variable 'outarray' to the .fst file
! given via 'infname', making use of the FORTRAN unit number 'unitNum'.  The
! data is written under the variable 'var_name', with data flags forresponding
! to the supplied iyear/imonth/iday/ihour.  If any of these values are less than 0,
! then the written date will correspond to Jan 1, 1901 at midnight.

module write_fst_mod

contains

subroutine write_fst(unitNum,infname,var_name,iyear,imonth,iday,ihour,ip31)
   ! Use the global module for access to a few key global variables, most
   ! notably the allocatable 'inarray'

   use area_watflood 

   ! Define variables

   implicit none
   integer :: unitNum, & ! Unit number to use for the .fst file
              dateo,   & ! Dateo parameter for rmnlib -- origin date
              datev,   & ! Computed 'current date' for output
              nhours,  & ! Difference in hours betwen the prior two
              key,     & ! Record key inside .fst file
              nnii,    & ! size of dimension 1
              nnjj,    & !                   2
              nnkk,    & !                   3
              ig1, ig2, ig3, ig4, & ! Grid parameters
              ip1, ip2, ip3, & ! Parameters for >> and ^^
              ier,     & ! Error code
              yyyymmdd,& ! Calculated human-readable date
              hhmmsshh,& ! Calculated human-readable time
              iyear,imonth,iday,ihour,& ! Date/time of variable to read
              ip31 !ip3 value of the variable to be written; ip3 is already used for lat-lon grids here

   character infname*(*)! Given input filename 
   character flname*999! Filename for output
   character*12 etiket ! variable label
   character*4 var_name ! variable name in the file, also 'nomvar'

   ! Since finding >> / ^^ parameters in the file already lets us match
   ! the output to the already-written grid, we need to be able to read
   ! those records -- these parameters are necessary for fstprm
   integer o_dateo, & ! date time stamp
           o_deet,  & ! Time step length in seconds
           o_npas,  & ! Time steup number
           o_ni, o_nj, o_nk, &
           o_nbits, & ! Number of bits kept
           o_datyp, & ! Data type
           o_ip1, o_ip2, o_ip3, &
           o_ig1, o_ig2, o_ig3, o_ig4, &
           o_swa,   & ! Starting word address
           o_lng,   & ! Record length
           o_dltf,  & ! Delete flag
           o_ubc,   & ! Unused bit count
           o_extra1, o_extra2, o_extra3

   character*12 o_etiket    ! Run label
   character*5  o_nomvar    ! Variable name
   character*3  o_typvar    ! Data type
   character*2  o_grtyp     ! Grid type


   ! External functions from rmnlib
   integer, external :: &  
            fnom,      & ! Associate filelname with unit number
            fstouv,    & ! Open the .fst file
            fstinf,    & ! Search for a record
            fstecr,    & ! Write a record
            fstfrm,    & ! Close the fst file
            fstvoi,    & ! Debugging: output fst listing
            fclos,     & ! Disassociate the filename and unit number
            fstprm,    & ! Parameters of searched-for record
            newdate,   & ! Date manipulation functions
            fstopc       ! Global option settings

   ! Temporary arrays for data.  Note that this uses the globally-
   ! defined xcount and ycount variables, so those must be properly
   ! set before the first call to write_fst.  If they are not
   ! initialized, then these allocations may do strange things.
   integer j ! Index
   real     latgridcnt(ycount),  & ! Latitude grid
            longridcnt(xcount),  & ! Longtitude grid
            tmparray(xcount,ycount), & ! Temporary array for data
            work(1)             ! Token work array for fstecr

   flname = trim(adjustl(infname))


   ! Open the .fst file for random-access I/O

   !ier = fnom(unitNum, flname, 'STD+RND', 0)
   ier = fnom(unitNum, flname, 'STD+RND', 0)
   if (ier < 0) then
      print *, 'Could not open ', flname(:), ' as ', unitNum, ' for writing'
      STOP 'fst error'
   endif

   ier = fstouv(unitNum, 'STD+RND')
   if (ier < 0) then
      print *, 'Could not open ', flname, ' as ', unitNum, ' for writing'
      STOP 'fst error'
   endif


   ! Now, search the file for '^^' and '>>' variables. If these are present,
   ! then we can re-use their IP1/2/3 parameters as IG1/2/3 for the real data.
   ! If they are not present, then we must create the latitude and longtitude
   ! grids ourselves.

   ! We'll search only for the latitude variable and trust that it is
   ! representative of the longtitude as well.  Note that this will probably
   ! fail for malformed files (with only lat but not lon) or files with more
   ! than one defined grid.
   etiket = ' '
   key = fstinf(unitNum, nnii, nnjj, nnkk, &
               -1, etiket, -1, -1, -1, ' ', '^^  ')
   !print *, 'Got key ', key
   if (key > 0) then
      ! If we found a record, then we want to read its parameters; the
      ! ip1/2/3 values from the lat/lon grid become replicated as the
      ! ig1/2/3 values for our data

      ier = fstprm(key, o_dateo, o_deet, o_npas, &
                   o_ni, o_nj, o_nk, o_nbits, o_datyp, &
                   o_ip1, o_ip2, o_ip3, o_typvar, o_nomvar, &
                   o_etiket, o_grtyp, o_ig1, o_ig2, o_ig3, &
                   o_ig4, o_swa, o_lng, o_dltf, o_ubc, &
                   o_extra1, o_extra2, o_extra3)
      if (ier < 0) STOP 'Error reading ^^ from fst file'
      ig1 = o_ip1
      ig2 = o_ip2
      ig3 = o_ip3
      ig4 = 0
   else
      ! If there is no record, then we must generate lat/lon grids
      ! from the origin/dx/dy global variables

      ! Initialize the lat/lon grids
      do j=1,ycount,1
         latgridcnt(j) = yorigin + ydelta*(j-0.5)
      end do
      do j=1,xcount,1
         longridcnt(j) = xorigin + xdelta*(j-0.5)
      end do

      ! Generate a token date
      ier = newdate(dateo, 19010101, 0, 3)

      ! Set ip1/2/3 to arbitrary values
      ip1 = 1001
      ip2 = 1002
      ip3 = 1003

      ! Generate ig1/2/3/4 valuse corresponding to an 'L'-type grid
      ! with origin at (0,0) and base units of 1 degree; this makes
      ! lat/lon literal values
      call cxgaig('L',ig1,ig2,ig3,ig4,0.0,0.0,1.0,1.0)

      etiket='WATROUTE'

      ! Write the grids
      ier = fstecr(latgridcnt,work,-32,unitNum,dateo, &
                   0,0, & ! deet = 0, 0 timesteps taken
                   1,ycount,1,ip1,ip2,ip3,'X', &
                   '^^  ',etiket,'L', &
                   ig1,ig2,ig3,ig4, 5, .true.)
!                   ig1,ig2,ig3,ig4, 5, .false.)
      if (ier < 0) STOP 'Error writing latitude grid'

      ier = fstecr(longridcnt,work,-32,unitNum,dateo, &
                   0,0, & ! deet = 0, 0 timesteps taken
                   xcount,1,1,ip1,ip2,ip3,'X', &
                   '>>  ',etiket,'L', &
                   ig1,ig2,ig3,ig4, 5, .true.)
!                   ig1,ig2,ig3,ig4, 5, .false.)
      if (ier < 0) STOP 'Error writing longtitude grid'

      ! Re-set ig1/2/3/4 for the data variable
      ig1 = ip1
      ig2 = ip2
      ig3 = ip3
      ig4 = 0
   endif

   ! Now, the 'outvar' is written elsewhere in a transposed format, such
   ! that an ASCII representation in .r2c files matches the graphical
   ! interpretation (albeit possibly upside-down).  Since this is not the
   ! format used by .fst files, we must transpose the data

   tmparray = transpose(outarray)

   ! Write the array
   etiket = 'WATROUTE'

   ! Compute the proper date for output
   ier = newdate(dateo,   & ! Datestamp corresponding to "now"
                 (iday) + 100*imonth + 10000*iyear, &
                 0*1000000,3)
   ! Because of inconsistencies regarding midnight as hour=24 in
   ! watroute, it makes the most sense to incorporate hour here
   ! via date math; make a date corresponding to midnight (using
   ! dateo as temporary), and then increment by the appropriate
   ! number of hours.
   call incdat(datev,dateo,ihour)

   if (ier < 0) STOP 'Error computing output datev'
   ier = newdate(dateo, & ! Datestamp corresponding to the start
                 day1 + 100*month1 + 10000*yr1, &
                 hour1*1000000,3)
   if (ier < 0) STOP 'Error computing output dateo'
   call difdat(datev,dateo,nhours)

   ier = fstecr(tmparray, work, -32, & ! 'pack' 32 bits per value, a full single-prec float
                unitNum, & ! Unit number
                dateo,& ! dateo -- origin date
                3600, & ! Time step size -- assume 1 ihour
                nhours, & ! Number of time steps since beginning
                xcount, & ! dimension 1 size
                ycount, & ! dimension 2 size
                1,      & ! dimension 3 size
                0,      & ! ip1 -- vertical level
                nhours, & ! ip2 -- hour
                ip31,   & ! ip3 -- user-defined
                'P',    & ! typvar -- 'P' for forecast to follow r2c_2_ftn
                var_name, & ! nomvar -- variable name
                etiket, & ! etiket -- label
                'Z',    & ! grtyp -- grid type; Z for non-equispaced cartesian
                ig1, ig2, ig3, ig4, & ! Grid parameters
                133,    & ! Variable type -- compressed floating point
                .true.) ! Rewrite flag
!                .false.) ! Rewrite flag
   if (ier < 0) STOP 'Error writing fst file'

   ! Now close the file
   ier = fstfrm(unitNum)
   ier = fclos(unitNum)
end subroutine

end module write_fst_mod
