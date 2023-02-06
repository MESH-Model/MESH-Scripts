! read_shed_fst -- reads the watershed (shd) and parameter (par) file
!                  from the RPN "standard" file format (fst).  Because
!                  of limitations on variable naming in the file format,
!                  names won't be exactly as represented in the .r2c 
!                  files.  Those changes are documented here.

module read_shed_fst_mod

contains

subroutine read_shed_fst(unitNum, iflname)
   use area_watflood ! Get access to the global variable list

   implicit none ! Practice safe computing

   integer unitNum ! Unit number for the .fst file, so as to not
                   ! conflict with the very large variety used
                   ! elsewhere
   character iflname*(*) ! Assumed-length string for iflname
   character flname*999   ! Internal variable for de-padded iflname

   ! Unfortunately, this routine has to be a bit lower-level
   ! than read/write_flowinit, and we cannot build on the
   ! nice read_fst routines.  Instead of dealing with solely
   ! full-grid variables, here we're dealing with a mix; we
   ! have to specifically treat the lat/lon grids in order to
   ! set up xorigin/yorigin and xdelta/ydelta.  Also, this
   ! routine is responsible for allocating most of the data 
   ! arrays used later on.

   ! Parameters for reading the .fst file; these are used via
   ! the fstinf call.  This avoids requiring many parameters
   ! to fully populate a call to fstprm (get information about
   ! a record).  In exchange, we give up some bulletproofing --
   ! notably, if the shed file is badly/maliciously designed,
   ! it would be possible to include multiple grids and confuse
   ! the x/yorigin/delta process.  This is probably not a big
   ! deal in real usage.

   integer :: nnii, nnjj, nnkk, & ! Grid sizes 
              ier,              & ! Error code
              key,              & ! Access key per-variable
              ii,jj               ! Looping variables
   character :: etiket*12 ! Long-form label for variable name

   ! External functions called from rmnlib
   integer, external :: &
            fnom,       & ! Associate iflname with unit number
            fstouv,     & ! Open .fst file
            fstinf,     & ! Search for a record in the file
            fstluk,     & ! Read an associated record
            fstfrm,     & ! Close the fst file
            fclos,      & ! ... and disassociate the unit number
            fstvoi,     & ! Debugging -- dump table of contents
            fstopc        ! Option settings for rmnlib

   real, allocatable :: & ! Local variables that need to be allocated
         in_array(:,:)    ! Two-dimensional input grid

   ! And we need a companion, integer 2D array to handle RANK/NEXT
   integer, allocatable :: in_intarray(:,:) 

   real pi / 3.14159265358979232846 /

   integer iminarrval,imaxarrval
   real minarrval,maxarrval
   
   ! Suppress non-error printouts
   ier = fstopc('MSGLVL','ERRORS',0)

   ! Unlike more general .fst readers, this code is going to
   ! be very indifferent about the date and parameters of the
   ! variables in the shed files, using wildcard searches when
   ! possible. Consequently, we don't need to care about the
   ! date conversion functions.

   ! Trim the supplied iflname to remove any spaces
   flname = ' '
   flname = trim(adjustl(iflname))

   ! This code does not make significant use of the label
   ! field in the fst file

   etiket = ' '

   ! Open and associate the given iflname with the
   ! supplied unit number.

   ier = fnom(unitNum, flname, 'STD+RND', 0)
   if (ier >= 0) ier = fstouv(unitNum,'STD+RND')
   if (ier < 0) then
      print *, 'Could not open fst file: ', flname, ' as unit ', unitNum
      STOP 'Could not read FST file'
   endif

   ! Now that the file is open, read in the ^^ and >> arrays for
   ! latitude/longtitude.  This will also let us define the overall
   ! 2D grid size.

   key = fstinf(unitNum,nnii,nnjj,nnkk, & ! Unit, grid sizes
                -1, etiket, -1, -1, -1, ' ', & ! Set wildcard flags
                '^^  ') ! Find latitude grid

   if (key < 0) then
      ! Key < 0 correspodns to not-found
      print *, 'Latitude grid ^^ not found in fst file: ', flname
      STOP 'Grid not found'
   end if

   ! Allocate the latitude grid.  nnjj cooresponds to the number
   ! of north/south levels.

   allocate(latgrid(nnjj))

   ! Assign ycount by nnjj.  This should be consistent for all further
   ! non-grid variables

   ycount = nnjj
   ! ycount and imax are used interchangeably in the code
   imax = ycount

   ! Read in ^^

   key = fstluk(latgrid,key,nnii,nnjj,nnkk)

   ! Define yorigin and ydelta based on this grid

   ! Ordinarily, the delta here would be the difference between
   ! two successive points.  However, this may not be entirely
   ! accurate when using 32-bit floats.  Instead, take the
   ! difference between the -last- and -first- point and divide
   ! by the grid size.  This helps avoid subtracting nearly-equal
   ! numbers.
   ydelta = (latgrid(nnjj) - latgrid(1))/(nnjj-1)
   
   ! The grid cells are staggered, such that the grid corners begin
   ! at the origin.  Therefore, we must subtract one half delta from
   ! latgrid(1) to find the origin.

   yorigin = latgrid(1) - 0.5*ydelta

   ! Now, repeat for the longtitude grid

   key = fstinf(unitNum,nnii,nnjj,nnkk, & ! Unit, grid sizes
                -1, etiket, -1, -1, -1, ' ', & ! Set wildcard flags
                '>>  ') ! Find longtitude grid

   if (key < 0) then
      ! Key < 0 corresponds to not-found
      print *, 'Longtitude grid >> not found in fst file: ', flname
      STOP 'Grid not found'
   end if

   xcount = nnii
   ! jmax and xcount are used interghangeably in the code
   jmax = nnii 
   allocate(longrid(nnii))

   key = fstluk(longrid,key,nnii,nnjj,nnkk)
   xdelta = (longrid(nnii)-longrid(1))/(nnii-1)
   xorigin = longrid(1) - 0.5*xdelta

   ! By convetion, watroute specifies longtitude on a -180->+180
   ! range, but standard files are often written with 0->360
   ! degree range.  This causes problems when trying to locate
   ! stream gauges and reservoir release locations.  In order
   ! to maintain compatibility, enforce that the xorigin lie within
   ! this range.  Note that this will still be problematic with a
   ! grid that straddles the 180-degree-longtitude line.

   ! Take the base origin value, and subtract all full revolutions
   ! away from 180W

   xorigin = xorigin - & 
             360*floor((xorigin+180.0)/360.0) 

   ! Various parts of the watroute code use 'al', which is a scale
   ! length for an element.  Scalar parameters like that, however,
   ! are not included here in the .fst file (and are of dubious
   ! utility anyway in .r2c files, but that's just my personal
   ! preference.)  However, the 'al' value can be recalculated here
   ! from the grid.  Since the grid cells are (away from the equator)
   ! more rectangular than square, 'al' will be the arithmetic mean
   ! of the E/W and N/S sizes.

   al = 0.5*6371000.0*pi/180*( & ! Earth radius in m, per degree
           ydelta +            & ! N/S distance
           xdelta*sin(yorigin*pi/180) & ! E/W distance
        )

   ! Set some derived grid areas that are occasionally used later
   ! in the code
   astep=al/1000.
   istep=int(astep)
   step2=astep*astep
   !al = m
   !astep = km
   !step2 = km^2

   ! Allocate the real and integer input arrays
   allocate(in_array(xcount,ycount), in_intarray(xcount,ycount))

   ! RANK

   ! The single most important variable in the shed file is the 'rank'
   ! variable: this defines how the 2D geophysical structure is
   ! translated into the 1D vectors used by the remainder of watroute.
   ! Cells -not- in the watershed but still on the grid have a rank
   ! of 0; other cells are labeled such that the flow is always into
   ! a cell of higher rank.  The safest way of doing this ordering is
   ! to rank the watershed from highest elevation to lowest.


   key = fstinf(unitNum,nnii,nnjj,nnkk, & ! Unit, grid sizes
                -1, etiket, -1, -1, -1, ' ', & ! Set wildcard flags
                'RANK') 

   if (key < 0) then
      ! Key < 0 correspodns to not-found
      print *, 'RANK variable not found in fst file: ', flname
      STOP 'Ordering not found'
   end if

   if (nnii .ne. xcount .or. nnjj .ne. ycount .or. nnkk .ne. 1) then
      print *, 'Received grid size for RANK ', nnii , '*', nnjj, '*', nnkk
      print *, 'Does not match the expected ', xcount , '*', ycount, '* 1'
      STOP 'Grid mismatch'
   end if

   ! Read in the data; this is stored as an integer variable
   ! in the shed file, so we must use the in_intarray

   key = fstluk(in_intarray,key,nnii,nnjj,nnkk)

   if (minval(in_intarray) < 0) then
      print *, 'Invalid rank of ', minval(in_intarray), ' found'
      STOP 'Stop being negative'
   end if

   ! The total number of grid cells is equal to the maximum rank
   na = maxval(in_intarray)

   ! Allocate and assign the xxx, yyy, and s arrays, which store this
   ! mapping.  Note that s is the -transpose- of the order we'd expect
   ! from the file.  This is odd but consistent throughout watroute
   allocate(xxx(na), yyy(na), s(ycount,xcount))
   
   s = transpose(in_intarray)

   ! and loop to assign xxx/yyy
   do ii = 1,xcount
      do jj = 1,ycount
         if (s(jj,ii) > 0) then
            xxx(s(jj,ii)) = ii
            yyy(s(jj,ii)) = jj
         end if
      end do
   end do

   ! naa is the number of grid cells in the basin, which should be
   ! equal to the maximum rank, or na - 1.  
   naa = na - 1

   ! Now, allocate, read, and assign the remaining variables.

   ! NEXT -- rank of the "flow into" cell, for each cell in the basin.
   !         This is an -integer- field in the shed file.

   allocate(next(na))
   key = fstinf(unitNum,nnii,nnjj,nnkk, & ! Unit, grid sizes
                -1, etiket, -1, -1, -1, ' ', & ! Set wildcard flags
                'NEXT') 

   if (key < 0) then
      ! Key < 0 correspodns to not-found
      print *, 'NEXT variable not found in fst file: ', flname
      STOP 'Invalid '
   end if

   if (nnii .ne. xcount .or. nnjj .ne. ycount .or. nnkk .ne. 1) then
      print *, 'Received grid size for NEXT ', nnii , '*', nnjj, '*', nnkk
      print *, 'Does not match the expected ', xcount , '*', ycount, '* 1'
      STOP 'Grid mismatch'
   end if

   ! Read in the data; this is stored as an integer variable
   ! in the shed file, so we must use the in_intarray

   key = fstluk(in_intarray,key,nnii,nnjj,nnkk)

   if (minval(in_intarray) < 0) then
      print *, 'Invalid next of ', minval(in_intarray), ' found'
      STOP 'Stop being negative'
   end if

   ! Assign to the next vector
   forall(ii=1:na) next(ii) = in_intarray(xxx(ii),yyy(ii))

   ! The remaining variablesare all reals, so they can be taken
   ! care of via a single subroutine. 

   ! DA -- drainage area for a given cell.  This accumulates going
   !       downstream
   call readit('da  ')

   ! Copy to the da array
   allocate(da(na))
   forall(ii=1:na) da(ii) = in_array(xxx(ii),yyy(ii))
   if (minval(da) < 0) then
      print *, 'Invalid drainage area minimum (', minval(da),') < 0'
      stop 'Areas must be >= 0'
   end if


   ! bkfl -- 'bankfull' in the r2c file
   ! csubich 2012 Dec 06 -- bankful is used only in diagnostic code
   ! for "graphical output" later on (see rotue.f), so this variable
   ! is effectively optional.  If it is not present in the shed file,
   ! print out a warning and initialize with a token value

   allocate(bnkfll(na))
   key = fstinf(unitNum,nnii,nnjj,nnkk,-1,etiket,-1,-1,-1,' ','bkfl')
   if (key .ge. 0) then
      call readit('bkfl')
      forall(ii=1:na) bnkfll(ii) = in_array(xxx(ii),yyy(ii))
   else
      print *, 'Warning: "bkfl" (bankfull) is not present in the shed file.'
      print *, 'Any output of istate is not reliable as a result.'
      bnkfll = 1
   endif

   ! cslp -- 'chnlslope' in the r2c file
   !         Channel slope.  This one requires some processing,
   !         because the underlying watroute algorithm uses the
   !         square root of slope for channel flow.  So that
   !         the square root is only taken once, it is done here,
   !         at read-in.

   call readit('cslp')
   allocate(slope(na))
   if (minval(in_array) < 0) then
      print *, 'Invalid channel slope variable (negative slopes)'
      stop 
   endif
   forall(ii=1:na) slope(ii) = sqrt(in_array(xxx(ii),yyy(ii)))

   ! elev -- Elevation.  This is present in the r2c file, but is
   !         not actually used in the watroute code, so it is
   !         *not* read in here.

   ! clen -- 'chnllength' -> channel length
   call readit('clen')
   allocate(rl(na))
   if (minval(in_array) < 0) then
      print *, 'Invalid channel length variable'
      stop
   endif
   forall(ii=1:na) rl(ii) = in_array(xxx(ii),yyy(ii))

   ! iak -- reach number.  This parameter is typically an
   ! integer, but is not expected to ever reach high enough
   ! values to be a concern when treated as a real

   ! If this variable does not exist, then we should set ibn
   ! to 0
   
   allocate(ibn(na)) ! ibn -- basin number
   key = fstinf(unitNum,nnii,nnjj,nnkk,-1,etiket,-1,-1,-1,' ','iak ')
   if (key .ge. 0) then
      call readit('iak ')
      forall(ii=1:na) ibn(ii) = int(in_array(xxx(ii),yyy(ii)))
   else
      ibn = 0
   endif

   ! islp -- 'intslope' interior slope, used in overland routing
   ! Commented out Dec 5 2012 by csubich; sl1/2 are not used in the rest
   ! of the code, so there is no need to read this variable
   !call readit('islp')
   !allocate(sl1(na),sl2(na))
   !forall(ii=1:na) sl1(ii) = in_array(xxx(ii),yyy(ii))
   !if (minval(sl1) < 0) then
   !   print *, 'Invalid minimum interior slope'
   !   stop
   !endif
   !sl2 = sqrt(sl1)

   ! chnl -- number of channels in a cell.  This is also
   !         an integer parameter, but it is also effectively
   !         capped (by physics) at a very low (~8) value.
   allocate(ichnl(na))
   if (chnlflg == 'y') then  ! Spatially varying field
     call readit_variable('chnl','VARIABLE    ')
   else                      ! Constant field
     call readit_variable('chnl','CONSTANT    ')
!     call readit('chnl')
   end if
   forall(ii=1:na) ichnl(ii) = nint(in_array(xxx(ii),yyy(ii)))  ! round the number off instead of truncating it
   forall(ii=1:na) ichnl(ii) = min(ichnl(ii),5)                 ! Limit ichnl to 1 to 5
   forall(ii=1:na) ichnl(ii) = max(ichnl(ii),1)
   iminarrval=1e9
   imaxarrval=0
   do ii=1,na
     if (ichnl(ii).lt.iminarrval) iminarrval=ichnl(ii)
     if (ichnl(ii).gt.imaxarrval) imaxarrval=ichnl(ii)
   end do
   write(*,*) 'The min/max values of CHNL: ',iminarrval,imaxarrval

   ! reac -- reach, used to define extent of lakes, another integer.
   call readit('reac')
   allocate(ireach(na))
   forall(ii=1:na) ireach(ii) = int(in_array(xxx(ii),yyy(ii)))

   ! grda -- gridarea, used for the area of a grid cell that's
   !         subject to getting routed (i.e. in the wateshed, not
   !         a lake.)

   call readit('grda')
   allocate(grid_area(na), frac(na))
   forall(ii=1:na) grid_area(ii) = in_array(xxx(ii),yyy(ii))
   ! frac is the area normalized by the scale area of al**2
   frac = grid_area/(al**2)

   !!!! The following values are typically from the 'par' file,
   !    and are used to define parameters that (generally) can be
   !    tweaked for optimization.
   ! Alocate them all in one fell swoop
   allocate(flz(na),flz2(na), pwr(na),r1n(na),r2n(na),rlake(na), &
    mndr(na),aa2(na),aa3(na), aa4(na), theta(na), widep(na), kcond(na))
   manningflg='y'
   ! flz
   call readit('flz ')
   forall(ii=1:na) flz(ii) = in_array(xxx(ii),yyy(ii))

   ! pwr
   call readit('pwr ')
   forall(ii=1:na) pwr(ii) = in_array(xxx(ii),yyy(ii))

   ! r1n                    ! Spatially- and temporally-varying fields are read in rte_sub.f
   if (r1nflg == 'n') then  ! If using values of r1n that are constant over time
     call readit('r1n ')
     forall(ii=1:na) r1n(ii) = in_array(xxx(ii),yyy(ii))
   end if

   ! r2n                    ! Spatially- and temporally-varying fields are read in rte_sub.f
   if (r2nflg == 'n') then  ! If using values of r2n that are constant over time
     call readit('r2n ')
     forall(ii=1:na) r2n(ii) = in_array(xxx(ii),yyy(ii))
   end if

   ! rlake
     call readit('rlak ')
     forall(ii=1:na) rlake(ii) = in_array(xxx(ii),yyy(ii))

   ! mndr -- meander
   if (mndrflg == 'y') then ! Spatially varying field
     call readit_variable('mndr','VARIABLE    ')
   else                     ! Constant field
     call readit_variable('mndr','CONSTANT    ')
!     call readit('mndr')
   end if
   forall(ii=1:na) mndr(ii) = in_array(xxx(ii),yyy(ii))
   minarrval=1e9
   maxarrval=0
   do ii=1,na
     if (mndr(ii).lt.minarrval) minarrval=mndr(ii)
     if (mndr(ii).gt.maxarrval) maxarrval=mndr(ii)
   end do
   write(*,*) 'The min/max values of MNDR: ',minarrval,maxarrval
   write(*,*) ' '

   ! Adjust the calculated channel length by the degree of meandering
   forall(ii=1:na) rl(ii) = rl(ii) * mndr(ii)

   ! aa2/3/4 -- used to determine channel depth as a funtion of drainage area
   call readit('aa2 ')
   forall(ii=1:na) aa2(ii) = in_array(xxx(ii),yyy(ii))
   call readit('aa3 ')
   forall(ii=1:na) aa3(ii) = in_array(xxx(ii),yyy(ii))
   call readit('aa4 ')
   forall(ii=1:na) aa4(ii) = in_array(xxx(ii),yyy(ii))

   ! widp -- widep, width to depth ratio
   call readit('widp')
   forall(ii=1:na) widep(ii) = in_array(xxx(ii),yyy(ii))

   ! thta -- theta, used in wetland routing.  This should probably be made
   !         optional.
   call readit('thta')
   forall(ii=1:na) theta(ii) = in_array(xxx(ii),yyy(ii))

   ! kcnd -- kcond, soil conductivity; same as theta
   call readit('kcnd')
   forall(ii=1:na) kcond(ii) = in_array(xxx(ii),yyy(ii))

   ! For land usage type, only 'water' is significant outside of wetland
   ! routing, and that is defined as the aclass(:,ntype) array.  Wetland
   ! routing itself is conditional on having another class (for wetlands,
   ! one would presume), but that's not represented in the sample date I
   ! have available at the moment.

   ! Check to see if water is even present
   key = fstinf(unitNum,nnii,nnjj,nnkk,-1,etiket,-1,-1,-1,' ','watr')
   if (key .ge. 0) then
      ntype = 1
      allocate(aclass(na,1))
      call readit('watr')
      forall(ii=1:na) aclass(ii,1) = in_array(xxx(ii),yyy(ii))
   else
      ntype = 0
   endif

   ! Close the shed file
   ier = fstfrm(unitNum)
   ier = fclos(unitNum)

   ! Allocate many of the arrays used by the routing code.  This block
   ! is deliberately skipping arrays that reference land use types,
   ! since wetland routing is currently not enabled (see above comment
   ! block), and most of the land use peculiarities are in WATFLOOD
   ! anyway, rather than this version of RPN-Watroute.  Many of these
   ! remaining allocations are probably unnecessary.

   allocate(qi1(na), qi2(na), qo1(na), qo2(na), qo2sim(na), &
            qo2rem(na), qo2remirr(na), &
            qr(na), d2(na), qda(na), cap(na), over(na), &
            qmax(na), res(na), sump(na), store1(na), &
            store2(na), att(na), qbase(na), nreach(maxval(ireach)), &
            totd1(na), totuzs(na), totsnw(na), qstream(na), &
            totchnl(na), totgrid(na), netflow(na), storinit(na), &
            lzs(na), sumrechrg(na), sumrff(na), rechrg(na), &
            qlz(na), qdrng(na), qdrngfs(na), qstrm(na), sumq1(na), &
            sumqint(na), sumq1fs(na), sumqintfs(na), strloss(na), &
            qdrng2(na), qdrngfs2(na), wetwid(na), chawid(na), &
            chadep(na), wstore1(na), wstore2(na), wcap(na), &
            flowxa(na), chaxa(na), satxa(na), wetxa(na), &
            hcha1(na), hcha2(na), hwet1(na), hwet2(na), qin(na), &
            qswevp(na), qswrain(na), qiwet1(na), qiwet2(na), &
            qowet1(na), qowet2(na), wetarea(na), chaarea(na), &
            bin_precip(na), wsat(na), wetfrac(na))
   
contains 
   
   ! Helper subroutine to read in a floating-point constant-valued variable from
   ! the fst file and perform basic sanity checking.
   subroutine readit(varname)
      CHARACTER varname*4 ! Variable name, fixed at length-4
      key = fstinf(unitNum,nnii,nnjj,nnkk, & ! Unit, grid sizes
                   -1, etiket, -1, -1, -1, ' ', & ! Set wildcard flags
                   varname) 

      if (key < 0) then
         ! Key < 0 correspodns to not-found
         print *, varname, ' variable not found in fst file: ', flname
         STOP 'Invalid '
      end if

      if (nnii .ne. xcount .or. nnjj .ne. ycount .or. nnkk .ne. 1) then
         print *, 'Received grid size for ', varname, ' ', nnii , '*', nnjj, '*', nnkk
         print *, 'Does not match the expected ', xcount , '*', ycount, '* 1'
         STOP 'Grid mismatch'
      end if
      key = fstluk(in_array,key,nnii,nnjj,nnkk)
   end subroutine

   ! Helper subroutine to read in a floating-point spatially-varying variable from
   ! the fst file and perform basic sanity checking.
   subroutine readit_variable(varname,etiket)
      CHARACTER varname*4,etiket*12 ! Variable name, fixed at length-4; etiket, up to 12 characters
      key = fstinf(unitNum,nnii,nnjj,nnkk, & ! Unit, grid sizes
                   -1, etiket, -1, -1, -1, ' ', & ! Set wildcard flags
                   varname) 

      if (key < 0) then
         ! Key < 0 correspodns to not-found
         print *, varname, ' variable not found in fst file: ', flname
         STOP 'Invalid '
      end if

      if (nnii .ne. xcount .or. nnjj .ne. ycount .or. nnkk .ne. 1) then
         print *, 'Received grid size for ', varname, ' ', nnii , '*', nnjj, '*', nnkk
         print *, 'Does not match the expected ', xcount , '*', ycount, '* 1'
         STOP 'Grid mismatch'
      end if
      key = fstluk(in_array,key,nnii,nnjj,nnkk)
   end subroutine

end subroutine

end module read_shed_fst_mod
