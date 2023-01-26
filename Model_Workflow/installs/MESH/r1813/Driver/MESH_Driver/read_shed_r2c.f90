!> Description:
!>  Subroutine to read basin properties from a single-frame 'r2c' format
!>  file. Values are parsed by order of RANK and stored in variables
!>  that must be allocated 1:NA.
!>
!> Input variables:
!*  iun: Unit of the input file.
!*  fname: Full path to the file (default: './MESH_drainage_database.r2c').
!*
!> Output variables:
!*  ierr: Return status.
!>
!> Input/output variables:
!*  shd: Basin 'shed' object (properties).
subroutine read_shed_r2c(shd, iun, fname, ierr)

    !> strings: For 'lowercase' function.
    !> sa_mesh_common: For common MESH variables and routines.
    !> ensim_io: For routines to read 'r2c' format file.
    use strings
    use sa_mesh_common
    use ensim_io
    use parse_utilities

    implicit none

    !> Input variables.
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Input/output variables.
    type(ShedGridParams) shd

    !> Local variables.
    type(ensim_keyword), dimension(:), allocatable :: vkeyword
    type(ensim_attr), dimension(:), allocatable :: vattr
    integer nkeyword, nattr, l, x, y, z
    real, dimension(:, :), allocatable :: dat
    real, dimension(:), allocatable :: ffield
    character(len = DEFAULT_LINE_LENGTH) line

    !> Initialize the return status.
    ierr = 0

    !> Open the file and read the header.
    call reset_tab()
    call print_message('READING: ' // trim(fname))
    call increase_tab()
    call open_ensim_input(iun, fname, ierr)
    if (ierr /= 0) return
    call parse_header_ensim(iun, vkeyword, nkeyword, ierr)
    if (ierr /= 0) return

    !> Get keywords.
    ierr = 0
    z = 0
    call get_keyword_value(iun, vkeyword, nkeyword, ':Projection', shd%CoordSys%Proj, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':Ellipsoid', shd%CoordSys%Ellips, z); if (z /= 0) ierr = z
    select case (lowercase(shd%CoordSys%Proj))
        case ('utm')
            call get_keyword_value(iun, vkeyword, nkeyword, ':Zone', shd%CoordSys%Zone, z); if (z /= 0) ierr = z
        case ('rotlatlong')
            call get_keyword_value(iun, vkeyword, nkeyword, ':CentreLatitude', shd%CoordSys%CentreLatitude, z)
            if (z /= 0) ierr = z
            call get_keyword_value(iun, vkeyword, nkeyword, ':CentreLongitude', shd%CoordSys%CentreLongitude, z)
            if (z /= 0) ierr = z
            call get_keyword_value(iun, vkeyword, nkeyword, ':RotationLatitude', shd%CoordSys%RotationLatitude, z)
            if (z /= 0) ierr = z
            call get_keyword_value(iun, vkeyword, nkeyword, ':RotationLongitude', shd%CoordSys%RotationLongitude, z)
            if (z /= 0) ierr = z
    end select
    call get_keyword_value(iun, vkeyword, nkeyword, ':xCount', shd%xCount, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':xOrigin', shd%xOrigin, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':xDelta', shd%xDelta, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':yCount', shd%yCount, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':yOrigin', shd%yOrigin, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':yDelta', shd%yDelta, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':NominalGridSize_AL', shd%AL, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':TotalNumOfGrids', shd%NA, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':NumGridsInBasin', shd%NAA, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':NumRiverClasses', shd%NRVR, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':ClassCount', shd%lc%NTYPE, z); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':DebugGridNo', shd%DebugGridNo, z); if (z /= 0) ierr = z
    if (ierr /= 0) then
        call print_error('Errors occurred reading attributes from: ' // trim(fname))
        return
    end if

    !> Adjust GRU count (exclude impervious).
    shd%lc%NTYPE = shd%lc%NTYPE - 1

    !> Calculate additional grid attributes.
!-    select case (lowercase(shd%CoordSys%Proj))
!-        case ('latlong')
!-            shd%iyMin = int(shd%yOrigin*60.0)
!-            shd%iyMax = int((shd%yOrigin + shd%yCount*shd%yDelta)*60.0)
!-            shd%jxMin = int(shd%xOrigin*60.0)
!-            shd%jxMax = int((shd%xOrigin + shd%xCount*shd%xDelta)*60.0)
!-            shd%GRDE = shd%xDelta*60.0
!-            shd%GRDN = shd%yDelta*60.0
!-        case ('utm')
!-            shd%GRDE = shd%xDelta/1000.0
!-            shd%GRDN = shd%yDelta/1000.0
!-            shd%jxMin = int(shd%xOrigin/1000.0)
!-            shd%jxMax = shd%jxMin + shd%GRDE*(shd%xCount - 1)
!-            shd%iyMin = int(shd%yOrigin/1000.0)
!-            shd%iyMax = shd%iyMin + shd%GRDN*(shd%yCount - 1)
!-        case default
!-            call print_error('Unsupported coordinate system: ' // trim(shd%CoordSys%Proj))
!-            return
!-    end select

    !> Check grid dimension.
    if (shd%NA < 1 .or. shd%NAA < 1) then
        call print_error('No grids are defined inside the basin.')
        write(line, FMT_GEN) shd%NA
        call print_message('Number of grids read from file: ' // trim(adjustl(line)))
        return
    end if
    if (shd%NAA >= shd%NA) then
        call print_warning('No outlets exist in the basin.')
        write(line, FMT_GEN) shd%NA
        call print_message('Total number of grids: ' // trim(adjustl(line)))
        write(line, FMT_GEN) shd%NAA
        call print_message('Total number of grids inside the basin: ' // trim(adjustl(line)))
    end if

    !> Allocate and initialize variables.
    allocate( &
        shd%RNKGRD(shd%yCount, shd%xCount), shd%xxx(shd%NA), shd%yyy(shd%NA), &
        shd%IROUGH(shd%NA), &
        shd%lc%ACLASS(shd%NA, shd%lc%NTYPE + 1), stat = ierr)
    if (ierr /= 0) then
        write(line, FMT_GEN) ierr
        call print_error("Unable to allocate 'shd' variables (error code: " // trim(adjustl(line)) // ").")
        return
    end if
    shd%RNKGRD = 0; shd%xxx = 0; shd%yyy = 0
    shd%IROUGH = 0
    shd%lc%ACLASS = 0.0

    !> Get the list of attributes.
    call parse_header_attribute_ensim(iun, vkeyword, nkeyword, vattr, nattr, ierr)
    if (ierr /= 0) then
        call print_error('Error reading attributes from the header in the file.')
        return
    end if
    if (nattr == 0) then
        ierr = 1
        call print_error('No attributes were found in the file.')
        return
    end if

    !> Advance past the end of the header.
    call advance_past_header(iun, fname, ierr)
    if (ierr /= 0) then
        call print_error('Encountered premature end of file.')
        return
    end if

    !> Read and parse the attribute data.
    call load_data_r2c(iun, fname, vattr, nattr, shd%xCount, shd%yCount, .false., ierr)
    if (ierr /= 0) then
        call print_error('Error reading attribute values in the file.')
        return
    end if

    !> Find and assign 'RANK'.
    do l = 1, nattr
        if (lowercase(vattr(l)%attr) == 'rank') then
            shd%RNKGRD = transpose(int(vattr(l)%val))
            exit
        end if
    end do
    if (all(shd%RNKGRD == 0)) then
        call print_error("Unable to read the 'RANK' attribute.")
        return
    end if

    !> Create the 'xxx' and 'yyy' reference tables.
    do x = 1, shd%xCount
        do y = 1, shd%yCount
            if (shd%RNKGRD(y, x) > 0) then
                shd%xxx(shd%RNKGRD(y, x)) = x
                shd%yyy(shd%RNKGRD(y, x)) = y
            end if
        end do
    end do

    !> Allocate and initialize variables.
    allocate( &
        shd%NEXT(shd%NA), &
        shd%IAK(shd%NA), shd%SLOPE_CHNL(shd%NA), shd%CHNL_LEN(shd%NA), shd%ICHNL(shd%NA), shd%IREACH(shd%NA), &
        shd%DA(shd%NA), shd%AREA(shd%NA), shd%FRAC(shd%NA), &
        shd%BNKFLL(shd%NA), &
        shd%ELEV(shd%NA), shd%SLOPE_INT(shd%NA), &
        shd%DRDN(shd%NA), &
        stat = ierr)
    if (ierr /= 0) then
        write(line, FMT_GEN) ierr
        call print_error("Unable to allocate 'shd' variables (error code: " // trim(adjustl(line)) // ").")
        return
    end if
    shd%NEXT = 0
    shd%IAK = 0; shd%SLOPE_CHNL = 0.0; shd%CHNL_LEN = 0.0; shd%ICHNL = 0; shd%IREACH = 0
    shd%DA = 0.0; shd%AREA = 0.0; shd%FRAC = 0.0
    shd%BNKFLL = 0.0
    shd%ELEV = 0.0; shd%SLOPE_INT = 0.0
    shd%DRDN = 0.0

    !> Scan and assign remaining variables.
    !> Cycle to where land cover (GRU) classes are expected.
    do l = 1, (nattr - (shd%lc%NTYPE + 1))

        !> Assign the data to a vector.
        if (DIAGNOSEMODE) call print_message("Reading '" // trim(vattr(l)%attr) // "'.")
        call r2c_to_rank(iun, vattr, nattr, l, shd%xxx, shd%yyy, shd%NA, ffield, shd%NA, ierr)
        if (ierr /= 0) then
            call print_error("Unable to read the '" // trim(vattr(l)%attr) // "' attribute.")
            return
        end if

        !> Determine and assign to the variable.
        select case (lowercase(vattr(l)%attr))
            case ('next')
                shd%NEXT = int(ffield)
            case ('iak')
                shd%IAK = int(ffield)
            case ('chnlslope')
                shd%SLOPE_CHNL = ffield
            case ('chnllength')
                shd%CHNL_LEN = ffield
            case ('chnl')
                shd%ICHNL = int(ffield)
            case ('reach')
                shd%IREACH = int(ffield)
            case ('da')
                shd%DA = ffield
            case ('gridarea')
                shd%AREA = ffield
                shd%FRAC = ffield/shd%AL/shd%AL
            case ('bankfull')
                shd%BNKFLL = ffield
            case ('elev')
                shd%ELEV = ffield
!?            case ('intslope')
            case ('demslope')
                shd%SLOPE_INT = ffield
            case ('drdn')
                shd%DRDN = ffield

                !> Convert DD from km/km^2 to m/m^2; WATROF expects m/m^2.
                shd%DRDN = ffield/1000.0
            case ('latitude')
                call allocate_variable(shd%ylat, shd%NA, z)
                if (btest(z, pstat%ALLOCATION_ERROR)) then
                    ierr = z
                else
                    shd%ylat = ffield
                end if
            case ('longitude')
                call allocate_variable(shd%xlng, shd%NA, z)
                if (btest(z, pstat%ALLOCATION_ERROR)) then
                    ierr = z
                else
                    shd%xlng = ffield
                end if
        end select
    end do
    if (ierr /= 0) then
        call print_error('Errors occurred assigning attributes from: ' // trim(fname))
        return
    end if

    !> Scan and assign land cover (GRU) classes.
    do l = (nattr - shd%lc%NTYPE), nattr

        !> Assign the data to a vector.
        z = 0
        call r2c_to_rank(iun, vattr, nattr, l, shd%xxx, shd%yyy, shd%NA, ffield, shd%NA, z)
        if (z /= 0) then
            write(line, FMT_GEN) l
            call print_warning('Unable to read Attribute ' // trim(adjustl(line)) // '.')
            cycle
        end if

        !> Assign to the variable.
        shd%lc%ACLASS(:, l - (nattr - shd%lc%NTYPE) + 1) = ffield
    end do

    !> Close the file to free the unit.
    close(iun)

end subroutine
