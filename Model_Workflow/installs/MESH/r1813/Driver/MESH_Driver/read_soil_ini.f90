!> Description:
!>  Open and read in values from soil.ini file to replace values
!>  calculated using %sand and %clay from CLASS.ini.
!>  Bruce Davison, August 13, 2004
subroutine READ_SOIL_INI(fls, shd, ierr)

    use model_files_variables
    use sa_mesh_common
    use FLAGS

    !> Input variables.
    type(fl_ids) :: fls
    type(ShedGridParams) :: shd

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    integer NTYPE, iun, m, j

    !> Initialize the return status.
    ierr = 0

    !> Return if the option is not active.
    if (SOILINIFLAG /= 5) return

    !> Open the file.
    call reset_tab()
    call print_message('READING: ' // trim(fls%fl(mfk%f54)%fn))
    call increase_tab()
    call print_message('REMARK: This file supports only 3 soil layers.')
    iun = fls%fl(mfk%f54)%iun
    open(iun, file = fls%fl(mfk%f54)%fn, status = 'old', action = 'read', iostat = ierr)
    if (ierr /= 0) then
        call print_error('Unable to open the file. Check if the file exists or update SOILINIFLAG.')
        call print_message('SOILINIFLAG 1 - MESH will use the soil percentages as specified.')
        call print_message('SOILINIFLAG 2 - MESH will adjust soil percentages in favor of sand.')
        call print_message('SOILINIFLAG 3 - MESH will adjust soil percentages in favor of clay.')
        call print_message('SOILINIFLAG 4 - MESH will proportionally adjust the soil percentages.')
        return
    end if

    !> Assign local variables.
    NTYPE = shd%lc%NTYPE

    !> Read variables from the file.
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%thpor(m, 1), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%thpor(m, 2), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%thpor(m, 3), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%thlret(m, 1), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%thlret(m, 2), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%thlret(m, 3), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%thlmin(m, 1), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%thlmin(m, 2), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%thlmin(m, 3), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%bi(m, 1), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%bi(m, 2), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%bi(m, 3), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%psisat(m, 1), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%psisat(m, 2), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%psisat(m, 3), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%grksat(m, 1), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%grksat(m, 2), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%grksat(m, 3), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%hcps(m, 1), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%hcps(m, 2), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%hcps(m, 3), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%tcs(m, 1), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%tcs(m, 2), m = 1, NTYPE)
    read(iun, *, err = 98)
    read(iun, *, err = 98) (pm%gru%tcs(m, 3), m = 1, NTYPE)
    close(iun)

    !> Distribute the variables.
    do m = 1, NTYPE
        do j = 4, shd%lc%IGND
            pm%gru%thpor(m, j) = pm%gru%thpor(m, 3)
            pm%gru%thlret(m, j) = pm%gru%thlret(m, 3)
            pm%gru%thlmin(m, j) = pm%gru%thlmin(m, 3)
            pm%gru%bi(m, j) = pm%gru%bi(m, 3)
            pm%gru%psisat(m, j) = pm%gru%psisat(m, 3)
            pm%gru%grksat(m, j) = pm%gru%grksat(m, 3)
            pm%gru%hcps(m, j) = pm%gru%hcps(m, 3)
            pm%gru%tcs(m, j) = pm%gru%tcs(m, 3)
        end do
    end do

    return

98  ierr = 1
    call print_error('Unable to read the file.')
    return

end subroutine
