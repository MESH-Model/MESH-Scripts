module cropland_irrigation_init

    use cropland_irrigation_variables

    implicit none

    contains

    subroutine runci_init(shd, fls)

        !> For 'il1,il2' indexing and ipid (current node).
        use mpi_module

        !> For 'fls%GENDIR_OUT'.
        use model_files_variables

        !> For 'shd' type (basin information) and 'FCAN' (canopy fractions).
        use sa_mesh_common

        !> For current date and counter.
        use model_dates

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls

        integer NML, k, m, ikey, ierr
        character(len = DEFAULT_LINE_LENGTH) fmt1, fmt2

        if (.not. cifg%PROCESS_ACTIVE) return

        call reset_tab()
        call print_message('CROPLANDIRRIGATION P-EVP component is ACTIVE.')
        call increase_tab()

        !> Local variables.
        NML = shd%lc%NML

        !> Allocate and initialize internal variables.
        allocate(&
            civ%icrop(NML), civ%jdini(NML), civ%jddev(NML), civ%jdmid(NML), civ%jdlate(NML), civ%jdend(NML), &
            civ%vars(civ%fk%kmin:civ%fk%kmax))
        civ%icrop = 0; civ%jdini = 0; civ%jddev = 0; civ%jdmid = 0; civ%jdlate = 0; civ%jdend = 0
        do ikey = civ%fk%kmin, civ%fk%kmax
            allocate( &
                civ%vars(ikey)%lqws2_mm(NML), civ%vars(ikey)%lqws1_mm(NML), &
                civ%vars(ikey)%pre_mm(NML), civ%vars(ikey)%pevp_mm(NML), civ%vars(ikey)%icu_mm(NML))
            civ%vars(ikey)%lqws2_mm = 0.0; civ%vars(ikey)%lqws1_mm = 0.0
            civ%vars(ikey)%pre_mm = 0.0; civ%vars(ikey)%pevp_mm = 0.0; civ%vars(ikey)%icu_mm = 0.0
        end do

        !> Check the parameter values.
        ierr = 0
        do m = 1, shd%lc%NTYPE
            write(fmt1, FMT_GEN) m
            if (pm%gru%fcan(m, 3) > 0.0) then
                write(fmt2, FMT_GEN) pm%gru%fcan(m, 3)
                call print_message( &
                    'GRU ' // trim(adjustl(fmt1)) // ' has ' // trim(adjustl(fmt2)) // ' contributing fraction of crops.')
                if (ciprot%jdsow(m) <= 0) then
                    call print_remark("'jdsow' is zero. Sow date will be determined by the growth index 'GRO'.")
                end if
                if (ciprot%ldini(m) <= 0) then
                    write(fmt2, FMT_GEN) ciprot%ldini(m)
                    call print_error("Parameter has bad value, 'ldini' = " // trim(adjustl(fmt2)) // '.')
                    ierr = ierr + 1
                end if
                if (ciprot%lddev(m) <= 0) then
                    write(fmt2, FMT_GEN) ciprot%lddev(m)
                    call print_error("Parameter has bad value, 'lddev' = " // trim(adjustl(fmt2)) // '.')
                    ierr = ierr + 1
                end if
                if (ciprot%ldmid(m) <= 0) then
                    write(fmt2, FMT_GEN) ciprot%ldmid(m)
                    call print_error("Parameter has bad value, 'ldmid' = " // trim(adjustl(fmt2)) // '.')
                    ierr = ierr + 1
                end if
                if (ciprot%ldlate(m) <= 0) then
                    write(fmt2, FMT_GEN) ciprot%ldlate(m)
                    call print_error("Parameter has bad value, 'ldlate' = " // trim(adjustl(fmt2)) // '.')
                    ierr = ierr + 1
                end if
                if (ciprot%Kcini(m) <= 0) then
                    write(fmt2, FMT_GEN) ciprot%Kcini(m)
                    call print_error("Parameter has bad value, 'Kcini' = " // trim(adjustl(fmt2)) // '.')
                    ierr = ierr + 1
                end if
                if (ciprot%Kcdev(m) <= 0) then
                    write(fmt2, FMT_GEN) ciprot%Kcdev(m)
                    call print_error("Parameter has bad value, 'Kcdev' = " // trim(adjustl(fmt2)) // '.')
                    ierr = ierr + 1
                end if
                if (ciprot%Kcmid(m) <= 0) then
                    write(fmt2, FMT_GEN) ciprot%Kcmid(m)
                    call print_error("Parameter has bad value, 'Kcmid' = " // trim(adjustl(fmt2)) // '.')
                    ierr = ierr + 1
                end if
                if (ciprot%Kclate(m) <= 0) then
                    write(fmt2, FMT_GEN) ciprot%Kclate(m)
                    call print_error("Parameter has bad value, 'Kclate' = " // trim(adjustl(fmt2)) // '.')
                    ierr = ierr + 1
                end if
            else
                call print_message('GRU ' // trim(adjustl(fmt1)) // ' has no crops active.')
            end if
        end do
        if (ierr > 0) then
            call print_error('Errors exist in the configuration.')
            call program_abort()
        end if

        !> Identify crop GRUs.
        civ%icrop(k) = 0
        do k = il1, il2

            !> FCAN(3) identifies as 'crop' in CLASS.ini.
            if (pm%tile%fcan(k, 3) > 0.0) then

                !> Identify the crop.
                civ%icrop(k) = 1

                !> Reset the starting day of the growing season.
                civ%jdini(k) = 0
            end if
        end do

    end subroutine

end module
