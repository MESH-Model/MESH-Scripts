module save_basin_output

    use print_routines
    use model_files_variabletypes, only: fl_ids
    use output_variables

    implicit none

    !> String read from run_options.ini.
    character(len = DEFAULT_LINE_LENGTH), save :: BASINAVGWBFILEFLAG = 'daily'
    character(len = DEFAULT_LINE_LENGTH), save :: BASINAVGEBFILEFLAG = 'daily'
    character(len = DEFAULT_LINE_LENGTH), save :: BASINAVGEVPFILEFLAG = 'none'
    character(len = DEFAULT_LINE_LENGTH), save :: BASINSWEOUTFLAG = 'none'
    character(len = DEFAULT_LINE_LENGTH), save :: STREAMFLOWOUTFLAG = 'daily'
    character(len = DEFAULT_LINE_LENGTH), save :: REACHOUTFLAG = 'daily'

    private

    public run_save_basin_output_init, run_save_basin_output, run_save_basin_output_finalize
    public run_save_basin_output_resume_read, run_save_basin_output_resume_save, run_save_basin_update_stg_ini
    public BASINAVGWBFILEFLAG, BASINAVGEBFILEFLAG, BASINAVGEVPFILEFLAG, BASINSWEOUTFLAG, STREAMFLOWOUTFLAG, REACHOUTFLAG

    !> Global types.

    !> For basin water balance.

!-    type BasinWaterBalance
!-        real, dimension(:), allocatable :: PRE, EVAP, ROF, ROFO, ROFS, ROFB, STG_INI, STG_FIN
!-    end type

!-    type, extends(BasinWaterBalance) :: BasinWaterStorage
!-        real, dimension(:), allocatable :: RCAN, SNCAN, SNO, WSNO, PNDW, LZS, DZS
!-        real, dimension(:, :), allocatable :: LQWS, FRWS
!-    end type

    !> For evaporation related outputs.

!-    type BasinEvp
!-        real EVAP, PEVP, EVPB, ARRD
!-    end type

    !> For energy balance.
    !*  FSIN: Incoming shortwave radiation at the surface. [J m-2 during acc.; W m-2 output].
    !*  ALBT: Total albedo of the surface (visible and near-infrared). [--].
    !*  FSOUT: Outgoing shortwave radiation at the surface. [J m-2 during acc.; W m-2 output].
    !*  FLIN: Incoming longwave radiation at the surface. [J m-2 during acc.; W m-2 output].
    !*  GTE: Effective black-body temperature at the surface. [dC].
    !*  FLOUT: Outgoing longwave radiation at the surface. [J m-2 during acc.; W m-2 output].
    !*  QH: Sensible heat flux at the surface. [J m-2 during acc.; W m-2 output].
    !*  QE: Latent heat flux at the surface. [J m-2 during acc.; W m-2 output].
    !*  GZERO: Heat flux into the ground. [J m-2 during acc.; W m-2 output].
    !*  TA: Air temperature. [dC].
    !*  TCAN: Vegetation canopy temperature. [dC].
    !*  CMAS: Vegetation canopy mass. [kg m-2].
    !*  TSNOW: Snowpack temperature. [dC].
    !*  TPOND: Temperature of ponded water. [dC].
    !*  TBAR: Temperature of soil layers. [dC].
    !*  BAL0: Balance at the beginning of the time-step. [W m-2].
    !*  BAL1: Balance at the end of the time-step. [W m-2].
!-    type BasinEnergyBalance
!-        integer, dimension(:), allocatable :: &
!-            IFS, IPOND, ICAN, ISNOW
!-        real, dimension(:), allocatable :: &
!-            FSIN, ALBT, FSOUT, FLIN, GTE, FLOUT, QH, QE, GZERO, &
!-            TA, TCAN, CMAS, TSNOW, TPOND
!-        real, dimension(:, :), allocatable :: TBAR
!-        real, dimension(:), allocatable :: BAL0, BAL1
!-    end type

    !> Basin output.

!-    type BasinOutput
!-        type(BasinWaterStorage), dimension(:), allocatable :: wb
!-        type(BasinEvp), dimension(:), allocatable :: evpdts
!-        type(BasinEnergyBalance), dimension(:), allocatable :: eb
!-    end type

    type BasinOutputConfigFlag
        integer :: t = 0
        integer, dimension(:), allocatable :: n, ns, nr
        logical :: fout_header = .true.
    end type

    type BasinOutputConfig
        type(BasinOutputConfigFlag) wb, eb, evp, swe
    end type

    !> Variable type: WF_RTE_fout_stfl
    !>  Description: Internal file keys used for output files for streamflow.
    !>
    !> Variables:
    !*  KDLY: Daily output
    !*  KTS: Per time-step output
    !*  freq: Time intervals of the output (daily, ts).
    !*  fout_hyd: .true. to print observed and simulated values (default).
    !*  fout_bal: .true. to print channel storage terms (optional).
    !*  fout_acc: .true. to print accumulated (cumulative) observed and simulated values (optional).
    !*  fout_header: .true. to print header (default).
    !*  fls: Output file definitions.
    type WF_RTE_fout_stfl
        integer :: KDLY = 0, KTS = 1
        integer :: kmin = 0, kmax = 1
        integer :: freq = 1
        logical :: fout_hyd = .true., fout_bal = .false., fout_acc = .false.
        logical :: fout_header = .true.
        type(fl_ids) :: fls
    end type

    !> Variable type: WF_RTE_fout_rsvr
    !>  Description: Internal file keys used for output files for lakes and reservoirs.
    !>
    !> Variables:
    !*  KTS: Per time-step output
    !*  freq: Time intervals of the output (ts).
    !*  fout_header: .true. to print header (default).
    !*  fls: Output file definitions.
    type WF_RTE_fout_rsvr
        integer :: KDLY = 0, KTS = 1, KHLY = 2
        integer :: kmin = 0, kmax = 2
        integer :: freq = 0
        logical :: fout_header = .true.
        type(fl_ids) :: fls
    end type

    real, dimension(:), allocatable :: WF_QHYD_CUM
    real, dimension(:), allocatable :: WF_QO2_ACC, WF_QO2_ACC_MM, WF_STORE2_ACC_MM

!todo: Move to ro%?
    integer RTE_TS

    !> Local type instances.

    type(BasinOutputConfig), save :: bnoflg

!-    type(BasinOutput), save :: bno

    !> Indices for basin average output.
    !* IKEY_ACC: Accumulated over the run (per time-step).
    !* IKEY_MIN: Min. index of the basin averages (used in the allocation of the variables).
    !* IKEY_MAX: Max. number of indices (used in the allocation of the variables).
    !* IKEY_DLY: Daily average.
    !* IKEY_MLY: Monthly average.
    !* IKEY_HLY: Hourly average.
    !*(IKEY_SSL: Seasonal average.)
!-    integer :: IKEY_ACC = 1, IKEY_DLY = 2, IKEY_MLY = 3, IKEY_HLY = 4, IKEY_TSP = 5, NKEY = 5

    !> Output files
    type(WF_RTE_fout_stfl), save :: WF_RTE_fstflout
    type(WF_RTE_fout_rsvr), save :: WF_RTE_frsvrout

    contains

    !> Global routines.

    subroutine run_save_basin_output_init(fls, shd, cm)

        use model_files_variables
        use sa_mesh_common
        use FLAGS
        use climate_forcing
        use strings

        !> Process modules.
        use WF_ROUTE_config
        use rte_module

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer, parameter :: MaxLenField = 20, MaxArgs = 20, MaxLenLine = 100
        integer NA
        integer NS, NR
        character(len = 4) ffmti
        character(len = 500) fn
        character(MaxLenField), dimension(MaxArgs) :: out_args
        integer nargs
!-        integer NA, NSL, ikey, ii, i
        integer n, l, j, i, iun, ierr
        character(len = 3) nc

        !> Return if basin output has been disabled.
!-        if (BASINBALANCEOUTFLAG == 0) return

        !> Return if not the head node.
        if (.not. ISHEADNODE) return

        !> Parse output file flags.
        if (len_trim(BASINAVGWBFILEFLAG) > 0) then
            call parse_basin_output_flag(shd, BASINAVGWBFILEFLAG, bnoflg%wb)
        end if
        if (len_trim(BASINAVGEBFILEFLAG) > 0) then
            call parse_basin_output_flag(shd, BASINAVGEBFILEFLAG, bnoflg%eb)
        end if
        if (len_trim(BASINAVGEVPFILEFLAG) > 0) then
            call parse_basin_output_flag(shd, BASINAVGEVPFILEFLAG, bnoflg%evp)
        end if
        if (len_trim(BASINSWEOUTFLAG) > 0) then
            call parse_basin_output_flag(shd, BASINSWEOUTFLAG, bnoflg%swe)
        end if

        !> Grab values for common indices.
!-        NA = shd%NA
!-        NSL = shd%lc%IGND

        !> Allocate and zero variables for accumulations.
!-        allocate(bno%wb(NKEY))
!-        allocate(bno%eb(NKEY))
!-        do ikey = 1, NKEY
!-            allocate(bno%wb(ikey)%PRE(NA), bno%wb(ikey)%EVAP(NA), bno%wb(ikey)%ROF(NA), &
!-                     bno%wb(ikey)%ROFO(NA), bno%wb(ikey)%ROFS(NA), bno%wb(ikey)%ROFB(NA), &
!-                     bno%wb(ikey)%RCAN(NA), bno%wb(ikey)%SNCAN(NA), &
!-                     bno%wb(ikey)%SNO(NA), bno%wb(ikey)%WSNO(NA), bno%wb(ikey)%PNDW(NA), &
!-                     bno%wb(ikey)%LQWS(NA, NSL), bno%wb(ikey)%FRWS(NA, NSL), &
!-                     bno%wb(ikey)%LZS(NA), bno%wb(ikey)%DZS(NA), &
!-                     bno%wb(ikey)%STG_INI(NA), bno%wb(ikey)%STG_FIN(NA))
!-            bno%wb(ikey)%PRE = 0.0
!-            bno%wb(ikey)%EVAP = 0.0
!-            bno%wb(ikey)%ROF = 0.0
!-            bno%wb(ikey)%ROFO = 0.0
!-            bno%wb(ikey)%ROFS = 0.0
!-            bno%wb(ikey)%ROFB = 0.0
!-            bno%wb(ikey)%RCAN = 0.0
!-            bno%wb(ikey)%SNCAN = 0.0
!-            bno%wb(ikey)%SNO = 0.0
!-            bno%wb(ikey)%WSNO = 0.0
!-            bno%wb(ikey)%PNDW = 0.0
!-            bno%wb(ikey)%LQWS = 0.0
!-            bno%wb(ikey)%FRWS = 0.0
!-            bno%wb(ikey)%LZS = 0.0
!-            bno%wb(ikey)%DZS = 0.0
!-            bno%wb(ikey)%STG_INI = 0.0
!-            allocate( &
!-                bno%eb(ikey)%IFS(NA), bno%eb(ikey)%ICAN(NA), bno%eb(ikey)%ISNOW(NA), bno%eb(ikey)%IPOND(NA), &
!-                bno%eb(ikey)%FSIN(NA), bno%eb(ikey)%ALBT(NA), bno%eb(ikey)%FSOUT(NA), &
!-                bno%eb(ikey)%FLIN(NA), bno%eb(ikey)%GTE(NA), bno%eb(ikey)%FLOUT(NA), &
!-                bno%eb(ikey)%QH(NA), bno%eb(ikey)%QE(NA), &
!-                bno%eb(ikey)%GZERO(NA), &
!-                bno%eb(ikey)%TA(NA), bno%eb(ikey)%TCAN(NA), bno%eb(ikey)%CMAS(NA), &
!-                bno%eb(ikey)%TSNOW(NA), bno%eb(ikey)%TPOND(NA), &
!-                bno%eb(ikey)%TBAR(NA, NSL), &
!-                bno%eb(ikey)%BAL0(NA), bno%eb(ikey)%BAL1(NA))
!-            bno%eb(ikey)%IFS = 0; bno%eb(ikey)%ICAN = 0; bno%eb(ikey)%ISNOW = 0; bno%eb(ikey)%IPOND = 0
!-            bno%eb(ikey)%FSIN = 0.0; bno%eb(ikey)%ALBT = 0.0; bno%eb(ikey)%FSOUT = 0.0
!-            bno%eb(ikey)%FLIN = 0.0; bno%eb(ikey)%GTE = 0.0; bno%eb(ikey)%FLOUT = 0.0
!-            bno%eb(ikey)%QH = 0.0; bno%eb(ikey)%QE = 0.0
!-            bno%eb(ikey)%GZERO = 0.0
!-            bno%eb(ikey)%TA = 0.0; bno%eb(ikey)%TCAN = 0.0; bno%eb(ikey)%CMAS = 0.0
!-            bno%eb(ikey)%TSNOW = 0.0; bno%eb(ikey)%TPOND = 0.0
!-            bno%eb(ikey)%TBAR = 0.0
!-            bno%eb(ikey)%BAL0 = 0.0; bno%eb(ikey)%BAL1 = 0.0
!-        end do

        !> Daily.
        if (btest(bnoflg%wb%t, 0)) then
            open(fls%fl(mfk%f900)%iun, &
                 file = './' // trim(fls%GENDIR_OUT) // '/' // trim(adjustl(fls%fl(mfk%f900)%fn)), &
                 iostat = ierr)
            call allocate_water_balance_out(shd, out%d)
            if (bnoflg%wb%fout_header) then
                call write_water_balance_header(fls, shd, fls%fl(mfk%f900)%iun, 86400)
            end if
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((fls%fl(mfk%f900)%iun*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        if (bnoflg%wb%fout_header) then
                            call write_water_balance_header(fls, shd, (fls%fl(mfk%f900)%iun*1000 + n), 86400)
                        end if
                    end if
                end do
            end if
        end if
        if (btest(bnoflg%eb%t, 0)) then
            open(901, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_energy_balance.csv')
            call allocate_energy_balance_out(shd, out%d)
            if (bnoflg%eb%fout_header) then
                call write_energy_balance_header(fls, shd, 901, 86400)
            end if
        end if
        if (btest(bnoflg%evp%t, 0)) then
            open(910, file = './' // trim(fls%GENDIR_OUT) // '/' // '/Basin_average_evap.csv')
            call allocate_evp_out(shd, out%d)
            if (bnoflg%evp%fout_header) then
                call update_evp_header(fls, shd, 910, 86400)
            end if
        end if

        !> Monthly.
        if (btest(bnoflg%wb%t, 1)) then
            open(902, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Monthly.csv')
            call allocate_water_balance_out(shd, out%m)
            if (bnoflg%wb%fout_header) then
                call write_water_balance_header(fls, shd, 902, 86400)
            end if
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((902*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Monthly_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        if (bnoflg%wb%fout_header) then
                            call write_water_balance_header(fls, shd, (902*1000 + n), 86400)
                        end if
                    end if
                end do
            end if
        end if
        if (btest(bnoflg%eb%t, 1)) then
            open(905, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_energy_balance_Monthly.csv')
            call allocate_energy_balance_out(shd, out%m)
            if (bnoflg%eb%fout_header) then
                call write_energy_balance_header(fls, shd, 905, 86400)
            end if
        end if
        if (btest(bnoflg%evp%t, 1)) then
            open(911, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_evap_Monthly.csv')
            call allocate_evp_out(shd, out%m)
            if (bnoflg%evp%fout_header) then
                call update_evp_header(fls, shd, 911, 86400)
            end if
        end if

        !> Hourly.
        if (btest(bnoflg%wb%t, 2)) then
            open(903, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Hourly.csv')
            call allocate_water_balance_out(shd, out%h)
            if (bnoflg%wb%fout_header) then
                call write_water_balance_header(fls, shd, 903, 3600)
            end if
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((903*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Hourly_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        if (bnoflg%wb%fout_header) then
                            call write_water_balance_header(fls, shd, (903*1000 + n), 3600)
                        end if
                    end if
                end do
            end if
        end if
        if (btest(bnoflg%eb%t, 2)) then
            open(906, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_energy_balance_Hourly.csv')
            call allocate_energy_balance_out(shd, out%h)
            if (bnoflg%eb%fout_header) then
                call write_energy_balance_header(fls, shd, 906, 3600)
            end if
        end if
        if (btest(bnoflg%evp%t, 2)) then
            open(912, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_evap_Hourly.csv')
            call allocate_evp_out(shd, out%h)
            if (bnoflg%evp%fout_header) then
                call update_evp_header(fls, shd, 912, 3600)
            end if
        end if

        !> Per time-step.
        if (btest(bnoflg%wb%t, 3)) then
            open(904, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_ts.csv')
            call allocate_water_balance_out(shd, out%ts)
            if (bnoflg%wb%fout_header) then
                call write_water_balance_header(fls, shd, 904, ic%dts)
            end if
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((904*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_ts_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        if (bnoflg%wb%fout_header) then
                            call write_water_balance_header(fls, shd, (904*1000 + n), ic%dts)
                        end if
                    end if
                end do
            end if
        end if
        if (btest(bnoflg%eb%t, 3)) then
            open(907, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_energy_balance_ts.csv')
            call allocate_energy_balance_out(shd, out%ts)
            if (bnoflg%eb%fout_header) then
                call write_energy_balance_header(fls, shd, 907, ic%dts)
            end if
        end if
        if (btest(bnoflg%evp%t, 3)) then
            open(913, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_evap_ts.csv')
            call allocate_evp_out(shd, out%ts)
            if (bnoflg%evp%fout_header) then
                call update_evp_header(fls, shd, 913, ic%dts)
            end if
        end if

        !> Allocate and zero variables for accumulations.
!-        if (BASINAVGEVPFILEFLAG > 0) then
!-            allocate(bno%evpdts(NKEY))
!-            bno%evpdts(:)%EVAP = 0.0
!-            bno%evpdts(:)%PEVP = 0.0
!-            bno%evpdts(:)%EVPB = 0.0
!-            bno%evpdts(:)%ARRD = 0.0
!-        end if

        if (btest(bnoflg%swe%t, 0)) then
            open(85, file = './' // trim(fls%GENDIR_OUT) // '/basin_SCA_alldays.csv')
            if (bnoflg%swe%fout_header) then
                write(85, 1010) VN_YEAR, VN_JDAY, VN_FSNO
            end if
            open(86, file = './' // trim(fls%GENDIR_OUT) // '/basin_SWE_alldays.csv')
            if (bnoflg%swe%fout_header) then
                write(86, 1010) VN_YEAR, VN_JDAY, VN_SNO
            end if
            call output_variables_activate(out%d%basin, (/ VN_DUMMY_LENGTH, VN_FSNO, VN_SNO /))
        end if

        RTE_TS = ic%dts
        if (WF_RTE_flgs%PROCESS_ACTIVE) RTE_TS = WF_RTE_flgs%RTE_TS
        if (rteflg%PROCESS_ACTIVE) RTE_TS = rteflg%RTE_TS

        NA = shd%NA
        NR = fms%rsvr%n
        NS = fms%stmg%n

        !> Allocate file object.
        allocate( &
            WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%kmin:WF_RTE_fstflout%kmax), &
            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%kmin:WF_RTE_frsvrout%kmax))
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%fn = 'MESH_output_streamflow.csv'
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%iun = 70
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%fn = 'MESH_output_streamflow_ts.csv'
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%iun = 71

        allocate(WF_QO2_ACC(NA), WF_QO2_ACC_MM(NA), WF_STORE2_ACC_MM(NA))
        WF_QO2_ACC = 0.0
        WF_QO2_ACC_MM = 0.0
        WF_STORE2_ACC_MM = 0.0

        if (NR > 0) then

            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KDLY)%fn = 'MESH_output_reach.csv'
            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KDLY)%iun = 708
            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%fn = 'MESH_output_reach_ts.csv'
            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%iun = 708+NR
!            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KHLY)%fn = 'MESH_output_reach_Hourly.csv'
!            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KHLY)%iun = 708+(NR*2)

            if (len_trim(REACHOUTFLAG) == 0) REACHOUTFLAG = 'REACHOUTFLAG default'
            call parse(REACHOUTFLAG, ' ', out_args, nargs)
            WF_RTE_frsvrout%freq = 0
            do j = 1, nargs
                select case (lowercase(out_args(j)))
                    case ('daily')
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KDLY)**WF_RTE_frsvrout%KDLY
                    case ('ts')
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KTS)**WF_RTE_frsvrout%KTS
                    case ('hourly')
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KHLY)**WF_RTE_frsvrout%KHLY
                    case ('default')
                        WF_RTE_frsvrout%freq = 0
                        exit
                    case ('no_header')
                        WF_RTE_frsvrout%fout_header = .false.
                    case ('all')
                        WF_RTE_frsvrout%freq = 0
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KDLY)**WF_RTE_frsvrout%KDLY
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KTS)**WF_RTE_frsvrout%KTS
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KHLY)**WF_RTE_frsvrout%KHLY
                        exit
                    case ('none')
                        WF_RTE_frsvrout%freq = 0
                        exit
                end select
            end do

            !> Open output files for reaches.
            do j = WF_RTE_frsvrout%kmin, WF_RTE_frsvrout%kmax
!temp: Code missing to write hourly values
                if (j == WF_RTE_frsvrout%KHLY) cycle
                if (btest(WF_RTE_frsvrout%freq, j)) then
                    do i = 1, fms%rsvr%n
                        iun = WF_RTE_frsvrout%fls%fl(j)%iun + i
                        write(ffmti, '(i3)') i
                        fn = trim(adjustl(WF_RTE_frsvrout%fls%fl(j)%fn))
                        call insertstr(fn, trim(adjustl(ffmti)), index(fn, 'reach') + len_trim('reach'))
                        open(iun, &
                             file = './' // trim(fls%GENDIR_OUT) // '/' // fn, &
                             status = 'unknown', action = 'write', &
                             iostat = ierr)
                        if (WF_RTE_frsvrout%fout_header) then
                            write(iun, 1010, advance = 'no') VN_YEAR, VN_JDAY
                            if (j == WF_RTE_frsvrout%KTS .or. j == WF_RTE_frsvrout%KHLY) write(iun, 1010, advance = 'no') VN_HOUR
                            if (j == WF_RTE_frsvrout%KTS) write(iun, 1010, advance = 'no') VN_MINS
                            write(iun, 1010, advance = 'no') VN_QI, VN_STGCH, VN_QO
                            write(iun, *)
                        end if
                    end do
                end if
            end do

            iun = 707
            open(iun, file = './' // trim(fls%GENDIR_OUT) // '/' // 'MESH_output_lake_level.csv', &
                 status = 'unknown', action = 'write')
            write(iun, 1010, advance = 'no') VN_YEAR, VN_JDAY
            do l = 1, fms%rsvr%n
                write(ffmti, '(i3)') l
                write(iun, 1010, advance = 'no') VN_ZLVL // trim(adjustl(ffmti))
            end do
            write(iun, *)
        end if

        if (NS > 0) then
            allocate(WF_QHYD_CUM(NS))
            WF_QHYD_CUM = 0.0

            if (len_trim(STREAMFLOWOUTFLAG) == 0) STREAMFLOWOUTFLAG = 'STREAMFLOWOUTFLAG default'
            call parse(STREAMFLOWOUTFLAG, ' ', out_args, nargs)
            WF_RTE_fstflout%freq = 0
            do j = 1, nargs
                select case (lowercase(out_args(j)))
                    case ('daily')
                        WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
                    case ('ts')
                        WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KTS)**WF_RTE_fstflout%KTS
                    case ('bal')
                        WF_RTE_fstflout%fout_bal = .true.
                    case ('acc')
                        WF_RTE_fstflout%fout_acc = .true.
                    case ('default')
                        WF_RTE_fstflout%freq = radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
                        WF_RTE_fstflout%fout_hyd = .true.
                        WF_RTE_fstflout%fout_bal = .false.
                        WF_RTE_fstflout%fout_acc = .false.
                        WF_RTE_fstflout%fout_header = .true.
                        exit
                    case ('no_header')
                        WF_RTE_fstflout%fout_header = .false.
                    case ('all')
                        WF_RTE_fstflout%freq = radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
                        WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KTS)**WF_RTE_fstflout%KTS
                        WF_RTE_fstflout%fout_hyd = .true.
                        WF_RTE_fstflout%fout_bal = .true.
                        WF_RTE_fstflout%fout_acc = .true.
                        exit
                    case ('none')
                        WF_RTE_fstflout%freq = 0
                        exit
                end select
            end do

            !> Open output files for streamflow.
            do j = WF_RTE_fstflout%kmin, WF_RTE_fstflout%kmax
                if (btest(WF_RTE_fstflout%freq, j)) then
                    iun = WF_RTE_fstflout%fls%fl(j)%iun
                    open(iun, &
                         file = './' // trim(fls%GENDIR_OUT) // '/' // trim(adjustl(WF_RTE_fstflout%fls%fl(j)%fn)), &
                         status = 'unknown', action = 'write', &
                         iostat = ierr)
                    if (WF_RTE_fstflout%fout_header) then
                        write(iun, 1010, advance = 'no') VN_YEAR, VN_JDAY
                        if (j == WF_RTE_fstflout%KTS) write(iun, 1010, advance = 'no') VN_HOUR, VN_MINS
                        do i = 1, fms%stmg%n
                            write(ffmti, '(i3)') i
                            if (WF_RTE_fstflout%fout_acc) then
                                write(iun, 1010, advance = 'no') &
                                    VN_QO // VN_MEAS // VN_ACC // trim(adjustl(ffmti)), &
                                    VN_QO // VN_SIM // VN_ACC // trim(adjustl(ffmti))
                            end if
                            if (WF_RTE_fstflout%fout_hyd) then
                                write(iun, 1010, advance = 'no') &
                                    VN_QO // VN_MEAS // trim(adjustl(ffmti)), VN_QO // VN_SIM // trim(adjustl(ffmti))
                            end if
                            if (WF_RTE_fstflout%fout_bal) then
                                write(iun, 1010, advance = 'no') &
                                    'RSIM' // trim(adjustl(ffmti)), VN_STGCH // trim(adjustl(ffmti))
                            end if
                        end do
                        write(iun, *)
                    end if
                end if
            end do
        end if

        !> Allocate output variables.
        call output_variables_activate(out%d%grid, (/ VN_DUMMY_LENGTH, VN_QI, VN_STGCH, VN_QO, VN_ZLVL /))

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_save_basin_output_resume_read(fls, shd)

        use model_files_variables
        use sa_mesh_common

        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
        integer ikey, ierr, iun

        !> Return if not the head node.
        if (.not. ISHEADNODE) return

        !> Open the resume file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.basin_output', status = 'old', action = 'read', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Basin totals for the water balance (old accumulated).
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)
!-        read(iun)

        !> Basin totals for the water balance (for all time-step intervals).
!-        do ikey = 1, NKEY
!-            read(iun) bno%wb(ikey)%PRE(shd%NAA)
!-            read(iun) bno%wb(ikey)%EVAP(shd%NAA)
!-            read(iun) bno%wb(ikey)%ROF(shd%NAA)
!-            read(iun) bno%wb(ikey)%ROFO(shd%NAA)
!-            read(iun) bno%wb(ikey)%ROFS(shd%NAA)
!-            read(iun) bno%wb(ikey)%ROFB(shd%NAA)
!-            read(iun) bno%wb(ikey)%RCAN(shd%NAA)
!-            read(iun) bno%wb(ikey)%SNCAN(shd%NAA)
!-            read(iun) bno%wb(ikey)%SNO(shd%NAA)
!-            read(iun) bno%wb(ikey)%WSNO(shd%NAA)
!-            read(iun) bno%wb(ikey)%PNDW(shd%NAA)
!-            read(iun) bno%wb(ikey)%LQWS(shd%NAA, :)
!-            read(iun) bno%wb(ikey)%FRWS(shd%NAA, :)
!-            read(iun) bno%wb(ikey)%STG_INI(shd%NAA)
!-        end do

        !> Energy balance.
!        read(iun) bno%eb%QEVP
!        read(iun) bno%eb%QH

        !> Close the file to free the unit.
        close(iun)

    end subroutine

    subroutine run_save_basin_update_stg_ini(fls, shd, cm)

        use model_files_variables
        use sa_mesh_common
        use climate_forcing

        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

!-        integer ikey, ii, i

        !> Return if not the head node.
        if (.not. ISHEADNODE) return

        !> Calculate initial storage and aggregate through neighbouring cells.
!-        do ikey = 1, NKEY
!-            bno%wb(ikey)%STG_INI = &
!-                (out%ts%grid%lqwscan + out%ts%grid%sncan + out%ts%grid%sno + out%ts%grid%lqwssno + out%ts%grid%lqwspnd + &
!-                 out%ts%grid%stggw + out%ts%grid%dzs + &
!-                 sum(out%ts%grid%lqwssol, 2) + sum(out%ts%grid%fzwssol, 2))*shd%FRAC
!-        end do
!-        do i = 1, shd%NAA
!-            ii = shd%NEXT(i)
!-            if (ii > 0) then
!-                do ikey = 1, NKEY
!-                    bno%wb(ikey)%STG_INI(ii) = bno%wb(ikey)%STG_INI(ii) + bno%wb(ikey)%STG_INI(i)
!-                end do
!-            end if
!-        end do

    end subroutine

    subroutine run_save_basin_output(fls, shd, cm)

        use model_files_variables
        use sa_mesh_common
        use FLAGS
        use model_dates
        use climate_forcing
        use txt_io

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer nmth, ndy, n
!-        real dnar
        integer k, ki, ierr
        integer l, i, iun

        !> SCA variables
        real TOTAL_AREA, FRAC, basin_SCA, basin_SWE

        !> Return if basin output has been disabled.
!-        if (BASINBALANCEOUTFLAG == 0) return

        !> Return if not the head node.
        if (.not. ISHEADNODE) return

        !> Update the water balance.
!-        call update_water_balance(shd, cm)

        !> For evaporation related outputs.
!-        if (BASINAVGEVPFILEFLAG > 0) then
!-            bno%evpdts(:)%EVAP = bno%evpdts(:)%EVAP + sum(out%ts%grid%et(1:shd%NA)*ic%dts*shd%FRAC)/sum(shd%FRAC)
!-            bno%evpdts(:)%PEVP = bno%evpdts(:)%PEVP + sum(out%ts%grid%potevp(1:shd%NA)*ic%dts*shd%FRAC)/sum(shd%FRAC)
!-            bno%evpdts(:)%EVPB = bno%evpdts(:)%EVPB + sum(out%ts%grid%evpb(1:shd%NA)*shd%FRAC)/sum(shd%FRAC)
!-            bno%evpdts(:)%ARRD = bno%evpdts(:)%ARRD + sum(out%ts%grid%arrd(1:shd%NA)*shd%FRAC)/sum(shd%FRAC)
!-        end if

        !> Update the energy balance.
!-        call update_energy_balance(shd, cm)

        !> Hourly: IKEY_HLY
        if (ic%now%hour /= ic%next%hour) then
!todo: change this to pass the index of the file object.
            if (btest(bnoflg%wb%t, 2)) then
!-                call save_water_balance(shd, 3600, IKEY_HLY)
                call write_water_balance(fls, shd, 903, 3600, shd%NAA, out%h)
                if (allocated(bnoflg%wb%ns)) then
                    do n = 1, size(bnoflg%wb%ns)
                        if (bnoflg%wb%ns(n) > 0) then
                            call write_water_balance(fls, shd, (903*1000 + n), 3600, fms%stmg%meta%rnk(bnoflg%wb%ns(n)), out%h)
                        end if
                    end do
                end if
!-                call reset_water_balance(IKEY_HLY)
            end if
            if (btest(bnoflg%evp%t, 2)) call update_evp(fls, shd, 912, 3600, shd%NAA, out%h)
            if (btest(bnoflg%eb%t, 2)) then
!-                call save_energy_balance(shd, 3600, IKEY_HLY)
                call write_energy_balance(fls, shd, 906, 3600, shd%NAA, out%h)
!-                call reset_energy_balance(IKEY_HLY)
            end if
        end if

        !> Daily: IKEY_DLY
        if (ic%now%day /= ic%next%day) then
            if (btest(bnoflg%wb%t, 0)) then
!-                call save_water_balance(shd, 86400, IKEY_DLY)
                call write_water_balance(fls, shd, fls%fl(mfk%f900)%iun, 86400, shd%NAA, out%d)
                if (allocated(bnoflg%wb%ns)) then
                    do n = 1, size(bnoflg%wb%ns)
                        if (bnoflg%wb%ns(n) > 0) then
                            call write_water_balance(fls, shd, (fls%fl(mfk%f900)%iun*1000 + n), 86400, &
                                                     fms%stmg%meta%rnk(bnoflg%wb%ns(n)), out%d)
                        end if
                    end do
                end if
!-                call reset_water_balance(IKEY_DLY)
            end if
            if (btest(bnoflg%evp%t, 0)) call update_evp(fls, shd, 910, 86400, shd%NAA, out%d)
            if (btest(bnoflg%eb%t, 0)) then
!-                call save_energy_balance(shd, 86400, IKEY_DLY)
                call write_energy_balance(fls, shd, 901, 86400, shd%NAA, out%d)
!-                call reset_energy_balance(IKEY_DLY)
            end if
        end if

        !> Monthly: IKEY_MLY
        if (ic%now%month /= ic%next%month) then

            !> Determine the next day in the month.
            call Julian2MonthDay((ic%now%jday + 1), ic%now%year, nmth, ndy)

            !> Write-out if the next day will be a new month (current day is the last of the month).
            if (ndy == 1 .or. (ic%now%jday + 1) > leap_year(ic%now%year)) then
                call Julian2MonthDay(ic%now%jday, ic%now%year, nmth, ndy)
                if (btest(bnoflg%wb%t, 1)) then
!-                    call save_water_balance(shd, (86400*ndy), IKEY_MLY)
                    call write_water_balance(fls, shd, 902, (86400*ndy), shd%NAA, out%m)
                    if (allocated(bnoflg%wb%ns)) then
                        do n = 1, size(bnoflg%wb%ns)
                            if (bnoflg%wb%ns(n) > 0) then
                                call write_water_balance(fls, shd, (902*1000 + n), (86400*ndy), &
                                                         fms%stmg%meta%rnk(bnoflg%wb%ns(n)), out%m)
                            end if
                        end do
                    end if
!-                    call reset_water_balance(IKEY_MLY)
                end if
                if (btest(bnoflg%evp%t, 1)) call update_evp(fls, shd, 911, (86400*ndy), shd%NAA, out%m)
                if (btest(bnoflg%eb%t, 1)) then
!-                    call save_energy_balance(shd, (86400*ndy), IKEY_MLY)
                    call write_energy_balance(fls, shd, 905, (86400*ndy), shd%NAA, out%m)
!-                    call reset_energy_balance(IKEY_MLY)
                end if
            end if
        end if

        !> Time-step: IKEY_TSP
        if (btest(bnoflg%wb%t, 3)) then
!-            call save_water_balance(shd, ic%dts, IKEY_TSP)
            call write_water_balance(fls, shd, 904, ic%dts, shd%NAA, out%ts)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        call write_water_balance(fls, shd, (904*1000 + n), ic%dts, fms%stmg%meta%rnk(bnoflg%wb%ns(n)), out%ts)
                    end if
                end do
            end if
!-            call reset_water_balance(IKEY_TSP)
        end if
        if (btest(bnoflg%evp%t, 3)) call update_evp(fls, shd, 913, ic%dts, shd%NAA, out%ts)
        if (btest(bnoflg%eb%t, 3)) then
!-            call save_energy_balance(shd, ic%dts, IKEY_TSP)
            call write_energy_balance(fls, shd, 907, ic%dts, shd%NAA, out%ts)
!-            call reset_energy_balance(IKEY_TSP)
        end if

        !> calculate and write the basin avg SCA similar to watclass3.0f5
        !> Same code than in wf_ensim.f subrutine of watclass3.0f8
        !> Especially for version MESH_Prototype 3.3.1.7b (not to be incorporated in future versions)
        !> calculate and write the basin avg SWE using the similar fudge factor!!!
        if (btest(bnoflg%swe%t, 0)) then
            if (ic%now%hour == 12 .and. ic%now%mins == 0) then
!-                basin_SCA = 0.0
!-                basin_SWE = 0.0
!-                TOTAL_AREA = sum(shd%FRAC)
!-                do k = 1, shd%lc%NML
!-                    ki = shd%lc%ILMOS(k)
!-                    FRAC = shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))*shd%FRAC(shd%lc%ILMOS(k))
!-                    basin_SCA = basin_SCA + vs%tile%fsno(k)*FRAC
!-                    basin_SWE = basin_SWE + vs%tile%sno(k)*FRAC
!-                end do
!-                basin_SCA = basin_SCA/TOTAL_AREA
!-                basin_SWE = basin_SWE/TOTAL_AREA
!-                if (btest(bnoflg%swe%t, 0)) then
!-                    write(85, "(i5,',', f10.3)") ic%now%jday, basin_SCA
!-                    write(86, "(i5,',', f10.3)") ic%now%jday, basin_SWE
!-                end if
                write(85, 1010) ic%now%year, ic%now%jday, out%d%basin%fsno(shd%NAA)
                write(86, 1010) ic%now%year, ic%now%jday, out%d%basin%sno(shd%NAA)
            end if
        end if

        if (mod(ic%ts_hourly*ic%dts, RTE_TS) == 0 .and. ro%RUNCHNL) then

            where (shd%DA > 0.0)
                WF_QO2_ACC_MM = WF_QO2_ACC_MM + vs%grid%qo/shd%DA/1000.0*RTE_TS
                WF_STORE2_ACC_MM = WF_STORE2_ACC_MM + vs%grid%stgch/shd%DA/1000.0
            elsewhere
                WF_QO2_ACC_MM = out%NO_DATA
                WF_STORE2_ACC_MM = out%NO_DATA
            end where

            !> Write per time-step output for reaches.
            !> Divide by number of time-steps in routing time-step to resolve issues when RTE_TS > ic%dts.
            if (btest(WF_RTE_frsvrout%freq, WF_RTE_frsvrout%KTS)) then
                do l = 1, fms%rsvr%n
                    iun = WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%iun + l
                    write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins
                    write(iun, 1010, advance = 'no') &
                        out%ts%grid%qi(fms%rsvr%meta%rnk(l))/real(RTE_TS/ic%dts), &
                        out%ts%grid%stgch(fms%rsvr%meta%rnk(l))/real(RTE_TS/ic%dts), &
                        out%ts%grid%qo(fms%rsvr%meta%rnk(l))/real(RTE_TS/ic%dts)
                    write(iun, *)
                end do
            end if

            !> Write per time-step output for streamflow.
            !> Divide by number of time-steps in routing time-step to resolve issues when RTE_TS > ic%dts.
            if (btest(WF_RTE_fstflout%freq, WF_RTE_fstflout%KTS)) then
                iun = WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%iun
                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins
                do i = 1, fms%stmg%n
!todo
                    if (WF_RTE_fstflout%fout_acc) write(iun, 1010, advance = 'no') out%NO_DATA, out%NO_DATA
                    if (WF_RTE_fstflout%fout_hyd) then
                        write(iun, 1010, advance = 'no') &
                            fms%stmg%qomeas%val(i), &
                            out%ts%grid%qo(fms%stmg%meta%rnk(i))/real(RTE_TS/ic%dts)
                    end if
!todo
                    if (WF_RTE_fstflout%fout_bal) write(iun, 1010, advance = 'no') out%NO_DATA, out%NO_DATA
                end do
                write(iun, *)
            end if

        end if

        !> This occurs the last time-step of the day.
        if (ic%now%day /= ic%next%day .and. ro%RUNCHNL) then

            if (fms%rsvr%n > 0) then
                where (out%d%grid%stgch(fms%rsvr%meta%rnk(:)) > 0.0 .and. fms%rsvr%rls%area > 0.0)
                    out%d%grid%zlvl(fms%rsvr%meta%rnk(:)) = out%d%grid%stgch(fms%rsvr%meta%rnk(:))/fms%rsvr%rls%area
                elsewhere
                    out%d%grid%zlvl(fms%rsvr%meta%rnk(:)) = out%NO_DATA
                end where
                iun = 707
                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday
                write(iun, 1010, advance = 'no') (out%d%grid%zlvl(fms%rsvr%meta%rnk(l)), l = 1, fms%rsvr%n)
                write(iun, *)
                if (btest(WF_RTE_frsvrout%freq, WF_RTE_frsvrout%KDLY)) then
                    do l = 1, fms%rsvr%n
                        iun = WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KDLY)%iun + l
                        write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday
                        write(iun, 1010, advance = 'no') &
                            out%d%grid%qi(fms%rsvr%meta%rnk(l)), &
                            out%d%grid%stgch(fms%rsvr%meta%rnk(l)), &
                            out%d%grid%qo(fms%rsvr%meta%rnk(l))
                        write(iun, *)
                    end do
                end if
            end if

            do i = 1, fms%stmg%n
                if (fms%stmg%qomeas%val(i) /= fms%stmg%qomeas%val(i)) then
                    WF_QHYD_CUM(i) = WF_QHYD_CUM(i) + fms%stmg%qomeas%val(i)
                else
                    WF_QHYD_CUM(i) = out%NO_DATA
                end if
            end do

            !> Write daily output for streamflow.
            if (btest(WF_RTE_fstflout%freq, WF_RTE_fstflout%KDLY)) then
                WF_QO2_ACC = WF_QO2_ACC + out%d%grid%qo
                where (WF_STORE2_ACC_MM /= out%NO_DATA) WF_STORE2_ACC_MM = WF_STORE2_ACC_MM/ic%ts_count
                iun = WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%iun
                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday
                do i = 1, fms%stmg%n
                    if (WF_RTE_fstflout%fout_acc) write(iun, 1010, advance = 'no') &
                        WF_QHYD_CUM(i), WF_QO2_ACC(fms%stmg%meta%rnk(i))
                    if (WF_RTE_fstflout%fout_hyd) write(iun, 1010, advance = 'no') &
                        fms%stmg%qomeas%val(i), out%d%grid%qo(fms%stmg%meta%rnk(i))
                    if (WF_RTE_fstflout%fout_bal) write(iun, 1010, advance = 'no') &
                        WF_QO2_ACC_MM(fms%stmg%meta%rnk(i)), WF_STORE2_ACC_MM(fms%stmg%meta%rnk(i))
                end do
                write(iun, *)
            end if
        end if

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_save_basin_output_resume_save(fls, shd)

        use mpi_module
        use model_files_variables
        use sa_mesh_common

        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
        integer i, ierr, iun

        !> Return if not the head node.
        if (.not. ISHEADNODE) return

        !> Open the resume file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.basin_output', status = 'replace', action = 'write', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Basin totals for the water balance.
!-        write(iun) bno%wb(IKEY_ACC)%PRE(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%EVAP(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%ROF(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%ROFO(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%ROFS(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%ROFB(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%LQWS(shd%NAA, :)
!-        write(iun) bno%wb(IKEY_ACC)%FRWS(shd%NAA, :)
!-        write(iun) bno%wb(IKEY_ACC)%RCAN(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%SNCAN(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%SNO(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%WSNO(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%PNDW(shd%NAA)
!-        write(iun) bno%wb(IKEY_ACC)%STG_INI(shd%NAA)

        !> Other accumulators for the water balance.
!-        do i = 1, NKEY
!-            write(iun) bno%wb(i)%PRE(shd%NAA)
!-            write(iun) bno%wb(i)%EVAP(shd%NAA)
!-            write(iun) bno%wb(i)%ROF(shd%NAA)
!-            write(iun) bno%wb(i)%ROFO(shd%NAA)
!-            write(iun) bno%wb(i)%ROFS(shd%NAA)
!-            write(iun) bno%wb(i)%ROFB(shd%NAA)
!-            write(iun) bno%wb(i)%RCAN(shd%NAA)
!-            write(iun) bno%wb(i)%SNCAN(shd%NAA)
!-            write(iun) bno%wb(i)%SNO(shd%NAA)
!-            write(iun) bno%wb(i)%WSNO(shd%NAA)
!-            write(iun) bno%wb(i)%PNDW(shd%NAA)
!-            write(iun) bno%wb(i)%LQWS(shd%NAA, :)
!-            write(iun) bno%wb(i)%FRWS(shd%NAA, :)
!-            write(iun) bno%wb(i)%STG_INI(shd%NAA)
!-        end do

        !> Energy balance.
!-        write(iun) !bno%eb(2)%QEVP
!-        write(iun) !bno%eb(2)%QH

        !> Other accumulators for the water balance.
!-        do i = 1, NKEY
!-            write(iun) bno%wb(i)%LZS(shd%NAA)
!-            write(iun) bno%wb(i)%DZS(shd%NAA)
!-        end do

        !> Close the file to free the unit.
        close(iun)

    end subroutine

    subroutine run_save_basin_output_finalize(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Return if basin output has been disabled.
!-        if (BASINBALANCEOUTFLAG == 0) return

        !> Return if not the head node.
        if (.not. ISHEADNODE) return

    end subroutine

    !> Local routines.

    !>
    !> Description: Subroutine to parse basin_output flag read from run_options.ini
    !>
    !> Input:
    !>  - in_line: basin_output flag read from run_options.ini
    !>
    !> Output:
    !>  - flg: Instance of type(BasinOutputConfigFlag) containing parsed information.
    !>
    subroutine parse_basin_output_flag(shd, in_line, flg)

        use sa_mesh_common
        use strings

        implicit none

        !> Variables.
        type(ShedGridParams) :: shd
        character(len = *), intent(in) :: in_line
        type(BasinOutputConfigFlag) :: flg

        !> Local variables.
        character(len = 20), dimension(100) :: out_args
        integer nargs, n, j, i, ierr
        character(1) :: delim = ' '

        !> Parse the string.
        call parse(in_line, delim, out_args, nargs)

        !> Reset and construct the flag for output frequency.
        flg%t = 0
        do j = 1, nargs
            select case (lowercase(out_args(j)))
                case ('daily')
                    flg%t = flg%t + 1
                case ('monthly')
                    flg%t = flg%t + 2
                case ('hourly')
                    flg%t = flg%t + 4
                case ('ts')
                    flg%t = flg%t + 8
                case ('all')
                    flg%t = 1
                    flg%t = flg%t + 2
                    flg%t = flg%t + 4
                    flg%t = flg%t + 8
                    exit
                case ('default')
                    flg%t = 1
                    exit
                case ('none')
                    flg%t = 0
                    exit
            end select
        end do

        !> Determine output forms.
        do j = 1, nargs
            select case (lowercase(out_args(j)))
                case ('ns')
                    if (allocated(flg%ns)) deallocate(flg%ns)
                    n = 0
                    do i = j + 1, nargs
                        if (is_letter(out_args(i)(1:1))) exit
                        n = n + 1
                    end do
                    if (n == 0) then
                        n = fms%stmg%n
                        allocate(flg%ns(n))
                        do i = 1, n
                            flg%ns(i) = i
                        end do
                    else
                        allocate(flg%ns(n))
                        do i = j + 1, j + n
                            call value(out_args(i), flg%ns(i - j), ierr)
                            if (flg%ns(i - j) > fms%stmg%n) flg%ns(i - j) = 0
                        end do
                    end if
                case ('nr')
                    if (allocated(flg%nr)) deallocate(flg%nr)
                    n = 0
                    do i = j + 1, nargs
                        if (is_letter(out_args(i)(1:1))) exit
                        n = n + 1
                    end do
                    if (n == 0) then
                        n = fms%rsvr%n
                        allocate(flg%nr(n))
                        do i = 1, n
                            flg%nr(i) = i
                        end do
                    else
                        allocate(flg%nr(n))
                        do i = j + 1, j + n
                            call value(out_args(i), flg%nr(i - j), ierr)
                            if (flg%nr(i - j) > fms%rsvr%n) flg%nr(i - j) = 0
                        end do
                    end if
                case ('n')
                    if (allocated(flg%n)) deallocate(flg%n)
                    n = 0
                    do i = j + 1, nargs
                        if (is_letter(out_args(i)(1:1))) exit
                        n = n + 1
                    end do
                    if (n > 0) then
                        allocate(flg%n(n))
                        do i = j + 1, j + n
                            call value(out_args(i), flg%n(i - j), ierr)
                            if (flg%n(i - j) > shd%NAA) flg%n(i - j) = 0
                        end do
                    end if
                case ('no_header')
                    flg%fout_header = .false.
            end select
        end do

    end subroutine

    subroutine allocate_water_balance_out(shd, series)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_series) series

        !> Allocate output variables.
        call output_variables_activate( &
            out%tot%basin, (/ &
                VN_DUMMY_LENGTH, VN_PREC, VN_ET, VN_ROF, VN_OVRFLW, VN_LATFLW, VN_DRAINSOL, VN_STGW /))
        call output_variables_activate( &
            series%basin, (/ &
                VN_DUMMY_LENGTH, VN_PREC, VN_ET, VN_ROF, VN_OVRFLW, VN_LATFLW, VN_DRAINSOL, &
                VN_LQWSCAN, VN_FZWSCAN, VN_SNO, VN_LQWSSNO, VN_LQWSPND, VN_STGGW, VN_LQWSSOL, VN_FZWSSOL, VN_ALWSSOL, &
                VN_STGW /))

    end subroutine

!-    subroutine update_water_balance(shd, cm)

        !> For 'shd' variable type and output variables.
!-        use sa_mesh_common

        !> For 'cm' variable type.
!-        use climate_forcing

        !> For 'ic' variable.
!-        use model_dates

        !> Input variables.
!-        type(ShedGridParams) :: shd
!-        type(clim_info) :: cm

        !> Local variables.
!-        real, dimension(:), allocatable :: PRE, EVAP, ROF, ROFO, ROFS, ROFB, RCAN, SNCAN, SNO, WSNO, PNDW, LZS, DZS
!-        real, dimension(:, :), allocatable :: LQWS, FRWS
!-        integer NA, NSL, ikey, j, ii, i

        !> Allocate temporary variables.
!-        NA = shd%NA
!-        NSL = shd%lc%IGND
!-        allocate(PRE(NA), EVAP(NA), ROF(NA), ROFO(NA), ROFS(NA), ROFB(NA), &
!-                 RCAN(NA), SNCAN(NA), SNO(NA), WSNO(NA), PNDW(NA), &
!-                 LQWS(NA, NSL), FRWS(NA, NSL), LZS(NA), DZS(NA))

        !> Accumulate variables and aggregate through neighbouring cells.
!-        PRE = out%ts%grid%prec*shd%FRAC
!-        EVAP = out%ts%grid%et*ic%dts*shd%FRAC
!-        ROF = out%ts%grid%rof*ic%dts*shd%FRAC
!-        ROFO = out%ts%grid%ovrflw*ic%dts*shd%FRAC
!-        ROFS = out%ts%grid%latflw*ic%dts*shd%FRAC
!-        ROFB = out%ts%grid%drainsol*ic%dts*shd%FRAC
!-        RCAN = out%ts%grid%lqwscan*shd%FRAC
!-        SNCAN = out%ts%grid%fzwscan*shd%FRAC
!-        SNO = out%ts%grid%sno*shd%FRAC
!-        WSNO = out%ts%grid%lqwssno*shd%FRAC
!-        PNDW = out%ts%grid%lqwspnd*shd%FRAC
!-        do j = 1, shd%lc%IGND
!-            LQWS(:, j) = out%ts%grid%lqws(:, j)*shd%FRAC
!-            FRWS(:, j) = out%ts%grid%fzwssol(:, j)*shd%FRAC
!-        end do
!-        LZS = out%ts%grid%stggw*shd%FRAC
!-        DZS = out%ts%grid%dzs*shd%FRAC

        !> Aggregate through neighbouring cells.
!-        do i = 1, shd%NAA
!-            ii = shd%NEXT(i)
!-            if (ii > 0) then
!-                PRE(ii) = PRE(ii) + PRE(i)
!-                EVAP(ii) = EVAP(ii) + EVAP(i)
!-                ROF(ii) = ROF(ii) + ROF(i)
!-                ROFO(ii) = ROFO(ii) + ROFO(i)
!-                ROFS(ii) = ROFS(ii) + ROFS(i)
!-                ROFB(ii) = ROFB(ii) + ROFB(i)
!-                RCAN(ii) = RCAN(ii) + RCAN(i)
!-                SNCAN(ii) = SNCAN(ii) + SNCAN(i)
!-                SNO(ii) = SNO(ii) + SNO(i)
!-                WSNO(ii) = WSNO(ii) + WSNO(i)
!-                PNDW(ii) = PNDW(ii) + PNDW(i)
!-                LQWS(ii, :) = LQWS(ii, :) + LQWS(i, :)
!-                FRWS(ii, :) = FRWS(ii, :) + FRWS(i, :)
!-                LZS(ii) = LZS(ii) + LZS(i)
!-                DZS(ii) = DZS(ii) + DZS(i)
!-            end if
!-        end do

        !> Update run total.
!-        do ikey = 1, NKEY
!-            bno%wb(ikey)%PRE = bno%wb(ikey)%PRE + PRE
!-            bno%wb(ikey)%EVAP = bno%wb(ikey)%EVAP + EVAP
!-            bno%wb(ikey)%ROF = bno%wb(ikey)%ROF + ROF
!-            bno%wb(ikey)%ROFO = bno%wb(ikey)%ROFO + ROFO
!-            bno%wb(ikey)%ROFS = bno%wb(ikey)%ROFS + ROFS
!-            bno%wb(ikey)%ROFB = bno%wb(ikey)%ROFB + ROFB
!-            bno%wb(ikey)%RCAN = bno%wb(ikey)%RCAN + RCAN
!-            bno%wb(ikey)%SNCAN = bno%wb(ikey)%SNCAN + SNCAN
!-            bno%wb(ikey)%SNO = bno%wb(ikey)%SNO + SNO
!-            bno%wb(ikey)%WSNO = bno%wb(ikey)%WSNO + WSNO
!-            bno%wb(ikey)%PNDW = bno%wb(ikey)%PNDW + PNDW
!-            bno%wb(ikey)%LQWS = bno%wb(ikey)%LQWS + LQWS
!-            bno%wb(ikey)%FRWS = bno%wb(ikey)%FRWS + FRWS
!-            bno%wb(ikey)%LZS = bno%wb(ikey)%LZS + LZS
!-            bno%wb(ikey)%DZS = bno%wb(ikey)%DZS + DZS
!-        end do

!-    end subroutine

!-    subroutine save_water_balance(shd, dts, ikdts)

!-        use sa_mesh_common
!-        use model_dates

        !> Input variables.
!-        type(ShedGridParams) :: shd
!-        integer dts, ikdts

        !> Local variables.
!-        real dnts

        !> Denominator for time-step averaged variables.
!-        dnts = real(dts/ic%dts)

        !> Time-average storage components.
!-        bno%wb(ikdts)%RCAN = bno%wb(ikdts)%RCAN/dnts
!-        bno%wb(ikdts)%SNCAN = bno%wb(ikdts)%SNCAN/dnts
!-        bno%wb(ikdts)%SNO = bno%wb(ikdts)%SNO/dnts
!-        bno%wb(ikdts)%WSNO = bno%wb(ikdts)%WSNO/dnts
!-        bno%wb(ikdts)%PNDW = bno%wb(ikdts)%PNDW/dnts
!-        bno%wb(ikdts)%LQWS = bno%wb(ikdts)%LQWS/dnts
!-        bno%wb(ikdts)%FRWS = bno%wb(ikdts)%FRWS/dnts
!-        bno%wb(ikdts)%LZS = bno%wb(ikdts)%LZS/dnts
!-        bno%wb(ikdts)%DZS = bno%wb(ikdts)%DZS/dnts

        !> Calculate storage for the period.
!-        bno%wb(ikdts)%STG_FIN = sum(bno%wb(ikdts)%LQWS, 2) + sum(bno%wb(ikdts)%FRWS, 2) + &
!-                                bno%wb(ikdts)%RCAN + bno%wb(ikdts)%SNCAN + bno%wb(ikdts)%SNO + &
!-                                bno%wb(ikdts)%WSNO + bno%wb(ikdts)%PNDW + &
!-                                bno%wb(ikdts)%LZS + bno%wb(ikdts)%DZS

        !> Calculate storage for the run.
!-        bno%wb(IKEY_ACC)%STG_FIN = (sum(bno%wb(IKEY_ACC)%LQWS, 2) + sum(bno%wb(IKEY_ACC)%FRWS, 2) + &
!-                                    bno%wb(IKEY_ACC)%RCAN + bno%wb(IKEY_ACC)%SNCAN + &
!-                                    bno%wb(IKEY_ACC)%SNO + bno%wb(IKEY_ACC)%WSNO + bno%wb(IKEY_ACC)%PNDW +&
!-                                    bno%wb(IKEY_ACC)%LZS + bno%wb(IKEY_ACC)%DZS) &
!-                                   /ic%ts_count

!-    end subroutine

    subroutine write_water_balance_header(fls, shd, fik, dts)

        use model_files_variables
        use sa_mesh_common

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts

        !> Local variables.
        integer j
        character(len = 3) ffmti

        !> Time-step information.
        write(fik, 1010, advance = 'no') VN_YEAR, VN_JDAY
        if (dts < 86400) write(fik, 1010, advance = 'no') VN_HOUR
        if (dts < 3600) write(fik, 1010, advance = 'no') VN_MINS

        !> Variables.
        write(fik, 1010, advance = 'no') &
            VN_PREC // VN_ACC, VN_ET // VN_ACC, VN_ROF // VN_ACC, VN_OVRFLW // VN_ACC, &
            VN_LATFLW // VN_ACC, VN_DRAINSOL // VN_ACC, VN_DSTGW // VN_ACC, &
            VN_PREC, VN_ET, VN_ROF, VN_OVRFLW, VN_LATFLW, VN_DRAINSOL, VN_FZWSCAN, VN_LQWSCAN, VN_SNO, VN_LQWSSNO, VN_LQWSPND
        do j = 1, shd%lc%IGND
            write(ffmti, '(i3)') j
            write(fik, 1010, advance = 'no') &
                VN_LQWSSOL // trim(adjustl(ffmti)), VN_FZWSSOL // trim(adjustl(ffmti)), &
                VN_ALWSSOL // trim(adjustl(ffmti))
        end do
        write(fik, 1010) VN_LQWSSOL, VN_FZWSSOL, VN_ALWSSOL, VN_STGGW, 'DZS', VN_STGW, VN_DSTGW

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine write_water_balance(fls, shd, fik, dts, ina, series)

        use model_files_variables
        use sa_mesh_common
        use model_dates

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts, ina
        type(output_series) series

        !> Local variables.
        integer j
        real :: wsno = 0.0, pndw = 0.0

        !> Make sure the cell is inside the basin.
        ina = min(ina, shd%NAA)

        !> Check for 'NO_DATA' values and transform temperatures to degrees C.
        if (series%basin%lqwssno(ina) /= out%NO_DATA) wsno = series%basin%lqwssno(ina)
        if (series%basin%lqwspnd(ina) /= out%NO_DATA) pndw = series%basin%lqwspnd(ina)

        !> Write the time-stamp for the period.
        write(fik, 1010, advance = 'no') ic%now%year
        write(fik, 1010, advance = 'no') ic%now%jday
        if (dts < 86400) write(fik, 1010, advance = 'no') ic%now%hour
        if (dts < 3600) write(fik, 1010, advance = 'no') ic%now%mins

        !> Write the water balance to file.
        write(fik, 1010) &
            out%tot%basin%prec(ina), out%tot%basin%et(ina)*ic%dts, out%tot%basin%rof(ina)*ic%dts, &
            out%tot%basin%ovrflw(ina)*ic%dts, sum(out%tot%basin%latflw(ina, :))*ic%dts, out%tot%basin%drainsol(ina)*ic%dts, &
            out%tot%basin%dstgw(ina), &
            series%basin%prec(ina), series%basin%et(ina)*ic%dts, series%basin%rof(ina)*ic%dts, &
            series%basin%ovrflw(ina)*ic%dts, sum(series%basin%latflw(ina, :))*ic%dts, series%basin%drainsol(ina)*ic%dts, &
            series%basin%fzwscan(ina), series%basin%lqwscan(ina), &
            series%basin%sno(ina), wsno, &
            pndw, &
            (series%basin%lqwssol(ina, j), series%basin%fzwssol(ina, j), &
             series%basin%alwssol(ina, j), j = 1, shd%lc%IGND), &
            sum(series%basin%lqwssol(ina, :)), &
            sum(series%basin%fzwssol(ina, :)), &
            sum(series%basin%alwssol(ina, :)), &
            series%basin%stggw(ina), 0.0, &
            series%basin%stgw(ina), &
            series%basin%dstgw(ina)

1010    format(9999(g15.7e2, ','))

    end subroutine

!-    subroutine reset_water_balance(ikdts)

        !> Input variables.
!-        integer ikdts

        !> Update the final storage.
!-        bno%wb(ikdts)%STG_INI = bno%wb(ikdts)%STG_FIN

        !> Reset the accumulation for time-averaged output.
!-        bno%wb(ikdts)%PRE = 0.0
!-        bno%wb(ikdts)%EVAP = 0.0
!-        bno%wb(ikdts)%ROF = 0.0
!-        bno%wb(ikdts)%ROFO = 0.0
!-        bno%wb(ikdts)%ROFS = 0.0
!-        bno%wb(ikdts)%ROFB = 0.0
!-        bno%wb(ikdts)%RCAN = 0.0
!-        bno%wb(ikdts)%SNCAN = 0.0
!-        bno%wb(ikdts)%SNO = 0.0
!-        bno%wb(ikdts)%WSNO = 0.0
!-        bno%wb(ikdts)%PNDW = 0.0
!-        bno%wb(ikdts)%LQWS = 0.0
!-        bno%wb(ikdts)%FRWS = 0.0

!-    end subroutine

    subroutine allocate_evp_out(shd, series)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_series) series

        !> Allocate output variables.
        call output_variables_activate(series%basin, (/ VN_DUMMY_LENGTH, VN_ET, VN_POTEVP, VN_EVPB, VN_ARRD /))

    end subroutine

    subroutine update_evp(fls, shd, fik, dts, ina, series)

        use model_files_variables
        use sa_mesh_common
        use model_dates

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts, ina
        type(output_series) series

        !> Local variables.
        real dnts

        !> Denominator for time-step averaged variables.
        dnts = real(dts/ic%dts)

        !> Average of the storage components.
!-        bno%evpdts(ikdts)%EVPB = bno%evpdts(ikdts)%EVPB/dnts
!-        bno%evpdts(ikdts)%ARRD = bno%evpdts(ikdts)%ARRD/dnts

        !> Write the time-stamp for the period.
        write(fik, 1010, advance = 'no') ic%now%year
        write(fik, 1010, advance = 'no') ic%now%jday
        if (dts < 86400) write(fik, 1010, advance = 'no') ic%now%hour
        if (dts < 3600) write(fik, 1010, advance = 'no') ic%now%mins

        !> Write the water balance to file.
        write(fik, 1010) &
            series%basin%ET(ina)*ic%dts, series%basin%POTEVP(ina)*ic%dts, series%basin%EVPB(ina), series%basin%ARRD(ina)

        !> Reset the accumulation for time-averaged output.
!-        bno%evpdts(ikdts)%EVAP = 0.0
!-        bno%evpdts(ikdts)%PEVP = 0.0
!-        bno%evpdts(ikdts)%EVPB = 0.0
!-        bno%evpdts(ikdts)%ARRD = 0.0

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine update_evp_header(fls, shd, fik, dts)

        use model_files_variables
        use sa_mesh_common

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts

        !> Time-step information.
        write(fik, 1010, advance = 'no') VN_YEAR, VN_JDAY
        if (dts < 86400) write(fik, 1010, advance = 'no') VN_HOUR
        if (dts < 3600) write(fik, 1010, advance = 'no') VN_MINS

        !> Variables.
        write(fik, 1010) VN_ET, VN_POTEVP, VN_EVPB, VN_ARRD

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine allocate_energy_balance_out(shd, series)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_series) series

        !> Allocate output variables.
        call output_variables_activate( &
            series%basin, (/ &
                VN_DUMMY_LENGTH, VN_FSIN, VN_FSOUT, VN_ALBT, VN_FLIN, VN_FLOUT, VN_GTE, VN_QEVP, VN_QSENS, VN_GZERO, &
                VN_TA, VN_TCAN, VN_CMAS, VN_TSNO, VN_TPND, VN_TSOL, VN_QA, VN_UV, VN_PRES /))

    end subroutine

!-    subroutine update_energy_balance(shd, cm)

        !> For 'shd' variable type and output variables.
!-        use sa_mesh_common

        !> For 'cm' variable type.
!-        use climate_forcing

        !> For 'ic' variable.
!-        use model_dates

        !> Input variables.
!-        type(ShedGridParams) :: shd
!-        type(clim_info) :: cm

        !> Local variables.
!-        integer, dimension(:), allocatable :: &
!-            IFS, ICAN, ISNOW, IPOND
!-        real, dimension(:), allocatable :: &
!-            FSIN, ALBT, FSOUT, FLIN, GTE, FLOUT, QH, QE, GZERO, &
!-            TA, TCAN, CMAS, TSNOW, TPOND
!-        real, allocatable :: TBAR(:, :)
!-        integer ikey, n, s, ii, i, j
!-        real :: TFREZ = 273.16

        !> Prepare local variables for accumulation.
!-        n = shd%NA
!-        s = shd%lc%IGND
!-        allocate( &
!-            IFS(n), ICAN(n), ISNOW(n), IPOND(n), &
!-            FSIN(n), ALBT(n), FSOUT(n), FLIN(n), GTE(n), FLOUT(n), QH(n), QE(n), GZERO(n), &
!-            TA(n), TCAN(n), CMAS(n), TSNOW(n), TPOND(n), TBAR(n, s))
!-        IFS = 0; ICAN = 0; ISNOW = 0; IPOND = 0
!-        FSIN = 0.0; ALBT = 0.0; FSOUT = 0.0; FLIN = 0.0; GTE = 0.0; FLOUT = 0.0; QH = 0.0; QE = 0.0; GZERO = 0.0
!-        TA = 0.0; TCAN = 0.0; CMAS = 0.0; TSNOW = 0.0; TPOND = 0.0; TBAR = 0.0

        !> Time-averaged variables and averaging counters.
!-        where (out%ts%grid%fsin > 0.0)
!-            ALBT = out%ts%grid%albt*shd%FRAC
!-            IFS = 1
!-        end where
!-        where (out%ts%grid%gte > 0.0) GTE = (out%ts%grid%gte - TFREZ)*shd%FRAC
!-        where (out%ts%grid%ta > 0.0) TA = (out%ts%grid%ta - TFREZ)*shd%FRAC
!-        where (out%ts%grid%tcan > 0.0)
!-            CMAS = out%ts%grid%cmas*shd%FRAC
!-            TCAN = (out%ts%grid%tcan - TFREZ)*shd%FRAC
!-            ICAN = 1
!-        end where
!-        where (out%ts%grid%sno > 0.0)
!-            TSNOW = (out%ts%grid%tsno - TFREZ)*shd%FRAC
!-            ISNOW = 1
!-        end where
!-        where (out%ts%grid%zpnd > 0.0)
!-            TPOND = (out%ts%grid%tpnd - TFREZ)*shd%FRAC
!-            IPOND = 1
!-        end where
!-        do j = 1, shd%lc%IGND
!-            where (out%ts%grid%tbar(:, j) > 0.0) TBAR(:, j) = (out%ts%grid%tsol(:, j) - TFREZ)*shd%FRAC
!-        end do

        !> Accumulated fluxes.
        !> Converted from (W m-2 = J m-2 s-1) to J m-2 for accumulation.
!-        FSIN = out%ts%grid%fsin*ic%dts*shd%FRAC
!-        where (ALBT > 0.0)
!-            FSOUT = out%ts%grid%fsout*ic%dts*shd%FRAC
!-        end where
!-        FLIN = out%ts%grid%flin*ic%dts*shd%FRAC
!-        where (out%ts%grid%gte > 0.0) FLOUT = out%ts%grid%flout*ic%dts*shd%FRAC
!-        QH = out%ts%grid%qsens*ic%dts*shd%FRAC
!-        QE = out%ts%grid%qevp*ic%dts*shd%FRAC
!-        GZERO = out%ts%grid%gzero*ic%dts*shd%FRAC

        !> Propagate through basin cells (by flow direction).
        !> Variables are weighted by FRAC during accumulation.
!-        do i = 1, shd%NAA
!-            ii = shd%NEXT(i)
!-            if (ii > 0) then

                !> Time-averaged variables.
!-                ALBT(ii) = ALBT(ii) + ALBT(i)
!-                GTE(ii) = GTE(ii) + GTE(i)
!-                TA(ii) = TA(ii) + TA(i)
!-                TCAN(ii) = TCAN(ii) + TCAN(i)
!-                CMAS(ii) = CMAS(ii) + CMAS(i)
!-                TSNOW(ii) = TSNOW(ii) + TSNOW(i)
!-                TPOND(ii) = TPOND(ii) + TPOND(i)
!-                TBAR(ii, :) = TBAR(ii, :) + TBAR(i, :)

                !> Counter for time-averaging is either on (1) if any cell has contributed or off (0).
!-                IFS(ii) = max(IFS(ii), IFS(i))
!-                ICAN(ii) = max(ICAN(ii), ICAN(i))
!-                ISNOW(ii) = max(ISNOW(ii), ISNOW(i))
!-                IPOND(ii) = max(IPOND(ii), IPOND(i))

                !> Accumulated fluxes.
!-                FSIN(ii) = FSIN(ii) + FSIN(i)
!-                FSOUT(ii) = FSOUT(ii) + FSOUT(i)
!-                FLIN(ii) = FLIN(ii) + FLIN(i)
!-                FLOUT(ii) = FLOUT(ii) + FLOUT(i)
!-                QH(ii) = QH(ii) + QH(i)
!-                QE(ii) = QE(ii) + QE(i)
!-                GZERO(ii) = GZERO(ii) + GZERO(i)
!-            end if
!-        end do

        !> Update totals.
!-        do ikey = 1, NKEY

            !> Time-averaged variables and averaging counters.
!-            bno%eb(ikey)%ALBT = bno%eb(ikey)%ALBT + ALBT
!-            bno%eb(ikey)%IFS = bno%eb(ikey)%IFS + IFS
!-            bno%eb(ikey)%GTE = bno%eb(ikey)%GTE + GTE
!-            bno%eb(ikey)%TA = bno%eb(ikey)%TA + TA
!-            bno%eb(ikey)%TCAN = bno%eb(ikey)%TCAN + TCAN
!-            bno%eb(ikey)%ICAN = bno%eb(ikey)%ICAN + ICAN
!-            bno%eb(ikey)%CMAS = bno%eb(ikey)%CMAS + CMAS
!-            bno%eb(ikey)%TSNOW = bno%eb(ikey)%TSNOW + TSNOW
!-            bno%eb(ikey)%ISNOW = bno%eb(ikey)%ISNOW + ISNOW
!-            bno%eb(ikey)%TPOND = bno%eb(ikey)%TPOND + TPOND
!-            bno%eb(ikey)%IPOND = bno%eb(ikey)%IPOND + IPOND
!-            bno%eb(ikey)%TBAR = bno%eb(ikey)%TBAR + TBAR

            !> Accumulated fluxes.
!-            bno%eb(ikey)%FSIN = bno%eb(ikey)%FSIN + FSIN
!-            bno%eb(ikey)%FSOUT = bno%eb(ikey)%FSOUT + FSOUT
!-            bno%eb(ikey)%FLIN = bno%eb(ikey)%FLIN + FLIN
!-            bno%eb(ikey)%FLOUT = bno%eb(ikey)%FLOUT + FLOUT
!-            bno%eb(ikey)%QH = bno%eb(ikey)%QH + QH
!-            bno%eb(ikey)%QE = bno%eb(ikey)%QE + QE
!-            bno%eb(ikey)%GZERO = bno%eb(ikey)%GZERO + GZERO
!-        end do

!-    end subroutine

!-    subroutine save_energy_balance(shd, dts, ikdts)

!-        use sa_mesh_common
!-        use model_dates

        !> Input variables.
!-        type(ShedGridParams) :: shd
!-        integer dts, ikdts

        !> Local variables.
!-        real dnts

        !> Number of elapsed time-steps for time-averaged variables.
        !> (dts: total seconds elapsed, s-1)/(ic%dts: seconds in time-step, s-1) = time-steps elapsed.
!-        dnts = real(dts/ic%dts)

        !> Time-averaged variables.
!-        where (bno%eb(ikdts)%IFS > 0)
!-            bno%eb(ikdts)%ALBT = bno%eb(ikdts)%ALBT/bno%eb(ikdts)%IFS
!-        end where
!-        bno%eb(ikdts)%GTE = bno%eb(ikdts)%GTE/dnts
!-        bno%eb(ikdts)%TA = bno%eb(ikdts)%TA/dnts
!-        where (bno%eb(ikdts)%ICAN > 0)
!-            bno%eb(ikdts)%CMAS = bno%eb(ikdts)%CMAS/bno%eb(ikdts)%ICAN
!-            bno%eb(ikdts)%TCAN = bno%eb(ikdts)%TCAN/bno%eb(ikdts)%ICAN
!-        end where
!-        where (bno%eb(ikdts)%ISNOW > 0) bno%eb(ikdts)%TSNOW = bno%eb(ikdts)%TSNOW/bno%eb(ikdts)%ISNOW
!-        where (bno%eb(ikdts)%IPOND > 0) bno%eb(ikdts)%TPOND = bno%eb(ikdts)%TPOND/bno%eb(ikdts)%IPOND
!-        bno%eb(ikdts)%TBAR = bno%eb(ikdts)%TBAR/dnts

        !> Number of seconds elapsed for accumulated fluxes.
        !> dts: total seconds elapsed, s-1.
!-        dnts = real(dts)

        !> Conversion of energy fluxes back to rates.
        !> Output (W m-2) = (Accumulation, J m-2)/(seconds elapsed, s-1).
!-        bno%eb(ikdts)%FSIN = bno%eb(ikdts)%FSIN/dnts
!-        bno%eb(ikdts)%FSOUT = bno%eb(ikdts)%FSOUT/dnts
!-        bno%eb(ikdts)%FLIN = bno%eb(ikdts)%FLIN/dnts
!-        bno%eb(ikdts)%FLOUT = bno%eb(ikdts)%FLOUT/dnts
!-        bno%eb(ikdts)%QH = bno%eb(ikdts)%QH/dnts
!-        bno%eb(ikdts)%QE = bno%eb(ikdts)%QE/dnts
!-        bno%eb(ikdts)%GZERO = bno%eb(ikdts)%GZERO/dnts

!-    end subroutine

    subroutine write_energy_balance_header(fls, shd, fik, dts)

        use model_files_variables
        use sa_mesh_common

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts

        !> Local variables.
        integer j
        character(len = 3) ffmti

        !> Time-step information.
        write(fik, 1010, advance = 'no') VN_YEAR, VN_JDAY
        if (dts < 86400) write(fik, 1010, advance = 'no') VN_HOUR
        if (dts < 3600) write(fik, 1010, advance = 'no') VN_MINS

        !> Variable names.
        write(fik, 1010, advance = 'no') &
            VN_FSIN, VN_FSOUT, VN_ALBT, VN_FLIN, VN_FLOUT, VN_GTE, VN_QSENS, VN_QEVP, VN_GZERO, &
            VN_TA, VN_TCAN, VN_CMAS, VN_TSNO, VN_TPND
        do j = 1, shd%lc%IGND
            write(ffmti, '(i3)') j
            write(fik, 1010, advance = 'no') VN_TSOL // trim(adjustl(ffmti))
        end do
        write(fik, 1010, advance = 'no') VN_QA, VN_UV, VN_PRES
        write(fik, 1010)

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine write_energy_balance(fls, shd, fik, dts, ina, series)

        use model_files_variables
        use sa_mesh_common
        use model_dates

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts, ina, ikdts
        type(output_series) series

        !> Local variables.
        integer j
!-        real frac
        real :: albt = 0.0, gte = 0.0, ta = 0.0, tcan = 0.0, cmas = 0.0, tsno = 0.0, tpnd = 0.0
        real tbar(shd%lc%IGND)
        real :: TFREZ = 273.16

        !> Make sure the cell is inside the basin.
        ina = min(ina, shd%NAA)

        !> Use 'frac' to normalize the accumulated weighting by contributing FRAC.
        !> Using DA to calculate 'frac' already accounts for grid FRAC accumulated by flow direction.
!-        frac = shd%DA(ina)/((shd%AL/1000.0)**2)

        !> Check for 'NO_DATA' values and transform temperatures to degrees C.
        albt = 0.0; gte = 0.0; ta = 0.0; tcan = 0.0; cmas = 0.0; tsno = 0.0; tpnd = 0.0
        if (series%basin%albt(ina) /= out%NO_DATA) albt = series%basin%albt(ina)
        if (series%basin%gte(ina) > (TFREZ - 100.0)) gte = series%basin%gte(ina) - TFREZ
        if (series%basin%ta(ina) > (TFREZ - 100.0)) ta = series%basin%ta(ina) - TFREZ
        if (series%basin%tcan(ina) > (TFREZ - 100.0)) tcan = series%basin%tcan(ina) - TFREZ
        if (series%basin%cmas(ina) /= out%NO_DATA) cmas = series%basin%cmas(ina)
        if (series%basin%tsno(ina) > (TFREZ - 100.0)) tsno = series%basin%tsno(ina) - TFREZ
        if (series%basin%tpnd(ina) > (TFREZ - 100.0)) tpnd = series%basin%tpnd(ina) - TFREZ
        tbar = 0.0
        where (series%basin%tsol(ina, :) > (TFREZ - 100.0)) tbar = series%basin%tsol(ina, :) - TFREZ

        !> Write the time-stamp for the period.
        write(fik, 1010, advance = 'no') ic%now%year
        write(fik, 1010, advance = 'no') ic%now%jday
        if (dts < 86400) write(fik, 1010, advance = 'no') ic%now%hour
        if (dts < 3600) write(fik, 1010, advance = 'no') ic%now%mins

        !> Calculate a balance for the period.
!-        bno%eb(ikdts)%BAL1 = ( &
!-            (bno%eb(ikdts)%FSIN(ina) - bno%eb(ikdts)%FSOUT(ina)) + &
!-            (bno%eb(ikdts)%FLIN(ina) - bno%eb(ikdts)%FLOUT(ina)) - &
!-            bno%eb(ikdts)%QH(ina) - bno%eb(ikdts)%QE(ina) - bno%eb(ikdts)%GZERO(ina))/frac

        !> Write the variables to file.
        write(fik, 1010) &
            series%basin%fsin(ina), series%basin%fsout(ina), &
            albt, &
            series%basin%flin(ina), series%basin%flout(ina), gte, &
            series%basin%qsens(ina), series%basin%qevp(ina), &
            series%basin%gzero(ina), &
            ta, tcan, cmas, &
            tsno, tpnd, &
            (tbar(j), j = 1, shd%lc%IGND), series%basin%qa(ina), series%basin%uv(ina), series%basin%pres(ina)

1010    format(9999(g15.7e2, ','))

    end subroutine

!-    subroutine reset_energy_balance(ikdts)

        !> Input variables.
!-        integer ikdts

        !> Preserve the previous balance value.
!-        bno%eb(ikdts)%BAL0 = bno%eb(ikdts)%BAL1

        !> Reset accumulated variables.
!-        bno%eb(ikdts)%IFS = 0; bno%eb(ikdts)%ICAN = 0; bno%eb(ikdts)%ISNOW = 0; bno%eb(ikdts)%IPOND = 0
!-        bno%eb(ikdts)%FSIN = 0.0; bno%eb(ikdts)%ALBT = 0.0; bno%eb(ikdts)%FSOUT = 0.0
!-        bno%eb(ikdts)%FLIN = 0.0; bno%eb(ikdts)%GTE = 0.0; bno%eb(ikdts)%FLOUT = 0.0
!-        bno%eb(ikdts)%QH = 0.0; bno%eb(ikdts)%QE = 0.0
!-        bno%eb(ikdts)%GZERO = 0.0
!-        bno%eb(ikdts)%TA = 0.0; bno%eb(ikdts)%TCAN = 0.0; bno%eb(ikdts)%CMAS = 0.0
!-        bno%eb(ikdts)%TSNOW = 0.0; bno%eb(ikdts)%TPOND = 0.0
!-        bno%eb(ikdts)%TBAR = 0.0

!-    end subroutine

end module
