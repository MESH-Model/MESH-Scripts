module SIMSTATS_config

    use model_files_variables

    implicit none

    type metricsout_files_keys

        !> function_out.txt
        integer :: fo = 1

        !> MonteCarlo.txt
        integer :: MC = 2

        !> NS.txt
        integer :: NSE = 3

        !> NSW.txt
        integer :: NSW = 4

        !> drms.txt
        integer :: RMSE = 5

        !> abserr.txt
        integer :: ABSE = 6

        !> Metrics_Out.txt
        integer :: out = 7

        !> pre_emption_value.txt
        integer :: PE = 8

    end type

    type metricsout_flags

        !> AUTOCALIBRATIONFLAG enables calculation and output of metrics of the simulation.
        integer :: AUTOCALIBRATIONFLAG = 0

        !> PREEMPTIONFLAG enables pre-emption in the simulation.
        integer :: PREEMPTIONFLAG = 0

    end type

    type(fl_ids), save :: mtsfl

    type(metricsout_files_keys), save :: mtsk

    type(metricsout_flags), save :: mtsflg

    contains

    subroutine init_metricsout_files()

        allocate(mtsfl%fl(8))

    end subroutine

end module
