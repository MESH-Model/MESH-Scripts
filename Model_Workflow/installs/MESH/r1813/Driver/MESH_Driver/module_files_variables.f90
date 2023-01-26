module model_files_variables

    use model_files_variabletypes

    implicit none

    type model_file_keys

        !> MESH_input_run_options.ini
        integer :: f53 = 1

        !> MESH_drainage_database.r2c
        integer :: f20 = 11

        !> MESH_parameters_CLASS.ini
        integer :: f50 = 2

        !> MESH_parameters_hydrology.ini
        integer :: f23 = 3

        !> MESH_input_soil_levels.txt
        integer :: f52 = 10

        !> MESH_input_soil_levels.txt
        integer :: f54 = 14

        !> MESH_ggeo.ini
        integer :: f18 = 7

        !> Basin_average_water_balance.csv
        integer :: f900 = 4

        !> out_response
        integer :: out_response = 8

        !> int_statVariables.seq
        integer :: f883 = 9

    end type

    type(model_file_keys), save :: mfk

    !> Description:
    !>  Keys for output file formats.
    !>      To assign: += radix(key)**key
    !>      To test: btest(val, key)
!-    integer, parameter :: IO_TYPE_R2C = 2
!-    integer, parameter :: IO_TYPE_SEQ = 3
!-    integer, parameter :: IO_TYPE_TXT = 4
!-    integer, parameter :: IO_TYPE_CSV = 6
!-    integer, parameter :: IO_TYPE_TSI = 7
!-    integer, parameter :: IO_TYPE_TSK = 8
!-    integer, parameter :: IO_TYPE_NC4 = 9
!-    integer, parameter :: IO_TYPE_R2C_BIN = 10

    !> Description:
    !>  Keys for output file scale.
!-    integer, parameter :: DATA_TYPE_GRID = 1
!-    integer, parameter :: DATA_TYPE_TILE = 2

    !> Description:
    !>  Keys for output file frequency.
    !>      To assign: += radix(key)**key
    !>      To test: btest(val, key)
    !* STA: Static (once; for fields that do not change in time).
    !* TOT: Accrued over the simulation.
    !* DLY: Daily.
    !* MLY: Monthly.
    !* HLY: Hourly.
    !* PTS: Per time-step.
    !* YLY: Yearly.
    !* SSL: Seasonal.
!-    integer, parameter :: IO_FREQ_STA = 0
!-    integer, parameter :: IO_FREQ_TOT = 1
!-    integer, parameter :: IO_FREQ_DLY = 2
!-    integer, parameter :: IO_FREQ_MLY = 3
!-    integer, parameter :: IO_FREQ_HLY = 4
!-    integer, parameter :: IO_FREQ_PTS = 5
!-    integer, parameter :: IO_FREQ_YLY = 6
!-    integer, parameter :: IO_FREQ_SSL = 7

end module
