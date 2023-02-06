module model_files_variabletypes

    implicit none

    integer, parameter :: iun_scratch = 99

    type file
        character*250 :: fn = ''
!        logical :: isInit = .false.
        logical :: init = .true.
        integer :: iun = iun_scratch
    end type

    type fl_ids
        character*500 pthIn     !Input absolute path for the param files
        character*500 pthOut    !Output absolute path for the output files
        character(10) GENDIR_OUT
        type(file), dimension(:), allocatable :: fl
    end type

end module
