!> Description:
!>  Stop the program with non-zero exit.
subroutine program_abort()

    !> 'print_routines: For print routines.
    use print_routines

    implicit none

    !> Print message.
    call print_message('Abnormal exit.')

    !> Stop with non-zero exit.
    stop -1

end subroutine
