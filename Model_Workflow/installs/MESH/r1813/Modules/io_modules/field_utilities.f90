module field_utilities

    !> 'mesh_io': For I/O field types, options and constants.
    use mesh_io_variables

    !> 'strings': For 'lowercase' function.
    use strings, only: lowercase

    implicit none

    interface map_dimensions
        module procedure map_dimensions_real2d
        module procedure map_dimensions_real3d
        module procedure map_dimensions_real4d
        module procedure map_dimensions_real5d
        module procedure map_dimensions_int2d
        module procedure map_dimensions_int3d
        module procedure map_dimensions_int4d
        module procedure map_dimensions_int5d
    end interface

    interface compact_field
        module procedure compact_field_real1d_from_real2d
        module procedure compact_field_real1d_from_real3d
        module procedure compact_field_real1d_from_real4d
        module procedure compact_field_real1d_from_real5d
        module procedure compact_field_real2d_from_real3d
        module procedure compact_field_real2d_from_real4d
        module procedure compact_field_real2d_from_real5d
        module procedure compact_field_real3d_from_real4d
        module procedure compact_field_real3d_from_real5d
        module procedure compact_field_real4d_from_real5d
    end interface

    interface assign_field
        module procedure assign_field_real
        module procedure assign_field_int
        module procedure assign_field_char
        module procedure assign_field_real1d
        module procedure assign_field_real2d
        module procedure assign_field_real3d
        module procedure assign_field_real4d
        module procedure assign_field_real5d
        module procedure assign_field_int1d
        module procedure assign_field_int2d
        module procedure assign_field_int3d
        module procedure assign_field_int4d
        module procedure assign_field_int5d
        module procedure assign_field_char1d
    end interface

    contains

    subroutine map_dimensions_real2d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :), intent(in) :: input_field
        integer, intent(in) :: desired_order(2)
        real, dimension(:, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Status.
        error_status = 0

        !> Allocate output variables.
        if (.not. allocated(output_field)) then
            allocate(output_field(size(input_field, desired_order(1)), size(input_field, desired_order(2))))
        end if

        !> Reset output field.
        output_field = huge(output_field)

        !> Map the field.
        if (desired_order(1) == 1 .and. desired_order(2) == 2) then
            output_field = input_field
        else
            output_field = transpose(input_field)
        end if

    end subroutine

    subroutine map_dimensions_real3d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :), intent(in) :: input_field
        integer, intent(in) :: desired_order(3)
        real, dimension(:, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables
        integer order(3), d3, d2, d1

        !> Status.
        error_status = 0

        !> Allocate output variables.
        if (.not. allocated(output_field)) then
            allocate(output_field(size(input_field, order(1)), size(input_field, order(2)), size(input_field, order(3))))
        end if

        !> Reset output field.
        output_field = huge(output_field)

        !> Map the field.
        order = desired_order
        if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        output_field(d1, d2, d3) = input_field(d1, d2, d3)
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        output_field(d1, d2, d3) = input_field(d2, d1, d3)
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        output_field(d1, d2, d3) = input_field(d1, d3, d2)
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        output_field(d1, d2, d3) = input_field(d2, d3, d1)
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        output_field(d1, d2, d3) = input_field(d3, d1, d2)
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        output_field(d1, d2, d3) = input_field(d3, d2, d1)
                    end do
                end do
            end do
        end if

    end subroutine

    subroutine map_dimensions_real4d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :, :), intent(in) :: input_field
        integer, intent(in) :: desired_order(4)
        real, dimension(:, :, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables
        integer order(4), d4, d3, d2, d1

        !> Status.
        error_status = 0

        !> Allocate output variables.
        if (.not. allocated(output_field)) then
            allocate(output_field( &
                size(input_field, order(1)), size(input_field, order(2)), size(input_field, order(3)), size(input_field, order(4))))
        end if

        !> Reset output field.
        output_field = huge(output_field)

        !> Map the field.
        order = desired_order
        if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            output_field(d1, d2, d3, d4) = input_field(d1, d2, d3, d4)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            output_field(d1, d2, d3, d4) = input_field(d2, d1, d3, d4)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            output_field(d1, d2, d3, d4) = input_field(d1, d3, d2, d4)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            output_field(d1, d2, d3, d4) = input_field(d2, d3, d1, d4)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            output_field(d1, d2, d3, d4) = input_field(d3, d1, d2, d4)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            output_field(d1, d2, d3, d4) = input_field(d3, d2, d1, d4)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            output_field(d1, d2, d3, d4) = input_field(d1, d2, d4, d3)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            output_field(d1, d2, d3, d4) = input_field(d2, d1, d4, d3)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            output_field(d1, d2, d3, d4) = input_field(d1, d3, d4, d2)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            output_field(d1, d2, d3, d4) = input_field(d2, d3, d4, d1)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            output_field(d1, d2, d3, d4) = input_field(d3, d1, d4, d2)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            output_field(d1, d2, d3, d4) = input_field(d3, d2, d4, d1)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            output_field(d1, d2, d3, d4) = input_field(d1, d4, d3, d2)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            output_field(d1, d2, d3, d4) = input_field(d2, d4, d3, d1)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            output_field(d1, d2, d3, d4) = input_field(d1, d4, d2, d3)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            output_field(d1, d2, d3, d4) = input_field(d2, d4, d1, d3)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            output_field(d1, d2, d3, d4) = input_field(d3, d4, d2, d1)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            output_field(d1, d2, d3, d4) = input_field(d3, d4, d1, d2)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            output_field(d1, d2, d3, d4) = input_field(d4, d2, d3, d1)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            output_field(d1, d2, d3, d4) = input_field(d4, d1, d3, d2)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            output_field(d1, d2, d3, d4) = input_field(d4, d3, d2, d1)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            output_field(d1, d2, d3, d4) = input_field(d4, d3, d1, d2)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            output_field(d1, d2, d3, d4) = input_field(d4, d1, d2, d3)
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            output_field(d1, d2, d3, d4) = input_field(d4, d2, d1, d3)
                        end do
                    end do
                end do
            end do
        end if

    end subroutine

    subroutine map_dimensions_real5d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :, :, :), intent(in) :: input_field
        integer, intent(in) :: desired_order(5)
        real, dimension(:, :, :, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables
        integer order(5), d5, d4, d3, d2, d1

        !> Status.
        error_status = 0

        !> Allocate output variables.
        if (.not. allocated(output_field)) then
            allocate(output_field( &
                size(input_field, order(1)), size(input_field, order(2)), size(input_field, order(3)), &
                size(input_field, order(4)), size(input_field, order(5))))
        end if

        !> Reset output field.
        output_field = huge(output_field)

        !> Map the field.
        order = desired_order
        if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d2, d3, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                         do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d1, d3, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d3, d2, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d3, d1, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d1, d2, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d2, d1, d4, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d2, d4, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d1, d4, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d3, d4, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d3, d4, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d1, d4, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d2, d4, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d4, d3, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d4, d3, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d4, d2, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d4, d1, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d4, d2, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d4, d1, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d2, d3, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d1, d3, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d3, d2, d1, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d3, d1, d2, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d1, d2, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d2, d1, d3, d5)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d2, d3, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d1, d3, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d3, d2, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d3, d1, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d1, d2, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d2, d1, d5, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d2, d4, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d1, d4, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d3, d4, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d3, d4, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d1, d4, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d2, d4, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d4, d3, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d4, d3, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d4, d2, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d4, d1, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d4, d2, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d4, d1, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d2, d3, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d1, d3, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d3, d2, d5, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d3, d1, d5, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d1, d2, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d2, d1, d5, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d2, d5, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d1, d5, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d3, d5, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d3, d5, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d1, d5, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d2, d5, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d2, d5, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d1, d5, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d3, d5, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d3, d5, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d1, d5, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d2, d5, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d4, d5, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d4, d5, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d4, d5, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d4, d5, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d4, d5, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d4, d5, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d2, d5, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d1, d5, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d3, d5, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d3, d5, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d1, d5, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d2, d5, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d5, d3, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d5, d3, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d5, d2, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d5, d1, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d5, d2, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d5, d1, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d5, d4, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d5, d4, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d5, d4, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d5, d4, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d5, d4, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d5, d4, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d5, d3, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d5, d3, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                output_field(d1, d2, d3, d4, d5) = input_field(d1, d5, d2, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                output_field(d1, d2, d3, d4, d5) = input_field(d2, d5, d1, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d5, d2, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                output_field(d1, d2, d3, d4, d5) = input_field(d3, d5, d1, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d5, d3, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d5, d3, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d5, d2, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d5, d1, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d5, d2, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                output_field(d1, d2, d3, d4, d5) = input_field(d4, d5, d1, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d2, d3, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                         do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d1, d3, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d3, d2, d4, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d3, d1, d4, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d1, d2, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d2, d1, d4, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d2, d4, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d1, d4, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d3, d4, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d3, d4, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d1, d4, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d2, d4, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d4, d3, d2, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d4, d3, d1, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d4, d2, d3, d1)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d4, d1, d3, d2)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d4, d2, d1, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d4, d1, d2, d3)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d2, d3, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d1, d3, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d3, d2, d1, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d3, d1, d2, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d1, d2, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                output_field(d1, d2, d3, d4, d5) = input_field(d5, d2, d1, d3, d4)
                            end do
                        end do
                    end do
                end do
            end do
        end if

    end subroutine

    subroutine map_dimensions_int2d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        integer, dimension(:, :), intent(in) :: input_field
        integer, intent(in) :: desired_order(2)
        integer, dimension(:, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, dimension(size(input_field, 1), size(input_field, 2)) :: dat2_r_in
        real, dimension(:, :), allocatable :: dat2_r_out

        !> Call 'real' version of function.
        dat2_r_in = real(input_field)
        call map_dimensions(dat2_r_in, desired_order, dat2_r_out, error_status)

        !> Transfer output.
        if (error_status == 0) then
            if (.not. allocated(output_field)) then
                allocate(output_field(size(dat2_r_out, 1), size(dat2_r_out, 2)))
            end if
            output_field = int(dat2_r_out)
        end if

    end subroutine

    subroutine map_dimensions_int3d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        integer, dimension(:, :, :), intent(in) :: input_field
        integer, intent(in) :: desired_order(3)
        integer, dimension(:, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, dimension(size(input_field, 1), size(input_field, 2), size(input_field, 3)) :: input_field_real
        real, dimension(:, :, :), allocatable :: output_field_real

        !> Call 'real' version of function.
        input_field_real = real(input_field)
        call map_dimensions(input_field_real, desired_order, output_field_real, error_status)

        !> Transfer output.
        if (error_status == 0) then
            if (.not. allocated(output_field)) then
                allocate(output_field(size(output_field_real, 1), size(output_field_real, 2), size(output_field_real, 3)))
            end if
            output_field = int(output_field_real)
        end if

    end subroutine

    subroutine map_dimensions_int4d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        integer, dimension(:, :, :, :), intent(in) :: input_field
        integer, intent(in) :: desired_order(4)
        integer, dimension(:, :, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, dimension(size(input_field, 1), size(input_field, 2), size(input_field, 3), size(input_field, 4)) :: input_field_real
        real, dimension(:, :, :, :), allocatable :: output_field_real

        !> Call 'real' version of function.
        input_field_real = real(input_field)
        call map_dimensions(input_field_real, desired_order, output_field_real, error_status)

        !> Transfer output.
        if (error_status == 0) then
            if (.not. allocated(output_field)) then
                allocate(output_field( &
                    size(output_field_real, 1), size(output_field_real, 2), size(output_field_real, 3), size(output_field_real, 4)))
            end if
            output_field = int(output_field_real)
        end if

    end subroutine


    subroutine map_dimensions_int5d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        integer, dimension(:, :, :, :, :), intent(in) :: input_field
        integer, intent(in) :: desired_order(5)
        integer, dimension(:, :, :, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, dimension( &
            size(input_field, 1), size(input_field, 2), size(input_field, 3), size(input_field, 4), size(input_field, 5)) :: &
            input_field_real
        real, dimension(:, :, :, :, :), allocatable :: output_field_real

        !> Call 'real' version of function.
        input_field_real = real(input_field)
        call map_dimensions(input_field_real, desired_order, output_field_real, error_status)

        !> Transfer output.
        if (error_status == 0) then
            if (.not. allocated(output_field)) then
                allocate(output_field( &
                    size(output_field_real, 1), size(output_field_real, 2), size(output_field_real, 3), &
                    size(output_field_real, 4), size(output_field_real, 5)))
            end if
            output_field = int(output_field_real)
        end if

    end subroutine

    subroutine compact_field_real1d_from_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :), intent(in) :: input_field
        real, dimension(:), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(2), d2
        integer i

        !> Status.
        error_status = 0

        !> Get shape of input.
        dim_size = shape(input_field)

        !> Find if the array can be compacted to the desired dimensions (extra dimensions must have a size of one).
        d2 = 0
        do i = 1, 2
            if (dim_size(i) == 1) d2 = i
        end do

        !> Compact the array if the extra dimensions have a size of one.
        if (d2 > 0) then
            select case (d2)
                case (1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2)))
                    output_field = input_field(1, :)
                case (2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1)))
                    output_field = input_field(:, 1)
            end select
        else

            !> Unable to compact the array.
            error_status = 1
        end if

    end subroutine

    subroutine compact_field_real1d_from_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :), intent(in) :: input_field
        real, dimension(:), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(3), dtest, d2, d3
        integer i

        !> Status.
        error_status = 0

        !> Get shape of input.
        dim_size = shape(input_field)

        !> Find if the array can be compacted to the desired dimensions (extra dimensions must have a size of one).
        d2 = 0
        d3 = 0
        do i = 1, 3
            if (dim_size(i) == 1) then
                if (d2 > 0) then
                    d3 = i
                else
                    d2 = i
                end if
            end if
        end do

        !> Compact the array if the extra dimensions have a size of one.
        if (d2 > 0 .and. d3 > 0) then

            !> Calculate the bit value of active indices.
            dtest = radix(d2)**d2 + radix(d3)**d3

            !> Find the missing indices.
            select case (dtest)
                case (radix(dtest)**1 + radix(dtest)**2)
                    !(d2 == 1 .and. d3 == 2)
                    !(d2 == 2 .and. d3 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(3)))
                    output_field = input_field(1, 1, :)
                case (radix(dtest)**1 + radix(dtest)**3)
                    !(d2 == 1 .and. d3 == 3)
                    !(d2 == 3 .and. d3 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2)))
                    output_field = input_field(1, :, 1)
                case (radix(dtest)**2 + radix(dtest)**3)
                    !(d2 == 2 .and. d3 == 3)
                    !(d2 == 3 .and. d3 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1)))
                    output_field = input_field(:, 1, 1)
            end select
        else

            !> Unable to compact the array.
            error_status = 1
        end if

    end subroutine

    subroutine compact_field_real1d_from_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :, :), intent(in) :: input_field
        real, dimension(:), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(4), dtest, d2, d3, d4
        integer i

        !> Status.
        error_status = 0

        !> Get shape of input.
        dim_size = shape(input_field)

        !> Find if the array can be compacted to the desired dimensions (extra dimensions must have a size of one).
        d2 = 0
        d3 = 0
        d4 = 0
        do i = 1, 4
            if (dim_size(i) == 1) then
                if (d2 > 0) then
                    if (d3 > 0) then
                        d4 = i
                    else
                        d3 = i
                    end if
                else
                    d2 = i
                end if
            end if
        end do

        !> Compact the array if the extra dimensions have a size of one.
        if (d2 > 0 .and. d3 > 0 .and. d4 > 0) then

            !> Calculate the bit value of active indices.
            dtest = radix(d2)**d2 + radix(d3)**d3 + radix(d4)**d4

            !> Find the missing indices.
            select case (dtest)
                case (radix(dtest)**1 + radix(dtest)**2 + radix(dtest)**3)
                    !(d2 == 1 .and. d3 == 2 .and. d4 == 3)
                    !(d2 == 1 .and. d3 == 3 .and. d4 == 2)
                    !(d2 == 2 .and. d3 == 1 .and. d4 == 3)
                    !(d2 == 2 .and. d3 == 3 .and. d4 == 1)
                    !(d2 == 3 .and. d3 == 1 .and. d4 == 2)
                    !(d2 == 3 .and. d3 == 2 .and. d4 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(4)))
                    output_field = input_field(1, 1, 1, :)
                case (radix(dtest)**1 + radix(dtest)**2 + radix(dtest)**4)
                    !(d2 == 1 .and. d3 == 2 .and. d4 == 4)
                    !(d2 == 1 .and. d3 == 4 .and. d4 == 2)
                    !(d2 == 2 .and. d3 == 1 .and. d4 == 4)
                    !(d2 == 2 .and. d3 == 4 .and. d4 == 1)
                    !(d2 == 4 .and. d3 == 1 .and. d4 == 2)
                    !(d2 == 4 .and. d3 == 2 .and. d4 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(3)))
                    output_field = input_field(1, 1, :, 1)
                case (radix(dtest)**1 + radix(dtest)**3 + radix(dtest)**4)
                    !(d2 == 1 .and. d3 == 3 .and. d4 == 4)
                    !(d2 == 1 .and. d3 == 4 .and. d4 == 3)
                    !(d2 == 3 .and. d3 == 1 .and. d4 == 4)
                    !(d2 == 3 .and. d3 == 4 .and. d4 == 1)
                    !(d2 == 4 .and. d3 == 1 .and. d4 == 3)
                    !(d2 == 4 .and. d3 == 3 .and. d4 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2)))
                    output_field = input_field(1, :, 1, 1)
                case (radix(dtest)**2 + radix(dtest)**3 + radix(dtest)**4)
                    !(d2 == 2 .and. d3 == 3 .and. d4 == 4)
                    !(d2 == 2 .and. d3 == 4 .and. d4 == 3)
                    !(d2 == 3 .and. d3 == 2 .and. d4 == 4)
                    !(d2 == 3 .and. d3 == 4 .and. d4 == 2)
                    !(d2 == 4 .and. d3 == 2 .and. d4 == 3)
                    !(d2 == 4 .and. d3 == 3 .and. d4 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1)))
                    output_field = input_field(:, 1, 1, 1)
            end select
        else

            !> Unable to compact the array.
            error_status = 1
        end if

    end subroutine

    subroutine compact_field_real1d_from_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :, :, :), intent(in) :: input_field
        real, dimension(:), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(5), dtest, d2, d3, d4, d5
        integer i

        !> Status.
        error_status = 0

        !> Get shape of input.
        dim_size = shape(input_field)

        !> Find if the array can be compacted to the desired dimensions (extra dimensions must have a size of one).
        d2 = 0
        d3 = 0
        d4 = 0
        d5 = 0
        do i = 1, 5
            if (dim_size(i) == 1) then
                if (d2 > 0) then
                    if (d3 > 0) then
                        if (d4 > 0) then
                            d5 = i
                        else
                            d4 = i
                        end if
                    else
                        d3 = i
                    end if
                else
                    d2 = i
                end if
            end if
        end do

        !> Compact the array if the extra dimensions have a size of one.
        if (d2 > 0 .and. d3 > 0 .and. d4 > 0 .and. d5 > 0) then

            !> Calculate the bit value of active indices.
            dtest = radix(d2)**d2 + radix(d3)**d3 + radix(d4)**d4 + radix(d5)**d5

            !> Find the missing indices.
            select case (dtest)
                case (radix(dtest)**1 + radix(dtest)**2 + radix(dtest)**3 + radix(dtest)**4)
                    !(d2 == 1 .and. d3 == 2 .and. d4 == 3 .and. d5 == 4)
                    !(d2 == 1 .and. d3 == 2 .and. d4 == 4 .and. d5 == 3)
                    !(d2 == 1 .and. d3 == 3 .and. d4 == 2 .and. d5 == 4)
                    !(d2 == 1 .and. d3 == 3 .and. d4 == 4 .and. d5 == 2)
                    !(d2 == 1 .and. d3 == 4 .and. d4 == 2 .and. d5 == 3)
                    !(d2 == 1 .and. d3 == 4 .and. d4 == 3 .and. d5 == 2)
                    !(d2 == 2 .and. d3 == 1 .and. d4 == 3 .and. d5 == 4)
                    !(d2 == 2 .and. d3 == 1 .and. d4 == 4 .and. d5 == 3)
                    !(d2 == 2 .and. d3 == 3 .and. d4 == 1 .and. d5 == 4)
                    !(d2 == 2 .and. d3 == 3 .and. d4 == 4 .and. d5 == 1)
                    !(d2 == 2 .and. d3 == 4 .and. d4 == 1 .and. d5 == 3)
                    !(d2 == 2 .and. d3 == 4 .and. d4 == 3 .and. d5 == 1)
                    !(d2 == 3 .and. d3 == 1 .and. d4 == 2 .and. d5 == 4)
                    !(d2 == 3 .and. d3 == 1 .and. d4 == 4 .and. d5 == 2)
                    !(d2 == 3 .and. d3 == 2 .and. d4 == 1 .and. d5 == 4)
                    !(d2 == 3 .and. d3 == 2 .and. d4 == 4 .and. d5 == 1)
                    !(d2 == 3 .and. d3 == 4 .and. d4 == 1 .and. d5 == 2)
                    !(d2 == 3 .and. d3 == 4 .and. d4 == 2 .and. d5 == 1)
                    !(d2 == 4 .and. d3 == 1 .and. d4 == 2 .and. d5 == 3)
                    !(d2 == 4 .and. d3 == 1 .and. d4 == 3 .and. d5 == 2)
                    !(d2 == 4 .and. d3 == 2 .and. d4 == 1 .and. d5 == 3)
                    !(d2 == 4 .and. d3 == 2 .and. d4 == 3 .and. d5 == 1)
                    !(d2 == 4 .and. d3 == 3 .and. d4 == 1 .and. d5 == 2)
                    !(d2 == 4 .and. d3 == 3 .and. d4 == 2 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(5)))
                    output_field = input_field(1, 1, 1, 1, :)
                case (radix(dtest)**1 + radix(dtest)**2 + radix(dtest)**3 + radix(dtest)**5)
                    !(d2 == 1 .and. d3 == 2 .and. d4 == 3 .and. d5 == 5)
                    !(d2 == 1 .and. d3 == 2 .and. d4 == 5 .and. d5 == 3)
                    !(d2 == 1 .and. d3 == 3 .and. d4 == 2 .and. d5 == 5)
                    !(d2 == 1 .and. d3 == 3 .and. d4 == 5 .and. d5 == 2)
                    !(d2 == 1 .and. d3 == 5 .and. d4 == 2 .and. d5 == 3)
                    !(d2 == 1 .and. d3 == 5 .and. d4 == 3 .and. d5 == 2)
                    !(d2 == 2 .and. d3 == 1 .and. d4 == 3 .and. d5 == 5)
                    !(d2 == 2 .and. d3 == 1 .and. d4 == 5 .and. d5 == 3)
                    !(d2 == 2 .and. d3 == 3 .and. d4 == 1 .and. d5 == 5)
                    !(d2 == 2 .and. d3 == 3 .and. d4 == 5 .and. d5 == 1)
                    !(d2 == 2 .and. d3 == 5 .and. d4 == 1 .and. d5 == 3)
                    !(d2 == 2 .and. d3 == 5 .and. d4 == 3 .and. d5 == 1)
                    !(d2 == 3 .and. d3 == 1 .and. d4 == 2 .and. d5 == 5)
                    !(d2 == 3 .and. d3 == 1 .and. d4 == 5 .and. d5 == 2)
                    !(d2 == 3 .and. d3 == 2 .and. d4 == 1 .and. d5 == 5)
                    !(d2 == 3 .and. d3 == 2 .and. d4 == 5 .and. d5 == 1)
                    !(d2 == 3 .and. d3 == 5 .and. d4 == 1 .and. d5 == 2)
                    !(d2 == 3 .and. d3 == 5 .and. d4 == 2 .and. d5 == 1)
                    !(d2 == 5 .and. d3 == 1 .and. d4 == 2 .and. d5 == 3)
                    !(d2 == 5 .and. d3 == 1 .and. d4 == 3 .and. d5 == 2)
                    !(d2 == 5 .and. d3 == 2 .and. d4 == 1 .and. d5 == 3)
                    !(d2 == 5 .and. d3 == 2 .and. d4 == 3 .and. d5 == 1)
                    !(d2 == 5 .and. d3 == 3 .and. d4 == 1 .and. d5 == 2)
                    !(d2 == 5 .and. d3 == 3 .and. d4 == 2 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(4)))
                    output_field = input_field(1, 1, 1, :, 1)
                case (radix(dtest)**1 + radix(dtest)**2 + radix(dtest)**4 + radix(dtest)**5)
                    !(d2 == 1 .and. d3 == 2 .and. d4 == 4 .and. d5 == 5)
                    !(d2 == 1 .and. d3 == 2 .and. d4 == 5 .and. d5 == 4)
                    !(d2 == 1 .and. d3 == 4 .and. d4 == 2 .and. d5 == 5)
                    !(d2 == 1 .and. d3 == 4 .and. d4 == 5 .and. d5 == 2)
                    !(d2 == 1 .and. d3 == 5 .and. d4 == 2 .and. d5 == 4)
                    !(d2 == 1 .and. d3 == 5 .and. d4 == 4 .and. d5 == 2)
                    !(d2 == 2 .and. d3 == 1 .and. d4 == 4 .and. d5 == 5)
                    !(d2 == 2 .and. d3 == 1 .and. d4 == 5 .and. d5 == 4)
                    !(d2 == 2 .and. d3 == 4 .and. d4 == 1 .and. d5 == 5)
                    !(d2 == 2 .and. d3 == 4 .and. d4 == 5 .and. d5 == 1)
                    !(d2 == 2 .and. d3 == 5 .and. d4 == 1 .and. d5 == 4)
                    !(d2 == 2 .and. d3 == 5 .and. d4 == 4 .and. d5 == 1)
                    !(d2 == 4 .and. d3 == 1 .and. d4 == 2 .and. d5 == 5)
                    !(d2 == 4 .and. d3 == 1 .and. d4 == 5 .and. d5 == 2)
                    !(d2 == 4 .and. d3 == 2 .and. d4 == 1 .and. d5 == 5)
                    !(d2 == 4 .and. d3 == 2 .and. d4 == 5 .and. d5 == 1)
                    !(d2 == 4 .and. d3 == 5 .and. d4 == 1 .and. d5 == 2)
                    !(d2 == 4 .and. d3 == 5 .and. d4 == 2 .and. d5 == 1)
                    !(d2 == 5 .and. d3 == 1 .and. d4 == 2 .and. d5 == 4)
                    !(d2 == 5 .and. d3 == 1 .and. d4 == 4 .and. d5 == 2)
                    !(d2 == 5 .and. d3 == 2 .and. d4 == 1 .and. d5 == 4)
                    !(d2 == 5 .and. d3 == 2 .and. d4 == 4 .and. d5 == 1)
                    !(d2 == 5 .and. d3 == 4 .and. d4 == 1 .and. d5 == 2)
                    !(d2 == 5 .and. d3 == 4 .and. d4 == 2 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(3)))
                    output_field = input_field(1, 1, :, 1, 1)
                case (radix(dtest)**1 + radix(dtest)**3 + radix(dtest)**4 + radix(dtest)**5)
                    !(d2 == 1 .and. d3 == 3 .and. d4 == 4 .and. d5 == 5)
                    !(d2 == 1 .and. d3 == 3 .and. d4 == 5 .and. d5 == 4)
                    !(d2 == 1 .and. d3 == 4 .and. d4 == 3 .and. d5 == 5)
                    !(d2 == 1 .and. d3 == 4 .and. d4 == 5 .and. d5 == 3)
                    !(d2 == 1 .and. d3 == 5 .and. d4 == 3 .and. d5 == 4)
                    !(d2 == 1 .and. d3 == 5 .and. d4 == 4 .and. d5 == 3)
                    !(d2 == 3 .and. d3 == 1 .and. d4 == 4 .and. d5 == 5)
                    !(d2 == 3 .and. d3 == 1 .and. d4 == 5 .and. d5 == 4)
                    !(d2 == 3 .and. d3 == 4 .and. d4 == 1 .and. d5 == 5)
                    !(d2 == 3 .and. d3 == 4 .and. d4 == 5 .and. d5 == 1)
                    !(d2 == 3 .and. d3 == 5 .and. d4 == 1 .and. d5 == 4)
                    !(d2 == 3 .and. d3 == 5 .and. d4 == 4 .and. d5 == 1)
                    !(d2 == 4 .and. d3 == 1 .and. d4 == 3 .and. d5 == 5)
                    !(d2 == 4 .and. d3 == 1 .and. d4 == 5 .and. d5 == 3)
                    !(d2 == 4 .and. d3 == 3 .and. d4 == 1 .and. d5 == 5)
                    !(d2 == 4 .and. d3 == 3 .and. d4 == 5 .and. d5 == 1)
                    !(d2 == 4 .and. d3 == 5 .and. d4 == 1 .and. d5 == 3)
                    !(d2 == 4 .and. d3 == 5 .and. d4 == 3 .and. d5 == 1)
                    !(d2 == 5 .and. d3 == 1 .and. d4 == 3 .and. d5 == 4)
                    !(d2 == 5 .and. d3 == 1 .and. d4 == 4 .and. d5 == 3)
                    !(d2 == 5 .and. d3 == 3 .and. d4 == 1 .and. d5 == 4)
                    !(d2 == 5 .and. d3 == 3 .and. d4 == 4 .and. d5 == 1)
                    !(d2 == 5 .and. d3 == 4 .and. d4 == 1 .and. d5 == 3)
                    !(d2 == 5 .and. d3 == 4 .and. d4 == 3 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2)))
                    output_field = input_field(1, :, 1, 1, 1)
                case (radix(dtest)**2 + radix(dtest)**3 + radix(dtest)**4 + radix(dtest)**5)
                    !(d2 == 2 .and. d3 == 3 .and. d4 == 4 .and. d5 == 5)
                    !(d2 == 2 .and. d3 == 3 .and. d4 == 5 .and. d5 == 4)
                    !(d2 == 2 .and. d3 == 4 .and. d4 == 3 .and. d5 == 5)
                    !(d2 == 2 .and. d3 == 4 .and. d4 == 5 .and. d5 == 3)
                    !(d2 == 2 .and. d3 == 5 .and. d4 == 3 .and. d5 == 4)
                    !(d2 == 2 .and. d3 == 5 .and. d4 == 4 .and. d5 == 3)
                    !(d2 == 3 .and. d3 == 2 .and. d4 == 4 .and. d5 == 5)
                    !(d2 == 3 .and. d3 == 2 .and. d4 == 5 .and. d5 == 4)
                    !(d2 == 3 .and. d3 == 4 .and. d4 == 2 .and. d5 == 5)
                    !(d2 == 3 .and. d3 == 4 .and. d4 == 5 .and. d5 == 2)
                    !(d2 == 3 .and. d3 == 5 .and. d4 == 2 .and. d5 == 4)
                    !(d2 == 3 .and. d3 == 5 .and. d4 == 4 .and. d5 == 2)
                    !(d2 == 4 .and. d3 == 2 .and. d4 == 3 .and. d5 == 5)
                    !(d2 == 4 .and. d3 == 2 .and. d4 == 5 .and. d5 == 3)
                    !(d2 == 4 .and. d3 == 3 .and. d4 == 2 .and. d5 == 5)
                    !(d2 == 4 .and. d3 == 3 .and. d4 == 5 .and. d5 == 2)
                    !(d2 == 4 .and. d3 == 5 .and. d4 == 2 .and. d5 == 3)
                    !(d2 == 4 .and. d3 == 5 .and. d4 == 3 .and. d5 == 2)
                    !(d2 == 5 .and. d3 == 2 .and. d4 == 3 .and. d5 == 4)
                    !(d2 == 5 .and. d3 == 2 .and. d4 == 4 .and. d5 == 3)
                    !(d2 == 5 .and. d3 == 3 .and. d4 == 2 .and. d5 == 4)
                    !(d2 == 5 .and. d3 == 3 .and. d4 == 4 .and. d5 == 2)
                    !(d2 == 5 .and. d3 == 4 .and. d4 == 2 .and. d5 == 3)
                    !(d2 == 5 .and. d3 == 4 .and. d4 == 3 .and. d5 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1)))
                    output_field = input_field(:, 1, 1, 1, 1)
            end select
        else

            !> Unable to compact the array.
            error_status = 1
        end if

    end subroutine

    subroutine expand_field_list(field_list, increment, error_status)

        !> Input/output variables.
        type(io_field_wrapper), dimension(:), allocatable :: field_list
        integer, intent(in) :: increment
        integer, intent(out) :: error_status

        !> Local variables.
        type(io_field_wrapper), dimension(:), allocatable :: temp
        integer i

        !> Status.
        error_status = 0

        !> Transfer fields.
        if (.not. allocated(field_list)) then

            !> No need to transfer if 'field_list' is not allocated.
            allocate(field_list(increment))
        else
            !> Allocate temporary field.
            allocate(temp(size(field_list) + increment))

            !> Transfer fields.
            do i = 1, size(field_list)
                if (allocated(field_list(i)%field)) then
                    allocate(temp(i)%field, source = field_list(i)%field)

                    !> Clean-up of source array.
                    deallocate(field_list(i)%field)
                end if
            end do

            !> Deallocate source array.
            deallocate(field_list)

            !> Reallocate source array and transfer fields.
            allocate(field_list(size(temp)))
            do i = 1, size(temp)
                if (allocated(temp(i)%field)) then
                    allocate(field_list(i)%field, source = temp(i)%field)
                    deallocate(temp(i)%field)
                end if
            end do
            deallocate(temp)
        end if

    end subroutine

    subroutine combine_field_list(field_list, buffer, error_status)

        !> Input/output variables.
        type(io_field_wrapper), dimension(:), allocatable :: field_list
        type(io_field_wrapper), dimension(:), allocatable :: buffer
        integer, intent(out) :: error_status

        !> Local variables.
        type(io_field_wrapper), dimension(:), allocatable :: temp
        integer n, j, i

        !> Status.
        error_status = 0

        !> Count allocated fields in 'buffer'.
        n = 0
        do i = 1, size(buffer)
            if (allocated(buffer(i)%field)) n = n + 1
        end do

        !> Combine fields.
        if (.not. allocated(field_list)) then

            !> No need to combine if 'field_list' is not allocated, just transfer.
            allocate(field_list(n))
            do i = 1, size(buffer)
                if (allocated(buffer(i)%field)) then
                    allocate(field_list(i)%field, source = buffer(i)%field)
                    deallocate(buffer(i)%field)
                end if
            end do
        else if (n > 0) then

            !> Combine the arrays.
            allocate(temp(size(field_list) + n))
            j = 1
            do i = 1, size(field_list)
                allocate(temp(j)%field, source = field_list(i)%field)
                deallocate(field_list(i)%field)
                j = j + 1
            end do
            deallocate(field_list)
            do i = 1, size(buffer)
                if (allocated(buffer(i)%field)) then
                    allocate(temp(j)%field, source = buffer(i)%field)
                    deallocate(buffer(i)%field)
                    j = j + 1
                end if
            end do
            deallocate(buffer)

            !> Transfer back to 'field_list'.
            allocate(field_list(size(temp)))
            do i = 1, size(temp)
                allocate(field_list(i)%field, source = temp(i)%field)
                deallocate(temp(i)%field)
            end do
            deallocate(temp)
        end if

    end subroutine

    subroutine cleanup_field_list(field_list, error_status)

        !> Input/output variables.
        type(io_field_wrapper), dimension(:), allocatable :: field_list
        integer, intent(out) :: error_status

        !> Local variables.
        type(io_field_wrapper), dimension(:), allocatable :: temp
        integer field_count, n, i

        !> Status.
        error_status = 0

        !> Count the number of valid fields.
        field_count = 0
        do i = 1, size(field_list)
            if (allocated(field_list(i)%field)) field_count = field_count + 1
        end do

        !> Compact the list.
        if (field_count > 0) then

            !> Transfer valid fields to a temporary list.
            allocate(temp(field_count))
            n = 1
            do i = 1, size(field_list)
                if (allocated(field_list(i)%field)) then
                    allocate(temp(n)%field, source = field_list(i)%field)
                    deallocate(field_list(i)%field)
                    n = n + 1
                end if
            end do

            !> Deallocate the source list.
            deallocate(field_list)

            !> Rebuild the source list with only the valid fields.
            allocate(field_list(field_count))
            do i = 1, field_count
                allocate(field_list(i)%field, source = temp(i)%field)
                deallocate(temp(i)%field)
            end do
            deallocate(temp)
        else

            !> Deallocate the list.
            do i = 1, size(field_list)
                if (allocated(field_list(i)%field)) deallocate(field_list(i)%field)
            end do
            deallocate(field_list)
        end if

    end subroutine

    subroutine compact_field_real2d_from_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :), intent(in) :: input_field
        real, dimension(:, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(3), d3
        integer i

        !> Status.
        error_status = 0

        !> Get shape of input.
        dim_size = shape(input_field)

        !> Find if the array can be compacted to the desired dimensions (extra dimensions must have a size of one).
        d3 = 0
        do i = 1, 3
            if (dim_size(i) == 1) d3 = i
        end do

        !> Compact the array if the extra dimensions have a size of one.
        if (d3 > 0) then
            select case (d3)
                case (1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(3)))
                    output_field = input_field(1, :, :)
                case (2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(3)))
                    output_field = input_field(:, 1, :)
                case (3)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2)))
                    output_field = input_field(:, :, 1)
            end select
        else

            !> Unable to compact the array.
            error_status = 1
        end if

    end subroutine

    subroutine compact_field_real2d_from_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :, :), intent(in) :: input_field
        real, dimension(:, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(4), dtest, d3, d4
        integer i

        !> Status.
        error_status = 0

        !> Get shape of input.
        dim_size = shape(input_field)

        !> Find if the array can be compacted to the desired dimensions (extra dimensions must have a size of one).
        d3 = 0
        d4 = 0
        do i = 1, 4
            if (dim_size(i) == 1) then
                if (d3 > 0) then
                    d4 = i
                else
                    d3 = i
                end if
            end if
        end do

        !> Compact the array if the extra dimensions have a size of one.
        if (d3 > 0 .and. d4 > 0) then

            !> Calculate the bit value of active indices.
            dtest = radix(d3)**d3 + radix(d4)**d4

            !> Find the missing indices.
            select case (dtest)
                case (radix(dtest)**1 + radix(dtest)**2)
                    !(d3 == 1 .and. d4 == 2)
                    !(d3 == 2 .and. d4 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(3), dim_size(4)))
                    output_field = input_field(1, 1, :, :)
                case (radix(dtest)**1 + radix(dtest)**3)
                    !(d3 == 1 .and. d4 == 3)
                    !(d3 == 3 .and. d4 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(4)))
                    output_field = input_field(1, :, 1, :)
                case (radix(dtest)**1 + radix(dtest)**4)
                    !(d3 == 1 .and. d4 == 4)
                    !(d3 == 4 .and. d4 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(3)))
                    output_field = input_field(1, :, :, 1)
                case (radix(dtest)**2 + radix(dtest)**3)
                    !(d3 == 2 .and. d4 == 3)
                    !(d3 == 3 .and. d4 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(4)))
                    output_field = input_field(:, 1, 1, :)
                case (radix(dtest)**2 + radix(dtest)**4)
                    !(d3 == 2 .and. d4 == 4)
                    !(d3 == 4 .and. d4 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(3)))
                    output_field = input_field(:, 1, :, 1)
                case (radix(dtest)**3 + radix(dtest)**4)
                    !(d3 == 3 .and. d4 == 4)
                    !(d3 == 4 .and. d4 == 3)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2)))
                    output_field = input_field(:, :, 1, 1)
            end select
        else

            !> Unable to compact the array.
            error_status = 1
        end if

    end subroutine

    subroutine compact_field_real2d_from_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :, :, :), intent(in) :: input_field
        real, dimension(:, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(5), dtest, d3, d4, d5
        integer i

        !> Status.
        error_status = 0

        !> Get shape of input.
        dim_size = shape(input_field)

        !> Find if the array can be compacted to the desired dimensions (extra dimensions must have a size of one).
        d3 = 0
        d4 = 0
        d5 = 0
        do i = 1, 5
            if (dim_size(i) == 1) then
                if (d3 > 0) then
                    if (d4 > 0) then
                        d5 = i
                    else
                        d4 = i
                    end if
                else
                    d3 = i
                end if
            end if
        end do

        !> Compact the array if the extra dimensions have a size of one.
        if (d3 > 0 .and. d4 > 0 .and. d5 > 0) then

            !> Calculate the bit value of active indices.
            dtest = radix(d3)**d3 + radix(d4)**d4 + radix(d5)**d5

            !> Find the missing indices.
            select case (dtest)
                case (radix(dtest)**1 + radix(dtest)**2 + radix(dtest)**3)
                    !(d3 == 1 .and. d4 == 2 .and. d5 == 3)
                    !(d3 == 1 .and. d4 == 3 .and. d5 == 2)
                    !(d3 == 2 .and. d4 == 1 .and. d5 == 3)
                    !(d3 == 2 .and. d4 == 3 .and. d5 == 1)
                    !(d3 == 3 .and. d4 == 1 .and. d5 == 2)
                    !(d3 == 3 .and. d4 == 2 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(4), dim_size(5)))
                    output_field = input_field(1, 1, 1, :, :)
                case (radix(dtest)**1 + radix(dtest)**2 + radix(dtest)**4)
                    !(d3 == 1 .and. d4 == 2 .and. d5 == 4)
                    !(d3 == 1 .and. d4 == 4 .and. d5 == 2)
                    !(d3 == 2 .and. d4 == 1 .and. d5 == 4)
                    !(d3 == 2 .and. d4 == 4 .and. d5 == 1)
                    !(d3 == 4 .and. d4 == 1 .and. d5 == 2)
                    !(d3 == 4 .and. d4 == 2 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(3), dim_size(5)))
                    output_field = input_field(1, 1, :, 1, :)
                case (radix(dtest)**1 + radix(dtest)**2 + radix(dtest)**5)
                    !(d3 == 1 .and. d4 == 2 .and. d5 == 5)
                    !(d3 == 1 .and. d4 == 5 .and. d5 == 2)
                    !(d3 == 2 .and. d4 == 1 .and. d5 == 5)
                    !(d3 == 2 .and. d4 == 5 .and. d5 == 1)
                    !(d3 == 5 .and. d4 == 1 .and. d5 == 2)
                    !(d3 == 5 .and. d4 == 2 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(3), dim_size(4)))
                    output_field = input_field(1, 1, :, :, 1)
                case (radix(dtest)**1 + radix(dtest)**3 + radix(dtest)**4)
                    !(d3 == 1 .and. d4 == 3 .and. d5 == 4)
                    !(d3 == 1 .and. d4 == 4 .and. d5 == 3)
                    !(d3 == 3 .and. d4 == 1 .and. d5 == 4)
                    !(d3 == 3 .and. d4 == 4 .and. d5 == 1)
                    !(d3 == 4 .and. d4 == 1 .and. d5 == 3)
                    !(d3 == 4 .and. d4 == 3 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(5)))
                    output_field = input_field(1, :, 1, 1, :)
                case (radix(dtest)**1 + radix(dtest)**3 + radix(dtest)**5)
                    !(d3 == 1 .and. d4 == 3 .and. d5 == 5)
                    !(d3 == 1 .and. d4 == 5 .and. d5 == 3)
                    !(d3 == 3 .and. d4 == 1 .and. d5 == 5)
                    !(d3 == 3 .and. d4 == 5 .and. d5 == 1)
                    !(d3 == 5 .and. d4 == 1 .and. d5 == 3)
                    !(d3 == 5 .and. d4 == 3 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(4)))
                    output_field = input_field(1, :, 1, :, 1)
                case (radix(dtest)**1 + radix(dtest)**4 + radix(dtest)**5)
                    !(d3 == 1 .and. d4 == 4 .and. d5 == 5)
                    !(d3 == 1 .and. d4 == 5 .and. d5 == 4)
                    !(d3 == 4 .and. d4 == 1 .and. d5 == 5)
                    !(d3 == 4 .and. d4 == 5 .and. d5 == 1)
                    !(d3 == 5 .and. d4 == 1 .and. d5 == 4)
                    !(d3 == 5 .and. d4 == 4 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(3)))
                    output_field = input_field(1, :, :, 1, 1)
                case (radix(dtest)**2 + radix(dtest)**3 + radix(dtest)**4)
                    !(d3 == 2 .and. d4 == 3 .and. d5 == 4)
                    !(d3 == 2 .and. d4 == 4 .and. d5 == 3)
                    !(d3 == 3 .and. d4 == 2 .and. d5 == 4)
                    !(d3 == 3 .and. d4 == 4 .and. d5 == 2)
                    !(d3 == 4 .and. d4 == 2 .and. d5 == 3)
                    !(d3 == 4 .and. d4 == 3 .and. d5 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(5)))
                    output_field = input_field(:, 1, 1, 1, :)
                case (radix(dtest)**2 + radix(dtest)**3 + radix(dtest)**5)
                    !(d3 == 2 .and. d4 == 3 .and. d5 == 5)
                    !(d3 == 2 .and. d4 == 5 .and. d5 == 3)
                    !(d3 == 3 .and. d4 == 2 .and. d5 == 5)
                    !(d3 == 3 .and. d4 == 5 .and. d5 == 2)
                    !(d3 == 5 .and. d4 == 2 .and. d5 == 3)
                    !(d3 == 5 .and. d4 == 3 .and. d5 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(4)))
                    output_field = input_field(:, 1, 1, :, 1)
                case (radix(dtest)**2 + radix(dtest)**4 + radix(dtest)**5)
                    !(d3 == 2 .and. d4 == 4 .and. d5 == 5)
                    !(d3 == 2 .and. d4 == 5 .and. d5 == 4)
                    !(d3 == 4 .and. d4 == 2 .and. d5 == 5)
                    !(d3 == 4 .and. d4 == 5 .and. d5 == 2)
                    !(d3 == 5 .and. d4 == 2 .and. d5 == 4)
                    !(d3 == 5 .and. d4 == 4 .and. d5 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(3)))
                    output_field = input_field(:, 1, :, 1, 1)
                case (radix(dtest)**3 + radix(dtest)**4 + radix(dtest)**5)
                    !(d3 == 3 .and. d4 == 4 .and. d5 == 5)
                    !(d3 == 3 .and. d4 == 5 .and. d5 == 4)
                    !(d3 == 4 .and. d4 == 3 .and. d5 == 5)
                    !(d3 == 4 .and. d4 == 5 .and. d5 == 3)
                    !(d3 == 5 .and. d4 == 3 .and. d5 == 4)
                    !(d3 == 5 .and. d4 == 4 .and. d5 == 3)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2)))
                    output_field = input_field(:, :, 1, 1, 1)
            end select
        else

            !> Unable to compact the array.
            error_status = 1
        end if

    end subroutine

    subroutine compact_field_real3d_from_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :, :), intent(in) :: input_field
        real, dimension(:, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(4), d4
        integer i

        !> Status.
        error_status = 0

        !> Get shape of input.
        dim_size = shape(input_field)

        !> Find if the array can be compacted to the desired dimensions (extra dimensions must have a size of one).
        d4 = 0
        do i = 1, 4
            if (dim_size(i) == 1) d4 = i
        end do

        !> Compact the array if the extra dimensions have a size of one.
        if (d4 > 0) then
            select case (d4)
                case (1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(3), dim_size(3)))
                    output_field = input_field(1, :, :, :)
                case (2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(3), dim_size(4)))
                    output_field = input_field(:, 1, :, :)
                case (3)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2), dim_size(4)))
                    output_field = input_field(:, :, 1, :)
                case (4)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2), dim_size(3)))
                    output_field = input_field(:, :, :, 1)
            end select
        else

            !> Unable to compact the array.
            error_status = 1
        end if

    end subroutine

    subroutine compact_field_real3d_from_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :, :, :), intent(in) :: input_field
        real, dimension(:, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(5), dtest, d4, d5
        integer i

        !> Status.
        error_status = 0

        !> Get shape of input.
        dim_size = shape(input_field)

        !> Find if the array can be compacted to the desired dimensions (extra dimensions must have a size of one).
        d4 = 0
        d5 = 0
        do i = 1, 5
            if (dim_size(i) == 1) then
                if (d4 > 0) then
                    d5 = i
                else
                    d4 = i
                end if
            end if
        end do

        !> Compact the array if the extra dimensions have a size of one.
        if (d4 > 0 .and. d5 > 0) then

            !> Calculate the bit value of active indices.
            dtest = radix(d4)**d4 + radix(d5)**d5

            !> Find the missing indices.
            select case (dtest)
                case (radix(dtest)**1 + radix(dtest)**2)
                    !(d4 == 1 .and. d5 == 2)
                    !(d4 == 2 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(3), dim_size(4), dim_size(5)))
                    output_field = input_field(1, 1, :, :, :)
                case (radix(dtest)**1 + radix(dtest)**3)
                    !(d4 == 1 .and. d5 == 3)
                    !(d4 == 3 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(4), dim_size(5)))
                    output_field = input_field(1, :, 1, :, :)
                case (radix(dtest)**1 + radix(dtest)**4)
                    !(d4 == 1 .and. d5 == 4)
                    !(d4 == 4 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(3), dim_size(5)))
                    output_field = input_field(1, :, :, 1, :)
                case (radix(dtest)**1 + radix(dtest)**5)
                    !(d4 == 1 .and. d5 == 5)
                    !(d4 == 5 .and. d5 == 1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(3), dim_size(4)))
                    output_field = input_field(1, :, :, :, 1)
                case (radix(dtest)**2 + radix(dtest)**3)
                    !(d4 == 2 .and. d5 == 3)
                    !(d4 == 3 .and. d5 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(4), dim_size(5)))
                    output_field = input_field(:, 1, 1, :, :)
                case (radix(dtest)**2 + radix(dtest)**4)
                    !(d4 == 2 .and. d5 == 4)
                    !(d4 == 4 .and. d5 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(3), dim_size(5)))
                    output_field = input_field(:, 1, :, 1, :)
                case (radix(dtest)**2 + radix(dtest)**5)
                    !(d4 == 2 .and. d5 == 5)
                    !(d4 == 5 .and. d5 == 2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(3), dim_size(4)))
                    output_field = input_field(:, 1, :, :, 1)
                case (radix(dtest)**3 + radix(dtest)**4)
                    !(d4 == 3 .and. d5 == 4)
                    !(d4 == 4 .and. d5 == 3)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2), dim_size(5)))
                    output_field = input_field(:, :, 1, 1, :)
                case (radix(dtest)**3 + radix(dtest)**5)
                    !(d4 == 3 .and. d5 == 5)
                    !(d4 == 5 .and. d5 == 3)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2), dim_size(4)))
                    output_field = input_field(:, :, 1, :, 1)
                case (radix(dtest)**4 + radix(dtest)**5)
                    !(d4 == 4 .and. d5 == 5)
                    !(d4 == 5 .and. d5 == 4)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2), dim_size(3)))
                    output_field = input_field(:, :, :, 1, 1)
            end select
        else

            !> Unable to compact the array.
            error_status = 1
        end if

    end subroutine

    subroutine compact_field_real4d_from_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        real, dimension(:, :, :, :, :), intent(in) :: input_field
        real, dimension(:, :, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(5), d5
        integer i

        !> Status.
        error_status = 0

        !> Get shape of input.
        dim_size = shape(input_field)

        !> Find if the array can be compacted to the desired dimensions (extra dimensions must have a size of one).
        d5 = 0
        do i = 1, 5
            if (dim_size(i) == 1) d5 = i
        end do

        !> Compact the array if the extra dimensions have a size of one.
        if (d5 > 0) then
            select case (d5)
                case (1)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(3), dim_size(4), dim_size(5)))
                    output_field = input_field(1, :, :, :, :)
                case (2)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(2), dim_size(3), dim_size(4), dim_size(5)))
                    output_field = input_field(:, 1, :, :, :)
                case (3)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2), dim_size(4), dim_size(5)))
                    output_field = input_field(:, :, 1, :, :)
                case (4)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2), dim_size(3), dim_size(5)))
                    output_field = input_field(:, :, :, 1, :)
                case (5)
                    if (.not. allocated(output_field)) allocate(output_field(dim_size(1), dim_size(2), dim_size(3), dim_size(4)))
                    output_field = input_field(:, :, :, :, 1)
            end select
        else

            !> Unable to compact the array.
            error_status = 1
        end if

    end subroutine

    subroutine assign_field_real(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        real output_field
        integer, intent(out) :: error_status

        !> Status.
        error_status = 0

        !> Reset output field.
        output_field = huge(output_field)

        !> Check input variable.
        select type (input_field)
            class is (io_field_real)
                output_field = input_field%dat
            class is (io_field_int)
                output_field = real(input_field%dat)
            class is (io_field_char)
                read(input_field%dat, *, iostat = error_status) output_field
                if (error_status /= 0) error_status = 1
            class is (io_field_real1d)
                if (size(input_field%dat) > 0) then
                    output_field = input_field%dat(1)
                else
                    error_status = 1
                end if
            class is (io_field_int1d)
                if (size(input_field%dat) > 0) then
                    output_field = real(input_field%dat(1))
                else
                    error_status = 1
                end if
            class is (io_field_char1d)
                if (size(input_field%dat) > 0) then
                read(input_field%dat(1), *, iostat = error_status) output_field
                if (error_status /= 0) error_status = 1
                else
                    error_status = 1
                end if
            class is (io_field_real2d)
                if (size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0) then
                    output_field = input_field%dat(1, 1)
                else
                    error_status = 1
                end if
            class is (io_field_int2d)
                if (size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0) then
                    output_field = real(input_field%dat(1, 1))
                else
                    error_status = 1
                end if
            class is (io_field_real3d)
                if (size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0) then
                    output_field = input_field%dat(1, 1, 1)
                else
                    error_status = 1
                end if
            class is (io_field_int3d)
                if (size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0) then
                    output_field = real(input_field%dat(1, 1, 1))
                else
                    error_status = 1
                end if
            class is (io_field_real4d)
                if ( &
                    size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0 .and. &
                    size(input_field%dat, 4) > 0) then
                    output_field = input_field%dat(1, 1, 1, 1)
                else
                    error_status = 1
                end if
            class is (io_field_int4d)
                if ( &
                    size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0 .and. &
                    size(input_field%dat, 4) > 0) then
                    output_field = real(input_field%dat(1, 1, 1, 1))
                else
                    error_status = 1
                end if
            class is (io_field_real5d)
                if ( &
                    size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0 .and. &
                    size(input_field%dat, 4) > 0 .and. size(input_field%dat, 5) > 0) then
                    output_field = input_field%dat(1, 1, 1, 1, 1)
                else
                    error_status = 1
                end if
            class is (io_field_int5d)
                if ( &
                    size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0 .and. &
                    size(input_field%dat, 4) > 0 .and. size(input_field%dat, 5) > 0) then
                    output_field = real(input_field%dat(1, 1, 1, 1, 1))
                else
                    error_status = 1
                end if
            class default
                error_status = 1
        end select

    end subroutine

    subroutine assign_field_int(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        integer output_field
        integer, intent(out) :: error_status

        !> Status.
        error_status = 0

        !> Reset output field.
        output_field = huge(output_field)

        !> Check input variable.
        select type (input_field)
            class is (io_field_real)
                output_field = int(input_field%dat)
            class is (io_field_int)
                output_field = input_field%dat
            class is (io_field_char)
                read(input_field%dat, *, iostat = error_status) output_field
                if (error_status /= 0) error_status = 1
            class is (io_field_real1d)
                if (size(input_field%dat) > 0) then
                    output_field = int(input_field%dat(1))
                else
                    error_status = 1
                end if
            class is (io_field_int1d)
                if (size(input_field%dat) > 0) then
                    output_field = input_field%dat(1)
                else
                    error_status = 1
                end if
            class is (io_field_char1d)
                if (size(input_field%dat) > 0) then
                read(input_field%dat(1), *, iostat = error_status) output_field
                if (error_status /= 0) error_status = 1
                else
                    error_status = 1
                end if
            class is (io_field_real2d)
                if (size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0) then
                    output_field = int(input_field%dat(1, 1))
                else
                    error_status = 1
                end if
            class is (io_field_int2d)
                if (size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0) then
                    output_field = input_field%dat(1, 1)
                else
                    error_status = 1
                end if
            class is (io_field_real3d)
                if (size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0) then
                    output_field = int(input_field%dat(1, 1, 1))
                else
                    error_status = 1
                end if
            class is (io_field_int3d)
                if (size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0) then
                    output_field = input_field%dat(1, 1, 1)
                else
                    error_status = 1
                end if
            class is (io_field_real4d)
                if ( &
                    size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0 .and. &
                    size(input_field%dat, 4) > 0) then
                    output_field = int(input_field%dat(1, 1, 1, 1))
                else
                    error_status = 1
                end if
            class is (io_field_int4d)
                if ( &
                    size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0 .and. &
                    size(input_field%dat, 4) > 0) then
                    output_field = input_field%dat(1, 1, 1, 1)
                else
                    error_status = 1
                end if
            class is (io_field_real5d)
                if ( &
                    size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0 .and. &
                    size(input_field%dat, 4) > 0 .and. size(input_field%dat, 5) > 0) then
                    output_field = int(input_field%dat(1, 1, 1, 1, 1))
                else
                    error_status = 1
                end if
            class is (io_field_int5d)
                if ( &
                    size(input_field%dat, 1) > 0 .and. size(input_field%dat, 2) > 0 .and. size(input_field%dat, 3) > 0 .and. &
                    size(input_field%dat, 4) > 0 .and. size(input_field%dat, 5) > 0) then
                    output_field = input_field%dat(1, 1, 1, 1, 1)
                else
                    error_status = 1
                end if
            class default
                error_status = 1
        end select

    end subroutine

    subroutine assign_field_char(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        character(len = *) output_field
        integer, intent(out) :: error_status

        !> Status.
        error_status = 0

        !> Reset output field.
        output_field = ''

        !> Check input variable.
        select type (input_field)
            class is (io_field_real)
                write(output_field, *, iostat = error_status) input_field%dat
                if (error_status /= 0) error_status = 1
            class is (io_field_int)
                write(output_field, *, iostat = error_status) input_field%dat
                if (error_status /= 0) error_status = 1
            class is (io_field_char)
                output_field = trim(input_field%dat)
            class is (io_field_real1d)
                if (size(input_field%dat) > 0) then
                    write(output_field, *, iostat = error_status) input_field%dat(1)
                    if (error_status /= 0) error_status = 1
                else
                    error_status = 1
                end if
            class is (io_field_int1d)
                if (size(input_field%dat) > 0) then
                    write(output_field, *, iostat = error_status) input_field%dat(1)
                    if (error_status /= 0) error_status = 1
                else
                    error_status = 1
                end if
            class is (io_field_char1d)
                if (size(input_field%dat) > 0) then
                    output_field = trim(input_field%dat(1))
                else
                    error_status = 1
                end if
            class default
                error_status = 1
        end select

        !> Readjust the output string.
        output_field = trim(adjustl(output_field))

    end subroutine

    subroutine assign_field_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        real, dimension(:), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, allocatable :: dat2_r(:, :), dat3_r(:, :, :), dat4_r(:, :, :, :), dat5_r(:, :, :, :, :)
        integer i

        !> Status.
        error_status = 0

        !> Check input variable.
        select type (input_field)
            class is (io_field_real)
                if (.not. allocated(output_field)) allocate(output_field(1))
                output_field = input_field%dat
            class is (io_field_int)
                if (.not. allocated(output_field)) allocate(output_field(1))
                output_field = real(input_field%dat)
            class is (io_field_char)
                if (.not. allocated(output_field)) allocate(output_field(1))
                read(input_field%dat, *, iostat = error_status) output_field(1)
                if (error_status /= 0) error_status = 1
            class is (io_field_real1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat)))
                output_field = input_field%dat
            class is (io_field_int1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat)))
                output_field = real(input_field%dat)
            class is (io_field_char1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat)))
                read(input_field%dat, *, iostat = error_status) (output_field(i), i = 1, size(input_field%dat))
                if (error_status /= 0) error_status = 1
            class is (io_field_real2d)
                call compact_field(input_field%dat, output_field, error_status)
            class is (io_field_int2d)
                allocate(dat2_r(size(input_field%dat, 1), size(input_field%dat, 2)))
                dat2_r = real(input_field%dat)
                call compact_field(dat2_r, output_field, error_status)
                deallocate(dat2_r)
            class is (io_field_real3d)
                call compact_field(input_field%dat, output_field, error_status)
            class is (io_field_int3d)
                allocate(dat3_r(size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3)))
                dat3_r = real(input_field%dat)
                call compact_field(dat3_r, output_field, error_status)
                deallocate(dat3_r)
            class is (io_field_real4d)
                call compact_field(input_field%dat, output_field, error_status)
            class is (io_field_int4d)
                allocate(dat4_r( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4)))
                dat4_r = real(input_field%dat)
                call compact_field(dat4_r, output_field, error_status)
                deallocate(dat4_r)
            class is (io_field_real5d)
                call compact_field(input_field%dat, output_field, error_status)
            class is (io_field_int5d)
                allocate(dat5_r( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4), &
                    size(input_field%dat, 5)))
                dat5_r = real(input_field%dat)
                call compact_field(dat5_r, output_field, error_status)
                deallocate(dat5_r)
            class default
                error_status = 1
        end select

    end subroutine

    subroutine assign_field_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        real, dimension(:, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, allocatable :: dat3_r(:, :, :), dat4_r(:, :, :, :), dat5_r(:, :, :, :, :)
        integer i

        !> Status.
        error_status = 0

        !> Check input variable.
        select type (input_field)
            class is (io_field_real)
                if (.not. allocated(output_field)) allocate(output_field(1, 1))
                output_field = input_field%dat
            class is (io_field_int)
                if (.not. allocated(output_field)) allocate(output_field(1, 1))
                output_field = real(input_field%dat)
            class is (io_field_char)
                if (.not. allocated(output_field)) allocate(output_field(1, 1))
                read(input_field%dat, *, iostat = error_status) output_field(1, 1)
                if (error_status /= 0) error_status = 1
            class is (io_field_real1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), 1))
                output_field(:, 1) = input_field%dat
            class is (io_field_int1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), 1))
                output_field(:, 1) = real(input_field%dat)
            class is (io_field_char1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat), 1))
                read(input_field%dat, *, iostat = error_status) (output_field(i, 1), i = 1, size(input_field%dat))
                if (error_status /= 0) error_status = 1
            class is (io_field_real2d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), size(input_field%dat, 2)))
                output_field = input_field%dat
            class is (io_field_int2d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), size(input_field%dat, 2)))
                output_field = real(input_field%dat)
            class is (io_field_real3d)
                call compact_field(input_field%dat, output_field, error_status)
            class is (io_field_int3d)
                allocate(dat3_r(size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3)))
                dat3_r = real(input_field%dat)
                call compact_field(dat3_r, output_field, error_status)
                deallocate(dat3_r)
            class is (io_field_real4d)
                call compact_field(input_field%dat, output_field, error_status)
            class is (io_field_int4d)
                allocate(dat4_r( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4)))
                dat4_r = real(input_field%dat)
                call compact_field(dat4_r, output_field, error_status)
                deallocate(dat4_r)
            class is (io_field_real5d)
                call compact_field(input_field%dat, output_field, error_status)
            class is (io_field_int5d)
                allocate(dat5_r( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4), &
                    size(input_field%dat, 5)))
                dat5_r = real(input_field%dat)
                call compact_field(dat5_r, output_field, error_status)
                deallocate(dat5_r)
            class default
                error_status = 1
        end select

    end subroutine

    subroutine assign_field_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        real, dimension(:, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, allocatable :: dat4_r(:, :, :, :), dat5_r(:, :, :, :, :)
        integer i

        !> Status.
        error_status = 0

        !> Check input variable.
        select type (input_field)
            class is (io_field_real)
                if (.not. allocated(output_field)) allocate(output_field(1, 1, 1))
                output_field = input_field%dat
            class is (io_field_int)
                if (.not. allocated(output_field)) allocate(output_field(1, 1, 1))
                output_field = real(input_field%dat)
            class is (io_field_char)
                if (.not. allocated(output_field)) allocate(output_field(1, 1, 1))
                read(input_field%dat, *, iostat = error_status) output_field(1, 1, 1)
                if (error_status /= 0) error_status = 1
            class is (io_field_real1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), 1, 1))
                output_field(:, 1, 1) = input_field%dat
            class is (io_field_int1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), 1, 1))
                output_field(:, 1, 1) = real(input_field%dat)
            class is (io_field_char1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat), 1, 1))
                read(input_field%dat, *, iostat = error_status) (output_field(i, 1, 1), i = 1, size(input_field%dat))
                if (error_status /= 0) error_status = 1
            class is (io_field_real2d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), size(input_field%dat, 2), 1))
                output_field(:, :, 1) = input_field%dat
            class is (io_field_int2d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), size(input_field%dat, 2), 1))
                output_field(:, :, 1) = real(input_field%dat)
            class is (io_field_real3d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3)))
                output_field = input_field%dat
            class is (io_field_int3d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3)))
                output_field = real(input_field%dat)
            class is (io_field_real4d)
                call compact_field(input_field%dat, output_field, error_status)
            class is (io_field_int4d)
                allocate(dat4_r( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4)))
                dat4_r = real(input_field%dat)
                call compact_field(dat4_r, output_field, error_status)
                deallocate(dat4_r)
            class is (io_field_real5d)
                call compact_field(input_field%dat, output_field, error_status)
            class is (io_field_int5d)
                allocate(dat5_r( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4), &
                    size(input_field%dat, 5)))
                dat5_r = real(input_field%dat)
                call compact_field(dat5_r, output_field, error_status)
                deallocate(dat5_r)
            class default
                error_status = 1
        end select

    end subroutine

    subroutine assign_field_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        real, dimension(:, :, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, allocatable :: dat5_r(:, :, :, :, :)
        integer i

        !> Status.
        error_status = 0

        !> Check input variable.
        select type (input_field)
            class is (io_field_real)
                if (.not. allocated(output_field)) allocate(output_field(1, 1, 1, 1))
                output_field = input_field%dat
            class is (io_field_int)
                if (.not. allocated(output_field)) allocate(output_field(1, 1, 1, 1))
                output_field = real(input_field%dat)
            class is (io_field_char)
                if (.not. allocated(output_field)) allocate(output_field(1, 1, 1, 1))
                read(input_field%dat, *, iostat = error_status) output_field(1, 1, 1, 1)
                if (error_status /= 0) error_status = 1
            class is (io_field_real1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), 1, 1, 1))
                output_field(:, 1, 1, 1) = input_field%dat
            class is (io_field_int1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), 1, 1, 1))
                output_field(:, 1, 1, 1) = real(input_field%dat)
            class is (io_field_char1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat), 1, 1, 1))
                read(input_field%dat, *, iostat = error_status) (output_field(i, 1, 1, 1), i = 1, size(input_field%dat))
                if (error_status /= 0) error_status = 1
            class is (io_field_real2d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), size(input_field%dat, 2), 1, 1))
                output_field(:, :, 1, 1) = input_field%dat
            class is (io_field_int2d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), size(input_field%dat, 2), 1, 1))
                output_field(:, :, 1, 1) = real(input_field%dat)
            class is (io_field_real3d)
                if (.not. allocated(output_field)) allocate( &
                    output_field(size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), 1))
                output_field(:, :, :, 1) = input_field%dat
            class is (io_field_int3d)
                if (.not. allocated(output_field)) allocate( &
                    output_field(size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), 1))
                output_field(:, :, :, 1) = real(input_field%dat)
            class is (io_field_real4d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4)))
                output_field = input_field%dat
            class is (io_field_int4d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4)))
                output_field = real(input_field%dat)
            class is (io_field_real5d)
                call compact_field(input_field%dat, output_field, error_status)
            class is (io_field_int5d)
                allocate(dat5_r( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4), &
                    size(input_field%dat, 5)))
                dat5_r = real(input_field%dat)
                call compact_field(dat5_r, output_field, error_status)
                deallocate(dat5_r)
            class default
                error_status = 1
        end select

    end subroutine

    subroutine assign_field_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        real, dimension(:, :, :, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer i

        !> Status.
        error_status = 0

        !> Check input variable.
        select type (input_field)
            class is (io_field_real)
                if (.not. allocated(output_field)) allocate(output_field(1, 1, 1, 1, 1))
                output_field = input_field%dat
            class is (io_field_int)
                if (.not. allocated(output_field)) allocate(output_field(1, 1, 1, 1, 1))
                output_field = real(input_field%dat)
            class is (io_field_char)
                if (.not. allocated(output_field)) allocate(output_field(1, 1, 1, 1, 1))
                read(input_field%dat, *, iostat = error_status) output_field(1, 1, 1, 1, 1)
                if (error_status /= 0) error_status = 1
            class is (io_field_real1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), 1, 1, 1, 1))
                output_field(:, 1, 1, 1, 1) = input_field%dat
            class is (io_field_int1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat, 1), 1, 1, 1, 1))
                output_field(:, 1, 1, 1, 1) = real(input_field%dat)
            class is (io_field_char1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat), 1, 1, 1, 1))
                read(input_field%dat, *, iostat = error_status) (output_field(i, 1, 1, 1, 1), i = 1, size(input_field%dat))
                if (error_status /= 0) error_status = 1
            class is (io_field_real2d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), 1, 1, 1))
                output_field(:, :, 1, 1, 1) = input_field%dat
            class is (io_field_int2d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), 1, 1, 1))
                output_field(:, :, 1, 1, 1) = real(input_field%dat)
            class is (io_field_real3d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), 1, 1))
                output_field(:, :, :, 1, 1) = input_field%dat
            class is (io_field_int3d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), 1, 1))
                output_field(:, :, :, 1, 1) = real(input_field%dat)
            class is (io_field_real4d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4), 1))
                output_field(:, :, :, :, 1) = input_field%dat
            class is (io_field_int4d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4), 1))
                output_field(:, :, :, :, 1) = real(input_field%dat)
            class is (io_field_real5d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4), &
                    size(input_field%dat, 5)))
                output_field = input_field%dat
            class is (io_field_int5d)
                if (.not. allocated(output_field)) allocate(output_field( &
                    size(input_field%dat, 1), size(input_field%dat, 2), size(input_field%dat, 3), size(input_field%dat, 4), &
                    size(input_field%dat, 5)))
                output_field = real(input_field%dat)
            class default
                error_status = 1
        end select

    end subroutine

    subroutine assign_field_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        integer, dimension(:), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, dimension(:), allocatable :: dat_r

        !> Call base-routine.
        call assign_field(input_field, dat_r, error_status)
        if (error_status == 0) then
            if (.not. allocated(output_field)) allocate(output_field(size(dat_r)))
            output_field = int(dat_r)
        end if

    end subroutine

    subroutine assign_field_int2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        integer, dimension(:, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, dimension(:, :), allocatable :: dat_r

        !> Call base-routine.
        call assign_field(input_field, dat_r, error_status)
        if (error_status == 0) then
            if (.not. allocated(output_field)) allocate(output_field(size(dat_r, 1), size(dat_r, 2)))
            output_field = int(dat_r)
        end if

    end subroutine

    subroutine assign_field_int3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        integer, dimension(:, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, dimension(:, :, :), allocatable :: dat_r

        !> Call base-routine.
        call assign_field(input_field, dat_r, error_status)
        if (error_status == 0) then
            if (.not. allocated(output_field)) allocate(output_field(size(dat_r, 1), size(dat_r, 2), size(dat_r, 3)))
            output_field = int(dat_r)
        end if

    end subroutine

    subroutine assign_field_int4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        integer, dimension(:, :, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, dimension(:, :, :, :), allocatable :: dat_r

        !> Call base-routine.
        call assign_field(input_field, dat_r, error_status)
        if (error_status == 0) then
            if (.not. allocated(output_field)) allocate( &
                output_field(size(dat_r, 1), size(dat_r, 2), size(dat_r, 3), size(dat_r, 4)))
            output_field = int(dat_r)
        end if

    end subroutine

    subroutine assign_field_int5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        integer, dimension(:, :, :, :, :), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real, dimension(:, :, :, :, :), allocatable :: dat_r

        !> Call base-routine.
        call assign_field(input_field, dat_r, error_status)
        if (error_status == 0) then
            if (.not. allocated(output_field)) allocate(output_field( &
                size(dat_r, 1), size(dat_r, 2), size(dat_r, 3), size(dat_r, 4), size(dat_r, 5)))
            output_field = int(dat_r)
        end if

    end subroutine

    subroutine assign_field_char1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(io_field), intent(in) :: input_field
        character(len = *), dimension(:), allocatable :: output_field
        integer, intent(out) :: error_status

        !> Local variables.
        integer i

        !> Status.
        error_status = 0

        !> Check input variable.
        select type (input_field)
            class is (io_field_real)
                if (.not. allocated(output_field)) allocate(output_field(1))
                write(output_field(1), *, iostat = error_status) input_field%dat
                if (error_status /= 0) error_status = 1
            class is (io_field_int)
                if (.not. allocated(output_field)) allocate(output_field(1))
                write(output_field(1), *, iostat = error_status) input_field%dat
                if (error_status /= 0) error_status = 1
            class is (io_field_char)
                if (.not. allocated(output_field)) allocate(output_field(1))
                output_field(1) = trim(input_field%dat)
            class is (io_field_real1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat)))
                do i = 1, size(input_field%dat)
                    write(output_field(i), *, iostat = error_status) input_field%dat(i)
                    if (error_status /= 0) then
                        error_status = 1
                        exit
                    else
                        output_field(i) = trim(adjustl(output_field(i)))
                    end if
                end do
            class is (io_field_int1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat)))
                do i = 1, size(input_field%dat)
                    write(output_field(i), *, iostat = error_status) input_field%dat(i)
                    if (error_status /= 0) then
                        error_status = 1
                        exit
                    else
                        output_field(i) = trim(adjustl(output_field(i)))
                    end if
                end do
            class is (io_field_char1d)
                if (.not. allocated(output_field)) allocate(output_field(size(input_field%dat)))
                do i = 1, size(input_field%dat)
                    output_field(i) = trim(input_field%dat(i))
                end do
            class default
                error_status = 1
        end select

    end subroutine

    subroutine get_dimension_order(field_dim_names, desired_dim_names, mapped_dim_order, error_status)

        !> Input variables.
        character(len = *), dimension(:), intent(in) :: field_dim_names
        character(len = *), dimension(:), intent(in) :: desired_dim_names

        !> Output variables.
        integer, dimension(:), allocatable, intent(out) :: mapped_dim_order
        integer, intent(out) :: error_status

        !> Local variables.
        integer j, i

        !> Status.
        error_status = 0

        !> Check dimension names.
        if (.not. allocated(mapped_dim_order)) allocate(mapped_dim_order(size(desired_dim_names)))
        mapped_dim_order = 0
        do i = 1, size(desired_dim_names)
            select case (lowercase(desired_dim_names(i)))
                case (DIM_NAME_LAT, DIM_NAME_LATITUDE, DIM_NAME_RLAT, DIM_NAME_Y)
                    do j = 1, size(field_dim_names)
                        select case (lowercase(field_dim_names(j)))
                            case (DIM_NAME_LAT, DIM_NAME_LATITUDE, DIM_NAME_RLAT, DIM_NAME_Y)
                                mapped_dim_order(i) = j
                                exit
                        end select
                    end do
                case (DIM_NAME_LON, DIM_NAME_LONGITUDE, DIM_NAME_RLON, DIM_NAME_X)
                    do j = 1, size(field_dim_names)
                        select case (lowercase(field_dim_names(j)))
                            case (DIM_NAME_LON, DIM_NAME_LONGITUDE, DIM_NAME_RLON, DIM_NAME_X)
                                mapped_dim_order(i) = j
                                exit
                        end select
                    end do
                case (DIM_NAME_TIME, DIM_NAME_T)
                    do j = 1, size(field_dim_names)
                        select case (lowercase(field_dim_names(j)))
                            case (DIM_NAME_TIME, DIM_NAME_T)
                                mapped_dim_order(i) = j
                                exit
                        end select
                    end do
                case (DIM_NAME_SUBBASIN, DIM_NAME_NSUBBASIN, DIM_NAME_N)
                    do j = 1, size(field_dim_names)
                        select case (lowercase(field_dim_names(j)))
                            case (DIM_NAME_SUBBASIN, DIM_NAME_NSUBBASIN, DIM_NAME_N)
                                mapped_dim_order(i) = j
                                exit
                        end select
                    end do
                case (DIM_NAME_GRU, DIM_NAME_NGRU, DIM_NAME_M)
                    do j = 1, size(field_dim_names)
                        select case (lowercase(field_dim_names(j)))
                            case (DIM_NAME_GRU, DIM_NAME_NGRU, DIM_NAME_M)
                                mapped_dim_order(i) = j
                                exit
                        end select
                    end do
                case (DIM_NAME_RVR, DIM_NAME_IAK, DIM_NAME_RIVERCLASS, DIM_NAME_NRVR, DIM_NAME_K)
                    do j = 1, size(field_dim_names)
                        select case (lowercase(field_dim_names(j)))
                            case (DIM_NAME_RVR, DIM_NAME_IAK, DIM_NAME_RIVERCLASS, DIM_NAME_NRVR, DIM_NAME_K)
                                mapped_dim_order(i) = j
                                exit
                        end select
                    end do
                case (DIM_NAME_BASIN, DIM_NAME_B)
                    do j = 1, size(field_dim_names)
                        select case (lowercase(field_dim_names(j)))
                            case (DIM_NAME_BASIN, DIM_NAME_B)
                                mapped_dim_order(i) = j
                                exit
                        end select
                    end do
                case (DIM_NAME_NML, DIM_NAME_LANDTILE, DIM_NAME_G)
                    do j = 1, size(field_dim_names)
                        select case (lowercase(field_dim_names(j)))
                            case (DIM_NAME_NML, DIM_NAME_LANDTILE, DIM_NAME_G)
                                mapped_dim_order(i) = j
                                exit
                        end select
                    end do
                case default
                    do j = 1, size(field_dim_names)
                        if (lowercase(field_dim_names(j)) == lowercase(desired_dim_names(i))) then
                            mapped_dim_order(i) = j
                            exit
                        end if
                    end do
            end select
        end do

        !> Check for missing dimensions.
        if (all(mapped_dim_order == 0)) error_status = 1

    end subroutine

    subroutine get_field_name_and_level(label, field_name, level_name, level_id, error_status)

        !> Input/output variables.
        character(len = *), intent(in) :: label
        character(len = *), intent(out), optional :: field_name
        character(len = *), intent(out), optional :: level_name
        integer, intent(out), optional :: level_id
        integer, intent(out) :: error_status

        !> Local variables.
        character(len = SHORT_FIELD_LENGTH) fld, lvl
        integer a1, a2, ilvl, ierr

        !> Status.
        error_status = 0

        !> Check for a level.
        fld = trim(label)
        a1 = index(trim(fld), ' ')
        a2 = index(trim(fld), ' ', back = .true.)
        lvl = ''
        ilvl = 0
        if (a1 > 0) then
            if (a2 /= a1) then
                a2 = index(fld((a1 + 1):), ' ')
                lvl = fld((a1 + 1):(a2 + a1))
            else
                lvl = fld((a1 + 1):)
            end if
            fld = fld(1:a1)
            read(lvl, *, iostat = ierr) ilvl
            if (ierr /= 0) ilvl = 0
        end if

        !> Assign to output variables.
        if (present(field_name)) field_name = trim(fld)
        if (present(level_name)) level_name = trim(lvl)
        if (present(level_id)) level_id = ilvl

    end subroutine

end module
