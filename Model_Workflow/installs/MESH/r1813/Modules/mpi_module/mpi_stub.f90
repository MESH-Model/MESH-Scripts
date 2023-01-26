!>
!> Stub for MPI subroutines if an MPI compiler cannot be used.
!>
module mpi

    implicit none

    integer :: &
        MPI_COMM_WORLD = 0, MPI_SUCCESS = 0, MPI_REQUEST_NULL = 0, MPI_STATUS_SIZE = 0, MPI_STATUS_IGNORE = 0, &
        MPI_REAL = 0, MPI_REAL8 = 0, MPI_INT = 0

    interface MPI_Send
        module procedure MPI_Send_Real_Arr, MPI_Send_Real, MPI_Send_Int_Arr, MPI_Send_Int
    end interface

    interface MPI_Isend
        module procedure MPI_Isend_Real_Arr, MPI_Isend_Real, MPI_Isend_Int_Arr, MPI_Isend_Int
    end interface

    interface MPI_Recv
        module procedure MPI_Recv_Real_Arr, MPI_Recv_Real, MPI_Recv_Int_Arr, MPI_Recv_Int
    end interface

    interface MPI_Irecv
        module procedure MPI_Irecv_Real_Arr, MPI_Irecv_Real, MPI_Irecv_Int_Arr, MPI_Irecv_Int
    end interface

    contains

    subroutine MPI_Comm_size(comm, elements, ierr)
        integer comm, elements, ierr
        elements = 1
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Comm_rank(comm, rank, ierr)
        integer comm, rank, ierr
        rank = 0
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Init(ierr)
        integer ierr
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Finalize(ierr)
        integer ierr
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Barrier(comm, ierr)
        integer comm, ierr
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Send_Real_Arr(buf, elements, datatype, dest, tag, comm, ierr)
        real buf(:)
        integer elements, datatype, dest, tag, comm, ierr
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Send_Real(buf, elements, datatype, dest, tag, comm, ierr)
        real buf
        integer elements, datatype, dest, tag, comm, ierr
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Send_Int_Arr(buf, elements, datatype, dest, tag, comm, ierr)
        integer buf(:), elements, datatype, dest, tag, comm, ierr
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Send_Int(buf, elements, datatype, dest, tag, comm, ierr)
        integer buf, elements, datatype, dest, tag, comm, ierr
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Isend_Real_Arr(buf, elements, datatype, dest, tag, comm, request, ierr)
        real buf(:)
        integer elements, datatype, dest, tag, comm, request, ierr
        request = MPI_REQUEST_NULL
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Isend_Real(buf, elements, datatype, dest, tag, comm, request, ierr)
        real buf
        integer elements, datatype, dest, tag, comm, request, ierr
        request = MPI_REQUEST_NULL
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Isend_Int_Arr(buf, elements, datatype, dest, tag, comm, request, ierr)
        integer buf(:), elements, datatype, dest, tag, comm, request, ierr
        request = MPI_REQUEST_NULL
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Isend_Int(buf, elements, datatype, dest, tag, comm, request, ierr)
        integer buf, elements, datatype, dest, tag, comm, request, ierr
        request = MPI_REQUEST_NULL
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Testall(elements, array_of_requests, flag, array_of_statuses, ierr)
        integer elements, array_of_requests(:), array_of_statuses(:, :), ierr
        logical flag
        array_of_statuses = MPI_SUCCESS
        flag = .true.
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Recv_Real_Arr(buf, elements, datatype, source, tag, comm, status, ierr)
        real buf(:)
        integer elements, datatype, source, tag, comm, status, ierr
        status = MPI_SUCCESS
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Recv_Real(buf, elements, datatype, source, tag, comm, status, ierr)
        real buf
        integer elements, datatype, source, tag, comm, status, ierr
        status = MPI_SUCCESS
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Recv_Int_Arr(buf, elements, datatype, source, tag, comm, status, ierr)
        integer buf(:), elements, datatype, source, tag, comm, status, ierr
        status = MPI_SUCCESS
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Recv_Int(buf, elements, datatype, source, tag, comm, status, ierr)
        integer buf, elements, datatype, source, tag, comm, status, ierr
        status = MPI_SUCCESS
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Irecv_Real_Arr(buf, elements, datatype, source, tag, comm, request, ierr)
        real buf(:)
        integer elements, datatype, source, tag, comm, request, ierr
        request = MPI_REQUEST_NULL
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Irecv_Real(buf, elements, datatype, source, tag, comm, request, ierr)
        real buf
        integer elements, datatype, source, tag, comm, request, ierr
        request = MPI_REQUEST_NULL
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Irecv_Int_Arr(buf, elements, datatype, source, tag, comm, request, ierr)
        integer buf(:), elements, datatype, source, tag, comm, request, ierr
        request = MPI_REQUEST_NULL
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Irecv_Int(buf, elements, datatype, source, tag, comm, request, ierr)
        integer buf, elements, datatype, source, tag, comm, request, ierr
        request = MPI_REQUEST_NULL
        ierr = MPI_SUCCESS
    end subroutine

    subroutine MPI_Abort(comm, errorcode, ierr)
        integer comm, errorcode, ierr
        ierr = MPI_SUCCESS
    end subroutine

end module !mpi
