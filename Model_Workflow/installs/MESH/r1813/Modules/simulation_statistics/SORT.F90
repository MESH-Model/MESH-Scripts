SUBROUTINE SORT (X,N)

!> Sort an array in decreasing order (Original program by John Mahaffy - 1995-03-10)

!> X - array of values to be sorted 
!> N - number of values

IMPLICIT NONE

!> Incoming variables
INTEGER N
REAL    X(N)

!> Local variables
INTEGER I, ISWAP(1), ITEMP, ISWAP1
REAL    TEMP

!> Intrinsic Function
INTRINSIC MAXLOC

DO I = 1, N-1
   ISWAP  = MAXLOC(X(I:N))
   ISWAP1 = ISWAP(1)+I-1
   IF(ISWAP1 .NE. I) THEN
      TEMP      = X(I)
      X(I)      = X(ISWAP1)
      X(ISWAP1) = TEMP
   ENDIF
ENDDO

RETURN
END