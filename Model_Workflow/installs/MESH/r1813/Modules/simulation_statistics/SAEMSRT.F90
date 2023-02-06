FUNCTION SAEMSRT(OBS,SIM,N,NS,NMIN,IW1,IW2)
!>
!>       DECEMBER 16, 2010 - M.A. Mekonnen & B. Davison
!>=======================================================================
!>
!>       The function computes the sum of absolute value of 
!>       moving sorted errors.
!>
!>=======================================================================
!>
!>       OBS        -   Observed values 
!>       SIM        -   Simulated values 
!>       N          -   Number of days
!>       NS         -   Number of stations
!>       NMIN       -   Minimum of number of days for model spin-up
!>       IW1        -   Window size (unit -> days)
!>       IW2        -   Window spacing (unit -> days)
!>                     -ve values indicate overlapping)
!>
!>       SAEMSRT    -   Sum of absolute value of moving errors (after sorting)
!>=======================================================================

    IMPLICIT NONE
    
    !> Incoming variables
    INTEGER N,NS,NMIN,IW1,IW2
    REAL    OBS(N,NS),SIM(N,NS)
    
    !> Outgoing variables
    REAL    SAEMSRT
    
    !> Local variables
    INTEGER I,NN,NW,IW1PW2,ISTRT,IEND
    REAL    QO(IW1,NS),QS(IW1,NS)
    
    !> Functions 
    REAL SAESRT
    
    !> Initialize objective function value
    SAEMSRT = 0.0
    
	!> Number of calibraton data - excluding model spin-up days.
	NN = N - NMIN + 1
    
	!> Sum of window size and window spacing
	IW1PW2 = IW1 + IW2
	
	!> Perform the computation only if the size of the 
	!> calibration days is wide enough to accommodate 
	!> a minimum of one window
	IF(NN > IW1PW2)THEN

		!> Compute number of windows by rearranging the eqn:
		!> N = (W1+1)*NW + (W2-1)*(NW-1) = NW*(W1+W2)-W2+1 
		NW = (N + IW2 - 1) / (IW1PW2)  
	    
		!> Loop through each window and Sum up the objective 
		!> function values of each window 
        DO I = 1, NW
          
           !> Starting and ending index of current window
           ISTRT   = NMIN + (I-1)*IW1PW2
           IEND    = ISTRT + IW1 - 1
           
           !> Extract the matrices of the current window	
           QO      = OBS(ISTRT:IEND,:)
           QS      = SIM(ISTRT:IEND,:)
           
           !> Compute the objective function value of the 
           !> current window and accumulate.
           SAEMSRT = SAEMSRT + SAESRT(QO,QS,IW1,NS,1)
           
	    ENDDO
	ELSE
	    PRINT*, "CHECK THE SUM OF WINDOW SIZE AND WINDOW SPACING WITH ", &
		        "THE NUMBER OF CALIBRATION DAYS MINUS MODEL SPIN-UP DAYS"
		STOP
    ENDIF
	
RETURN
END