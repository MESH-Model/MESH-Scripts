      SUBROUTINE RESUME_STATE_R2C(NML,NLTEST,NMTEST,NCOUNT, &
                    IMIN,ACLASS,NR2C_R,GRD_R,GAT_R,GRDGAT_R,R2C_ATTRIBUTES_R, &
                    NLAT,XXX,YYY,XCOUNT,YCOUNT,ILMOS,JLMOS,ILG,IC,ICP1,IG, &
                       TBARGAT,THLQGAT,THICGAT,TPNDGAT,ZPNDGAT, &
                       TBASGAT,ALBSGAT,TSNOGAT,RHOSGAT,SNOGAT,  &
                       TCANGAT,RCANGAT,SCANGAT,GROGAT, CMAIGAT, &
                       FCANGAT,LNZ0GAT,ALVCGAT,ALICGAT,PAMXGAT, &
                       PAMNGAT,CMASGAT,ROOTGAT,RSMNGAT,QA50GAT, &
                       VPDAGAT,VPDBGAT,PSGAGAT,PSGBGAT,PAIDGAT, &
                       HGTDGAT,ACVDGAT,ACIDGAT,TSFSGAT,WSNOGAT, &
                       THPGAT, THRGAT, THMGAT, BIGAT,  PSISGAT, &
                       GRKSGAT,THRAGAT,HCPSGAT,TCSGAT,          &
                       THFCGAT,PSIWGAT,DLZWGAT,ZBTWGAT,         &
                       ZSNLGAT,ZPLGGAT,ZPLSGAT,TACGAT, QACGAT,  &
                       DRNGAT, XSLPGAT,XDGAT,WFSFGAT,KSGAT,     &
                       ALGWGAT,ALGDGAT,ASVDGAT,ASIDGAT,AGVDGAT, &
                       AGIDGAT,ISNDGAT,RADJGAT,ZBLDGAT,Z0ORGAT, &
                       ZRFMGAT,ZRFHGAT,ZDMGAT, ZDHGAT, FSVHGAT, &
                       FSIHGAT,CSZGAT, FDLGAT, ULGAT,  VLGAT,   &
                       TAGAT,  QAGAT,  PRESGAT,PREGAT, PADRGAT, &
                       VPDGAT, TADPGAT,RHOAGAT,RPCPGAT,TRPCGAT, &
                       SPCPGAT,TSPCGAT,RHSIGAT,FCLOGAT,DLONGAT, &
                       GGEOGAT,                                 &
                       CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT, &
                       QFSGAT, QFXGAT, PETGAT, GAGAT,  EFGAT,   &
                       GTGAT,  QGGAT,  ALVSGAT,ALIRGAT, &
                       SFCTGAT,SFCUGAT,SFCVGAT,SFCQGAT,FSNOGAT, &
                       FSGVGAT,FSGSGAT,FSGGGAT,FLGVGAT,FLGSGAT, &
                       FLGGGAT,HFSCGAT,HFSSGAT,HFSGGAT,HEVCGAT, &
                       HEVSGAT,HEVGGAT,HMFCGAT,HMFNGAT,HTCCGAT, &
                       HTCSGAT,PCFCGAT,PCLCGAT,PCPNGAT,PCPGGAT, &
                       QFGGAT, QFNGAT, QFCLGAT,QFCFGAT,ROFGAT,  &
                       ROFOGAT,ROFSGAT,ROFBGAT,TROFGAT,TROOGAT, &
                       TROSGAT,TROBGAT,ROFCGAT,ROFNGAT,ROVGGAT, &
                       WTRCGAT,WTRSGAT,WTRGGAT,DRGAT,  GFLXGAT, &
                       HMFGGAT,HTCGAT, QFCGAT,MANNGAT, DDGAT,   &
                       SANDGAT,CLAYGAT,IGDRGAT,VMODGAT,QLWOGAT, &
                       coordsys1,datum1, zone1,  XORIGIN,YORIGIN,XDELTA,YDELTA)
!> This subroutine is used to write a resume file in r2c format.
!> The resume file contains all of the GAT_R variables used
!> in the loop for 
     
IMPLICIT NONE

INTEGER i,j,k,n

!     * LAND SURFACE PROGNOSTIC VARIABLES.
REAL    TBARGAT(ILG,IG),   THLQGAT(ILG,IG),   THICGAT(ILG,IG), &
        TPNDGAT(ILG),      ZPNDGAT(ILG),      TBASGAT(ILG),   &
        ALBSGAT(ILG),      TSNOGAT(ILG),      RHOSGAT(ILG),   &
        SNOGAT (ILG),      TCANGAT(ILG),      RCANGAT(ILG),   &
        SCANGAT(ILG),      GROGAT (ILG),      CMAIGAT(ILG), &
        TSFSGAT(ILG,4),    TACGAT (ILG),      QACGAT (ILG), &
        WSNOGAT(ILG)

!     * CANOPY AND SOIL INFORMATION ARRAYS.
!     * (THE LENGTH OF THESE ARRAYS IS DETERMINED BY THE NUMBER
!     * OF SOIL LAYERS (3) AND THE NUMBER OF BROAD VEGETATION
!     * CATEGORIES (4, OR 5 INCLUDING URBAN AREAS).)
!
REAL    FCANGAT(ILG,ICP1),   LNZ0GAT(ILG,ICP1), &
        ALVCGAT(ILG,ICP1),   ALICGAT(ILG,ICP1), &
        PAMXGAT(ILG,IC),     PAMNGAT(ILG,IC),   &
        CMASGAT(ILG,IC),     ROOTGAT(ILG,IC),   &
        RSMNGAT(ILG,IC),     QA50GAT(ILG,IC),   &
        VPDAGAT(ILG,IC),     VPDBGAT(ILG,IC),   &
        PSGAGAT(ILG,IC),     PSGBGAT(ILG,IC),   &
        PAIDGAT(ILG,IC),     HGTDGAT(ILG,IC),   &
        ACVDGAT(ILG,IC),     ACIDGAT(ILG,IC) 

REAL    THPGAT (ILG,IG),   THRGAT (ILG,IG),   THMGAT (ILG,IG), &
        BIGAT  (ILG,IG),   PSISGAT(ILG,IG),   GRKSGAT(ILG,IG), &   
        THRAGAT(ILG,IG),   HCPSGAT(ILG,IG),                    &
        TCSGAT (ILG,IG),   THFCGAT(ILG,IG),   PSIWGAT(ILG,IG), &
        DLZWGAT(ILG,IG),   ZBTWGAT(ILG,IG),                    &
        DRNGAT (ILG),      XSLPGAT(ILG),      XDGAT(ILG),      &
        WFSFGAT(ILG),      KSGAT(ILG),        ALGWGAT(ILG),    &   
        ALGDGAT(ILG),      ASVDGAT(ILG),      ASIDGAT(ILG),    &    
        AGVDGAT(ILG),      AGIDGAT(ILG),      ZSNLGAT(ILG),    &
        ZPLGGAT(ILG),      ZPLSGAT(ILG)

INTEGER ISNDGAT(ILG,IG)

!     * ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES.
!
REAL    ZRFMGAT(ILG), ZRFHGAT(ILG), ZDMGAT (ILG), ZDHGAT (ILG), &
        FSVHGAT(ILG), FSIHGAT(ILG), CSZGAT (ILG), FDLGAT (ILG), &
        ULGAT  (ILG), VLGAT  (ILG), TAGAT  (ILG), QAGAT  (ILG), &
        PRESGAT(ILG), PREGAT (ILG), PADRGAT(ILG), VPDGAT (ILG), &
        TADPGAT(ILG), RHOAGAT(ILG), ZBLDGAT(ILG), Z0ORGAT(ILG), &
        RPCPGAT(ILG), TRPCGAT(ILG), SPCPGAT(ILG), TSPCGAT(ILG), &
        RHSIGAT(ILG), FCLOGAT(ILG), DLONGAT(ILG), GGEOGAT(ILG), &
        RADJGAT(ILG), VMODGAT(ILG), QLWOGAT(ILG)

!     * LAND SURFACE DIAGNOSTIC VARIABLES.

REAL    CDHGAT (ILG),  CDMGAT (ILG),  HFSGAT (ILG),  TFXGAT (ILG), &
        QEVPGAT(ILG),  QFSGAT (ILG),  QFXGAT (ILG),  PETGAT (ILG), &
        GAGAT  (ILG),  EFGAT  (ILG),  GTGAT  (ILG),  QGGAT  (ILG), &
        ALVSGAT(ILG),  ALIRGAT(ILG),  FSNOGAT(ILG), &
        SFCTGAT(ILG),  SFCUGAT(ILG),  SFCVGAT(ILG),  SFCQGAT(ILG), &
        FSGVGAT(ILG),  FSGSGAT(ILG),  FSGGGAT(ILG),  FLGVGAT(ILG), &
        FLGSGAT(ILG),  FLGGGAT(ILG),  HFSCGAT(ILG),  HFSSGAT(ILG), &
        HFSGGAT(ILG),  HEVCGAT(ILG),  HEVSGAT(ILG),  HEVGGAT(ILG), &
        HMFCGAT(ILG),  HMFNGAT(ILG),  HTCCGAT(ILG),  HTCSGAT(ILG), &
        PCFCGAT(ILG),  PCLCGAT(ILG),  PCPNGAT(ILG),  PCPGGAT(ILG), &
        QFGGAT (ILG),  QFNGAT (ILG),  QFCLGAT(ILG),  QFCFGAT(ILG), &
        ROFGAT (ILG),  ROFOGAT(ILG),  ROFSGAT(ILG),  ROFBGAT(ILG), &
        TROFGAT(ILG),  TROOGAT(ILG),  TROSGAT(ILG),  TROBGAT(ILG), &
        ROFCGAT(ILG),  ROFNGAT(ILG),  ROVGGAT(ILG),  WTRCGAT(ILG), &
        WTRSGAT(ILG),  WTRGGAT(ILG),  DRGAT  (ILG)

REAL    HMFGGAT(ILG,IG),  HTCGAT (ILG,IG),  QFCGAT (ILG,IG), &
        GFLXGAT(ILG,IG)
    
! * WATROF DECLARATIONS
REAL    DDGAT(ILG),MANNGAT(ILG)

!     * SAND AND CLAY
REAL    SANDGAT(ILG,IG),   CLAYGAT(ILG,IG)

!INTEGER NML,NLTEST,NMTEST,NCOUNT,IMIN,NR2C,NLAT,ILG,XCOUNT,YCOUNT,IC,ICP1,IG
INTEGER NML,NLTEST,NMTEST,NCOUNT,IMIN,NR2C_R,NLAT,ILG,IC,ICP1,IG
!INTEGER XXX(NLAT),YYY(NLAT)
INTEGER ILMOS(ILG),JLMOS(ILG),IGDRGAT(ILG)
INTEGER GRD_R(NR2C_R),GAT_R(NR2C_R),GRDGAT_R(NR2C_R)

!REAL    ACLASS(NLTEST,NMTEST+1)
   
INTEGER COUNT,XCOUNT,YCOUNT,counter
REAL    DATAIN(ILG)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: DATAOUT
CHARACTER(*) R2C_ATTRIBUTES_R(NR2C_R,3)

integer resumeIostat
character(10) ctime,coordsys1,datum1,zone1
character(8)  cday
real xorigin,yorigin,xdelta,ydelta
REAL ACLASS(NLTEST,NMTEST+1)
INTEGER XXX(NLAT),YYY(NLAT)

LOGICAL RESUMESTATER2C
INTEGER PAS
INTEGER NR2CSTATES,IOS,DELTR2C_R,II,JJ
character*50 junk
character*10 attribute_test

!> *********************************************************************
!> Open and read in values from resume_state_r2c.txt file (needs fixing, kind of dumb to open the file twice (in the driver and here)
!> *********************************************************************
NR2CSTATES = 0
   INQUIRE(FILE='resume_state_r2c.txt', EXIST = RESUMESTATER2C)
   IF(RESUMESTATER2C)THEN
      OPEN(57, FILE = 'resume_state_r2c.txt')
      READ(57,*,IOSTAT=IOS)NR2C_R,DELTR2C_R
      IF(IOS /= 0 .OR. MOD(DELTR2C_R,30) /= 0)THEN
         WRITE(6,9002)
         STOP

9002 FORMAT('ERROR IN READING resume_state_r2c.txt FILE. ',/, &
            'THE FIRST RECORD AT THE FIRST LINE IS FOR THE NUMBER OF ALL THE ', &
            'VARIABLES LISTED IN THE save_state_r2c.txt FILE.',/,&
            'THE SECOND RECORD AT THE FIRST LINE IS TIME STEP FOR R2C OUTPUT. ', &
            'IT SHOULD BE AN INTEGER MULTIPLE OF 30. ',/,&
            'THE REMAINING RECORDS SHOULD CONTAIN 3 COLUMNS FOR EACH VARIABLE WITH INTEGER VALUES OF ', &
            'EITHER 0 OR 1 AND 3 COLUMNS CONTAINING INFORMATION ABOUT THE VARIABLES')

      ENDIF
      
      PRINT*
      PRINT*,'THE FOLLOWING VARIABLES WILL BE READ-IN FROM AN R2C FILE (resume_state.r2c):'

      DO I = 1, NR2C_R
          READ(57,*,IOSTAT = IOS)GRD_R(I),GAT_R(I),GRDGAT_R(I),(R2C_ATTRIBUTES_R(I,J),J=1,3)
          IF(IOS /= 0)THEN
             PRINT*,'ERROR READING resume_state_r2c.txt FILE AT LINE ', I + 1
             STOP
          ELSE
           IF(GRD_R(I)==1)THEN
              NR2CSTATES = NR2CSTATES + 1
              PRINT*,NR2CSTATES,' (GRD_R)    : ',R2C_ATTRIBUTES_R(I,3)
           ENDIF
           IF(GAT_R(I)==1)THEN
              do j=1,nmtest
                 NR2CSTATES = NR2CSTATES + 1
                 PRINT*,NR2CSTATES,' (GAT_R)    : ',R2C_ATTRIBUTES_R(I,3),' CLASS ',j
              enddo
           ENDIF
          ENDIF
      ENDDO
      CLOSE(57)
   ELSE
      PRINT*
      PRINT*,"resume_state_r2c.txt FILE DOESN'T EXIST. ", &
             "SAVERESUMEFLAG SHOULD BE SET TO ZERO IF R2C OUTPUTS ARE NOT NEEDED."
      PRINT*
      STOP
   ENDIF

ALLOCATE (DATAOUT(NR2CSTATES,XCOUNT,YCOUNT))

!open output resume file
      OPEN (263, FILE="./resume_state.r2c", STATUS="OLD", &
           IOSTAT=resumeIostat)
        if(resumeIostat.ne.0) then
          print*,'There is a problem reading resume_state.r2c. IOSTAT = ',resumeIostat
          stop
        endif
!	  read the header
        read(263,*) junk
        read(263,*) junk
        read(263,*) junk
	    read(263,*) junk
        read(263,*) junk
        read(263,*) junk
	    read(263,*) junk
	    read(263,*) junk
        read(263,*) junk
        read(263,*) junk
	    read(263,*) junk
        read(263,*) junk
        read(263,*) junk
        read(263,*) junk
	  read(263,*) junk
	  if(coordsys1.eq.'LATLONG   ')then
	    read(263,*) junk
	  endif
	  if(coordsys1.eq.'UTM       ')then
	    read(263,*) junk
          read(263,*) junk
	  endif
        read(263,*) junk
        read(263,*) junk
        read(263,*) junk
        read(263,*) junk
        read(263,*) junk
        read(263,*) junk
        do i = 1,NR2C_R
          if(GRD_R(I).eq.1) then
            read(263,'(28X,a10)') attribute_test
              if(TRIM(attribute_test).ne.TRIM(R2C_ATTRIBUTES_R(I,1))) then
                print*,'resume_state_r2c.txt is incompatible with resume_state.r2c'
                print*,'please fix MESH_input_run_options.ini, resume_state_r2c.txt or resume_state.r2c'
                print*,'standalone MESH will terminate now'
                stop
              endif
            read(263,*) junk
          endif
          if(GAT_R(I).eq.1) then
          do j=1,nmtest
            read(263,'(28X,a10)') attribute_test
              if(TRIM(attribute_test).ne.TRIM(R2C_ATTRIBUTES_R(I,1))) then
                print*,'resume_state_r2c.txt is incompatible with resume_state.r2c'
                print*,'please fix MESH_input_run_options.ini, resume_state_r2c.txt or resume_state.r2c'
                print*,'standalone MESH will terminate now'
                stop
              endif
            read(263,*) junk
          enddo
          endif
        enddo
        read(263,*) junk
        read(263,*) junk
        read(263,*) junk
        read(263,*) junk
        read(263,*) junk
        read(263,*) junk
     	read(263,*) junk
! end of header

! Read-in the variables, from save_state.r2c based on save_state_r2c.txt, and set GAT_R values accordingly
! Note: if both GRD_R(N) and GAT_R(N) = 1, then the GAT_R will over-ride the GRD_R by design
      COUNT = 0

      DO N = 1, NR2C_R
       IF(GRD_R(N) == 1)THEN
         COUNT = COUNT + 1
         DO J = 1, YCOUNT
!           READ(263,'(999(E10.3))')(DATAOUT(COUNT,I,J),I=1,XCOUNT)
!           READ(263,'(999(F5.2))')(DATAOUT(COUNT,I,J),I=1,XCOUNT)
           READ(263,*)(DATAOUT(COUNT,I,J),I=1,XCOUNT)
         ENDDO      
         DO I = 1, NML
           II = XXX(ILMOS(I))
           JJ = YYY(ILMOS(I))
           DATAIN(I) = DATAOUT(COUNT,II,JJ)
         END DO
       ENDIF
       IF(GAT_R(N) == 1)THEN
          DO K = 1, NMTEST
          COUNT = COUNT + 1
             DO J = 1, YCOUNT
                READ(263,'(999(E10.3))')(DATAOUT(COUNT,I,J),I=1,XCOUNT)
             ENDDO
             DO I = 1, NML
               II = XXX(ILMOS(I))
               JJ = YYY(ILMOS(I))
               DATAIN(I) = DATAOUT(COUNT,II,JJ)
             END DO
          ENDDO
       ENDIF

       ! These must be in the same order as in R2C_DATA.F90 for this to work properly
       ! use a counter to account for the option to have a different numbers of soil layers
       counter = 0

       ! if both GRD_R and GAT_R = 1, then DATAIN will come from GAT_R while GRD_R will be ignored
        IF(GRD_R(N).EQ.1.OR.GAT_R(N).EQ.1)THEN

          !> ATTRIBUTE 1 - 
          DO J = 1, IG
          counter = counter + 1
          ! make sure that we are using the correct DATAIN
          IF(N.eq.counter) THEN
            ! set the GAT_R variable from the save_state.r2c
            TBARGAT(:,J) = DATAIN
          ENDIF
          ENDDO
       
          !> ATTRIBUTE 2 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            THLQGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 3 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            THICGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 4 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TPNDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 5 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ZPNDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 6 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TBASGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 7 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ALBSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 8 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TSNOGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 9 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            RHOSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 10 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            SNOGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 11 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TCANGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 12 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            RCANGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 13 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            SCANGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 14 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            GROGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 15 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            CMAIGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 16 - 
          DO J = 1, ICP1
          counter = counter + 1
          IF(N.eq.counter) THEN
            FCANGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 17 - 
          DO J = 1, ICP1
          counter = counter + 1
          IF(N.eq.counter) THEN
            LNZ0GAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 18 - 
          DO J = 1, ICP1
          counter = counter + 1
          IF(N.eq.counter) THEN
            ALVCGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 19 - 
          DO J = 1, ICP1
          counter = counter + 1
          IF(N.eq.counter) THEN
            ALICGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 20 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            PAMXGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 21 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            PAMNGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 22 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            CMASGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 23 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            ROOTGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 24 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            RSMNGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 25 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            QA50GAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 26 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            VPDAGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 27 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            VPDBGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 28 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            PSGAGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 29 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            PSGBGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 30 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            PAIDGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 31 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            HGTDGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 32 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            ACVDGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 33 - 
          DO J = 1, IC
          counter = counter + 1
          IF(N.eq.counter) THEN
            ACIDGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 34 - 
          DO J = 1, 4
          counter = counter + 1
          IF(N.eq.counter) THEN
            TSFSGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 35 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            WSNOGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 36 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            THPGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 37 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            THRGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 38 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            THMGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 39 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            BIGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 40 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            PSISGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 41 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            GRKSGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 42 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            THRAGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 43 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            HCPSGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 44 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            TCSGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 45 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            THFCGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 46 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            PSIWGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 47 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            DLZWGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 48 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            ZBTWGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 49 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ZSNLGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 50 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ZPLGGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 51 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ZPLSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 52 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TACGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 53 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QACGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 54 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            DRNGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 55 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            XSLPGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 56 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            XDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 57 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            WFSFGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 58 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            KSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 59 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ALGWGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 60 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ALGDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 61 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ASVDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 62 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ASIDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 63 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            AGVDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 64 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            AGIDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 65 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            ISNDGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 66 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            RADJGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 67 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ZBLDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 68 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            Z0ORGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 69 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ZRFMGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 70 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ZRFHGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 71 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ZDMGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 72 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ZDHGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 73 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FSVHGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 74 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FSIHGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 75 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            CSZGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 76 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FDLGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 77 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ULGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 78 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            VLGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 79 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TAGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 80 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QAGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 81 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            PRESGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 82 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            PREGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 83 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            PADRGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 84 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            VPDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 85 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TADPGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 86 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            RHOAGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 87 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            RPCPGAT = DATAIN
          ENDIF
          
          !> ATTRIBUTE 88 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TRPCGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 89 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            SPCPGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 90 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TSPCGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 91 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            RHSIGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 92 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FCLOGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 93 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            DLONGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 94 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            GGEOGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 95 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            CDHGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 96 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            CDMGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 97 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HFSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 98 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TFXGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 99 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QEVPGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 100 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QFSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 101 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QFXGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 102 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            PETGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 103 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            GAGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 104 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            EFGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 105 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            GTGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 106 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QGGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 107 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ALVSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 108 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ALIRGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 109 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            SFCTGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 110 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            SFCUGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 111 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            SFCVGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 112 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            SFCQGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 113 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FSNOGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 114 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FSGVGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 115 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FSGSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 116 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FSGGGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 117 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FLGVGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 118 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FLGSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 119 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            FLGGGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 120 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HFSCGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 121 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HFSSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 122 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HFSGGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 123 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HEVCGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 124 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HEVSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 125 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HEVGGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 126 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HMFCGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 127 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HMFNGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 128 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HTCCGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 129 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            HTCSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 130 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            PCFCGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 131 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            PCLCGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 132 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            PCPNGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 133 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            PCPGGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 134 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QFGGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 135 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QFNGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 136 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QFCLGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 137 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QFCFGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 138 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ROFGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 139 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ROFOGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 140 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ROFSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 141 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ROFBGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 142 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TROFGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 143 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TROOGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 144 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TROSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 145 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            TROBGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 146 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ROFCGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 147 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ROFNGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 148 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            ROVGGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 149 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            WTRCGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 150 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            WTRSGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 151 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            WTRGGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 152 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            DRGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 153 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            GFLXGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 154 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            HMFGGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 155 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            HTCGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 156 - 
          DO J = 1, IG
          counter = counter + 1
          IF(N.eq.counter) THEN
            QFCGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 157 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            MANNGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 158 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            DDGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 159 - 
          DO J = 1, IG
          counter = counter + 1
          ! make sure that we are using the correct DATAIN
          IF(N.eq.counter) THEN
            ! set the GAT_R variable from the save_state.r2c
            SANDGAT(:,J) = DATAIN
          ENDIF
          ENDDO
       
          !> ATTRIBUTE 160 - 
          DO J = 1, IG
          counter = counter + 1
          ! make sure that we are using the correct DATAIN
          IF(N.eq.counter) THEN
            ! set the GAT_R variable from the save_state.r2c
            CLAYGAT(:,J) = DATAIN
          ENDIF
          ENDDO

          !> ATTRIBUTE 161 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            IGDRGAT = DATAIN
          ENDIF

          !> ATTRIBUTE 162 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            VMODGAT = DATAIN
          ENDIF
          
          !> ATTRIBUTE 163 - 
          counter = counter + 1
          IF(N.eq.counter) THEN
            QLWOGAT = DATAIN
          ENDIF
       
        ENDIF ! IF(GRD_R(N).EQ.1.OR.GAT_R(N).EQ.1)THEN
      ENDDO ! N = 1, NR2C_R

close(263)
DEALLOCATE(DATAOUT)

      END
