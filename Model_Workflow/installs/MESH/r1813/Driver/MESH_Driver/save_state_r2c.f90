      SUBROUTINE SAVE_STATE_R2C(NML,NLTEST,NMTEST,NCOUNT, &
                    IMIN,ACLASS,NR2C,GRD_S,GAT_S,GRDGAT_S,R2C_ATTRIBUTES_S, &
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
!> The resume file contains all of the GAT variables used
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
INTEGER NML,NLTEST,NMTEST,NCOUNT,IMIN,NR2C,NLAT,ILG,IC,ICP1,IG
!INTEGER XXX(NLAT),YYY(NLAT)
INTEGER ILMOS(ILG),JLMOS(ILG),IGDRGAT(ILG)
INTEGER GRD_S(NR2C),GAT_S(NR2C),GRDGAT_S(NR2C)

!REAL    ACLASS(NLTEST,NMTEST+1)
   
INTEGER COUNT,XCOUNT,YCOUNT
REAL    DATAIN(ILG)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: DATAOUT
CHARACTER(*) R2C_ATTRIBUTES_S(NR2C,3)

integer resumeIostat
character(10) ctime,coordsys1,datum1,zone1
character(8)  cday
real xorigin,yorigin,xdelta,ydelta
REAL ACLASS(NLTEST,NMTEST+1)
INTEGER XXX(NLAT),YYY(NLAT)

LOGICAL SAVESTATER2C
INTEGER PAS
INTEGER NR2CSTATES,IOS,DELTR2C,counter

!> *********************************************************************
!> Open and read in values from save_state_r2c.txt file
!> *********************************************************************
NR2CSTATES = 0
   INQUIRE(FILE='save_state_r2c.txt', EXIST = SAVESTATER2C)
   IF(SAVESTATER2C)THEN
      OPEN(57, FILE = 'save_state_r2c.txt')
      READ(57,*,IOSTAT=IOS)NR2C,DELTR2C
      IF(IOS /= 0 .OR. MOD(DELTR2C,30) /= 0)THEN
         WRITE(6,9002)
         STOP

9002 FORMAT('ERROR IN READING save_state_r2c.txt FILE. ',/, &
            'THE FIRST RECORD AT THE FIRST LINE IS FOR THE NUMBER OF ALL THE ', &
            'VARIABLES LISTED IN THE save_state_r2c.txt FILE.',/,&
            'THE SECOND RECORD AT THE FIRST LINE IS TIME STEP FOR R2C OUTPUT. ', &
            'IT SHOULD BE AN INTEGER MULTIPLE OF 30. ',/,&
            'THE REMAINING RECORDS SHOULD CONTAIN 3 COLUMNS FOR EACH VARIABLE WITH INTEGER VALUES OF ', &
            'EITHER 0 OR 1 AND 3 COLUMNS CONTAINING INFORMATION ABOUT THE VARIABLES')

      ENDIF
      
      PRINT*
      PRINT*,'THE FOLLOWING VARIABLES WILL BE SAVED IN AN R2C FILE (save_state.r2c):'

      DO I = 1, NR2C
          READ(57,*,IOSTAT = IOS)GRD_S(I),GAT_S(I),GRDGAT_S(I),(R2C_ATTRIBUTES_S(I,J),J=1,3)
          IF(IOS /= 0)THEN
             PRINT*,'ERROR READING save_state_r2c.txt FILE AT LINE ', I + 1
             STOP
          ELSE
           IF(GRD_S(I)==1)THEN
              NR2CSTATES = NR2CSTATES + 1
              PRINT*,NR2CSTATES,' (GRD_S)    : ',R2C_ATTRIBUTES_S(I,3)
           ENDIF
           IF(GAT_S(I)==1)THEN
              do j=1,nmtest
                 NR2CSTATES = NR2CSTATES + 1
                 PRINT*,NR2CSTATES,' (GAT_S)    : ',R2C_ATTRIBUTES_S(I,3),' CLASS ',j
              enddo
           ENDIF
          ENDIF
      ENDDO
      CLOSE(57)
   ELSE
      PRINT*
      PRINT*,"save_state_r2c.txt FILE DOESN'T EXIST. ", &
             "SAVERESUMEFLAG SHOULD BE SET TO ZERO IF R2C OUTPUTS ARE NOT NEEDED."
      PRINT*
      STOP
   ENDIF

ALLOCATE (DATAOUT(NR2CSTATES,XCOUNT,YCOUNT))

!open output resume file
      OPEN (263, FILE="./save_state.r2c", STATUS="REPLACE", &
           IOSTAT=resumeIostat)
!	  write the header
        write(263,3005)'########################################'
        write(263,3005)':FileType r2c  ASCII  EnSim 1.0         '
        write(263,3005)'#                                       '
	  write(263,3005)'# DataType               2D Rect Cell   '
        write(263,3005)'#                                       '
        write(263,3005)':Application             EnSimHydrologic'
	  write(263,3005)':Version                 2.1.23         '
	  write(263,3021)':WrittenBy               MESH Driver    '
        call date_and_time(cday,ctime)
	  write(263,3010)':CreationDate       ', &
            cday(1:4),cday(5:6),cday(7:8),ctime(1:2),ctime(3:4)
3010  format(a20,a4,'-',a2,'-',a2,2x,a2,':',a2)
        write(263,3005)'#                                       '
	  write(263,3005)'#---------------------------------------'
        write(263,3005)'#                                       '
	  write(263,3021)':Name   All GAT Variables in GRD format'
        write(263,3005)'#                                       '
	  write(263,3004)':Projection         ',coordsys1
	  if(coordsys1.eq.'LATLONG   ')then
	    write(263,3004)':Ellipsoid          ',datum1
	  endif
	  if(coordsys1.eq.'UTM       ')then
	    write(263,3004)':Ellipsoid          ',datum1
          write(263,3004)':Zone               ',zone1
	  endif
        write(263,3005)'#                                       '
        write(263,3003)':xOrigin            ',xorigin
        write(263,3003)':yOrigin            ',yorigin
        write(263,3005)'#                                       '
        write(263,3021)':SourceFile          standalone MESH    '
        write(263,3005)'#                                       '
        counter = 1
        do i = 1,NR2C
        if(GRD_S(I).eq.1) then
          write(263,3024)':AttributeName ',counter,R2C_ATTRIBUTES_S(i,1),' GRID'
          write(263,3023)':AttributeUnits',counter,R2C_ATTRIBUTES_S(i,2)  
          counter = counter + 1
        endif
        if(GAT_S(I).eq.1) then
          do j=1,nmtest
          write(263,3025)':AttributeName ',counter,R2C_ATTRIBUTES_S(i,1),'CLASS',j
          write(263,3023)':AttributeUnits',counter,R2C_ATTRIBUTES_S(i,2)  
          counter = counter + 1
          enddo
        endif
        enddo
        write(263,3005)'#                                       '
        write(263,3001)':xCount             ',xcount
        write(263,3001)':yCount             ',ycount
        write(263,3003)':xDelta             ',xdelta
        write(263,3003)':yDelta             ',ydelta
        write(263,3005)'#                                       '
     	write(263,3005)':endHeader                              '

!        ensure that GRDGAT_S = 0 since we don't want to save or resume GRDGAT_S as a state
         GRDGAT_S = 0
!>COLLECT OUTPUT DATA
      CALL R2C_DATA(DATAIN,DATAOUT,NML,NLTEST,NMTEST,NCOUNT, &
              IMIN,ACLASS,NR2C,GRD_S,GAT_S,GRDGAT_S,NR2CSTATES, &
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
                       HMFGGAT,HTCGAT, QFCGAT,                  &
                       MANNGAT,DDGAT,  SANDGAT,CLAYGAT,IGDRGAT, &
                       VMODGAT,QLWOGAT)
 
      COUNT = 0

      DO N = 1, NR2C
       IF(GRD_S(N) == 1)THEN
         COUNT = COUNT + 1
         DO J = 1, YCOUNT
           WRITE(263,'(999(E10.3))')(DATAOUT(COUNT,I,J),I=1,XCOUNT)
         ENDDO      
       ENDIF
       IF(GAT_S(N) == 1)THEN
          DO K = 1, NMTEST
          COUNT = COUNT + 1
             DO J = 1, YCOUNT
                WRITE(263,'(999(E10.3))')(DATAOUT(COUNT,I,J),I=1,XCOUNT)
             ENDDO
          ENDDO
       ENDIF
      ENDDO

      close(263)
      DEALLOCATE(DATAOUT)
          
      RETURN
 
 1300 format(9999(1x,f5.0))
 1301 format(9999(1x,f12.4))
 3000 format(a10,i5)
 3001 format(a20,i16)
 3002 format(2a20)
 3003 format(a20,f16.7)
 3004 format(a20,a10,2x,a10)
 3005 format(a40)
 3006 format(a3,a10)
 3007 format(a14,i5,a6,i5)
 3012 format(a9)
 3020 format(a20,a40)
 3021 format(a40)
 3022 format(2a15,a5,i5)
 3023 format(a15,i5,1X,a10)
 3024 format(a15,i5,1X,a10,a5)
 3025 format(a15,i5,1X,a10,a5,i5)

      END
