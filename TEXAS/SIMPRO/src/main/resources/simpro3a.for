      SUBROUTINE EXEC
C
C *** ************************************************************** ***
C *** *                                                            * ***
C *** *  COPYRIGHT (C) 1989 by The University of Texas at Austin   * ***
C *** *                                                            * ***
C *** * Permission is hereby granted to use, modify, copy, and     * ***
C *** * distribute this software and its documentation for any     * ***
C *** * purpose only without profit, provided that the above       * ***
C *** * Copyright Notice appears in all copies and that both the   * ***
C *** * Copyright Notice and this Permission Notice appears in     * ***
C *** * every copy of supporting documentation.  No title to nor   * ***
C *** * ownership of the software is transferred hereby.  The name * ***
C *** * of The University of Texas at Austin shall not be used in  * ***
C *** * advertising or publicity related to the distribution of    * ***
C *** * the software without specific, written, prior permission.  * ***
C *** * This software is provided as-delivered without expressed   * ***
C *** * or implied warranty.  The University of Texas at Austin    * ***
C *** * makes no representation about the suitability of this      * ***
C *** * software for any purpose and accepts no responsibility for * ***
C *** * its use.                                                   * ***
C *** *                                                            * ***
C *** ************************************************************** ***
C *** *                                                            * ***
C *** * This program is free software; you can redistribute it     * ***
C *** * and/or modify it under the terms of the GNU General Public * ***
C *** * License as published by the Free Software Foundation;      * ***
C *** * either version 2 of the License, or (at your option) any   * ***
C *** * later version.                                             * ***
C *** *                                                            * ***
C *** * This program is distributed in the hope that it will be    * ***
C *** * useful, but WITHOUT ANY WARRANTY; without even the implied * ***
C *** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ***
C *** * PURPOSE.  See the GNU General Public License for more      * ***
C *** * details.                                                   * ***
C *** *                                                            * ***
C *** * You should have received a copy of the GNU General Public  * ***
C *** * License along with this program; if not, write to the Free * ***
C *** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ***
C *** * Floor, Boston, MA 02110-1301, USA.                         * ***
C *** *                                                            * ***
C *** * For more information: http://www.gnu.org/licenses/gpl.html * ***
C *** *                                                            * ***
C *** ************************************************************** ***
C
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ANIMAT'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LOOPS'
      INCLUDE 'PHASES'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'SUMST2'
C=    INCLUDE 'TESTER'
      INCLUDE 'TITLE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      BYTE              SSAMID
      INTEGER           I,ICAMPL,IPRCNT,ITIM1,ITIM2,ITIM3,J,JSISET(NLA)
C9    INTEGER           ITIM,ITNOW
      REAL*4            SSAMTM
      DOUBLE PRECISION  DTLAGD
      DATA     ICAMPL / 0 /
      DATA     JSISET / NLA*0 /
  501 FORMAT(I5,I2.2,2I3)
  502 FORMAT(40I2)
  998 FORMAT(25(1H*))
  999 FORMAT(50HRAN OUT OF VEHICLES BEFORE END OF SIMULATION TIME ,
     *       10HAT TIME = ,F7.2,8H SECONDS)
C6701 FORMAT('Q'F7.2' (')
C
C-----SUBROUTINE EXEC IS THE MAIN DRIVER FOR SIMPRO AND CONTROLS THE
C-----CALLING OF THE VARIOUS OTHER ROUTINES
C
C[    I          = -2147483647
C[    IPRCNT     = -2147483647
C[    ITIM1      = -2147483647
C[    ITIM2      = -2147483647
C[    ITIM3      = -2147483647
C[    J          = -2147483647
C*    NRNAME = 1
C*    IRNAME(NRNAME) = 'EXEC'
      IQ(0)     = 0
      IQ(NVEP1) = NVEP1
      NEWPDA    = .FALSE.
      NEWPSI    = .FALSE.
      NEWTDA    = .FALSE.
      NEWTSI    = .FALSE.
C-----IF THE INTERSECTION IS TEXAS DIAMOND FIGURE 3, 4, 6, OR 7 SIGNAL
C-----CONTROLLED THEN INITIALIZE THE TEXAS DIAMOND FIGURE 3, 4, 6, OR 7
C-----SIGNAL CONTROLLER
      IF ( ( ICONTR .GE. ICTDF3 ) . AND . ( ICONTR .LE. ICDDF7 ) )
     *                                           CALL TX3467
C-----IF THE INTERSECTION IS NEMA MULTI RING CONTROLLED THEN INITIALIZE
C-----THE NEMA MULTI RING SIGNAL CONTROLLER
      IF ( ( ICONTR .GE. ICNEMA ) . AND . ( ICONTR .LE. ICNEMV ) )
     *                                           CALL NEMA
C-----IF THE INTERSECTION IS HARDWARE-IN-THE-LOOP SIGNAL CONTROLLED THEN
C-----INITIALIZE THE HARDWARE INTERFACE
      IF ( ICONTR . EQ . ICHDWR )                CALL HDWARE
      IPRCNT = NPRCNT
C9    ITIM = IDNINT( TSTATS/DT )
C-----GET TM TIME FOR THIS JOB AT THE END OF INITIALIZATION
C>    CALL  EXTIME  ( 2 )
C-----GET TM TIME FOR THIS JOB AT THE END OF START-UP TIME
C>    CALL  EXTIME  ( 3 )
 1010 CONTINUE
C;    WRITE (TC3,331)
C;331 FORMAT(' TIME                                      '
C;   *       'IA IL  IV    IQ I LATP PV PS NX   NFC '
C;   *       'IPRC 1NORC IPRC 2NORC')
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGES THAT HAVE ENDED
      DO 1011  I = IVMSMB , NVMSM
      IF ( DVMSST(I)+DVMSAT(I) . LT . TIME )     THEN
        IVMSMB = IVMSMB + 1
      END IF
 1011 CONTINUE
C-----SET ALL VEHICLES AS NOT UPDATED THIS DT AND CHECK EMERGENCY
C-----VEHICLE CHECK CONFLICTS
      EVCCON = .FALSE.
      DO 1012  IV = 1 , NVE
      IUPDAT(IV) = .FALSE.
      IF ( IAND( VEHTYP(IV),LAVTE ) .NE. 0 )     EVCCON = .TRUE.
 1012 CONTINUE
C-----COUNT THE TOTAL NUMBER OF VEHICLES IN THE LANE AT THE INTERSECTION
C-----AND THEN INITIALIZE THE NUMBER OF VEHICLES IN THE LANE AT THE
C-----INTERSECTION
      KVILAI = 0
      DO 1014  IAN = 1 , NIBA
      IA = LIBA(IAN)
      DO 1013  ILN = 1 , NLANES(IA)
      KVILAI = KVILAI + NVILAI(ILN,IA)
      NVILAI(ILN,IA) = 0
 1013 CONTINUE
 1014 CONTINUE
C-----SUM THE NUMBER OF VEHICLES IN THE SYSTEM DURING SIMULATION TIME
                    IF ( TIME . GT . STRTIM )    NVSYA = NVSYA + NVSY
      MNVSY = MAX0( MNVSY,NVSY )
C-----GET TM TIME FOR THIS JOB AT THE END OF START-UP TIME
C>                  IF ( TIME . LE . STRTIM )    CALL EXTIME  ( 3 )
C-----IF THE TIME INTO THE SIMULATION IS GT THE SIMULATION TIME THEN END
                    IF ( TIME . GT . SIMTIM )    GO TO 4020
C[    IF ( IPRCNT             .EQ.-2147483647   )STOP 'EXEC   IPRCNT 01'
      IPRCNT = IPRCNT + 1
      IF ( IPRCNT . GE . NPRCNT )                THEN
        IPRCNT = 0
C=      IF ( IPGZ . EQ . 0 )                     THEN
          IF ( ( ICONTR . EQ . ICHDWR ) . AND .
     *         ( DTLAG  . GT . 0      ) )        THEN
            DTLAGD = DBLE( DTLAG ) / 1.0D3
            WRITE (*,'(F6.1,I4,F10.3)') TIME,NVSY,DTLAGD
          ELSE
            WRITE (*,'(F6.1,I4)') TIME,NVSY
          END IF
C=      END IF
      ELSE
        IF ( ( ICONTR . EQ . ICHDWR ) . AND .
     *       ( DTLAG  . GT . 0      ) )          THEN
          DTLAGD = DBLE( DTLAG ) / 1.0D3
          WRITE (*,'(F6.1,4X,F10.3)') TIME,DTLAGD
        END IF
      END IF
      IF ( (IPOLL . EQ . INO   ) . OR .
     *     (TIME  . LT . BEGT20) . OR .
     *     (TIME  . GT . ENDT20) )               GO TO 103
      ITIM1 =    INT(  TIME + 0.005D0      )
      ITIM2 = IDNINT( (TIME-ITIM1)*100.0D0 )
      ITIM3 = POSNON - POSTIM
      NEWTSI = ( ( ICONTR .GE. ICPSIG ) . AND . ( ICAMPC .NE. ICAMPL ) )
                    IF ( NEWTSI )                ITIM3 = ITIM3 - POSTSI
                    IF ( NEWPSI )                ITIM3 = ITIM3 - POSPSI
      WRITE (NPD,501) ITIM1,ITIM2,0,ITIM3
      IF ( NEWTSI )                              THEN
        DO 101  I = 1 , NRLAN
        JSISET(I) = 0
  101   CONTINUE
        DO 102  I = 1 , NIBL
        J = LLANER(I)
        JSISET(J) = ISISET(ICAMPC,I)
  102   CONTINUE
        WRITE (NPD,502) (JSISET(I),I=1,NRLAN)
        IF ( ICONTR . EQ . ICHDWR )              THEN
          WRITE (*,'(F6.1,A,25I3)') TIME," TSI",
     *                              (JSISET(I),I=1,NRLAN)
        END IF
        NEWTSI = .FALSE.
      END IF
      IF ( NEWPSI )                              THEN
        WRITE (NPD,502) (PEDINT(LPHASE(I)),I=1,NPHASE)
        IF ( ICONTR . EQ . ICHDWR )              THEN
          WRITE (*,'(F6.1,A,25I3)') TIME," PSI",
     *                              (PEDINT(LPHASE(I)),I=1,NPHASE)
        END IF
        NEWPSI = .FALSE.
      END IF
  103 CONTINUE
C6    WRITE (IPP,701) TIME
C6    WRITE (IPV,701) TIME
C6    WRITE (IPA,701) TIME
C-----WRITE SURROGATE SAFETY ASSESSMENT METHODOLOGY DATA FOR INTERSECTION
      IF ( ISSAM . EQ . IYES )                   THEN
C-----  WRITE SSAM TIMESTEP RECORD
C-----  SSAMID = RECORD TYPE (2=TIMESTEP)
C-----  SSAMTM = TIME IN SECONDS SINCE THE START OF THE SIMULATION
        SSAMID = 2
        SSAMTM = TIME
        WRITE (ISS) SSAMID,SSAMTM
      END IF
      ICAMPL = ICAMPC
C-----DETERMINE WHICH VEHICLES IN THE QUEUE BUFFERS ARE TO BE LOGGED
C-----INTO THE SYSTEM THIS DT
      CALL  QUEUE
C-----IF THERE ARE NO VEHICLES IN THE SYSTEM AND THERE ARE NO VEHICLES
C-----IN THE QUEUE BUFFERS TO BE LOGGED INTO THE SYSTEM THEN END
                    IF ( NVSY+IQF+IQFVDI.LE.0 )  GO TO 4010
C-----IF THERE ARE NO VEHICLES IN THE SYSTEM BUT THERE ARE VEHICLES
C-----IN THE QUEUE BUFFERS TO BE LOGGED INTO THE SYSTEM THEN GO TO 2010
C-----AND PROCESS ONLY THE INBOUND APPROACHES THIS DT
                    IF ( NVSY . LE . 0 )         GO TO 2020
                    IF ( NVOBA . LE . 0 )        GO TO 1020
C-----PROCESS THE VEHICLES ON THE OUTBOUND APPROACHES
                    IF ( (.NOT. DIAMON) )        GO TO 1015
      CALL  OBAP    ( ILETTR )
 1015 CONTINUE
      CALL  OBAP    ( ILETTL )
 1020 CONTINUE
C-----PROCESS THE VEHICLES ON THE INTERSECTION PATHS
                    IF ( (.NOT. DIAMON) )        GO TO 2010
                    IF ( NVIN . LE . 0 )         GO TO 2000
      CALL  INTERP  ( ILETTI )
 2000 CONTINUE
                    IF ( NVIBA . LE . 0 )        GO TO 2005
      CALL  IBAP    ( ILETTI )
 2005 CONTINUE
                    IF ( NVIN . LE . 0 )         GO TO 2020
      CALL  INTERP  ( ILETTR )
 2010 CONTINUE
                    IF ( NVIN . LE . 0 )         GO TO 2020
      CALL  INTERP  ( ILETTL )
 2020 CONTINUE
                    IF ( NVIBA+IQF+IQFVDI.LE.0 ) GO TO 3010
C-----PROCESS THE VEHICLES ON THE INBOUND APPROACHES AND LOG NEW
C-----VEHICLES INTO THE SYSTEM FROM THE QUEUE BUFFERS AS REQUIRED
                    IF ( (.NOT. DIAMON) )        GO TO 3005
      CALL  IBAP    ( ILETTR )
 3005 CONTINUE
      CALL  IBAP    ( ILETTL )
 3010 CONTINUE
C-----IF THE INTERSECTION IS PRE-TIMED SIGNAL CONTROLLED THEN SIMULATE
C-----THE PRE-TIMED SIGNAL CONTROLLER
                    IF ( ICONTR . EQ . ICPSIG )  CALL PRESIG
C-----IF THE INTERSECTION IS SEMI-ACTUATED OR FULL-ACTUATED SIGNAL
C-----CONTROLLED THEN SIMULATE THE SEMI-ACTUATED OR FULL-ACTUATED SIGNAL
C-----CONTROLLER
      IF ( ( ICONTR .GE. ICSACT) . AND . ( ICONTR .LE. ICFACT ) )
     *                                           CALL ACTSIG
C-----IF THE INTERSECTION IS TEXAS DIAMOND FIGURE 3, 4, 6, OR 7 SIGNAL
C-----CONTROLLED THEN SIMULATE THE TEXAS DIAMOND FIGURE 3, 4, 6, OR 7 SIGNAL
C-----CONTROLLER
      IF ( ( ICONTR .GE. ICTDF3 ) . AND . ( ICONTR .LE. ICDDF7 ) )
     *                                           CALL TX3467
C-----IF THE INTERSECTION IS NEMA MULTI RING CONTROLLED THEN SIMULATE
C-----THE NEMA MULTI RING SIGNAL CONTROLLER
      IF ( ( ICONTR .GE. ICNEMA ) . AND . ( ICONTR .LE. ICNEMV ) )
     *                                           CALL NEMA
C-----IF THE INTERSECTION IS HARDWARE CONTROLLED THEN INTERFACE
C-----WITH EXTERNAL HARDWARE CONTROLLER
      IF ( ICONTR .EQ. ICHDWR )                  CALL HDWARE
C-----IF THE TIME INTO THE SIMULATION IS AN INTEGER MULTIPLE OF THE TIME
C-----INTERVAL FOR INTERMEDIATE STATISTICS THEN PRINT THE INTERMEDIATE
C-----STATISTICS
C9    ITNOW = IDNINT( (TIME-STRTIM)/DT )
C9        IF ( ((ITNOW/ITIM)*ITIM).EQ.ITNOW )    CALL INTSTA
      IF ( (IPOLL . EQ . INO   ) . OR .
     *     (TIME  . LT . BEGT20) . OR .
     *     (TIME  . GT . ENDT20) )               GO TO 104
      ITIM3 = POSNON
                    IF ( NEWTDA )                ITIM3 = ITIM3 - POSTDA
                    IF ( NEWPDA )                ITIM3 = ITIM3 - POSPDA
      IF ( ITIM3 . NE . POSNON )                 THEN
        WRITE (NPD,501) ITIM1,ITIM2,0,ITIM3
        IF ( NEWTDA)                             THEN
          WRITE (NPD,502) (LDSTAT(LLOOPS(I)),I=1,NLOOPS)
          IF ( ICONTR . EQ . ICHDWR )              THEN
            WRITE (*,'(F6.1,A,25I3)') TIME," TDA",
     *                                (LDSTAT(LLOOPS(I)),I=1,NLOOPS)
          END IF
          NEWTDA = .FALSE.
        END IF
        IF ( NEWPDA )                            THEN
          WRITE (NPD,502) (PDSTAT(LPHASE(I)),I=1,NPHASE)
          IF ( ICONTR . EQ . ICHDWR )              THEN
            WRITE (*,'(F6.1,A,25I3)') TIME," PDA",
     *                                (PDSTAT(LPHASE(I)),I=1,NPHASE)
          END IF
          NEWPDA = .FALSE.
        END IF
      END IF
  104 CONTINUE
C-----INCREMENT THE TIME INTO THE SIMULATION AND RECYCLE
      TIME = TIME + DT
      GO TO 1010
 4010 CONTINUE
      WRITE (WRNMSG,999) TIME
      WRITE (6,FMT) CHAR( 12 )
      WRITE (6,998)
      CALL  PRTWRN  ( WRNMSG )
      WRITE (6,998)
 4020 CONTINUE
      IF ( IPRCNT . GT . 0 )                     THEN
        IPRCNT = 0
C=      IF ( IPGZ . EQ . 0 )                     THEN
          WRITE (*,'(F6.1,I4)') TIME-DT,NVSY
C=      END IF
      END IF
      IF ( ICONTR .EQ. ICHDWR )                  CALL  ENDCID
      RETURN
      END                                                               EXEC
C
C
C
      SUBROUTINE OBAP   ( AFLAG )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      BYTE              SSAMID,SSAMLN
      CHARACTER*1       AFLAG
      LOGICAL           IFORCE,LCHNGE,LVMSDD,MFGOFS
CA    LOGICAL           IHPRT
CG    LOGICAL           IAPRT,ILPRT
      INTEGER           IAE,IPE,IPR,IVC,IVE,JL,JP,KV,LAT,LV,NV,NXVEH
C OLD INTEGER           JA,JLN
CA    INTEGER           IDESPD
CE    INTEGER           ICE
CF    INTEGER           IRN
CG    INTEGER           ICG
      INTEGER*4         SSAMLK,SSAMVI
      REAL*4            SSAMVA,SSAMVL,SSAMVS,SSAMVW,SSAMXF,SSAMXR,
     *                  SSAMYF,SSAMYR
      DOUBLE PRECISION  ANGLE,DISEND,DISLCH,DLENV,POSADD,POSNFB,POSNRB,
     *                  POSOFB,POSORB,VELLCH,VEHLNG
C OLD DOUBLE PRECISION  POSLAT,POSR
CA    DOUBLE PRECISION  POSLCA
C7    DOUBLE PRECISION  POSLC7
  501 FORMAT(I3,I4,2I3,I1,I3,I6,I2,I1,I3)
CA701 FORMAT(/,12H OUTBOUND ( ,A1,5H ) AT,F8.2,4H SEC,/,
CA   *       49H AP LN VEH  NUM NOF NOR NRC VEHPOS VEHVEL VEHACC ,
CA   *       46HACCSLP DS VC DC NX OA ST LG LF LC PR LPOS   SG)
C7702 FORMAT(F7.2,5I4,2F7.1)
CA703 FORMAT(2I3,I4,I6,2I4,2(I3,I4),2F7.2,2F7.3,10I3,F5.1,I5,3(1X,A10))
CF704 FORMAT(18(1X,A6))
CE751 FORMAT(8H APPRO  ,I3,1X,A1,1X,39I3)
CE753 FORMAT(8H LANE   ,I3,1X,28I4)
CE756 FORMAT(8H VEHD   ,I3,4F6.0,3I2,2I3,2I4,F6.0,2I4,I3,2I4,F4.0,F6.0,
CE   *                  I2,2(I3,I4),I3,1X,13L1,1X,7L1,/,8X,
CE   *                  21HCONTINUE W/ INTERNALS,18X,2I4,F6.0,2I4,3X,
CE   *                  2I4,4X,F6.0,L2,I3,L2,I6)
CE757 FORMAT(8H VEHF   ,I3,1X,10I4,F4.2,4I4,F4.2)
C
C-----SUBROUTINE OBAP PROCESSES THE VEHICLES ON THE OUTBOUND APPROACHES
C
C-----IPR VALUES
C-----0 FIRST DT OF A LANE CHANGE TO THE RIGHT THUS DO NOT WRITE VEHICLE
C-----1 NORMAL
C-----2 LOGIN
C-----3 LOGIBI
C-----4 LOGIOB OR LOGIIN
C-----5 LOGOUT
C-----6 LANE CHANGE
C
C-----LVMSDD = DISTRACTED DRIVER VMS MESSAGE WAS CANCELLED OR TIMED OUT
C
C[    IAE        = -2147483647
C[    IPE        = -2147483647
C[    IPR        = -2147483647
C[    IVE        = -2147483647
C[    LAT        = -2147483647
C[    NV         = -2147483647
C[    NXVEH      = -2147483647
C[    DISEND     = -2147483647.0
C[    DISLCH     = -2147483647.0
C*    NRNAME = 1
C*    IRNAME(NRNAME) = 'OBAP'
CA    IHPRT = .FALSE.
      IGO = 0
C-----PROCESS EACH OUTBOUND APPROACH
      DO 6010  IAN = 1 , NOBA
      IA = LOBA(IAN)
            IF ( IAFLAG(IA) . NE . AFLAG )       GO TO 6010
C-----IF THERE ARE NO VEHICLES ON THIS OUTBOUND APPROACH THEN SKIP TO
C-----THE NEXT OUTBOUND APPROACH
                    IF ( NVIA(IA) . LE . 0 )     GO TO 6010
CA                  IF ( IHPRT )                 GO TO 101
CC                  IF ( TIME . LT . TPRINT )    GO TO 101
CA    WRITE (6,701) AFLAG,TIME
CA    IHPRT = .TRUE.
CA101 CONTINUE
CG    IAPRT = .FALSE.
CG                  IF ( (.NOT. IAPRT) )         GO TO 102
CE                  IF ( TIME . LT . TPRINT )    GO TO 102
CE    WRITE (6,751) IA,IAFLAG(IA),NLANES(IA),ISLIM(IA),IALEFT(IA),
CE   *              NSDR(IA),(LLANES(ICE,IA),ICE=1,NAL),
CE   *              (NVIL(ICE,IA),ICE=1,NAL),
CE   *              (ISDRN(ICE,IA),ICE=1,NIA-1),
CE   *              (ISDRA(ICE,IA),ICE=1,NIA-1)
CE102 CONTINUE
C-----PROCESS EACH LANE ON THE OUTBOUND APPROACH
      DO 5010  ILN = 1 , NLANES(IA)
C-----IF THERE ARE NO VEHICLES IN THIS LANE THEN SKIP TO THE NEXT LANE
                    IF ( NVIL(ILN,IA) . LE . 0 ) GO TO 5010
      IL = LLANES(ILN,IA)
CG    ILPRT = .FALSE.
CG                  IF ( (.NOT. ILPRT) )         GO TO 103
CE                  IF ( TIME . LT . TPRINT )    GO TO 103
CE    WRITE (6,753) ILN,LWID(IL),NLL(IL),NLR(IL),ISNA(IL),NPINT(IL),
CE   *              IFVL(IL),ILVL(IL),LCONTR(IL),LTURN(IL),NLDL(IL),
CE   *              IBLN(IL),(LINTP(ICE,IL),ICE=1,NLP),
CE   *              (LLDL(ICE,IL),ICE=1,NLO),(LGEOM(ICE,IL),ICE=1,4)
CE103 CONTINUE
C-----SET THE INDEX FOR THE FIRST VEHICLE ON THE LANE
      IV    = IFVL(IL)
      NV    = NVIL(ILN,IA)
      NCQ   = 0
      IVPV  = 0
      PVPOS = POSBIG
      PVVEL = ISLIM(IA)
      PVACC = 0.0
      PVSLP = 0.0
C-----PROCESS EACH VEHICLE ON THE LANE
      DO 4010  IVN = 1 , NV
      IPR = 1
C*    NRNAME = 1
      ENDLN  = POSBIG
      LCHNGE = .FALSE.
      JP     = LPREV(IV)
C-----RESET THE PREVIOUS VEHICLE PARAMETERS TO THE NEW NOF IF THE
C-----VEHICLE IS LANE CHANGING, AND INITIALIZE SEVERAL PARAMETERS
C-----FOR THE VEHICLE
      CALL  PREST1  ( .FALSE.,LVMSDD )
                    IF ( (.NOT. MFINL(IV)) )     GO TO 1010
C-----THIS VEHICLE IS THE FIRST VEHICLE IN THE LANE THUS RESET THE
C-----PREVIOUS VEHICLE PARAMETERS
C-----IF THE LANE IS BLOCKED THEN (1) SET THE FLAG TO DISCONTINUE ANY
C-----CALCULATED DECELERATION TO A STOP, (2) SET THE DISTANCE FOR A
C-----LANE CHANGE, (3) SET THE DISTANCE FROM THE END OF THE BLOCKED LANE
C-----EQUAL TO ONE-HALF THE REMAINING DISTANCE TO THE END OF THE BLOCKED
C-----LANE WITH A MINIMUM OF DISLCH AND A MAXIMUM OF ONE-HALF THE LENGTH
C-----OF THE BLOCKED LANE, AND (4) SET THE POSITION OF THE PREVIOUS
C-----VEHICLE (WHERE THE CURRENT VEHICLE SHOULD STOP) EQUAL TO THE
C-----MAXIMUM OF ENDLN-DISEND AND POSNEW ELSE SET THE POSITION OF THE
C-----PREVIOUS VEHICLE EQUAL TO ENDLN
      IF ( IVPV . EQ . 0 )                       THEN
        PVVEL = 0.0
        PVACC = 0.0
        PVSLP = 0.0
        IF ( MBLOCK(IV) )                        THEN
          ENDLN  = DBLE( LGEOM(2,IL) )
          MSFLG(IV) = .FALSE.
          CALL  UNBIAS
          VELLCH = 0.2D0*DBLE( ISPD(IV) )
          VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
          VEHLNG = DMIN1( 25.0D0,LENVAP )
          DISLCH = 0.5D0*(ENDLN-POSNEW)
          DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
          DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
          IF ( MAJRLC(IV) )                      THEN
            DISLCH = 0.5D0*XRELMI
          END IF
          DISEND = DMAX1(DISLCH,DMIN1(0.5D0*ENDLN,0.5D0*(ENDLN-POSNEW)))
          PVPOS  = DMAX1(ENDLN-DISEND,POSNEW)
        ELSE
          PVPOS = ENDLN
          PVVEL = DBLE( ISPD(IV) )
          PVACC = 0.0
          PVSLP = 0.0
        END IF
        IF ( MFSTPF(IV) )                        THEN
          IF ( FSTACT(IV) )                      THEN
            IF ( FSTPOS(IV) . LT . PVPOS )       PVPOS = FSTPOS(IV)
          END IF
          IF ( VMSASM(IV) . GT . 0 )             THEN
            IF ( (IVMSMG(VMSASM(IV)).EQ.VMSMSI) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSL) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSM) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSC) )THEN
              IF ( VMSPST(IV) . LT . PVPOS )     PVPOS = VMSPST(IV)
            END IF
          END IF
        END IF
      END IF
 1010 CONTINUE
      MFGOFS = MFGOF(IV)
C-----IF THIS VEHICLE IS TRYING TO REVERSE A LANE CHANGE THEN SET
C-----FORCED GO FLAG
      IF ( MAJRLC(IV) )                          THEN
        IF ( (PVPOS-IPOS(IV)).GE.-0.5D0*XRELMI ) THEN
          MFGOF(IV) = .TRUE.
        END IF
      END IF
C-----COMPUTE NEW ACC/DEC LOGIC FOR THE VEHICLE
      CALL  PREST2
      MFGOF(IV) = MFGOFS
CG          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 107
CE                  IF ( TIME . LT . TPRINT )    GO TO 107
CA                  IF ( IHPRT )                 GO TO 104
CA    WRITE (6,701) AFLAG,TIME
CA    IHPRT = .TRUE.
CA104 CONTINUE
CG                  IF ( IAPRT )                 GO TO 105
CG    WRITE (6,751) IA,IAFLAG(IA),NLANES(IA),ISLIM(IA),IALEFT(IA),
CG   *              NSDR(IA),(LLANES(ICG,IA),ICG=1,NAL),
CG   *              (NVIL(ICG,IA),ICG=1,NAL),
CG   *              (ISDRN(ICG,IA),ICG=1,NIA-1),
CG   *              (ISDRA(ICG,IA),ICG=1,NIA-1)
CG    IAPRT = .TRUE.
CG105 CONTINUE
CG                  IF ( ILPRT )                 GO TO 106
CG    WRITE (6,753) ILN,LWID(IL),NLL(IL),NLR(IL),ISNA(IL),NPINT(IL),
CG   *              IFVL(IL),ILVL(IL),LCONTR(IL),LTURN(IL),NLDL(IL),
CG   *              IBLN(IL),(LINTP(ICG,IL),ICG=1,NLP),
CG   *              (LLDL(ICG,IL),ICG=1,NLO),(LGEOM(ICG,IL),ICG=1,4)
CG    ILPRT = .TRUE.
CG106 CONTINUE
CE    WRITE (6,757) IV,IDRICL(IV),IVEHCL(IV),ISPD  (IV),NOF   (IV),
CE   *                 NOR   (IV),LNEXT (IV),LPRES (IV),ITURN (IV),
CE   *                 IBAPS (IV),IPRTLO(IV),IEXTIM(IV),NOBAPD(IV),
CE   *                 INT2P (IV),INT2S (IV),INT1T (IV),IEXTII(IV)
CE    WRITE (6,756) IV,ISLP  (IV),IACC  (IV),IVEL  (IV),IPOS  (IV),
CE   *                 ISET  (IV),LCHGE (IV),ISPDP (IV),LEGAL (IV),
CE   *                 IPRTM (IV),ITIMV (IV),IQDS  (IV),ISPDS (IV),
CE   *                 ISDS  (IV),IDVS  (IV),ISTCON(IV),IVMAXA(IV),
CE   *                 IVMAXD(IV),LATPOS(IV),IDTS  (IV),LALT  (IV),
CE   *                 IPRC(1,IV),NORC(1,IV),IPRC(2,IV),NORC(2,IV),
CE   *                 LOGFLG(IV),MBLOCK(IV),MFGOF (IV),MFINL (IV),
CE   *                 MFSTPF(IV),MLAG  (IV),MOASF (IV),MPOBS (IV),
CE   *                 MPRO  (IV),MSAOR (IV),MSFLG (IV),MSTPF (IV),
CE   *                 MTCARS(IV),MININT(IV),IACDS (IV),IACLDS(IV),
CE   *                 ICDFS (IV),IFVA  (IV),IRSTOP(IV),ISDEC (IV),
CE   *                 ISTMO (IV),ITIMVI(IV),IQDSI (IV),ISPDSI(IV),
CE   *                 ISDSI (IV),IDVSI (IV),IVMXAI(IV),IVMXDI(IV),
CE   *                 IDTSI (IV),IUPDAT(IV),JVCNOR(IV),IDISPD(IV),
CE   *                 IQ(IV)
CE107 CONTINUE
 2010 CONTINUE
C-----UNBIAS THE VEHICLE ATTRIBUTES AND PREDICT THE NEW POS/VEL/ACC
      CALL  UNBIAS
      NXVEH = NOR(IV)
                    IF ( ISET(IV) . NE . 1 )     GO TO 2020
      IPR = 6
C-----COMPUTE THE NEW LATERAL POSITION FOR A LANE CHANGE USING A COSINE
C-----CURVE AND IF FINISHED THEN END THE LANE CHANGE
      CALL  LCHGEO
      LCHNGE = .TRUE.
 2020 CONTINUE
C-----IF THE VEHICLES REAR BUMPER IS ON THE OUTBOUND LANE THEN ALLOW
C-----LANE CHANGES
      IF ( ISET(IV) . EQ . 6 )                   THEN
        IF ( ( LENVAP . GT . (0.5D0*ENDLN)                ) . OR .
     *       ( (POSNEW-DBLE( LGEOM(1,IL) )) . GE . LENVAP ) )
     *                                           THEN
          ISET(IV) = 5
        END IF
      END IF
C-----IF THE OUTBOUND LANE IS NOT BLOCKED THEN DO NOT ALLOW LANE CANGES
      IF ( ( ISET(IV) . NE . 1 ) . AND .
     *     ( .NOT. MBLOCK(IV)  ) )               THEN
        ISET(IV) = 6
      END IF
C-----IF EMERGENCY VEHICLE THEN ALLOW LANE CHANGES
      IF ( ( ISET(IV)                 .EQ. 6 ) . AND .
     *     ( IAND( VEHTYP(IV),LAVTE ) .NE. 0 ) ) THEN
        ISET(IV) = 5
      END IF
                    IF ( LCHNGE )                GO TO 2055
                    IF ( MAJCLC )                GO TO 2040
C-----PROCESS VEHICLE MESSAGE SYSTEM CHANGE LANE MESSAGE
      IF ( VMSACM(IV) . GT . 0 )                 THEN
        IF ( ( IVMSMG(VMSACM(IV)) .EQ. VMSMCL ) . OR .
     *       ( IVMSMG(VMSACM(IV)) .EQ. VMSMCR ) )THEN
          IF ( ISET(IV) . NE . 1 )               GO TO 2040
        END IF
      END IF
                    IF ( ISET(IV) . LE . 1 )     GO TO 2055
                    IF ( ISET(IV) . GE . 6 )     GO TO 2050
 2030 CONTINUE
 2040 CONTINUE
C-----DO NOT LET A LANE CHANGE START UNTIL THE VEHICLES REAR BUMPER HAS
C-----CLEARED ALL INTERSECTION CONFLICTS AND IS ON THE OUTBOUND LANE
      IF ( JP . GT . 0 )                         THEN
        IF ( ISTCON(IV) . LE . NGEOCP(JP) )      GO TO 2055
        IF ( ( LENVAP . LE . (0.5D0*ENDLN)                ) . AND .
     *       ( (POSNEW-DBLE( LGEOM(1,IL) )) . LT . LENVAP ) )
     *                                           GO TO 2055
      END IF
C-----DETERMINE IF A LANE CHANGE CLOSE TO THE INTERSECTION IS DESIRABLE
      CALL  LCHDES  ( .FALSE.,.FALSE. )
                    IF ( ISET(IV) . EQ . 1 )     IPR = 6
      IF ( LALT(IV) . EQ . 6 )                   THEN
        IPR = 0
        LALT(IV) = 5
C3      JPFLAG = 'START LCHG'
C3      KPFLAG = ' RIGHT'
        GO TO 3020
      END IF
 2050 CONTINUE
                    IF ( ISET(IV) . LE . 1 )     GO TO 2055
C-----DO NOT LET A LANE CHANGE START UNTIL THE VEHICLES REAR BUMPER HAS
C-----CLEARED ALL INTERSECTION CONFLICTS AND IS ON THE OUTBOUND LANE
      IF ( JP . GT . 0 )                         THEN
        IF ( ISTCON(IV) . LE . NGEOCP(JP) )      GO TO 2055
        IF ( ( LENVAP . LE . (0.5D0*ENDLN)                ) . AND .
     *       ( (POSNEW-DBLE( LGEOM(1,IL) )) . LT . LENVAP ) )
     *                                           GO TO 2055
      END IF
C-----DETERMINE IF A LANE CHANGE FROM BEHIND A SLOW VEHICLE IS DESIRABLE
      CALL  LCHDES  ( .TRUE.,.TRUE. )
                    IF ( ISET(IV) . EQ . 1 )     IPR = 6
      IF ( LALT(IV) . EQ . 6 )                   THEN
        IPR = 0
        LALT(IV) = 5
C3      JPFLAG = 'START LCHG'
C3      KPFLAG = ' RIGHT'
        GO TO 3020
      END IF
 2055 CONTINUE
C-----CHECK THE ACC/DEC LOGICAL DEPENDENT ATTRIBUTES, CALL THE
C-----APPROPRIATE ACC/DEC ROUTINES, AND COMPUTE THE VEHICLES NEW POS/
C-----VEL/ACC
      CALL  ACDCP   ( .FALSE. )
C-----IF THE VEHICLES REAR BUMPER IS STILL ON THE INTERSECTION PATH THEN
C-----CLEAR INTERSECTION CONFLICTS
      IF ( JP . GT . 0 )                         THEN
        IF ( ISTCON(IV) . LE . NGEOCP(JP) )      THEN
          POSORB = POSOLD - DBLE( LGEOM(1,IL) ) - LENVAP
          IF ( POSORB . LE . 0.1D0 )             THEN
            POSNRB = POSNEW - DBLE( LGEOM(1,IL) ) - LENVAP
C-----      IF THE VEHICLES REAR BUMPER IS CROSSING FROM THE INTERSECTION
C-----      PATH TO THE OUTBOUND LANE THIS DT THEN FORCE CLEARING OF ALL
C-----      INTERSECTION CONFLICTS
            IFORCE = ((POSNRB+0.1D0) . GE . 0.0D0)
          ELSE
            IFORCE = .TRUE.
          END IF
          POSNFB = POSNEW - DBLE( LGEOM(1,IL) ) + DBLE( LENP(JP) )
          CALL  CLRCON ( JP,POSNFB,IFORCE )
        END IF
      END IF
C7    IF ( ISET(IV) . EQ . 1 )                   THEN
C7      POSLC7 = LATPOS(IV)
C7    ELSE
C7      POSLC7 = 0.0D0
C7    END IF
C7    WRITE (PPP,702) TIME,IQ(IV),1,IA,IL,IVEHCL(IV),POSNEW,POSLC7
CI          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 108
C-----PRINT POS/VEL/ACC FOR THE VEHICLE
CH    CALL  PVAPRT
CI108 CONTINUE
      IF ( PVPOS+XRELMI . LE . POSNEW )          THEN
C-----  PRINT THE COLLISION INFORMATION AND RESET THE VEHICLES
C-----  POS/VEL/ACC
        CALL  BANGS  ( 3,IVPV,RELVEL,RELPOS,0.0D0 )
      END IF
C-----IF THE VEHICLE LEFT THE OUTBOUND APPROACH THEN GO TO 3030 AND LOG
C-----THE VEHICLE OUT OF THE SYSTEM
      IF ( POSNEW . GT . DBLE( LGEOM(4,IL) ) )   GO TO 3030
                    IF ( ICONTR . LT . ICSACT )  GO TO 2080
                    IF ( JP . LE . 0 )           GO TO 2080
      JL = LIBL(JP)
                    IF ( JL . LE . 0 )           GO TO 2080
                    IF ( NLDL(JL) . LE . 0 )     GO TO 2080
C-----CHECK IF REAR BUMPER STILL ON INBOUND LANE
      POSORB = POSOLD - DBLE( LGEOM(1,IL)-LENP(JP) ) - LENVAP
                    IF ( POSORB . GE . 0.0D0 )   GO TO 2080
      POSOFB = POSOLD - DBLE( LGEOM(1,IL)-LENP(JP)-LGEOM(4,JL) )
      POSNFB = POSNEW - DBLE( LGEOM(1,IL)-LENP(JP)-LGEOM(4,JL) )
C-----CHECK EACH DETECTOR FOR THIS LANE TO SEE IF THIS VEHICLE TRIPPED
C-----ANY OF THEM THIS DT
      CALL  CHKLDT  ( JL,POSOFB,POSNFB )
 2080 CONTINUE
C-----UPDATE THE VEHICLES SIMULATION STATISTICS ON THE OUTBOUND APPROACH
      CALL  SSOBAP
 3010 CONTINUE
C-----BIAS THE VEHICLE ATTRIBUTES, SET THE PREVIOUS VEHICLE PARAMETERS,
C-----AND UPDATE THE MAXIMUM ACC/DEC FOR THE VEHICLE
      CALL  BIAS    ( IPR )
 3020 CONTINUE
C-----PRINT SELECTED ATTRIBUTES FOR THE VEHICLE
                    IF ( JPRTM . GT . 0 )        IPRTM(IV) = JPRTM
CD          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 109
CC                  IF ( TIME . LT . TPRINT )    GO TO 109
C3                  IF ( JPRTM . GT . 0 )        JPFLAG = 'PIJR TIME '
CA    IDESPD = IDNINT( DESVEL )
CA    IF ( ISET(IV) . EQ . 1 )                   THEN
CA      POSLCA = LATPOS(IV)
CA    ELSE
CA      POSLCA = 0.0D0
CA    END IF
CA    WRITE (6,703) IA,ILN,IV,IQ(IV),NOF(IV),NOR(IV),IPRC(1,IV),
CA   *              NORC(1,IV),IPRC(2,IV),NORC(2,IV),POSNEW,VELNEW,
CA   *              ACCNEW,SLPNEW,IDESPD,IVEHCL(IV),IDRICL(IV),
CA   *              LNEXT(IV),NOBAPD(IV),ISET(IV),LEGAL(IV),LOGFLG(IV),
CA   *              LCHGE(IV),IPRTM(IV),POSLCA,0,IPFLAG,JPFLAG,KPFLAG
CC109 CONTINUE
      GO TO 3040
 3030 CONTINUE
C-----ADD THE VEHICLES SIMULATION STATISTICS FOR THE INBOUND APPROACH
C-----AND TURN CODE AND LOG THE VEHICLE OUT OF THE SYSTEM, THE OUTBOUND
C-----APPROACH, AND THE OUTBOUND LANE
      CALL  LOGOUT
      IPR = 5
C-----IF THE VEHICLES REAR BUMPER IS STILL ON THE INTERSECTION PATH THEN
C-----UNSET INTERSECTION CONFLICTS
      IF ( JP . GT . 0 )                         THEN
        IF ( ISTCON(IV) . LE . NGEOCP(JP) )      THEN
          CALL  UNSETC
        END IF
      END IF
 3040 CONTINUE
C-----SET VEHICLE HEADING
      ANGLE = DBLE( IAAZIM(IA) ) + STEERA(IV)
                    IF ( ANGLE . LT .   0.0D0 )  ANGLE = ANGLE+360.0D0
                    IF ( ANGLE . GE . 360.0D0 )  ANGLE = ANGLE-360.0D0
      HEADNG(IV) = ANGLE
      STEERA(IV) = 0.0D0
      IF ( MAJCOL(IV) .AND. ( IPR . NE . 5 ) )   THEN
C-----  CHECK CLEARING PREVIOUS PATH MAJOR COLLISION
        DLENV = LENVAP + DBLE( LGEOM(1,IL) )
        IF ( ( POSOLD    . LE . DLENV ) . AND .
     *       ( POSNEW    . GT . DLENV ) . AND .
     *       ( LPREV(IV) . GT . 0     ) )        THEN
C-----    FIND THE FIRST COLLISION VEHICLE IN THE PREVIOUS PATH STARTING
C-----    FROM THE FIRST VEHICLE IN THE PREVIOUS PATH
          CALL  FFCVLP  ( .FALSE.,LPREV(IV),.TRUE.,KV,POSADD )
          IF ( KV . EQ . 0 )                     THEN
            PMJCOL(LPREV(IV)) = .FALSE.
          END IF
        END IF
      END IF
CG          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 110
CF                  IF ( TIME . LT . TPRINT )    GO TO 110
CF    WRITE (6,704) (IRNAME(IRN),IRN=1,NRNAME)
CF110 CONTINUE
C[    IF ( IPR                .EQ.-2147483647   )STOP 'OBAP   IPR    01'
      IF ( (IPR   . EQ . 0     ) . OR .
     *     (IPOLL . EQ . INO   ) . OR .
     *     (TIME  . LT . BEGT20) . OR .
     *     (TIME  . GT . ENDT20) )               GO TO 112
      IPE = IDNINT( POSNEW )
                    IF ( IPE . GT . 9999 )       IPE = 9999 - IPE
C-----SET ANIMATION VEHICLE CODE
C-----ICNONE   0 VEHICLE CODE - NONE
      IVC = ICNONE
C-----ICYLTS   1 VEHICLE CODE - YELLOW LEFT TURN SIGNALS FRONT AND REAR
C     IF ( ( ITURN(IV) . EQ . ITURNU ) . OR .
C    *     ( ITURN(IV) . EQ . ITURNL )       )   IVC = IVC + ICYLTS
C-----ICYRTS   2 VEHICLE CODE - YELLOW RIGHT TURN SIGNALS FRONT AND REAR
C     IF (   ITURN(IV) . EQ . ITURNR         )   IVC = IVC + ICYRTS
C-----ICRBRK   4 VEHICLE CODE - RED BRAKE LIGHTS IN REAR
      IF ( ( ACCNEW . LE . DECBRK ) . OR .
     *     ( VELNEW . EQ . 0.0D0  )          )   IVC = IVC + ICRBRK
C-----ICVBMC   8 VEHICLE CODE - VEHICLE BLOCKED BY A MAJOR COLLISION
      IF ( MAJCLB(IV) . OR . MAJCLL(IV)      )   IVC = IVC + ICVBMC
C-----ICVIMC  16 VEHICLE CODE - VEHICLE INVOLVED IN A MAJOR COLLISION
      IF ( MAJCOL(IV)                        )   IVC = IVC + ICVIMC
C-----ICEVRC  32 VEHICLE CODE - EMERGENCY VEHICLE RUNNING A CALL
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   IVC = IVC + ICEVRC
C-----ICVREV  64 VEHICLE CODE - VEHICLE REACTING TO EMERG VEH RUN A CALL
      IF ( RESPEV                            )   IVC = IVC + ICVREV
C-----ICRVMS 128 VEHICLE CODE - VEHICLE REACTING TO VMS MESSAGE
      IF ( ( VMSACM(IV) . GT . 0 ) . OR .
     *     ( VMSASM(IV) . GT . 0 )           )   IVC = IVC + ICRVMS
      IVE = IDNINT( VELNEW )
      IF ( ISET(IV) . EQ . 1 )                   THEN
        LAT = IDNINT( LATPOS(IV)*100.0D0 )
      ELSE
        LAT = 0
      END IF
      IAE = IDNINT( ACCNEW )
                    IF ( IPR . EQ . 1 )          GO TO 111
      IF ( IPOLL . EQ . IANI )                   THEN
C[      IF ( IPE              .EQ.-2147483647   )STOP 'OBAP   IPE    01'
C[      IF ( IPR              .EQ.-2147483647   )STOP 'OBAP   IPR    02'
C[      IF ( IVE              .EQ.-2147483647   )STOP 'OBAP   IVE    01'
C[      IF ( LAT              .EQ.-2147483647   )STOP 'OBAP   LAT    01'
        WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT
      ELSE
C[      IF ( IAE              .EQ.-2147483647   )STOP 'OBAP   IAE    01'
C[      IF ( IPE              .EQ.-2147483647   )STOP 'OBAP   IPE    02'
C[      IF ( IPR              .EQ.-2147483647   )STOP 'OBAP   IPR    03'
C[      IF ( IVE              .EQ.-2147483647   )STOP 'OBAP   IVE    02'
C[      IF ( LAT              .EQ.-2147483647   )STOP 'OBAP   LAT    02'
        WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT,IVEHCL(IV),
     *                  IPRTLO(IV),IAE
      END IF
      GO TO 112
  111 CONTINUE
      IF ( IPOLL . EQ . IANI )                   THEN
C[      IF ( IPE              .EQ.-2147483647   )STOP 'OBAP   IPE    03'
        WRITE (NPD,501) IV,IPE,IVC
      ELSE
C[      IF ( IAE              .EQ.-2147483647   )STOP 'OBAP   IAE    02'
C[      IF ( IPE              .EQ.-2147483647   )STOP 'OBAP   IPE    04'
C[      IF ( IPR              .EQ.-2147483647   )STOP 'OBAP   IPR    04'
C[      IF ( IVE              .EQ.-2147483647   )STOP 'OBAP   IVE    03'
C[      IF ( LAT              .EQ.-2147483647   )STOP 'OBAP   LAT    03'
        WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT,IVEHCL(IV),
     *                  IPRTLO(IV),IAE
      END IF
  112 CONTINUE
C-----WRITE SURROGATE SAFETY ASSESSMENT METHODOLOGY DATA FOR OBAP
      IF ( ( ISSAM . EQ . IYES ) . AND .
     *     ( IPR   . GT . 0    ) . AND .
     *     ( IPR   . NE . 5    ) )               THEN
        SSAMXF = VEHCLE(IV)%FBX
        SSAMYF = VEHCLE(IV)%FBY
        SSAMXR = VEHCLE(IV)%UNIT(VEHCLE(IV)%UNITS)%HPX
        SSAMYR = VEHCLE(IV)%UNIT(VEHCLE(IV)%UNITS)%HPY
C OLD   IF ( ISET(IV) . EQ . 1 )                 THEN
C OLD     POSLAT = LATPOS(IV)
C OLD   ELSE
C OLD     POSLAT = 0.0
C OLD   END IF
C OLD   CALL FNDXYA ( IA,ILN,POSNEW,POSLAT,SSAMXF,SSAMYF )
C OLD   POSR = POSNEW - LENVAP
C OLD   IF ( POSR . GE . DBLE( LGEOM(1,IL) ) )   THEN
C OLD     CALL FNDXYA ( IA,ILN,POSR,POSLAT,SSAMXR,SSAMYR )
C OLD   ELSE
C OLD     POSR = POSR - DBLE( LGEOM(1,IL) ) + DBLE( LENP(LPREV(IV)) )
C OLD     IF ( POSR . GE . 0.0D0 )               THEN
C OLD       CALL FNDXYP ( LPREV(IV),POSR,SSAMXR,SSAMYR )
C OLD     ELSE
C OLD       JL   = LIBL(LPREV(IV))
C OLD       JA   = ISNA(JL)
C OLD       JLN  = ISNL(JL)
C OLD       POSR = POSR + DBLE( LGEOM(4,JL) )
C OLD       IF ( POSR . LT . 0.0D0 )             POSR = 0.0D0
C OLD       CALL FNDXYA ( JA,JLN,POSR,0.0D0,SSAMXR,SSAMYR )
C OLD     END IF
C OLD   END IF
C-----  WRITE SSAM VEHICLE RECORD
C-----  SSAMID = RECORD TYPE (3=VEHICLE)
C-----  SSAMVI = VEHICLE ID
C-----  SSAMLK = LINK ID
C-----  SSAMLN = LANE ID (LEFT TO RIGHT STARTING AT 1)
C-----  SSAMVL = LENV(IVEHCL(IV))
C-----  SSAMXF = X COORDINATE OF MIDDLE OF FRONT BUMPER
C-----  SSAMYF = Y COORDINATE OF MIDDLE OF FRONT BUMPER
C-----  SSAMXR = X COORDINATE OF MIDDLE OF REAR  BUMPER
C-----  SSAMYR = Y COORDINATE OF MIDDLE OF REAR  BUMPER
C-----  SSAMVL = VEHICLE LENGTH (FRONT TO BACK)
C-----  SSAMVW = VEHICLE WIDTH  (LEFT TO RIGHT)
C-----  SSAMVS = VEHICLE SPEED (UNITS/SEC)
C-----  SSAMVA = VEHICLE ACCELERATION (UNITS/SEC/SEC)
        SSAMID = 3
        SSAMVI = IQ(IV)
        SSAMLK = IA
        SSAMLN = ILN
        SSAMVL = LENV(IVEHCL(IV))
        SSAMVW = WIDV(IVEHCL(IV))
        SSAMVS = VELNEW
        SSAMVA = ACCNEW
        WRITE (ISS) SSAMID,SSAMVI,SSAMLK,SSAMLN,
     *              SSAMXF,SSAMYF,SSAMXR,SSAMYR,
     *              SSAMVL,SSAMVW,SSAMVS,SSAMVA
      END IF
      NCQ = NCQ + MAX0( IDNINT( LENVAP/20.0D0 ),1 )
      IF ( IPR . EQ . 5 )                        THEN
C-----  VEHICLE LOGGED OUT
        IF ( ISET(IV) . EQ . 1 )                 THEN
C-----    END THE LANE CHANGE AND RESET THE LANE CHANGE FLAGS
          CALL  ENDLCH
        END IF
C-----  CLEAR IVCNOF/JVCNOF AND IVCNOR/JVCNOR
        LV = IVCNOF(IV)
        IF ( LV . GT . 0 )                       THEN
          IF ( JVCNOF(LV) . EQ . IV )            THEN
            JVCNOF(LV) = 0
          END IF
        END IF
        LV = JVCNOF(IV)
        IF ( LV . GT . 0 )                       THEN
          IF ( IVCNOF(LV) . EQ . IV )            THEN
            IVCNOF(LV) = 0
          END IF
        END IF
        LV = IVCNOR(IV)
        IF ( LV . GT . 0 )                       THEN
          IF ( JVCNOR(LV) . EQ . IV )            THEN
            JVCNOR(LV) = 0
          END IF
        END IF
        LV = JVCNOR(IV)
        IF ( LV . GT . 0 )                       THEN
          IF ( IVCNOR(LV) . EQ . IV )            THEN
            IVCNOR(LV) = 0
          END IF
        END IF
C-----  INITIALIZE ALL VEHICLE IV ATTRIBUTES
        CALL  INIVEH  ( IV )
      END IF
C-----SET THE INDEX FOR THE NEXT VEHICLE TO BE PROCESSED FOR THIS LANE
C[    IF ( NXVEH              .EQ.-2147483647   )STOP 'OBAP   NXVEH  01'
      IV = NXVEH
C-----END OF VEHICLE LOOP
 4010 CONTINUE
C-----END OF OUTBOUND LANE LOOP
 5010 CONTINUE
C-----END OF OUTBOUND APPROACH LOOP
 6010 CONTINUE
      RETURN
      END                                                               OBAP
C
C
C
      SUBROUTINE LOGOUT
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SUMST1'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           FIRSTP
      INTEGER           INDEX,INT1TC(4,4),IPASS,JA,JTURN,KV
      DOUBLE PRECISION  ACMXV,AVGSPD,AVGVEL,DCMXV,DESPD,POSADD,XDISTL,
     *                  XDISTT,XDMPH,XQD,XSD,XSTIME,XTD,XVMT
      SAVE              FIRSTP
      DATA     FIRSTP / .TRUE. /
C-----INT1TC IS THE ITURN CODE FOR STATISTICS FOR THE 1ST INTERSECTION
C-----OF A DIAMOND INTERCHANGE AND IS INDEXED BY (INT1T(IV),ITURN(IV))
C-----1 = U-TURN
C-----2 = LEFT TURN
C-----3 = STRAIGHT
C-----4 = RIGHT TURN
C-----5 = INT LEFT
C-----6 = INT RIGHT
C-----                  (1,1) (2,1) (3,1) (4,1) (1,2) (2,2) (3,2) (4,2)
C-----                  (1,3) (2,3) (3,3) (4,3) (1,4) (2,4) (3,4) (4,4)
      DATA     INT1TC /   1  , -1  , -1  , -1  , -1  ,  5  ,  5  ,  5  ,
     *                   -1  ,  2  ,  3  ,  4  , -1  ,  6  ,  6  ,  6  /
  601 FORMAT(11H ***** VEH=,I6,5H IBA=,I2,6H TURN=,I1,4H TD=F9.4,
     *       4H QD=,F9.4,4H SD=,F9.4,4H DM=,F9.4,5H VMT=,F9.4,
     *       4H ST=,F9.4,4H AS=,F9.4,4H DS=,F9.4,4H AM=,F9.4,
     *       4H DM=,F9.4)
  602 FORMAT(F7.2,I7,I3,I2,3F10.4)
C
C-----SUBROUTINE LOGOUT ADDS THE VEHICLES SIMULATION STATISTICS FOR THE
C-----INBOUND APPROACH AND TURN CODE AND LOGS THE VEHICLE OUT OF THE
C-----SYSTEM, THE OUTBOUND APPROACH, AND THE OUTBOUND LANE
C
C[    INDEX      = -2147483647
C[    IPASS      = -2147483647
C[    JA         = -2147483647
C[    JTURN      = -2147483647
C[    ACMXV      = -2147483647.0
C[    AVGSPD     = -2147483647.0
C[    AVGVEL     = -2147483647.0
C[    DCMXV      = -2147483647.0
C[    DESPD      = -2147483647.0
C[    XDISTL     = -2147483647.0
C[    XDMPH      = -2147483647.0
C[    XQD        = -2147483647.0
C[    XSD        = -2147483647.0
C[    XSTIME     = -2147483647.0
C[    XTD        = -2147483647.0
C[    XVMT       = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'LOGOUT'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----CALCULATE THE DISTANCE AND TIME TO TRAVEL TO THE END OF THE
C-----OUTBOUND LANE
      XDISTL = DBLE( LGEOM(4,IL) ) - POSOLD
      CALL  TIMPOS  ( DBLE( LGEOM(4,IL) ),POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                DT,1.05D0*DT,XDISTT                              )
                    IF ( XDISTT . EQ . TIMERR )  XDISTT = XDISTL/VELOLD
      XDISTT = DMAX1( DMIN1( XDISTT,1.05D0*DT ),0.0D0 )
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER XDISTT SECONDS
      CALL  NEWVEL  ( XDISTT,XDISTT**2,XDISTT**3 )
            IF ( IAND( IPRTLO(IV),2 ) . EQ . 0 ) GO TO 1002
C-----WRITE VEHICLE
C[    IF ( JA                 .EQ.-2147483647   )STOP 'LOGOUT JA     02'
C[    IF ( JTURN              .EQ.-2147483647   )STOP 'LOGOUT JTURN  02'
C[    IF ( AVGVEL             .EQ.-2147483647.0 )STOP 'LOGOUT AVGVEL 01'
C[    IF ( XDMPH              .EQ.-2147483647.0 )STOP 'LOGOUT XDMPH  01'
C[    IF ( XQD                .EQ.-2147483647.0 )STOP 'LOGOUT XQD    01'
C[    IF ( XSD                .EQ.-2147483647.0 )STOP 'LOGOUT XSD    01'
C[    IF ( XSTIME             .EQ.-2147483647.0 )STOP 'LOGOUT XSTIME 02'
C[    IF ( XTD                .EQ.-2147483647.0 )STOP 'LOGOUT XTD    01'
C[    IF ( XVMT               .EQ.-2147483647.0 )STOP 'LOGOUT XVMT   01'
C-----WRITE THE TIME WHEN THE VEHICLES FRONT BUMPER CROSSES THE END OF
C-----THE OUTBOUND LANE, THE VEHICLE NUMBER, THE OUTBOUND APPROACH
C-----NUMBER, THE OUTBOUND LANE NUMBER, AND THE VELOCITY, ACCELERATION,
C-----AND JERK WHEN THE VEHICLES FRONT BUMPER CROSSES THE END OF THE
C-----OUTBOUND LANE
      WRITE (NWV,602) (TIME-DT+XDISTT),IQ(IV),IA,ILN,VELNEW,ACCNEW,
     *                SLPNEW
 1002 CONTINUE
C-----IF THE TIME INTO THE SIMULATION IS LE THE START-UP TIME FOR THE
C-----SIMULATION THEN DO NOT ADD THE VEHICLES SIMULATION STATISTICS
      IF ( TIME . LE . STRTIM )                  THEN
        NUMPSU = NUMPSU + 1
        GO TO 1055
      END IF
      NUMPST = NUMPST + 1
C-----COMPUTE THE INDEX FOR THE /SUMSTA/ ARRAYS DIMENSIONED TO (NIS,NTS)
C-----OR (NIS*NTS) WHERE NIS=NIA+2 AND NTS=NTC+1 THEREFORE (NIA+2,NTC+1)
C-----BASED ON THE INBOUND APPROACH NUMBER (IAN=1,NIA) AND TURN CODE OF
C-----THE VEHICLE (ITC=1,NTC) FOR (LIBAR(JA),JTURN)
C-----(NIA+1=NON-INTERNAL INBOUND APPROACH TOTALS FOR INTERSECTION)
C-----(NIA+2=INTERNAL     INBOUND APPROACH TOTALS FOR INTERSECTION)
C-----(NTC+1=TURN CODE TOTALS FOR APPROACH)
      IPASS = 1
      JA = LIBA( IBAPS(IV) )
      IF ( INT1T(IV) . EQ . 0 )                  THEN
        JTURN = ITURN(IV)
      ELSE
        JTURN = INT1TC(INT1T(IV),ITURN(IV))
        IF ( JTURN . LE . 0 )                    GO TO 9220
      END IF
 1006 CONTINUE
C[    IF ( JA                 .EQ.-2147483647   )STOP 'LOGOUT JA     01'
C[    IF ( JTURN              .EQ.-2147483647   )STOP 'LOGOUT JTURN  01'
      INDEX = (JTURN-1)*NIS + LIBAR(JA)
C[    IF ( INDEX              .EQ.-2147483647   )STOP 'LOGOUT INDEX  01'
      NUMPRO(INDEX) = NUMPRO(INDEX) + 1
C-----COMPLETE THE VEHICLES SIMULATION STATISTICS
      XSTIME = DT*(IEXTIM(IV) +ITIMV(IV)) + XDISTT
      AVGSPD = ISPDS(IV)/DBLE( ITIMV(IV) )
      IDTS(IV) = OLDDTS + XDISTL
C-----COMPUTE THE VEHICLES TOTAL DELAY = (THE ACTUAL TRAVEL TIME) -
C-----(THE TIME TO TRAVEL THE SAME DISTANCE AT THE AVERAGE DESIRED
C-----SPEED)
      XTD = DMAX1( XSTIME-(IDTS(IV) )/AVGSPD,0.0D0 )
                    IF ( XTD . LE . 0.0D0 )      GO TO 1010
C[    IF ( INDEX              .EQ.-2147483647   )STOP 'LOGOUT INDEX  02'
      TD(INDEX)  = TD(INDEX)  + XTD
      NTD(INDEX) = NTD(INDEX) + 1
 1010 CONTINUE
C-----COMPUTE THE VEHICLES QUEUE DELAY
      XQD = IQDS(IV)*DT
                    IF ( XQD . LE . 0.0D0 )      GO TO 1020
C[    IF ( INDEX              .EQ.-2147483647   )STOP 'LOGOUT INDEX  03'
      QD(INDEX)  = QD(INDEX)  + XQD
      NQD(INDEX) = NQD(INDEX) + 1
 1020 CONTINUE
C-----COMPUTE THE VEHICLES STOPPED DELAY
      XSD = ISDS(IV)*DT
                    IF ( XSD . LE . 0.0D0 )      GO TO 1030
C[    IF ( INDEX              .EQ.-2147483647   )STOP 'LOGOUT INDEX  04'
      SD(INDEX)  = SD(INDEX)  + XSD
      NSD(INDEX) = NSD(INDEX) + 1
 1030 CONTINUE
C-----COMPUTE THE VEHICLES DELAY BELOW XX MPH
      XDMPH = IDVS(IV)*DT
                    IF ( XDMPH . LE . 0.0D0 )    GO TO 1040
C[    IF ( INDEX              .EQ.-2147483647   )STOP 'LOGOUT INDEX  05'
      DMPH(INDEX)  = DMPH(INDEX)  + XDMPH
      NDMPH(INDEX) = NDMPH(INDEX) + 1
 1040 CONTINUE
C-----COMPUTE THE VEHICLES MILES OF TRAVEL
      XVMT = IDTS(IV)/5280.0D0
C[    IF ( INDEX              .EQ.-2147483647   )STOP 'LOGOUT INDEX  06'
      VMT(INDEX) = VMT(INDEX) + XVMT
C-----ADD THE VEHICLES TRAVEL TIME
C[    IF ( XSTIME             .EQ.-2147483647.0 )STOP 'LOGOUT XSTIME 01'
      STIME(INDEX) = STIME(INDEX) + XSTIME
C-----COMPUTE THE VEHICLES AVERAGE VELOCITY
      AVGVEL = 3600.0D0*XVMT/XSTIME
      ASPEED(INDEX) = ASPEED(INDEX) + AVGVEL
C-----COMPUTE THE VEHICLES AVERAGE DESIRED SPEED
C[    IF ( AVGSPD             .EQ.-2147483647.0 )STOP 'LOGOUT AVGSPD 01'
      DESPD = AVGSPD*FPS2MH
      ADESPD(INDEX) = ADESPD(INDEX) + DESPD
C-----COMPUTE THE VEHICLES MAXIMUM ACC/DEC
      ACMXV = (IVMAXA(IV)/10.0D0)/AUTOL
      VMAXA(INDEX) = VMAXA(INDEX) + ACMXV
      DCMXV = (IVMAXD(IV)/10.0D0)/DUTOL
      VMAXD(INDEX) = VMAXD(INDEX) + DCMXV
            IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 1050
      IF ( FIRSTP )                              THEN
        CALL  PHEADR  ( 6 )
        FIRSTP = .FALSE.
      END IF
C-----PRINT THE VEHICLES SIMULATION STATISTICS
C[    IF ( JA                 .EQ.-2147483647   )STOP 'LOGOUT JA     02'
C[    IF ( JTURN              .EQ.-2147483647   )STOP 'LOGOUT JTURN  02'
C[    IF ( AVGVEL             .EQ.-2147483647.0 )STOP 'LOGOUT AVGVEL 01'
C[    IF ( XDMPH              .EQ.-2147483647.0 )STOP 'LOGOUT XDMPH  01'
C[    IF ( XQD                .EQ.-2147483647.0 )STOP 'LOGOUT XQD    01'
C[    IF ( XSD                .EQ.-2147483647.0 )STOP 'LOGOUT XSD    01'
C[    IF ( XSTIME             .EQ.-2147483647.0 )STOP 'LOGOUT XSTIME 02'
C[    IF ( XTD                .EQ.-2147483647.0 )STOP 'LOGOUT XTD    01'
C[    IF ( XVMT               .EQ.-2147483647.0 )STOP 'LOGOUT XVMT   01'
      WRITE (6,601) IQ(IV),JA,JTURN,XTD,XQD,XSD,XDMPH,XVMT,XSTIME,
     *              AVGVEL,DESPD,ACMXV,DCMXV
 1050 CONTINUE
C[    IF ( IPASS              .EQ.-2147483647   )STOP 'LOGOUT IPASS  01'
                    IF ( IPASS . EQ . 2 )        GO TO 1055
                    IF ( INT2P(IV) . EQ . 0 )    GO TO 1055
      IPASS = 2
      JA = ISNA( LIBL( INT2P(IV) ) )
      JTURN      = ITURN (IV)
      IEXTIM(IV) = IEXTII(IV)
      ITIMV (IV) = ITIMVI(IV)
      IQDS  (IV) = IQDSI (IV)
      ISDS  (IV) = ISDSI (IV)
      IDVS  (IV) = IDVSI (IV)
      ISPDS (IV) = ISPDSI(IV)
      IVMAXA(IV) = IVMXAI(IV)
      IVMAXD(IV) = IVMXDI(IV)
      OLDDTS     = OLDDTI
      GO TO 1006
 1055 CONTINUE
C-----LOG THE VEHICLE OUT OF THE SYSTEM, THE OUTBOUND APPROACH, AND THE
C-----OUTBOUND LANE
      NVSY = NVSY - 1
      NVOBA = NVOBA - 1
      IQ(IV) = 0
      NVIL(ILN,IA) = NVIL(ILN,IA) - 1
      NVIA(IA) = NVIA(IA) - 1
C-----SET THE FIRST VEHICLE IN THE LANE TO THIS VEHICLES NOR
      IFVL(IL) = NOR(IV)
                    IF ( NOR(IV) . GT . 0 )      GO TO 1060
C-----IF THERE IS NO VEHICLE TO THE REAR THEN SET THE LAST VEHICLE IN
C-----THE LANE TO ZERO (NOR EQ 0)
      ILVL(IL) = 0
      GO TO 1070
 1060 CONTINUE
C-----SET MFINL AND MOASF TO TRUE, RESET IACC TO SLIGHTLY DECELERATING
C-----IF MSFLG EQ TRUE AND THE VEHICLE IS NOT DECELERATING, SET MSFLG
C-----TO FALSE, AND FINALLY STORE 0 FOR NOF FOR THE NOR VEHICLE
C-----(NOR GT 0)
      CALL  FLGNOR ( .TRUE.,0 )
 1070 CONTINUE
      IF ( MAJCOL(IV) )                          THEN
C-----  CHECK CLEARING PREVIOUS PATH MAJOR COLLISION
        IF ( LPREV(IV) . GT . 0  )               THEN
C-----    FIND THE FIRST COLLISION VEHICLE IN THE PREVIOUS PATH
C-----    STARTING FROM THE FIRST VEHICLE IN THE PREVIOUS PATH
          CALL  FFCVLP  ( .FALSE.,LPREV(IV),.TRUE.,KV,POSADD )
          IF ( KV . EQ . 0 )                     THEN
            PMJCOL(LPREV(IV)) = .FALSE.
          END IF
        END IF
C-----  CHECK CLEARING CURRENT LANE MAJOR COLLISION
C-----  FIND THE FIRST COLLISION VEHICLE IN THE CURRENT LANE STARTING
C-----  FROM THE FIRST VEHICLE IN THE CURRENT LANE
        CALL  FFCVLP  ( .TRUE.,IL,.TRUE.,KV,POSADD )
        IF ( KV . EQ . 0 )                       THEN
          LMJCOL(IL) = .FALSE.
        END IF
      END IF
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9220 CONTINUE
      CALL  ABORTR  ( 'STOP 922 - '                    //
     *                'ILLEGAL COMBO OF TURN CODES - ' //
     *                'LOGOUT'                            )
      STOP  922
      END                                                               LOGOUT
C
C
C
      SUBROUTINE INTERP ( AFLAG )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'CONFLT'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      BYTE              SSAMID,SSAMLN
      CHARACTER*1       AFLAG
      LOGICAL           DIRPTH,EVRESP,IFORCE,LVMSDD,NFINL,NPRO
CJ    LOGICAL           IHPRT
CP    LOGICAL           IPPRT
      INTEGER           IAE,IPE,IPR,IVC,IVE,JL,LAT,LV,NV,NXVEH
CJ    INTEGER           IDESPD
CN    INTEGER           ICN
CO    INTEGER           IRN
CP    INTEGER           ICP
      INTEGER*4         SSAMLK,SSAMVI
      REAL*4            SSAMVA,SSAMVL,SSAMVS,SSAMVW,SSAMXF,SSAMXR,
     *                  SSAMYF,SSAMYR
      DOUBLE PRECISION  ACCVEH,ANGLE,ATAN36,AZIM36,HWM,PERLEN,POSADD,
     *                  POSCHK,POSNFB,POSNRB,POSOFB,POSORB,POSVEH,SAFR,
     *                  SAFVEL,SLPTMP,SLPVEH,VELVEH,X,Y
C OLD DOUBLE PRECISION  POSR
C7    DOUBLE PRECISION  POSLC7
  501 FORMAT(I3,I4,2I3,I1,I3,I6,I2,I1,I3)
CJ701 FORMAT(/,16H INTERSECTION ( ,A1,5H ) AT,F8.2,4H SEC,/,
CJ   *       49H PATH  VEH  NUM NOF NOR NRC VEHPOS VEHVEL VEHACC ,
CJ   *       46HACCSLP DS VC DC NX OA ST LG LF LC PR SCON   SG)
C7702 FORMAT(F7.2,I6,4I4,2F7.1)
CJ703 FORMAT(2I3,I4,I6,2I4,2(I3,I4),2F7.2,2F7.3,10I3,F5.1,I5,3(1X,A10))
CO704 FORMAT(18(1X,A6))
CN754 FORMAT(8H PATH   ,I3,1X,12I4,1X,60I1)
CN756 FORMAT(8H VEHD   ,I3,4F6.0,3I2,2I3,2I4,F6.0,2I4,I3,2I4,F4.0,F6.0,
CN   *                  I2,2(I3,I4),I3,1X,13L1,1X,7L1,/,8X,
CN   *                  21HCONTINUE W/ INTERNALS,18X,2I4,F6.0,2I4,3X,
CN   *                  2I4,4X,F6.0,L2,I3,L2,I6)
CN757 FORMAT(8H VEHF   ,I3,1X,10I4,F4.2,4I4,F4.2)
C
C-----SUBROUTINE INTERP PROCESSES THE VEHICLES ON THE INTERSECTION PATHS
C
C-----IPR VALUES
C-----0 FIRST DT OF A LANE CHANGE TO THE RIGHT THUS DO NOT WRITE VEHICLE
C-----1 NORMAL
C-----2 LOGIN
C-----3 LOGIBI
C-----4 LOGIOB OR LOGIIN
C-----5 LOGOUT
C-----6 LANE CHANGE
C
C-----LVMSDD = DISTRACTED DRIVER VMS MESSAGE WAS CANCELLED OR TIMED OUT
C
C[    IAE        = -2147483647
C[    IPE        = -2147483647
C[    IPR        = -2147483647
C[    IVE        = -2147483647
C[    LAT        = -2147483647
C[    NV         = -2147483647
C[    NXVEH      = -2147483647
C*    NRNAME = 1
C*    IRNAME(NRNAME) = 'INTERP'
CJ    IHPRT = .FALSE.
      IGO = 0
C-----PROCESS EACH INTERSECTION PATH
      DO 5010  IP = 1 , NPATHS
      NV = NVIP(IP)
C-----IF THERE ARE NO VEHICLES ON THE INTERSECTION PATH THEN SKIP TO THE
C-----NEXT INTERSECTION PATH
                    IF ( NV . LE . 0 )           GO TO 5010
      IA = ISNA( LIBL(IP) )
            IF ( IAFLAG(IA) . NE . AFLAG )       GO TO 5010
CJ                  IF ( IHPRT )                 GO TO 101
CL                  IF ( TIME . LT . TPRINT )    GO TO 101
CJ    WRITE (6,701) AFLAG,TIME
CJ    IHPRT = .TRUE.
CJ101 CONTINUE
CP    IPPRT = .FALSE.
CP                  IF ( (.NOT. IPPRT) )         GO TO 102
CN                  IF ( TIME . LT . TPRINT )    GO TO 102
CN    WRITE (6,754) IP,LENP(IP),IOPT(IP),LIBL(IP),LOBL(IP),IFVP(IP),
CN   *              ILVP(IP),LIMP(IP),IPT(IP),NGEOCP(IP),NCPSET(IP),
CN   *              IOA(IP),ILCH(IP),(ICPSET(ICN,IP),ICN=1,NCP)
CN102 CONTINUE
C-----SET THE INDEX FOR THE FIRST VEHICLE ON THE INTERSECTION PATH
      IV    = IFVP(IP)
      IVPV  = 0
      PVPOS = LENP(IP)
      PVVEL = LIMP(IP)
      PVACC = 0.0D0
      PVSLP = 0.0D0
C-----PROCESS EACH VEHICLE ON THE INTERSECTION PATH
C[    IF ( NV                 .EQ.-2147483647   )STOP 'INTERP NV     01'
      DO 4010  IVN = 1 , NV
      IPR = 1
C*    NRNAME = 1
      ENDLN = LENP(IP)
C-----INITIALIZE SEVERAL PARAMETERS FOR THE VEHICLE
      CALL  PREST1  ( .FALSE.,LVMSDD )
      NFINL = MFINL(IV)
      NPRO  = MPRO (IV)
                    IF ( IVPV . GT . 0 )         GO TO 1020
                    IF ( (.NOT. MFINL(IV)) )     GO TO 1020
C-----LOOK AHEAD AS FAR AS POSSIBLE FROM THE INTERSECTION PATH
C-----INTO THE LINKING OUTBOUND LANE FOR THIS VEHICLE (MAY BE AN
C-----INTERNAL INBOUND LANE FOR A DIAMOND INTERSECTION) AND IF A DIAMOND
C-----INTERSECTION THEN POSSIBLY INTO THE LINKING INTERSECTION 2 PATH
C-----AND THE LINKING OUTBOUND LANE FOR THE LINKING INTERSECTION 2 PATH
C-----FOR THIS VEHICLE AND IF THERE IS A VEHICLE AHEAD THEN RESET THE
C-----PREVIOUS VEHICLE PARAMETERS TO THE VEHICLE AHEAD ELSE RESET THE
C-----PREVIOUS VEHICLE PARAMETERS TO THE END OF THE LINK
      CALL  LOKIOB  ( DIRPTH,POSADD )
      IF ( IVPV . GT . 0 )                       THEN
        IF ( ( DIRPTH         ) . AND .
     *       (.NOT. MAJCOL(IV)) . AND .
     *       ( MAJCON(IVPV)   ) )                THEN
          MAJCON(IV) = .TRUE.
          POSCON(IV) = DMIN1( POSCON(IV),POSCON(IVPV)+POSADD )
C-----    IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVPV
C-----    VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----    FALSE
          EVRESP = ( ( IAND( VEHTYP(IV  ),LAVTE ) . EQ . 0 ) . AND .
     *               ( IAND( VEHTYP(IVPV),LAVTE ) . NE . 0 ) )
          RESPEV = ( RESPEV . OR . EVRESP )
        END IF
      END IF
 1020 CONTINUE
C-----COMPUTE NEW ACC/DEC LOGIC FOR THE VEHICLE
      CALL  PREST2
      MFINL(IV) = NFINL
      MPRO (IV) = NPRO
CP          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 105
CN                  IF ( TIME . LT . TPRINT )    GO TO 105
CJ                  IF ( IHPRT )                 GO TO 103
CJ    WRITE (6,701) AFLAG,TIME
CJ    IHPRT = .TRUE.
CJ103 CONTINUE
CP                  IF ( IPPRT )                 GO TO 104
CP    WRITE (6,754) IP,LENP(IP),IOPT(IP),LIBL(IP),LOBL(IP),IFVP(IP),
CP   *              ILVP(IP),LIMP(IP),IPT(IP),NGEOCP(IP),NCPSET(IP),
CP   *              IOA(IP),ILCH(IP),(ICPSET(ICP,IP),ICP=1,NCP)
CP    IPPRT = .TRUE.
CP104 CONTINUE
CN    WRITE (6,757) IV,IDRICL(IV),IVEHCL(IV),ISPD  (IV),NOF   (IV),
CN   *                 NOR   (IV),LNEXT (IV),LPRES (IV),ITURN (IV),
CN   *                 IBAPS (IV),IPRTLO(IV),IEXTIM(IV),NOBAPD(IV),
CN   *                 INT2P (IV),INT2S (IV),INT1T (IV),IEXTII(IV)
CN    WRITE (6,756) IV,ISLP  (IV),IACC  (IV),IVEL  (IV),IPOS  (IV),
CN   *                 ISET  (IV),LCHGE (IV),ISPDP (IV),LEGAL (IV),
CN   *                 IPRTM (IV),ITIMV (IV),IQDS  (IV),ISPDS (IV),
CN   *                 ISDS  (IV),IDVS  (IV),ISTCON(IV),IVMAXA(IV),
CN   *                 IVMAXD(IV),LATPOS(IV),IDTS  (IV),LALT  (IV),
CN   *                 IPRC(1,IV),NORC(1,IV),IPRC(2,IV),NORC(2,IV),
CN   *                 LOGFLG(IV),MBLOCK(IV),MFGOF (IV),MFINL (IV),
CN   *                 MFSTPF(IV),MLAG  (IV),MOASF (IV),MPOBS (IV),
CN   *                 MPRO  (IV),MSAOR (IV),MSFLG (IV),MSTPF (IV),
CN   *                 MTCARS(IV),MININT(IV),IACDS (IV),IACLDS(IV),
CN   *                 ICDFS (IV),IFVA  (IV),IRSTOP(IV),ISDEC (IV),
CN   *                 ISTMO (IV),ITIMVI(IV),IQDSI (IV),ISPDSI(IV),
CN   *                 ISDSI (IV),IDVSI (IV),IVMXAI(IV),IVMXDI(IV),
CN   *                 IDTSI (IV),IUPDAT(IV),JVCNOR(IV),IDISPD(IV),
CN   *                 IQ(IV)
CN105 CONTINUE
C-----UNBIAS THE VEHICLE ATTRIBUTES AND PREDICT THE NEW POS/VEL/ACC
      CALL  UNBIAS
      NXVEH = NOR(IV)
C-----CHECK IF THERE IS A MAJOR COLLISION
      IF ( MAJCLB(IV) . OR . MAJCLL(IV) )        THEN
C-----  THERE IS A COLLISION VEHICLE ON THE INTERSECTION PATH BEFORE
C-----  THIS VEHICLE OR DOWNSTREAM THUS CALCULATE A DECEL TO A STOP JUST
C-----  BEFORE THE POINT OF COLLISION
        HWM    = 0.5D0*WIDV(IVEHCL(IV))
        SAFVEL = DMAX1( VELOLD,VELNEW,DESVEL )
        SAFR   = (SAFDIS+(SAFVEL/SAFSPD))/DCHAR(IDRICL(IV))
        POSVEH = DMIN1( POSCLB(IV),POSCLL(IV) ) - 2.0D0*HWM - SAFR
        VELVEH = 0.0D0
        ACCVEH = 0.0D0
        SLPVEH = 0.0D0
        CALL  SLPCFS  ( SLPTMP,IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                            POSVEH,VELVEH,ACCVEH,SLPVEH  )
        IF ( SLPTMP . NE . 0.0D0 )               THEN
          SLPBLK = DMIN1( SLPBLK,SLPTMP )
        END IF
      END IF
C-----CHECK THE ACC/DEC LOGICAL DEPENDENT ATTRIBUTES, CALL THE
C-----APPROPRIATE ACC/DEC ROUTINES, AND COMPUTE THE VEHICLES NEW POS/
C-----VEL/ACC
      CALL  ACDCP   ( .FALSE. )
C-----IF THE VEHICLE MAY PROCEED THEN AVOID INTERSECTION CONFLICTS
      IF ( MPRO(IV)           . AND .
     *     (VELNEW.GT.VELSTP) . AND .
     *     (.NOT. MSFLG(IV) ) )                  THEN
        CALL  AVDCON
      END IF
C7    POSLC7 = 0.0D0
C7    WRITE (PPP,702) TIME,IQ(IV),2,IP,IP,IVEHCL(IV),POSNEW,POSLC7
CR          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 106
C-----PRINT POS/VEL/ACC FOR THE VEHICLE
CQ    CALL  PVAPRT
CR106 CONTINUE
                    IF ( ICONTR . LT . ICSACT )  GO TO 1030
      JL = LPREV(IV)
                    IF ( JL . LE . 0 )           GO TO 1030
                    IF ( NLDL(JL) . LE . 0 )     GO TO 1030
C-----CHECK IF REAR BUMPER STILL ON INBOUND LANE
      POSORB = POSOLD - LENVAP
                    IF ( POSORB . GE . 0.1D0 )   GO TO 1030
      POSOFB = POSOLD + DBLE( LGEOM(4,JL) )
      POSNFB = POSNEW + DBLE( LGEOM(4,JL) )
C-----CHECK EACH DETECTOR FOR THIS LANE TO SEE IF THIS VEHICLE TRIPPED
C-----ANY OF THEM THIS DT
      CALL  CHKLDT  ( JL,POSOFB,POSNFB )
 1030 CONTINUE
C-----UPDATE THE VEHICLES SIMULATION STATISTICS IN THE INTERSECTION
      CALL  SSINTR
            IF ( ISTCON(IV) . GT . NGEOCP(IP) )  GO TO 2010
C-----CLEAR THE INTERSECTION CONFLICTS AS THE REAR BUMPER PASSES THEM
      POSNRB = POSNEW - LENVAP
C-----IF THE VEHICLES REAR BUMPER CROSSED FROM THE INTERSECTION PATH TO
C-----THE OUTBOUND LANE THEN FORCE CLEARING OF ALL INTERSECTION
C-----CONFLICTS
      IFORCE = ((POSNRB+0.1D0) . GT . DBLE( LENP(IP) ))
      CALL  CLRCON ( IP,POSNEW,IFORCE )
 2010 CONTINUE
      IF ( PVPOS+XRELMI . LE . POSNEW )          THEN
C-----  PRINT THE COLLISION INFORMATION AND RESET THE VEHICLES
C-----  POS/VEL/ACC
        CALL  BANGS  ( 2,IVPV,RELVEL,RELPOS,0.0D0 )
      END IF
C-----CHECK FOR INTERSECTION COLLISIONS 
      CALL  CHKCOL
C-----IF THE VEHICLE LEFT THE INTERSECTION PATH THEN LOG THE VEHICLE OUT
C-----OF THE INTERSECTION PATH AND INTO THE LINKING OUTBOUND LANE
            IF ( POSNEW.LT.DBLE( LENP(IP) ) )    GO TO 3010
C-----LOG THE VEHICLE OUT OF THE INTERSECTION PATH AND INTO THE LINKING
C-----OUTBOUND LANE
      IF ( IAFLAG(IOA(IP)) . EQ . ILETTI )       THEN
        CALL  LOGIIN
      ELSE
        CALL  LOGIOB
      END IF
C3    KPFLAG = 'LEAVE INTR'
      IPR = 4
 3010 CONTINUE
C-----BIAS THE VEHICLE ATTRIBUTES, SET THE PREVIOUS VEHICLE PARAMETERS,
C-----AND UPDATE THE MAXIMUM ACC/DEC FOR THE VEHICLE
      CALL  BIAS    ( IPR )
C-----SET VEHICLE HEADING
      IF ( IPR . EQ . 4 )                        THEN
C-----  VEHICLE LOGGED OUT OF INTERSECTION INTO APPROACH
        IA  = ISNA(LPRES(IV))
        HEADNG(IV) = DBLE( IAAZIM(IA) )
        STEERA(IV) = 0.0D0
      ELSE
C-----  VEHICLE ON INTERSECTION PATH
        POSCHK = POSNEW
C-----  CHECK IF VEHICLE IS ON THE 1ST SEGMENT - LINE 1
        IF ( LL1(IP) . GT . 0 )                    THEN
          IF ( POSCHK . LE . DBLE( LL1(IP) ) )     THEN
            X = DBLE( JXL1(IP)-IXL1(IP) )
            Y = DBLE( JYL1(IP)-IYL1(IP) )
            HEADNG(IV) = AZIM36( Y,X )
            STEERA(IV) = 0.0D0
            GO TO 3020
          ELSE
            POSCHK = POSCHK - LL1(IP)
          END IF
        END IF
C-----  CHECK IF VEHICLE IS ON THE 2ND SEGMENT - ARC 1
        IF ( LA1(IP) . GT . 0 )                    THEN
          IF ( POSCHK . LE . DBLE( LA1(IP) ) )     THEN
            PERLEN = POSCHK / DBLE( LA1(IP) )
            ANGLE = DBLE( IBA1(IP) ) + PERLEN*DBLE( IDA1(IP) )
            ANGLE = ANGLE + SIGN( 90.0D0,DBLE( IDA1(IP) ) )
            DO WHILE ( ANGLE . LT . 0.0D0 )
              ANGLE = ANGLE + 360.0D0
            END DO
            DO WHILE ( ANGLE . GE . 360.0D0 )
              ANGLE = ANGLE - 360.0D0
            END DO
            HEADNG(IV) = ANGLE
C-----      STEERING ANGLE IS UNIT 1 LENGTH AT THE RADIUS OF THE ARC
            ANGLE = DBLE( ULEN(1,IVEHCL(IV)) )/DBLE( IRA1(IP) )
            X = DBLE( IRA1(IP) )*DSIN( ANGLE )
            Y = DBLE( IRA1(IP) )*(1.0D0-DCOS( ANGLE ))
            STEERA(IV) = SIGN( RAD2DG*ATAN36( Y,X ),DBLE( IDA1(IP) ) )
            GO TO 3020
          ELSE
            POSCHK = POSCHK - LA1(IP)
          END IF
        END IF
C-----  CHECK IF VEHICLE IS ON THE 3RD SEGMENT - ARC 2
        IF ( LA2(IP) . GT . 0 )                    THEN
          IF ( POSCHK . LE . DBLE( LA2(IP) ) )     THEN
            PERLEN = POSCHK / DBLE( LA2(IP) )
            ANGLE = DBLE( IBA2(IP) ) + PERLEN*DBLE( IDA2(IP) )
            ANGLE = ANGLE + SIGN( 90.0D0,DBLE( IDA2(IP) ) )
            DO WHILE ( ANGLE . LT . 0.0D0 )
              ANGLE = ANGLE + 360.0D0
            END DO
            DO WHILE ( ANGLE . GE . 360.0D0 )
              ANGLE = ANGLE - 360.0D0
            END DO
            HEADNG(IV) = ANGLE
C-----      STEERING ANGLE IS UNIT 1 LENGTH AT THE RADIUS OF THE ARC
            ANGLE = DBLE( ULEN(1,IVEHCL(IV)) )/DBLE( IRA2(IP) )
            X = DBLE( IRA2(IP) )*DSIN( ANGLE )
            Y = DBLE( IRA2(IP) )*(1.0D0-DCOS( ANGLE ))
            STEERA(IV) = SIGN( RAD2DG*ATAN36( Y,X ),DBLE( IDA2(IP) ) )
            GO TO 3020
          ELSE
            POSCHK = POSCHK - LA2(IP)
          END IF
        END IF
C-----  CHECK IF VEHICLE IS ON THE 4TH SEGMENT - LINE 2
        IF ( LL2(IP) . GT . 0 )                    THEN
          IF ( POSCHK . LE . DBLE( LL2(IP) ) )     THEN
            X = DBLE( JXL2(IP)-IXL2(IP) )
            Y = DBLE( JYL2(IP)-IYL2(IP) )
            HEADNG(IV) = AZIM36( Y,X )
            STEERA(IV) = 0.0D0
            GO TO 3020
          ELSE
            GO TO 9760
          END IF
        END IF
        GO TO 9760
      END IF
 3020 CONTINUE
C-----CHECK CLEARING PREVIOUS LANE MAJOR COLLISION
      IF ( MAJCOL(IV) .AND. ( IPR . NE . 4 ) )   THEN
        IF ( ( POSOLD    . LE . LENVAP ) . AND .
     *       ( POSNEW    . GT . LENVAP ) . AND .
     *       ( LPREV(IV) . GT . 0      ) )       THEN
C-----    FIND THE FIRST COLLISION VEHICLE IN THE PREVIOUS LANE STARTING
C-----    FROM THE FIRST VEHICLE IN THE PREVIOUS LANE
          CALL  FFCVLP  ( .TRUE.,LPREV(IV),.TRUE.,LV,POSADD )
          IF ( LV . EQ . 0 )                     THEN
            LMJCOL(LPREV(IV)) = .FALSE.
          END IF
        END IF
      END IF
C-----PRINT SELECTED ATTRIBUTES FOR THE VEHICLE
                    IF ( JPRTM . GT . 0 )        IPRTM(IV) = JPRTM
CM          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 107
CL                  IF ( TIME . LT . TPRINT )    GO TO 107
C3                  IF ( JPRTM . GT . 0 )        JPFLAG = 'PIJR TIME '
CJ    IDESPD = IDNINT( DESVEL )
CJ    WRITE (6,703) IA,ILN,IV,IQ(IV),NOF(IV),NOR(IV),IPRC(1,IV),
CJ   *              NORC(1,IV),IPRC(2,IV),NORC(2,IV),POSNEW,VELNEW,
CJ   *              ACCNEW,SLPNEW,IDESPD,IVEHCL(IV),IDRICL(IV),
CJ   *              LNEXT(IV),NOBAPD(IV),ISET(IV),LEGAL(IV),LOGFLG(IV),
CJ   *              LCHGE(IV),IPRTM(IV),POSLCA,0,IPFLAG,JPFLAG,KPFLAG
CL107 CONTINUE
CP          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 108
CO                  IF ( TIME . LT . TPRINT )    GO TO 108
CO    WRITE (6,704) (IRNAME(IRN),IRN=1,NRNAME)
CO108 CONTINUE
      IF ( (IPOLL . EQ . INO   ) . OR .
     *     (TIME  . LT . BEGT20) . OR .
     *     (TIME  . GT . ENDT20) )               GO TO 110
      IPE = IDNINT( POSNEW )
      IVE = IDNINT( VELNEW )
      LAT = 0
C-----SET ANIMATION VEHICLE CODE
C-----ICNONE   0 VEHICLE CODE - NONE
      IVC = ICNONE
C-----ICYLTS   1 VEHICLE CODE - YELLOW LEFT TURN SIGNALS FRONT AND REAR
      IF ( ( ITURN(IV) . EQ . ITURNU ) . OR .
     *     ( ITURN(IV) . EQ . ITURNL )       )   IVC = IVC + ICYLTS
C-----ICYRTS   2 VEHICLE CODE - YELLOW RIGHT TURN SIGNALS FRONT AND REAR
      IF (   ITURN(IV) . EQ . ITURNR         )   IVC = IVC + ICYRTS
C-----ICRBRK   4 VEHICLE CODE - RED BRAKE LIGHTS IN REAR
      IF ( ( ACCNEW . LE . DECBRK ) . OR .
     *     ( VELNEW . EQ . 0.0D0  )          )   IVC = IVC + ICRBRK
C-----ICVBMC   8 VEHICLE CODE - VEHICLE BLOCKED BY A MAJOR COLLISION
      IF ( MAJCLB(IV) . OR . MAJCLL(IV)      )   IVC = IVC + ICVBMC
C-----ICVIMC  16 VEHICLE CODE - VEHICLE INVOLVED IN A MAJOR COLLISION
      IF ( MAJCOL(IV)                        )   IVC = IVC + ICVIMC
C-----ICEVRC  32 VEHICLE CODE - EMERGENCY VEHICLE RUNNING A CALL
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   IVC = IVC + ICEVRC
C-----ICVREV  64 VEHICLE CODE - VEHICLE REACTING TO EMERG VEH RUN A CALL
      IF ( RESPEV                            )   IVC = IVC + ICVREV
C-----ICRVMS 128 VEHICLE CODE - VEHICLE REACTING TO VMS MESSAGE
      IF ( ( VMSACM(IV) . GT . 0 ) . OR .
     *     ( VMSASM(IV) . GT . 0 )           )   IVC = IVC + ICRVMS
      IAE = IDNINT( ACCNEW )
C[    IF ( IPR                .EQ.-2147483647   )STOP 'INTERP IPR    01'
                    IF ( IPR . EQ . 1 )          GO TO 109
      IF ( IPOLL . EQ . IANI )                   THEN
C[      IF ( IPE              .EQ.-2147483647   )STOP 'INTERP IPE    01'
C[      IF ( IPR              .EQ.-2147483647   )STOP 'INTERP IPR    02'
C[      IF ( IVE              .EQ.-2147483647   )STOP 'INTERP IVE    01'
        WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV)
      ELSE
C[      IF ( IAE              .EQ.-2147483647   )STOP 'INTERP IAE    01'
C[      IF ( IPE              .EQ.-2147483647   )STOP 'INTERP IPE    02'
C[      IF ( IPR              .EQ.-2147483647   )STOP 'INTERP IPR    03'
C[      IF ( IVE              .EQ.-2147483647   )STOP 'INTERP IVE    02'
C[      IF ( LAT              .EQ.-2147483647   )STOP 'INTERP LAT    01'
        WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT,IVEHCL(IV),
     *                  IPRTLO(IV),IAE
      END IF
      GO TO 110
  109 CONTINUE
      IF ( IPOLL . EQ . IANI )                   THEN
C[      IF ( IPE              .EQ.-2147483647   )STOP 'INTERP IPE    03'
        WRITE (NPD,501) IV,IPE,IVC
      ELSE
C[      IF ( IAE              .EQ.-2147483647   )STOP 'INTERP IAE    02'
C[      IF ( IPE              .EQ.-2147483647   )STOP 'INTERP IPE    04'
C[      IF ( IPR              .EQ.-2147483647   )STOP 'INTERP IPR    04'
C[      IF ( IVE              .EQ.-2147483647   )STOP 'INTERP IVE    03'
C[      IF ( LAT              .EQ.-2147483647   )STOP 'INTERP LAT    02'
        WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT,IVEHCL(IV),
     *                  IPRTLO(IV),IAE
      END IF
  110 CONTINUE
C-----WRITE SURROGATE SAFETY ASSESSMENT METHODOLOGY DATA FOR INTERP
      IF ( ISSAM . EQ . IYES )                   THEN
        SSAMXF = VEHCLE(IV)%FBX
        SSAMYF = VEHCLE(IV)%FBY
        SSAMXR = VEHCLE(IV)%UNIT(VEHCLE(IV)%UNITS)%HPX
        SSAMYR = VEHCLE(IV)%UNIT(VEHCLE(IV)%UNITS)%HPY
        IF ( IPR . EQ . 4 )                      THEN
C-----    VEHICLE LOGGED OUT OF INTERSECTION INTO APPROACH
          IA  = ISNA(LPRES(IV))
          ILN = ISNL(LPRES(IV))
          IL  = LLANES(ILN,IA)
          SSAMLK = IA
          SSAMLN = ILN
C OLD     CALL FNDXYA ( IA,ILN,POSNEW,0.0D0,SSAMXF,SSAMYF )
C OLD     POSR = POSNEW - LENVAP
C OLD     IF ( POSR . GE . DBLE( LGEOM(1,IL) ) ) THEN
C OLD       CALL FNDXYA ( IA,ILN,POSR,0.0D0,SSAMXR,SSAMYR )
C OLD     ELSE
C OLD       POSR = POSR - DBLE( LGEOM(1,IL) ) + DBLE( LENP(IP) )
C OLD       IF ( POSR . GE . 0.0D0 )             THEN
C OLD         CALL FNDXYP ( IP,POSR,SSAMXR,SSAMYR )
C OLD       ELSE
C OLD         IL   = LIBL(IP)
C OLD         IA   = ISNA(IL)
C OLD         ILN  = ISNL(IL)
C OLD         POSR = POSR + DBLE( LGEOM(4,IL) )
C OLD         IF ( POSR . LT . 0.0D0 )           POSR = 0.0D0
C OLD         CALL FNDXYA ( IA,ILN,POSR,0.0D0,SSAMXR,SSAMYR )
C OLD       END IF
C OLD     END IF
        ELSE
C-----    VEHICLE IN INTERSECTION
          SSAMLK = 1000+IP
          SSAMLN = 0
C OLD     CALL FNDXYP ( IP,POSNEW,SSAMXF,SSAMYF )
C OLD     POSR = POSNEW - LENVAP
C OLD     IF ( POSR . GE . 0.0D0 )               THEN
C OLD       CALL FNDXYP ( IP,POSR,SSAMXR,SSAMYR )
C OLD     ELSE
C OLD       IA   = ISNA(LPREV(IV))
C OLD       ILN  = ISNL(LPREV(IV))
C OLD       IL   = LLANES(ILN,IA)
C OLD       POSR = POSR + DBLE( LGEOM(4,IL) )
C OLD       IF ( POSR . LT . 0.0D0 )             POSR = 0.0D0
C OLD       CALL FNDXYA ( IA,ILN,POSR,0.0D0,SSAMXR,SSAMYR )
C OLD     END IF
        END IF
C-----  WRITE SSAM VEHICLE RECORD
C-----  SSAMID = RECORD TYPE (3=VEHICLE)
C-----  SSAMVI = VEHICLE ID
C-----  SSAMLK = LINK ID
C-----  SSAMLN = LANE ID (LEFT TO RIGHT STARTING AT 1)
C-----  SSAMVL = LENV(IVEHCL(IV))
C-----  SSAMXF = X COORDINATE OF MIDDLE OF FRONT BUMPER
C-----  SSAMYF = Y COORDINATE OF MIDDLE OF FRONT BUMPER
C-----  SSAMXR = X COORDINATE OF MIDDLE OF REAR  BUMPER
C-----  SSAMYR = Y COORDINATE OF MIDDLE OF REAR  BUMPER
C-----  SSAMVL = VEHICLE LENGTH (FRONT TO BACK)
C-----  SSAMVW = VEHICLE WIDTH  (LEFT TO RIGHT)
C-----  SSAMVS = VEHICLE SPEED (UNITS/SEC)
C-----  SSAMVA = VEHICLE ACCELERATION (UNITS/SEC/SEC)
        SSAMID = 3
        SSAMVI = IQ(IV)
        SSAMVL = LENV(IVEHCL(IV))
        SSAMVW = WIDV(IVEHCL(IV))
        SSAMVS = VELNEW
        SSAMVA = ACCNEW
        WRITE (ISS) SSAMID,SSAMVI,SSAMLK,SSAMLN,
     *              SSAMXF,SSAMYF,SSAMXR,SSAMYR,
     *              SSAMVL,SSAMVW,SSAMVS,SSAMVA
      END IF
C-----SET THE INDEX FOR THE NEXT VEHICLE ON THE INTERSECTION PATH TO BE
C-----PROCESSED
C[    IF ( NXVEH              .EQ.-2147483647   )STOP 'INTERP NXVEH  01'
      IV = NXVEH
C-----END OF VEHICLE LOOP
 4010 CONTINUE
C-----END OF INTERSECTION PATH LOOP
 5010 CONTINUE
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9760 CONTINUE
      CALL  ABORTR  ( 'STOP 976 - '//
     *                'VEHICLE POSITION IS BEYOND PATH END - '//
     *                'INTERP'                                   )
      STOP  976
      END                                                               INTERP
C
C
C
      SUBROUTINE AVDCON
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'CONCHK'
      INCLUDE 'CONFLT'
      INCLUDE 'CONSTN'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      LOGICAL           EVRESP,IGNORE,LVMSGO,LVMSRR
      INTEGER           IANGLE,INDEX,IVATIN,IVCONF,JH,JM,JL,JNDEX,JP,
     *                  JPRC,JSTCON,KOUNT,KPRTM,KV,GETLCV,NOFC,SA,SLN,SP
C     INTEGER           JSISET
      DOUBLE PRECISION  A,ACCVEH,ACH,ACM,AFACT,B,C,CARDIS,CRISLP,DCH,
     *                  DCM,DCOND,DCOSAN,DESSPD,DISCLH,DISCLM,DSINAN,
     *                  DSLOWH,DSLOWM,DTANAN,DVH,DVM,ERRJUD,FHES,GHES,
     *                  HHES,HWH,HWM,LVH,PCH,PCM,PIJRIV,POSEND,POSSAV,
     *                  POSSTP,POSVEH,PT,POT,SAFF,SAFR,SCH,SCM,SLOPE,
     *                  SLPTCM,SLPTFZ,SLPTMP,SLPTRZ,SLPVEH,T,TCH,TCM,
     *                  TCRASH,TFZ,THES,TIM,TMAX,TMP,TPASCH,TPASCM,
     *                  TPASSC,TPASSH,TPASSM,TRZ,TS,VCH,VCM,VELREL,
     *                  VELVEH
C     DOUBLE PRECISION  FCLEAR,RCLEAR
C 601 FORMAT(I6,F8.2,1X,A6,I5,2I4,4I3,I4,F6.2,4F6.1,I6,3F6.2,4F6.1,
C    *       2F7.2)
C4701 FORMAT(4H AVD,I4,4H VEH,I4,5H TCM=,F5.2,5H VCM=,F4.1,5H DVM=,F5.1,
C4   *       5H DCM=,F5.1,6H   VEH,I4,5H TFZ=,F5.2,5H TCH=,F5.2,
C4   *       5H TRZ=,F5.2,5H VCH=,F4.1,5H DVH=,F4.1,5H DCH=,F5.1)
C
C-----SUBROUTINE AVDCON AVOIDS INTERSECTION CONFLICTS
C
C[    INDEX      = -2147483647
C[    IVCONF     = -2147483647
C[    JH        = -2147483647
C[    JL         = -2147483647
C[    JM        = -2147483647
C[    JNDEX      = -2147483647
C[    JP         = -2147483647
C[    JSTCON     = -2147483647
C[    KOUNT      = -2147483647
C[    NOFC       = -2147483647
C[    SA         = -2147483647
C[    SLN        = -2147483647
C[    SP         = -2147483647
C[    ACCVEH     = -2147483647.0
C[    ACH        = -2147483647.0
C[    ACM        = -2147483647.0
C[    AFACT      = -2147483647.0
C[    CARDIS     = -2147483647.0
C[    CRISLP     = -2147483647.0
C[    DCH        = -2147483647.0
C[    DCM        = -2147483647.0
C[    DVH        = -2147483647.0
C[    DVM        = -2147483647.0
C[    ERRJUD     = -2147483647.0
C[    FHES       = -2147483647.0
C[    GHES       = -2147483647.0
C[    HHES       = -2147483647.0
C[    POSVEH     = -2147483647.0
C[    SLOPE      = -2147483647.0
C[    SLPTCM     = -2147483647.0
C[    SLPTFZ     = -2147483647.0
C[    SLPTMP     = -2147483647.0
C[    SLPTRZ     = -2147483647.0
C[    SLPVEH     = -2147483647.0
C[    TCH        = -2147483647.0
C[    TCM        = -2147483647.0
C[    TCRASH     = -2147483647.0
C[    TFZ        = -2147483647.0
C[    THES       = -2147483647.0
C[    TIM        = -2147483647.0
C[    TMP        = -2147483647.0
C[    TPASCH     = -2147483647.0
C[    TPASCM     = -2147483647.0
C[    TPASSC     = -2147483647.0
C[    TPASSH     = -2147483647.0
C[    TPASSM     = -2147483647.0
C[    TRZ        = -2147483647.0
C[    VCH        = -2147483647.0
C[    VCM        = -2147483647.0
C[    VELREL     = -2147483647.0
C[    VELVEH     = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'AVDCON'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      SLPCON = 0.0D0
                    IF ( IPRTM(IV) . GT . 0 )    RETURN
      EVRESP = .FALSE.
      HWM    =  0.5D0*WIDV (IVEHCL(IV))
      PIJRIV =        PIJR (IDRICL(IV))
      CRISLP = SLPMAX*DCHAR(IDRICL(IV))
C-----SET INTERSECTION PATH FOR ME AND CONFLICT NUMBER TO START CHECKING
      IF ( MININT(IV) )                          THEN
        IX     = IP
        JSTCON = ISTCON(IV)
        IL     = LIBL(IP)
        IA     = ISNA(IL)
        IAN    = 0
        SA     = 0
        SLN    = 0
        SP     = IP
        POSEND = LENP(IP)
      ELSE
        IX     = LNEXT(IV)
                    IF ( IX . EQ . 0 )           RETURN
        JSTCON = 1
        SA     = IA
        SLN    = ILN
        SP     = 0
        IF ( LGEOM(3,IL) . EQ . LGEOM(4,IL) )    THEN
          POSEND = DBLE( LGEOM(2,IL) )
        ELSE
          POSEND = DBLE( LGEOM(4,IL) )
        END IF
      END IF
C-----IF THE VEHICLE IS FORCED TO GO OR THE VEHICLE IS FORCED TO RUN THE
C-----RED SIGNAL THEN GO TO 3020 AND THE VEHICLE MAY PROCEED NORMALLY
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        LVMSGO = ( IVMSMG(VMSASM(IV)) . EQ . VMSMGO )
        LVMSRR = ( IVMSMG(VMSASM(IV)) . EQ . VMSMRR )
      ELSE
        LVMSGO = .FALSE.
        LVMSRR = .FALSE.
      END IF
      IF ( ( MFGOF(IV)                                ) .OR.
     *     ( ( FRRTIM(IV) . GT . 0.0D0              ) .AND.
     *       ( TIME .GE.  FRRTIM(IV)                ) .AND.
     *       ( TIME .LE. (FRRTIM(IV)+FRRATM(IV))    ) ) .OR.
     *     ( LVMSGO                                   ) .OR.
     *     ( LVMSRR                                   ) )
     *                                           GO TO 3020
C-----IF THERE ARE NO GEOMETRIC CONFLICTING PATHS THEN GO TO 3020 AND
C-----THE VEHICLE MAY PROCEED NORMALLY
                    IF ( NGEOCP(IX) . LE . 0 )   GO TO 3020
C-----IF THERE ARE NO INTERSECTION CONFLICTS SET THEN GO TO 3020 AND THE
C-----VEHICLE MAY PROCEED NORMALLY
                    IF ( NCPSET(IX) . LE . 0 )   GO TO 3020
C-----IF THE VEHICLES REAR BUMPER HAS PASSED ALL GEOMETRIC CONFLICTING
C-----PATHS THEN GO TO 3020 AND THE VEHICLE MAY PROCEED NORMALLY
C[    IF ( JSTCON             .EQ.-2147483647   )STOP 'AVDCON JSTCON 01'
            IF ( JSTCON . GT . NGEOCP(IX) )      GO TO 3020
C-----CHECK EACH GEOMETRIC CONFLICTING INTERSECTION PATH
      DO 3010  INDEX = JSTCON , NGEOCP(IX)
C-----IF THE INTERSECTION CONFLICT IS NOT SET THEN GO TO 3010 AND SKIP
C-----TO THE NEXT INTERSECTION CONFLICT
                    IF ( ICPSET(INDEX,IX).EQ.0 ) GO TO 3010
C-----INITIALIZE SOME PARAMETERS FOR AVDCON
      JNDEX = IGEOCP(INDEX,IX)
      KOUNT = 0
      IF ( IX . EQ . ICONP(1,JNDEX) )            THEN
        IANGLE =      ICONAN(JNDEX)
        JH     = 2
        JM     = 1
      ELSE
        IANGLE = (360-ICONAN(JNDEX))
        JH     = 1
        JM     = 2
      END IF
      DSINAN = DABS( DSIN( DBLE( IANGLE )*DEG2RD ) )
      DCOSAN = DABS( DCOS( DBLE( IANGLE )*DEG2RD ) )
      IF ( DCOSAN . LE . 1.0D-8 )                THEN
        DTANAN = 1.0D99
      ELSE
        DTANAN = DSINAN / DCOSAN
      END IF
C-----SET IVCONF TO THE NEXT VEHICLE THAT HAS NOT CLEARED THE
C-----INTERSECTION CONFLICT
      IVCONF = ICONV(JH,JNDEX)
      JP = ICONP(JH,JNDEX)
      JL = LIBL(JP)
      TCM = 0.0D0
C-----SET NOFC TO THE IVCONF VEHICLE
      NOFC = IVCONF
C;    WRITE (TC3,331) TIME,'AVDCON 1 NOFC = IVCONF             ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'IGEOCP(INDEX,IX)=',IGEOCP(INDEX,IX),
C;   *                'JM=',INDEX,'N=',NGEOCP(IX)
C;331 FORMAT(F6.1,1X,A36,I2,I3,I4,I6,1X,L1,F5.1,3I3,I6,2(I4,I6),
C;   *       5(1X,A,I4))
            IF ( MININT(NOFC) )                  GO TO 1020
            IF ( LPRES(NOFC) . EQ . LOBL(JP) )   GO TO 1020
C-----THE NOFC VEHICLE WAS NOT IN THE INTERSECTION THUS SET THE NOFC
C-----VEHICLE TO THE FIRST VEHICLE IN THE OTHER LANE
      NOFC = IFVL(JL)
C;    WRITE (TC3,331) TIME,'AVDCON 2 NOFC = IFVL(JL)           ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JL=',JL,'JM=',INDEX,'N=',NGEOCP(IX)
 1020 CONTINUE
C-----SET THIS VEHICLES PARAMETERS FOR PREDICTING TIME AND VELOCITY TO
C-----AN INTERSECTION CONFLICT
      CALL  SETPTV  ( IL )
      SO = SLPNEW
      IF ( MININT(IV) )                          THEN
        PO = PO + LGEOM4
      END IF
C[    IF ( JM                 .EQ.-2147483647   )STOP 'AVDCON JM     01'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'AVDCON JNDEX  01'
      P = ICOND(JM,JNDEX) + LGEOM4
      DVM = JSPD
      DCM = P - PO
C-----IF THERE IS NO DISTANCE TO TRAVEL TO THE INTERSECTION CONFLICT FOR
C-----ME THEN GO TO 3010 AND SKIP TO THE NEXT INTERSECTION CONFLICT
                    IF ( P-PO . LE . 0.0D0 )     GO TO 3010
C-----PREDICT THE TIME AND VELOCITY TO AN INTERSECTION CONFLICT
      CALL  SNOFPV  ( IX )
      CALL  PREDTV  ( TCM,PCM,VCM,ACM,SCM )
C-----IF THERE IS NO TIME TO THE INTERSECTION CONFLICT FOR ME THEN GO TO
C-----3010 AND SKIP TO THE NEXT INTERSECTION CONFLICT
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'AVDCON TCM    01'
                    IF ( TCM . LE . 0.0D0 )      GO TO 3010
                    IF ( MININT(IV) )            GO TO 1060
                    IF ( LCONTV(IV) .GE. LCSIGX )GO TO 1060
                    IF ( (.NOT. MATSTL(IV)) )    GO TO 1060
C-----THE LANE IS NOT SIGNAL CONTROLLED AND THE VEHICLE IS STOPPED AT
C-----THE STOP LINE THUS INCREMENT THE TIME TO THE CONFLICT FOR ME BY
C-----THE AVERAGE HESITATION TIME
C-----TCM = TCM + 4.0D0*PIJRIV
C-----ABOVE IS ORIGINAL EQUATION, REPLACED BY NEW FUNCTION ON 1/30/87
C OLD MVATIN = NVATIN
C OLD               IF ( DIAMON )                MVATIN = XVATIN / 2
C OLD MVATIN = MAX0( MVATIN-1,0 )
C OLD THES = HESFAC + PIJRIV*(1.0D0+DMIN1(ONED6*MVATIN,1.5D0))
C OLD THES = HESFAC + PIJRIV*(1.0D0-DMIN1(ONED6*MVATIN,1.0D0))
      FHES = 1.0D0 - (AMAX0( KVILAI-1,0 )/AMAX0( NIBLAI-1,1 ))
      FHES = DMAX1( DMIN1( FHES,1.0D0 ),0.0D0 )
      GHES = (MVILNI(ILN,IA)-MIN0( NVILNI(ILN,IA),MVILNI(ILN,IA) )) /
     *       AMAX0( MVILNI(ILN,IA)-1,1 )
      GHES = DMAX1( DMIN1( GHES,1.0D0 ),0.0D0 )
      HHES = DMAX1( DMIN1( GHES*FHES + (1.0D0-GHES)*GHES,1.0D0 ),0.0D0 )
      THES = 4.0D0*PIJRIV*HHES
      IF ( ((IQDS(IV)*DT)+THES) . LT . HESFAC )  THEN
        THES = HESFAC - IQDS(IV)*DT
      END IF
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'AVDCON TCM    02'
      TCM = TCM + THES
 1060 CONTINUE
C-----FIND THE TIME FOR MY VEHICLE TO PASS THE INTERSECTION CONFLICT AT
C-----THE VELOCITY AT THE INTERSECTION CONFLICT FOR ME
      TPASCM = 1.0D+9
      TPASSM = 1.0D+9
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'AVDCON VCM    01'
                    IF ( VCM . LE . 0.0D0 )      GO TO 1070
      HWH    = 0.5D0*WIDMAX
C-----IF THE INTERSECTION CONFLICT IS NOT A MERGE (THE LINKING OUTBOUND
C-----LANE FOR THE INTERSECTION PATHS ARE DIFFERENT) THEN CALCULATE
C-----DISCLM ELSE DISCLM IS ZERO
      DISCLM = 0.0D0
      IF ( LOBL(IX) . NE . LOBL(JP) )            THEN
        IF ( DTANAN . LE . 0.5D0 )               THEN
          DISCLM = DISCLM + 2.0D0*HWM
        ELSE
          DISCLM = DISCLM + HWM/DTANAN
        END IF
        IF ( DSINAN . LE . 0.5D0 )               THEN
          DISCLM = DISCLM + 2.0D0*HWH
        ELSE
          DISCLM = DISCLM + HWH/DSINAN
        END IF
      END IF
      TPASCM = DISCLM/VCM
      TPASSM = LENVAP/VCM
 1070 CONTINUE
C-----START OF LOOP FOR CHECKING FOR THIS INTERSECTION CONFLICT
C[    IF ( KOUNT              .EQ.-2147483647   )STOP 'AVDCON KOUNT  01'
      KOUNT = KOUNT + 1
                    IF ( KOUNT . GT . 50 )       GO TO 9330
C-----IF THE IVCONF VEHICLE HAS NOT SET CONFLICTS THEN HE MAY NOT
C-----PROCEED INTO THE INTERSECTION THUS THERE CAN BE NO INTERSECTION
C-----CONFLICT THUS GO TO 3010 AND SKIP TO THE NEXT INTERSECTION
C-----CONFLICT (THIS ONE IS CLEAR)
                    IF ( IVCONF . EQ . 0 )       GO TO 3010
      IF      ( IPRC(1,IVCONF) . EQ . JP )       THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IVCONF) . EQ . JP )       THEN
        JPRC = 2
      ELSE IF ( IPRC(1,IVCONF) . EQ .  0 )       THEN
        JPRC = 1
        NORC(1,IVCONF) = NVEP1
      ELSE IF ( IPRC(2,NOFC) . EQ .  0 )         THEN
        JPRC = 2
        NORC(2,IVCONF) = NVEP1
      ELSE
        GO TO 9460
      END IF
            IF ( NORC(JPRC,IVCONF) . EQ . NVEP1 )GO TO 3010
C-----IF THE NOFC VEHICLE IS THE IVCONF VEHICLE THEN GO TO 1080 AND
C-----CHECK THE IVCONF VEHICLE
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'AVDCON IVCONF 01'
C[    IF ( NOFC               .EQ.-2147483647   )STOP 'AVDCON NOFC   01'
                    IF ( NOFC . EQ . IVCONF )    GO TO 1080
C-----IF THE NOFC VEHICLE HAS NOT SET CONFLICTS THEN HE MAY NOT PROCEED
C-----INTO THE INTERSECTION THUS HE WILL BLOCK THE IVCONF VEHICLE FROM
C-----PROCEEDING INTO THE INTERSECTION ALSO THUS THERE CAN BE NO
C-----INTERSECTION CONFLICT WITH THE IVCONF VEHICLE THUS GO TO 3010 AND
C-----SKIP TO THE NEXT INTERSECTION CONFLICT (THIS ONE IS CLEAR)
      IF      ( IPRC(1,NOFC) . EQ . JP )         THEN
        JPRC = 1
      ELSE IF ( IPRC(2,NOFC) . EQ . JP )         THEN
        JPRC = 2
      ELSE IF ( IPRC(1,NOFC) . EQ .  0 )         THEN
        JPRC = 1
        NORC(1,NOFC) = NVEP1
      ELSE IF ( IPRC(2,NOFC) . EQ .  0 )         THEN
        JPRC = 2
        NORC(2,NOFC) = NVEP1
      ELSE
        GO TO 9460
      END IF
            IF ( NORC(JPRC,NOFC) . EQ . NVEP1 )  GO TO 3010
C-----SET THE NOFC VEHICLE TO THE NOR VEHICLE FOR THE CURRENT NOFC
C-----VEHICLE
      IF ( NOR(NOFC) . EQ . 0 )                  THEN
        IF ( LPRES(NOFC) . EQ . LOBL(JP) )       THEN
          NOFC = NORC(JPRC,NOFC)
C;        WRITE (TC3,331) TIME,'AVDCON 3 NOFC = NORC(JPRC,NOFC)    ',
C;   *                    IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                    LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                    IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                    IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                    'JM=',INDEX,'N=',NGEOCP(IX),'JPRC=',JPRC
        ELSE
          NOFC = IFVL(JL)
C;        WRITE (TC3,331) TIME,'AVDCON 4 NOFC = IFVL(JL)           ',
C;   *                    IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                    LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                    IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                    IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                    'JL=',JL,'JM=',INDEX,'N=',NGEOCP(IX)
        END IF
      ELSE
        NOFC = NOR(NOFC)
C;      WRITE (TC3,331) TIME,'AVDCON 5 NOFC = NOR(NOFC)          ',
C;   *                  IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                  LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                  IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                  IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                  'JM=',INDEX,'N=',NGEOCP(IX)
      END IF
C-----IF THERE IS A NEW NOFC VEHICLE THEN GO TO 1070 AND CHECK AGAIN
                    IF ( NOFC . GT . 0 )         GO TO 1070
C-----THE OLD NOFC VEHICLE HAD TO BE THE LAST VEHICLE ON THE
C-----INTERSECTION PATH THUS SET THE NOFC VEHICLE TO THE FIRST VEHICLE
C-----ON THE LANE AND GO TO 1070 AND CHECK AGAIN
C[    IF ( JL                 .EQ.-2147483647   )STOP 'AVDCON JL     01'
      NOFC = IFVL(JL)
C;    WRITE (TC3,331) TIME,'AVDCON 6 NOFC = IFVL(JL)           ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JL=',JL,'JM=',INDEX,'N=',NGEOCP(IX)
      GO TO 1070
 1080 CONTINUE
C-----SET THE IVCONF VEHICLES PARAMETERS FOR PREDICTING TIME AND
C-----VELOCITY TO AN INTERSECTION CONFLICT
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'AVDCON IVCONF 02'
      CALL  SPVAS   ( IVCONF,PO,VO,AO,SO,
     *                .FALSE.,.FALSE.,.TRUE.,.FALSE. )
C-----IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVCONF
C-----VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE FALSE
      EVRESP = ( ( IAND( VEHTYP(IV    ),LAVTE ) . EQ . 0 ) . AND .
     *           ( IAND( VEHTYP(IVCONF),LAVTE ) . NE . 0 ) )
C[    IF ( JL                 .EQ.-2147483647   )STOP 'AVDCON JL     02'
      POSSAV = PO
      VELVEH = VO
      ACCVEH = AO
      SLPVEH = SO
      LGEOM4 = LGEOM(4,JL)
      PO = PO + LGEOM4
      CALL  SETDSP  ( IVCONF,IPOS(IVCONF),DBLE( ISPD(IVCONF) ),.FALSE.,
     *                DESSPD                                           )
      JSPD = IDNINT( DESSPD )
      JSPDP = 1
      KPRTM = 0
C-----IF THE IVCONF VEHICLE IS IN THE INTERSECTION THEN GO TO 2010 AND
C-----CONTINUE ELSE SET SOME ADDITIONAL PARAMETERS
                    IF ( MININT(IVCONF) )        GO TO 2010
      IF ( LPRES(IVCONF) . EQ . LIBL(JP) )       THEN
C-----  IVCONF VEHICLE IS ON THE INBOUND LANE FOR THE INTERSECTION PATH
        PO = PO - LGEOM4
      ELSE
C-----  IVCONF VEHICLE IS ON THE OUTBOUND LANE FOR THE INTERSECTION PATH
        PO = PO + DBLE( LENP(JP) - LGEOM(1,LPRES(IVCONF)) )
      END IF
      JSPDP = ISPDP(IVCONF)
                    IF ( JSPD .NE. ISPD(IVCONF) )JSPDP = 1
      KPRTM = IPRTM(IVCONF)
      IF ( IUPDAT(IVCONF) . AND . (KPRTM . GT . 0) )
     *                                           KPRTM = KPRTM + 1
C-----IF THE IVCONF VEHICLE HAS ALREADY SET HIS DESIRED SPEED FOR HIS
C-----INTERSECTION PATH THEN GO TO 2010 ELSE GET ADDITIONAL PARAMETERS
                    IF ( JSPDP . NE . 0 )        GO TO 2010
C[    IF ( JP                 .EQ.-2147483647   )STOP 'AVDCON JP     01'
      MIMP = LIMP(JP)
C[    IF ( JL                 .EQ.-2147483647   )STOP 'AVDCON JL     03'
      JSLIM = ISLIM(ISNA(JL))
      IF ( LOBL(JP) . GT . 0 )                   THEN
        IF ( LPRES(IVCONF) . EQ . LOBL(JP) )     THEN
          JSLIM = ISLIM(ISNA(LOBL(JP)))
        END IF
      END IF
 2010 CONTINUE
C-----FIND ADDITIONAL PARAMETERS FOR THE IVCONF VEHICLE
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'AVDCON IVCONF 03'
      JDCONF = IDRICL(IVCONF)
      JVCONF = IVEHCL(IVCONF)
C[    IF ( JH                 .EQ.-2147483647   )STOP 'AVDCON JH     01'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'AVDCON JNDEX  02'
      P = ICOND(JH,JNDEX) + LGEOM4
      DVH = JSPD
      DCH = P - PO
      TCH = 1.0D0
      HWH = 0.5D0*WIDV(IVEHCL(IVCONF))
      LVH =       LVAP(       IVCONF )
C[    IF ( VCM              .EQ.-2147483647.0 )  STOP 'AVDCON VCM    01'
      SAFF = (SAFDIS+(VCM/SAFSPD))/DCHAR(IDRICL(IV))
      SAFR = SAFF
C-----IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVCONF VEHICLE
C-----IS AN EMERGENCY VEHICLE THEN CHECK SAFF AND SAFR
      IF ( EVRESP )                              THEN
        SAFF = DMAX1( SAFF,EVEHFZ*VCM )
        SAFR = DMAX1( SAFR,EVEHRZ*VCM )
      END IF
      POT = PO
      PT  = P
 2030 CONTINUE
C-----IF THE IVCONF VEHICLE IS INVOLVED IN A MAJOR COLLISION OR IT STOPS
C-----BEFORE GETTING TO THE INTERSECTION CONFLICT THEN IT WILL NEVER
C-----MOVE THUS CHECK DISTANCES
      IF ( ( MAJCOL(IVCONF)                   ) . OR .
     *     ( MAJCLB(IVCONF).AND.(VO.EQ.0.0D0) ) . OR .
     *     ( MAJCLL(IVCONF).AND.(VO.EQ.0.0D0) ) . OR .
     *     ( CKINTB(IVCONF).AND.(VO.EQ.0.0D0) ) . OR .
     *     ( TCH.LE.0.0D0                     ) )THEN
C-----  SET TCM AND TCH FOR TEST ON STATEMENT BEFORE 3010 SO IF THE
C-----  IVCONF VEHICLE HAS AN NORC THEN IT WILL CHECK IT
        TCM = 2.0D0
        IF ( TCH . GT . TCM )                    TCH = 1.0D0
C-----  IF THE VEHICLES FRONT BUMPER PLUS EXTRA SAFETY ZONE HAS ARRIVED
C-----  AT THE INTERSECTION CONFLICT AND THE VEHICLES REAR BUMPER PLUS
C-----  EXTRA SAFETY ZONE HAS NOT PASSED THE INTERSECTION CONFLICT THEN
C-----  SET THAT THE VEHICLE MUST CHECK CONFLICTS BUT MAY NOT BE BLOCKED
        IF ( .NOT. MAJCOL(IV) )                  THEN
          IF ( ( MAJCOL(IVCONF)                   ) . OR .
     *         ( MAJCLB(IVCONF).AND.(VO.EQ.0.0D0) ) . OR .
     *         ( MAJCLL(IVCONF).AND.(VO.EQ.0.0D0) ) )
     *                                           THEN
            IF ( ((POT    +SAFCON*SAFF+HWM) . GE . PT) . AND .
     *           ((POT-LVH-SAFCON*SAFR-HWM) . LE . PT) )
     *                                           THEN
              IF ( MININT(IV) )                  THEN
                DCOND = DBLE( ICOND(JM,JNDEX) )
              ELSE
                DCOND = DBLE( LGEOM(4,IL)+ICOND(JM,JNDEX) )
              END IF
              MAJCON(IV) = .TRUE.
              POSCON(IV) = DMIN1( POSCON(IV),DCOND )
              RESPEV = ( RESPEV . OR . EVRESP )
            END IF
          END IF
        END IF
C-----  IF THE VEHICLES FRONT BUMPER HAS NOT ARRIVED AT THE INTERSECTION
C-----  CONFLICT THEN GO TO 3010 AND SKIP TO THE NEXT INTERSECTION
C-----  CONFLICT
            IF ( (POT    +SAFF+HWM) . LT . PT )  GO TO 3010
C-----  IF THE VEHICLES REAR BUMPER HAS PASSED THE INTERSECTION CONFLICT
C-----  THEN GO TO 2120 AND PROCESS THE NORC VEHICLE
            IF ( (POT-LVH-SAFR-HWM) . GT . PT )  GO TO 2120
C-----  THE IVCONF VEHICLE BLOCKS THE POINT OF INTERSECTION CONFLICT
C-----  THUS CALCULATE A DECEL TO A STOP AT THE STOP LINE OR JUST BEFORE
C-----  THE POINT OF INTERSECTION CONFLICT
        POSVEH = POSOLD + DCM - 2.0D0*HWM - SAFR
        IF ( TCH . GT . 0.0D0 )                  THEN
          POSVEH = POSVEH - 20.0D0
        END IF
        POSVEH = DMIN1( POSEND+1.5D0,POSVEH )
        IF ( ( MININT(IV)                 ) . AND .
     *       ( POSOLD . LE . 2.0D0*XRELMI ) . AND .
     *       ( VELOLD . EQ . 0.0D0        ) )    THEN
          POSVEH = POSOLD + 1.5D0
        END IF
        VELVEH = 0.0D0
        ACCVEH = 0.0D0
        SLPVEH = 0.0D0
        CALL  SLPCFS  ( SLPTMP,IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                            POSVEH,VELVEH,ACCVEH,SLPVEH  )
        IF ( SLPTMP . NE . 0.0D0 )               THEN
          IF ( SLPTMP . LT . SLPBLK )            THEN
            SLPBLK = SLPTMP
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
        END IF
        IF ( .NOT. MAJCOL(IV) )                  THEN
          IF ( ( MAJCOL(IVCONF)                   ) . OR .
     *         ( MAJCLB(IVCONF).AND.(VO.EQ.0.0D0) ) )
     *                                           THEN
C-----      SET THAT THE VEHICLE IS BLOCKED BY A MAJOR COLLISION
            IF ( MININT(IV) )                    THEN
              DCOND = DBLE( ICOND(JM,JNDEX) )
            ELSE
              DCOND = DBLE( LGEOM(4,IL)+ICOND(JM,JNDEX) )
            END IF
            MAJCLB(IV) = .TRUE.
            POSCLB(IV) = DMIN1( POSCLB(IV),DCOND )
C-----      SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----      COLLISION MAY BLOCK THIS VEHICLE
            MAJCON(IV) = .TRUE.
            POSCON(IV) = DMIN1( POSCON(IV),DCOND )
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
        END IF
        GO TO 3010
      END IF
C-----IF THE FRONT BUMPER OF THE IVCONF VEHICLE IS WITHIN SAFF+HWM
C-----DISTANCE OF THE POINT OF INTERSECTION CONFLICT AND IS STOPPED THEN
C-----SET TCH TO 0 AND GO TO 2030 AND CHECK BLOCKAGE
      IF ( (POT+SAFF+HWM) . GE . PT )            THEN
        IF ( VO . LE . VELSTP )                  THEN
          TCH = 0.0D0
          GO TO 2030
        END IF
      END IF
C-----IF THE FRONT BUMPER OF THE IVCONF VEHICLE IS BEYOND THE POINT OF
C-----INTERSECTION CONFLICT THEN CALCULATE THE TIME TO STOP AND THE
C-----POSITION OF THE FRONT BUMPER WHEN IT STOPS
      IF ( POT . GE . PT )                       THEN
        TMAX = 30.0D0
        CALL  TIMSTP  ( VO,AO,SO,TMAX,TS )
                    IF ( TS . EQ . TIMERR )      GO TO 2050
        POSSTP = POT + VO*TS + 0.5D0*AO*TS**2 + ONED6*SO*TS**3
C-----  IF THE REAR BUMPER OF THE IVCONF VEHICLE WHEN IT STOPS IS WITHIN
C-----  SAFR+HWM DISTANCE OF THE POINT OF INTERSECTION CONFLICT THEN SET
C-----  TCH TO 0 AND GO TO 2030 AND CHECK BLOCKAGE
        IF ( (POSSTP-LVH-SAFR-HWM) . LE . PT )   THEN
          TCH = 0.0D0
          GO TO 2030
        END IF
      END IF
 2050 CONTINUE
C-----PREDICT TIME AND VELOCITY TO AN INTERSECTION CONFLICT FOR HIM
      CALL  SNOFCV  ( IVCONF,JP )
      CALL  PREDTV  ( TCH,PCH,VCH,ACH,SCH )
C-----IF THE VEHICLE STOPS BEFORE THE INTERSECTION CONFLICT THEN GO TO
C-----2030 AND CHECK DISTANCES
      IF ( TCH . LE . -999.9D0 )                 THEN
        POT = PCH
        GO TO 2030
      END IF
C-----INCREMENT THE TIME TO THE CONFLICT FOR HIM BY HIS PIJR TIMER
C[    IF ( TCH                .EQ.-2147483647.0 )STOP 'AVDCON TCH    01'
      TCH = TCH + KPRTM*DT
C-----FIND THE TIME FOR HIS VEHICLE TO PASS THE INTERSECTION CONFLICT AT
C-----THE VELOCITY AT THE INTERSECTION CONFLICT FOR HIM
      TPASCH = 1.0D+9
      TPASSH = 1.0D+9
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'AVDCON VCH    01'
                    IF ( VCH . LE . 0.0D0 )      GO TO 2060
C-----IF THE INTERSECTION CONFLICT IS NOT A MERGE (THE LINKING OUTBOUND
C-----LANE FOR THE INTERSECTION PATHS ARE DIFFERENT) THEN CALCULATE
C-----DISCLH ELSE DISCLH IS ZERO
      DISCLH = 0.0D0
      IF ( LOBL(IX) . NE . LOBL(JP) )            THEN
        IF ( DTANAN . LE . 0.5D0 )               THEN
          DISCLH = DISCLH + 2.0D0*HWH
        ELSE
          DISCLH = DISCLH + HWH/DTANAN
        END IF
        IF ( DSINAN . LE . 0.5D0 )               THEN
          DISCLH = DISCLH + 2.0D0*HWM
        ELSE
          DISCLH = DISCLH + HWM/DSINAN
        END IF
      END IF
      TPASCH = DISCLH      /VCH
      TPASSH = LVAP(IVCONF)/VCH
 2060 CONTINUE
C-----FIND THE ERROR IN JUDGMENT
C[    IF ( TCH                .EQ.-2147483647.0 )STOP 'AVDCON TCH    02'
      ERRJUD = DMAX1( 0.0D0,PIJRIV*(TCH-5.0D0)/7.0D0 )
C-----IF THE IVCONF VEHICLES TIME TO THE INTERSECTION CONFLICT IS GT 5
C-----SECONDS AND HE SHOULD FOLLOW THE VEHICLE AHEAD THEN INCREMENT THE
C-----TIME TO THE INTERSECTION CONFLICT FOR HIM BY 0.5D0 SECONDS
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'AVDCON IVCONF 04'
      IF ( (TCH.GT.5.0D0) . AND . IFVA(IVCONF) ) TCH = TCH + 0.5D0
C-----FIND THE TIME FOR THE FRONT ZONE FOR THE IVCONF VEHICLE
C[    IF ( TPASSM             .EQ.-2147483647.0 )STOP 'AVDCON TPASSM 01'
C[    IF ( TPASCM             .EQ.-2147483647.0 )STOP 'AVDCON TPASCM 01'
      TFZ = TPASSM + TPASCM + TLEAD + PIJRIV + 0.5D0*ERRJUD
C-----FIND THE TIME FOR THE REAR ZONE FOR THE IVCONF VEHICLE
C[    IF ( TPASSH             .EQ.-2147483647.0 )STOP 'AVDCON TPASSH 01'
C[    IF ( TPASCH             .EQ.-2147483647.0 )STOP 'AVDCON TPASCH 01'
      TRZ = TPASSH + TPASCH + TLAG  + PIJRIV + 0.5D0*ERRJUD + TPASCM
C-----IF THE INTERSECTION CONFLICT IS NOT A MERGE (THE LINKING OUTBOUND
C-----LANE FOR THE INTERSECTION PATHS ARE DIFFERENT) THEN GO TO 2080
            IF ( LOBL(IX) . NE . LOBL(JP) )      GO TO 2080
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'AVDCON TCM    03'
                    IF ( TCM    . GT . TCH    )  GO TO 2070
C-----THIS VEHICLE AND THE IVCONF VEHICLE ARE MERGING AT THE
C-----INTERSECTION CONFLICT INTO THE SAME LANE AND THIS VEHICLE WILL
C-----ARRIVE BEFORE THE IVCONF VEHICLE THUS MAX THE TIME FOR THE FRONT
C-----ZONE FOR THE IVCONF VEHICLE WITH THE TIME REQUIRED FOR THIS
C-----VEHICLE TO PASS THROUGH THE INTERSECTION CONFLICT PLUS THE TIME
C-----REQUIRED FOR THIS VEHICLE TO PASS THE CAR-FOLLOWING DISTANCE PLUS
C-----THE ERROR IN JUDGMENT
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'AVDCON VCH    02'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'AVDCON VCM    02'
      VELREL = VCM - VCH
C-----FIND THE CONSERVATIVE CAR FOLLOWING DISTANCE
      CARDIS = DMAX1( XRELMX,1.7D0*VCM )/DCHAR(IDRICL(IV))
      IF ( VELREL . LT . 0.0D0 )                 THEN
C-----  VCM LT VCH
C-----  FIND THE TIME FOR HIM TO SLOW DOWN TO ME AT -CRISLP
C-----  VCM = VCH + ACH*T + 0.5D0*(-CRISLP)*T**2
C-----  (-0.5D0*CRISLP)*T**2 + (ACH)*T + (VCH-VCM) = 0
        A = -0.5D0*CRISLP
        B = ACH
        C = VCH-VCM
        TMAX = 30.0D0
        CALL  TMQUAD  ( A,B,C,TMAX,T )
        IF ( T . EQ . TIMERR )                   THEN
          IF ( C . LE . VSMALL )                 T = 0.0D0
        END IF
        IF ( T . EQ . TIMERR )                   THEN
          CARDIS = CARDIS + 4.0D0*VELREL**2/DCHAR(IDRICL(IV))
        ELSE
          DSLOWH = VCH*T + 0.5D0*ACH*T**2 - ONED6*CRISLP*T**3
          DSLOWM = VCM*T + 0.5D0*ACM*T**2 + ONED6*SCM   *T**3
          CARDIS = CARDIS + DMAX1( DSLOWH-DSLOWM,0.0D0 )
        END IF
      END IF
      IF ( VCM . LE . 0.0D0 )                    THEN
        TPASSC = 1.0D+9
      ELSE
        TPASSC = CARDIS/VCM
      END IF
C[    IF ( TPASSM             .EQ.-2147483647.0 )STOP 'AVDCON TPASSM 02'
C[    IF ( TPASCM             .EQ.-2147483647.0 )STOP 'AVDCON TPASCM 02'
      TMP = TPASSM + TPASCM + TPASSC + 0.5D0*ERRJUD
C[    IF ( TFZ                .EQ.-2147483647.0 )STOP 'AVDCON TFZ    01'
      TFZ = DMAX1( TFZ,TMP )
 2070 CONTINUE
C[    IF ( TCH                .EQ.-2147483647.0 )STOP 'AVDCON TCH    03'
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'AVDCON TCM    04'
                    IF ( TCH . GT . TCM )        GO TO 2080
C-----THIS VEHICLE AND THE IVCONF VEHICLE ARE MERGING AT THE
C-----INTERSECTION CONFLICT INTO THE SAME LANE AND THE IVCONF VEHICLE
C-----WILL ARRIVE BEFORE THIS VEHICLE THUS MAX THE TIME FOR THE REAR
C-----ZONE FOR THE IVCONF VEHICLE WITH THE TIME REQUIRED FOR THE IVCONF
C-----VEHICLE TO PASS THROUGH THE INTERSECTION CONFLICT PLUS THE TIME
C-----REQUIRED FOR THE IVCONF VEHICLE TO PASS THE CAR-FOLLOWING
C-----DISTANCE PLUS THE ERROR IN JUDGMENT
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'AVDCON VCH    03'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'AVDCON VCM    03'
      VELREL = VCH - VCM
C-----FIND THE CONSERVATIVE CAR FOLLOWING DISTANCE
      CARDIS = DMAX1( XRELMX,1.7D0*VCH )/DCHAR(IDRICL(IV))
      IF ( VELREL . LT . 0.0D0 )                 THEN
C-----  VCH LT VCM
C-----  FIND THE TIME FOR ME TO SLOW DOWN TO HIM AT -CRISLP
C-----  VCH = VCM + ACM*T + 0.5D0*(-CRISLP)*T**2
C-----  (-0.5D0*CRISLP)*T**2 + (ACM)*T + (VCM-VCH) = 0
        A = -0.5D0*CRISLP
        B = ACM
        C = VCM-VCH
        TMAX = 30.0D0
        CALL  TMQUAD  ( A,B,C,TMAX,T )
        IF ( T . EQ . TIMERR )                   THEN
          IF ( C . LE . VSMALL )                 T = 0.0D0
        END IF
        IF ( T . EQ . TIMERR )                   THEN
          CARDIS = CARDIS + 4.0D0*VELREL**2/DCHAR(IDRICL(IV))
        ELSE
          DSLOWM = VCM*T + 0.5D0*ACM*T**2 - ONED6*CRISLP*T**3
          DSLOWH = VCH*T + 0.5D0*ACH*T**2 + ONED6*SCH   *T**3
          CARDIS = CARDIS + DMAX1( DSLOWM-DSLOWH,0.0D0 )
        END IF
      END IF
      IF ( VCH . LE . 0.0D0 )                    THEN
        TPASSC = 1.0D+9
      ELSE
        TPASSC = CARDIS/VCH
      END IF
C[    IF ( TPASSH             .EQ.-2147483647.0 )STOP 'AVDCON TPASSH 02'
C[    IF ( TPASCH             .EQ.-2147483647.0 )STOP 'AVDCON TPASCH 02'
C[    IF ( TPASCM             .EQ.-2147483647.0 )STOP 'AVDCON TPASCM 02'
      TMP = TPASSH + TPASCH + TPASSC + 0.5D0*ERRJUD + TPASCM
C[    IF ( TRZ                .EQ.-2147483647.0 )STOP 'AVDCON TRZ    01'
      TRZ = DMAX1( TRZ,TMP )
C-----SET SLPNOF TO CAR-FOLLOW OR STOP BEHIND THE IVCONF VEHICLE
C[    IF ( DCH                .EQ.-2147483647.0 )STOP 'AVDCON DCH    01'
C[    IF ( DCM                .EQ.-2147483647.0 )STOP 'AVDCON DCM    01'
      POSVEH = DCM - DCH - LVAP(IVCONF) - XRELMI
                    IF ( POSVEH . LE . 0.0D0 )   GO TO 2080
      POSVEH = POSOLD + POSVEH
C[    IF ( ACCVEH             .EQ.-2147483647.0 )STOP 'AVDCON ACCVEH 01'
C[    IF ( SLPVEH             .EQ.-2147483647.0 )STOP 'AVDCON SLPVEH 01'
C[    IF ( VELVEH             .EQ.-2147483647.0 )STOP 'AVDCON VELVEH 01'
      CALL  SLPCFS  ( SLPTMP,IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                          POSVEH,VELVEH,ACCVEH,SLPVEH  )
      IF ( SLPTMP . NE . 0.0D0 )                 THEN
        IF ( SLPTMP . LT . SLPNOF )              THEN
          SLPNOF = SLPTMP
          RESPEV = ( RESPEV . OR . EVRESP )
        END IF
      END IF
 2080 CONTINUE
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'AVDCON VCH    04'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'AVDCON VCM    04'
                    IF ( VCM - VCH )             2090 , 2110 , 2100
 2090 CONTINUE
C-----THIS VEHICLE WILL BE TRAVELING SLOWER THAN THE IVCONF VEHICLE AT
C-----THE INTERSECTION CONFLICT THUS MAX THE TIME FOR THE FRONT ZONE FOR
C-----THE IVCONF VEHICLE WITH THE TIME REQUIRED FOR THE IVCONF VEHICLE
C-----TO REDUCE ITS SPEED TO MY SPEED PLUS THIS DRIVERS REACTION TIME
C-----MULTIPLIED BY THE COSINE OF THE INTERSECTION CONFLICT ANGLE RAISED
C-----TO THE 4TH POWER
C-----AFACT(00)=1.00 (10)=0.94 (20)=0.78 (30)=0.56 (40)=0.34 (50)=0.17
      SLOPE = -0.75D0*SLPMAX*DCHAR(JDCONF)
C[    IF ( ACH                .EQ.-2147483647.0 )STOP 'AVDCON ACH    01'
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'AVDCON VCH    05'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'AVDCON VCM    05'
C-----VCM = VCH + ACH*TCRASH + 0.5*SLOPE*TCRASH**2
C-----0.5*SLOPE*TCRASH**2 + ACH*TCRASH + (VCH-VCM) = 0
      A = 0.5D0*SLOPE
      B = ACH
      C = VCH - VCM
      TMAX  = 30.0D0
      CALL  TMQUAD  ( A,B,C,TMAX,TCRASH )
                    IF ( TCRASH . EQ . TIMERR )  GO TO 2110
      AFACT = DCOSAN**4
      TMP = AFACT*(TCRASH+PIJRIV)
C[    IF ( TFZ                .EQ.-2147483647.0 )STOP 'AVDCON TFZ    02'
      TFZ = DMAX1( TFZ,TMP )
      GO TO 2110
 2100 CONTINUE
C-----THIS VEHICLE WILL BE TRAVELING FASTER THAN THE IVCONF VEHICLE AT
C-----THE INTERSECTION CONFLICT THUS MAX THE TIME FOR THE REAR ZONE FOR
C-----THE IVCONF VEHICLE WITH THE TIME REQUIRED FOR THIS VEHICLE TO
C-----REDUCE ITS SPEED TO THE IVCONF VEHICLE SPEED PLUS THIS DRIVERS
C-----REACTION TIME MULTIPLIED BY THE COSINE OF THE INTERSECTION
C-----CONFLICT ANGLE RAISED TO THE 4TH POWER
C-----AFACT(00)=1.00 (10)=0.94 (20)=0.78 (30)=0.56 (40)=0.34 (50)=0.17
      SLOPE = -0.75D0*SLPMAX*DCHAR(IDRICL(IV))
C[    IF ( ACM                .EQ.-2147483647.0 )STOP 'AVDCON ACM    01'
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'AVDCON VCH    06'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'AVDCON VCM    06'
C-----VCH = VCM + ACM*TCRASH + 0.5*SLOPE*TCRASH**2
C-----0.5*SLOPE*TCRASH**2 + ACM*TCRASH + (VCM-VCH) = 0
      A = 0.5D0*SLOPE
      B = ACM
      C = VCM - VCH
      TMAX  = 30.0D0
      CALL  TMQUAD  ( A,B,C,TMAX,TCRASH )
                    IF ( TCRASH . EQ . TIMERR )  GO TO 2110
      AFACT = DCOSAN**4
      TMP = AFACT*(TCRASH+PIJRIV)
C[    IF ( TRZ                .EQ.-2147483647.0 )STOP 'AVDCON TRZ    02'
      TRZ = DMAX1( TRZ,TMP )
 2110 CONTINUE
C-----IF THE IVCONF VEHICLE IS DECELERATING AT THE POINT OF CONFLICT
C-----THEN INCREASE TRZ
      IF ( ACH . LT . 0.0D0 )                    THEN
        TRZ = TRZ - 0.1D0*ACH
      END IF
C-----IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVCONF VEHICLE
C-----IS AN EMERGENCY VEHICLE THEN CHECK TFZ AND TRZ
      IF ( EVRESP )                              THEN
        TFZ = DMAX1( TFZ,EVEHFZ )
        TRZ = DMAX1( TRZ,EVEHRZ )
      END IF
C-----FIND THE TIME THE FRONT ZONE AND REAR ZONE SHOULD ARRIVE AT THE
C-----INTERSECTION CONFLICT
C[    IF ( TCH                .EQ.-2147483647.0 )STOP 'AVDCON TCH    04'
C[    IF ( TFZ                .EQ.-2147483647.0 )STOP 'AVDCON TFZ    03'
C[    IF ( TRZ                .EQ.-2147483647.0 )STOP 'AVDCON TRZ    04'
      TFZ = TCH - TFZ
C[    IF ( TRZ                .EQ.-2147483647.0 )STOP 'AVDCON TRZ    03'
      TRZ = TCH + TRZ
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 102
C4                  IF ( TIME . LT . TPRINT )    GO TO 102
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'AVDCON IVCONF 04'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'AVDCON JNDEX  06'
C[    IF ( DCH                .EQ.-2147483647.0 )STOP 'AVDCON DCH    02'
C[    IF ( DCM                .EQ.-2147483647.0 )STOP 'AVDCON DCM    02'
C[    IF ( DVH                .EQ.-2147483647.0 )STOP 'AVDCON DVH    01'
C[    IF ( DVM                .EQ.-2147483647.0 )STOP 'AVDCON DVM    01'
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'AVDCON TCM    05'
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'AVDCON VCH    07'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'AVDCON VCM    07'
C4    WRITE (6,701) JNDEX,IV,TCM,VCM,DVM,DCM,IVCONF,TFZ,TCH,TRZ,VCH,DVH,
C4   *              DCH
C4102 CONTINUE

C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'AVDCON IVCONF 05'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'AVDCON JNDEX  07'
C[    IF ( SA                 .EQ.-2147483647   )STOP 'AVDCON SA     01'
C[    IF ( SLN                .EQ.-2147483647   )STOP 'AVDCON SLN    01'
C[    IF ( SP                 .EQ.-2147483647   )STOP 'AVDCON SP     01'
C[    IF ( ACH                .EQ.-2147483647.0 )STOP 'AVDCON ACH    02'
C[    IF ( ACM                .EQ.-2147483647.0 )STOP 'AVDCON ACM    02'
C[    IF ( DCH                .EQ.-2147483647.0 )STOP 'AVDCON DCH    03'
C[    IF ( DCM                .EQ.-2147483647.0 )STOP 'AVDCON DCM    03'
C[    IF ( DVH                .EQ.-2147483647.0 )STOP 'AVDCON DVH    02'
C[    IF ( DVM                .EQ.-2147483647.0 )STOP 'AVDCON DVM    02'
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'AVDCON TCM    06'
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'AVDCON VCH    08'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'AVDCON VCM    08'
C     IF ( LCONTV(IV) . GE . LCSIGX )          THEN
C       JSISET = ISISET(ICAMPC,IBLN(IL))
C     ELSE
C       JSISET = 0
C     END IF
C     FCLEAR = TFZ - TCM
C     RCLEAR = TCM - TRZ
C     WRITE (TC3,601) IQ(IV),TIME,'AVDCON',SA,SLN,SP,ITURN(IV),ICONTR,
C    *                 LCONTV(IV),JSISET,JNDEX,TCM,VCM,ACM,DVM,DCM,
C    *                 IQ(IVCONF),TFZ,TCH,TRZ,VCH,ACH,DVH,DCH,FCLEAR,
C    *                 RCLEAR
C-----IF THE IVCONF VEHICLE IS FORCED TO GO OR THE IVCONF VEHICLE IS
C-----FORCED TO RUN THE RED SIGNAL THEN IGNORE THE IVCONF VEHICLE
      IF ( VMSASM(IVCONF) . GT . 0 )             THEN
        LVMSGO = ( IVMSMG(VMSASM(IVCONF)) . EQ . VMSMGO )
        LVMSRR = ( IVMSMG(VMSASM(IVCONF)) . EQ . VMSMRR )
      ELSE
        LVMSGO = .FALSE.
        LVMSRR = .FALSE.
      END IF
      IGNORE = ( ( MFGOF(IVCONF)                                ) .OR.
     *           ( ( FRRTIM(IVCONF) . GT . 0.0D0              ) .AND.
     *             ( TIME .GE.  FRRTIM(IVCONF)                ) .AND.
     *             ( TIME .LE. (FRRTIM(IVCONF)+FRRATM(IVCONF))) ) .OR.
     *           ( LVMSGO                                       ) .OR.
     *           ( LVMSRR                                       ) )
      IF ( .NOT. IGNORE )                        THEN
C-----  THE IVCONF VEHICLE SHOULD NOT BE IGNORED
C-----  SET TIM TO GIVE PRIORITY TO STRAIGHT VEHICLES (U=1,L=2,S=3,R=4)
C-----  SET TIM FOR ME=ST    AND HIM=ST    -> TCH
C-----  SET TIM FOR ME=ST    AND HIM=LT/RT -> TCH+1.5 (NEWSSG TCH-1.0)
C-----  SET TIM FOR ME=LT/RT AND HIM=ST    -> TCH-1.5
C-----  SET TIM FOR ME=LT/RT AND HIM=LT/RT -> TCH
        IF ( ITURN(IV) . EQ . ITURNS )           THEN
C[        IF ( IVCONF         .EQ.-2147483647   )STOP 'AVDCON IVCONF 06'
          IF ( ITURN(IVCONF) . EQ . ITURNS )     THEN
C[          IF ( TCH          .EQ.-2147483647.0 )STOP 'AVDCON TCH    06'
            TIM = TCH
          ELSE
            IF ( NEWSSG )                        THEN
C[            IF ( TCH        .EQ.-2147483647.0 )STOP 'AVDCON TCH    07'
              TIM = TCH - 1.0D0
            ELSE
C[            IF ( TCH        .EQ.-2147483647.0 )STOP 'AVDCON TCH    08'
              TIM = TCH + 1.5D0
            END IF
          END IF
        ELSE
C[        IF ( IVCONF         .EQ.-2147483647   )STOP 'AVDCON IVCONF 07'
          IF ( ITURN(IVCONF) . EQ . ITURNS )     THEN
C[          IF ( TCH          .EQ.-2147483647.0 )STOP 'AVDCON TCH    09'
            TIM = TCH - 1.5D0
          ELSE
C[          IF ( TCH          .EQ.-2147483647.0 )STOP 'AVDCON TCH    10'
            TIM = TCH
          END IF
        END IF
C-----  IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVCONF
C-----  VEHICLE IS AN EMERGENCY VEHICLE THEN CHECK TIM
        IF ( EVRESP )                            THEN
          TIM = TCH - 5.0D0
        END IF
C-----  IF THE TIME TO THE INTERSECTION CONFLICT FOR ME FALLS BETWEEN
C-----  THE TIME THE FRONT ZONE OF THE IVCONF VEHICLE SHOULD ARRIVE AT
C-----  THE INTERSECTION CONFLICT AND THE TIME THE REAR ZONE OF THE
C-----  IVCONF VEHICLE SHOULD ARRIVE AT THE INTERSECTION CONFLICT THEN
C-----  SET THE SLPCON TO CORRECT THE VEHICLE TRAJECTORY
        IF ( (TCM-TFZ)*(TCM-TRZ).LE.0.0D0 )      THEN
                    IF ( TCM . LE . 0.0D0 )      GO TO 2115
          IF ( MININT(IVCONF) )                  THEN
C-----      THE IVCONF VEHICLE IS IN THE INTERSECTION THUS IF IT'S
C-----      POSITION IS GREATER THAN 4 FEET INTO THE INTERSECTION AND
C-----      STOPPED THEN GO TO 2030 AND CHECK FOR BLOCKAGE
            IF ( ( IPOS  (IVCONF) .GT. 4.0D0  ) . AND .
     *           ( IVEL  (IVCONF) .LE. VELSTP ) )THEN
              TCH = 0.0D0
C-----        IF THE POINT OF INTERSECTION CONFLICT IS A MERGE (THE
C-----        LINKING OUTBOUND LANE FOR THE INTERSECTION PATHS ARE THE
C-----        SAME) THEN RESET POT AND PT FOR A MERGE
              IF ( LOBL(IX) . EQ . LOBL(JP) )    THEN
                POT = POSSAV
                PT  = ICOND(JH,JNDEX)
              END IF
              GO TO 2030
            END IF
          ELSE
C-----      THE IVCONF VEHICLE IS ON AN INBOUND LANE
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVCONF
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN GO TO 2113 AND YIELD TO
C-----      THE EMERGENCY VEHICLE
            IF ( EVRESP )                        THEN
              RESPEV = ( RESPEV . OR . EVRESP )
              GO TO 2113
            END IF
C-----      IF THE IVCONF VEHICLE IS STOPPED AT THE STOP LINE AND MY
C-----      LANE CONTROL IS NOT SIGNAL CONTROLLED AND MY LANE CONTROL IS
C-----      LESS THAN HIS OR HE IS ON THE LIST OF VEHICLE STOPPED AT THE
C-----      STOP LINE THEN GO TO 2115 AND PROCESS THE NEXT VEHICLE
            IF ( ( MATSTL(IVCONF)           ) . AND .
     *           ( LCONTV(IV) . LT . LCSIGX ) )  THEN
              IF ( LCONTV(IV) . LT . GETLCV ( IVCONF,LPRES(IVCONF) ) )
     *                                           THEN
                GO TO 2115
              END IF
              DO 2112  IVATIN = 1 , NVATIN
              KV = LVATIN(IVATIN)
                    IF ( IV     . EQ . KV )      GO TO 2115
                    IF ( IVCONF . EQ . KV )      GO TO 2115
              IF ( DIAMON )                      THEN
C-----          SKIP IF VEHICLES ARE ON OPPOSITE SIDES OF THE DIAMOND
                IF ( ( IA              . LE . 4 ) . AND .
     *               ( ISNA(LPRES(KV)) . GE . 5 ) )
     *                                           THEN
                  GO TO 2112
                END IF
                IF ( ( IA              . GE . 5 ) . AND .
     *               ( ISNA(LPRES(KV)) . LE . 4 ) )
     *                                           THEN
                  GO TO 2112
                END IF
              END IF
              IF ( MAJCLB(KV) . OR . MAJCLL(KV) )GO TO 2115
 2112         CONTINUE
            END IF
          END IF
 2113     CONTINUE
C-----    CALCULATE THE SLOPE FOR THIS VEHICLE TO ARRIVE AT THE POINT OF
C-----    INTERSECTION CONFLICT AT TIME TCM
C[        IF ( DCM            .EQ.-2147483647.0 )STOP 'AVDCON DCM    04'
          SLPTCM = 6.0D0*(DCM-VELOLD*TCM-0.5D0*ACCOLD*TCM**2)/TCM**3
C[        IF ( TIM            .EQ.-2147483647.0 )STOP 'AVDCON TIM    01'
          IF ( TCM . LE . TIM )                  THEN
C-----      THIS VEHICLE IS TRYING TO GO IN FRONT OF THE IVCONF VEHICLE
            IF ( TFZ . LE . 0.0D0 )              THEN
C-----        THIS VEHICLE IS TRYING TO GO IN FRONT OF THE IVCONF
C-----        VEHICLE AND THE TIME THE FRONT ZONE OF THE IVCONF VEHICLE
C-----        SHOULD ARRIVE AT THE INTERSECTION CONFLICT IS LE 0 THUS
C-----        THE FRONT ZONE OF THE IVCONF VEHICLE HAS ALREADY PASSED
C-----        THE POINT OF INTERSECTION CONFLICT THUS ACCELERATE AS MUCH
C-----        AS POSSIBLE AND GO TO 2115 AND PROCESS THE NEXT VEHICLE
              SLPTFZ = 6.0D0*CRISLP
            ELSE
C-----        THIS VEHICLE IS TRYING TO GO IN FRONT OF THE IVCONF
C-----        VEHICLE AND THE TIME THE FRONT ZONE OF THE IVCONF VEHICLE
C-----        SHOULD ARRIVE AT THE INTERSECTION CONFLICT IS GT 0 THUS
C-----        THE FRONT ZONE OF THE IVCONF VEHICLE HAS NOT PASSED THE
C-----        POINT OF INTERSECTION CONFLICT THUS THIS VEHICLE CAN GO IN
C-----        FRONT OF THE IVCONF VEHICLE THUS CALCULATE THE SLOPE FOR
C-----        THIS VEHICLE TO ARRIVE AT THE POINT OF INTERSECTION
C-----        CONFLICT AT TIME TFZ AND GO TO 2115 AND PROCESS THE NEXT
C-----        VEHICLE
C[            IF ( DCM        .EQ.-2147483647.0 )STOP 'AVDCON DCM    05'
              SLPTFZ = 6.0D0*(DCM-VELOLD*TFZ-0.5D0*ACCOLD*TFZ**2)/TFZ**3
            END IF
            SLPTMP = DMAX1( SLPTFZ-SLPTCM,0.0D0 )
            IF ( SLPTMP . GT . 0.0D0 )           THEN
              IF ( IVPV . EQ . 0 )               THEN
                IF ( SLPTMP . GT . SLPCON )      THEN
                  SLPCON = SLPTMP
                  RESPEV = ( RESPEV . OR . EVRESP )
                END IF
              ELSE
                IF ( VELOLD . LT . DESVEL )      THEN
C-----            FIND THE CONSERVATIVE CAR FOLLOWING DISTANCE
                  IF ( RELVEL . GE . 0.0D0 )     THEN
C-----              THE PREVIOUS VEHICLE IS GOING FASTER THAN THIS
C-----              VEHICLE
                    CARDIS =
     *                DMAX1( XRELMX,1.7D0*PVVEL                 )
     *                / DCHAR(IDRICL(IV))
                  ELSE
C-----              THE PREVIOUS VEHICLE IS GOING SLOWER THAN THIS
C-----              VEHICLE
                    CARDIS =
     *                DMAX1( XRELMX,1.7D0*PVVEL+4.0D0*RELVEL**2 )
     *                / DCHAR(IDRICL(IV))
                  END IF
C-----            IF THE VEHICLE IS FURTHER THAN CARDIS FROM THE
C-----            PREVIOUS VEHICLE SET SLPCON
                  IF ( RELPOS . GT . CARDIS )    THEN
                    IF ( SLPTMP . GT . SLPCON )  THEN
                      SLPCON = SLPTMP
                      RESPEV = ( RESPEV . OR . EVRESP )
                    END IF
                  END IF
                END IF
              END IF
            END IF
            GO TO 2115
          ELSE
                    IF ( TRZ . LE . 0.0D0 )      GO TO 2115
C-----      THIS VEHICLE IS TRYING TO GO BEHIND THE IVCONF VEHICLE THUS
C-----      CALCULATE THE SLOPE FOR THIS VEHICLE TO ARRIVE AT THE POINT
C-----      OF INTERSECTION CONFLICT AT TIME TRZ AND IF NEGATIVE THEN GO
C-----      TO 3030 AND USE THE SLOPE ELSE GO TO 2115 AND PROCESS THE
C-----      NEXT VEHICLE
C[          IF ( DCM          .EQ.-2147483647.0 )STOP 'AVDCON DCM    06'
            SLPTRZ = 6.0D0*(DCM-VELOLD*TRZ-0.5D0*ACCOLD*TRZ**2)/TRZ**3
C[          IF ( SLPTCM       .EQ.-2147483647.0 )STOP 'AVDCON SLPTCM 03'
            SLPTMP = DMIN1( 4.5D0*(SLPTRZ-SLPTCM),0.0D0 )
            IF ( SLPTMP . LT . 0.0D0 )           THEN
              SLPCON = SLPTMP
              RESPEV = ( RESPEV . OR . EVRESP )
              GO TO 3030
            END IF
            GO TO 2115
          END IF
        END IF
      END IF
 2115 CONTINUE
      IF ( ( MININT(IVCONF)               ) . AND .
     *     ( IPOS  (IVCONF) . GE . 4.0D0  ) . AND .
     *     ( IVEL  (IVCONF) . LE . VELSTP ) )    THEN
        TCH = 0.0D0
C-----  IF THE POINT OF INTERSECTION CONFLICT IS A MERGE (THE LINKING
C-----  OUTBOUND LANE FOR THE INTERSECTION PATHS ARE THE SAME) THEN
C-----  RESET POT AND PT FOR A MERGE
        IF ( LOBL(IX) . EQ . LOBL(JP) )          THEN
          POT = POSSAV
          PT  = ICOND(JH,JNDEX)
        END IF
        GO TO 2030
      END IF
 2120 CONTINUE
C-----SET THE NOFC VEHICLE TO THE IVCONF VEHICLE AND SET THE IVCONF
C-----VEHICLE TO THE NEXT VEHICLE THAT SHOULD HAVE TO CLEAR THE SAME
C-----INTERSECTION CONFLICT
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'AVDCON IVCONF 08'
      NOFC = IVCONF
C;    WRITE (TC3,331) TIME,'AVDCON 7 NOFC = IVCONF             ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JM=',INDEX,'N=',NGEOCP(IX)
      IF      ( IPRC(1,NOFC) . EQ . JP )         THEN
        JPRC = 1
      ELSE IF ( IPRC(2,NOFC) . EQ . JP )         THEN
        JPRC = 2
      ELSE
        GO TO 9460
      END IF
      IVCONF = NORC(JPRC,NOFC)
C-----IF THERE IS ANOTHER VEHICLE THAT HAS TO CLEAR THE SAME
C-----INTERSECTION CONFLICT AND THIS VEHICLE HAS TO GO BEHIND THE LAST
C-----IVCONF VEHICLE THEN GO TO 1070 AND CHECK THE NEW IVCONF VEHICLE
C[    IF ( TCH                .EQ.-2147483647.0 )STOP 'AVDCON TCH    11'
      IF ( (IVCONF.GT.0) .AND. (TCM.GT.TCH) )    GO TO 1070
C-----END OF GEOMETRIC CONFLICTING PATH LOOP
 3010 CONTINUE
 3020 CONTINUE
C-----IF SLPBLK, SLPCON, SLPLCH, AND SLPNOF ARE NOT SET THEN RETURN
      IF ( ( SLPBLK . EQ . 0.0D0 ) . AND .
     *     ( SLPCON . EQ . 0.0D0 ) . AND .
     *     ( SLPLCH . EQ . 0.0D0 ) . AND .
     *     ( SLPNOF . EQ . 0.0D0 ) )             RETURN
 3030 CONTINUE
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER DT SECONDS
      CALL  ACDCP   ( .TRUE. )
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9330 CONTINUE
      CALL  ABORTR  ( 'STOP 933 - INFINITE LOOP - AVDCON' )
      STOP  933
 9460 CONTINUE
      CALL  ABORTR  ( 'STOP 946 - INVALID IPRC/NORC - AVDCON' )
      STOP  946
      END                                                               AVDCON
C
C
C
      SUBROUTINE CHKCOL
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'CONCHK'
      INCLUDE 'CONFLT'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INTEGER           I,IANGLE,INDEX,IVCONF,JH,JL,JM,JNDEX,JP,JPRC,JV,
     *                  KOUNT,MIBL,MOBL,NOFC
      DOUBLE PRECISION  ACCVEH,CLEARD,CLEART,DCH,DCM,DCOSAN,DFH,DFM,
     *                  DISCLH,DISCLM,DRH,DRM,DSINAN,DTANAN,HWH,HWM,LVH,
     *                  LVM,MDH,MDM,PAD,PM,POM,POP,POSADD,POSJRB,POSNRB,
     *                  POSREL,POSVEH,SLPVEH,VELREL,VELVEH
C
C-----SUBROUTINE CHKCOL CHECKS FOR INTERSECTION COLLISIONS WHILE A
C-----VEHICLE IS ON AN INTERSECTION PATH
C
C-----OLD POS/VEL/ACC/SLP ARE USED SO THE COLLISION WOULD HAVE ALREADY
C-----OCCURRED
C
C[    I          = -2147483647
C[    INDEX      = -2147483647
C[    IVCONF     = -2147483647
C[    JH         = -2147483647
C[    JL         = -2147483647
C[    JM         = -2147483647
C[    JNDEX      = -2147483647
C[    JP         = -2147483647
C[    JPRC       = -2147483647
C[    KOUNT      = -2147483647
C[    NOFC       = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CHKCOL'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      HWM = 0.5D0*WIDV(IVEHCL(IV))
      LVM = LENVAP
C-----CHECK THE LAST VEHICLE ON ALL INTERSECTIONS PATHS FROM THIS
C-----LANE OR THE LAST VEHICLE ON THE LINKING OUTBOUND LANE FOR THE
C-----INTERSECTION PATH FROM THIS LANE IF THERE IS NO VEHICLE ON THE
C-----INTERSECTION PATH (POSSIBLY A LONG VEHICLE) WHOSE REAR BUMPER IS
C-----STILL ON THIS LANE OR THAT HAS NOT TRAVELED FAR ENOUGH TO CLEAR
C-----THIS VEHICLE
      POSNRB = POSNEW - LENVAP
      MIBL   = LIBL(IP)
      DO  I = 1 , NPINT(MIBL)
        JP = LINTP(I,MIBL)
C-----  IF THE INTERSECTION PATH JP IS INTERSECTION PATH IP THEN SKIP
                    IF ( JP . EQ . IP )          CYCLE
        JV = ILVP(JP)
        IF ( JV . EQ . 0 )                       THEN
          MOBL = LOBL(JP)
                    IF ( MOBL . EQ . 0 )         CYCLE
          JV = ILVL(MOBL)
                    IF ( JV . EQ . 0         )   CYCLE
                    IF ( JP . NE . LPREV(JV) )   CYCLE
C-----    CHECK IF THE REAR BUMPER OF THE LAST VEHICLE ON INTERSECTION
C-----    PATH JP LINKING OUTBOUND LANE HAS NOT TRAVELED FAR ENOUGH TO
C-----    CLEAR THIS VEHICLE
          CALL  SPVAS   ( JV,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                    .FALSE.,.FALSE.,.FALSE.,.TRUE.  )
          POSJRB = POSVEH - LVAP(JV)
          IF ( POSJRB.GE.DBLE( LGEOM(1,MOBL) ) ) CYCLE
          POSADD = DBLE( LENP(JP) - LGEOM(1,MOBL) )
        ELSE
C-----    CHECK IF THE REAR BUMPER OF THE LAST VEHICLE ON INTERSECTION
C-----    PATH JP HAS NOT TRAVELED FAR ENOUGH TO CLEAR THIS VEHICLE
          CALL  SPVAS   ( JV,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                    .FALSE.,.FALSE.,.FALSE.,.TRUE.  )
          POSADD = 0
        END IF
        POSVEH = POSVEH + POSADD
        POSJRB = POSVEH - LVAP(JV)
        HWH = 0.5D0*WIDV(IVEHCL(JV))
C-----  REDUCE CLEARD BY 1 FT TO ACCOUNT FOR ANGLE AND HARD COLLISION
        CLEARD = HWM + HWH - 1.0D0
        CALL  CCLEAR  ( IP,JP,CLEARD,.TRUE.,CLEART )
                    IF ( CLEART . EQ . 0.0D0 )   CYCLE
                    IF ( POSNRB . GT . CLEART )  CYCLE
                    IF ( POSJRB . GT . CLEART )  CYCLE
        IF ( POSNEW . GT . POSVEH )              THEN
          IF ( POSVEH . GE . POSNRB )            THEN
C-----      VEHICLE IV IS AHEAD OF VEHICLE JV AND VEHICLE JV'S FRONT
C-----      BUMPER IS AHEAD OF VEHICLE IV'S REAR BUMPER THUS THERE IS A
C-----      COLLISION
C-----      PRINT THE COLLISION INFORMATION
            VELREL = VELNEW - VELVEH
            POSREL = POSNRB - POSVEH
            CALL  BANGS   ( 5,JV,VELREL,POSREL,POSADD )
          END IF
        ELSE
          IF ( POSNEW . GE . POSJRB )            THEN
C-----      VEHICLE JV IS AHEAD OF VEHICLE IV AND VEHICLE IV'S FRONT
C-----      BUMPER IS AHEAD OF VEHICLE JV'S REAR BUMPER THUS THERE IS A
C-----      COLLISION
C-----      PRINT THE COLLISION INFORMATION
            VELREL = VELVEH - VELNEW
            POSREL = POSJRB - POSNEW
            CALL  BANGS   ( 5,JV,VELREL,POSREL,POSADD )
          END IF
        END IF
C-----  END OF LOOP FOR INTERSECTION PATHS ORIGINATING FROM THE
C-----  LINKING INBOUND LANE FOR INTERSECTION PATH IP
      END DO
C-----IF THERE ARE NO GEOMETRIC CONFLICTING PATHS THEN GO TO 3020 AND
C-----THERE ARE NO INTERSECTION COLLISIONS
                    IF ( NGEOCP(IP) . LE . 0 )   GO TO 3020
C-----IF THERE ARE NO INTERSECTION CONFLICTS SET THEN GO TO 3020 AND
C-----THERE ARE NO INTERSECTION COLLISIONS
                    IF ( NCPSET(IP) . LE . 0 )   GO TO 3020
C-----IF THE VEHICLES REAR BUMPER HAS PASSED ALL GEOMETRIC CONFLICTING
C-----PATHS THEN GO TO 3020 AND THERE ARE NO INTERSECTION COLLISIONS
            IF ( ISTCON(IV) . GT . NGEOCP(IP) )  GO TO 3020
C-----CHECK EACH GEOMETRIC CONFLICTING INTERSECTION PATH
      DO 3010  INDEX = ISTCON(IV) , NGEOCP(IP)
C-----IF THE INTERSECTION CONFLICT IS NOT SET THEN GO TO 3010 AND SKIP
C-----TO THE NEXT INTERSECTION CONFLICT
                    IF ( ICPSET(INDEX,IP).EQ.0 ) GO TO 3010
C-----INITIALIZE SOME PARAMETERS FOR CHKCOL
      JNDEX  = IGEOCP(INDEX,IP)
      KOUNT  = 0
      IF ( IP . EQ . ICONP(1,JNDEX) )            THEN
        IANGLE =      ICONAN(JNDEX)
        JH     = 2
        JM     = 1
      ELSE
        IANGLE = (360-ICONAN(JNDEX))
        JH     = 1
        JM     = 2
      END IF
      DSINAN = DABS( DSIN( DBLE( IANGLE )*DEG2RD ) )
      DCOSAN = DABS( DCOS( DBLE( IANGLE )*DEG2RD ) )
      IF ( DCOSAN . LE . 1.0D-8 )                THEN
        DTANAN = 1.0D99
      ELSE
        DTANAN = DSINAN / DCOSAN
      END IF
C-----SET IVCONF TO THE NEXT VEHICLE THAT HAS NOT CLEARED THE
C-----INTERSECTION CONFLICT
      IVCONF = ICONV(JH,JNDEX)
      JP = ICONP(JH,JNDEX)
      JL = LIBL(JP)
C-----SET NOFC TO THE IVCONF VEHICLE
      NOFC = IVCONF
            IF ( MININT(NOFC) )                  GO TO 1020
            IF ( LPRES(NOFC) . EQ . LOBL(JP) )   GO TO 1020
C-----THE NOFC VEHICLE WAS NOT IN THE INTERSECTION THUS SET THE NOFC
C-----VEHICLE TO THE FIRST VEHICLE IN THE OTHER LANE
      NOFC = IFVL(JL)
 1020 CONTINUE
      PO = POSOLD
C[    IF ( JM                 .EQ.-2147483647   )STOP 'CHKCOL JM     01'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'CHKCOL JNDEX  01'
      P = ICOND(JM,JNDEX)
C-----INITIALLY USE MAXIMUM VEHICLE WIDTH FOR HIM TO BE ON THE SAFE SIDE
      HWH = 0.5D0*WIDMAX
C-----IF THE INTERSECTION CONFLICT IS A MERGE (THE LINKING OUTBOUND LANE
C-----FOR THE INTERSECTION PATHS ARE EQUAL) THEN CHECK CLEARANCE ELSE
C-----CHECK DISTANCE TO CONFLICT
      IF ( LOBL(IP) . EQ . LOBL(JP) )            THEN
C-----  REDUCE CLEARD BY 1 FT TO ACCOUNT FOR ANGLE AND HARD COLLISION
        CLEARD = HWM + HWH - 1.0D0
        CALL  CCLEAR  ( IP,JP,CLEARD,.FALSE.,CLEART )
        IF ( (PO+CLEART) .LT. DBLE( LENP(IP) ) ) GO TO 3010
      ELSE
        DISCLM = 0.0D0
        IF ( DTANAN . LE . 0.5D0 )               THEN
          DISCLM = DISCLM + 2.0D0*HWM
        ELSE
          DISCLM = DISCLM + HWM/DTANAN
        END IF
        IF ( DSINAN . LE . 0.5D0 )               THEN
          DISCLM = DISCLM + 2.0D0*HWH
        ELSE
          DISCLM = DISCLM + HWH/DSINAN
        END IF
C-----  IF THE VEHICLES FRONT BUMPER HAS NOT ARRIVED AT THE INTERSECTION
C-----  CONFLICT THEN GO TO 3010 AND SKIP TO THE NEXT INTERSECTION
C-----  CONFLICT
        IF ( (PO    +DISCLM) . LT . P )          GO TO 3010
C-----  IF THE VEHICLES REAR BUMPER HAS PASSED THE INTERSECTION CONFLICT
C-----  THEN GO TO 3010 AND SKIP TO THE NEXT INTERSECTION CONFLICT
        IF ( (PO-LVM-DISCLM) . GT . P )          GO TO 3010
      END IF
C-----THIS VEHICLE IS IN THE CONFLICT ZONE
      PM  = P
      POM = PO
      DFM =   (PO    )-P
      DRM = P-(PO-LVM)
      MDM = DMIN1( DFM,DRM )
      DCM = P - PO
 1070 CONTINUE
C-----START OF LOOP FOR CHECKING FOR THIS INTERSECTION CONFLICT
C[    IF ( KOUNT              .EQ.-2147483647   )STOP 'CHKCOL KOUNT  01'
      KOUNT = KOUNT + 1
                    IF ( KOUNT . GT . 50 )       GO TO 9330
C-----IF THE NOFC VEHICLE IS THE IVCONF VEHICLE THEN GO TO 1080 AND
C-----CHECK THE IVCONF VEHICLE
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'CHKCOL IVCONF 01'
C[    IF ( NOFC               .EQ.-2147483647   )STOP 'CHKCOL NOFC   01'
                    IF ( NOFC . EQ . IVCONF )    GO TO 1080
C-----IF THE NOFC VEHICLE HAS NOT SET CONFLICTS THEN HE MAY NOT PROCEED
C-----INTO THE INTERSECTION THUS HE WILL BLOCK THE IVCONF VEHICLE FROM
C-----PROCEEDING INTO THE INTERSECTION ALSO THUS THERE CAN BE NO
C-----INTERSECTION CONFLICT WITH THE IVCONF VEHICLE THUS GO TO 3010 AND
C-----SKIP TO THE NEXT INTERSECTION CONFLICT (THIS ONE IS CLEAR)
            IF ( NOFC       . EQ . 0     )       GO TO 3010
      IF      ( IPRC(1,NOFC) . EQ . JP )         THEN
        JPRC = 1
      ELSE IF ( IPRC(2,NOFC) . EQ . JP )         THEN
        JPRC = 2
      ELSE IF ( IPRC(1,NOFC) . EQ .  0 )         THEN
        JPRC = 1
        NORC(1,NOFC) = NVEP1
      ELSE IF ( IPRC(2,NOFC) . EQ .  0 )         THEN
        JPRC = 2
        NORC(2,NOFC) = NVEP1
      ELSE
        GO TO 9460
      END IF
            IF ( NORC(JPRC,NOFC) . EQ . NVEP1 )  GO TO 3010
C-----SET THE NOFC VEHICLE TO THE NOR VEHICLE FOR THE CURRENT NOFC
C-----VEHICLE
      IF ( NOR(NOFC) . EQ . 0 )                  THEN
        IF ( LPRES(NOFC) . EQ . LOBL(JP) )       THEN
          NOFC = NORC(JPRC,NOFC)
        ELSE
          NOFC = IFVL(JL)
        END IF
      ELSE
        NOFC = NOR(NOFC)
      END IF
C-----IF THERE IS A NEW NOFC VEHICLE THEN GO TO 1070 AND CHECK AGAIN
                    IF ( NOFC . GT . 0 )         GO TO 1070
C-----THE OLD NOFC VEHICLE HAD TO BE THE LAST VEHICLE ON THE
C-----INTERSECTION PATH THUS SET THE NOFC VEHICLE TO THE FIRST VEHICLE
C-----ON THE LANE AND GO TO 1070 AND CHECK AGAIN
C[    IF ( JL                 .EQ.-2147483647   )STOP 'CHKCOL JL     01'
      NOFC = IFVL(JL)
      GO TO 1070
 1080 CONTINUE
C-----SET THE IVCONF VEHICLES PARAMETERS
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'CHKCOL IVCONF 02'
      CALL  SPVAS   ( IVCONF,PO,VO,AO,SO,
     *                .FALSE.,.FALSE.,.TRUE.,.FALSE. )
C[    IF ( JL                 .EQ.-2147483647   )STOP 'CHKCOL JL     02'
      POP    = PO
      LGEOM4 = LGEOM(4,JL)
      PO     = PO + LGEOM4
                    IF ( MININT(IVCONF) )        GO TO 2010
      IF ( LPRES(IVCONF) . EQ . LIBL(JP) )       THEN
C-----  IVCONF VEHICLE IS ON THE INBOUND LANE FOR THE INTERSECTION PATH
        POP = POP - LGEOM4
        PO  = PO  - LGEOM4
      ELSE
C-----  IVCONF VEHICLE IS ON THE OUTBOUND LANE FOR THE INTERSECTION PATH
        PAD = DBLE( LENP(JP) - LGEOM(1,LPRES(IVCONF)) )
        POP = POP + PAD
        PO  = PO  + PAD
      END IF
 2010 CONTINUE
C[    IF ( JH                 .EQ.-2147483647   )STOP 'CHKCOL JH     01'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'CHKCOL JNDEX  02'
      P   = ICOND(JH,JNDEX) + LGEOM4
      HWH = 0.5D0*WIDV(IVEHCL(IVCONF))
      LVH =       LVAP(       IVCONF )
C-----IF THE INTERSECTION CONFLICT IS A MERGE (THE LINKING OUTBOUND LANE
C-----FOR THE INTERSECTION PATHS ARE EQUAL) THEN CHECK CLEARANCE ELSE
C-----CHECK DISTANCE TO CONFLICT
      IF ( LOBL(IP) . EQ . LOBL(JP) )            THEN
C-----  REDUCE CLEARD BY 1 FT TO ACCOUNT FOR ANGLE AND HARD COLLISION
        CLEARD = HWM + HWH - 1.0D0
        CALL  CCLEAR  ( IP,JP,CLEARD,.FALSE.,CLEART )
        IF ( (POM+CLEART) .LT. DBLE( LENP(IP) ) )GO TO 2020
        IF ( (POP+CLEART) .LT. DBLE( LENP(JP) ) )GO TO 3010
        DCH = P - PO
        IF ( DCM . LE . DCH )                    THEN
          IF ( (DCM+LVM) . GT . DCH )            THEN
            GO TO 2130
          ELSE
            GO TO 3010
          END IF
        ELSE
          IF ( (DCH+LVH) . GT . DCM )            THEN
            GO TO 2130
          ELSE
            GO TO 2120
          END IF
        END IF
      ELSE
        DISCLH = 0.0D0
        IF ( DTANAN . LE . 0.5D0 )               THEN
          DISCLH = DISCLH + 2.0D0*HWH
        ELSE
          DISCLH = DISCLH + HWH/DTANAN
        END IF
        IF ( DSINAN . LE . 0.5D0 )               THEN
          DISCLH = DISCLH + 2.0D0*HWM
        ELSE
          DISCLH = DISCLH + HWM/DSINAN
        END IF
C-----  IF THE OTHER VEHICLES FRONT BUMPER HAS NOT ARRIVED AT THE
C-----  INTERSECTION CONFLICT THEN GO TO 2020 AND SKIP TO THE NEXT
C-----  INTERSECTION CONFLICT
        IF ( (PO    +DISCLH) . LT . P )          GO TO 3010
C-----  IF THE OTHER VEHICLES REAR BUMPER HAS NOT PASSED THE
C-----  INTERSECTION CONFLICT THEN THEN GO TO 2120 AND PROCESS THE NORC
C-----  VEHICLE
        IF ( (PO-LVH-DISCLH) . GT . P )          GO TO 2120
C-----  RE-CHECK DISCLM USING ACTUAL VEHICLE WIDTH FOR HIM
        DISCLM = 0.0D0
        IF ( DTANAN . LE . 0.5D0 )               THEN
          DISCLM = DISCLM + 2.0D0*HWM
        ELSE
          DISCLM = DISCLM + HWM/DTANAN
        END IF
        IF ( DSINAN . LE . 0.5D0 )               THEN
          DISCLM = DISCLM + 2.0D0*HWH
        ELSE
          DISCLM = DISCLM + HWH/DSINAN
        END IF
C-----  IF THE VEHICLES FRONT BUMPER HAS NOT ARRIVED AT THE INTERSECTION
C-----  CONFLICT THEN GO TO 2020 AND SKIP TO THE NEXT INTERSECTION
C-----  CONFLICT
        IF ( (POM    +DISCLM) . LT . PM )        GO TO 2020
C-----  IF THE VEHICLES REAR BUMPER HAS PASSED THE INTERSECTION CONFLICT
C-----  THEN GO TO 2020 AND SKIP TO THE NEXT INTERSECTION CONFLICT
        IF ( (POM-LVM-DISCLM) . GT . PM )        GO TO 2020
C-----  CHECK VEHICLES BASED ON DIRECTION OF THE OTHER VEHICLE
        DISCLH = HWM*DSINAN
        DISCLM = HWH*DSINAN
        IF      ( ( IANGLE . GE .  92 ) . AND .
     *            ( IANGLE . LE . 268 ) )        THEN
C-----    THE OTHER VEHICLE IS APPROACHING FROM THE FRONT
C-----
C-----    IF THIS VEHICLES FRONT BUMPER IS BEFORE THE POINT OF
C-----    INTERSECTION CONFLICT AND THE OTHER VEHICLES FRONT BUMPER IS
C-----    BEFORE THE POINT OF INTERSECTION CONFLICT THEN GO TO 3010 AND
C-----    SKIP TO THE NEXT INTERSECTION CONFLICT
          IF ( (  POM             . LT . PM ) . AND .
     *         ( (PO     +DISCLH) . LT . P  ) )  THEN
            GO TO 3010
          END IF
C-----    IF THE OTHER VEHICLES FRONT BUMPER IS BEFORE THE POINT OF
C-----    INTERSECTION CONFLICT AND THIS VEHICLES FRONT BUMPER IS BEFORE
C-----    THE POINT OF INTERSECTION CONFLICT THEN GO TO 3010 AND SKIP TO
C-----    THE NEXT INTERSECTION CONFLICT
          IF ( (  PO              . LT . P  ) . AND .
     *         ( (POM    +DISCLM) . LT . PM ) )  THEN
            GO TO 3010
          END IF
C-----    IF THIS VEHICLES REAR BUMPER IS BEYOND THE POINT OF
C-----    INTERSECTION CONFLICT AND THE OTHER VEHICLES REAR BUMPER IS
C-----    BEYOND THE POINT OF INTERSECTION CONFLICT THEN GO TO 2120 AND
C-----    PROCESS THE NORC VEHICLE
          IF ( (  POM-LVM         . GT . PM ) . AND .
     *         ( (PO -LVH-DISCLH) . GT . P  ) )  THEN
            GO TO 2120
          END IF
C-----    IF THE OTHER VEHICLES REAR BUMPER IS BEYOND THE POINT OF
C-----    INTERSECTION CONFLICT AND THIS VEHICLES REAR BUMPER IS BEYOND
C-----    THE POINT OF INTERSECTION CONFLICT THEN GO TO 2120 AND PROCESS
C-----    THE NORC VEHICLE
          IF ( (  PO -LVH         . GT . P  ) . AND .
     *         ( (POM-LVM-DISCLM) . GT . PM ) )  THEN
            GO TO 2120
          END IF
        ELSE IF ( ( IANGLE . LT .  88 ) . OR .
     *            ( IANGLE . GT . 272 ) )        THEN
C-----    THE OTHER VEHICLE IS APPROACHING FROM THE REAR
C-----
C-----    IF THIS VEHICLES FRONT BUMPER IS BEFORE THE POINT OF
C-----    INTERSECTION CONFLICT AND THE OTHER VEHICLES REAR BUMPER IS
C-----    BEYOND THE POINT OF INTERSECTION CONFLICT THEN GO TO 2120 AND
C-----    PROCESS THE NORC VEHICLE
          IF ( (  POM             . LT . PM ) . AND .
     *         ( (PO -LVH-DISCLH) . GT . P  ) )  THEN
            GO TO 2120
          END IF
C-----    IF THE OTHER VEHICLES REAR BUMPER IS BEYOND THE POINT OF
C-----    INTERSECTION CONFLICT AND THIS VEHICLES FRONT BUMPER IS BEFORE
C-----    THE POINT OF INTERSECTION CONFLICT THEN GO TO 2120 AND PROCESS
C-----    THE NORC VEHICLE
          IF ( (  PO -LVH         . GT . P  ) . AND .
     *         ( (POM    +DISCLM) . LT . PM ) )  THEN
            GO TO 2120
          END IF
C-----    IF THIS VEHICLES REAR BUMPER IS BEYOND THE POINT OF
C-----    INTERSECTION CONFLICT AND THE OTHER VEHICLES FRONT BUMPER IS
C-----    BEFORE THE POINT OF INTERSECTION CONFLICT THEN GO TO 3010 AND
C-----    SKIP TO THE NEXT INTERSECTION CONFLICT
          IF ( (  POM-LVM         . GT . PM ) . AND .
     *         ( (PO     +DISCLH) . LT . P  ) )  THEN
            GO TO 3010
          END IF
C-----    IF THE OTHER VEHICLES FRONT BUMPER IS BEFORE THE POINT OF
C-----    INTERSECTION CONFLICT AND THIS VEHICLES REAR BUMPER IS BEYOND
C-----    THE POINT OF INTERSECTION CONFLICT THEN GO TO 3010 AND SKIP TO
C-----    THE NEXT INTERSECTION CONFLICT
          IF ( (  PO              . LT . P  ) . AND .
     *         ( (POM-LVM-DISCLM) . GT . PM ) )  THEN
            GO TO 3010
          END IF
        ELSE
C-----    THE OTHER VEHICLE IS APPROACHING FROM THE SIDE
C-----
C-----    IF THIS VEHICLES FRONT BUMPER IS BEFORE THE POINT OF
C-----    INTERSECTION CONFLICT OR THIS VEHICLES REAR BUMPER IS BEYOND
C-----    THE POINT OF INTERSECTION CONFLICT THEN IF THE OTHER VEHICLES
C-----    FRONT BUMPER IS BEFORE THE POINT OF INTERSECTION CONFLICT THEN
C-----    GO TO 3010 AND SKIP TO THE NEXT INTERSECTION CONFLICT ELSE GO
C-----    TO 2120 AND PROCESS THE NORC VEHICLE
          IF ( ( (POM    +HWH   ) . LT . PM ) . OR .
     *         ( (POM-LVM-HWH   ) . GT . PM ) )  THEN
            IF (  PO              . LT . P  )    THEN
              GO TO 3010
            ELSE
              GO TO 2120
            END IF
          END IF
C-----    IF THE OTHER VEHICLES FRONT BUMPER IS BEFORE THE POINT OF
C-----    INTERSECTION CONFLICT THEN GO TO 3010 AND SKIP TO THE NEXT
C-----    INTERSECTION CONFLICT
          IF (   (PO     +HWM   ) . LT . P  )    THEN
            GO TO 3010
          END IF
C-----    IF THE OTHER VEHICLES REAR BUMPER IS BEYOND THE POINT OF
C-----    INTERSECTION CONFLICT THEN GO TO 2120 AND PROCESS THE NORC
C-----    VEHICLE
          IF (   (PO -LVH-HWM   ) . GT . P  )    THEN
            GO TO 2120
          END IF
        END IF
C-----  THERE IS A COLLISION
        GO TO 2130
      END IF
 2020 CONTINUE
                    IF ( PO . LT . P )           GO TO 3010
 2120 CONTINUE
C-----SET THE NOFC VEHICLE TO THE IVCONF VEHICLE AND SET THE IVCONF
C-----VEHICLE TO THE NEXT VEHICLE THAT SHOULD HAVE TO CLEAR THE SAME
C-----INTERSECTION CONFLICT
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'CHKCOL IVCONF 08'
      NOFC = IVCONF
      IF      ( IPRC(1,NOFC) . EQ . JP )         THEN
        JPRC = 1
      ELSE IF ( IPRC(2,NOFC) . EQ . JP )         THEN
        JPRC = 2
      ELSE
        GO TO 9460
      END IF
      IVCONF = NORC(JPRC,NOFC)
C-----IF THERE IS ANOTHER VEHICLE THAT HAS TO CLEAR THE SAME
C-----INTERSECTION CONFLICT AND THIS VEHICLE HAS TO GO BEHIND THE LAST
C-----IVCONF VEHICLE THEN GO TO 1070 AND CHECK THE NEW IVCONF VEHICLE
C-----ELSE GO TO 3010 AND SKIP TO THE NEXT INTERSECTION CONFLICT
      IF ( IVCONF . GT . 0 )                     THEN
        GO TO 1070
      ELSE
        GO TO 3010
      END IF
 2130 CONTINUE
      DFH =   (PO    )-P
      DRH = P-(PO-LVH)
      MDH = DMIN1( DFH,DRH )
C-----BOTH VEHICLES ARE AT THE INTERSECTION CONFLICT THUS THERE IS A
C-----COLLISION
C-----PRINT THE COLLISION INFORMATION
      VELREL = VO  - VELOLD
      POSREL = DMIN1( MDM,MDH )
      CALL  BANGS   ( 4,IVCONF,VELREL,POSREL,0.0D0 )
C-----END OF GEOMETRIC CONFLICTING PATH LOOP
 3010 CONTINUE
 3020 CONTINUE
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9330 CONTINUE
      CALL  ABORTR  ( 'STOP 933 - INFINITE LOOP - CHKCOL' )
      STOP  933
 9460 CONTINUE
      CALL  ABORTR  ( 'STOP 946 - INVALID IPRC/NORC - CHKCOL' )
      STOP  946
      END                                                               CHKCOL
C
C
C
      SUBROUTINE CLRCON ( KP,POSNFB,IFORCE )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'CONFLT'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           EVRESP,IFORCE,LBVSTP,LCHKCF
      INTEGER           IK,IPOSCK,JCONI,JGEOCP,JH,JM,JP,JPRC,JSTCON,KP,
     *                  KV
      DOUBLE PRECISION  HWM,LVM,POSMJC,POSNFB,SAFR,SAFVEL
CN752 FORMAT(8H CONFLT ,I3,1X,12I4)
C
C-----SUBROUTINE CLRCON CLEARS THE INTERSECTION CONFLICTS AS THE REAR
C-----BUMPER PASSES THEM
C
C[    IK         = -2147483647
C[    IPOSCK     = -2147483647
C[    JCONI      = -2147483647
C[    JGEOCP     = -2147483647
C[    JH         = -2147483647
C[    JM         = -2147483647
C[    JP         = -2147483647
C[    JSTCON     = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CLRCON'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IF      ( IPRC(1,IV) . EQ . KP )           THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IV) . EQ . KP )           THEN
        JPRC = 2
      ELSE
        IF ( ISTCON(IV) . EQ . (NGEOCP(KP)+1) )  RETURN
        GO TO 9460
      END IF
      SAFVEL = DCHRMX*DCHRMX*DBLE( LIMP(KP) )
      SAFR   = (SAFDIS+(SAFVEL/SAFSPD))/DCHRMN
      HWM    = 0.5D0*WIDV(IVEHCL(IV))
      LVM    = LENVAP
      IPOSCK = POSNFB - LVM - SAFR - HWM - 0.5D0
C-----CHECK THE INTERSECTION CONFLICTS THAT THE VEHICLE HAS NOT CLEARED
      JSTCON = ISTCON(IV)
      DO 1030  IK = JSTCON , NGEOCP(KP)
      ISTCON(IV) = IK
      JGEOCP = IGEOCP(IK,KP)
CP          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 101
CN                  IF ( TIME . LT . TPRINT )    GO TO 101
CN    WRITE (6,752) JGEOCP,ICONP(1,JGEOCP),ICONP(2,JGEOCP),
CN   *              ICONA(1,JGEOCP),ICONA(2,JGEOCP),ICOND(1,JGEOCP),
CN   *              ICOND(2,JGEOCP),ICONAN(JGEOCP),ICONI(1,JGEOCP),
CN   *              ICONI(2,JGEOCP),ICONV(1,JGEOCP),ICONV(2,JGEOCP)
CN101 CONTINUE
      IF ( KP . EQ . ICONP(1,JGEOCP) )           THEN
        JH = 2
        JM = 1
      ELSE
        JH = 1
        JM = 2
      END IF
C-----IF THE VEHICLE IS TO LEAVE THE INTERSECTION PATH THIS DT THEN
C-----CLEAR ALL REMAINING INTERSECTION CONFLICTS
C           IF ( POSNFB.GE.DBLE( LENP(KP) ) )    GO TO 1020
            IF ( IFORCE )                        GO TO 1020
C-----IF THE POSITION OF THE REAR BUMPER IS LT THE DISTANCE TO THE
C-----INTERSECTION CONFLICT THEN DO NOT CLEAR THE INTERSECTION CONFLICT
C[    IF ( IPOSCK             .EQ.-2147483647   )STOP 'CLRCON IPOSCK 01'
            IF ( IPOSCK . LT . ICOND(JM,JGEOCP) )RETURN
 1020 CONTINUE
C-----SET THE VEHICLES NORC AS THE NEXT VEHICLE THAT HAS NOT CLEARED THE
C-----INTERSECTION CONFLICT
C[    IF ( JM                 .EQ.-2147483647   )STOP 'CLRCON JM     01'
C[    IF ( JGEOCP             .EQ.-2147483647   )STOP 'CLRCON JGEOCP 01'
      ICONV(JM,JGEOCP) = NORC(JPRC,IV)
C;    WRITE (TC3,331) TIME,'CLRCON 1 ICONV(JM,JGEOCP) = NORC(IV)',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),-1,
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JM=',JM,'JGEOCP=',JGEOCP,'JM=',IK,'N=',
C;   *                NGEOCP(KP),'JPRC=',JPRC
                    IF ( NORC(JPRC,IV).GT.0 )    GO TO 1030
C-----UNSET THE INTERSECTION CONFLICT FOR THE OTHER INTERSECTION PATH
      JP = ICONP(JH,JGEOCP)
      NCPSET(JP) = MAX0( NCPSET(JP)-1,0 )
      JCONI = ICONI(JH,JGEOCP)
      ICPSET(JCONI,JP) = 0
C-----END OF INTERSECTION CONFLICT LOOP
 1030 CONTINUE
C-----ALL THE INTERSECTION CONFLICTS HAVE BEEN PASSED BY THE VEHICLE
      ISTCON(IV) = NGEOCP(KP) + 1
      IF      ( IPRC(1,IV) . EQ . KP )           THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IV) . EQ . KP )           THEN
        JPRC = 2
      ELSE IF ( IPRC(1,IV) . EQ .  0 )           THEN
        JPRC = 1
        IPRC(1,IV) = KP
      ELSE IF ( IPRC(2,IV) . EQ .  0 )           THEN
        JPRC = 2
        IPRC(2,IV) = KP
      ELSE
        GO TO 9460
      END IF
      NORC(JPRC,IV) = NVEP1
C;    WRITE (TC3,331) TIME,'CLRCON 2 NORC(IV) = NVEP1          ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),-1,
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JPRC=',JPRC
C;331 FORMAT(F6.1,1X,A36,I2,I3,I4,I6,1X,L1,F5.1,3I3,I6,2(I4,I6),
C;   *       5(1X,A,I4))
      IPRC(JPRC,IV) = 0
      IF ( SMJCOL . AND . (.NOT. MAJCOL(IV)) )   THEN
        MAJCLB(IV) = .FALSE.
        MAJCLL(IV) = .FALSE.
        MAJCON(IV) = .FALSE.
        POSCLB(IV) = POSBIG
        POSCLL(IV) = POSBIG
        POSCON(IV) = POSBIG
        IF ( MININT(IV) )                        THEN
          CALL  CPMJCL ( IP,KV,LCHKCF,LBVSTP,POSMJC )
          IF ( KV . GT . 0 )                     THEN
C-----      THERE IS A COLLISION VEHICLE ON THE INTERSECTION PATH BEFORE
C-----      THIS VEHICLE OR DOWNSTREAM THUS SET THAT THE VEHICLE IS
C-----      BLOCKED BY A MAJOR COLLISION
            MAJCLB(IV) = .TRUE.
            POSCLB(IV) = DMIN1( POSCLB(IV),POSMJC )
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----      FALSE
            EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *                 ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
C-----    SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----    COLLISION MAY BLOCK THIS VEHICLE
          IF ( LCHKCF )                          THEN
            MAJCON(IV) = .TRUE.
            POSCON(IV) = DMIN1( POSCON(IV),POSMJC )
          END IF
        ELSE
          CALL  CLMJCL  ( IL,LNEXT(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
          IF ( KV . GT . 0 )                     THEN
C-----      THERE IS A COLLISION VEHICLE ON THE LANE BEFORE THIS VEHICLE
C-----      OR DOWNSTREAM THUS SET THAT THE VEHICLE IS BLOCKED BY A
C-----      MAJOR COLLISION
            MAJCLB(IV) = .TRUE.
            POSCLB(IV) = DMIN1( POSCLB(IV),POSMJC )
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----      FALSE
            EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *                 ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
C-----    SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----    COLLISION MAY BLOCK THIS VEHICLE
          IF ( LCHKCF )                          THEN
            MAJCON(IV) = .TRUE.
            POSCON(IV) = DMIN1( POSCON(IV),POSMJC )
          END IF
        END IF
      END IF
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9140 CONTINUE
      CALL  ABORTR  ( 'STOP 914 - LNEXT EQ 0 - CLRCON' )
      STOP  914
 9460 CONTINUE
      CALL  ABORTR  ( 'STOP 946 - INVALID IPRC/NORC - CLRCON' )
      STOP  946
      END                                                               CLRCON
C
C
C
      SUBROUTINE LOGIIN
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
C6    INCLUDE 'PRTPVA'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      LOGICAL           EVRESP,LBVSTP,LCHKCF
      INTEGER           KV,GETLCV,NOFT,NORT
      DOUBLE PRECISION  DPOS,POSMJC,VELAVG
C
C-----SUBROUTINE LOGIIN LOGS THE VEHICLE OUT OF THE INTERSECTION PATH
C-----AND INTO THE LINKING INTERNAL APPROACH AND LANE
C
C[    NOFT       = -2147483647
C[    NORT       = -2147483647
C[    DPOS       = -2147483647.0
C[    VELAVG     = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'LOGIIN'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IL = LNEXT(IV)
      IA = ISNA(IL)
C-----SET LANE CONTROL FOR VEHICLE
      LCONTV(IV) = GETLCV( IV,IL )
C-----SET NOFT TO THE LAST VEHICLE IN THE LINKING INTERNAL LANE
      NOFT = ILVL(IL)
      NOF(IV) = NOFT
      DPOS = (POSNEW - DBLE( LENP(IP) ))
      IDTSI(IV) = DPOS
      VELAVG = 0.5D0*(VELOLD+VELNEW)
      IF ( VELAVG . EQ . 0.0D0 )                 THEN
        IEXTII(IV) = 0.0D0
      ELSE
        IEXTII(IV) = (DPOS/VELAVG)/DT
      END IF
      POSOLD = POSOLD - DBLE( LENP(IP) ) + DBLE( LGEOM(1,IL) )
      POSNEW = POSNEW - DBLE( LENP(IP) ) + DBLE( LGEOM(1,IL) )
C-----INCREMENT THE NUMBER OF VEHICLES ON THE LINKING INTERNAL APPROACH
C-----AND LANE
      NVIBA = NVIBA + 1
      NVIA(IA) = NVIA(IA) + 1
C     DO 1010  ILN = 1 , NLANES(IA)
C           IF ( IL . EQ . LLANES(ILN,IA) )      GO TO 1020
C1010 CONTINUE
C     GO TO 9240
C1020 CONTINUE
      ILN = ISNL(IL)
      NVIL(ILN,IA) = NVIL(ILN,IA) + 1
C-----DECREMENT THE NUMBER OF VEHICLES ON THE INTERSECTION PATH
      NVIP(IP) = NVIP(IP) - 1
      NVIN = NVIN - 1
C-----SET THE FIRST VEHICLE ON THE INTERSECTION PATH TO THIS VEHICLES
C-----NORT
      NORT = NOR(IV)
      IFVP(IP) = NORT
                    IF ( NORT . GT . 0 )         GO TO 1030
C-----SET THE LAST VEHICLE ON THE INTERSECTION PATH TO 0 (OLD NORT EQ 0)
      ILVP(IP) = 0
      GO TO 2010
 1030 CONTINUE
C-----SET MFINL AND MOASF TO TRUE, RESET IACC TO SLIGHTLY DECELERATING
C-----IF MSFLG EQ TRUE AND THE VEHICLE IS NOT DECELERATING, SET MSFLG
C-----TO FALSE, AND FINALLY STORE 0 FOR NOFT FOR THE NORT VEHICLE (OLD
C-----NORT GT 0)
      CALL  FLGNOR ( .TRUE.,0 )
 2010 CONTINUE
C[    IF ( NOFT               .EQ.-2147483647   )STOP 'LOGIIN NOFT   01'
                    IF ( NOFT . EQ . 0 )         GO TO 2020
C-----CHECK WHICH VEHICLE ON THE INTERNAL LANE THAT THIS VEHICLE SHOULD
C-----BE BEHIND (NEW NOFT GT 0)
C-----IF THE POSITION OF THIS VEHICLE IS LE THE POSITION OF THE NOFT
C-----VEHICLE THEN GO TO 2030 AND PUT THIS VEHICLE BEHIND THE NOFT
C-----VEHICLE
      IF ( POSNEW . LE . DBLE( IPOS(NOFT) ) )    GO TO 2030
C-----SET THE VEHICLE AHEAD OF THE NOFT VEHICLE AS THE NEW NOFT VEHICLE
      NOFT = NOF(NOFT)
      NOF(IV) = NOFT
C-----IF THERE WAS A VEHICLE AHEAD OF THE NOFT VEHICLE THEN GO TO 2010
C-----AND CHECK THE POSITION ELSE SET THIS VEHICLE AS THE NEW FIRST
C-----VEHICLE ON THE LINKING INTERNAL LANE
                    IF ( NOFT . GT . 0 )         GO TO 2010
 2020 CONTINUE
C-----SET THIS VEHICLE AS THE NEW FIRST VEHICLE ON THE LINKING INTERNAL
C-----LANE (NEW NOFT EQ 0)
      NORT = IFVL(IL)
      NOR(IV) = NORT
      IFVL(IL) = IV
      MFINL(IV) = .TRUE.
      MOASF(IV) = .TRUE.
      IVPV  = 0
      PVPOS = DBLE( LGEOM(2,IL) ) + 1.5D0
      IF ( MFSTPF(IV) )                          THEN
        IF ( FSTACT(IV) )                        THEN
          IF ( FSTPOS(IV) . LT . PVPOS )         PVPOS = FSTPOS(IV)
        END IF
        IF ( VMSASM(IV) . GT . 0 )               THEN
          IF ( ( IVMSMG(VMSASM(IV)).EQ.VMSMSI ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSL ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSM ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSC ) )THEN
            IF ( VMSPST(IV) . LT . PVPOS )       PVPOS = VMSPST(IV)
          END IF
        END IF
      END IF
                    IF ( NORT . GT . 0 )         GO TO 2050
      GO TO 2040
 2030 CONTINUE
C-----SET THIS VEHICLE BEHIND THE NOFT VEHICLE ON THE LINKING INTERNAL
C-----APPROACH (NEW NOFT GT 0)
      MFINL(IV) = .FALSE.
      MOASF(IV) = .FALSE.
C[    IF ( NOFT               .EQ.-2147483647   )STOP 'LOGIIN NOFT   02'
                    IF ( IVEL(NOFT) .LE. 0.0D0 ) MOASF(IV) = .TRUE.
      IVPV  = NOFT
      CALL  SPVAS   ( IVPV,PVPOS,PVVEL,PVACC,PVSLP,
     *                .TRUE.,.TRUE.,.TRUE.,.FALSE.  )
      IF ( MFSTPF(IV) )                          THEN
        IF ( FSTACT(IV) )                        THEN
          IF ( FSTPOS(IV) . LT . PVPOS )         PVPOS = FSTPOS(IV)
        END IF
        IF ( VMSASM(IV) . GT . 0 )               THEN
          IF ( ( IVMSMG(VMSASM(IV)).EQ.VMSMSI ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSL ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSM ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSC ) )THEN
            IF ( VMSPST(IV) . LT . PVPOS )       PVPOS = VMSPST(IV)
          END IF
        END IF
      END IF
      NORT = NOR(NOFT)
      NOR(IV) = NORT
      NOR(NOFT) = IV
                    IF ( NORT . GT . 0 )         GO TO 2050
 2040 CONTINUE
C-----SET THE LAST VEHICLE ON THE LINKING INTERNAL LANE TO THIS VEHICLE
C-----(NEW NORT EQ 0)
      ILVL(IL) = IV
      GO TO 3010
 2050 CONTINUE
C-----SET MFINL AND MOASF TO FALSE, RESET IACC TO SLIGHTLY DECELERATING
C-----IF MSFLG EQ TRUE AND THE VEHICLE IS NOT DECELERATING, SET MSFLG
C-----TO FALSE, AND FINALLY STORE IV FOR NOFT FOR THE NORT VEHICLE (NEW
C-----NORT GT 0)
      CALL  FLGNOR ( .FALSE.,IV )
 3010 CONTINUE
      LEGAL(IV) = 4
      LCHGE(IV) = 1
C[    IF ( NOFT               .EQ.-2147483647   )STOP 'LOGIIN NOFT   03'
                    IF ( NOFT . EQ . 0 )         GO TO 3020
                    IF ( ISET(NOFT) . EQ . 1 )   LCHGE(IV) = 3
 3020 CONTINUE
      LPREV(IV) = LPRES(IV)
      LPRES(IV) = IL
      LNEXT(IV) = 0
C-----IF THE VEHICLE IS INVOLVED IN A MAJOR COLLISION THEN SET THAT THE
C-----NEW LANE HAS A MAJOR COLLISION
      IF ( MAJCOL(IV) )                        THEN
        LMJCOL(IL) = .TRUE.
      END IF
C-----CHECK MY LANE AND IF BLOCKED THEN SET PARAMETERS FOR BLOCKED LANE
      CALL  CHKMLN
      LALT(IV) = 5
      IF ( MBLOCK(IV) )                          THEN
        ENDLN = DBLE( LGEOM(2,IL) )
      ELSE
        ENDLN = DBLE( LGEOM(4,IL) ) + 1.5D0
      END IF
      RELEND = ENDLN - POSOLD
C-----RESET SOME OF THE VEHICLE ATTRIBUTES
                    IF ( IDISPD(IV) )            ISPD(IV) = ISPD(IV)/2
      ISPD  (IV) = IDNINT( DBLE( ISPD(IV)*ISLIM(IA) )/DBLE( LIMP(IP) ) )
      ISPDP (IV) = 0
      LATPOS(IV) = 0.0D0
      IDISPD(IV) = .FALSE.
      MLAG  (IV) = .FALSE.
      MPRO  (IV) = .FALSE.
      MSFLG (IV) = .FALSE.
      MTCARS(IV) = .TRUE.
      MININT(IV) = .FALSE.
C6    DISTAD(IV) = DISTAD(IV) + LENP(IP)
      MATSTL(IV) = .FALSE.
      MCHKCF(IV) = .FALSE.
      MDEDIC(IV) = .FALSE.
      MINFLZ(IV) = .FALSE.
      MIUNC (IV) = .FALSE.
      MLRTOR(IV) = .FALSE.
      MLSTOP(IV) = .FALSE.
      MLUNC (IV) = .FALSE.
      MLYELD(IV) = .FALSE.
      MSSGRN(IV) = .FALSE.
      MSSRED(IV) = .FALSE.
      ICHKCF(IV) = .FALSE.
      ICONTN(IV) = .FALSE.
      IDEDIC(IV) = .FALSE.
      IERROR(IV) = .FALSE.
      ILSTOP(IV) = .FALSE.
      ILUNC (IV) = .FALSE.
      ILYELD(IV) = .FALSE.
      INFLZ (IV) = .FALSE.
      INT1T (IV) = ITURN(IV)
      ITURN (IV) = 0
      ISET  (IV) = 6
C-----IF THE VEHICLES REAR BUMPER IS ON THE DIAMOND INTERNAL INBOUND
C-----LANE THEN ALLOW LANE CHANGES
      IF ( (POSNEW-DBLE( LGEOM(1,IL) )) . GE . LENVAP )
     *                                           THEN
        ISET(IV) = 5
      END IF
      LOGFLG(IV) = 2
      LOGTMP     = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        MCHKCF(IV) = .TRUE.
        MPRO  (IV) = .FALSE.
        LOGFLG(IV) = 1
        LOGTMP     = 1
      END IF
      CALL  PATHF   ( .FALSE.,0,'LOGIIN')
                    IF ( LNEXT(IV) . EQ . 0 )    GO TO 3030
      IGO = 1
      CALL  INFLZN
      IGO = 0
 3030 CONTINUE
      IF ( SMJCOL . AND . (.NOT. MAJCOL(IV)) )   THEN
        MAJCLB(IV) = .FALSE.
        MAJCLL(IV) = .FALSE.
        MAJCON(IV) = .FALSE.
        POSCLB(IV) = POSBIG
        POSCLL(IV) = POSBIG
        POSCON(IV) = POSBIG
        CALL  CLMJCL  ( IL,LNEXT(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
        IF ( KV . GT . 0 )                 THEN
C-----    THERE IS A COLLISION VEHICLE ON THE LANE BEFORE THIS VEHICLE
C-----    OR DOWNSTREAM THUS SET THAT THE VEHICLE IS BLOCKED BY A MAJOR
C-----    COLLISION
          MAJCLB(IV) = .TRUE.
          POSCLB(IV) = DMIN1( POSCLB(IV),POSMJC )
C-----    IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----    VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----    FALSE
          EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *               ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
          RESPEV = ( RESPEV . OR . EVRESP )
        END IF
C-----  SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----  COLLISION MAY BLOCK THIS VEHICLE
        IF ( LCHKCF )                            THEN
          MAJCON(IV) = .TRUE.
          POSCON(IV) = DMIN1( POSCON(IV),POSMJC )
        END IF
      END IF
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
C9240 CONTINUE
C     CALL  ABORTR  ( 'STOP 924 - '                    //
C    *                'LNEXT IS NOT ON LLANES LIST - ' //
C    *                'LOGIIN'                            )
C     STOP  924
      END                                                               LOGIIN
C
C
C
      SUBROUTINE LOGIOB
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
C6    INCLUDE 'PRTPVA'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           EVRESP,LBVSTP,LCHKCF
      INTEGER           KV,GETLCV,NOFT,NORT
      DOUBLE PRECISION  POSMJC
C
C-----SUBROUTINE LOGIOB LOGS THE VEHICLE OUT OF THE INTERSECTION PATH
C-----AND INTO THE LINKING OUTBOUND APPROACH AND LANE
C
C[    NOFT       = -2147483647
C[    NORT       = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'LOGIOB'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IL = LNEXT(IV)
      IA = ISNA(IL)
C-----SET LANE CONTROL FOR VEHICLE
      LCONTV(IV) = GETLCV( IV,IL )
C-----SET NOFT TO THE LAST VEHICLE IN THE LINKING OUTBOUND LANE
      NOFT = ILVL(IL)
      NOF(IV) = NOFT
      POSOLD = POSOLD - DBLE( LENP(IP) ) + DBLE( LGEOM(1,IL) )
      POSNEW = POSNEW - DBLE( LENP(IP) ) + DBLE( LGEOM(1,IL) )
C-----INCREMENT THE NUMBER OF VEHICLES ON THE LINKING OUTBOUND APPROACH
C-----AND LANE
      NVOBA = NVOBA + 1
      NVIA(IA) = NVIA(IA) + 1
C     DO 1010  ILN = 1 , NLANES(IA)
C           IF ( IL . EQ . LLANES(ILN,IA) )      GO TO 1020
C1010 CONTINUE
C     GO TO 9020
C1020 CONTINUE
      ILN = ISNL(IL)
      NVIL(ILN,IA) = NVIL(ILN,IA) + 1
C-----DECREMENT THE NUMBER OF VEHICLES ON THE INTERSECTION PATH
      NVIP(IP) = NVIP(IP) - 1
      NVIN = NVIN - 1
C-----SET THE FIRST VEHICLE ON THE INTERSECTION PATH TO THIS VEHICLES
C-----NORT
      NORT = NOR(IV)
      IFVP(IP) = NORT
                    IF ( NORT . GT . 0 )         GO TO 1030
C-----SET THE LAST VEHICLE ON THE INTERSECTION PATH TO 0 (OLD NORT EQ 0)
      ILVP(IP) = 0
      GO TO 2010
 1030 CONTINUE
C-----SET MFINL AND MOASF TO TRUE, RESET IACC TO SLIGHTLY DECELERATING
C-----IF MSFLG EQ TRUE AND THE VEHICLE IS NOT DECELERATING, SET MSFLG
C-----TO FALSE, AND FINALLY STORE 0 FOR NOFT FOR THE NORT VEHICLE (OLD
C-----NORT GT 0)
      CALL  FLGNOR ( .TRUE.,0 )
 2010 CONTINUE
C[    IF ( NOFT               .EQ.-2147483647   )STOP 'LOGIOB NOFT   01'
                    IF ( NOFT . EQ . 0 )         GO TO 2020
C-----CHECK WHICH VEHICLE ON THE OUTBOUND LANE THAT THIS VEHICLE SHOULD
C-----BE BEHIND (NEW NOFT GT 0)
C-----IF THE POSITION OF THIS VEHICLE IS LE THE POSITION OF THE NOFT
C-----VEHICLE THEN GO TO 2030 AND PUT THIS VEHICLE BEHIND THE NOFT
C-----VEHICLE
      IF ( POSNEW . LE . DBLE( IPOS(NOFT) ) )    GO TO 2030
C-----SET THE VEHICLE AHEAD OF THE NOFT VEHICLE AS THE NEW NOFT VEHICLE
      NOFT = NOF(NOFT)
      NOF(IV) = NOFT
C-----IF THERE WAS A VEHICLE AHEAD OF THE NOFT VEHICLE THEN GO TO 2010
C-----AND CHECK THE POSITION ELSE SET THIS VEHICLE AS THE NEW FIRST
C-----VEHICLE ON THE LINKING OUTBOUND LANE
                    IF ( NOFT . GT . 0 )         GO TO 2010
 2020 CONTINUE
C-----SET THIS VEHICLE AS THE NEW FIRST VEHICLE ON THE LINKING OUTBOUND
C-----LANE (NEW NOFT EQ 0)
      NORT = IFVL(IL)
      NOR(IV) = NORT
      IFVL(IL) = IV
      MFINL(IV) = .TRUE.
      MOASF(IV) = .TRUE.
                    IF ( NORT . GT . 0 )         GO TO 2050
      GO TO 2040
 2030 CONTINUE
C-----SET THIS VEHICLE BEHIND THE NOFT VEHICLE ON THE LINKING OUTBOUND
C-----APPROACH (NEW NOFT GT 0)
      MFINL(IV) = .FALSE.
      MOASF(IV) = .FALSE.
C[    IF ( NOFT               .EQ.-2147483647   )STOP 'LOGIOB NOFT   02'
                    IF ( IVEL(NOFT) .LE. 0.0D0 ) MOASF(IV) = .TRUE.
      NORT = NOR(NOFT)
      NOR(IV) = NORT
      NOR(NOFT) = IV
                    IF ( NORT . GT . 0 )         GO TO 2050
 2040 CONTINUE
C-----SET THE LAST VEHICLE ON THE LINKING OUTBOUND LANE TO THIS VEHICLE
C-----(NEW NORT EQ 0)
      ILVL(IL) = IV
      GO TO 3010
 2050 CONTINUE
C-----SET MFINL AND MOASF TO FALSE, RESET IACC TO SLIGHTLY DECELERATING
C-----IF MSFLG EQ TRUE AND THE VEHICLE IS NOT DECELERATING, SET MSFLG
C-----TO FALSE, AND FINALLY STORE IV FOR NOFT FOR THE NORT VEHICLE (NEW
C-----NORT GT 0)
      CALL  FLGNOR ( .FALSE.,IV )
 3010 CONTINUE
      LEGAL(IV) = 2
      LPREV(IV) = LPRES(IV)
      LPRES(IV) = IL
      LNEXT(IV) = 0
C-----IF THE VEHICLE IS INVOLVED IN A MAJOR COLLISION THEN SET THAT THE
C-----NEW LANE HAS A MAJOR COLLISION
      IF ( MAJCOL(IV) )                          THEN
        LMJCOL(IL) = .TRUE.
      END IF
C-----CHECK MY LANE AND IF BLOCKED THEN SET PARAMETERS FOR BLOCKED LANE
      CALL  CHKMLN
      LALT(IV) = 1
C-----IF THERE IS A LANE TO THE RIGHT AND IF THE VEHICLE IS ALLOWED ON
C-----THE LANE TO THE RIGHT THEN SET LALT FOR RIGHT LANE IS AN ALTERNATE
      IF ( NLR(IL) . GT . 0 )                    THEN
        IF ( IAND( VEHTYP(IV),LAVT(NLR(IL)) ) . NE . 0 )
     *                                           THEN
          LALT(IV) = LALT(IV) + 1
        END IF
      END IF
C-----IF THERE IS A LANE TO THE LEFT AND IF THE VEHICLE IS ALLOWED ON
C-----THE LANE TO THE LEFT THEN SET LALT FOR LEFT LANE IS AN ALTERNATE
      IF ( NLL(IL) . GT . 0 )                    THEN
        IF ( IAND( VEHTYP(IV),LAVT(NLL(IL)) ) . NE . 0 )
     *                                           THEN
          LALT(IV) = LALT(IV) + 2
        END IF
      END IF
      ISET(IV) = 6
C-----IF THE VEHICLES REAR BUMPER IS ON THE OUTBOUND LANE THEN ALLOW
C-----LANE CHANGES
      IF ( (POSNEW-DBLE( LGEOM(1,IL) )) . GE . LENVAP )
     *                                           THEN
        ISET(IV) = 5
      END IF
C-----RESET SOME OF THE VEHICLE ATTRIBUTES
      IF ( INT1T(IV) . EQ . 0 )                  THEN
        INT2P(IV) = 0
      ELSE
        INT2P(IV) = IP
      END IF
                    IF ( IDISPD(IV) )            ISPD(IV) = ISPD(IV)/2
      ISPD  (IV) = IDNINT( DBLE( ISPD(IV)*ISLIM(IA) )/DBLE( LIMP(IP) ) )
      ISPDP (IV) = 0
      LATPOS(IV) = 0.0D0
      IDISPD(IV) = .FALSE.
      MPRO  (IV) = .TRUE.
      MSFLG (IV) = .FALSE.
      MTCARS(IV) = .FALSE.
      MININT(IV) = .FALSE.
C6    DISTAD(IV) = DISTAD(IV) + LENP(IP)
      IF ( SMJCOL . AND . (.NOT. MAJCOL(IV)) )   THEN
        MAJCLB(IV) = .FALSE.
        MAJCLL(IV) = .FALSE.
        MAJCON(IV) = .FALSE.
        POSCLB(IV) = POSBIG
        POSCLL(IV) = POSBIG
        POSCON(IV) = POSBIG
        CALL  CLMJCL  ( IL,LNEXT(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
        IF ( KV . GT . 0 )                       THEN
C-----    THERE IS A COLLISION VEHICLE ON THE LANE BEFORE THIS VEHICLE
C-----    OR DOWNSTREAM THUS SET THAT THE VEHICLE IS BLOCKED BY A MAJOR
C-----    COLLISION
          MAJCLB(IV) = .TRUE.
          POSCLB(IV) = DMIN1( POSCLB(IV),POSMJC )
C-----    IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----    VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----    FALSE
          EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *               ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
          RESPEV = ( RESPEV . OR . EVRESP )
        END IF
C-----  SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----  COLLISION MAY BLOCK THIS VEHICLE
        IF ( LCHKCF )                            THEN
          MAJCON(IV) = .TRUE.
          POSCON(IV) = DMIN1( POSCON(IV),POSMJC )
        END IF
      END IF
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
C9020 CONTINUE
C     CALL  ABORTR  ( 'STOP 902 - '                    //
C    *                'LNEXT IS NOT ON LLANES LIST - ' //
C    *                'LOGIOB'                            )
C     STOP  902
      END                                                               LOGIOB
C
C
C
      SUBROUTINE IBAP   ( AFLAG )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'CONFLT'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'SUMST2'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      BYTE              SSAMID,SSAMLN
      CHARACTER*1       AFLAG
      LOGICAL           ANYLAN,DIRPTH,EVRESP,IBLOCK,IFORCE,INQUE,LSFLG,
     *                  LCHNGE,LTPACT,LVMSDD,LVMSGO,LVMSRR,MAJCBL,
     *                  MFGOFS,NFINL,NPRO,WLNEXT
CS    LOGICAL           IHPRT
CY    LOGICAL           IAPRT,ILPRT
      INTEGER           IAE,IGARP,INDEX,IPE,IPR,IVC,IVE,JL,JH,JM,JNDEX,
     *                  JP,JSISET,KP,KSISET,KV,LAT,LV,MELIM,NV,NXVEH
C OLD INTEGER           JA.JLN
CS    INTEGER           IDESPD
CW    INTEGER           ICW
CX    INTEGER           IRN
CY    INTEGER           ICY
      INTEGER*4         SSAMLK,SSAMVI
      REAL*4            SSAMVA,SSAMVL,SSAMVS,SSAMVW,SSAMXF,SSAMXR,
     *                  SSAMYF,SSAMYR
      DOUBLE PRECISION  ACCVEH,ANGLE,DISEND,DISLCH,DLENV,DSPLCH,ENDADD,
     *                  ENDANG,ENDARC,ENDLST,FLENV,HWM,POSADD,POSCBL,
     *                  POSCST,POSLOG,POSNFB,POSNRB,POSOFB,POSORB,POSR,
     *                  POSSTP,POSVEH,PVILNI,SAFR,SAFVEL,SLPTMP,SLPVEH,
     *                  SSIPOS,TCRIT,TESTLP,TMAX,TS,VELLCH,VEHLNG,
     *                  VELVEH,X,XCRIT
C OLD DOUBLE PRECISION  POSLAT
CS    DOUBLE PRECISION  POSLCS
C7    DOUBLE PRECISION  POSLC7
  501 FORMAT(I3,I4,2I3,I1,I3,I6,I2,I1,2I3)
CS701 FORMAT(/,11H INBOUND ( ,A1,5H ) AT,F8.2,4H SEC,/,
CS   *       49H AP LN VEH  NUM NOF NOR NRC VEHPOS VEHVEL VEHACC ,
CS   *       46HACCSLP DS VC DC NX OA ST LG LF LC PR LPOS Q SG)
C7702 FORMAT(F7.2,I6,4I4,2F7.1)
CS703 FORMAT(2I3,I4,I6,2I4,2(I3,I4),2F7.2,2F7.3,10I3,F5.1,I5,3(1X,A10))
CX704 FORMAT(18(1X,A6))
CW751 FORMAT(8H APPRO  ,I3,1X,A1,1X,39I3)
CW753 FORMAT(8H LANE   ,I3,1X,28I4)
CW756 FORMAT(8H VEHD   ,I3,4F6.0,3I2,2I3,2I4,F6.0,2I4,I3,2I4,F4.0,F6.0,
CW   *                  I2,2(I3,I4),I3,1X,13L1,1X,7L1,/,8X,
CW   *                  21HCONTINUE W/ INTERNALS,18X,2I4,F6.0,2I4,3X,
CW   *                  2I4,4X,F6.0,L2,I3,L2,I6)
CW757 FORMAT(8H VEHF   ,I3,1X,10I4,F4.2,4I4,F4.2)
CW758 FORMAT(8H VEHIL  ,I3,1X,11L2,3X,8L2)
C
C-----SUBROUTINE IBAP PROCESSES THE VEHICLES ON THE INBOUND APPROACHES
C-----AND LOGS NEW VEHICLES INTO THE SYSTEM FROM THE QUEUE BUFFERS AS
C-----REQUIRED
C
C-----IPR VALUES
C-----0 FIRST DT OF A LANE CHANGE TO THE RIGHT THUS DO NOT WRITE VEHICLE
C-----1 NORMAL
C-----2 LOGIN
C-----3 LOGIBI
C-----4 LOGIOB OR LOGIIN
C-----5 LOGOUT
C-----6 LANE CHANGE
C
C-----LVMSDD = DISTRACTED DRIVER VMS MESSAGE WAS CANCELLED OR TIMED OUT
C
C[    IAE        = -2147483647
C[    IGARP      = -2147483647
C[    INDEX      = -2147483647
C[    IPE        = -2147483647
C[    IPR        = -2147483647
C[    IVE        = -2147483647
C[    JH         = -2147483647
C[    JL         = -2147483647
C[    JM         = -2147483647
C[    JNDEX      = -2147483647
C[    JP         = -2147483647
C[    JSISET     = -2147483647
C[    LAT        = -2147483647
C[    MELIM      = -2147483647
C[    NV         = -2147483647
C[    NXVEH      = -2147483647
C[    DISEND     = -2147483647.0
C[    DISLCH     = -2147483647.0
C[    FLENV      = -2147483647.0
C[    PVILNI     = -2147483647.0
C[    SSIPOS     = -2147483647.0
C[    TESTLP     = -2147483647.0
C*    NRNAME = 1
C*    IRNAME(NRNAME) = 'IBAP'
CS    IHPRT = .FALSE.
C-----PROCESS EACH INBOUND APPROACH
      DO 6010  IAN = 1 , NIBA
      IA = LIBA(IAN)
            IF ( IAFLAG(IA) . NE . AFLAG )       GO TO 6010
CS                  IF ( IHPRT )                 GO TO 101
CU                  IF ( TIME . LT . TPRINT )    GO TO 101
CS    WRITE (6,701) AFLAG,TIME
CS    IHPRT = .TRUE.
CS101 CONTINUE
CY    IAPRT = .FALSE.
CY                  IF ( (.NOT. IAPRT) )         GO TO 102
CW                  IF ( TIME . LT . TPRINT )    GO TO 102
CW    WRITE (6,751) IA,IAFLAG(IA),NLANES(IA),ISLIM(IA),IALEFT(IA),
CW   *              NSDR(IA),(LLANES(ICW,IA),ICW=1,NAL),
CW   *              (NVIL(ICW,IA),ICW=1,NAL),
CW   *              (ISDRN(ICW,IA),ICW=1,NIA-1),
CW   *              (ISDRA(ICW,IA),ICW=1,NIA-1)
CW102 CONTINUE
C-----PROCESS EACH LANE OF THE INBOUND APPROACH
      DO 5010  ILN = 1 , NLANES(IA)
C-----IF THERE ARE NO VEHICLES IN THIS LANE OR NO VEHICLES TO BE LOGGED
C-----INTO THIS LANE THIS DT THEN SKIP TO THE NEXT LANE
      IF ( NVIL(ILN,IA)+LQ(IAN,ILN) . LE . 0 )   THEN
        NVILNI(ILN,IA) = 0
        GO TO 5010
      END IF
      IL = LLANES(ILN,IA)
CY    ILPRT = .FALSE.
CY                  IF ( (.NOT. ILPRT) )         GO TO 103
CW                  IF ( TIME . LT . TPRINT )    GO TO 103
CW    WRITE (6,753) ILN,LWID(IL),NLL(IL),NLR(IL),ISNA(IL),NPINT(IL),
CW   *              IFVL(IL),ILVL(IL),LCONTR(IL),LTURN(IL),NLDL(IL),
CW   *              IBLN(IL),(LINTP(ICW,IL),ICW=1,NLP),
CW   *              (LLDL(ICW,IL),ICW=1,NLO),(LGEOM(ICW,IL),ICW=1,4)
CW103 CONTINUE
C-----IF THERE ARE NO VEHICLES IN THIS LANE THEN LOG IN THE NEW VEHICLE
      IF ( NVIL(ILN,IA) . LE . 0 )               THEN
        NVILNI(ILN,IA) = 0
        GO TO 4020
      END IF
      IGO = 1
      JSISET = 0
C-----IF THIS LANE IS NOT SIGNAL CONTROLLED OR THE SIGNAL INDICATION FOR
C-----THIS LANE HAS NOT CHANGED FROM THE OLD CAM STACK POSITION
C-----INDICATION THEN GO TO 1020 ELSE SET THE SIGNAL INDICATION FOR THE
C-----CURRENT CAM STACK POSITION AND INBOUND LANE NUMBER
                    IF ( LCONTR(IL) .LE. LCSTOP )GO TO 1020
                    IF ( ICAMPC . EQ . ICAMPO )  GO TO 1020
            IF ( ISISET(ICAMPC,IBLN(IL)) . EQ .
     *           ISISET(ICAMPO,IBLN(IL)) )       GO TO 1020
      JSISET = ISISET(ICAMPC,IBLN(IL))
 1020 CONTINUE
C-----SET THE INDEX FOR THE FIRST VEHICLE ON THIS LANE
      IV     = IFVL(IL)
      NV     = NVIL(ILN,IA)
      NCQ    = 0
      IVPV   = 0
      PVPOS  = DBLE( LGEOM(4,IL) ) + 1.5D0
      PVVEL  = ISLIM(IA)
      PVACC  = 0.0D0
      PVSLP  = 0.0D0
      IBLOCK = .FALSE.
      MAJCBL = .FALSE.
      POSCBL = 0.0D0
      INQUE  = .TRUE.
C-----PROCESS EACH VEHICLE ON THIS LANE
      DO 4010  IVN = 1 , NV
      IPR = 1
C*    NRNAME = 1
      HWM = 0.5D0*WIDV(IVEHCL(IV))
      IF ( MBLOCK(IV) )                          THEN
        ENDLN = DBLE( LGEOM(2,IL) )
      ELSE
        ENDLN = DBLE( LGEOM(4,IL) ) + 1.5D0
      END IF
      LCHNGE = .FALSE.
      RELEND = ENDLN - IPOS(IV)
C-----IF LEFT TURNING VEHICLES ARE ALLOWED TO PULL OUT INTO THE
C-----INTERSECTION, VEHICLE IS FIRST VEHICLE IN LANE, VEHICLE IS LEFT
C-----TURNER, LANE IS SIGNAL CONTROLLED, VEHICLE SIGNAL SETTING IS
C-----GREEN, LANE IS NOT BLOCKED, VEHICLE IS NOT BLOCKED BY A MAJOR
C-----COLLISION, VEHICLE IS NOT BLOCKED BY A MAJOR COLLISION ON A
C-----NON-DIRECT PATH, VEHICLE HAS CHOSEN AN INTERSECTION PATH, VEHICLES
C-----POSITION IS BEYOND XRELMX FEET BEYOND END OF LANE, AND THERE ARE
C-----NO EMERGENCY VEHICLE ANYWHERE IN THE SYSTEM THEN MOVE THE END OF
C-----THE LANE CLOSE TO THE FIRST GEOMETRIC INTERSECTION CONFLICT WHICH
C-----HAS POTENTIAL CONFLICTS (A VEHICLE THAT HAS NOT CLEARED THE
C-----INTERSECTION CONFLICT, AN INTERSECTION PATH FROM AN UNCONTROLLED
C-----LANE, OR AN INTERSECTION PATH FROM A SIGNAL CONTROLLED LANE WITH
C-----SIGNAL SETTING NOT RED)
      LTPACT = .FALSE.
      IF ( ( LTPOUT                   ) . AND .
     *     ( MFINL(IV)                ) . AND .
     *     ( ITURN(IV)  . EQ . ITURNL ) . AND .
     *     ( LCONTV(IV) . GE . LCSIGX ) . AND .
     *     ( MSSGRN(IV)               ) . AND .
     *     ( .NOT. MBLOCK(IV)         ) . AND .
     *     ( .NOT. MAJCLB(IV)         ) . AND .
     *     ( .NOT. MAJCLL(IV)         ) . AND .
     *     ( LNEXT(IV)  . GT . 0      ) . AND .
     *     ( RELEND     . LE . XRELMX ) . AND .
     *     ( .NOT. EVCCON             ) )        THEN
        KP = LNEXT(IV)
C-----  IF THERE ARE NO GEOMETRIC INTERSECTION CONFLICTS THEN GO TO 1026
C-----  AND DO NOT SET LEFT TURN PULL OUT
        IF ( NGEOCP(KP) . LE . 0 )               GO TO 1026
        DO 1022  INDEX = 1 , NGEOCP(KP)
        JNDEX = IGEOCP(INDEX,KP)
        IF ( KP . EQ . ICONP(1,JNDEX) )          THEN
          JH = 2
          JM = 1
        ELSE
          JH = 1
          JM = 2
        END IF
C-----  IF THE INTERSECTION CONFLICT IS SET THEN GO TO 1024 AND USE THIS
C-----  INTERSECTION CONFLICT
                    IF ( ICPSET(INDEX,KP).EQ.0 ) GO TO 1024
        JP = ICONP(JH,JNDEX)
        JL = LIBL(JP)
C-----  IF THE INTERSECTION PATH COMES FROM AN UNCONTROLLED LANE THEN
C-----  GO TO 1024 AND USE THIS INTERSECTION CONFLICT
        IF ( LCONTR(JL) . EQ . LCUNCT )          GO TO 1024
        IF ( LCONTR(JL) . LE . LCSTOP )          GO TO 1022
        CALL  SIGARP  ( ISISET(ICAMPC,IBLN(JL)),RITURN(IPT(JP)),IGARP )
C-----  IF THE INTERSECTION PATH COMES FROM A SIGNAL CONTROLLED LANE
C-----  WITH SIGNAL SETTING NOT RED THEN GO TO 1024 AND USE THIS
C-----  INTERSECTION CONFLICT
C[      IF ( IGARP            .EQ.-2147483647   )STOP 'IBAP   IGARP  01'
        IF ( IGARP . NE . 3 )                    GO TO 1024
 1022   CONTINUE
        GO TO 1026
 1024   CONTINUE
C[      IF ( JH               .EQ.-2147483647   )STOP 'IBAP   JH     01'
C[      IF ( JNDEX            .EQ.-2147483647   )STOP 'IBAP   JNDEX  01'
        LTPACT = .TRUE.
        SAFVEL = DBLE( LIMP(ICONP(JH,JNDEX)) )
        SAFR   = SAFDIS+(SAFVEL/SAFSPD)
        ENDADD = DMIN1( DMAX1( ICOND(JM,JNDEX)-2.0D0*HWM-SAFR,0.0D0 ),
     *                  2.0D0*XRELMX                                   )
        IF ( ( IRA1(KP) .GT. 0               ) . AND .
     *       ( ENDADD   .GT. DBLE( LL1(KP) ) ) ) THEN
          ENDANG = 1.0D0
     *        - 0.5D0*DBLE( LWID(IL)-WIDV(IVEHCL(IV)) )/DBLE( IRA1(KP) )
          ENDARC = DBLE( LL1(KP) ) + DBLE( IRA1(KP) )*DACOS( ENDANG )
     *           - SAFR
          ENDADD = DMIN1( ENDADD,DMAX1( ENDARC,0.0D0 ) )
        END IF
        ENDLN = ENDLN + ENDADD
        PVPOS = ENDLN
        IF ( MFSTPF(IV) )                        THEN
          IF ( FSTACT(IV) )                      THEN
            IF ( FSTPOS(IV) . LT . PVPOS )       PVPOS = FSTPOS(IV)
          END IF
          IF ( VMSASM(IV) . GT . 0 )             THEN
            IF ( (IVMSMG(VMSASM(IV)).EQ.VMSMSI) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSL) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSM) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSC) )THEN
              IF ( VMSPST(IV) . LT . PVPOS )     PVPOS = VMSPST(IV)
            END IF
          END IF
        END IF
        RELEND = ENDLN - IPOS(IV)
        XREL = XRELMX
                    IF ( MAJRLC(IV)         )    XREL = -0.5D0*XRELMI
                    IF ( RELEND . GT . XREL )    MSAOR(IV) = .FALSE.
 1026   CONTINUE
      END IF
C-----RESET THE PREVIOUS VEHICLE PARAMETERS TO THE NEW NOF IF THE
C-----VEHICLE IS LANE CHANGING, AND INITIALIZE SEVERAL PARAMETERS
C-----FOR THE VEHICLE
      CALL  PREST1  ( .FALSE.,LVMSDD )
      SSIPOS = PVPOS
      NFINL  = MFINL(IV)
      NPRO   = MPRO (IV)
      LSFLG  = .FALSE.
                    IF ( (.NOT. MFINL(IV)) )     GO TO 1040
                    IF ( MBLOCK(IV) )            GO TO 1030
                    IF ( IVN . EQ . 1 )          GO TO 1030
                    IF ( PVVEL . GT . VELSTP )   GO TO 1030
      MFINL(IV) = .FALSE.
      LSFLG = .TRUE.
      IF ( PVPOS .LE. (IPOS(IV)-LENVAP) )        THEN
        PVPOS = PVPOS + ENDLN - 1.5D0
      END IF
      IF ( MFSTPF(IV) )                          THEN
        IF ( FSTACT(IV) )                        THEN
          IF ( FSTPOS(IV) . LT . PVPOS )         PVPOS = FSTPOS(IV)
        END IF
        IF ( VMSASM(IV) . GT . 0 )               THEN
          IF ( ( IVMSMG(VMSASM(IV)).EQ.VMSMSI ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSL ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSM ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSC ) )THEN
            IF ( VMSPST(IV) . LT . PVPOS )       PVPOS = VMSPST(IV)
          END IF
        END IF
      END IF
      GO TO 1035
 1030 CONTINUE
C-----THIS VEHICLE IS THE FIRST VEHICLE IN THE LANE THUS RESET THE
C-----PREVIOUS VEHICLE PARAMETERS FOR THE END OF THE LANE
C-----IF THE LANE IS BLOCKED THEN (1) SET THE FLAG TO DISCONTINUE ANY
C-----CALCULATED DECELERATION TO A STOP, (2) SET THE DISTANCE FOR A
C-----LANE CHANGE, (3) SET THE DISTANCE FROM THE END OF THE BLOCKED LANE
C-----EQUAL TO ONE-HALF THE REMAINING DISTANCE TO THE END OF THE BLOCKED
C-----LANE WITH A MINIMUM OF DISLCH AND A MAXIMUM OF ONE-HALF THE LENGTH
C-----OF THE BLOCKED LANE, AND (4) SET THE POSITION OF THE PREVIOUS
C-----VEHICLE (WHERE THE CURRENT VEHICLE SHOULD STOP) EQUAL TO THE
C-----MAXIMUM OF ENDLN-DISEND AND POSNEW ELSE SET THE POSITION OF THE
C-----PREVIOUS VEHICLE EQUAL TO ENDLN
      IF ( IVPV . EQ . 0 )                       THEN
        PVVEL = 0.0D0
        PVACC = 0.0D0
        PVSLP = 0.0D0
        IF ( MBLOCK(IV) )                        THEN
          MSFLG(IV) = .FALSE.
          CALL  UNBIAS
          DSPLCH = DBLE( ISPD(IV) )
          IF ( IDISPD(IV) )                      THEN
            DSPLCH = 0.5D0*DSPLCH
          END IF
          IF ( ISPDP (IV) . EQ . 1 )             THEN
            DSPLCH = DSPLCH*DBLE( ISLIM(IA) )/DBLE( LIMP(LNEXT(IV)) )
          END IF
          VELLCH = 0.2D0*DSPLCH
          VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
          VEHLNG = DMIN1( 25.0D0,LENVAP )
          DISLCH = 0.5D0*(ENDLN-POSNEW)
          DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
          DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
          IF ( MAJRLC(IV) )                      THEN
            DISLCH = 0.5D0*XRELMI
          END IF
          DISEND = DMAX1(DISLCH,DMIN1(0.5D0*ENDLN,0.5D0*(ENDLN-POSNEW)))
          PVPOS  = DMAX1(ENDLN-DISEND,POSNEW)
        ELSE
          PVPOS = ENDLN
          PVVEL = DBLE( ISPD(IV) )
          PVACC = 0.0
          PVSLP = 0.0
        END IF
        IF ( MFSTPF(IV) )                        THEN
          IF ( FSTACT(IV) )                      THEN
            IF ( FSTPOS(IV) . LT . PVPOS )       PVPOS = FSTPOS(IV)
          END IF
          IF ( VMSASM(IV) . GT . 0 )             THEN
            IF ( (IVMSMG(VMSASM(IV)).EQ.VMSMSI) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSL) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSM) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSC) )THEN
              IF ( VMSPST(IV) . LT . PVPOS )     PVPOS = VMSPST(IV)
            END IF
          END IF
        END IF
      END IF
 1035 CONTINUE
      SSIPOS = PVPOS
                    IF ( IVPV . GT . 0 )         GO TO 1040
                    IF ( LNEXT(IV) . EQ . 0 )    GO TO 1040
      IF ( MPRO(IV) . OR . LTPACT )              THEN
        IF ( (.NOT. MPRO(IV)) )                  THEN
          KV = ILVP(LNEXT(IV))
          IF ( KV . EQ . 0 )                     GO TO 1040
          POSR = IPOS(KV) - LVAP(KV) - XRELMI + DBLE( LGEOM(4,IL) )
          IF ( POSR . GE . ENDLN )               GO TO 1040
        END IF
C-----  LOOK AHEAD AS FAR AS POSSIBLE FROM THE INBOUND LANE INTO THE
C-----  LINKING INTERSECTION PATH FOR THIS VEHICLE AND INTO THE LINKING
C-----  OUTBOUND LANE FOR THE LINKING INTERSECTION PATH (MAY BE AN
C-----  INTERNAL INBOUND LANE FOR A DIAMOND INTERSECTION) AND IF A
C-----  DIAMOND INTERSECTION THEN POSSIBLY INTO THE LINKING INTERSECTION
C-----  2 PATH AND THE LINKING OUTBOUND LANE FOR THE LINKING
C-----  INTERSECTION 2 PATH FOR THIS VEHICLE AND IF THERE IS A VEHICLE
C-----  AHEAD THEN RESET THE PREVIOUS VEHICLE PARAMETERS TO THE VEHICLE
C-----  AHEAD ELSE RESET THE PREVIOUS VEHICLE PARAMETERS TO THE END OF
C-----  THE LINK
        CALL  LOKIBI  ( DIRPTH,POSADD )
        IF ( IVPV . GT . 0 )                     THEN
          IF ( ( DIRPTH         ) . AND .
     *         (.NOT. MAJCOL(IV)) . AND .
     *         ( MAJCON(IVPV)   ) )              THEN
            MAJCON(IV) = .TRUE.
            POSCON(IV) = DMIN1( POSCON(IV),POSCON(IVPV)+POSADD )
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVPV
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----      FALSE
            EVRESP = ( ( IAND( VEHTYP(IV  ),LAVTE ) . EQ . 0 ) . AND .
     *                 ( IAND( VEHTYP(IVPV),LAVTE ) . NE . 0 ) )
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
        END IF
C-----  IF THE PREVIOUS VEHICLES REAR BUMPER IS STILL IN THIS LANE AND
C-----  THE PREVIOUS VEHICLE IS NO LONGER STOPPED THEN SET THE FLAG TO
C-----  DISCONTINUE DECELERATION FOR A STOP
        IF ( ( PVPOS . LT . ENDLN ) . AND .
     *       ( PVVEL . GT . 0.0D0 ) )            MSFLG(IV) = .FALSE.
      END IF
 1040 CONTINUE
C-----IF THIS IS THE FIRST DT OF A LANE CHANGE THEN SET THE FLAG TO
C-----DISCONTINUE DECELERATION FOR A STOP
      TESTLP = DABS( DABS( LATPOS(IV) )-(0.5D0*LEGAL(IV) ) )
                    IF ( TESTLP . LE . 0.01D0 )  MSFLG(IV) = .FALSE.
      MFGOFS = MFGOF(IV)
C-----IF THIS VEHICLE IS TRYING TO REVERSE A LANE CHANGE THEN SET
C-----FORCED GO FLAG
      IF ( MAJRLC(IV) )                          THEN
        IF ( (PVPOS-IPOS(IV)).GE.-0.5D0*XRELMI ) THEN
          MFGOF(IV) = .TRUE.
        END IF
      END IF
C-----COMPUTE NEW ACC/DEC LOGIC FOR THE VEHICLE
      CALL  PREST2
      MFGOF(IV) = MFGOFS
      MFINL(IV) = NFINL
      MPRO (IV) = NPRO
                    IF ( LOGFLG(IV) . GT . 1 )   LOGFLG(IV)=LOGFLG(IV)-1
      LOGTMP = LOGFLG(IV)
CY          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 107
CW                  IF ( TIME . LT . TPRINT )    GO TO 107
CS                  IF ( IHPRT )                 GO TO 104
CS    WRITE (6,701) AFLAG,TIME
CS    IHPRT = .TRUE.
CS104 CONTINUE
CY                  IF ( IAPRT )                 GO TO 105
CY    WRITE (6,751) IA,IAFLAG(IA),NLANES(IA),ISLIM(IA),IALEFT(IA),
CY   *              NSDR(IA),(LLANES(ICY,IA),ICY=1,NAL),
CY   *              (NVIL(ICY,IA),ICY=1,NAL),
CY   *              (ISDRN(ICY,IA),ICY=1,NIA-1),
CY   *              (ISDRA(ICY,IA),ICY=1,NIA-1)
CY    IAPRT = .TRUE.
CY105 CONTINUE
CY                  IF ( ILPRT )                 GO TO 106
CY    WRITE (6,753) ILN,LWID(IL),NLL(IL),NLR(IL),ISNA(IL),NPINT(IL),
CY   *              IFVL(IL),ILVL(IL),LCONTR(IL),LTURN(IL),NLDL(IL),
CY   *              IBLN(IL),(LINTP(ICY,IL),ICY=1,NLP),
CY   *              (LLDL(ICY,IL),ICY=1,NLO),(LGEOM(ICY,IL),ICY=1,4)
CY    ILPRT = .TRUE.
CY106 CONTINUE
CW    WRITE (6,757) IV,IDRICL(IV),IVEHCL(IV),ISPD  (IV),NOF   (IV),
CW   *                 NOR   (IV),LNEXT (IV),LPRES (IV),ITURN (IV),
CW   *                 IBAPS (IV),IPRTLO(IV),IEXTIM(IV),NOBAPD(IV),
CW   *                 INT2P (IV),INT2S (IV),INT1T (IV),IEXTII(IV)
CW    WRITE (6,756) IV,ISLP  (IV),IACC  (IV),IVEL  (IV),IPOS  (IV),
CW   *                 ISET  (IV),LCHGE (IV),ISPDP (IV),LEGAL (IV),
CW   *                 IPRTM (IV),ITIMV (IV),IQDS  (IV),ISPDS (IV),
CW   *                 ISDS  (IV),IDVS  (IV),ISTCON(IV),IVMAXA(IV),
CW   *                 IVMAXD(IV),LATPOS(IV),IDTS  (IV),LALT  (IV),
CW   *                 IPRC(1,IV),NORC(1,IV),IPRC(2,IV),NORC(2,IV),
CW   *                 LOGFLG(IV),MBLOCK(IV),MFGOF (IV),MFINL (IV),
CW   *                 MFSTPF(IV),MLAG  (IV),MOASF (IV),MPOBS (IV),
CW   *                 MPRO  (IV),MSAOR (IV),MSFLG (IV),MSTPF (IV),
CW   *                 MTCARS(IV),MININT(IV),IACDS (IV),IACLDS(IV),
CW   *                 ICDFS (IV),IFVA  (IV),IRSTOP(IV),ISDEC (IV),
CW   *                 ISTMO (IV),ITIMVI(IV),IQDSI (IV),ISPDSI(IV),
CW   *                 ISDSI (IV),IDVSI (IV),IVMXAI(IV),IVMXDI(IV),
CW   *                 IDTSI (IV),IUPDAT(IV),JVCNOR(IV),IDISPD(IV),
CW   *                 IQ(IV)
CW107 CONTINUE
                    IF ( LOGFLG(IV) . NE . 1 )   GO TO 1060
C-----COMPUTE NEW INTERSECTION CONTROL LOGIC
      CALL  LOGIC   ( 8,IV )
      LOGTMP = 2
 1060 CONTINUE
CY          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 108
CW                  IF ( TIME . LT . TPRINT )    GO TO 108
CW    WRITE (6,758) IV,MDEDIC(IV),MINFLZ(IV),MLUNC (IV),MIUNC (IV),
CW   *                 MLYELD(IV),MLSTOP(IV),MATSTL(IV),MSSRED(IV),
CW   *                 MLRTOR(IV),MSSGRN(IV),MCHKCF(IV),
CW   *                 IDEDIC(IV),INFLZ (IV),ILUNC (IV),ILYELD(IV),
CW   *                 ILSTOP(IV),ICONTN(IV),ICHKCF(IV),IERROR(IV)
CW108 CONTINUE
C-----UNBIAS THE VEHICLE ATTRIBUTES AND PREDICT THE NEW POS/VEL/ACC
      CALL  UNBIAS
      NXVEH = NOR(IV)
                    IF ( ISPDP(IV) . EQ . 1 )    GO TO 1080
                    IF ( MBLOCK(IV) )            GO TO 1080
                    IF ( LNEXT(IV) . EQ . 0 )    GO TO 1080
                    IF ( RELEND . LE . 25.0D0 )  GO TO 1070
                    IF ( VELOLD . LE .  0.0D0 )  GO TO 1080
 1070 CONTINUE
C-----CHECK TO SEE IF THE VEHICLE SHOULD RESET HIS DESIRED SPEED TO THE
C-----DESIRED SPEED OF HIS INTERSECTION PATH SO THAT HE CAN GRADUALLY
C-----DECELERATE TO HIS NEW DESIRED SPEED BEFORE HE ENTERS THE
C-----INTERSECTION
      CALL  CHKDSP
 1080 CONTINUE
                    IF ( IGO . EQ . 2 )          IGO = 3
      IF ( LALT(IV) . EQ . 6 )                   THEN
C-----  THIS IS THE FIRST DT OF A LANE CHANGE TO THE RIGHT THEREFORE
C-----  CALL INFLZN TO SET UP THE CORRECT RESPONSE TO THE NEW LANE'S
C-----  CONTROL
C-----  CHGMLN DOES THIS FOR THE FIRST DT OF A LANE CHANGE TO THE LEFT
        IPR = 6
        CALL  INFLZN
C-----  COMPUTE NEW INTERSECTION CONTROL LOGIC
        CALL  LOGIC   ( 8,IV )
        LOGTMP = 2
      END IF
C[    IF ( JSISET             .EQ.-2147483647   )STOP 'IBAP   JSISET 01'
      KSISET = JSISET
C-----THIS LANE IS NOT SIGNAL CONTROLLED THEN GO TO 1090
                    IF ( LCONTV(IV).LE.LCSTOP )  GO TO 1090
C-----IF THE VEHICLE IS FORCED TO GO OR THE VEHICLE IS FORCED TO RUN THE
C-----RED SIGNAL OR A DISTRACTED DRIVER MESSAGE IS CANCELLED THEN SET
C-----THE SIGNAL INDICATION FOR THE CURRENT CAM STACK POSITION AND
C-----INBOUND LANE NUMBER
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        LVMSGO = ( IVMSMG(VMSASM(IV)) . EQ . VMSMGO )
        LVMSRR = ( IVMSMG(VMSASM(IV)) . EQ . VMSMRR )
      ELSE
        LVMSGO = .FALSE.
        LVMSRR = .FALSE.
      END IF
      IF ( ( MFGOF(IV)                                ) .OR.
     *     ( ( FRRTIM(IV) . GT . 0.0D0              ) .AND.
     *       ( TIME .GE.  FRRTIM(IV)                ) .AND.
     *       ( TIME .LE. (FRRTIM(IV)+FRRATM(IV))    ) ) .OR.
     *     ( LVMSGO                                   ) .OR.
     *     ( LVMSRR                                   ) .OR.
     *     ( LVMSDD                                   ) )
     *                                           THEN
        KSISET = ISISET(ICAMPC,IBLN(IL))
      END IF
      IF ( SIGRGO(IV) . AND . (VELOLD.EQ.0.0D0) )THEN
        KSISET = ISISET(ICAMPC,IBLN(IL))
        MPRO(IV) = .FALSE.
      END IF
      IF ( MAJCBP . OR . MAJSIG(IV) )            THEN
        KSISET = ISISET(ICAMPC,IBLN(IL))
        MAJSIG(IV) = .FALSE.
      END IF
 1090 CONTINUE
C-----IF THE VEHICLE IS PROCESSING A DISTRACTED DRIVER MESSAGE THEN DO
C-----NOT PROCESS THE SIGNAL SETTING
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        IF ( IVMSMG(VMSASM(IV)) . EQ . VMSMDD )  THEN
          KSISET = 0
        END IF
      END IF
                    IF ( KSISET . EQ . 0 )       GO TO 2010
                    IF ( (.NOT. MDEDIC(IV)) )    GO TO 2010
                    IF ( (.NOT. MINFLZ(IV)) )    GO TO 2010
C-----THE SIGNAL INDICATION HAS CHANGED FOR THIS LANE, THE VEHICLE IS
C-----DEDICATED TO AN INTERSECTION PATH, AND THE VEHICLE IS WITHIN THE
C-----INFLUENCE ZONE OF THE INTERSECTION CONTROL THUS DETERMINE THE
C-----APPROPRIATE DRIVER RESPONSE FOR THE NEW SIGNAL INDICATION
      IF ( (.NOT. MSSGRN(IV)) . AND . (.NOT. MSSRED(IV)) )
     *                                           THEN
        MSSGRN(IV) = .TRUE.
        MPRO  (IV) = .FALSE.
      END IF
      CALL  SIGRES  ( KSISET )
 2010 CONTINUE
      IF ( LALT(IV) . EQ . 6 )                   THEN
        LALT(IV) = 5
        GO TO 2020
      END IF
                    IF ( ISET(IV) . NE . 1 )     GO TO 2020
      IPR = 6
C-----COMPUTE THE NEW LATERAL POSITION FOR A LANE CHANGE USING A COSINE
C-----CURVE AND IF FINISHED THEN END THE LANE CHANGE
      CALL  LCHGEO
      LCHNGE = .TRUE.
                    IF ( ISET(IV) . EQ . 1 )     GO TO 2020
C-----THE LANE CHANGE IS FINISHED THUS FIND THE INTERSECTION PATH FOR
C-----THIS VEHICLE BASED ON THE CURRENT APPROACH, CURRENT LANE, AND THE
C-----DESIRED OUTBOUND APPROACH AND CALL INFLZN TO SET UP THE CORRECT
C-----RESPONSE TO THE NEW LANE'S CONTROL (PATHF MAY HAVE SET NEW LNEXT)
      CALL  PATHF   ( .FALSE.,0,'IBAP' )
      CALL  INFLZN
 2020 CONTINUE
C-----FIND THE TIME AND DISTANCE TO SWITCH FROM LANE CHANGING FROM
C-----BEHIND A SLOW VEHICLE TO LANE CHANGING CLOSE TO THE INTERSECTION
      TCRIT = (LCSTIM+PIJR(IDRICL(IV)))/DCHAR(IDRICL(IV))
      XCRIT = TCRIT*DBLE( ISPD(IV) )
      JP    = LPREV(IV)
C-----IF THE VEHICLES REAR BUMPER IS ON THE DIAMOND INTERNAL INBOUND
C-----LANE THEN ALLOW LANE CHANGES
      IF ( (IATYPE(IA)                 . EQ . DINBDL ) . AND .
     *     (ISET(IV)                   . EQ . 6      ) . AND .
     *     (POSNEW-DBLE( LGEOM(1,IL) ) . GE . LENVAP ) )
     *                                           THEN
        ISET(IV) = 5
      END IF
C-----CHECK IF ISET CAN BE SET TO 5
      CALL  CKISET  ( IV,POSNEW )
C-----IF EMERGENCY VEHICLE THEN ALLOW LANE CHANGES
      IF ( ( ISET(IV)                 .EQ. 6 ) . AND .
     *     ( IAND( VEHTYP(IV),LAVTE ) .NE. 0 ) ) THEN
        ISET(IV) = 5
      END IF
                    IF ( MAJCLC )                GO TO 2040
C-----PROCESS VEHICLE MESSAGE SYSTEM CHANGE LANE MESSAGE
      IF ( VMSACM(IV) . GT . 0 )                 THEN
        IF ( ( IVMSMG(VMSACM(IV)) .EQ. VMSMCL ) . OR .
     *       ( IVMSMG(VMSACM(IV)) .EQ. VMSMCR ) )THEN
          IF ( ISET(IV) . NE . 1 )               GO TO 2040
        END IF
      END IF
                    IF ( ISET(IV) . LE . 1 )     GO TO 2055
                    IF ( ISET(IV) . GE . 6 )     GO TO 2050
C-----IF THE CURRENT LANE IS BLOCKED THEN DETERMINE IF A LANE CHANGE IS
C-----DESIRABLE
                    IF ( MBLOCK(IV) )            GO TO 2040
C-----IF EMERGENCY VEHICLE THEN DETERMINE IF A LANE CHANGE IS DESIRABLE
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   GO TO 2040
      FLENV = 4.0D0*LENVAP
      IF ( LALT(IV) . GE . 5 )                   CALL  CKLALT
C-----IF THE LEFT LANE IS AN ALTERNATIVE LANE THEN MIN FLENV WITH 0.5D0
C-----TIMES THE LENGTH OF THE LEFT LANE
      IF ( (LALT(IV).EQ.3) .OR. (LALT(IV).EQ.4) )THEN
        FLENV = DMIN1( FLENV,
     *                 0.5D0*DBLE( LGEOM(4,NLL(IL))-LGEOM(3,NLL(IL)) ) )
      END IF
C-----IF THE RIGHT LANE IS AN ALTERNATIVE LANE THEN MIN FLENV WITH 0.5D0
C-----TIMES THE LENGTH OF THE RIGHT LANE
      IF ( (LALT(IV).EQ.2) .OR. (LALT(IV).EQ.4) )THEN
        FLENV = DMIN1( FLENV,
     *                 0.5D0*DBLE( LGEOM(4,NLR(IL))-LGEOM(3,NLR(IL)) ) )
      END IF
C-----IF THE DISTANCE TO THE END OF THE LANE IS GT 4 VEHICLE LENGTHS
C-----THEN DETERMINE IF A LANE CHANGE IS DESIRABLE
                    IF ( RELEND . GT . FLENV )   GO TO 2040
C-----IF THE DISTANCE TO THE END OF THE LANE IS LT 2 VEHICLE LENGTHS
C-----THEN A LANE CHANGE SHOULD NO LONGER BE CONSIDERED
                    IF ( RELEND.LT.0.5D0*FLENV ) GO TO 2030
C-----IF THE LANE CHANGE IS FORCED (NOT OPTIONAL) WHEN THE DISTANCE TO
C-----THE END OF THE LANE IS BETWEEN 2 AND 4 VEHICLE LENGTHS THEN
C-----DETERMINE IF A LANE CHANGE IS DESIRABLE ELSE A LANE CHANGE SHOULD
C-----NO LONGER BE CONSIDERED
                    IF ( LEGAL(IV) . EQ . 1 )    GO TO 2040
                    IF ( LEGAL(IV) . EQ . 3 )    GO TO 2040
 2030 CONTINUE
C-----A LANE CHANGE SHOULD NO LONGER BE CONSIDERED BECAUSE IT IS TOO
C-----CLOSE TO THE END OF THE LANE
      ISET(IV) = 6
                    IF ( LNEXT(IV) . GT . 0 )    GO TO 2055
C-----THE VEHICLE CAN NOT CHANGE LANES AND IT HAS NOT YET FOUND AN
C-----INTERSECTION PATH THUS FORCE AN INTERSECTION PATH TO BE FOUND FOR
C-----THIS VEHICLE BASED ON THE CURRENT APPROACH, CURRENT LANE, AND THE
C-----DESIRED OUTBOUND APPROACH
      CALL  PATHF   ( .TRUE.,0,'IBAP' )
C-----CALL INFLZN TO SET UP THE CORRECT RESPONSE TO THE NEW PATH LANE
C-----CONTROL
      CALL  INFLZN
      GO TO 2055
 2040 CONTINUE
                    IF ( LCHNGE )                GO TO 2055
C-----DO NOT LET A LANE CHANGE START UNTIL THE VEHICLES REAR BUMPER HAS
C-----CLEARED ALL INTERSECTION CONFLICTS AND IS ON THE INBOUND LANE
      IF ( JP . GT . 0 )                         THEN
        IF ( ISTCON(IV) . LE . NGEOCP(JP) )      GO TO 2055
        IF ( ( LENVAP . LE . (0.5D0*ENDLN)                ) . AND .
     *       ( (POSNEW-DBLE( LGEOM(1,IL) )) . LT . LENVAP ) )
     *                                           GO TO 2055
      END IF
C-----IF THE VEHICLE IS GREATER THAN OR EQUAL TO LCSTIM SECONDS FROM THE
C-----END OF THE LANE THEN DO NOT CHECK FOR A LANE CHANGE CLOSE TO THE
C-----INTERSECTION
                    IF ( RELEND . GE . XCRIT )   GO TO 2050
C-----DETERMINE IF A LANE CHANGE CLOSE TO THE INTERSECTION IS DESIRABLE
      CALL  LCHDES  ( .FALSE.,.FALSE. )
                    IF ( ISET(IV) . EQ . 1 )     IPR = 6
      IF ( LALT(IV) . EQ . 6 )                   THEN
        IPR = 0
C3      JPFLAG = 'START LCHG'
C3      KPFLAG = ' RIGHT'
        PVILNI = POSNEW
        GO TO 3020
      END IF
      GO TO 2055
 2050 CONTINUE
                    IF ( LCHNGE )                GO TO 2055
                    IF ( ISET(IV) . LE . 1 )     GO TO 2055
C-----DO NOT LET A LANE CHANGE START UNTIL THE VEHICLES REAR BUMPER HAS
C-----CLEARED ALL INTERSECTION CONFLICTS AND IS ON THE INBOUND LANE
      IF ( JP . GT . 0 )                         THEN
        IF ( ISTCON(IV) . LE . NGEOCP(JP) )      GO TO 2055
        IF ( ( LENVAP . LE . (0.5D0*ENDLN)                ) . AND .
     *       ( (POSNEW-DBLE( LGEOM(1,IL) )) . LT . LENVAP ) )
     *                                           GO TO 2055
      END IF
C-----IF EMERGENCY VEHICLE THEN DETERMINE IF A LANE CHANGE FROM BEHIND A
C-----SLOW VEHICLE IS DESIRABLE
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   THEN
        ANYLAN = .TRUE.
      ELSE
C-----  IF THE VEHICLE IS LESS THAN LCSTIM SECONDS FROM THE END OF THE
C-----  LANE THEN DO NOT CHECK FOR A LANE CHANGE FROM BEHIND A SLOW
C-----  VEHICLE
                    IF ( RELEND . LT . XCRIT )   GO TO 2055
        ANYLAN = (RELEND . GT . 2.0D0*XCRIT)
      END IF
C-----DETERMINE IF A LANE CHANGE FROM BEHIND A SLOW VEHICLE IS DESIRABLE
      CALL  LCHDES  ( .TRUE.,ANYLAN )
                    IF ( ISET(IV) . EQ . 1 )     IPR = 6
      IF ( LALT(IV) . EQ . 6 )                   THEN
        IPR = 0
C3      JPFLAG = 'START LCHG'
C3      KPFLAG = ' RIGHT'
        PVILNI = POSNEW
        GO TO 3020
      END IF
 2055 CONTINUE
C-----CHECK THE NOF VEHICLE
      IF ( (.NOT. MBLOCK(IV)) . AND .
     *     (      MFINL (IV)) . AND .
     *     (.NOT. MPRO  (IV)) )                  THEN
        CALL  CHKNOF
      END IF
C-----CHECK IF THIS VEHICLE WOULD BLOCK THE INTERSECTION
      CKINTB(IV) = .FALSE.
      IF ( (.NOT. MBLOCK(IV)                      ) . AND .
     *     (.NOT. IBLOCK                          ) . AND .
     *     (POSOLD.LE.DBLE( LGEOM(4,IL) )+2*XRELMI) )
     *                                           THEN
        CALL  CHKINT  ( LNEXT(IV),.TRUE.,.TRUE.,IBLOCK,MAJCBL,POSCBL )
        CKINTB(IV) = IBLOCK
      END IF
C-----CHECK IF THERE IS A MAJOR COLLISION SOMEWHERE IN THE SYSTEM
      POSSTP = POSBIG
      POSCST = POSSTP
      IF ( MAJCLB(IV) . OR . MAJCLL(IV) )        THEN
C-----  THERE IS A COLLISION VEHICLE ON THE LANE BEFORE THIS VEHICLE OR
C-----  DOWNSTREAM THUS CALCULATE A DECEL TO A STOP JUST BEFORE THE
C-----  POINT OF COLLISION OR THE STOP LINE WHICHEVER IS CLOSER
        SAFVEL = DMAX1( VELOLD,VELNEW,DESVEL )
        SAFR   = (SAFDIS+(SAFVEL/SAFSPD))/DCHAR(IDRICL(IV))
        POSCST = DMIN1( POSCLB(IV),POSCLL(IV) ) - 2.0D0*HWM - SAFR
        ENDLST = ENDLN
        IF ( LNEXT(IV) . GT . 0 )                THEN
          IF ( .NOT. CKINTB(IV) )                THEN
            CALL  CHKINT  ( LNEXT(IV),.TRUE.,.TRUE.,IBLOCK,MAJCBL,
     *                      POSCBL                                 )
            CKINTB(IV) = IBLOCK
          END IF
          IF ( .NOT. CKINTB(IV) )                THEN
            ENDLST = POSCST
          END IF
        END IF
        POSVEH = DMIN1( POSCST,ENDLST )
        VELVEH = 0.0D0
        ACCVEH = 0.0D0
        SLPVEH = 0.0D0
        CALL  SLPCFS  ( SLPTMP,IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                            POSVEH,VELVEH,ACCVEH,SLPVEH  )
        IF ( SLPTMP . NE . 0.0D0 )               THEN
          SLPBLK = DMIN1( SLPBLK,SLPTMP )
          IF ( ( MFINL (IV)                   ) . AND .
     *         ( POSNEW . GE . (ENDLN+XRELMX) ) )THEN
C-----      SET INTERSECTION CONFLICTS
            CALL  SETCON
          END IF
        END IF
      END IF
C-----CHECK THE ACC/DEC LOGICAL DEPENDENT ATTRIBUTES, CALL THE
C-----APPROPRIATE ACC/DEC ROUTINES, AND COMPUTE THE VEHICLES NEW POS/
C-----VEL/ACC
      CALL  ACDCP   ( .FALSE. )
      PVILNI = POSNEW
C-----IF THE VEHICLE CAME FROM AN INTERSECTION PATH (THIS IS A DIAMOND
C-----INTERCHANGE INTERNAL INBOUND LANE) AND THE VEHICLES REAR BUMPER
C-----IS STILL ON THE INTERSECTION PATH THEN CLEAR INTERSECTION
C-----CONFLICTS
      JP = LPREV(IV)
      IF ( JP . GT . 0 )                         THEN
        IF ( ISTCON(IV) . LE . NGEOCP(JP) )      THEN
          POSORB = POSOLD - DBLE( LGEOM(1,IL) ) - LENVAP
          IF ( POSORB . LE . 0.1D0 )             THEN
            POSNRB = POSNEW - DBLE( LGEOM(1,IL) ) - LENVAP
C-----      IF THE VEHICLES REAR BUMPER IS CROSSING FROM THE INTERSECTION
C-----      PATH TO THE INBOUND LANE THIS DT THEN FORCE CLEARING OF ALL
C-----      INTERSECTION CONFLICTS
            IFORCE = ((POSNRB+0.1D0) . GE. 0.0D0)
          ELSE
            IFORCE = .TRUE.
          END IF
          POSNFB = POSNEW - DBLE( LGEOM(1,IL) ) + DBLE( LENP(JP) )
          CALL  CLRCON ( JP,POSNFB,IFORCE )
        END IF
      END IF
C-----IF THE VEHICLE HAS THE RIGHT TO ENTER THE INTERSECTION THEN AVOID
C-----INTERSECTION CONFLICTS
      IF ( MPRO(IV)           . AND .
     *     (VELNEW.GT.VELSTP) . AND .
     *     (.NOT. MSFLG(IV) ) )                  THEN
        CALL  AVDCON
      END IF
C7    IF ( ISET(IV) . EQ . 1 )                   THEN
C7      POSLC7 = LATPOS(IV)
C7    ELSE
C7      POSLC7 = 0.0D0
C7    END IF
C7    WRITE (PPP,702) TIME,IQ(IV),1,IA,IL,IVEHCL(IV),POSNEW,POSLC7
C0          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 109
C-----PRINT POS/VEL/ACC FOR THE VEHICLE
CZ    CALL  PVAPRT
C0109 CONTINUE
                    IF ( ICONTR . LT . ICSACT )  GO TO 2065
                    IF ( NLDL(IL) . LE . 0 )     GO TO 2060
C-----CHECK EACH DETECTOR FOR THIS LANE TO SEE IF THIS VEHICLE TRIPPED
C-----ANY OF THEM THIS DT
      CALL  CHKLDT  ( IL,POSOLD,POSNEW )
 2060 CONTINUE
                    IF ( JP . LE . 0 )           GO TO 2065
      JL = LIBL(JP)
                    IF ( JL . LE . 0 )           GO TO 2065
                    IF ( NLDL(JL) . LE . 0 )     GO TO 2065
C-----CHECK IF REAR BUMPER STILL ON INBOUND LANE
      POSORB = POSOLD - DBLE( LGEOM(1,IL)-LENP(JP) ) - LENVAP
                    IF ( POSORB . GE . 0.0D0 )   GO TO 2065
      POSOFB = POSOLD - DBLE( LGEOM(1,IL)-LENP(JP)-LGEOM(4,JL) )
      POSNFB = POSNEW - DBLE( LGEOM(1,IL)-LENP(JP)-LGEOM(4,JL) )
C-----CHECK EACH DETECTOR FOR THIS LANE TO SEE IF THIS VEHICLE TRIPPED
C-----ANY OF THEM THIS DT
      CALL  CHKLDT  ( JL,POSOFB,POSNFB )
 2065 CONTINUE
                    IF ( LOGFLG(IV) - 1 )        2080 , 2070 , 2080
 2070 CONTINUE
C-----IF THIS IS NOT THE FIRST DT OF A LANE CHANGE THEN CALL INTLOG
      IF ( LPRES(IV) . EQ . IL )                 THEN
C-----  CHECK THE INTERSECTION CONTROL LOGICAL DEPENDENT ATTRIBUTES AND
C-----  CALL THE APPROPRIATE INTERSECTION CONTROL ROUTINES
        CALL  INTLOG
      END IF
C-----CHECK IF THIS VEHICLE WOULD BLOCK THE INTERSECTION
      IF ( (.NOT. MBLOCK(IV)                      ) . AND .
     *     (.NOT. IBLOCK                          ) . AND .
     *     (POSOLD.LE.DBLE( LGEOM(4,IL) )+2*XRELMI) )
     *                                           THEN
        CALL  CHKINT  ( LNEXT(IV),.TRUE.,.TRUE.,IBLOCK,MAJCBL,POSCBL )
        IF ( IBLOCK )                            THEN
C-----    CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER DT SECONDS
          CALL  ACDCP  ( .TRUE. )
        END IF
      END IF
 2080 CONTINUE
C-----UPDATE THE VEHICLES SIMULATION STATISTICS ON THE INBOUND APPROACH
C[    IF ( SSIPOS             .EQ.-2147483647.0 )STOP 'IBAP   SSIPOS 01'
      CALL  SSIBAP  ( SSIPOS,INQUE )
      IF ( PVPOS+XRELMI . LE . POSNEW )          THEN
C-----  PRINT THE COLLISION INFORMATION AND RESET THE VEHICLES
C-----  POS/VEL/ACC
        CALL  BANGS  ( 1,IVPV,RELVEL,RELPOS,0.0D0 )
      END IF
C-----CHECK IF THE VEHICLE SHOULD NOT LOGIBI
      IF ( POSNEW . LE . DBLE( LGEOM(4,IL) ) )   GO TO 3010
                    IF ( (.NOT. MFINL(IV))   )   GO TO 3010
                    IF ( (.NOT. MPRO(IV))    )   GO TO 3010
                    IF ( IPRTM(IV) . GT . 1  )   GO TO 3010
                    IF ( LNEXT(IV) . EQ . 0  )   GO TO 3010
                    IF ( IBLOCK              )   GO TO 3010
                    IF ( CKINTB(IV)          )   GO TO 3010
                    IF ( VELNEW . EQ . 0.0D0 )   GO TO 3010
      TMAX = 30.0D0
      CALL  TIMSTP  ( VELOLD,ACCOLD,SLPNEW,TMAX,TS )
                    IF ( TS . EQ . TIMERR )      GO TO 2090
      IF ( TS . GT . 0.0D0  )                    THEN
        X = POSOLD + VELOLD*TS + 0.5D0*ACCOLD*TS**2 + ONED6*SLPNEW*TS**3
        POSLOG = DBLE( LGEOM(4,IL) ) + 2.0D0*XRELMX
                    IF ( X . LE . POSLOG )       GO TO 3010
      END IF
      IF ( LTPACT .OR. MAJCOL(IV) .OR. MAJCLB(IV) .OR. MAJCLL(IV) )
     *                                           THEN
        POSLOG = DBLE( LGEOM(4,IL) )
        IF ( NGEOCP(LNEXT(IV)) . GT . 0 )        THEN
          JNDEX = IGEOCP(1,LNEXT(IV))
          IF ( LNEXT(IV) . EQ . ICONP(1,JNDEX) ) THEN
            JH = 2
            JM = 1
          ELSE
            JH = 1
            JM = 2
          END IF
          IF ( LTPACT )                          THEN
            SAFVEL = DBLE( LIMP(ICONP(JH,JNDEX)) )
            SAFR   = SAFDIS+(SAFVEL/SAFSPD)
          ELSE
            SAFVEL = 0.5D0*DBLE( LIMP(ICONP(JH,JNDEX)) )
            SAFR   = SAFDIS+(SAFVEL/SAFSPD)
          END IF
          POSLOG = POSLOG + DBLE( ICOND(JM,JNDEX) ) - 2.0D0*HWM - SAFR
        ELSE
          POSLOG = POSLOG + 2.0D0*XRELMX
        END IF
        IF ( POSSTP . LT . POSCST )              THEN
          POSLOG = 0.0D0
        END IF
        IF ( POSNEW . LE .POSLOG )               GO TO 3010
      END IF
 2090 CONTINUE
                    IF ( SLPBLK . NE . 0.0D0 )   GO TO 3010
C-----LOG THE VEHICLE OUT OF THE INBOUND APPROACH AND LANE AND INTO THE
C-----LINKING INTERSECTION PATH FOR THE VEHICLE
      CALL  LOGIBI
C3    KPFLAG = 'ENTER INTR'
      IPR = 3
 3010 CONTINUE
C-----BIAS THE VEHICLE ATTRIBUTES, SET THE PREVIOUS VEHICLE PARAMETERS,
C-----AND UPDATE THE MAXIMUM ACC/DEC FOR THE VEHICLE
      CALL  BIAS    ( IPR )
 3020 CONTINUE
C-----SET VEHICLE HEADING
      ANGLE = DBLE( IAAZIM(IA) ) + STEERA(IV)
                    IF ( ANGLE . LT .   0.0D0 )  ANGLE = ANGLE+360.0D0
                    IF ( ANGLE . GE . 360.0D0 )  ANGLE = ANGLE-360.0D0
      HEADNG(IV) = ANGLE
      STEERA(IV) = 0.0D0
C-----CHECK CLEARING PREVIOUS PATH MAJOR COLLISION
      IF ( MAJCOL(IV) .AND. ( IPR . NE . 3 ) )   THEN
        DLENV = LENVAP + DBLE( LGEOM(1,IL) )
        IF ( ( POSOLD    . LE . DLENV ) . AND .
     *       ( POSNEW    . GT . DLENV ) . AND .
     *       ( LPREV(IV) . GT . 0     ) )        THEN
C-----    FIND THE FIRST COLLISION VEHICLE IN THE PREVIOUS PATH
C-----    STARTING FROM THE FIRST VEHICLE IN THE PREVIOUS PATH
          CALL  FFCVLP  ( .FALSE.,LPREV(IV),.TRUE.,LV,POSADD )
          IF ( LV . EQ . 0 )                     THEN
            PMJCOL(LPREV(IV)) = .FALSE.
          END IF
        END IF
      END IF
C-----PRINT SELECTED ATTRIBUTES FOR VEHICLE IV
                    IF ( JPRTM . NE . 0 )        IPRTM(IV) = JPRTM
CV          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 110
CU                  IF ( TIME . LT . TPRINT )    GO TO 110
C3                  IF ( JPRTM . GT . 0 )        JPFLAG = 'PIJR TIME '
CS    IDESPD = IDNINT( DESVEL )
CS    IF ( ISET(IV) . EQ . 1 )                   THEN
CS      POSLCS = LATPOS(IV)
CS    ELSE
CS      POSLCS = 0.0D0
CS    END IF
CS    WRITE (6,703) IA,ILN,IV,IQ(IV),NOF(IV),NOR(IV),IPRC(1,IV),
CS   *              NORC(1,IV),IPRC(2,IV),NORC(2,IV),POSNEW,VELNEW,
CS   *              ACCNEW,SLPNEW,IDESPD,IVEHCL(IV),IDRICL(IV),
CS   *              LNEXT(IV),NOBAPD(IV),ISET(IV),LEGAL(IV),LOGFLG(IV),
CS   *              LCHGE(IV),IPRTM(IV),POSLCA,0,IPFLAG,JPFLAG,KPFLAG
CU110 CONTINUE
      LOGFLG(IV) = LOGTMP
                    IF ( LSFLG )                 MSFLG(IV) = .FALSE.
      IF ( (IPOLL . EQ . INO   ) . OR .
     *     (TIME  . LT . BEGT20) . OR .
     *     (TIME  . GT . ENDT20) )               GO TO 112
C[    IF ( IPR                .EQ.-2147483647   )STOP 'IBAP   IPR    01'
                    IF ( IPR . EQ . 0 )          GO TO 112
      IPE = IDNINT( POSNEW )
                    IF ( IPE . GT . 9999 )       IPE = 9999 - IPE
C-----SET ANIMATION VEHICLE CODE
C-----ICNONE   0 VEHICLE CODE - NONE
      IVC = ICNONE
C-----ICYLTS   1 VEHICLE CODE - YELLOW LEFT TURN SIGNALS FRONT AND REAR
      IF ( ( ITURN(IV) . EQ . ITURNU ) . OR .
     *     ( ITURN(IV) . EQ . ITURNL )       )   IVC = IVC + ICYLTS
C-----ICYRTS   2 VEHICLE CODE - YELLOW RIGHT TURN SIGNALS FRONT AND REAR
      IF (   ITURN(IV) . EQ . ITURNR         )   IVC = IVC + ICYRTS
C-----ICRBRK   4 VEHICLE CODE - RED BRAKE LIGHTS IN REAR
      IF ( ( ACCNEW . LE . DECBRK ) . OR .
     *     ( VELNEW . EQ . 0.0D0  )          )   IVC = IVC + ICRBRK
C-----ICVBMC   8 VEHICLE CODE - VEHICLE BLOCKED BY A MAJOR COLLISION
      IF ( MAJCLB(IV) . OR . MAJCLL(IV)      )   IVC = IVC + ICVBMC
C-----ICVIMC  16 VEHICLE CODE - VEHICLE INVOLVED IN A MAJOR COLLISION
      IF ( MAJCOL(IV)                        )   IVC = IVC + ICVIMC
C-----ICEVRC  32 VEHICLE CODE - EMERGENCY VEHICLE RUNNING A CALL
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   IVC = IVC + ICEVRC
C-----ICVREV  64 VEHICLE CODE - VEHICLE REACTING TO EMERG VEH RUN A CALL
      IF ( RESPEV                            )   IVC = IVC + ICVREV
C-----ICRVMS 128 VEHICLE CODE - VEHICLE REACTING TO VMS MESSAGE
      IF ( ( VMSACM(IV) . GT . 0 ) . OR .
     *     ( VMSASM(IV) . GT . 0 )           )   IVC = IVC + ICRVMS
      IVE = IDNINT( VELNEW )
      IF ( ISET(IV) . EQ . 1 )                   THEN
        LAT = IDNINT( LATPOS(IV)*100.0D0 )
      ELSE
        LAT = 0
      END IF
      IAE = IDNINT( ACCNEW )
C-----WRITE LNEXT IF IPR IS NOT 3 (LOGIBI) AND THE VEHICLE'S NEW
C-----POSITION IS BEYOND THE END OF THE LANE
      WLNEXT = ( ( IPR    . NE . 3                           ) . AND .
     *           ( POSNEW . GT . (DBLE( LGEOM(4,IL) )+2.0D0) ) )
                    IF ( IPR . EQ . 1 )          GO TO 111
      IF ( IPOLL . EQ . IANI )                   THEN
C[      IF ( IPE              .EQ.-2147483647   )STOP 'IBAP   IPE    01'
C[      IF ( IPR              .EQ.-2147483647   )STOP 'IBAP   IPR    02'
C[      IF ( IVE              .EQ.-2147483647   )STOP 'IBAP   IVE    01'
C[      IF ( LAT              .EQ.-2147483647   )STOP 'IBAP   LAT    01'
        IF ( WLNEXT )                            THEN
          WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT,IVEHCL(IV),
     *                    IPRTLO(IV),IAE,LNEXT(IV)
        ELSE
          WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT
        END IF
      ELSE
C[      IF ( IAE              .EQ.-2147483647   )STOP 'IBAP   IAE    01'
C[      IF ( IPE              .EQ.-2147483647   )STOP 'IBAP   IPE    02'
C[      IF ( IPR              .EQ.-2147483647   )STOP 'IBAP   IPR    03'
C[      IF ( IVE              .EQ.-2147483647   )STOP 'IBAP   IVE    02'
C[      IF ( LAT              .EQ.-2147483647   )STOP 'IBAP   LAT    02'
        IF ( WLNEXT )                            THEN
          WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT,IVEHCL(IV),
     *                    IPRTLO(IV),IAE,LNEXT(IV)
        ELSE
          WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT,IVEHCL(IV),
     *                    IPRTLO(IV),IAE
        END IF
      END IF
      GO TO 112
  111 CONTINUE
      IF ( IPOLL . EQ . IANI )                   THEN
C[      IF ( IPE              .EQ.-2147483647   )STOP 'IBAP   IPE    03'
        IF ( WLNEXT )                            THEN
          WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT,IVEHCL(IV),
     *                    IPRTLO(IV),IAE,LNEXT(IV)
        ELSE
          WRITE (NPD,501) IV,IPE,IVC
        END IF
      ELSE
C[      IF ( IAE              .EQ.-2147483647   )STOP 'IBAP   IAE    02'
C[      IF ( IPE              .EQ.-2147483647   )STOP 'IBAP   IPE    04'
C[      IF ( IPR              .EQ.-2147483647   )STOP 'IBAP   IPR    04'
C[      IF ( IVE              .EQ.-2147483647   )STOP 'IBAP   IVE    03'
C[      IF ( LAT              .EQ.-2147483647   )STOP 'IBAP   LAT    03'
        IF ( WLNEXT )                            THEN
          WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT,IVEHCL(IV),
     *                    IPRTLO(IV),IAE,LNEXT(IV)
        ELSE
          WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),LAT,IVEHCL(IV),
     *                    IPRTLO(IV),IAE
        END IF
      END IF
  112 CONTINUE
CY          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 113
CX                  IF ( TIME . LT . TPRINT )    GO TO 113
CX    WRITE (6,704) (IRNAME(IRN),IRN=1,NRNAME)
CX113 CONTINUE
C[    IF ( PVILNI             .EQ.-2147483647.0 )STOP 'IBAP   PVILNI 01'
      IF ( PVILNI . GE . LVILAI(ILN,IA) )        NVILAI(ILN,IA) = 1
                    IF ( IVN . EQ . 1 )          NVILNI(ILN,IA) = 0
      IF ( PVILNI . GE . LVILNI(ILN,IA) )        NVILNI(ILN,IA) =
     *                                           NVILNI(ILN,IA) + 1
C-----WRITE SURROGATE SAFETY ASSESSMENT METHODOLOGY DATA FOR IBAP
      IF ( (ISSAM.EQ.IYES) . AND . (IPR.GT.0) )  THEN
        SSAMXF = VEHCLE(IV)%FBX
        SSAMYF = VEHCLE(IV)%FBY
        SSAMXR = VEHCLE(IV)%UNIT(VEHCLE(IV)%UNITS)%HPX
        SSAMYR = VEHCLE(IV)%UNIT(VEHCLE(IV)%UNITS)%HPY
        IF ( IPR . EQ . 3 )                      THEN
C-----    VEHICLE LOGGED OUT OF APPROACH INTO INTERSECTION
          SSAMLK = 1000+LPRES(IV)
          SSAMLN = 0
C OLD     CALL FNDXYP ( LPRES(IV),POSNEW,SSAMXF,SSAMYF )
C OLD     POSR = POSNEW - LENVAP
C OLD     IF ( POSR . GE . 0.0D0 )               THEN
C OLD       CALL FNDXYP ( LPRES(IV),POSR,SSAMXR,SSAMYR )
C OLD     ELSE
C OLD       POSR = POSR + DBLE( LGEOM(4,IL) )
C OLD       IF ( POSR . LT . 0.0D0 )             POSR = 0.0D0
C OLD       CALL FNDXYA ( IA,ILN,POSR,0.0D0,SSAMXR,SSAMYR )
C OLD     END IF
        ELSE
C-----    VEHICLE ON APPROACH
          SSAMLK = IA
          SSAMLN = ILN
C OLD     IF ( ISET(IV) . EQ . 1 )               THEN
C OLD       POSLAT = LATPOS(IV)
C OLD     ELSE
C OLD       POSLAT = 0.0D0
C OLD     END IF
C OLD     CALL FNDXYA ( IA,ILN,POSNEW,POSLAT,SSAMXF,SSAMYF )
C OLD     POSR = POSNEW - LENVAP
C OLD     IF ( POSR . GE . DBLE( LGEOM(1,IL) ) ) THEN
C OLD       CALL FNDXYA ( IA,ILN,POSR,POSLAT,SSAMXR,SSAMYR )
C OLD     ELSE
C OLD       IF ( LPREV(IV) . EQ . 0 )            THEN
C OLD         POSR = DBLE( LGEOM(1,IL) )
C OLD         CALL FNDXYA ( IA,ILN,POSR,POSLAT,SSAMXR,SSAMYR )
C OLD       ELSE
C OLD         POSR = POSR - DBLE( LGEOM(1,IL)       )
C OLD*                    + DBLE( LENP(LPREV(IV)) )
C OLD         IF ( POSR . GE . 0.0D0 )           THEN
C OLD           CALL FNDXYP ( LPREV(IV),POSR,SSAMXR,SSAMYR )
C OLD         ELSE
C OLD           JL   = LIBL(LPREV(IV))
C OLD           JA   = ISNA(JL)
C OLD           JLN  = ISNL(JL)
C OLD           POSR = POSR + DBLE( LGEOM(4,JL) )
C OLD           IF ( POSR . LT . 0.0D0 )         POSR = 0.0D0
C OLD           CALL FNDXYA ( JA,JLN,POSR,0.0D0,SSAMXR,SSAMYR )
C OLD         END IF
C OLD       END IF
C OLD     END IF
        END IF
C-----  WRITE SSAM VEHICLE RECORD
C-----  SSAMID = RECORD TYPE (3=VEHICLE)
C-----  SSAMVI = VEHICLE ID
C-----  SSAMLK = LINK ID
C-----  SSAMLN = LANE ID (LEFT TO RIGHT STARTING AT 1)
C-----  SSAMVL = LENV(IVEHCL(IV))
C-----  SSAMXF = X COORDINATE OF MIDDLE OF FRONT BUMPER
C-----  SSAMYF = Y COORDINATE OF MIDDLE OF FRONT BUMPER
C-----  SSAMXR = X COORDINATE OF MIDDLE OF REAR  BUMPER
C-----  SSAMYR = Y COORDINATE OF MIDDLE OF REAR  BUMPER
C-----  SSAMVL = VEHICLE LENGTH (FRONT TO BACK)
C-----  SSAMVW = VEHICLE WIDTH  (LEFT TO RIGHT)
C-----  SSAMVS = VEHICLE SPEED (UNITS/SEC)
C-----  SSAMVA = VEHICLE ACCELERATION (UNITS/SEC/SEC)
        SSAMID = 3
        SSAMVI = IQ(IV)
        SSAMVL = LENV(IVEHCL(IV))
        SSAMVW = WIDV(IVEHCL(IV))
        SSAMVS = VELNEW
        SSAMVA = ACCNEW
        WRITE (ISS) SSAMID,SSAMVI,SSAMLK,SSAMLN,
     *              SSAMXF,SSAMYF,SSAMXR,SSAMYR,
     *              SSAMVL,SSAMVW,SSAMVS,SSAMVA
      END IF
      NCQ = NCQ + MAX0( IDNINT( LENVAP/20.0D0 ),1 )
C-----SET THE INDEX FOR THE NEXT VEHICLE ON THE INBOUND LANE TO BE
C-----PROCESSED
C[    IF ( NXVEH              .EQ.-2147483647   )STOP 'IBAP   NXVEH  01'
      IV = NXVEH
C-----END OF VEHICLE LOOP
 4010 CONTINUE
                    IF ( LQ(IAN,ILN) . LE . 0 )  GO TO 5010
 4020 CONTINUE
C*    NRNAME = 1
C-----LOG THE NEW VEHICLE INTO THE INBOUND APPROACH AND LANE AND
C-----INITIALIZE THE VEHICLE ATTRIBUTES
      MELIM = NELIM(IAN)
      CALL  LOGIN
                    IF ( MELIM.NE.NELIM(IAN) )   GO TO 5010
C-----SET VEHICLE HEADING
      HEADNG(IV) = DBLE( IAAZIM(IA) )
      STEERA(IV) = 0.0D0
      IF ( POSNEW . GE . DBLE( LVILAI(ILN,IA) ) )NVILAI(ILN,IA) = 1
      IF ( POSNEW . GE . DBLE( LVILNI(ILN,IA) ) )NVILNI(ILN,IA) =
     *                                           NVILNI(ILN,IA) + 1
      IF ( (IPOLL . EQ . INO   ) . OR .
     *     (TIME  . LT . BEGT20) . OR .
     *     (TIME  . GT . ENDT20) )               GO TO 114
      IPR = 2
      IPE = IDNINT( POSNEW )
      LAT = 0
      IVE = IDNINT( VELNEW )
C-----ICNONE   0 VEHICLE CODE - NONE
      IVC = ICNONE
C-----ICYLTS   1 VEHICLE CODE - YELLOW LEFT TURN SIGNALS FRONT AND REAR
      IF ( ( ITURN(IV) . EQ . ITURNU ) . OR .
     *     ( ITURN(IV) . EQ . ITURNL ) )         IVC = IVC + ICYLTS
C-----ICYRTS   2 VEHICLE CODE - YELLOW RIGHT TURN SIGNALS FRONT AND REAR
      IF (   ITURN(IV) . EQ . ITURNR   )         IVC = IVC + ICYRTS
C-----ICRBRK   4 VEHICLE CODE - RED BRAKE LIGHTS IN REAR
      IF ( ( ACCNEW . LE . DECBRK ) . OR .
     *     ( VELNEW . EQ . 0.0D0  ) )            IVC = IVC + ICRBRK
C-----ICVBMC   8 VEHICLE CODE - VEHICLE BLOCKED BY A MAJOR COLLISION
      IF ( MAJCLB(IV) . OR . MAJCLL(IV) )        IVC = IVC + ICVBMC
C-----ICVIMC  16 VEHICLE CODE - VEHICLE INVOLVED IN A MAJOR COLLISION
      IF ( MAJCOL(IV)                   )        IVC = IVC + ICVIMC
C-----ICEVRC  32 VEHICLE CODE - EMERGENCY VEHICLE RUNNING A CALL
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   IVC = IVC + ICEVRC
C-----ICVREV  64 VEHICLE CODE - VEHICLE REACTING TO EMERG VEH RUN A CALL
      IF ( .FALSE.                      )        IVC = IVC + ICVREV
C-----ICRVMS 128 VEHICLE CODE - VEHICLE REACTING TO VMS MESSAGE
      IF ( ( VMSACM(IV) . GT . 0 ) . OR .
     *     ( VMSASM(IV) . GT . 0 ) )             IVC = IVC + ICRVMS
      IAE = IDNINT( ACCNEW )
      IF ( IPOLL . EQ . IANI )                   THEN
        WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),IQ(IV),IVEHCL(IV),
     *                  IPRTLO(IV)
      ELSE
        WRITE (NPD,501) IV,IPE,IVC,IVE,IPR,LPRES(IV),IQ(IV),IVEHCL(IV),
     *                  IPRTLO(IV),IAE
      END IF
  114 CONTINUE
C-----WRITE SURROGATE SAFETY ASSESSMENT METHODOLOGY DATA FOR LOGIN
      IF ( ISSAM . EQ . IYES )                   THEN
        SSAMXF = VEHCLE(IV)%FBX
        SSAMYF = VEHCLE(IV)%FBY
        SSAMXR = VEHCLE(IV)%UNIT(VEHCLE(IV)%UNITS)%HPX
        SSAMYR = VEHCLE(IV)%UNIT(VEHCLE(IV)%UNITS)%HPY
C OLD   IF ( ISET(IV) . EQ . 1 )                 THEN
C OLD     POSLAT = LATPOS(IV)
C OLD   ELSE
C OLD     POSLAT = 0.0D0
C OLD   END IF
C OLD   CALL FNDXYA ( IA,ILN,POSNEW,POSLAT,SSAMXF,SSAMYF )
C OLD   POSR = POSNEW - LENVAP
C OLD   IF ( POSR . GE . DBLE( LGEOM(1,IL)) )    THEN
C OLD     CALL FNDXYA ( IA,ILN,POSR,POSLAT,SSAMXR,SSAMYR )
C OLD   ELSE
C OLD     POSR = DBLE( LGEOM(1,IL) )
C OLD     CALL FNDXYA ( IA,ILN,POSR,POSLAT,SSAMXR,SSAMYR )
C OLD   END IF
C-----  WRITE SSAM VEHICLE RECORD
C-----  SSAMID = RECORD TYPE (3=VEHICLE)
C-----  SSAMVI = VEHICLE ID
C-----  SSAMLK = LINK ID
C-----  SSAMLN = LANE ID (LEFT TO RIGHT STARTING AT 1)
C-----  SSAMVL = LENV(IVEHCL(IV))
C-----  SSAMXF = X COORDINATE OF MIDDLE OF FRONT BUMPER
C-----  SSAMYF = Y COORDINATE OF MIDDLE OF FRONT BUMPER
C-----  SSAMXR = X COORDINATE OF MIDDLE OF REAR  BUMPER
C-----  SSAMYR = Y COORDINATE OF MIDDLE OF REAR  BUMPER
C-----  SSAMVL = VEHICLE LENGTH (FRONT TO BACK)
C-----  SSAMVW = VEHICLE WIDTH  (LEFT TO RIGHT)
C-----  SSAMVS = VEHICLE SPEED (UNITS/SEC)
C-----  SSAMVA = VEHICLE ACCELERATION (UNITS/SEC/SEC)
        SSAMID = 3
        SSAMVI = IQ(IV)
        SSAMLK = IA
        SSAMLN = ILN
        SSAMVL = LENV(IVEHCL(IV))
        SSAMVW = WIDV(IVEHCL(IV))
        SSAMVS = VELNEW
        SSAMVA = ACCNEW
        WRITE (ISS) SSAMID,SSAMVI,SSAMLK,SSAMLN,
     *              SSAMXF,SSAMYF,SSAMXR,SSAMYR,
     *              SSAMVL,SSAMVW,SSAMVS,SSAMVA
      END IF
CY          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 115
CX                  IF ( TIME . LT . TPRINT )    GO TO 115
CX    WRITE (6,704) (IRNAME(IRN),IRN=1,NRNAME)
CX115 CONTINUE
C-----END OF INBOUND LANE LOOP
 5010 CONTINUE
C-----END OF INBOUND APPROACH LOOP
 6010 CONTINUE
      RETURN
      END                                                               IBAP
C
C
C
      SUBROUTINE CHKDSP
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      DOUBLE PRECISION  A,B,C,SLOPE,SPD,T,TMAX,XCRIT
C
C-----SUBROUTINE CHKDSP CHECKS TO SEE IF THE VEHICLE SHOULD RESET HIS
C-----DESIRED SPEED TO THE DESIRED SPEED OF HIS INTERSECTION PATH SO
C-----THAT HE CAN GRADUALLY DECELERATE TO HIS NEW DESIRED SPEED BEFORE
C-----HE ENTERS THE INTERSECTION
C
C[    SLOPE      = -2147483647.0
C[    SPD        = -2147483647.0
C[    T          = -2147483647.0
C[    XCRIT      = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CHKDSP'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----FIND THE DESIRED SPEED FOR THE INTERSECTION PATH
      SPD = DBLE( ISPD(IV)*LIMP(LNEXT(IV)) ) / DBLE( ISLIM(IA) )
C-----IF THE DISTANCE TO THE END OF THE LANE IS LE 25 FEET THEN GO TO
C-----1010 AND SET THE DESIRED SPEED FOR THE INTERSECTION PATH
                    IF ( RELEND . LE . 25.0D0 )  GO TO 1010
C-----IF THE PRESENT VELOCITY OF THE VEHICLE IS LESS THAN OR EQUAL TO
C-----THE DESIRED SPEED FOR THE INTERSECTION PATH THEN RETURN
                    IF ( VELOLD . LE . SPD )     RETURN
C-----FIND THE DISTANCE REQUIRED TO REDUCE THE PRESENT VELOCITY OF THE
C-----VEHICLE TO THE DESIRED SPEED OF THE INTERSECTION PATH USING SLOPE
      SLOPE = -0.375D0*SLPMAX*DCHAR(IDRICL(IV))
C-----FIND THE TIME REQUIRED TO REDUCE THE PRESENT VELOCITY OF THE
C-----VEHICLE TO THE DESIRED SPEED OF THE INTERSECTION PATH USING SLOPE
C-----SPD = VELOLD + ACCOLD*T + 0.5*SLOPE*T**2
C-----(0.5*SLOPE)*T**2 + (ACCOLD)*T + (VELOLD-SPD) = 0
      A     = 0.5D0*SLOPE
      B     = ACCOLD
      C     = VELOLD - SPD
      TMAX  = 30.0D0
      CALL  TMQUAD  ( A,B,C,TMAX,T )
      IF ( T . EQ . TIMERR )                     THEN
        IF ( DABS( C ) . LE . VSMALL )           THEN
          T = 0.0D0
        ELSE
          RETURN
        END IF
      END IF
      T = DMAX1( T,0.001D0 )
      XCRIT = VELOLD*T + 0.5D0*ACCOLD*T**2 + ONED6*SLOPE*T**3
C-----IF THE DISTANCE TO THE END OF THE LANE IS GT THE DISTANCE REQUIRED
C-----TO REDUCE THE PRESENT VELOCITY OF THE VEHICLE TO THE DESIRED SPEED
C-----OF THE INTERSECTION PATH THEN RETURN
                    IF ( RELEND . GT . XCRIT )   RETURN
 1010 CONTINUE
C-----SET THE VEHICLES DESIRED SPEED TO THE DESIRED SPEED FOR THE
C-----INTERSECTION PATH AND SET THE FLAG TO INDICATE THAT THE VEHICLES
C-----DESIRED SPEED HAS BEEN RESET
C[    IF ( SPD                .EQ.-2147483647.0 )STOP 'CHKDSP SPD    01'
      ISPD(IV) = IDNINT( SPD )
      CALL  SETDSP  ( IV,POSNEW,DBLE( ISPD(IV) ),.FALSE.,DESVEL )
      ISPDP(IV) = 1
      RETURN
      END                                                               CHKDSP
C
C
C
      SUBROUTINE SETDSP ( JV,POSNFB,DESPDI,USEISP,DESPDR )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           USEISP
      INTEGER           JV
      DOUBLE PRECISION  DESPDI,DESPDR,DESSPD,POSNFB
C
C-----SUBROUTINE SETDSP SETS THE DESIRED SPEED FOR VEHICLE JV TO DESPDI,
C-----MODIFIES IT IF THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----COLLISION MAY BLOCK THIS VEHICLE (IF USEISP TRUE THEN USES
C-----ISPD(JV) ELSE USES DESPDI), MODIFIES IT IF THE VEHICLE HAS A VMS
C-----SPEED MESSAGE ACTIVE, AND FINALLY RETURNS DESPDR
C
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'SETDSP'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----SET THE DESIRED SPEED FOR VEHICLE JV TO DESPDI
      DESPDR = DESPDI
C-----IF THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR COLLISION MAY
C-----BLOCK THIS VEHICLE THEN SET COLLISION DESIRED SPEED
      IF ( (.NOT.MAJCLB(JV)) .AND. MAJCON(JV) )  THEN
        IF ( (POSCON(JV)-POSNFB)        . LE .
     *       (DISCON/DCHAR(IDRICL(JV))) )        THEN
          IF ( USEISP )                          THEN
            DESSPD = DBLE( ISPD(JV) )
          ELSE
            DESSPD = DESPDI
          END IF
          DESSPD = DMIN1( PDSCOL*DCHAR(IDRICL(JV)),1.0D0 )*DESSPD
          DESPDR = DMIN1( DESPDR,DESSPD )
        END IF
      END IF
C-----PROCESS VEHICLE MESSAGE SYSTEM SPEED MESSAGE
      IF ( VMSASM(JV) . GT . 0 )                 THEN
        IF ( ( IVMSMG(VMSASM(JV)) .EQ. VMSMAM ) . OR .
     *       ( IVMSMG(VMSASM(JV)) .EQ. VMSMAN ) )THEN
C-----    SET VEHICLE MESSAGE SYSTEM DESIRED SPEED
          DESPDR = DVMSMP(VMSASM(JV))
        END IF
      END IF
      RETURN
      END                                                               SETDSP
C
C
C
      SUBROUTINE MAXDSP  ( JV,BRAKE,DVJV,PNJV,VOJV,AOJV,RPJV,
     *                     KV,VKV,AKV,SKV                     )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INTEGER           JV,KV
      DOUBLE PRECISION  AKV,AOJV,BRAKE,DESSPD,DS,DVJV,PNJV,RELTS,RPJV,
     *                  SKV,TMAX,TS,TSKV,VELTS,VKV,VOJV
C
C-----SUBROUTINE MAXDSP CALCULATES THE MAXIMUM DESIRED SPEED WHERE THE
C-----VEHICLE CAN DECEL TO A STOP IN THE SPECIFIED DISTANCE AND NOT
C-----EXCEED THE MAXIMUM DECELERATION RATE FOR THE DRIVER
C
C-----JV     = VEHICLE NUMBER
C-----BRAKE  = BRAKING VALUE (NEGATIVE) (-6.0 TO -8.0)
C-----DVJV   = DESVEL FOR JV
C-----PNJV   = POSNEW FOR JV
C-----VOJV   = VELOLD FOR JV
C-----AOJV   = ACCOLD FOR JV
C-----RPJV   = RELPOS FOR JV
C-----KV     = IVPV   FOR JV
C-----VKV    = PVVEL  FOR JV
C-----AKV    = PVACC  FOR JV
C-----SKV    = PVSLP  FOR JV
C
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'MAXDSP'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IF ( VOJV . EQ . 0.0D0 )                   THEN
        IF ( DVJV . EQ . 0.0D0 )                 THEN
          GO TO 1010
        END IF
        VELTS = DVJV
      ELSE
        VELTS = VOJV
      END IF
C-----CALCULATE THE TIME AND DISTANCE TO STOP NOT EXCEEDING THE MAXIMUM
C-----DECELERATION RATE FOR THE DRIVER
      CALL  TDSTPM  ( JV,BRAKE,DVJV,VOJV,AOJV,.TRUE.,TS,DS )
      IF ( (TS.EQ.TIMERR) . OR . (DS.LE.0.0D0) ) THEN
        GO TO 1010
      END IF
      RELTS = RPJV
      IF ( KV . GT . 0 )                         THEN
        TMAX = 30.0D0
        CALL  TIMSTP  ( VKV,AKV,SKV,TMAX,TSKV )
        IF ( TSKV . EQ . TIMERR )                THEN
          TSKV = TS
        ELSE
          TSKV = DMIN1( TS,TSKV )
        END IF
        RELTS = RELTS + VKV*TSKV + 0.5D0*AKV*TSKV**2 + ONED6*SKV*TSKV**3
      END IF
      IF ( RELTS . LE . 0.0D0 )                  THEN
        IF ( ( MAJRLC(JV)                 ) .AND.
     *       ( RELTS . GE . -0.5D0*XRELMI ) )    THEN
          DVJV = DBLE( ISPD(JV) )
          DVJV = DMIN1( PDSCOL*DCHAR(IDRICL(JV)),1.0D0 )*DVJV
        ELSE
          DVJV = 0.0D0
        END IF
      ELSE
        DESSPD = VELTS*DSQRT( RELTS )/DSQRT( DS )
        DVJV = DMIN1( DMAX1( DESSPD,0.0D0 ),DVJV )
      END IF
 1010 CONTINUE
      CALL  SETDSP  ( JV,PNJV,DVJV,.TRUE.,DVJV )
      RETURN
      END                                                               MAXDSP
C
C
C
      SUBROUTINE LOGIBI
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
C6    INCLUDE 'PRTPVA'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
C8    INCLUDE 'TAPE10'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      LOGICAL           EVRESP,LBVSTP,LCHKCF
      INTEGER           I,J,JP,KV
      DOUBLE PRECISION  POSMJC,POSNFB
C8    DOUBLE PRECISION  DTIME8,POSTO8
C+    DOUBLE PRECISION  DTIMEP,POSTOP
C8601 FORMAT(4I2,I6,F7.1,2I4,2I2,F7.1)
C+602 FORMAT(2I2,I6,I2,3F7.2)
C
C-----SUBROUTINE LOGIBI LOGS THE VEHICLE OUT OF THE INBOUND APPROACH AND
C-----LANE AND INTO THE LINKING INTERSECTION PATH FOR THE VEHICLE
C
C[    I          = -2147483647
C[    J          = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'LOGIBI'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      JP = LPREV(IV)
      IF ( JP . GT . 0 )                         THEN
        IF ( ISTCON(IV) . LE . NGEOCP(JP) )      THEN
          POSNFB = POSNEW - DBLE( LGEOM(1,IL) ) + DBLE( LENP(JP) )
C-----    CLEAR INTERSECTION CONFLICTS ON PREVIOUS INTERSECTION PATH
          CALL  CLRCON ( JP,POSNFB,.TRUE. )
        END IF
      END IF
      IP = LNEXT(IV)
C-----REMOVE THE VEHICLE FROM THE LIST OF VEHICLES AT THE INTERSECTION
      J = 0
      DO 1010  I = 1 , NVATIN
                    IF ( LVATIN(I) . EQ . IV )   J = J + 1
                    IF ( J . EQ . 0 )            GO TO 1010
      LVATIN(I) = LVATIN(I+J)
      TVATIN(I) = TVATIN(I+J)
 1010 CONTINUE
C[    IF ( J                  .EQ.-2147483647   )STOP 'LOGIBI J      01'
      NVATIN = NVATIN - J
                    IF ( IP . EQ . 0 )           GO TO 2010
                    IF ( ISET(IV) . NE . 1 )     GO TO 2020
 2010 CONTINUE
C-----END THE LANE CHANGE AND RESET THE LANE CHANGE FLAGS
      CALL  ENDLCH
C-----FORCE AN INTERSECTION PATH TO BE FOUND FOR THIS VEHICLE BASED ON
C-----THE CURRENT APPROACH, CURRENT LANE, AND THE DESIRED OUTBOUND
C-----APPROACH
      CALL  PATHF   ( .TRUE.,0,'LOGIBI' )
      IP = LNEXT(IV)
 2020 CONTINUE
C-----SET CONFLICTS FOR THE VEHICLE FOR HIS INTERSECTION PATH
      CALL  SETCON
C-----SET THE VEHICLES NOFT TO THE LAST VEHICLE ON THE LINKING
C-----INTERSECTION PATH
      NOF(IV) = ILVP(IP)
C-----SET THIS VEHICLE AS THE NEW LAST VEHICLE ON THE LINKING
C-----INTERSECTION PATH
      ILVP(IP) = IV
C-----DECREMENT THE NUMBER OF VEHICLES ON THE INBOUND APPROACH AND LANE
      NVIA(IA) = NVIA(IA) - 1
      NVIBA = NVIBA - 1
      NVIL(ILN,IA) = NVIL(ILN,IA) - 1
C-----INCREMENT THE NUMBER OF VEHICLES ON THE INTERSECTION PATH
      NVIN = NVIN + 1
      NVIP(IP) = NVIP(IP) + 1
      MFINL(IV) = .FALSE.
                    IF ( IFVP(IP) . GT . 0 )     GO TO 3020
C-----SET THE VEHICLE AS THE NEW FIRST VEHICLE ON THE INTERSECTION PATH
      IFVP(IP) = IV
      MFINL(IV) = .TRUE.
 3020 CONTINUE
C-----UPDATE THE LINK INDICES
      LPREV(IV) = LPRES(IV)
      LPRES(IV) = IP
      LNEXT(IV) = LOBL(IP)
C-----IF THE VEHICLE IS INVOLVED IN A MAJOR COLLISION THEN SET THAT THE
C-----NEW INTERSECTION PATH HAS A MAJOR COLLISION
      IF ( MAJCOL(IV) )                          THEN
        PMJCOL(IP) = .TRUE.
      END IF
C-----SET THE FIRST VEHICLE IN THE INBOUND LANE AS THE NORT OF THIS
C-----VEHICLE
      IFVL(LPREV(IV)) = NOR(IV)
                    IF ( NOR(IV) . GT . 0 )      GO TO 3030
C-----SET THE LAST VEHICLE IN THE INBOUND LANE = 0 (OLD NORT EQ 0)
      ILVL(LPREV(IV)) = 0
      GO TO 3040
 3030 CONTINUE
C-----SET MFINL AND MOASF TO TRUE, RESET IACC TO SLIGHTLY DECELERATING
C-----IF MSFLG EQ TRUE AND THE VEHICLE IS NOT DECELERATING, SET MSFLG
C-----TO FALSE, AND FINALLY STORE 0 FOR NOFT OF THE NORT VEHICLE
C-----(OLD NORT GT 0)
      CALL  FLGNOR ( .TRUE.,0 )
C-----WAKE THE NORT VEHICLE UP FOR INTERSECTION CONTROL LOGIC
            IF ( LOGFLG(NOR(IV)) . LE . 2 )      GO TO 3040
      LOGFLG(NOR(IV)) = 2
 3040 CONTINUE
C-----SET THIS VEHICLES NORT = 0
      NOR(IV) = 0
      MOASF(IV) = .TRUE.
      ISTCON(IV) = 1
                    IF ( ISPDP(IV) . EQ . 1 )    GO TO 3050
C-----THE VEHICLE HAS NOT PREVIOUSLY RESET HIS DESIRED SPEED THUS SET
C-----THE DESIRED SPEED FOR THE INTERSECTION PATH
      ISPD(IV) = IDNINT( DBLE( ISPD(IV)*LIMP(LPRES(IV)) ) / 
     *                   DBLE(          ISLIM(IA)       ) )
 3050 CONTINUE
C-----IF THIS VEHICLE IS A YELLOW GO FROM A STOPPED POSITION AT THE STOP
C-----LINE AND HAS CLEARED HIS INTERSECTION CONFLICTS THEN DOUBLE THE
C-----DESIRED SPEED FOR THE VEHICLE ON THE INTERSECTION PATH
                    IF ( IDISPD(IV) )            ISPD(IV) = 2*ISPD(IV)
C-----RESET SOME OF THE VEHICLES ATTRIBUTES
C8    POSTO8 = POSNEW - POSOLD
CP    POSTOP = POSNEW - POSOLD
      POSNEW = POSNEW - DBLE( LGEOM(4,IL) )
C8                  IF ( POSTO8 . LE . 0.0D0 )   GO TO 101
C8    DTIME8 = TIME - DT*DMIN1( DMAX1( 0.0D0,POSNEW/POSTO8 ),1.0D0 )
C8    GO TO 102
C8101 CONTINUE
C8    DTIME8 = TIME + IPRTM(IV)*DT
C8102 CONTINUE
C8    WRITE (IQD,601) IA,ILN,ITURN(IV),IVEHCL(IV),IQ(IV),DTIME8,
C8   *                IQDS(IV),ISDS(IV),IPRTM(IV),NQUEUE(IV),TQUEUE(IV)
C8    NQUEUE(IV) = 0
C8    TQUEUE(IV) = 0.0D0
C+                  IF ( POSTOP . LE . 0.0D0 )   GO TO 103
C+    DTIMEP = TIME - DT*DMIN1( DMAX1( 0.0D0,POSNEW/POSTOP ),1.0D0 )
C+    GO TO 104
C+103 CONTINUE
C+    DTIMEP = TIME + IPRTM(IV)*DT
C+104 CONTINUE
C+    WRITE (IDH,602) IA,ILN,IQ(IV),ITURN(IV),TIME,DTIMEP,DTIMEP-TIME
      LATPOS(IV) = 0.0D0
      MATSTL(IV) = .FALSE.
      MININT(IV) = .TRUE.
      MBLOCK(IV) = .FALSE.
      MPRO  (IV) = .TRUE.
      MSFLG (IV) = .FALSE.
      MTCARS(IV) = .FALSE.
      LOGTMP = 0
C6    DISTAD(IV) = DBLE( LGEOM(4,IL) )
                    IF ( NOF(IV) . EQ . 0 )      GO TO 3060
C-----SET THIS VEHICLE AS THE NORT OF THE LAST VEHICLE ON THE
C-----INTERSECTION PATH
      NOR(NOF(IV)) = IV
C-----CHECK IF THE VEHICLE AHEAD IS STOPPED
      MOASF(IV) = .FALSE.
            IF ( IVEL(NOF(IV)) . LE . 0.0D0 )    MOASF(IV) = .TRUE.
 3060 CONTINUE
      IF ( SMJCOL . AND . (.NOT. MAJCOL(IV)) )   THEN
        MAJCLB(IV) = .FALSE.
        MAJCLL(IV) = .FALSE.
        MAJCON(IV) = .FALSE.
        POSCLB(IV) = POSBIG
        POSCLL(IV) = POSBIG
        POSCON(IV) = POSBIG
        CALL  CPMJCL ( IP,KV,LCHKCF,LBVSTP,POSMJC )
        IF ( KV . GT . 0 )                       THEN
C-----    THERE IS A COLLISION VEHICLE ON THE INTERSECTION PATH BEFORE
C-----    THIS VEHICLE OR DOWNSTREAM THUS SET THAT THE VEHICLE IS
C-----    BLOCKED BY A MAJOR COLLISION
          MAJCLB(IV) = .TRUE.
          POSCLB(IV) = DMIN1( POSCLB(IV),POSMJC )
C-----    IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----    VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----    FALSE
          EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *               ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
          RESPEV = ( RESPEV . OR . EVRESP )
        END IF
C-----  SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----  COLLISION MAY BLOCK THIS VEHICLE
        IF ( LCHKCF )                            THEN
          MAJCON(IV) = .TRUE.
          POSCON(IV) = DMIN1( POSCON(IV),POSMJC )
        END IF
      END IF
      RETURN
      END                                                               LOGIBI
C
C
C
      SUBROUTINE PREST1 ( CLOGIN,LVMSDD )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
C3    INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      LOGICAL           CLOGIN,DIRPTH,DIRVAL,EVRESP,IBLOCK,LBLOCK,
     *                  LBVSTP,LCCLN,LCHKCF,LVMSDD,MAJCBL,MAJCLP
      INTEGER           I,ILL,ILR,INT2PB,J,JA,JL,JP,JV,K,KP,KV,LANSI,
     *                  LOK,MNEXT,MNT2P,MOBAPD,MOBL,NOFIVC,NOFIVN,
     *                  NORJVC,NP
      DOUBLE PRECISION  ACCJV,ACCNOF,CLEARD,CLEART,ENDLP,HWH,HWM,NORVAL,
     *                  PAIVPV,POSADD,POSCBL,POSJV,POSMJC,POSNOF,POSNRB,
     *                  POSREL,SAFR,SAFVEL,SLPJV,SLPNOG,VELJV,VELNOF
  702 FORMAT('VEHICLE ',I6,' ON APPROACH ',I2,' AT TIME = ',F7.1,
     *       ' WAS FORCED TO USE DESIRED OUTBOUND APPROACH ',I2,
     *       ' INSTEAD OF APPROACH ',I2,' (',A,')')
C
C-----SUBROUTINE PREST1 RESETS THE PREVIOUS VEHICLE PARAMETERS TO
C-----THE NEW NOFT IF THE VEHICLE IS LANE CHANGING, AND
C-----INITIALIZES SEVERAL PARAMETERS FOR THE VEHICLE
C
C-----LVMSDD = DISTRACTED DRIVER VMS MESSAGE WAS CANCELLED OR TIMED OUT
C
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'PREST1'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C3    IPFLAG = IBLNK1
C3    JPFLAG = IBLNK1
C3    KPFLAG = IBLNK1
      STEERA(IV) = 0.0D0
      DIRPTH = .FALSE.
      DIRVAL = .FALSE.
      MAJCBL = .FALSE.
      POSCBL = 0.0D0
      MAJCBP = .FALSE.
      MAJCLC = .FALSE.
      NEWSSG = .FALSE.
      RESPEV = .FALSE.
      EVRESP = .FALSE.
      HWM    = 0.5D0*WIDV(IVEHCL(IV))
      MAJCLP = ( MAJCLB(IV) . OR . MAJCLL(IV) )
      LVMSDD = .FALSE.
C-----IF PREST1 NOT CALLED BY SUBROUTINE LOGIN THEN GET THE
C-----POS/VEL/ACC/SLP FOR THE CURRENT VEHICLE
      IF ( .NOT. CLOGIN )                        THEN
        CALL  SPVAS  ( IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                 .FALSE.,.FALSE.,.FALSE.,.FALSE. )
        CALL  SPVAS  ( IV,POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                 .FALSE.,.FALSE.,.FALSE.,.TRUE.  )
        SLPNEW = SLPOLD
      END IF
      LENVAP = LVAP(IV)
      POSNRB = POSNEW - LENVAP
C-----SET IVPV TO THE MOST CRITICAL OF THE NOF, IVCNOF, OR JVCNOR
C-----VEHICLES
      NOFIVN = NOF   (IV)
      NOFIVC = IVCNOF(IV)
      NORJVC = JVCNOR(IV)
      IF ( MININT(IV) )                          THEN
        ENDLP = DBLE( LENP(IP)    ) + 1.5D0
      ELSE
        ENDLP = DBLE( LGEOM(4,IL) ) + 1.5D0
      END IF
      POSADD = 0.0D0
      IF ( NOFIVN . EQ . 0 )                     THEN
        POSNOF = POSBIG
      ELSE
        CALL  SPVAS  ( NOFIVN,POSNOF,VELNOF,ACCNOF,SLPNOG,
     *                 .TRUE.,.TRUE.,.FALSE.,.TRUE.        )
      END IF
C-----SET IVPV TO THE NOF VEHICLE
      IVPV   = NOFIVN
      DIRPTH = .TRUE.
      PAIVPV = 0.0D0
C-----IF THERE IS AN IVCNOF VEHICLE AND IT IS IN THE INTERSECTION THEN
C-----CANCEL THE IVCNOF CONNECTION
      IF ( NOFIVC . GT . 0 )                     THEN
        IF ( MININT(NOFIVC) )                    THEN
          JVCNOF(NOFIVC) = 0
          IVCNOF(IV    ) = 0
          NOFIVC         = 0
        ELSE
          IF ( ISET(IV) . EQ . 1 )               THEN
            IF ( LATPOS(IV) . GT . 0.0D0 )       THEN
              CALL  CLCCLN ( IV,NLR(IL),LCCLN )
            ELSE
              CALL  CLCCLN ( IV,NLL(IL),LCCLN )
            END IF
            IF ( LCCLN )                         THEN
              JVCNOF(NOFIVC) = 0
              IVCNOF(IV    ) = 0
              NOFIVC         = 0
            END IF
          END IF
        END IF
      END IF
C-----IF THERE IS AN JVCNOR VEHICLE AND IT IS IN THE INTERSECTION THEN
C-----CANCEL THE JVCNOR CONNECTION
      IF ( NORJVC . GT . 0 )                     THEN
        IF ( MININT(NORJVC) )                    THEN
          IVCNOR(NORJVC) = 0
          JVCNOR(IV    ) = 0
          NORJVC         = 0
        ELSE
          IF ( ISET(NORJVC) . EQ . 1 )           THEN
            IF ( LATPOS(NORJVC) . GT . 0.0D0 )   THEN
              CALL  CLCCLN ( NORJVC,NLR(LPRES(NORJVC)),LCCLN )
            ELSE
              CALL  CLCCLN ( NORJVC,NLL(LPRES(NORJVC)),LCCLN )
            END IF
            IF ( LCCLN )                         THEN
              IVCNOR(NORJVC) = 0
              JVCNOR(IV    ) = 0
              NORJVC         = 0
            END IF
          END IF
        END IF
      END IF
      IF ( MININT(IV) )                          THEN
C
C-----  THIS VEHICLE IS IN THE INTERSECTION
C
C-----  IF THERE IS AN IVCNOF VEHICLE THEN CANCEL THE IVCNOF CONNECTION
        IF ( NOFIVC . GT . 0 )                   THEN
          JVCNOF(NOFIVC) = 0
          IVCNOF(IV    ) = 0
          NOFIVC         = 0
        END IF
C-----  IF THERE IS A JVCNOR VEHICLE THEN CANCEL THE JVCNOR CONNECTION
        IF ( NORJVC . GT . 0 )                   THEN
          IVCNOR(NORJVC) = 0
          JVCNOR(IV    ) = 0
          NORJVC         = 0
        END IF
        IF ( NOFIVN . EQ . 0 )                   THEN
C-----    THERE IS NO NOF VEHICLE AND THIS VEHICLE IS IN THE
C-----    INTERSECTION THUS COMPARE THE IVPV VEHICLE AND (1) THE LAST
C-----    VEHICLE ON ALL INTERSECTIONS PATHS FROM THIS PATHS LINKING
C-----    INBOUND LANE WHOSE REAR BUMPER IS STILL ON THIS PATHS LINKING
C-----    INBOUND LANE OR THAT HAS NOT TRAVELED FAR ENOUGH TO CLEAR THIS
C-----    VEHICLE OR (2) THE LAST VEHICLE ON THE LINKING OUTBOUND LANE
C-----    FOR THIS INTERSECTION PATH WHOSE REAR BUMPER IS STILL ON THIS
C-----    PATHS LINKING INBOUND LANE OR THAT HAS NOT TRAVELED FAR ENOUGH
C-----    TO CLEAR THIS VEHICLE (POSSIBLY A LONG VEHICLE)
          JL = LIBL(IP)
          DIRVAL = .TRUE.
          DO  I = 1 , NPINT(JL)
            JP = LINTP(I,JL)
            IF ( JP . EQ . IP )                  THEN
              JV = 0
            ELSE
              JV = ILVP(JP)
            END IF
            IF ( JV . EQ . 0 )                   THEN
              MOBL = LOBL(JP)
              IF ( MOBL . EQ . 0 )               CYCLE
              JV = ILVL(MOBL)
              IF ( JV . EQ . 0 )                 CYCLE
              IF ( ( JP . NE . IP        ) . AND .
     *             ( JP . NE . LPREV(JV) ) )     CYCLE
              CALL  SPVAS   ( JV,POSJV,VELJV,ACCJV,SLPJV,
     *                        .TRUE.,.FALSE.,.FALSE.,.TRUE. )
              IF ( ( JP    . NE . IP       ) . AND .
     *             ( POSJV . GE .
     *               DBLE( LGEOM(1,MOBL) ) ) )   CYCLE
              POSADD = DBLE( LENP(JP)-LGEOM(1,MOBL) )
            ELSE
              CALL  SPVAS   ( JV,POSJV,VELJV,ACCJV,SLPJV,
     *                        .TRUE.,.FALSE.,.FALSE.,.TRUE. )
              POSADD = 0.0D0
            END IF
            POSJV = POSJV + POSADD
            IF ( POSJV . LT . POSNRB )           CYCLE
            IF ( ( POSJV . LE . 0.0D0  ) . OR .
     *           ( JP    . EQ . IP     ) )       THEN
              CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                        POSNOF,.FALSE.,JV,POSADD,
     *                        DIRVAL,DIRPTH,PAIVPV         )
              CYCLE
            END IF
            HWH    = 0.5D0*WIDV(IVEHCL(JV))
            SAFVEL = DMAX1( IVEL(IV),DBLE( ISPD(IV) ),
     *                      IVEL(JV),DBLE( ISPD(JV) ) )
            SAFR   = (SAFDIS+(SAFVEL/SAFSPD))/DCHAR(IDRICL(IV))
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE JV
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----      FALSE
            EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *                 ( IAND( VEHTYP(JV),LAVTE ) . NE . 0 ) )
            IF ( EVRESP )                        THEN
              SAFR = DMAX1( SAFR,EVEHRZ*SAFVEL )
            END IF
            CLEARD = HWM + SAFR + HWH
            CALL  CCLEAR  ( IP,JP,CLEARD,.TRUE.,CLEART )
            IF ( POSJV . LE . CLEART )           THEN
              CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                        POSNOF,.FALSE.,JV,POSADD,
     *                        DIRVAL,DIRPTH,PAIVPV         )
              CYCLE
            END IF
C-----    END OF LOOP FOR INTERSECTION PATHS ORIGINATING FROM THIS
C-----    INTERSECTION PATHS LINKING INBOUND LANE
          END DO
C-----    COMPARE THE IVPV VEHICLE AND THE LAST VEHICLE ON THE LINKING
C-----    OUTBOUND LANE FOR THE INTERSECTION PATH
          MOBL = LOBL(IP)
          IF ( MOBL . GT . 0 )                   THEN
            JV = ILVL(MOBL)
            IF ( JV . GT . 0 )                   THEN
              POSADD = DBLE( LENP(IP) )
              CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                        POSNOF,.FALSE.,JV,POSADD,
     *                        DIRVAL,DIRPTH,PAIVPV         )
            END IF
C-----      COMPARE THE IVPV VEHICLE AND THE ALL VEHICLES ON THE LANE TO
C-----      THE LEFT AND THE LANE TO THE RIGHT OF THE LINKING OUTBOUND
C-----      LANE FOR THE INTERSECTION PATH WHICH IS LANE CHANGING OUT OF
C-----      THE LINKING OUTBOUND LANE FOR THE INTERSECTION PATH
            ILL = NLL(MOBL)
            IF ( ILL . GT . 0 )                  THEN
              JV = IFVL(ILL)
              DO WHILE ( JV . GT . 0 )
                IF ( ( ISET  (JV) .EQ. 1     ) . AND .
     *               ( LATPOS(JV) .GT. 0.0D0 ) ) THEN
                  CALL  CLCCLN ( JV,ILL,LCCLN )
                  IF ( .NOT. LCCLN )             THEN
                    POSADD = DBLE( LENP(IP) )
                    CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                              POSNOF,.TRUE.,JV,POSADD,
     *                              DIRVAL,DIRPTH,PAIVPV         )
                  END IF
                END IF
                JV = NOR(JV)
              END DO
            END IF
            ILR = NLR(MOBL)
            IF ( ILR . GT . 0 )                  THEN
              JV = IFVL(ILR)
              DO WHILE ( JV . GT . 0 )
                IF ( ( ISET  (JV) .EQ. 1     ) . AND .
     *               ( LATPOS(JV) .LT. 0.0D0 ) ) THEN
                  CALL  CLCCLN ( JV,ILR,LCCLN )
                  IF ( .NOT. LCCLN )             THEN
                    POSADD = DBLE( LENP(IP) )
                    CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                              POSNOF,.TRUE.,JV,POSADD,
     *                              DIRVAL,DIRPTH,PAIVPV         )
                  END IF
                END IF
                JV = NOR(JV)
              END DO
            END IF
          END IF
        END IF
      ELSE
C
C-----  THIS VEHICLE IS ON AN INBOUND LANE OR AN OUTBOUND LANE
C
C-----  SET THE LANE TO THE LEFT AND THE LANE TO THE RIGHT OF THE
C-----  CURRENT LANE
        ILL = NLL(IL)
        ILR = NLR(IL)
        IF ( ILL . EQ . 0 )                      THEN
          ILL = IL
        END IF
        IF ( ILR . EQ . 0 )                      THEN
          ILR = IL
        END IF
C-----  IF THERE IS AN NOFIVC VEHICLE AND IT IS ON THE LANE TO THE LEFT
C-----  OR THE CURRENT LANE OR THE LANE TO THE RIGHT THEN COMPARE THE
C-----  IVPV AND THE NOFIVC VEHICLES ELSE IGNORE THE NOFIVC VEHICLE
        IF ( NOFIVC . GT . 0 )                   THEN
          IF ( ( LPRES(NOFIVC) . EQ . ILL ) . OR .
     *         ( LPRES(NOFIVC) . EQ . IL  ) . OR .
     *         ( LPRES(NOFIVC) . EQ . ILR ) )    THEN
            DIRVAL = (LPRES(NOFIVC).EQ.LPRES(IV))
            CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                      POSNOF,(.NOT. DIRVAL),NOFIVC,POSADD,
     *                      DIRVAL,DIRPTH,PAIVPV                 )
          ELSE
            NOFIVC = 0
          END IF
        END IF
C-----  IF THERE IS A NORJVC VEHICLE AND IT IS ON THE LANE TO THE LEFT
C-----  OR THE CURRENT LANE OR THE LANE TO THE RIGHT THEN COMPARE THE
C-----  IVPV AND THE NORJVC VEHICLES ELSE IGNORE THE NORJVC VEHICLE
        IF ( NORJVC . GT . 0 )                   THEN
          IF ( ( LPRES(NORJVC) . EQ .  ILL ) . OR .
     *         ( LPRES(NORJVC) . EQ .  IL  ) . OR .
     *         ( LPRES(NORJVC) . EQ .  ILR ) )   THEN
            DIRVAL = (LPRES(NORJVC).EQ.LPRES(IV))
            CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                      POSNOF,(.NOT. DIRVAL),NORJVC,POSADD,
     *                      DIRVAL,DIRPTH,PAIVPV                 )
          ELSE
            NORJVC = 0
          END IF
        END IF
        IF ( NOFIVN . EQ . 0 )                   THEN
C-----    THERE IS NO NOF VEHICLE AND THIS VEHICLE IS ON AN INBOUND LANE
C-----    OR AN OUTBOUND LANE THUS IF THIS IS AN INBOUND LANE OR
C-----    INTERNAL INBOUND LANE THEN COMPARE THE IVPV VEHICLE AND (1)
C-----    THE LAST VEHICLE ON ALL INTERSECTIONS PATHS FROM THIS LANE
C-----    WHOSE REAR BUMPER IS STILL ON THIS LANE OR THAT HAS NOT
C-----    TRAVELED FAR ENOUGH TO CLEAR THIS VEHICLE OR (2) THE LAST
C-----    VEHICLE ON THE LINKING OUTBOUND LANE FOR ALL INTERSECTION
C-----    PATHS FROM THIS LANE WHOSE REAR BUMPER IS STILL ON THIS LANE
C-----    OR THAT HAS NOT TRAVELED FAR ENOUGH TO CLEAR THIS VEHICLE
C-----    (POSSIBLY A LONG VEHICLE)
          IF ( ( ILTYPE(IL) . EQ . INBNDL ) . OR .
     *         ( ILTYPE(IL) . EQ . DINBDL ) )    THEN
            NP = LNEXT(IV)
            DIRVAL = .TRUE.
            DO  I = 1 , NPINT(IL)
              JP = LINTP(I,IL)
              JV = ILVP(JP)
              IF ( JV . EQ . 0 )                 THEN
                MOBL = LOBL(JP)
                IF ( MOBL . EQ . 0 )             CYCLE
                JV = ILVL(MOBL)
                IF ( JV . EQ . 0 )               CYCLE
                IF ( ( NP . GT . 0         ) . AND .
     *               ( JP . NE . NP        ) . AND .
     *               ( JP . NE . LPREV(JV) ) )   CYCLE
                CALL  SPVAS  ( JV,POSJV,VELJV,ACCJV,SLPJV,
     *                         .TRUE.,.FALSE.,.FALSE.,.TRUE. )
                IF ( ( NP    . GT . 0        ) . AND .
     *               ( JP    . NE . NP       ) . AND .
     *               ( POSJV . GE .
     *                 DBLE( LGEOM(1,MOBL) ) ) ) CYCLE
                POSADD = DBLE( LGEOM(4,IL)+LENP(JP)-LGEOM(1,MOBL) )
              ELSE
                CALL  SPVAS  ( JV,POSJV,VELJV,ACCJV,SLPJV,
     *                         .TRUE.,.FALSE.,.FALSE.,.TRUE. )
                POSADD = DBLE( LGEOM(4,IL) )
              END IF
              POSJV = POSJV  + POSADD
              IF ( POSJV . LT . POSNRB )         CYCLE
              IF ( POSJV . LE . ENDLP  )         THEN
                CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                          POSNOF,.FALSE.,JV,POSADD,
     *                          DIRVAL,DIRPTH,PAIVPV         )
                CYCLE
              END IF
              IF ( (.NOT. MPRO(IV)) )            CYCLE
              IF ( NP . EQ . 0  )                CYCLE
              IF ( JP . EQ . NP )                THEN
                CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                          POSNOF,.FALSE.,JV,POSADD,
     *                          DIRVAL,DIRPTH,PAIVPV         )
                CYCLE
              END IF
              HWH    = 0.5D0*WIDV(IVEHCL(JV))
              SAFVEL = DMAX1( IVEL(IV),DBLE( ISPD(IV) ),
     *                        IVEL(JV),DBLE( ISPD(JV) ) )
              SAFR   = (SAFDIS+(SAFVEL/SAFSPD))/DCHAR(IDRICL(IV))
C-----        IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE JV
C-----        VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----        FALSE
              EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *                   ( IAND( VEHTYP(JV),LAVTE ) . NE . 0 ) )
              IF ( EVRESP )                      THEN
                SAFR = DMAX1( SAFR,EVEHRZ*SAFVEL )
              END IF
              CLEARD = HWM + SAFR + HWH
              CALL  CCLEAR  ( NP,JP,CLEARD,.TRUE.,CLEART )
              CLEART = CLEART + DBLE( LGEOM(4,IL) )
              IF ( POSJV . LE . CLEART )         THEN
                CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                          POSNOF,.FALSE.,JV,POSADD,
     *                          DIRVAL,DIRPTH,PAIVPV         )
                CYCLE
              END IF
C-----      END OF LOOP FOR INTERSECTION PATHS ORIGINATING FROM THIS
C-----      LANE
            END DO
          END IF
        END IF
C-----  SET THE LANE TO THE LEFT AND THE LANE TO THE RIGHT OF THE
C-----  CURRENT LANE
        ILL = NLL(IL)
        ILR = NLR(IL)
        POSADD = 0.0D0
        DIRVAL = .FALSE.
        IF ( ILL . GT . 0 )                      THEN
C-----    CHECK ALL VEHICLES AHEAD OF THIS VEHICLE IN THE LANE TO THE
C-----    LEFT THAT ARE CHANGING LANES TO THE LEFT
          JV = IFVL(ILL)
          DO WHILE ( JV . GT . 0 )
            IF ( IPOS(JV) . LE . POSNEW )        THEN
              EXIT
            END IF
            IF ( ( ISET  (JV) . EQ . 1     ) . AND .
     *           ( LATPOS(JV) . GT . 0.0D0 ) )   THEN
              CALL  CLCCLN ( JV,ILL,LCCLN )
              IF ( .NOT. LCCLN )                 THEN
                CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                          POSNOF,.TRUE.,JV,POSADD,
     *                          DIRVAL,DIRPTH,PAIVPV         )
              END IF
            END IF
            JV = NOR(JV)
          END DO
        END IF
        IF ( ILR . GT . 0 )                      THEN
C-----    CHECK ALL VEHICLES AHEAD OF THIS VEHICLE IN THE LANE TO THE
C-----    RIGHT THAT ARE CHANGING LANES TO THE RIGHT
          JV = IFVL(ILR)
          DO WHILE ( JV . GT . 0 )
            IF ( IPOS(JV) . LE . POSNEW )        THEN
              EXIT
            END IF
            IF ( ( ISET  (JV) . EQ . 1     ) . AND .
     *           ( LATPOS(JV) . LT . 0.0D0 ) )   THEN
              CALL  CLCCLN ( JV,ILR,LCCLN )
              IF ( .NOT. LCCLN )                 THEN
                CALL  SCVIVN  ( POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                          POSNOF,.TRUE.,JV,POSADD,
     *                          DIRVAL,DIRPTH,PAIVPV         )
              END IF
            END IF
            JV = NOR(JV)
          END DO
        END IF
      END IF
C-----CHECK IF THERE IS A MAJOR COLLISION SOMEWHERE IN THE SYSTEM
      IF ( SMJCOL . AND . (.NOT. MAJCOL(IV)) )   THEN
        MAJCLB(IV) = .FALSE.
        MAJCLL(IV) = .FALSE.
        MAJCON(IV) = .FALSE.
        POSCLB(IV) = POSBIG
        POSCLL(IV) = POSBIG
        POSCON(IV) = POSBIG
        IF ( MININT(IV) )                        THEN
          CALL  CPMJCL ( IP,KV,LCHKCF,LBVSTP,POSMJC )
          IF ( KV . GT . 0 )                     THEN
C-----      THERE IS A COLLISION VEHICLE ON THE INTERSECTION PATH BEFORE
C-----      THIS VEHICLE OR DOWNSTREAM THUS SET THAT THE VEHICLE IS
C-----      BLOCKED BY A MAJOR COLLISION
            MAJCLB(IV) = .TRUE.
            POSCLB(IV) = DMIN1( POSCLB(IV),POSMJC )
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----      FALSE
            EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *                 ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
C-----    SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----    COLLISION MAY BLOCK THIS VEHICLE
          IF ( LCHKCF )                          THEN
            MAJCON(IV) = .TRUE.
            POSCON(IV) = DMIN1( POSCON(IV),POSMJC )
          END IF
        ELSE
          CALL  CLMJCL  ( IL,LNEXT(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
          IF ( KV . GT . 0 )                     THEN
C-----      THERE IS A COLLISION VEHICLE ON THE LANE BEFORE THIS VEHICLE
C-----      OR DOWNSTREAM THUS SET THAT THE VEHICLE IS BLOCKED BY A
C-----      MAJOR COLLISION
            MAJCLB(IV) = .TRUE.
            POSCLB(IV) = DMIN1( POSCLB(IV),POSMJC )
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----      FALSE
            EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *                 ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
C-----    SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----    COLLISION MAY BLOCK THIS VEHICLE
          IF ( LCHKCF )                          THEN
            MAJCON(IV) = .TRUE.
            POSCON(IV) = DMIN1( POSCON(IV),POSMJC )
          END IF
        END IF
      END IF
      IF ( IVPV . GT . 0 )                       THEN
C-----  SET THE PREVIOUS VEHICLE PARAMETERS TO THE IVPV VEHICLE
        CALL  SPVAS  ( IVPV,PVPOS,PVVEL,PVACC,PVSLP,
     *                 .TRUE.,.TRUE.,.FALSE.,.TRUE.  )
        PVPOS = PVPOS + PAIVPV
C-----  IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVPV
C-----  VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE FALSE
        EVRESP = ( ( IAND( VEHTYP(IV  ),LAVTE ) . EQ . 0 ) . AND .
     *             ( IAND( VEHTYP(IVPV),LAVTE ) . NE . 0 ) )
        IF ( .NOT. MAJCOL(IV) )                  THEN
          IF ( DIRPTH )                          THEN
C-----      SET THAT THE VEHICLE IS BLOCKED BY A MAJOR COLLISION ON A
C-----      DIRECT PATH
            IF ( (.NOT. MAJCLB(IV  )) . AND .
     *           (      MAJCLB(IVPV)) )          THEN
              MAJCLB(IV) = .TRUE.
              POSCLB(IV) = DMIN1( POSCLB(IV),POSCLB(IVPV)+PAIVPV )
              RESPEV = ( RESPEV . OR . EVRESP )
            END IF
            IF ( (.NOT. MAJCLL(IV  )) . AND .
     *           (      MAJCLL(IVPV)) )          THEN
              MAJCLL(IV) = .TRUE.
              POSCLL(IV) = DMIN1( POSCLL(IV),POSCLB(IVPV)+PAIVPV )
              RESPEV = ( RESPEV . OR . EVRESP )
            END IF
C-----      SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----      COLLISION MAY BLOCK THIS VEHICLE
            IF ( (.NOT. MAJCON(IV  )) . AND .
     *           (      MAJCON(IVPV)) )          THEN
              MAJCON(IV) = .TRUE.
              POSCON(IV) = DMIN1( POSCON(IV),POSCON(IVPV)+PAIVPV )
              RESPEV = ( RESPEV . OR . EVRESP )
            END IF
          ELSE
C-----      SET THAT THE VEHICLE IS BLOCKED BY A MAJOR COLLISION ON A
C-----      NON-DIRECT PATH
            IF ( (.NOT. MAJCLL(IV  )) . AND .
     *           (      MAJCLB(IVPV)) )          THEN
              MAJCLL(IV) = .TRUE.
              POSCLL(IV) = DMIN1( POSCLL(IV),POSCLB(IVPV)+PAIVPV )
              RESPEV = ( RESPEV . OR . EVRESP )
            END IF
C-----      SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----      COLLISION MAY BLOCK THIS VEHICLE
            IF ( (.NOT. MAJCON(IV  )) . AND .
     *           (      MAJCON(IVPV)) )          THEN
              MAJCON(IV) = .TRUE.
              POSCON(IV) = DMIN1( POSCON(IV),POSCON(IVPV)+PAIVPV )
              RESPEV = ( RESPEV . OR . EVRESP )
            END IF
          END IF
        END IF
      END IF
C-----CHECK IF THIS VEHICLE WOULD BLOCK THE INTERSECTION
      IF ( SMJCOL . AND . (.NOT. MAJCOL(IV)) )   THEN
        IF ( (.NOT. MININT(IV)                            ) . AND .
     *       (.NOT. MAJCLB(IV)                            ) . AND .
     *       (.NOT. MBLOCK(IV)                            ) . AND .
     *       (IPOS(IV).LE.DBLE( LGEOM(4,IL) )+2.0D0*XRELMI) )
     *                                           THEN
          CALL  CHKINT  ( LNEXT(IV),.TRUE.,.FALSE.,IBLOCK,MAJCBL,
     *                    POSCBL                                  )
          IF ( IBLOCK . AND . MAJCBL )           THEN
            MAJCLB(IV) = .TRUE.
            POSCLB(IV) = DMIN1( POSCLB(IV),POSCBL )
          END IF
        END IF
      END IF
      IF ( MBLOCK(IV) )                          THEN
        ENDLP = DBLE( LGEOM(2,IL) )
        IF ( PVPOS . GE . ENDLP )                MFINL(IV) = .TRUE.
      END IF
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGES
      CALL  CVMSGS  ( LVMSDD )
C-----IF PREST1 CALLED BY SUBROUTINE LOGIN THEN RETURN
                    IF ( CLOGIN )                RETURN
C-----IF THE VEHICLE IS BLOCKED BY A MAJOR COLLISION AND STOPPED AND HAS
C-----NOT STARTED THE ABORT PATH TIMER THEN START THE ABORT PATH TIMER
      IF ( ( .NOT. MAJCOL(IV)                           ) . AND .
     *     ( MAJCLB(IV) . OR . MAJCLP . OR . MAJCLL(IV) ) . AND .
     *     ( IVEL  (IV) . EQ . 0.0D0                    ) . AND .
     *     ( ABTPTM(IV) . LE . 0                        ) )
     *                                           THEN
        CALL  LGNRML ( ABTPMN,ABTPSD,NORVAL )
        NORVAL = DMAX1( NORVAL/DCHAR(IDRICL(IV)),2.0D0*PIJR(IDRICL(IV)))
        ABTPTI(IV) = -1
        ABTPTM(IV) = IDNINT( NORVAL/DT )
      END IF
C-----IF THE ABORT PATH TIME IS ACTIVE THEN PROCESS THE ABORT PATH TIMER
      IF ( ABTPTM(IV) . GT . 0 )                 THEN
C-----  INCREMENT THE ABORT PATH TIMER
        ABTPTI(IV) = ABTPTI(IV) + 1
C-----  IF THE ABORT PATH TIME IS REACHED AND THE VEHICLE IS BLOCKED BY
C-----  A MAJOR COLLISION AND THE VEHICLE IS ON AN INBOUND LANE OR A
C-----  DIAMOND INTERNAL INBOUND LANE THEN LOOK FOR AN INTERSECTION PATH
C-----  FROM THIS LANE THAT IS NOT BLOCKED OR REVERSE A LANE CHANGE
        IF ( ( .NOT. MAJCOL(IV)                           ) . AND .
     *       ( MAJCLB(IV) . OR . MAJCLP . OR . MAJCLL(IV) ) . AND .
     *       ( ABTPTI(IV) . GE . ABTPTM(IV)               ) )
     *                                           THEN
          IF ( (.NOT. MININT(IV)) )              THEN
            IF ( ( ILTYPE(IL) . EQ . INBNDL ) . OR .
     *           ( ILTYPE(IL) . EQ . DINBDL ) )  THEN
              MNEXT  = LNEXT (IV)
              MNT2P  = INT2P (IV)
              MOBAPD = NOBAPD(IV)
              INT2PB = 0
C-----        THE VEHICLES INTERSECTION PATH IS BLOCKED THUS LOOK FOR
C-----        AN INTERSECTION PATH FROM THIS LANE THAT IS NOT BLOCKED
C-----        THAT IS A STRAIGHT
              DO  I = 1 , NPINT(IL)
                JP = LINTP(I,IL)
C-----          IF INTERSECTION PATH JP TURN TYPE IS NOT STRAIGHT THEN
C-----          CHECK THE NEXT INTERSECTION PATH
                IF ( IPT(JP) . NE . 2 )          THEN
                  CYCLE
                END IF
                MOBL = LOBL(JP)
                IF ( MOBL . EQ . 0 )             CYCLE
                IF ( ILTYPE(MOBL) . EQ .
     *               DINBDL              )       THEN
C-----            INTERSECTION PATH JP LEADS TO A DIAMOND INTERNAL
C-----            INBOUND LANE
C-----            FIND THE BEST PATH (STRAIGHT) IN THE 2ND INTERSECTION
C-----            FROM THE LINKING OUTBOUND LANE FOR INTERSECTION PATH
C-----            JP THAT IS NOT BLOCKED
                  IF ( NPINT(MOBL) . GT . 0 )    THEN
                    DO  K = 1 , NPINT(MOBL)
                      KP = LINTP(K,MOBL)
                      IF ( ( JP . EQ . MNEXT ) . AND .
     *                     ( KP . EQ . MNT2P ) ) CYCLE
                      LNEXT(IV) = JP
                      INT2P(IV) = KP
                      CALL  CLMJCL  ( IL,LNEXT(IV),INT2P(IV),KV,LCHKCF,
     *                                LBVSTP,POSMJC                    )
                      LNEXT(IV) = MNEXT
                      INT2P(IV) = MNT2P
                      IF ( KV . EQ . 0 )         THEN
                        INT2PB = KP
C-----                  IF INTERSECTION PATH KP TURN TYPE IS STRAIGHT
C-----                  THEN CHOOSE LNEXT = JP AND INT2P = KP
                        IF ( IPT(KP) . EQ . 2 )  THEN
C-----                    EXIT THE K LOOP
                          EXIT
                        END IF
                      END IF
C-----              END OF THE K LOOP
                    END DO
                    IF ( INT2PB . GT . 0 )       THEN
                      NOBAPD(IV) = IOA(INT2PB)
C-----                EXIT THE I LOOP
                      EXIT
                    END IF
                  END IF
C-----            FIND THE BEST PATH (STRAIGHT) IN THE 2ND INTERSECTION
C-----            FROM ALL LANES ON THE APPROACH FOR THE LINKING
C-----            OUTBOUND LANE FOR INTERSECTION PATH JP THAT IS NOT
C-----            BLOCKED
                  JA = ISNA(MOBL)
                  DO  J = 1 , NLANES(JA)
                    JL = LLANES(NLANES(JA)-J+1,JA)
                    IF ( JL . EQ . MOBL )        CYCLE
                    IF ( NPINT(JL) . EQ . 0 )    CYCLE
                    DO  K = 1 , NPINT(JL)
                      KP = LINTP(K,JL)
                      IF ( ( JP . EQ . MNEXT ) . AND .
     *                     ( KP . EQ . MNT2P ) ) CYCLE
                      LNEXT(IV) = JP
                      INT2P(IV) = KP
                      CALL  CLMJCL  ( IL,LNEXT(IV),INT2P(IV),KV,LCHKCF,
     *                                LBVSTP,POSMJC                    )
                      LNEXT(IV) = MNEXT
                      INT2P(IV) = MNT2P
                      IF ( KV . EQ . 0 )         THEN
                        INT2PB = KP
C-----                  IF INTERSECTION PATH KP TURN TYPE IS STRAIGHT
C-----                  THEN CHOOSE LNEXT = JP AND INT2P = KP
                        IF ( IPT(KP) . EQ . 2 )  THEN
C-----                    EXIT THE K LOOP
                          EXIT
                        END IF
                      END IF
C-END-              DO  K = 1 , NPINT(JL)
                    END DO
                    IF ( INT2PB . GT . 0 )       THEN
                      NOBAPD(IV) = IOA(INT2PB)
C-----                EXIT THE J LOOP
                      EXIT
                    END IF
C-END-            DO  J = 1 , NLANES(JA)
                  END DO
                  IF ( MOBAPD .NE. NOBAPD(IV) )  THEN
C-----              EXIT THE I LOOP
                    EXIT
                  END IF
                ELSE
C-----            INTERSECTION PATH JP LEADS TO AN OUTBOUND LANE
                  IF ( JP . EQ . MNEXT )         CYCLE
                  LNEXT(IV) = JP
                  INT2P(IV) = 0
                  CALL  CLMJCL  ( IL,LNEXT(IV),0,KV,LCHKCF,LBVSTP,
     *                            POSMJC                           )
                  LNEXT(IV) = MNEXT
                  INT2P(IV) = MNT2P
                  IF ( KV . EQ . 0 )             THEN
                    NOBAPD(IV) = IOA(JP)
C-----              EXIT THE I LOOP
                    EXIT
                  END IF
                END IF
C-END-        DO  I = 1 , NPINT(IL) FOR STRAIGHT
              END DO
              IF ( MOBAPD . EQ . NOBAPD(IV) )    THEN
                DO  I = 1 , NPINT(IL)
                  JP = LINTP(I,IL)
                  MOBL = LOBL(JP)
                  IF ( MOBL . EQ . 0 )           CYCLE
                  IF ( ILTYPE(MOBL) . EQ .
     *                 DINBDL              )     THEN
C-----              INTERSECTION PATH JP LEADS TO A DIAMOND INTERNAL
C-----              INBOUND LANE
C-----              FIND THE BEST PATH (STRAIGHT) IN THE 2ND
C-----              INTERSECTION FROM THE LINKING OUTBOUND LANE FOR
C-----              INTERSECTION PATH JP THAT IS NOT BLOCKED
                    IF ( NPINT(MOBL) . GT . 0 )  THEN
                      DO  K = 1 , NPINT(MOBL)
                        KP = LINTP(K,MOBL)
                        IF ( ( JP .EQ. MNEXT ) . AND .
     *                       ( KP .EQ. MNT2P ) ) CYCLE
                        LNEXT(IV) = JP
                        INT2P(IV) = KP
                        CALL  CLMJCL  ( IL,LNEXT(IV),INT2P(IV),KV,
     *                                  LCHKCF,LBVSTP,POSMJC       )
                        LNEXT(IV) = MNEXT
                        INT2P(IV) = MNT2P
                        IF ( KV . EQ . 0 )       THEN
                          INT2PB = KP
C-----                    IF INTERSECTION PATH KP TURN TYPE IS STRAIGHT
C-----                    THEN CHOOSE LNEXT = JP AND INT2P = KP
                          IF ( IPT(KP) . EQ . 2 )THEN
C-----                      EXIT THE K LOOP
                            EXIT
                          END IF
                        END IF
C-----                END OF THE K LOOP
                      END DO
                      IF ( INT2PB . GT . 0 )     THEN
                        NOBAPD(IV) = IOA(INT2PB)
C-----                  EXIT THE I LOOP
                        EXIT
                      END IF
                    END IF
C-----              FIND THE BEST PATH (STRAIGHT) IN THE 2ND
C-----              INTERSECTION FROM ALL LANES ON THE APPROACH FOR THE
C-----              LINKING OUTBOUND LANE FOR INTERSECTION PATH JP THAT
C-----              IS NOT BLOCKED
                    JA = ISNA(MOBL)
                    DO  J = 1 , NLANES(JA)
                      JL = LLANES(NLANES(JA)-J+1,JA)
                      IF ( JL . EQ . MOBL )      CYCLE
                      IF ( NPINT(JL) . EQ . 0 )  CYCLE
                      DO  K = 1 , NPINT(JL)
                        KP = LINTP(K,JL)
                        IF ( ( JP .EQ. MNEXT ) . AND .
     *                       ( KP .EQ. MNT2P ) ) CYCLE
                        LNEXT(IV) = JP
                        INT2P(IV) = KP
                        CALL  CLMJCL  ( IL,LNEXT(IV),INT2P(IV),KV,
     *                                  LCHKCF,LBVSTP,POSMJC       )
                        LNEXT(IV) = MNEXT
                        INT2P(IV) = MNT2P
                        IF ( KV . EQ . 0 )       THEN
                          INT2PB = KP
C-----                    IF INTERSECTION PATH KP TURN TYPE IS STRAIGHT
C-----                    THEN CHOOSE LNEXT = JP AND INT2P = KP
                          IF ( IPT(KP) . EQ . 2 )THEN
C-----                      EXIT THE K LOOP
                            EXIT
                          END IF
                        END IF
C-END-                DO  K = 1 , NPINT(JL)
                      END DO
                      IF ( INT2PB . GT . 0 )     THEN
                        NOBAPD(IV) = IOA(INT2PB)
C-----                  EXIT THE J LOOP
                        EXIT
                      END IF
C-END-              DO  J = 1 , NLANES(JA)
                    END DO
                    IF ( MOBAPD .NE. NOBAPD(IV) )THEN
C-----                EXIT THE I LOOP
                      EXIT
                    END IF
                  ELSE
C-----              INTERSECTION PATH JP LEADS TO AN OUTBOUND LANE
                    IF ( JP . EQ . MNEXT )       CYCLE
                    LNEXT(IV) = JP
                    INT2P(IV) = 0
                    CALL  CLMJCL  ( IL,LNEXT(IV),0,KV,LCHKCF,LBVSTP,
     *                              POSMJC                           )
                    LNEXT(IV) = MNEXT
                    INT2P(IV) = MNT2P
                    IF ( KV . EQ . 0 )           THEN
                      NOBAPD(IV) = IOA(JP)
C-----                EXIT THE I LOOP
                      EXIT
                    END IF
                  END IF
C-END-          DO  I = 1 , NPINT(IL)
                END DO
C-END-        IF ( MOBAPD . EQ . NOBAPD(IV) )
              END IF
C-----        LOOK FOR PATHS FROM OTHER LANES ON THIS LEG IN THE FUTURE
              IF ( MOBAPD . NE . NOBAPD(IV) )    THEN
                WRITE (WRNMSG,702) IQ(IV),IA,TIME,NOBAPD(IV),MOBAPD,
     *                             'PREST1'
                CALL  PRTWRN  ( WRNMSG )
                IF ( IQ(IV) . LT . 0 )           THEN
                  CALL  VDIERR  ( TIME,-IQ(IV),VDIFDO )
                END IF
                CALL  UNSETC
                INT2P (IV) = 0
                MLRTOR(IV) = .FALSE.
C-----          FIND AN INTERSECTION PATH FOR THIS VEHICLE BASED ON THE
C-----          CURRENT APPROACH, CURRENT LANE, AND THE DESIRED OUTBOUND
C-----          APPROACH
                IF ( JP . EQ . MNEXT )           MNEXT = 0
                IF ( ISET(IV) . NE . 1 )         THEN
                  ISET (IV) = 5
                  LEGAL(IV) = 4
                  LALT (IV) = 5
                  CALL  CKLALT
                END IF
                CALL  PATHF  ( .TRUE.,MNEXT,'PREST1' )
                LOGTMP     = 1
                LOGFLG(IV) = 1
                MAJCLB(IV) = .FALSE.
                MAJCLL(IV) = .FALSE.
                MAJCON(IV) = .FALSE.
                POSCLB(IV) = POSBIG
                POSCLL(IV) = POSBIG
                POSCON(IV) = POSBIG
                CALL  CLMJCL  ( IL,LNEXT(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
                IF ( KV . GT . 0 )               THEN
C-----            THERE IS A COLLISION VEHICLE ON THE LANE BEFORE THIS
C-----            VEHICLE OR DOWNSTREAM THUS SET THAT THE VEHICLE IS
C-----            BLOCKED BY A MAJOR COLLISION
                  MAJCLB(IV) = .TRUE.
                  POSCLB(IV) = DMIN1( POSCLB(IV),POSMJC )
C-----            IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----            VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE
C-----            ELSE FALSE
                  EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) .AND.
     *                       ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
                  RESPEV = ( RESPEV . OR . EVRESP )
                END IF
C-----          SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A
C-----          MAJOR COLLISION MAY BLOCK THIS VEHICLE
                IF ( LCHKCF )                    THEN
                  MAJCON(IV) = .TRUE.
                  POSCON(IV) = DMIN1( POSCON(IV),POSMJC )
                END IF
                MAJCBP = .TRUE.
                RETURN
C-END-        IF ( MOBAPD .NE. NOBAPD(IV) )      THEN
              END IF
C-END-      IF ( ( ILTYPE(IL) . EQ . INBNDL ) . OR .
C-END*           ( ILTYPE(IL) . EQ . DINBDL ) )  THEN
            END IF
            IF ( IVPV . GT . 0 )                 THEN
              POSREL = PVPOS + XRELMI - POSNEW
            ELSE
              POSREL = ENDLP + XRELMI - POSNEW
            END IF
C-----      IF THE VEHICLE IS BLOCKED BY A MAJOR COLLISION, IS CHANGING
C-----      LANES, AND ITS POSITION IS GREATER THAN OR EQUAL TO XRELMI
C-----      FROM THE PREVIOUS VEHICLE OR END OF LANE THEN CHECK IF THE
C-----      LANE CHANGE CAN BE REVERSED BACK INTO THE PREVIOUS LANE
            IF ( ( MAJCLB(IV)             ) . AND .
     *           ( ISET(IV) . EQ . 1      ) . AND .
     *           ( POSREL   . GE . XRELMI ) . AND .
     *           ( .NOT. MAJRLC(IV)       ) )    THEN
C-----        SET THE INDEX FOR THE LANE ON THE SIDE OF INTEREST TO
C-----        CHECK
              IF ( LATPOS(IV) . GT . 0.0D0 )     THEN
C-----          THE VEHICLE IS CHANGING LANES TO THE LEFT
                LANSI = NLR(IL)
              ELSE
C-----          THE VEHICLE IS CHANGING LANES TO THE RIGHT
                LANSI = NLL(IL)
              END IF
              LBLOCK = .FALSE.
              IF ( LANSI . GT . 0 )              THEN
C-----          CHECK THE LANE ON THE SIDE OF INTEREST TO SEE IF THE
C-----          LANE IS AVAILABLE AT THE CURRENT POSITION OF THE VEHICLE
C-----          AND CLEAR TO THE INTERSECTION
                CALL  CHKLSI  ( LANSI,POSNEW,LEGAL(IV),LOK )
                IF ( LOK . EQ . 0 )              THEN
                  IF ( LALT(IV) . GE . 5 )       CALL  CKLALT
                  IF ( LATPOS(IV) . LT . 0.0D0 ) THEN
C-----              THE ORIGINAL LANE IS TO THE LEFT
                    CALL  CLMJCL  ( NLL(IL),LALTPL(IV),0,KV,LCHKCF,
     *                              LBVSTP,POSMJC                   )
                    IF ( KV . GT . 0 )           THEN
C-----                THE LANE TO THE LEFT IS BLOCKED BY A MAJOR
C-----                COLLISION THUS SET LANE BLOCKED
                      LBLOCK = .TRUE.
                    ELSE
                      IF ( LALTPL(IV) . GT . 0 ) THEN
                        CALL  CHKINT  ( LALTPL(IV),.FALSE.,.FALSE.,
     *                                  IBLOCK,MAJCBL,POSMJC        )
                        IF ( IBLOCK.AND.MAJCBL ) THEN
C-----                    THE LANE TO THE LEFT IS NOT BLOCKED BY A
C-----                    MAJOR COLLISION BUT THE INTERSECTION PATH FROM
C-----                    THE LANE TO THE LEFT TO THE DESIRED OUTBOUND
C-----                    APPROACH IS BLOCKED BY A MAJOR COLLISION THUS
C-----                    SET LANE BLOCKED
                          LBLOCK = .TRUE.
                        END IF
                      END IF
                    END IF
                  ELSE
C-----              THE ORIGINAL LANE IS TO THE RIGHT
                    CALL  CLMJCL  ( NLR(IL),LALTPR(IV),0,KV,LCHKCF,
     *                              LBVSTP,POSMJC                   )
                    IF ( KV . GT . 0 )           THEN
C-----                THE LANE TO THE RIGHT IS BLOCKED BY A MAJOR
C-----                COLLISION THUS SET LANE BLOCKED
                      LBLOCK = .TRUE.
                    ELSE
                      IF ( LALTPR(IV) . GT . 0 ) THEN
                        CALL  CHKINT  ( LALTPR(IV),.FALSE.,.FALSE.,
     *                                  IBLOCK,MAJCBL,POSMJC        )
                        IF ( IBLOCK.AND.MAJCBL ) THEN
C-----                    THE LANE TO THE RIGHT IS NOT BLOCKED BY A
C-----                    MAJOR COLLISION BUT THE INTERSECTION PATH FROM
C-----                    THE LANE TO THE RIGHT TO THE DESIRED OUTBOUND
C-----                    APPROACH IS BLOCKED BY A MAJOR COLLISION THUS
C-----                    SET LANE BLOCKED
                          LBLOCK = .TRUE.
                        END IF
                      END IF
                    END IF
                  END IF
                  IF ( (.NOT. LBLOCK) )          THEN
C-----              THE LANE ON THE SIDE OF INTEREST IS NOT BLOCKED BY A
C-----              MAJOR COLLISION THUS TRY TO REVERSE THE LANE CHANGE
                    MAJCLC = .TRUE.
                  END IF
C-END-          IF ( LOK . EQ . 0 )              THEN
                END IF
C-END-        IF ( LANSI . GT . 0 )              THEN
              END IF
C-END-      IF ( ( MAJCLB(IV)             ) . AND .
C-END*           ( ISET(IV) . EQ . 1      ) . AND .
C-END*           ( POSREL   . GE . XRELMI ) . AND .
C-END*           ( .NOT. MAJRLC(IV)       ) )    THEN
            END IF
C-END-    IF ( (.NOT. MININT(IV)) )              THEN
          END IF
C-END-  IF ( ( .NOT. MAJCOL(IV)                           ) . AND .
C-END*       ( MAJCLB(IV) . OR . MAJCLP . OR . MAJCLL(IV) ) . AND .
C-END*       ( ABTPTI(IV) . GE . ABTPTM(IV)               ) )
C-END*                                           THEN
        END IF
C-END-IF ( ABTPTM(IV) . GT . 0 )                 THEN
      END IF
      RETURN
      END                                                               PREST1
C
C
C
      SUBROUTINE CVMSGS ( LVMSDD )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           FCVMSC,FCVMSS,FNVMSC,FNVMSS,LVMSDD
      INTEGER           ACMVMS,ASMVMS,CCMVMS,CSMVMS,I
      DOUBLE PRECISION  DISEND,DISLCH,DSPLCH,MRT,POSEND,VELLCH,VEHLNG
C 601 FORMAT(' VMS MSG ',I4,' NUM ',I2,' PAR ',F7.2,' VEH ',I6,' TIME ',
C    *       F7.2,' POS ',F7.2,' VEL ',F7.2,' A/D ',F7.2,' SLP ',F7.2)
C 602 FORMAT(' VMS MSG ',I4,' NUM ',I2,' PAR ',F7.2,' VEH ',I6,' TIME ',
C    *       F7.2,' MRT ',I4)
C
C-----SUBROUTINE CVMSGS CHECKS VEHICLE MESSAGE SYSTEM MESSAGES
C
C-----LVMSDD = DISTRACTED DRIVER VMS MESSAGE WAS CANCELLED OR TIMED OUT
C
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CVMSGS'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      LVMSDD = .FALSE.
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGES
                    IF ( NVMSM . EQ . 0 )        GO TO 2010
C-----DETERMINE HIGHEST PRIORITY ACTIVE CHANGE LANE MESSAGE (USED LAST
C-----DT OR NEXT)
      ACMVMS = VMSACM(IV)
      IF ( VMSACN(IV) . GT . 0 )                 THEN
C-----  A NEXT MESSAGE MUST BE OF EQUAL OR HIGHER PRIORITY THAN THE
C-----  ACTIVE MESSAGE
        ACMVMS = VMSACN(IV)
      END IF
C-----DETERMINE HIGHEST PRIORITY ACTIVE SPEED MESSAGE (USED LAST DT OR
C-----NEXT)
      ASMVMS = VMSASM(IV)
      IF ( VMSASN(IV) . GT . 0 )                 THEN
C-----  A NEXT MESSAGE MUST BE OF EQUAL OR HIGHER PRIORITY THAN THE
C-----  ACTIVE MESSAGE
        ASMVMS = VMSASN(IV)
      END IF
C-----SEARCH ALL MESSAGES THAT HAVE NOT EXPIRED TO DETERMINE THE HIGHEST
C-----PRIORITY CHANGE LANE AND SPEED MESSAGE THAT WOULD APPLY TO THIS
C-----VEHICLE THIS DT
      CCMVMS = 0
      CSMVMS = 0
      DO 1010  I = IVMSMB , NVMSM
C-----IF THE CURRENT TIME IS LESS THAN THE VEHICLE MESSAGE SYSTEM
C-----MESSAGE START TIME THEN GO TO 1020 AND IGNORE THE MESSAGE AND
C-----IGNORE ALL REMAINING VEHICLE MESSAGE SYSTEM MESSAGES
      IF ( TIME . LT . DVMSST(I) )               GO TO 1020
C-----IF THE CURRENT TIME IS GREATER THAN THE VEHICLE MESSAGE SYSTEM
C-----MESSAGE START TIME PLUS ACTIVE TIME THEN GO TO 1010 AND IGNORE THE
C-----MESSAGE AND CHECK THE NEXT MESSAGE
      IF ( TIME . GT . DVMSST(I)+DVMSAT(I) )     GO TO 1010
C-----IF THE VEHICLE MESSAGE SYSTEM MESSAGE TYPE IS DRIVER DMS AND
C-----THE DRIVER-VEHICLE UNIT CAN IGNORE DMS MESSAGES THEN GO TO 1010
C-----AND IGNORE THE MESSAGE AND CHECK THE NEXT MESSAGE
      IF ( ( IVMSMT(I) . EQ . VMSTDD ) . AND .
     *     DMSI(IV)                    )         GO TO 1010
C-----IF THE VEHICLE MESSAGE SYSTEM MESSAGE TYPE IS DRIVER IVDMS AND
C-----THE DRIVER-VEHICLE UNIT CAN IGNORE IVDMS MESSAGES THEN GO TO 1010
C-----AND IGNORE THE MESSAGE AND CHECK THE NEXT MESSAGE
      IF ( ( IVMSMT(I) . EQ . VMSTDI ) . AND .
     *     IVDMSI(IV)                )           GO TO 1010
C-----IF THE VEHICLE MESSAGE SYSTEM MESSAGE TYPE IS DRIVER IVDMS AND
C-----THE DRIVER-VEHICLE UNIT DOES NOT HAVE AN OPERATIONAL IVDMS THEN
C-----GO TO 1010 AND IGNORE THE MESSAGE AND CHECK THE NEXT MESSAGE
      IF ( ( IVMSMT(I) . EQ . VMSTDI ) . AND .
     *     (.NOT. IVDMSO(IV)         ) )         GO TO 1010
C-----IF THE VEHICLE MESSAGE SYSTEM MESSAGE TYPE IS VEHICLE IVDMS AND
C-----THE DRIVER-VEHICLE UNIT DOES NOT HAVE AN OPERATIONAL IVDMS THEN
C-----GO TO 1010 AND IGNORE THE MESSAGE AND CHECK THE NEXT MESSAGE
      IF ( ( IVMSMT(I) . EQ . VMSTVI ) . AND .
     *     (.NOT. IVDMSO(IV)         ) )         GO TO 1010
C-----IF THE VEHICLE MESSAGE SYSTEM APPROACH NUMBER OR THE INTERSECTION
C-----PATH NUMBER IS NOT EQUAL TO THE CURRENT APPROACH NUMBER OR
C-----INTERSECTION PATH NUMBER THEN GO TO 1010 AND IGNORE THE MESSAGE
C-----AND CHECK THE NEXT MESSAGE
      IF ( MININT(IV) )                          THEN
C-----  THE VEHICLE IS ON AN INTERSECTION PATH
        IF ( -IVMSAP(I) . NE . IP )              GO TO 1010
C-----  IF THE VEHICLE MESSAGE SYSTEM MESSAGE IS VMSMCL CHANGE LANES TO
C-----  THE LEFT OR VMSMCR CHANGE LANES TO THE RIGHT THEN GO TO 1010 AND
C-----  IGNORE THE MESSAGE AND CHECK THE NEXT MESSAGE
        IF ( ( IVMSMG(I) . EQ . VMSMCL ) . OR .
     *       ( IVMSMG(I) . EQ . VMSMCR ) )       GO TO 1010
        POSEND = LENP(IP)
      ELSE
C-----  THE VEHICLE IS ON AN INBOUND OR OUTBOUND APPROACH
        IF (  IVMSAP(I) . NE . IA )              GO TO 1010
C-----  IF THE CURRENT LANE NUMBER IS NOT BETWEEN THE VEHICLE MESSAGE
C-----  SYSTEM BEGINNING LANE NUMBER AND THE ENDING LANE NUMBER THEN GO
C-----  TO 1010 AND IGNORE THE MESSAGE AND CHECK THE NEXT MESSAGE
        IF ( ( ILN . LT . IVMSLB(I) ) . OR .
     *       ( ILN . GT . IVMSLE(I) ) )          GO TO 1010
C-----  IF THE VEHICLE MESSAGE SYSTEM MESSAGE IS VMSMCL CHANGE LANES TO
C-----  THE LEFT AND THERE IS NO LANE TO THE LEFT THEN GO TO 1010 AND
C-----  IGNORE THE MESSAGE AND CHECK THE NEXT MESSAGE
        IF ( ( IVMSMG(I) . EQ . VMSMCL ) . AND .
     *       ( ILN       . EQ . 1      ) )       GO TO 1010
C-----  IF THE VEHICLE MESSAGE SYSTEM MESSAGE IS VMSMCR CHANGE LANES TO
C-----  THE RIGHT AND THERE IS LANE TO THE RIGHT THEN GO TO 1010 AND
C-----  IGNORE THE MESSAGE AND CHECK THE NEXT MESSAGE
        IF ( ( IVMSMG(I) . EQ . VMSMCR     ) . AND .
     *       ( ILN       . EQ . NLANES(IA) ) )   GO TO 1010
        IF ( LGEOM(3,IL) . EQ . LGEOM(4,IL) )    THEN
          POSEND = DBLE( LGEOM(2,IL) )
        ELSE
          POSEND = DBLE( LGEOM(4,IL) )
        END IF
      END IF
C-----IF THE CURRENT POSITION OF THE FRONT BUMPER OF THE VEHICLE IS
C-----LESS THAN THE VEHICLE MESSAGE SYSTEM BEGINNING POSITION THEN GO
C-----TO 1010 AND IGNORE THE MESSAGE AND CHECK THE NEXT MESSAGE
      IF ( IPOS(IV) . LT . DVMSPB(I) )           GO TO 1010
C-----IF THE CURRENT POSITION OF THE POSITION BEHIND THE FRONT BUMPER OF
C-----THE VEHICLE THAT THE VEHICLE MESSAGE SYSTEM MESSAGES CAN BE
C-----DETECTED IS GREATER THAN THE VEHICLE MESSAGE SYSTEM ENDING
C-----POSITION THEN GO TO 1010 AND IGNORE THE MESSAGE AND CHECK THE NEXT
C-----MESSAGE
      IF ( ( DVMSPE(I)         .LT. POSEND    ) . AND .
     *     ( (IPOS(IV)-VMSPOS) .GT. DVMSPE(I) ) )GO TO 1010
C-----IF (1) THE VEHICLE MESSAGE SYSTEM MESSAGE TYPE IS DRIVER IVDMS OR
C-----VEHICLE IVDMS AND (2) THE VEHICLE MESSAGE SYSTEM MESSAGE VEHICLE
C-----NUMBER IS GREATER THAN ZERO AND (3) THE VEHICLE MESSAGE SYSTEM
C-----MESSAGE VEHICLE NUMBER IS NOT THE CURRENT VEHICLE NUMBER THEN GO
C-----TO 1010 AND IGNORE THE MESSAGE AND CHECK THE NEXT MESSAGE
      IF ( ( (IVMSMT(I).EQ.VMSTDI) .OR. (IVMSMT(I).EQ.VMSTVI) ) . AND .
     *     ( IVMSVN(I) . GT . 0                               ) . AND .
     *     ( IVMSVN(I) . NE . IQ(IV)                          ) )
     *                                           GO TO 1010
C-----THE VEHICLE MESSAGE SYSTEM MESSAGE APPLIES TO THIS VEHICLE THIS DT
C     WRITE (6,601)  I,IVMSMG(I),DVMSMP(I),IQ(IV),TIME,IPOS(IV),
C    *               IVEL(IV),IACC(IV),ISLP(IV)
C     WRITE (*,601)  I,IVMSMG(I),DVMSMP(I),IQ(IV),TIME,IPOS(IV),
C    *               IVEL(IV),IACC(IV),ISLP(IV)
C-----FIND HIGHEST PRIORITY (1=HIGHEST) CURRENT CHANGE LANE MESSAGE
      IF ( IVMSMC(IVMSMG(I)) . EQ . VMSMCC )     THEN
        IF ( CCMVMS . GT . 0 )                   THEN
          IF ( IVMSPR(IVMSMG(I     )) . LT .
     *         IVMSPR(IVMSMG(CCMVMS)) )          THEN
            CCMVMS = I
          END IF
        ELSE
          CCMVMS = I
        END IF
C-----  IF MESSAGE IS TO A SINGLE VEHICLE THEN FORCE HIGHEST PRIORITY
        IF ( ( IVMSVN(I) . GT . 0      ) . AND .
     *       ( IVMSVN(I) . EQ . IQ(IV) ) )       THEN
          CCMVMS = I
        END IF
      END IF
C-----FIND HIGHEST PRIORITY (1=HIGHEST) CURRENT SPEED MESSAGE
      IF ( IVMSMC(IVMSMG(I)) . EQ . VMSMCS )     THEN
        IF ( CSMVMS . GT . 0 )                   THEN
          IF ( IVMSPR(IVMSMG(I     )) . LT .
     *         IVMSPR(IVMSMG(CSMVMS)) )          THEN
            CSMVMS = I
          END IF
        ELSE
          CSMVMS = I
        END IF
C-----  IF MESSAGE IS TO A SINGLE VEHICLE THEN FORCE HIGHEST PRIORITY
        IF ( ( IVMSVN(I) . GT . 0      ) . AND .
     *       ( IVMSVN(I) . EQ . IQ(IV) ) )       THEN
          CSMVMS = I
        END IF
      END IF
 1010 CONTINUE
 1020 CONTINUE
C-----SET HIGHEST PRIORITY (1=HIGHEST) CHANGE LANE MESSAGE BETWEEN
C-----CURRENT AND ACTIVE AND SET ACTION TO TAKE
      FCVMSC = .FALSE.
      FNVMSC = .FALSE.
      IF ( CCMVMS . GT . 0 )                     THEN
        IF ( ACMVMS . GT . 0 )                   THEN
          IF ( ( IVMSPR(IVMSMG(CCMVMS)) . LE .
     *           IVMSPR(IVMSMG(ACMVMS)) ) . AND .
     *         ( ACMVMS . NE . CCMVMS   ) )      THEN
C-----      INITIATE CURRENT MESSAGE
            FNVMSC = .TRUE.
          ELSE
C-----      CONTINUE ACTIVE MESSAGE
          END IF
        ELSE
C-----    INITIATE CURRENT MESSAGE
          FNVMSC = .TRUE.
        END IF
C-----  IF MESSAGE IS TO A SINGLE VEHICLE THEN FORCE HIGHEST PRIORITY
        IF ( IVMSVN(I) . EQ . IQ(IV) )           THEN
          IF ( ACMVMS . NE . CCMVMS )            THEN
            FNVMSC = .TRUE.
          END IF
        END IF
      ELSE
        IF ( ACMVMS . GT . 0 )                   THEN
C-----    CANCEL ACTIVE MESSAGE
          FCVMSC = .TRUE.
        ELSE
C-----    CONTINUE NO MESSAGE
        END IF
      END IF
C-----SET HIGHEST PRIORITY (1=HIGHEST) SPEED MESSAGE BETWEEN
C-----CURRENT AND ACTIVE AND SET ACTION TO TAKE
      FCVMSS = .FALSE.
      FNVMSS = .FALSE.
      IF ( CSMVMS . GT . 0 )                     THEN
        IF ( ASMVMS . GT . 0 )                   THEN
          IF ( ( IVMSPR(IVMSMG(CSMVMS)) . LE .
     *           IVMSPR(IVMSMG(ASMVMS)) ) . AND .
     *         ( ASMVMS . NE . CSMVMS   ) )      THEN
C-----      INITIATE CURRENT MESSAGE
            FNVMSS = .TRUE.
          ELSE
C-----      CONTINUE ACTIVE MESSAGE
          END IF
        ELSE
C-----    INITIATE CURRENT MESSAGE
          FNVMSS = .TRUE.
        END IF
C-----  IF MESSAGE IS TO A SINGLE VEHICLE THEN FORCE HIGHEST PRIORITY
        IF ( IVMSVN(I) . EQ . IQ(IV) )           THEN
          IF ( ASMVMS . NE . CSMVMS )            THEN
            FNVMSS = .TRUE.
          END IF
        END IF
      ELSE
        IF ( ASMVMS . GT . 0 )                   THEN
C-----    CANCEL ACTIVE MESSAGE
          FCVMSS = .TRUE.
        ELSE
C-----    CONTINUE NO MESSAGE
        END IF
      END IF
C-----PROCESS CANCEL VEHICLE MESSAGE SYSTEM SPEED MESSAGE
      IF ( FCVMSS )                              THEN
        IF ( VMSASM(IV) . GT . 0 )               THEN
          IF ( ( IVMSMG(VMSASM(IV)).EQ.VMSMSI ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSL ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSM ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSC ) )THEN
C-----      VMSMSI - STOP AT THE INTERSECTION STOP LINE
C-----      VMSMSL - STOP AT LOCATION XX
C-----      VMSMSM - STOP IMMEDIATELY USING MAXIMUM VEHICLE DECELERATION
C-----      VMSMSC - STOP IMMEDIATELY USING COLLISION DECELERATION
            IF ( (.NOT. FSTACT(IV)) )            MFSTPF(IV) = .FALSE.
            VMSPST(IV) = 0.0D0
            LOGTMP     = 1
            LOGFLG(IV) = 1
          ELSE IF ( IVMSMG(VMSASM(IV)).EQ.VMSMGO )
     *                                           THEN
C-----      VMSMGO - FORCED GO
            MFGOF (IV) = .FALSE.
            MFSTPF(IV) = .FALSE.
            FSTACT(IV) = .FALSE.
            SDWELL(IV) = 0
            LOGTMP     = 1
            LOGFLG(IV) = 1
          ELSE IF ( IVMSMG(VMSASM(IV)).EQ.VMSMRR )
     *                                           THEN
C-----      VMSMRR - FORCED RUN THE RED SIGNAL
          ELSE IF ( IVMSMG(VMSASM(IV)).EQ.VMSMDD )
     *                                           THEN
C-----      VMSMDD - DISTRACTED DRIVER
            LVMSDD = .TRUE.
          END IF
        END IF
        VMSASM(IV) = 0
        VMSASN(IV) = 0
        VMSAST(IV) = 0
      END IF
C-----PROCESS NEW VEHICLE MESSAGE SYSTEM SPEED MESSAGE
      IF ( FNVMSS )                              THEN
        VMSASN(IV) = CSMVMS
        VMSAST(IV) = 1
        MRT = 0.0D0
        IF      ( CVMSDN(CSMVMS) .EQ. "CONSTAN" )THEN
C-----    PROCESS CONSTANT RANDOM DEVIATES
          CALL  CONST   ( DVMSDM(CSMVMS)                         ,MRT )
        ELSE IF ( CVMSDN(CSMVMS) .EQ. "ERLANG"  )THEN
C-----    PROCESS ERLANG RANDOM DEVIATES
          CALL  ERLANG  ( DVMSDM(CSMVMS),IDNINT( DVMSDP(CSMVMS) ),MRT )
        ELSE IF ( CVMSDN(CSMVMS) .EQ. "GAMMA"   )THEN
C-----    PROCESS GAMMA RANDOM DEVIATES
          CALL  GAMMA   ( DVMSDM(CSMVMS),        DVMSDP(CSMVMS)  ,MRT )
        ELSE IF ( CVMSDN(CSMVMS) .EQ. "LOGNRML" )THEN
C-----    PROCESS LOGNORMAL RANDOM DEVIATES
          CALL  LGNRML  ( DVMSDM(CSMVMS),        DVMSDP(CSMVMS)  ,MRT )
        ELSE IF ( CVMSDN(CSMVMS) .EQ. "NEGEXP"  )THEN
C-----    PROCESS NEGATIVE EXPONENTIAL RANDOM DEVIATES
          CALL  NEGEXP  ( DVMSDM(CSMVMS)                         ,MRT )
        ELSE IF ( CVMSDN(CSMVMS) .EQ. "SNEGEXP" )THEN
C----     PROCESS SHIFTED NEGATIVE EXPONENTIAL RANDOM DEVIATES
          CALL  SNEGEX  ( DVMSDM(CSMVMS),        DVMSDP(CSMVMS)  ,MRT )
        ELSE IF ( CVMSDN(CSMVMS) .EQ. "UNIFORM" )THEN
C-----    PROCESS UNIFORM RANDOM DEVIATES
          CALL  UNIFRM  ( DVMSDM(CSMVMS),        DVMSDP(CSMVMS)  ,MRT )
        END IF
        IF ( ( IVMSMT(I) . EQ . VMSTDD ) . OR .
     *       ( IVMSMT(I) . EQ . VMSTDI ) )       THEN
          MRT = MRT + PIJR(IDRICL(IV)) - APIJR
        END IF
        VMSAST(IV) = MAX0( IDNINT( MRT/DT ),0 ) + 1
C       WRITE (6,602)  VMSASN(IV),IVMSMG(VMSASN(IV)),DVMSMP(VMSASN(IV)),
C    *                 IQ(IV),TIME,VMSAST(IV)
      END IF
C-----DECREMENT MESSAGE REACTION TIME TIMER FOR VEHICLE MESSAGE SYSTEM
C-----SPEED MESSAGE AND TAKE ACTION IF TIMED OUT
      IF ( VMSAST(IV) . GT . 0 )                 THEN
        VMSAST(IV) = VMSAST(IV) - 1
C       WRITE (6,602)  VMSASN(IV),IVMSMG(VMSASN(IV)),DVMSMP(VMSASN(IV)),
C    *                 IQ(IV),TIME,VMSAST(IV)
        IF ( VMSAST(IV) . EQ . 0 )               THEN
          VMSASM(IV) = VMSASN(IV)
          VMSASN(IV) = 0
        END IF
      END IF
C-----PROCESS VEHICLE MESSAGE SYSTEM SPEED MESSAGE
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        IF ( ( IVMSMG(VMSASM(IV)) .EQ. VMSMSI ) . OR .
     *       ( IVMSMG(VMSASM(IV)) .EQ. VMSMSL ) . OR .
     *       ( IVMSMG(VMSASM(IV)) .EQ. VMSMSM ) . OR .
     *       ( IVMSMG(VMSASM(IV)) .EQ. VMSMSC ) )THEN
C-----    VMSMSI - STOP AT THE INTERSECTION STOP LINE
C-----    VMSMSL - STOP AT LOCATION XX
C-----    VMSMSM - STOP IMMEDIATELY USING MAXIMUM VEHICLE DECELERATION
C-----    VMSMSC - STOP IMMEDIATELY USING COLLISION DECELERATION
          MFSTPF(IV) = .TRUE.
          MFGOF (IV) = .FALSE.
          GACTIM(IV) = 0
          MPRO  (IV) = .FALSE.
          IF      ( IVMSMG(VMSASM(IV)).EQ.VMSMSI)THEN
            IF ( MININT(IV) )                    THEN
              VMSPST(IV) = 1.5D0
            ELSE
              VMSPST(IV) = DBLE( LGEOM(4,IL) ) + 1.5D0
            END IF
          ELSE IF ( IVMSMG(VMSASM(IV)).EQ.VMSMSL)THEN
            VMSPST(IV) = DVMSMP(VMSASM(IV))
          ELSE
            VMSPST(IV) = IPOS(IV) + XRELMI
          END IF
          PVPOS = VMSPST(IV)
          PVVEL = 0.0D0
          PVACC = 0.0D0
          PVSLP = 0.0D0
        ELSE IF ( IVMSMG(VMSASM(IV)).EQ.VMSMGO ) THEN
C-----    VMSMGO - FORCED GO
          MFGOF (IV) = .TRUE.
          MFSTPF(IV) = .FALSE.
          FSTACT(IV) = .FALSE.
          SDWELL(IV) = 0
          MSAOR (IV) = .FALSE.
          LOGTMP     = 1
          LOGFLG(IV) = 1
C-----    SET PVPOS TO END OF LANE/PATH
          IF ( MININT(IV) )                      THEN
            IVPV  = 0
            PVPOS = ENDLN
            PVVEL = LIMP(IP)
            PVACC = 0.0D0
            PVSLP = 0.0D0
          ELSE
            IVPV  = 0
            PVPOS = ENDLN
            PVVEL = ISLIM(IA)
            PVACC = 0.0D0
            PVSLP = 0.0D0
          END IF
C-----    IF THERE IS A VEHICLE AHEAD IN THE SAME LANE THEN SET PVPOS TO
C-----    THAT VEHICLE
          IF ( NOF(IV) . GT . 0 )                THEN
            IVPV  = NOF(IV)
            CALL  SPVAS  ( IVPV,PVPOS,PVVEL,PVACC,PVSLP,
     *                     .TRUE.,.TRUE.,.FALSE.,.TRUE.  )
          END IF
C-----    IF LANE IS BLOCKED AND PVPOS IS BEYOND ENDLN THEN SET MFINL TRUE
          IF ( MBLOCK(IV).AND.(PVPOS.GE.ENDLN) ) THEN
            IVPV = 0
            MFINL(IV) = .TRUE.
          END IF
C-----    IF FIRST IN LANE AND LANE IS BLOCKED THEN SET PVPOS FOR END OF
C-----    BLOCKED LANE
          IF ( MFINL(IV) . AND . MBLOCK(IV) )    THEN
            ENDLN  = DBLE( LGEOM(2,IL) )
            DSPLCH = DBLE( ISPD(IV) )
            IF ( IDISPD(IV) )                    THEN
              DSPLCH = 0.5D0*DSPLCH
            END IF
            IF ( ISPDP (IV) . EQ . 1 )           THEN
              IF ( MININT(IV) )                  THEN
                IF ( LOBL(IP) . GT . 0 )         THEN
                  DSPLCH = DSPLCH*DBLE( ISLIM(ISNA(LOBL(IP))) )
     *                   /        DBLE( LIMP (          IP  ) )
                END IF
              ELSE
                DSPLCH   = DSPLCH*DBLE( ISLIM(          IA  ) )
     *                   /        DBLE( LIMP (LNEXT(    IV )) )
              END IF
            END IF
            VELLCH = 0.2D0*DSPLCH
            VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
            VEHLNG = DMIN1( 25.0D0,LENVAP )
            DISLCH = 0.5D0*(ENDLN-POSNEW)
            DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
            DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
            IF ( MAJRLC(IV) )                    THEN
              DISLCH = 0.5D0*XRELMI
            END IF
            DISEND = DMAX1( DISLCH,DMIN1( 0.5D0*ENDLN,
     *                                    0.5D0*(ENDLN-POSNEW)) )
            IVPV   = 0
            PVPOS  = DMAX1( ENDLN-DISEND,POSNEW )
            PVVEL  = 0.001D0
            PVACC  = -32.2D0
            PVSLP  =   0.0D0
          END IF
        ELSE IF ( IVMSMG(VMSASM(IV)).EQ.VMSMRR ) THEN
C-----    VMSMRR - FORCED RUN THE RED SIGNAL
        ELSE IF ( IVMSMG(VMSASM(IV)).EQ.VMSMDD ) THEN
C-----    VMSMDD - DISTRACTED DRIVER
C-----    SET CANCEL VEHICLE MESSAGE SYSTEM CHANGE LANE MESSAGE
          IF ( VMSACM(IV) . GT . 0 )             FCVMSC = .TRUE.
C-----    SET TO NOT INITIATE CURRENT CHANGE LANE MESSAGE
          FNVMSC = .FALSE.
        END IF
      END IF
C-----PROCESS CANCEL VEHICLE MESSAGE SYSTEM CHANGE LANE MESSAGE
      IF ( FCVMSC )                              THEN
        IF ( VMSACM(IV) . GT . 0 )               THEN
          IF ( ISET(IV) . NE . 1 )               THEN
            IF ( .NOT. MININT(IV) )              THEN
C-----        IF THE VEHICLES REAR BUMPER IS ON THE DIAMOND INTERNAL
C-----        INBOUND LANE THEN ALLOW LANE CHANGES
              IF ( (IATYPE(IA)                   . EQ . DINBDL ) . AND .
     *             (ISET(IV)                     . EQ . 6      ) . AND .
     *             ((POSNEW-DBLE( LGEOM(1,IL) )) . GE . LENVAP ) )
     *                                           THEN
                ISET(IV) = 5
              END IF
C-----        CHECK IF ISET CAN BE SET TO 5
              CALL  CKISET  ( IV,POSNEW )
            END IF
            LEGAL(IV) = 2
            IF ( LALT(IV) . GE . 5 )             CALL  CKLALT
            CALL  PATHF   ( .FALSE.,0,'CVMSGS' )
          END IF
        END IF
        VMSACM(IV) = 0
        VMSACN(IV) = 0
        VMSACT(IV) = 0
      END IF
C-----PROCESS NEW VEHICLE MESSAGE SYSTEM CHANGE LANE MESSAGE
      IF ( FNVMSC )                              THEN
        VMSACN(IV) = CCMVMS
        VMSACT(IV) = 1
        MRT = 0.0D0
        IF      ( CVMSDN(CCMVMS) .EQ. "CONSTAN" )THEN
C-----    PROCESS CONSTANT RANDOM DEVIATES
          CALL  CONST   ( DVMSDM(CCMVMS)                         ,MRT )
        ELSE IF ( CVMSDN(CCMVMS) .EQ. "ERLANG"  )THEN
C-----    PROCESS ERLANG RANDOM DEVIATES
          CALL  ERLANG  ( DVMSDM(CCMVMS),IDNINT( DVMSDP(CCMVMS) ),MRT )
        ELSE IF ( CVMSDN(CCMVMS) .EQ. "GAMMA"   )THEN
C-----    PROCESS GAMMA RANDOM DEVIATES
          CALL  GAMMA   ( DVMSDM(CCMVMS),        DVMSDP(CCMVMS)  ,MRT )
        ELSE IF ( CVMSDN(CCMVMS) .EQ. "LOGNRML" )THEN
C-----    PROCESS LOGNORMAL RANDOM DEVIATES
          CALL  LGNRML  ( DVMSDM(CCMVMS),        DVMSDP(CCMVMS)  ,MRT )
        ELSE IF ( CVMSDN(CCMVMS) .EQ. "NEGEXP"  )THEN
C-----    PROCESS NEGATIVE EXPONENTIAL RANDOM DEVIATES
          CALL  NEGEXP  ( DVMSDM(CCMVMS)                         ,MRT )
        ELSE IF ( CVMSDN(CCMVMS) .EQ. "SNEGEXP" )THEN
C----     PROCESS SHIFTED NEGATIVE EXPONENTIAL RANDOM DEVIATES
          CALL  SNEGEX  ( DVMSDM(CCMVMS),        DVMSDP(CCMVMS)  ,MRT )
        ELSE IF ( CVMSDN(CCMVMS) .EQ. "UNIFORM" )THEN
C-----    PROCESS UNIFORM RANDOM DEVIATES
          CALL  UNIFRM  ( DVMSDM(CCMVMS),        DVMSDP(CCMVMS)  ,MRT )
        END IF
        IF ( ( IVMSMT(I) . EQ . VMSTDD ) . OR .
     *       ( IVMSMT(I) . EQ . VMSTDI ) )       THEN
          MRT = MRT + PIJR(IDRICL(IV)) - APIJR
        END IF
        VMSACT(IV) = MAX0( IDNINT( MRT/DT ),0 ) + 1
C       WRITE (6,602)  VMSACN(IV),IVMSMG(VMSACN(IV)),DVMSMP(VMSACN(IV)),
C    *                 IQ(IV),TIME,VMSACT(IV)
      END IF
C-----DECREMENT MESSAGE REACTION TIME TIMER FOR VEHICLE MESSAGE SYSTEM
C-----CHANGE LANE MESSAGE AND TAKE ACTION IF TIMED OUT
      IF ( VMSACT(IV) . GT . 0 )                 THEN
        VMSACT(IV) = VMSACT(IV) - 1
C       WRITE (6,602)  VMSACN(IV),IVMSMG(VMSACN(IV)),DVMSMP(VMSACN(IV)),
C    *                 IQ(IV),TIME,VMSACT(IV)
        IF ( VMSACT(IV) . EQ . 0 )               THEN
C-----    IF THE VEHICLE IS CURRENTLY CHANGING LANES THEN DELAY THE
C-----    VEHICLE MESSAGE SYSTEM CHANGE LANE MESSAGE ELSE ACTIVATE NOW
C-----    (THERE MAY NEED TO BE A MORE SOPHISTICATED ALGORITHM TO DEAL
C-----     WITH A VEHICLE RECEIVING A VMS CHANGE LANE MESSAGE WHEN THE
C-----     VEHICLE IS ALREADY CHANGING LANES)
          IF ( ISET(IV) . EQ . 1 )               THEN
            VMSACT(IV) = 1
          ELSE
            VMSACM(IV) = VMSACN(IV)
            VMSACN(IV) = 0
            IF ( .NOT. MININT(IV) )              THEN
C-----        IF THE VEHICLES REAR BUMPER IS ON THE DIAMOND INTERNAL
C-----        INBOUND LANE THEN ALLOW LANE CHANGES
              IF ( (IATYPE(IA)                   . EQ . DINBDL ) . AND .
     *             (ISET(IV)                     . EQ . 6      ) . AND .
     *             ((POSNEW-DBLE( LGEOM(1,IL) )) . GE . LENVAP ) )
     *                                           THEN
                ISET(IV) = 5
              END IF
C-----        CHECK IF ISET CAN BE SET TO 5
              CALL  CKISET  ( IV,POSNEW )
            END IF
            IF ( IVMSMG(VMSACM(IV)).EQ.VMSMCL )  THEN
C-----        IF THERE IS A LANE TO THE LEFT AND IF THE VEHICLE IS
C-----        ALLOWED ON THE LANE TO THE LEFT THEN SET LEGAL FOR FORCED
C-----        LANE CHANGE TO THE LEFT
              IF ( NLL(IL) . GT . 0 )            THEN
                IF ( IAND( VEHTYP(IV),LAVT(NLL(IL)) ) . NE . 0 )
     *                                           THEN
                  LEGAL(IV) = 1
                END IF
              END IF
            ELSE
C-----        IF THERE IS A LANE TO THE RIGHT AND IF THE VEHICLE IS
C-----        ALLOWED ON THE LANE TO THE RIGHT THEN SET LEGAL FOR FORCED
C-----        LANE CHANGE TO THE RIGHT
              IF ( NLR(IL) . GT . 0 )            THEN
                IF ( IAND( VEHTYP(IV),LAVT(NLR(IL)) ) . NE . 0 )
     *                                           THEN
                  LEGAL(IV) = 3
                END IF
              END IF
            END IF
            IF ( LALT(IV) . GE . 5 )             CALL  CKLALT
          END IF
        END IF
      END IF
 2010 CONTINUE
C-----PROCESS VEHICLE FORCED TO GO
      IF ( MFGOF(IV) )                           THEN
        IF ( GACTIM(IV) . GT . 0 )               THEN
          GACTIM(IV) = GACTIM(IV) - 1
        END IF
C-----  IF FORCED GO DWELL TIME HAS ENDED THEN REMOVE FORCED GO
        IF ( GACTIM(IV) . EQ . 0 )               THEN
          MFGOF (IV) = .FALSE.
          MFSTPF(IV) = .FALSE.
          FSTACT(IV) = .FALSE.
          SDWELL(IV) = 0
          LOGTMP     = 1
          LOGFLG(IV) = 1
        END IF
      END IF
C-----CHECK IF VEHICLE IS FORCED TO STOP
      IF ( FSTTIM(IV) . GT . 0.0D0 )             THEN
C-----  IF VEHICLE IS FORCED TO STOP THIS DT THEN CHECK MORE CRITERIA
        IF ( (FSTTIM(IV) . GT . (TIME-DT) ) . AND .
     *       (FSTTIM(IV) . LE .  TIME     ) )    THEN
C-----    IF VEHICLE IS IN THE INTERSECTION AND VEHICLE'S PATH IS THE
C-----    INTERSECTION PATH SPECIFIED THEN SET VEHICLE FORCED TO STOP
C-----    AND SET LOCATION TO STOP
          IF ( MININT(IV) )                      THEN
            IF ( LPRES(IV) . EQ . -FSTPIA(IV) )  THEN
              MFSTPF(IV) = .TRUE.
              FSTACT(IV) = .TRUE.
              SDWELL(IV) = 0
              MFGOF (IV) = .FALSE.
              GACTIM(IV) = 0
              PVPOS = FSTPOS(IV)
              PVVEL = 0.0D0
              PVACC = 0.0D0
              PVSLP = 0.0D0
              RETURN
            END IF
          ELSE
C-----    IF VEHICLE IS NOT IN THE INTERSECTION AND VEHICLE'S APPROACH
C-----    IS THE APPROACH SPECIFIED THEN SET VEHICLE FORCED TO STOP AND
C-----    SET LOCATION TO STOP
            IF ( ISNA(LPRES(IV)).EQ.FSTPIA(IV) ) THEN
              MFSTPF(IV) = .TRUE.
              FSTACT(IV) = .TRUE.
              SDWELL(IV) = 0
              MFGOF (IV) = .FALSE.
              GACTIM(IV) = 0
              PVPOS = FSTPOS(IV)
              PVVEL = 0.0D0
              PVACC = 0.0D0
              PVSLP = 0.0D0
              RETURN
            END IF
          END IF
        END IF
      END IF
C-----CHECK IF VEHICLE IS FORCED TO GO
      IF ( FGOTIM(IV) . GT . 0.0D0 )             THEN
C-----  IF VEHICLE IS FORCED TO GO THIS DT THEN SET PARAMETERS
        IF ( (FGOTIM(IV) . GT . (TIME-DT) ) . AND .
     *       (FGOTIM(IV) . LE .  TIME     ) )    THEN
          MFGOF (IV) = .TRUE.
          GACTIM(IV) = IDNINT( FGOATM(IV)/DT )
          MFSTPF(IV) = .FALSE.
          FSTACT(IV) = .FALSE.
          SDWELL(IV) = 0
          MSAOR (IV) = .FALSE.
          LOGTMP     = 1
          LOGFLG(IV) = 1
C-----    SET PVPOS TO END OF LANE/PATH
          IF ( MININT(IV) )                      THEN
            IVPV  = 0
            PVPOS = ENDLN
            PVVEL = LIMP(IP)
            PVACC = 0.0D0
            PVSLP = 0.0D0
          ELSE
            IVPV  = 0
            PVPOS = ENDLN
            PVVEL = ISLIM(IA)
            PVACC = 0.0D0
            PVSLP = 0.0D0
          END IF
C-----    IF THERE IS A VEHICLE AHEAD IN THE SAME LANE THEN SET PVPOS TO
C-----    THAT VEHICLE
          IF ( NOF(IV) . GT . 0 )                THEN
            IVPV  = NOF(IV)
            CALL  SPVAS  ( IVPV,PVPOS,PVVEL,PVACC,PVSLP,
     *                     .TRUE.,.TRUE.,.FALSE.,.TRUE.  )
          END IF
C-----    IF LANE IS BLOCKED AND PVPOS IS BEYOND ENDLN THEN SET MFINL TRUE
          IF ( MBLOCK(IV).AND.(PVPOS.GE.ENDLN) ) THEN
            IVPV = 0
            MFINL(IV) = .TRUE.
          END IF
C-----    IF FIRST IN LANE AND LANE IS BLOCKED THEN SET PVPOS FOR END OF
C-----    BLOCKED LANE
          IF ( MFINL(IV) . AND . MBLOCK(IV) )    THEN
            ENDLN  = DBLE( LGEOM(2,IL) )
            DSPLCH = DBLE( ISPD(IV) )
            IF ( IDISPD(IV) )                    THEN
              DSPLCH = 0.5D0*DSPLCH
            END IF
            IF ( ISPDP (IV) . EQ . 1 )           THEN
              IF ( MININT(IV) )                  THEN
                IF ( LOBL(IP) . GT . 0 )         THEN
                  DSPLCH = DSPLCH*DBLE( ISLIM(ISNA(LOBL(IP))) )
     *                   /        DBLE( LIMP (          IP  ) )
                END IF
              ELSE
                DSPLCH   = DSPLCH*DBLE( ISLIM(          IA  ) )
     *                   /        DBLE( LIMP (LNEXT(    IV )) )
              END IF
            END IF
            VELLCH = 0.2D0*DSPLCH
            VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
            VEHLNG = DMIN1( 25.0D0,LENVAP )
            DISLCH = 0.5D0*(ENDLN-POSNEW)
            DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
            DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
            IF ( MAJRLC(IV) )                    THEN
              DISLCH = 0.5D0*XRELMI
            END IF
            DISEND = DMAX1( DISLCH,DMIN1( 0.5D0*ENDLN,
     *                                    0.5D0*(ENDLN-POSNEW)) )
            IVPV   = 0
            PVPOS  = DMAX1( ENDLN-DISEND,POSNEW )
            PVVEL  = 0.001D0
            PVACC  = -32.2D0
            PVSLP  =   0.0D0
          END IF
        END IF
      END IF
      RETURN
      END                                                               CVMSGS
C
C
C
      SUBROUTINE PREST2
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
C
C-----SUBROUTINE PREST2 COMPUTES NEW ACC/DEC LOGIC
C
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'PREST2'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----SET PARAMETERS FOR NEW ACC/DEC LOGIC
      MOASF(IV) = .FALSE.
                    IF ( PVVEL . LE . 0.0D0 )    MOASF(IV) = .TRUE.
      JPRTM = MAX0( IPRTM(IV)-1,0 )
      IPRTM(IV) = JPRTM
                    IF ( JPRTM . GT . 0 )        RETURN
C-----COMPUTE NEW ACC/DEC LOGIC
      CALL  LOGIC   ( 6,IV )
      RETURN
      END                                                               PREST2
C
C
C
      SUBROUTINE LOGIC  ( ICOM,KV )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHIL'
      LOGICAL           TBLOCK,TFGOF ,TFINL ,TFSTPF,TMLAG ,TOASF ,
     *                  TPOBS ,TPRO  ,TSAOR ,TSFLG ,TSTPF ,TTCARS
      LOGICAL           FBLOCK,FFGOF ,FFINL ,FFSTPF,FMLAG ,FOASF ,
     *                  FPOBS ,FPRO  ,FSAOR ,FSFLG ,FSTPF ,FTCARS
      LOGICAL           TATSTL,TCHKCF,TDEDIC,TINFLZ,TIUNC ,TLRTOR,
     *                  TLSTOP,TLUNC ,TLYELD,TSSGRN,TSSRED
      LOGICAL           FATSTL,FCHKCF,FDEDIC,FINFLZ,FIUNC ,FLRTOR,
     *                  FLSTOP,FLUNC ,FLYELD,FSSGRN,FSSRED
      INTEGER           ICOM,KV
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'LOGIC'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
                    IF ( ICOM . EQ . 8 )         GO TO 1010
                    IF ( ICOM . NE . 6 )         GO TO 9300
C
C-----PROCESS AC/DC LOGIC
C
C-----SET LOCAL LOGICAL VARIABLES TO VALUES FOR VEHICLE KV
      TBLOCK = MBLOCK(KV)
      TFGOF  = MFGOF (KV)
      TFINL  = MFINL (KV)
      TFSTPF = MFSTPF(KV)
      TMLAG  = MLAG  (KV)
      TOASF  = MOASF (KV)
      TPOBS  = MPOBS (KV)
      TPRO   = MPRO  (KV)
      TSAOR  = MSAOR (KV)
      TSFLG  = MSFLG (KV)
      TSTPF  = MSTPF (KV)
      TTCARS = MTCARS(KV)
      FBLOCK = (.NOT. TBLOCK)
      FFGOF  = (.NOT. TFGOF )
      FFINL  = (.NOT. TFINL )
      FFSTPF = (.NOT. TFSTPF)
      FMLAG  = (.NOT. TMLAG )
      FOASF  = (.NOT. TOASF )
      FPOBS  = (.NOT. TPOBS )
      FPRO   = (.NOT. TPRO  )
      FSAOR  = (.NOT. TSAOR )
      FSFLG  = (.NOT. TSFLG )
      FSTPF  = (.NOT. TSTPF )
      FTCARS = (.NOT. TTCARS)
C-----CALCULATE VALUES OF DEPENDENT VARIABLES
      IACDS (KV) = ( FFSTPF .AND. TFGOF                     ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. TSTPF  .AND.
     *               FPOBS  .AND. TFINL  .AND. TSAOR  .AND.
     *               TPRO                                   ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. TSTPF  .AND.
     *               FPOBS  .AND. TFINL  .AND. FSAOR        ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               FMLAG  .AND. TFINL  .AND. FTCARS .AND.
     *               FBLOCK .AND. TPRO                      ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. TSTPF  .AND.
     *               FPOBS  .AND. FFINL  .AND. TOASF  .AND.
     *               FSAOR                                  )
      IACLDS(KV) = ( FFSTPF .AND. FFGOF  .AND. TSTPF  .AND.
     *               FPOBS  .AND. FFINL  .AND. FOASF        )
      ICDFS (KV) = ( TFSTPF .AND. FSAOR  .AND. TSFLG        ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               FMLAG  .AND. TFINL  .AND. TTCARS .AND.
     *               TSFLG                                  ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               FMLAG  .AND. TFINL  .AND. FTCARS .AND.
     *               TBLOCK .AND. TSFLG                     ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               FMLAG  .AND. TFINL  .AND. FTCARS .AND.
     *               FBLOCK .AND. FPRO   .AND. TSFLG        ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               FMLAG  .AND. FFINL  .AND. TOASF  .AND.
     *               TSFLG                                  )
      IFVA  (KV) = ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               TMLAG                                  ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               FMLAG  .AND. FFINL  .AND. FOASF        )
      IRSTOP(KV) = ( FFSTPF .AND. FFGOF  .AND. TSTPF  .AND.
     *               FPOBS  .AND. TFINL  .AND. TSAOR  .AND.
     *               FPRO                                   ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. TSTPF  .AND.
     *               FPOBS  .AND. FFINL  .AND. TOASF  .AND.
     *               TSAOR                                  )
      ISDEC (KV) = ( TFSTPF .AND. FSAOR  .AND. FSFLG        ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               FMLAG  .AND. TFINL  .AND. TTCARS .AND.
     *               FSFLG                                  ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               FMLAG  .AND. TFINL  .AND. FTCARS .AND.
     *               TBLOCK .AND. FSFLG                     ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               FMLAG  .AND. TFINL  .AND. FTCARS .AND.
     *               FBLOCK .AND. FPRO   .AND. FSFLG        ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. FSTPF  .AND.
     *               FMLAG  .AND. FFINL  .AND. TOASF  .AND.
     *               FSFLG                                  )
      ISTMO (KV) = ( TFSTPF .AND. TSAOR                     ) . OR .
     *             ( FFSTPF .AND. FFGOF  .AND. TSTPF  .AND.
     *               TPOBS                                  )
      RETURN
C
C-----PROCESS INTERSECTION LOGIC
C
C-----SET LOCAL LOGICAL VARIABLES TO VALUES FOR VEHICLE KV
 1010 CONTINUE
      TATSTL = MATSTL(KV)
      TCHKCF = MCHKCF(KV)
      TDEDIC = MDEDIC(KV)
      TINFLZ = MINFLZ(KV)
      TIUNC  = MIUNC (KV)
      TLRTOR = MLRTOR(KV)
      TLSTOP = MLSTOP(KV)
      TLUNC  = MLUNC (KV)
      TLYELD = MLYELD(KV)
      TSSGRN = MSSGRN(KV)
      TSSRED = MSSRED(KV)
      FATSTL = (.NOT. TATSTL)
      FCHKCF = (.NOT. TCHKCF)
      FDEDIC = (.NOT. TDEDIC)
      FINFLZ = (.NOT. TINFLZ)
      FIUNC  = (.NOT. TIUNC )
      FLRTOR = (.NOT. TLRTOR)
      FLSTOP = (.NOT. TLSTOP)
      FLUNC  = (.NOT. TLUNC )
      FLYELD = (.NOT. TLYELD)
      FSSGRN = (.NOT. TSSGRN)
      FSSRED = (.NOT. TSSRED)
C-----CALCULATE VALUES OF DEPENDENT VARIABLES
      ICHKCF(KV) = ( TDEDIC .AND. TINFLZ .AND. FLUNC  .AND.
     *               FLYELD .AND. FLSTOP .AND. TSSRED .AND.
     *               TLRTOR                                 ) . OR .
     *             ( TDEDIC .AND. TINFLZ .AND. TLUNC  .AND.
     *               FIUNC  .AND. TCHKCF                    ) . OR .
     *             ( TDEDIC .AND. TINFLZ .AND. FLUNC  .AND.
     *               FLYELD .AND. FLSTOP .AND. FSSRED .AND.
     *               TSSGRN .AND. TCHKCF                    )
      ICONTN(KV) = ( TDEDIC .AND. TINFLZ .AND. FLUNC  .AND.
     *               FLYELD .AND. TLSTOP .AND. FATSTL       ) . OR .
     *             ( TDEDIC .AND. TINFLZ .AND. FLUNC  .AND.
     *               FLYELD .AND. FLSTOP .AND. TSSRED .AND.
     *               FLRTOR                                 ) . OR .
     *             ( TDEDIC .AND. TINFLZ .AND. TLUNC  .AND.
     *               FIUNC  .AND. FCHKCF                    ) . OR .
     *             ( TDEDIC .AND. TINFLZ .AND. FLUNC  .AND.
     *               FLYELD .AND. FLSTOP .AND. FSSRED .AND.
     *               TSSGRN .AND. FCHKCF                    )
      IDEDIC(KV) = ( FDEDIC                                 )
      IERROR(KV) = ( TDEDIC .AND. TINFLZ .AND. FLUNC  .AND.
     *               FLYELD .AND. FLSTOP .AND. FSSRED .AND.
     *               FSSGRN                                 )
      ILSTOP(KV) = ( TDEDIC .AND. TINFLZ .AND. FLUNC  .AND.
     *               FLYELD .AND. TLSTOP .AND. TATSTL       )
      ILUNC (KV) = ( TDEDIC .AND. TINFLZ .AND. TLUNC  .AND.
     *               TIUNC                                  )
      ILYELD(KV) = ( TDEDIC .AND. TINFLZ .AND. FLUNC  .AND.
     *               TLYELD                                 )
      INFLZ (KV) = ( TDEDIC .AND. FINFLZ                    )
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9300 CONTINUE
      CALL  ABORTR  ( 'STOP 930 - ICOM IS NOT 6 OR 8 - LOGIC' )
      STOP  930
      END                                                               LOGIC
C
C
C
      SUBROUTINE UNBIAS
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'LANECH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      DOUBLE PRECISION  CRISLP,MAXDEC,T
C
C-----SUBROUTINE UNBIAS UNBIASES THE VEHICLE ATTRIBUTES AND PREDICTS THE
C-----NEW POS/VEL/ACC
C
C[    T          = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'UNBIAS'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----UNBIAS THE VEHICLE ATTRIBUTES
      CALL  SPVAS  ( IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *               .FALSE.,.FALSE.,.FALSE.,.FALSE. )
      LENVAP = LVAP(IV)
C-----INITIALIZE SEVERAL VEHICLE PARAMETERS
      RELEND = ENDLN - POSOLD
      SLPNEW = SLPOLD
      OLDDTS = IDTS (IV)
      OLDDTI = IDTSI(IV)
      SLPBLK = 0.0D0
      SLPCON = 0.0D0
      SLPLCH = 0.0D0
      SLPNOF = 0.0D0
      ISIDE  = 2
      NOSF   = 0
      NOSFS  = 0
      NOSR   = 0
      NOSRS  = 0
      VVSF   = 0.0D0
      VVSR   = 0.0D0
      ACCNEW = ACCOLD + SLPNEW*DT
      MAXDEC = -IACCSH
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        IF ( IVMSMG(VMSASM(IV)) . EQ . VMSMSC )  THEN
          MAXDEC = DVMSMP(VMSASM(IV))
        END IF
      END IF
      IF ( ACCNEW . LT . MAXDEC )                THEN
        CRISLP = SLPMAX*DCHAR(IDRICL(IV))
        SLPNEW = ((MAXDEC+1.0D-6)-ACCOLD)/DT
        SLPNEW = DMIN1( DMAX1( SLPNEW,-1.6D0*CRISLP ),1.6D0*CRISLP )
      END IF
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER DT SECONDS
      CALL  NEWVEL  ( DT,DTSQ,DTCU )
      CALL  SETDSP  ( IV,POSNEW,DBLE( ISPD(IV) ),.FALSE.,DESVEL )
C-----IF THE VEHICLES VELOCITY IS GT 0 THEN RETURN
                    IF (  VELNEW .GT. VELSTP )   RETURN
C-----IF THE VEHICLE IS ACCELERATING THEN RETURN
                    IF ( (VELNEW .GT. 0.0D0  ) . AND .
     *                   (ACCNEW .GT. 0.0D0  ) . AND .
     *                   (SLPNEW .GT. 0.0D0  ) ) RETURN
C-----THE VEHICLE STOPPED THIS DT THUS CALCULATE THE TIME REQUIRED TO
C-----BRING THE VEHICLE TO A STOP WITHIN THIS DT
      VELOLD = DMAX1( VELOLD,0.0D0 )
      CALL  TIMSTP  ( VELOLD,ACCOLD,SLPNEW,1.05D0*DT,T )
      IF ( T . EQ . TIMERR )                     THEN
        IF ( VELOLD . LE . VSMALL )              THEN
          T = 0.0D0
        ELSE
          T = DT
        END IF
      END IF
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER T SECONDS
C-----(THE VELOCITY SHOULD BE 0)
C[    IF ( T                  .EQ.-2147483647.0 )STOP 'UNBIAS T      01'
      CALL  NEWVEL  ( T,T**2,T**3 )
      VELNEW = 0.0D0
      ACCNEW = 0.0D0
      SLPNEW = 0.0D0
      RETURN
      END                                                               UNBIAS
C
C
C
      SUBROUTINE NEWVEL ( T,TSQ,TCU )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'LANECH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           ACOSET,ERRPRT,SLNSET,VEOSET
      DOUBLE PRECISION  ACNVMS,ACOVMS,CRISLP,DESSPD,DPOS,MAXDEC,SLNVMS,
     *                  SLOVMS,SLPTMP,T,TCU,TSQ,VENVMS,VEOVMS
  601 FORMAT(/,
     *       14H ***** VEHICLE,I6,26H EXCEEDED DECELERATION OF ,F5.1,
     *       19H FT/SEC/SEC AT T = ,F7.2,9H SECONDS,/,
     *       15H       POSOLD =,F7.2,10H  VELOLD =,F7.2,10H  ACCOLD =,
     *       F8.3,10H  SLPOLD =,F8.3,10H  IVEHCL =,I3,10H  IDRICL =,I3/,
     *       15H       POSNEW =,F7.2,10H  VELNEW =,F7.2,10H  ACCNEW =,
     *       F8.3,10H  SLPNEW =,F8.3,9H  IBAPS =,I3)
C
C-----SUBROUTINE NEWVEL CALCULATES THE POS/VEL/ACC FOR THE VEHICLE AFTER
C-----T SECONDS
C
C[    CRISLP     = -2147483647.0
C[    DPOS       = -2147483647.0
C[    SLPTMP     = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'NEWVEL'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----PROCESS VEHICLE MESSAGE SYSTEM SPEED MESSAGE
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        IF ( ( IVMSMG(VMSASM(IV)) .EQ. VMSMAM ) . OR .
     *       ( IVMSMG(VMSASM(IV)) .EQ. VMSMAN ) )THEN
C-----    SET VEHICLE MESSAGE SYSTEM DESIRED SPEED
          DESSPD = DVMSMP(VMSASM(IV))
          DESVEL = DMIN1( DESVEL,DESSPD )
          SLOVMS = SLPOLD
          SLNVMS = SLPNEW
          ACOVMS = ACCOLD
          ACNVMS = ACCNEW
          VEOVMS = VELOLD
          VENVMS = VELNEW
          CALL  ACCEL   ( SLNSET,ACOSET,VEOSET )
          IF ( MSFLG(IV)                . AND .
     *         ( SLNVMS . LE . SLPNEW ) )        THEN
            SLPOLD = SLOVMS
            SLPNEW = SLNVMS
            ACCOLD = ACOVMS
            ACCNEW = ACNVMS
            VELOLD = VEOVMS
            VELNEW = VENVMS
          ELSE
            SLPOLD = SLOVMS
            IF ( (.NOT. SLNSET) )                THEN
              SLPNEW = DMIN1( SLPNEW,SLNVMS )
            END IF
            IF ( (.NOT. ACOSET) )                ACCOLD = ACOVMS
            ACCNEW = ACNVMS
            IF ( (.NOT. VEOSET) )                VELOLD = VEOVMS
            VELNEW = VENVMS
            IF ( MSFLG(IV)                . AND .
     *           ( SLNVMS . NE . SLPNEW ) )      MSFLG(IV) = .FALSE.
          END IF
        END IF
      END IF
C-----IF THE VEHICLE IS FORCED TO GO THEN GO TO 5010 AND DO NOT PROCESS
C-----SLPCON, SLPLCH, SLPBLK, OR SLPNOF
      IF ( MFGOF(IV) )                           GO TO 5010
C-----PROCESS SLOPE FOR AVOIDING CONFLICTS
                    IF ( SLPCON . EQ . 0.0D0 )   GO TO 1010
                    IF ( T . NE . DT )           GO TO 1010
      IF ( ( IVPV   . GT . 0     ) . AND .
     *     ( SLPCON . GT . 0.0D0 ) . AND .
     *     ( SLPNEW . LT . 0.0D0 ) )             THEN
        GO TO 1010
      END IF
C3    KPFLAG = 'SLPCON SET'
      IF ( ( VELOLD . LE . VELSTP ) . AND .
     *     ( SLPCON . LT . 0.0D0  ) )            THEN
        SLPNEW = SLPCON
      ELSE
        SLPNEW = SLPNEW + SLPCON
      END IF
      CRISLP = SLPMAX*DCHAR(IDRICL(IV))
      IF ( SLPNEW . GT . 0.0D0 )                 THEN
        SLPTMP = 2.0D0*(DESVEL-VELOLD-ACCOLD)
        IF ( SLPTMP . LT . 0.0D0 )               THEN
          SLPNEW = SLPTMP
        ELSE
          SLPNEW = DMIN1( SLPNEW,SLPTMP )
        END IF
      END IF
      SLPNEW = DMIN1( DMAX1( SLPNEW,-1.6D0*CRISLP ),1.6D0*CRISLP )
      MSFLG(IV) = .FALSE.
      IPRTM(IV) = 0
      JPRTM = 0
 1010 CONTINUE
C-----PROCESS SLOPE FOR A LANE CHANGE
                    IF ( SLPLCH . EQ . 0.0D0 )   GO TO 3010
      IF ( (VELOLD.EQ.0.0D0) . AND .
     *     (SLPLCH.LT.0.0D0) )                   GO TO 6010
                    IF ( SLPNEW - SLPLCH )       2010 , 3010 , 2020
 2010 CONTINUE
C-----THE ACC/DEC SLOPE FOR A LANE CHANGE IS GT THE SLOPE CALCULATED BY
C-----ACDCP THUS IF LANE CHANGE DECISION FLAG SET FOR SPEED UP THEN SET
C-----SLOW DOWN AND REJECT GAP
      IF ( ISET(IV) . EQ . 3 )                   THEN
        ISET(IV) = 4
      END IF
      GO TO 3010
 2020 CONTINUE
C-----THE ACC/DEC SLOPE FOR A LANE CHANGE IS LT THE SLOPE CALCULATED BY
C-----ACDCP THUS USE THE ACC/DEC SLOPE FOR A LANE CHANGE
C3    KPFLAG = 'SLPLCH MIN'
      SLPNEW = SLPLCH
      MSFLG(IV) = .FALSE.
      IPRTM(IV) = 0
      JPRTM = 0
 3010 CONTINUE
C-----PROCESS SLOPE FOR INTERSECTION BLOCKAGE
                    IF ( SLPBLK . EQ . 0.0D0 )   GO TO 4010
      IF ( ( VELOLD . EQ . 0.0D0 ) . AND .
     *     ( SLPBLK . LT . 0.0D0 ) )             GO TO 6010
                    IF ( SLPBLK . GE . SLPNEW )  GO TO 4010
C-----THE ACC/DEC SLOPE FOR INTERSECTION BLOCKAGE IS LT THE SLOPE
C-----CALCULATED BY ACDCP THUS USE THE ACC/DEC SLOPE FOR INTERSECTION
C-----BLOCKAGE
C3    KPFLAG = 'SLPBLK MIN'
      SLPNEW = SLPBLK
      MSFLG(IV) = .FALSE.
      IPRTM(IV) = 0
      JPRTM = 0
 4010 CONTINUE
C-----PROCESS SLOPE FOR THE NOF VEHICLE
                    IF ( SLPNOF . EQ . 0.0D0 )   GO TO 5010
      IF ( (VELOLD.EQ.0.0D0) . AND .
     *     (SLPNOF.LT.0.0D0) )                   GO TO 6010
                    IF ( SLPNOF . GE . SLPNEW )  GO TO 5010
C-----THE ACC/DEC SLOPE FOR THE NOF VEHICLE IS LT THE SLOPE CALCULATED
C-----BY ACDCP THUS USE THE ACC/DEC SLOPE FOR THE NOF VEHICLE
C3    KPFLAG = 'SLPNOF MIN'
      SLPNEW = SLPNOF
      MSFLG(IV) = .FALSE.
      IPRTM(IV) = 0
      JPRTM = 0
 5010 CONTINUE
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER T SECONDS
      ERRPRT = .FALSE.
 5015 CONTINUE
      ACCNEW = ACCOLD + SLPNEW*T
      VELNEW = VELOLD + ACCOLD*T + 0.5D0*SLPNEW*TSQ
      DPOS   = VELOLD*T + 0.5D0*ACCOLD*TSQ + ONED6*SLPNEW*TCU
      POSNEW = POSOLD + DMAX1( DPOS,0.0D0 )
      MAXDEC = -IACCSH
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        IF ( IVMSMG(VMSASM(IV)) . EQ . VMSMSC )  THEN
          MAXDEC = DVMSMP(VMSASM(IV))
        END IF
      END IF
                    IF ( ACCNEW . GE . MAXDEC  ) GO TO 5020
                    IF ( T . NE . DT )           GO TO 5020
                    IF ( VELNEW . LE . -0.01D0 ) GO TO 5020
                    IF ( ERRPRT )                GO TO 5020
      WRITE (6,601) IQ(IV),MAXDEC,TIME,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *              IVEHCL(IV),IDRICL(IV),POSNEW,VELNEW,ACCNEW,SLPNEW,
     *              IBAPS(IV)
      SLPNEW = ((MAXDEC+1.0D-6)-ACCOLD)/T
      ERRPRT = .TRUE.
      GO TO 5015
 5020 CONTINUE
C-----UPDATE SOME OF THE VEHICLE PARAMETERS
C[    IF ( DPOS               .EQ.-2147483647.0 )STOP 'NEWVEL DPOS   01'
      IDTS(IV) = OLDDTS + DPOS
      IF ( INT1T(IV) . NE . 0 )                  THEN
        IDTSI(IV) = OLDDTI + DPOS
      END IF
      RELVEL = PVVEL - VELNEW
      RELPOS = PVPOS - POSNEW
                    IF ( T . EQ . DT )           RETURN
                    IF ( DPOS . LE . 0.0D0 )     RETURN
      RELEND = RELEND - DPOS
      RETURN
 6010 CONTINUE
C-----THE VEHICLE IS STOPPED AT THE BEGINNING OF THE DT AND THE LANE
C-----CHANGE SLOPE FLAG OR INTERSECTION BLOCKAGE FLAG OR THE NOF VEHICLE
C-----FLAG IS SET TO REMAIN STOPPED THUS REMAIN STOPPED
C3    KPFLAG = 'LCHBLKNOFS'
      SLPNEW = 0.0D0
      ACCNEW = 0.0D0
      VELNEW = 0.0D0
      POSNEW = POSOLD
      MSTPF(IV) = .TRUE.
      MSAOR(IV) = .TRUE.
      MSFLG(IV) = .FALSE.
      IPRTM(IV) = 0
      JPRTM = 0
      RETURN
      END                                                               NEWVEL
C
C
C
      SUBROUTINE LCHGEO
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           CLRCFV
      INTEGER           JV,LWID1,LWID2,WIDV1,WIDV2
      DOUBLE PRECISION  CLRLAT,COSVAL,DISLCH,DSPLCH,DVFACT,PER,PEROP1,
     *                  PEROP2,POSLAT,TLDIST,VELLCH,VEHLNG,XNEW,XOLD,
     *                  XTOT
C
C-----SUBROUTINE LCHGEO COMPUTES THE NEW LATERAL POSITION FOR A LANE
C-----CHANGE USING A COSINE CURVE AND IF FINISHED THEN ENDS THE LANE
C-----CHANGE
C
C[    JV         = -2147483647
C[    DVFACT     = -2147483647.0
C[    POSLAT     = -2147483647.0
C[    TLDIST     = -2147483647.0
C[    VEHLNG     = -2147483647.0
C[    XNEW       = -2147483647.0
C[    XOLD       = -2147483647.0
C[    XTOT       = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'LCHGEO'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
                    IF ( VELOLD . LE . 0.0D0 )   RETURN
C-----FIND THE OLD LATERAL POSITION AND THE TOTAL LATERAL DISTANCE FOR
C-----THE LANE CHANGE
      DVFACT = DCHAR(IDRICL(IV))*VCHAR(IVEHCL(IV))
      POSLAT = LATPOS(IV)
      TLDIST = 0.5D0*LEGAL(IV)
C-----DEFINE THE LENGTH OF THE LANE CHANGE TO BE TIMELC SECONDS AT THE
C-----VELOCITY OF THE VEHICLE WITH A MINIMUM OF 1.5 VEHICLE LENGTHS
C-----WITH A MAXIMUM VEHICLE LENGTH OF 25.0 FEET
      DSPLCH = DBLE( ISPD(IV) )
      IF ( IDISPD(IV) )                          THEN
        DSPLCH = 0.5D0*DSPLCH
      END IF
      IF ( ISPDP (IV) . EQ . 1 )                 THEN
        IF ( MININT(IV) )                        THEN
          IF ( LOBL(IP) . GT . 0 )               THEN
            DSPLCH = DSPLCH*DBLE( ISLIM(ISNA(LOBL(IP))) )
     *             /        DBLE( LIMP (          IP  ) )
          END IF
        ELSE
          DSPLCH   = DSPLCH*DBLE( ISLIM(          IA  ) )
     *             /        DBLE( LIMP (LNEXT(    IV )) )
        END IF
      END IF
      VELLCH = 0.2D0*DSPLCH
      VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
      VEHLNG = DMIN1( 25.0D0,LENVAP )
      DISLCH = 0.5D0*(ENDLN-POSNEW)
      DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
      DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
      IF ( MAJRLC(IV) )                          THEN
        DISLCH = 0.5D0*XRELMI
      END IF
      XTOT   = DISLCH/DVFACT
C-----DEFINE THE PRESENT POSITION ON THE COSINE CURVE
      XOLD = XTOT*DACOS( 2.0D0*DABS( POSLAT )/TLDIST-1.0D0 )/PI
C-----UPDATE THE POSITION OF THE VEHICLE ON THE COSINE CURVE
      XNEW = XOLD + 
     *  DMAX1( VELOLD*DT + 0.5D0*ACCOLD*DTSQ + ONED6*SLPNEW*DTCU,0.0D0 )
                    IF ( XNEW . GE . XTOT )      GO TO 1010
C-----CHECK IF THERE IS A VEHICLE TO THE REAR IN THE OLD LANE
      IF ( IVCNOF(IV) . EQ . 0 )                 THEN
        IF ( IVCNOR(IV) . EQ . 0 )               THEN
          GO TO 1005
        ELSE
          JV = IVCNOR(IV)
        END IF
      ELSE
        IF ( IVCNOR(IV) . EQ . 0 )               THEN
          JV = IVCNOF(IV)
        ELSE
          JV = IVCNOR(IV)
        END IF
      END IF
      CLRCFV = .TRUE.
      PER = XNEW/XTOT
C-----IF THE NEW POSITION OF THE VEHICLE ON THE COSINE CURVE IS LT
C-----60 PERCENT OF THE TOTAL LENGTH OF THE LANE CHANGE THEN DO NOT TURN
C-----OFF LANE CHANGE CAR FOLLOWING FOR THE VEHICLE TO THE REAR IN THE
C-----OLD LANE
      IF ( PER . LT . 0.60D0 )                   THEN
        CLRCFV = .FALSE.
      ELSE
        LWID1 = LWID(IL)
        IF ( POSLAT . LT . 0.0D0 )               THEN
          LWID2 = LWID(NLL(IL))
        ELSE
          LWID2 = LWID(NLR(IL))
        END IF
        WIDV1 = WIDV(IVEHCL(IV))
        WIDV2 = WIDV(IVEHCL(JV))
        COSVAL = (LWID1-LWID2-2.0D0*WIDV1)/(LWID1+LWID2)
        COSVAL = DMAX1(DMIN1(COSVAL,1.0D0),-1.0D0)
        PEROP1 = DACOS(COSVAL)/PI
C-----  IF THE NEW POSITION OF THE VEHICLE ON THE COSINE CURVE IS LT THE
C-----  PERCENT OF THE TOTAL LENGTH OF THE LANE CHANGE REQUIRED TO MAKE
C-----  THE VEHICLE TOTALLY WITHIN THE NEW LANE THEN DO NOT TURN OFF
C-----  LANE CHANGE CAR FOLLOWING FOR THE VEHICLE TO THE REAR IN THE OLD
C-----  LANE
        IF ( PER . LT . PEROP1 )                 THEN
          CLRCFV = .FALSE.
        ELSE
          IF ( IVCNOF(IV) . GT . 0 )             THEN
            JVCNOF(IVCNOF(IV)) = 0
            IVCNOF(IV        ) = 0
          END IF
          CLRLAT = LATCLR*DCHAR(IDRICL(JV))
          COSVAL = (LWID1+LWID2-2.0D0*WIDV1-2.0D0*WIDV2-4.0D0*CLRLAT) /
     *             (LWID1+LWID2)
          COSVAL = DMAX1(DMIN1(COSVAL,1.0D0),-1.0D0)
          PEROP2 = DACOS(COSVAL)/PI
C-----    IF THE NEW POSITION OF THE VEHICLE ON THE COSINE CURVE IS LT
C-----    THE PERCENT OF THE TOTAL LENGTH OF THE LANE CHANGE REQUIRED TO
C-----    HAVE THE REQUIRED CLEARANCE DISTANCE BETWEEN VEHICLES THEN DO
C-----    NOT TURN OFF LANE CHANGE CAR FOLLOWING FOR THE VEHICLE TO THE
C-----    REAR IN THE OLD LANE
          IF ( PER . LT . PEROP2 )               THEN
            CLRCFV = .FALSE.
          END IF
        END IF
      END IF
      IF ( CLRCFV )                              THEN
        IF ( IVCNOR(IV) . GT . 0 )               THEN
          JVCNOR(IVCNOR(IV)) = 0
          IVCNOR(IV) = 0
        END IF
      END IF
 1005 CONTINUE
C-----IF THE NEW POSITION OF THE VEHICLE ON THE COSINE CURVE IS GE
C-----98 PERCENT OF THE TOTAL LENGTH OF THE LANE CHANGE THEN GO TO 1010
C-----AND END THE LANE CHANGE AND RESET THE LANE CHANGE FLAGS
C[    IF ( XNEW               .EQ.-2147483647.0 )STOP 'LCHGEO XNEW   01'
C[    IF ( XTOT               .EQ.-2147483647.0 )STOP 'LCHGEO XTOT   01'
                    IF ( XNEW .GE. 0.98D0*XTOT ) GO TO 1010
C-----FIND THE NEW LATERAL POSITION FOR THE LANE CHANGE
C[    IF ( POSLAT             .EQ.-2147483647.0 )STOP 'LCHGEO POSLAT 01'
C[    IF ( TLDIST             .EQ.-2147483647.0 )STOP 'LCHGEO TLDIST 01'
      POSLAT = SIGN( 0.5D0*TLDIST*(1.0D0+DCOS( PI*XNEW/XTOT )),POSLAT )
C-----BIAS THE NEW LATERAL POSITION FOR THE LANE CHANGE
      LATPOS(IV) = POSLAT
C-----CALCULATE THE LANE CHANGE STEERING ANGLE
      STEERA(IV) = SIGN( DATAN( TLDIST/XTOT )*RAD2DG,-POSLAT )
C-----IF THE NEW LATERAL POSITION FOR THE LANE CHANGE IS GT 0.1 FEET
C-----THEN RETURN ELSE END THE LANE CHANGE AND RESET THE LANE CHANGE
C-----FLAGS
            IF ( DABS( POSLAT ) . GT . 0.1D0 )   RETURN
 1010 CONTINUE
C-----END THE LANE CHANGE AND RESET THE LANE CHANGE FLAGS
      CALL  ENDLCH
      RETURN
      END                                                               LCHGEO
C
C
C
      SUBROUTINE ENDLCH
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'LANE'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INTEGER           LV
C
C-----SUBROUTINE ENDLCH ENDS THE LANE CHANGE AND RESETS THE LANE CHANGE
C-----FLAGS
C
C[    LV         = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'ENDLCH'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----END THE LANE CHANGE
      STEERA(IV) = 0.0D0
      MAJRLC(IV) = .FALSE.
      LEGAL (IV) = 4
      IF ( LNEXT(IV) . GT . 0 )                  THEN
C OLD   CALL  UNSETC
      END IF
      LNEXT (IV) = 0
      ISET  (IV) = 6
C-----IF THE VEHICLES REAR BUMPER IS ON THE DIAMOND INTERNAL INBOUND
C-----LANE THEN ALLOW LANE CHANGES
      IF ( (IATYPE(IA)                   . EQ . DINBDL ) . AND .
     *     (ISET(IV)                     . EQ . 6      ) . AND .
     *     ((POSNEW-DBLE( LGEOM(1,IL) )) . GE . LENVAP ) )
     *                                           THEN
        ISET(IV) = 5
      END IF
C-----CHECK IF ISET CAN BE SET TO 5
      CALL  CKISET  ( IV,POSNEW )
      LATPOS(IV) = 0.0D0
      LCHGE (IV) = 1
      LV = IVCNOF(IV)
      IF ( LV . GT . 0 )                         THEN
        JVCNOF(LV) = 0
      END IF
      IVCNOF(IV) = 0
      LV = IVCNOR(IV)
      IF ( LV . GT . 0 )                         THEN
        JVCNOR(LV) = 0
      END IF
      IVCNOR(IV) = 0
                    IF ( NOF(IV) . EQ . 0 )      GO TO 1010
C-----RESET THE LANE CHANGE FLAGS FOR THIS VEHICLE
                    IF ( ISET(NOF(IV)) . EQ . 1 )LCHGE(IV) = 3
 1010 CONTINUE
                    IF ( NOR(IV) . EQ . 0 )      RETURN
C-----RESET THE LANE CHANGE FLAG FOR THE NORT VEHICLE
                    IF ( LCHGE(NOR(IV)) .EQ. 3 ) LCHGE(NOR(IV)) = 1
      RETURN
      END                                                               ENDLCH
C
C
C
      SUBROUTINE LCHDES ( SLOWV,ANYLAN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           ANYLAN,EVRESP,IBLOCK,LBLOCK,LBVSTP,LCHKCF,
     *                  MAJCBL,SLOWV
      INTEGER           IFLCHG,ILL,ILR,JSET,KV,LANSI,LOK,MEGAL,NOQ,
     *                  NOSFL,NOSFSL,NOSFR,NOSFSR,NOSRL,NOSRSL,NOSRR,
     *                  NOSRSR
      DOUBLE PRECISION  ALEGAP,AVSFL,AVSFR,AVSRL,AVSRR,CARDIS,CRISLP,
     *                  DISLCH,FACT,GAPMIN,POSLAT,POSMJC,PREVM,PREVS,
     *                  PVSFL,PVSFR,PVSRL,PVSRR,RELDIS,RELSCD,RELSPD,
     *                  SLPTMP,SVSFL,SVSFR,SVSRL,SVSRR,VCRIT,VEHLNG,
     *                  VELLCH,VELMIN,VVSFL,VVSFR,VVSRL,VVSRR
C;    DOUBLE PRECISION  ALEGAS
      DATA     GAPMIN / 8.0D0 /
      DATA     VELMIN / 3.0D0 /
C
C-----SUBROUTINE LCHDES DETERMINES IF A LANE CHANGE IS DESIRABLE
C-----IF SLOWV  IS TRUE  THEN CHECK FOR A LANE CHANGE FROM BEHIND A SLOW
C-----                        VEHICLE
C-----IF SLOWV  IS FALSE THEN CHECK FOR A LANE CHANGE CLOSE TO THE
C-----                        INTERSECTION
C-----IF ANYLAN IS TRUE  THEN ANY LANE CAN BE USED FOR A LANE CHANGE
C-----                        FROM BEHIND A SLOW VEHICE
C-----IF ANYLAN IS FALSE THEN IF FORCED LANE CHANGE TO THE LEFT THEN DO
C-----                        NOT CHECK THE LANE TO THE RIGHT AND
C-----                        IF FORCED LANE CHANGE TO THE RIGHT THEN DO
C-----                        NOT CHECK THE LANE TO THE LEFT
C
C[    IFLCHG     = -2147483647
C[    LANSI      = -2147483647
C[    LOK        = -2147483647
C[    NOQ        = -2147483647
C[    ALEGAP     = -2147483647.0
C[    CARDIS     = -2147483647.0
C[    CRISLP     = -2147483647.0
C[    FACT       = -2147483647.0
C[    PREVM      = -2147483647.0
C[    PREVS      = -2147483647.0
C[    RELDIS     = -2147483647.0
C[    RELSCD     = -2147483647.0
C[    RELSPD     = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'LCHDES'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----IF VMS DISTRACTED DRIVER MESSAGE IS ACTIVE THEN RETURN AND DO NOT
C-----CONSIDER A LANE CHANGE
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        IF ( IVMSMG(VMSASM(IV)) . EQ . VMSMDD )  THEN
          RETURN
        END IF
      END IF
C-----IF LEGAL HAS NOT BEEN SET THEN FIND THE INTERSECTION PATH FOR THIS
C-----THIS VEHICLE BASED ON THE CURRENT APPROACH, CURRENT LANE, AND THE
C-----DESIRED OUTBOUND APPROACH
      IF ( LEGAL(IV) . GE . 4 )                  THEN
        CALL  PATHF   ( .FALSE.,0,'LCHDES' )
      END IF
      LBLOCK = .FALSE.
C-----IF TRYING TO REVERSE LANE CHANGE THEN SET PARAMETERS
      IF ( MAJCLC )                              THEN
        MEGAL = LEGAL(IV)
        IF ( LATPOS(IV) . GT . 0.0D0 )           THEN
C-----    THE VEHICLE IS CHANGING LANES TO THE LEFT (THE VEHICLE IS
C-----    ALLOWED ON THE LANE TO THE RIGHT BECAUSE IT JUST LANE CHANGED
C-----    FROM THERE)
          LANSI = NLR(IL)
          ISIDE = 3
        ELSE
C-----    THE VEHICLE IS CHANGING LANES TO THE RIGHT (THE VEHICLE IS
C-----    ALLOWED ON THE LANE TO THE LEFT BECAUSE IT JUST LANE CHANGED
C-----    FROM THERE)
          LANSI = NLL(IL)
          ISIDE = 1
        END IF
        LEGAL(IV) = ISIDE
C-----  CHECK THE LANE ON THE SIDE OF INTEREST TO SEE IF THE LANE IS
C-----  AVAILABLE AT THE CURRENT POSITION OF THE VEHICLE AND CLEAR TO
C-----  THE INTERSECTION
        CALL  CHKLSI  ( LANSI,POSNEW,LEGAL(IV),LOK )
        GO TO 1015
      END IF
C-----IF SLOWV IS TRUE THEN CHECK FOR A LANE CHANGE FROM BEHIND A SLOW
C-----VEHICE ELSE CHECK FOR A LANE CHANGE CLOSE TO THE INTERSECTION
                    IF ( SLOWV )                 GO TO 7010
C-----CHECK THE DESIRABILITY OF THE LANE CHANGE BASED ON LEGAL
C-----        FL   OPT  FR   ERR  ERR
      GO TO ( 1010,2010,1010,9030,9040 ) , LEGAL(IV)
 1010 CONTINUE
C-----THE TURN IS LEGAL FROM THE APPROACH BUT NOT FROM THIS LANE THUS
C-----SET WHICH SIDE THE VEHICLE SHOULD CHANGE TO
      ISIDE = LEGAL(IV)
C-----SET THE INDEX FOR THE LANE ON THE SIDE OF INTEREST TO CHECK
      LANSI = NLL(IL)
                    IF ( ISIDE . EQ . 3 )        LANSI = NLR(IL)
                    IF ( LANSI . EQ . 0 )        GO TO 9050
C-----CHECK THE LANE ON THE SIDE OF INTEREST TO SEE IF THE LANE IS
C-----AVAILABLE AT THE CURRENT POSITION OF THE VEHICLE AND CLEAR TO THE
C-----INTERSECTION
      CALL  CHKLSI  ( LANSI,POSNEW,LEGAL(IV),LOK )
      IF ( ( SMJCOL           ) . AND .
     *     ( .NOT. MAJCLB(IV) ) )                THEN
C-----  THERE IS A MAJOR COLLISION AND THIS VEHICLE IS NOT BLOCKED BY A
C-----  MAJOR COLLISION
        IF ( LALT(IV) . GE . 5 )                 CALL  CKLALT
        IF ( ISIDE . EQ . 1 )                    THEN
C-----    THE FORCED LANE CHANGE IS TO THE LEFT
          CALL  CLMJCL  ( NLL(IL),LALTPL(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
          IF ( KV . GT . 0 )                     THEN
C-----      THE LANE TO THE LEFT IS BLOCKED BY A MAJOR COLLISION THUS
C-----      SET LANE BLOCKED
            LBLOCK = .TRUE.
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----      FALSE
            EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *                 ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
            RESPEV = ( RESPEV . OR . EVRESP )
          ELSE
            IF ( LALTPL(IV) . GT . 0 )           THEN
              CALL  CHKINT  ( LALTPL(IV),.FALSE.,.FALSE.,IBLOCK,MAJCBL,
     *                        POSMJC                                   )
              IF ( IBLOCK . AND . MAJCBL )       THEN
C-----          THE LANE TO THE LEFT IS NOT BLOCKED BY A MAJOR
C-----          COLLISION BUT THE INTERSECTION PATH FROM THE LANE TO
C-----          THE LEFT TO THE DESIRED OUTBOUND APPROACH IS BLOCKED BY
C-----          A MAJOR COLLISION THUS SET LANE BLOCKED
                LBLOCK = .TRUE.
              END IF
            ELSE
C-----        THE LANE TO THE LEFT IS NOT BLOCKED BY A MAJOR COLLISION
C-----        AND THERE IS NO INTERSECTION PATH FROM THE LANE TO THE
C-----        LEFT TO THE DESIRED OUTBOUND APPROACH THUS SET LANE
C-----        BLOCKED
              POSMJC = POSMAX
              LBLOCK = .TRUE.
            END IF
          END IF
        ELSE
C-----    THE FORCED LANE CHANGE IS TO THE RIGHT
          CALL  CLMJCL  ( NLR(IL),LALTPR(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
          IF ( KV . GT . 0 )                     THEN
C-----      THE LANE TO THE RIGHT IS BLOCKED BY A MAJOR COLLISION THUS
C-----      SET LANE BLOCKED
            LBLOCK = .TRUE.
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----      FALSE
            EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *                 ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
            RESPEV = ( RESPEV . OR . EVRESP )
          ELSE
            IF ( LALTPR(IV) . GT . 0 )           THEN
              CALL  CHKINT  ( LALTPR(IV),.FALSE.,.FALSE.,IBLOCK,MAJCBL,
     *                        POSMJC                                   )
              IF ( IBLOCK . AND . MAJCBL )       THEN
C-----          THE LANE TO THE RIGHT IS NOT BLOCKED BY A MAJOR
C-----          COLLISION  BUT THE INTERSECTION PATH FROM THE LANE TO
C-----          THE RIGHT TO THE DESIRED OUTBOUND APPROACH IS BLOCKED BY
C-----          A MAJOR COLLISION THUS SET LANE BLOCKED
                LBLOCK = .TRUE.
              END IF
            ELSE
C-----        THE LANE TO THE RIGHT IS NOT BLOCKED BY A MAJOR COLLISION
C-----        AND THERE IS NO INTERSECTION PATH FROM THE LANE TO THE
C-----        RIGHT TO THE DESIRED OUTBOUND APPROACH THUS SET LANE
C-----        BLOCKED
              POSMJC = POSMAX
              LBLOCK = .TRUE.
            END IF
          END IF
        END IF
        IF ( LBLOCK )                            THEN
C-----    THE LANE ON THE SIDE OF INTEREST IS BLOCKED BY A MAJOR
C-----    COLLISION THUS IF THE VEHICLE IS PAST THE END OF LANE ON SIDE
C-----    OF INTEREST THEN GO TO 5010 AND TAKE THE FORCED PATH FOR
C-----    CURRENT LANE ELSE SET THAT THE VEHICLE IS BLOCKED BY A MAJOR
C-----    COLLISION NOT ON ITS DIRECT PATH
                    IF ( LOK . EQ . 2 )          GO TO 5010
          POSMJC     = DBLE( LGEOM(4,IL) ) + 1.5D0
          MAJCLL(IV) = .TRUE.
          POSCLL(IV) = DMIN1( POSCLL(IV),POSMJC )
          GO TO 6010
        END IF
      END IF
 1015 CONTINUE
C-----FIND THE NEAREST VEHICLE TO THE FRONT AND THE NEAREST VEHICLE TO
C-----THE REAR IN THE LANE ON THE SIDE OF INTEREST FOR THIS VEHICLE
      CALL  SVEHU   ( NOQ )
C-----IF THE LANE ON THE SIDE OF INTEREST IS BLOCKED FOR THIS VEHICLE
C-----THEN CAR-FOLLOW THE NOF VEHICLE IN THAT LANE ELSE CHECK IF THERE
C-----IS AN ACCEPTABLE GAP TO LANE CHANGE INTO
C[    IF ( LOK                .EQ.-2147483647   )STOP 'LCHDES LOK    01'
                    IF ( LOK . NE . 0 )          GO TO 4010
      GO TO 2020
 2010 CONTINUE
C-----THE TURN IS LEGAL FROM THIS LANE BUT IF THE VEHICLE IS NOT
C-----DEDICATED TO AN INTERSECTION PATH THEN RETURN AND WAIT UNTIL THE
C-----VEHICLE IS DEDICATED TO AN INTERSECTION PATH
                    IF ( LNEXT(IV) . EQ . 0 )    GO TO 6010
C-----IF THERE ARE NO LANE ALTERNATIVES THEN RETURN AND DO NOT CHECK THE
C-----THE DESIRABILITY OF A LANE CHANGE ANY MORE
      IF ( LALT(IV) . EQ . 1 )                   THEN
        ISET(IV) = 6
        GO TO 6010
      END IF
C-----FIND THE LEGAL LANE FOR THE VEHICLE WITH THE MINIMUM EXPECTED
C-----DELAY
      CALL  DELAY
C-----IF THE VEHICLE SHOULD STAY IN THIS LANE THEN RETURN
                    IF ( ISIDE . EQ . 2 )        GO TO 6010
      LANSI = NLL(IL)
                    IF ( ISIDE . EQ . 3 )        LANSI = NLR(IL)
                    IF ( LANSI . EQ . 0 )        GO TO 9050
 2020 CONTINUE
C-----CHECK IF THERE IS AN ACCEPTABLE GAP TO LANE CHANGE INTO AND IF NOT
C-----THEN DETERMINE THE APPROPRIATE DRIVER RESPONSE FOR LANE CHANGING
C[    IF ( LANSI              .EQ.-2147483647   )STOP 'LCHDES LANSI  01'
      CALL  GAPACC  ( LANSI,IFLCHG )
C[    IF ( IFLCHG             .EQ.-2147483647   )STOP 'LCHDES IFLCHG 01'
                    IF ( IFLCHG . EQ . 1 )       GO TO 4010
C-----IF THERE IS AN ACCEPTABLE GAP THEN GO TO 3010 AND LOG THE VEHICLE
C-----OUT OF ITS PRESENT LANE AND INTO THE NEW LANE ELSE RESET THE LANE
C-----CHANGE FLAG AND RETURN
                    IF ( ISET(IV) . EQ . 1 )     GO TO 3010
      ISIDE = 2
      GO TO 6010
 3010 CONTINUE
C-----THERE IS AN ACCEPTABLE GAP SO LOG THE VEHICLE OUT OF HIS PRESENT
C-----LANE AND INTO THE NEW LANE
      IF ( MAJCLC )                              THEN
        POSLAT = DABS( LATPOS(IV) )
        CALL ENDLCH
        ISET(IV) = 1
      END IF
      CALL  CHGMLN
      IF ( MAJCLC )                              THEN
        MAJRLC(IV) = .TRUE.
        POSLAT = 0.5D0*DBLE( LEGAL(IV) ) - POSLAT
                    IF ( ISIDE . EQ . 3 )        POSLAT = -POSLAT
        LATPOS(IV) = POSLAT
      END IF
      RETURN
 4010 CONTINUE
C[    IF ( LOK                .EQ.-2147483647   )STOP 'LCHDES LOK    02'
                    IF ( LOK . EQ . 2 )          GO TO 5010
C-----IF THERE IS NO VEHICLE IN THE LANE ON THE SIDE OF INTEREST THEN
C-----STOP AT THE STOP LINE FOR THE LANE ON THE SIDE OF INTEREST
      CRISLP = SLPMAX*DCHAR(IDRICL(IV))
      RELSPD = VVSF - VELNEW
      PREVS  = VVSF   + AVSF  *DT + 0.5D0*SVSF        *DTSQ
      PREVM  = VELNEW + ACCNEW*DT + 0.5D0*1.3D0*CRISLP*DTSQ
      RELSCD = DMIN1( PREVS-PREVM,RELSPD )
                    IF ( NOSF . EQ . 0 )         GO TO 4020
C-----THERE IS A VEHICLE IN THE LANE ON THE SIDE OF INTEREST
      RELDIS = DMAX1( PVSF-POSNEW,0.01D0 )
      IF ( RELSPD . LT . 0.0D0 )                 THEN
        CARDIS = DMAX1( XRELMX,1.7D0*VVSF+4.0D0*RELSCD**2 )
     *           / DCHAR(IDRICL(IV))
      ELSE
        CARDIS = DMAX1( XRELMX,1.7D0*VVSF )
     *           / DCHAR(IDRICL(IV))
      END IF
      FACT = FACTOR*DCHAR(IDRICL(IV))*VCHAR(IVEHCL(IV))
      ALEGAP = (2.0D0+0.7D0*VELNEW-(DABS(RELSPD)*RELSPD*0.05D0))/FACT
C;    ALEGAS = ALEGAP
      ALEGAP = DMAX1( ALEGAP,GAPMIN/DCHAR(IDRICL(IV)) )
C;    IF ( (IAND( IPRTLO(IV),1 ).NE.0). AND . (TIME.GE.TPRINT) )
C;   *WRITE (6,601) NOSF,AVSF,VVSF,PVSF,ACCNEW,VELNEW,POSNEW,RELSPD,
C;   *              RELDIS,CARDIS,ALEGAS,ALEGAP
C;601 FORMAT(' ',I3,' AS=',F7.3,' VS=',F6.3,' PS=',F7.3,
C;   *              ' AN=',F7.3,' VN=',F6.3,' PN=',F7.3,
C;   *              ' RV=',F7.3,' RP=',F7.3,' CD=',F9.3,
C;   *              ' ALGP=',F7.3,' ALGPMX=',F7.3)
C[    IF ( CARDIS             .EQ.-2147483647.0 )STOP 'LCHDES CARDIS 01'
      CARDIS = DMAX1( CARDIS,ALEGAP )
      IF ( VVSF . LE . 0.0D0 )                   THEN
        IF ( VELNEW . LE . 0.0D0 )               THEN
          IF ( RELDIS . GT . XRELMX )            THEN
C-----NOSF VEL LE 0 & VEH VEL LE 0 & RELDIS GT XRELMX THUS MOVE UP
            GO TO 6010
          ELSE
C-----NOSF VEL LE 0 & VEH VEL LE 0 & RELDIS LE XRELMX THUS STAY STOPPED
            SLPLCH = -1.0D0
            GO TO 6010
          END IF
        END IF
C-----NOSF VEL LE 0 & VEH VEL GT 0                    THUS STOP NOSF RB
        GO TO 4020
      ELSE
        IF ( VELNEW . LE . 0.0D0 )               THEN
          IF ( ( RELDIS . GT . 1.2D0*ALEGAP ) . AND .
     *         ( VVSF   . GT . VELMIN       ) )  THEN
C-----NOSF VEL GT 0 & VEH VEL LE 0 & RELDIS GT ALEGAP & VVSF GT VELMIN
C-----THUS ACCELERATE
            GO TO 6010
          ELSE
C-----NOSF VEL GT 0 & VEH VEL LE 0 & RELDIS LE ALEGAP OR
C-----NOSF VEL GT 0 & VEH VEL LE 0 & VVSF LE VELMIN THUS WAIT ALEGAP
C-----AND VELMIN
            SLPLCH = -1.0D0
            GO TO 6010
          END IF
        END IF
      END IF
C-----NOSF VEL GT 0 & VEH VEL GT 0 THUS IF RELDIS GT CARDIS THEN ACCEL
C[    IF ( CARDIS             .EQ.-2147483647.0 )STOP 'LCHDES CARDIS 02'
                    IF ( RELDIS . GT . CARDIS )  GO TO 6010
C-----NOSF VEL GT 0 & VEH VEL GT 0 & RELDIS LE CARDIS THUS
C-----IF RELDIS GT ALEGAP AND RELSPD GT VELMIN THEN ACCELERATE
C[    IF ( RELSPD             .EQ.-2147483647.0 )STOP 'LCHDES RELSPD 01'
      IF ( ( RELDIS . GT . 1.2D0*ALEGAP ) . AND .
     *     ( RELSPD . GT . VELMIN       ) )      GO TO 6010
C-----NOSF VEL GT 0 & VEH VEL GT 0 & RELDIS LE CARDIS THUS CAR FOLLOW
C-----FIND THE ACC/DEC SLOPE TO CAR-FOLLOW THE NOF VEHICLE IN THE LANE
C-----ON THE SIDE OF INTEREST UNTIL THE LANE IS NO LONGER BLOCKED FOR
C-----THIS VEHICLE
      FACT = FACTOR*DCHAR(IDRICL(IV))*VCHAR(IVEHCL(IV))
C[    IF ( RELSCD             .EQ.-2147483647.0 )STOP 'LCHDES RELSCD 01'
      ALEGAP = (2.0D0+0.7D0*VELNEW-(DABS(RELSCD)*RELSCD*0.05D0))/FACT
      ALEGAP = DMAX1( ALEGAP,GAPMIN/DCHAR(IDRICL(IV)) )
      PVSF = PVSF - 1.3D0*ALEGAP
      CALL  SLPCFS  ( SLPTMP,IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                          PVSF  ,VVSF  ,AVSF  ,SVSF    )
      IF ( SLPTMP . NE . 0.0D0 )                 THEN
        IF ( SLPTMP . LT . SLPLCH )              THEN
          SLPLCH = SLPTMP
          IF ( NOSF . GT . 0 )                   THEN
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE NOSF
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----      FALSE
            EVRESP = ( ( IAND( VEHTYP(IV  ),LAVTE ) . EQ . 0 ) . AND .
     *                 ( IAND( VEHTYP(NOSF),LAVTE ) . NE . 0 ) )
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
        END IF
      END IF
      GO TO 6010
 4020 CONTINUE
C-----FIND THE ACC/DEC SLOPE TO STOP AT THE END OF LANE ON THE SIDE OF
C-----INTEREST OR BEHIND THE REAR OF THE NOSF VEHICLE
      IF ( NOSF . EQ . 0 )                       THEN
        VVSF = 0.0D0
      ELSE
        FACT = FACTOR*DCHAR(IDRICL(IV))*VCHAR(IVEHCL(IV))
C[      IF ( RELSCD           .EQ.-2147483647.0 )STOP 'LCHDES RELSCD 02'
        ALEGAP = (2.0D0+0.7D0*VELNEW-(DABS(RELSCD)*RELSCD*0.05D0))/FACT
        ALEGAP = DMAX1( ALEGAP,GAPMIN/DCHAR(IDRICL(IV)) )
        PVSF = PVSF - 1.3D0*ALEGAP
      END IF
C-----FIND THE MAXIMUM DECELERATION RATE THAT THE DRIVER WOULD USE TO
C-----STOP FROM HIS OLD VELOCITY USING LINEAR DECELERATION AND BOUND
C-----IT WITH THE MAXIMUM DECELERATION RATE FOR THE VEHICLE
      CALL  SLPCFS  ( SLPTMP,IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                          PVSF  ,VVSF  ,AVSF  ,SVSF    )
      IF ( SLPTMP . NE . 0.0D0 )                 THEN
        IF ( SLPTMP . LT . SLPLCH )              THEN
          SLPLCH = SLPTMP
          IF ( NOSF . GT . 0 )                   THEN
C-----      IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE NOSF
C-----      VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----      FALSE
            EVRESP = ( ( IAND( VEHTYP(IV  ),LAVTE ) . EQ . 0 ) . AND .
     *                 ( IAND( VEHTYP(NOSF),LAVTE ) . NE . 0 ) )
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
        END IF
      END IF
      GO TO 6010
 5010 CONTINUE
C-----VEHICLE IS PAST THE END OF LANE ON SIDE OF INTEREST SO TAKE FORCED
C-----PATH FOR CURRENT LANE
      LEGAL(IV) = 2
      ISET (IV) = 6
C-----IF THE VEHICLES REAR BUMPER IS ON THE DIAMOND INTERNAL INBOUND
C-----LANE THEN ALLOW LANE CHANGES
      IF ( (IATYPE(IA)                   . EQ . DINBDL ) . AND .
     *     ((POSNEW-DBLE( LGEOM(1,IL) )) . GE . LENVAP ) )
     *                                           THEN
        ISET(IV) = 5
      END IF
C-----CHECK IF ISET CAN BE SET TO 5
      CALL  CKISET  ( IV,POSNEW )
      CALL  PATHF   ( .TRUE.,0,'LCHDES' )
 6010 CONTINUE
      IF ( MAJCLC )                              THEN
        LEGAL(IV) = MEGAL
      END IF
      RETURN
 7010 CONTINUE
C-----CHECK FOR A LANE CHANGE FROM BEHIND A SLOW VEHICE
C-----IF THE CURRENT VEHICLE IS CURRENTLY CHANGING LANES THEN RETURN
                    IF ( ISET(IV) . EQ . 1      )RETURN
C-----IF THERE IS NO PREVIOUS VEHICLE THEN RETURN
                    IF ( IVPV     . EQ . 0      )RETURN
C-----IF THE CURRENT VEHICLE IS ACCELERATING THEN RETURN
                    IF ( ACCNEW   . GT . 0.0D0  )RETURN
C-----IF THE SPEED OF THE PREVIOUS VEHICLE IS ABOVE THE DESIRED SPEED OF
C-----THE CURRENT VEHICLE THEN RETURN
                    IF ( PVVEL    . GE . DESVEL )RETURN
C-----IF EMERGENCY VEHICLE THEN USE DESVEL FOR VCRIT ELSE SET VCRIT
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   THEN
        VCRIT = DESVEL
      ELSE
        VCRIT = DMIN1( LCSPER*DCHAR(IDRICL(IV)),1.0D0 )*DESVEL
      END IF
C-----IF THE SPEED OF THE CURRENT VEHICLE IS NEAR ITS DESIRED SPEED THEN
C-----RETURN
                    IF ( VELNEW   . GE . VCRIT  )RETURN
      ILL = NLL(IL)
      ILR = NLR(IL)
C-----IF THE VEHICLE IS NOT ALLOWED ON THE LANE TO THE LEFT THEN DO NOT
C-----CHECK THE LANE TO THE LEFT
      IF ( ILL . GT . 0 )                        THEN
        IF ( IAND( VEHTYP(IV),LAVT(ILL) ).EQ.0 ) ILL = 0
      END IF
C-----IF THE VEHICLE IS NOT ALLOWED ON THE LANE TO THE RIGHT THEN DO NOT
C-----CHECK THE LANE TO THE RIGHT
      IF ( ILR . GT . 0 )                        THEN
        IF ( IAND( VEHTYP(IV),LAVT(ILR) ).EQ.0 ) ILR = 0
      END IF
C-----IF ANY LANE CAN NOT BE USED THEN CHECK LEGAL
      IF ( .NOT. ANYLAN )                        THEN
C-----  IF FORCED LANE CHANGE TO THE LEFT THEN DO NOT CHECK THE LANE TO
C-----  THE RIGHT
        IF ( LEGAL(IV) . EQ . 1 )                ILR = 0
C-----  IF FORCED LANE CHANGE TO THE RIGHT THEN DO NOT CHECK THE LANE TO
C-----  THE LEFT
        IF ( LEGAL(IV) . EQ . 3 )                ILL = 0
      END IF
C-----IF LANE TO THE LEFT AND TO THE RIGHT ARE NOT AVAILABLE THEN RETURN
      IF ( ( ILL.EQ.0 ) . AND . ( ILR.EQ.0 ) )   RETURN
      MEGAL  = LEGAL(IV)
      JSET   = ISET (IV)
      NOSFL  = 0
      NOSFSL = 0
      NOSRL  = 0
      NOSRSL = 0
      PVSFL  = 0.0D0
      PVSRL  = 0.0D0
      VVSFL  = 0.0D0
      VVSRL  = 0.0D0
      AVSFL  = 0.0D0
      AVSRL  = 0.0D0
      SVSFL  = 0.0D0
      SVSRL  = 0.0D0
      NOSFR  = 0
      NOSFSR = 0
      NOSRR  = 0
      NOSRSR = 0
      PVSFR  = 0.0D0
      PVSRR  = 0.0D0
      VVSFR  = 0.0D0
      VVSRR  = 0.0D0
      AVSFR  = 0.0D0
      AVSRR  = 0.0D0
      SVSFR  = 0.0D0
      SVSRR  = 0.0D0
      IF ( ILL . GT . 0 )                        THEN
C-----  CHECK LANE TO THE LEFT
        ISIDE = 1
        LANSI = ILL
C-----  CHECK THE LANE ON THE SIDE OF INTEREST TO SEE IF THE LANE IS
C-----  AVAILABLE AT THE CURRENT POSITION OF THE VEHICLE AND CLEAR TO
C-----  THE INTERSECTION
        CALL  CHKLSI  ( LANSI,POSNEW,1,LOK )
C-----  IF THE LANE IS AVAILABLE THEN CONTINUE CHECKING ELSE DO NOT
C-----  CHECK LEFT
        IF ( LOK . EQ . 0 )                      THEN
C-----    FIND THE NEAREST VEHICLE TO THE FRONT AND THE NEAREST VEHICLE
C-----    TO THE REAR IN THE LANE ON THE SIDE OF INTEREST FOR THIS
C-----    VEHICLE
          CALL  SVEHU   ( NOQ )
          NOSFL  = NOSF
          NOSFSL = NOSFS
          NOSRL  = NOSR
          NOSRSL = NOSRS
          PVSFL  = PVSF
          PVSRL  = PVSR
          VVSFL  = VVSF
          VVSRL  = VVSR
          AVSFL  = AVSF
          AVSRL  = AVSR
          SVSFL  = SVSF
          SVSRL  = SVSR
        ELSE
          ILL = 0
        END IF
      END IF
      IF ( ILR . GT . 0 )                        THEN
C-----  CHECK LANE TO THE RIGHT
        ISIDE = 3
        LANSI = ILR
C-----  CHECK THE LANE ON THE SIDE OF INTEREST TO SEE IF THE LANE IS
C-----  AVAILABLE AT THE CURRENT POSITION OF THE VEHICLE AND CLEAR TO
C-----  THE INTERSECTION
        CALL  CHKLSI  ( LANSI,POSNEW,3,LOK )
C-----  IF THE LANE IS AVAILABLE THEN CONTINUE CHECKING ELSE DO NOT
C-----  CHECK RIGHT
        IF ( LOK . EQ . 0 )                      THEN
C-----    FIND THE NEAREST VEHICLE TO THE FRONT AND THE NEAREST VEHICLE
C-----    TO THE REAR IN THE LANE ON THE SIDE OF INTEREST FOR THIS
C-----    VEHICLE
          CALL  SVEHU   ( NOQ )
          NOSFR  = NOSF
          NOSFSR = NOSFS
          NOSRR  = NOSR
          NOSRSR = NOSRS
          PVSFR  = PVSF
          PVSRR  = PVSR
          VVSFR  = VVSF
          VVSRR  = VVSR
          AVSFR  = AVSF
          AVSRR  = AVSR
          SVSFR  = SVSF
          SVSRR  = SVSR
        ELSE
          ILR = 0
        END IF
      END IF
C-----IF LANE TO THE LEFT AND TO THE RIGHT ARE NOT AVAILABLE THEN GO TO
C-----7020 AND RESET LANE CHANGE PARAMETERS TO THEIR ORIGINAL VALUES AND
C-----RETURN
      IF ( ( ILL.EQ.0 ) . AND . ( ILR.EQ.0 ) )   GO TO 7020
      IF ( ( VVSFL . EQ . 0.0D0 ) . AND .
     *     ( VVSFR . EQ . 0.0D0 ) )              GO TO 7020
C-----FIND THE DISTANCE TO PERFORM 2.5 LANE CHANGES
      VELLCH = DMAX1( DESVEL,VELOLD,VELNEW )
      VEHLNG = DMIN1( 25.0D0,LENVAP )
      DISLCH = 2.5D0*TIMELC*VELLCH
      DISLCH = DMAX1( DISLCH,5.0D0*VEHLNG )
      IF ( ( ILL   . GT . 0     ) . AND .
     *     ( VVSFL . GE . VVSFR ) )              THEN
        IF ( ( NOSFSL          .EQ.0        ) . OR .
     *       ( ( VVSFL         .GT.PVVEL  ) . AND .
     *         ( (PVSFL-POSNEW).GE.DISLCH ) ) )  THEN
C-----    TRY TO CHANGE LANES TO THE LEFT
          ISIDE = 1
          LANSI = ILL
          NOSF  = NOSFL
          NOSFS = NOSFSL
          NOSR  = NOSRL
          NOSRS = NOSRSL
          PVSF  = PVSFL
          PVSR  = PVSRL
          VVSF  = VVSFL
          VVSR  = VVSRL
          AVSF  = AVSFL
          AVSR  = AVSRL
          SVSF  = SVSFL
          SVSR  = SVSRL
C-----    CHECK IF THERE IS AN ACCEPTABLE GAP TO LANE CHANGE INTO
C[        IF ( LANSI          .EQ.-2147483647   )STOP 'LCHDES LANSI  01'
          CALL  GAPACC  ( LANSI,IFLCHG )
C-----    IF THERE IS AN ACCEPTABLE GAP THEN GO TO 3010 AND LOG THE
C-----    VEHICLE OUT OF ITS PRESENT LANE AND INTO THE NEW LANE
                      IF ( ISET(IV) . EQ . 1 )   GO TO 3010
C-----    SET LEFT LANE NOT AVAILABLE BECAUSE THERE IS NO ACCEPTABLE GAP
          ILL = 0
        END IF
      END IF
      IF ( ILR . GT . 0 )                        THEN
        IF ( ( NOSFSR          .EQ.0        ) . OR .
     *       ( ( VVSFR         .GT.PVVEL  ) . AND .
     *         ( (PVSFR-POSNEW).GE.DISLCH ) ) )  THEN
C-----    TRY TO CHANGE LANES TO THE RIGHT
          ISIDE = 3
          LANSI = ILR
          NOSF  = NOSFR
          NOSFS = NOSFSR
          NOSR  = NOSRR
          NOSRS = NOSRSR
          PVSF  = PVSFR
          PVSR  = PVSRR
          VVSF  = VVSFR
          VVSR  = VVSRR
          AVSF  = AVSFR
          AVSR  = AVSRR
          SVSF  = SVSFR
          SVSR  = SVSRR
C-----    CHECK IF THERE IS AN ACCEPTABLE GAP TO LANE CHANGE INTO
C[        IF ( LANSI          .EQ.-2147483647   )STOP 'LCHDES LANSI  01'
          CALL  GAPACC  ( LANSI,IFLCHG )
C-----    IF THERE IS AN ACCEPTABLE GAP THEN GO TO 3010 AND LOG THE
C-----    VEHICLE OUT OF ITS PRESENT LANE AND INTO THE NEW LANE
                      IF ( ISET(IV) . EQ . 1 )   GO TO 3010
        END IF
      END IF
      IF ( ILL . GT . 0 )                        THEN
        IF ( ( NOSFSL          .EQ.0        ) . OR .
     *       ( ( VVSFL         .GT.PVVEL  ) . AND .
     *         ( (PVSFL-POSNEW).GE.DISLCH ) ) )  THEN
C-----    TRY TO CHANGE LANES TO THE LEFT
          ISIDE = 1
          LANSI = ILL
          NOSF  = NOSFL
          NOSFS = NOSFSL
          NOSR  = NOSRL
          NOSRS = NOSRSL
          PVSF  = PVSFL
          PVSR  = PVSRL
          VVSF  = VVSFL
          VVSR  = VVSRL
          AVSF  = AVSFL
          AVSR  = AVSRL
          SVSF  = SVSFL
          SVSR  = SVSRL
C-----    CHECK IF THERE IS AN ACCEPTABLE GAP TO LANE CHANGE INTO
C[        IF ( LANSI          .EQ.-2147483647   )STOP 'LCHDES LANSI  01'
          CALL  GAPACC  ( LANSI,IFLCHG )
C-----    IF THERE IS AN ACCEPTABLE GAP THEN GO TO 3010 AND LOG THE
C-----    VEHICLE OUT OF ITS PRESENT LANE AND INTO THE NEW LANE
                      IF ( ISET(IV) . EQ . 1 )   GO TO 3010
        END IF
      END IF
 7020 CONTINUE
C-----RESET LANE CHANGE PARAMETERS TO THEIR ORIGINAL VALUES
      LEGAL(IV) = MEGAL
      ISET (IV) = JSET
      RETURN
C-----PROCESS THE EXECUTION ERRORS AND STOP
 9030 CONTINUE
      CALL  ABORTR  ( 'STOP 903 - LEGAL NOT CHECKED - LCHDES' )
      STOP  903
 9040 CONTINUE
      CALL  ABORTR  ( 'STOP 904 - ILLEGAL TURN CODE - LCHDES' )
      STOP  904
 9050 CONTINUE
      CALL  ABORTR  ( 'STOP 905 - '                          //
     *                'TRYING TO CHANGE LANES WHEN NO LANE ' //
     *                'ALTERNATIVE EXISTS - '                //
     *                'LCHDES'                                  )
      STOP  905
      END                                                               LCHDES
C
C
C
      SUBROUTINE CHKLSI ( LANSI,POSCFB,MEGAL,LOK )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INTEGER           LANSI,LB,LE,LOK,MEGAL,MGEOM1,MGEOM2,MGEOM3,
     *                  MGEOM4
      DOUBLE PRECISION  DISLCH,DSPLCH,POSCFB,POSCRB,VELLCH,VEHLNG
C
C-----SUBROUTINE CHKLSI CHECKS THE LANE ON THE SIDE OF INTEREST TO SEE
C-----IF THE LANE IS AVAILABLE AT POSCFB, CLEAR TO THE INTERSECTION, AND
C-----THE REAR BUMPER WILL BE PAST THE START OF THE LANE AFTER A LANE
C-----CHANGE
C-----(LOK=0=OK, LOK=1=NOT AVAILABLE YET, AND LOK=2=PAST END OR BLOCKED)
C
C[    LB         = -2147483647
C[    LE         = -2147483647
C[    MGEOM1     = -2147483647
C[    MGEOM2     = -2147483647
C[    MGEOM3     = -2147483647
C[    MGEOM4     = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CHKLSI'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----SET FLAG FOR BLOCKED LANE (LOK=2=PAST END OR BLOCKED)
      LOK = 2
C-----IF THE LANE ON THE SIDE OF INTEREST IS INVALID THEN RETURN WITH
C-----THE FLAG SET FOR BLOCKED LANE (LOK=2=PAST END OR BLOCKED)
                    IF ( LANSI . LE . 0 )        RETURN
C-----IF THE VEHICLE IS NOT ALLOWED ON THE LANE ON THE SIDE OF INTEREST
C-----THEN RETURN WITH THE FLAG SET FOR BLOCKED LANE (LOK=2=PAST END OR
C-----BLOCKED)
      IF ( IAND( VEHTYP(IV),LAVT(LANSI) ).EQ.0 ) RETURN
      MGEOM1 = LGEOM(1,LANSI)
      MGEOM2 = LGEOM(2,LANSI)
      MGEOM3 = LGEOM(3,LANSI)
      MGEOM4 = LGEOM(4,LANSI)
C-----CHECK THE LANE ON THE SIDE OF INTEREST TO SEE IF THE LANE IS
C-----AVAILABLE AT POSCFB
C-----CHECK LANE CONTINUOUSLY AVAILABLE
      IF ( MGEOM2 . EQ . MGEOM4 )                THEN
        LB = MGEOM1
        LE = MGEOM4
        GO TO 1010
      END IF
C-----CHECK LANE ONLY AVAILABLE AT FIRST (DO NOT ALLOW A VEHICLE TO
C-----OPTIONALLY CHANGE LANES INTO A LANE THAT IS NOT AVAILABLE AT THE
C-----INTERSECTION)
      IF ( MGEOM3 . EQ . MGEOM4 )                THEN
        LOK = 2
        IF ( MEGAL . EQ . 2 )                    RETURN
        IF ( (.NOT. MBLOCK(IV)) )                RETURN
        IF ( MGEOM2 . LT . LGEOM(2,IL) )         RETURN
        LB = MGEOM1
        LE = MGEOM2
        IF ( LENVAP . GT . ((LE-LB)/4) )         RETURN
        GO TO 1010
      END IF
C-----CHECK LANE ONLY AVAILABLE AT LAST
      IF ( MGEOM1 . EQ . MGEOM2 )                THEN
        LB = MGEOM3
        LE = MGEOM4
        GO TO 1010
      END IF
C-----CHECK LANE ONLY AVAILABLE AT FIRST AND LAST (DO NOT ALLOW A
C-----VEHICLE TO OPTIONALLY CHANGE LANES INTO A LANE THAT IS NOT
C-----AVAILABLE AT THE INTERSECTION)
      IF ( POSCFB . LT . DBLE( MGEOM2 ) )        THEN
        LB = MGEOM1
        LE = MGEOM2
        LOK = 2
        IF ( LENVAP . GT . ((LE-LB)/4) )         RETURN
        LOK = 1
        IF ( MEGAL . EQ . 2 )                    RETURN
        IF ( (.NOT. MBLOCK(IV)) )                RETURN
        IF ( MGEOM2 . LT . LGEOM(2,IL) )         RETURN
        GO TO 1010
      ELSE
        LB = MGEOM3
        LE = MGEOM4
        GO TO 1010
      END IF
 1010 CONTINUE
C[    IF ( LB                 .EQ.-2147483647   )STOP 'CHKLSI LB     01'
C[    IF ( LE                 .EQ.-2147483647   )STOP 'CHKLSI LE     01'
      IF ( POSCFB . GT . DBLE( LE ) )            THEN
C-----  THE POSITION OF THE FRONT BUMPER OF THE VEHICLE IS GT THE END
C-----  POSITION OF THE LANE THUS RETURN WITH THE FLAG SET FOR BLOCKED
C-----  LANE (LOK=2=PAST END OR BLOCKED)
        LOK = 2
        RETURN
      END IF
      IF ( LENVAP . GT . (LE-LB) )               THEN
C-----  THE LENGTH OF THE VEHICLE IS GREATER THAN THE REMAINING DISTANCE
C-----  IN THE LANE ON THE SIDE OF INTEREST THUS RETURN WITH THE FLAG
C-----  SET FOR BLOCKED LANE (LOK=2=PAST END OR BLOCKED)
        LOK = 2
        RETURN
      END IF
      IF ( POSCFB . LT . DBLE( LB ) )            THEN
C-----  THE POSITION OF THE FRONT BUMPER OF THE VEHICLE IS LT THE BEGIN
C-----  POSITION OF THE LANE THUS RETURN WITH THE FLAG SET FOR BLOCKED
C-----  LANE (LOK=1=NOT AVAILABLE YET)
        LOK = 1
        RETURN
      END IF
C-----DEFINE THE LENGTH OF THE LANE CHANGE TO BE TIMELC SECONDS
C-----AT THE VELOCITY OF THE VEHICLE WITH A MINIMUM OF 1.5
C-----VEHICLE LENGTHS WITH A MAXIMUM VEHICLE LENGTH OF 25.0 FEET
      DSPLCH = DBLE( ISPD(IV) )
      IF ( IDISPD(IV) )                          THEN
        DSPLCH = 0.5D0*DSPLCH
      END IF
      IF ( ISPDP (IV) . EQ . 1 )                 THEN
        IF ( MININT(IV) )                        THEN
          IF ( LOBL(IP) . GT . 0 )               THEN
            DSPLCH = DSPLCH*DBLE( ISLIM(ISNA (LOBL(IP))) )
     *             /        DBLE( LIMP (           IP  ) )
          END IF
        ELSE
          DSPLCH   = DSPLCH*DBLE( ISLIM(           IA  ) )
     *             /        DBLE( LIMP (LNEXT(     IV )) )
        END IF
      END IF
      VELLCH = 0.2D0*DSPLCH
      VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
      VEHLNG = DMIN1( 25.0D0,DBLE( LENVAP ) )
      DISLCH = 0.5D0*(ENDLN-POSCFB)
      DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
      DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
      POSCRB = POSCFB - DBLE( LENVAP ) + DISLCH
      IF ( POSCRB . LT . DBLE( LB ) )            THEN
C-----  THE POSITION OF THE REAR BUMPER OF THE VEHICLE AFTER A LANE
C-----  CHANGE IS LT THE BEGIN POSITION OF THE LANE THUS RETURN WITH THE
C-----  FLAG SET FOR BLOCKED LANE (LOK=1=NOT AVAILABLE YET)
        LOK = 1
        RETURN
      END IF
C-----RETURN WITH THE FLAG SET FOR LANE NOT BLOCKED (LOK=0=OK)
      LOK = 0
      RETURN
      END                                                               CHKLSI
C
C
C
      SUBROUTINE SVEHU  ( NOQ )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           LCCLN
      INTEGER           I,IPMIN,IPSTR,IVMIN,IVP,JP,JV,LANSI,LANSII,
     *                  MEGAL,MGEOM4,MOBL,NOQ
      DOUBLE PRECISION  ACCVEH,DESSPD,DPP,POSVEH,PVSFRB,SLPVEH,TPMIN,
     *                  TPP,VELMIN,VELVEH
      DATA     VELMIN / 3.0D0 /
C
C-----SUBROUTINE SVEHU FINDS THE NEAREST VEHICLE TO THE FRONT AND THE
C-----NEAREST VEHICLE TO THE REAR IN THE LANE ON THE SIDE OF INTEREST
C-----FOR THIS VEHICLE
C
C[    IPMIN      = -2147483647
C[    IPSTR      = -2147483647
C[    IVMIN      = -2147483647
C[    IVP        = -2147483647
C[    LANSI      = -2147483647
C[    MEGAL      = -2147483647
C[    MGEOM4     = -2147483647
C[    DPP        = -2147483647.0
C[    TPMIN      = -2147483647.0
C[    TPP        = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'SVEHU'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----SET THE INDEX FOR THE LANE ON THE SIDE OF INTEREST BASED ON ISIDE
      IF ( ISIDE . EQ . 1 )                      THEN
        LANSI = NLL(IL)
      ELSE
        LANSI = NLR(IL)
      END IF
C-----IF THERE IS A LANE ON THE SIDE OF INTEREST AND IF THE VEHICLE IS
C-----NOT ALLOWED ON THE LANE ON THE SIDE OF INTEREST THEN SET NO LANE
C-----ON THE SIDE OF INTEREST
      IF ( LANSI . GT . 0 )                      THEN
        IF ( IAND( VEHTYP(IV),LAVT(LANSI) ) . EQ . 0 )
     *                                           LANSI = 0
      END IF
      LANSII = 0
      IF ( LANSI . GT . 0 )                      THEN
        IF ( ISIDE . EQ . 1 )                    THEN
          LANSII = NLL(LANSI)
        ELSE
          LANSII = NLR(LANSI)
        END IF
C-----  IF THERE IS AN LANSII LANE AND IF THE VEHICLE IS NOT ALLOWED ON
C-----  THE LANSII LANE THEN SET NO LANSII LANE
        IF ( LANSII . GT . 0 )                   THEN
          IF ( IAND( VEHTYP(IV),LAVT(LANSII) ) . EQ . 0 )
     *                                           LANSII = 0
        END IF
      END IF
C-----INITIALIZE SOME PARAMETERS FOR SVEHU
      NOQ   = 0
      NOSF  = 0
      NOSFS = 0
      NOSR  = 0
      NOSRS = 0
      IF ( LANSI . EQ . 0 )                      THEN
        PVSF = DBLE( LGEOM(4,IL) )
      ELSE
        PVSF = DBLE( LGEOM(4,LANSI) )
      END IF
      PVSR = 0.0D0
      VVSF = DMAX1( VELNEW,VELMIN )
      VVSR = VVSF
      AVSF = 0.0D0
      AVSR = AVSF
      SVSF = 0.0D0
      SVSR = SVSF
C-----IF OUTBOUND THEN SET PVSF VERY LARGE AND VVSF HIGHER
      IF ( IATYPE(IA) . EQ . OUTBDL )            THEN
        PVSF = POSBIG
        CALL  SETDSP  ( IV,POSNEW,DBLE( ISPD(IV) ),.FALSE.,DESSPD )
        VVSF = DMAX1( 0.5D0*(DESSPD+VELNEW),VELMIN )
      END IF
                    IF ( LANSI . GT . 0 )        GO TO 1010
C-----THERE IS NO LANE ALTERNATIVE ON THE SIDE OF INTEREST THUS RETURN
      ISIDE = 2
      ISET(IV) = 6
C-----IF THE VEHICLES REAR BUMPER IS ON THE DIAMOND INTERNAL INBOUND
C-----LANE THEN ALLOW LANE CHANGES
      IF ( (IATYPE(IA)                   . EQ . DINBDL ) . AND .
     *     (ISET(IV)                     . EQ . 6      ) . AND .
     *     ((POSNEW-DBLE( LGEOM(1,IL) )) . GE . LENVAP ) )
     *                                           THEN
        ISET(IV) = 5
      END IF
C-----CHECK IF ISET CAN BE SET TO 5
      CALL  CKISET  ( IV,POSNEW )
      GO TO 3010
 1010 CONTINUE
C-----SET THE POSITION OF THE NEAREST VEHICLE TO THE FRONT TO THE END OF
C-----THE LANE ON THE SIDE OF INTEREST
C[    IF ( LANSI              .EQ.-2147483647   )STOP 'SVEHU  LANSI  01'
      MGEOM4 = LGEOM(4,LANSI)
C-----CHECK IF THE REAR BUMPER OF ANY VEHICLE ON A LINKING INTERSECTION
C-----PATH IS STILL ON THE LANE
      DO  I = 1 , NPINT(LANSI)
        JP = LINTP(I,LANSI)
        NOSF = ILVP(JP)
        IF ( NOSF . EQ . 0 )                     THEN
          MOBL = LOBL(JP)
                    IF ( MOBL . EQ . 0 )         CYCLE
          NOSF = ILVL(MOBL)
                    IF (       NOSF  .EQ. 0  )   CYCLE
                    IF ( LPREV(NOSF) .NE. JP )   CYCLE
C-----    FIND THE POSITION AND SPEED OF THE FIRST VEHICLE IN THE LANE
C-----    ON THE SIDE OF INTEREST
          CALL  SPVAS  ( NOSF,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                   .FALSE.,.FALSE.,.FALSE.,.TRUE.    )
          PVSFRB = POSVEH - LVAP(NOSF) - XRELMI + DBLE( LENP(JP) )
          IF ( PVSFRB . LE . 0.0D0 )             THEN
            NOQ = MAX0( IDNINT( -PVSFRB/20.0D0 ),1 )
            PVSF = POSVEH + MGEOM4 + DBLE( LENP(JP) )
            VVSF = VELVEH
            AVSF = ACCVEH
            SVSF = SLPVEH
C-----      SET THE NEAREST VEHICLE TO THE REAR IN THE LANE ON THE
C-----      SIDE OF INTEREST TO THE FIRST VEHICLE IN THE LANE ON THE
C-----      SIDE OF INTEREST
            NOSR = IFVL(LANSI)
            GO TO 1050
          END IF
        ELSE
C-----    FIND THE POSITION AND SPEED OF THE FIRST VEHICLE IN THE LANE
C-----    ON THE SIDE OF INTEREST
          CALL  SPVAS  ( NOSF,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                   .FALSE.,.FALSE.,.FALSE.,.TRUE.    )
          PVSFRB = POSVEH - LVAP(NOSF) - XRELMI
          IF ( PVSFRB . LE . 0.0D0 )             THEN
            NOQ = MAX0( IDNINT( -PVSFRB/20.0D0 ),1 )
            PVSF = POSVEH + MGEOM4
            VVSF = VELVEH
            AVSF = ACCVEH
            SVSF = SLPVEH
C-----      SET THE NEAREST VEHICLE TO THE REAR IN THE LANE ON THE SIDE
C-----      OF INTEREST TO THE FIRST VEHICLE IN THE LANE ON THE SIDE OF
C-----      INTEREST
            NOSR = IFVL(LANSI)
            GO TO 1050
          END IF
        END IF
      END DO
C-----SET NOSF TO THE FIRST VEHICLE IN THE LANE ON THE SIDE OF INTEREST
      NOSF = IFVL(LANSI)
C-----IF NO FIRST VEHICLE IN THE LANE ON THE SIDE OF INTEREST THEN GO TO
C-----2010 AND CHECK FOR VEHICLES ON INTERSECTION PATHS FEEDING AN
C-----INTERNAL INBOUND OR OUTBOUND APPROACH
      IF ( NOSF . EQ . 0 )                       GO TO 2010
C-----FIND THE POSITION AND SPEED OF THE FIRST VEHICLE IN THE LANE ON
C-----THE SIDE OF INTEREST
      CALL  SPVAS   ( NOSF,PVSF,VVSF,AVSF,SVSF,
     *                .FALSE.,.FALSE.,.FALSE.,.TRUE. )
                    IF ( VVSF . GT . 0.0D0 )     GO TO 1030
      MEGAL = LEGAL(NOSF)
                    IF ( MEGAL . EQ . 2 )        GO TO 1030
                    IF ( MEGAL . GT . 3 )        GO TO 1030
C-----THE FIRST VEHICLE IN THE LANE ON THE SIDE OF INTEREST IS STOPPED
C-----AND HE MUST CHANGE LANES THUS SET NOQ TO BE THE NUMBER OF 20 FOOT
C-----VEHICLES THAT WOULD OCCUPY THE DISTANCE FROM THE FIRST VEHICLE IN
C-----THE LANE ON THE SIDE OF INTEREST TO THE END OF THAT LANE
C[    IF ( MGEOM4             .EQ.-2147483647   )STOP 'SVEHU  MGEOM4 01'
      NOQ = MAX0( IDNINT( (MGEOM4-PVSF)/20.0D0 ),1 )
 1030 CONTINUE
C-----IF THE POSITION OF THE FIRST VEHICLE IN THE LANE ON THE SIDE OF
C-----INTEREST IS GT THE POSITION OF THIS VEHICLE THEN GO TO 1040 AND
C-----CHECK THE NEXT VEHICLE IN THE LANE ON THE SIDE OF INTEREST ELSE
C-----SET THE NEAREST VEHICLE TO THE FRONT TO NO VEHICLE AND SET THE
C-----NEAREST VEHICLE TO THE REAR TO THE FIRST VEHICLE IN THE LANE ON
C-----THE SIDE OF INTEREST
                    IF ( PVSF . GT . POSNEW )    GO TO 1040
      NOSR = NOSF
      NOSF = 0
      PVSR = PVSF
C[    IF ( MGEOM4             .EQ.-2147483647   )STOP 'SVEHU  MGEOM4 02'
      PVSF = MGEOM4
      VVSR = VVSF
      VVSF = DMAX1( VELNEW,VELMIN )
      AVSR = AVSF
      AVSF = 0.0D0
      SVSR = SVSF
      SVSF = 0.0D0
      NOQ = 0
      GO TO 3010
 1040 CONTINUE
C-----INCREMENT THE NUMBER OF VEHICLES IN THE LANE ON THE SIDE OF
C-----INTEREST AHEAD OF THIS VEHICLE
      NOQ = NOQ + MAX0( IDNINT( LVAP(NOSF)/20.0D0 ),1 )
C-----SET THE NEAREST VEHICLE TO THE REAR IN THE LANE ON THE SIDE OF
C-----INTEREST TO THE NOR FOR THE NOSF VEHICLE
      NOSR = NOR(NOSF)
 1050 CONTINUE
C-----IF THERE IS NO VEHICLE BEHIND THE NOSF VEHICLE THEN GO TO 2010 AND
C-----CHECK FOR VEHICLES ON INTERSECTION PATHS FEEDING AN INTERNAL
C-----INBOUND OR OUTBOUND APPROACH ELSE FIND THE POSITION AND SPEED OF
C-----THE NOSR VEHICLE IN THE LANE ON THE SIDE OF INTEREST
                    IF ( NOSR . EQ . 0 )         GO TO 2010
      CALL  SPVAS   ( NOSR,PVSR,VVSR,AVSR,SVSR,
     *                .FALSE.,.FALSE.,.FALSE.,.TRUE. )
                    IF ( VVSR . GT . 0.0D0 )     GO TO 1060
      MEGAL = LEGAL(NOSR)
                    IF ( MEGAL . EQ . 2 )        GO TO 1060
                    IF ( MEGAL . GT . 3 )        GO TO 1060
C-----THE NOSR VEHICLE IN THE LANE ON THE SIDE OF INTEREST IS STOPPED
C-----AND HE MUST CHANGE LANES THUS SET NOQ TO BE THE NUMBER OF 20 FOOT
C-----VEHICLES THAT WOULD OCCUPY THE DISTANCE FROM THE NOSR VEHICLE IN
C-----THE LANE ON THE SIDE OF INTEREST TO THE END OF THAT LANE
C[    IF ( MGEOM4             .EQ.-2147483647   )STOP 'SVEHU  MGEOM4 03'
      NOQ = MAX0( IDNINT( (MGEOM4-PVSR)/20.0D0 ),1 )
 1060 CONTINUE
C-----IF THE POSITION OF THE NOSR VEHICLE IN THE LANE ON THE SIDE OF
C-----INTEREST IS LE THE POSITION OF THIS VEHICLE THEN GO TO 3010 AND
C-----SET THE POSITIONS ELSE SET THE NEW NOSF VEHICLE TO THE NOSR
C-----VEHICLE AND SET THE NEW NOSR VEHICLE TO NO VEHICLE AND CHECK AGAIN
                    IF ( PVSR . LE . POSNEW )    GO TO 3010
      NOSF = NOSR
      NOSR = 0
      PVSF = PVSR
      PVSR = 0.0D0
      VVSF = VVSR
      VVSR = DMAX1( VELNEW,VELMIN )
      AVSF = AVSR
      AVSR = 0.0D0
      SVSF = SVSR
      SVSR = 0.0D0
      GO TO 1040
 2010 CONTINUE
C-----THERE IS NO FIRST VEHICLE IN THE LANE ON THE SIDE OF INTEREST OR
C-----THERE IS NO NOSR VEHICLE THEREFORE CHECK FOR VEHICLES ON
C-----INTERSECTION PATHS FEEDING AN INTERNAL INBOUND OR OUTBOUND
C-----APPROACH AND SET AS THE NOSR VEHICLE ELSE SET THE BEGINNING OF
C-----THE STRAIGHT INTERSECTION PATH AS PVSR WITH VVSR = 0.8*LIMP
            IF ( IATYPE(IA) . EQ . INBNDL )      GO TO 3010
      IPSTR = 0
      IPMIN = 0
      IVMIN = 0
      TPMIN = 1.0D9
      DO 2030  IP = 1 , NPATHS
C[    IF ( LANSI              .EQ.-2147483647   )STOP 'SVEHU  LANSI  02'
                    IF ( LOBL(IP) . NE . LANSI ) GO TO 2030
                    IF ( IPSTR . EQ . 0 )        IPSTR = IP
                    IF ( IPT(IP) . EQ . 2 )      IPSTR = IP
      IVP = IFVP(IP)
                    IF ( IVP . EQ . 0 )          GO TO 2030
      DPP = LENP(IP) - IPOS(IVP)
      IF ( IVEL(IVP) . LE . 0.0D0 )              THEN
        TPP = 1000.0D0 + DPP
      ELSE
        TPP = DPP/IVEL(IVP)
      END IF
                    IF ( TPP . GT . TPMIN )      GO TO 2030
      IPMIN = IP
      IVMIN = IVP
      TPMIN = TPP
 2030 CONTINUE
                    IF ( IPMIN . GT . 0 )        GO TO 2040
      NOSR = 0
C[    IF ( IPSTR              .EQ.-2147483647   )STOP 'SVEHU  IPSTR  01'
      IF ( IPSTR . EQ . 0 )                      THEN
        PVSR = -1000.0D0
        VVSR = 0.8D0*VELNEW
      ELSE
        PVSR = -LENP(IPSTR)
        VVSR = 0.8D0*LIMP(IPSTR)
      END IF
      AVSR = 0.0D0
      SVSR = 0.0D0
      GO TO 3010
 2040 CONTINUE
C[    IF ( IPMIN              .EQ.-2147483647   )STOP 'SVEHU  IPMIN  01'
      IP   = IPMIN
C[    IF ( IVMIN              .EQ.-2147483647   )STOP 'SVEHU  IVMIN  01'
      NOSR = IVMIN
      CALL  SPVAS   ( NOSR,PVSR,VVSR,AVSR,SVSR,
     *                .FALSE.,.FALSE.,.FALSE.,.TRUE. )
      PVSR = PVSR - LENP(IPMIN)
 3010 CONTINUE
C-----SAVE THE NOSF AND NOSR THAT ARE THE VEHICLES IN THE LANE ON THE
C-----SIDE OF INTEREST NOT A VEHICLE POSSIBLY CHANGING LANES OUT OF THE
C-----LANE ON THE SIDE OF INTEREST
      NOSFS = NOSF
      NOSRS = NOSR
C-----CHECK THE LANSII LANE ON THE SIDE OF INTEREST
      IF ( LANSII . GT . 0 )                     THEN
C-----  CHECK FOR A VEHICLE CHANGING OUT OF LANSII THAT HAS NOT CLEARED
C-----  LANSII
        JV = IFVL(LANSII)
C-----  CHECK FOR NOSF VEHICLE
        DO WHILE ( JV . GT . 0 )
          CALL  SPVAS  ( JV,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                   .FALSE.,.FALSE.,.FALSE.,.TRUE. )
          IF ( POSVEH . LE . POSNEW )            EXIT
          IF ( ISIDE . EQ . 1 )                  THEN
C-----      CHECK LANE TO THE LEFT FOR VEHICLE CHANGING LANES TO THE
C-----      LEFT
            IF ( ( ISET(JV)   . EQ . 1     ) . AND .
     *           ( LATPOS(JV) . GT . 0.0D0 ) )   THEN
              CALL  CLCCLN ( JV,LANSII,LCCLN )
              IF ( .NOT. LCCLN )                 THEN
                NOQ = NOQ + MAX0( IDNINT( LVAP(JV)/20.0D0 ),1 )
                IF ( POSVEH . LT . PVSF )        THEN
C-----            VEHICLE JV IN THE LANE TO THE LEFT OF THE LANE TO THE
C-----            LEFT IS AHEAD OF THIS VEHICLE AND HAS NOT COMPLETED
C-----            ENOUGH OF ITS LANE CHANGE TO BE CLEAR OF THE LANE TO
C-----            THE LEFT AND IT IS CLOSER TO THIS VEHICLE THAN THE
C-----            CURRENT NOSF VEHICLE THUS SET THE NOSF VEHICLE TO
C-----            VEHICLE JV
                  NOSF = JV
                  PVSF = POSVEH
                  VVSF = VELVEH
                  AVSF = ACCVEH
                  SVSF = SLPVEH
                END IF
              END IF
            END IF
          ELSE
C-----      CHECK LANE TO THE RIGHT FOR VEHICLE CHANGING LANES TO THE
C-----      RIGHT
            IF ( ( ISET(JV)   . EQ . 1     ) . AND .
     *           ( LATPOS(JV) . LT . 0.0D0 ) )   THEN
              CALL  CLCCLN ( JV,LANSII,LCCLN )
              IF ( .NOT. LCCLN )                 THEN
                NOQ = NOQ + MAX0( IDNINT( LVAP(JV)/20.0D0 ),1 )
                IF ( POSVEH . LT . PVSF )        THEN
C-----            VEHICLE JV IN THE LANE TO THE RIGHT OF THE LANE TO THE
C-----            RIGHT IS AHEAD OF THIS VEHICLE AND HAS NOT COMPLETED
C-----            ENOUGH OF ITS LANE CHANGE TO BE CLEAR OF THE LANE TO
C-----            THE RIGHT AND IT IS CLOSER TO THIS VEHICLE THAN THE
C-----            CURRENT NOSF VEHICLE THUS SET THE NOSF VEHICLE TO
C-----            VEHICLE JV
                  NOSF = JV
                  PVSF = POSVEH
                  VVSF = VELVEH
                  AVSF = ACCVEH
                  SVSF = SLPVEH
                END IF
              END IF
            END IF
          END IF
          JV = NOR(JV)
        END DO
C-----  CHECK FOR NOSR VEHICLE
C-----  JV WILL BE SET TO 0 IF THERE IS NO VEHICLE AHEAD OF THIS VEHICLE
C-----  ELSE JV WILL BE SET TO THE FIRST VEHICLE BEHIND THIS VEHICLE
        DO WHILE ( JV . GT . 0 )
          CALL  SPVAS  ( JV,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                   .FALSE.,.FALSE.,.FALSE.,.TRUE. )
          IF ( POSVEH . GT . POSNEW )            EXIT
          IF ( ISIDE . EQ . 1 )                  THEN
C-----      CHECK LANE TO THE LEFT FOR VEHICLE CHANGING LANES TO THE
C-----      LEFT
            IF ( ( ISET(JV)   . EQ . 1     ) . AND .
     *           ( LATPOS(JV) . GT . 0.0D0 ) )   THEN
              CALL  CLCCLN ( JV,LANSII,LCCLN )
              IF ( .NOT. LCCLN )                 THEN
                IF ( POSVEH . GT . PVSR )        THEN
C-----            VEHICLE JV IN THE LANE TO THE LEFT OF THE LANE TO THE
C-----            LEFT IS BEHIND THIS VEHICLE AND HAS NOT COMPLETED
C-----            ENOUGH OF ITS LANE CHANGE TO BE CLEAR OF THE LANE TO
C-----            THE LEFT AND IT IS CLOSER TO THIS VEHICLE THAN THE
C-----            CURRENT NOSR VEHICLE THUS SET THE NOSR VEHICLE TO
C-----            VEHICLE JV
                  NOSR = JV
                  PVSR = POSVEH
                  VVSR = VELVEH
                  AVSR = ACCVEH
                  SVSR = SLPVEH
                  EXIT
                END IF
              END IF
            END IF
          ELSE
C-----      CHECK LANE TO THE RIGHT FOR VEHICLE CHANGING LANES TO THE
C-----      RIGHT
            IF ( ( ISET(JV)   . EQ . 1     ) . AND .
     *           ( LATPOS(JV) . LT . 0.0D0 ) )   THEN
              CALL  CLCCLN ( JV,LANSII,LCCLN )
              IF ( .NOT. LCCLN )                 THEN
                IF ( POSVEH . GT . PVSR )        THEN
C-----            VEHICLE JV IN THE LANE TO THE RIGHT OF THE LANE TO THE
C-----            RIGHT IS BEHIND THIS VEHICLE AND HAS NOT COMPLETED
C-----            ENOUGH OF ITS LANE CHANGE TO BE CLEAR OF THE LANE TO
C-----            THE RIGHT AND IT IS CLOSER TO THIS VEHICLE THAN THE
C-----            CURRENT NOSR VEHICLE THUS SET THE NOSR VEHICLE TO
C-----            VEHICLE JV
                  NOSR = JV
                  PVSR = POSVEH
                  VVSR = VELVEH
                  AVSR = ACCVEH
                  SVSR = SLPVEH
                  EXIT
                END IF
              END IF
            END IF
          END IF
          JV = NOR(JV)
        END DO
      END IF
      IF ( NOSF . GT . 0 )                       THEN
        PVSF = PVSF - LVAP(NOSF) - XRELMI
      END IF
      RETURN
      END                                                               SVEHU
C
C
C
      SUBROUTINE PRENEW ( PPO,PVO,PAO,PSO )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
C*    CHARACTER*6       IPRENW
      DOUBLE PRECISION  PAN,PAO,PPN,PPO,PSN,PSO,PVN,PVO,T
C*    DATA     IPRENW / 'PRENEW' /
C
C-----SUBROUTINE PRENEW CALCULATES THE POS/VEL/ACC FOR THE VEHICLE AFTER
C-----DT SECONDS
C
C[    PAN        = -2147483647.0
C[    PPN        = -2147483647.0
C[    PSN        = -2147483647.0
C[    PVN        = -2147483647.0
C[    T          = -2147483647.0
C*          IF ( IRNAME(NRNAME) . EQ . IPRENW )  GO TO 101
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'PRENEW'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C*101 CONTINUE
C-----CALCULATE THE POS/VEL/ACC/SLP FOR THE VEHICLE AFTER DT SECONDS
C-----(PREDICT FORWARD ONE DT SECOND)
      PSN = PSO
      PAN = PAO + PSN*DT
      PVN = PVO + PAO*DT + 0.5D0*PSN*DTSQ
      PPN = PPO + PVO*DT + 0.5D0*PAO*DTSQ + ONED6*PSN*DTCU
C-----IF THE VEHICLES VELOCITY IS GT 0 THEN GO TO 2010 AND USE NEW
C-----POS/VEL/ACC/SLP
                    IF (  PVN .GT. VELSTP )      GO TO 2010
                    IF ( (PVN .GT. 0.0D0  ) . AND .
     *                   (PAN .GT. 0.0D0  ) . AND .
     *                   (PSN .GT. 0.0D0  ) )    GO TO 2010
C-----THE VEHICLE STOPPED THIS DT THUS CALCULATE THE TIME REQUIRED TO
C-----BRING THE VEHICLE TO A STOP WITHIN THIS DT
      CALL  TIMSTP  ( PVO,PAO,PSN,1.05D0*DT,T )
      IF ( T . EQ . TIMERR )                     THEN
        IF ( PVO . LE . VSMALL )                 THEN
          T = 0.0D0
        ELSE
          T = DT
        END IF
      END IF
C-----CALCULATE THE POS/VEL/ACC/SLP FOR THE VEHICLE AFTER T SECONDS
C-----(THE VELOCITY SHOULD BE 0)
      PSN = 0.0D0
      PAN = 0.0D0
      PVN = 0.0D0
C[    IF ( T                  .EQ.-2147483647.0 )STOP 'PRENEW T      01'
      PPN = PPO + PVO*T + 0.5D0*PAO*T**2 + ONED6*PSO*T**3
 2010 CONTINUE
C-----SET NEW POS/VEL/ACC/SLP FOR THE VEHICLE
C[    IF ( PSN                .EQ.-2147483647.0 )STOP 'PRENEW PSN    01'
      PSO = PSN
C[    IF ( PAN                .EQ.-2147483647.0 )STOP 'PRENEW PAN    01'
      PAO = PAN
C[    IF ( PVN                .EQ.-2147483647.0 )STOP 'PRENEW PVN    01'
      PVO = PVN
C[    IF ( PPN                .EQ.-2147483647.0 )STOP 'PRENEW PPN    01'
      PPO = PPN
      RETURN
      END                                                               PRENEW
C
C
C
      SUBROUTINE PREOLD ( PPN,PVN,PAN,PSN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
C*    CHARACTER*6       IPREOD
      DOUBLE PRECISION  PAN,PAO,PPN,PPO,PSN,PSO,PVN,PVO
C*    DATA     IPREOD / 'PREOLD' /
C
C-----SUBROUTINE PREOLD CALCULATES THE POS/VEL/ACC FOR THE VEHICLE
C-----BEFORE DT SECONDS
C
C[    PAO        = -2147483647.0
C[    PPO        = -2147483647.0
C[    PSO        = -2147483647.0
C[    PVO        = -2147483647.0
C*          IF ( IRNAME(NRNAME) . EQ . IPREOD )  GO TO 101
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'PREOLD'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C*101 CONTINUE
C-----CALCULATE THE POS/VEL/ACC/SLP FOR THE VEHICLE BEFORE DT SECONDS
C-----(PREDICT BACKWARD ONE DT SECOND)
      PSO = PSN
      PAO = PAN - PSO*DT
      PVO = PVN - PAO*DT - 0.5D0*PSO*DTSQ
      PPO = PPN - PVO*DT - 0.5D0*PAO*DTSQ - ONED6*PSO*DTCU
C-----IF THE VEHICLES VELOCITY IS GT 0 THEN GO TO 2010 AND USE NEW
C-----POS/VEL/ACC/SLP
                    IF (  PVO .GT. VELSTP )      GO TO 1010
                    IF ( (PVO .GT. 0.0D0  ) . AND .
     *                   (PAO .GT. 0.0D0  ) . AND .
     *                   (PSO .GT. 0.0D0  ) )    GO TO 1010
C-----THE VEHICLE WAS STOPPED BEFORE THIS DT THUS SET THE VEHICLE
C-----STOPPED
      PSO = 0.0D0
      PAO = 0.0D0
      PVO = 0.0D0
 1010 CONTINUE
C-----SET NEW POS/VEL/ACC/SLP FOR THE VEHICLE
C[    IF ( PSO                .EQ.-2147483647.0 )STOP 'PREOLD PSO    01'
      PSN = PSO
C[    IF ( PAO                .EQ.-2147483647.0 )STOP 'PREOLD PAO    01'
      PAN = PAO
C[    IF ( PVO                .EQ.-2147483647.0 )STOP 'PREOLD PVO    01'
      PVN = PVO
C[    IF ( PPO                .EQ.-2147483647.0 )STOP 'PREOLD PPO    01'
      PPN = PPO
      RETURN
      END                                                               PREOLD
C
C
C
      SUBROUTINE DELAY
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           IBLOCK,LCHKCF,LBVSTP,MAJCBL,TURNL(16),TURNR(16),
     *                  TURNS(16)
      INTEGER           CLTURN,IPENTC(4,4),JTURN,KV,LANSI,LLTURN,LOBLC,
     *                  LOBLL,LOBLR,LOK,NOQ,NORF,NORFS,NORR,NORRS,RLTURN
      DOUBLE PRECISION  AVRF,AVRR,POSCBL,POSMJC,PVRF,PVRR,QUEA,QUEC,
     *                  QUEL,QUER,SVRF,SVRR,VVRF,VVRR
C-----         ME-NOF   UU LU SU RU UL LL SL RL US LS SS RS UR LR SR RR
      DATA     IPENTC / 1, 1, 4, 4, 1, 1, 4, 4, 0, 0, 0, 0, 2, 2, 2, 1 /
C-----THE FIRST VALUE (VALUE=0) IN ARRAY LTURN IS AN OUTBOUND OR BLOCKED
C-----INBOUND LANE AND IS NOT AVAILABLE TO BE USED.  THE SENSE IS
C-----REVERSED SO THAT THE .NOT. IN THE IF TESTS WILL IGNORE ADDING TO
C-----THE ARTIFICIAL QUEUE LENGTH AND QUEUE ADD (QUEA).
      DATA     TURNL  / .TRUE. ,.FALSE.,.FALSE.,.FALSE.,.TRUE. ,.TRUE. ,
     *                  .TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     *                  .TRUE. ,.TRUE. ,.TRUE. ,.TRUE. /
      DATA     TURNS  / .TRUE. ,.FALSE.,.TRUE. ,.TRUE. ,.FALSE.,.FALSE.,
     *                  .TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.TRUE. ,.TRUE. ,
     *                  .FALSE.,.FALSE.,.TRUE. ,.TRUE. /
      DATA     TURNR  / .TRUE. ,.TRUE. ,.FALSE.,.TRUE. ,.FALSE.,.TRUE. ,
     *                  .FALSE.,.TRUE. ,.FALSE.,.TRUE. ,.FALSE.,.TRUE. ,
     *                  .FALSE.,.TRUE. ,.FALSE.,.TRUE. /
C
C-----SUBROUTINE DELAY FINDS THE LEGAL LANE FOR THE VEHICLE WITH THE
C-----MINIMUM EXPECTED DELAY
C
C[    CLTURN     = -2147483647
C[    JTURN      = -2147483647
C[    LANSI      = -2147483647
C[    LLTURN     = -2147483647
C[    LOBLC      = -2147483647
C[    LOBLL      = -2147483647
C[    LOBLR      = -2147483647
C[    LOK        = -2147483647
C[    NOQ        = -2147483647
C[    NORF       = -2147483647
C[    NORR       = -2147483647
C[    RLTURN     = -2147483647
C[    AVRF       = -2147483647.0
C[    AVRR       = -2147483647.0
C[    PVRF       = -2147483647.0
C[    PVRR       = -2147483647.0
C[    QUEA       = -2147483647.0
C[    QUEC       = -2147483647.0
C[    QUEL       = -2147483647.0
C[    QUER       = -2147483647.0
C[    SVRF       = -2147483647.0
C[    SVRR       = -2147483647.0
C[    VVRF       = -2147483647.0
C[    VVRR       = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'DELAY'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----IF EMERGENCY VEHICLE THEN SET AGGRESSIVE LANE CHANGING
      IF ( IAND( VEHTYP(IV),LAVTE ) .NE. 0 )     THEN
        QUEA = 1.0D0
      ELSE
        QUEA = 2.0D0
      END IF
      JTURN = ITURNS
                    IF ( NOF(IV) . EQ . 0 )      GO TO 1010
      JTURN = ITURN(NOF(IV))
                    IF ( JTURN . EQ . 0 )        JTURN = ITURNS
 1010 CONTINUE
C-----FIND THE EQUIVALENT NUMBER OF VEHICLES IN THE QUEUE IN THE LANE
C-----STRAIGHT AHEAD BASED ON THE TURN CODE OF THIS VEHICLE AND THE TURN
C-----CODE FOR THE NOFT VEHICLE
C[    IF ( JTURN              .EQ.-2147483647   )STOP 'DELAY  JTURN  01'
      QUEC = NCQ + IPENTC(ITURN(IV),JTURN)*DCHAR(IDRICL(IV))
C-----IF THE VEHICLE IS BLOCKED BY A MAJOR COLLISION THEN MAKE THE
C-----CURRENT LANE QUEUE LENGTH EXTREMELY LONG SO THE VEHICLE WILL
C-----CHANGE LANES IF POSSIBLE
      IF ( MAJCLB(IV) . OR . MAJCLL(IV) )        THEN
        QUEC = POSBIG
      END IF
C-----IF THE VEHICLES INTERSECTION PATH CHANGES LANES WITHIN THE
C-----INTERSECTION THEN INCREASE THE EQUIVALENT NUMBER OF VEHICLES IN
C-----THE QUEUE IN THE LANE STRAIGHT AHEAD
            IF ( ILCH(LNEXT(IV)) . NE . 0 )      QUEC = QUEC + 10.0D0
C-----INITIALIZE THE VALUES FOR THE EQUIVALENT NUMBER OF VEHICLES IN THE
C-----LANE TO THE LEFT AND THE LANE TO THE RIGHT
      QUER =  POSBIG
      QUEL =  POSBIG
 1020 CONTINUE
C-----PROCESS BY THE LANE ALTERNATIVE
C-----        NONE RGHT LEFT BOTH NOTC NOTD
      GO TO ( 2010,4010,5010,4010,3010,6020 ) , LALT(IV)
 2010 CONTINUE
C-----THERE ARE NO LANE ALTERNATIVES THUS RETURN AND DO NOT CHECK THE
C-----DESIRABILITY OF A LANE CHANGE ANY MORE
      ISET(IV) = 6
      GO TO 6020
 3010 CONTINUE
C-----CHECK THE LANE ALTERNATIVES FOR THIS LANE
      CALL  CKLALT
      GO TO 1020
 4010 CONTINUE
C-----FIND THE EQUIVALENT NUMBER OF VEHICLES IN THE QUEUE IN THE LANE TO
C-----THE RIGHT
      LANSI = NLR(IL)
C-----CHECK THE LANE ON THE SIDE OF INTEREST TO SEE IF THE LANE IS
C-----AVAILABLE AT THE CURRENT POSITION OF THE VEHICLE AND CLEAR TO THE
C-----INTERSECTION
      CALL  CHKLSI  ( LANSI,POSNEW,LEGAL(IV),LOK )
C-----IF THE LANE IS NOT AVAILABLE FOR THIS VEHICLE THEN GO TO 5010 AND
C-----CHECK THE LANE ON THE LEFT
C[    IF ( LOK                .EQ.-2147483647   )STOP 'DELAY  LOK    01'
                    IF ( LOK . NE . 0 )          GO TO 5010
      ISIDE = 3
      JTURN = ITURNS
C-----FIND THE NEAREST VEHICLE TO THE FRONT AND THE NEAREST VEHICLE TO
C-----THE REAR IN THE LANE ON THE SIDE OF INTEREST FOR THIS VEHICLE
      CALL  SVEHU   ( NOQ )
C-----SAVE THE VEHICLE PARAMETERS FOR THE LANE TO THE RIGHT
      NORF  = NOSF
      NORFS = NOSFS
      NORR  = NOSR
      NORRS = NOSRS
      PVRF  = PVSF
      PVRR  = PVSR
      VVRF  = VVSF
      VVRR  = VVSR
      AVRF  = AVSF
      AVRR  = AVSR
      SVRF  = SVSF
      SVRR  = SVSR
                    IF ( NOSF . EQ . 0 )         GO TO 4020
C-----FIND THE LEAD VEHICLES TURN CODE
      JTURN = ITURN(NOSF)
C-----IF THE LEAD VEHICLES TURN CODE EQ 0 THEN SET FOR STRAIGHT
                    IF ( JTURN . EQ . 0 )        JTURN = ITURNS
 4020 CONTINUE
C-----COMPUTE THE EQUIVALENT NUMBER OF VEHICLES IN THE QUEUE IN THE LANE
C-----TO THE RIGHT BASED ON THE TURN CODE OF THE VEHICLE AND THE TURN
C-----CODE OF THE LEAD VEHICLE ON THE RIGHT
C[    IF ( JTURN              .EQ.-2147483647   )STOP 'DELAY  JTURN  02'
C[    IF ( NOQ                .EQ.-2147483647   )STOP 'DELAY  NOQ    01'
      QUER = NOQ+QUEA + IPENTC(ITURN(IV),JTURN)*DCHAR(IDRICL(IV))
      IF ( SMJCOL )                              THEN
        IF ( LALT(IV) . GE . 5 )                 CALL  CKLALT
        CALL  CLMJCL  ( NLR(IL),LALTPR(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
        IF ( KV . GT . 0 )                       THEN
          QUER = POSBIG
        ELSE
          IF ( LALTPR(IV) . GT . 0 )             THEN
            CALL  CHKINT  ( LALTPR(IV),.FALSE.,.FALSE.,IBLOCK,MAJCBL,
     *                      POSCBL                                    )
            IF ( IBLOCK . AND . MAJCBL )         THEN
              QUER = POSBIG
            END IF
          ELSE
            QUER = POSBIG
          END IF
        END IF
      END IF
 5010 CONTINUE
C-----IF THE LANE TO THE LEFT IS NOT AN ALTERNATIVE FOR THIS LANE THEN
C-----GO TO 6010 AND DETERMINE WHICH LANE HAS THE MINIMUM EXPECTED DELAY
                    IF ( LALT(IV) . EQ . 2 )     GO TO 6010
      LANSI = NLL(IL)
C-----CHECK THE LANE ON THE SIDE OF INTEREST TO SEE IF THE LANE IS
C-----AVAILABLE AT THE CURRENT POSITION OF THE VEHICLE AND CLEAR TO THE
C-----INTERSECTION
      CALL  CHKLSI  ( LANSI,POSNEW,LEGAL(IV),LOK )
C-----IF THE LANE TO THE LEFT IS NOT AVAILABLE FOR THE VEHICLE THEN GO
C-----TO 6010 AND DETERMINE WHICH LANE HAS THE MINIMUM EXPECTED DELAY
C[    IF ( LOK                .EQ.-2147483647   )STOP 'DELAY  LOK    02'
                    IF ( LOK . NE . 0 )          GO TO 6010
      ISIDE = 1
      JTURN = ITURNS
C-----FIND THE NEAREST VEHICLE TO THE FRONT AND THE NEAREST VEHICLE TO
C-----THE REAR IN THE LANE ON THE SIDE OF INTEREST FOR THIS VEHICLE
      CALL  SVEHU   ( NOQ )
                    IF ( NOSF . EQ . 0 )         GO TO 5020
C-----FIND THE LEAD VEHICLES TURN CODE
      JTURN = ITURN(NOSF)
C-----IF THE LEAD VEHICLES TURN CODE EQ 0 THEN SET FOR STRAIGHT
                    IF ( JTURN . EQ . 0 )        JTURN = ITURNS
 5020 CONTINUE
C[    IF ( JTURN              .EQ.-2147483647   )STOP 'DELAY  JTURN  03'
C[    IF ( NOQ                .EQ.-2147483647   )STOP 'DELAY  NOQ    02'
      QUEL = NOQ+QUEA + IPENTC(ITURN(IV),JTURN)*DCHAR(IDRICL(IV))
      IF ( SMJCOL )                              THEN
        IF ( LALT(IV) . GE . 5 )                 CALL  CKLALT
        CALL  CLMJCL  ( NLL(IL),LALTPL(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
        IF ( KV . GT . 0 )                       THEN
          QUEL = POSBIG
        ELSE
          IF ( LALTPL(IV) . GT . 0 )             THEN
            CALL  CHKINT  ( LALTPL(IV),.FALSE.,.FALSE.,IBLOCK,MAJCBL,
     *                      POSCBL                                    )
            IF ( IBLOCK . AND . MAJCBL )         THEN
              QUEL = POSBIG
            END IF
          ELSE
            QUEL = POSBIG
          END IF
        END IF
      END IF
C-----COMPUTE THE EQUIVALENT NUMBER OF VEHICLES IN THE QUEUE IN THE LANE
C-----TO THE LEFT BASED ON THE TURN CODE OF THE VEHICLE AND THE TURN
C-----CODE OF THE LEAD VEHICLE ON THE LEFT
 6010 CONTINUE
C-----IF VEHICLE IV HAS AN INTERSECTION PATH (INT2P) FOR THE SECOND
C-----INTERSECTION AND IS ON THE INBOUND APPROACH, THEN WEIGHT THE QUEUE
C-----LENGTHS FOR THE CURRENT INBOUND LANES ACCORDING TO HIS TURN CODE
C-----FOR THE 2ND INTERSECTION AND WHETHER THE CONNECTING INTERNAL LANE
C-----HAS A PATH TO GET THE VEHICLE TO HIS DESIRED OUTBOUND APPROACH.
C-----VEHICLE IV MUST HAVE A VALID LNEXT TO CALL DELAY BUT HIS TURN CODE
C-----IS FOR THE FIRST INTERSECTION AND THE LINKING OUTBOUND LANE
C-----(INTERNAL INBOUND LANE) FOR THE LNEXT PATH MAY NOT HAVE A PATH TO
C-----THE DESIRED OUTBOUND APPROACH
C;    IF ( (IAND( IPRTLO(IV),1 ).NE.0) . AND . (TIME.GE.TPRINT) )
C;   *WRITE (6,601) QUEL,QUEC,QUER,INT2P(IV),IAN,LIBAR(IA),IAFLAG(IA)
C;601 FORMAT(' QUEL=',F6.1,' QUEC=',F6.1,' QUER=',F6.1,' INT2P=',I3,
C;   *       ' IAN=',I2,' LIBAR=',I2,' IAFLAG=',A)
      IF ( ( DIAMON                   ) . AND .
     *     ( INT2P (IV) . GT . 0      ) . AND .
     *     ( IATYPE(IA) . EQ . INBNDL ) . AND .
     *     ( LNEXT (IV) . GT . 0      ) )        THEN
C-----START OF SETUP TO WEIGHT QUEUE LENGTHS
        LOBLC = LOBL(LNEXT(IV))
        IF ( LOBLC . EQ . 0 )                    THEN
          LOBLL  = 0
          LOBLR  = 0
          CLTURN = 0
        ELSE
          LOBLL  = NLL  (LOBLC)
          LOBLR  = NLR  (LOBLC)
          CLTURN = LTURN(LOBLC)
C-----    IF THERE IS AN LOBLL LANE AND IF THE VEHICLE IS NOT ALLOWED ON
C-----    THE LOBLL LANE THEN SET NO LOBLL LANE
          IF ( LOBLL . GT . 0 )                  THEN
            IF ( IAND( VEHTYP(IV),LAVT(LOBLL) ) . EQ . 0 )
     *                                           LOBLL = 0
          END IF
C-----    IF THERE IS AN LOBLR LANE AND IF THE VEHICLE IS NOT ALLOWED ON
C-----    THE LOBLR LANE THEN SET NO LOBLR LANE
          IF ( LOBLR . GT . 0 )                  THEN
            IF ( IAND( VEHTYP(IV),LAVT(LOBLR) ) . EQ . 0 )
     *                                           LOBLR = 0
          END IF
        END IF
        IF ( LOBLL . EQ . 0 )                    THEN
          LLTURN = 0
        ELSE
          LLTURN = LTURN(LOBLL)
        END IF
C[      IF ( LLTURN           .EQ.-2147483647   )STOP 'DELAY  LLTURN 01'
        IF ( LOBLR . EQ . 0 )                    THEN
          RLTURN = 0
        ELSE
          RLTURN = LTURN(LOBLR)
        END IF
C[    IF ( RLTURN             .EQ.-2147483647   )STOP 'DELAY  RLTURN 01'
C-----END OF SETUP
C;    IF ( (IAND( IPRTLO(IV),1 ).NE.0) . AND . (TIME.GE.TPRINT) )
C;   *WRITE (6,602) IPT(INT2P(IV)),LLTURN,CLTURN,RLTURN
C;602 FORMAT(' IPT=',I2,' LLTURN=',I2,' CLTURN=',I2,' RLTURN=',I2)
C-----START CHECKING TURN CODE OF PATH 2 AS LEFT (IPT=4)
C-----NORMALLY ADD PENALTIES LEFT=+0 CURRENT=+10 RIGHT=+20
C-----NORMALLY ADD PENALTIES LEFT=+0 CURRENT=+ 3 RIGHT=+10 DUAL LEFTS
        IF ( IPT(INT2P(IV)) . EQ . LTURNL )      THEN
          QUEA = 10.0D0
          IF ( (.NOT. TURNL(LLTURN+1)) )         THEN
C[          IF ( QUEL         .EQ.-2147483647.0 )STOP 'DELAY  QUEL   01'
            QUEL = QUEL + QUEA
            QUEA = QUEA + 10.0D0
          END IF
          IF ( (.NOT. TURNL(CLTURN+1)) )         THEN
C[          IF ( QUEC         .EQ.-2147483647.0 )STOP 'DELAY  QUEC   01'
            QUEC = QUEC + QUEA
            QUEA = QUEA + 10.0D0
          END IF
          IF ( (.NOT. TURNL(RLTURN+1)) )         THEN
C[          IF ( QUER         .EQ.-2147483647.0 )STOP 'DELAY  QUER   01'
            QUER = QUER + QUEA
          END IF
          IF ( (LLTURN . GT . 0) . AND .
     *         (CLTURN . GT . 0) . AND .
     *         TURNL(LLTURN+1)   . AND .
     *         TURNL(CLTURN+1)   )               THEN
C[          IF ( QUEC         .EQ.-2147483647.0 )STOP 'DELAY  QUEC   02'
            QUEC = QUEC + 3.0D0
C[          IF ( QUER         .EQ.-2147483647.0 )STOP 'DELAY  QUER   02'
            QUER = QUER + 10.0D0
          END IF
          IF ( (CLTURN . GT . 0) . AND .
     *         (RLTURN . GT . 0) . AND .
     *         TURNL(CLTURN+1)   . AND .
     *         TURNL(RLTURN+1)   )               THEN
C[          IF ( QUER         .EQ.-2147483647.0 )STOP 'DELAY  QUER   03'
            QUER = QUER + 3.0D0
          END IF
        END IF
C-----START CHECKING TURN CODE OF PATH 2 AS RIGHT (IPT=1)
C-----NORMALLY ADD PENALTIES RIGHT=+0 CURRENT=+10 LEFT=+20
C-----NORMALLY ADD PENALTIES RIGHT=+0 CURRENT=+ 3 LEFT=+10 DUAL RIGHTS
        IF ( IPT(INT2P(IV)) . EQ . 1 )           THEN
          QUEA = 10.0D0
          IF ( (.NOT. TURNR(RLTURN+1)) )         THEN
C[          IF ( QUER         .EQ.-2147483647.0 )STOP 'DELAY  QUER   04'
            QUER = QUER + QUEA
            QUEA = QUEA + 10.0D0
          END IF
          IF ( (.NOT. TURNR(CLTURN+1)) )         THEN
C[          IF ( QUEC         .EQ.-2147483647.0 )STOP 'DELAY  QUEC   03'
            QUEC = QUEC + QUEA
            QUEA = QUEA + 10.0D0
          END IF
          IF ( (.NOT. TURNR(LLTURN+1)) )         THEN
C[          IF ( QUEL         .EQ.-2147483647.0 )STOP 'DELAY  QUEL   02'
            QUEL = QUEL + QUEA
          END IF
          IF ( (RLTURN . GT . 0) . AND .
     *         (CLTURN . GT . 0) . AND .
     *         TURNR(RLTURN+1)   . AND .
     *         TURNR(CLTURN+1)   )               THEN
C[          IF ( QUEC         .EQ.-2147483647.0 )STOP 'DELAY  QUEC   04'
            QUEC = QUEC + 3.0D0
C[          IF ( QUEL         .EQ.-2147483647.0 )STOP 'DELAY  QUEL   03'
            QUEL = QUEL + 10.0D0
          END IF
          IF ( (CLTURN . GT . 0) . AND .
     *         (LLTURN . GT . 0) . AND .
     *         TURNR(CLTURN+1)   . AND .
     *         TURNR(LLTURN+1)   )               THEN
C[          IF ( QUEL         .EQ.-2147483647.0 )STOP 'DELAY  QUEL   04'
            QUEL = QUEL + 3.0D0
          END IF
        END IF
C-----START CHECKING TURN CODE OF PATH 2 AS STRAIGHT (IPT=2)
C-----NORMALLY ADD PENALTIES CURRENT=+0 RIGHT=+5 LEFT=+10
        IF ( IPT(INT2P(IV)) . EQ . 2 )           THEN
          QUEA = 5.0D0
          IF ( (.NOT. TURNS(CLTURN+1)) )         THEN
C[          IF ( QUEC         .EQ.-2147483647.0 )STOP 'DELAY  QUEC   05'
            QUEC = QUEC + QUEA
            QUEA = QUEA + 5.0D0
          END IF
          IF ( (.NOT. TURNS(RLTURN+1)) )         THEN
C[          IF ( QUER         .EQ.-2147483647.0 )STOP 'DELAY  QUER   05'
            QUER = QUER + QUEA
            QUEA = QUEA + 5.0D0
          END IF
          IF ( (.NOT. TURNS(LLTURN+1)) )         THEN
C[          IF ( QUEL         .EQ.-2147483647.0 )STOP 'DELAY  QUEL   05'
            QUEL = QUEL + QUEA
          END IF
        END IF
      END IF
C-----IF THE EQUIVALENT NUMBER OF VEHICLES IN THE QUEUE IN THIS LANE IS
C-----LE THE EQUIVALENT NUMBER OF VEHICLES IN THE QUEUE IN THE LANE TO
C-----THE LEFT AND IN THE LANE TO THE RIGHT THEN GO TO 6020 AND SET NO
C-----LANE CHANGE DESIRABLE
C;    IF ( (IAND( IPRTLO(IV),1 ).NE.0) . AND . (TIME.GE.TPRINT) )
C;   *WRITE (6,603) QUEL,QUEC,QUER
C;603 FORMAT(' QUEL=',F6.1,' QUEC=',F6.1,' QUER=',F6.1)
C[    IF ( QUEC               .EQ.-2147483647.0 )STOP 'DELAY  QUEC   06'
C[    IF ( QUEL               .EQ.-2147483647.0 )STOP 'DELAY  QUEL   06'
C[    IF ( QUER               .EQ.-2147483647.0 )STOP 'DELAY  QUER   06'
      QUEL = DMIN1( QUEL,POSBIG )
      QUEC = DMIN1( QUEC,POSBIG )
      QUER = DMIN1( QUER,POSBIG )
      IF ((QUEC.LE.QUEL  ).AND.(QUEC.LE.QUER  )) GO TO 6020
      IF ((QUEL.GE.POSBIG).AND.(QUER.GE.POSBIG)) GO TO 6020
C-----LESS DELAY CAN BE EXPECTED IF THIS VEHICLE WOULD CHANGE LANES THUS
C-----IF THE EQUIVALENT NUMBER OF VEHICLES IN THE QUEUE IN THE LANE TO
C-----THE LEFT IS LE THE EQUIVALENT NUMBER OF VEHICLES IN THE QUEUE IN
C-----THE LANE TO THE RIGHT THEN RETURN WITH THE POSITION AND INDEX OF
C-----THE LEAD AND LAG VEHICLES IN THE LEFT LANE SET AND TRY TO CHANGE
C-----LANES ELSE SET THE POSITION AND THE INDEX OF THE LEAD AND LAG
C-----VEHICLES FOR THE RIGHT LANE AND TRY TO CHANGE LANES
                    IF ( QUEL . LE . QUER )      RETURN
      ISIDE = 3
C[    IF ( NORF               .EQ.-2147483647   )STOP 'DELAY  NORF   01'
      NOSF  = NORF
      NOSFS = NORFS
C[    IF ( NORR               .EQ.-2147483647   )STOP 'DELAY  NORR   01'
      NOSR  = NORR
      NOSRS = NORRS
C[    IF ( PVRF               .EQ.-2147483647.0 )STOP 'DELAY  PVRF   01'
      PVSF  = PVRF
C[    IF ( PVRR               .EQ.-2147483647.0 )STOP 'DELAY  PVRR   01'
      PVSR  = PVRR
C[    IF ( VVRF               .EQ.-2147483647.0 )STOP 'DELAY  VVRF   01'
      VVSF  = VVRF
C[    IF ( VVRR               .EQ.-2147483647.0 )STOP 'DELAY  VVRR   01'
      VVSR  = VVRR
C[    IF ( AVRF               .EQ.-2147483647.0 )STOP 'DELAY  AVRF   01'
      AVSF  = AVRF
C[    IF ( AVRR               .EQ.-2147483647.0 )STOP 'DELAY  AVRR   01'
      AVSR  = AVRR
C[    IF ( SVRF               .EQ.-2147483647.0 )STOP 'DELAY  SVRF   01'
      SVSF  = SVRF
C[    IF ( SVRR               .EQ.-2147483647.0 )STOP 'DELAY  SVRR   01'
      SVSR  = SVRR
      RETURN
 6020 CONTINUE
C-----SET NO LANE CHANGE DESIRABLE FLAG AND RETURN
      ISIDE = 2
      NOSF  = 0
      NOSFS = 0
      NOSR  = 0
      NOSRS = 0
      RETURN
      END                                                               DELAY
C
C
C
      SUBROUTINE CKLALT
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INTEGER           I,ILL,ILR,IPATH,LOBAPD
C
C-----SUBROUTINE CKLALT CHECKS THE LANE ALTERNATIVES FOR THIS LANE
C
C[    I          = -2147483647
C[    ILL        = -2147483647
C[    ILR        = -2147483647
C[    IPATH      = -2147483647
C[    LOBAPD     = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CKLALT'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      LALTPL(IV) = 0
      LALTPR(IV) = 0
      LOBAPD = IABS( NOBAPD(IV) )
      IF ( IAFLAG(IA) . EQ . IAFLAG(LOBAPD) )    GO TO 1005
      IF ( IAFLAG(IA) . EQ . ILETTI         )    GO TO 1005
      IF ( NOBAPD(IV) . LE . 0 )                 THEN
        LOBAPD = INTLNU(LOBAPD)
      ELSE
        LOBAPD = INTLNK(LOBAPD)
      END IF
 1005 CONTINUE
      ILL = NLL(IL)
      ILR = NLR(IL)
C-----IF THERE IS AN ILL LANE AND IF THE VEHICLE IS NOT ALLOWED ON THE
C-----ILL LANE THEN SET NO ILL LANE
      IF ( ILL . GT . 0 )                        THEN
        IF ( IAND( VEHTYP(IV),LAVT(ILL) ).EQ.0 ) ILL = 0
      END IF
C-----IF THERE IS AN ILR LANE AND IF THE VEHICLE IS NOT ALLOWED ON THE
C-----ILR LANE THEN SET NO ILR LANE
      IF ( ILR . GT . 0 )                        THEN
        IF ( IAND( VEHTYP(IV),LAVT(ILR) ).EQ.0 ) ILR = 0
      END IF
C-----INITIALIZE THE LANE ALTERNATIVES FOR NO LANE ALTERNATIVE
      LALT(IV) = 1
C-----IF THERE IS NO LANE TO THE RIGHT THEN GO TO 2010 AND CHECK THE
C-----LANE TO THE LEFT
                    IF ( ILR . EQ . 0 )          GO TO 2010
C-----IF THERE ARE NO PATHS INTO THE INTERSECTION FROM THE LANE TO THE
C-----RIGHT THEN GO TO 2010 AND CHECK THE LANE TO THE LEFT
                    IF ( NPINT(ILR) . EQ . 0 )   GO TO 2010
C-----CHECK EACH INTERSECTION PATH FROM THE LANE TO THE RIGHT TO SEE IF
C-----IT GOES TO THE VEHICLES DESIRED OUTBOUND APPROACH
      DO 1010  I = 1 , NPINT(ILR)
      IPATH = LINTP(I,ILR)
C-----IF THE INTERSECTION PATH DOES NOT ALLOW THE VEHICLE TYPE THEN GO
C-----TO 1010 AND SKIP TO THE NEXT INTERSECTION PATH
      IF ( IAND( VEHTYP(IV),PAVT(IPATH) ).EQ.0 ) GO TO 1010
C-----IF THE INTERSECTION PATH BEING CHECKED CHANGES LANES WITHIN THE
C-----INTERSECTION THEN GO TO 1010 AND SKIP TO THE NEXT INTERSECTION
C-----PATH
                    IF ( ILCH(IPATH) . NE . 0 )  GO TO 1010
C-----IF THE LINKING OUTBOUND APPROACH FOR THE INTERSECTION PATH IS EQ
C-----TO THE DESIRED OUTBOUND APPROACH FOR THIS VEHICLE THEN GO TO 1020
C-----AND SET THE LANE TO THE RIGHT AS A LANE ALTERNATIVE
C[    IF ( LOBAPD             .EQ.-2147483647   )STOP 'CKLALT LOBAPD 01'
                    IF ( IOA(IPATH) .EQ. LOBAPD )GO TO 1020
 1010 CONTINUE
C-----NONE OF THE INTERSECTION PATHS FROM THE LANE TO THE RIGHT GOES TO
C-----THE VEHICLES DESIRED OUTBOUND APPROACH THUS GO TO 2010 AND CHECK
C-----THE LANE TO THE LEFT
      GO TO 2010
 1020 CONTINUE
C-----SET THE LANE TO THE RIGHT AS A LANE ALTERNATIVE
      LALT  (IV) = LALT(IV) + 1
      LALTPR(IV) = IPATH
 2010 CONTINUE
C-----IF THERE IS NO LANE TO THE LEFT THEN RETURN
C[    IF ( ILL                .EQ.-2147483647   )STOP 'CKLALT ILL    01'
                    IF ( ILL . EQ . 0 )          RETURN
C-----IF THERE ARE NO PATHS INTO THE INTERSECTION FROM THE LANE TO THE
C-----LEFT THEN RETURN
                    IF ( NPINT(ILL) . EQ . 0 )   RETURN
C-----CHECK EACH INTERSECTION PATH FROM THE LANE TO THE LEFT TO SEE IF
C-----IT GOES TO THE VEHICLES DESIRED OUTBOUND APPROACH
      DO 2020  I = 1 , NPINT(ILL)
      IPATH = LINTP(I,ILL)
C-----IF THE INTERSECTION PATH DOES NOT ALLOW THE VEHICLE TYPE THEN GO
C-----TO 2020 AND SKIP TO THE NEXT INTERSECTION PATH
      IF ( IAND( VEHTYP(IV),PAVT(IPATH) ).EQ.0 ) GO TO 2020
C-----IF THE INTERSECTION PATH BEING CHECKED CHANGES LANES WITHIN THE
C-----INTERSECTION THEN GO TO 2020 AND SKIP TO THE NEXT INTERSECTION
C-----PATH
                    IF ( ILCH(IPATH) . NE . 0 )  GO TO 2020
C-----IF THE LINKING OUTBOUND APPROACH FOR THE INTERSECTION PATH IS EQ
C-----TO THE DESIRED OUTBOUND APPROACH FOR THIS VEHICLE THEN GO TO 2030
C-----AND SET THE LANE TO THE LEFT AS A LANE ALTERNATIVE
C[    IF ( LOBAPD             .EQ.-2147483647   )STOP 'CKLALT LOBAPD 02'
                    IF ( IOA(IPATH) .EQ. LOBAPD )GO TO 2030
 2020 CONTINUE
C-----NONE OF THE INTERSECTION PATHS FROM THE LANE TO THE LEFT GOES TO
C-----THE VEHICLES DESIRED OUTBOUND APPROACH THUS RETURN
      RETURN
 2030 CONTINUE
C-----SET THE LANE TO THE LEFT AS A LANE ALTERNATIVE
      LALT  (IV) = LALT(IV) + 2
      LALTPL(IV) = IPATH
      RETURN
      END                                                               CKLALT
C
C
C
      SUBROUTINE GAPACC ( LANSI,IFLCHG )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           EVRESP,LAGAPA,LEGAPA
      INTEGER           I,IFLCHG,JSISET,LANSI,GETLCV
      DOUBLE PRECISION  A,ACCVEH,ALAGAP,ALEGAP,AME,AVR,B,C,CRISLP,
     *                  DESSPD,DISLAN,DISLCH,DISVEH,DSPLCH,DVFACT,FACT,
     *                  FLENV,GAPLA,GAPLE,GAPMIN,GAPNEW,GAPOLD,GFACT,
     *                  PME,POSLAT,PVR,RELNEW,RESPLA,RESPLE,SLOPE,SME,
     *                  SVR,T,TLDIST,TM,TMAX,TPOS,TS,TSTP,VELLCH,VEHLNG,
     *                  VELMIN,VM,VME,VMET,VS,VVR,VVRT,XCRIT,XM,XPOS,XS,
     *                  XTOT
      DATA     GAPMIN / 8.0D0 /
      DATA     VELMIN / 3.0D0 /
  601 FORMAT('** LANE CHANGE WARNING  AT T=',F7.2,' SECS VEH=',I6,
     *       ' APPR=',I2,' LANE=',I1,
     *       ' - LANE CHANGE NEEDS ',I3,' FT IN FIRST PART OF LANE',1X,
     *       '**  WARNING  **')
C4701 FORMAT('  RESPLE  ALEGAP   GAPLE  RESPLA  ALAGAP   GAPLA ISET',
C4   *       ' NOSF    PVSF    VVSF    AVSF    SVSF NOSR    PVSR   ',
C4   *       ' VVSR    AVSR    SVSR',/,6F8.2,2I5,4F8.2,I5,4F8.2)
C4702 FORMAT(' TM=',F7.2,' TS=',F7.2,' XM=',F7.2,' XS=',F7.2,
C4   *       ' XCRIT=',F7.2,' GAPLE =',F7.2)
C4703 FORMAT(' TM=',F7.2,' TS=',F7.2,' XM=',F7.2,' XS=',F7.2,
C4   *       ' XCRIT=',F7.2,' GAPLA =',F7.2)
C4704 FORMAT(' TM=',F7.2,' TS=',F7.2,' XM=',F7.2,' XS=',F7.2,
C4   *       ' XCRIT=',F7.2,' GAPLE =',F7.2,' FOR ACCEL AND ISET=3')
C
C-----SUBROUTINE GAPACC CHECKS IF THERE IS AN ACCEPTABLE GAP TO LANE
C-----CHANGE INTO AND IF NOT THEN DETERMINE THE APPROPRIATE DRIVER
C-----RESPONSE FOR LANE CHANGING
C
C[    I          = -2147483647
C[    JSISET     = -2147483647
C[    A          = -2147483647.0
C[    ACCVEH     = -2147483647.0
C[    ALAGAP     = -2147483647.0
C[    ALEGAP     = -2147483647.0
C[    AME        = -2147483647.0
C[    AVR        = -2147483647.0
C[    B          = -2147483647.0
C[    C          = -2147483647.0
C[    CRISLP     = -2147483647.0
C[    DISLAN     = -2147483647.0
C[    DISVEH     = -2147483647.0
C[    FACT       = -2147483647.0
C[    FLENV      = -2147483647.0
C[    GAPLA      = -2147483647.0
C[    GAPLE      = -2147483647.0
C[    GAPNEW     = -2147483647.0
C[    GAPOLD     = -2147483647.0
C[    GFACT      = -2147483647.0
C[    PME        = -2147483647.0
C[    PVR        = -2147483647.0
C[    RESPLA     = -2147483647.0
C[    RESPLE     = -2147483647.0
C[    SLOPE      = -2147483647.0
C[    SME        = -2147483647.0
C[    SVR        = -2147483647.0
C[    TM         = -2147483647.0
C[    TS         = -2147483647.0
C[    VM         = -2147483647.0
C[    VME        = -2147483647.0
C[    VMET       = -2147483647.0
C[    VS         = -2147483647.0
C[    VVR        = -2147483647.0
C[    VVRT       = -2147483647.0
C[    XCRIT      = -2147483647.0
C[    XM         = -2147483647.0
C[    XS         = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'GAPACC'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----IF THE LANE ON THE SIDE OF INTEREST IS INVALID THEN GO TO 5025 AND
C-----REJECT THE GAP
                    IF ( LANSI . LE . 0 )        GO TO 5025
C-----IF THE VEHICLE IS NOT ALLOWED ON THE LANE ON THE SIDE OF INTEREST
C-----THEN GO TO 5025 AND REJECT THE GAP
      IF ( IAND( VEHTYP(IV),LAVT(LANSI) ).EQ.0 ) GO TO 5025
C-----INITIALIZE SOME PARAMETERS FOR CHECKING FOR A GAP
      FACT = FACTOR*DCHAR(IDRICL(IV))*VCHAR(IVEHCL(IV))
      CRISLP = SLPMAX*DCHAR(IDRICL(IV))
      IFLCHG = 0
C-----IF THERE IS A LEAD VEHICLE ON LANE ON THE SIDE OF INTEREST THEN GO
C-----TO 1020
                    IF ( NOSF . GT . 0 )         GO TO 1020
C-----IF TURN IS LEGAL FROM CURRENT LANE GO TO 1020
                    IF ( LEGAL(IV) . EQ . 2 )    GO TO 1020
C-----IF INTERSECTION IS CONTROLLED BUT LANE IS UNCONTROLLED GO TO 1020
      IF ( (ICONTR.GT.ICUNCT) .AND. (GETLCV ( IV,LANSI ).EQ.LCUNCT) )
     *                                           GO TO 1020
C-----IF LANE IS NOT SIGNAL CONTROLLED GO TO 1010
            IF ( GETLCV ( IV,LANSI ).LE.LCSTOP ) GO TO 1010
      JSISET = ISISET(ICAMPC,IBLN(LANSI))
C-----IF LANE SIGNAL CONTROL IS GREEN GO TO 1020
                    IF ( JSISET . EQ . 01 )      GO TO 1020
                    IF ( JSISET . EQ . 04 )      GO TO 1020
C-----IF CHANGING LEFT AND SIGNAL IS PROTECTED LEFT GO TO 1020
      IF ( (ISIDE.EQ.1) . AND . (JSISET.GE.23) ) GO TO 1020
C-----OTHERWISE GO TO 1010
                    IF ( ISIDE . EQ . 1 )        GO TO 1010
C-----VEHICLE CHANGING RIGHT AND CHECKS FOR RIGHT TURN SIGNAL GREEN
                    IF ( JSISET . EQ . 07 )      GO TO 1020
                    IF ( JSISET . EQ . 09 )      GO TO 1020
                    IF ( JSISET . EQ . 13 )      GO TO 1020
                    IF ( JSISET . EQ . 15 )      GO TO 1020
                    IF ( JSISET . EQ . 17 )      GO TO 1020
                    IF ( JSISET . EQ . 18 )      GO TO 1020
                    IF ( JSISET . EQ . 23 )      GO TO 1020
 1010 CONTINUE
C-----SET UP MINIMUM ACCEPTABLE LEAD VEHICLE PARAMETERS FOR LANE CHANGE
      VVSF = VELMIN
 1020 CONTINUE
C-----IF THERE IS A PREVIOUS VEHICLE IN THE CURRENT LANE THEN CHECK TO
C-----SEE IF THERE IS ENOUGH CLEARANCE DISTANCE SO THAT THIS VEHICLE CAN
C-----BE TOTALLY IN THE NEW LANE BEFORE THIS VEHICLES FRONT BUMPER
C-----REACHES THE REAR BUMPER OF THE PREVIOUS VEHICLE IN THE CURRENT
C-----LANE
      IF ( ( IVPV                     .EQ. 0 ) . OR .
     *     ( MAJCLC                          ) . OR .
     *     ( IAND( VEHTYP(IV),LAVTE ) .NE. 0 ) ) THEN
        GO TO 1030
      END IF
C-----IF THERE IS NOT SUFFICIENT CLEARANCE DISTANCE FROM THE VEHICLE
C-----AHEAD IN THE SAME LANE (THIS VEHICLES WIDTH) THEN REJECT THE GAP
C-----AND CONTINUE NORMALLY
      DVFACT = DCHAR(IDRICL(IV))*VCHAR(IVEHCL(IV))
      TLDIST = 0.5D0*(LWID(IL)+LWID(LANSI))
C-----DEFINE THE LENGTH OF THE LANE CHANGE TO BE TIMELC SECONDS AT THE
C-----VELOCITY OF THE VEHICLE WITH A MINIMUM OF 1.5 VEHICLE LENGTHS
C-----WITH A MAXIMUM VEHICLE LENGTH OF 25.0 FEET
      DSPLCH = DBLE( ISPD(IV) )
      IF ( IDISPD(IV) )                          THEN
        DSPLCH = 0.5D0*DSPLCH
      END IF
      IF ( ISPDP (IV) . EQ . 1 )                 THEN
        DSPLCH = DSPLCH*DBLE( ISLIM(IA) )/DBLE( LIMP(LNEXT(IV)) )
      END IF
      VELLCH = 0.2D0*DSPLCH
      VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
      VEHLNG = DMIN1( 25.0D0,LENVAP )
      DISLCH = 0.5D0*(ENDLN-POSNEW)
      DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
      DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
      XTOT   = DISLCH/DVFACT
      POSLAT = 0.5D0*(WIDV(IVEHCL(IV))+WIDV(IVEHCL(IVPV)))
C-----DEFINE THE POSITION ON THE COSINE CURVE AT POSLAT
      XPOS   = XTOT*DACOS( 2.0D0*DABS( POSLAT )/TLDIST-1.0D0 )/PI
      IF ( VELLCH . GT . 0.0D0 )                 THEN
        TPOS = XPOS/VELLCH
      ELSE
        TPOS = TIMELC*XPOS/XTOT
      END IF
      TMAX = 30.0D0
      CALL  TIMSTP  ( PVVEL,PVACC,PVSLP,TMAX,TSTP )
      IF ( TSTP . NE . TIMERR )                  THEN
        TPOS = DMIN1( TPOS,TSTP )
      END IF
      RELNEW = RELPOS +
     * DMAX1( PVVEL*TPOS+0.5D0*PVACC*TPOS**2+ONED6*PVSLP*TPOS**3,0.0D0 )
      IF ( RELNEW . LE . XPOS )                  THEN
C-----  REJECT THE GAP AND CONTINUE NORMALLY
        ISET(IV) = 5
        RETURN
      END IF
 1030 CONTINUE
C-----FIND THE ACCEPTABLE LEAD GAP AND THE ACTUAL LEAD GAP
      RESPLE = VELNEW - VVSF
      ALEGAP = (2.0D0+0.7D0*VELNEW+(DABS(RESPLE)*RESPLE*0.05D0))/FACT
      ALEGAP = DMAX1( ALEGAP,GAPMIN/DCHAR(IDRICL(IV)) )
      GAPLE  = PVSF - POSNEW
      IF ( VVSF . GT . VELNEW )                  THEN
        GFACT = DMAX1( DMIN1( 1.0D0-((VVSF-VELNEW)/10.0D0),1.0D0 ),
     *                 0.0D0                                        )
        ALEGAP = GFACT*ALEGAP
      END IF
      IF ( ( MAJCLC                          ) . OR .
     *     ( IAND( VEHTYP(IV),LAVTE ) .NE. 0 ) ) THEN
        ALEGAP = 0.0D0
      END IF
      LEGAPA = .FALSE.
C-----IF THE ACTUAL LEAD GAP IS GE THE ACCEPTABLE LEAD GAP THEN SET THE
C-----LEAD GAP OK FLAG
                    IF ( GAPLE . GE . ALEGAP )   LEGAPA = .TRUE.
C-----FIND THE ACCEPTABLE LAG GAP AND THE ACTUAL LAG GAP
      RESPLA = VVSR - VELNEW
      ALAGAP = (XRELMI+1.4D0*VELNEW+(DABS(RESPLA)*RESPLA*0.10D0))/FACT
      ALAGAP = DMAX1( ALAGAP,GAPMIN/DCHAR(IDRICL(IV)) )
      GAPLA  = POSNEW - LENVAP - XRELMI - PVSR
      IF ( VELNEW . GT . VVSR )                  THEN
        GFACT = DMAX1( DMIN1( 1.0D0-((VELNEW-VVSR)/10.0D0),1.0D0 ),
     *                 0.0D0                                        )
        ALAGAP = GFACT*ALAGAP
      END IF
      IF ( ( MAJCLC                          ) . OR .
     *     ( IAND( VEHTYP(IV),LAVTE ) .NE. 0 ) ) THEN
        ALAGAP = 0.0D0
      END IF
      LAGAPA = .FALSE.
C-----IF THE ACTUAL LAG GAP IS GE THE ACCEPTABLE LAG GAP THEN SET THE
C-----LAG GAP OK FLAG
                    IF ( GAPLA . GE . ALAGAP )   LAGAPA = .TRUE.
C-----IF THE VEHICLE IS IN THE FIRST PART OF A BLOCKED LANE AND THE LEAD
C-----GAP IS OK THEN IF 0.75 TIMES THE LENGTH OF THE FIRST PART OF A
C-----BLOCKED LANE IS LE THE LENGTH OF THE VEHICLE PLUS THE ACCEPTABLE
C-----LAG GAP AND THE VEHICLE HAS BEEN PROCESSED AT LEAST ONE DT AND THE
C-----LAG GAP IS NOT OK AND THERE IS NO NOSR VEHICLE THEN PRINT WARNING
C-----MESSAGE FOR LANE TOO SHORT FOR LANE CHANGE AND SET THE LAG GAP OK
C-----FLAG (THE VEHICLE WILL NEVER HAVE AN ACCEPTABLE LAG GAP BECUASE
C-----THE FIRST PART OF THE BLOCKED LANE IS TOO SHORT)
      IF ( (POSNEW . LT . (LENVAP+DBLE( LGEOM(2,IL) ))) . AND .
     *     (MBLOCK(IV)                                ) . AND .
     *     (LEGAPA                                    ) )
     *                                           THEN
        DISLAN = 0.75D0*DBLE( LGEOM(2,IL)-LGEOM(1,IL) )
        DISVEH = LENVAP + XRELMI + ALAGAP
        IF ( (DISLAN . LE . DISVEH) . AND .
     *       (ITIMV(IV) . GE . 1  ) . AND .
     *       (.NOT. LAGAPA        ) . AND .
     *       (NOSR . EQ . 0       ) )            THEN
          WRITE (WRNMSG,601) TIME,IQ(IV),IA,ILN,
     *                       IDINT( (DISVEH/0.75D0)+0.9999D0 )
          CALL  PRTWRN  ( WRNMSG )
          IF ( IQ(IV) . LT . 0 )                 THEN
            CALL  VDIERR  ( TIME,-IQ(IV),VDILCL )
          END IF
          RESPLA = 0.0D0
          GAPLA  = ALAGAP
          LAGAPA = .TRUE.
        END IF
      END IF
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 101
C4                  IF ( TIME . LT . TPRINT )    GO TO 101
C[    IF ( ALEGAP             .EQ.-2147483647.0 )STOP 'GAPACC ALEGAP 01'
C[    IF ( GAPLA              .EQ.-2147483647.0 )STOP 'GAPACC GAPLA  01'
C[    IF ( GAPLE              .EQ.-2147483647.0 )STOP 'GAPACC GAPLE  01'
C[    IF ( RESPLA             .EQ.-2147483647.0 )STOP 'GAPACC RESPLA 01'
C[    IF ( RESPLE             .EQ.-2147483647.0 )STOP 'GAPACC RESPLE 01'
C4    WRITE (6,701) RESPLE,ALEGAP,GAPLE,RESPLA,ALAGAP,GAPLA,ISET(IV),
C4   *              NOSF,PVSF,VVSF,AVSF,SVSF,NOSR,PVSR,VVSR,AVSR,SVSR
C4101 CONTINUE
C-----IF THIS VEHICLE IS THE FIRST VEHICLE IN THE LANE AND THE LANE IS
C-----BLOCKED THEN DO NOT TEST VELOCITY ELSE IF THIS VEHICLES VELOCITY
C-----IS BELOW VELMIN THEN GO TO 5020 AND REJECT THE GAP
      IF ( (.NOT. (MFINL(IV) .AND. MBLOCK(IV))) . AND .
     *     (VELNEW . LT . VELMIN              ) . AND .
     *     (.NOT. MAJCLC                      ) . AND .
     *     (IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) )GO TO 5020
C-----IF THE LEAD GAP IS NOT OK AND THE LEAD VEHICLE IS ALMOST STOPPED
C-----THEN GO TO 5020 AND REJECT THE GAP
      IF ( (.NOT. LEGAPA                     ) . AND .
     *     (VVSF . LT . VELMIN               ) . AND .
     *     (.NOT. MAJCLC                     ) . AND .
     *     (IAND( VEHTYP(IV),LAVTE ) . EQ . 0) ) GO TO 5020
C-----IF THE LAG GAP IS NOT OK THEN GO TO 5010 AND CHECK THE LEAD GAP
                    IF ( (.NOT. LAGAPA) )        GO TO 5010
C-----IF THE LEAD GAP IS NOT OK WHEN THE LAG GAP IS OK THEN GO TO 4010
C-----AND REJECT THE GAP
                    IF ( (.NOT. LEGAPA) )        GO TO 4010
C-----BOTH THE LEAD GAP AND THE LAG GAP ARE OK THUS CHECK TO SEE THAT
C-----THERE WILL NOT BE A COLLISION IF THIS VEHICLE CHANGES LANES
C[    IF ( RESPLE             .EQ.-2147483647.0 )STOP 'GAPACC RESPLE 02'
                    IF ( RESPLE . LT . VELMIN )  GO TO 2010
C-----FIND THE RELATIVE DISTANCE REQUIRED FOR THIS VEHICLE TO DECELERATE
C-----TO THE LEAD VEHICLES SPEED (LEAD VEHICLE IS NEW VALUES WHEREAS
C-----THIS VEHICLE IS OLD VALUES THEREFORE LEAD VEHICLE IS ONE DT AHEAD
C-----OF THIS VEHICLE)
      IF ( VVSF . LE . 0.0D0 )                   THEN
        SLOPE = -CRISLP
      ELSE
        SLOPE = -0.75D0*CRISLP
      END IF
                    IF ( LEGAL(IV) . NE . 2 )    SLOPE = 1.50D0*SLOPE
                    IF ( SLOPE . EQ . SVSF )     SLOPE = 1.01D0*SLOPE
C-----CALCULATE THE TIME FOR THIS VEHICLE TO DECELERATE TO THE LEAD
C-----VEHICLES SPEED
C-----(VVSF                          )+(AVSF           )*T+SVSF *T**2/2=
C-----(VELOLD+ACCOLD*DT+SLOPE*DT**2/2)+(ACCOLD+SLOPE*DT)*T+SLOPE*T**2/2
C-----(0.5*(SLOPE                           - SVSF))*T**2 +
C-----(ACCOLD +  SLOPE*DT                   - AVSF )*T    +
C-----(VELOLD + ACCOLD*DT + 0.5*SLOPE*DT**2 - VVSF )      = 0
      A    = 0.5D0*(SLOPE                          - SVSF)
      B    = ACCOLD + SLOPE *DT                    - AVSF
      C    = VELOLD + ACCOLD*DT + 0.5D0*SLOPE*DTSQ - VVSF
      TMAX = 30.0D0
      CALL  TMQUAD  ( A,B,C,TMAX,TS )
      IF ( TS . EQ . TIMERR )                    THEN
        IF ( DABS( C ) . LE . VSMALL )           THEN
          TS = 0.0D0
        ELSE
          LEGAPA = .FALSE.
          GO TO 4010
        END IF
      END IF
      TM = TS + DT
      VS = VVSF + AVSF*TS + 0.5D0*SVSF*TS**2
      IF ( VS . LE . 0.0D0 )                     THEN
        TMAX = 30.0D0
        CALL  TIMSTP  ( VVSF,AVSF,SVSF,TMAX,T )
        IF ( T . NE . TIMERR )                   THEN
          TS = T
          XS = VVSF*TS + 0.5D0*AVSF*TS**2 + ONED6*SVSF*TS**3
          SLOPE = -CRISLP
                    IF ( LEGAL(IV) . NE . 2 )    SLOPE = 1.5D0*SLOPE
          TMAX = 30.0D0
          CALL  TIMSTP  ( VELOLD,ACCOLD,SLOPE,TMAX,T )
          IF ( T . NE . TIMERR )                 THEN
            TM = T
            XM = VELOLD*TM + 0.5D0*ACCOLD*TM**2 + ONED6*SLOPE*TM**3
          END IF
        END IF
      ELSE
        XM = VELOLD*TM + 0.5D0*ACCOLD*TM**2 + ONED6*SLOPE*TM**3
        XS = VVSF  *TS + 0.5D0*AVSF  *TS**2 + ONED6*SVSF *TS**3
      END IF
      XCRIT = XM - XS
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 102
C4                  IF ( TIME . LT . TPRINT )    GO TO 102
C[    IF ( GAPLE              .EQ.-2147483647.0 )STOP 'GAPACC GAPLE  02'
C4    WRITE (6,702) TM,TS,XM,XS,XCRIT,GAPLE
C4102 CONTINUE
C-----IF THE ACTUAL LEAD GAP IS LT THE RELATIVE DISTANCE REQUIRED FOR
C-----THIS VEHICLE TO DECELERATE TO THE LEAD VEHICLES SPEED THEN GO TO
C-----4010 AND REJECT THE GAP
C[    IF ( GAPLE              .EQ.-2147483647.0 )STOP 'GAPACC GAPLE  03'
C[    IF ( XCRIT              .EQ.-2147483647.0 )STOP 'GAPACC XCRIT  01'
      IF ( GAPLE . LT . XCRIT )                  THEN
        LEGAPA = .FALSE.
        GO TO 4010
      END IF
 2010 CONTINUE
C[    IF ( RESPLA             .EQ.-2147483647.0 )STOP 'GAPACC RESPLA 02'
                    IF ( RESPLA . LT . VELMIN )  GO TO 3010
                    IF ( MBLOCK(IV) )            GO TO 2020
C-----FIND THE RELATIVE DISTANCE REQUIRED FOR THE LAG VEHICLE TO
C-----DECELERATE TO THIS VEHICLES SPEED (LAG VEHICLE IS NEW VALUES
C-----WHEREAS THIS VEHICLE IS OLD VALUES THEREFORE LAG VEHICLE IS ONE DT
C-----AHEAD OF THIS VEHICLE)
      SLOPE = -0.75D0*CRISLP
                    IF ( SLOPE . EQ . SLPOLD )   SLOPE = 1.01D0*SLOPE
C-----CALCULATE THE TIME FOR THE LAG VEHICLE TO DECELERATE TO THIS
C-----VEHICLES SPEED
C-----(VELOLD+ACCOLD*DT+SLPOLD*DT**2/2)+(ACCOLD+SLPOLD*DT)*T
C-----                                                   +SLPOLD*T**2/2=
C-----(VVSR                           )+(AVSR            )*T
C-----                                                   +SLOPE *T**2/2
C-----(0.5*(SLOPE - SLPOLD                         ))*T**2 +
C-----(AVSR  - ACCOLD - SLPOLD*DT                   )*T    +
C-----(VVSR  - VELOLD - ACCOLD*DT - 0.5*SLPOLD*DT**2)      = 0
      A    = 0.5D0*(SLOPE  - SLPOLD)
      B    = AVSR - ACCOLD - SLPOLD*DT
      C    = VVSR - VELOLD - ACCOLD*DT - 0.5D0*SLPOLD*DTSQ
      TMAX = 30.0D0
      CALL  TMQUAD  ( A,B,C,TMAX,TS )
      IF ( TS . EQ . TIMERR )                    THEN
        IF ( DABS( C ) . LE . VSMALL )           THEN
          TS = 0.0D0
        ELSE
          LAGAPA = .FALSE.
          GO TO 5010
        END IF
      END IF
      TM = TS + DT
      VM = VELOLD + ACCOLD*TM + 0.5D0*SLPOLD*TM**2
      IF ( VM . LE . 0.0D0 )                     THEN
        TMAX = 30.0D0
        CALL  TIMSTP  ( VELOLD,ACCOLD,SLPOLD,TMAX,T )
        IF ( T . NE . TIMERR )                   THEN
          TM = T
          XM = VELOLD*TM + 0.5D0*ACCOLD*TM**2 + ONED6*SLPOLD*TM**3
          TMAX = 30.0D0
          CALL  TIMSTP  ( VVSR,AVSR,SLOPE,TMAX,T )
          IF ( T . NE . TIMERR )                 THEN
            TS = T
            XS = VVSR*TS + 0.5D0*AVSR*TS**2 + ONED6*SLOPE*TS**3
          END IF
        END IF
      ELSE
        XM = VELOLD*TM + 0.5D0*ACCOLD*TM**2 + ONED6*SLPOLD*TM**3
        XS = VVSR  *TS + 0.5D0*AVSR  *TS**2 + ONED6*SLOPE *TS**3
      END IF
      XCRIT = XS - XM
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 103
C4                  IF ( TIME . LT . TPRINT )    GO TO 103
C[    IF ( GAPLA              .EQ.-2147483647.0 )STOP 'GAPACC GAPLA  02'
C4    WRITE (6,703) TM,TS,XM,XS,XCRIT,GAPLA
C4103 CONTINUE
C-----IF THE ACTUAL LAG GAP IS LT THE RELATIVE DISTANCE REQUIRED FOR THE
C-----LAG VEHICLE TO DECELERATE TO THIS VEHICLES SPEED THEN GO TO 6010
C-----AND CHECK TO SEE IF THIS VEHICLE CAN ACCELERATE FOR THE GAP ELSE
C-----GO TO 3010 AND INITIATE THE LANE CHANGE
C[    IF ( GAPLA              .EQ.-2147483647.0 )STOP 'GAPACC GAPLA  03'
C[    IF ( XCRIT              .EQ.-2147483647.0 )STOP 'GAPACC XCRIT  02'
      IF ( GAPLA . LT . XCRIT )                  THEN
        GO TO 6010
      ELSE
        GO TO 3010
      END IF
 2020 CONTINUE
C-----THIS IS A BLOCKED LANE THUS DETERMINE IF THE LAG VEHICLE WILL
C-----COLLIDE WITH THIS VEHICLE WITHIN 10 SECS WHILE THE LAG VEHICLE
C-----DECELERATES TO A STOP AND THIS VEHICLE ACCELERATES TO THE MINIMUM
C-----OF THIS VEHICLES DESIRED SPEED AND THE LEAD VEHICLE VELOCITY USING
C-----0.75 CRITICAL SLOPE
      PME    = POSOLD
      VME    = VELOLD
      AME    = ACCOLD
      SME    = SLPOLD
      CALL  SETDSP  ( IV,POSNEW,DBLE( ISPD(IV) ),.FALSE.,DESSPD )
      VMET   = DMIN1( DESSPD,VVSF )
      PVR    = PVSR
      VVR    = VVSR
      AVR    = AVSR
      SVR    = SVSR
      VVRT   = 0.0D0
      GAPOLD = PME - LENVAP - XRELMI - PVR
      DO 2030  I = 1 , IDINT( 10.0D0/DT )
      CALL  CALNEW  ( PME,VME,AME,SME,VMET,     IV      ,CRISLP )
      CALL  CALNEW  ( PVR,VVR,AVR,SVR,VVRT,MAX0(IV,NOSR),CRISLP )
      GAPNEW = PME - LENVAP - XRELMI - PVR
C-----IF THE LAG VEHICLE WILL COLLIDE WITH THIS VEHICLE THEN GO TO 2040
                    IF ( GAPNEW . LT . 0.0D0 )   GO TO 2040
C-----IF THE LAG VEHICLE STOPS OR THE NEW GAP IS LARGER THAN THE OLD GAP
C-----THEN GO TO 3010 AND INITIATE THE LANE CHANGE
                    IF ( VVR    . LE . VELSTP )  GO TO 3010
                    IF ( GAPNEW . GT . GAPOLD )  GO TO 3010
      GAPOLD = GAPNEW
 2030 CONTINUE
C-----THE LAG VEHICLE WILL NOT COLLIDE WITH THIS VEHICLE WITHIN 10 SECS
C-----THUS GO TO 3010 AND INITIATE THE LANE CHANGE
      GO TO 3010
 2040 CONTINUE
C-----THE LAG VEHICLE WILL COLLIDE WITH THIS VEHICLE WITHIN 10 SECS THUS
C-----IF THIS IS THE FIRST VEHICLE IN THE BLOCKED PART OF THE LANE AND
C-----THERE IS NO NOSR VEHICLE AND THIS VEHICLE IS CURRENTLY STOPPED
C-----THEN PRINT WARNING MESSAGE FOR LANE TOO SHORT FOR LANE CHANGE AND
C-----GO TO 3010 AND INITIATE THE LANE CHANGE (THE VEHICLE WILL NEVER
C-----HAVE AN ACCEPTABLE LAG GAP BECUASE THE FIRST PART OF THE BLOCKED
C-----LANE IS TOO SHORT) ELSE GO TO 6010 AND CHECK TO SEE IF THIS
C-----VEHICLE CAN ACCELERATE FOR THE GAP
      IF ( (MFINL(IV)    ) . AND .
     *     (NOSR  .EQ.0  ) . AND .
     *     (VELOLD.LE.0.0D0) )                   THEN
C[      IF ( ALAGAP           .EQ.-2147483647.0 )STOP 'GAPACC ALAGAP 01'
        DISVEH = LENVAP + XRELMI + ALAGAP
        WRITE (WRNMSG,601) TIME,IQ(IV),IA,ILN,
     *                     IDINT( (DISVEH/0.75D0)+0.9999D0 )
        CALL  PRTWRN  ( WRNMSG )
        IF ( IQ(IV) . LT . 0 )                   THEN
          CALL  VDIERR  ( TIME,-IQ(IV),VDILCL )
        END IF
        GO TO 3010
      ELSE
        GO TO 6010
      END IF
 3010 CONTINUE
C-----EVERYTHING SEEMS TO BE OK SO INITIATE THE LANE CHANGE
      ISET(IV) = 1
                    IF ( NOSR . EQ . 0 )         RETURN
      IF ( MININT(NOSR) )                        THEN
        NOSR  = 0
        NOSRS = 0
      END IF
      RETURN
 4010 CONTINUE
C-----THE LAG GAP IS OK BUT THE LEAD GAP IS NOT OK THUS IF THE VEHICLE
C-----HAS BEEN ACCELERATING FOR THE GAP THEN GO TO 3010 AND INITIATE THE
C-----LANE CHANGE
                    IF ( ISET (IV) . EQ . 3 )    GO TO 3010
C-----IF THE LANE CHANGE IS FORCED THEN GO TO 5030
                    IF ( LEGAL(IV) . EQ . 1 )    GO TO 5030
                    IF ( LEGAL(IV) . EQ . 3 )    GO TO 5030
C-----CALCULATE THE LANE CHANGE ACC/DEC SLOPE TO REDUCE THE VEHICLES
C-----VELOCITY TO 85 PERCENT OF THE LEAD VEHICLES SPEED IN ONE SECOND
      SLPLCH = (0.85D0*VVSF-VELOLD-ACCOLD)/0.5D0
C-----BOUND THE LANE CHANGE ACC/DEC SLOPE
      SLPLCH = DMIN1( DMAX1( SLPLCH,-CRISLP ),CRISLP )
      IF ( NOSF . GT . 0 )                       THEN
C-----  IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE NOSF
C-----  VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE FALSE
        EVRESP = ( ( IAND( VEHTYP(IV  ),LAVTE ) . EQ . 0 ) . AND .
     *             ( IAND( VEHTYP(NOSF),LAVTE ) . NE . 0 ) )
        RESPEV = ( RESPEV . OR . EVRESP )
      END IF
C-----REJECT THE GAP AND CAR-FOLLOW THE LEAD VEHICLE
      ISET(IV) = 2
      RETURN
 5010 CONTINUE
C-----THE LAG GAP IS NOT OK THUS IF THE LEAD GAP IS OK THEN GO TO 6010
C-----AND CHECK TO SEE IF THIS VEHICLE CAN ACCELERATE FOR THE GAP
                    IF ( LEGAPA )                GO TO 6010
 5020 CONTINUE
C-----NEITHER THE LEAD GAP NOR THE LAG GAP IS OK THUS IF THE LANE CHANGE
C-----IS FORCED THEN GO TO 5030
                    IF ( LEGAL(IV) . EQ . 1 )    GO TO 5030
                    IF ( LEGAL(IV) . EQ . 3 )    GO TO 5030
 5025 CONTINUE
C-----REJECT THE GAP AND CONTINUE NORMALLY
      ISET(IV) = 5
                    IF ( ISET(IV) . NE . 3 )     RETURN
                    IF ( NOSR     . EQ . 0 )     RETURN
C-----FLAG THE NOSR VEHICLE TO NOT DECELERATE TO FOLLOW A LANE CHANGING
C-----VEHICLE
      MLAG(NOSR) = .FALSE.
      RETURN
 5030 CONTINUE
C-----PROCESS FORCED LANE CHANGE
C-----IF THE LANE ON THE SIDE OF INTEREST IS AVAILABLE ONLY AT THE
C-----INTERSECTION OR AVAILABLE AT THE BEGINNING AND AT THE INTERSECTION
C-----AND THE VEHICLES POSITION IS GE THE START OF THE PORTION AVAILABLE
C-----AT THE INTERSECTION THEN GO TO 5040 AND REJECT THE GAP AND SET
C-----LCHDES TO STOP AT THE END OF LANE ON THE SIDE OF INTEREST OR
C-----BEHIND THE REAR OF THE NOSF VEHICLE
      IF ( (LGEOM(3,LANSI).NE.LGEOM(4,LANSI)) . AND .
     *     (LGEOM(2,LANSI).NE.LGEOM(4,LANSI)) . AND .
     *     (POSNEW.GE.DBLE( LGEOM(3,LANSI) )) )  GO TO 5040
C-----IF THE VEHICLES POSITION IS GE THE END OF THE CURRENT LANE MINUS
C-----EIGHT VEHICLE LENGTHS AND THE NOSF VEHICLES VELOCITY IS GE VELMIN
C-----AND THE ABSOLUTE VALUE OF THE NOSF VEHICLES VELOCITY MINUS THE
C-----VEHICLES VELOCITY IS LE 3.0*VELMIN AND THE NOSF VEHICLE IS
C-----ACCELERATING AND THE ACTUAL LEAD GAP IS POSITIVE AND THE ACTUAL
C-----LAG GAP IS POSITIVE THEN GO TO 5040 AND REJECT THE GAP AND SET
C-----LCHDES TO STOP AT THE END OF LANE ON THE SIDE OF INTEREST OR
C-----BEHIND THE REAR OF THE NOSF VEHICLE
      FLENV = 4.0D0*LENVAP
      IF ( LALT(IV) . GE . 5 )                   CALL  CKLALT
      IF ( (LALT(IV).EQ.3) .OR. (LALT(IV).EQ.4) )THEN
        FLENV = DMIN1( FLENV,
     *                 0.5D0*DBLE( LGEOM(4,NLL(IL))-LGEOM(3,NLL(IL)) ) )
      END IF
      IF ( (LALT(IV).EQ.2) .OR. (LALT(IV).EQ.4) )THEN
        FLENV = DMIN1( FLENV,
     *                 0.5D0*DBLE( LGEOM(4,NLR(IL))-LGEOM(3,NLR(IL)) ) )
      END IF
C[    IF ( GAPLA              .EQ.-2147483647.0 )STOP 'GAPACC GAPLA  04'
C[    IF ( GAPLE              .EQ.-2147483647.0 )STOP 'GAPACC GAPLE  04'
      IF ( (POSNEW              . GE . (ENDLN-2.0D0*FLENV)) . AND .
     *     (VVSF                . GE . VELMIN             ) . AND .
     *     (DABS( VVSF-VELNEW ) . LE . (3.0D0*VELMIN)     ) . AND .
     *     (AVSF                . GE . 0.0D0              ) . AND .
     *     (GAPLE               . GE . 0.0D0              ) . AND .
     *     (GAPLA               . GE . 0.0D0              ) )
     *                                           GO TO 5040
C-----REJECT THE GAP AND CONTINUE NORMALLY
      ISET(IV) = 5
      RETURN
 5040 CONTINUE
C-----REJECT THE GAP AND SET LCHDES TO STOP AT THE END OF LANE ON THE
C-----SIDE OF INTEREST OR BEHIND THE REAR OF THE NOSF VEHICLE
      ISET(IV) = 4
      IFLCHG = 1
                    IF ( NOSR . EQ . 0 )         RETURN
C-----FLAG THE NOSR VEHICLE TO DECELERATE TO FOLLOW A LANE CHANGING
C-----VEHICLE
      MLAG(NOSR) = .TRUE.
      RETURN
 6010 CONTINUE
C-----THE LEAD GAP IS OK BUT THE LAG GAP IS NOT OK THUS CHECK IF THE
C-----VEHICLE CAN ACCELERATE TO CHANGE AHEAD OF THE LAG VEHICLE
C-----IF THE ACTUAL LAG GAP IS LT 0 THEN DO NOT ACCELERATE FOR THE GAP
C[    IF ( GAPLA              .EQ.-2147483647.0 )STOP 'GAPACC GAPLA  05'
                    IF ( GAPLA . LT . 0.0D0 )    GO TO 5020
C-----IF THE VEHICLE IS STOPPING THEN DO NOT ACCELERATE FOR THE GAP
                    IF ( ICDFS(IV) )             GO TO 5020
C-----IF THE LAG VEHICLE SPEED IS GT 6 FPS MORE THAN THIS VEHICLES SPEED
C-----THEN DO NOT ACCELERATE FOR THE GAP
C[    IF ( RESPLA             .EQ.-2147483647.0 )STOP 'GAPACC RESPLA 03'
                    IF ( RESPLA . GT . 6.0D0 )   GO TO 5020
C-----IF THIS VEHICLES ACC/DEC IS LT -CRISLP THEN DO NOT ACCELERATE FOR
C-----THE GAP
                    IF ( ACCOLD . LT . -CRISLP ) GO TO 5020
                    IF ( NOSR . EQ . 0 )         GO TO 6020
C-----IF THE LAG VEHICLE IS ACCELERATING FOR A GAP THEN DO NOT
C-----ACCELERATE FOR THE GAP
                    IF ( ISET(NOSR) . EQ . 3 )   GO TO 5020
 6020 CONTINUE
C-----IF THE ACTUAL LEAD GAP PLUS THE ACTUAL LAG GAP IS LT 1.2 TIMES THE
C-----ACCEPTABLE LEAD GAP PLUS THE ACCEPTABLE LAG GAP THEN DO NOT
C-----ACCELERATE FOR THE GAP
C[    IF ( ALAGAP             .EQ.-2147483647.0 )STOP 'GAPACC ALAGAP 02'
C[    IF ( ALEGAP             .EQ.-2147483647.0 )STOP 'GAPACC ALEGAP 02'
C[    IF ( GAPLA              .EQ.-2147483647.0 )STOP 'GAPACC GAPLA  06'
C[    IF ( GAPLE              .EQ.-2147483647.0 )STOP 'GAPACC GAPLE  05'
      IF ( GAPLE+GAPLA.LT.1.2D0*(ALEGAP+ALAGAP) )GO TO 5020
C-----IF THE DISTANCE TO THE PREVIOUS VEHICLE IN THIS LANE IS LT THE
C-----DISTANCE THAT MUST BE MADE UP IN THE LAG GAP THEN DO NOT
C-----ACCELERATE FOR THE GAP
            IF ( RELPOS.LT.ALEGAP+ALAGAP-GAPLA ) GO TO 5020
C[    IF ( RESPLE             .EQ.-2147483647.0 )STOP 'GAPACC RESPLE 03'
                    IF ( RESPLE . LE . 6.0D0 )   GO TO 6030
C-----FIND THE RELATIVE DISTANCE REQUIRED FOR THIS VEHICLE TO DECELERATE
C-----TO THE LEAD VEHICLES SPEED (LEAD VEHICLE IS NEW VALUES WHEREAS
C-----THIS VEHICLE IS OLD VALUES THEREFORE LEAD VEHICLE IS ONE DT AHEAD
C-----OF THIS VEHICLE)
      IF ( VVSF . LE . 0.0D0 )                   THEN
        SLOPE = -CRISLP
      ELSE
        SLOPE = -0.75D0*CRISLP
      END IF
                    IF ( LEGAL(IV) . NE . 2 )    SLOPE = 1.50D0*SLOPE
                    IF ( SLOPE . EQ . SVSF )     SLOPE = 1.01D0*SLOPE
C-----CALCULATE THE TIME FOR THIS VEHICLE TO DECELERATE TO THE LEAD
C-----VEHICLES SPEED
C-----(VVSF                          )+(AVSF           )*T+SVSF *T**2/2=
C-----(VELOLD+ACCOLD*DT+SLOPE*DT**2/2)+(ACCOLD+SLOPE*DT)*T+SLOPE*T**2/2
C-----(0.5*(SLOPE                           - SVSF))*T**2 +
C-----(ACCOLD + SLOPE *DT                   - AVSF )*T    +
C-----(VELOLD + ACCOLD*DT + 0.5*SLOPE*DT**2 - VVSF )      = 0
      A    = 0.5D0*(SLOPE                          - SVSF)
      B    = ACCOLD + SLOPE *DT                    - AVSF
      C    = VELOLD + ACCOLD*DT + 0.5D0*SLOPE*DTSQ - VVSF
      TMAX = 30.0D0
      CALL  TMQUAD  ( A,B,C,TMAX,TS )
      IF ( TS . EQ . TIMERR )                    THEN
        IF ( DABS( C ) . LE . VSMALL )           THEN
          TS = 0.0D0
        ELSE
          GO TO 5020
        END IF
      END IF
      TM = TS + DT
      VS = VVSF + AVSF*TS + 0.5D0*SVSF*TS**2
      IF ( VS . LE . 0.0D0 )                     THEN
        TMAX = 30.0D0
        CALL  TIMSTP  ( VVSF,AVSF,SVSF,TMAX,T )
        IF ( T . NE . TIMERR )                   THEN
          TS = T
          XS = VVSF*TS + 0.5D0*AVSF*TS**2 + ONED6*SVSF*TS**3
          SLOPE = -CRISLP
                    IF ( LEGAL(IV) . NE . 2 )    SLOPE = 1.5D0*SLOPE
          TMAX = 30.0D0
          CALL  TIMSTP  ( VELOLD,ACCOLD,SLOPE,TMAX,T )
          IF ( T . NE . TIMERR )                 THEN
            TM = T
            XM = VELOLD*TM + 0.5D0*ACCOLD*TM**2 + ONED6*SLOPE*TM**3
          END IF
        END IF
      ELSE
        XM = VELOLD*TM + 0.5D0*ACCOLD*TM**2 + ONED6*SLOPE*TM**3
        XS = VVSF  *TS + 0.5D0*AVSF  *TS**2 + ONED6*SVSF *TS**3
      END IF
      XCRIT = XM - XS
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 104
C4                  IF ( TIME . LT . TPRINT )    GO TO 104
C[    IF ( GAPLE              .EQ.-2147483647.0 )STOP 'GAPACC GAPLE  06'
C4    WRITE (6,704) TM,TS,XM,XS,XCRIT,GAPLE
C4104 CONTINUE
C-----IF THE ACTUAL LEAD GAP IS LT THE RELATIVE DISTANCE REQUIRED FOR
C-----THIS VEHICLE TO DECELERATE TO THE LEAD VEHICLES SPEED THEN DO NOT
C-----ACCELERATE FOR THE GAP
C[    IF ( GAPLE              .EQ.-2147483647.0 )STOP 'GAPACC GAPLE  07'
C[    IF ( XCRIT              .EQ.-2147483647.0 )STOP 'GAPACC XCRIT  03'
                    IF ( GAPLE . LT . XCRIT )    GO TO 5020
 6030 CONTINUE
C-----CALCULATE THE LANE CHANGE ACC/DEC SLOPE REQUIRED TO ACCELERATE THE
C-----VEHICLE AT 75 PERCENT OF THE MAXIMUM ACCELERATION FOR THE VEHICLE
C-----AT THE CURRENT VELOCITY IN ONE SECOND
      ISET(IV) = 3
      ACCVEH = AMAX(IVEHCL(IV))*(1.0D0-(VELOLD/VMAX(IVEHCL(IV))))
      ACCVEH = 0.75D0*DCHAR(IDRICL(IV))*ACCVEH
      SLPLCH = DMIN1( (ACCVEH-ACCOLD),CRISLP )
                    IF ( NOSR . EQ . 0 )         RETURN
C-----FLAG THE NOSR VEHICLE TO DECELERATE TO FOLLOW A LANE CHANGING
C-----VEHICLE
      MLAG(NOSR) = .TRUE.
      RETURN
      END                                                               GAPACC
C
C
C
      SUBROUTINE CALNEW ( POS,VEL,ACC,SLP,VELTGT,KV,SLPCRI )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
C*    CHARACTER*6       ICALNW
      INTEGER           KV
      DOUBLE PRECISION  ACC,ACCMAX,ACCTGT,ACCTIM,ACCVEH,DECMAX,POS,SLP,
     *                  SLPACC,SLPCRI,SLPVEL,TQCSLP,VEL,VELDIF,VELTGT
C*    DATA     ICALNW / 'CALNEW' /
C
C-----SUBROUTINE CALNEW CALCULATES NEW POS/VEL/ACC/SLP AFTER DT SECONDS
C-----WHILE TRYING TO REACH TARGET SPEED VELTGT USING DRIVER AND VEHICLE
C-----PARAMETERS FOR VEHICLE KV AND CRITICAL SLOPE SLPCRI
C
C[    ACCMAX     = -2147483647.0
C[    ACCTGT     = -2147483647.0
C[    ACCTIM     = -2147483647.0
C[    ACCVEH     = -2147483647.0
C[    DECMAX     = -2147483647.0
C[    SLPACC     = -2147483647.0
C[    SLPVEL     = -2147483647.0
C[    TQCSLP     = -2147483647.0
C[    VELDIF     = -2147483647.0
C*          IF ( IRNAME(NRNAME-1).EQ.ICALNW )    GO TO 101
C*          IF ( IRNAME(NRNAME)  .EQ.ICALNW )    GO TO 101
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CALNEW'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C*101 CONTINUE
      VELDIF = VEL - VELTGT
      TQCSLP = 0.75D0*SLPCRI
                    IF ( VELDIF )                1010 , 1020 , 1030
 1010 CONTINUE
C-----SET ACC/DEC TARGET TO ACCELERATE VEHICLE
      ACCMAX = AUTOL*(3.2D0+0.08D0*VELTGT)*DCHAR(IDRICL(KV))
      ACCVEH = AMAX(IVEHCL(KV))*(1.0D0-(VEL/VMAX(IVEHCL(KV))))
      ACCTGT = DMIN1( ACCMAX,ACCVEH )*(1.0D0-(VEL/(1.15D0*VELTGT)))
C-----PROCESS VEHICLE MESSAGE SYSTEM SPEED MESSAGE
      IF ( VMSASM(KV) . GT . 0 )                 THEN
        IF ( IVMSMG(VMSASM(KV)) .EQ. VMSMAM )    THEN
C-----    VEHICLE MESSAGE SYSTEM MESSAGE IS ACTIVE
C-----    ACCELERATE OR DECELERATE TO SPEED XX USING MAXIMUM VEHICLE
C-----    ACCELERATION OR DECELERATION
          ACCTGT = ACCVEH
        END IF
      END IF
      GO TO 1040
 1020 CONTINUE
C-----SET ACC/DEC TARGET ZERO
      ACCTGT = 0.0D0
      GO TO 1040
 1030 CONTINUE
C-----SET ACC/DEC TARGET TO DECELERATE VEHICLE
      DECMAX = DUTOL*(-6.0D0-(VEL/44.0D0))*DCHAR(IDRICL(KV))
      ACCTGT = DMAX1( DECMAX,DMAX(IVEHCL(KV)) )
C-----PROCESS VEHICLE MESSAGE SYSTEM SPEED MESSAGE
      IF ( VMSASM(KV) . GT . 0 )                 THEN
        IF ( IVMSMG(VMSASM(KV)) .EQ. VMSMAM )    THEN
C-----    VEHICLE MESSAGE SYSTEM MESSAGE IS ACTIVE
C-----    ACCELERATE OR DECELERATE TO SPEED XX USING MAXIMUM VEHICLE
C-----    ACCELERATION OR DECELERATION
          ACCTGT = DMAX(IVEHCL(KV))
        END IF
      END IF
 1040 CONTINUE
C-----CALCULATE TIME REQUIRED TO BRING THE CURRENT ACC/DEC TO ZERO USING
C-----SLOPE TQCSLP
C[    IF ( TQCSLP             .EQ.-2147483647.0 )STOP 'CALNEW TQCSLP 01'
      ACCTIM = DABS(ACC)/TQCSLP
                    IF ( ACCTIM . EQ . 0.0D0 )   ACCTIM = 0.001D0
C-----CALCULATE THE SLOPE REQUIRED TO BRING THE CURRENT VELOCITY TO THE
C-----TARGET VELOCITY IN TIME ACCTIM
      SLPVEL = (VELTGT-VEL-ACC*ACCTIM)/(0.5D0*ACCTIM**2)
C-----CALCULATE THE SLOPE REQUIRED TO BRING THE CURRENT ACC/DEC TO THE
C-----TARGET ACC/DEC IN ONE SECOND
C[    IF ( ACCTGT             .EQ.-2147483647.0 )STOP 'CALNEW ACCTGT 01'
      SLPACC =  ACCTGT-ACC
C[    IF ( VELDIF             .EQ.-2147483647.0 )STOP 'CALNEW VELDIF 01'
                    IF ( VELDIF )                2010 , 2020 , 2030
 2010 CONTINUE
C-----USE MINIMUM SLOPE IF ACCELERATING
C[    IF ( SLPACC             .EQ.-2147483647.0 )STOP 'CALNEW SLPACC 01'
C[    IF ( SLPVEL             .EQ.-2147483647.0 )STOP 'CALNEW SLPVEL 01'
C[    IF ( TQCSLP             .EQ.-2147483647.0 )STOP 'CALNEW TQCSLP 02'
      SLP    = DMAX1( DMIN1( TQCSLP,SLPVEL,SLPACC ),-TQCSLP )
      GO TO 2040
 2020 CONTINUE
C-----USE VELOCITY SLOPE IF AT TARGET VELOCITY
C[    IF ( SLPVEL             .EQ.-2147483647.0 )STOP 'CALNEW SLPVEL 02'
C[    IF ( TQCSLP             .EQ.-2147483647.0 )STOP 'CALNEW TQCSLP 03'
      SLP    = DMAX1( DMIN1( TQCSLP,SLPVEL        ),-TQCSLP )
      GO TO 2040
 2030 CONTINUE
C-----USE MAXIMUM SLOPE IF DECELERATING
C[    IF ( SLPACC             .EQ.-2147483647.0 )STOP 'CALNEW SLPACC 02'
C[    IF ( SLPVEL             .EQ.-2147483647.0 )STOP 'CALNEW SLPVEL 03'
C[    IF ( TQCSLP             .EQ.-2147483647.0 )STOP 'CALNEW TQCSLP 04'
      SLP    = DMIN1( DMAX1( -TQCSLP,SLPVEL,SLPACC ),TQCSLP )
 2040 CONTINUE
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER DT SECONDS
      CALL  PRENEW  ( POS,VEL,ACC,SLP )
      RETURN
      END                                                               CALNEW
C
C
C
      SUBROUTINE CHGMLN
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      CHARACTER*4       AVTYPS
      LOGICAL           EVRESP,LBVSTP,LCHKCF,LTF
      INTEGER           GETLCV,JGO,KV,LV,NOFT,NORT,OLDIL,OLDILN
      DOUBLE PRECISION  ACCVEH,CRISLP,DS,POSLAT,POSMJC,POSNRB,POSVEH,
     *                  PVVELM,SLPVEH,TCRIT,TS,VELVEH,XCRIT
  973 FORMAT('STOP 973 - LANE ALLOWED VEHICLE TYPE = (',A,
     *                   ') DOES NOT ALLOW VEHICLE TYPE = (',A,
     *                   ') - CHGMLN')
  975 FORMAT('STOP 975 - LANE ALLOWED VEHICLE TYPE = (',A,
     *                   ') DOES NOT ALLOW VEHICLE TYPE = (',A,
     *                   ') - CHGMLN')
C
C-----SUBROUTINE CHGMLN LOGS THE VEHICLE OUT OF HIS PRESENT LANE AND
C-----INTO THE NEW LANE
C
C[    JBLN       = -2147483647
C[    JGO        = -2147483647
C[    MCONTR     = -2147483647
C[    NOFT       = -2147483647
C[    NORT       = -2147483647
C[    DECMAX     = -2147483647.0
C[    POSLAT     = -2147483647.0
C[    XCRIT      = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CHGMLN'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      NORT = NOR(IV)
      NOFT = NOF(IV)
      OLDIL  = IL
      OLDILN = ILN
      IF ( ISIDE . EQ . 1 )                      THEN
C-----  THE VEHICLE IS CHANGING LANES TO THE LEFT
C-----  IF THE LANE TO THE LEFT IS INVALID THEN ERROR
        IF ( NLL(IL) . LE . 0 )                  GO TO 9720
C-----  IF THE VEHICLE IS NOT ALLOWED ON THE LANE TO THE LEFT THEN ERROR
        IF ( IAND( VEHTYP(IV),LAVT(NLL(IL)) ) . EQ . 0 )
     *                                           GO TO 9730
        LPRES(IV) = NLL(IL)
        IL        = NLL(IL)
        ILN       = ILN - 1
      ELSE
C-----  THE VEHICLE IS CHANGING LANES TO THE RIGHT
C-----  IF THE LANE TO THE RIGHT IS INVALID THEN ERROR
        IF ( NLR(IL) . LE . 0 )                  GO TO 9740
C-----  IF THE VEHICLE IS NOT ALLOWED ON THE LANE TO THE RIGHT THEN
C-----  ERROR
        IF ( IAND( VEHTYP(IV),LAVT(NLR(IL)) ) . EQ . 0 )
     *                                           GO TO 9750
        LPRES(IV) = NLR(IL)
        IL        = NLR(IL)
        ILN       = ILN + 1
      END IF
C-----SET THE LANE CHANGE FLAG
      LCHGE(IV) = 2
C-----SET LANE CONTROL FOR VEHICLE
      LCONTV(IV) = GETLCV( IV,IL )
C-----SET US TO CAR FOLLOW THE NOF VEHICLE IF CRITICAL
      IF ( NOFT . GT . 0 )                       THEN
        LV = JVCNOF(NOFT)
        IF ( LV . GT . 0 )                       THEN
          IVCNOF(LV) = 0
        END IF
        IVCNOF(IV  ) = NOFT
        JVCNOF(NOFT) = IV
      ELSE
        IVCNOF(IV  ) = 0
      END IF
C-----CLEAR THE FLAG FOR ME TO CAR FOLLOW A VEHICLE CHANGING LANES
      LV = JVCNOR(IV)
      IF ( LV . GT . 0 )                         THEN
        IF ( (LV.EQ.NOSF) . OR . (LV.EQ.NOSFS) ) THEN
          IVCNOR(LV) = 0
          JVCNOR(IV) = 0
        END IF
      END IF
C-----IF THERE IS A VEHICLE TO THE REAR IN THE CURRENT LANE THEN SET
C-----THAT VEHICLE TO CAR FOLLOW ME UNTIL I COMPLETE 60 PERCENT OF MY
C-----LANE CHANGE AND OTHER CRITERIA
      IF ( NORT . EQ . 0 )                       THEN
        LV = JVCNOR(IV)
        IF ( LV . GT . 0 )                       THEN
          IVCNOR(LV) = 0
        END IF
        JVCNOR(IV) = 0
      ELSE
        IF ( JVCNOR(NORT) . EQ . 0 )             THEN
          JVCNOR(NORT) = IV
          IVCNOR(IV  ) = NORT
        ELSE
          CALL  SPVAS  ( JVCNOR(NORT),POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                   .TRUE.,.TRUE.,.FALSE.,.TRUE.              )
          POSNRB = POSNEW - LENVAP - XRELMI
          IF ( POSNRB . LT . POSVEH )            THEN
            JVCNOR(NORT) = IV
            IVCNOR(IV  ) = NORT
          END IF
        END IF
      END IF
C-----RESET SOME OF THE VEHICLES PARAMETERS
C-----USE SAVED NOSF AND NOSR THAT ARE THE VEHICLES IN THE LANE ON THE
C-----SIDE OF INTEREST NOT A VEHICLE POSSIBLY CHANGING LANES OUT OF THE
C-----LANE ON THE SIDE OF INTEREST OR IN THE INTERSECTION OR ON THE
C-----LINKING OUTBOUND LANE FOR THE INTERSECTION PATH
      NOSF = NOSFS
      NOSR = NOSRS
      IF ( NOSF . GT . 0 )                       THEN
        IF ( ( MININT(NOSF)                 ) . OR .
     *       ( (.NOT. MININT(NOSF)) . AND .
     *         (LPRES(NOSF).NE.IL )         ) )  THEN
          NOSF  = 0
          NOSFS = 0
          PVSF  = DBLE( LGEOM(4,IL) )
          VVSF  = 0.0D0
          AVSF  = 0.0D0
          SVSF  = 0.0D0
C-----    SEARCH FOR A VEHICLE AHEAD OF THIS VEHICLE IN THE NEW LANE
          KV = IFVL(IL)
          DO WHILE ( KV . GT . 0 )
            IF ( IPOS(KV) . LT . POSNEW )        THEN
              EXIT
            END IF
            NOSF  = KV
            NOSFS = KV
            KV    = NOR(KV)
          END DO
          IF ( NOSF . GT . 0 )                   THEN
            CALL  SPVAS  ( NOSF,PVSF,VVSF,AVSF,SVSF,
     *                     .TRUE.,.TRUE.,.FALSE.,.TRUE. )
          END IF
        END IF
      END IF
      IF ( NOSR . GT . 0 )                       THEN
        IF ( ( MININT(NOSR)                 ) . OR .
     *       ( (.NOT. MININT(NOSR)) . AND .
     *         (LPRES(NOSR).NE.IL )         ) )  THEN
          NOSR  = 0
          NOSRS = 0
          PVSR  = DBLE( LGEOM(1,IL) )
          VVSR  = 0.0D0
          AVSR  = 0.0D0
          SVSR  = 0.0D0
C-----    SEARCH FOR A VEHICLE BEHIND THIS VEHICLE IN THE NEW LANE
          KV = ILVL(IL)
          DO WHILE ( KV . GT . 0 )
            IF ( IPOS(KV) . GT . POSNEW )        THEN
              EXIT
            END IF
            NOSR  = KV
            NOSRS = KV
            KV    = NOF(KV)
          END DO
          IF ( NOSR . GT . 0 )                   THEN
            CALL  SPVAS  ( NOSR,PVSR,VVSR,AVSR,SVSR,
     *                     .FALSE.,.FALSE.,.FALSE.,.TRUE. )
          END IF
        END IF
      END IF
      IVPV  = NOSF
      PVPOS = PVSF
      IF ( MFSTPF(IV) )                          THEN
        IF ( FSTACT(IV) )                        THEN
          IF ( FSTPOS(IV) . LT . PVPOS )         PVPOS = FSTPOS(IV)
        END IF
        IF ( VMSASM(IV) . GT . 0 )               THEN
          IF ( ( IVMSMG(VMSASM(IV)).EQ.VMSMSI ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSL ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSM ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSC ) )THEN
            IF ( VMSPST(IV) . LT . PVPOS )       PVPOS = VMSPST(IV)
          END IF
        END IF
      ELSE
        PVVEL = VVSF
        PVACC = AVSF
        PVSLP = SVSF
      END IF
      MSFLG (IV) = .FALSE.
      LALT  (IV) = 5
C-----CHECK MY LANE AND IF BLOCKED THEN SET PARAMETERS FOR BLOCKED LANE
      CALL  CHKMLN
      IPRTM(IV) = 0
      JPRTM = 0
                    IF ( NOSF . EQ . 0 )         GO TO 1020
                    IF ( PVVEL . LE . 0.01D0 )   GO TO 1020
C-----THE LEAD VEHICLE IS MOVING SO SET THE VEHICLE TO CAR-FOLLOW HIM
      PVVELM = DMAX1( DMIN1( PVVEL,PVVEL+PVACC*DT+0.5D0*PVSLP*DTSQ),
     *                0.0D0                                          )
      DESVEL = DMIN1( DESVEL,0.95D0*PVVELM )
      CALL  SETDSP  ( IV,POSNEW,DESVEL,.TRUE.,DESVEL )
 1020 CONTINUE
C-----COMPUTE NEW ACC/DEC LOGIC
      CALL  LOGIC   ( 6,IV )
      RELPOS = PVPOS - POSNEW
      RELVEL = PVVEL - VELNEW
C-----DECREMENT THE NUMBER OF VEHICLES IN THE PRESENT LANE
      NVIL(OLDILN,IA) = NVIL(OLDILN,IA) - 1
C-----LOG THE VEHICLE OUT OF THE PRESENT LANE
      LTF = .FALSE.
                    IF ( NOFT . GT . 0 )         GO TO 2010
C-----SET THE FIRST VEHICLE IN THE PRESENT LANE TO THIS VEHICLES OLD
C-----NORT (OLD NOFT EQ 0)
      LTF = .TRUE.
      IFVL(OLDIL ) = NORT
      GO TO 2020
 2010 CONTINUE
C-----SET THE NOR FOR THE OLD NOFT VEHICLE TO THIS VEHICLES OLD NORT
C-----(OLD NOFT GT 0)
      NOR(NOFT) = NORT
 2020 CONTINUE
                    IF ( NORT . GT . 0 )         GO TO 2030
C-----SET THE LAST VEHICLE IN THE PRESENT LANE TO THIS VEHICLES OLD NOFT
C-----(OLD NOR EQ 0)
      ILVL(OLDIL ) = NOFT
      GO TO 2040
 2030 CONTINUE
C-----SET MFINL AND MOASF TO LTF, RESET IACC TO SLIGHTLY DECELERATING
C-----IF MSFLG EQ TRUE AND THE VEHICLE IS NOT DECELERATING, SET MSFLG
C-----TO FALSE, AND FINALLY STORE NOFT FOR NOFT FOR THE OLD NOR VEHICLE
C-----(OLD NOR GT 0)
      CALL  FLGNOR ( LTF,NOFT )
                    IF ( NOFT . EQ . 0 )         GO TO 2040
C-----SET THE CORRECT VALUE FOR MOASF FOR THE OLD NOR VEHICLE
C-----(OLD NOR GT 0 AND OLD NOFT GT 0)
      MOASF(NORT) = .FALSE.
                    IF ( IVEL(NOFT) .LE. 0.0D0 ) MOASF(NORT) = .TRUE.
 2040 CONTINUE
C-----LOG THE VEHICLE INTO THE NEW LANE
C-----SET THE VEHICLES NEW NOFT AND NORT FOR THE NEW LANE
      NOF(IV) = NOSF
      NOFT = NOSF
      IVPV = NOFT
      NOR(IV) = NOSR
      NORT = NOR(IV)
C-----INCREMENT THE NUMBER OF VEHICLES IN THE NEW LANE
      NVIL(ILN,IA) = NVIL(ILN,IA) + 1
C-----IF THE VEHICLE IS CHANGING LANES TO THE RIGHT THEN SET THE FLAG
C-----FOR ALREADY PROCESSED IN THIS DT
                    IF ( ISIDE . EQ . 3 )        LALT(IV) = 6
                    IF ( NOFT . GT . 0 )         GO TO 3010
C-----SET THIS VEHICLE AS THE NEW FIRST VEHICLE IN THE NEW LANE
C-----(NEW NOFT EQ 0)
      MFINL(IV) = .TRUE.
      MOASF(IV) = .TRUE.
      IFVL(IL    ) = IV
                    IF ( NORT . EQ . 0 )         GO TO 3020
C-----CHECK IF THE NEW NORT VEHICLES LANE CHANGING FLAG CAN BE TURNED
C-----BACK ON (NEW NOFT EQ 0 AND NEW NORT GT 0)
                    IF ( ISET (NORT) . NE . 6 )  GO TO 3020
                    IF ( LEGAL(NORT) . EQ . 4 )  GO TO 3020
C-----CHECK IF ISET CAN BE SET TO 5 FOR THE NEW NORT VEHICLE
      CALL  CKISET  ( NORT,IPOS(NORT) )
      GO TO 3020
 3010 CONTINUE
C-----SET THIS VEHICLE AS THE NEW NORT FOR THE NEW NOFT VEHICLE AND FIND
C-----THE NEW VALUE FOR MOASF FOR THIS VEHICLE (NEW NOFT GT 0)
      MFINL(IV) = .FALSE.
      NOR(NOFT) = IV
      MOASF(IV) = .FALSE.
                    IF ( IVEL(NOFT) .LE. 0.0D0 ) MOASF(IV) = .TRUE.
 3020 CONTINUE
                    IF ( NORT . GT . 0 )         GO TO 3030
C-----SET THIS VEHICLE AS THE NEW LAST VEHICLE IN THE NEW LANE
C-----(NEW NORT EQ 0)
      ILVL(IL    ) = IV
      GO TO 3040
 3030 CONTINUE
C-----SET MFINL AND MOASF TO FALSE, RESET IACC TO SLIGHTLY DECELERATING
C-----IF MSFLG EQ TRUE AND THE VEHICLE IS NOT DECELERATING, SET MSFLG
C-----TO FALSE, AND FINALLY STORE IV FOR NOFT FOR THE NEW NORT VEHICLE
C-----(NEW NORT GT 0)
      CALL  FLGNOR  ( .FALSE.,IV )
C-----FLAG THE NEW NORT VEHICLE THAT HE IS FOLLOWING A LANE CHANGING
C-----VEHICLE
      LCHGE(NORT) = 3
 3040 CONTINUE
C-----SET THE TOTAL LATERAL DISTANCE FOR THE LANE CHANGE (BIASED BY 2)
      LEGAL(IV) = LWID(IL) + LWID(OLDIL)
C-----SET THE CURRENT LATERAL POSITION FOR THE LANE CHANGE TO THE TOTAL
C-----LATERAL DISTANCE FOR THE LANE CHANGE
C-----(A POSITIVE VALUE FOR POSLAT MEANS THE VEHICLE IS CHANGING LEFT)
C-----(A NEGATIVE VALUE FOR POSLAT MEANS THE VEHICLE IS CHANGING RIGHT)
      POSLAT = 0.5D0*DBLE( LEGAL(IV) )
                    IF ( ISIDE . EQ . 3 )        POSLAT = -POSLAT
C-----BIAS THE CURRENT LATERAL POSITION FOR THE LANE CHANGE
      LATPOS(IV) = POSLAT
            IF ( IA . EQ . IABS( NOBAPD(IV) ) )  GO TO 4020
                    IF ( LNEXT(IV) . EQ . 0 )    GO TO 3050
C-----UNSET THE INTERSECTION CONFLICTS FOR THE INTERSECTION PATH FOR THE
C-----VEHICLE
      CALL  UNSETC
      LNEXT(IV) = 0
 3050 CONTINUE
C-----FIND AN INTERSECTION PATH FOR THIS VEHICLE BASED ON THE CURRENT
C-----APPROACH, THE NEW LANE, AND THE DESIRED OUTBOUND APPROACH
      CALL  PATHF   ( .FALSE.,0,'CHGMLN' )
C-----SET THE TOTAL LATERAL DISTANCE FOR THE LANE CHANGE (BIASED BY 2)
      LEGAL(IV) = LWID(IL) + LWID(OLDIL)
C-----THIS VEHICLE SHOULD CHECK TO SEE IF IT SHOULD BE WITHIN THE
C-----INFLUENCE ZONE OF THE INTERSECTION CONTROL THUS IF THE VEHICLE HAS
C-----NOT DEDICATED HIMSELF TO AN INTERSECTION PATH THEN RETURN AND WAIT
C-----UNTIL THE VEHICLE IS DEDICATED TO AN INTERSECTION PATH
                    IF ( LNEXT(IV) . EQ . 0 )    GO TO 4010
      IF ( MBLOCK(IV) )                          THEN
        ENDLN = DBLE( LGEOM(2,IL) )
      ELSE
        ENDLN = DBLE( LGEOM(4,IL) ) + 1.5D0
      END IF
      RELEND = ENDLN - POSOLD
                    IF ( MINFLZ(IV) )            GO TO 3060
      CRISLP = SLPMAX*DCHAR(IDRICL(IV))
C-----CALCULATE THE TIME AND DISTANCE TO STOP NOT EXCEEDING THE MAXIMUM
C-----DECELERATION RATE FOR THE DRIVER
      CALL  TDSTPM  ( IV,-6.0D0,VELOLD,VELOLD,ACCOLD,.TRUE.,TS,DS )
C-----CALCULATE THE THRESHOLD DISTANCE FROM THE END OF THE LANE THAT THE
C-----VEHICLE SHOULD BECOME WITHIN THE INFLUENCE ZONE OF THE
C-----INTERSECTION CONTROL (LET 4+PIJR SECONDS AT THE CURRENT VELOCITY
C-----PLUS THE STOPPING DISTANCE BE THE THRESHOLD DISTANCE)
      XCRIT = VELOLD*(4.0D0+PIJR(IDRICL(IV))) + DS
C-----LET 400 FEET BE THE MINIMUM THRESHOLD DISTANCE
      XCRIT = DMAX1( XCRIT,400.0D0 )
C-----IF THE DISTANCE FROM THE END OF THE END OF THE LANE IS GT THE
C-----THRESHOLD DISTANCE THEN RETURN AND WAIT UNTIL THE VEHICLE IS
C-----CLOSER
                    IF ( RELEND . GT . XCRIT )   GO TO 4010
 3060 CONTINUE
C-----RESET ALL ACC/DECEL INDEPENDENT ATTRIBUTES SO THAT NEW VALUES WILL
C-----BE SET FOR THE NEW LANE AS APPROPRIATE
      MATSTL(IV) = .FALSE.
      MCHKCF(IV) = .FALSE.
      MDEDIC(IV) = .FALSE.
      MINFLZ(IV) = .FALSE.
      MIUNC (IV) = .FALSE.
      MLRTOR(IV) = .FALSE.
      MLSTOP(IV) = .FALSE.
      MLUNC (IV) = .FALSE.
      MLYELD(IV) = .FALSE.
C-----FIND THE TIME AND DISTANCE TO SWITCH FROM LANE CHANGING FROM
C-----BEHIND A SLOW VEHICLE TO LANE CHANGING CLOSE TO THE INTERSECTION
      TCRIT = (LCSTIM+PIJR(IDRICL(IV))-APIJR)/DCHAR(IDRICL(IV))
      XCRIT = TCRIT*DBLE( ISPD(IV) )
C-----IF THE VEHICLE IS LESS THAN LCSTIM SECONDS FROM THE END OF THE
C-----LANE THEN SET TO CHECK FOR A LANE CHANGE CLOSE TO THE INTERSECTION
      IF ( RELEND . LT . XCRIT )                 THEN
        MDEDIC(IV) = .TRUE.
        MINFLZ(IV) = .TRUE.
      END IF
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        MCHKCF(IV) = .TRUE.
        MPRO  (IV) = .FALSE.
        LOGTMP     = 1
        LOGFLG(IV) = 1
      END IF
                    IF ( ISIDE . EQ . 3 )        GO TO 4010
                    IF ( .NOT. MINFLZ(IV) )      GO TO 4010
C-----WHEN CHANGING TO THE LEFT, MUST UPDATE THE IV VEHICLE NOW TO
C-----CATCH UP ITS RESPONSE TO THE SIGNAL TO ITS NOSF AND NOSR
C
C-----THE VEHICLE WAS WITHIN THE INFLUENCE ZONE OF THE INTERSECTION
C-----CONTROL SO SET THE PARAMETERS NECESSARY TO CALL INFLZN FOR THE
C-----NEW LANE
      JGO = IGO
      IGO = 1
C-----INITIALIZE THE VEHICLES INTERSECTION CONTROL LOGICAL ATTRIBUTES
C-----BASED ON THE TYPE OF TRAFFIC CONTROL FOR THE NEW LANE
      CALL  INFLZN
C-----RESET PARAMETERS FOR THE PRESENT LANE
      IGO = JGO
 4010 CONTINUE
C-----SET THE INTERSECTION CONTROL LOGIC TIMER SO THIS VEHICLE WILL BE
C-----PROCESSED NEXT DT
      LOGTMP     = 2
      LOGFLG(IV) = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP     = 1
        LOGFLG(IV) = 1
      END IF
                    IF ( ISIDE . EQ . 3 )        GO TO 4030
 4020 CONTINUE
C-----COMPUTE NEW ACC/DEC LOGIC FOR NEW CONDITIONS
      CALL  LOGIC   ( 6,IV )
 4030 CONTINUE
      IF ( SMJCOL . AND . (.NOT. MAJCOL(IV)) )   THEN
        MAJCLB(IV) = .FALSE.
        MAJCLL(IV) = .FALSE.
        MAJCON(IV) = .FALSE.
        POSCLB(IV) = POSBIG
        POSCLL(IV) = POSBIG
        POSCON(IV) = POSBIG
        CALL  CLMJCL  ( IL,LNEXT(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
        IF ( KV . GT . 0 )                 THEN
C-----    THERE IS A COLLISION VEHICLE ON THE LANE BEFORE THIS VEHICLE
C-----    OR DOWNSTREAM THUS SET THAT THE VEHICLE IS BLOCKED BY A MAJOR
C-----    COLLISION
          MAJCLB(IV) = .TRUE.
          POSCLB(IV) = DMIN1( POSCLB(IV),POSMJC )
C-----    IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----    VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----    FALSE
          EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *               ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
          RESPEV = ( RESPEV . OR . EVRESP )
        END IF
C-----  SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----  COLLISION MAY BLOCK THIS VEHICLE
        IF ( LCHKCF )                            THEN
          MAJCON(IV) = .TRUE.
          POSCON(IV) = DMIN1( POSCON(IV),POSMJC )
        END IF
      END IF
      IL  = OLDIL
      ILN = OLDILN
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9720 CONTINUE
      CALL  ABORTR  ( 'STOP 972 - '                        //
     *                'LANE TO THE LEFT DOES NOT EXIST - ' //
     *                'CHGMLN'                                )
      STOP  972
 9730 CONTINUE
      WRITE (ERRMSG,973) AVTYPS( LAVT(NLL(IL)) ),AVTYPS( VEHTYP(IV) )
      CALL  ABORTR  ( ERRMSG )
      STOP  973
 9740 CONTINUE
      CALL  ABORTR  ( 'STOP 974 - '                         //
     *                'LANE TO THE RIGHT DOES NOT EXIST - ' //
     *                'CHGMLN'                                 )
      STOP  974
 9750 CONTINUE
      WRITE (ERRMSG,975) AVTYPS( LAVT(NLR(IL)) ),AVTYPS( VEHTYP(IV) )
      CALL  ABORTR  ( ERRMSG )
      STOP  975
      END                                                               CHGMLN
C
C
C
      SUBROUTINE CHKNOF
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           EVRESP
      INTEGER           IVDV,K,KL,KP,LENL1
      DOUBLE PRECISION  ACCVEH,POSRB,POSVEH,SLPTMP,SLPVEH,VELVEH
C
C-----SUBROUTINE CHKNOF CHECKS THE NOF VEHICLE OR ANY VEHICLE WHOSE REAR
C-----BUMPER HAS NOT CROSSED THE STOP LINE AND SETS SLPNOF TO CAR-FOLLOW
C-----OR STOP BEHIND THAT VEHICLE IF THIS VEHICLES LANE IS NOT BLOCKED,
C-----IF THIS VEHICLE IS THE FIRST VEHICLE IN THE LANE, AND THIS VEHICLE
C-----MAY NOT PROCEED INTO THE INTERSECTION
C
C[    IVDV       = -2147483647
C[    K          = -2147483647
C[    KP         = -2147483647
C[    LENL1      = -2147483647
C[    ACCVEH     = -2147483647.0
C[    POSRB      = -2147483647.0
C[    POSVEH     = -2147483647.0
C[    SLPVEH     = -2147483647.0
C[    VELVEH     = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CHKNOF'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      LENL1 = 0
      SLPNOF = 0.0D0
C-----IF THERE IS A VEHICLE AHEAD ON THE SAME LANE THEN USE THAT VEHICLE
C-----AS THE DESIGNATED VEHICLE
      IVDV = NOF(IV)
                    IF ( IVDV . GT . 0 )         GO TO 2010
C-----IF THE REAR BUMPER OF THE LAST VEHICLE ON ANY LINKING INTERSECTION
C-----PATH FOR THE LANE HAS NOT CROSSED THE END OF THE LANE THEN USE
C-----THAT VEHICLE AS THE DESIGNATED VEHICLE
      DO 1010  K = 1 , NPINT(IL)
      KP = LINTP(K,IL)
      IVDV = ILVP(KP)
      LENL1 = LGEOM(4,IL)
      IF ( IVDV . EQ . 0 )                       THEN
        KL = LOBL(KP)
                    IF ( KL . EQ . 0 )           GO TO 1010
        IVDV = ILVL(KL)
                    IF (       IVDV  .EQ. 0  )   GO TO 1010
                    IF ( LPREV(IVDV) .NE. KP )   GO TO 1010
        POSRB = IPOS(IVDV) - LVAP(IVDV) - XRELMI
     *                     - DBLE( LGEOM(1,KL)-LENP(KP) )
        LENL1 = LENL1             - LGEOM(1,KL) + LENP(KP)
      ELSE
        POSRB = IPOS(IVDV) - LVAP(IVDV) - XRELMI
      END IF
      IF ( POSRB . LT . 0.0D0 )                  GO TO 2010
 1010 CONTINUE
      RETURN
 2010 CONTINUE
C-----SET SLPNOF TO CAR-FOLLOW OR STOP BEHIND THE DESIGNATED VEHICLE
      CALL  SPVAS   ( IVDV,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                .TRUE.,.TRUE.,.TRUE.,.FALSE.      )
      POSVEH = POSVEH + LENL1
      CALL  SLPCFS  ( SLPTMP,IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                          POSVEH,VELVEH,ACCVEH,SLPVEH  )
      IF ( SLPTMP . NE . 0.0D0 )                 THEN
        IF ( SLPTMP . LT . SLPNOF )              THEN
          SLPNOF = SLPTMP
C-----    IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVDV
C-----    VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----    FALSE
          EVRESP = ( ( IAND( VEHTYP(IV  ),LAVTE ) . EQ . 0 ) . AND .
     *               ( IAND( VEHTYP(IVDV),LAVTE ) . NE . 0 ) )
          RESPEV = ( RESPEV . OR . EVRESP )
        END IF
      END IF
      END                                                               CHKNOF
C
C
C
      SUBROUTINE CHKINT ( MP,CHKOTH,SETSLP,IBLOCK,MAJCBL,POSCBL )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           ALLMOV,CHKOTH,EVRESP,IBLOCK,LCCLN,MAJCBL,SETSLP
C[    LOGICAL           IBLOCS
      INTEGER           IPASS,IVIL,IVJL,IVJP,JA,JL,JP,KP,LP,MIBL,MP
      DOUBLE PRECISION  AIVJL,ACCVEH,CLEARD,CLEART,DISAVL,DISLCH,DSPLCH,
     *                  HWH,HWM,LGEOM2,PIVJL,POSCBL,POSLCH,POSVEH,SAFR,
     *                  SAFVEL,SIVJL,SLPTMP,SLPVEH,SUMLEN,VELLCH,VEHLNG,
     *                  VELVEH,VIVIL,VIVJL
C
C-----SUBROUTINE CHKINT CHECKS IF THIS VEHICLE WOULD BLOCK THE
C-----INTERSECTION
C
C[    IVIL       = -2147483647
C[    IVJL       = -2147483647
C[    IVJP       = -2147483647
C[    JA         = -2147483647
C[    JL         = -2147483647
C[    JP         = -2147483647
C[    ACCVEH     = -2147483647.0
C[    DISAVL     = -2147483647.0
C[    PIVJL      = -2147483647.0
C[    POSVEH     = -2147483647.0
C[    SLPVEH     = -2147483647.0
C[    SUMLEN     = -2147483647.0
C[    VELVEH     = -2147483647.0
C[    VIVIL      = -2147483647.0
C[    VIVJL      = -2147483647.0
C[    IBLOCS     = .TRUE.
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CHKINT'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IBLOCK = .FALSE.
      MAJCBL = .FALSE.
      POSCBL = 0.0D0
C-----IF EMERGENCY VEHICLE THEN RETURN NOT BLOCKED
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   THEN
        RETURN
      END IF
      EVRESP = .FALSE.
C-----IF DIAMOND INTERSECTION FREE U-TURNS ARE DEFINED AND
C-----(1) THE APPROACH IS 1 OR 6              OR
C-----(2) THE APPROACH IS 3 AND THE LANE IS 1 OR
C-----(3) THE APPROACH IS 8 AND THE LANE IS 1
C-----THEN RETURN NOT BLOCKED
      IF ( NFUT . GT . 0 )                       THEN
        IF ( ( IA .EQ. 1 ) . OR . ( IA .EQ. 6 ) )THEN
          RETURN
        END IF
        IF ( ILN . EQ . 1 )                      THEN
          IF ( ( IA .EQ. 3 ) .OR. ( IA .EQ. 8 ) )THEN
            RETURN
          END IF
        END IF
      END IF
      POSCBL = POSBIG
      HWM = 0.5D0*WIDV(IVEHCL(IV))
C-----IF THIS VEHICLE DOES NOT HAVE AN INTERSECTION PATH THEN GO TO 3010
C-----AND SET THE SLOPE FOR INTERSECTION BLOCKAGE AND RETURN
      KP = MP
                    IF (      KP  . EQ . 0 )     GO TO 3010
                    IF ( LOBL(KP) . EQ . 0 )     GO TO 3010
      IPASS = 1
 1005 CONTINUE
C-----SET THE LENGTH OF LANE AVAILABLE FOR THE LINKING INTERNAL INBOUND
C-----OR OUTBOUND LANE FOR THIS VEHICLES INTERSECTION PATH
                    IF ( KP . EQ . 0 )           GO TO 4010
      JL   = LOBL(KP)
                    IF ( JL . EQ . 0 )           GO TO 4010
      JA   = ISNA(JL)
      DISAVL = DBLE( LGEOM(2,JL) - LGEOM(1,JL) )
      LGEOM2 = DBLE( LGEOM(2,JL) )
      IF ( LGEOM(2,JL) . EQ . LGEOM(4,JL) )      THEN
        LGEOM2 = LGEOM2 + 2.2D0*XRELMX
      END IF
      ALLMOV = .TRUE.
C-----SUM THE LENGTH OF ALL THE VEHICLES ON THE LINKING INTERNAL INBOUND
C-----OR OUTBOUND LANE FOR THIS VEHICLES INTERSECTION PATH
      SUMLEN = 0.0D0
      IVJL = IFVL(JL)
      DO WHILE ( IVJL . GT . 0 )
        IF ( IPOS(IVJL) . GT . LGEOM2 )          THEN
          IVJL = NOR(IVJL)
          CYCLE
        END IF
        IF ( MAJCOL(IVJL) . OR . MAJCLB(IVJL) )  THEN
          MAJCBL = .TRUE.
          POSCBL = DMIN1( POSCBL,
     *                    POSCLB(IVJL)+DBLE( LGEOM(4,IL)+LENP(KP) ) )
C-----    IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVJL
C-----    VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----    FALSE
          EVRESP = ( ( IAND( VEHTYP(IV  ),LAVTE ) . EQ . 0 ) . AND .
     *               ( IAND( VEHTYP(IVJL),LAVTE ) . NE . 0 ) )
          RESPEV = ( RESPEV . OR . EVRESP )
        END IF
        IF ( IVEL(IVJL) . LE . VELSTP )          THEN
          SUMLEN = DBLE( LGEOM(4,JL)-LGEOM(1,JL) ) - IPOS(IVJL)
        END IF
        IF ( MAJCOL(IVJL) )                      THEN
C-----    THE IVJL VEHICLE HAS A MAJOR COLLISION THUS RESET SUMLEN TO
C-----    ITS DISTANCE FROM THE BEGINNING OF THE LANE AND SUBTRACT THE
C-----    DISTANCE REQUIRED FOR A NORMAL LANE CHANGE CLEAR (A VEHICLE
C-----    NORMALLY MUST TRAVEL 70 PERCENT OF THE LANE CHANGE DISTANCE TO
C-----    MOVE THE VEHICLE OUT OF THE OLD LANE AND WE USE 50 PERCENT OF
C-----    THE SPEED LIMIT OF THE APPROACH FOR THE VEHICLE SPEED AND 25
C-----    FEET FOR 1.5 TIMES THE VEHICLE LENGTH BECAUSE WE DO NOT KNOW A
C-----    SPECIFIC VEHICLE)
          CALL  SPVAS  ( IVJL,PIVJL,VIVJL,AIVJL,SIVJL,
     *                   .FALSE.,.FALSE.,.FALSE.,.TRUE. )
          DSPLCH = DBLE( ISPD(IV) )
          IF ( IDISPD(IV) )                      THEN
            DSPLCH = 0.5D0*DSPLCH
          END IF
          IF ( ISPDP (IV) . EQ . 1 )             THEN
            IF ( MININT(IV) )                    THEN
              IF ( LOBL(IP) . GT . 0 )           THEN
                DSPLCH = DSPLCH*DBLE( ISLIM(ISNA(LOBL(IP))) )
     *                 /        DBLE( LIMP (          IP  ) )
              END IF
            ELSE
              IF ( LNEXT(IV) . GT . 0 )          THEN
                DSPLCH   = DSPLCH*DBLE( ISLIM(          IA  ) )
     *                   /        DBLE( LIMP (LNEXT(    IV )) )
              END IF
            END IF
          END IF
          CALL  SPVAS  ( IV,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                   .FALSE.,.FALSE.,.FALSE.,.TRUE.  )
          VELLCH = 0.2D0*DSPLCH
          VELLCH = DMAX1( VELLCH,IVEL(IV),VELVEH )
          VEHLNG = DMIN1( 25.0D0,LENVAP )
          DISLCH = 0.5D0*(DBLE( LGEOM(2,JL) )-PIVJL)
          DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
          DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
          IF ( MAJRLC(IV) )                      THEN
            DISLCH = 0.5D0*XRELMI
          END IF
          POSLCH = DBLE( LGEOM(2,JL) ) - DISLCH
          SUMLEN = DBLE( LGEOM(4,JL)-LGEOM(1,JL) )-(PIVJL-0.7D0*DISLCH)
          ALLMOV = .TRUE.
C-----    IF THERE IS ROOM FOR A FULL LANE CHANGE AHEAD OF THE COLLISION
C-----    VEHICLE AND THIS VEHICLE WILL BE COMING INTO A LANE DIFFERENT
C-----    FROM THE LANE FOR THE COLLISION VEHICLE THEN SET SUMLEN TO A
C-----    LARGE NEGATIVE NUMBER SO SUMLEN WILL NOT BE GREATER THAN
C-----    DISAVL
          IF ( PIVJL . LE . POSLCH )             THEN
            IF ( KP . NE . LNEXT(IV) )           THEN
              SUMLEN = -POSBIG
            END IF
          END IF
        END IF
C-----  IF THE IVJL VEHICLE IS TO CAR FOLLOW ANOTHER VEHICLE FOR LANE
C-----  CHANGING THEN IF BOTH THE IVJL VEHICLE AND THE VEHICLE TO CAR
C-----  FOLLOW FOR LANE CHANGING ARE STOPPED THUS RESET SUMLEN TO ITS
C-----  DISTANCE FROM THE BEGINNING OF THE LANE
        IF ( JVCNOR(IVJL) . GT . 0 )             THEN
          IF ( ( IVEL(JVCNOR(IVJL)).LE.VELSTP ) . AND .
     *         ( IVEL(       IVJL ).LE.VELSTP ) )THEN
            CALL  SPVAS  ( IVJL,PIVJL,VIVJL,AIVJL,SIVJL,
     *                     .FALSE.,.FALSE.,.FALSE.,.TRUE. )
            SUMLEN = DBLE( LGEOM(4,JL)-LGEOM(1,JL) ) - PIVJL
            ALLMOV = .TRUE.
          END IF
        END IF
        IF ( IVEL(IVJL) .LE. 0.15D0*ISPD(IVJL) ) THEN
          ALLMOV = .FALSE.
        END IF
C-----  SUM THE VEHICLES LENGTH PLUS THE AVERAGE OF XRELMI AND XRELMX
        SUMLEN = SUMLEN + LVAP(IVJL) + 0.6D0*(XRELMI+XRELMX)
        IVJL = NOR(IVJL)
      END DO
C-----SUM THE LENGTH OF ALL THE VEHICLES ON AN INTERSECTION PATH
C-----CONNECTING WITH THE LINKING INTERNAL INBOUND OR OUTBOUND LANE
C-----FOR THIS VEHICLES INTERSECTION PATH AND IF ANY OF THOSE VEHICLES
C-----IS TRAVELING LESS THAN OR EQUAL TO VELSTP THEN GO TO 3010 AND SET
C-----THE SLOPE FOR INTERSECTION BLOCKAGE AND RETURN
      DO 1020  JP = 1 , NPATHS
                    IF ( LOBL(JP) . NE . JL )    GO TO 1020
      IVJP   = IFVP(JP)
 1010 CONTINUE
C[    IF ( IVJP               .EQ.-2147483647   )STOP 'CHKINT IVJP   01'
                    IF ( IVJP . EQ . 0 )         GO TO 1020
C-----IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVJP
C-----VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE FALSE
      EVRESP = ( ( IAND( VEHTYP(IV  ),LAVTE ) . EQ . 0 ) . AND .
     *           ( IAND( VEHTYP(IVJP),LAVTE ) . NE . 0 ) )
      IF ( ( MAJCOL(IVJP)                           ) . OR .
     *     ( MAJCLB(IVJP).AND.(IVEL(IVJP).EQ.0.0D0) ) )
     *                                           THEN
        IF ( JP . EQ . KP )                      THEN
          MAJCBL = .TRUE.
          POSCBL = DMIN1( POSCBL,POSCLB(IVJP)+DBLE( LGEOM(4,IL) ) )
          RESPEV = ( RESPEV . OR . EVRESP )
          GO TO 3010
        ELSE
          HWH = 0.5D0*WIDV(IVEHCL(IVJP))
          IF ( LNEXT(IV) . GT . 0 )              THEN
            SAFVEL = DBLE( LIMP(LNEXT(IV)) )
          ELSE
            SAFVEL = DBLE( LIMP(KP       ) )
          END IF
          SAFR = (SAFDIS+(SAFVEL/SAFSPD))/DCHAR(IDRICL(IV))
          IF ( EVRESP )                          THEN
            SAFR = DMAX1( SAFR,EVEHRZ*SAFVEL )
          END IF
          CLEARD = HWM + SAFR + HWH
          CALL  CCLEAR  ( JP,KP,CLEARD,.FALSE.,CLEART )
          IF ( (IPOS(IVJP)+CLEART) . GE . DBLE( LENP(JP) ) )
     *                                           THEN
            MAJCBL = .TRUE.
            POSCBL = DMIN1( POSCBL,POSCLB(IVJP)+DBLE( LGEOM(4,IL) ) )
            RESPEV = ( RESPEV . OR . EVRESP )
            GO TO 3010
          END IF
        END IF
      END IF
C-----IF THE IVJP VEHICLE IS INVOLVED IN A MAJOR COLLISION THEN IF IT IS
C-----ON THE VEHICLES INTERSECTION PATH THEN GO TO 3010 AND SET BLOCKED
C-----ELSE DO NOT ADD THE VEHICLES LENGTH AND PROCESS THE NEXT
C-----INTERSECTION PATH
      IF ( MAJCOL(IVJP) )                        THEN
        IF ( JP . EQ . LNEXT(IV) )               THEN
          GO TO 3010
        ELSE
          GO TO 1020
        END IF
      END IF
C-----IF THE IVJP VEHICLE IS STOPPED AND ITS POSITION IS BEYOND XRELMX
C-----THEN GO TO 3010 AND SET BLOCKED
      IF ( ( IVEL(IVJP) . LE . VELSTP    ) . AND .
     *     ( IPOS(IVJP) . GT . XRELMX    ) . AND .
     *     ( JP         . EQ . LNEXT(IV) ) )     GO TO 3010
      IF ( IVEL(IVJP) . LE . 0.15D0*ISPD(IVJP) ) THEN
        ALLMOV = .FALSE.
      END IF
C-----SUM LENGTH
C[    IF ( SUMLEN           .EQ.-2147483647.0 )STOP 'CHKINT SUMLEN 01'
      SUMLEN = SUMLEN + LVAP(IVJP) + 0.6D0*(XRELMI+XRELMX)
      IVJP   = NOR(IVJP)
      GO TO 1010
 1020 CONTINUE
C-----SUM THE LENGTH OF ALL THE VEHICLES ON THIS VEHICLES INBOUND LANE
C-----THAT ARE AHEAD OF THIS VEHICLE AND USING THIS VEHICLES
C-----INTERSECTION PATH AND IF ANY OF THOSE VEHICLES IS TRAVELING LESS
C-----THAN OR EQUAL TO VELSTP THEN GO TO 3010 AND SET THE SLOPE FOR
C-----INTERSECTION BLOCKAGE AND RETURN
      IVIL   = IFVL(IL)
 2010 CONTINUE
C[    IF ( IVIL               .EQ.-2147483647   )STOP 'CHKINT IVIL   01'
                    IF ( IVIL . EQ . 0  )        GO TO 2020
                    IF ( IVIL . EQ . IV )        GO TO 2020
      VIVIL = IVEL(IVIL)
                    IF ( VIVIL . LE . VELSTP )   GO TO 3010
      IF ( LNEXT(IVIL) . EQ . KP )               THEN
        IF ( IVEL(IVIL) .LE. 0.15D0*ISPD(IVIL) ) THEN
          ALLMOV = .FALSE.
        END IF
C[    IF ( SUMLEN             .EQ.-2147483647.0 )STOP 'CHKINT SUMLEN 02'
        SUMLEN = SUMLEN + LVAP(IVIL) + 0.6D0*(XRELMI+XRELMX)
      END IF
      IVIL   = NOR(IVIL)
      GO TO 2010
 2020 CONTINUE
C-----IF THE SUM OF THE LENGTHS OF ALL THE VEHICLES ON AN INTERSECTION
C-----PATH CONNECTING WITH THE LINKING INTERNAL INBOUND OR OUTBOUND LANE
C-----FOR THIS VEHICLES INTERSECTION PATH PLUS THE SUM OF THE LENGTHS OF
C-----ALL THE VEHICLES ON THIS VEHICLES INBOUND LANE THAT ARE AHEAD OF
C-----THIS VEHICLE AND USING THIS VEHICLES INTERSECTION PATH IS EQ 0.0
C-----THEN GO TO 4010 AND LEAVE THE SLOPE FOR INTERSECTION BLOCKAGE
C-----UNSET AND RETURN (NO VEHICLES AHEAD)
C[    IF ( SUMLEN             .EQ.-2147483647.0 )STOP 'CHKINT SUMLEN 03'
                    IF ( SUMLEN . EQ . 0.0D0 )   GO TO 4010
C-----ADD THE LENGTH OF THIS VEHICLE THAT IS GREATER THAN XRELMX FEET
C-----(TYPICALLY, A VEHICLE OVERHANGING XRELMX FEET ONTO THE
C-----INTERSECTION PATH WHEN STOPPED AT THE BEGINNING OF THE INTERNAL
C-----INBOUND OR OUTBOUND LANE WILL NOT BLOCK OTHER VEHICLES)
      SUMLEN = SUMLEN + DMAX1( LENVAP-XRELMX,0.0D0 )
C-----IF THE LENGTH OF LANE AVAILABLE IS GE THE SUM OF THE LENGTHS OF
C-----ALL THE VEHICLES ON AN INTERSECTION PATH CONNECTING WITH THE
C-----LINKING INTERNAL INBOUND OR OUTBOUND LANE FOR THIS VEHICLES
C-----INTERSECTION PATH PLUS THE SUM OF THE LENGTHS OF ALL THE VEHICLES
C-----ON THIS VEHICLES INBOUND LANE THAT ARE AHEAD OF THIS VEHICLE AND
C-----USING THIS VEHICLES INTERSECTION PATH PLUS THE LENGTH OF THIS
C-----VEHICLE THAT IS GREATER THAN XRELMX FEET THEN GO TO 4010 AND LEAVE
C-----THE SLOPE FOR INTERSECTION BLOCKAGE UNSET AND RETURN
C[    IF ( DISAVL             .EQ.-2147483647.0 )STOP 'CHKINT DISAVL 01'
                    IF ( DISAVL . GE . SUMLEN )  GO TO 4010
C-----IF ALL VEHICLES AHEAD ARE MOVING AND THE LENGTH OF LANE AVAILABLE
C-----IS GE THE SUM OF THE LENGTHS OF ALL THE VEHICLES ON AN
C-----INTERSECTION PATH CONNECTING WITH THE LINKING INTERNAL INBOUND OR
C-----OUTBOUND LANE FOR THIS VEHICLES INTERSECTION PATH PLUS THE SUM OF
C-----THE LENGTHS OF ALL THE VEHICLES ON THIS VEHICLES INBOUND LANE THAT
C-----ARE AHEAD OF THIS VEHICLE AND USING THIS VEHICLES INTERSECTION
C-----PATH PLUS THE LENGTH OF THIS VEHICLE THAT IS GREATER THAN XRELMX
C-----FEET MINUS 2*XRELMX THEN GO TO 4010 AND LEAVE THE SLOPE FOR
C-----INTERSECTION BLOCKAGE UNSET AND RETURN
      IF ( ( ALLMOV                                            ) . AND .
     *     ( DISAVL .GE. (SUMLEN-2.0*XRELMX*DCHAR(IDRICL(IV))) ) )
     *                                           THEN
        GO TO 4010
      END IF
 3010 CONTINUE
C-----SET THE INTERSECTION BLOCKED FLAG TRUE
      IBLOCK = .TRUE.
C[    IBLOCS     = .FALSE.
C-----SET THE SLOPE FOR INTERSECTION BLOCKAGE
      IF ( SETSLP )                              THEN
        POSVEH = DBLE( LGEOM(4,IL) ) + 1.5D0
        VELVEH = 0.0D0
        ACCVEH = 0.0D0
        SLPVEH = 0.0D0
        CALL  SLPCFS  ( SLPTMP,IV,IPOS(IV),IVEL(IV),IACC(IV),ISLP(IV),
     *                            POSVEH  ,VELVEH  ,ACCVEH  ,SLPVEH    )
        IF ( SLPTMP . NE . 0.0D0 )               THEN
          SLPBLK = DMIN1( SLPBLK,SLPTMP )
        END IF
      END IF
      RETURN
 4010 CONTINUE
C-----DO NOT CHECK OTHER INTERSECTION PATHS IF THE VEHICLE HAS BEEN
C-----DELAYED FOR A LONG TIME (THE VEHICLE MAY LATER ABORT ITS DESIRED
C-----OUTBOUND APPROACH)
      IF ( IAFLAG(IA) . EQ . ILETTI )            THEN
        IF ( (DBLE( IQDSI(IV) )*DT     ) . GT .
     *       (TIMOIP/DCHAR(IDRICL(IV))) )        THEN
          GO TO 5010
        END IF
      ELSE
        IF ( (DBLE( IQDS (IV) )*DT     ) . GT .
     *       (TIMOIP/DCHAR(IDRICL(IV))) )        THEN
          GO TO 5010
        END IF
      END IF
C-----CHECK IVPV PATH
      IF ( (CHKOTH         ) . AND .
     *     (IPASS . LT . 2 ) . AND .
     *     (IVPV  . GT . 0 ) )                   THEN
        IPASS = 2
        IF ( MININT(IVPV) )                      THEN
          IF ( IAFLAG(ISNA(LIBL(LPRES(IVPV)))) . EQ . 
     *         IAFLAG(IA                     ) ) THEN
            LP = LPRES(IVPV)
          ELSE
            LP = KP
          END IF
        ELSE
          IF ( LPRES(IV) . EQ . LPRES(IVPV) )    THEN
            LP = LNEXT(IVPV)
          ELSE
            LP = LPREV(IVPV)
          END IF
        END IF
        IF ( LNEXT(IV) . NE . LP )               THEN
          IF ( LP . GT . 0 )                     THEN
            KP = LP
            GO TO 1005
          END IF
        END IF
      END IF
C-----CHECK A PATH FROM IA TO LIBL(INT2P(IV))
      IF ( (CHKOTH             ) . AND .
     *     (IPASS     . LT . 3 ) . AND .
     *     (INT2P(IV) . GT . 0 ) )               THEN
        IPASS = 3
        IF ( INT2P(IV) . NE . LNEXT(IV) )        THEN
          MIBL = LIBL(INT2P(IV))
          DO  LP = 1 , NPATHS
            IF ( ( IIA (LP)  . EQ . IA   ) . AND .
     *           ( LOBL(LP)  . EQ . MIBL ) . AND .
     *           ( LNEXT(IV) . NE . LP   ) . AND .
     *           ( KP        . NE . LP   ) )     THEN
              KP = LP
              GO TO 1005
            END IF
          END DO
        END IF
      END IF
C-----CHECK INTERSECTION PATH MP LINKING OUTBOUND LANE TO THE LEFT FOR
C-----LANE CHANGING VEHICLES THAT ARE LANE CHANGING OUT OF THE
C-----INTERSECTION PATH MP LINKING OUTBOUND LANE
      JL = NLL(LOBL(MP))
      IF ( (CHKOTH          ) . AND .
     *     (IPASS  . LT . 4 ) . AND .
     *     (JL     . GT . 0 ) )                  THEN
        IPASS = 4
        IVJL = ILVL(JL)
        DO WHILE ( IVJL . GT . 0 )
          IF ( ( ISET  (IVJL) . EQ . 1     ) . AND .
     *         ( LATPOS(IVJL) . GT . 0.0D0 ) )   THEN
            CALL  CLCCLN ( IVJL,JL,LCCLN )
            IF ( .NOT. LCCLN )                   THEN
              CALL  SPVAS  ( IVJL,PIVJL,VIVJL,AIVJL,SIVJL,
     *                      .FALSE.,.FALSE.,.FALSE.,.TRUE. )
              SUMLEN = PIVJL - LVAP(IVJL) - DBLE( LGEOM(1,JL) )
     *               - 0.6D0*(XRELMI+XRELMX) 
     *               - DMAX1( LENVAP-XRELMX,0.0D0 )
              IF ( ( SUMLEN . LT . 0.0D0 ) . AND .
     *             ( VIVJL  . LT . 3.0D0 ) )     THEN
                GO TO 3010
              END IF
              EXIT
            END IF
          END IF
          IVJL = NOF(IVJL)
        END DO
      END IF
C-----CHECK INTERSECTION PATH MP LINKING OUTBOUND LANE TO THE RIGHT FOR
C-----LANE CHANGING VEHICLES THAT ARE LANE CHANGING OUT OF THE
C-----INTERSECTION PATH MP LINKING OUTBOUND LANE
      JL = NLR(LOBL(MP))
      IF ( (CHKOTH          ) . AND .
     *     (IPASS  . LT . 5 ) . AND .
     *     (JL     . GT . 0 ) )                  THEN
        IPASS = 5
        IVJL = ILVL(JL)
        DO WHILE ( IVJL . GT . 0 )
          IF ( ( ISET(IVJL)   . EQ . 1     ) . AND .
     *         ( LATPOS(IVJL) . LT . 0.0D0 ) )   THEN
            CALL  CLCCLN ( IVJL,JL,LCCLN )
            IF ( .NOT. LCCLN )                   THEN
              CALL  SPVAS  ( IVJL,PIVJL,VIVJL,AIVJL,SIVJL,
     *                      .FALSE.,.FALSE.,.FALSE.,.TRUE. )
              SUMLEN = PIVJL - LVAP(IVJL) - DBLE( LGEOM(1,JL) )
     *               - 0.6D0*(XRELMI+XRELMX)
     *               - DMAX1( LENVAP-XRELMX,0.0D0 )
              IF ( ( SUMLEN . LT . 0.0D0 ) . AND .
     *             ( VIVJL  . LT . 3.0D0 ) )     THEN
                GO TO 3010
              END IF
              EXIT
            END IF
          END IF
          IVJL = NOF(IVJL)
        END DO
      END IF
 5010 CONTINUE
C-----SET THE INTERSECTION BLOCKED FLAG FALSE
      IBLOCK = .FALSE.
      MAJCBL = .FALSE.
      POSCBL = 0.0D0
C[    IBLOCS     = .FALSE.
C-----LEAVE THE SLOPE FOR INTERSECTION BLOCKAGE UNSET
      RETURN
      END                                                               CHKINT
C
C
C
      SUBROUTINE ACDCP ( ICAVDC )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      LOGICAL           ACOSET,ICAVDC,SLNSET,VEOSET
      INTEGER           K
      DOUBLE PRECISION  CARDIS,CRISLP,DESSPD,FACT,DISEND,DISLCH,DSPLCH,
     *                  PVVELM,T,TMAX,TS,VELLCH,VEHLNG
C3701 FORMAT(3HDM=,F7.3)
C
C-----SUBROUTINE ACDCP CHECKS THE ACC/DEC LOGICAL DEPENDENT ATTRIBUTES,
C-----CALLS THE APPROPRIATE ACC/DEC ROUTINES, AND COMPUTES THE VEHICLES
C-----NEW POS/VEL/ACC
C
C[    K          = -2147483647
C[    CRISLP     = -2147483647.0
C[    T          = -2147483647.0
C[    TS         = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'ACDCP'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----IF VMS DISTRACTED DRIVER MESSAGE IS ACTIVE THEN GO TO 6010 AND
C-----CALL NEWVEL USING CURRENT SLPNEW
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        IF ( IVMSMG(VMSASM(IV)) . EQ . VMSMDD )  THEN
          SLPBLK = 0.0D0
          SLPCON = 0.0D0
          SLPLCH = 0.0D0
          SLPNOF = 0.0D0
          GO TO 6010
        END IF
      END IF
C-----IF ACDCP CALLED BY AVDCON OR CHKCON THEN GO TO 6010 AND CALL
C-----NEWVEL
                    IF ( ICAVDC )                GO TO 6010
                    IF ( (.NOT. MLAG(IV)) )      GO TO 1005
C-----A VEHICLE IS TRYING TO CHANGE LANES AHEAD OF THIS VEHICLE THUS SET
C-----THE LANE CHANGE ACC/DEC SLOPE TO 75 PERCENT OF THE DRIVERS
C-----CRITICAL SLOPE
      CRISLP = SLPMAX*DCHAR(IDRICL(IV))
      SLPLCH = -0.75D0*CRISLP
C-----IF THE DRIVERS ACC/DEC IS ALREADY LT THE DRIVERS CRITICAL SLOPE
C-----THEN USE ONLY HALF OF THE LANE CHANGE ACC/DEC SLOPE
            IF ( ACCOLD . LT . -CRISLP )         SLPLCH = 0.5D0*SLPLCH
C3    JPFLAG = 'FOLLOW LCG'
      MLAG(IV) = .FALSE.
 1005 CONTINUE
C-----IF THE VEHICLE IS IN PIJR TIME THEN GO TO 7090 HOLD THE VEHICLES
C-----SPEED
                    IF ( JPRTM . GT . 0 )        GO TO 7090
      XREL = XRELMX
      MSFLG(IV) = .FALSE.
C-----IF THIS VEHICLE IS THE FIRST VEHICLE IN THIS LANE WHO DECIDED TO
C-----STOP AT THE STOP LINE FOR A YELLOW SIGNAL THEN GO TO 4020 AND
C-----CHECK CRITICAL STOPPING DISTANCE FOR A DECELERATION TO A STOP
                    IF ( IGO . EQ . 2 )          GO TO 4020
                    IF ( (.NOT. ICDFS(IV)) )     GO TO 1010
C-----CONTINUE DECELERATION FOR A STOP
      TMAX = 30.0D0
      CALL  SLPSTD  ( PVPOS,POSOLD,VELOLD,ACCOLD,SLPOLD,TMAX,TS,SLPNEW )
                    IF ( TS . EQ . TIMERR )      SLPNEW = SLPOLD
      SLPNEW = DMAX1( DMIN1( SLPNEW,-0.001D0 ),-ISLPSH )
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        IF ( IVMSMG(VMSASM(IV)) .EQ. VMSMSM )    THEN
C-----    VMSMSM - STOP IMMEDIATELY USING MAXIMUM VEHICLE DECELERATION
          SLPNEW = (DMAX(IVEHCL(IV))-ACCOLD)/DT
          SLPNEW = DMIN1( DMAX1( SLPNEW,-2.0D0*CRISLP ),2.0D0*CRISLP )
        END IF
        IF ( IVMSMG(VMSASM(IV)) .EQ. VMSMSC )    THEN
C-----    VMSMSC - STOP IMMEDIATELY USING COLLISION DECELERATION
          SLPNEW = (DVMSMP(VMSASM(IV))+1.0D-6-ACCOLD)/DT
          SLPNEW = DMIN1( DMAX1( SLPNEW,-SLPCOL ),SLPCOL )
        END IF
      END IF
      MSFLG(IV) = .TRUE.
C-----IF THE PREVIOUS VEHICLE IS NO LONGER STOPPED THEN SET THE FLAG TO
C-----DISCONTINUE DECELERATION FOR A STOP
                    IF ( PVVEL . GT . 0.0D0 )    MSFLG(IV) = .FALSE.
C3    IPFLAG = 'STOPPING  '
      GO TO 6010
 1010 CONTINUE
                    IF ( (.NOT. IFVA(IV)) )      GO TO 2010
 1020 CONTINUE
C-----CALCULATE THE ACC/DEC SLOPE REQUIRED TO FOLLOW THE VEHICLE AHEAD
      CALL  CARFOL
      GO TO 6010
 2010 CONTINUE
                    IF ( (.NOT. IACLDS(IV)) )    GO TO 3010
 2020 CONTINUE
C-----ACCELERATE ACCORDING TO THE LEAD VEHICLES SPEED
      PVVELM = DMAX1( DMIN1( PVVEL,PVVEL+PVACC*DT+0.5D0*PVSLP*DTSQ),
     *                0.0D0                                          )
      DESVEL = DMIN1( PVVELM,DBLE( ISPD(IV) ) )
      CALL  SETDSP  ( IV,POSNEW,DESVEL,.TRUE.,DESVEL )
      XREL = XRELMX
                    IF ( RELVEL . GE . 0.0D0 )   XREL = XRELMI
                    IF ( RELPOS . LE . XREL )    GO TO 3020
      CALL  SETDSP  ( IV,POSNEW,DBLE( ISPD(IV) ),.FALSE.,DESSPD )
      IF ( DESSPD . GT . DESVEL )                THEN
C-----  THE VEHICLES DESIRED SPEED IS GT THE PREVIOUS VEHICLES SPEED SO
C-----  FACTOR THE VEHICLES DESIRED SPEED FOR ACCELERATION
C-----  DESVEL AT ENTRY = MINIMUM OF PVVEL AND ISPD(IV)
C-----  DESSPD AT ENTRY = ISPD(IV)
C-----  (FACT = 0 AND DESVEL = DESVEL WHEN RELPOS =     XRELMX)
C-----  (FACT = 1 AND DESVEL = DESSPD WHEN RELPOS = 3.0*CARDIS)
        IF ( RELVEL . GE . 0.0D0 )               THEN
C-----    THE PREVIOUS VEHICLE IS GOING FASTER THAN THIS VEHICLE
          CARDIS = DMAX1( XRELMX,1.7D0*PVVEL                 )
     *             / DCHAR(IDRICL(IV))
        ELSE
C-----    THE PREVIOUS VEHICLE IS GOING SLOWER THAN THIS VEHICLE
          CARDIS = DMAX1( XRELMX,1.7D0*PVVEL+4.0D0*RELVEL**2 )
     *             / DCHAR(IDRICL(IV))
        END IF
        FACT =
     *    DMAX1( DMIN1( (RELPOS-XRELMX)/(3.0D0*CARDIS-XRELMX),1.0D0 ),
     *           0.0D0                                                 )
            IF ( RELPOS . LE . 0.8D0*CARDIS )    FACT = 0.0D0
        DESVEL = DESVEL + FACT*(DESSPD-DESVEL)
      END IF
      GO TO 3040
 3010 CONTINUE
                    IF ( (.NOT. IACDS(IV)) )     GO TO 4010
 3020 CONTINUE
                    IF ( VELOLD . EQ . 0.0D0 )   GO TO 3030
C-----IF THERE IS A PREVIOUS VEHICLE THEN GO TO 1020 AND CAR FOLLOW THE
C-----PREVIOUS VEHICLE OR GO TO 4020 AND CHECK CRITICAL STOPPING
C-----DISTANCE FOR A DECELERATION TO A STOP
      IF ( IVPV . GT . 0 )                       THEN
        IF ( PVVEL . GT . VELSTP )               THEN
          GO TO 1020
        ELSE
          GO TO 4020
        END IF
      END IF
 3030 CONTINUE
      CALL  MAXDSP  ( IV,-6.0D0,DESVEL,POSNEW,VELOLD,ACCOLD,RELPOS,
     *                IVPV,PVVEL,PVACC,PVSLP                        )
 3040 CONTINUE
C-----ACCELERATE ACCORDING TO THE DESIRED SPEED FOR THIS VEHICLE
      CALL  ACCEL   ( SLNSET,ACOSET,VEOSET )
      GO TO 6010
 4010 CONTINUE
C-----IF THE REMAIN STOPPED FLAG IS SET THEN GO TO 7080 AND REMAIN
C-----STOPPED
                    IF (        IRSTOP(IV)  )    GO TO 7080
                    IF ( (.NOT. ISDEC (IV)) )    GO TO 5010
 4020 CONTINUE
                    IF ( VELOLD . LE . 0.0D0 )   GO TO 7080
 4030 CONTINUE
C-----CHECK CRITICAL STOPPING DISTANCE FOR A DECELERATION TO A STOP AND
C-----IF VIOLATED THEN INITIATE A DECELERATION TO A STOP
      CALL  CRIDIS  ( K )
C-----IF THE VEHICLE DID NOT VIOLATE THE CRITICAL STOPPING DISTANCE FOR
C-----A DECELERATION TO A STOP THIS DT OR WITHIN PIJR TIME THEN GO TO
C-----3030 AND ACCELERATE ACCORDING TO THE DESIRED SPEED FOR THIS
C-----VEHICLE
C[    IF ( K                  .EQ.-2147483647   )STOP 'ACDCP  K      01'
                    IF ( K . EQ . 2 )            GO TO 3030
      GO TO 7010
 5010 CONTINUE
                    IF ( (.NOT. ISTMO(IV)) )     GO TO 9060
C-----CHECK IF STOPPED BUS OR PARKED VEHICLE SHOULD START TO MOVE
                    IF ( (.NOT. MFSTPF(IV)) )    GO TO 9070
C-----PROCESS VEHICLE FORCED TO STOP AND DWELL
      IF ( SDWELL(IV) . GT . 0 )                 THEN
        SDWELL(IV) = SDWELL(IV) - 1
C-----  IF FORCED STOP DWELL TIME HAS ENDED THEN REMOVE FORCED STOP
        IF ( SDWELL(IV) . EQ . 0 )               THEN
          IF ( VMSASM(IV) . GT . 0 )             THEN
            IF ( (IVMSMG(VMSASM(IV)).EQ.VMSMSI) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSL) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSM) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSC) )THEN
              MFSTPF(IV) = .TRUE.
            ELSE
              MFSTPF(IV) = .FALSE.
            END IF
          END IF
          FSTACT(IV) = .FALSE.
          MSAOR (IV) = .FALSE.
          LOGTMP     = 1
          LOGFLG(IV) = 1
C-----    SET PVPOS TO END OF LANE/PATH
          IF ( MININT(IV) )                      THEN
            IVPV  = 0
            PVPOS = ENDLN
            PVVEL = LIMP(IP)
            PVACC = 0.0D0
            PVSLP = 0.0D0
          ELSE
            IVPV  = 0
            PVPOS = ENDLN
            PVVEL = ISLIM(IA)
            PVACC = 0.0D0
            PVSLP = 0.0D0
          END IF
C-----    IF THERE IS A VEHICLE AHEAD IN THE SAME LANE THEN SET PVPOS TO
C-----    THAT VEHICLE
          IF ( NOF(IV) . GT . 0 )                THEN
            IVPV  = NOF(IV)
            CALL  SPVAS  ( IVPV,PVPOS,PVVEL,PVACC,PVSLP,
     *                     .TRUE.,.TRUE.,.FALSE.,.TRUE.  )
          END IF
C-----    IF LANE IS BLOCKED AND PVPOS IS BEYOND ENDLN THEN SET MFINL
C-----    TRUE
          IF ( MBLOCK(IV).AND.(PVPOS.GE.ENDLN) ) THEN
            IVPV = 0
            MFINL(IV) = .TRUE.
          END IF
C-----    IF FIRST IN LANE AND LANE IS BLOCKED THEN SET PVPOS FOR END OF
C-----    BLOCKED LANE
          IF ( MFINL(IV) . AND . MBLOCK(IV) )    THEN
            ENDLN  = DBLE( LGEOM(2,IL) )
            DSPLCH = DBLE( ISPD(IV) )
            IF ( IDISPD(IV) )                    THEN
              DSPLCH = 0.5D0*DSPLCH
            END IF
            IF ( ISPDP (IV) . EQ . 1 )           THEN
              IF ( MININT(IV) )                  THEN
                IF ( LOBL(IP) . GT . 0 )         THEN
                  DSPLCH = DSPLCH*DBLE( ISLIM(ISNA(LOBL(IP))) )
     *                   /        DBLE( LIMP (          IP  ) )
                END IF
              ELSE
                DSPLCH   = DSPLCH*DBLE( ISLIM(          IA  ) )
     *                   /        DBLE( LIMP (LNEXT(    IV )) )
              END IF
            END IF
            VELLCH = 0.2D0*DSPLCH
            VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
            VEHLNG = DMIN1( 25.0D0,LENVAP )
            DISLCH = 0.5D0*(ENDLN-POSNEW)
            DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
            DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
            IF ( MAJRLC(IV) )                    THEN
              DISLCH = 0.5D0*XRELMI
            END IF
            DISEND = DMAX1( DISLCH,DMIN1( 0.5D0*ENDLN,
     *                                    0.5D0*(ENDLN-POSNEW)) )
            IVPV   = 0
            PVPOS  = DMAX1( ENDLN-DISEND,POSNEW )
            PVVEL  = 0.001D0
            PVACC  = -32.0D0
            PVSLP  =   0.0D0
          END IF
        END IF
      END IF
 6010 CONTINUE
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER DT SECONDS
C-----(POS/VEL/ACC IS ALSO COMPUTED IN CRIDIS IF K NE 2 BUT GOES TO 7010
C-----AFTERWARDS AND DOES NOT COME THROUGH THIS CODE)
      CALL  NEWVEL  ( DT,DTSQ,DTCU )
C-----IF THIS VEHICLE WAS PREVIOUSLY STOPPED AND THE NEW VELOCITY IS EQ
C-----ZERO THEN GO TO 7080 AND REMAIN STOPPED
      IF ( (VELOLD.EQ.0.0D0) . AND .
     *     (VELNEW.EQ.0.0D0) )                   GO TO 7080
 7010 CONTINUE
      MSTPF(IV) = .FALSE.
C-----IF THIS VEHICLES VELOCITY IS GT 0 THEN RETURN
                    IF (  VELNEW .GT. VELSTP )   RETURN
                    IF ( (VELNEW .GT. 0.0D0  ) . AND .
     *                   (ACCNEW .GT. 0.0D0  ) . AND .
     *                   (SLPNEW .GT. 0.0D0  ) ) RETURN
C-----THE VEHICLE STOPPED THIS DT
      LOGTMP = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
C-----CALCULATE THE TIME REQUIRED TO BRING THE VEHICLE TO A STOP WITHIN
C-----THIS DT
      VELOLD = DMAX1( VELOLD,0.0D0 )
      CALL  TIMSTP  ( VELOLD,ACCOLD,SLPNEW,1.05D0*DT,T )
      IF ( T . EQ . TIMERR )                     THEN
        IF ( VELOLD . LE . VSMALL )              THEN
          T = 0.0D0
        ELSE
          T = DT
        END IF
      END IF
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER T SECONDS
C-----(THE VELOCITY SHOULD BE 0)
C[    IF ( T                  .EQ.-2147483647.0 )STOP 'ACDCP  T      01'
      CALL  NEWVEL  ( T,T**2,T**3 )
C3    WRITE (JPFLAG,701) ACCNEW
C-----UPDATE THE VEHICLES MAXIMUM DECELERATION RATE
      IVMAXD(IV) = MAX0( IVMAXD(IV),IDNINT( -ACCNEW*10.0D0 ) )
      VELNEW = 0.0D0
      ACCNEW = 0.0D0
      SLPNEW = 0.0D0
 7040 CONTINUE
                    IF (        MCHKCF(IV)  )    XREL = XRELMI
                    IF (        MININT(IV)  )    GO TO 7080
                    IF ( (.NOT. MFINL (IV)) )    GO TO 7080
                    IF (        MATSTL(IV)  )    GO TO 7080
                    IF ( RELEND . GT . XREL )    GO TO 7080
      MATSTL(IV) = .FALSE.
C-----THIS VEHICLE IS LE XREL DISTANCE FROM THE END OF THE LANE THUS IF
C-----THE LANE IS NOT BLOCKED THEN THE VEHICLE IS STOPPED AT THE STOP
C-----LINE
                    IF ( (.NOT. MBLOCK(IV)) )    MATSTL(IV) = .TRUE.
                    IF ( (.NOT. MATSTL(IV)) )    GO TO 7080
C-----THE VEHICLE IS STOPPED AT THE STOP LINE ON AN INBOUND APPROACH SO
C-----ADD THE STOPPED VEHICLE TO THE LIST OF VEHICLES AT THE
C-----INTERSECTION
      CALL  ADLVAI
C-----CHECK IF LEFT-TURN-ON-RED OR RIGHT-TURN-ON-RED MAY BE MADE BASED
C-----ON THE LANE CONTROL FOR THIS LANE
C-----                                            NO
C-----                                           LTOR   LTOR   RTOR
C-----                                           RTOR
                    IF ( LCONTV(IV) - LCSLTR )   7075 , 7050 , 7060
 7050 CONTINUE
C-----LEFT-TURN-ON-RED PERMITTED FOR THIS LANE
C-----IF THIS VEHICLE IS NOT GOING TO TURN LEFT THEN GO TO 7075 ELSE SET
C-----LEFT-TURN-ON-RED FLAG
                    IF ( ITURN(IV) .NE. ITURNL ) GO TO 7075
      GO TO 7070
 7060 CONTINUE
C-----RIGHT-TURN-ON-RED PERMITTED FOR THIS LANE
C-----IF THIS VEHICLE IS NOT GOING TO TURN RIGHT THEN GO TO 7075 ELSE SET
C-----RIGHT-TURN-ON-RED FLAG
                    IF ( ITURN(IV) .NE. ITURNR ) GO TO 7075
 7070 CONTINUE
C-----SET THE LEFT-TURN-ON-RED OR RIGHT-TURN-ON-RED FLAG
      MLRTOR(IV) = .TRUE.
      MTCARS(IV) = .FALSE.
      LOGTMP = 2 + IPIJR(IDRICL(IV))
C3    KPFLAG = 'I MAY RTOR'
      GO TO 7080
 7075 CONTINUE
      LOGTMP     = 1
      LOGFLG(IV) = 1
 7080 CONTINUE
C-----THE VEHICLE IS STOPPED
C-----SET THE VEHICLES ACC/DEC LOGIC TIMER
      IPRTM(IV) = MAX0( MIN0( MPRTM,LOGTMP-2 ),0 )
C-----IF THE VEHICLE WAS TRYING NOT TO STOP THEN RESET THE VEHICLES
C-----ACC/DEC LOGIC TIMER TO ZERO
                    IF ( SLPNEW . GT . 0.0D0 )   IPRTM(IV) = 0
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
C-----RESET SOME OF THE VEHICLES PARAMETERS
      SLPNEW = 0.0D0
      ACCNEW = 0.0D0
      VELNEW = 0.0D0
      MSTPF(IV) = .TRUE.
      MSAOR(IV) = .FALSE.
      MSFLG(IV) = .FALSE.
C3    IPFLAG = 'MOVE UP   '
C-----IF THE VEHICLE IS STOPPED MORE THAN XREL FEET FROM THE PREVIOUS
C-----VEHICLE THEN MOVE UP ELSE REMAIN STOPPED
      XREL = XRELMX
                    IF ( PVACC  . GT . 0.0  )    XREL = XRELMI
                    IF ( MAJRLC(IV)         )    XREL = -0.5D0*XRELMI
                    IF ( RELPOS . GT . XREL )    GO TO 7085
      MSAOR(IV) = .TRUE.
      IPRTM(IV) = 0
      IF ( MFSTPF(IV) )                          THEN
        IF ( FSTACT(IV) )                        THEN
          IF ( SDWELL(IV).EQ.0 )                 THEN
            SDWELL(IV) = IDNINT( FSTDTM(IV)/DT )
C-----      IF FORCED STOP DWELL TIME HAS ENDED THEN REMOVE FORCED STOP
            IF ( SDWELL(IV) . EQ . 0 )           THEN
              MFSTPF(IV) = .FALSE.
              FSTACT(IV) = .FALSE.
              MSAOR (IV) = .FALSE.
              LOGTMP     = 1
              LOGFLG(IV) = 1
C-----        SET PVPOS TO END OF LANE/PATH
              IF ( MININT(IV) )                  THEN
                IVPV  = 0
                PVPOS = ENDLN
                PVVEL = LIMP(IP)
                PVACC = 0.0D0
                PVSLP = 0.0D0
              ELSE
                IVPV  = 0
                PVPOS = ENDLN
                PVVEL = ISLIM(IA)
                PVACC = 0.0D0
                PVSLP = 0.0D0
              END IF
            END IF
          END IF
C-----    IF THERE IS A VEHICLE AHEAD IN THE SAME LANE THEN SET PVPOS TO
C-----    THAT VEHICLE
          IF ( NOF(IV) . GT . 0 )                THEN
            IVPV  = NOF(IV)
            CALL  SPVAS  ( IVPV,PVPOS,PVVEL,PVACC,PVSLP,
     *                     .TRUE.,.TRUE.,.FALSE.,.TRUE.  )
          END IF
C-----    IF LANE IS BLOCKED AND PVPOS IS BEYOND ENDLN THEN SET MFINL TRUE
          IF ( MBLOCK(IV).AND.(PVPOS.GE.ENDLN) ) THEN
            IVPV = 0
            MFINL(IV) = .TRUE.
          END IF
C-----    IF FIRST IN LANE AND LANE IS BLOCKED THEN SET PVPOS FOR END OF
C-----    BLOCKED LANE
          IF ( MFINL(IV) . AND . MBLOCK(IV) )    THEN
            ENDLN  = DBLE( LGEOM(2,IL) )
            DSPLCH = DBLE( ISPD(IV) )
            IF ( IDISPD(IV) )                    THEN
              DSPLCH = 0.5D0*DSPLCH
            END IF
            IF ( ISPDP (IV) . EQ . 1 )           THEN
              IF ( MININT(IV) )                  THEN
                IF ( LOBL(IP) . GT . 0 )         THEN
                  DSPLCH = DSPLCH*DBLE( ISLIM(ISNA(LOBL(IP))) )
     *                   /        DBLE( LIMP (          IP  ) )
                END IF
              ELSE
                DSPLCH   = DSPLCH*DBLE( ISLIM(          IA  ) )
     *                   /        DBLE( LIMP (LNEXT(    IV )) )
              END IF
            END IF
            VELLCH = 0.2D0*DSPLCH
            VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
            VEHLNG = DMIN1( 25.0D0,LENVAP )
            DISLCH = 0.5D0*(ENDLN-POSNEW)
            DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
            DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
            IF ( MAJRLC(IV) )                    THEN
              DISLCH = 0.5D0*XRELMI
            END IF
            DISEND = DMAX1( DISLCH,DMIN1( 0.5D0*ENDLN,
     *                      0.5D0*(ENDLN-POSNEW))      )
            IVPV   = 0
            PVPOS  = DMAX1( ENDLN-DISEND,POSNEW )
            PVVEL  = 0.001D0
            PVACC  = -32.0D0
            PVSLP  =   0.0D0
          END IF
        END IF
      END IF
C3    IPFLAG = 'STOPPED   '
C3                  IF ( RELPOS . LT . -1.0D0 )  KPFLAG = 'NEG RELPOS'
 7085 CONTINUE
C-----COMPUTE NEW INTERSECTION CONTROL LOGIC
      IF ( LOGTMP . EQ.  1 )                     THEN
        CALL  LOGIC  ( 8,IV )
      END IF
      RETURN
 7090 CONTINUE
C-----IF THE VEHICLE IS MOVING, SHOULD BE CHECKING TO STOP, AND HAS
C-----PIJR TIME THEN GO TO 4030 AND CALL CRIDIS
                    IF ( VELOLD . LE . 0.0D0 )   RETURN
                    IF ( ISDEC(IV) )             GO TO 4030
C-----HOLD THE VEHICLES SPEED AT ITS CURRENT VALUE
      CALL  HOLDSP  ( JPRTM )
      RETURN
C-----PROCESS THE EXECUTION ERRORS AND STOP
 9060 CONTINUE
      CALL  ABORTR  ( 'STOP 906 - '                      //
     *                'NO INDEPENDENT ATTRIBUTE TRUE - ' //
     *                'ACDCP'                               )
      STOP  906
 9070 CONTINUE
      CALL  ABORTR  ( 'STOP 907 - '                              //
     *                'MPOBS STOPPED VEHICLES NOT PROGRAMMED - ' //
     *                'ACDCP'                                       )
      STOP  907
      END                                                               ACDCP
C
C
C
      SUBROUTINE CARFOL
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'LANECH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           ACOSET,SLNSET,VEOSET
      INTEGER           NOFT
      DOUBLE PRECISION  A,ACCMAX,ACCN,B,C,CARDEC,CARDIS,CRISLP,DECVEH,
     *                  DESSPD,DIST,FACT,PIJRIV,PVSTP,PVVELM,RANGE,
     *                  SFACT,SLOPE,SLOPEU,SLPSTP,SPD,T,TMAX,TS,TSMAX,
     *                  T1,VT1,XCRIT
      DATA     RANGE  / -6.0D0 /
      DATA     SFACT  /  1.0D0 /
C3701 FORMAT(3HRV=,F7.2)
C3702 FORMAT(3HRP=,F7.2)
C3703 FORMAT(3HCD=,F7.2)
C
C-----SUBROUTINE CARFOL CALCULATES THE ACC/DEC SLOPE REQUIRED TO FOLLOW
C-----THE VEHICLE AHEAD
C
C[    NOFT       = -2147483647
C[    A          = -2147483647.0
C[    ACCMAX     = -2147483647.0
C[    ACCN       = -2147483647.0
C[    B          = -2147483647.0
C[    C          = -2147483647.0
C[    CARDEC     = -2147483647.0
C[    CARDIS     = -2147483647.0
C[    CRISLP     = -2147483647.0
C[    DECVEH     = -2147483647.0
C[    DIST       = -2147483647.0
C[    FACT       = -2147483647.0
C[    PVSTP      = -2147483647.0
C[    SLOPE      = -2147483647.0
C[    SLOPEU     = -2147483647.0
C[    SLPSTP     = -2147483647.0
C[    SPD        = -2147483647.0
C[    T          = -2147483647.0
C[    TS         = -2147483647.0
C[    T1         = -2147483647.0
C[    VT1        = -2147483647.0
C[    XCRIT      = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CARFOL'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      NOFT = NOF(IV)
C-----INITIALIZE SOME PARAMETERS FOR CARFOL
      DECVEH = DMAX(IVEHCL(IV))
      CRISLP = SLPMAX*DCHAR(IDRICL(IV))
      PIJRIV =        PIJR (IDRICL(IV))
C3    JPFLAG = 'FOLLOWING '
      PVVEL = DMAX1( PVVEL,0.01D0 )
      PVSTP = -POSMAX
C-----IF THE PREVIOUS VEHICLE IS GOING FASTER THAN THIS VEHICLE THEN
C-----GO TO 4010 AND CHECK FURTHER
      XREL = XRELMX
                    IF ( RELVEL . GE . 0.0D0 )   XREL = XRELMI
      IF ( ( RELVEL . GE . 0.0D0 ) . AND .
     *     ( RELPOS . GE . XREL  ) )             GO TO 4010
 2010 CONTINUE
C-----FIND THE CONSERVATIVE CAR FOLLOWING DISTANCE
      CARDIS = DMAX1( XRELMX,1.7D0*PVVEL+4.0D0*RELVEL**2 )
     *         / DCHAR(IDRICL(IV))
C-----IF THE VEHICLE IS FURTHER THAN CARDIS FROM THE PREVIOUS VEHICLE
C-----THEN GO TO 7010 AND CHECK FURTHER
                    IF ( RELPOS . GT . CARDIS )  GO TO 7010
      IF ( RELPOS . LE . 0.0D0 )                 THEN
        SLPNEW = -2.0D0*CRISLP
        RETURN
      END IF
      RELPOS = DMAX1( RELPOS,0.01D0 )
C-----CALCULATE THE REQUIRED ACC/DEC USING THE NON-INTEGER GENERALIZED
C-----CAR FOLLOWING EQUATION
      CARDEC = CAREQA * ((VELOLD**CAREQM)/(RELPOS**CAREQL)) * RELVEL
C-----BOUND THE REQUIRED ACC/DEC
      CARDEC = DMIN1( DMAX1( CARDEC,DECVEH ),-0.04D0 )
C-----CALCULATE THE REQUIRED ACC/DEC SLOPE TO BRING THE VEHICLES ACC/DEC
C-----TO CARDEC IN ONE SECOND
      SLPNEW = CARDEC-ACCOLD
                    IF ( RELPOS . LE . XRELMI )  GO TO 2020
C-----IF NO VEHICLE AHEAD THEN GO TO 2020
                    IF ( IVPV . EQ . 0 )         GO TO 2020
C-----AASHTO RECOMMENDS 11.2FT/SEC/SEC DECELERATION RATE
      TSMAX = DMAX1( 1.5D0*(PVVEL/11.2D0 + PIJRIV),10.0D0 )
      CALL  TIMSTP  ( PVVEL,PVACC,PVSLP,TSMAX,T )
                    IF ( T . EQ . TIMERR )       GO TO 2020
C-----CALCLATE THE POSITION FOR THE VEHICLE AHEAD AFTER IT STOPS
      PVSTP = PVPOS + PVVEL*T + 0.5D0*PVACC*T**2 + ONED6*PVSLP*T**3
 2015 CONTINUE
C-----CALCLATE THE PARAMETERS FOR A DECELERATION TO A STOP BEHIND THE
C-----VEHICLE AHEAD WHEN IT STOPS
      XCRIT = DMIN1( 0.9D0*(PVSTP-POSOLD),(PVSTP-POSNEW) )
C-----AASHTO RECOMMENDS 11.2FT/SEC/SEC DECELERATION RATE
      TSMAX = DMAX1( 1.5D0*(VELOLD/11.2D0 + PIJRIV),10.0D0 )
      CALL  SLPSTD  ( POSOLD+XCRIT,POSOLD,VELOLD,ACCOLD,SLPNEW,
     *                TSMAX,TS,SLPSTP                           )
      IF ( ( TS     . EQ . TIMERR ) . OR .
     *     ( SLPSTP . EQ . 0.0D0  ) )            GO TO 2020
C-----USE THE SLOPE TO STOP BEHIND THE VEHICLE AHEAD WHEN IT STOPS
      SLPNEW = SLPSTP
C-----IF VEHICLE AHEAD IS DECELERATING FOR A STOP THEN ALLOW MAXIMUM
C-----JERK ELSE LIMIT JERK
      IF ( MSFLG(IVPV) )                         THEN
        SLPNEW = DMIN1( DMAX1( SLPNEW,-ISLPSH ),-0.001D0 )
      ELSE
C-----IF THE VEH ACC/DEC SLOPE IS GE -SLPMAX THEN
C-----     FACT = -1.0
C-----IF THE VEH ACC/DEC SLOPE IS GT -SLPMAX+RANGE AND LT -SLPMAX THEN
C-----     FACT = -1.0 - SFACT*(SLPNEW+SLPMAX)/RANGE
C-----IF THE VEH ACC/DEC SLOPE IS LE -SLPMAX+RANGE THEN
C-----     FACT = -1.0 - SFACT
        FACT = - 1.0D0
     *         - SFACT*DMAX1( DMIN1( 1.0D0,(SLPNEW+SLPMAX)/RANGE ),
     *                               0.0D0 )
C-----BOUND THE ACC/DEC SLOPE FOR CAR FOLLOWING
        SLPNEW = DMIN1( DMAX1( SLPNEW,FACT*1.3D0*CRISLP ),1.3D0*CRISLP )
      END IF
C;    WRITE (TC6,'(3F12.2)') CAREQA*(ACCOLD+SLPNEW*DT)/CARDEC,CARDEC,
C;   *                              (ACCOLD+SLPNEW*DT)
      GO TO 2025
 2020 CONTINUE
C-----IF THE VEH ACC/DEC SLOPE IS GE -SLPMAX THEN
C-----     FACT = -1.0
C-----IF THE VEH ACC/DEC SLOPE IS GT -SLPMAX+RANGE AND LT -SLPMAX THEN
C-----     FACT = -1.0 - SFACT*(SLPNEW+SLPMAX)/RANGE
C-----IF THE VEH ACC/DEC SLOPE IS LE -SLPMAX+RANGE THEN
C-----     FACT = -1.0 - SFACT
      FACT = - 1.0D0
     *       - SFACT*DMAX1( DMIN1( 1.0D0,(SLPNEW+SLPMAX)/RANGE ),
     *                             0.0D0 )
C-----BOUND THE ACC/DEC SLOPE FOR CAR FOLLOWING
      SLPNEW = DMIN1( DMAX1( SLPNEW,FACT*0.65D0*CRISLP ),1.3D0*CRISLP )
 2025 CONTINUE
C3    WRITE (IPFLAG,701) RELVEL
C3    WRITE (JPFLAG,702) RELPOS
      GO TO 7030
 2030 CONTINUE
      SLPNEW = -CRISLP
                    IF ( VELOLD . EQ . 0.0D0   ) RETURN
                    IF ( PVSTP  . EQ . -POSMAX ) PVSTP = PVPOS
      GO TO 2015
 3010 CONTINUE
 3020 CONTINUE
 4010 CONTINUE
C-----THE PREVIOUS VEHICLE IS GOING FASTER THAN THIS VEHICLE SO RESET
C-----THE CAR FOLLOWING DISTANCE
      CARDIS = DMAX1( XRELMX,1.7D0*PVVEL )/DCHAR(IDRICL(IV))
C-----IF THE RELATIVE POSITION OF THE VEHICLE IS LT 1.2 TIMES THE CAR
C-----FOLLOWING DISTANCE THEN GO TO 5010 AND CHECK FURTHER
      IF ( RELPOS . LT . 1.2D0*DMAX1( CARDIS,18.0D0 ) )
     *                                           GO TO 5010
 4020 CONTINUE
      CALL  SETDSP  ( IV,POSNEW,DBLE( ISPD(IV) ),.FALSE.,DESVEL )
                    IF ( DESVEL . LE . VELSTP )  GO TO 2030
                    IF ( DESVEL . LE . PVVEL )   GO TO 4030
C-----THE VEHICLES DESIRED SPEED IS GT THE PREVIOUS VEHICLES SPEED SO
C-----FACTOR THE VEHICLES DESIRED SPEED FOR ACCELERATION
C-----DESVEL AT ENTRY = ISPD(IV)
C-----(FACT = 0 AND DESVEL = PVVEL  WHEN RELPOS =     XRELMX)
C-----(FACT = 1 AND DESVEL = DESVEL WHEN RELPOS = 3.0*CARDIS)
      IF ( RELVEL . GE . 0.0D0 )                 THEN
C-----  THE PREVIOUS VEHICLE IS GOING FASTER THAN THIS VEHICLE
        CARDIS = DMAX1( XRELMX,1.7D0*PVVEL                 )
     *           / DCHAR(IDRICL(IV))
      ELSE
C-----  THE PREVIOUS VEHICLE IS GOING SLOWER THAN THIS VEHICLE
        CARDIS = DMAX1( XRELMX,1.7D0*PVVEL+4.0D0*RELVEL**2 )
     *           / DCHAR(IDRICL(IV))
      END IF
      FACT =
     *  DMAX1( DMIN1( (RELPOS-XRELMX)/(3.0D0*CARDIS-XRELMX),1.0D0 ),
     *         0.0D0                                                 )
            IF ( RELPOS . LE . 0.8D0*CARDIS )    FACT = 0.0D0
      PVVELM = DMAX1( DMIN1( PVVEL,PVVEL+PVACC*DT+0.5D0*PVSLP*DTSQ),
     *                0.0D0                                          )
      DESVEL = PVVELM + FACT*(DESVEL-PVVELM)
      CALL  SETDSP  ( IV,POSNEW,DESVEL,.TRUE.,DESVEL )
                    IF ( DESVEL . LE . VELSTP )  GO TO 2030
 4030 CONTINUE
      CALL  MAXDSP  ( IV,-6.0D0,DESVEL,POSNEW,VELOLD,ACCOLD,RELPOS,
     *                IVPV,PVVEL,PVACC,PVSLP                        )
C-----ACCELERATE ACCORDING TO THE DESIRED SPEED FOR THIS VEHICLE
      CALL  ACCEL   ( SLNSET,ACOSET,VEOSET )
      RETURN
 5010 CONTINUE
C-----THE VEHICLES RELATIVE POSITION IS LT 1.2*CARDIS SO RESET CARDIS
C[    IF ( CARDIS             .EQ.-2147483647.0 )STOP 'CARFOL CARDIS 02'
      CARDIS = 0.8D0*CARDIS
C-----IF THE VEHICLES RELATIVE POSITION IS BETWEEN 80 PERCENT AND 120
C-----PERCENT OF THE CARDIS FROM STATEMENT 4010 THEN GO TO 6010 AND
C-----ACCELERATE TO THE PREVIOUS VEHICLES SPEED
                    IF ( RELPOS . GT . CARDIS )  GO TO 6010
C-----IF THE VEHICLES OLD VELOCITY IS LE THE PREVIOUS VEHICLES VELOCITY
C-----THEN GO TO 4020 AND ACCELERATE TO THE FACTORED DESIRED SPEED
                    IF ( VELOLD . LE . PVVEL )   GO TO 4020
C-----FIND THE TIME AND VELOCITY WHEN THE VEHICLES ACCELERATION WOULD BE
C-----ZERO USING HALF THE CRITICAL SLOPE FOR THE DRIVER
      SLPNEW = 0.5D0*CRISLP
      T1 = -ACCOLD/SLPNEW
      VT1 = VELOLD + ACCOLD*T1 + 0.5D0*SLPNEW*T1**2
      CALL  SETDSP  ( IV,POSNEW,DBLE( ISPD(IV) ),.FALSE.,DESSPD )
                    IF ( DESSPD . LE . VELSTP )  GO TO 2030
      PVVELM = DMAX1( DMIN1( PVVEL,PVVEL+PVACC*DT+0.5D0*PVSLP*DTSQ),
     *                0.0D0                                          )
      SPD = DMIN1( DESSPD,PVVELM )
                    IF ( SPD . LE . VELSTP )     GO TO 2030
C-----FIND THE ACCELERATION THE VEHICLE WOULD USE TO GET TO HIS DESIRED
C-----SPEED
      ACCMAX = AUTOL*(3.2D0+0.08D0*SPD)*DCHAR(IDRICL(IV))
      ACCN   = ACCMAX*(1.0D0-(VT1/(1.15D0*SPD)))
                    IF ( ACCN . LE . 0.0D0 )     GO TO 5020
C-----FIND THE TIME AND RELATIVE DISTANCE TRAVELED WHILE BRINGING THE
C-----VELOCITY BACK UP TO THE DESIRED SPEED
      T = T1 + ACCN/SLPNEW + 0.25D0*DTMAX
      DIST = VELOLD*T + 0.5D0*ACCOLD*T**2 + ONED6*SLPNEW*T**3 - PVVEL*T
C-----IF THE NEW RELATIVE DISTANCE WOULD BE GE THE CAR FOLLOWING
C-----DISTANCE THEN START ACCELERATING AT HALF CRITICAL SLOPE
C[    IF ( CARDIS             .EQ.-2147483647.0 )STOP 'CARFOL CARDIS 03'
            IF ( RELPOS-DIST . GE . CARDIS )     GO TO 5030
 5020 CONTINUE
C-----SET THE ACC/DEC SLOPE TO MOVE THE VEHICLE BACK AWAY FROM THE
C-----PREVIOUS VEHICLE
C[    IF ( CARDIS             .EQ.-2147483647.0 )STOP 'CARFOL CARDIS 04'
      SLPNEW = 0.10D0*DECVEH*DCHAR(IDRICL(IV))*(CARDIS-RELPOS)/CARDIS
 5030 CONTINUE
C-----BOUND THE ACC/DEC SLOPE WHEN THE VEHICLE IS LT 0.8*CARDIS AND
C-----CHECK FOR DECELERATION TO THE DESIRED SPEED
      SLPNEW = DMAX1( SLPNEW,-CRISLP )
C[    IF ( CARDIS             .EQ.-2147483647.0 )STOP 'CARFOL CARDIS 05'
C3    WRITE (IPFLAG,703) CARDIS
C3    WRITE (JPFLAG,702) RELPOS
      GO TO 7030
 6010 CONTINUE
C-----THE VEHICLES RELATIVE POSITION IS BETWEEN 80 AND 120 PERCENT OF
C-----CARDIS SO ACCELERATE TO THE MINIMUM OF THE DESIRED SPEED AND THE
C-----PREVIOUS VEHICLES VELOCITY
C3    JPFLAG = 'CARDIS    '
      PVVELM = DMAX1( DMIN1( PVVEL,PVVEL+PVACC*DT+0.5D0*PVSLP*DTSQ),
     *                0.0D0                                          )
      DESVEL = DMIN1( DESVEL,PVVELM )
      CALL  SETDSP  ( IV,POSNEW,DESVEL,.TRUE.,DESVEL )
                    IF ( DESVEL . LE . VELSTP )  GO TO 2030
      GO TO 4030
 7010 CONTINUE
C-----THE PREVIOUS VEHICLE IS GOING SLOWER THAN THIS VEHICLE BUT IF HIS
C-----RELATIVE POSITION IS GT 120 PERCENT OF CARDIS THEN ACCELERATE
C[    IF ( CARDIS             .EQ.-2147483647.0 )STOP 'CARFOL CARDIS 06'
      IF ( RELPOS . GT . 1.2D0*DMAX1( CARDIS,18.0D0 ) )
     *                                           GO TO 4020
C-----IF THE VEHICLES ACC/DEC IS VERY SMALL THEN GO TO 7020 AND SET
C-----THE VEHICLES ACC/DEC AND HIS ACC/DEC SLOPE TO ZERO
            IF ( DABS( ACCOLD ) . LE . 0.01D0 )  GO TO 7020
C-----FIND THE ACC/DEC SLOPE TO BRING THE VEHICLES ACC/DEC TO ZERO IN
C-----PIJR TIME
      SLPNEW = -1.01D0*ACCOLD/PIJRIV
C-----IF THE VEHICLES ACC/DEC SLOPE OLD IS GT THE VEHICLES ACC/DEC SLOPE
C-----NEW AND THE SLOPES ARE THE SAME SIGN THEN USE THE VEHICLES OLD
C-----ACC/DEC SLOPE
      IF ( ( DABS( SLPOLD ).GT.DABS( SLPNEW ) ) . AND .
     *     ( SLPOLD*SLPNEW .GT.0.0D0          ) )SLPNEW = SLPOLD
      SLPNEW = DMIN1( DMAX1( SLPNEW,-CRISLP ),CRISLP )
      ACCNEW = ACCOLD + SLPNEW*DT
C-----IF THE ACC/DEC CHANGES SIGNS IN ONE DT THEN SET THE ACC/DEC SLOPE
C-----TO MAKE THE VEHICLES ACC/DEC ZERO IN ONE DT
                    IF ( ACCOLD*ACCNEW.LT.0.0D0 )SLPNEW = -ACCOLD/DT
C3    IPFLAG = 'REDUCE A/D'
C3    JPFLAG = 'TO 0 CARFL'
      GO TO 7030
 7020 CONTINUE
C-----SET THE VEHICLES ACC/DEC AND ACC/DEC SLOPE TO ZERO
      ACCOLD = 0.0D0
      SLPNEW = 0.0D0
C3    IPFLAG = 'STEADY    '
C3    JPFLAG = 'CARDIS    '
 7030 CONTINUE
C-----IF THE VEHICLES OLD VELOCITY IS LE HIS DESIRED SPEED THEN RETURN
C-----ELSE CHECK TO SEE IF THIS VEHICLE SHOULD BEGIN TO DECELERATE TO
C-----HIS DESIRED SPEED ONE SECOND BEFORE HE REACHES THE END OF HIS LANE
                    IF ( VELOLD . LE . DESVEL )  RETURN
      SLOPE = -0.25D0*CRISLP
                    IF ( ACCOLD . LT . SLOPE )   SLOPE = 0.5D0*SLOPE
      IF ( ENDLN . GE . POSBIG )                 THEN
        T = PIJRIV
        GO TO 7035
      END IF
C-----DESVEL = VELOLD + ACCOLD*T + 0.5*SLOPE*T**2
C-----ENDLN - DESVEL = POSOLD + VELOLD*T + ACCOLD*T**2/2 + SLOPE*T**3/6
C-----(ACCOLD/6)*T**2 + ((2*VELOLD+DESVEL)/3)*T + (POSOLD-ENDLN+DESVEL)
C-----                                                               = 0
      A    = ONED6*ACCOLD
      B    = ONED3*(2.0D0*VELOLD+DESVEL)
      C    = POSOLD - DMIN1( PVPOS,ENDLN ) + DESVEL
      TMAX = 30.0D0
      CALL  TMQUAD  ( A,B,C,TMAX,T )
                    IF ( T . EQ . TIMERR )       GO TO 7040
C-----FIND THE ACC/DEC SLOPE REQUIRED TO REDUCE THE VEHICLES VELOCITY
C-----TO HIS DESIRED SPEED BEFORE HE GETS TO THE END OF HIS LANE AND
C-----BOUND THE ACC/DEC SLOPE
 7035 CONTINUE
C[    IF ( SLOPE              .EQ.-2147483647.0 )STOP 'CARFOL SLOPE  01'
C[    IF ( T                  .EQ.-2147483647.0 )STOP 'CARFOL T      01'
      IF ( T . GT . 0.0D0 )                      THEN
        SLOPE = DMIN1( SLOPE,2.0D0*(DESVEL-VELOLD-ACCOLD*T)/T**2 )
      END IF
 7040 CONTINUE
                    IF ( ACCOLD . GE . 0.0D0 )   GO TO 7050
C-----FIND THE ACC/DEC SLOPE REQUIRED TO BRING THE VEHICLES ACC/DEC TO
C-----ZERO BY THE TIME THE VEHICLE REACHES HIS DESIRED SPEED
      SLOPEU = -0.5D0*ACCOLD**2/(DESVEL-VELOLD)
            IF ( SLOPEU . LT . 0.40D0*CRISLP )   GO TO 7050
C-----THE VEHICLE SHOULD START BRINGING THE ACC/DEC TO ZERO THUS BOUND
C-----THE ACC/DEC SLOPE FOR DECELERATING TO THE VEHICLES DESIRED SPEED
      SLOPE = DMIN1( SLOPEU,CRISLP )
 7050 CONTINUE
C-----BOUND THE ACC/DEC SLOPE FOR DECELERATING TO THE VEHICLES DESIRED
C-----SPEED
C[    IF ( SLOPE              .EQ.-2147483647.0 )STOP 'CARFOL SLOPE  02'
      SLOPE = DMAX1( SLOPE,-CRISLP )
                    IF ( SLOPE . GT . SLPNEW )   RETURN
C-----SET THE ACC/DEC SLOPE FOR DECELERATING TO THE VEHICLES DESIRED
C-----SPEED
C3    KPFLAG = 'DEC DESPD '
      SLPNEW = SLOPE
      RETURN
      END                                                               CARFOL
C
C
C
      SUBROUTINE ACCEL ( SLNSET,ACOSET,VEOSET )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      LOGICAL           ACOSET,SLNSET,VEOSET
      DOUBLE PRECISION  ACCMAX,ACCN,ACCVEH,CRISLP,PIJRIV,PIJRT2,PIJRTS,
     *                  RELPN,SLOPE,T,VT
C3701 FORMAT(3HAC=,F7.3)
C
C-----SUBROUTINE ACCEL ACCELERATES ACCORDING TO THE DESIRED SPEED FOR
C-----THIS VEHICLE
C
C[    ACCMAX     = -2147483647.0
C[    ACCN       = -2147483647.0
C[    ACCVEH     = -2147483647.0
C[    CRISLP     = -2147483647.0
C[    RELPN      = -2147483647.0
C[    SLOPE      = -2147483647.0
C[    T          = -2147483647.0
C[    VT         = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'ACCEL '
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----INITIALIZE SOME PARAMETERS FOR ACCEL
C3    IPFLAG = 'STEADY SPD'
      CRISLP = SLPMAX*DCHAR(IDRICL(IV))
      PIJRIV =        PIJR (IDRICL(IV))
      PIJRT2 = 2.0D0*PIJRIV
      PIJRTS = PIJRT2**2
      ACOSET = .FALSE.
      SLNSET = .FALSE.
      VEOSET = .FALSE.
                    IF ( DESVEL . LT . 0.5D0 )   DESVEL = 0.0D0
C-----IF THE VEHICLES OLD VELOCITY IS LT HIS DESIRED SPEED THEN GO TO
C-----1010 AND CHECK FOR ACCELERATION TO THE VEHICLES DESIRED SPEED
            IF ( VELOLD . LE . DESVEL-0.5D0 )    GO TO 1010
C-----IF THE VEHICLES OLD VELOCITY IS GT HIS DESIRED SPEED THEN GO TO
C-----2010 AND CHECK FOR DECELERATION TO THE VEHICLES DESIRED SPEED
            IF ( VELOLD . GT . DESVEL+1.0D0 )    GO TO 2010
C-----THE VEHICLES VELOCITY IS VERY NEAR THE VEHICLES DESIRED SPEED THUS
C-----IF THE VEHICLES ACC/DEC IS GT A VALUE THAT COULD BE REDUCED TO
C-----ZERO IN ONE HALF SECOND THEN GO TO 4010 AND REDUCE THE VEHICLES
C-----ACC/DEC TO ZERO IN ONE SECOND
            IF ( DABS(ACCOLD) .GT. CRISLP*0.5D0 )GO TO 4010
C3                  IF ( ACCOLD . NE . 0.0D0 )   KPFLAG = 'VLOLD=DSVL'
 1005 CONTINUE
      IF ( DABS( DESVEL-VELOLD ) . LE . 0.1D0 )  THEN
C-----  SET THIS VEHICLE AT HIS DESIRED SPEED WITH ACC/DEC AND ACC/DEC
C-----  SLOPE OF ZERO
        SLPNEW = 0.0D0
        ACCOLD = 0.0D0
        VELOLD = DESVEL
        SLNSET = .TRUE.
        ACOSET = .TRUE.
        VEOSET = .TRUE.
      ELSE
C-----  FIND THE ACC/DEC SLOPE REQUIRED TO BRING THE VEHICLES VELOCITY
C-----  TO HIS DESIRED SPEED IN ONE DT
        SLPNEW = (DESVEL-VELOLD-ACCOLD*DT)/(0.5D0*DTSQ)
        SLPNEW = DMIN1( DMAX1( SLPNEW,-CRISLP ),CRISLP )
        SLNSET = .TRUE.
      END IF
      RETURN
 1010 CONTINUE
C-----ACCELERATE THE VEHICLE TO HIS DESIRED SPEED
C-----CALCULATE THE MAXIMUM ACCELERATION THE DRIVER WOULD USE TO GET TO
C-----HIS DESIRED SPEED IN THE LINEAR ACCELERATION MODEL
      ACCMAX = AUTOL*(3.2D0+0.08D0*DESVEL)*DCHAR(IDRICL(IV))
C-----CALCULATE THE MAXIMUM ACCELERATION OF THE VEHICLE AT THE CURRENT
C-----VELOCITY USING THE NON-UNIFORM THEORY OF ACCELERATION
      ACCVEH = AMAX(IVEHCL(IV))*(1.0D0-(VELOLD/VMAX(IVEHCL(IV))))
C-----CALCULATE THE PORTION OF THE MAXIMUM ACCELERATION THAT THE DRIVER
C-----WOULD USE TO GET TO HIS DESIRED SPEED FROM HIS CURRENT VELOCITY
      ACCN = DMIN1( ACCMAX,ACCVEH )*(1.0D0-(VELOLD/(1.15D0*DESVEL)))
C-----PROCESS VEHICLE MESSAGE SYSTEM SPEED MESSAGE
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        IF ( IVMSMG(VMSASM(IV)) .EQ. VMSMAM )    THEN
C-----    VEHICLE MESSAGE SYSTEM MESSAGE IS ACTIVE
C-----    ACCELERATE OR DECELERATE TO SPEED XX USING MAXIMUM VEHICLE
C-----    ACCELERATION OR DECELERATION
          ACCN = ACCVEH
        END IF
      END IF
C-----IF THIS VEHICLE MAY PROCEED INTO THE INTERSECTION AND IS THE FIRST
C-----VEHICLE IN HIS LANE THEN GO TO 1020 AND ACCELERATE TO ACCN
            IF ( MPRO(IV) . AND . MFINL(IV) )    GO TO 1020
C-----FIND THE NEW RELATIVE POSITION OF THE VEHICLE AFTER DT SECONDS IF
C-----THE ACCELERATION WAS INITIATED TO ACCN
      RELPN = RELPOS + DMAX1( PVVEL *DT + 0.5D0*PVACC*DTSQ,0.0D0 )
     *               - DMAX1( VELOLD*DT + 0.5D0*ACCN *DTSQ,0.0D0 )
C-----IF THE NEW RELATIVE POSITION IS GT 80 PERCENT OF THE OLD RELATIVE
C-----POSITION THEN GO TO 1020 AND INITIATE THE ACCELERATION TO ACCN
                    IF ( RELPN.GT.0.80D0*RELPOS )GO TO 1020
C-----CALCULATE THE ACC/DEC THAT WOULD MOVE THE VEHICLE NOT MORE THAN 20
C-----PERCENT OF HIS OLD RELATIVE POSITION IN ONE SECOND
      ACCN = DMAX1( 2.0D0*(0.2D0*RELPOS-VELOLD),0.0D0 )
 1020 CONTINUE
C-----IF THE VEHICLES ACC/DEC IS LT THE DESIRED ACC/DEC THEN GO TO 3010
C-----AND MOVE THE VEHICLES ACC/DEC TO ACCN IN 2*PIJR TIME
C[    IF ( ACCN               .EQ.-2147483647.0 )STOP 'ACCEL  ACCN   01'
                    IF ( ACCOLD . LT . ACCN )    GO TO 3010
C-----CALCULATE THE ACC/DEC SLOPE REQUIRED TO BRING THE VEHICLES ACC/DEC
C-----TO ACCN IN ONE SECOND
      SLPNEW = ACCN - ACCOLD
C-----BOUND THE VEHICLES ACC/DEC SLOPE AND CHECK THE NEW VELOCITY
      SLPNEW = DMIN1( DMAX1( SLPNEW,-CRISLP ),1.3D0*CRISLP )
      SLNSET = .TRUE.
C3    IPFLAG = 'ACCELERATE'
      GO TO 3020
 2010 CONTINUE
C-----FIND THE ACC/DEC SLOPE REQUIRED TO REDUCE THE VEHICLES VELOCITY TO
C-----HIS DESIRED SPEED BEFORE HE REACHES THE END OF HIS LANE AND BOUND
C-----THE ACC/DEC SLOPE
 2015 CONTINUE
 2020 CONTINUE
 2025 CONTINUE
 2027 CONTINUE
      IF ( DABS( DESVEL-VELOLD ) . LE . 0.1D0 )  GO TO 1005
      IF ( ACCOLD . EQ . 0.0D0 )                 THEN
C-----  SET SLOPE TO REACH DESIRED SPEED IN 2*PIJR SECONDS
        SLOPE = 2.0D0*(DESVEL-VELOLD)/PIJRTS
      ELSE
C-----  SET SLOPE TO REACH DESIRED SPEED AND FINAL ACCELERATION IS ZERO
        SLOPE = -0.5D0*ACCOLD**2/(DESVEL-VELOLD)
        SLOPE = DMIN1( DMAX1( SLOPE,-CRISLP ),CRISLP )
        IF ( SLOPE . NE . 0.0D0 )                THEN
          T = -ACCOLD/SLOPE
          IF ( ( T . LT . 0.0D0        ) . OR .
     *         ( T . GT . 4.0D0*PIJRIV ) )       THEN
C-----      SET SLOPE TO REACH DESIRED SPEED IN 2*PIJR SECONDS
            SLOPE = 2.0D0*(DESVEL-VELOLD-ACCOLD*PIJRT2)/PIJRTS
          END IF
        END IF
      END IF
C-----SET THE ACC/DEC SLOPE TO BRING THE ACC/DEC TO ZERO BY THE TIME THE
C-----VEHICLES VELOCITY REACHES HIS DESIRED SPEED
      SLPNEW = SLOPE
      SLNSET = .TRUE.
 2030 CONTINUE
C-----BOUND THE ACC/DEC SLOPE TO DECELERATE TO HIS DESIRED SPEED
      SLPNEW = DMIN1( DMAX1( SLPNEW,-CRISLP ),CRISLP )
      SLNSET = .TRUE.
      RETURN
 3010 CONTINUE
C-----THE VEHICLES OLD ACC/DEC IS LT THE NEW ACC/DEC THUS IF THE
C-----VEHICLES RELATIVE POSITION IS LE ZERO THEN GO TO 4010 AND REDUCE
C-----THE VEHICLES ACC/DEC TO ZERO IN ONE SECOND
                    IF ( RELPOS . LE . 0.0D0 )   GO TO 4010
C-----CALCULATE THE ACC/DEC SLOPE REQUIRED TO BRING THE VEHICLES ACC/DEC
C-----TO THE NEW ACC IN 2*PIJR TIME
C3    IPFLAG = 'MOVE ACC  '
C[    IF ( ACCN               .EQ.-2147483647.0 )STOP 'ACCEL  ACCN   02'
      SLPNEW = 1.01D0*(ACCN-ACCOLD)/PIJRT2
C-----BOUND THE ACC/DEC SLOPE FOR ACCELERATION TO ACCN IN PIJR TIME
      SLPNEW = DMIN1( DMAX1( SLPNEW,SLPOLD ),1.3D0*CRISLP )
      SLNSET = .TRUE.
      ACCNEW = ACCOLD + SLPNEW*DT
C-----IF THE VEHICLES ACC/DEC AFTER DT SECONDS WILL STILL BE LT ACCN
C-----THEN GO TO 3020 AND CHECK THE VELOCITY AFTER T SECONDS ELSE
C-----CALCULATE THE ACC/DEC SLOPE REQUIRED TO BRING THE VEHICLES ACC/DEC
C-----TO ACCN IN ONE SECOND AND CHECK VELOCITY AFTER T SECONDS
                    IF ( ACCNEW . LT . ACCN )    GO TO 3020
      SLPNEW = ACCN - ACCOLD
      SLNSET = .TRUE.
 3020 CONTINUE
C-----CHECK TO SEE THAT THE VEHICLES VELOCITY WOULD NOT BE ABOVE THE
C-----DESIRED SPEED AFTER THE ACC/DEC FOR THE VEHICLE WAS REDUCED TO
C-----ZERO AT HALF THE CRITICAL SLOPE
C[    IF ( ACCN               .EQ.-2147483647.0 )STOP 'ACCEL  ACCN   03'
C3    WRITE (JPFLAG,701) ACCN
      SLOPE = -0.50D0*CRISLP
      T = DMAX1( -ACCOLD/SLOPE,0.001D0 )
                    IF ( T . LT . DT )           GO TO 2025
      VT = VELOLD + ACCOLD*T + 0.5D0*SLOPE*T**2
                    IF ( VT . LT . DESVEL )      RETURN
C-----CALCULATE THE ACC/DEC SLOPE REQUIRED SO THAT VT WOULD NOT EXCEED
C-----THE DESIRED SPEED BEFORE THE ACC/DEC COULD BE REDUCED TO ZERO AND
C-----BOUND THE ACC/DEC SLOPE
      SLPNEW = DMIN1( DMAX1( (VT/DESVEL)*(-ACCOLD/T),-CRISLP ),
     *                1.3D0*CRISLP                              )
      SLNSET = .TRUE.
      RETURN
 4010 CONTINUE
C-----CALCULATE THE ACC/DEC SLOPE REQUIRED TO REDUCE THE VEHICLES
C-----ACC/DEC TO ZERO IN ONE SECOND AND BOUND THE ACC/DEC SLOPE
C3    IPFLAG = 'REDUCE A/D'
C3    JPFLAG = 'TO 0 ACCEL'
      SLPNEW = DMIN1( DMAX1( -ACCOLD,-CRISLP ),CRISLP )
      SLNSET = .TRUE.
      RETURN
      END                                                               ACCEL
C
C
C
      SUBROUTINE CRIDIS ( K )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'LANECH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      DOUBLE PRECISION  DECMIN
      PARAMETER       ( DECMIN = -1.0D0/IACCFR )
      INTEGER           K
      DOUBLE PRECISION  A,CRISLP,DECMAX,DECTST,DISPRT,DS,OLDACC,OLDVEL,
     *                  PIJRIV,REACTT,RELNEW,RELOLD,SLPN,T,TS,TSMAX,V,X,
     *                  XCRIT
C3701 FORMAT(3HDM=,F7.3)
C3702 FORMAT(3HSN=,F7.3)
C
C-----SUBROUTINE CRIDIS CHECKS CRITICAL STOPPING DISTANCE FOR A
C-----DECELERATION TO A STOP AND IF VIOLATED THEN INITIATES A
C-----DECELERATION TO A STOP
C
C-----K = 1 = CRITICAL STOPPING DISTANCE VIOLATED THIS DT
C-----K = 2 = CRITICAL STOPPING DISTANCE NOT VIOLATED THIS DT OR WITHIN
C-----        PIJR TIME
C-----K = 3 = CRITICAL STOPPING DISTANCE VIOLATED WITHIN PIJR TIME
C
C[    CRISLP     = -2147483647.0
C[    DECMAX     = -2147483647.0
C[    DECTST     = -2147483647.0
C[    DISPRT     = -2147483647.0
C[    OLDACC     = -2147483647.0
C[    OLDVEL     = -2147483647.0
C[    REACTT     = -2147483647.0
C[    RELNEW     = -2147483647.0
C[    RELOLD     = -2147483647.0
C[    SLPN       = -2147483647.0
C[    T          = -2147483647.0
C[    V          = -2147483647.0
C[    X          = -2147483647.0
C[    XCRIT      = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CRIDIS'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----INITIALIZE SOME PARAMETERS FOR CRIDIS
C3    IPFLAG = 'STEADY DIS'
      K = 2
      RELOLD = PVPOS - POSOLD
      CRISLP = SLPMAX*DCHAR(IDRICL(IV))
      PIJRIV =        PIJR (IDRICL(IV))
C-----INITIALIZE OLDACC AND REACTT FOR A NORMAL DECELERATION TO A STOP
C-----(REDUCE ACCOLD TO ZERO IN PIJR TIME)
      OLDACC = 0.0D0
      REACTT = PIJRIV
      OLDVEL = VELOLD
      DISPRT = 0.0D0
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        IF ( IVMSMG(VMSASM(IV)) .EQ. VMSMSM )    THEN
C-----    VMSMSM - STOP IMMEDIATELY USING MAXIMUM VEHICLE DECELERATION
          SLPNEW = (DMAX(IVEHCL(IV))-ACCOLD)/DT
          SLPNEW = DMIN1( DMAX1( SLPNEW,-2.0D0*CRISLP ),2.0D0*CRISLP )
          K = 1
          GO TO 3020
        END IF
        IF ( IVMSMG(VMSASM(IV)) .EQ. VMSMSC )    THEN
C-----    VMSMSC - STOP IMMEDIATELY USING COLLISION DECELERATION
          SLPNEW = (DVMSMP(VMSASM(IV))+1.0D-6-ACCOLD)/DT
          SLPNEW = DMIN1( DMAX1( SLPNEW,-SLPCOL ),SLPCOL )
          K = 1
          GO TO 3020
        END IF
      END IF
C-----IF THIS IS THE FIRST VEHICLE IN THE LANE THAT DECIDED TO STOP ON
C-----A YELLOW SIGNAL INDICATION THEN RESET THE REACTION TIME TO ZERO
                    IF ( IGO . EQ . 2 )          REACTT = 0.0D0
                    IF ( ACCOLD . GE . 0.0D0 )   GO TO 1030
C1010 CONTINUE
C-----SET OLDACC AND REACTT FOR A QUICK DECELERATION TO A STOP AND CHECK
C-----FOR A DECELERATION FOR A STOP (DECELERATION STARTS WITH THE
C-----CURRENT VALUE OF ACCOLD AND NO REACTION TIME)
      OLDACC = ACCOLD
      REACTT = 0.0D0
      GO TO 1040
 1020 CONTINUE
C-----SET REACTT TO PIJR TIME FOR THE DRIVER
      REACTT = PIJRIV
 1030 CONTINUE
C-----NEW DECMAX AND XCRIT TO ACCOUNT FOR CHANGE IN HOLDSP
                    IF ( REACTT . LE . 0.0D0 )   GO TO 1040
      SLPN = DMIN1( DMAX1( -ACCOLD/REACTT,-CRISLP ),CRISLP )
      OLDVEL = VELOLD        +       ACCOLD*REACTT
     *                                      + 0.5D0*SLPN*REACTT**2
      DISPRT = VELOLD*REACTT + 0.5D0*ACCOLD*REACTT**2 
     *                                      + ONED6*SLPN*REACTT**3
 1040 CONTINUE
C-----CALCULATE THE TIME AND DISTANCE TO STOP NOT EXCEEDING THE MAXIMUM
C-----DECELERATION RATE FOR THE DRIVER
      CALL  TDSTPM  ( IV,-6.0D0,OLDVEL,OLDVEL,OLDACC,.TRUE.,TS,DS )
C-----COMPUTE THE CRITICAL STOPPING DISTANCE FOR THE VEHICLE
C[    IF ( DISPRT             .EQ.-2147483647.0 )STOP 'CRIDIS DISPRT 01'
      XCRIT = DISPRT + OLDVEL*DTMAX + DS
C-----SET K FOR CRITICAL STOPPING DISTANCE VIOLATED THIS DT
      K = 1
C-----IF THE CRITICAL STOPPING DISTANCE IS VIOLATED THIS DT THEN GO TO
C-----3005 AND CHECK FOR A DECELERATION FOR A STOP
                    IF ( RELOLD . LE . XCRIT )   GO TO 3005
C-----IF THIS VEHICLE IS THE FIRST VEHICLE IN THE LANE WHICH DECIDED TO
C-----STOP ON A YELLOW SIGNAL INDICATION AND THE REACTION TIME IS EQ
C-----ZERO AND CRITICAL STOPPING DISTANCE IS NOT VIOLATED THIS DT THEN
C-----GO TO 1020 AND SET REACTT TO PIJR FOR THE DRIVER AND CHECK AGAIN
      IF ( (IGO.EQ.2) . AND . (REACTT.EQ.0.0D0) )GO TO 1020
C-----SET K FOR CRITICAL STOPPING DISTANCE NOT VIOLATED THIS DT OR
C-----WITHIN PIJR TIME
      K = 2
C-----CALCULATE THE NEW RELATIVE POSITION AFTER PIJR SECONDS OR THE TIME
C-----REQUIRED TO REDUCE THE VEHICLES ACC/DEC TO ZERO AT 0.5*CRISLP OR 1
C-----SECOND
      T = DMAX1( PIJRIV,ACCOLD/(0.5D0*CRISLP),1.0D0,2.0D0*DT )
      RELNEW = RELOLD - OLDVEL*T - 0.5D0*ACCOLD*T**2 - ONED6*SLPOLD*T**3
C-----IF THE CRITICAL STOPPING DISTANCE WILL NOT BE VIOLATED WITHIN PIJR
C-----TIME THEN RETURN AND ACCELERATE ACCORDING TO DESIRED SPEED
                    IF ( RELNEW . GT . XCRIT )   RETURN
C-----SET K FOR CRITICAL STOPPING DISTANCE VIOLATED WITHIN PIJR TIME
      K = 3
C-----IF THE VEHICLE WAS DECELERATING THEN CHECK FOR DECELERATION TO
C-----DESIRED SPEED
      IF ( ACCOLD . EQ . 0.0D0 )                 THEN
        SLPNEW = 0.0D0
        GO TO 3020
      END IF
C-----REDUCE THE VEHICLES ACCELERATION TO ZERO FOR UPCOMING DECELERATION
C-----TO A STOP
C3    IPFLAG = 'REDUCE ACC'
C3    JPFLAG = 'FOR DECEL '
      T = 0.0D0
 2010 CONTINUE
C[    IF ( T                  .EQ.-2147483647.0 )STOP 'CRIDIS T      01'
      T = T + DT
                    IF ( T . GT . 10.0D0 )       GO TO 3020
C-----CALCULATE THE ACC/DEC SLOPE REQUIRED TO REDUCE THE ACCELERATION TO
C-----0.0 IN T SECONDS AND FIND THE VELOCITY AND POSITION OF THE VEHICLE
C-----AFTER T SECONDS
      SLPNEW = DMIN1( DMAX1( -ACCOLD/T,-CRISLP ),CRISLP )
      A =                  ACCOLD      +       SLPNEW*T
      V = VELOLD   +       ACCOLD*T    + 0.5D0*SLPNEW*T**2
      X = VELOLD*T + 0.5D0*ACCOLD*T**2 + ONED6*SLPNEW*T**3
                    IF ( V . LE . VELSTP )       GO TO 3020
C-----CALCULATE THE TIME AND DISTANCE TO STOP NOT EXCEEDING THE MAXIMUM
C-----DECELERATION RATE FOR THE DRIVER
      CALL  TDSTPM  ( IV,-6.0D0,V,V,A,.TRUE.,TS,DS )
C-----CALCULATE THE CRITICAL STOPPING DISTANCE AFTER T SECONDS
      XCRIT = DISPRT + V*DTMAX + DS
C-----IF THE CRITICAL STOPPING DISTANCE WILL NOT BE VIOLATED WITHIN T
C-----SECONDS THUS GO TO 2010 AND INCREASE T BY DT AND CHECK AGAIN ELSE
C-----USE THE SLOPE TO CALCULATE THE NEW POS/VEL/ACC
                    IF ( RELOLD-X . GT . XCRIT ) GO TO 2010
      GO TO 3020
 3005 CONTINUE
                    IF ( JPRTM . NE . 0 )        REACTT = JPRTM*DT
 3010 CONTINUE
      RELOLD = DMAX1( RELOLD,0.01D0 )
      RELNEW = RELOLD
      IF ( REACTT . GT . 0.0D0 )                 THEN
        SLPN = DMIN1( DMAX1( -ACCOLD/REACTT,-CRISLP ),CRISLP )
        RELNEW = RELOLD - OLDVEL*REACTT - 0.5D0*ACCOLD*REACTT**2
     *                                  - ONED6*SLPN  *REACTT**3
C-----  IF THE NEW RELATIVE POSITION WILL BE LT 20 PERCENT OF THE OLD
C-----  RELATIVE POSITION AND THE REACTION TIME IS GT ZERO THEN GO TO
C-----  7010 AND REDUCE THE REACTION TIME BY DT AND CHECK AGAIN
        IF ( (RELNEW.LT.0.2D0*RELOLD) . AND . (REACTT.GT.0.0D0) )
     *                                           GO TO 7010
      END IF
C-----CALCULATE A DECELERATION TO A STOP (IF A DECELERATION TO A STOP
C-----CAN NOT BE CALCULATED THEN GO TO 4010 AND REDUCE ANY DECELERATION
C-----TO ZERO)
      IF ( RELNEW . LE . 0.0D0 )                 THEN
        TS     = 0.0D0
        SLPNEW = -2.0D0*CRISLP
      ELSE
C-----  AASHTO RECOMMENDS 11.2FT/SEC/SEC DECELERATION RATE
        TSMAX = DMAX1( 1.5D0*(OLDVEL/11.2D0 + PIJRIV),10.0D0 )
        CALL  SLPSTD  ( POSOLD+RELNEW,POSOLD,OLDVEL,OLDACC,SLPOLD,
     *                  TSMAX,TS,SLPNEW                            )
        IF ( TS . EQ . TIMERR )                  THEN
          IF ( REACTT . GT . 0.0D0 )             GO TO 7010
          IF ( OLDVEL . LE . VSMALL )            THEN
            TS     = 0.0D0
            SLPNEW = -CRISLP
          ELSE
            GO TO 4010
          END IF
        END IF
      END IF
      DECMAX = OLDACC + SLPNEW*TS
      DECTST = DUTOL*(-6.0D0-(OLDVEL/44.0D0))*DCHAR(IDRICL(IV))
      DECTST = DMAX1( DECTST,DMAX(IVEHCL(IV)) )
C-----IF THE ACC/DEC VALUE FOR A DECELERATION TO A STOP IS GT 25 PERCENT
C-----OF THE MAXIMUM ACC/DEC VALUE FOR A DECELERATION TO A STOP THEN GO
C-----TO 4010 AND REDUCE ANY DECELERATION TO ZERO
            IF ( DECMAX . GT . 0.25D0*DECTST )   GO TO 4010
C-----IF THE ACC/DEC SLOPE FOR A DECELERATION TO A STOP IS LT -2.0 TIMES
C-----CRITICAL SLOPE AND THE REACTION TIME IS GT ZERO THEN GO TO 7010
C-----AND DECREASE THE REACTION TIME BY DT AND CHECK AGAIN
      IF ( (SLPNEW.LT.-2.0D0*CRISLP) . AND . (REACTT.GT.0.0D0) )
     *                                           GO TO 7010
C-----BOUND THE ACC/DEC SLOPE FOR A DECELERATION TO A STOP
      SLPNEW = DMIN1( DMAX1( SLPNEW,-2.0D0*CRISLP ),2.0D0*CRISLP )
C-----IF THE LANE CHANGE ACC/DEC SLOPE IS LT THE ACC/DEC SLOPE FOR A
C-----DECELERATION TO A STOP THEN GO TO 3030 AND DO NOT INITIATE A
C-----DECELERATION TO A STOP ELSE INITIATE A DECELERATION TO A STOP
                    IF ( SLPLCH . LT . SLPNEW )  GO TO 3030
      IPRTM(IV) = IDNINT( REACTT/DT )
      JPRTM = IPRTM(IV)
C-----MOVE FLAG SO THAT VEHICLE WILL RECALCULATE AFTER PIJR TIME
C3    IPFLAG = 'DECEL PIJR'
C[    IF ( DECMAX             .EQ.-2147483647.0 )STOP 'CRIDIS DECMAX 01'
C3    WRITE (KPFLAG,701) DMAX1( DMIN1( DECMAX,+99.999 ),-99.999 )
C-----IF THERE IS REACTION TIME THEN GO TO 5010 AND HOLD THE SPEED
                    IF ( IPRTM(IV) . GT . 0 )    GO TO 5010
      MSFLG(IV) = .TRUE.
C3    IPFLAG = 'DECEL OARS'
 3020 CONTINUE
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER DT SECONDS
      CALL  NEWVEL  ( DT,DTSQ,DTCU )
      RETURN
 3030 CONTINUE
C-----THE LANE CHANGE ACC/DEC SLOPE IS LT THE ACC/DEC SLOPE FOR A
C-----DECELERATION TO A STOP THUS DO NOT INITIATE THE DECELERATION FOR
C-----A STOP
C3    IPFLAG = 'DECEL LCHG'
C3    WRITE (KPFLAG,702) SLPNEW
      GO TO 3020
 4010 CONTINUE
C-----REDUCE THE VEHICLES ACC/DEC TO ZERO
C3    IPFLAG = 'REDUCE DEC'
C3    JPFLAG = 'POS SLOPE '
C-----CALCULATE THE ACC/DEC SLOPE REQUIRED TO REDUCE THE ACC/DEC TO
C-----DECMIN IN ONE SECOND
      SLPNEW =  DECMIN-ACCOLD
      SLPNEW = DMIN1( DMAX1( SLPNEW,-CRISLP ),0.0D0 )
      GO TO 3020
 5010 CONTINUE
C-----HOLD THE VEHICLES SPEED AT ITS CURRENT VALUE
      CALL  HOLDSP  ( IPRTM(IV) )
      RETURN
 6010 CONTINUE
C-----SET K FOR CRITICAL STOPPING DISTANCE NOT VIOLATED THIS DT OR
C-----WITHIN PIJR TIME SO ACCELERATE ACCORDING TO DESIRED SPEED
      K = 2
      RETURN
 7010 CONTINUE
C-----REDUCE THE REACTION TIME BY DT AND RE-CALCULATE A DECELERATION TO
C-----A STOP
      REACTT = REACTT - DT
      GO TO 3010
      END                                                               CRIDIS
C
C
C
      SUBROUTINE HOLDSP ( KPRTM )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INTEGER           KPRTM,LPRTM
C
C-----SUBROUTINE HOLDSP HOLDS THE VEHICLES SPEED AT ITS CURRENT VALUE
C
C[    LPRTM      = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'HOLDSP'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----SAVE THE CURRENT VALUE OF SOME OF THE VEHICLES PARAMETERS
      LPRTM = KPRTM
C-----SET THE VEHICLES ACC/DEC AND ACC/DEC SLOPE TO ZERO TO HOLD THE
C-----SPEED
      SLPNEW = DMAX1( DMIN1( -ACCOLD/(LPRTM*DT),8.0D0 ),-ISLPSH )
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER DT SECONDS
      CALL  NEWVEL  ( DT,DTSQ,DTCU )
      RETURN
      END                                                               HOLDSP
C
C
C
      SUBROUTINE INTLOG
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      DOUBLE PRECISION  DS,TS,XCRIT
C
C-----SUBROUTINE INTLOG CHECKS THE INTERSECTION CONTROL LOGICAL
C-----DEPENDENT ATTRIBUTES AND CALL THE APPROPRIATE INTERSECTION CONTROL
C-----ROUTINES
C
C[    DECMAX     = -2147483647.0
C[    XCRIT      = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'INTLOG'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----CHECK THE INTERSECTION CONTROL LOGICAL DEPENDENT ATTRIBUTES
                    IF ( (.NOT. ICONTN(IV)) )    GO TO 1010
C-----THE VEHICLE SHOULD CONTINUE AS PRESENTLY
      RETURN
 1010 CONTINUE
                    IF ( (.NOT. ILUNC(IV)) )     GO TO 2010
C-----FOLLOW THE UNCONTROLLED LANE LOGIC (UNCONTROLLED LANE AT
C-----UNCONTROLLED INTERSECTION) THUS IF THE VEHICLE IS STOPPED
C-----AT THE STOP LINE THEN FOLLOW THE STOP SIGN CONTROLLED LOGIC ELSE
C-----CHECK SIGHT DISTANCE RESTRICTIONS AND IF CLEAR THEN CHECK
C-----INTERSECTION CONFLICTS AND IF CLEAR THEN THIS VEHICLE MAY PROCEED
C-----INTO THE INTERSECTION
                    IF ( MATSTL(IV) )            GO TO 3020
      GO TO 4020
 2010 CONTINUE
                    IF ( (.NOT. ILYELD(IV)) )    GO TO 3010
C-----FOLLOW THE YIELD SIGN CONTROLLED LOGIC THUS IF THIS VEHICLE IS THE
C-----FIRST VEHICLE IN THE LANE OR THE VEHICLE AHEAD MAY PROCEED INTO
C-----THE INTERSECTION THEN FOLLOW THE STOP SIGN CONTROLLED LOGIC EVEN
C-----THOUGH THIS VEHICLE IS NOT STOPPED AT THE STOP LINE ELSE RETURN
C-----AND CHECK AGAIN NEXT DT
                    IF ( NOF(IV) . EQ . 0 )      GO TO 3020
                    IF ( MPRO(NOF(IV)) )         GO TO 3020
      RETURN
 3010 CONTINUE
                    IF ( (.NOT. ILSTOP(IV)) )    GO TO 4010
 3020 CONTINUE
C-----FOLLOW THE STOP SIGN CONTROLLED LOGIC THUS IF THE VEHICLE MAY
C-----PROCEED INTO THE INTERSECTION THEN RETURN
                    IF ( MPRO(IV) )              RETURN
C-----CHECK TO SEE IF THE VEHICLE MAY ENTER THE INTERSECTION WITHOUT
C-----BLOCKING ANY VEHICLE STOPPED AT THE INTERSECTION BEFORE THIS
C-----VEHICLE AND IF OK THEN CHECK SIGHT DISTANCE RESTRICTIONS AND IF
C-----CLEAR THEN CHECK INTERSECTIONS CONFLICTS AND IF CLEAR THEN THE
C-----VEHICLE MAY PROCEED INTO THE INTERSECTION
      CALL  LSTOP
      RETURN
 4010 CONTINUE
                    IF ( (.NOT. ICHKCF(IV)) )    GO TO 5010
 4020 CONTINUE
C-----THIS VEHICLE MUST CHECK FOR CONFLICTS THUS IF THE VEHICLE MAY
C-----PROCEED INTO THE INTERSECTION OR THE VEHICLE IS NOT THE FIRST
C-----VEHICLE IN THE LANE OR THE TRAFFIC CONTROL AHEAD REQUIRES THIS
C-----VEHICLE TO STOP THEN RETURN AND CONTINUE AS PRESENTLY
                    IF (        MPRO  (IV)  )    RETURN
      IF ( ( IAND( VEHTYP(IV),LAVTE ) .EQ. 0 ) . AND .
     *     ( (.NOT. MFINL (IV))              ) ) RETURN
                    IF (        MTCARS(IV)  )    RETURN
C-----CHECK SIGHT DISTANCE RESTRICTIONS AND IF CLEAR THEN CHECK
C-----INTERSECTIONS CONFLICTS AND IF CLEAR THEN THE VEHICLE MAY PROCEED
C-----INTO THE INTERSECTION
      CALL  CHKSDR
      RETURN
 5010 CONTINUE
                    IF ( (.NOT. INFLZ(IV)) )     GO TO 6010
C-----THIS VEHICLE SHOULD CHECK TO SEE IF IT SHOULD BE WITHIN THE
C-----INFLUENCE ZONE OF THE INTERSECTION CONTROL THUS IF THE VEHICLE HAS
C-----NOT DEDICATED HIMSELF TO AN INTERSECTION PATH THEN RETURN AND WAIT
C-----UNTIL THE VEHICLE IS DEDICATED TO AN INTERSECTION PATH
                    IF ( LNEXT(IV) . EQ . 0 )    RETURN
C-----IF THE VEHICLE IS AN EMERGENCY VEHICLE THEN XCRIT IS 30 SECONDS AT
C-----THE VEHICLES DESIRED SPEED ELSE CALCULATE XCRIT
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   THEN
        XCRIT = 30.0D0*DBLE( ISPD(IV) )
      ELSE
C-----  CALCULATE THE TIME AND DISTANCE TO STOP NOT EXCEEDING THE
C-----  MAXIMUM DECELERATION RATE FOR THE DRIVER
        CALL  TDSTPM  ( IV,-6.0D0,VELOLD,VELOLD,ACCOLD,.TRUE.,TS,DS )
C-----  CALCULATE THE THRESHOLD DISTANCE FROM THE END OF THE LANE THAT
C-----  THE VEHICLE SHOULD BECOME WITHIN THE INFLUENCE ZONE OF THE
C-----  INTERSECTION CONTROL (LET 4+PIJR SECONDS AT THE CURRENT VELOCITY
C-----  PLUS THE STOPPING DISTANCE BE THE THRESHOLD DISTANCE)
        XCRIT = VELNEW*(4.0D0+PIJR(IDRICL(IV))) + DS
C-----  LET 400 FEET BE THE MINIMUM THRESHOLD DISTANCE
        XCRIT = DMAX1( XCRIT,400.0D0 )
      END IF
C-----IF THE DISTANCE FROM THE END OF THE LANE IS GT THE THRESHOLD
C-----DISTANCE THEN RETURN AND WAIT UNTIL THE VEHICLE IS CLOSER
                    IF ( RELEND . GT . XCRIT )   RETURN
C-----INITIALIZE THE VEHICLES INTERSECTION CONTROL LOGICAL ATTRIBUTES
C-----BASED ON THE TYPE OF TRAFFIC CONTROL FOR THIS LANE
      CALL  INFLZN
C-----IF THIS VEHICLE MUST CHECK INTERSECTION CONFLICTS THEN GO TO 4020
C-----AND CHECK INTERSECTION CONFLICTS
                    IF ( MCHKCF(IV) )            GO TO 4020
      RETURN
 6010 CONTINUE
                    IF ( (.NOT. IDEDIC(IV)) )    GO TO 9100
C-----THIS VEHICLE SHOULD CHECK TO SEE IF IT SHOULD DEDICATE ITSELF TO
C-----AN INTERSECTION PATH
C-----IF THE VEHICLE IS AN EMERGENCY VEHICLE THEN XCRIT IS 35 SECONDS AT
C-----THE VEHICLES DESIRED SPEED ELSE CALCULATE XCRIT
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   THEN
        XCRIT = 35.0D0*DBLE( ISPD(IV) )
      ELSE
C-----  CALCULATE THE THRESHOLD DISTANCE FROM THE START OF THE LANE THAT
C-----  THE VEHICLE CAN DEDICATE ITSELF TO AN INTERSECTION PATH (LET THE
C-----  THRESHOLD DISTANCE BE THE ACCEPTABLE LAG GAP FOR LANE CHANGING)
        XCRIT = (4.0D0+1.4D0*VELOLD) /
     *          (FACTOR*DCHAR(IDRICL(IV))*VCHAR(IVEHCL(IV)))
        XCRIT = DBLE( LGEOM(1,IL) ) + XRELMI + XCRIT + LENVAP
      END IF
C-----IF THE DISTANCE FROM THE START OF THE LANE IS LT THE THRESHOLD
C-----DISTANCE THEN RETURN AND WAIT UNTIL THE VEHICLE IS FURTHER DOWN
C-----THE LANE
                    IF ( POSNEW . LT . XCRIT )   RETURN
      IF ( ISET(IV) . EQ . 6 )                   THEN
        ISET(IV) = 5
      END IF
      LOGTMP = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
C-----FIND AN INTERSECTION PATH FOR THIS VEHICLE BASED ON THE CURRENT
C-----APPROACH, CURRENT LANE, AND THE DESIRED OUTBOUND APPROACH
      CALL  PATHF   ( .FALSE.,0,'INTLOG' )
      RETURN
C-----PROCESS THE EXECUTION ERRORS AND STOP
 9100 CONTINUE
                    IF ( (.NOT. IERROR(IV)) )    GO TO 9110
      CALL  ABORTR  ( 'STOP 910 - NO LANE CONTROL SET - INTLOG' )
      STOP  910
 9110 CONTINUE
      CALL  ABORTR  ( 'STOP 911 - '                          //
     *                'NO VEHIL DEPENDENT ATTRIBUTE TRUE - ' //
     *                'INTLOG'                                  )
      STOP  911
      END                                                               INTLOG
C
C
C
      SUBROUTINE SIGRES ( JSISET )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'CONFLT'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'PHASES'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      LOGICAL           DZSCHK,FGARP3,IBLOCK,LVMSGO,LVMSRR,MAJCBL,MPROL
      INTEGER           IGARP,JSISET
      DOUBLE PRECISION  CRISLP,DISEND,DS,PIJRIV,PN,POSCBL,PS,TIMEND,TS,
     *                  TSMAX,XCRIT
C
C-----SUBROUTINE SIGRES DETERMINES THE APPROPRIATE DRIVER RESPONSE FOR
C-----THE NEW SIGNAL INDICATION
C
C[    IGARP      = -2147483647
C[    CRISLP     = -2147483647.0
C[    DECMAX     = -2147483647.0
C[    TS         = -2147483647.0
C[    XCRIT      = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'SIGRES'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      CRISLP     = SLPMAX*DCHAR(IDRICL(IV))
      PIJRIV     =        PIJR (IDRICL(IV))
      MPROL      = MPRO(IV)
      SIGRGO(IV) = .FALSE.
      FGARP3     = .FALSE.
      MAJCBL     = .FALSE.
      POSCBL     = 0.0D0
C-----SET TO NOT CHECK DILEMMA ZONE STATISTICS
      DZSCHK     = .FALSE.
C-----INITIALIZE THE INTERSECTION CONTROL LOGIC TIMER TO PROCESS NEXT DT
      LOGTMP     = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
      IF ( ITURN(IV) . EQ . 0 )                  THEN
        IGARP = 3
      ELSE
        CALL  SIGARP  ( JSISET,ITURN(IV),IGARP )
      END IF
C-----IF VEHICLE IS FORCED TO GO OR THE VEHICLE IS FORCED TO RUN THE RED
C-----SIGNAL THEN SET PARAMETERS FOR PROTECTED GREEN
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        LVMSGO = ( IVMSMG(VMSASM(IV)) . EQ . VMSMGO )
        LVMSRR = ( IVMSMG(VMSASM(IV)) . EQ . VMSMRR )
      ELSE
        LVMSGO = .FALSE.
        LVMSRR = .FALSE.
      END IF
      IF ( ( MFGOF(IV)                                ) .OR.
     *     ( ( FRRTIM(IV) . GT . 0.0D0              ) .AND.
     *       ( TIME .GE.  FRRTIM(IV)                ) .AND.
     *       ( TIME .LE. (FRRTIM(IV)+FRRATM(IV))    ) ) .OR.
     *     ( LVMSGO                                   ) .OR.
     *     ( LVMSRR                                   ) )
     *                                           THEN
        IGARP = 4
      END IF
C-----IF THE MAJOR COLLISION EFFECTS THIS VEHICLE THEN RESPOND AS IF
C-----THERE IS A RED SIGNAL INDICATION (IGARP=3) OR FORCE CHECKING OF
C-----INTERSECTION CONFLICTS (MAJCON(IV)=TRUE)
      IF ( MAJCLB(IV) . OR . MAJCLL(IV) )        THEN
        IGARP  = 3
        FGARP3 = .TRUE.
      END IF
C-----PROCESS THE SIGNAL INDICATION BY THE SIGNAL SETTING
C[    IF ( IGARP              .EQ.-2147483647   )STOP 'SIGRES IGARP  01'
C-----          G    A    R    P
      GO TO ( 1010,2010,3010,4010 ) , IGARP
 1010 CONTINUE
C-----GREEN LIGHT IS DISPLAYED
      IPRTM(IV) = 0
                    IF ( MPROL )                 GO TO 1020
                    IF ( (.NOT. MFINL(IV)) )     GO TO 1020
                    IF ( MSSGRN(IV) )            GO TO 1020
                    IF ( VELOLD . GT . 0.0D0 )   GO TO 1020
C-----THIS VEHICLE IS THE FIRST VEHICLE IN HIS LANE AND HIS LAST SIGNAL
C-----INDICATION WAS NOT GREEN AND HE IS STOPPED THUS SET THE DELAY FOR
C-----THE FIRST VEHICLE IN THE QUEUE TO DISCHARGE
      IPRTM(IV) = IDNINT( 0.5D0/DT ) + IPIJR(IDRICL(IV))
                    IF ( ITURN(IV) .GT. ITURNL ) GO TO 1020
C-----THIS VEHICLE IS TURNING LEFT THUS SET THE INTERSECTION CONTROL
C-----LOGIC TIMER ALSO
      LOGTMP = MIN0( 2+IPRTM(IV),MPRTM )
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
 1020 CONTINUE
C-----SET THE INTERSECTION CONTROL LOGIC FOR GO ON GREEN
C3    KPFLAG = 'SIG GREEN '
 1030 CONTINUE
                    IF ( (.NOT. MSSGRN(IV)) )    NEWSSG = .TRUE.
      MCHKCF(IV) = .FALSE.
      MSSGRN(IV) = .TRUE.
      MSSRED(IV) = .FALSE.
      MPRO  (IV) = .TRUE.
      MSFLG (IV) = .FALSE.
      MTCARS(IV) = .FALSE.
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        MCHKCF(IV) = .TRUE.
        MPRO  (IV) = .FALSE.
      END IF
C-----IF THIS VEHICLE IS NOT THE FIRST VEHICLE IN THE LANE OR IF THERE
C-----IS NO VEHICLE AHEAD THEN GO TO 1040 AND CONTINUE ELSE SET THIS
C-----VEHICLE AS NOT THE FIRST VEHICLE IN THE LANE, RESET THE PREVIOUS
C-----VEHICLE PARAMETERS TO THE VEHICLE AHEAD, AND SET OBJECT AHEAD
C-----STOPPING FLAG
                    IF ( (.NOT. MFINL(IV)) )     GO TO 1040
                    IF ( NOF(IV) . EQ . 0 )      GO TO 1040
      IVPV  = NOF(IV)
      CALL  SPVAS   ( IVPV,PVPOS,PVVEL,PVACC,PVSLP,
     *                .TRUE.,.TRUE.,.FALSE.,.TRUE.  )
      IF ( MFSTPF(IV) )                          THEN
        IF ( FSTACT(IV) )                        THEN
          IF ( FSTPOS(IV) . LT . PVPOS )         THEN
            PVPOS = FSTPOS(IV)
            MPRO(IV) = .FALSE.
          END IF
        END IF
        IF ( VMSASM(IV) . GT . 0 )               THEN
          IF ( ( IVMSMG(VMSASM(IV)).EQ.VMSMSI ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSL ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSM ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSC ) )THEN
            IF ( VMSPST(IV) . LT . PVPOS )       THEN
              PVPOS = VMSPST(IV)
              MPRO(IV) = .FALSE.
            END IF
          END IF
        END IF
      END IF
C$    WRITE (6,'(6H IVPV=,I3,7H PVPOS=,F9.0)') IVPV,PVPOS
      MFINL(IV) = .FALSE.
      MOASF(IV) = .FALSE.
                    IF ( PVVEL . LE . VELSTP )   MOASF(IV) = .TRUE.
 1040 CONTINUE
C-----IF MUST CHECK CONFLICTS BECAUSE OF INTERSECTION COLLISION OR AN
C-----EMERGENCY VEHICLE THEN GO TO 1045 AND CHECK INTERSECTION CONFLICTS
                    IF ( SMJCOL . OR . EVCCON )  GO TO 1045
C-----IF THIS VEHICLE IS NOT TURNING LEFT AND HIS INTERSECTION PATH DOES
C-----NOT CHANGE LANES WITHIN THE INTERSECTION THEN GO TO 1050 AND
C-----CONTINUE ELSE SET THAT THIS VEHICLE MUST CHECK FOR CONFLICTS AND
C-----MAY NOT PROCEED INTO THE INTERSECTION
                    IF ( LNEXT(IV) . EQ . 0 )    GO TO 1050
      IF ( (ITURN(IV)       . GT . ITURNL) . AND .
     *     (ILCH(LNEXT(IV)) . EQ . 0     ) )     GO TO 1050
                    IF ( MPROL )                 GO TO 1050
 1045 CONTINUE
      MCHKCF(IV) = .TRUE.
      MPRO  (IV) = .FALSE.
C-----IF THE SIGNAL IS GREEN AND THIS VEHICLE IS MAKING A U-TURN OR LEFT
C-----TURN THEN (1) IF THIS VEHICLE IS THE FIRST VEHICLE IN THE LANE AND
C-----THE DRIVERS PIJR TIME IS GREATER THAN THE AVERAGE PIJR TIME MINUS
C-----0.25 SECONDS OR THE VEHICLE CHARACTERISTIC IF LESS THAN OR EQUAL
C-----TO 105 OR THE VEHICLE LENGTH IS GREATER THAN 18 FEET OR (2) IF
C-----THIS VEHICLE IS NOT THE FIRST VEHICLE IN THE LANE THEN DO NOT CALL
C-----CHKSDR UNTIL NEXT DT (THIS PREVENTS (1) A LEFT-TURNING VEHICLE
C-----WITH A SLOW DRIVER OR SLOW VEHICLE OR LONG VEHICLE OR (2) A
C-----VEHICLE THAT IS NOT THE FIRST VEHICLE IN THE LANE FROM GOING IN
C-----FRONT OF OPPOSING TRAFFIC AT THE START OF GREEN BECAUSE THE
C-----OPPOSING TRAFFIC IS PROCESSED LATER IN THE DT)
      IF ( ( IGARP     . EQ . 1      ) . AND .
     *     ( ITURN(IV) . LE . ITURNL ) )         THEN
        IF ( MFINL(IV) )                         THEN
          IF ( ( PIJRIV            . GT . (APIJR-0.25D0) ) . OR .
     *         ( VCHAR(IVEHCL(IV)) . LE . 1.05D0         ) . OR .
     *         ( LENVAP            . GT . 18             ) )
     *                                           THEN
            GO TO 1050
          ELSE
            GO TO 1047
          END IF
        ELSE
          GO TO 1050
        END IF
      END IF
 1047 CONTINUE
C-----CHECK SIGHT DISTANCE RESTRICTIONS AND IF CLEAR THEN CHECK
C-----INTERSECTION CONFLICTS AND IF CLEAR THEN THE VEHICLE MAY PROCEED
C-----INTO THE INTERSECTION
      CALL  CHKSDR
 1050 CONTINUE
C-----GO TO 5010 AND FINISH PROCESSING
      GO TO 5010
 2010 CONTINUE
C-----YELLOW LIGHT IS DISPLAYED
C3    KPFLAG = 'YELLOW AGN'
C-----IF THE LAST SIGNAL INDICATION WAS NOT GREEN THEN THIS IS NOT THE
C-----FIRST TIME THE VEHICLE HAS GONE THROUGH THE YELLOW DECISION CODE
C-----THUS IMPLEMENT THE DECISION FROM LAST TIME BY GOING TO 5020 AND
C-----FINISH PROCESSING
                    IF ( (.NOT. MSSGRN(IV)) )    GO TO 5020
C-----SET TO CHECK DILEMMA ZONE STATISTICS
      DZSCHK = .TRUE.
C-----IF LANE IS NOT BLOCKED, VEHICLE HAS CHOSEN AN INTERSECTION PATH,
C-----VEHICLES POSITION IS 2*XRELMI FEET BEYOND END OF LANE, AND VEHICLE
C-----WOULD NOT BLOCK THE INTERSECTION THEN SET THE INTERSECTION CONTROL
C-----FLAGS SO THAT HE MAY PROCEED INTO THE INTERSECTION IF HIS
C-----CONFLICTS CLEAR (GO ON YELLOW INDICATION)
      IF ( (.NOT. MBLOCK(IV)                                 ) . AND .
     *     (LNEXT(IV) . GT . 0                               ) . AND .
     *     (POSNEW    . GT . DBLE( LGEOM(4,IL) )+2.0D0*XRELMI) )
     *                                           THEN
        CALL  CHKINT  ( LNEXT(IV),.TRUE.,.FALSE.,IBLOCK,MAJCBL,POSCBL )
        IF ( (.NOT. IBLOCK) )                    THEN
C3        KPFLAG = 'SIG AM LTP'
          IDISPD(IV) = .TRUE.
          LOGFLG(IV) = 2
C-----    IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE
C-----    SOMEWHERE IN THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK
C-----    INTERSECTION CONFLICTS
          IF ( SMJCOL . OR . EVCCON )            THEN
            LOGFLG(IV) = 1
          END IF
          GO TO 1030
        END IF
      END IF
                    IF ( (.NOT. MFINL (IV)) )    GO TO 2020
                    IF ( (.NOT. MCHKCF(IV)) )    GO TO 2020
                    IF ( RELEND . GT . XRELMX )  GO TO 2020
C-----THIS VEHICLE IS THE FIRST VEHICLE IN HIS LANE AND HE MUST CHECK
C-----FOR CONFLICTS AND HE IS AT THE STOP LINE THUS SET THE INTERSECTION
C-----CONTROL FLAGS SO THAT HE MAY PROCEED INTO THE INTERSECTION IF HIS
C-----CONFLICTS CLEAR
C3    KPFLAG = 'YELLOW GO '
      IDISPD(IV) = .TRUE.
      LOGFLG(IV) = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGFLG(IV) = 1
      END IF
      GO TO 1030
 2020 CONTINUE
C-----SET THE INTERSECTION CONTROL LOGIC FOR FOLLOW YELLOW STOP
C3    KPFLAG = 'FOL YL STP'
      MSSGRN(IV) = .FALSE.
      MSSRED(IV) = .TRUE.
      MPRO  (IV) = .FALSE.
      MSFLG (IV) = .FALSE.
      MTCARS(IV) = .TRUE.
                    IF ( IGO . LE . 1 )          GO TO 2030
C-----THE PREVIOUS VEHICLE DECIDED TO YELLOW STOP THUS FOLLOW YELLOW
C-----STOP
      IGO = 3
C-----GO TO 5010 AND FINISH PROCESSING
      GO TO 5010
 2030 CONTINUE
C-----IF THE VEHICLE IS DECELERATING TO A STOP OR HIS VELOCITY IS LE 0
C-----THEN GO TO 2040 AND YELLOW STOP
                    IF ( MSFLG(IV) )             GO TO 2040
                    IF ( VELOLD . LE . 0.0D0 )   GO TO 2040
C-----CALCULATE THE TIME AND DISTANCE TO STOP NOT EXCEEDING THE MAXIMUM
C-----DECELERATION RATE FOR THE DRIVER (HARD BRAKING)
      CALL  TDSTPM  ( IV,-8.0D0,VELOLD,VELOLD,ACCOLD,.TRUE.,TS,DS )
      XCRIT = DS
C-----SET THE INTERSECTION CONTROL LOGIC FOR YELLOW GO
C3    KPFLAG = 'YELLOW GO '
C-----IF THE CRITICAL STOPPING DISTANCE IS GT THE DISTANCE TO THE STOP
C-----LINE THEN GO TO 1030 AND YELLOW GO ELSE YELLOW STOP
                    IF ( XCRIT . GT . RELEND )   GO TO 1030
 2040 CONTINUE
C-----SET THE INTERSECTION CONTROL LOGIC FOR YELLOW STOP
C3    KPFLAG = 'YELLOW STP'
C-----IF THE VEHICLE MAY MAKE A LEFT-TURN-ON-RED OR A RIGHT-TURN-ON-RED
C-----THEN THE TRAFFIC CONTROL AHEAD DOES NOT REQUIRE A STOP
                    IF ( MLRTOR(IV) )            MTCARS(IV) = .FALSE.
      MSFLG (IV) = .FALSE.
C-----IF THERE IS NO VEHICLE AHEAD THEN GO TO 2060 AND SET THIS VEHICLE
C-----AS THE FIRST VEHICLE IN THIS LANE ELSE CHECK THE VEHICLE AHEAD
                    IF ( NOF(IV) . EQ . 0 )      GO TO 2060
C-----IF THE VEHICLE AHEAD MAY PROCEED INTO THE INTERSECTION THEN GO TO
C-----2060 AND SET THIS VEHICLE AS THE FIRST VEHICLE IN THIS LANE
                    IF ( MPRO(NOF(IV)) )         GO TO 2060
C-----SET THIS VEHICLE TO FOLLOW THE VEHICLE AHEAD (FOLLOW YELLOW STOP)
      IGO = 3
      MFINL(IV) = .FALSE.
      MOASF(IV) = .FALSE.
                    IF ( PVVEL . LE . VELSTP )   MOASF(IV) = .TRUE.
C-----GO TO 5010 AND FINISH PROCESSING
      GO TO 5010
 2060 CONTINUE
C-----SET THIS VEHICLE AS THE FIRST VEHICLE IN THIS LANE AND YELLOW STOP
      IGO = 2
      MFINL(IV) = .TRUE.
      MOASF(IV) = .TRUE.
C-----RESET THE PREVIOUS VEHICLE PARAMETERS
      IVPV  = 0
      PVPOS = ENDLN
      PVVEL = 0.0D0
      PVACC = 0.0D0
      PVSLP = 0.0D0
C-----GO TO 5010 AND FINISH PROCESSING
      GO TO 5010
 3010 CONTINUE
C-----RED LIGHT IS DISPLAYED
C3    KPFLAG = 'SIG RED GO'
      IF ( ITURN(IV) . EQ . 0 )                  GO TO 3020
C-----IF LANE IS NOT BLOCKED, VEHICLE HAS CHOSEN AN INTERSECTION PATH,
C-----AND VEHICLE WOULD NOT BLOCK THE INTERSECTION THEN CHECK FURTHER
      CALL  CHKINT  ( LNEXT(IV),.TRUE.,.FALSE.,IBLOCK,MAJCBL,POSCBL )
      IF ( (.NOT. MBLOCK(IV)                             ) . AND .
     *     (LNEXT(IV) . GT . 0                           ) . AND .
     *     (.NOT. IBLOCK                                 ) )
     *                                           THEN
C-----  IF THE VEHICLE MAY PROCEED INTO THE INTERSECTION THEN GO TO 5020
C-----  AND FINISH PROCESSING (GO ON RED INDICATION)
        IF ( ( MPRO(IV)            ) . AND .
     *       ( .NOT. FGARP3        ) . AND .
     *       ( VELOLD . GT . 0.0D0 ) )           THEN
C-----    CHECK IF THE VEHICLE IS DECELERATING TO A STOP
C-----    AASHTO RECOMMENDS 11.2FT/SEC/SEC DECELERATION RATE
          TSMAX = DMAX1( 1.5D0*(VELOLD/11.2D0+PIJRIV),10.0D0 )
          CALL  TIMSTP  ( VELOLD,ACCOLD,SLPNEW,TSMAX,TS )
          IF ( TS . EQ . TIMERR )                THEN
            SIGRGO(IV) = .TRUE.
            GO TO 5020
          ELSE
            PN = POSOLD + VELOLD*TS + 0.5D0*ACCOLD*TS**2
     *                                              + ONED6*SLPNEW*TS**3
            PS = DBLE( LGEOM(4,IL) ) + 2.0D0*XRELMX
            IF ( PN . GT . PS )                  THEN
              SIGRGO(IV) = .TRUE.
              GO TO 5020
            END IF
          END IF
        END IF
C-----  IF THE VEHICLES POSITION IS 4*XRELMI FEET BEYOND END OF LANE 
C-----  THEN SET FOR NO PIJR TIME AND PROCESS AS IF THERE WAS A
C-----  PROTECTED GREEN (GO ON RED INDICATION LEFT TURN PULLOUT)
        IF ( POSNEW . GT . DBLE( LGEOM(4,IL) )+4.0D0*XRELMI)
     *                                           THEN
C3        KPFLAG = 'SIG PG LTP'
          IPRTM(IV) = 0
          GO TO 4030
        END IF
      END IF
 3020 CONTINUE
C-----SET THE INTERSECTION CONTROL LOGIC FOR STOP ON RED
C3    KPFLAG = 'SIG RED   '
      IGO = 3
      MSSGRN(IV) = .FALSE.
      MSSRED(IV) = .TRUE.
      MPRO  (IV) = .FALSE.
      MSFLG (IV) = .FALSE.
      MTCARS(IV) = .TRUE.
C-----IF THE VEHICLE MAY MAKE A LEFT-TURN-ON-RED OR A RIGHT-TURN-ON-RED
C-----THEN THE TRAFFIC CONTROL AHEAD DOES NOT REQUIRE A STOP
      IF ( MLRTOR(IV) . AND . (.NOT.FGARP3) )    MTCARS(IV) = .FALSE.
C-----GO TO 5010 AND FINISH PROCESSING
      GO TO 5010
 4010 CONTINUE
C-----GREEN PROTECTED LIGHT IS DISPLAYED
      IPRTM(IV) = 0
                    IF ( MPROL )                 GO TO 4020
                    IF ( (.NOT. MFINL(IV)) )     GO TO 4020
                    IF ( MSSGRN(IV) )            GO TO 4020
                    IF ( VELOLD . GT . 0.0D0 )   GO TO 4020
C-----THIS VEHICLE IS THE FIRST VEHICLE IN HIS LANE AND HIS LAST SIGNAL
C-----INDICATION WAS NOT GREEN AND HE IS STOPPED THUS SET THE DELAY FOR
C-----THE FIRST VEHICLE IN THE QUEUE TO DISCHARGE
      IPRTM(IV) = IDNINT( 0.5D0/DT ) + IPIJR(IDRICL(IV))
                    IF ( ITURN(IV) .GT. ITURNL ) GO TO 4020
C-----THIS VEHICLE IS TURNING LEFT THUS SET THE INTERSECTION CONTROL
C-----LOGIC TIMER ALSO
      LOGTMP = MIN0( 2+IPRTM(IV),MPRTM )
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
 4020 CONTINUE
C-----SET THE INTERSECTION CONTROL LOGIC FOR GO ON PROTECTED GREEN
C3    KPFLAG = 'SIG P GRN '
 4030 CONTINUE
                    IF ( (.NOT. MSSGRN(IV)) )    NEWSSG = .TRUE.
      MCHKCF(IV) = .FALSE.
      MSSGRN(IV) = .TRUE.
      MSSRED(IV) = .FALSE.
      MPRO  (IV) = .TRUE.
      MSFLG (IV) = .FALSE.
      MTCARS(IV) = .FALSE.
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        MCHKCF(IV) = .TRUE.
        MPRO  (IV) = .FALSE.
      END IF
C-----IF THIS VEHICLE IS NOT THE FIRST VEHICLE IN THE LANE OR IF THERE
C-----IS NO VEHICLE AHEAD THEN GO TO 4040 AND CONTINUE ELSE SET THIS
C-----VEHICLE AS NOT THE FIRST VEHICLE IN THE LANE, RESET THE PREVIOUS
C-----VEHICLE PARAMETERS TO THE VEHICLE AHEAD, AND SET OBJECT AHEAD
C-----STOPPING FLAG
                    IF ( (.NOT. MFINL(IV)) )     GO TO 4040
                    IF ( NOF(IV) . EQ . 0 )      GO TO 4040
      IVPV  = NOF(IV)
      CALL  SPVAS   ( IVPV,PVPOS,PVVEL,PVACC,PVSLP,
     *                .TRUE.,.TRUE.,.FALSE.,.TRUE.  )
      IF ( MFSTPF(IV) )                          THEN
        IF ( FSTACT(IV) )                        THEN
          IF ( FSTPOS(IV) . LT . PVPOS )         THEN
            PVPOS = FSTPOS(IV)
            MPRO(IV) = .FALSE.
          END IF
        END IF
        IF ( VMSASM(IV) . GT . 0 )               THEN
          IF ( ( IVMSMG(VMSASM(IV)).EQ.VMSMSI ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSL ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSM ) . OR .
     *         ( IVMSMG(VMSASM(IV)).EQ.VMSMSC ) )THEN
            IF ( VMSPST(IV) . LT . PVPOS )       THEN
              PVPOS = VMSPST(IV)
              MPRO(IV) = .FALSE.
            END IF
          END IF
        END IF
      END IF
C$    WRITE (6,'(6H IVPV=,I3,7H PVPOS=,F9.0)') IVPV,PVPOS
      MFINL(IV) = .FALSE.
      MOASF(IV) = .FALSE.
                    IF ( PVVEL . LE . VELSTP )   MOASF(IV) = .TRUE.
 4040 CONTINUE
 5010 CONTINUE
      IF ( ( DZSCHK             ) . AND .
     *     ( TIME . GT . STRTIM ) )              THEN
C-----  SUM DILEMMA ZONE STATISTICS
        IF ( VELOLD . GE . (0.2D0*ISLIM(IA)) )   THEN
          DISEND     = DBLE( LGEOM(4,IL) )  - POSOLD
          TIMEND     = DISEND / VELOLD
          IF      (   TIMEND . LT . DZETIM   )   THEN
C-----      VEHICLE IS BETWEEN STOP LINE AND END OF DILEMMA ZONE
            IF ( MPRO(IV) )                      THEN
              DZS2EG(IA) = DZS2EG(IA) + 1
            ELSE
              DZS2ES(IA) = DZS2ES(IA) + 1
            END IF
          ELSE IF ( ( TIMEND . GE . DZETIM ) . AND .
     *              ( TIMEND . LE . DZBTIM ) )   THEN
C-----      VEHICLE IS BETWEEN END AND BEGIN OF DILEMMA ZONE
            IF ( MPRO(IV) )                      THEN
              DZE2BG(IA) = DZE2BG(IA) + 1
            ELSE
              DZE2BS(IA) = DZE2BS(IA) + 1
            END IF
          ELSE
C-----      VEHICLE IS BETWEEN BEGIN OF DILEMMA ZONE AND LANE BEGIN
            IF ( MPRO(IV) )                      THEN
              DZB2BG(IA) = DZB2BG(IA) + 1
            ELSE
              DZB2BS(IA) = DZB2BS(IA) + 1
            END IF
          END IF
        END IF
      END IF
C-----COMPUTE NEW ACC/DEC LOGIC
      CALL  LOGIC   ( 6,IV )
 5020 CONTINUE
C-----FINISH PROCESSING ALL SIGNAL INDICATIONS
      JPRTM = IPRTM(IV)
                    IF (        MCHKCF(IV)  )    GO TO 5030
                    IF ( (.NOT. MPRO  (IV)) )    GO TO 5030
C-----SET THE INTERSECTION CONTROL LOGIC TIMER FOR NEVER PROCESS AGAIN
      LOGTMP = 0
                    IF ( LNEXT(IV) . EQ . 0 )    RETURN
C-----SET CONFLICTS FOR THE VEHICLE FOR HIS INTERSECTION PATH
      CALL  SETCON
      RETURN
 5030 CONTINUE
C-----SET THE INTERSECTION CONTROL LOGIC TIMER FOR PROCESS NEXT DT
      LOGTMP = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
                    IF ( LNEXT(IV) . EQ . 0 )    RETURN
C-----UNSET THE CONFLICTS FOR THE VEHICLE FOR HIS INTERSECTION PATH
      CALL  UNSETC
      RETURN
      END                                                               SIGRES
C
C
C
      SUBROUTINE SIGARP ( JSISET,ITURNP,IGARP )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INTEGER           IGARP,ITURNP,ITURNQ,JSISET,JTURN,KSISET
C[    JTURN      = -2147483647
C[    KSISET     = -2147483647
C[    ITURNQ     = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'SIGARP'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
                    IF ( JSISET . LE . 0 )       GO TO 9120
                    IF ( ITURNP . EQ . 0 )       GO TO 9680
C-----IF THE SIGNAL INDICATION IS NOT FOR ALL TURN CODES THEN GO TO 5010
C-----AND PROCESS THE SIGNAL INDICATION BY TURN CODES
      ITURNQ = MAX0( ITURNL,ITURNP )
                    IF ( JSISET . GT . 4 )       GO TO 5010
C-----PROCESS THE SIGNAL INDICATION BY THE SIGNAL SETTING NUMBER
C-----         AG   AA   AR   AP
      GO TO ( 1010,2010,3010,4010 ) , JSISET
 1010 CONTINUE
      IGARP = 1
      RETURN
 2010 CONTINUE
      IGARP = 2
      RETURN
 3010 CONTINUE
      IGARP = 3
      RETURN
 4010 CONTINUE
      IGARP = 4
      RETURN
 5010 CONTINUE
                    IF ( JSISET . GT . 10 )      GO TO 5020
C-----SET PARAMETERS FOR CHECKING LEFT TURN PRIMARY AND PROCESS THE
C-----SIGNAL INDICATION BY THE SIGNAL SETTING NUMBER
      KSISET = JSISET - 4
      JTURN = ITURNL
      GO TO 5040
 5020 CONTINUE
                    IF ( JSISET . GT . 16 )      GO TO 5030
C-----SET PARAMETERS FOR CHECKING STRAIGHT PRIMARY AND PROCESS THE
C-----SIGNAL INDICATION BY THE SIGNAL SETTING NUMBER
      KSISET = JSISET - 10
      JTURN = ITURNS
      GO TO 5040
 5030 CONTINUE
                    IF ( JSISET . GT . 22 )      GO TO 5060
C-----SET PARAMETERS FOR CHECKING RIGHT TURN PRIMARY AND PROCESS THE
C-----SIGNAL INDICATION BY THE SIGNAL SETTING NUMBER
      KSISET = JSISET - 16
      JTURN = ITURNR
 5040 CONTINUE
C-----IF THE TURN CODE FOR THE VEHICLE IS NE THE PRIMARY TURN CODE THEN
C-----GO TO 5050 AND PROCESS FOR THE OTHER TURN CODE ELSE PROCESS FOR
C-----THE PRIMARY TURN CODE
C[    IF ( JTURN              .EQ.-2147483647   )STOP 'SIGARP JTURN  01'
C[    IF ( ITURNQ             .EQ.-2147483647   )STOP 'SIGARP ITURNQ 01'
                    IF ( ITURNQ . NE . JTURN )   GO TO 5050
C-----PROCESS FOR THE PRIMARY TURN CODE (FIRST CHARACTER IN SET OF 2)
C[    IF ( KSISET             .EQ.-2147483647   )STOP 'SIGARP KSISET 01'
C-----         GA   GR   AG   AR   RG   RA
      GO TO ( 1010,1010,2010,2010,3010,3010 ) , KSISET
 5050 CONTINUE
C-----PROCESS FOR THE OTHER TURN CODE (SECOND CHARACTER IN SET OF 2)
C[    IF ( KSISET             .EQ.-2147483647   )STOP 'SIGARP KSISET 02'
C-----         GA   GR   AG   AR   RG   RA
      GO TO ( 2010,3010,1010,3010,1010,2010 ) , KSISET
 5060 CONTINUE
                    IF ( JSISET . GT . 25)       GO TO 9120
C-----CHECK FOR PROTECTED GREEN THUS IF THIS VEHICLE IS TURNING LEFT
C-----THEN GO TO 4010 AND PROCESS PROTECTED GREEN
C[    IF ( ITURNQ             .EQ.-2147483647   )STOP 'SIGARP ITURNQ 02'
                    IF ( ITURNQ . EQ . ITURNL )  GO TO 4010
C-----SET PARAMETERS FOR CHECKING PROTECTED GREEN
      KSISET = JSISET - 22
C-----PROCESS FOR THE OTHER TURN CODE (SECOND CHARACTER IN SET OF 2)
C-----         PG   PA   PR
      GO TO ( 1010,2010,3010 ) , KSISET
C-----PROCESS THE EXECUTION ERROR AND STOP
 9120 CONTINUE
      CALL  ABORTR  ( 'STOP 912 - JSISET LE 0 OR GT 25 - SIGARP' )
      STOP  912
 9680 CONTINUE
      CALL  ABORTR  ( 'STOP 968 - ITURNP EQ 0 - SIGARP' )
      STOP  968
      END                                                               SIGARP
C
C
C
      SUBROUTINE LSTOP
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'CONFLT'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
C;    COMMON / TWRD   / DVILNI(NAL,NAP),FVILNI(NAL,NAP),GVILNI(NAL,NAP),
C;   *                  HVILNI(NAL,NAP),TVILNI(NAL,NAP)
C;    DOUBLE PRECISION  DVILNI         ,FVILNI         ,GVILNI         ,
C;   *                  HVILNI         ,TVILNI
C;    COMMON / TWR4   / KVILNI(NAL,NAP)
C;    INTEGER*4         KVILNI
      INTEGER           INDEX,IVATIN,JNDEX,KV,MNEXT
      DOUBLE PRECISION  FHES,GHES,HHES,PIJRIV,THES
C
C-----SUBROUTINE LSTOP CHECKS TO SEE IF THE VEHICLE MAY ENTER THE
C-----INTERSECTION WITHOUT BLOCKING ANY VEHICLE STOPPED AT THE
C-----INTERSECTION BEFORE THIS VEHICLE AND IF OK THEN CHECKS SIGHT
C-----DISTANCE RESTRICTIONS AND IF CLEAR THEN CHECKS INTERSECTIONS
C-----CONFLICTS AND IF CLEAR THEN THE VEHICLE MAY PROCEED INTO THE
C-----INTERSECTION
C
C[    INDEX      = -2147483647
C[    IVATIN     = -2147483647
C[    JNDEX      = -2147483647
C[    KV         = -2147483647
C[    MNEXT      = -2147483647
C[    FHES       = -2147483647.0
C[    GHES       = -2147483647.0
C[    HHES       = -2147483647.0
C[    THES       = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'LSTOP'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IX = LNEXT(IV)
      PIJRIV = PIJR(IDRICL(IV))
C-----SET THE INTERSECTION CONTROL LOGIC TIMER FOR PROCESSING NEXT DT
      LOGTMP = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
C-----IF THERE ARE NO VEHICLES AT THE INTERSECTION THEN GO TO 2010 AND
C-----CHECK SIGHT DISTANCE RESTRICTIONS AND INTERSECTION CONFLICTS
                    IF ( NVATIN . EQ . 0 )       GO TO 2010
C-----IF THE VEHICLES INTERSECTION PATH DOES NOT HAVE ANY GEOMETRIC
C-----CONFLICTS THEN GO TO 2010 AND CHECK SIGHT DISTANCE RESTRICTIONS
                    IF ( NGEOCP(IX) . LE . 0 )   GO TO 2010
C-----CHECK EACH VEHICLE ON THE LIST OF VEHICLES AT THE INTERSECTION
C-----(UNTIL MYSELF) TO SEE IF THIS VEHICLE MAY ENTER THE INTERSECTION
C-----WITHOUT BLOCKING ANY VEHICLE STOPPED AT THE INTERSECTION BEFORE
C-----THIS VEHICLE
      DO 1030  IVATIN = 1 , NVATIN
      KV = LVATIN(IVATIN)
C-----IF THE VEHICLE ON THE LIST OF VEHICLES AT THE INTERSECTION IS ME
C-----THEN GO TO 2010 AND CHECK SIGHT DISTANCE RESTRICTIONS AND
C-----INTERSECTION CONFLICTS
                    IF ( IV . EQ . KV )          GO TO 2010
      IF ( DIAMON )                              THEN
C-----  SKIP IF VEHICLES ARE ON OPPOSITE SIDES OF THE DIAMOND
        IF ( ( IA              . LE . 4 ) . AND .
     *       ( ISNA(LPRES(KV)) . GE . 5 ) )
     *                                           THEN
          GO TO 1030
        END IF
        IF ( ( IA              . GE . 5 ) . AND .
     *       ( ISNA(LPRES(KV)) . LE . 4 ) )
     *                                           THEN
          GO TO 1030
        END IF
      END IF
C-----IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV VEHICLE IS
C-----AN EMERGENCY VEHICLE THEN RETURN AND YIELD TO THE EMERGENCY
C-----VEHICLE
      IF ( ( IAND( VEHTYP(IV),LAVTE ) .EQ. 0 ) . AND .
     *     ( IAND( VEHTYP(KV),LAVTE ) .NE. 0 ) ) THEN
        RETURN
      END IF
C-----IF THE VEHICLE ON THE LIST OF VEHICLES AT THE INTERSECTION MAY
C-----PROCEED INTO THE INTERSECTION THEN GO TO 1030 AND SKIP TO THE NEXT
C-----VEHICLE ON THE LIST OF VEHICLES AT THE INTERSECTION (LET AVDCON
C-----PROCESS THIS VEHICLE)
                    IF ( MPRO(KV) )              GO TO 1030
C-----IF THE VEHICLE ON THE LIST OF VEHICLES AT THE INTERSECTION IS
C-----BLOCKED BECAUSE OF A MAJOR COLLISION THEN GO TO 1030 AND SKIP TO
C-----THE NEXT VEHICLE ON THE LIST OF VEHICLES AT THE INTERSECTION
            IF ( MAJCLB(KV) . OR . MAJCLL(KV) )  GO TO 1030
C-----IF THE VEHICLE ON THE LIST OF VEHICLES AT THE INTERSECTION IS TO
C-----FOLLOW THE UNCONTROLLED LANE LOGIC AND THE INTERSECTION IS NOT
C-----UNCONTROLLED THEN THIS VEHICLE MAY NOT BLOCK HIM THUS RETURN
      IF ( MLUNC(KV) .AND. (ICONTR.GT.ICUNCT) )  RETURN
      MNEXT = LNEXT(KV)
C-----IF THE VEHICLE ON THE LIST OF VEHICLES AT THE INTERSECTION HAS
C-----INTERSECTION CONFLICTS SET AGAINST HIM AND HE IS NOT GOING TO
C-----WAKE UP FOR INTERSECTION CONTROL LOGIC WITHIN THE NEXT DT THEN
C-----GO TO 1030 AND CHECK THE NEXT VEHICLE ON THE LIST OF VEHICLES AT
C-----THE INTERSECTION (THIS VEHICLE MAY BLOCK HIM)
      IF ( (NCPSET(MNEXT) . GT . 0) . AND .
     *     (LOGFLG(KV)    . GT . 2) )            GO TO 1030
C-----CHECK EACH OF MY INTERSECTION CONFLICTS AND SEE IF THE VEHICLE ON
C-----THE LIST OF VEHICLES AT THE INTERSECTION IS ON AN INTERSECTION
C-----PATH THAT CONFLICTS WITH MY INTERSECTION PATH
      DO 1020  INDEX = 1 , NGEOCP(IX)
      JNDEX = IGEOCP(INDEX,IX)
C-----IF THE VEHICLE ON THE LIST OF VEHICLES AT THE INTERSECTION IS ON
C-----AN INTERSECTION PATH THAT CONFLICTS WITH MY INTERSECTION PATH THEN
C-----THIS VEHICLE MAY NOT BLOCK HIM THUS RETURN
            IF ( ICONP(1,JNDEX) . EQ . MNEXT )   RETURN
C-----IF THE VEHICLE ON THE LIST OF VEHICLES AT THE INTERSECTION IS ON
C-----AN INTERSECTION PATH THAT CONFLICTS WITH MY INTERSECTION PATH THEN
C-----THIS VEHICLE MAY NOT BLOCK HIM THUS RETURN
            IF ( ICONP(2,JNDEX) . EQ . MNEXT )   RETURN
C-----END OF INTERSECTION CONFLICT LOOP
 1020 CONTINUE
C-----END OF LIST OF VEHICLES AT THE INTERSECTION LOOP
 1030 CONTINUE
C-----THIS VEHICLE MAY ENTER THE INTERSECTION WITHOUT BLOCKING ANY
C-----VEHICLE STOPPED AT THE INTERSECTION BEFORE THIS VEHICLE
 2010 CONTINUE
C-----CHECK SIGHT DISTANCE RESTRICTIONS AND IF CLEAR THEN CHECK
C-----INTERSECTION CONFLICTS AND IF CLEAR THEN THE VEHICLE MAY PROCEED
C-----INTO THE INTERSECTION
      CALL  CHKSDR
C-----IF THE VEHICLE HAS A SIGHT DISTANCE RESTRICTION OR AN INTERSECTION
C-----CONFLICT AND MAY NOT PROCEED INTO THE INTERSECTION THEN RETURN
C-----ELSE THE VEHICLE MAY PROCEED INTO THE INTERSECTION
                    IF ( (.NOT. MPRO(IV)) )      RETURN
C3    KPFLAG = 'I MAY ENTR'
C-----IF THE VEHICLE IS NOT STOPPED AT THE STOP LINE THEN RETURN ELSE
C-----SET THE VEHICLES ACC/DEC LOGIC TIMER FOR HESITATION
                    IF ( (.NOT. MATSTL(IV)) )    RETURN
C-----THES = 3.0D0*PIJRIV + (PIJRIV+1.0D0)*DMIN1( ONED6*NVATIN,1.5D0 )
C-----ABOVE IS ORIGINAL EQUATION(DIFFERENT THAN 184-1 REPORT?) FOR THIS
C-----DR. LEE WANTS A FUNCTION OF 2.0*DSQRT(PIJR) TO REPLACE THE 3*PIJR
C-----AND TO HAVE THE +1.0 REMOVED FORM THE SECOND TERM OF EQUATION.
C-----I AM APPROXIMATING THE 2*DSQRT(PIJR) WITH 1.0+PIJR.
C-----I AM READING IN HESFAC(DEFAULT=2.0), IT COULD BE MODIFIED IN THE
C-----RANGE OF -PIJR TO SEVERAL SECONDS
C OLD MVATIN = NVATIN
C OLD               IF ( DIAMON )                MVATIN = XVATIN / 2
C OLD MVATIN = MAX0( MVATIN-1,0 )
C OLD THES = HESFAC + PIJRIV*(1.0D0+DMIN1( ONED6*MVATIN,1.5D0 ))
C OLD THES = HESFAC + PIJRIV*(1.0D0-DMIN1( ONED6*MVATIN,1.0D0 ))
      FHES = 1.0D0 - (AMAX0( KVILAI-1,0 )/AMAX0( NIBLAI-1,1 ))
      FHES = DMAX1( DMIN1( FHES,1.0D0 ),0.0D0 )
      GHES = (MVILNI(ILN,IA)-MIN0( NVILNI(ILN,IA),MVILNI(ILN,IA) )) /
     *       AMAX0( MVILNI(ILN,IA)-1,1 )
      GHES = DMAX1( DMIN1( GHES,1.0D0 ),0.0D0 )
      HHES = DMAX1( DMIN1( GHES*FHES + (1.0D0-GHES)*GHES,1.0D0 ),0.0D0 )
      THES = 4.0D0*PIJRIV*HHES
C;    KVILNI(ILN,IA) = KVILNI(ILN,IA) + 1
C;    FVILNI(ILN,IA) = FVILNI(ILN,IA) + FHES
C;    GVILNI(ILN,IA) = GVILNI(ILN,IA) + GHES
C;    HVILNI(ILN,IA) = HVILNI(ILN,IA) + HHES
      IF ( ((IQDS(IV)*DT)+THES) . LT . HESFAC )  THEN
C;      DVILNI(ILN,IA) = DVILNI(ILN,IA) + (HESFAC - IQDS(IV)*DT) - THES
        THES = HESFAC - IQDS(IV)*DT
      END IF
C;    TVILNI(ILN,IA) = TVILNI(ILN,IA) + THES
      IPRTM(IV) = IDNINT( DMAX1( DMIN1( THES/DT,6.0D0/DT,
     *                                  DBLE( MPRTM )    ),0.0D0 ) )
      JPRTM = IPRTM(IV)
      RETURN
      END                                                               LSTOP
C
C
C
      SUBROUTINE CHKSDR
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'CONCHK'
      INCLUDE 'CONFLT'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SDR'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      LOGICAL           JSDRA(NAS),LVMSGO,LVMSRR
      INTEGER           I,IANGLE,IPNDEX,ISDR,JA,JH,JM,JNDEX,JP,MSDR
      DOUBLE PRECISION  ACM,DCM,DESSPD,DISCLM,DCOSAN,DSINAN,DTANAN,
     *                  ERRJUD,HWH,HWM,PCM,PIJRIV,SCM,TCH,TCM,TEL,TFZ,
     *                  TPASCM,TPASSM,VCM
C4    DOUBLE PRECISION  DCH
C4701 FORMAT(8H VEHICLE,I4,3H IS,F7.2,29H SEC FROM THE END OF HIS LANE)
C4702 FORMAT(11H CHKSDR CON,I5,9H APPROACH,I3,4H PO=,F7.1,5H DCH=,
C4   *       F7.1,5H TCH=,F7.2,5H TFZ=,F7.2,5H DCM=,F7.1,5H TCM=,
C4   *       F7.2,5H VCM=,F5.1)
C
C-----SUBROUTINE CHKSDR CHECKS SIGHT DISTANCE RESTRICTIONS AND IF CLEAR
C-----THEN CHECKS INTERSECTION CONFLICTS AND IF CLEAR THEN THE VEHICLE
C-----MAY PROCEED INTO THE INTERSECTION
C
C[    I          = -2147483647
C[    IPNDEX     = -2147483647
C[    ISDR       = -2147483647
C[    JA         = -2147483647
C[    JH         = -2147483647
C[    JM         = -2147483647
C[    JNDEX      = -2147483647
C[    MSDR       = -2147483647
C[    ACM        = -2147483647.0
C[    ERRJUD     = -2147483647.0
C[    TCH        = -2147483647.0
C[    TCM        = -2147483647.0
C[    TEL        = -2147483647.0
C[    TFZ        = -2147483647.0
C[    TPASCM     = -2147483647.0
C[    TPASSM     = -2147483647.0
C[    VCM        = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CHKSDR'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IX = LNEXT(IV)
      PIJRIV = PIJR(IDRICL(IV))
C-----INITIALIZE SOME PARAMETERS FOR CHKSDR
      MPRO(IV) = .FALSE.
      LOGTMP = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
C-----IF THE VEHICLE IS NOT DEDICATED TO AN INTERSECTION PATH THEN
C-----RETURN
                    IF ( IX . EQ . 0 )           RETURN
C-----IF THE VEHICLE IS FORCED TO GO OR THE VEHICLE IS FORCED TO RUN THE
C-----RED SIGNAL THEN GO TO 3010 AND CHECK INTERSECTION CONFLICTS
C-----(IGNORE ANY SIGHT DISTANCE RESTRICTIONS)
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        LVMSGO = ( IVMSMG(VMSASM(IV)) . EQ . VMSMGO )
        LVMSRR = ( IVMSMG(VMSASM(IV)) . EQ . VMSMRR )
      ELSE
        LVMSGO = .FALSE.
        LVMSRR = .FALSE.
      END IF
      IF ( ( MFGOF(IV)                                ) .OR.
     *     ( ( FRRTIM(IV) . GT . 0.0D0              ) .AND.
     *       ( TIME .GE.  FRRTIM(IV)                ) .AND.
     *       ( TIME .LE. (FRRTIM(IV)+FRRATM(IV))    ) ) .OR.
     *     ( LVMSGO                                   ) .OR.
     *     ( LVMSRR                                   ) )
     *                                           GO TO 3010
C-----IF THE LANE IS NOT UNCONTROLLED OR YIELD SIGN CONTROLLED THEN GO
C-----TO 3010 AND CHECK INTERSECTION CONFLICTS (NO SIGHT DISTANCE
C-----RESTRICTIONS FOR STOP SIGN CONTROLLED OR SIGNAL CONTROLLED)
                    IF ( LCONTV(IV).GE.LCSTOP )  GO TO 3010
C-----THE LANE IS UNCONTROLLED OR YIELD SIGN CONTROLLED THUS IF
C-----THERE ARE VEHICLES STOPPED AT THE INTERSECTION WAITING TO ENTER
C-----AND THIS VEHICLE IS NOT STOPPED AT THE STOP LINE AND THE
C-----INTERSECTION IS UNCONTROLLED THEN RETURN AND WAIT UNTIL THE
C-----VEHICLE IS STOPPED AT THE STOP LINE OR THERE ARE NO VEHICLES
C-----WAITING TO ENTER
      IF ( ( IAND( VEHTYP(IV),LAVTE ) .EQ. 0 ) . AND .
     *     ( NVATIN . GT . 0                 ) . AND .
     *     ( (.NOT. MATSTL(IV))              ) . AND .
     *     (        MIUNC (IV)               ) ) RETURN
C-----IF THERE ARE NO SIGHT DISTANCE RESTRICTIONS FOR THIS APPROACH THEN
C-----GO TO 3010 AND CHECK INTERSECTION CONFLICTS
                    IF ( NSDR(IA)  . LE . 0 )    GO TO 3010
C-----IF THE VEHICLES LANE IS UNCONTROLLED WHILE THE INTERSECTION IS
C-----NOT UNCONTROLLED (YIELD SIGN CONTROLLED) THEN THERE ARE NO SIGHT
C-----DISTANCE RESTRICTIONS THUS GO TO 3010 AND CHECK INTERSECTION
C-----CONFLICTS
      IF ( MLUNC(IV) . AND . (.NOT. MIUNC(IV)) ) GO TO 3010
C-----IF THE VEHICLE IS STOPPED AT THE STOP LINE THEN THERE ARE NO SIGHT
C-----DISTANCE RESTRICTIONS THUS GO TO 3010 AND CHECK INTERSECTION
C-----CONFLICTS
                    IF ( MATSTL(IV) )            GO TO 3010
C-----IF THE VEHICLES INTERSECTION PATH DOES NOT HAVE INTERSECTION
C-----CONFLICTS THEN THERE ARE NO SIGHT DISTANCE RESTRICTIONS THUS GO TO
C-----3010 AND CHECK INTERSECTION CONFLICTS
                    IF ( NGEOCP(IX) . LE . 0 )   GO TO 3010
                    IF ( ILVP(IX) . EQ . 0 )     GO TO 1020
C-----IF THE LAST VEHICLE ON THIS VEHICLES INTERSECTION PATH IS STOPPED
C-----THEN RETURN AND WAIT UNTIL IT IS MOVING
            IF ( IVEL(ILVP(IX)) . LE . 0.0D0 )   RETURN
 1020 CONTINUE
C-----SET THE MAXIMUM TIME FROM THE END OF THE LANE THAT THIS VEHICLE
C-----MAY DECIDE TO PROCEED IF THE SIGHT DISTANCE RESTRICTIONS ARE CLEAR
      TEL = 4.0D0
      IF ( MLUNC(IV) )                           THEN
        TEL = TEL + TLEAD + APIJR + 2.0D0
      END IF
                    IF ( MIUNC(IV) )             TEL = 2.0D0
C-----IF EMERGENCY VEHICLE THEN SET TEL TO 30 SECONDS
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   THEN
        TEL = 2.0D0*EVEHFZ
      END IF
C-----SET THIS VEHICLES PARAMETERS FOR PREDICTING TIME AND VELOCITY TO
C-----AN INTERSECTION CONFLICT
      CALL  SETPTV  ( IL )
C-----SET THE POSITION OF THE CONFLICT AS THE END OF THE LANE
      P = LGEOM4
C-----PREDICT THE TIME AND VELOCITY TO THE END OF THE LANE
      CALL  SNOFPV  ( IX )
      CALL  PREDTV  ( TCM,PCM,VCM,ACM,SCM )
C-----IF THERE IS NO TIME TO THE END OF THE LANE FOR ME THEN
C-----GO TO 4020 AND RETURN
                    IF ( TCM . LE . 0.0D0 )      GO TO 4020
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 101
C4                  IF ( TIME . LT . TPRINT )    GO TO 101
C4    WRITE (6,701) IV,TCM
C4101 CONTINUE
C-----IF THE TIME TO THE END OF THE LANE IS GT THE MAXIMUM TIME FROM
C-----THE END OF THE LANE THAT THIS VEHICLE MAY DECIDE TO PROCEED IF
C-----THE SIGHT DISTANCE RESTRICTIONS ARE CLEAR THEN GO TO 4010 AND SET
C-----THE WAKE UP TIME
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKSDR TCM    01'
C[    IF ( TEL                .EQ.-2147483647.0 )STOP 'CHKSDR TEL    01'
                    IF ( TCM . GT . TEL )        GO TO 4010
C-----SET EACH APPROACH THAT THIS APPROACH HAS A SIGHT DISTANCE
C-----RESTRICTION WITH TO NOT CHECKED
      DO 1030  I = 1 , NSDR(IA)
      JSDRA(I) = .FALSE.
 1030 CONTINUE
      MSDR = 0
C-----PROCESS THE INTERSECTION CONFLICTS FROM LAST TO FIRST
      DO 2040  I = 1 , NGEOCP(IX)
C-----IF EACH APPROACH THAT THIS APPROACH HAS A SIGHT DISTANCE
C-----RESTRICTION WITH HAS BEEN CHECKED THEN GO TO 3010 AND CHECK
C-----INTERSECTION CONFLICTS (SIGHT DISTANCE RESTRICTIONS CLEAR)
                    IF ( MSDR . EQ . NSDR(IA) )  GO TO 3010
      JNDEX = IGEOCP(NGEOCP(IX)-I+1,IX)
      IF ( IX . EQ . ICONP(1,JNDEX) )            THEN
        IANGLE =      ICONAN(JNDEX)
        JH     = 2
        JM     = 1
      ELSE
        IANGLE = (360-ICONAN(JNDEX))
        JH     = 1
        JM     = 2
      END IF
      DSINAN = DABS( DSIN( DBLE( IANGLE )*DEG2RD ) )
      DCOSAN = DABS( DCOS( DBLE( IANGLE )*DEG2RD ) )
      IF ( DCOSAN . LE . 1.0D-8 )                THEN
        DTANAN = 1.0D99
      ELSE
        DTANAN = DSINAN / DCOSAN
      END IF
C-----FIND THE LINKING INBOUND APPROACH NUMBER FOR THE CONFLICTING PATH
      JA = ICONA(JH,JNDEX)
      JP = ICONP(JH,JNDEX)
C-----CHECK EACH APPROACH THAT THIS APPROACH HAS A SIGHT DISTANCE
C-----RESTRICTION WITH
      DO 1050  ISDR = 1 , NSDR(IA)
C-----IF THE INTERSECTION PATH CAME FROM AN APPROACH THAT HAS A SIGHT
C-----DISTANCE RESTRICTION WITH US THEN GO TO 1060 AND CONTINUE
            IF ( ISDRA(ISDR,IA) . EQ . JA )      GO TO 1060
 1050 CONTINUE
C-----THE INTERSECTION PATH DID NOT COME FROM AN APPROACH THAT HAS A
C-----SIGHT DISTANCE RESTRICTION WITH US THUS SKIP TO THE NEXT
C-----INTERSECTION CONFLICT
      GO TO 2040
 1060 CONTINUE
C-----IF THE LINKING INBOUND APPROACH THAT THE INTERSECTION PATH CAME
C-----FROM HAS ALREADY BEEN CHECKED THEN SKIP TO THE NEXT INTERSECTION
C-----CONFLICT
C[    IF ( ISDR               .EQ.-2147483647   )STOP 'CHKSDR ISDR   01'
                    IF ( JSDRA(ISDR) )           GO TO 2040
C-----SET THE PARAMETERS FOR CHECKING SIGHT DISTANCE RESTRICTIONS
C-----SET THIS VEHICLES PARAMETERS FOR PREDICTING TIME AND VELOCITY TO
C-----AN INTERSECTION CONFLICT
      CALL  SETPTV  ( IL )
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'CHKSDR JNDEX  01'
      P = ICOND(JM,JNDEX) + LGEOM4
                    IF ( (.NOT. IFVA(IV)) )      GO TO 1070
                    IF ( IVPV . EQ . 0 )         GO TO 1070
C-----THIS VEHICLES ACC/DEC LOGIC SAYS TO FOLLOW THE VEHICLE AHEAD THUS
C-----MIN THE DESIRED SPEED WITH THE DESIRED SPEED OF THE VEHICLE AHEAD
      CALL  SETDSP  ( IVPV,IPOS(IVPV),DBLE( ISPD(IVPV) ),.FALSE.,
     *                DESSPD                                      )
      JSPD = MIN0( IDNINT( DESSPD ),JSPD )
 1070 CONTINUE
      DCM = P - PO
C-----IF THERE IS NO DISTANCE TO TRAVEL THEN GO TO 2010 AND FIND THE
C-----TIME TO THE INTERSECTION CONFLICT FOR ME
                    IF ( P-PO . LE . 0.0D0 )     GO TO 2010
C-----PREDICT THE TIME AND VELOCITY TO AN INTERSECTION CONFLICT
      CALL  SNOFPV  ( IX )
      CALL  PREDTV  ( TCM,PCM,VCM,ACM,SCM )
C-----IF THE VEHICLE STOPS BEFORE THE INTERSECTION CONFLICT THEN GO TO
C-----4020 AND RETURN
                    IF ( TCM . LE . -999.9D0 )   GO TO 4020
      GO TO 2020
 2010 CONTINUE
C-----THERE WAS NO DISTANCE TO TRAVEL THUS SET THE TIME TO THE CONFLICT
C-----FOR ME TO 0 AND THE VELOCITY AT THE CONFLICT TO MY CURRENT SPEED
      TCM = 0.0D0
      VCM = VO
                    IF ( VCM . LE . 0.0D0 )      GO TO 2020
C-----THE CURRENT SPEED IS GT 0 THUS COMPUTE THE TIME TO THE CONFLICT
      TCM = (P-PO)/VCM
 2020 CONTINUE
      TPASCM = 1.0D+9
      TPASSM = 1.0D+9
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'CHKSDR VCM    01'
                    IF ( VCM . LE . 0 )          GO TO 2030
C-----FIND THE TIME FOR MY VEHICLE TO PASS THE CONFLICT AT THE VELOCITY
C-----AT THE CONFLICT FOR ME
      HWH = 0.5D0*WIDMAX
      HWM = 0.5D0*WIDV(IVEHCL(IV))
C-----IF THE INTERSECTION CONFLICT IS NOT A MERGE (THE LINKING OUTBOUND
C-----LANE FOR THE INTERSECTION PATHS ARE DIFFERENT) THEN CALCULATE
C-----DISCLM ELSE DISCLM IS ZERO
      DISCLM = 0.0D0
      IF ( LOBL(IX) . NE . LOBL(JP) )            THEN
        IF ( DTANAN . LE . 0.5D0 )               THEN
          DISCLM = DISCLM + 2.0D0*HWM
        ELSE
          DISCLM = DISCLM + HWM/DTANAN
        END IF
        IF ( DSINAN . LE . 0.5D0 )               THEN
          DISCLM = DISCLM + 2.0D0*HWH
        ELSE
          DISCLM = DISCLM + HWH/DSINAN
        END IF
      END IF
      TPASCM = DISCLM/VCM
      TPASSM = LENVAP/VCM
 2030 CONTINUE
C-----SET UP AN ARTIFICIAL VEHICLE ON THE OTHER APPROACH
C[    IF ( JA                 .EQ.-2147483647   )STOP 'CHKSDR JA     01'
      JSLIM = ISLIM(JA)
C-----THE VELOCITY OF THE ARTIFICIAL VEHICLE WILL BE THE SPEED LIMIT OF
C-----THE OTHER APPROACH
      VO = JSLIM
      IPNDEX = MIN0 ( IDINT( POSNEW/25.0D0 )+1,40 )
C-----THE POSITION OF THE ARTIFICIAL VEHICLE WILL BE AT THE POINT JUST
C-----VISIBLE BY THIS VEHICLE AROUND THE SIGHT DISTANCE RESTRICTION
C[    IF ( ISDR               .EQ.-2147483647   )STOP 'CHKSDR ISDR   02'
      PO = ICANSE(IPNDEX,ISDR)
C[    IF ( JH                 .EQ.-2147483647   )STOP 'CHKSDR JH     02'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'CHKSDR JNDEX  02'
      LGEOM4 = LGEOM(4,LIBL(ICONP(JH,JNDEX)))
C-----THE POSITION THE ARTIFICIAL VEHICLE HAS TO TRAVEL TO IS THE
C-----INTERSECTION CONFLICT
      P = ICOND(JH,JNDEX) + LGEOM4
C4    DCH = P - PO
C-----COMPUTE THE TIME TO THE CONFLICT FOR HIM BASED ON THE DISTANCE HE
C-----HAS TO TRAVEL AND A CONSTANT SPEED (SPEED LIMIT FOR THE APPROACH)
      TCH = (P-PO)/VO
C-----FIND THE ERROR IN JUDGMENT
      ERRJUD = DMAX1( 0.0D0,PIJRIV*(TCH-5.0D0)/7.0D0)
C-----FIND THE TIME THAT HIS FRONT ZONE WILL ARRIVE AT THE CONFLICT
C[    IF ( TPASSM             .EQ.-2147483647.0 )STOP 'CHKSDR TPASSM 01'
C[    IF ( TPASCM             .EQ.-2147483647.0 )STOP 'CHKSDR TPASCM 01'
      TFZ = TCH - TPASSM - TPASCM - TLEAD - PIJRIV - 0.5D0*ERRJUD
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 102
C4                  IF ( TIME . LT . TPRINT )    GO TO 102
C[    IF ( JA                 .EQ.-2147483647   )STOP 'CHKSDR JA     02'
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKSDR TCM    02'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'CHKSDR VCM    02'
C4    WRITE (6,702) JNDEX,JA,PO,DCH,TCH,TFZ,DCM,TCM,VCM
C4102 CONTINUE
C-----IF THE TIME TO THE CONFLICT FOR ME IS GT THE TIME HIS FRONT ZONE
C-----WILL ARRIVE AT THE CONFLICT THEN I AM BLOCKED BY HIM THUS GO TO
C-----4020 AND SET THE WAKE UP TIME
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKSDR TCM    03'
C[    IF ( TFZ                .EQ.-2147483647.0 )STOP 'CHKSDR TFZ    01'
                    IF ( TCM . GT . TFZ )        GO TO 4020
C-----SET THE OTHER APPROACH CHECKED
C[    IF ( ISDR               .EQ.-2147483647   )STOP 'CHKSDR ISDR   03'
      JSDRA(ISDR) = .TRUE.
C[    IF ( MSDR               .EQ.-2147483647   )STOP 'CHKSDR MSDR   01'
      MSDR = MSDR + 1
C-----END OF INTERSECTION CONFLICT LOOP
 2040 CONTINUE
C-----ALL SIGHT DISTANCE RESTRICTIONS ARE CLEAR
 3010 CONTINUE
C-----CHECK INTERSECTION CONFLICTS AND IF CLEAR THEN THE VEHICLE MAY
C-----PROCEED INTO THE INTERSECTION
      CALL  CHKCON
      RETURN
 4010 CONTINUE
C-----THE TIME TO THE CONFLICT IS GT THE MAXIMUM TIME FROM THE END OF
C-----THE LANE THAT THIS VEHICLE MAY DECIDE TO PROCEED IF THE SIGHT
C-----DISTANCE RESTRICTIONS ARE CLEAR THUS SET THE INTERSECTION CONTROL
C-----LOGIC TIMER TO WAKE UP WHEN CLOSER
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKSDR TCM    04'
C[    IF ( TEL                .EQ.-2147483647.0 )STOP 'CHKSDR TEL    02'
      LOGTMP = MAX0( MIN0( IDINT(2.0D0+5.0D0/DT)         ,
     *                     MPRTM                         ,
     *                     IDINT(2.0D0+(TCM-TEL-DTMAX)/DT) ),2 )
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
      RETURN
 4020 CONTINUE
C-----THE TIME TO THE CONFLICT FOR ME IS GT THE TIME HIS FRONT ZONE
C-----WILL ARRIVE AT THE CONFLICT THUS I AM BLOCKED BY HIM
      RETURN
      END                                                               CHKSDR
C
C
C
      SUBROUTINE CHKCON
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'CONCHK'
      INCLUDE 'CONFLT'
      INCLUDE 'CONSTN'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      LOGICAL           EVRESP,IGNORE,LVMSGO,LVMSRR
      INTEGER           I,IANGLE,INDEX,IVATIN,IVCONF,JH,JL,JM,JNDEX,JP,
     *                  JPRC,JV,KOUNT,KPRTM,KV,GETLCV,MOBL,NOFC
C     INTEGER           JSISET
      DOUBLE PRECISION  A,ACCJV,ACCVEH,ACH,ACM,AFACT,B,C,CARDIS,CLEARD,
     *                  CLEART,CRISLP,DCH,DCM,DCOSAN,DESSPD,DISCLH,
     *                  DISCLM,DSINAN,DSLOWH,DSLOWM,DTANAN,DVH,DVM,
     *                  ERRJUD,FHES,GHES,HHES,HWH,HWM,LVH,LVM,PAD,PCH,
     *                  PCM,PIJRIV,POP,POSADD,POSEND,POSJV,POSNRB,
     *                  POSSAV,POSSTP,POSVEH,POT,PT,SAFF,SAFR,SAFVEL,
     *                  SCH,SCM,SLOPE,SLPJV,SLPTMP,SLPVEH,T,TCH,TCM,
     *                  TCRASH,TEL,TFZ,THES,TMAX,TMP,TPASCH,TPASCM,
     *                  TPASSC,TPASSH,TPASSM,TRZ,TS,VCH,VCM,VELJV,
     *                  VELREL,VELVEH
C     DOUBLE PRECISION  FCLEAR,RCLEAR
C 601 FORMAT(I6,F8.2,1X,A6,I5,2I4,4I3,I4,F6.2,4F6.1,I6,3F6.2,4F6.1,
C    *       2F7.2)
C4701 FORMAT(8H VEHICLE,I4,3H IS,F7.2,29H SEC FROM THE END OF HIS LANE)
C4702 FORMAT(4H CON,I4,4H VEH,I4,5H TCM=,F5.2,5H VCM=,F4.1,5H DVM=,F5.1,
C4   *       5H DCM=,F5.1,6H   VEH,I4,5H TFZ=,F5.2,5H TCH=,F5.2,
C4   *       5H TRZ=,F5.2,5H VCH=,F4.1,5H DVH=,F4.1,5H DCH=,F5.1)
C
C-----SUBROUTINE CHKCON CHECKS INTERSECTION CONFLICTS AND IF CLEAR THEN
C-----THE VEHICLE MAY PROCEED INTO THE INTERSECTION
C
C[    I          = -2147483647
C[    INDEX      = -2147483647
C[    IVCONF     = -2147483647
C[    JH         = -2147483647
C[    JL         = -2147483647
C[    JM         = -2147483647
C[    JNDEX      = -2147483647
C[    JP         = -2147483647
C[    KOUNT      = -2147483647
C[    KPRTM      = -2147483647
C[    NOFC       = -2147483647
C[    ACH        = -2147483647.0
C[    ACM        = -2147483647.0
C[    AFACT      = -2147483647.0
C[    CARDIS     = -2147483647.0
C[    DCH        = -2147483647.0
C[    DCM        = -2147483647.0
C[    DVH        = -2147483647.0
C[    DVM        = -2147483647.0
C[    ERRJUD     = -2147483647.0
C[    FHES       = -2147483647.0
C[    GHES       = -2147483647.0
C[    HHES       = -2147483647.0
C[    SLOPE      = -2147483647.0
C[    TCH        = -2147483647.0
C[    TCM        = -2147483647.0
C[    TCRASH     = -2147483647.0
C[    TEL        = -2147483647.0
C[    TFZ        = -2147483647.0
C[    THES       = -2147483647.0
C[    TMP        = -2147483647.0
C[    TPASCH     = -2147483647.0
C[    TPASCM     = -2147483647.0
C[    TPASSC     = -2147483647.0
C[    TPASSH     = -2147483647.0
C[    TPASSM     = -2147483647.0
C[    TRZ        = -2147483647.0
C[    VCH        = -2147483647.0
C[    VCM        = -2147483647.0
C[    VELREL     = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CHKCON'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      PIJRIV =        PIJR (IDRICL(IV))
      CRISLP = SLPMAX*DCHAR(IDRICL(IV))
      EVRESP = .FALSE.
C-----IF THE VEHICLE IS FORCED TO GO OR THE VEHICLE IS FORCED TO RUN THE
C-----RED SIGNAL THEN GO TO 3020 AND THE VEHICLE MAY PROCEED INTO THE
C-----INTERSECTION
      IF ( VMSASM(IV) . GT . 0 )                 THEN
        LVMSGO = ( IVMSMG(VMSASM(IV)) . EQ . VMSMGO )
        LVMSRR = ( IVMSMG(VMSASM(IV)) . EQ . VMSMRR )
      ELSE
        LVMSGO = .FALSE.
        LVMSRR = .FALSE.
      END IF
      IF ( ( MFGOF(IV)                                ) .OR.
     *     ( ( FRRTIM(IV) . GT . 0.0D0              ) .AND.
     *       ( TIME .GE.  FRRTIM(IV)                ) .AND.
     *       ( TIME .LE. (FRRTIM(IV)+FRRATM(IV))    ) ) .OR.
     *     ( LVMSGO                                   ) .OR.
     *     ( LVMSRR                                   ) )
     *                                           GO TO 3020
      IX  = LNEXT(IV)
                    IF ( IX . EQ . 0 )           RETURN
C-----IF THE LAST VEHICLE ON THE INTERSECTION PATH IS STOPPED THEN
C-----RETURN AND WAIT UNTIL HE IS MOVING OUT
      IF ( ILVP(IX) . GT . 0 )                   THEN
        IF ( IVEL(ILVP(IX)) . LE . 0.0D0 )       RETURN
      END IF
      HWM = 0.5D0*WIDV(IVEHCL(IV))
      LVM = LENVAP
C-----SET THE END OF THE LANE
      IF ( LGEOM(3,IL) . EQ . LGEOM(4,IL) )      THEN
        POSEND = DBLE( LGEOM(2,IL) )
      ELSE
        POSEND = DBLE( LGEOM(4,IL) )
      END IF
C-----CHECK THE LAST VEHICLE ON ALL INTERSECTIONS PATHS FROM THIS
C-----LANE OR THE LAST VEHICLE ON THE LINKING OUTBOUND LANE FOR THE
C-----INTERSECTION PATH FROM THIS LANE IF THERE IS NO VEHICLE ON THE
C-----INTERSECTION PATH (POSSIBLY A LONG VEHICLE) WHOSE REAR BUMPER IS
C-----STILL ON THIS LANE OR THAT HAS NOT TRAVELED FAR ENOUGH TO CLEAR
C-----THIS VEHICLE AND IS STOPPED
      POSNRB = POSNEW - LENVAP
      DO  I = 1 , NPINT(IL)
        JP = LINTP(I,IL)
        JV = ILVP(JP)
        IF ( JV . EQ . 0 )                       THEN
          MOBL = LOBL(JP)
                    IF ( MOBL . EQ . 0 )         CYCLE
          JV = ILVL(MOBL)
                    IF ( JV . EQ . 0 )           CYCLE
          IF ( ( JP . NE . IX        ) . AND .
     *         ( JP . NE . LPREV(JV) ) )         CYCLE
C-----    CHECK IF THE REAR BUMPER OF THE LAST VEHICLE ON INTERSECTION
C-----    PATH JP LINKING OUTBOUND LANE HAS NOT TRAVELED FAR ENOUGH TO
C-----    CLEAR THIS VEHICLE
          CALL  SPVAS  ( JV,POSJV,VELJV,ACCJV,SLPJV,
     *                   .TRUE.,.FALSE.,.FALSE.,.TRUE. )
          IF ( POSJV . GE .
     *         DBLE( LGEOM(1,MOBL) ) )           CYCLE
          POSADD = DBLE( LGEOM(4,IL)+LENP(JP)-LGEOM(1,MOBL) )
        ELSE
C-----    CHECK IF THE REAR BUMPER OF THE LAST VEHICLE ON INTERSECTION
C-----    PATH JP HAS NOT TRAVELED FAR ENOUGH TO CLEAR THIS VEHICLE
          CALL  SPVAS  ( JV,POSJV,VELJV,ACCJV,SLPJV,
     *                   .TRUE.,.FALSE.,.FALSE.,.TRUE. )
          POSADD = DBLE( LGEOM(4,IL) )
        END IF
        POSJV = POSJV  + POSADD
        IF ( POSJV . LT . POSNRB )               CYCLE
        IF ( ( POSJV . LE . POSEND ) . AND .
     *       ( VELJV . LE . 0.0D0  ) )           THEN
          RETURN
        END IF
        IF ( JP . EQ . IX )                      THEN
          IF ( VELJV . LE . 0.0D0 )              THEN
            RETURN
          ELSE
            CYCLE
          END IF
        END IF
        HWH    = 0.5D0*WIDV(IVEHCL(JV))
        SAFVEL = DMAX1( VELOLD  ,VELNEW,DBLE( ISPD(IV) ),
     *                  IVEL(JV),VELJV ,DBLE( ISPD(JV) ) )
        SAFR   = (SAFDIS+(SAFVEL/SAFSPD))/DCHAR(IDRICL(IV))
C-----  IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE JV
C-----  VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE FALSE
        EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *             ( IAND( VEHTYP(JV),LAVTE ) . NE . 0 ) )
        IF ( EVRESP )                            THEN
          SAFR = DMAX1( SAFR,EVEHRZ*SAFVEL )
        END IF
        CLEARD = HWM + SAFR + HWH
        CALL  CCLEAR  ( IX,JP,CLEARD,.TRUE.,CLEART )
                    IF ( CLEART . EQ . 0.0D0 )   CYCLE
        CLEART = CLEART + DBLE( LGEOM(4,IL) )
        IF ( ( POSJV . LE . CLEART ) . AND .
     *       ( VELJV . LE . 0.0D0  ) )           THEN
          RESPEV = ( RESPEV . OR . EVRESP )
          RETURN
        END IF
C-----END OF LOOP FOR INTERSECTION PATHS ORIGINATING FROM THIS
C-----LANE
      END DO
C-----IF ANY VEHICLE IN FRONT OF THIS VEHICLE IN THIS LANE MAY NOT
C-----PROCEED INTO THE INTERSECTION OR IS STOPPED THEN THIS VEHICLE MAY
C-----NOT PROCEED INTO THE INTERSECTION
      JV = NOF(IV)
      DO  WHILE ( JV . GT . 0 )
        IF ( ( .NOT. MPRO(JV)        ) . OR .
     *       ( IVEL(JV) . EQ . 0.0D0 ) )         THEN
          RETURN
        END IF
        JV = NOF(JV)
      END DO
C-----IF THERE ARE NO GEOMETRIC CONFLICTING PATHS THEN GO TO 3020 AND
C-----THE VEHICLE MAY PROCEED INTO THE INTERSECTION
                    IF ( NGEOCP(IX) . LE . 0 )   GO TO 3020
C-----IF THE VEHICLE IS WITHIN XRELMX OF THE END OF THE LANE THEN GO TO
C-----1010 AND CHECK CONFLICTS
                    IF ( RELEND . LE . XRELMX )  GO TO 1010
C-----SET THE MAXIMUM TIME FROM THE END OF THE LANE THAT THIS VEHICLE
C-----MAY DECIDE TO PROCEED IF THE INTERSECTION CONFLICTS ARE CLEAR
      TEL = 4.0D0
      IF ( MLUNC(IV) )                           THEN
        TEL = TEL + TLEAD + APIJR + 2.0D0
      END IF
                    IF ( MIUNC(IV) )             TEL = 2.0D0
C-----IF EMERGENCY VEHICLE THEN SET TEL TO 30 SECONDS
      IF ( IAND( VEHTYP(IV),LAVTE ) . NE . 0 )   THEN
        TEL = 2.0D0*EVEHFZ
      END IF
C-----SET THIS VEHICLES PARAMETERS FOR PREDICTING TIME AND VELOCITY TO
C-----AN INTERSECTION CONFLICT
      CALL  SETPTV  ( IL )
C-----SET THE POSITION OF THE CONFLICT AS THE END OF THE LANE
      P = LGEOM4
C-----IF THE VEHICLE IS BEYOND THE END OF THE LANE THEN GO TO 1010 AND
C-----CHECK CONFLICTS
                    IF ( PO . GE . P )           GO TO 1010
C-----PREDICT THE TIME AND VELOCITY TO THE END OF THE LANE
      CALL  SNOFPV  ( IX )
      CALL  PREDTV  ( TCM,PCM,VCM,ACM,SCM )
C-----IF THERE IS NO TIME TO THE END OF THE LANE FOR ME THEN
C-----GO TO 4020 AND RETURN
                    IF ( TCM . LE . 0.0D0 )      GO TO 4020
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 101
C4                  IF ( TIME . LT . TPRINT )    GO TO 101
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKCON TCM    01'
C4    WRITE (6,701) IV,TCM
C4101 CONTINUE
C-----IF THE TIME TO THE END OF THE LANE IS GT THE MAXIMUM TIME FROM
C-----THE END OF THE LANE THAT THIS VEHICLE MAY DECIDE TO PROCEED IF
C-----THE INTERSECTION CONFLICTS ARE CLEAR THEN GO TO 4010 AND SET THE
C-----WAKE UP TIME
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKCON TCM    02'
C[    IF ( TEL                .EQ.-2147483647.0 )STOP 'CHKCON TEL    01'
                    IF ( TCM . GT . TEL )        GO TO 4010
 1010 CONTINUE
C-----IF THERE ARE NO INTERSECTION CONFLICTS SET THEN GO TO 3020 AND
C-----THE VEHICLE MAY PROCEED INTO THE INTERSECTION
                    IF ( NCPSET(IX) . LE . 0 )   GO TO 3020
C-----IF THE VEHICLES REAR BUMPER HAS PASSED ALL GEOMETRIC CONFLICTING
C-----PATHS THEN GO TO 3020 AND THE VEHICLE MAY PROCEED INTO THE
C-----INTERSECTION
            IF ( ISTCON(IV) . GT . NGEOCP(IX) )  GO TO 3020
C-----CHECK EACH GEOMETRIC CONFLICTING INTERSECTION PATH
      DO 3010  INDEX = 1 , NGEOCP(IX)
C-----IF THE INTERSECTION CONFLICT IS NOT SET THEN SKIP TO THE NEXT
C-----INTERSECTION CONFLICT
                    IF ( ICPSET(INDEX,IX).EQ.0 ) GO TO 3010
C-----INITIALIZE SOME PARAMETERS FOR CHKCON
      JNDEX = IGEOCP(INDEX,IX)
      KOUNT = 0
      IF ( IX . EQ . ICONP(1,JNDEX) )            THEN
        IANGLE =      ICONAN(JNDEX)
        JH     = 2
        JM     = 1
      ELSE
        IANGLE = (360-ICONAN(JNDEX))
        JH     = 1
        JM     = 2
      END IF
      DSINAN = DABS( DSIN( DBLE( IANGLE )*DEG2RD ) )
      DCOSAN = DABS( DCOS( DBLE( IANGLE )*DEG2RD ) )
      IF ( DCOSAN . LE . 1.0D-8 )                THEN
        DTANAN = 1.0D99
      ELSE
        DTANAN = DSINAN / DCOSAN
      END IF
C-----SET IVCONF TO THE NEXT VEHICLE THAT HAS NOT CLEARED THE
C-----INTERSECTION CONFLICT
      IVCONF = ICONV(JH,JNDEX)
      JP = ICONP(JH,JNDEX)
      JL = LIBL(JP)
      TCM = 0.0D0
C-----SET NOFC TO THE IVCONF VEHICLE
      NOFC = IVCONF
C;    WRITE (TC3,331) TIME,'CHKCON 1 NOFC = IVCONF             ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'IGEOCP(INDEX,IX)=',IGEOCP(INDEX,IX),
C;   *                'JM=',INDEX,'N=',NGEOCP(IX)
C;331 FORMAT(F6.1,1X,A36,I2,I3,I4,I6,1X,L1,F5.1,3I3,I6,2(I4,I6),
C;   *       5(1X,A,I4))
            IF ( MININT(NOFC) )                  GO TO 1020
            IF ( LPRES(NOFC) . EQ . LOBL(JP) )   GO TO 1020
C-----THE NOFC VEHICLE WAS NOT IN THE INTERSECTION THUS SET THE NOFC
C-----VEHICLE TO THE FIRST VEHICLE IN THE OTHER LANE
      NOFC = IFVL(JL)
C;    WRITE (TC3,331) TIME,'CHKCON 2 NOFC = IFVL(JL)           ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JL=',JL,'JM=',INDEX,'N=',NGEOCP(IX)
 1020 CONTINUE
C-----SET THIS VEHICLES PARAMETERS FOR PREDICTING TIME AND VELOCITY TO
C-----AN INTERSECTION CONFLICT
      CALL  SETPTV  ( IL )
C[    IF ( JM                 .EQ.-2147483647   )STOP 'CHKCON JM     01'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'CHKCON JNDEX  01'
      P = ICOND(JM,JNDEX) + LGEOM4
                    IF ( (.NOT. IFVA(IV)) )      GO TO 1030
                    IF ( IVPV . EQ . 0 )         GO TO 1030
C-----THIS VEHICLES ACC/DEC LOGIC SAYS TO FOLLOW THE VEHICLE AHEAD THUS
C-----MIN THE DESIRED SPEED WITH THE DESIRED SPEED OF THE VEHICLE AHEAD
      CALL  SETDSP  ( IVPV,IPOS(IVPV),DBLE( ISPD(IVPV) ),.FALSE.,
     *                DESSPD                                      )
      JSPD = MIN0( IDNINT( DESSPD ),JSPD )
 1030 CONTINUE
      DVM = JSPD
      DCM = P - PO
C-----IF THERE IS NO DISTANCE TO TRAVEL TO THE INTERSECTION CONFLICT
C-----THEN GO TO 1050 AND FIND THE TIME TO THE INTERSECTION CONFLICT FOR
C-----ME
                    IF ( P-PO . LE . 0.0D0 )     GO TO 1050
 1040 CONTINUE
C-----PREDICT THE TIME AND VELOCITY TO AN INTERSECTION CONFLICT
      CALL  SNOFPV  ( IX )
      CALL  PREDTV  ( TCM,PCM,VCM,ACM,SCM )
C-----IF THE VEHICLE STOPS BEFORE THE INTERSECTION CONFLICT THEN GO TO
C-----4020 AND RETURN
                    IF ( TCM . LE . -999.9D0 )   GO TO 4020
                    IF ( LCONTV(IV).GE.LCSIGX )  GO TO 1060
                    IF ( (.NOT. MATSTL(IV)) )    GO TO 1060
C-----THE LANE IS NOT SIGNAL CONTROLLED AND THE VEHICLE IS STOPPED AT
C-----THE STOP LINE THUS INCREMENT THE TIME TO THE CONFLICT FOR ME BY
C-----THE AVERAGE HESITATION TIME
C-----TCM = TCM + 4.0*PIJRIV
C-----ABOVE IS ORIGINAL EQUATION, REPLACED BY NEW FUNCTION ON 1/30/87
C OLD MVATIN = NVATIN
C OLD               IF ( DIAMON )                MVATIN = XVATIN / 2
C OLD MVATIN = MAX0( MVATIN-1,0 )
C OLD THES = HESFAC + PIJRIV*(1.0D0+DMIN1( ONED6*MVATIN,1.5D0 ))
C OLD THES = HESFAC + PIJRIV*(1.0D0-DMIN1( ONED6*MVATIN,1.0D0 ))
      FHES = 1.0D0 - (AMAX0( KVILAI-1,0 )/AMAX0( NIBLAI-1,1 ))
      FHES = DMAX1( DMIN1( FHES,1.0D0 ),0.0D0 )
      GHES = (MVILNI(ILN,IA)-MIN0( NVILNI(ILN,IA),MVILNI(ILN,IA) )) /
     *       AMAX0( MVILNI(ILN,IA)-1,1 )
      GHES = DMAX1( DMIN1( GHES,1.0D0 ),0.0D0 )
      HHES = DMAX1( DMIN1( GHES*FHES + (1.0D0-GHES)*GHES,1.0D0 ),0.0D0 )
      THES = 4.0D0*PIJRIV*HHES
      IF ( ((IQDS(IV)*DT)+THES) . LT . HESFAC )  THEN
        THES = HESFAC - IQDS(IV)*DT
      END IF
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKCON TCM    03'
      TCM = TCM + THES
      GO TO 1060
 1050 CONTINUE
C-----THERE IS NO DISTANCE TO TRAVEL TO THE INTERSECTION CONFLICT THUS
C-----FIND THE TIME TO THE INTERSECTION CONFLICT FOR ME
      TCM = 0.0D0
      ACM = AO
      VCM = VO
                    IF ( VCM . LE . 0.0D0 )      GO TO 1060
      TCM = (P-PO)/VCM
 1060 CONTINUE
C-----FIND THE TIME FOR MY VEHICLE TO PASS THE INTERSECTION CONFLICT AT
C-----THE VELOCITY AT THE INTERSECTION CONFLICT FOR ME
      TPASCM = 1.0D+9
      TPASSM = 1.0D+9
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'CHKCON VCM    01'
                    IF ( VCM . LE . 0.0D0 )      GO TO 1070
      HWH    = 0.5D0*WIDMAX
C-----IF THE INTERSECTION CONFLICT IS NOT A MERGE (THE LINKING OUTBOUND
C-----LANE FOR THE INTERSECTION PATHS ARE DIFFERENT) THEN CALCULATE
C-----DISCLM ELSE DISCLM IS ZERO
      DISCLM = 0.0D0
      IF ( LOBL(IX) . NE . LOBL(JP) )            THEN
        IF ( DTANAN . LE . 0.5D0 )               THEN
          DISCLM = DISCLM + 2.0D0*HWM
        ELSE
          DISCLM = DISCLM + HWM/DTANAN
        END IF
        IF ( DSINAN . LE . 0.5D0 )               THEN
          DISCLM = DISCLM + 2.0D0*HWH
        ELSE
          DISCLM = DISCLM + HWH/DSINAN
        END IF
      END IF
      TPASCM = DISCLM/VCM
      TPASSM = LENVAP/VCM
 1070 CONTINUE
C-----START OF LOOP FOR CHECKING FOR THIS INTERSECTION CONFLICT
C[    IF ( KOUNT              .EQ.-2147483647   )STOP 'CHKCON KOUNT  01'
      KOUNT = KOUNT + 1
                    IF ( KOUNT . GT . 50 )       GO TO 9130
C-----IF THE IVCONF VEHICLE HAS NOT SET CONFLICTS THEN HE MAY NOT
C-----PROCEED INTO THE INTERSECTION THUS THERE CAN BE NO INTERSECTION
C-----CONFLICT THUS GO TO 3010 AND SKIP TO THE NEXT INTERSECTION
C-----CONFLICT (THIS ONE IS CLEAR)
                    IF ( IVCONF . EQ . 0 )       GO TO 3010
      IF      ( IPRC(1,IVCONF) . EQ . JP )       THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IVCONF) . EQ . JP )       THEN
        JPRC = 2
      ELSE IF ( IPRC(1,IVCONF) . EQ .  0 )       THEN
        JPRC = 1
        NORC(1,IVCONF) = NVEP1
      ELSE IF ( IPRC(2,NOFC) . EQ .  0 )         THEN
        JPRC = 2
        NORC(2,IVCONF) = NVEP1
      ELSE
        GO TO 9460
      END IF
            IF ( NORC(JPRC,IVCONF) . EQ . NVEP1 )GO TO 3010
C-----IF THE NOFC VEHICLE IS THE IVCONF VEHICLE THEN GO TO 1080 AND
C-----CHECK THE IVCONF VEHICLE
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'CHKCON IVCONF 01'
C[    IF ( NOFC               .EQ.-2147483647   )STOP 'CHKCON NOFC   01'
                    IF ( NOFC . EQ . IVCONF )    GO TO 1080
C-----IF THE NOFC VEHICLE HAS NOT SET CONFLICTS THEN HE MAY NOT PROCEED
C-----INTO THE INTERSECTION THUS HE WILL BLOCK THE IVCONF VEHICLE FROM
C-----PROCEEDING INTO THE INTERSECTION ALSO THUS THERE CAN BE NO
C-----INTERSECTION CONFLICT WITH THE IVCONF VEHICLE THUS GO TO 3010 AND
C-----SKIP TO THE NEXT INTERSECTION CONFLICT (THIS ONE IS CLEAR)
                    IF ( NOFC . EQ . 0 )         GO TO 3010
      IF      ( IPRC(1,NOFC) . EQ . JP )         THEN
        JPRC = 1
      ELSE IF ( IPRC(2,NOFC) . EQ . JP )         THEN
        JPRC = 2
      ELSE IF ( IPRC(1,NOFC) . EQ .  0 )         THEN
        JPRC = 1
        NORC(1,NOFC) = NVEP1
      ELSE IF ( IPRC(2,NOFC) . EQ .  0 )         THEN
        JPRC = 2
        NORC(2,NOFC) = NVEP1
      ELSE
        GO TO 9460
      END IF
            IF ( NORC(JPRC,NOFC) . EQ . NVEP1 )  GO TO 3010
C-----SET THE NOFC VEHICLE TO THE NOR VEHICLE FOR THE CURRENT NOFC
C-----VEHICLE
      IF ( NOR(NOFC) . EQ . 0 )                  THEN
        IF ( LPRES(NOFC) . EQ . LOBL(JP) )       THEN
          NOFC = NORC(JPRC,NOFC)
C;        WRITE (TC3,331) TIME,'CHKCON 3 NOFC = NORC(JPRC,NOFC)    ',
C;   *                    IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                    LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                    IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                    IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                    'JM=',INDEX,'N=',NGEOCP(IX),'JPRC=',JPRC
        ELSE
          NOFC = IFVL(JL)
C;        WRITE (TC3,331) TIME,'CHKCON 4 NOFC = IFVL(JL)           ',
C;   *                    IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                    LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                    IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                    IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                    'JL=',JL,'JM=',INDEX,'N=',NGEOCP(IX)
        END IF
      ELSE
        NOFC = NOR(NOFC)
C;      WRITE (TC3,331) TIME,'CHKCON 5 NOFC = NOR(NOFC)          ',
C;   *                  IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                  LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                  IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                  IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                  'JM=',INDEX,'N=',NGEOCP(IX)
      END IF
C-----IF THERE IS A NEW NOFC VEHICLE THEN GO TO 1070 AND CHECK AGAIN
                    IF ( NOFC . GT . 0 )         GO TO 1070
C-----THE OLD NOFC VEHICLE HAD TO BE THE LAST VEHICLE ON THE
C-----INTERSECTION PATH THUS SET THE NOFC VEHICLE TO THE FIRST VEHICLE
C-----ON THE LANE AND GO TO 1070 AND CHECK AGAIN
C[    IF ( JL                 .EQ.-2147483647   )STOP 'CHKCON JL     01'
      NOFC = IFVL(JL)
C;    WRITE (TC3,331) TIME,'CHKCON 6 NOFC = IFVL(JL)           ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JL=',JL,'JM=',INDEX,'N=',NGEOCP(IX)
      GO TO 1070
 1080 CONTINUE
C-----SET THE IVCONF VEHICLES PARAMETERS FOR PREDICTING TIME AND
C-----VELOCITY TO AN INTERSECTION CONFLICT
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'CHKCON IVCONF 02'
      CALL  SPVAS   ( IVCONF,PO,VO,AO,SO,
     *                .FALSE.,.FALSE.,.TRUE.,.FALSE. )
C-----IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVCONF
C-----VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE FALSE
      EVRESP = ( ( IAND( VEHTYP(IV    ),LAVTE ) . EQ . 0 ) . AND .
     *           ( IAND( VEHTYP(IVCONF),LAVTE ) . NE . 0 ) )
C[    IF ( JL                 .EQ.-2147483647   )STOP 'CHKCON JL     02'
      POSSAV = PO
      POP    = PO
      LGEOM4 = LGEOM(4,JL)
      PO     = PO + LGEOM4
      CALL  SETDSP  ( IVCONF,IPOS(IVCONF),DBLE( ISPD(IVCONF) ),.FALSE.,
     *                DESSPD                                           )
      JSPD = IDNINT( DESSPD )
      JSPDP = 1
      KPRTM = 0
C-----IF THE IVCONF VEHICLE IS IN THE INTERSECTION THEN GO TO 2010 AND
C-----CONTINUE ELSE SET SOME ADDITIONAL PARAMETERS
                    IF ( MININT(IVCONF) )        GO TO 2010
      IF ( LPRES(IVCONF) . EQ . LIBL(JP) )       THEN
C-----  IVCONF VEHICLE IS ON THE INBOUND LANE FOR THE INTERSECTION PATH
        POP = POP - LGEOM4
        PO  = PO  - LGEOM4
      ELSE
C-----  IVCONF VEHICLE IS ON THE OUTBOUND LANE FOR THE INTERSECTION PATH
        PAD = DBLE( LENP(JP) - LGEOM(1,LPRES(IVCONF)) )
        POP = POP + PAD
        PO  = PO  + PAD
      END IF
      JSPDP = ISPDP(IVCONF)
                    IF ( JSPD .NE. ISPD(IVCONF) )JSPDP = 1
      KPRTM = IPRTM(IVCONF)
      IF ( IUPDAT(IVCONF) . AND . (KPRTM . GT . 0) )
     *                                           KPRTM = KPRTM + 1
C-----IF THE IVCONF VEHICLE HAS ALREADY SET HIS DESIRED SPEED FOR HIS
C-----INTERSECTION PATH THEN GO TO 2010 ELSE GET ADDITIONAL PARAMETERS
                    IF ( JSPDP . NE . 0 )        GO TO 2010
C[    IF ( JP                 .EQ.-2147483647   )STOP 'CHKCON JP     01'
      MIMP = LIMP(JP)
      JSLIM = ISLIM(ISNA(JL))
 2010 CONTINUE
C-----FIND ADDITIONAL PARAMETERS FOR THE IVCONF VEHICLE
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'CHKCON IVCONF 03'
      JDCONF = IDRICL(IVCONF)
      JVCONF = IVEHCL(IVCONF)
C[    IF ( JH                 .EQ.-2147483647   )STOP 'CHKCON JH     01'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'CHKCON JNDEX  02'
      P = ICOND(JH,JNDEX) + LGEOM4
      DVH = JSPD
      DCH = P - PO
      TCH = 1.0D0
      HWH = 0.5D0*WIDV(IVEHCL(IVCONF))
      LVH =       LVAP(       IVCONF )
C[    IF ( VCM              .EQ.-2147483647.0 )  STOP 'CHKCON VCM    01'
      SAFF = (SAFDIS+(VCM/SAFSPD))/DCHAR(IDRICL(IV))
      SAFR = SAFF
      IF ( EVRESP )                              THEN
        SAFF = DMAX1( SAFF,EVEHFZ*VCM )
        SAFR = DMAX1( SAFR,EVEHRZ*VCM )
      END IF
      POT = PO
      PT  = P
 2030 CONTINUE
C-----IF THE IVCONF VEHICLE IS INVOLVED IN A MAJOR COLLISION OR IT STOPS
C-----BEFORE GETTING TO THE INTERSECTION CONFLICT THEN IT WILL NEVER
C-----MOVE THUS CHECK DISTANCES
      IF ( ( MAJCOL(IVCONF)                   ) . OR .
     *     ( MAJCLB(IVCONF).AND.(VO.EQ.0.0D0) ) . OR .
     *     ( MAJCLL(IVCONF).AND.(VO.EQ.0.0D0) ) . OR .
     *     ( CKINTB(IVCONF).AND.(VO.EQ.0.0D0) ) . OR .
     *     ( TCH.LE.0.0D0                     ) )THEN
C-----  SET TCM AND TCH FOR TEST ON STATEMENT BEFORE 3010
        TCM = 2.0D0
        IF ( TCH . GT . TCM )                    TCH = 1.0D0
C-----  IF THE VEHICLES FRONT BUMPER PLUS EXTRA SAFETY ZONE HAS ARRIVED
C-----  AT THE INTERSECTION CONFLICT AND THE VEHICLES REAR BUMPER PLUS
C-----  EXTRA SAFETY ZONE HAS NOT PASSED THE INTERSECTION CONFLICT THEN
C-----  SET THAT THE VEHICLE MUST CHECK CONFLICTS BUT MAY NOT BE BLOCKED
        IF ( .NOT. MAJCOL(IV) )                  THEN
          IF ( ( MAJCOL(IVCONF)                   ) . OR .
     *         ( MAJCLB(IVCONF).AND.(VO.EQ.0.0D0) ) . OR .
     *         ( MAJCLL(IVCONF).AND.(VO.EQ.0.0D0) ) )
     *                                           THEN
            IF ( ((POT    +SAFCON*SAFF+HWM) . GE . PT) . AND .
     *           ((POT-LVH-SAFCON*SAFR-HWM) . LE . PT) )
     *                                           THEN
              MAJCON(IV) = .TRUE.
              POSCON(IV) = DMIN1( POSCON(IV),
     *                            DBLE( LGEOM4+ICOND(JM,JNDEX) ) )
              RESPEV = ( RESPEV . OR . EVRESP )
            END IF
          END IF
        END IF
C-----  PERFORM CHKCOL CHECKS
C-----  IF THE INTERSECTION CONFLICT IS A MERGE (THE LINKING OUTBOUND
C-----  LANE FOR THE INTERSECTION PATHS ARE EQUAL) THEN CHECK CLEARANCE
C-----  ELSE CHECK DISTANCE TO CONFLICT
        IF ( LOBL(IX) . EQ . LOBL(JP) )          THEN
          CLEARD = HWM + SAFR + HWH
          CALL  CCLEAR  ( IX,JP,CLEARD,.FALSE.,CLEART )
          IF ( (POP+CLEART).LT.DBLE( LENP(JP) ) )GO TO 3010
          IF ( DCM . LE . DCH )                  THEN
            GO TO 3010
          ELSE
            GO TO 2040
          END IF
        ELSE
          DISCLH = 0.0D0
          IF ( DTANAN . LE . 0.5D0 )             THEN
            DISCLH = DISCLH + 2.0D0*HWH
          ELSE
            DISCLH = DISCLH + HWH/DTANAN
          END IF
          IF ( DSINAN . LE . 0.5D0 )             THEN
            DISCLH = DISCLH + 2.0D0*HWM
          ELSE
            DISCLH = DISCLH + (HWM+(SAFDIS/DCHAR(IDRICL(IV))))/DSINAN
          END IF
C-----    IF THE OTHER VEHICLES FRONT BUMPER HAS PASSED THE INTERSECTION
C-----    CONFLICT AND THE OTHER VEHICLES REAR BUMPER HAS NOT PASSED THE
C-----    INTERSECTION CONFLICT THEN THEN GO TO 2040
          IF ( ( (POT     +DISCLH) . GE . PT  ) . AND .
     *         ( (POT -LVH-DISCLH) . LE . PT  ) )GO TO 2040
        END IF
C-----  IF THE VEHICLES FRONT BUMPER HAS NOT ARRIVED AT THE INTERSECTION
C-----  CONFLICT THEN GO TO 3010 AND SKIP TO THE NEXT INTERSECTION
C-----  CONFLICT
            IF ( (POT    +SAFF+HWM) . LT . PT )  GO TO 3010
C-----  IF THE VEHICLES REAR BUMPER HAS PASSED THE INTERSECTION CONFLICT
C-----  THEN GO TO 2120 AND PROCESS THE NORC VEHICLE
            IF ( (POT-LVH-SAFR-HWM) . GT . PT )  GO TO 2120
 2040   CONTINUE
C-----  THE IVCONF VEHICLE BLOCKS THE POINT OF INTERSECTION CONFLICT
C-----  THUS CALCULATE A DECEL TO A STOP AT THE STOP LINE OR JUST BEFORE
C-----  THE POINT OF INTERSECTION CONFLICT
        POSVEH = POSOLD + DCM - 2.0D0*HWM - SAFR
        IF ( TCH . GT . 0.0D0 )                  THEN
          POSVEH = POSVEH - 20.0D0
        END IF
        POSVEH = DMIN1( POSEND+1.5D0,POSVEH )
        VELVEH = 0.0D0
        ACCVEH = 0.0D0
        SLPVEH = 0.0D0
        CALL  SLPCFS  ( SLPTMP,IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                            POSVEH,VELVEH,ACCVEH,SLPVEH  )
        IF ( SLPTMP . NE . 0.0D0 )               THEN
          IF ( SLPTMP . LT . SLPBLK )            THEN
            SLPBLK = SLPTMP
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
        END IF
        IF ( .NOT. MAJCOL(IV) )                  THEN
          IF ( ( MAJCOL(IVCONF)                   ) . OR .
     *         ( MAJCLB(IVCONF).AND.(VO.EQ.0.0D0) ) )
     *                                           THEN
C-----      SET THAT THE VEHICLE IS BLOCKED BY A MAJOR COLLISION
            MAJCLB(IV) = .TRUE.
            POSCLB(IV) = DMIN1( POSCLB(IV),POSVEH )
C-----      SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----      COLLISION MAY BLOCK THIS VEHICLE
            MAJCON(IV) = .TRUE.
            POSCON(IV) = DMIN1( POSCON(IV),POSVEH )
            RESPEV = ( RESPEV . OR . EVRESP )
          ELSE IF ( MAJCLL(IVCONF).AND.(VO.EQ.0.0D0) )
     *                                           THEN
C-----      SET THAT THE VEHICLE IS BLOCKED BY A MAJOR COLLISION
            MAJCLL(IV) = .TRUE.
            POSCLL(IV) = DMIN1( POSCLL(IV),POSVEH )
            RESPEV = ( RESPEV . OR . EVRESP )
          END IF
        END IF
        GO TO 4020
      END IF
C-----IF THE FRONT BUMPER OF THE IVCONF VEHICLE IS WITHIN SAFF+HWM
C-----DISTANCE OF THE POINT OF INTERSECTION CONFLICT AND IS STOPPED THEN
C-----SET TCH TO 0 AND GO TO 2030 AND CHECK BLOCKAGE
      IF ( (POT+SAFF+HWM) . GE . PT )            THEN
        IF ( VO . LE . VELSTP )                  THEN
          TCH = 0.0D0
          GO TO 2030
        END IF
      END IF
C-----IF THE FRONT BUMPER OF THE IVCONF VEHICLE IS BEYOND THE POINT OF
C-----INTERSECTION CONFLICT THEN CALCULATE THE TIME TO STOP AND THE
C-----POSITION OF THE FRONT BUMPER WHEN IT STOPS
      IF ( POT . GE . PT )                       THEN
        TMAX = 30.0D0
        CALL  TIMSTP  ( VO,AO,SO,TMAX,TS )
                    IF ( TS . EQ . TIMERR )      GO TO 2050
        POSSTP = POT + VO*TS + 0.5D0*AO*TS**2 + ONED6*SO*TS**3
C-----  IF THE REAR BUMPER OF THE IVCONF VEHICLE WHEN IT STOPS IS WITHIN
C-----  SAFR+HWM DISTANCE OF THE POINT OF INTERSECTION CONFLICT THEN SET
C-----  TCH TO 0 AND GO TO 2030 AND CHECK BLOCKAGE
        IF ( (POSSTP-LVH-SAFR-HWM) . LE . PT )   THEN
          TCH = 0.0D0
          GO TO 2030
        END IF
      END IF
 2050 CONTINUE
C-----PREDICT TIME AND VELOCITY TO AN INTERSECTION CONFLICT FOR HIM
      CALL  SNOFCV  ( IVCONF,JP )
      CALL  PREDTV  ( TCH,PCH,VCH,ACH,SCH )
C-----IF THE VEHICLE STOPS BEFORE THE INTERSECTION CONFLICT THEN GO TO
C-----2030 AND CHECK DISTANCES (PO IS THE POSITION WHERE THE VEHICLE
C-----STOPPED)
      IF ( TCH . LE . -999.9D0 )                 THEN
        POT = PCH
        GO TO 2030
      END IF
C[    IF ( KPRTM              .EQ.-2147483647   )STOP 'CHKCON KPRTM  01'
C[    IF ( TCH                .EQ.-2147483647.0 )STOP 'CHKCON TCH    01'
      TCH = TCH + KPRTM*DT
C-----FIND THE TIME FOR HIS VEHICLE TO PASS THE INTERSECTION CONFLICT AT
C-----THE VELOCITY AT THE INTERSECTION CONFLICT FOR HIM
      TPASCH = 1.0D+9
      TPASSH = 1.0D+9
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'CHKCON VCH    01'
                    IF ( VCH . LE . 0.0D0 )      GO TO 2060
C-----IF THE INTERSECTION CONFLICT IS NOT A MERGE (THE LINKING OUTBOUND
C-----LANE FOR THE INTERSECTION PATHS ARE DIFFERENT) THEN CALCULATE
C-----DISCLH ELSE DISCLH IS ZERO
      DISCLH = HWH + HWM
      IF ( LOBL(IX) . NE . LOBL(JP) )            THEN
        DISCLH = 0.0D0
        IF ( DTANAN . LE . 0.5D0 )               THEN
          DISCLH = DISCLH + 2.0D0*HWH
        ELSE
          DISCLH = DISCLH + HWH/DTANAN
        END IF
        IF ( DSINAN . LE . 0.5D0 )               THEN
          DISCLH = DISCLH + 2.0D0*HWM
        ELSE
          DISCLH = DISCLH + HWM/DSINAN
        END IF
      END IF
      TPASCH = DISCLH      /VCH
      TPASSH = LVAP(IVCONF)/VCH
 2060 CONTINUE
C-----FIND THE ERROR IN JUDGMENT
C[    IF ( TCH                .EQ.-2147483647.0 )STOP 'CHKCON TCH    02'
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKCON TCM    04'
      ERRJUD = DMAX1( 0.0D0,PIJRIV*(TCH-5.0D0)/7.0D0 )
C-----IF THE IVCONF VEHICLES TIME TO THE INTERSECTION CONFLICT IS GT 5
C-----SECONDS AND HE SHOULD FOLLOW THE VEHICLE AHEAD THEN INCREMENT THE
C-----TIME TO THE INTERSECTION CONFLICT FOR HIM BY 0.5D0 SECONDS
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'CHKCON IVCONF 04'
      IF ( (TCH.GT.5.0D0) . AND . IFVA(IVCONF) ) TCH = TCH + 0.5D0
C-----FIND THE TIME FOR THE FRONT ZONE FOR THE IVCONF VEHICLE
C[    IF ( TPASSM             .EQ.-2147483647.0 )STOP 'CHKCON TPASSM 01'
C[    IF ( TPASCM             .EQ.-2147483647.0 )STOP 'CHKCON TPASCM 01'
      TFZ = TPASSM + TPASCM + TLEAD + PIJRIV + 0.5D0*ERRJUD
C-----FIND THE TIME FOR THE REAR ZONE FOR THE IVCONF VEHICLE
C[    IF ( TPASSH             .EQ.-2147483647.0 )STOP 'CHKCON TPASSH 01'
C[    IF ( TPASCH             .EQ.-2147483647.0 )STOP 'CHKCON TPASCH 01'
      TRZ = TPASSH + TPASCH + TLAG  + PIJRIV + 0.5D0*ERRJUD + TPASCM
C-----IF THE INTERSECTION CONFLICT IS NOT A MERGE (THE LINKING OUTBOUND
C-----LANE FOR THE INTERSECTION PATHS ARE DIFFERENT) THEN GO TO 2080
            IF ( LOBL(IX) . NE . LOBL(JP) )      GO TO 2080
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKCON TCM    03'
                    IF ( TCM . GT . TCH )        GO TO 2070
C-----THIS VEHICLE AND THE IVCONF VEHICLE ARE MERGING AT THE
C-----INTERSECTION CONFLICT INTO THE SAME LANE AND THIS VEHICLE WILL
C-----ARRIVE BEFORE THE IVCONF VEHICLE THUS MAX THE TIME FOR THE FRONT
C-----ZONE FOR THE IVCONF VEHICLE WITH THE TIME REQUIRED FOR THIS
C-----VEHICLE TO PASS THROUGH THE INTERSECTION CONFLICT PLUS THE TIME
C-----REQUIRED FOR THIS VEHICLE TO PASS THE CAR-FOLLOWING DISTANCE PLUS
C-----THE ERROR IN JUDGMENT
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'CHKCON VCH    02'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'CHKCON VCM    02'
      VELREL = VCM - VCH
C-----FIND THE CONSERVATIVE CAR FOLLOWING DISTANCE
      CARDIS = DMAX1( XRELMX,1.7D0*VCM )/DCHAR(IDRICL(IV))
      IF ( VELREL . LT . 0.0D0 )                 THEN
C-----  VCM LT VCH
C-----  FIND THE TIME FOR HIM TO SLOW DOWN TO ME AT -CRISLP
C-----  VCM = VCH + ACH*T + 0.5D0*(-CRISLP)*T**2
C-----  (-0.5D0*CRISLP)*T**2 + (ACH)*T + (VCH-VCM) = 0
        A = -0.5D0*CRISLP
        B = ACH
        C = VCH-VCM
        TMAX = 30.0D0
        CALL  TMQUAD  ( A,B,C,TMAX,T )
        IF ( T . EQ . TIMERR )                   THEN
          IF ( C . LE . VSMALL )                 T = 0.0D0
        END IF
        IF ( T . EQ . TIMERR )                   THEN
          CARDIS = CARDIS + 4.0D0*VELREL**2/DCHAR(IDRICL(IV))
        ELSE
          DSLOWH = VCH*T + 0.5D0*ACH*T**2 - ONED6*CRISLP*T**3
          DSLOWM = VCM*T + 0.5D0*ACM*T**2 + ONED6*SCM   *T**3
          CARDIS = CARDIS + DMAX1( DSLOWH-DSLOWM,0.0D0 )
        END IF
      END IF
      IF ( VCM . LE . 0.0D0 )                    THEN
        TPASSC = 1.0D+9
      ELSE
        TPASSC = CARDIS/VCM
      END IF
C[    IF ( TPASSM             .EQ.-2147483647.0 )STOP 'CHKCON TPASSM 02'
C[    IF ( TPASCM             .EQ.-2147483647.0 )STOP 'CHKCON TPASCM 02'
      TMP = TPASSM + TPASCM + TPASSC + 0.5D0*ERRJUD
C[    IF ( TFZ                .EQ.-2147483647.0 )STOP 'CHKCON TFZ    01'
      TFZ = DMAX1( TFZ,TMP )
 2070 CONTINUE
C[    IF ( TCH                .EQ.-2147483647.0 )STOP 'CHKCON TCH    03'
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKCON TCM    05'
                    IF ( TCH . GT . TCM )        GO TO 2080
C-----THIS VEHICLE AND THE IVCONF VEHICLE ARE MERGING AT THE
C-----INTERSECTION CONFLICT INTO THE SAME LANE AND THE IVCONF VEHICLE
C-----WILL ARRIVE BEFORE THIS VEHICLE THUS MAX THE TIME FOR THE REAR
C-----ZONE FOR THE IVCONF VEHICLE WITH THE TIME REQUIRED FOR THE IVCONF
C-----VEHICLE TO PASS THROUGH THE INTERSECTION CONFLICT PLUS THE TIME
C-----REQUIRED FOR THE IVCONF VEHICLE TO PASS THE CAR-FOLLOWING
C-----DISTANCE PLUS THE ERROR IN JUDGMENT
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'CHKCON VCH    03'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'CHKCON VCM    03'
      VELREL = VCH - VCM
C-----FIND THE CONSERVATIVE CAR FOLLOWING DISTANCE
      CARDIS = DMAX1( XRELMX,1.7D0*VCH )/DCHAR(IDRICL(IV))
      IF ( VELREL . LT . 0.0D0 )                 THEN
C-----  VCH LT VCM
C-----  FIND THE TIME FOR ME TO SLOW DOWN TO HIM AT -CRISLP
C-----  VCH = VCM + ACM*T + 0.5D0*(-CRISLP)*T**2
C-----  (-0.5D0*CRISLP)*T**2 + (ACM)*T + (VCM-VCH) = 0
        A = -0.5D0*CRISLP
        B = ACM
        C = VCM-VCH
        TMAX = 30.0D0
        CALL  TMQUAD  ( A,B,C,TMAX,T )
        IF ( T . EQ . TIMERR )                   THEN
          IF ( C . LE . VSMALL )                 T = 0.0D0
        END IF
        IF ( T . EQ . TIMERR )                   THEN
          CARDIS = CARDIS + 4.0D0*VELREL**2/DCHAR(IDRICL(IV))
        ELSE
          DSLOWM = VCM*T + 0.5D0*ACM*T**2 - ONED6*CRISLP*T**3
          DSLOWH = VCH*T + 0.5D0*ACH*T**2 + ONED6*SCH   *T**3
          CARDIS = CARDIS + DMAX1( DSLOWM-DSLOWH,0.0D0 )
        END IF
      END IF
      IF ( VCH . LE . 0.0D0 )                    THEN
        TPASSC = 1.0D+9
      ELSE
        TPASSC = CARDIS/VCH
      END IF
C[    IF ( TPASSH             .EQ.-2147483647.0 )STOP 'CHKCON TPASSH 02'
C[    IF ( TPASCH             .EQ.-2147483647.0 )STOP 'CHKCON TPASCH 02'
C[    IF ( TPASCM             .EQ.-2147483647.0 )STOP 'CHKCON TPASCM 02'
      TMP = TPASSH + TPASCH + TPASSC + 0.5D0*ERRJUD + TPASCM
C[    IF ( TRZ                .EQ.-2147483647.0 )STOP 'CHKCON TRZ    01'
      TRZ = DMAX1( TRZ,TMP )
 2080 CONTINUE
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'CHKCON VCH    04'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'CHKCON VCM    04'
                    IF ( VCM - VCH )             2090 , 2110 , 2100
 2090 CONTINUE
C-----THIS VEHICLE WILL BE TRAVELING SLOWER THAN THE IVCONF VEHICLE AT
C-----THE INTERSECTION CONFLICT THUS MAX THE TIME FOR THE FRONT ZONE FOR
C-----THE IVCONF VEHICLE WITH THE TIME REQUIRED FOR THE IVCONF VEHICLE
C-----TO REDUCE ITS SPEED TO MY SPEED PLUS THIS DRIVERS REACTION TIME
C-----MULTIPLIED BY THE COSINE OF THE INTERSECTION CONFLICT ANGLE RAISED
C-----TO THE 4TH POWER
C-----AFACT(00)=1.00 (10)=0.94 (20)=0.78 (30)=0.56 (40)=0.34 (50)=0.17
      SLOPE = -0.75D0*SLPMAX*DCHAR(JDCONF)
C[    IF ( ACH                .EQ.-2147483647.0 )STOP 'CHKCON ACH    01'
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'CHKCON VCH    05'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'CHKCON VCM    05'
C-----VCM = VCH + ACH*TCRASH + 0.5*SLOPE*TCRASH**2
C-----0.5*SLOPE*TCRASH**2 + ACH*TCRASH + (VCH-VCM) = 0
      A = 0.5D0*SLOPE
      B = ACH
      C = VCH - VCM
      TMAX  = 30.0D0
      CALL  TMQUAD  ( A,B,C,TMAX,TCRASH )
                    IF ( TCRASH . EQ . TIMERR )  GO TO 2110
      AFACT = DCOSAN**4
      TMP = AFACT*(TCRASH+PIJRIV)
C[    IF ( TFZ                .EQ.-2147483647.0 )STOP 'CHKCON TFZ    02'
      TFZ = DMAX1( TFZ,TMP )
      GO TO 2110
 2100 CONTINUE
C-----THIS VEHICLE WILL BE TRAVELING FASTER THAN THE IVCONF VEHICLE AT
C-----THE INTERSECTION CONFLICT THUS MAX THE TIME FOR THE REAR ZONE FOR
C-----THE IVCONF VEHICLE WITH THE TIME REQUIRED FOR THIS VEHICLE TO
C-----REDUCE ITS SPEED TO THE IVCONF VEHICLE SPEED PLUS THIS DRIVERS
C-----REACTION TIME MULTIPLIED BY THE COSINE OF THE INTERSECTION
C-----CONFLICT ANGLE RAISED TO THE 4TH POWER
C-----AFACT(00)=1.00 (10)=0.94 (20)=0.78 (30)=0.56 (40)=0.34 (50)=0.17
      SLOPE = -0.75D0*SLPMAX*DCHAR(IDRICL(IV))
C[    IF ( ACM                .EQ.-2147483647.0 )STOP 'CHKCON ACM    01'
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'CHKCON VCH    06'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'CHKCON VCM    06'
C-----VCH = VCM + ACM*TCRASH + 0.5*SLOPE*TCRASH**2
C-----0.5*SLOPE*TCRASH**2 + ACM*TCRASH + (VCM-VCH) = 0
      A = 0.5D0*SLOPE
      B = ACM
      C = VCM - VCH
      TMAX  = 30.0D0
      CALL  TMQUAD  ( A,B,C,TMAX,TCRASH )
                    IF ( TCRASH . EQ . TIMERR )  GO TO 2110
      AFACT = DCOSAN**4
      TMP = AFACT*(TCRASH+PIJRIV)
C[    IF ( TRZ                .EQ.-2147483647.0 )STOP 'CHKCON TRZ    02'
      TRZ = DMAX1( TRZ,TMP )
 2110 CONTINUE
C-----IF THE IVCONF VEHICLE IS DECELERATING AT THE POINT OF CONFLICT
C-----THEN INCREASE TRZ
      IF ( ACH . LT . 0.0D0 )                    THEN
        TRZ = TRZ - 0.1D0*ACH
      END IF
      IF ( EVRESP )                              THEN
        TFZ = DMAX1( TFZ,EVEHFZ )
        TRZ = DMAX1( TRZ,EVEHRZ )
      END IF
C-----FIND THE TIME THE FRONT ZONE AND REAR ZONE SHOULD ARRIVE AT THE
C-----INTERSECTION CONFLICT
C[    IF ( TCH                .EQ.-2147483647.0 )STOP 'CHKCON TCH    04'
C[    IF ( TFZ                .EQ.-2147483647.0 )STOP 'CHKCON TFZ    03'
C[    IF ( TRZ                .EQ.-2147483647.0 )STOP 'CHKCON TRZ    03'
      TFZ = TCH - TFZ
      TRZ = TCH + TRZ
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 102
C4                  IF ( TIME . LT . TPRINT )    GO TO 102
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'CHKCON IVCONF 05'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'CHKCON JNDEX  06'
C[    IF ( DCH                .EQ.-2147483647.0 )STOP 'CHKCON DCH    01'
C[    IF ( DCM                .EQ.-2147483647.0 )STOP 'CHKCON DCM    01'
C[    IF ( DVH                .EQ.-2147483647.0 )STOP 'CHKCON DVH    01'
C[    IF ( DVM                .EQ.-2147483647.0 )STOP 'CHKCON DVM    01'
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKCON TCM    06'
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'CHKCON VCH    07'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'CHKCON VCM    07'
C4    WRITE (6,702) JNDEX,IV,TCM,VCM,DVM,DCM,IVCONF,TFZ,TCH,TRZ,VCH,DVH,
C4   *              DCH
C4102 CONTINUE
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'CHKCON IVCONF 06'
C[    IF ( JNDEX              .EQ.-2147483647   )STOP 'CHKCON JNDEX  07'
C[    IF ( ACH                .EQ.-2147483647.0 )STOP 'CHKCON ACH    02'
C[    IF ( ACM                .EQ.-2147483647.0 )STOP 'CHKCON ACM    02'
C[    IF ( DCH                .EQ.-2147483647.0 )STOP 'CHKCON DCH    02'
C[    IF ( DCM                .EQ.-2147483647.0 )STOP 'CHKCON DCM    02'
C[    IF ( DVH                .EQ.-2147483647.0 )STOP 'CHKCON DVH    02'
C[    IF ( DVM                .EQ.-2147483647.0 )STOP 'CHKCON DVM    02'
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKCON TCM    07'
C[    IF ( VCH                .EQ.-2147483647.0 )STOP 'CHKCON VCH    08'
C[    IF ( VCM                .EQ.-2147483647.0 )STOP 'CHKCON VCM    08'
C     IF ( LCONTV(IV) . GE . LCSIGX )          THEN
C       JSISET = ISISET(ICAMPC,IBLN(IL))
C     ELSE
C       JSISET = 0
C     END IF
C     FCLEAR = TFZ - TCM
C     RCLEAR = TCM - TRZ
C     WRITE (TC3,601) IQ(IV),TIME,'CHKCON',IA,ILN,0,ITURN(IV),ICONTR,
C    *                 LCONTV(IV),JSISET,JNDEX,TCM,VCM,ACM,DVM,DCM,
C    *                 IQ(IVCONF),TFZ,TCH,TRZ,VCH,ACH,DVH,DCH,FCLEAR,
C    *                 RCLEAR
C-----IF THE IVCONF VEHICLE IS FORCED TO GO OR THE IVCONF VEHICLE IS
C-----FORCED TO RUN THE RED SIGNAL THEN IGNORE THE IVCONF VEHICLE
      IF ( VMSASM(IVCONF) . GT . 0 )             THEN
        LVMSGO = ( IVMSMG(VMSASM(IVCONF)) . EQ . VMSMGO )
        LVMSRR = ( IVMSMG(VMSASM(IVCONF)) . EQ . VMSMRR )
      ELSE
        LVMSGO = .FALSE.
        LVMSRR = .FALSE.
      END IF
      IGNORE = ( ( MFGOF(IVCONF)                                ) .OR.
     *           ( ( FRRTIM(IVCONF) . GT . 0.0D0              ) .AND.
     *             ( TIME .GE.  FRRTIM(IVCONF)                ) .AND.
     *             ( TIME .LE. (FRRTIM(IVCONF)+FRRATM(IVCONF))) ) .OR.
     *           ( LVMSGO                                       ) .OR.
     *           ( LVMSRR                                       ) )
C-----IF THE IVCONF VEHICLE SHOULD NOT BE IGNORED AND THE TIME TO THE
C-----INTERSECTION CONFLICT FOR ME FALLS BETWEEN THE TIME THE FRONT ZONE
C-----OF THE IVCONF VEHICLE SHOULD ARRIVE AT THE INTERSECTION CONFLICT
C-----AND THE TIME THE REAR ZONE OF THE IVCONF VEHICLE SHOULD ARRIVE AT
C-----THE INTERSECTION CONFLICT THEN GO TO 4020 AND RETURN (THERE IS AN
C-----INTERSECTION CONFLICT)
      IF ( (.NOT. IGNORE                ) .AND. 
     *     ((TCM-TFZ)*(TCM-TRZ).LE.0.0D0) )      THEN
        IF ( MININT(IVCONF) )                    THEN
C-----    THE IVCONF VEHICLE IS IN THE INTERSECTION THUS IF IT'S
C-----    POSITION IS GREATER THAN 4 FEET INTO THE INTERSECTION AND
C-----    STOPPED THEN GO TO 2030 AND CHECK FOR BLOCKAGE
          IF ( ( IPOS  (IVCONF) .GT. 4.0D0  ) . AND .
     *         ( IVEL  (IVCONF) .LE. VELSTP ) )  THEN
            TCH = 0.0D0
C-----      IF THE POINT OF INTERSECTION CONFLICT IS A MERGE (THE
C-----      LINKING OUTBOUND LANE FOR THE INTERSECTION PATHS ARE THE
C-----      SAME) THEN RESET POT AND PT FOR A MERGE
            IF ( LOBL(IX) . EQ . LOBL(JP) )      THEN
              POT = POSSAV
              PT  = ICOND(JH,JNDEX)
            END IF
            GO TO 2030
          END IF
        ELSE
C-----    THE IVCONF VEHICLE IS ON AN INBOUND LANE
C-----    IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE IVCONF
C-----    VEHICLE IS AN EMERGENCY VEHICLE THEN GO TO 4020 AND RETURN
C-----    (THERE IS AN INTERSECTION CONFLICT)
          IF ( EVRESP )                          THEN
            RESPEV = ( RESPEV . OR . EVRESP )
            GO TO 4020
          END IF
C-----    IF THE IVCONF VEHICLE IS STOPPED AT THE STOP LINE AND MY LANE
C-----    LANE CONTROL IS NOT SIGNAL CONTROLLED AND MY LANE CONTROL IS
C-----    LESS THAN HIS OR HE IS ON THE LIST OF VEHICLE STOPPED AT THE
C-----    STOP LINE THEN GO TO 2115 AND PROCESS THE NEXT VEHICLE
          IF ( ( MATSTL(IVCONF)           ) . AND .
     *         ( LCONTV(IV) . LT . LCSIGX ) )    THEN
            IF ( LCONTV(IV) . LT . GETLCV ( IVCONF,LPRES(IVCONF) ) )
     *                                           THEN
              GO TO 2115
            END IF
            DO 2112  IVATIN = 1 , NVATIN
            KV = LVATIN(IVATIN)
                    IF ( IV     . EQ . KV )      GO TO 2115
                    IF ( IVCONF . EQ . KV )      GO TO 2114
            IF ( DIAMON )                        THEN
C-----        SKIP IF VEHICLES ARE ON OPPOSITE SIDES OF THE DIAMOND
              IF ( ( IA              . LE . 4 ) . AND .
     *             ( ISNA(LPRES(KV)) . GE . 5 ) )
     *                                           THEN
                GO TO 2112
              END IF
              IF ( ( IA              . GE . 5 ) . AND .
     *             ( ISNA(LPRES(KV)) . LE . 4 ) )
     *                                           THEN
                GO TO 2112
              END IF
            END IF
            IF ( MAJCLB(KV) . OR . MAJCLL(KV) )  GO TO 2115
 2112       CONTINUE
 2114       CONTINUE
          END IF
        END IF
        RESPEV = ( RESPEV . OR . EVRESP )
        GO TO 4020
      END IF
 2115 CONTINUE
      IF ( ( MININT(IVCONF)               ) . AND .
     *     ( IPOS  (IVCONF) . GE . 4.0D0  ) . AND .
     *     ( IVEL  (IVCONF) . LE . VELSTP ) )    THEN
        TCH = 0.0D0
C-----  IF THE POINT OF INTERSECTION CONFLICT IS A MERGE (THE LINKING
C-----  OUTBOUND LANE FOR THE INTERSECTION PATHS ARE THE SAME) THEN
C-----  RESET POT AND PT FOR A MERGE
        IF ( LOBL(IX) . EQ . LOBL(JP) )          THEN
          POT = POSSAV
          PT  = ICOND(JH,JNDEX)
        END IF
        GO TO 2030
      END IF
 2120 CONTINUE
C-----SET THE NOFC VEHICLE TO THE IVCONF VEHICLE AND SET THE IVCONF
C-----VEHICLE TO THE NEXT VEHICLE THAT SHOULD HAVE TO CLEAR THE SAME
C-----INTERSECTION CONFLICT
C[    IF ( IVCONF             .EQ.-2147483647   )STOP 'CHKCON IVCONF 07'
      NOFC = IVCONF
C;    WRITE (TC3,331) TIME,'CHKCON 7 NOFC = IVCONF             ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JM=',INDEX,'N=',NGEOCP(IX)
      IF      ( IPRC(1,NOFC) . EQ . JP )         THEN
        JPRC = 1
      ELSE IF ( IPRC(2,NOFC) . EQ . JP )         THEN
        JPRC = 2
      ELSE
        GO TO 9460
      END IF
      IVCONF = NORC(JPRC,NOFC)
C-----IF THERE IS ANOTHER VEHICLE THAT HAS TO CLEAR THE SAME
C-----INTERSECTION CONFLICT AND THIS VEHICLE HAS TO GO BEHIND THE LAST
C-----IVCONF VEHICLE THEN GO TO 1070 AND CHECK THE NEW IVCONF VEHICLE
            IF ( (IVCONF.GT.0).AND.(TCM.GT.TFZ) )GO TO 1070
C-----END OF GEOMETRIC CONFLICTING PATH LOOP
 3010 CONTINUE
 3020 CONTINUE
C-----THIS VEHICLE MAY PROCEED INTO THE INTERSECTION THUS SET THE FLAGS
      MCHKCF(IV) = .FALSE.
      MPRO  (IV) = .TRUE.
      MSFLG (IV) = .FALSE.
      MTCARS(IV) = .FALSE.
      IPRTM (IV) = 0
      JPRTM = 0
C-----SET CONFLICTS FOR THE VEHICLE FOR HIS INTERSECTION PATH
      CALL  SETCON
C-----COMPUTE NEW ACC/DEC LOGIC
      CALL  LOGIC   ( 6,IV )
      RETURN
 4010 CONTINUE
C-----THE TIME TO THE END OF THE LANE IS GT THE MAXIMUM TIME FROM THE
C-----END OF THE LANE THAT THIS VEHICLE MAY DECIDE TO PROCEED IF THE
C-----INTERSECTION CONFLICTS ARE CLEAR THUS SET THE WAKE UP TIME
C[    IF ( TCM                .EQ.-2147483647.0 )STOP 'CHKCON TCM    08'
C[    IF ( TEL                .EQ.-2147483647.0 )STOP 'CHKCON TEL    02'
      LOGTMP = MAX0( MIN0( IDINT(2.0D0+5.0D0/DT)         ,
     *                     MPRTM                         ,
     *                     IDINT(2.0D0+(TCM-TEL-DTMAX)/DT) ),2 )
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
 4020 CONTINUE
C-----THE TIME TO THE INTERSECTION CONFLICT FOR ME FALLS BETWEEN THE
C-----TIME THE FRONT ZONE OF THE IVCONF VEHICLE SHOULD ARRIVE AT THE
C-----INTERSECTION CONFLICT AND THE TIME THE REAR ZONE OF THE IVCONF
C-----VEHICLE SHOULD ARRIVE AT THE INTERSECTION CONFLICT
C-----IF SLPBLK, SLPCON, SLPLCH, AND SLPNOF ARE NOT SET THEN RETURN
      IF ( ( SLPBLK . EQ . 0.0D0 ) . AND .
     *     ( SLPCON . EQ . 0.0D0 ) . AND .
     *     ( SLPLCH . EQ . 0.0D0 ) . AND .
     *     ( SLPNOF . EQ . 0.0D0 ) )             RETURN
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER DT SECONDS
      CALL  ACDCP   ( .TRUE. )
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9130 CONTINUE
      CALL  ABORTR  ( 'STOP 913 - INFINITE LOOP - CHKCON' )
      STOP  913
 9460 CONTINUE
      CALL  ABORTR  ( 'STOP 946 - INVALID IPRC/NORC - CHKCON' )
      STOP  946
      END                                                               CHKCON
C
C
C
      SUBROUTINE SETCON
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'CONCHK'
      INCLUDE 'CONFLT'
      INCLUDE 'INDEX'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INTEGER           I,INOW,JH,JM,JCONI,JGEOCP,JP,JPRC,KOUNT,NOFC
      DOUBLE PRECISION  IPOSCK,JPOS,POSLAT
C4701 FORMAT(30H SETTING CONFLICTS FOR VEHICLE,I4,9H FOR PATH,I4)
C
C-----SUBROUTINE SETCON SETS CONFLICTS FOR THE VEHICLE FOR HIS
C-----INTERSECTION PATH
C
C[    I          = -2147483647
C[    INOW       = -2147483647
C[    IPOSCK     = -2147483647.0
C[    JH         = -2147483647
C[    JM         = -2147483647
C[    JCONI      = -2147483647
C[    JGEOCP     = -2147483647
C[    JP         = -2147483647
C[    JPOS       = -2147483647.0
C[    NOFC       = -2147483647
C[    POSLAT     = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'SETCON'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IX = LNEXT(IV)
C-----SET THE INTERSECTION CONTROL LOGIC TIMER FOR NEVER PROCESS AGAIN
      LOGTMP = 0
C-----IF THE VEHICLE HAS ALREADY SET CONFLICTS THEN RETURN
      IF      ( IPRC(1,IV) . EQ . IX )           THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IV) . EQ . IX )           THEN
        JPRC = 2
      ELSE IF ( IPRC(1,IV) . EQ .  0 )           THEN
        JPRC = 1
        NORC(1,IV) = NVEP1
      ELSE IF ( IPRC(2,IV) . EQ .  0 )           THEN
        JPRC = 2
        NORC(2,IV) = NVEP1
      ELSE
        GO TO 9460
      END IF
                    IF ( NORC(JPRC,IV).NE.NVEP1 )RETURN
                    IF ( IX . EQ . 0 )           GO TO 9140
C-----SET THE POSITION FOR CHECKING TO THE NEW POSITION
      IPOSCK = POSNEW
                    IF ( ISET(IV) . NE . 1 )     GO TO 1010
      POSLAT = LATPOS(IV)
                    IF ( POSLAT . GE . 0.0D0 )   GO TO 1010
      IF ( DABS( DABS( POSLAT )-(0.5D0*LEGAL(IV)) ) . GT . 0.01D0 )
     *                                           GO TO 1010
C-----THE VEHICLE JUST STARTED LANE CHANGING TO THE RIGHT THUS SET THE
C-----POSITION FOR CHECKING TO THE OLD POSITION
      IPOSCK = IPOS(IV)
 1010 CONTINUE
C-----INITIALIZE THE NEAREST VEHICLE TO THE REAR FOR CONFLICT CHECKING
      IF      ( IPRC(1,IV) . EQ . IX )           THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IV) . EQ . IX )           THEN
        JPRC = 2
      ELSE IF ( IPRC(1,IV) . EQ .  0 )           THEN
        JPRC = 1
        IPRC(1,IV) = IX
      ELSE IF ( IPRC(2,IV) . EQ .  0 )           THEN
        JPRC = 2
        IPRC(2,IV) = IX
      ELSE
        GO TO 9460
      END IF
      NORC(JPRC,IV) = 0
C;    WRITE (TC3,331) TIME,'SETCON 1 NORC(IV) = 0              ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JPRC=',JPRC
C;331 FORMAT(F6.1,1X,A36,I2,I3,I4,I6,1X,L1,F5.1,3I3,I6,2(I4,I6),
C;   *       5(1X,A,I4))
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 101
C4                  IF ( TIME . LT . TPRINT )    GO TO 101
C4    WRITE (6,701) IV,IX
C4101 CONTINUE
                    IF ( NOR(IV) . EQ . 0 )      GO TO 1030
C-----WAKE UP THE NORT VEHICLE FOR INTERSECTION CONTROL LOGIC
      LOGFLG(NOR(IV)) = MIN0( LOGFLG(NOR(IV)),2 )
 1030 CONTINUE
C-----IF THERE ARE NO GEOMETRIC CONFLICTING PATHS THEN RETURN
                    IF ( NGEOCP(IX) . LE . 0 )   RETURN
C-----PROCESS EACH GEOMETRIC CONFLICTING PATH
      DO 1090  I = 1 , NGEOCP(IX)
C-----INITIALIZE SOME PARAMETERS FOR THIS LOOP
      KOUNT = 0
      JGEOCP = IGEOCP(I,IX)
      IF ( IX . EQ . ICONP(1,JGEOCP) )           THEN
        JH = 2
        JM = 1
      ELSE
        JH = 1
        JM = 2
      END IF
      JP = ICONP(JH,JGEOCP)
      JCONI = ICONI(JH,JGEOCP)
C-----IF THE OTHER INTERSECTION PATH INVOLVED IN THIS INTERSECTION
C-----CONFLICT ALREADY HAS THE INTERSECTION CONFLICT SET THEN GO TO 1050
C-----AND CHECK WHERE THIS VEHICLE FITS IN
            IF ( ICPSET(JCONI,JP) . EQ . 1 )      GO TO 1050
C-----SET THIS VEHICLE AS THE NEXT VEHICLE THAT HAS NOT CLEARED THE
C-----INTERSECTION CONFLICT
      ICONV(JM,JGEOCP) = IV
C;    WRITE (TC3,331) TIME,'SETCON 2 ICONV(JM,JGEOCP) = IV      ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JM=',JM,'JGEOCP=',JGEOCP,'I=',I,'N=',NGEOCP(IX)
C-----INCREMENT THE NUMBER OF CONFLICTS SET FOR THE OTHER INTERSECTION
C-----PATH INVOLVED IN THE INTERSECTION CONFLICT
      NCPSET(JP) = NCPSET(JP) + 1
C-----SET THE CONFLICT FOR THE OTHER INTERSECTION PATH INVOLVED IN THE
C-----INTERSECTION CONFLICT
      ICPSET(JCONI,JP) = 1
C-----SKIP TO THE NEXT GEOMETRIC CONFLICT FOR THIS INTERSECTION PATH
      GO TO 1090
 1050 CONTINUE
C-----THE OTHER INTERSECTION PATH INVOLVED IN THIS INTERSECTION
C-----CONFLICT ALREADY HAS THE INTERSECTION CONFLICT SET THUS CHECK
C-----WHERE THIS VEHICLE FITS IN THUS SET THE NOFC AND INOW VEHICLE TO
C-----THE NEXT VEHICLE THAT HAS NOT CLEARED THE CONFLICT
C[    IF ( JM                 .EQ.-2147483647   )STOP 'SETCON JM     01'
C[    IF ( JGEOCP             .EQ.-2147483647   )STOP 'SETCON JGEOCP 01'
      NOFC = ICONV(JM,JGEOCP)
C;    WRITE (TC3,331) TIME,'SETCON 3 NOFC = ICONV(JH,JGEOCP)    ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JH=',JH,'JGEOCP=',JGEOCP,'I=',I,'N=',NGEOCP(IX)
      INOW = NOFC
 1060 CONTINUE
C-----FIND SOME ATTRIBUTES OF THE INOW VEHICLE
      KOUNT = KOUNT + 1
                    IF ( KOUNT . GT . 50 )       GO TO 9330
C[    IF ( INOW               .EQ.-2147483647   )STOP 'SETCON INOW   01'
      LGEOM4 = LGEOM(4,IL)
      JPOS = IPOS(INOW) + LGEOM4
      IF ( (.NOT. MININT(INOW)) )                THEN
        IF ( LPRES(INOW) . EQ . IL )             THEN
C-----    INOW VEHICLE IS ON THE INBOUND LANE FOR THE INTERSECTION PATH
          JPOS = JPOS - LGEOM4
        ELSE
C-----    INOW VEHICLE IS ON THE OUTBOUND LANE FOR THE INTERSECTION PATH
          JPOS = JPOS + LENP(IX)
        END IF
      END IF
C-----IF THERE IS NO VEHICLE TO THE REAR OF THE INOW VEHICLE THAT HAS
C-----TO CLEAR THE SAME CONFLICT THEN GO TO 1070 AND CHECK SETTING NORC
      IF      ( IPRC(1,INOW) . EQ . IX )         THEN
        JPRC = 1
      ELSE IF ( IPRC(2,INOW) . EQ . IX )         THEN
        JPRC = 2
      ELSE
        GO TO 9460
      END IF
                    IF ( NORC(JPRC,INOW).EQ.0 )  GO TO 1070
C-----IF THE INOW VEHICLE IS NOT IN THE INTERSECTION AND THIS VEHICLE IS
C-----FURTHER DOWN THE LANE THAN THE INOW VEHICLE THEN GO TO 1080 AND
C-----SET THIS VEHICLE BETWEEN THE NOFC VEHICLE TO THE FRONT AND THE
C-----INOW VEHICLE TO THE REAR
C[    IF ( IPOSCK             .EQ.-2147483647.0 )STOP 'SETCON IPOSCK 01'
      IF ( ( (.NOT. MININT(INOW))    ) . AND .
     *     ( IPOSCK      . GT . JPOS ) )         GO TO 1080
C-----SET THE NOFC VEHICLE TO THE INOW VEHICLE AND SET THE INOW VEHICLE
C-----TO THE NEXT VEHICLE TO THE REAR THAT HAS TO CLEAR THE SAME
C-----INTERSECTION CONFLICT AND CHECK AGAIN
      NOFC = INOW
C;    WRITE (TC3,331) TIME,'SETCON 4 NOFC = INOW               ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'I=',I,'N=',NGEOCP(IX)
      INOW = NORC(JPRC,INOW)
      GO TO 1060
 1070 CONTINUE
C-----THERE IS NO VEHICLE TO THE REAR OF THE INOW VEHICLE THAT HAS TO
C-----TO CLEAR THE SAME CONFLICT THUS IF THE INOW VEHICLE IS IN THE
C-----INTERSECTION OR THIS VEHICLE IS BEHIND THE INOW VEHICLE ON THE
C-----LANE THEN GO TO 2020 AND SET THIS VEHICLE AS THE NORC VEHICLE FOR
C-----THE INOW VEHICLE AND RETURN (THIS VEHICLES NORC IS 0)
C[    IF ( INOW               .EQ.-2147483647   )STOP 'SETCON INOW   02'
                    IF ( MININT(INOW) )          GO TO 2020
C[    IF ( IPOSCK             .EQ.-2147483647.0 )STOP 'SETCON IPOSCK 02'
C[    IF ( JPOS               .EQ.-2147483647.0 )STOP 'SETCON JPOS   01'
                    IF ( IPOSCK . LT . JPOS )    GO TO 2020
 1080 CONTINUE
C-----THE INOW VEHICLE IS NOT IN THE INTERSECTION AND THIS VEHICLE IS
C-----FURTHER DOWN THE LANE THAN THE INOW VEHICLE THUS THIS VEHICLE
C-----SHOULD FIT BETWEEN THE NOFC VEHICLE TO THE FRONT AND THE INOW
C-----VEHICLE TO THE REAR THUS SET THIS VEHICLES NORC TO THE INOW
C-----VEHICLE
C[    IF ( INOW               .EQ.-2147483647   )STOP 'SETCON INOW   03'
      IF      ( IPRC(1,IV) . EQ . IX )           THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IV) . EQ . IX )           THEN
        JPRC = 2
      ELSE IF ( IPRC(1,IV) . EQ .  0 )           THEN
        JPRC = 1
        IPRC(1,IV) = IX
      ELSE IF ( IPRC(2,IV) . EQ .  0 )           THEN
        JPRC = 2
        IPRC(2,IV) = IX
      ELSE
        GO TO 9460
      END IF
      NORC(JPRC,IV) = INOW
C;    WRITE (TC3,331) TIME,'SETCON 5 NORC(IV) = INOW           ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'I=',I,'N=',NGEOCP(IX),'JPRC=',JPRC
C-----IF THE INOW VEHICLE IS NOT THE NEXT VEHICLE THAT HAS NOT CLEARED
C-----THE INTERSECTION CONFLICT THEN GO TO 2010 AND SET THE NORC OF THE
C-----NOFC VEHICLE TO THIS VEHICLE AND RETURN ELSE SET THIS VEHICLE AS
C-----THE NEXT VEHICLE THAT HAS NOT CLEARED THE INTERSECTION CONFLICT
C-----AND CHECK THE NEXT GEOMETRIC CONFLICTING PATH
C[    IF ( JM                 .EQ.-2147483647   )STOP 'SETCON JM     02'
C[    IF ( JGEOCP             .EQ.-2147483647   )STOP 'SETCON JGEOCP 02'
            IF ( INOW . NE . ICONV(JM,JGEOCP) )  GO TO 2010
      ICONV(JM,JGEOCP) = IV
C;    WRITE (TC3,331) TIME,'SETCON 6 ICONV(JM,JGEOCP) = IV      ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JM=',JM,'JGEOCP=',JGEOCP,'I=',I,'N=',NGEOCP(IX)
C-----END OF GEOMETRIC CONFLICTING PATH LOOP
 1090 CONTINUE
      RETURN
 2010 CONTINUE
C-----THE INOW VEHICLE IS NOT THE NEXT VEHICLE THAT HAS NOT CLEARED THE
C-----INTERSECTION CONFLICT THUS SET THE NORC OF THE NOFC VEHICLE TO
C-----THIS VEHICLE AND RETURN
C[    IF ( NOFC               .EQ.-2147483647   )STOP 'SETCON NOFC   01'
      IF      ( IPRC(1,NOFC) . EQ . IX )         THEN
        JPRC = 1
      ELSE IF ( IPRC(2,NOFC) . EQ . IX )         THEN
        JPRC = 2
      ELSE IF ( IPRC(1,NOFC) . EQ .  0 )         THEN
        JPRC = 1
        IPRC(1,NOFC) = IX
      ELSE IF ( IPRC(2,NOFC) . EQ .  0 )         THEN
        JPRC = 2
        IPRC(2,NOFC) = IX
      ELSE
        GO TO 9460
      END IF
      NORC(JPRC,NOFC) = IV
C;    WRITE (TC3,331) TIME,'SETCON 7 NORC(NOFC) = IV           ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'I=',I,'N=',NGEOCP(IX),'JPRC=',JPRC
      RETURN
 2020 CONTINUE
C-----THERE IS NO VEHICLE TO THE REAR OF THE INOW VEHICLE THAT HAS TO
C-----CLEAR THE SAME CONFLICT AND THE INOW VEHICLE IS IN THE
C-----INTERSECTION OR THIS VEHICLE IS BEHIND THE INOW VEHICLE ON THE
C-----LANE THUS SET THIS VEHICLE AS THE NORC VEHICLE FOR THE INOW
C-----VEHICLE AND RETURN (THIS VEHICLES NORC IS 0)
C[    IF ( INOW               .EQ.-2147483647   )STOP 'SETCON INOW   04'
      IF      ( IPRC(1,INOW) . EQ . IX )         THEN
        JPRC = 1
      ELSE IF ( IPRC(2,INOW) . EQ . IX )         THEN
        JPRC = 2
      ELSE IF ( IPRC(1,INOW) . EQ .  0 )         THEN
        JPRC = 1
        IPRC(1,INOW) = IX
      ELSE IF ( IPRC(2,INOW) . EQ .  0 )         THEN
        JPRC = 2
        IPRC(2,INOW) = IX
      ELSE
        GO TO 9460
      END IF
      NORC(JPRC,INOW) = IV
C;    WRITE (TC3,331) TIME,'SETCON 8 NORC(JPRC,INOW) = IV      ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'INOW=',IQ(INOW),'I=',I,'N=',NGEOCP(IX),
C;   *                'JPRC=',JPRC
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9140 CONTINUE
      CALL  ABORTR  ( 'STOP 914 - LNEXT EQ 0 - SETCON' )
      STOP  914
 9330 CONTINUE
      CALL  ABORTR  ( 'STOP 933 - INFINITE LOOP - SETCON' )
      STOP  933
 9460 CONTINUE
      CALL  ABORTR  ( 'STOP 946 - INVALID IPRC/NORC - SETCON' )
      STOP  946
      END                                                               SETCON
C
C
C
      SUBROUTINE UNSETC
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'CONFLT'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INTEGER           I,JH,JM,JCONI,JGEOCP,JP,JPRC,KPRC,NOFC
C4701 FORMAT(32H UNSETTING CONFLICTS FOR VEHICLE,I4,9H FOR PATH,I4)
C
C-----SUBROUTINE UNSETC UNSETS THE CONFLICTS FOR THE VEHICLE FOR HIS
C-----INTERSECTION PATH
C
C[    I          = -2147483647
C[    JH         = -2147483647
C[    JM         = -2147483647
C[    JCONI      = -2147483647
C[    JGEOCP     = -2147483647
C[    JP         = -2147483647
C[    NOFC       = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'UNSETC'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IX = LNEXT(IV)
C-----SET THE INTERSECTION CONTROL LOGIC TIMER FOR PROCESS NEXT DT
      LOGTMP = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGTMP = 1
      END IF
C-----IF THE VEHICLE HAS NOT SET CONFLICTS THEN RETURN
      IF      ( IPRC(1,IV) . EQ . IX )           THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IV) . EQ . IX )           THEN
        JPRC = 2
      ELSE IF ( IPRC(1,IV) . EQ .  0 )           THEN
        JPRC = 1
        NORC(1,IV) = NVEP1
      ELSE IF ( IPRC(2,IV) . EQ .  0 )           THEN
        JPRC = 2
        NORC(2,IV) = NVEP1
      ELSE
        GO TO 9460
      END IF
                    IF ( NORC(JPRC,IV).EQ.NVEP1 )RETURN
C5          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 101
C4                  IF ( TIME . LT . TPRINT )    GO TO 101
C4    WRITE (6,701) IV,IX
C4101 CONTINUE
C-----IF THERE ARE NO GEOMETRIC CONFLICTING PATHS THEN GO TO 2010 AND
C-----SET THE FLAG FOR CONFLICTS NOT SET AND RETURN
                    IF ( NGEOCP(IX) . LE . 0 )   GO TO 2010
C-----PROCESS EACH GEOMETRIC CONFLICTING PATH
      DO 1070  I = 1 , NGEOCP(IX)
C-----INITIALIZE SOME PARAMETERS FOR THE GEOMETRIC CONFLICTING PATH LOOP
      JGEOCP = IGEOCP(I,IX)
      IF ( IX . EQ . ICONP(1,JGEOCP) )           THEN
        JH = 2
        JM = 1
      ELSE
        JH = 1
        JM = 2
      END IF
C-----IF THIS CONFLICT HAS BEEN CLEARED THEN GO TO 1070 AND CHECK THE
C-----NEXT GEOMETRIC CONFLICTING PATH
            IF ( ICONV(JM,JGEOCP) . EQ . 0 )     GO TO 1070
C-----IF THE NEXT VEHICLE THAT HAS NOT CLEARED THE INTERSECTION CONFLICT
C-----IS NOT THIS VEHICLE THEN GO TO 1040 AND CHAIN DOWN THE LINKS OF
C-----NORC VEHICLES AND REMOVE THIS VEHICLE FROM THE CHAIN
            IF ( ICONV(JM,JGEOCP) . NE . IV )    GO TO 1040
C-----THE NEXT VEHICLE THAT HAS NOT CLEARED THE INTERSECTION CONFLICT IS
C-----THIS VEHICLE THUS IF THERE IS NO VEHICLE TO THE REAR THAT HAS TO
C-----CLEAR THE SAME INTERSECTION CONFLICT THEN GO TO 1030 AND CLEAR THE
C-----INTERSECTION CONFLICT ELSE SET THE NEXT VEHICLE THAT HAS NOT
C-----CLEARED THE INTERSECTION CONFLICT TO THE VEHICLE TO THE REAR OF
C-----THIS VEHICLE THAT HAS TO CLEAR THE SAME INTERSECTION CONFLICT
      IF      ( IPRC(1,IV) . EQ . IX )           THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IV) . EQ . IX )           THEN
        JPRC = 2
      ELSE
        GO TO 9460
      END IF
                    IF ( NORC(JPRC,IV).EQ.0 )    GO TO 1030
      ICONV(JM,JGEOCP) = NORC(JPRC,IV)
C;    WRITE (TC3,331) TIME,'UNSETC 1 ICONV(JM,JGEOCP) = NORC(IV)',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JM=',JM,'JGEOCP=',JGEOCP,'I=',I,'N=',NGEOCP(IX),
C;   *                'JPRC=',JPRC
C;331 FORMAT(F6.1,1X,A36,I2,I3,I4,I6,1X,L1,F5.1,3I3,I6,2(I4,I6),
C;   *       5(1X,A,I4))
C-----GO TO 1070 AND CHECK THE NEXT GEOMETRIC CONFLICTING PATH
      GO TO 1070
 1030 CONTINUE
C-----THE NEXT VEHICLE THAT HAS NOT CLEARED THE INTERSECTION CONFLICT IS
C-----THIS VEHICLE AND THERE IS NO VEHICLE TO THE REAR THAT HAS TO CLEAR
C-----THE SAME INTERSECTION CONFLICT THUS CLEAR THE INTERSECTION
C-----CONFLICT
C[    IF ( JM                 .EQ.-2147483647   )STOP 'UNSETC JM     01'
C[    IF ( JGEOCP             .EQ.-2147483647   )STOP 'UNSETC JGEOCP 01'
      ICONV(JM,JGEOCP) = 0
C;    WRITE (TC3,331) TIME,'UNSETC 2 ICONV(JM,JGEOCP) = 0       ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JM=',JM,'JGEOCP=',JGEOCP,'I=',I,'N=',NGEOCP(IX)
      JP = ICONP(JH,JGEOCP)
      JCONI = ICONI(JH,JGEOCP)
C-----DECREMENT THE NUMBER OF CONFLICTS SET FOR THE OTHER INTERSECTION
C-----PATH INVOLVED IN THE INTERSECTION CONFLICT
      NCPSET(JP) = MAX0( NCPSET(JP)-1,0 )
C-----UNSET THE CONFLICT FOR THE OTHER INTERSECTION PATH INVOLVED IN THE
C-----INTERSECTION CONFLICT
      ICPSET(JCONI,JP) = 0
C-----GO TO 1070 AND CHECK THE NEXT GEOMETRIC CONFLICTING PATH
      GO TO 1070
 1040 CONTINUE
C-----THE NEXT VEHICLE THAT HAS NOT CLEARED THE INTERSECTION CONFLICT IS
C-----NOT THIS VEHICLE THUS CHAIN DOWN THE LINKS OF NORC VEHICLES AND
C-----REMOVE THIS VEHICLE FROM THE CHAIN THUS SET THE NOFC VEHICLE TO
C-----THE NEXT VEHICLE THAT HAS NOT CLEARED THE INTERSECTION CONFLICT
C[    IF ( JM                 .EQ.-2147483647   )STOP 'UNSETC JM     02'
C[    IF ( JGEOCP             .EQ.-2147483647   )STOP 'UNSETC JGEOCP 02'
      NOFC = ICONV(JM,JGEOCP)
C;    WRITE (TC3,331) TIME,'UNSETC 3 NOFC = ICONV(JM,JGEOCP)    ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JM=',JM,'JGEOCP=',JGEOCP,'I=',I,'N=',NGEOCP(IX)
 1050 CONTINUE
C-----IF THE NORC VEHICLE FOR THE NOFC VEHICLE IS THIS VEHICLE THEN GO
C-----TO 1060 AND SET THE NORC VEHICLE OF THE NOFC VEHICLE TO THE NORC
C-----VEHICLE FOR THIS VEHICLE (BREAK THIS VEHICLE OUT OF THE CHAIN
C-----BETWEEN THE NOFC VEHICLE AND HIS NORC VEHICLE)
C[    IF ( NOFC               .EQ.-2147483647   )STOP 'UNSETC NOFC   01'
      IF      ( IPRC(1,NOFC) . EQ . IX )         THEN
        JPRC = 1
      ELSE IF ( IPRC(2,NOFC) . EQ . IX )         THEN
        JPRC = 2
      ELSE
        GO TO 9460
      END IF
                    IF ( NORC(JPRC,NOFC).EQ.IV ) GO TO 1060
C-----IF THERE IS NO VEHICLE TO THE REAR OF THE NOFC VEHICLE THAT HAS TO
C-----CLEAR THE SAME INTERSECTION CONFLICT THEN GO TO 1070 AND CHECK THE
C-----NEXT GEOMETRIC CONFLICTING PATH ELSE SET THE NOFC VEHICLE TO
C-----THE NORC VEHICLE FOR THE OLD NOFC VEHICLE AND CHECK AGAIN
                    IF ( NORC(JPRC,NOFC).EQ.0 )  GO TO 1070
      NOFC = NORC(JPRC,NOFC)
C;    WRITE (TC3,331) TIME,'UNSETC 4 NOFC = NORC(JPRC,NOFC)    ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'I=',I,'N=',NGEOCP(IX),'JPRC=',JPRC
      GO TO 1050
 1060 CONTINUE
C-----THE NORC VEHICLE FOR THE NOFC VEHICLE IS THIS VEHICLE THUS SET THE
C-----NORC VEHICLE OF THE NOFC VEHICLE TO THE NORC VEHICLE FOR THIS
C-----VEHICLE (BREAK THIS VEHICLE OUT OF THE CHAIN BETWEEN THE NOFC
C-----VEHICLE AND HIS NORC VEHICLE) AND GO TO 1070 AND CHECK THE NEXT
C-----GEOMETRIC CONFLICTING PATH
C[    IF ( NOFC               .EQ.-2147483647   )STOP 'UNSETC NOFC   02'
      IF      ( IPRC(1,IV) . EQ . IX )           THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IV) . EQ . IX )           THEN
        JPRC = 2
      ELSE
        GO TO 9460
      END IF
      IF      ( IPRC(1,NOFC) . EQ . IX )         THEN
        KPRC = 1
      ELSE IF ( IPRC(2,NOFC) . EQ . IX )         THEN
        KPRC = 2
      ELSE
        GO TO 9460
      END IF
      NORC(KPRC,NOFC) = NORC(JPRC,IV)
C;    WRITE (TC3,331) TIME,'UNSETC 5 NORC(NOFC) = NORC(JPRC,IV)',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'I=',I,'N=',NGEOCP(IX),'JPRC=',JPRC,'KPRC=',KPRC
C-----END OF GEOMETRIC CONFLICTING PATH LOOP
 1070 CONTINUE
 2010 CONTINUE
C-----SET THE FLAG FOR CONFLICTS NOT SET AND RETURN
      IF      ( IPRC(1,IV) . EQ . IX )           THEN
        JPRC = 1
      ELSE IF ( IPRC(2,IV) . EQ . IX )           THEN
        JPRC = 2
      ELSE
        GO TO 9460
      END IF
      NORC(JPRC,IV) = NVEP1
C;    WRITE (TC3,331) TIME,'UNSETC 6 NORC(IV) = NVEP1          ',
C;   *                IA,IL,IV,IQ(IV),MININT(IV),LATPOS(IV),
C;   *                LPREV(IV),LPRES(IV),LNEXT(IV),IQ(NOFC),
C;   *                IPRC(1,IV),IQ(NORC(1,IV)),
C;   *                IPRC(2,IV),IQ(NORC(2,IV)),
C;   *                'JPRC=',JPRC
      IPRC(JPRC,IV) = 0
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9460 CONTINUE
      CALL  ABORTR  ( 'STOP 946 - INVALID IPRC/NORC - UNSETC' )
      STOP  946
      END                                                               UNSETC
C
C
C
      SUBROUTINE INFLZN
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
C
C-----SUBROUTINE INFLZN INITIALIZES THE VEHICLES INTERSECTION CONTROL
C-----LOGICAL ATTRIBUTES BASED ON THE TYPE OF TRAFFIC CONTROL FOR THIS
C-----LANE
C
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'INFLZN'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IF ( LNEXT(IV) . EQ . 0 )                  THEN
        MDEDIC(IV) = .FALSE.
C-----  SET THE INTERSECTION CONTROL LOGIC TIMER SO THIS VEHICLE WILL BE
C-----  PROCESSED NEXT DT
        LOGTMP     = 2
        LOGFLG(IV) = 2
C-----  IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE
C-----  IN THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----  CONFLICTS
        IF ( SMJCOL . OR . EVCCON )              THEN
          LOGTMP     = 1
          LOGFLG(IV) = 1
        END IF
        RETURN
      END IF
C-----SET SOME PARAMETERS FOR ALL TYPES OF LANE CONTROL
C-----(ALL INTERSECTION CONTROL LOGICAL INDEPENDENT ATTRIBUTES SET FALSE
C-----IN SUBROUTINE LOGIN)
      MINFLZ(IV) = .TRUE.
                    IF ( ICONTR . EQ . ICUNCT )  MIUNC(IV) = .TRUE.
C-----SET THE INTERSECTION CONTROL LOGIC TIMER FOR PROCESS NEXT DT
      LOGTMP     = 2
      LOGFLG(IV) = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        MCHKCF(IV) = .TRUE.
        MPRO  (IV) = .FALSE.
        LOGTMP     = 1
        LOGFLG(IV) = 1
      END IF
C-----PROCESS BASED ON THE LANE CONTROL
C-----        OUTB  UC  YSC  SSC  SIG SLTOR SRTOR
      GO TO ( 1010,2010,3010,4010,5010,5010,5010 ) , LCONTV(IV)
 1010 CONTINUE
C-----THIS LANE IS OUTBOUND OR A BLOCKED INBOUND LANE
      GO TO 9150
 2010 CONTINUE
C-----THIS LANE IS UNCONTROLLED THUS SET THAT THE TRAFFIC CONTROL AHEAD
C-----DOES NOT REQUIRE ME TO STOP
      MLUNC (IV) = .TRUE.
      MTCARS(IV) = .FALSE.
C-----THIS LANE IS UNCONTROLLED AND IF THE INTERSECTION IS ALSO
C-----UNCONTROLLED THEN RETURN
                    IF ( MIUNC(IV) )             RETURN
C-----THIS LANE IS UNCONTROLLED AND THE INTERSECTION IS CONTROLLED THUS
C-----SET THAT INTERSECTION CONFLICTS MUST BE CHECKED (FOR LEFT TURNS
C-----AND LANE CHANGES WITHIN THE INTERSECTION)
      MCHKCF(IV) = .TRUE.
C-----IF THE VEHICLE IS TURNING LEFT THEN RETURN
                    IF ( ITURN(IV) .LE. ITURNL ) RETURN
C-----IF THE VEHICLES INTERSECTION PATH CHANGES LANES WITHIN THE
C-----INTERSECTION THEN RETURN
            IF ( ILCH(LNEXT(IV)) . NE . 0 )      RETURN
C-----THIS LANE IS UNCONTROLLED AND THE INTERSECTION IS CONTROLLED THUS
C-----SET THAT THE VEHICLE MAY PROCEED INTO THE INTERSECTION AND THAT
C-----INTERSECTION CONFLICTS NEED NOT BE CHECKED AND THAT THE TRAFFIC
C-----CONTROL AHEAD DOES NOT REQUIRE ME TO STOP
      MSFLG (IV) = .FALSE.
      MTCARS(IV) = .FALSE.
      MCHKCF(IV) = .FALSE.
      MPRO  (IV) = .TRUE.
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        MCHKCF(IV) = .TRUE.
        MPRO  (IV) = .FALSE.
        RETURN
      END IF
C-----SET CONFLICTS FOR THE VEHICLE FOR HIS INTERSECTION PATH AND RETURN
      CALL  SETCON
      RETURN
 3010 CONTINUE
C-----THIS LANE IS YIELD SIGN CONTROLLED THUS SET THAT THE TRAFFIC
C-----CONTROL AHEAD DOES NOT REQUIRE ME TO STOP AND RETURN
      MTCARS(IV) = .FALSE.
      MLYELD(IV) = .TRUE.
      RETURN
 4010 CONTINUE
C-----THIS LANE IS STOP SIGN CONTROLLED
      MLSTOP(IV) = .TRUE.
      RETURN
 5010 CONTINUE
C-----THIS LANE IS SIGNAL CONTROLLED THUS CHECK THE SIGNAL INDICATION
      IF ( (.NOT. MSSGRN(IV)) . AND . (.NOT. MSSRED(IV)) )
     *                                           THEN
        MSSGRN(IV) = .TRUE.
      END IF
      MPRO(IV) = .FALSE.
C-----DETERMINE THE APPROPRIATE DRIVER RESPONSE FOR THE SIGNAL
C-----INDICATION
      CALL  SIGRES  ( ISISET(ICAMPC,IBLN(IL)) )
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9150 CONTINUE
      CALL  ABORTR  ( 'STOP 915 - LCONTR EQ 1 - INFLZN' )
      STOP  915
      END                                                               INFLZN
C
C
C
      SUBROUTINE PATHF  ( IFORCE,PAVOID,CSUB )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      CHARACTER*(*)     CSUB
      LOGICAL           IFORCE
      INTEGER           I,II,ILANE,ILL,ILR,IP2,JA,KOUNT,LFORCE,LFTYPE,
     *                  LOBAPD,LPATH,LPTYPE,MPATH,MPRES,MPTYPE,PAVOID
  701 FORMAT('VEHICLE ',I6,' ON APPROACH ',I2,' AT TIME = ',F7.1,
     *       ' WAS FORCED TO USE DESIRED OUTBOUND APPROACH ',I2,
     *       ' INSTEAD OF APPROACH ',I2,' (',A,') FREEUT')
  702 FORMAT('VEHICLE ',I6,' ON APPROACH ',I2,' AT TIME = ',F7.1,
     *       ' WAS FORCED TO USE DESIRED OUTBOUND APPROACH ',I2,
     *       ' INSTEAD OF APPROACH ',I2,' (',A,')')
  703 FORMAT('VEHICLE ',I6,' ON APPROACH ',I2,' AT TIME = ',F7.1,
     *       ' WAS FORCED TO USE PATH ',I3,' TO APPROACH ',I2,
     *       ' INSTEAD OF TO APPROACH ',I2,' (',A,')')
C
C-----SUBROUTINE PATHF FINDS AN INTERSECTION PATH FOR THIS VEHICLE BASED
C-----ON THE CURRENT APPROACH, CURRENT LANE, AND THE DESIRED OUTBOUND
C-----APPROACH
C
C[    I          = -2147483647
C[    II         = -2147483647
C[    ILANE      = -2147483647
C[    IP2        = -2147483647
C[    JA         = -2147483647
C[    KOUNT      = -2147483647
C[    LFORCE     = -2147483647
C[    LFTYPE     = -2147483647
C[    LOBAPD     = -2147483647
C[    LPATH      = -2147483647
C[    LPTYPE     = -2147483647
C[    MPATH      = -2147483647
C[    MPRES      = -2147483647
C[    MPTYPE     = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'PATHF'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      KOUNT = 0
      LPATH = 0
      MDEDIC(IV) = .TRUE.
      MPRES = LPRES(IV)
      IF ( ILTYPE(MPRES) . EQ . OUTBDL )         THEN
C-----  THE VEHICLE IS ON AN OUTBOUND LANE THUS SET LEGAL FOR TURN IS
C-----  LEGAL FROM LEG AND LANE (CONSIDER OPTIONAL LANE) AND DO NOT
C-----  CHANGE INTERSECTION PATHS
        IF ( LEGAL(IV) . GE . 4 )                THEN
          LEGAL(IV) = 2
        END IF
        RETURN
      END IF
 1010 CONTINUE
C-----SET LOBAPD FOR DESIRED OUTBOUND APPROACH
      LOBAPD = IABS( NOBAPD(IV) )
      IF ( IAFLAG(IA) . EQ . IAFLAG(LOBAPD) )    GO TO 1020
      IF ( IAFLAG(IA) . EQ . ILETTI         )    GO TO 1020
      IF ( NOBAPD(IV) . LE . 0 )                 THEN
C-----  IAFLAG OF IA AND IAFLAG OF LOBAPD ARE DIFFERENT, IAFLAG OF IA IS
C-----  NOT AN INTERNAL, AND THE LOBAPD IS NEGATIVE THUS SET LOBAPD FOR
C-----  FREE U-TURN PATH NEEDED TO GET TO LOBAPD
        LOBAPD = INTLNU(LOBAPD)
      ELSE
C-----  IAFLAG OF IA AND IAFLAG OF LOBAPD ARE DIFFERENT, IAFLAG OF IA IS
C-----  NOT AN INTERNAL, AND THE LOBAPD IS POSITIVE THUS SET LOBAPD FOR
C-----  INTERNAL TO GET TO LOBAPD
        LOBAPD = INTLNK(LOBAPD)
      END IF
 1020 CONTINUE
                    IF ( MBLOCK(IV) )            GO TO 1040
C-----INITIALIZE THE FORCED PATH TO NOT SET
C-----LPTPNS = NOT SET
      LFORCE = 0
      LFTYPE = LPTPNS
C-----IF THERE ARE NO INTERSECTION PATHS FROM LANE MPRES THEN GO TO 1040
C-----AND CHECK EACH LANE OF THIS APPROACH FOR AN INTERSECTION PATH TO
C-----THIS VEHICLES DESIRED OUTBOUND APPROACH
            IF ( NPINT(MPRES) . LE . 0 )         GO TO 1040
C-----CHECK EACH INTERSECTION PATH FROM LANE MPRES
      DO 1030  I = 1 , NPINT(MPRES)
      LPATH = LINTP(I,MPRES)
C-----IF THE INTERSECTION PATH SHOULD BE AVOIDED THEN GO TO 1030 AND
C-----CHECK THE NEXT INTERSECTION PATH
      IF ( (PAVOID.GT.0).AND.(LPATH.EQ.PAVOID) ) GO TO 1030
C-----IF THE INTERSECTION PATH DOES NOT ALLOW THE VEHICLE TYPE THEN GO
C-----TO 1030 AND CHECK THE NEXT INTERSECTION PATH
      IF ( IAND( VEHTYP(IV),PAVT(LPATH) ).EQ.0 ) GO TO 1030
C[    IF ( LOBAPD             .EQ.-2147483647   )STOP 'PATHF  LOBAPD 01'
      CALL  PTYPE   ( LFORCE,LFTYPE,LOBAPD,LPATH,LPTYPE )
C-----IF THE LINKING OUTBOUND APPROACH FOR THE INTERSECTION PATH IS EQ
C-----THE DESIRED OUTBOUND PATH FOR THIS VEHICLE AND THE INTERSECTION
C-----PATH DOES NOT LANE CHANGE WITHIN THE INTERSECTION THEN GO TO 4010
C-----AND SET THIS VEHICLE TO USE THIS INTERSECTION PATH
C[    IF ( LPTYPE             .EQ.-2147483647   )STOP 'PATHF  LPTYPE 01'
C-----LPTP01 = RAD OK  NO LANE CHANGE WITHIN THE INTERSECTION TO LOBAPD
                    IF ( LPTYPE . EQ . LPTP01 )  GO TO 4010
C-----END ON INTERSECTION PATH LOOP
 1030 CONTINUE
                    IF ( ISET(IV) . EQ . 1 )     RETURN
C-----SET THE INTERSECTION PATH FOR THIS VEHICLE TO THE FORCED PATH AND
C-----IF PATHF IS SUPPOSE TO FORCE A PATH THEN GO TO 4010 AND SET THIS
C-----VEHICLE TO USE THE FORCED INTERSECTION PATH
C[    IF ( LFORCE             .EQ.-2147483647   )STOP 'PATHF  LFORCE 01'
      LPATH = LFORCE
      IF ( IFORCE )                              THEN
C[      IF ( LOBAPD           .EQ.-2147483647   )STOP 'PATHF  LOBAPD 02'
        IF ( IAFLAG(IA) . EQ . IAFLAG(LOBAPD) )  GO TO 4010
        IF ( IAFLAG(IA) . EQ . ILETTI         )  GO TO 4010
        IF ( LOBAPD.EQ.INTLNU(IABS(NOBAPD(IV))) )THEN
          NOBAPD(IV) = INTLNK(IABS(NOBAPD(IV)))
          WRITE (WRNMSG,701) IQ(IV),IA,TIME,NOBAPD(IV),LOBAPD,CSUB
          CALL  PRTWRN  ( WRNMSG )
          IF ( IQ(IV) . LT . 0 )                 THEN
            CALL  VDIERR  ( TIME,-IQ(IV),VDIFDF )
          END IF
          GO TO 1010
        END IF
        GO TO 4010
      END IF
 1040 CONTINUE
                    IF ( IFORCE )                GO TO 9160
                    IF ( ISET(IV) . EQ . 1 )     GO TO 5010
                    IF ( (.NOT. MBLOCK(IV)) )    GO TO 2010
      ILL = NLL(MPRES)
      ILR = NLR(MPRES)
C-----IF THERE IS A LANE TO THE LEFT AND IF THE VEHICLE IS NOT ALLOWED
C-----ON THE LANE TO THE LEFT THEN SET NO LANE TO THE LEFT
      IF ( ILL . GT . 0 )                        THEN
        IF ( IAND( VEHTYP(IV),LAVT(ILL) ).EQ.0 ) ILL = 0
      END IF
C-----IF THERE IS A LANE TO THE RIGHT AND IF THE VEHICLE IS NOT ALLOWED
C-----ON THE LANE TO THE RIGHT THEN SET NO LANE TO THE RIGHT
      IF ( ILR . GT . 0 )                        THEN
        IF ( IAND( VEHTYP(IV),LAVT(ILR) ).EQ.0 ) ILR = 0
      END IF
      IF ( (ILL.EQ.0) . AND . (ILR.EQ.0) )       GO TO 2010
C-----SET WHICH SIDE THE VEHICLE SHOULD CHANGE LANES INTO
      IF ( (ILL.GT.0) . AND . (ILR.GT.0) )       THEN
        IF ( ( LGEOM(2,ILL) . EQ . LGEOM(4,ILL) ) . AND .
     *       ( LGEOM(2,ILR) . EQ . LGEOM(4,ILR) ) )
     *                                           THEN
C-----    NLL AND NLR EXIST, NLL AND NLR CONTINUOUS THUS GO TO 2010
          GO TO 2010
        END IF
        IF ( LGEOM(2,ILL) . EQ . LGEOM(4,ILL) )  THEN
C-----    NLL AND NLR EXIST, NLL CONTINUOUS AND NLR NOT CONTINUOUS THUS
C-----    CHANGE LEFT
          ISET (IV) = 5
          LEGAL(IV) = 1
          GO TO 5010
        END IF
        IF ( LGEOM(2,ILR) . EQ . LGEOM(4,ILR) )  THEN
C-----    NLL AND NLR EXIST, NLR CONTINUOUS AND NLL NOT CONTINUOUS THUS
C-----    CHANGE RIGHT
          ISET (IV) = 5
          LEGAL(IV) = 3
          GO TO 5010
        END IF
        IF ( ( LGEOM(2,ILL) . GT . LGEOM(2,MPRES ) ) . AND .
     *       ( LGEOM(2,ILR) . GT . LGEOM(2,MPRES ) ) )
     *                                           THEN
C-----    NLL AND NLR EXIST, NEITHER CONTINUOUS, NLL AND NLR LONGER, USE
C-----    LONGEST
          ISET (IV) = 5
          IF ( LGEOM(2,ILL) . GT . LGEOM(2,ILR) )THEN
C-----      NLL AND NLR EXIST, NEITHER CONTINUOUS, NLL LONGER, USE LEFT
            LEGAL(IV) = 1
          ELSE
C-----      NLL AND NLR EXIST, NEITHER CONTINUOUS, NLR LONGER, USE RIGHT
            LEGAL(IV) = 3
          END IF
          GO TO 5010
        END IF
        IF ( LGEOM(2,ILL) . GT . LGEOM(2,MPRES ) )
     *                                           THEN
C-----    NLL AND NLR EXIST, NEITHER CONTINUOUS, NLL LONGER, USE LEFT
          ISET (IV) = 5
          LEGAL(IV) = 1
          GO TO 5010
        END IF
        IF ( LGEOM(2,ILR) . GT . LGEOM(2,MPRES ) )
     *                                           THEN
C-----    NLL AND NLR EXIST, NEITHER CONTINUOUS, NLR LONGER, USE RIGHT
          ISET (IV) = 5
          LEGAL(IV) = 3
          GO TO 5010
        END IF
      ELSE
C-----  NLL OR NLR EXIST BUT NOT BOTH THUS CHANGE TO LANE WHICH EXIST
        ISET (IV) = 5
        LEGAL(IV) = 1
                    IF ( ILL . EQ . 0 )          LEGAL(IV) = 3
        GO TO 5010
      END IF
 2010 CONTINUE
C-----CHECK EACH LANE OF THIS APPROACH FOR AN INTERSECTION PATH TO THIS
C-----VEHICLES DESIRED OUTBOUND APPROACH
C-----INITIALIZE THE FORCED PATH TO NOT SET
C-----LPTPNS = NOT SET
      LFORCE = 0
      LFTYPE = LPTPNS
      DO 2030  II = 1 , NLANES(IA)
      ILANE = LLANES(II,IA)
C-----IF THERE ARE NO INTERSECTION PATHS FROM LANE ILANE THEN GO TO 2030
C-----AND CHECK THE NEXT LANE
                    IF ( NPINT(ILANE) . LE . 0 ) GO TO 2030
C-----CHECK EACH INTERSECTION PATH FROM LANE ILANE TO SEE IF IT GOES TO
C-----THIS VEHICLES DESIRED OUTBOUND APPROACH
      DO 2020  I = 1 , NPINT(ILANE)
      LPATH = LINTP(I,ILANE)
C-----IF THE INTERSECTION PATH SHOULD BE AVOIDED THEN GO TO 2020 AND
C-----CHECK THE NEXT INTERSECTION PATH
      IF ( (PAVOID.GT.0).AND.(LPATH.EQ.PAVOID) ) GO TO 2020
C-----IF THE INTERSECTION PATH DOES NOT ALLOW THE VEHICLE TYPE THEN GO
C-----TO 2020 AND CHECK THE NEXT INTERSECTION PATH
      IF ( IAND( VEHTYP(IV),PAVT(LPATH) ).EQ.0 ) GO TO 2020
C[    IF ( LOBAPD             .EQ.-2147483647   )STOP 'PATHF  LOBAPD 03'
      CALL  PTYPE   ( LFORCE,LFTYPE,LOBAPD,LPATH,LPTYPE )
C-----IF THE LINKING OUTBOUND APPROACH FOR THE INTERSECTION PATH IS EQ
C-----THE DESIRED OUTBOUND PATH FOR THIS VEHICLE AND THE INTERSECTION
C-----PATH DOES NOT LANE CHANGE WITHIN THE INTERSECTION THEN GO TO 3010
C-----AND SET WHICH SIDE THE VEHICLE SHOULD LANE CHANGE TO
C[    IF ( LPTYPE             .EQ.-2147483647   )STOP 'PATHF  LPTYPE 02'
C-----LPTP01 = RAD OK  NO LANE CHANGE WITHIN THE INTERSECTION TO LOBAPD
                    IF ( LPTYPE . EQ . LPTP01 )  GO TO 3010
C-----END OF INTERSECTION PATH LOOP
 2020 CONTINUE
C-----END OF LANE LOOP
 2030 CONTINUE
C-----IF THE HIGHEST PRIORITY INTERSECTION PATH FROM ANY LANE FOR THIS
C-----APPROACH GOES TO THIS VEHICLES DESIRED OUTBOUND APPROACH (POSSIBLY
C-----CHANGING LANES WITHIN THE INTERSECTION) THEN USE THIS INTERSECTION
C-----PATH
C[    IF ( LFTYPE             .EQ.-2147483647   )STOP 'PATHF  LFTYPE 01'
C-----LPTP01 = RAD OK  NO LANE CHANGE WITHIN THE INTERSECTION TO LOBAPD
C-----LPTP02 = RAD OK     LANE CHANGE WITHIN THE INTERSECTION TO LOBAPD
      IF ( LFTYPE . LE . LPTP02 )                THEN
C[      IF ( LFORCE           .EQ.-2147483647   )STOP 'PATHF  LFORCE 02'
                    IF ( LFORCE . EQ . 0 )       GO TO 9690
        LPATH = LFORCE
        ILANE = LIBL(LPATH)
C-----  IF THE HIGHEST PRIORITY INTERSECTION PATH FROM ANY LANE FOR THIS
C-----  APPROACH THAT GOES TO THIS VEHICLES DESIRED OUTBOUND APPROACH
C-----  (POSSIBLY CHANGING LANES WITHIN THE INTERSECTION) ORIGINATES
C-----  FROM THIS VEHICLES CURRENT LANE THEN GO TO 4010 AND SET THIS
C-----  VEHICLE TO USE THIS INTERSECTION PATH ELSE GO TO 3010 AND SET
C-----  WHICH SIDE THE VEHICLE SHOULD LANE CHANGE TO
        IF ( ILANE . EQ . MPRES )                THEN
          GO TO 4010
        ELSE
          GO TO 3010
        END IF
      END IF
C-----NO INTERSECTION PATH FROM ANY LANE FOR THIS APPROACH GOES TO THE
C-----DESIRED OUTBOUND APPROACH FOR THIS VEHICLE THUS THE TURN IS
C-----ILLEGAL FROM THIS APPROACH (SOMETHING IS VERY WRONG)
C[    IF ( LOBAPD             .EQ.-2147483647   )STOP 'PATHF  LOBAPD 04'
      IF ( IAFLAG(IA) . EQ . IAFLAG(LOBAPD) )    GO TO 2040
      IF ( IAFLAG(IA) . EQ . ILETTI         )    GO TO 2040
      IF ( LOBAPD.EQ.INTLNU(IABS(NOBAPD(IV))) )  THEN
        NOBAPD(IV) = INTLNK(IABS(NOBAPD(IV)))
        WRITE (WRNMSG,701) IQ(IV),IA,TIME,NOBAPD(IV),LOBAPD,CSUB
        CALL  PRTWRN  ( WRNMSG )
        IF ( IQ(IV) . LT . 0 )                   THEN
          CALL  VDIERR  ( TIME,-IQ(IV),VDIFDF )
        END IF
        GO TO 1010
      END IF
 2040 CONTINUE
      IF ( KOUNT . EQ . 0 )                      THEN
C-----CHANGE THE DESIRED OUTBOUND APPROACH FOR THE VEHICLE
C[      IF ( LFORCE           .EQ.-2147483647   )STOP 'PATHF  LFORCE 02'
C[      IF ( LOBAPD           .EQ.-2147483647   )STOP 'PATHF  LOBAPD 05'
                    IF ( LFORCE . EQ . 0 )       GO TO 9690
        NOBAPD(IV) = IOA(LFORCE)
        IF ( IAFLAG(NOBAPD(IV)) . EQ . ILETTI )  THEN
          NOBAPD(IV) = INTLNK(NOBAPD(IV))
C-----    CHECK EACH LANE OF THE NOBAPD APPROACH FOR AN INTERSECTION PATH
C-----    INITIALIZE THE FORCED PATH TO NOT SET
C-----    LPTPNS = NOT SET
          LFORCE = 0
          LFTYPE = LPTPNS
          DO 2060  II = 1 , NLANES(NOBAPD(IV))
          ILANE = LLANES(II,NOBAPD(IV))
C-----    IF THERE ARE NO INTERSECTION PATHS FROM LANE ILANE THEN GO TO
C-----    2060 AND CHECK THE NEXT LANE
                    IF ( NPINT(ILANE) . LE . 0 ) GO TO 2060
C-----    CHECK EACH INTERSECTION PATH FROM LANE ILANE
          DO 2050  I = 1 , NPINT(ILANE)
          LPATH = LINTP(I,ILANE)
C-----    IF THE INTERSECTION PATH SHOULD BE AVOIDED THEN GO TO 2050 AND
C-----    CHECK THE NEXT INTERSECTION PATH
          IF ( (PAVOID.GT.0).AND.(LPATH.EQ.PAVOID) )
     *                                           GO TO 2050
C-----    IF THE INTERSECTION PATH DOES NOT ALLOW THE VEHICLE TYPE THEN
C-----    GO TO 2050 AND CHECK THE NEXT INTERSECTION PATH
          IF ( IAND( VEHTYP(IV),PAVT(LPATH) ) . EQ . 0 )
     *                                           GO TO 2050
          CALL  PTYPE   ( LFORCE,LFTYPE,LOBAPD,LPATH,LPTYPE )
C-----    END OF INTERSECTION PATH LOOP
 2050     CONTINUE
C-----    END OF LANE LOOP
 2060     CONTINUE
                    IF ( LFORCE . EQ . 0 )       GO TO 9690
          NOBAPD(IV) = IOA(LFORCE)
        END IF
        WRITE (WRNMSG,702) IQ(IV),IA,TIME,NOBAPD(IV),LOBAPD,CSUB
        CALL  PRTWRN  ( WRNMSG )
        IF ( IQ(IV) . LT . 0 )                   THEN
          CALL  VDIERR  ( TIME,-IQ(IV),VDIFDO )
        END IF
        KOUNT = KOUNT + 1
        GO TO 1010
      END IF
      GO TO 9310
 3010 CONTINUE
C-----ONE OF THE LANES FOR THIS APPROACH HAS AN INTERSECTION PATH THAT
C-----GOES TO THE DESIRED OUTBOUND APPROACH FOR THIS VEHICLE THUS SET
C-----WHICH SIDE THE VEHICLE SHOULD LANE CHANGE TO
                    IF ( ISET(IV) . EQ . 1 )     GO TO 5010
C-----IF THE LANE NUMBER OF THE LANE THAT HAS AN INTERSECTION PATH THAT
C-----GOES TO THE DESIRED OUTBOUND APPROACH FOR THIS APPROACH IS LT THE
C-----LANE NUMBER FOR THE PRESENT LANE THEN SET THIS VEHICLE TO
C-----CHANGE LANES LEFT ELSE SET THIS VEHICLE TO CHANGE LANES RIGHT
      LEGAL(IV) = 3
C[    IF ( ILANE              .EQ.-2147483647   )STOP 'PATHF  ILANE  01'
                    IF ( ILANE . LT . MPRES )    LEGAL(IV) = 1
      ISET(IV) = 5
C-----GO TO 5010 AND FINISH PROCESSING
      GO TO 5010
 4010 CONTINUE
C-----SET THIS VEHICLE TO USE INTERSECTION PATH LPATH
                    IF ( ISET(IV) . NE . 1 )     LEGAL(IV) = 2
C-----CHECK MY LANE AND IF BLOCKED THEN SET PARAMETERS FOR BLOCKED LANE
      CALL  CHKMLN
C-----ITURNU     1 ITURN CODE - U-TURN
C-----ITURNL     2 ITURN CODE - LEFT TURN
C-----ITURNS     3 ITURN CODE - STRAIGHT
C-----ITURNR     4 ITURN CODE - RIGHT TURN
C-----LTURNR     1 LTURN, IPT, AND IPTURN CODE - RIGHT TURN
C-----LTURNS     2 LTURN, IPT, AND IPTURN CODE - STRAIGHT
C-----LTURNL     4 LTURN, IPT, AND IPTURN CODE - LEFT TURN
C-----LTURNU     8 LTURN, IPT, AND IPTURN CODE - U-TURN
C-----SET THIS VEHICLES TURN CODE ITURN (1=U 2=LEFT 3=STRAIGHT 4=RIGHT)
C-----BASED UPON THE PATH TURN CODE IPT (1=RIGHT 2=STRAIGHT 4=LEFT 8=U)
C[    IF ( LPATH              .EQ.-2147483647   )STOP 'PATHF  LPATH  01'
      ITURN(IV) = RITURN(IPT(LPATH))
      IF ( LNEXT(IV) . GT . 0 )                  THEN
        CALL  UNSETC
      END IF
      LNEXT(IV) = LPATH
C[    IF ( LOBAPD             .EQ.-2147483647   )STOP 'PATHF  LOBAPD 06'
                    IF ( IOA(LPATH) .EQ. LOBAPD )GO TO 5010
C-----CHANGE THE PATH FOR THE VEHICLE
      WRITE (WRNMSG,703) IQ(IV),IA,TIME,LNEXT(IV),IOA(LPATH),LOBAPD,CSUB
      CALL  PRTWRN  ( WRNMSG )
      IF ( IQ(IV) . LT . 0 )                     THEN
        CALL  VDIERR  ( TIME,-IQ(IV),VDIFIP )
      END IF
      LOBAPD = IABS( NOBAPD(IV) )
      NOBAPD(IV) = IOA(LPATH)
      IF ( IAFLAG(NOBAPD(IV)) . EQ . ILETTI )    THEN
        NOBAPD(IV) = INTLNK(NOBAPD(IV))
C-----  CHECK EACH LANE OF THE NOBAPD APPROACH FOR AN INTERSECTION PATH
C-----  INITIALIZE THE FORCED PATH TO NOT SET
C-----  LPTPNS = NOT SET
        LFORCE = 0
        LFTYPE = LPTPNS
        DO 4016  II = 1 , NLANES(NOBAPD(IV))
        ILANE = LLANES(II,NOBAPD(IV))
C-----  IF THERE ARE NO INTERSECTION PATHS FROM LANE ILANE THEN GO TO
C-----  4016 AND CHECK THE NEXT LANE
                  IF ( NPINT(ILANE) . LE . 0 )   GO TO 4016
C-----  CHECK EACH INTERSECTION PATH FROM LANE ILANE
        DO 4013  I = 1 , NPINT(ILANE)
        LPATH = LINTP(I,ILANE)
C-----  IF THE INTERSECTION PATH SHOULD BE AVOIDED THEN GO TO 4013 AND
C-----  CHECK THE NEXT INTERSECTION PATH
        IF ( (PAVOID.GT.0).AND.(LPATH.EQ.PAVOID) )
     *                                           GO TO 4013
C-----  IF THE INTERSECTION PATH DOES NOT ALLOW THE VEHICLE TYPE THEN
C-----  GO TO 4013 AND CHECK THE NEXT INTERSECTION PATH
        IF ( IAND( VEHTYP(IV),PAVT(LPATH) ) . EQ . 0 )
     *                                           GO TO 4013
        CALL  PTYPE   ( LFORCE,LFTYPE,LOBAPD,LPATH,LPTYPE )
C-----  END OF INTERSECTION PATH LOOP
 4013   CONTINUE
C-----  END OF LANE LOOP
 4016   CONTINUE
                  IF ( LFORCE . EQ . 0 )         GO TO 9690
        NOBAPD(IV) = IOA(LFORCE)
      END IF
      INT2P (IV) = 0
                    IF ( (.NOT. DIAMON) )        GO TO 5010
                    IF ( LOBL(LPATH) . EQ . 0 )  GO TO 5010
      JA = ISNA( LOBL(LPATH) )
            IF ( IAFLAG(JA) . NE . ILETTI )      GO TO 5010
C-----FIND MINIMUM MPTYPE PATH FROM ANY LANE FOR INTERNAL APPROACH JA
C-----LPTPNS = NOT SET
      LFORCE = 0
      LFTYPE = LPTPNS
      DO 4030  II = 1 , NLANES(JA)
      ILANE = LLANES(II,JA)
                    IF ( NPINT(ILANE) . LE . 0 ) GO TO 4030
      DO 4020  I = 1 , NPINT(ILANE)
      MPATH = LINTP(I,ILANE)
C-----IF THE INTERSECTION PATH SHOULD BE AVOIDED THEN GO TO 4020 AND
C-----CHECK THE NEXT INTERSECTION PATH
      IF ( (PAVOID.GT.0).AND.(MPATH.EQ.PAVOID) ) GO TO 4020
C-----IF THE INTERSECTION PATH DOES NOT ALLOW THE VEHICLE TYPE THEN GO
C-----TO 4020 AND CHECK THE NEXT INTERSECTION PATH
      IF ( IAND( VEHTYP(IV),PAVT(MPATH) ).EQ.0 ) GO TO 4020
C[    IF ( LOBAPD             .EQ.-2147483647   )STOP 'PATHF  LOBAPD 07'
      CALL  PTYPE   ( LFORCE,LFTYPE,LOBAPD,MPATH,MPTYPE )
C[    IF ( MPTYPE             .EQ.-2147483647   )STOP 'PATHF  MPTYPE 01'
C-----LPTP01 = RAD OK  NO LANE CHANGE WITHIN THE INTERSECTION TO LOBAPD
C-----LPTP02 = RAD OK     LANE CHANGE WITHIN THE INTERSECTION TO LOBAPD
C-----LPTP03 = RAD OK  NO LANE CHANGE WITHIN THE INTERSECTION STRAIGHT
C-----LPTP04 = RAD OK     LANE CHANGE WITHIN THE INTERSECTION STRAIGHT
C-----LPTP05 = RAD OK  NO LANE CHANGE WITHIN THE INTERSECTION ANY PATH
C-----LPTP06 = RAD OK     LANE CHANGE WITHIN THE INTERSECTION ANY PATH
                    IF ( MPTYPE . LE . LPTP06 )  GO TO 4040
 4020 CONTINUE
 4030 CONTINUE
C[    IF ( LFORCE             .EQ.-2147483647   )STOP 'PATHF  LFORCE 03'
      MPATH = LFORCE
 4040 CONTINUE
C-----CHANGE THE DESIRED OUTBOUND APPROACH FOR THE VEHICLE
C[    IF ( LOBAPD             .EQ.-2147483647   )STOP 'PATHF  LOBAPD 08'
C[    IF ( MPATH              .EQ.-2147483647   )STOP 'PATHF  MPATH  01'
      WRITE (WRNMSG,702) IQ(IV),IA,TIME,IOA(MPATH),LOBAPD,CSUB
      CALL  PRTWRN  ( WRNMSG )
      IF ( IQ(IV) . LT . 0 )                     THEN
        CALL  VDIERR  ( TIME,-IQ(IV),VDIFDO )
      END IF
      NOBAPD(IV) = IOA(MPATH)
      IF ( IAFLAG(NOBAPD(IV)) . EQ . ILETTI )    THEN
        NOBAPD(IV) = INTLNK(NOBAPD(IV))
C-----  CHECK EACH LANE OF THE NOBAPD APPROACH FOR AN INTERSECTION PATH
C-----  INITIALIZE THE FORCED PATH TO NOT SET
C-----  LPTPNS = NOT SET
        LFORCE = 0
        LFTYPE = LPTPNS
        DO 4060  II = 1 , NLANES(NOBAPD(IV))
        ILANE = LLANES(II,NOBAPD(IV))
C-----  IF THERE ARE NO INTERSECTION PATHS FROM LANE ILANE THEN GO TO
C-----  4060 AND CHECK THE NEXT LANE
                  IF ( NPINT(ILANE) . LE . 0 )   GO TO 4060
C-----  CHECK EACH INTERSECTION PATH FROM LANE ILANE
        DO 4050  I = 1 , NPINT(ILANE)
        LPATH = LINTP(I,ILANE)
C-----  IF THE INTERSECTION PATH SHOULD BE AVOIDED THEN GO TO 4050 AND
C-----  CHECK THE NEXT INTERSECTION PATH
        IF ( (PAVOID.GT.0).AND.(LPATH.EQ.PAVOID) )
     *                                           GO TO 4050
C-----  IF THE INTERSECTION PATH DOES NOT ALLOW THE VEHICLE TYPE THEN
C-----  GO TO 4050 AND CHECK THE NEXT INTERSECTION PATH
        IF ( IAND( VEHTYP(IV),PAVT(LPATH) ) . EQ . 0 )
     *                                           GO TO 4050
        CALL  PTYPE   ( LFORCE,LFTYPE,LOBAPD,LPATH,LPTYPE )
C-----  END OF INTERSECTION PATH LOOP
 4050   CONTINUE
C-----  END OF LANE LOOP
 4060   CONTINUE
                  IF ( LFORCE . EQ . 0 )         GO TO 9690
        NOBAPD(IV) = IOA(LFORCE)
      END IF
      GO TO 5040
 5010 CONTINUE
                    IF ( (.NOT. DIAMON) )        RETURN
C[    IF ( LPATH              .EQ.-2147483647   )STOP 'PATHF  LPATH  02'
                    IF (      LPATH  . EQ . 0 )  RETURN
                    IF ( LOBL(LPATH) . EQ . 0 )  RETURN
      JA = ISNA(LOBL(LPATH))
            IF ( IAFLAG(JA) . NE . ILETTI )      RETURN
C-----FOR THE DIAMOND AND WHEN THE LINKING APPROACH FOR THE PATH IS AN
C-----INTERNAL APPROACH FIND ANY SECOND INTERSECTION PATH THAT GOES FROM
C-----THE LINKING APPROACH TO THE VEHICLES DESIRED OUTBOUND APPROACH SO
C-----THE TURN CODE AT THE SECOND INTERSECTION CAN BE FOUND
C-----LPTPNS = NOT SET
      IP2 = 0
      LFORCE = 0
      LFTYPE = LPTPNS
      LOBAPD = IABS( NOBAPD(IV) )
      DO 5030  II = 1 , NLANES(JA)
      ILANE = LLANES(II,JA)
                    IF ( NPINT(ILANE) . LE . 0 ) GO TO 5030
      DO 5020  I = 1 , NPINT(ILANE)
      MPATH = LINTP(I,ILANE)
C-----IF THE INTERSECTION PATH SHOULD BE AVOIDED THEN GO TO 5020 AND
C-----CHECK THE NEXT INTERSECTION PATH
      IF ( (PAVOID.GT.0).AND.(MPATH.EQ.PAVOID) ) GO TO 5020
C-----IF THE INTERSECTION PATH DOES NOT ALLOW THE VEHICLE TYPE THEN GO
C-----TO 5020 AND CHECK THE NEXT INTERSECTION PATH
      IF ( IAND( VEHTYP(IV),PAVT(MPATH) ).EQ.0 ) GO TO 5020
      CALL  PTYPE   ( LFORCE,LFTYPE,LOBAPD,MPATH,MPTYPE )
C[    IF ( MPTYPE             .EQ.-2147483647   )STOP 'PATHF  MPTYPE 02'
C-----LPTP01 = RAD OK  NO LANE CHANGE WITHIN THE INTERSECTION TO LOBAPD
                    IF ( MPTYPE . NE . LPTP01 )  GO TO 5020
      IP2 = MPATH
C[    IF ( LPATH              .EQ.-2147483647   )STOP 'PATHF  LPATH  03'
            IF ( LOBL(LPATH) . EQ . LIBL(IP2) )  GO TO 5040
 5020 CONTINUE
 5030 CONTINUE
C[    IF ( LFORCE             .EQ.-2147483647   )STOP 'PATHF  LFORCE 04'
C[    IF ( LFTYPE             .EQ.-2147483647   )STOP 'PATHF  LFTYPE 01'
C-----LPTP01 = RAD OK  NO LANE CHANGE WITHIN THE INTERSECTION TO LOBAPD
C-----LPTP02 = RAD OK     LANE CHANGE WITHIN THE INTERSECTION TO LOBAPD
C-----LPTP03 = RAD OK  NO LANE CHANGE WITHIN THE INTERSECTION STRAIGHT
C-----LPTP04 = RAD OK     LANE CHANGE WITHIN THE INTERSECTION STRAIGHT
C-----LPTP05 = RAD OK  NO LANE CHANGE WITHIN THE INTERSECTION ANY PATH
C-----LPTP06 = RAD OK     LANE CHANGE WITHIN THE INTERSECTION ANY PATH
                    IF ( LFTYPE . LE . LPTP06 )  IP2 = LFORCE
C[    IF ( IP2                .EQ.-2147483647   )STOP 'PATHF  IP2    01'
                    IF ( IP2 . EQ . 0 )          GO TO 9270
      MPATH = IP2
 5040 CONTINUE
C[    IF ( MPATH              .EQ.-2147483647   )STOP 'PATHF  MPATH  02'
      INT2P(IV) = MPATH
      RETURN
C-----PROCESS THE EXECUTION ERRORS AND STOP
 9160 CONTINUE
      CALL  ABORTR  ( 'STOP 916 - '                           //
     *                'NO PATHS FROM LANE FOR FORCED PATH - ' //
     *                'PATHF'                                    )
      STOP  916
 9270 CONTINUE
      CALL  ABORTR  ( 'STOP 927 - '                        //
     *                'NO PATH FOR SECOND INTERSECTION - ' //
     *                'PATHF'                                 )
      STOP  927
 9310 CONTINUE
      CALL  ABORTR  ( 'STOP 931 - '                              //
     *                'NO PATHS FROM APPROACH FOR NEW NOBAPD - ' //
     *                'PATHF'                                       )
      STOP  931
 9690 CONTINUE
      CALL  ABORTR  ( 'STOP 969 - '                    //
     *                'NO FORCED PATH FOR APPROACH - ' //
     *                'PATHF'                             )
      STOP  969
      END                                                               PATHF
C
C
C
      SUBROUTINE CHKMLN
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'LANE'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      CHARACTER*3       BEGEND
      INTEGER           ILL,ILR,JLN,MPRES
  601 FORMAT('** BLOCK ZONE INTRUSION AT T=',F7.2,' SECS VEH=',I6,
     *       ' APPR=',I2,' LANE=',I1,
     *       ' - VEHICLE IN BLOCKED PART AT ',A3,' OF LANE',8X,
     *       '** INTRUSION **')
C
C-----SUBROUTINE CHKMLN CHECKS MY LANE AND IF BLOCKED THEN SETS
C-----PARAMETERS FOR BLOCKED LANE
C
C[    BEGEND     = '~~~'
C[    JLN        = -2147483647
C[    MPRES      = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'CHKMLN'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----INITIALIZE THE LANE NOT BLOCKED
      MBLOCK(IV) = .FALSE.
      MPRES = LPRES(IV)
C-----IF THE LANE IS CONTINUOUS THEN RETURN (NOT BLOCKED)
      IF ( LGEOM(2,MPRES).EQ.LGEOM(4,MPRES) )    RETURN
C-----IF THE LANE ONLY EXISTS IN THE FIRST PART THEN GO TO 2010 AND
C-----CHECK A LANE THAT ONLY EXISTS IN THE FIRST PART OR A LANE THAT IS
C-----ONLY BLOCKED IN THE MIDDLE
      IF ( LGEOM(1,MPRES).NE.LGEOM(2,MPRES) )    GO TO 2010
 1010 CONTINUE
C-----THE LANE ONLY EXISTS IN THE LAST PART OR THE LANE IS ONLY BLOCKED
C-----IN THE MIDDLE AND THE VEHICLE IS SUPPOSED TO BE IN THE LAST PART
C-----THUS IF THE VEHICLES NEW POSITION IS GE THE START OF THE LAST PART
C-----THEN RETURN (NOT BLOCKED) ELSE THE VEHICLE IS IN THE BLOCKED
C-----PORTION OF THE LANE
      IF ( POSNEW .GE.  DBLE( LGEOM(3,MPRES) ) ) RETURN
      IF ( POSNEW .GE. (DBLE( LGEOM(3,MPRES) )-LENVAP) )
     *                                           THEN
        MBLOCK(IV) = .TRUE.
        BEGEND = 'END'
        GO TO 3010
      END IF
      GO TO 9170
 2010 CONTINUE
C-----THE LANE ONLY EXISTS IN THE FIRST PART OR THE LANE IS ONLY BLOCKED
C-----IN THE MIDDLE THUS IF THE LANE IS ONLY BLOCKED IN THE MIDDLE
C-----AND IF THE VEHICLES NEW POSITION IS GE THE CENTER OF THE BLOCKED
C-----PORTION OF THE LANE THEN GO TO 1010 AND CHECK FOR THE VEHICLE
C-----BEING IN THE LAST PART OF THE LANE
      IF ( (LGEOM(3,MPRES).NE. LGEOM(4,MPRES)                  ) . AND .
     *     (POSNEW        .GE.
     *              0.5D0*DBLE( LGEOM(2,MPRES)+LGEOM(3,MPRES) )) )
     *                                           GO TO 1010
C-----THE LANE ONLY EXISTS IN THE FIRST PART OR THE LANE IS ONLY BLOCKED
C-----IN THE MIDDLE AND THE VEHICLE IS SUPPOSED TO BE IN THE FIRST PART
C-----THUS BLOCKED LANE
      MBLOCK(IV) = .TRUE.
      MFINL (IV) = .FALSE.
C-----IF THE PREVIOUS VEHICLES POSITION IS GT THE END OF THE FIRST PART
C-----OF THE LANE THEN THIS VEHICLE IS THE FIRST VEHICLE IN THE FIRST
C-----PART OF THE LANE
      IF ( PVPOS . GE . DBLE( LGEOM(2,MPRES) ) ) MFINL(IV) = .TRUE.
C-----IF THE VEHICLE IS LANE CHANGING THEN GO TO 2020 AND CHECK
C-----POSITION
                    IF ( ISET(IV) . EQ . 1 )     GO TO 2020
C-----SET WHICH SIDE THE VEHICLE SHOULD CHANGE LANES INTO
      ILL = NLL(MPRES)
      ILR = NLR(MPRES)
C-----IF THERE IS A LANE TO THE LEFT AND IF THE VEHICLE IS NOT ALLOWED
C-----ON THE LANE TO THE LEFT THEN SET NO LANE TO THE LEFT
      IF ( ILL . GT . 0 )                        THEN
        IF ( IAND( VEHTYP(IV),LAVT(ILL) ).EQ.0 ) ILL = 0
      END IF
C-----IF THERE IS A LANE TO THE RIGHT AND IF THE VEHICLE IS NOT ALLOWED
C-----ON THE LANE TO THE RIGHT THEN SET NO LANE TO THE RIGHT
      IF ( ILR . GT . 0 )                        THEN
        IF ( IAND( VEHTYP(IV),LAVT(ILR) ).EQ.0 ) ILR = 0
      END IF
      IF ( (ILL.EQ.0) . AND . (ILR.EQ.0) )       GO TO 9180
C-----CHECK IF ISET CAN BE SET TO 5
      CALL  CKISET  ( IV,POSNEW )
      IF ( (ILL.GT.0) . AND . (ILR.GT.0) )       THEN
        IF ( ( LGEOM(2,ILL) . EQ . LGEOM(4,ILL) ) . AND .
     *       ( LGEOM(2,ILR) . EQ . LGEOM(4,ILR) ) . AND .
     *       ( LNEXT(IV)    . GT . 0            ) )
     *                                           THEN
C-----    NLL AND NLR EXIST, NLL AND NLR CONTINUOUS THUS U-TURNS AND
C-----    LEFTS GO LEFT AND STRAIGHTS AND RIGHTS GO RIGHT
C-----    ITURN(IV) = U=1 L=2 S=3 R=4
          IF ( ITURN(IV) . LE . ITURNL )         THEN
            LEGAL(IV) = 1
          ELSE
            LEGAL(IV) = 3
          END IF
          GO TO 2020
        END IF
        IF ( LGEOM(2,ILL) . EQ . LGEOM(4,ILL) )  THEN
C-----    NLL AND NLR EXIST, NLL CONTINUOUS THUS CHANGE LEFT
          LEGAL(IV) = 1
          GO TO 2020
        END IF
        IF ( LGEOM(2,ILR) . EQ . LGEOM(4,ILR) )  THEN
C-----    NLL AND NLR EXIST, NLR CONTINUOUS THUS CHANGE RIGHT
          LEGAL(IV) = 3
          GO TO 2020
        END IF
        IF ( ( LGEOM(2,ILL) . GT . LGEOM(2,MPRES ) ) . AND .
     *       ( LGEOM(2,ILR) . GT . LGEOM(2,MPRES ) ) )
     *                                           THEN
C-----    NLL AND NLR EXIST, NEITHER CONTINUOUS, NLL AND NLR LONGER, USE
C-----    LONGEST
          IF ( LGEOM(2,ILL) . GT . LGEOM(2,ILR) )THEN
C-----      NLL AND NLR EXIST, NEITHER CONTINUOUS, NLL LONGER, USE LEFT
            LEGAL(IV) = 1
          ELSE
C-----      NLL AND NLR EXIST, NEITHER CONTINUOUS, NLR LONGER, USE RIGHT
            LEGAL(IV) = 3
          END IF
          GO TO 2020
        END IF
        IF ( LGEOM(2,ILL) . GT . LGEOM(2,MPRES ) )
     *                                           THEN
C-----    NLL AND NLR EXIST, NEITHER CONTINUOUS, NLL LONGER, USE LEFT
          LEGAL(IV) = 1
          GO TO 2020
        END IF
        IF ( LGEOM(2,ILR) . GT . LGEOM(2,    MPRES ) )
     *                                           THEN
C-----    NLL AND NLR EXIST, NEITHER CONTINUOUS, NLR LONGER, USE RIGHT
          LEGAL(IV) = 3
          GO TO 2020
        END IF
        GO TO 9180
      ELSE
C-----  NLL OR NLR EXIST BUT NOT BOTH THUS CHANGE TO LANE WHICH EXIST
        LEGAL(IV) = 1
                    IF ( ILR . GT . 0 )          LEGAL(IV) = 3
      END IF
 2020 CONTINUE
C-----IF THE VEHICLES NEW POSITION IS LT THE END OF THE FIRST PART THEN
C-----RETURN (BLOCKED LANE) ELSE THE VEHICLE IS IN THE BLOCKED PORTION
C-----OF THE LANE
      IF ( POSNEW .LT.  DBLE( LGEOM(2,MPRES) ) ) RETURN
      IF ( POSNEW .LT. (DBLE( LGEOM(2,MPRES) )+LENVAP) )
     *                                           THEN
        BEGEND = 'BEG'
        GO TO 3010
      END IF
      GO TO 9170
 3010 CONTINUE
C     DO 3020  JLN = 1 , NLANES(IA)
C     IF ( MPRES . EQ . LLANES(JLN,IA) )         GO TO 3030
C3020 CONTINUE
C     JLN = 9
C3030 CONTINUE
      JLN = ISNL(MPRES)
C-----PRINT WARNING MESSAGE FOR BLOCKED ZONE INTRUSION AT BEG/END
C[    IF ( BEGEND             .EQ.'~~~' )        STOP 'CHKMLN BEGEND 01'
C[    IF ( JLN                .EQ.-2147483647   )STOP 'CHKMLN JLN    01'
      WRITE (WRNMSG,601) TIME,IQ(IV),IA,JLN,BEGEND
      CALL  PRTWRN  ( WRNMSG )
      IF ( IQ(IV) . LT . 0 )                     THEN
        CALL  VDIERR  ( TIME,-IQ(IV),VDIBZI )
      END IF
C-----RETURN (LANE BLOCKED)
      RETURN
C-----PROCESS THE EXECUTION ERRORS AND STOP
 9170 CONTINUE
      CALL  ABORTR  ( 'STOP 917 - '                      //
     *                'LANE DOES NOT EXIST AT POSNEW - ' //
     *                'CHKMLN'                              )
      STOP  917
 9180 CONTINUE
      CALL  ABORTR  ( 'STOP 918 - '                             //
     *                'NO LANE ALTERNATIVE FOR BLOCKED LANE - ' //
     *                'CHKMLN'                                     )
      STOP  918
      END                                                               CHKMLN
C
C
C
      SUBROUTINE BANGS  ( IWHERE,JVPV,VELREL,POSREL,PADD )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'SUMST2'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      CHARACTER*7       SVMSDN
      CHARACTER*12      IAFORM,IPFORM
      LOGICAL           ISAME,PRECOL
      INTEGER           I,IDESPD,ISIG,IWHERE,JA,JAN,JBAPS,JDRICL,JL,JLN,
     *                  JP,JPRC,JSET,JSIG,JTURN,JV,JVEHCL,JVMSAP,JVMSLB,
     *                  JVMSLE,JVMSMG,JVMSMT,JVMSVN,JVPV,K,KP,KPRC,
     *                  KPRTM,KSPD,MCHGE,MEGAL,MLANES,MNEXT,MOBAPD,MOF,
     *                  MOGFLG,MOR,MORC,MPRES
      DOUBLE PRECISION  ACCVEH,DESSPD,DISEND,DISLCH,DSPLCH,PADD,POSEND,
     *                  POSL,POSLAT,POSREL,POSVAL,POSVEH,RVMSAT,RVMSDM,
     *                  RVMSDP,RVMSMP,RVMSPB,RVMSPE,RVMSST,SLPVEH,
     *                  VELLCH,VEHLNG,VELREL,VELVEH
      DATA     IAFORM / ' AP LN VLPOS' /
      DATA     IPFORM / ' PATH  VSCON' /
  601 FORMAT(/,
     *       ' ** CLEAR ZONE INTRUSION AT T=',F7.2,' SECS VEH=',I6,
     *       ' APPR=',I2,' LANE=',I1,' - ON VEH=',I6,' VELREL=',F6.2,
     *       ' POSREL=',F7.2,8X,'** INTRUSION **')
  602 FORMAT(/,
     *       ' ** CLEAR ZONE INTRUSION AT T=',F7.2,' SECS VEH=',I6,
     *       ' INTER-PATH=',I3,' - ON VEH=',I6,' VELREL=',F6.2,
     *       ' POSREL=',F7.2,8X,'** INTRUSION **')
  603 FORMAT(A,41HEH  NUM NOF NOR NRC VEHPOS VEHVEL VEHACC ,
     *       37HACCSLP DS VC DC NX OA ST LG LF LC PR ,A,5H   SG,
     *       12H ITURN IBAPS)
  604 FORMAT(2I3,I4,I6,3I4,2F7.2,2F7.3,10I3,F5.1,I5,2I6)
  605 FORMAT(I4 ,I6,I6,3I4,2F7.2,2F7.3,10I3, I5 ,I5,2I6)
  606 FORMAT(A,' at TIME=',F7.2,' seconds VEH=',I6,' and VEH=',I6)
C
C-----SUBROUTINE BANGS PRINTS THE COLLISION INFORMATION AND RESETS THE
C-----VEHICLES POS/VEL/ACC
C
C[    IDESPD     = -2147483647
C[    ISIG       = -2147483647
C[    JA         = -2147483647
C[    JBAPS      = -2147483647
C[    JDRICL     = -2147483647
C[    JLN        = -2147483647
C[    JP         = -2147483647
C[    JSET       = -2147483647
C[    JSIG       = -2147483647
C[    JTURN      = -2147483647
C[    JVEHCL     = -2147483647
C[    KPRTM      = -2147483647
C[    KSPD       = -2147483647
C[    MCHGE      = -2147483647
C[    MEGAL      = -2147483647
C[    MLANES     = -2147483647
C[    MNEXT      = -2147483647
C[    MOBAPD     = -2147483647
C[    MOF        = -2147483647
C[    MOGFLG     = -2147483647
C[    MOR        = -2147483647
C[    MORC       = -2147483647
C[    MPRES      = -2147483647
C[    POSL       = -2147483647.0
C[    POSLAT     = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'BANGS'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----DISMISS COLLISION IF JVPV IS ZERO
                    IF ( JVPV . EQ . 0 )         RETURN
C-----SKIP PRINTOUT IF COLLISION PREVIOUSLY DETECTED AND PRINTED
      DO 1002  K = 1 , NVMCOL(IV)
            IF ( IVMCOL(K,IV  ) . EQ . JVPV )    GO TO 1004
 1002 CONTINUE
      GO TO 1008
 1004 CONTINUE
      DO 1006  K = 1 , NVMCOL(JVPV)
            IF ( IVMCOL(K,JVPV) . EQ . IV   )    RETURN
 1006 CONTINUE
 1008 CONTINUE
C-----INITIALIZE SOME PARAMETERS FOR BANGS
      MPRES = LPRES(JVPV)
      ISIG  = 0
      JSIG  = 0
      ISAME = .TRUE.
      IF ( MININT(JVPV) )                        THEN
        JA     = 0
        JL     = 0
        JLN    = 0
        MLANES = 0
        JP     = MPRES
      ELSE
        JL     = MPRES
        JA     = ISNA(JL)
        JLN    = ISNL(JL)
        MLANES = NLANES(JA)
        JP     = 0
C       DO 2020  JLN = 1 , MLANES
C       JL = LLANES(JLN,JA)
C                   IF ( JL . EQ . MPRES )       GO TO 4010
C2020   CONTINUE
C       GO TO 9190
      END IF
      POSVEH = PVPOS
      VELVEH = PVVEL
      ACCVEH = PVACC
      SLPVEH = PVSLP
C-----PROCESS COLLISION BASED ON THE LOCATION OF THE REAR VEHICLE
C-----        IBAP INTR OBAP CHKCOL CHKCOL
      GO TO ( 1010,2010,3010,2020,  2020 ) , IWHERE
 1010 CONTINUE
C
C-----PROCESS VEHICLE ON INBOUND APPROACH
C
C-----DISMISS COLLISION IF JVPV IS NOT THE VEHICLE AHEAD
      IF ( NOF(IV) . EQ . 0 )                    THEN
        IF ( LNEXT(IV) . EQ . 0 )                THEN
          RETURN
        ELSE
          IF ( ILVP(LNEXT(IV)) . EQ . 0 )        THEN
            RETURN
          ELSE
            IF ( JVPV . NE . ILVP(LNEXT(IV)) )   THEN
              RETURN
            END IF
          END IF
        END IF
      ELSE
        IF ( JVPV . NE . NOF(IV) )               THEN
          RETURN
        END IF
      END IF
      IF ( LNEXT(IV) . EQ . 0 )                  THEN
        JPRC = 1
        KPRC = 1
      ELSE
        IF      ( IPRC(1,IV).EQ.LNEXT(IV) )      THEN
          JPRC = 1
        ELSE IF ( IPRC(2,IV).EQ.LNEXT(IV) )      THEN
          JPRC = 2
        ELSE IF ( IPRC(1,IV) . EQ .  0 )         THEN
          JPRC = 1
          NORC(1,IV) = NVEP1
        ELSE IF ( IPRC(2,IV) . EQ .  0 )         THEN
          JPRC = 2
          NORC(2,IV) = NVEP1
        ELSE
          GO TO 9460
        END IF
        IF ( MININT(JVPV) )                      THEN
          KP = LPRES(JVPV)
        ELSE
          IF ( LPREV(JVPV) . EQ . 0 )            THEN
            KP = LNEXT(JVPV)
          ELSE
            KP = LPREV(JVPV)
          END IF
        END IF
        IF      ( IPRC(1,JVPV) . EQ . KP )       THEN
          KPRC = 1
        ELSE IF ( IPRC(2,JVPV) . EQ . KP )       THEN
          KPRC = 2
        ELSE IF ( IPRC(1,JVPV) . EQ .  0 )       THEN
          KPRC = 1
          NORC(1,JVPV) = NVEP1
        ELSE IF ( IPRC(2,JVPV) . EQ .  0 )       THEN
          KPRC = 2
          NORC(2,JVPV) = NVEP1
        ELSE
          GO TO 9460
        END IF
      END IF
C-----PRINT THE TIME INTO THE SIMULATION AND THE VEHICLES INVOLVED IN
C-----THE COLLISION (THE FRONT VEHICLE IS THE PREVIOUS VEHICLE AND THE
C-----REAR VEHICLE IS THIS VEHICLE)
      WRITE (SER,601) TIME,IQ(IV),IA,ILN,IQ(JVPV),VELREL,POSREL
C4    WRITE (6  ,601) TIME,IQ(IV),IA,ILN,IQ(JVPV),VELREL,POSREL
C-----THE REAR VEHICLE WAS ON AN INBOUND APPROACH THUS SET THE SIGNAL
C-----INDICATION FOR THE REAR VEHICLE AND THE FLAG THAT THE FRONT
C-----VEHICLE IS NOT ON THE SAME LINK AS THE REAR VEHICLE
      ISIG = ISISET(ICAMPC,IBLN(IL))
      ISAME = .FALSE.
C-----IF THE FRONT VEHICLE IS IN THE INTERSECTION THEN GO TO 4010 AND
C-----PRINT THE FRONT VEHICLE INFORMATION
                    IF ( MININT(JVPV) )          GO TO 4010
C-----THE FRONT VEHICLE WAS ALSO ON THE INBOUND APPROACH THUS SET THE
C-----SIGNAL INDICATION FOR THE REAR VEHICLE AND THE FLAG THAT THE FRONT
C-----VEHICLE IS ON THE SAME LINK AS THE REAR VEHICLE AND GO TO 4010 AND
C-----PRINT THE FRONT VEHICLE INFORMATION
      JSIG = ISIG
      ISAME = .TRUE.
      GO TO 4010
 2010 CONTINUE
C
C-----PROCESS VEHICLE ON INTERSECTION PATH
C
C-----DISMISS COLLISION IF JVPV IS NOT THE VEHICLE AHEAD
      IF ( NOF(IV) . EQ . 0 )                    THEN
        IF ( LOBL(IP) . EQ . 0 )                 RETURN
        IF ( ILVL(LOBL(IP)) . EQ . 0 )           RETURN
        IF ( JVPV . NE . ILVL(LOBL(IP)) )        RETURN
        IF ( MININT(JVPV) )                      THEN
C-----    DISMISS THE COLLISION IF THE OTHER VEHICLE IS IN THE
C-----    INTERSECTION AND THEY ARE ON DIFFERENT INTERSECTION PATHS
C-----    (CHKCOL WILL DETECT THESE TYPE OF COLLISIONS)
          IF ( LPRES(IV).NE.LPRES(JVPV) )        RETURN
        ELSE
C-----    DISMISS THE COLLISION IF THE OTHER VEHICLE HAS NOT CLEARED
C-----    INTERSECTION CONFLICTS (CHKCOL WILL DETECT THESE TYPE OF
C-----    COLLISIONS)
          IF ( LPREV(JVPV) . GT . 0   )          THEN
            IF ( ISTCON(JVPV) . LE .
     *           NGEOCP(LPREV(JVPV)) )           RETURN
          END IF
        END IF
      ELSE
        IF ( JVPV . NE . NOF(IV) )               RETURN
      END IF
 2020 CONTINUE
      IF ( LPRES(IV) . EQ . 0 )                  THEN
        JPRC = 1
        KPRC = 1
      ELSE
        IF      ( IPRC(1,IV).EQ.LPRES(IV) )      THEN
          JPRC = 1
        ELSE IF ( IPRC(2,IV).EQ.LPRES(IV) )      THEN
          JPRC = 2
        ELSE IF ( IPRC(1,IV) . EQ .  0 )         THEN
          JPRC = 1
          NORC(1,IV) = NVEP1
        ELSE IF ( IPRC(2,IV) . EQ .  0 )         THEN
          JPRC = 2
          NORC(2,IV) = NVEP1
        ELSE
          GO TO 9460
        END IF
        IF ( MININT(JVPV) )                      THEN
          KP = LPRES(JVPV)
        ELSE
          IF ( LPREV(JVPV) . EQ . 0 )            THEN
            KP = LNEXT(JVPV)
          ELSE
            KP = LPREV(JVPV)
          END IF
        END IF
        IF      ( IPRC(1,JVPV) . EQ . KP )       THEN
          KPRC = 1
        ELSE IF ( IPRC(2,JVPV) . EQ . KP )       THEN
          KPRC = 2
        ELSE IF ( IPRC(1,JVPV) . EQ .  0 )       THEN
          KPRC = 1
          NORC(1,JVPV) = NVEP1
        ELSE IF ( IPRC(2,JVPV) . EQ .  0 )       THEN
          KPRC = 2
          NORC(2,JVPV) = NVEP1
        ELSE
          GO TO 9460
        END IF
      END IF
      IF ( IWHERE . GE . 4 )                     THEN
        CALL  SPVAS   ( JVPV,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                  .FALSE.,.FALSE.,.FALSE.,.TRUE.    )
        POSVEH = POSVEH + PADD
      END IF
C-----PRINT THE TIME INTO THE SIMULATION AND THE VEHICLES INVOLVED IN
C-----THE COLLISION (THE FRONT VEHICLE IS THE PREVIOUS VEHICLE AND THE
C-----REAR VEHICLE IS THIS VEHICLE)
      WRITE (SER,602) TIME,IQ(IV),IP,IQ(JVPV),VELREL,POSREL
C4    WRITE (6  ,602) TIME,IQ(IV),IP,IQ(JVPV),VELREL,POSREL
C-----THE REAR VEHICLE WAS IN THE INTERSECTION THUS IF THE FRONT VEHICLE
C-----WAS ALSO IN THE INTERSECTION THEN GO TO 4010 AND PRINT THE FRONT
C-----VEHICLE INFORMATION ELSE FIND THE OUTBOUND APPROACH INFORMATION
C-----FOR THE FRONT VEHICLE AND GO TO 4010 AND PRINT THE FRONT VEHICLE
C-----INFORMATION
                    IF ( MININT(JVPV) )          GO TO 4010
      ISAME = .FALSE.
      GO TO 4010
 3010 CONTINUE
C
C-----PROCESS VEHICLE ON OUTBOUND APPROACH
C
C-----DISMISS COLLISION IF JVPV IS NOT THE VEHICLE AHEAD
      IF ( NOF(IV) . EQ . 0 )                    THEN
        RETURN
      ELSE
        IF ( JVPV . NE . NOF(IV) )               THEN
          RETURN
        END IF
      END IF
      IF ( LPREV(IV) . EQ . 0 )                  THEN
        JPRC = 1
        KPRC = 1
      ELSE
        IF      ( IPRC(1,IV) . EQ . LPREV(IV) )  THEN
          JPRC = 1
        ELSE IF ( IPRC(2,IV) . EQ . LPREV(IV) )  THEN
          JPRC = 2
        ELSE IF ( IPRC(1,IV) . EQ . 0           )THEN
          JPRC = 1
          NORC(1,IV) = NVEP1
        ELSE IF ( IPRC(2,IV) . EQ . 0           )THEN
          JPRC = 2
          NORC(2,IV) = NVEP1
        ELSE
          GO TO 9460
        END IF
        IF ( MININT(JVPV) )                      THEN
          KP = LPRES(JVPV)
        ELSE
          IF ( LPREV(JVPV) . EQ . 0 )            THEN
            KP = LNEXT(JVPV)
          ELSE
            KP = LPREV(JVPV)
          END IF
        END IF
        IF      ( IPRC(1,JVPV) . EQ . KP )       THEN
          KPRC = 1
        ELSE IF ( IPRC(2,JVPV) . EQ . KP )       THEN
          KPRC = 2
        ELSE IF ( IPRC(1,JVPV) . EQ .  0 )       THEN
          KPRC = 1
          NORC(1,JVPV) = NVEP1
        ELSE IF ( IPRC(2,JVPV) . EQ .  0 )       THEN
          KPRC = 2
          NORC(2,JVPV) = NVEP1
        ELSE
          GO TO 9460
        END IF
      END IF
C-----PRINT THE TIME INTO THE SIMULATION AND THE VEHICLES INVOLVED IN
C-----THE COLLISION (THE FRONT VEHICLE IS THE PREVIOUS VEHICLE AND THE
C-----REAR VEHICLE IS THIS VEHICLE)
      WRITE (SER,601) TIME,IQ(IV),IA,ILN,IQ(JVPV),VELREL,POSREL
C4    WRITE (6  ,601) TIME,IQ(IV),IA,ILN,IQ(JVPV),VELREL,POSREL
C-----THE REAR VEHICLE WAS ON THE OUTBOUND APPROACH THUS THE FRONT
C-----VEHICLE MUST BE ON THE OUTBOUND APPROACH ALSO
 4010 CONTINUE
C-----FIND THE INFORMATION FOR THE FRONT VEHICLE
      MOF    = NOF   (JVPV)
      MOR    = NOR   (JVPV)
      MORC   = NORC  (KPRC,JVPV)
      CALL  SETDSP  ( JVPV,IPOS(JVPV),DBLE( ISPD(JVPV) ),.FALSE.,
     *                DESSPD                                      )
      KSPD   = IDNINT( DESSPD )
      JVEHCL = IVEHCL(JVPV)
      JDRICL = IDRICL(JVPV)
      MNEXT  = LNEXT (JVPV)
      MOBAPD = IABS( NOBAPD(JVPV) )
      JSET   = ISET  (JVPV)
      MEGAL  = LEGAL (JVPV)
      MOGFLG = LOGFLG(JVPV)
      MCHGE  = LCHGE (JVPV)
      KPRTM  = IPRTM (JVPV)
      JTURN  = ITURN (JVPV)
      JBAPS  = IBAPS (JVPV)
C-----IF THE FRONT VEHICLE WAS IN THE INTERSECTION THEN GO TO 4020 AND
C-----PRINT THE INTERSECTION INFORMATION FOR THE FRONT VEHICLE ELSE
C-----PRINT THE INBOUND/OUTBOUND APPROACH INFORMATION FOR THE FRONT
C-----VEHICLE
                    IF ( MININT(JVPV) )          GO TO 4020
      POSL = LATPOS(JVPV)
                    IF ( JSET . NE . 1 )         POSL = 0.0D0
      WRITE (SER,603) IAFORM(1:8),IAFORM(9:12)
C4    WRITE (6  ,603) IAFORM(1:8),IAFORM(9:12)
C[    IF ( JBAPS              .EQ.-2147483647   )STOP 'BANGS  JBAPS  01'
C[    IF ( JDRICL             .EQ.-2147483647   )STOP 'BANGS  JDRICL 01'
C[    IF ( JSET               .EQ.-2147483647   )STOP 'BANGS  JSET   01'
C[    IF ( JTURN              .EQ.-2147483647   )STOP 'BANGS  JTURN  01'
C[    IF ( JVEHCL             .EQ.-2147483647   )STOP 'BANGS  JVEHCL 01'
C[    IF ( KPRTM              .EQ.-2147483647   )STOP 'BANGS  KPRTM  01'
C[    IF ( KSPD               .EQ.-2147483647   )STOP 'BANGS  KSPD   01'
C[    IF ( MCHGE              .EQ.-2147483647   )STOP 'BANGS  MCHGE  01'
C[    IF ( MEGAL              .EQ.-2147483647   )STOP 'BANGS  MEGAL  01'
C[    IF ( MNEXT              .EQ.-2147483647   )STOP 'BANGS  MNEXT  01'
C[    IF ( MOBAPD             .EQ.-2147483647   )STOP 'BANGS  MOBAPD 01'
C[    IF ( MOF                .EQ.-2147483647   )STOP 'BANGS  MOF    01'
C[    IF ( MOGFLG             .EQ.-2147483647   )STOP 'BANGS  MOGFLG 01'
C[    IF ( MOR                .EQ.-2147483647   )STOP 'BANGS  MOR    01'
C[    IF ( MORC               .EQ.-2147483647   )STOP 'BANGS  MORC   01'
      WRITE (SER,604) JA,JLN,JVPV,IQ(JVPV),MOF,MOR,MORC,POSVEH,VELVEH,
     *                ACCVEH,SLPVEH,KSPD,JVEHCL,JDRICL,MNEXT,MOBAPD,
     *                JSET,MEGAL,MOGFLG,MCHGE,KPRTM,POSL,JSIG,JTURN,
     *                JBAPS
C4    WRITE (6  ,604) JA,JLN,JVPV,IQ(JVPV),MOF,MOR,MORC,POSVEH,VELVEH,
C4   *                ACCVEH,SLPVEH,KSPD,JVEHCL,JDRICL,MNEXT,MOBAPD,
C4   *                JSET,MEGAL,MOGFLG,MCHGE,KPRTM,POSL,JSIG,JTURN,
C4   *                JBAPS
      GO TO 5010
 4020 CONTINUE
C-----THE FRONT VEHICLE WAS IN THE INTERSECTION THUS PRINT THE
C-----INTERSECTION INFORMATION FOR THE FRONT VEHICLE
      WRITE (SER,603) IPFORM(1:8),IPFORM(9:12)
C4    WRITE (6  ,603) IPFORM(1:8),IPFORM(9:12)
C[    IF ( JBAPS              .EQ.-2147483647   )STOP 'BANGS  JBAPS  02'
C[    IF ( JDRICL             .EQ.-2147483647   )STOP 'BANGS  JDRICL 02'
C[    IF ( JSET               .EQ.-2147483647   )STOP 'BANGS  JSET   02'
C[    IF ( JTURN              .EQ.-2147483647   )STOP 'BANGS  JTURN  02'
C[    IF ( JVEHCL             .EQ.-2147483647   )STOP 'BANGS  JVEHCL 02'
C[    IF ( KPRTM              .EQ.-2147483647   )STOP 'BANGS  KPRTM  02'
C[    IF ( KSPD               .EQ.-2147483647   )STOP 'BANGS  KSPD   02'
C[    IF ( MCHGE              .EQ.-2147483647   )STOP 'BANGS  MCHGE  02'
C[    IF ( MEGAL              .EQ.-2147483647   )STOP 'BANGS  MEGAL  02'
C[    IF ( MNEXT              .EQ.-2147483647   )STOP 'BANGS  MNEXT  02'
C[    IF ( MOBAPD             .EQ.-2147483647   )STOP 'BANGS  MOBAPD 02'
C[    IF ( MOF                .EQ.-2147483647   )STOP 'BANGS  MOF    02'
C[    IF ( MOGFLG             .EQ.-2147483647   )STOP 'BANGS  MOGFLG 02'
C[    IF ( MOR                .EQ.-2147483647   )STOP 'BANGS  MOR    02'
C[    IF ( MORC               .EQ.-2147483647   )STOP 'BANGS  MORC   02'
      WRITE (SER,605) JP,JVPV,IQ(JVPV),MOF,MOR,MORC,POSVEH,VELVEH,
     *                ACCVEH,SLPVEH,KSPD,JVEHCL,JDRICL,MNEXT,MOBAPD,
     *                JSET,MEGAL,MOGFLG,MCHGE,KPRTM,ISTCON(JVPV),JSIG,
     *                JTURN,JBAPS
C4    WRITE (6  ,605) JP,JVPV,IQ(JVPV),MOF,MOR,MORC,POSVEH,VELVEH,
C4   *                ACCVEH,SLPVEH,KSPD,JVEHCL,JDRICL,MNEXT,MOBAPD,
C4   *                JSET,MEGAL,MOGFLG,MCHGE,KPRTM,ISTCON(JVPV),JSIG,
C4   *                JTURN,JBAPS
 5010 CONTINUE
C-----SET THE PARAMETERS FOR PRINTING THE REAR VEHICLES INFORMATION
      IDESPD = IDNINT( DESVEL )
C-----IF THE REAR VEHICLE WAS IN THE INTERSECTION THEN GO TO 5030 AND
C-----PRINT THE INTERSECTION INFORMATION FOR THE REAR VEHICLE ELSE
C-----PRINT THE INBOUND/OUTBOUND APPROACH INFORMATION FOR THE REAR
C-----VEHICLE
                    IF ( MININT(IV) )            GO TO 5030
      IF ( ISET(IV) . EQ . 1 )                   THEN
        POSLAT = LATPOS(IV)
      ELSE
        POSLAT = 0.0
      END IF
                    IF ( ISAME )                 GO TO 5020
C-----THE FRONT VEHICLE AND THE REAR VEHICLE WERE NOT ON THE SAME LINK
C-----THUS PRINT THE HEADER FOR THE REAR VEHICLE
      WRITE (SER,603) IAFORM(1:8),IAFORM(9:12)
C4    WRITE (6  ,603) IAFORM(1:8),IAFORM(9:12)
 5020 CONTINUE
C[    IF ( IDESPD             .EQ.-2147483647   )STOP 'BANGS  IDESPD 01'
C[    IF ( POSLAT             .EQ.-2147483647.0 )STOP 'BANGS  POSLAT 01'
      WRITE (SER,604) IA,ILN,IV,IQ(IV),NOF(IV),NOR(IV),NORC(JPRC,IV),
     *                POSNEW,VELNEW,ACCNEW,SLPNEW,IDESPD,IVEHCL(IV),
     *                IDRICL(IV),LNEXT(IV),NOBAPD(IV),ISET(IV),
     *                LEGAL(IV),LOGFLG(IV),LCHGE(IV),IPRTM(IV),POSLAT,
     *                ISIG,ITURN(IV),IBAPS(IV)
C4    WRITE (6  ,604) IA,ILN,IV,IQ(IV),NOF(IV),NOR(IV),NORC(JPRC,IV),
C4   *                POSNEW,VELNEW,ACCNEW,SLPNEW,IDESPD,IVEHCL(IV),
C4   *                IDRICL(IV),LNEXT(IV),NOBAPD(IV),ISET(IV),
C4   *                LEGAL(IV),LOGFLG(IV),LCHGE(IV),IPRTM(IV),POSLAT,
C4   *                ISIG,ITURN(IV),IBAPS(IV)
      GO TO 6010
 5030 CONTINUE
C-----THE REAR VEHICLE WAS IN THE INTERSECTION THUS PRINT THE
C-----INTERSECTION INFORMATION FOR THE REAR VEHICLE
                    IF ( ISAME )                 GO TO 5040
C-----THE FRONT VEHICLE AND THE REAR VEHICLE WERE NOT ON THE SAME LINK
C-----THUS PRINT THE HEADER FOR THE REAR VEHICLE
      WRITE (SER,603) IPFORM(1:8),IPFORM(9:12)
C4    WRITE (6  ,603) IPFORM(1:8),IPFORM(9:12)
 5040 CONTINUE
C[    IF ( IDESPD             .EQ.-2147483647   )STOP 'BANGS  IDESPD 02'
      WRITE (SER,605) IP,IV,IQ(IV),NOF(IV),NOR(IV),NORC(JPRC,IV),POSNEW,
     *                VELNEW,ACCNEW,SLPNEW,IDESPD,IVEHCL(IV),IDRICL(IV),
     *                LNEXT(IV),NOBAPD(IV),ISET(IV),LEGAL(IV),
     *                LOGFLG(IV),LCHGE(IV),IPRTM(IV),ISTCON(IV),ISIG,
     *                ITURN(IV),IBAPS(IV)
C4    WRITE (6  ,605) IP,IV,IQ(IV),NOF(IV),NOR(IV),NORC(JPRC,IV),POSNEW,
C4   *                VELNEW,ACCNEW,SLPNEW,IDESPD,IVEHCL(IV),IDRICL(IV),
C4   *                LNEXT(IV),NOBAPD(IV),ISET(IV),LEGAL(IV),
C4   *                LOGFLG(IV),LCHGE(IV),IPRTM(IV),ISTCON(IV),ISIG,
C4   *                ITURN(IV),IBAPS(IV)
 6010 CONTINUE
C-----INCREMENT THE NUMBER OF COLLISIONS
C3    KPFLAG = '*INTRUSION'
      NBANG(IBAPS(IV)) = NBANG(IBAPS(IV)) + 1
      PRECOL = MAJCOL(IV)
      MAJCOL(IV) = .FALSE.
C-----DETERMINE WHETHER THIS IS A MAJOR COLLISION
      IF ( STOPMC )                              THEN
        POSVAL = POSRMX - (POSRMX/VELRMX)*DMIN1( DABS( VELREL ),VELRMX )
        IF ( POSREL . LT . POSVAL )              THEN
          MAJCOL(IV) = .TRUE.
        END IF
        IF ( IWHERE . EQ . 4 )                   THEN
          MAJCOL(IV) = .TRUE.
        END IF
      END IF
      IF ( MAJCOL(IV) )                          THEN
C-----  THIS IS A MAJOR COLLISION
        WRITE(WRNMSG,606) 'Major Collision',TIME,IQ(JVPV),IQ(IV)
        CALL  PRTWRN  ( WRNMSG )
        IF ( IQ(IV) . LT . 0 )                   THEN
          CALL  VDIERR  ( TIME,-IQ(IV),VDIMCL )
        END IF
      ELSE
C-----  THIS IS NOT A MAJOR COLLISION
        WRITE(WRNMSG,606) 'Clear Zone Intrusion',TIME,IQ(JVPV),IQ(IV)
        CALL  PRTWRN  ( WRNMSG )
        IF ( IQ(IV) . LT . 0 )                   THEN
          CALL  VDIERR  ( TIME,-IQ(IV),VDICZI )
        END IF
C-----  IF THIS VEHICLE WAS PREVIOUSLY INVOLVED IN A MAJOR COLLISION
C-----  THEN RESTORE MAJOR COLLISION AND RETURN
        IF ( PRECOL )                            THEN
          MAJCOL(IV) = PRECOL
          RETURN
        END IF
        IF ( IWHERE . GE . 4 )                   THEN
C-----    DO NOT RESET VEHICLE INVOLVED IN A COLLISION DETECTED BY CHKCOL
          RETURN
        END IF
C-----  RESET THIS VEHICLES POS/VEL/ACC AND RETURN
        SLPNEW = SLPVEH
        ACCNEW = ACCVEH
        VELNEW = DMIN1( 0.95D0*VELVEH,DESVEL,VELNEW )
        POSNEW = DMAX1( 0.0D0,POSVEH-0.1D0 )
        RELPOS = POSVEH - POSNEW
        RELVEL = VELVEH - VELNEW
        MSFLG(IV) = .FALSE.
        MSTPF(IV) = .FALSE.
        IF ( VELNEW . LE . VELSTP )              THEN
C-----    THE VEHICLE IS STOPPED
          MSTPF(IV) = .TRUE.
          LOGTMP = 2
C-----    IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE
C-----    SOMEWHERE IN THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK
C-----    INTERSECTION CONFLICTS
          IF ( SMJCOL . OR . EVCCON )            THEN
            LOGTMP = 1
          END IF
C-----    UPDATE THE VEHICLES MAXIMUM DECELERATION RATE
          IVMAXD(IV) = MAX0( IVMAXD(IV),IDNINT( -ACCNEW*10.0D0 ) )
          VELNEW = 0.0D0
          ACCNEW = 0.0D0
          SLPNEW = 0.0D0
          XREL = XRELMX
                    IF (        MCHKCF(IV)  )    XREL = XRELMI
                    IF (        MININT(IV)  )    GO TO 6060
                    IF ( (.NOT. MFINL (IV)) )    GO TO 6060
                    IF (        MATSTL(IV)  )    GO TO 6060
          ENDLN = DBLE( LGEOM(4,IL) ) + 1.5D0
          IF ( MBLOCK(IV) )                      THEN
            ENDLN = DBLE( LGEOM(2,IL) )
          END IF
          RELEND = ENDLN - POSNEW
                    IF ( RELEND . GT . XREL )    GO TO 6060
          MATSTL(IV) = .FALSE.
C-----    THIS VEHICLE IS LE XREL DISTANCE FROM THE END OF THE LANE THUS
C-----    IF THE LANE IS NOT BLOCKED THEN THE VEHICLE IS STOPPED AT THE
C-----    STOP LINE
                    IF ( (.NOT. MBLOCK(IV)) )    MATSTL(IV) = .TRUE.
                    IF ( (.NOT. MATSTL(IV)) )    GO TO 6060
C-----    THE VEHICLE IS STOPPED AT THE STOP LINE ON AN INBOUND APPROACH
C-----    SO ADD THE STOPPED VEHICLE TO THE LIST OF VEHICLES AT THE
C-----    INTERSECTION
          CALL  ADLVAI
C-----    CHECK IF LEFT-TURN-ON-RED OR RIGHT-TURN-ON-RED MAY BE MADE
C-----    BASED ON THE LANE CONTROL FOR THIS LANE
C-----                                            NO
C-----                                           LTOR   LROT   RTOR
C-----                                           RTOR
                    IF ( LCONTV(IV) - LCSLTR )   6050 , 6020 , 6030
 6020     CONTINUE
C-----    LEFT-TURN-ON-RED PERMITTED FOR THIS LANE AND IF THIS VEHICLE
C-----    IS NOT GOING TO TURN LEFT THEN GO TO 6050 ELSE SET
C-----    LEFT-TURN-ON-RED FLAG
                    IF ( ITURN(IV) .NE. ITURNL ) GO TO 6050
          GO TO 6040
 6030     CONTINUE
C-----    RIGHT-TURN-ON-RED PERMITTED FOR THIS LANE AND IF THIS VEHICLE
C-----    IS NOT GOING TO TURN RIGHT THEN GO TO 6050 ELSE SET
C-----    RIGHT-TURN-ON-RED FLAG
                    IF ( ITURN(IV) .NE. ITURNR ) GO TO 6050
 6040     CONTINUE
C-----    SET THE LEFT-TURN-ON-RED OR RIGHT-TURN-ON-RED FLAG
          MLRTOR(IV) = .TRUE.
          MTCARS(IV) = .FALSE.
          LOGTMP = 2 + IPIJR(IDRICL(IV))
          GO TO 6060
 6050     CONTINUE
          LOGTMP     = 1
          LOGFLG(IV) = 1
 6060     CONTINUE
C-----    THE VEHICLE IS STOPPED
C-----    SET THE VEHICLES ACC/DEC LOGIC TIMER
          IPRTM(IV) = MAX0( MIN0( MPRTM,LOGTMP-2 ),0 )
C-----    IF THE VEHICLE WAS TRYING NOT TO STOP THEN RESET THE VEHICLES
C-----    ACC/DEC LOGIC TIMER TO ZERO
                    IF ( SLPNEW . GT . 0.0D0 )   IPRTM(IV) = 0
C-----    IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE
C-----    SOMEWHERE IN THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK
C-----    INTERSECTION CONFLICTS
          IF ( SMJCOL . OR . EVCCON )            THEN
            LOGTMP = 1
          END IF
C-----    RESET SOME OF THE VEHICLES PARAMETERS
          MSTPF(IV) = .TRUE.
          MSAOR(IV) = .FALSE.
          MSFLG(IV) = .FALSE.
C-----    IF THE VEHICLE IS STOPPED MORE THAN XREL FEET FROM THE
C-----    PREVIOUS VEHICLE THEN MOVE UP ELSE REMAIN STOPPED
          XREL = XRELMX
                    IF ( ACCVEH . GT . 0.0  )    XREL = XRELMI
                    IF ( MAJRLC(IV)         )    XREL = -0.5D0*XRELMI
                    IF ( RELPOS . GT . XREL )    GO TO 6070
          MSAOR(IV) = .TRUE.
          IPRTM(IV) = 0
          IF ( MFSTPF(IV) )                      THEN
            IF ( FSTACT(IV) )                    THEN
              IF ( SDWELL(IV).EQ.0 )             THEN
                SDWELL(IV) = IDNINT( FSTDTM(IV)/DT )
C-----          IF FORCED STOP DWELL TIME HAS ENDED THEN REMOVE FORCED
C-----          STOP
                IF ( SDWELL(IV) . EQ . 0 )       THEN
                  MFSTPF(IV) = .FALSE.
                  FSTACT(IV) = .FALSE.
                  MSAOR (IV) = .FALSE.
                  LOGTMP     = 1
                  LOGFLG(IV) = 1
C-----            SET PVPOS TO END OF LANE/PATH
                  IF ( MININT(IV) )              THEN
                    IVPV  = 0
                    PVPOS = ENDLN
                    PVVEL = LIMP(IP)
                    PVACC = 0.0D0
                    PVSLP = 0.0D0
                  ELSE
                    IVPV  = 0
                    PVPOS = ENDLN
                    PVVEL = ISLIM(IA)
                    PVACC = 0.0D0
                    PVSLP = 0.0D0
                  END IF
                END IF
              END IF
C-----        IF THERE IS A VEHICLE AHEAD IN THE SAME LANE THEN SET
C-----        PVPOS TO THAT VEHICLE
              IF ( NOF(IV) . GT . 0 )            THEN
                IVPV  = NOF(IV)
                CALL  SPVAS  ( IVPV,PVPOS,PVVEL,PVACC,PVSLP,
     *                         .TRUE.,.TRUE.,.FALSE.,.TRUE.  )
              END IF
C-----        IF LANE IS BLOCKED AND PVPOS IS BEYOND ENDLN THEN SET
C-----        MFINL TRUE
              IF ( MBLOCK(IV)       . AND .
     *             (PVPOS.GE.ENDLN) )            THEN
                IVPV = 0
                MFINL(IV) = .TRUE.
              END IF
C-----        IF FIRST IN LANE AND LANE IS BLOCKED THEN SET PVPOS FOR
C-----        END OF BLOCKED LANE
              IF ( MFINL(IV) . AND . MBLOCK(IV) )THEN
                ENDLN  = DBLE( LGEOM(2,IL) )
                DSPLCH = DBLE( ISPD(IV) )
                IF ( IDISPD(IV) )                THEN
                  DSPLCH = 0.5D0*DSPLCH
                END IF
                IF ( ISPDP (IV) . EQ . 1 )       THEN
                  IF ( MININT(IV) )              THEN
                    IF ( LOBL(IP) . GT . 0 )     THEN
                      DSPLCH = DSPLCH*DBLE( ISLIM(ISNA(LOBL(IP))) )
     *                       /        DBLE( LIMP (          IP  ) )
                    END IF
                  ELSE
                    DSPLCH   = DSPLCH*DBLE( ISLIM(          IA  ) )
     *                       /        DBLE( LIMP (LNEXT(    IV )) )
                  END IF
                END IF
                VELLCH = 0.2D0*DSPLCH
                VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
                VEHLNG = DMIN1( 25.0D0,LENVAP )
                DISLCH = 0.5D0*(ENDLN-POSNEW)
                DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
                DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
                IF ( MAJRLC(IV) )                THEN
                  DISLCH = 0.5D0*XRELMI
                END IF
                DISEND = DMAX1( DISLCH,DMIN1( 0.5D0*ENDLN,
     *                          0.5D0*(ENDLN-POSNEW))      )
                IVPV   = 0
                PVPOS  = DMAX1( ENDLN-DISEND,POSNEW )
                PVVEL  = 0.001D0
                PVACC  = -32.0D0
                PVSLP  =   0.0D0
              END IF
            END IF
          END IF
 6070     CONTINUE
        END IF
        GO TO 8110
      END IF
C-----SET THAT THERE IS A MAJOR COLLISION SOMEWHERE IN THE SYSTEM
      SMJCOL       = .TRUE.
C-----SET THAT THE VEHICLES ARE INVOLVED IN A MAJOR COLLISION
      MAJCOL(IV)   = .TRUE.
      MAJCLB(IV)   = .TRUE.
      MAJCON(IV)   = .TRUE.
      POSCLB(IV)   = POSOLD
      POSCON(IV)   = POSOLD
      IF ( IWHERE . GE . 4 )                     THEN
        CALL  SPVAS   ( JVPV,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                  .FALSE.,.FALSE.,.TRUE.,.FALSE.    )
        POSVEH = POSVEH + PADD
      END IF
      MAJCOL(JVPV) = .TRUE.
      MAJCLB(JVPV) = .TRUE.
      MAJCON(JVPV) = .TRUE.
      POSCLB(JVPV) = POSVEH
      POSCON(JVPV) = POSVEH
      IF ( NVMCOL(IV)   . GE . MVMCOL )          GO TO 9470
      IF ( NVMCOL(JVPV) . GE . MVMCOL )          GO TO 9470
      NVMCOL(IV)   = NVMCOL(IV)   + 1
      NVMCOL(JVPV) = NVMCOL(JVPV) + 1
      IVMCOL(NVMCOL(IV  ),IV  ) = JVPV
      IVMCOL(NVMCOL(JVPV),JVPV) = IV
C-----SET THAT THERE IS A MAJOR COLLISION ON THE LANE/PATH FOR BOTH
C-----VEHICLES AND THE PREVIOUS LANE/PATH IF THE REAR BUMPER IS STILL ON
C-----THE PREVIOUS LANE/PATH
      IF ( MININT(IV) )                          THEN
        PMJCOL(IP) = .TRUE.
        IF ( POSNEW . LT . LENVAP )              THEN
          IF ( LPREV(IV) . GT . 0 )              THEN
            LMJCOL(LPREV(IV)) = .TRUE.
          END IF
        END IF
      ELSE
        LMJCOL(IL) = .TRUE.
        IF ( POSNEW . LT . LENVAP )              THEN
          IF ( LPREV(IV) . GT . 0 )              THEN
            PMJCOL(LPREV(IV)) = .TRUE.
          END IF
        END IF
      END IF
      CALL  SPVAS   ( JVPV,POSVEH,VELVEH,ACCVEH,SLPVEH,
     *                .FALSE.,.FALSE.,.FALSE.,.TRUE.    )
      IF ( MININT(JVPV) )                        THEN
        PMJCOL(JP) = .TRUE.
        IF ( POSVEH . LT . LVAP(JVPV) )          THEN
          IF ( LPREV(JVPV) . GT . 0 )            THEN
            LMJCOL(LPREV(JVPV)) = .TRUE.
          END IF
        END IF
      ELSE
        LMJCOL(JL) = .TRUE.
        IF ( POSVEH . LT . LVAP(JVPV) )          THEN
          IF ( LPREV(JVPV) . GT . 0 )            THEN
            PMJCOL(LPREV(JVPV)) = .TRUE.
          END IF
        END IF
      END IF
C-----MAJOR COLLISION THUS STOP THE VEHICLES IMMEDIATELY USING COLLISION
C-----DECELERATION
C-----SET THAT THE DRIVER-VEHICLE UNIT DOES HAVE AN OPERATIONAL IVDMS
      IVDMSO(IV) = .TRUE.
C-----VEHICLE MESSAGE SYSTEM MESSAGE TYPE - VEHICLE IVDMS
      JVMSMT = VMSTVI
C-----VEHICLE MESSAGE SYSTEM MESSAGE - STOP IMMEDIATELY USING COLLISION
C-----DECELERATION
      JVMSMG = VMSMSC
C-----VEHICLE MESSAGE SYSTEM MESSAGE PARAMETER - COLLISION DECEL IN
C-----FT/SEC/SEC
C-----DECCOL = VEHICLE MESSAGE SYSTEM COLLISION DECELERATION MAXIMUM
      RVMSMP = -DECCOL
C-----VEHICLE MESSAGE SYSTEM MESSAGE STARTING TIME
      RVMSST = TIME
C-----VEHICLE MESSAGE SYSTEM MESSAGE ACTIVE TIME
      RVMSAT = SIMTIM - TIME + 2.0D0*DT
C-----VEHICLE MESSAGE SYSTEM MESSAGE APPROACH (+) OR INTERSECTION PATH (-)
C-----VEHICLE MESSAGE SYSTEM MESSAGE LANE BEGIN
C-----VEHICLE MESSAGE SYSTEM MESSAGE LANE END
      IF ( MININT(IV) )                          THEN
        JVMSAP = -IP
        JVMSLB = 0
        JVMSLE = 0
        POSEND = LENP(IP)
      ELSE
        JVMSAP = IA
        JVMSLB = 1
        JVMSLE = NLANES(JVMSAP)
        POSEND = -1.0D0
        DO 7010  JLN = JVMSLB , JVMSLE
        JL = LLANES(JLN,JVMSAP)
        IF ( LGEOM(3,JL) . EQ . LGEOM(4,JL) )    THEN
          POSEND = DMAX1( POSEND,DBLE( LGEOM(2,JL) ) )
        ELSE
          POSEND = DMAX1( POSEND,DBLE( LGEOM(4,JL) ) )
        END IF
 7010   CONTINUE
      END IF
C-----VEHICLE MESSAGE SYSTEM MESSAGE POSITION BEGIN
      RVMSPB = 0.0D0
C-----VEHICLE MESSAGE SYSTEM MESSAGE POSITION END
      RVMSPE = POSEND
C-----VEHICLE MESSAGE SYSTEM MESSAGE VEHICLE NUMBER (O=ALL)
      JVMSVN = IQ(IV)
C-----VEHICLE MESSAGE SYSTEM MESSAGE REACTION TIME DISTRIBUTION NAME
      SVMSDN = 'CONSTAN'
C-----VEHICLE MESSAGE SYSTEM MESSAGE REACTION TIME DISTRIBUTION MEAN
      RVMSDM = 0.0D0
C-----VEHICLE MESSAGE SYSTEM MESSAGE REACTION TIME DISTRIBUTION PARAMETER
      RVMSDP = 0.0D0
C-----DISMISS IF VEHICLE MESSAGE SYSTEM MESSAGE IS ALREADY INSERTED
      DO 7020  I = IVMSMB , NVMSM
C-----IF THE FOLLOWING ITEMS ARE THE SAME THEN DO NOT INSERT VMS MESSAGE
C-----VMS MESSAGE TYPE
C-----VMS MESSAGE
C-----VMS MESSAGE PARAMETER (MPH FOR SPEED)
C-----VMS MESSAGE APPROACH (+) OR INTERSECTION PATH (-)
C-----VMS MESSAGE LANE BEGIN
C-----VMS MESSAGE LANE END
C-----VMS MESSAGE POSITION BEGIN
C-----VMS MESSAGE POSITION END
C-----VMS MESSAGE VEHICLE NUMBER (O=ALL)
C-----VMS MESSAGE REACTION TIME DISTRIBUTION NAME
C-----VMS MESSAGE REACTION TIME DISTRIBUTION MEAN
C-----VMS MESSAGE REACTION TIME DISTRIBUTION PARAMETER
      IF ( ( IVMSMT(I) . EQ . JVMSMT ) . AND .
     *     ( IVMSMG(I) . EQ . JVMSMG ) . AND .
     *     ( DVMSMP(I) . EQ . RVMSMP ) . AND .
     *     ( IVMSAP(I) . EQ . JVMSAP ) . AND .
     *     ( IVMSLB(I) . EQ . JVMSLB ) . AND .
     *     ( IVMSLE(I) . EQ . JVMSLE ) . AND .
     *     ( DVMSPB(I) . EQ . RVMSPB ) . AND .
     *     ( DVMSPE(I) . EQ . RVMSPE ) . AND .
     *     ( IVMSVN(I) . EQ . JVMSVN ) . AND .
     *     ( CVMSDN(I) . EQ . SVMSDN ) . AND .
     *     ( DVMSDM(I) . EQ . RVMSDM ) . AND .
     *     ( DVMSDP(I) . EQ . RVMSDP ) )         GO TO 7030
 7020 CONTINUE
      CALL  IVMSG   ( JVMSMT,JVMSMG,RVMSMP,RVMSST,RVMSAT,JVMSAP,
     *                JVMSLB,JVMSLE,RVMSPB,RVMSPE,JVMSVN,SVMSDN,
     *                RVMSDM,RVMSDP                              )
 7030 CONTINUE
C-----ADD THE SAME MESSAGE FOR LNEXT
      IF ( LNEXT(IV) . EQ . 0 )                  GO TO 8010
C-----VEHICLE MESSAGE SYSTEM MESSAGE APPROACH (+) OR INTERSECTION PATH (-)
C-----VEHICLE MESSAGE SYSTEM MESSAGE LANE BEGIN
C-----VEHICLE MESSAGE SYSTEM MESSAGE LANE END
      IF ( MININT(IV) )                          THEN
        JVMSAP = ISNA(LNEXT(IV))
        JVMSLB = 1
        JVMSLE = NLANES(JVMSAP)
        POSEND = -1.0D0
        DO 7040  JLN = JVMSLB , JVMSLE
        JL = LLANES(JLN,JVMSAP)
        IF ( LGEOM(3,JL) . EQ . LGEOM(4,JL) )    THEN
          POSEND = DMAX1( POSEND,DBLE( LGEOM(2,JL) ) )
        ELSE
          POSEND = DMAX1( POSEND,DBLE( LGEOM(4,JL) ) )
        END IF
 7040   CONTINUE
      ELSE
        JVMSAP = -LNEXT(IV)
        JVMSLB = 0
        JVMSLE = 0
        POSEND = LENP(LNEXT(IV))
      END IF
C-----VEHICLE MESSAGE SYSTEM MESSAGE POSITION BEGIN
      RVMSPB = 0.0D0
C-----VEHICLE MESSAGE SYSTEM MESSAGE POSITION END
      RVMSPE = POSEND
C-----DISMISS IF VEHICLE MESSAGE SYSTEM MESSAGE IS ALREADY INSERTED
      DO 7050  I = IVMSMB , NVMSM
C-----IF THE FOLLOWING ITEMS ARE THE SAME THEN DO NOT INSERT VMS MESSAGE
C-----VMS MESSAGE TYPE
C-----VMS MESSAGE
C-----VMS MESSAGE PARAMETER (MPH FOR SPEED)
C-----VMS MESSAGE APPROACH (+) OR INTERSECTION PATH (-)
C-----VMS MESSAGE LANE BEGIN
C-----VMS MESSAGE LANE END
C-----VMS MESSAGE POSITION BEGIN
C-----VMS MESSAGE POSITION END
C-----VMS MESSAGE VEHICLE NUMBER (O=ALL)
C-----VMS MESSAGE REACTION TIME DISTRIBUTION NAME
C-----VMS MESSAGE REACTION TIME DISTRIBUTION MEAN
C-----VMS MESSAGE REACTION TIME DISTRIBUTION PARAMETER
      IF ( ( IVMSMT(I) . EQ . JVMSMT ) . AND .
     *     ( IVMSMG(I) . EQ . JVMSMG ) . AND .
     *     ( DVMSMP(I) . EQ . RVMSMP ) . AND .
     *     ( IVMSAP(I) . EQ . JVMSAP ) . AND .
     *     ( IVMSLB(I) . EQ . JVMSLB ) . AND .
     *     ( IVMSLE(I) . EQ . JVMSLE ) . AND .
     *     ( DVMSPB(I) . EQ . RVMSPB ) . AND .
     *     ( DVMSPE(I) . EQ . RVMSPE ) . AND .
     *     ( IVMSVN(I) . EQ . JVMSVN ) . AND .
     *     ( CVMSDN(I) . EQ . SVMSDN ) . AND .
     *     ( DVMSDM(I) . EQ . RVMSDM ) . AND .
     *     ( DVMSDP(I) . EQ . RVMSDP ) )         GO TO 8010
 7050 CONTINUE
      CALL  IVMSG   ( JVMSMT,JVMSMG,RVMSMP,RVMSST,RVMSAT,JVMSAP,
     *                JVMSLB,JVMSLE,RVMSPB,RVMSPE,JVMSVN,SVMSDN,
     *                RVMSDM,RVMSDP                              )
 8010 CONTINUE
C-----ADD THE SAME MESSAGE FOR THE OTHER VEHICLE
C-----VEHICLE MESSAGE SYSTEM MESSAGE APPROACH (+) OR INTERSECTION PATH (-)
C-----VEHICLE MESSAGE SYSTEM MESSAGE LANE BEGIN
C-----VEHICLE MESSAGE SYSTEM MESSAGE LANE END
      IF ( MININT(JVPV) )                        THEN
        JVMSAP = -LPRES(JVPV)
        JVMSLB = 0
        JVMSLE = 0
        POSEND = LENP(LPRES(JVPV))
      ELSE
        JVMSAP = ISNA(LPRES(JVPV))
        JVMSLB = 1
        JVMSLE = NLANES(JVMSAP)
        POSEND = -1.0D0
        DO 8020  JLN = JVMSLB , JVMSLE
        JL = LLANES(JLN,JVMSAP)
        IF ( LGEOM(3,JL) . EQ . LGEOM(4,JL) )    THEN
          POSEND = DMAX1( POSEND,DBLE( LGEOM(2,JL) ) )
        ELSE
          POSEND = DMAX1( POSEND,DBLE( LGEOM(4,JL) ) )
        END IF
 8020   CONTINUE
      END IF
C-----VEHICLE MESSAGE SYSTEM MESSAGE POSITION BEGIN
      RVMSPB = 0.0D0
C-----VEHICLE MESSAGE SYSTEM MESSAGE POSITION END
      RVMSPE = POSEND
C-----VEHICLE MESSAGE SYSTEM MESSAGE VEHICLE NUMBER (O=ALL)
      JVMSVN = IQ(JVPV)
C-----DISMISS IF VEHICLE MESSAGE SYSTEM MESSAGE IS ALREADY INSERTED
      DO 8030  I = IVMSMB , NVMSM
C-----IF THE FOLLOWING ITEMS ARE THE SAME THEN DO NOT INSERT VMS MESSAGE
C-----VMS MESSAGE TYPE
C-----VMS MESSAGE
C-----VMS MESSAGE PARAMETER (MPH FOR SPEED)
C-----VMS MESSAGE APPROACH (+) OR INTERSECTION PATH (-)
C-----VMS MESSAGE LANE BEGIN
C-----VMS MESSAGE LANE END
C-----VMS MESSAGE POSITION BEGIN
C-----VMS MESSAGE POSITION END
C-----VMS MESSAGE VEHICLE NUMBER (O=ALL)
C-----VMS MESSAGE REACTION TIME DISTRIBUTION NAME
C-----VMS MESSAGE REACTION TIME DISTRIBUTION MEAN
C-----VMS MESSAGE REACTION TIME DISTRIBUTION PARAMETER
      IF ( ( IVMSMT(I) . EQ . JVMSMT ) . AND .
     *     ( IVMSMG(I) . EQ . JVMSMG ) . AND .
     *     ( DVMSMP(I) . EQ . RVMSMP ) . AND .
     *     ( IVMSAP(I) . EQ . JVMSAP ) . AND .
     *     ( IVMSLB(I) . EQ . JVMSLB ) . AND .
     *     ( IVMSLE(I) . EQ . JVMSLE ) . AND .
     *     ( DVMSPB(I) . EQ . RVMSPB ) . AND .
     *     ( DVMSPE(I) . EQ . RVMSPE ) . AND .
     *     ( IVMSVN(I) . EQ . JVMSVN ) . AND .
     *     ( CVMSDN(I) . EQ . SVMSDN ) . AND .
     *     ( DVMSDM(I) . EQ . RVMSDM ) . AND .
     *     ( DVMSDP(I) . EQ . RVMSDP ) )         GO TO 8040
 8030 CONTINUE
      CALL  IVMSG   ( JVMSMT,JVMSMG,RVMSMP,RVMSST,RVMSAT,JVMSAP,
     *                JVMSLB,JVMSLE,RVMSPB,RVMSPE,JVMSVN,SVMSDN,
     *                RVMSDM,RVMSDP                              )
 8040 CONTINUE
C-----ADD THE SAME MESSAGE FOR LNEXT
      IF ( LNEXT(JVPV) . EQ . 0 )                GO TO 8070
C-----VEHICLE MESSAGE SYSTEM MESSAGE APPROACH (+) OR INTERSECTION PATH (-)
C-----VEHICLE MESSAGE SYSTEM MESSAGE LANE BEGIN
C-----VEHICLE MESSAGE SYSTEM MESSAGE LANE END
      IF ( MININT(JVPV) )                          THEN
        JVMSAP = ISNA(LNEXT(JVPV))
        JVMSLB = 1
        JVMSLE = NLANES(JVMSAP)
        POSEND = -1.0D0
        DO 8050  JLN = JVMSLB , JVMSLE
        JL = LLANES(JLN,JVMSAP)
        IF ( LGEOM(3,JL) . EQ . LGEOM(4,JL) )    THEN
          POSEND = DMAX1( POSEND,DBLE( LGEOM(2,JL) ) )
        ELSE
          POSEND = DMAX1( POSEND,DBLE( LGEOM(4,JL) ) )
        END IF
 8050   CONTINUE
      ELSE
        JVMSAP = -LNEXT(JVPV)
        JVMSLB = 0
        JVMSLE = 0
        POSEND = LENP(LNEXT(JVPV))
      END IF
C-----VEHICLE MESSAGE SYSTEM MESSAGE POSITION BEGIN
      RVMSPB = 0.0D0
C-----VEHICLE MESSAGE SYSTEM MESSAGE POSITION END
      RVMSPE = POSEND
C-----DISMISS IF VEHICLE MESSAGE SYSTEM MESSAGE IS ALREADY INSERTED
      DO 8060  I = IVMSMB , NVMSM
C-----IF THE FOLLOWING ITEMS ARE THE SAME THEN DO NOT INSERT VMS MESSAGE
C-----VMS MESSAGE TYPE
C-----VMS MESSAGE
C-----VMS MESSAGE PARAMETER (MPH FOR SPEED)
C-----VMS MESSAGE APPROACH (+) OR INTERSECTION PATH (-)
C-----VMS MESSAGE LANE BEGIN
C-----VMS MESSAGE LANE END
C-----VMS MESSAGE POSITION BEGIN
C-----VMS MESSAGE POSITION END
C-----VMS MESSAGE VEHICLE NUMBER (O=ALL)
C-----VMS MESSAGE REACTION TIME DISTRIBUTION NAME
C-----VMS MESSAGE REACTION TIME DISTRIBUTION MEAN
C-----VMS MESSAGE REACTION TIME DISTRIBUTION PARAMETER
      IF ( ( IVMSMT(I) . EQ . JVMSMT ) . AND .
     *     ( IVMSMG(I) . EQ . JVMSMG ) . AND .
     *     ( DVMSMP(I) . EQ . RVMSMP ) . AND .
     *     ( IVMSAP(I) . EQ . JVMSAP ) . AND .
     *     ( IVMSLB(I) . EQ . JVMSLB ) . AND .
     *     ( IVMSLE(I) . EQ . JVMSLE ) . AND .
     *     ( DVMSPB(I) . EQ . RVMSPB ) . AND .
     *     ( DVMSPE(I) . EQ . RVMSPE ) . AND .
     *     ( IVMSVN(I) . EQ . JVMSVN ) . AND .
     *     ( CVMSDN(I) . EQ . SVMSDN ) . AND .
     *     ( DVMSDM(I) . EQ . RVMSDM ) . AND .
     *     ( DVMSDP(I) . EQ . RVMSDP ) )         GO TO 8070
 8060 CONTINUE
      CALL  IVMSG   ( JVMSMT,JVMSMG,RVMSMP,RVMSST,RVMSAT,JVMSAP,
     *                JVMSLB,JVMSLE,RVMSPB,RVMSPE,JVMSVN,SVMSDN,
     *                RVMSDM,RVMSDP                              )
 8070 CONTINUE
C-----FORCE EVERY VEHICLE ON AN INBOUND LANE TO CHECK INTERSECTION
C-----CONFLICTS
      DO 8100  JAN = 1 , NIBA
      JA = LIBA(JAN)
      DO 8090  JLN = 1 , NLANES(JA)
      JL = LLANES(JLN,JA)
      JV = IFVL(JL)
      DO 8080 WHILE ( JV . GT . 0 )
      MCHKCF(JV) = .TRUE.
      MPRO  (JV) = .FALSE.
      IF ( MDEDIC(JV) . AND . MINFLZ(JV) )       THEN
        LOGFLG(JV) = 1
        MAJSIG(JV) = ( LCONTR(JL) . GE . LCSIGX )
      END IF
      JV = NOR(JV)
 8080 CONTINUE
 8090 CONTINUE
 8100 CONTINUE
 8110 CONTINUE
C-----COMPUTE NEW ACC/DEC LOGIC
      CALL  LOGIC   ( 6,IV )
C-----COMPUTE NEW INTERSECTION CONTROL LOGIC
      CALL  LOGIC   ( 8,IV )
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
C9190 CONTINUE
C     CALL  ABORTR  ( 'STOP 919 - '                      //
C    *                'NO LANE ON LIST MATCHES MPRES - ' //
C    *                'BANGS'                               )
C     STOP  919
 9460 CONTINUE
      CALL  ABORTR  ( 'STOP 946 - INVALID IPRC/NORC - BANGS' )
      STOP  946
 9470 CONTINUE
      CALL  ABORTR  ( 'STOP 947 - NVMCOL GT MVMCOL - BANGS' )
      STOP  947
      END                                                               BANGS
C
C
C
      SUBROUTINE BIAS   ( IPR )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INTEGER           IPR
C
C-----SUBROUTINE BIAS BIASES THE VEHICLE ATTRIBUTES, SETS THE PREVIOUS
C-----VEHICLE PARAMETERS, AND UPDATES MAXIMUM ACC/DEC FOR THE VEHICLE
C
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'BIAS'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      CALL  DISPRE  ( VEHCLE(IV),VEHCLA(IVEHCL(IV)),POSNEW,
     *                LPRES(IV),IPR,LATPOS(IV),IVEHCL(IV)        )
C OLD LENVAP = DBLE( LENV(IVEHCL(IV)) )
      LENVAP = VEHCLE(IV)%LENLP
C-----BIAS THE VEHICLES ATTRIBUTES
      IPOS  (IV) = POSNEW
      IVEL  (IV) = VELNEW
      IACC  (IV) = ACCNEW
      ISLP  (IV) = SLPNEW
      LVAP  (IV) = LENVAP
      IUPDAT(IV) = .TRUE.
                    IF ( IVEL(IV) . EQ . 0.0D0 ) MSTPF(IV) = .TRUE.
                    IF ( IVEL(IV) . GT . 0.0D0 ) MSAOR(IV) = .FALSE.
      IF ( MAJCOL(IV) )                          THEN
        POSCLB(IV) = POSNEW
        POSCON(IV) = POSNEW
      END IF
C-----SET THE PREVIOUS VEHICLE PARAMETERS
      IVPV  = IV
      PVPOS = POSNEW - LENVAP - XRELMI
      PVVEL = VELNEW
      PVACC = ACCNEW
      PVSLP = SLPNEW
C-----IF THE VEHICLE WAS ACCELERATING THEN GO TO 1010 AND UPDATE THE
C-----MAXIMUM ACCELERATION FOR THE VEHICLE ELSE UPDATE THE MAXIMUM
C-----DECELERATION FOR THE VEHICLE
                    IF ( ACCOLD . GT . 0.0D0 )   GO TO 1010
      IVMAXD(IV) = MAX0( IVMAXD(IV),IDNINT( -ACCNEW*10.0D0 ) )
      IF ( INT1T(IV) . NE . 0 )                  THEN
        IVMXDI(IV) = MAX0( IVMXDI(IV),IDNINT( -ACCNEW*10.0D0 ) )
      END IF
      RETURN
 1010 CONTINUE
C-----UPDATE THE MAXIMUM ACCELERATION FOR THE VEHICLE
      IVMAXA(IV) = MAX0( IVMAXA(IV),IDNINT( ACCOLD*10.0D0 ) )
      IF ( INT1T(IV) . NE . 0 )                  THEN
        IVMXAI(IV) = MAX0( IVMXAI(IV),IDNINT( ACCOLD*10.0D0 ) )
      END IF
      RETURN
      END                                                               BIAS
C
C
C
      SUBROUTINE LOGIN
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'PATH'
C6    INCLUDE 'PRTPVA'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'SUMST2'
      INCLUDE 'TITLE'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      INTEGER           NITERS
      PARAMETER       ( NITERS = 8 )
      CHARACTER*1       IFUT
      CHARACTER*3       VEMERG
      CHARACTER*4       AVTYPS
      LOGICAL           EVRESP,LBVSTP,LCHKCF,LVMSDD
      INTEGER           I,IB,IPR,JB,JP,JVN,KV,GETLCV,NOFT
      DOUBLE PRECISION  ACCVEH,CRISLP,DECMAX,DISEND,DISLCH,DIST,DSPLCH,
     *                  FACT,HWM,POSMJC,POSVEH,PVVELM,SAFR,SAFVEL,SLPN,
     *                  SLPVEH,SLPTMP,T,TMAX,TSTP,V,VELLCH,VEHLNG,
     *                  VELVEH,XSTP,XTIMEL
CS    DOUBLE PRECISION  POSLCS
C7    DOUBLE PRECISION  POSLC7
  502 FORMAT(F6.2,I2,I1,I3,2I2,2I1,A1,F7.2,I4,6F7.2,I6,1X,A3,3F7.3)
  601 FORMAT(' ** VEHICLE ELIMINATED   AT T=',F7.2,' SECS VEH=',I6,
     *       ' APPR=',I2,' LANE=',I1,' OBAP=',I3,' VCL=',I2,' DCL=',I1,
     *       ' DVEL=',I3,' SPRT=',I1,' - LANE FULL **  WARNING  **')
  602 FORMAT('** VEHICLE ELIMINATED    AT T=',F7.2,' SECS VEH=',I6,
     *       ' - LANE FULL **  WARNING  **')
  603 FORMAT('** VEHICLE WARNING       AT T=',F7.2,' SECS VEH=',I6,
     *       ' - INPUT ENTRY SPEED = ',F6.2,
     *       ' IS GT CALCULATED ENTRY SPEED = ',F6.2' **  WARNING  **')
C7701 FORMAT(F7.2,I6,4I4,2F7.1)
C3702 FORMAT(3HLV=,F7.2)
C3703 FORMAT(3HET=,F7.5)
CS704 FORMAT(2I3,I4,I6,2I4,2(I3,I4),2F7.2,2F7.3,10I3,F5.1,I5,3(1X,A10))
C1705 FORMAT(19H INPUT QUEUE BUFFER,I3,9H  VEHICLE,I5,10H  READIN =,
C1   *       F10.2,7I5)
CW756 FORMAT(8H VEHD   ,I3,4F6.0,3I2,2I3,2I4,F6.0,2I4,I3,2I4,F4.0,F6.0,
CW   *                  I2,2(I3,I4),I3,1X,13L1,1X,7L1,/,8X,
CW   *                  21HCONTINUE W/ INTERNALS,18X,2I4,F6.0,2I4,3X,
CW   *                  2I4,4X,F6.0,L2,I3,L2,I6)
CW757 FORMAT(8H VEHF   ,I3,1X,10I4,F4.2,4I4,F4.2)
CW758 FORMAT(8H VEHIL  ,I3,1X,11L2,3X,8L2)
  920 FORMAT('STOP 920 - MORE THAN ',I4,' VEHICLES IN SYSTEM - LOGIN')
  952 FORMAT('STOP 952 - FORCED STOP PATH NUM = ',I3,' GT NUM PATHS = ',
     *                   I3,' - LOGIN')
  953 FORMAT('STOP 953 - FORCED STOP PATH POS = ',F7.2,
     *                   ' GT LEN PATH = ',I4,' - LOGIN')
  970 FORMAT('STOP 970 - INBOUND LANE ALLOWED VEHICLE TYPE = (',A,
     *                   ') DOES NOT ALLOW VEHICLE TYPE = (',A,
     *                   ') - LOGIN')
  971 FORMAT('STOP 971 - DESIRED OUTBOUND APPROACH ALLOWED VEHICLE TYP',
     *                   'E = (',A,
     *                   ') DOES NOT ALLOW VEHICLE TYPE = (',A,
     *                   ') - LOGIN')
C
C-----SUBROUTINE LOGIN LOGS THE NEW VEHICLE INTO THE INBOUND APPROACH
C-----AND LANE AND INITIALIZES THE VEHICLE ATTRIBUTES
C
C-----LVMSDD = DISTRACTED DRIVER VMS MESSAGE WAS CANCELLED OR TIMED OUT
C
C[    IFUT       = '~'
C[    I          = -2147483647
C[    IB         = -2147483647
C[    JB         = -2147483647
C[    NOFT       = -2147483647
C[    CRISLP     = -2147483647.0
C[    DECMAX     = -2147483647.0
C[    DISEND     = -2147483647.0
C[    DISLCH     = -2147483647.0
C[    DIST       = -2147483647.0
C[    FACT       = -2147483647.0
C[    SLPN       = -2147483647.0
C[    T          = -2147483647.0
C[    TSTP       = -2147483647.0
C[    V          = -2147483647.0
C[    XSTP       = -2147483647.0
C[    XTIMEL     = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'LOGIN'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----FIND THE NEXT AVAILABLE INDEX
      DO 1010  I = 1 , NVE
      IQQ = IQQ + 1
                    IF ( IQQ . GT . NVE )        IQQ = 1
C-----IF INDEX IQQ IS NOT IN USE THEN GO TO 1020 AND USE INDEX IQQ
C-----FOR THE NEW VEHICLE
                    IF ( IQ(IQQ) . LE . 0 )      GO TO 1020
 1010 CONTINUE
      GO TO 9200
 1020 CONTINUE
C-----FIND THE QUEUE BUFFER FOR THE NEW VEHICLE TO BE LOGGED IN BASED ON
C-----THE INBOUND APPROACH AND LANE NUMBER
      IB = LQ(IAN,ILN)
      IF ( IB . GT . NIL )                       THEN
        JB = IB - NIL
      ELSE
        JB = IB
      END IF
C-----LET THE NEW VEHICLE USE INDEX IQQ
      IV = IQQ
C6    DISTAD(IV) = 0.0D0
C-----SAVE THE SEQUENTIAL VEHICLE NUMBER FOR THE NEW VEHICLE AND FLAG
C-----THE INDEX IN USE
      IF ( IB . GT . NIL )                       THEN
        IQ(IV) = -VDIIVN(JB)
      ELSE
        IQ(IV) = IBUF(IBUFVN,IB)
      END IF
C-----INITIALIZE ALL VEHICLE IV ATTRIBUTES
      CALL  INIVEH  ( IV )
      EVRESP = .FALSE.
C-----SET COLLISION POSITIONS TO POSBIG
      POSCLB(IV) = POSBIG
      POSCLL(IV) = POSBIG
      POSCON(IV) = POSBIG
C-----SET TRAFFIC CONTROL AHEAD REQUIRES STOP TO TRUE
      MTCARS(IV) = .TRUE.
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        MCHKCF(IV) = .TRUE.
        MPRO  (IV) = .FALSE.
        LOGTMP     = 1
        LOGFLG(IV) = 1
      END IF
C-----SET THE LANE CHANGE FLAG FOR THE NEW VEHICLE TO NO LANE CHANGE
      NOFT = ILVL(IL)
      LCHGE(IV) = 1
                    IF ( NOFT . EQ . 0 )         GO TO 1040
C-----THE LAST VEHICLE PROCESSED WAS ON THIS LANE THUS IF THAT VEHICLE
C-----WAS CHANGING LANES THEN SET THE LANE CHANGE FLAG FOR THE NEW
C-----VEHICLE TO FOLLOWING A LANE CHANGING VEHICLE
                    IF ( ISET(NOFT) . EQ . 1 )   LCHGE(IV) = 3
 1040 CONTINUE
C-----SET THE NEAREST VEHICLE TO THE FRONT AS LAST VEHICLE ON THIS LANE
C[    IF ( NOFT               .EQ.-2147483647   )STOP 'LOGIN  NOFT   01'
      NOF(IV) = NOFT
C-----IF THERE IS A VEHICLE AHEAD THEN GO TO 2020 AND SET THE NEW
C-----VEHICLE AS THE NOR VEHICLE FOR THE VEHICLE AHEAD
                    IF ( NOFT . GT . 0 )         GO TO 2020
C-----SET THE NEW VEHICLE AS THE FIRST VEHICLE IN THE LANE
      IFVL(IL) = IV
C-----INITIALIZE SOME PARAMETERS FOR THE NEW VEHICLE (FIRST IN LANE)
      MFINL(IV) = .TRUE.
      MOASF(IV) = .TRUE.
      IVPV  = 0
      PVPOS = DBLE( LGEOM(4,IL) )
C[    IF ( IB                 .EQ.-2147483647   )STOP 'LOGIN  IB     01'
      IF ( IB . GT . NIL )                       THEN
        PVVEL = VDIDSP(JB)
      ELSE
        PVVEL = IBUF(IBUFDV,IB)
      END IF
      PVACC = 0.0D0
      PVSLP = 0.0D0
      GO TO 2030
 2020 CONTINUE
C-----SET THE NEW VEHICLE AS THE NOR VEHICLE FOR THE VEHICLE AHEAD
      MFINL(IV) = .FALSE.
C[    IF ( NOFT               .EQ.-2147483647   )STOP 'LOGIN  NOFT   02'
      NOR(NOFT) = IV
      MOASF(IV) = .FALSE.
                    IF ( PVVEL . LE . VELSTP )   MOASF(IV) = .TRUE.
 2030 CONTINUE
C-----SET THE LAST VEHICLE IN THE LANE TO THE NEW VEHICLE
      ILVL(IL) = IV
C-----INITIALIZE THE VEHD ATTRIBUTES
      ISET  (IV) = 6
      LEGAL (IV) = 4
      LALT  (IV) = 5
      LOGFLG(IV) = 2
C-----IF THERE IS A MAJOR COLLISION OR AN EMERGENCY VEHICLE SOMEWHERE IN
C-----THE SYSTEM THEN FORCE EVERY VEHICLE TO CHECK INTERSECTION
C-----CONFLICTS
      IF ( SMJCOL . OR . EVCCON )                THEN
        LOGFLG(IV) = 1
      END IF
      NORC(1,IV) = NVEP1
      NORC(2,IV) = NVEP1
      IF ( IB . GT . NIL )                       THEN
        FGOATM(IV) = VDIFGA(JB)
        FGOTIM(IV) = VDIFGT(JB)
        FRRATM(IV) = VDIFRA(JB)
        FRRTIM(IV) = VDIFRT(JB)
        FSTDTM(IV) = VDIFSD(JB)
        FSTPIA(IV) = VDIFSL(JB)
        FSTPOS(IV) = VDIFSP(JB)
        FSTTIM(IV) = VDIFST(JB)
      ELSE
        FGOATM(IV) = QGOATM(IB)
        FGOTIM(IV) = QGOTIM(IB)
        FRRATM(IV) = QRRATM(IB)
        FRRTIM(IV) = QRRTIM(IB)
        FSTDTM(IV) = QSTDTM(IB)
        FSTPIA(IV) = IBUF  (IBUFFS,IB)
        FSTPOS(IV) = QSTPOS(IB)
        FSTTIM(IV) = QSTTIM(IB)
      END IF
      IF ( FSTTIM(IV) . GT . 0.0D0 )             THEN
        IF ( FSTPIA(IV) . LT . 0 )               THEN
          JP = -FSTPIA(IV)
          IF ( JP . GT . NPATHS )                GO TO 9520
          IF ( FSTPOS(IV) .GT. DBLE( LENP(JP) ) )GO TO 9530
        END IF
      END IF
C-----INITIALIZE THE VEHF ATTRIBUTES
C[    IF ( IB                 .EQ.-2147483647   )STOP 'LOGIN  IB     02'
      IF ( IB . GT . NIL )                       THEN
        IVEHCL(IV) = VDIVCN(JB)
        IDRICL(IV) = VDIDCN(JB)
        ISPD  (IV) = VDIDSP(JB)
        NOBAPD(IV) = VDIOBN(JB)
        XTIMEL = TIME - VDIQIT(JB)
      ELSE
        IVEHCL(IV) = IBUF(IBUFVC,IB)
        IDRICL(IV) = IBUF(IBUFDC,IB)
        ISPD  (IV) = IBUF(IBUFDV,IB)
        NOBAPD(IV) = IBUF(IBUFDO,IB)
        XTIMEL = TIME - QTIME(IB)
      END IF
      IEXTIM(IV) = XTIMEL/DT
      IBAPS (IV) = IAN
      LPRES (IV) = IL
      IF ( IB . GT . NIL )                       THEN
        IF ( VDIPLO(JB) . EQ . 0 )               THEN
          IPRTLO(IV) = 0
        ELSE
          IPRTLO(IV) = 1
        END IF
      ELSE
        IF ( IBUF(IBUFPL,IB) . EQ . 0 )          THEN
          IPRTLO(IV) = 0
        ELSE
          IPRTLO(IV) = 1
        END IF
      END IF
      IVDMSO(IV) = .TRUE.
C-----SET LENGTH OF VEHICLE ALONG PATH
      LENVAP = DBLE( LENV(IVEHCL(IV)) )
      LVAP  (IV) = LENVAP
C-----SET VEHICLE TYPES
      VEHTYP(IV) = 0
      IF ( ( CLASSV(IVEHCL(IV)) . EQ . 'BC'   ) . OR .
     *     ( CLASSV(IVEHCL(IV)) . EQ . 'BC-1' ) )THEN
C-----  SET BICYCLE
        VEHTYP(IV)  = VEHTYP(IV) + LAVTB
      END IF
C-----  SET EMERGENCY VEHICLE
      IF ( IB . GT . NIL )                       THEN
        IF ( VDIEMV(JB) . GT . 0 )               THEN
          VEHTYP(IV)  = VEHTYP(IV) + LAVTE
        END IF
      ELSE
        IF ( IEMERG(IB) )                        THEN
          VEHTYP(IV)  = VEHTYP(IV) + LAVTE
        END IF
      END IF
      IF ( CLASSV(IVEHCL(IV)) . EQ . 'RAIL' )    THEN
C-----  SET RAIL VEHICLE
        VEHTYP(IV)  = VEHTYP(IV) + LAVTR
      END IF
      IF ( VEHTYP(IV) . EQ . 0 )                 THEN
C-----  SET NORMAL VEHICLE
        VEHTYP(IV)  = LAVTV
      END IF
C-----CHECK INBOUND LANE AND OUTBOUND APPROACH ALLOWED VEHICLE TYPE
      IF ( IAND( VEHTYP(IV),LAVT(IL)                 ) . EQ . 0 )
     *                                           GO TO 9700
      IF ( IAND( VEHTYP(IV),AAVT(IABS( NOBAPD(IV) )) ) . EQ . 0 )
     *                                           GO TO 9710
C-----CHECK EMERGENCY VEHICLE CHECK CONFLICTS
      IF ( IAND( VEHTYP(IV),LAVTE ) .NE. 0 )     EVCCON = .TRUE.
C-----SET LANE CONTROL FOR VEHICLE
      LCONTV(IV) = GETLCV( IV,IL )
C-----INITIALIZE THE UNBIASED VEHICLE PARAMETERS
      IF ( IB . GT . NIL )                       THEN
        IF ( ( VDISPD(JB) . LE . 0.0D0       ) . OR .
     *       ( VDISPD(JB) . GT . DBLE( MDS ) ) ) THEN
          VELOLD = DBLE( ISPD(IV) )
          ACCOLD = 0.0D0
          SLPOLD = 0.0D0
        ELSE
          VELOLD = VDISPD(JB)
          ACCOLD = VDIACC(JB)
          SLPOLD = VDISLP(JB)
        END IF
      ELSE
        IF ( ( ENTVEL(IB) . LE . 0.0D0       ) . OR .
     *       ( ENTVEL(IB) . GT . DBLE( MDS ) ) ) THEN
          VELOLD = DBLE( ISPD(IV) )
          ACCOLD = 0.0D0
          SLPOLD = 0.0D0
        ELSE
          VELOLD = ENTVEL(IB)
          ACCOLD = ENTACC(IB)
          SLPOLD = ENTSLP(IB)
          IPRTLO(IV) = IPRTLO(IV) + 2
        END IF
      END IF
      VELNEW = VELOLD
      ACCNEW = ACCOLD
      SLPNEW = SLPOLD
      OLDDTS = 0.0D0
      POSOLD = DBLE( LGEOM(1,IL) )
      POSNEW = DBLE( LGEOM(1,IL) )
      SLPBLK = 0.0D0
      SLPCON = 0.0D0
      SLPLCH = 0.0D0
      SLPNOF = 0.0D0
      DESVEL = ISPD(IV)
      IPOS(IV) = POSNEW
      IVEL(IV) = VELNEW
      IACC(IV) = ACCNEW
      ISLP(IV) = SLPNEW
C-----CHECK MY LANE AND IF BLOCKED THEN SET PARAMETERS FOR BLOCKED LANE
      CALL  CHKMLN
C-----RESET THE PREVIOUS VEHICLE PARAMETERS TO THE NEW NOF IF THE
C-----VEHICLE IS LANE CHANGING, AND INITIALIZE SEVERAL PARAMETERS
C-----FOR THE VEHICLE
      CALL  PREST1  ( .TRUE.,LVMSDD )
C-----IF THIS LANE IS BLOCKED AND THE PREVIOUS VEHICLES POSITION IS GE
C-----THE END OF THE BLOCKED LANE THEN THIS VEHICLE IS THE FIRST VEHICLE
C-----IN THIS BLOCKED LANE
      IF ( (MBLOCK(IV)                      ) . AND .
     *     (PVPOS . GE . DBLE( LGEOM(2,IL) )) )  MFINL(IV) = .TRUE.
C-----IF THE NEW VEHICLE IS THE FIRST VEHICLE IN THE LANE AND THE LANE
C-----IS BLOCKED THEN RESET THE PREVIOUS VEHICLE POSITION TO THE END OF
C-----THE BLOCKED LANE
      IF ( MFINL(IV) . AND . MBLOCK(IV) )        THEN
        ENDLN  = DBLE( LGEOM(2,IL) )
        DSPLCH = DBLE( ISPD(IV) )
        VELLCH = 0.2D0*DSPLCH
        VELLCH = DMAX1( VELLCH,VELOLD,VELNEW )
        VEHLNG = DMIN1( 25.0D0,LENVAP )
        DISLCH = 0.5D0*(ENDLN-POSNEW)
        DISLCH = DMIN1( DISLCH,TIMELC*VELLCH )
        DISLCH = DMAX1( DISLCH,1.5D0*VEHLNG )
        IF ( MAJRLC(IV) )                        THEN
          DISLCH = 0.5D0*XRELMI
        END IF
        DISEND = DMAX1( DISLCH,DMIN1( 0.5D0*ENDLN,
     *                  0.5D0*(ENDLN-POSNEW))      )
        IVPV   = 0
        PVPOS  = DMAX1( ENDLN-DISEND,POSNEW )
        IF ( MFSTPF(IV) )                        THEN
          IF ( FSTACT(IV) )                      THEN
            IF ( FSTPOS(IV) . LT . PVPOS )       PVPOS = FSTPOS(IV)
          END IF
          IF ( VMSASM(IV) . GT . 0 )             THEN
            IF ( (IVMSMG(VMSASM(IV)).EQ.VMSMSI) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSL) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSM) . OR .
     *           (IVMSMG(VMSASM(IV)).EQ.VMSMSC) )THEN
              IF ( VMSPST(IV) . LT . PVPOS )     PVPOS = VMSPST(IV)
            END IF
          END IF
        ELSE
          PVVEL  = 0.001D0
          PVACC  = -32.0D0
          PVSLP  =   0.0D0
        END IF
      END IF
C-----IF THE NEW VEHICLE IS THE FIRST VEHICLE IN THE LANE THEN GO TO
C-----2070 AND CONTINUE ELSE FIND THE MAXIMUM VELOCITY THAT THE NEW
C-----VEHICLE CAN LOG IN AT
      IF ( MFINL(IV) . AND . (.NOT. MBLOCK(IV)) )GO TO 2070
      DIST = PVPOS - DBLE( LGEOM(1,IL) )
C-----IF THE REAR BUMPER OF THE PREVIOUS VEHICLE IS OFF THE START OF THE
C-----LANE THEN GO TO 5010 AND ELIMINATE THE NEW VEHICLE (LANE FULL)
                    IF ( DIST . LT . 0.0D0 )     GO TO 5010
      CRISLP = -0.75D0*SLPMAX*DCHAR(IDRICL(IV))
C-----IF THE PREVIOUS VEHICLE WAS ACCELERATING OR TRAVELING AT A STEADY
C-----SPEED THEN GO TO 2050 AND FIND THE MAXIMUM LOG IN VELOCITY WHEN
C-----THE PREVIOUS VEHICLE WAS ACCELERATING ELSE FIND THE MAXIMUM LOG IN
C-----VELOCITY WHEN THE PREVIOUS VEHICLE WAS DECELERATING
                    IF ( PVACC . GE . 0.0D0 )    GO TO 2050
C-----FIND THE TIME AND DISTANCE REQUIRED TO STOP THE PREVIOUS VEHICLE
C-----AT A CRITICAL ACC/DEC SLOPE OF -SLPMAX TIMES THE MAXIMUM DRIVER
C-----CHARACTERISTIC
      SLPN = -SLPMAX*DCHRMX
      TMAX = 30.0D0
      CALL  TIMSTP  ( PVVEL,PVACC,SLPN,TMAX,TSTP )
                    IF ( TSTP . EQ . TIMERR )    TSTP = 0.0D0
      XSTP = DIST 
     *     + PVVEL*TSTP + 0.5D0*PVACC*TSTP**2 + ONED6*SLPN*TSTP**3
C-----FIND THE TIME TO STOP THIS VEHICLE BEHIND THE PREVIOUS VEHICLE
C-----(WHEN IT STOPS) USING THE CRITICAL SLOPE FOR THIS VEHICLE
      T = (-3.0D0*XSTP/CRISLP)**ONED3
C-----FIND THE VELOCITY THE VEHICLE COULD HAVE BEEN TRAVELING AND STILL
C-----STOP BEHIND THE PREVIOUS VEHICLE
      V = -0.5D0*CRISLP*T**2
C-----FIND THE ACC/DEC AT THE END OF THE STOP FOR THIS VEHICLE
      ACCNEW = CRISLP*T
C-----ITERATE TO FIND THE NEW VEHICLE LOG IN SPEED (DECMAX CHANGES AS
C-----VELOLD CHANGES AND 4 ITERATIONS OF THE LOOP LETS VELOLD CONVERGE
C-----ON AN ADEQUATE LOG IN VELOCITY WHERE THE NEW VEHICLE CAN STOP IN
C-----THE AVAILABLE DISTANCE WITHOUT EXCEEDING HIS CRITICAL SLOPE OR
C-----MAXIMUM DECELERATION FROM THAT LOG IN VELOCITY)
      DO 2040  I = 1 , 4
C-----FIND THE MAXIMUM DECELERATION THAT THE VEHICLE WOULD BE WILLING TO
C-----USE TO STOP FROM HIS OLD VELOCITY
      DECMAX = DUTOL*(-6.0D0-(VELOLD/44.0D0))*DCHAR(IDRICL(IV))
      DECMAX = DMAX1( DECMAX,DMAX(IVEHCL(IV)) )
      VELOLD = 0.0D0
C-----IF THE ACC AT THE TIME OF STOPPING IS GE THE MAXIMUM DECELERATION
C-----THAT THE VEHICLE WOULD BE WILLING TO USE TO STOP FROM VELOLD THEN
C-----SET VELOLD TO THE MAXIMUM OF VELOLD AND V (ACCNEW DOES NOT EXCEED
C-----DECMAX AND THUS IS OK)
      IF ( ACCNEW . GE . DECMAX )                THEN
        VELOLD = DMAX1( VELOLD,V )
      END IF
C-----FIND THE VELOCITY THE VEHICLE COULD HAVE BEEN AT TO STOP IN THE
C-----AVAILABLE DISTANCE AND NOT EXCEEDING DECMAX
C[    IF ( XSTP               .EQ.-2147483647.0 )STOP 'LOGIN  XSTP   01'
      V = DSQRT( -0.75D0*XSTP*DECMAX )
C-----FIND THE TIME TO STOP FROM V
      T = -2.0D0*V/DECMAX
C-----FIND THE ACC/DEC SLOPE REQUIRED TO GET TO DECMAX IN T SECONDS
C-----IF THE ACC/DEC SLOPE REQUIRED TO GET TO DECMAX IN T SECONDS IS GE
C-----THE DRIVERS CRITICAL SLOPE THEN SET VELOLD TO THE MAXIMUM OF
C-----VELOLD AND V (SLOPE DOES NOT EXCEED CRISLP AND THUS IS OK)
      IF ( DECMAX/T . GE . CRISLP )              THEN
        VELOLD = DMAX1( VELOLD,V )
      END IF
C-----SET VELOLD TO THE MINIMUM OF VELOLD AND THE VEHICLES DESIRED SPEED
      VELOLD = DMIN1( VELOLD,DESVEL )
C-----END OF ITERATION LOOP
 2040 CONTINUE
C-----GO TO 2070 AND CONTINUE
      GO TO 2070
 2050 CONTINUE
C-----THE PREVIOUS VEHICLE WAS ACCELERATING OR TRAVELING AT A STEADY
C-----SPEED THUS DECREMENT THE AVAILABLE DISTANCE BY A CAR FOLLOWING
C-----DISTANCE
C[    IF ( DIST               .EQ.-2147483647.0 )STOP 'LOGIN  DIST   01'
      DIST = DIST - 1.7D0*PVVEL/DCHAR(IDRICL(IV))
C-----IF THE AVAILABLE DISTANCE IS LE 0 THEN SET THIS VEHICLES VELOLD TO
C-----THE PREVIOUS VEHICLES VELOCITY
      IF ( DIST . LE . 0.0D0 )                   THEN
        PVVELM = DMAX1( DMIN1( PVVEL,PVVEL+PVACC*DT+0.5D0*PVSLP*DTSQ),
     *                  0.0D0                                          )
        VELOLD = DMIN1( PVVELM,DESVEL )
      END IF
C-----IF THIS VEHICLES OLD VELOCITY IS LE THE PREVIOUS VEHICLES VELOCITY
C-----THEN GO TO 2070 AND CONTINUE
                    IF ( VELOLD . LE . PVVEL )   GO TO 2070
C-----FIND THE TIME REQUIRED TO REDUCE THE VEHICLES VELOCITY TO THE
C-----PREVIOUS VEHICLES VELOCITY AT CRITICAL SLOPE AND WITHIN THE
C-----AVAILABLE DISTANCE
C[    IF ( CRISLP             .EQ.-2147483647.0 )STOP 'LOGIN  CRISLP 01'
      T = (-3.0D0*DIST/CRISLP)**ONED3
C-----FIND THE VELOCITY THE VEHICLE COULD HAVE BEEN AT AND STILL REDUCE
C-----HIS VELOCITY TO THE PREVIOUS VEHICLES VELOCITY IN THE AVAILABLE
C-----DISTANCE
      V = PVVEL - 0.5D0*CRISLP*T**2
C-----FIND THE ACC/DEC AT THE TIME THIS VEHICLES VELOCITY WAS REDUCED TO
C-----THE PREVIOUS VEHICLES VELOCITY
      ACCNEW = CRISLP*T
C-----ITERATE TO FIND THE NEW VEHICLE LOG IN SPEED (DECMAX CHANGES AS
C-----VELOLD CHANGES AND NITERS ITERATIONS OF THE LOOP LETS VELOLD
C-----CONVERGE ON AN ADEQUATE LOG IN VELOCITY WHERE THE NEW VEHICLE CAN
C-----REDUCE HIS LOG IN VELOCITY TO THE PREVIOUS VEHICLES VELOCITY
C-----WITHIN THE AVAILABLE DISTANCE WITHOUT EXCEEDING HIS CRITICAL SLOPE
C-----OR MAXIMUM DECELERATION FROM THAT LOG IN VELOCITY)
      DO 2060  I = 1 , NITERS
C-----FIND THE PORTION OF THE MAXIMUM DECELERATION THAT THE DRIVER WOULD
C-----USE TO STOP HIS VEHICLE FROM VELOLD THAT HE IS WILLING TO USE TO
C-----REDUCE HIS LOG IN VELOCITY TO THE PREVIOUS VEHICLES VELOCITY
      FACT = (VELOLD**2-PVVEL**2)/VELOLD**2
      FACT = DMAX1( DMIN1( FACT,1.0D0 ),0.1D0 )
C-----FIND THE MAXIMUM DECELERATION THAT THE DRIVER WOULD USE TO
C-----DECELERATE TO THE PREVIOUS VEHICLES SPEED
      DECMAX = DUTOL*(-6.0D0-(VELOLD/44.0D0))*DCHAR(IDRICL(IV))*FACT
      DECMAX = DMAX1( DECMAX,DMAX(IVEHCL(IV)) )
      VELOLD = 0.0D0
C-----IF THE ACC AT THE TIME OF REACHING THE PREVIOUS VEHICLES VELOCITY
C-----IS GE THE MAXIMUM DECELERATION THAT THE VEHICLE WOULD BE WILLING
C-----TO USE TO DECELERATE TO THE PREVIOUS VEHICLES SPEED THEN SET
C-----VELOLD TO THE MAXIMUM OF VELOLD AND V (ACCNEW DOES NOT EXCEED
C-----DECMAX AND THUS IS OK)
      IF ( ACCNEW . GE . DECMAX )                THEN
        VELOLD = DMAX1( VELOLD,V )
      END IF
C-----FIND THE VELOCITY THAT THE VEHICLE COULD HAVE BEEN AT AND STILL
C-----REDUCE IT TO THE PREVIOUS VEHICLES VELOCITY IN THE AVAILABLE
C-----DISTANCE AND NOT EXCEED DECMAX
C[    IF ( DIST               .EQ.-2147483647.0 )STOP 'LOGIN  DIST   02'
      IF ( DECMAX . LT . 0.0D0 )                 THEN
        V = PVVEL + DSQRT( -0.75D0*DIST*DECMAX )
C-----  FIND THE TIME TO REDUCE THAT VELOCITY TO THE PREVIOUS VEHICLES
C-----  VELOCITY AND NOT EXCEED DECMAX
        T = -2.0D0*(V-PVVEL)/DECMAX
C-----  FIND THE ACC/DEC SLOPE REQUIRED TO GET TO DECMAX IN T SECONDS
C-----  IF THE ACC/DEC SLOPE REQUIRED TO GET TO DECMAX IN T SECONDS IS
C-----  GE THE DRIVERS CRITICAL SLOPE THEN SET VELOLD TO THE MAXIMUM OF
C-----  VELOLD AND V (SLOPE DOES NOT EXCEED CRISLP AND THUS IS OK)
        IF ( DECMAX/T . GE . CRISLP )              THEN
          VELOLD = DMAX1( VELOLD,V )
        END IF
      END IF
C-----SET VELOLD TO THE MINIMUM OF VELOLD AND THE DRIVERS DESIRED SPEED
      VELOLD = DMIN1( VELOLD,DESVEL )
      IF ( ( VELOLD . LE . DESVEL ) . AND .
     *     ( VELOLD . LE . PVVEL  ) )            THEN
        GO TO 2070
      END IF
      IF ( VELOLD . LE . VELSTP )                THEN
        VELOLD = DESVEL*DBLE( NITERS-I )/DBLE( NITERS )
      END IF
C-----END OF ITERATION LOOP
 2060 CONTINUE
 2070 CONTINUE
      IF ( VELOLD . LE . VELSTP )                THEN
        VELOLD = 2.0D0*VELSTP
      END IF
      IF ( IB . GT . NIL )                       THEN
        IF ( ( VDISPD(JB) . GT . 0.0D0       ) . AND .
     *       ( VDISPD(JB) . LE . DBLE( MDS ) ) ) THEN
          IF ( VDISPD(JB) . GT . VELOLD )        THEN
            WRITE (WRNMSG,603) VDIQIT(JB),IQ(IV),VDISPD(JB),VELOLD
            CALL  PRTWRN  ( WRNMSG )
            CALL  VDIERR  ( TIME,VDIIVN(JB),VDIESD )
          END IF
          VELOLD = VDISPD(JB)
          ACCOLD = VDIACC(JB)
          SLPOLD = VDISLP(JB)
        END IF
      ELSE
        IF ( ( ENTVEL(IB) . GT . 0.0D0       ) . AND .
     *       ( ENTVEL(IB) . LE . DBLE( MDS ) ) ) THEN
          IF ( ENTVEL(IB) . GT . VELOLD )        THEN
            WRITE (WRNMSG,603) QTIME(IB) ,IQ(IV),ENTVEL(IB),VELOLD
            CALL  PRTWRN  ( WRNMSG )
          END IF
          VELOLD = ENTVEL(IB)
          ACCOLD = ENTACC(IB)
          SLPOLD = ENTSLP(IB)
        END IF
      END IF
      CRISLP = -SLPMAX*DCHAR(IDRICL(IV))
C-----INITIALIZE SOME PARAMETERS NECESSARY FOR SUBROUTINE ACCEL AND
C-----SUBROUTINE CARFOL
      ENDLN = DBLE( LGEOM(4,IL) ) + 1.5D0
      IF ( MBLOCK(IV) )                          THEN
        ENDLN = DBLE( LGEOM(2,IL) )
      END IF
      RELEND = ENDLN - POSOLD
C-----CHECK IF THERE IS A MAJOR COLLISION SOMEWHERE IN THE SYSTEM
      IF ( SMJCOL . AND . (.NOT. MAJCOL(IV)) )   THEN
C-----  CHECK LANE AND VEHICLES DOWNSTREAM FOR A MAJOR COLLISION
        CALL  CLMJCL  ( IL,LNEXT(IV),0,KV,LCHKCF,LBVSTP,POSMJC )
        IF ( KV . GT . 0 )                       THEN
C-----    SET THAT THE VEHICLE IS BLOCKED BY A MAJOR COLLISION
          MAJCLB(IV) = (MAJCLB(IV).OR.LBVSTP)
          POSCLB(IV) = DMIN1( POSCLB(IV),POSMJC )
C-----    IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV
C-----    VEHICLE IS AN EMERGENCY VEHICLE THEN SET EVRESP TRUE ELSE
C-----    FALSE
          EVRESP = ( ( IAND( VEHTYP(IV),LAVTE ) . EQ . 0 ) . AND .
     *               ( IAND( VEHTYP(KV),LAVTE ) . NE . 0 ) )
          RESPEV = ( RESPEV . OR . EVRESP )
C-----    THERE IS A COLLISION VEHICLE ON THE LANE BEFORE THIS VEHICLE
C-----    OR DOWNSTREAM THUS CALCULATE A DECEL TO A STOP JUST BEFORE THE
C-----    POINT OF COLLISION OR THE STOP LINE WHICHEVER IS CLOSER
          HWM    = 0.5D0*WIDV(IVEHCL(IV))
          SAFVEL = DMAX1( VELOLD,VELNEW,DESVEL )
          SAFR   = (SAFDIS+(SAFVEL/SAFSPD))/DCHAR(IDRICL(IV))
C-----    IF THIS VEHICLE IS NOT AN EMERGENCY VEHICLE AND THE KV VEHICLE
C-----    IS AN EMERGENCY VEHICLE THEN CHECK SAFR
          IF ( EVRESP )                          THEN
            SAFR = DMAX1( SAFR,EVEHRZ*SAFVEL )
          END IF
          POSVEH = DMIN1( POSCLB(IV)-2.0D0*HWM-SAFR,
     *                    DBLE( LGEOM(4,IL) )+1.5D0 )
          VELVEH = 0.0D0
          ACCVEH = 0.0D0
          SLPVEH = 0.0D0
          CALL  SLPCFS  ( SLPTMP,IV,POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                              POSVEH,VELVEH,ACCVEH,SLPVEH  )
          IF ( SLPTMP . NE . 0.0D0 )             THEN
            IF ( SLPTMP . LT . SLPBLK )          THEN
              SLPBLK = SLPTMP
              RESPEV = ( RESPEV . OR . EVRESP )
            END IF
          END IF
        END IF
C-----  SET THAT THE VEHICLE MUST CHECK CONFLICTS BECAUSE A MAJOR
C-----  COLLISION MAY BLOCK THIS VEHICLE
        IF ( LCHKCF )                            THEN
          MAJCON(IV) = .TRUE.
          POSCON(IV) = DMIN1( POSCON(IV),POSMJC )
        END IF
      END IF
 2080 CONTINUE
C-----PREDICT THE POS/VEL/ACC FOR THE VEHICLE AFTER XTIMEL SECONDS
C[    IF ( XTIMEL             .EQ.-2147483647.0 )STOP 'LOGIN  XTIMEL 01'
      CALL  NEWVEL  ( XTIMEL,XTIMEL**2,XTIMEL**3 )
                    IF ( MFINL(IV) )             GO TO 2100
C-----CALCULATE THE ACC/DEC SLOPE REQUIRED TO FOLLOW THE VEHICLE AHEAD
      CALL  CARFOL
C[    IF ( CRISLP             .EQ.-2147483647.0 )STOP 'LOGIN  CRISLP 02'
                    IF ( SLPNEW.GE.0.8D0*CRISLP )GO TO 2090
      VELOLD = 0.95D0*VELOLD
C*    NRNAME = NRNAME - 2
                    IF ( VELOLD . GT . VELSTP )  GO TO 2080
 2090 CONTINUE
C-----CALCULATE THE POS/VEL/ACC FOR THE VEHICLE AFTER XTIMEL SECONDS
C[    IF ( XTIMEL             .EQ.-2147483647.0 )STOP 'LOGIN  XTIMEL 02'
      CALL  NEWVEL  ( XTIMEL,XTIMEL**2,XTIMEL**3 )
C-----IF THIS VEHICLE HAD A COLLISION WITH THE PREVIOUS VEHICLE OR THE
C-----VEHICLE STOPPED DURING THE PORTION OF THIS DT THEN GO TO 5010 AND
C-----ELIMINATE THE NEW VEHICLE
            IF ( POSNEW . GE . PVPOS+XRELMI )    GO TO 5010
                    IF ( VELNEW . LE . VELSTP )  GO TO 5010
 2100 CONTINUE
C-----UPDATE THE AVERAGE PERCENT LOGIN VELOCITY TO DESIRED SPEED FOR
C-----THIS APPROACH
      PLVDV(IAN) = PLVDV(IAN) + VELOLD/DBLE( ISPD(IV) )
      NLVDV(IAN) = NLVDV(IAN) + 1
C-----UPDATE THE NEW VEHICLES SIMULATION STATISTICS THUS IF THE
C-----VELOCITY IS LE XFPS THEN INCREMENT THE DELAY BELOW XX MPH
C[    IF ( XTIMEL             .EQ.-2147483647.0 )STOP 'LOGIN  XTIMEL 03'
      IF ( VELNEW . LE . XFPS )                  THEN
        IDVS(IV) = IDNINT( XTIMEL/DT )
      END IF
C-----INCREMENT THE NUMBER OF VEHICLES IN THE SYSTEM, THE INBOUND
C-----APPROACH, AND THE INBOUND LANE
      NVSY = NVSY + 1
      NVIA(IA) = NVIA(IA) + 1
      NVIBA = NVIBA + 1
      NVIL(ILN,IA) = NVIL(ILN,IA) + 1
C-----BIAS THE VEHICLE ATTRIBUTES, SET THE PREVIOUS VEHICLE PARAMETERS,
C-----AND UPDATES THE MAXIMUM ACC/DEC FOR THE VEHICLE
      IPR = 2
      CALL  BIAS    ( IPR )
C7    POSLC7 = 0.0D0
C7    WRITE (PPP,701) TIME,IQ(IV),1,IA,IL,IVEHCL(IV),POSNEW,POSLC7
C0          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 101
C-----PRINT POS/VEL/ACC FOR THE VEHICLE
CZ    CALL  PVAPRT
C0101 CONTINUE
CV          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 102
CU                  IF ( TIME . LT . TPRINT )    GO TO 102
C3    WRITE (IPFLAG,702) VELOLD
C[    IF ( XTIMEL             .EQ.-2147483647.0 )STOP 'LOGIN  XTIMEL 04'
C3    WRITE (JPFLAG,703) XTIMEL
C3    KPFLAG = 'LOGGED IN '
CS    POSLCS = 0.0D0
C[    IF ( NOFT               .EQ.-2147483647   )STOP 'LOGIN  NOFT   03'
CS    WRITE (6,704) IA,ILN,IV,IQ(IV),NOFT,NOR(IV),IPRC(1,IV),
CS   *              NORC(1,IV),IPRC(2,IV),NORC(2,IV),POSNEW,VELNEW,
CS   *              ACCNEW,SLPNEW,ISPD(IV),IVEHCL(IV),IDRICL(IV),
CS   *              LNEXT(IV),NOBAPD(IV),ISET(IV),LEGAL(IV),LOGFLG(IV),
CS   *              LCHGE(IV),IPRTM(IV),POSLCS,ISISET(ICAMPC,IBLN(IL)),
CS   *              IPFLAG,JPFLAG,KPFLAG
CU102 CONTINUE
CY          IF ( IAND( IPRTLO(IV),1 ) . EQ . 0 ) GO TO 103
CW                  IF ( TIME . LT . TPRINT )    GO TO 103
CW    WRITE (6,756) IV,ISLP  (IV),IACC  (IV),IVEL  (IV),IPOS  (IV),
CW   *                 ISET  (IV),LCHGE (IV),ISPDP (IV),LEGAL (IV),
CW   *                 IPRTM (IV),ITIMV (IV),IQDS  (IV),ISPDS (IV),
CW   *                 ISDS  (IV),IDVS  (IV),ISTCON(IV),IVMAXA(IV),
CW   *                 IVMAXD(IV),LATPOS(IV),IDTS  (IV),LALT  (IV),
CW   *                 IPRC(1,IV),NORC(1,IV),IPRC(2,IV),NORC(2,IV),
CW   *                 LOGFLG(IV),MBLOCK(IV),MFGOF (IV),MFINL (IV),
CW   *                 MFSTPF(IV),MLAG  (IV),MOASF (IV),MPOBS (IV),
CW   *                 MPRO  (IV),MSAOR (IV),MSFLG (IV),MSTPF (IV),
CW   *                 MTCARS(IV),MININT(IV),IACDS (IV),IACLDS(IV),
CW   *                 ICDFS (IV),IFVA  (IV),IRSTOP(IV),ISDEC (IV),
CW   *                 ISTMO (IV),ITIMVI(IV),IQDSI (IV),ISPDSI(IV),
CW   *                 ISDSI (IV),IDVSI (IV),IVMXAI(IV),IVMXDI(IV),
CW   *                 IDTSI (IV),IUPDAT(IV),JVCNOR(IV),IDISPD(IV),
CW   *                 IQ(IV)
CW    WRITE (6,757) IV,IDRICL(IV),IVEHCL(IV),ISPD  (IV),NOF   (IV),
CW   *                 NOR   (IV),LNEXT (IV),LPRES (IV),ITURN (IV),
CW   *                 IBAPS (IV),IPRTLO(IV),IEXTIM(IV),NOBAPD(IV),
CW   *                 INT2P (IV),INT2S (IV),INT1T (IV),IEXTII(IV)
CW    WRITE (6,758) IV,MDEDIC(IV),MINFLZ(IV),MLUNC (IV),MIUNC (IV),
CW   *                 MLYELD(IV),MLSTOP(IV),MATSTL(IV),MSSRED(IV),
CW   *                 MLRTOR(IV),MSSGRN(IV),MCHKCF(IV),
CW   *                 IDEDIC(IV),INFLZ (IV),ILUNC (IV),ILYELD(IV),
CW   *                 ILSTOP(IV),ICONTN(IV),ICHKCF(IV),IERROR(IV)
CW103 CONTINUE
 4010 CONTINUE
C-----IF LAST VEHICLE FROM VEHICLE DATA INSERT THEN GO TO 4030
                    IF ( IB . GT . NIL )         GO TO 4030
C-----IF THERE HAS ALREADY BEEN AN END-OF-FILE ENCOUNTERED ON THE
C-----DRIVER-VEHICLE PROCESSOR TAPE THEN GO TO 4020 AND FLAG THE QUEUE
C-----BUFFER NOT IN USE, DECREMENT THE NUMBER OF VEHICLES IN THE QUEUE
C-----BUFFERS, AND SET THE END-OF-FILE FLAG TRUE
                    IF ( IEF )                   GO TO 4020
C-----READ THE NEXT VEHICLE FROM THE DRIVER-VEHICLE PROCESSOR TAPE INTO
C-----THE QUEUE BUFFER JUST ASSIGNED
C[    IF ( IB                 .EQ.-2147483647   )STOP 'LOGIN  IB     03'
C-----READ QTIME,IVEHCL,IDRICL,DESVEL,NOBAPD,IA,ILN,IPRTLO,IFUT,
C-----     QSTTIM,FSTPIA,QSTPOS,QSTDTM,QGOTIM,QGOATM,QRRTIM,QRRATM,
C-----     JVN,VEMERG,ENTVEL,ENTACC,ENTSLP
      READ (IVEHP,502,END=4020) QTIME(IB),(IBUF(I,IB),I=1,IBUFPL),IFUT,
     *                          QSTTIM(IB),IBUF(IBUFFS,IB),QSTPOS(IB),
     *                          QSTDTM(IB),
     *                          QGOTIM(IB),QGOATM(IB),
     *                          QRRTIM(IB),QRRATM(IB),
     *                          JVN,VEMERG,
     *                          ENTVEL(IB),ENTACC(IB),ENTSLP(IB)
C[    IF ( IFUT               .EQ.'~' )          STOP 'LOGIN  IFUT   01'
      IF ( IFUT . EQ . 'F' )                     THEN
        IF ( GVERSN . LT . 3.12 )                GO TO 9370
        IF ( DVERSN . LT . 3.12 )                GO TO 9380
        IBUF(IBUFDO,IB) = -IBUF(IBUFDO,IB)
      END IF
                    IF ( JVN    . EQ . 0      )  JVN    = NUMV
                    IF ( VEMERG . EQ . IBLNK1 )  VEMERG = INO
      IEMERG(IB) = (VEMERG . EQ . IYES)
C-----SET THE SEQUENTIAL VEHICLE NUMBER FOR THIS VEHICLE
      IBUF(IBUFVN,IB) = JVN
      NUMV = NUMV + 1
C2                  IF ( IBUF(IBUFPL,IB) .EQ. 0 )GO TO 104
C1                  IF ( TIME . LT . TPRINT )    GO TO 104
C1    WRITE (6,705) IB,IBUF(IBUFVN,IB),QTIME(IB),(IBUF(I,IB),I=1,IBUFPL)
C1104 CONTINUE
      GO TO 4030
 4020 CONTINUE
C-----FLAG THE QUEUE BUFFER NOT IN USE, DECREMENT THE NUMBER OF VEHICLES
C-----IN THE QUEUE BUFFER, AND SET THE END-OF-FILE FLAG TRUE
C[    IF ( IB                 .EQ.-2147483647   )STOP 'LOGIN  IB     04'
      IF ( IB . LE . NIL )                       THEN
        QTIME(IB) = -1.0D0
        IQF = IQF - 1
        IEF = .TRUE.
      END IF
 4030 CONTINUE
C-----CLEAR THE QUEUE BUFFER POINTER
      LQ(IAN,ILN) = 0
      RETURN
 5010 CONTINUE
C-----ELIMINATE THE VEHICLE FROM THE SIMULATION THUS INCREMENT THE
C-----NUMBER OF VEHICLES ELIMINATED FOR THIS APPROACH
      NELIM(IAN) = NELIM(IAN) + 1
C[    IF ( IB                 .EQ.-2147483647   )STOP 'LOGIN  IB     06'
      IF ( IB . GT . NIL )                       THEN
        WRITE (SER   ,601) VDIQIT(JB),VDIIVN(JB),     VDIIBN(JB),
     *                                VDIILN(JB),     VDIOBN(JB),
     *                                VDIVCN(JB),     VDIDCN(JB),
     *                                VDIDSP(JB),     VDIPLO(JB)
        WRITE (WRNMSG,602) VDIQIT(JB),VDIIVN(JB)
        CALL  PRTWRN  ( WRNMSG )
        CALL  VDIERR  ( TIME,VDIIVN(JB),VDIELF )
      ELSE
        WRITE (SER   ,601) QTIME(IB) ,IBUF(IBUFVN,IB),IBUF(IBUFIA,IB),
     *                                IBUF(IBUFLN,IB),IBUF(IBUFDO,IB),
     *                                IBUF(IBUFVC,IB),IBUF(IBUFDC,IB),
     *                                IBUF(IBUFDV,IB),IBUF(IBUFPL,IB)
        WRITE (WRNMSG,602) QTIME(IB) ,IBUF(IBUFVN,IB)
        CALL  PRTWRN  ( WRNMSG )
      END IF
C-----FLAG THE INDEX NOT IN USE
      IQ(IV) = 0
      IQQ = IQQ - 1
                    IF ( IQQ . LE . 0 )          IQQ = NVE
C-----SET THE LAST VEHICLE IN THE LANE TO THIS VEHICLES NOFT
C[    IF ( NOFT               .EQ.-2147483647   )STOP 'LOGIN  NOFT   04'
      ILVL(IL) = NOFT
                    IF ( NOFT . GT . 0 )         GO TO 5020
C-----THERE WAS NO NOFT VEHICLE THUS SET THE FIRST VEHICLE IN THE
C-----LANE TO ZERO
      IFVL(IL) = 0
      GO TO 4010
 5020 CONTINUE
C-----SET THE NOR FOR THE NOFT VEHICLE TO ZERO
C[    IF ( NOFT               .EQ.-2147483647   )STOP 'LOGIN  NOFT   05'
      NOR(NOFT) = 0
      GO TO 4010
C-----PROCESS THE EXECUTION ERROR AND STOP
 9200 CONTINUE
      WRITE (ERRMSG,920) NVE
      CALL  ABORTR  ( ERRMSG )
      STOP  920
 9370 CONTINUE
      CALL  ABORTR  ( 'STOP 937 - '                                //
     *                'GEOPRO VERSION LT 3.12 FOR FREE U-TURNS - ' //
     *                'LOGIN'                                         )
      STOP  937
 9380 CONTINUE
      CALL  ABORTR  ( 'STOP 938 - '                               //
     *                'DVPRO VERSION LT 3.12 FOR FREE U-TURNS - ' //
     *                'LOGIN'                                        )
      STOP  938
 9520 CONTINUE
      WRITE (ERRMSG,952) JP,NPATHS
      CALL  ABORTR  ( ERRMSG )
      STOP  952
 9530 CONTINUE
      WRITE (ERRMSG,953) FSTPOS(IV),LENP(JP)
      CALL  ABORTR  ( ERRMSG )
      STOP  953
 9700 CONTINUE
      WRITE (ERRMSG,970) AVTYPS( LAVT(IL) ),AVTYPS( VEHTYP(IV) )
      CALL  ABORTR  ( ERRMSG )
      STOP  970
 9710 CONTINUE
      WRITE (ERRMSG,971) AVTYPS( AAVT(IABS( NOBAPD(IV) )) ),
     *                   AVTYPS( VEHTYP(IV)               )
      CALL  ABORTR  ( ERRMSG )
      STOP  971
      END                                                               LOGIN
