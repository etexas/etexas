      SUBROUTINE SIMDT(TMPARM) BIND(C, name='simulateDT')
C
C *** ************************************************************** ***
C *** *                                                            * ***
C *** *  Copyright (c) 2010 Harmonia Holdings Group LLC            * ***
C *** *                                                            * ***
C *** * Permission is hereby granted to use, modify, copy, and     * ***
C *** * distribute this software and its documentation for any     * ***
C *** * purpose only without profit, provided that the above       * ***
C *** * Copyright Notice appears in all copies and that both the   * ***
C *** * Copyright Notice and this Permission Notice appears in     * ***
C *** * every copy of supporting documentation.  No title to nor   * ***
C *** * ownership of the software is transferred hereby.  The name * ***
C *** * of Harmonia Holdings Group LLC shall not be used in        * ***
C *** * advertising or publicity related to the distribution of    * ***
C *** * the software without specific, written, prior permission.  * ***
C *** * This software is provided as-delivered without expressed   * ***
C *** * or implied warranty.  Harmonia Holdings Group LLC          * ***
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

      DOUBLE PRECISION TMPARM

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
C-----SUBROUTINE SIMDT IS THE MAIN DRIVER FOR SIMPRO AND CONTROLS THE
C-----CALLING OF THE VARIOUS OTHER ROUTINES
C-----
C----- PARAMETERS:
C-----    TMPARM - THE CURRENT STEP IN TIME FOR CALLERS TO USE
C
C
C-----BEGIN TIMESTEP SIMULATION
C
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
C[    IF ( IPRCNT             .EQ.-2147483647   )STOP 'SIMDT   IPRCNT 01'
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
                    IF ( NVSY+IQF . LE . 0 )     GO TO 4010
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
                    IF ( NVIBA+IQF . LE . 0 )    GO TO 3010
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
C
C-----ASSIGN THE TMPARM SUBROUTINE PARAMETER USING THE CURRENT
C-----TIME SO CALLING PROGRAMS CAN USE THIS DATA
C
      TMPARM=TIME
      GO TO 4030
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
C
C-----ASSIGN THE TMPARM SUBROUTINE PARAMETER A VALUE INDICATING
C-----THAT THE SIMULATION IS DONE
C
      TMPARM = -1.0
 4030 CONTINUE
      RETURN
      END                                                               SIMDT