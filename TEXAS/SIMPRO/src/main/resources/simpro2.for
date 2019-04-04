      SUBROUTINE INITAL
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
      INCLUDE 'APPRO'
      INCLUDE 'ARC'
      INCLUDE 'CHARAC'
      INCLUDE 'CWDDIR'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LINE'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INCLUDE 'LOOPS'
      INCLUDE 'PATH'
      INCLUDE 'PHASES'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SDR'
      INCLUDE 'SIGCAM'
      INCLUDE 'SUMST2'
      INCLUDE 'TITLE'
      INCLUDE 'USER'
      INCLUDE 'USFILE'
      BYTE              SSAMID,SSAMUN
      CHARACTER*5       IFORM
      CHARACTER*60      DEFFN
      LOGICAL           ERR,OUTBND
      INTEGER           DEG,FILNUM,I,IARC,ILINE,ILNB,ILOOP,ISDRC,J,JA,
     *                  JTYPLD,K,LL,N,NC,NC1,NCPV(PARNSP),NOUT,NUMREP,P
C8    INTEGER           ISIG8
C+    INTEGER           ISIGP
      INTEGER*4         SSAMAX,SSAMAY,SSAMIX,SSAMIY
      REAL*4            ADD,ADDAZ,SSAMSC,XPOS,YPOS,ZERO
      DOUBLE PRECISION  POSPTH,TIMVAL
      DATA FILNUM / 0          /
      DATA NCPV   / PARNSP*-1  /
      DATA ZERO   / 0.000001D0 /
  401 FORMAT(F4.2)
  501 FORMAT(A)
  502 FORMAT(A,1X,F4.2)
  503 FORMAT(2F6.1,F4.2,29X,I2,A3,A5,F6.1,L1,A3)
  504 FORMAT(20I4)
  505 FORMAT(I3,6I4,2I3,3I4,2I3,5I4,I3,2I2)
  506 FORMAT(A)
  507 FORMAT(3I4,1X,A8,I3,4(I3,3I2,2I3,2I2),:(/,24X,4(I3,3I2,2I3,2I2)))
  602 FORMAT(1X,A,' PROCESSOR FILE NAME AND TITLE:',/,1X,A,/,1X,A,/)
  603 FORMAT(A,' Processor file name and title:',/,A,/,A,/)
C8605 FORMAT(3I2,6X,F6.1)
C+606 FORMAT(3I2,F7.1)
C6701 FORMAT('1 POSITION (20.0 FT PER COL)'/
C6   *     'CAREQL ='F10.4'  CAREQM ='F10.4'  CAREQA ='F10.4/
C6   *     10X'0 200   400     600       8001000'
C6   * '1200  1400    1600      18002000'
C6   * '2200  2400'/
C6   *     10X'%IIIIIIIII%IIIIIIIII%IIIIIIIII%IIIIIIIII%IIIIIIIII%'
C6   * 'IIIIIIIII%IIIIIIIII%IIIIIIIII%IIIIIIIII%IIIIIIIII%'
C6   * 'IIIIIIIII%IIIIIIIII%')
C6702 FORMAT('1 VELOCITY (0.5 FT/SEC PER COL)'/
C6   *     'CAREQL ='F10.4'  CAREQM ='F10.4'  CAREQA ='F10.4/
C6   *     10X'0    1020'
C6   * '    3040'
C6   * '    5060'/
C6   *     10X'%IIIIIIIIIIIIIIIIIII%IIIIIIIIIIIIIIIIIII%'
C6   * 'IIIIIIIIIIIIIIIIIII%IIIIIIIIIIIIIIIIIII%'
C6   * 'IIIIIIIIIIIIIIIIIII%IIIIIIIIIIIIIIIIIII%')
C6703 FORMAT('1 ACCELERATION/DECELERATION (0.2 FT/SEC/SEC PER COL)'/
C6   *     'CAREQL ='F10.4'  CAREQM ='F10.4'  CAREQA ='F10.4/
C6   *     9X'10    9    87    6  5    4    3 2    1    0'
C6   * '   -1   -2   -3   -4 -5   -6   -7-8   -9  -10'
C6   * '  -11  -12  -13  -14-15'/
C6   *     9X' %IIII%IIII%IIII%IIII%IIII%IIII%IIII%IIII%IIII%IIII%'
C6   * 'IIII%IIII%IIII%IIII%IIII%IIII%IIII%IIII%IIII%IIII%'
C6   * 'IIII%IIII%IIII%IIII%IIII%')
  801 FORMAT('END-OF-FILE ON FIRST READ OF ',A,' INPUT ON TAPE',I2)
  893 FORMAT(49HREAD ERROR READING REPRUN VALUE FROM COMMAND LINE)
  894 FORMAT(14HREPRUN VALUE =,I6,14H IS LT 1 OR GT,I3)
C
C-----SUBROUTINE INITAL INITIALIZES THE PARAMETERS FOR THE SIMULATION
C
C[    IFORM      = '~~~~~'
C[    I          = -2147483647
C[    N          = -2147483647
C[    NC1        = -2147483647
C[    NC         = -2147483647
C[    NOUT       = -2147483647
C[    NUMREP     = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'INITAL'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----CHECK PARAMETER VALUES - CODE MUST BE MODIFIED IF DIFFERENT
C-----SUBROUTINE RCAMSD - FORMAT 502 - 75A1 (25A3)
                    IF ( NIL . NE . 25 )         GO TO 9650
C-----SUBROUTINE RPHASD - FORMAT 503 - 40I2 (3 NUMS PLUS 37 DETECTORS)
                    IF ( NPL . GT . 37 )         GO TO 9660
C-----TIMERR MUST BE A LARGE POSITIVE TIME (9960 SECS MAX STRTIM+SIMTIM)
                    IF ( TIMERR .LT. 10000.0D0 ) GO TO 9670
      READ (IVERSN(2:5),401,ERR=9340) SVERSN
C-----GET TM TIME FOR THIS JOB AT ITS BEGINNING
C>    CALL  EXTIME  ( 1 )
      CALL  CLFILE  ( FILNUM,PARNSP,PAR,PARVAL,NCPV,'simpro' )
      CWDDIR = WRKDIR
      CALL  TOUPR   ( PAUSND )
      N = 1
      CALL  GDVS0   ( N,NGD,NOUT,NIO,I,NPRCNT,I,I,SYSDAT,
     *                'NO','NO','NO',USFILE,'NO','NPRCNT','NO','NO' )
      IF ( N . EQ . -1 )                         GO TO 8840
      IF ( NCPV(10)       .EQ.0 )  SSAM  ='ssam.trj'
C%    IF ( ILNB ( CWDDIR ).GT.0 )  USFILE=CWDDIR
C%    IF ( ILNB ( ERRFIL ).GT.0 )  CALL PCFS ( ERRFIL,USFILE,ERRFIL )
C%    IF ( ILNB ( IFILE  ).GT.0 )  CALL PCFS ( IFILE ,USFILE,IFILE  )
C%    IF ( ILNB ( LFILE  ).GT.0 )  CALL PCFS ( LFILE ,USFILE,LFILE  )
C%    IF ( ILNB ( PVAFIL ).GT.0 )  CALL PCFS ( PVAFIL,USFILE,PVAFIL )
C%    IF ( ILNB ( SSAM   ).GT.0 )  CALL PCFS ( SSAM  ,USFILE,SSAM   )
C%    IF ( ILNB ( STAFIL ).GT.0 )  CALL PCFS ( STAFIL,USFILE,STAFIL )
C%    IF ( ILNB ( T8FILE ).GT.0 )  CALL PCFS ( T8FILE,USFILE,T8FILE )
C%    IF ( ILNB ( T9FILE ).GT.0 )  CALL PCFS ( T9FILE,USFILE,T9FILE )
      NCERR  = ILNB( ERRFIL )
      NCIF   = ILNB( IFILE  )
      NCLF   = ILNB( LFILE  )
      NCPVA  = ILNB( PVAFIL )
      NCSSAM = ILNB( SSAM   )
      NCSTA  = ILNB( STAFIL )
      NCT8   = ILNB( T8FILE )
      NCT9   = ILNB( T9FILE )
      IF ( NCERR . EQ . 0 )                      THEN
        ERRFIL = DEFFN( SER )
        NCERR = ILNB( ERRFIL )
      END IF
      IF ( NCPVA . EQ . 0 )                      THEN
        PVAFIL = DEFFN( NPD )
        NCPVA = ILNB( PVAFIL )
      END IF
      IF ( NCSTA . EQ . 0 )                      THEN
        STAFIL = DEFFN( ICS )
        NCSTA = ILNB( STAFIL )
      END IF
      IF ( NCLF . GT . 0 )                       THEN
        CALL  OPENDL  ( 6,LFILE )
      END IF
      REWIND (6)
      CALL  PHEADR  ( 6 )
      CALL  TXINQ   ( 6,' ',' ',LFILE,ERR )
      NCLF = ILNB( LFILE )
      CALL  OPNSIF  ( NSP,INPUT )
      CALL  OPNSIF  ( NGP,IGEOP )
      CALL  OPNSIF  ( NDV,IVEHP )
      IF ( NCERR . GT . 0 )                      THEN
        CALL  OPENDL  ( SER,ERRFIL )
      END IF
      REWIND (SER)
      CALL  PHEADR  ( SER )
      CALL  TXINQ   ( SER,' ',' ',ERRFIL,ERR )
      NCERR = ILNB( ERRFIL )
      ICAMPC = 1
C-----READ AND ECHO-PRINT THE TITLE FROM THE GEOMETRY PROCESSOR TAPE
      READ (IGEOP,501,END=8010) GTITLE
      NC1 = MAX0( 1,ILNB( GTITLE ) )
      WRITE (6  ,602) 'GEOMETRY',T8FILE(1:NCT8),GTITLE(1:NC1)
      WRITE (SER,602) 'GEOMETRY',T8FILE(1:NCT8),GTITLE(1:NC1)
      NC1 = MIN0( 77,NC1 )                                              CCODE=C#
C}    NC1 = MIN0( 77,NC1 )
C%    NC1 = MIN0( 77,NC1 )
      WRITE (*  ,603) 'Geometry',T8FILE(1:NCT8),GTITLE(1:NC1)
C|    WRITE (*  ,501) ' '
C?    WRITE (*  ,501) ' '
C8    WRITE (IQD,501) GTITLE
C+    WRITE (IDH,501) GTITLE
C-----READ AND ECHO-PRINT THE TITLE FROM THE DRIVER-VEHICLE PROCESSOR
C-----TAPE
      READ (IVEHP,501,END=8020) DTITLE
      NC1 = MAX0( 1,ILNB( DTITLE ) )
      WRITE (6  ,602) 'DRIVER-VEHICLE',T9FILE(1:NCT9),DTITLE(1:NC1)
      WRITE (SER,602) 'DRIVER-VEHICLE',T9FILE(1:NCT9),DTITLE(1:NC1)
      NC1 = MIN0( 77,NC1 )                                              CCODE=C#
C}    NC1 = MIN0( 77,NC1 )
C%    NC1 = MIN0( 77,NC1 )
      WRITE (*  ,603) 'Driver-Vehicle',T9FILE(1:NCT9),DTITLE(1:NC1)
C|    WRITE (*  ,501) ' '
C?    WRITE (*  ,501) ' '
C8    WRITE (IQD,501) DTITLE
C+    WRITE (IDH,501) DTITLE
C-----READ AND ECHO-PRINT THE TITLE FROM THE INPUT DIRECTLY TO THE
C-----SIMULATION PROCESSOR
      READ (INPUT,502,END=8030) STITLE,SVERSR
      IF ( SVERSR . LE . 0.0 )                   SVERSR = 5.99
      CALL  GETCDT  ( STITLE(61:80) )
      STITLE(60:60) = IBLNK1
      IF ( REPRUN . NE . INO )                   THEN
        NC = ILNB( REPRUN )
        IF ( NC . GT . 0 )                       THEN
          WRITE (IFORM,'(2H(I,I2.2,1H))') NC
          READ (REPRUN,IFORM,ERR=8930) NUMREP
          IF ( ( NUMREP . LT . 1              ) . OR .
     *         ( NUMREP . GT . MIN0( 99,NRP ) ) )GO TO 8940
          WRITE (STITLE(56:59),'(2H R,I2.2)') NUMREP
        END IF
      END IF
      NC1 = MAX0( 1,ILNB( STITLE ) )
      WRITE (6  ,602) 'SIMULATION',IFILE(1:NCIF),STITLE(1:NC1)
      WRITE (SER,602) 'SIMULATION',IFILE(1:NCIF),STITLE(1:NC1)
      NC1 = MIN0( 77,NC1 )                                              CCODE=C#
C}    NC1 = MIN0( 77,NC1 )
C%    NC1 = MIN0( 77,NC1 )
      WRITE (*  ,603) 'Simulation',IFILE(1:NCIF),STITLE(1:NC1)
C|    WRITE (*  ,501) ' '
C?    WRITE (*  ,501) ' '
C8    WRITE (IQD,501) STITLE
C+    WRITE (IDH,501) STITLE
      WRITE (*  ,501) '  Time  Vehicles'
C-----READ THE USER DATA FROM CARD 2 OF THE INPUT DIRECTLY TO THE
C-----SIMULATION PROCESSOR AND CHECK FOR ERRORS
      CALL  RUSERD
C6    WRITE (IPP,701) CAREQL,CAREQM,CAREQA
C6    WRITE (IPV,702) CAREQL,CAREQM,CAREQA
C6    WRITE (IPA,703) CAREQL,CAREQM,CAREQA
C-----READ THE GEOMETRY PROCESSOR DATA FROM THE GEOMETRY PROCESSOR TAPE
C-----AND READ THE LANE CONTROL INFORMATION FROM CARD 3 OF THE INPUT
C-----DIRECTLY TO THE SIMULATION PROCESSOR AND CHECK FOR ERRORS
      CALL  RGEOPD
      NPHASE = 0
C-----IF THE INTERSECTION IS NOT SIGNAL CONTROLLED THEN GO TO 1010 ELSE
C-----READ THE CAM STACK INFORMATION FROM THE INPUT DIRECTLY TO THE
C-----SIMULATION PROCESSOR AND CHECK FOR ERRORS
                    IF ( ICONTR . LT . ICPSIG )  GO TO 1010
      CALL  RCAMSD
C-----IF THE INTERSECTION IS NOT SEMI-ACTUATED, FULL-ACTUATED SIGNAL,
C-----TEX-DIA, DAL-DIA, NEMA, OR HARDWARE CONTROLLED THEN GO TO 1010
C-----ELSE READ THE SIGNAL PHASE INFORMATION FROM THE INPUT DIRECTLY TO
C-----THE SIMULATION PROCESSOR AND CHECK FOR ERRORS
                    IF ( ICONTR . LT . ICSACT )  GO TO 1010
      CALL  RPHASD
C-----IF NO DETECTORS WERE DECLARED FOR ANY OF THE SEMI-ACTUATED OR
C-----FULL-ACTUATED SIGNAL PHASES THEN GO TO 1010 ELSE READ THE DETECTOR
C-----INFORMATION FROM THE INPUT DIRECTLY TO THE SIMULATION PROCESSOR
C-----AND CHECK FOR ERRORS
                    IF ( NLOOPS . LE . 0 )       GO TO 1010
      CALL  RLOOPD
 1010 CONTINUE
C-----READ THE DRIVER-VEHICLE PROCESSOR DATA FROM THE DRIVER-VEHICLE
C-----PROCESSOR TAPE, INITIALIZE THE QUEUE BUFFERS, AND CHECK FOR ERRORS
      CALL  RDVPRD
C8    ISIG8 = 0
C8                  IF ( ICONTR . EQ . ICPSIG )  ISIG8 = 99
C8                  IF ( ICONTR . GE . ICSACT )  ISIG8 = 88
C8    WRITE (IQD,605) ISIG8,ICPHAS,ICAMPC,TIME
C+    ISIGP = 0
C+                  IF ( ICONTR . EQ . ICPSIG )  ISIGP = 99
C+                  IF ( ICONTR . GE . ICSACT )  ISIGP = 88
C+    WRITE (IDH,606) ISIGP,ICPHAS,ICAMPC,TIME
C-----READ THE VEHICLE MESSAGE SYSTEM DATA
      CALL  RVMSM
C-----DETERMINE MAXIMUM AND MINIMUM X AND Y COORDINATES
      XMAXIN = -9999.0D0
      XMININ =  9999.0D0
      YMAXIN = -9999.0D0
      YMININ =  9999.0D0
C-----PROCESS EACH INBOUND APPROACH
      DO 2020  IAN = 1 , NIBA
      IA = LIBA(IAN)
C-----PROCESS EACH LANE OF THE INBOUND APPROACH
      DO 2010  ILN = 1 , NLANES(IA)
      IL = LLANES(ILN,IA)
      IF ( LGEOM(1,IL) . EQ . LGEOM(2,IL) )      THEN
        POSPTH = LGEOM(3,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
        POSPTH = LGEOM(4,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
      ELSE IF ( LGEOM(3,IL) .EQ. LGEOM(4,IL) )   THEN
        POSPTH = LGEOM(1,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
        POSPTH = LGEOM(2,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
      ELSE
        POSPTH = LGEOM(1,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
        POSPTH = LGEOM(4,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
      END IF
 2010 CONTINUE
 2020 CONTINUE
C-----PROCESS EACH OUTBOUND APPROACH
      DO 2040  IAN = 1 , NOBA
      IA = LOBA(IAN)
C-----PROCESS EACH LANE OF THE OUTBOUND APPROACH
      DO 2030  ILN = 1 , NLANES(IA)
      IL = LLANES(ILN,IA)
      IF ( LGEOM(1,IL) . EQ . LGEOM(2,IL) )      THEN
        POSPTH = LGEOM(3,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
        POSPTH = LGEOM(4,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
      ELSE IF ( LGEOM(3,IL) .EQ. LGEOM(4,IL) )   THEN
        POSPTH = LGEOM(1,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
        POSPTH = LGEOM(2,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
      ELSE
        POSPTH = LGEOM(1,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
        POSPTH = LGEOM(4,IL)
        CALL FNDXYA ( IA,ILN,POSPTH,0.0D0,XPOS,YPOS )
        XMAXIN = AMAX1( XMAXIN,XPOS )
        XMININ = AMIN1( XMININ,XPOS )
        YMAXIN = AMAX1( YMAXIN,YPOS )
        YMININ = AMIN1( YMININ,YPOS )
      END IF
 2030 CONTINUE
 2040 CONTINUE
      DO 2070  I = 1 , NARCS
      IARC = LARCS(I)
      ADDAZ = SIGN( AMIN1( FLOAT( IABS( IARCSW(IARC) ) )/10.0,5.0 ),
     *              FLOAT( IARCSW(IARC) )                            )
      ADD = -ADDAZ
 2050 CONTINUE
      IF ( ( ABS( ADD ) . GE . FLOAT( IABS( IARCSW(IARC) ) ) ) . OR .
     *     ( ABS( ADD-FLOAT( IARCSW(IARC) ) ) . LE . ZERO    ) )
     *                                           GO TO 2070
 2060 CONTINUE
      ADD = ADD + ADDAZ
      IF ( ( ABS( ADD ) . GE . FLOAT( IABS( IARCSW(IARC) ) ) ) . OR .
     *     ( ABS( ADD-FLOAT( IARCSW(IARC) ) ) . LE . ZERO    ) )
     *                                           THEN
        ADD = DBLE( IARCSW(IARC) )
      END IF
      DEG = 90 - (IARCAZ(IARC)+ADD)
      XPOS = IARCX(IARC) + IARCR(IARC)*COS( FLOAT( DEG )*DEG2RD )
      YPOS = IARCY(IARC) + IARCR(IARC)*SIN( FLOAT( DEG )*DEG2RD )
      XMAXIN = AMAX1( XMAXIN,XPOS )
      XMININ = AMIN1( XMININ,XPOS )
      YMAXIN = AMAX1( YMAXIN,YPOS )
      YMININ = AMIN1( YMININ,YPOS )
      IF ( ( ABS( ADD ) . GE . FLOAT( IABS( IARCSW(IARC) ) ) ) . OR .
     *     ( ABS( ADD-FLOAT( IARCSW(IARC) ) ) . LE . ZERO    ) )
     *                                           GO TO 2070
      GO TO 2060
 2070 CONTINUE
      DO 2080  I = 1 , NLINES
      ILINE = LLINES(I)
      XMAXIN = AMAX1( XMAXIN,FLOAT( ILX1(ILINE) ) )
      XMININ = AMIN1( XMININ,FLOAT( ILX1(ILINE) ) )
      YMAXIN = AMAX1( YMAXIN,FLOAT( ILY1(ILINE) ) )
      YMININ = AMIN1( YMININ,FLOAT( ILY1(ILINE) ) )
      XMAXIN = AMAX1( XMAXIN,FLOAT( ILX2(ILINE) ) )
      XMININ = AMIN1( XMININ,FLOAT( ILX2(ILINE) ) )
      YMAXIN = AMAX1( YMAXIN,FLOAT( ILY2(ILINE) ) )
      YMININ = AMIN1( YMININ,FLOAT( ILY2(ILINE) ) )
 2080 CONTINUE
      DO 2090  I = 1 , NSDRC
      ISDRC = LSDRC(I)
      XMAXIN = AMAX1( XMAXIN,FLOAT( IXSDRC(ISDRC) ) )
      XMININ = AMIN1( XMININ,FLOAT( IXSDRC(ISDRC) ) )
      YMAXIN = AMAX1( YMAXIN,FLOAT( IYSDRC(ISDRC) ) )
      YMININ = AMIN1( YMININ,FLOAT( IYSDRC(ISDRC) ) )
 2090 CONTINUE
C-----WRITE SURROGATE SAFETY ASSESSMENT METHODOLOGY DATA FOR INTERSECTION
      IF ( ISSAM . EQ . IYES )                   THEN
C-----  WRITE SSAM DIMENSION RECORD
C-----  SSAMID = RECORD TYPE (1=DIMENSION)
C-----  SSAMSC = SCALE (DISTANCE PER UNIT)
C-----  SSAMUN = UNITS (0=ENGLISH)
C-----  SSAMIX = MIN X COORDINATE
C-----  SSAMIY = MIN Y COORDINATE
C-----  SSAMAX = MAX X COORDINATE
C-----  SSAMAY = MAX Y COORDINATE
        SSAMID = 1
        SSAMUN = 0
        SSAMSC = 1.0
        SSAMIX = INT( XMININ-0.9999 )
        SSAMIY = INT( YMININ-0.9999 )
        SSAMAX = INT( XMAXIN+0.9999 )
        SSAMAY = INT( YMAXIN+0.9999 )
        WRITE (ISS) SSAMID,SSAMUN,SSAMSC,SSAMIX,SSAMIY,SSAMAX,SSAMAY
      END IF
                    IF ( IPOLL . EQ . INO )      GO TO 3015
C-----WRITE POL/ANI FILE DATA
      WRITE (NPD,501) GTITLE//IMAGOP
      WRITE (NPD,501) DTITLE
      WRITE (NPD,501) STITLE
      IF ( IMAGOP . EQ . 'YES' )                 THEN
        WRITE (NPD,506) IMAGL1
        WRITE (NPD,506) IMAGL2
      END IF
      WRITE (NPD,503) STRTIM,SIMTIM,DT,ICONTR,IPOLL,IVERSN,ENDT20,
     *                DIAMON
      LENMAX = 0
      WIDMAX = 0
      DO 3010  I = 1 , NVEHCL
      LENMAX = MAX( LENMAX,LENV(I) )
      WIDMAX = MAX( WIDMAX,WIDV(I) )
 3010 CONTINUE
      WRITE (NPD,504) INT(XMININ-0.9999),INT(XMAXIN+0.9999),
     *                INT(YMININ-0.9999),INT(YMAXIN+0.9999),
     *                LENMAX,WIDMAX
      WRITE (NPD,504) NAPS,NIBA,NOBA,(LIBA(I),I=1,NIBA),
     *                (LOBA(I),I=1,NOBA),NLEGS,NFUT,NPHASE
 3015 CONTINUE
      DO 3020  I = 1 , NIBA
      JA = LIBA(I)
      CALL  INITAP  ( JA,IAAZIM(JA),IAPX(JA),IAPY(JA) )
                    IF ( IPOLL . EQ . INO )      GO TO 3020
      WRITE (NPD,504) JA,IAAZIM(JA),IAPX(JA),IAPY(JA),NLANES(JA),
     *                (LLANES(K,JA),K=1,NLANES(JA))
 3020 CONTINUE
      DO 3030  I = 1 , NOBA
      JA = LOBA(I)
      CALL  INITAP  ( JA,IAAZIM(JA),IAPX(JA),IAPY(JA) )
                    IF ( IPOLL . EQ . INO )      GO TO 3030
      WRITE (NPD,504) JA,IAAZIM(JA),IAPX(JA),IAPY(JA),NLANES(JA),
     *                (LLANES(K,JA),K=1,NLANES(JA))
 3030 CONTINUE
                    IF ( IPOLL . EQ . INO )      GO TO 3035
      WRITE (NPD,504) NRLAN
 3035 CONTINUE
      K = 0
      DO 3040  I = 1 , NRLAN
      OUTBND = ( IBLN(I) . EQ . 0 )
      CALL  INITLN  ( I,ISNA(I),K,LGEOM(1,I),LGEOM(4,I),LWID(I),OUTBND,
     *                DIAMON                                           )
                    IF ( IPOLL . EQ . INO )      GO TO 3040
      WRITE (NPD,504) I,ISNA(I),(LGEOM(LL,I),LL=1,4),LWID(I),LCONTR(I)
 3040 CONTINUE
                    IF ( IPOLL . EQ . INO )      GO TO 3045
      WRITE (NPD,504) NPATHS
 3045 CONTINUE
      DO 3050  I = 1 , NPATHS
      PTH1X1(I) = IXL1(I)
      PTH1Y1(I) = IYL1(I)
      PTH1X2(I) = JXL1(I)
      PTH1Y2(I) = JYL1(I)
      PTH2XC(I) = IXA1(I)
      PTH2YC(I) = IYA1(I)
      PTH2R (I) = IRA1(I)
      PTH2A1(I) = IBA1(I)
      PTH2A2(I) = IDA1(I)
      PTH3XC(I) = IXA2(I)
      PTH3YC(I) = IYA2(I)
      PTH3R (I) = IRA2(I)
      PTH3A1(I) = IBA2(I)
      PTH3A2(I) = IDA2(I)
      PTH4X1(I) = IXL2(I)
      PTH4Y1(I) = IYL2(I)
      PTH4X2(I) = JXL2(I)
      PTH4Y2(I) = JYL2(I)
      K         = LENP(I)
      PATHFR(I) = LIBL(I)
      PATHTO(I) = LOBL(I)
                    IF ( IPOLL . EQ . INO )      GO TO 3050
      WRITE (NPD,505) I,
     *                IXL1(I),IYL1(I),JXL1(I),JYL1(I),
     *                IXA1(I),IYA1(I),IRA1(I),IBA1(I),IDA1(I),
     *                IXA2(I),IYA2(I),IRA2(I),IBA2(I),IDA2(I),
     *                IXL2(I),IYL2(I),JXL2(I),JYL2(I),
     *                LENP(I),LIBL(I),LOBL(I)
 3050 CONTINUE
      CALL  INITPA  ( NPATHS )
                    IF ( IPOLL . EQ . INO )      GO TO 3140
      WRITE (NPD,504) NVEHCL,NVEHAT
      DO 3060  I = 1 , NVEHCL
      WRITE (NPD,507) LENV(I),WIDV(I),HEIGHT(I),CLASSV(I),NUNITS(I),
     *                (ULEN  (J,I),UWID  (J,I),UDRWSQ(J,I),
     *                 UFPD  (J,I),URPD  (J,I),URHPD (J,I),
     *                 UTRLEN(J,I),UTRWID(J,I),J=1,NUNITS(I))
 3060 CONTINUE
      WRITE (NPD,504) NARCS
                    IF ( NARCS . EQ . 0 )        GO TO 3080
      DO 3070  I = 1 , NARCS
      IARC = LARCS(I)
      WRITE (NPD,504) IARC,IARCX (IARC),IARCY(IARC),IARCAZ(IARC),
     *                     IARCSW(IARC),IARCR(IARC)
 3070 CONTINUE
 3080 CONTINUE
      WRITE (NPD,504) NLINES
                    IF ( NLINES . EQ . 0 )       GO TO 3100
      DO 3090  I = 1 , NLINES
      ILINE = LLINES(I)
      WRITE (NPD,504) ILINE,ILX1(ILINE),ILY1(ILINE),
     *                      ILX2(ILINE),ILY2(ILINE)
 3090 CONTINUE
 3100 CONTINUE
      WRITE (NPD,504) NSDRC
                    IF ( NSDRC . LE . 0 )        GO TO 3120
      DO 3110  I = 1 , NSDRC
      ISDRC = LSDRC(I)
      WRITE (NPD,504) ISDRC,IXSDRC(ISDRC),IYSDRC(ISDRC)
 3110 CONTINUE
 3120 CONTINUE
      WRITE (NPD,504) NLOOPS
                    IF ( NLOOPS . LE . 0 )       GO TO 3140
      DO 3130  I = 1 , NLOOPS
      ILOOP = LLOOPS(I)
      IF      ( ITYPLD(ILOOP) . EQ . ICLAS )     THEN
        JTYPLD = 1
      ELSE IF ( ITYPLD(ILOOP) . EQ . IPULS )     THEN
        JTYPLD = 2
      ELSE IF ( ITYPLD(ILOOP) . EQ . IPRES )     THEN
        JTYPLD = 3
      ELSE
        JTYPLD = 0
      END IF
      WRITE (NPD,504) ILOOP,JTYPLD       ,LDSTRT(ILOOP),LDSTOP(ILOOP),
     *                      LDA   (ILOOP),NLDLN (ILOOP),
     *                      (LLDLN(K,ILOOP),K=1,NLDLN(ILOOP))
 3130 CONTINUE
 3140 CONTINUE
C-----IF THE INTERSECTION IS NOT NEMA OR HARDWARE CONTROLLED THEN GO TO
C-----3150 ELSE INITIALIZE ANY PEDESTRIAN DETECTORS
                    IF ( ICONTR . LT . ICNEMA )  GO TO 3150
      DO  I = 1 , NPHASE
        P = LPHASE(I)
        IF ( ( PEDS  (P)          ) . AND .
     *       ( PEDVOL(P) . GT . 0 ) )            THEN
          TIMVAL = 0.0D0
          IF      ( PEDDIS(P) .EQ. "CONSTAN" )   THEN
C-----      PROCESS CONSTANT RANDOM DEVIATES
            CALL  CONST   ( PEDTMN(P)                    ,TIMVAL )
          ELSE IF ( PEDDIS(P) .EQ. "ERLANG"  )   THEN
C-----      PROCESS ERLANG RANDOM DEVIATES
            CALL  ERLANG  ( PEDTMN(P),IDNINT( PEDPAR(P) ),TIMVAL )
          ELSE IF ( PEDDIS(P) .EQ. "GAMMA"   )   THEN
C-----      PROCESS GAMMA RANDOM DEVIATES
            CALL  GAMMA   ( PEDTMN(P),        PEDPAR(P)  ,TIMVAL )
          ELSE IF ( PEDDIS(P) .EQ. "LOGNRML" )   THEN
C-----      PROCESS LOG NORMAL RANDOM DEVIATES
            CALL  LGNRML  ( PEDTMN(P),        PEDPAR(P)  ,TIMVAL )
          ELSE IF ( PEDDIS(P) .EQ. "NEGEXP"  )   THEN
C-----      PROCESS NEGATIVE EXPONENTIAL RANDOM DEVIATES
            CALL  NEGEXP  ( PEDTMN(P)                    ,TIMVAL )
          ELSE IF ( PEDDIS(P) .EQ. "SNEGEXP" )   THEN
C----       PROCESS SHIFTED NEGATIVE EXPONENTIAL RANDOM DEVIATES
            CALL  SNEGEX  ( PEDTMN(P),        PEDPAR(P)  ,TIMVAL )
          ELSE IF ( PEDDIS(P) .EQ. "UNIFORM" )   THEN
C-----      PROCESS UNIFORM RANDOM DEVIATES
            CALL  UNIFRM  ( PEDTMN(P),        PEDPAR(P)  ,TIMVAL )
          END IF
          PEDTIM(P) = DMAX1( TIMVAL,0.0D0 )
        ELSE
          PEDTIM(P) = 0.0D0
        END IF
      END DO
 3150 CONTINUE
      RETURN
C-----PROCESS THE INPUT ERRORS AND STOP
 8010 CONTINUE
      WRITE (ERRMSG,801) 'GEOPRO',IGEOP
      CALL  PRTERR  ( 'STOP 801 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'INITAL'                             )
      CLOSE (UNIT=IGEOP,STATUS='KEEP')
      OPEN  (UNIT=IGEOP,FILE=T8FILE(1:NCT8),STATUS='OLD')
      CLOSE (UNIT=IGEOP,STATUS='DELETE')
      STOP  801
 8020 CONTINUE
      WRITE (ERRMSG,801) 'DVPRO' ,IVEHP
      CALL  PRTERR  ( 'STOP 802 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'INITAL'                             )
      CLOSE (UNIT=IVEHP,STATUS='KEEP')
      OPEN  (UNIT=IVEHP,FILE=T9FILE(1:NCT9),STATUS='OLD')
      CLOSE (UNIT=IVEHP,STATUS='DELETE')
      STOP  802
 8030 CONTINUE
      WRITE (ERRMSG,801) 'SIMPRO',INPUT
      CALL  PRTERR  ( 'STOP 803 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'INITAL'                             )
      CLOSE (UNIT=INPUT,STATUS='KEEP')
      OPEN  (UNIT=INPUT,FILE=IFILE(1:NCIF),STATUS='OLD')
      CLOSE (UNIT=INPUT,STATUS='DELETE')
      STOP  803
 8840 CONTINUE
      CALL  PRTERR  ( 'STOP 884 - '                  //
     *                'ERROR READING GDVS00 FILE - ' //
     *                'INITAL'                          )
      STOP  884
 8930 CONTINUE
      WRITE (ERRMSG,893)
      CALL  PRTERR  ( 'STOP 893 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'INITAL'                             )
      STOP  893
 8940 CONTINUE
      WRITE (ERRMSG,894) NUMREP,MIN0( 99,NRP )
      CALL  PRTERR  ( 'STOP 894 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'INITAL'                             )
      STOP  894
 9340 CONTINUE
      CALL  PRTERR  ( 'STOP 934 - ' //
     *                'ERROR READING SVERSN FROM IVERSN - ' //
     *                'INITAL'                                 )
      STOP  934
 9650 CONTINUE
      CALL  PRTERR  ( 'STOP 965 - NIL NE 25 - INITAL' )
      STOP  965
 9660 CONTINUE
      CALL  PRTERR  ( 'STOP 966 - NPL GT 37 - INITAL' )
      STOP  966
 9670 CONTINUE
      CALL  PRTERR  ( 'STOP 967 - TIMERR LT 10000 - INITAL' )
      STOP  967
      END                                                               INITAL
C
C
C
      SUBROUTINE OPNSIF ( IPU,IUNIT )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
      INCLUDE 'USER'
      CHARACTER*60      DEFFN
      LOGICAL           ERR
      INTEGER           ILNB,IPU,IUNIT
C
C-----OPEN SIMULATION INPUT FILE
C
C     IPU   - UNIT NUMBER OF FILE WITH POINTER TO GEOPRO/DVPRO OUTPUT
C             USE   NGP   FOR GEOPRO DATA
C                   NDV   FOR DVPRO  DATA
C                   NSP   FOR SIMPRO DATA
C     IUNIT - INIT NUMBER FOR OPENING GEOPRO/DVPRO OUTPUT FILE
C             USE <IGEOP> FOR GEOPRO DATA
C                 <IVEHP> FOR DVPRO  DATA
C                 <INPUT> FOR SIMPRO DATA
C
      IF ( IUNIT . EQ . INPUT )                  THEN
        IF ( NCIF . GT . 0 )                     THEN
          NLINE(20:) = IFILE(1:NCIF)
          GO TO 100
        END IF
        GO TO 50
      END IF
      IF ( IUNIT . EQ . IGEOP )                  THEN
        IF ( NCT8 . GT . 0 )                     THEN
          NLINE(20:) = T8FILE(1:NCT8)
          GO TO 100
        END IF
        GO TO 50
      END IF
      IF ( IUNIT . EQ . IVEHP )                  THEN
        IF ( NCT9 . GT . 0 )                     THEN
          NLINE(20:) = T9FILE(1:NCT9)
          GO TO 100
        END IF
      END IF
   50 CONTINUE
      NLINE = DEFFN( IPU )
      CALL  OPENRO  ( IPU,NLINE,'UNKNOWN','DIRECT','FORMATTED',80,ERR )
                    IF ( ERR )                   GO TO 200
      READ (IPU,FMT,REC=1,ERR=200) NLINE
      IF ( (IUNIT.EQ.INPUT)               . AND .
     *     (NLINE(1:14).NE.' #%&$FILESIMC1') )   GO TO 200
      IF ( NLINE(1:9) . NE . ' #%&$FILE' )       GO TO 200
  100 CONTINUE
      CALL  OPENRO  ( IUNIT,NLINE(20:),'OLD','SEQUENTIAL','FORMATTED',0,
     *                ERR )
                    IF ( ERR )                   GO TO 200
      GO TO 300
  200 CONTINUE
C-----TRY TO USE DEFAULT FILE NAMES
      IF ( IUNIT .EQ. INPUT )                    THEN
        NLINE = 'sim'
      ELSE
        NLINE = DEFFN ( IUNIT )
      END IF
      CALL  OPENRO  ( IUNIT,NLINE,'UNKNOWN','SEQUENTIAL','FORMATTED',0,
     *                ERR )
  300 CONTINUE
      IF ( IUNIT . EQ . INPUT )                  THEN
        CALL  TXINQ  ( IUNIT,' ',' ',IFILE,ERR )
        NCIF = ILNB ( IFILE )
      END IF
      IF ( IUNIT . EQ . IGEOP )                  THEN
        CALL  TXINQ  ( IUNIT,' ',' ',T8FILE,ERR )
        NCT8 = ILNB ( T8FILE )
      END IF
      IF( IUNIT . EQ . IVEHP )                   THEN
        CALL  TXINQ  ( IUNIT,' ',' ',T9FILE,ERR )
        NCT9 = ILNB ( T9FILE )
      END IF
      REWIND (IUNIT)
      RETURN
      END                                                               OPNSIF
C
C
C
      SUBROUTINE RUSERD
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'PHASES'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SUMST2'
      INCLUDE 'TITLE'
      INCLUDE 'TXDSIG'
      INCLUDE 'USER'
      BYTE              SSAMEN,SSAMID
      CHARACTER*1       IPOUT
      CHARACTER*3       ISIMGO,MCSTOP
      CHARACTER*29      ICNTRL(19)
      LOGICAL           ERR
      INTEGER           IABTMN,IABTSD,ILNB,IPDSCL,ISTATS,JCONTR,MCNTRL
      REAL*4            SSAMVN
      DOUBLE PRECISION  DTMAXV,DTMINV,XMPH,XTMAX
      DATA     ICNTRL / '(UNCONTROLLED)               ',                 1
     *                  '(YIELD SIGN)                 ',                 2
     *                  '(LESS-THAN-ALL-WAY STOP SIGN)',                 3
     *                  '(ALL-WAY STOP SIGN)          ',                 4
     *                  '(PRE-TIMED SIGNAL)           ',                 5
     *                  '(SEMI-ACTUATED SIGNAL)       ',                 6
     *                  '(FULL-ACTUATED SIGNAL)       ',                 7
     *                  '(TEXAS DIAMOND FIG 3 SIGNAL) ',                 8
     *                  '(TEXAS DIAMOND FIG 4 SIGNAL) ',                 9
     *                  '(TEXAS DIAMOND FIG 6 SIGNAL) ',                10
     *                  '(TEXAS DIAMOND FIG 7 SIGNAL) ',                11
     *                  '(DALLAS DIAMOND FIG 3 SIGNAL)',                12
     *                  '(DALLAS DIAMOND FIG 4 SIGNAL)',                13
     *                  '(DALLAS DIAMOND FIG 6 SIGNAL)',                14
     *                  '(DALLAS DIAMOND FIG 7 SIGNAL)',                15
     *                  '(NEMA SIGNAL)                ',                16
     *                  '(NEMA VOLUME DENSITY SIGNAL) ',                17
     *                  '(HARDWARE-IN-THE-LOOP SIGNAL)',                18
     *                  'UNKNOWN INTERSECTION CONTROL '/                19
  501 FORMAT(F4.1,F6.1,F5.2,2F3.0,2F6.3,F5.0,I2,2(A1,A3),2(1X,A3),2F5.2,
     *       F4.0,I3,F6.1,A1,2F4.1,I4,A3,I3,2I4,2A3,2F3.1,I3)
  601 FORMAT(/,
     *54H START-UP TIME (MINUTES) --------------------------- =,F9.1/,
     *54H SIMULATION TIME (MINUTES) ------------------------- =,F9.1/,
     *54H TIME INCREMENT FOR SIMULATION (SECONDS) ----------- =,F10.2//,
     *54H SPECIAL STATISTIC SPEED (MPH) --------------------- =,F10.2//,
     *54H MAXIMUM CLEAR DISTANCE TO BE IN QUEUE (FT) -------- =,F10.2//,
     *54H CAR FOLLOWING EQUATION - LAMBDA ------------------- =,F11.3/,
     *54H CAR FOLLOWING EQUATION - MU ----------------------- =,F11.3/,
     *54H CAR FOLLOWING EQUATION - ALPHA -------------------- =,F11.3//,
     *54H SUMMARY STATISTICS PRINT WIDTH (W=132 N=80) ------- =,6X,A1,/,
     *54H SUMMARY STATISTICS PRINTED BY TURNING MOVEMENTS --- =,4X,A3/,
     *54H SUMMARY STATISTICS PRINTED BY INBOUND APPROACH ---- =,4X,A3//,
     *54H SUMMARY STATISTICS SPREADSHEET FILE --------------- =,4X,A3//,
     *54H ANIMATION/POLLUTION DISPERSION MODEL FILE --------- =,4X,A3//,
     *54H SURROGATE SAFETY ASSESSMENT METHODOLOGY FILE ------ =,4X,A3)
  602 FORMAT(
     *54H ANIMATION/POLLUTION DISPERSION MODEL BEG TIME (MIN) =,I7,/,
     *54H ANIMATION/POLLUTION DISPERSION MODEL END TIME (MIN) =,I7)
  603 FORMAT(/,
     *54H ALLOW LEFT TURNERS TO PULL OUT INTO INTERSECTION -- =,6X,A1//,
     *54H LEAD TIME GAP FOR CONFLICT CHECKING (SECONDS) ----- =,F10.2/,
     *54H LAG TIME GAP FOR CONFLICT CHECKING (SECONDS) ------ =,F10.2//,
     *54H INTERSECTION TRAFFIC CONTROL TYPE ----------------- =,I7,
     *                                                            2X,A/,
     *54H NUMBER OF VEHICLE MESSAGE SYSTEM MESSAGES --------- =,I7/,
     *54H STOP VEHICLES INVOLVED IN A MAJOR COLLISION ------- =,4X,A3)
  604 FORMAT(
     *54H PERCENT OF DESIRED SPEED FOR PASSING A MAJOR COLL - =,I7,/,
     *54H TAKE EVASIVE ACTION BLOCKED BY COLLISION MEAN (SECS)=,I7,/,
     *54H TAKE EVASIVE ACTION BLOCKED BY COLLISION S.D. (SECS)=,I7)
  701 FORMAT(/,
     *54H TIME INTERVAL FOR INTERMEDIATE STATISTICS (SECONDS) =,I7)
  702 FORMAT(/,
     *54H TIME INTO SIMULATION FOR DEBUG PRINTING (SECONDS) - =,F10.2)
  703 FORMAT(/,
     *51H ** SIMULATION INPUT PARAMETER CHECKING DISABLED **)
  804 FORMAT(
     *16H START-UP TIME =,F7.1,21H IS LT 2.0 OR GT 15.0)
  805 FORMAT(
     *18H SIMULATION TIME =,F7.1,23H IS LT 10.0 OR GT 164.0)
  806 FORMAT(
     *32H TIME INCREMENT FOR SIMULATION =,F7.2,7H IS LT ,F4.2,
     *7H OR GT ,F4.2)
  807 FORMAT(
     *26H SPECIAL STATISTIC SPEED =,F7.2,21H IS LT 0.0 OR GT 40.0)
  808 FORMAT(
     *40H MAXIMUM CLEAR DISTANCE TO BE IN QUEUE =,F7.2,
     *21H IS LT 4.0 OR GT 40.0)
  809 FORMAT(
     *34H CAR FOLLOWING EQUATION - LAMBDA =,F9.5,
     *20H IS LT 0.0 OR GT 4.0)
  810 FORMAT(
     *30H CAR FOLLOWING EQUATION - MU =,F9.5,
     *20H IS LT 0.0 OR GT 4.0)
  811 FORMAT(
     *33H CAR FOLLOWING EQUATION - ALPHA =,F9.5,
     *23H IS LT 0.0 OR GT 9999.9)
  812 FORMAT(
     *36H INTERSECTION TRAFFIC CONTROL TYPE =,I3,14H IS LT 1 OR GT,I3)
  891 FORMAT(
     *48H SUMMARY STATISTICS PRINT WIDTH (W=132 N=80) = (,A1,
     *19H) IS NOT (W) OR (N))
  813 FORMAT(
     *52H SUMMARY STATISTICS PRINTED BY TURNING MOVEMENTS = (,A3,
     *23H) IS NOT (YES) OR (NO ))
  892 FORMAT(
     *53H ALLOW LEFT TURNERS TO PULL OUT INTO INTERSECTION = (,A1,
     *19H) IS NOT (Y) OR (N))
  814 FORMAT(
     *51H SUMMARY STATISTICS PRINTED BY INBOUND APPROACH = (,A3,
     *23H) IS NOT (YES) OR (NO ))
  815 FORMAT(
     *38H LEAD TIME GAP FOR CONFLICT CHECKING =,F6.2,
     *20H IS LT 0.5 OR GT 3.0)
  816 FORMAT(
     *37H LAG TIME GAP FOR CONFLICT CHECKING =,F6.2,
     *20H IS LT 0.5 OR GT 3.0)
  817 FORMAT(
     *40H SUMMARY STATISTICS SPREADSHEET FILE = (,A3,
     *23H) IS NOT (YES) OR (NO ))
  818 FORMAT(
     *46H ANIMATION/POLLUTION DISPERSION MODEL FILE = (,A3,
     *41H) IS NOT (YES) OR (NO ) OR (ANI) OR (POL))
  770 FORMAT(
     *45H NUMBER OF VEHICLE MESSAGE SYSTEM MESSAGES = ,I4,
     *15H IS LT 0 OR GT ,I4)
  739 FORMAT(
     *48H STOP VEHICLES INVOLVED IN A MAJOR COLLISION = (,A3,
     *23H) IS NOT (YES) OR (NO ))
  738 FORMAT(
     *52H PERCENT OF DESIRED SPEED FOR PASSING A COLLISION = ,I3,
     *18H IS LE 0 OR GT 100)
  737 FORMAT(
     *47H ABORT DESTINATION BLOCKED BY COLLISION MEAN = ,I4,
     *19H IS LT 0 OR GT 9960)
  736 FORMAT(
     *47H ABORT DESTINATION BLOCKED BY COLLISION S.D. = ,I4,
     *19H IS LT 0 OR GT 9960)
  720 FORMAT(
     *49H SURROGATE SAFETY ASSESSMENT METHODOLOGY FILE = (,A3,
     *23H) IS NOT (YES) OR (NO ))
  717 FORMAT(
     *41H ENABLE SIMULTANEOUS GAP OUT FOR NEMA = (,A3,
     *23H) IS NOT (YES) OR (NO ))
  716 FORMAT(
     *27H DILEMMA ZONE BEGIN TIME = ,F3.1,
     *20H IS LT 1.0 OR GT 9.9)
  715 FORMAT(
     *25H DILEMMA ZONE END TIME = ,F3.1,
     *20H IS LT 1.0 OR GT 9.9)
  714 FORMAT(
     *27H DILEMMA ZONE BEGIN TIME = ,F3.1,
     *31H IS LE DILEMMA ZONE END TIME = ,F3.1)
  713 FORMAT(
     *42H HARDWARE-IN-THE-LOOP SIGNAL SLEEP TIME = ,I3,
     *18H IS LT 0 OR GT 300)
C
C-----SUBROUTINE RUSERD READS THE USER DATA FROM CARD 2 OF THE INPUT
C-----DIRECTLY TO THE SIMULATION PROCESSOR AND CHECKS FOR ERRORS
C
C[    IPOUT      = '~'
C[    MCSTOP     = '~'
C[    ISTATS     = -2147483647
C[    JCONTR     = -2147483647
C[    XMPH       = -2147483647.0
C[    XTMAX      = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'RUSERD'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----READ THE USER DATA FROM CARD 2 OF THE INPUT DIRECTLY TO THE
C-----SIMULATION PROCESSOR
      READ (INPUT,501) STRTIM,SIMTIM,DT,XMPH,XQDIST,CAREQL,CAREQM,
     *                 CAREQA,ICONTR,IWNOUT,IPTC,IPOUT,IPAP,IPUNCH,
     *                 IPOLL,TLEAD,TLAG,ENDT20,ISTATS,TPRINT,IXXX,
     *                 HESFAC,XTMAX,NVMSM,MCSTOP,IPDSCL,IABTMN,IABTSD,
     *                 ISSAM,ISIMGO,DZBTIM,DZETIM,HITLST
      BEGT20 = 0.0D0
C-----SET THE DEFAULTS FOR THE USER DATA
                    IF ( IWNOUT . EQ . IBLNK1 )  IWNOUT = ILETTW
                    IF ( IPTC   . EQ . IBLNK1 )  IPTC   = IYES
C[    IF ( IPOUT              .EQ.'~'           )STOP 'RUSERD IPOUT  01'
                    IF ( IPOUT  . EQ . IBLNK1 )  IPOUT  = INO(1:1)
                    IF ( IPAP   . EQ . IBLNK1 )  IPAP   = IYES
                    IF ( IPTC   . EQ . IYES )    IPAP   = IYES
                    IF ( IPUNCH . EQ . IBLNK1 )  IPUNCH = IYES
                    IF ( REPRUN . NE . INO    )  IPUNCH = IYES
                    IF ( IPOLL  . EQ . IBLNK1 )  IPOLL  = INO
                    IF ( IPOLL  . EQ . IYES )    IPOLL  = IANI
      IF ( IPOLL . EQ . IANI )                   THEN
                    IF ( ENDT20 . EQ . 0.0D0 )   ENDT20 = 5.0D0
      END IF
      IF ( IPOLL . EQ . IPOL )                   THEN
                    IF ( ENDT20 . EQ . 0.0D0 )   ENDT20 = STRTIM+SIMTIM
      END IF
                    IF ( ENDT20 . LT . 0.0D0 )   IPOLL  = INO
                    IF ( HESFAC . EQ . 0.0D0 )   HESFAC = 2.00D0
C[    IF ( XTMAX              .EQ.-2147483647.0 )STOP 'RUSERD XTMAX  01'
                    IF ( XTMAX  . NE . 0.0D0 )   DTMAX = XTMAX
C[    IF ( MCSTOP             .EQ.'~'           )STOP 'RUSERD MCSTOP 04'
                    IF ( MCSTOP . EQ . IBLNK1 )  MCSTOP = INO
                    IF ( REPRUN . NE . INO    )  MCSTOP = INO
                    IF ( IPDSCL . LE . 0 )       IPDSCL = 25
                    IF ( IABTMN . LE . 0 )       IABTMN = 300
                    IF ( IABTSD . LE . 0 )       IABTSD = 120
                    IF ( ISSAM  . EQ . '   ' )   ISSAM  = INO
                    IF ( ISIMGO . EQ . '   ' )   ISIMGO = INO
                    IF ( DZBTIM . EQ . 0.0D0 )   DZBTIM = 5.5D0
                    IF ( DZETIM . EQ . 0.0D0 )   DZETIM = 2.5D0
                    IF ( HITLST . EQ . 0     )   HITLST = 120
C-----ECHO-PRINT THE USER DATA
C[    IF ( XMPH               .EQ.-2147483647.0 )STOP 'RUSERD XMPH   01'
      WRITE (6,601) STRTIM,SIMTIM,DT,XMPH,XQDIST,CAREQL,CAREQM,CAREQA,
     *              IWNOUT,IPTC,IPAP,IPUNCH,IPOLL,ISSAM
      IF ( IPOLL . NE . INO )                    THEN
        WRITE (6,602) IDNINT( BEGT20 ),IDNINT( ENDT20 )
      END IF
C[    IF ( IPOUT              .EQ.'~'           )STOP 'RUSERD IPOUT  02'
      JCONTR = ICONTR
                    IF ( ICONTR . LT . ICUNCT )  JCONTR = 19
                    IF ( ICONTR . GT . ICHDWR )  JCONTR = 19
C[    IF ( MCSTOP             .EQ.'~'           )STOP 'RUSERD MCSTOP 04'
      IF ( MCSTOP . EQ . IYES )                  THEN
        STOPMC = .TRUE.
      ELSE
        STOPMC = .FALSE.
      END IF
      WRITE (6,603) IPOUT,TLEAD,TLAG,ICONTR,ICNTRL(JCONTR),NVMSM,MCSTOP
      IF ( STOPMC )                              THEN
        WRITE (6,604) IPDSCL,IABTMN,IABTSD
      END IF
C-----CHECK USER DATA FOR ERRORS
      MCNTRL = ICNEMV
      MCNTRL = ICHDWR                                                   CCODE=C,
      IF ( ICONTR . LT . ICHDWR )                THEN
        DTMINV = 0.01D0
        DTMAXV = 1.00D0
      ELSE
        DTMINV = HDTMIN
        DTMAXV = HDTMAX
      END IF
      DTMINV = DTMINV - 0.0001D0
      DTMAXV = DTMAXV + 0.0001D0
                    IF ( IXXX   . EQ . ILETTX   )GO TO 1010
                    IF ( STRTIM . LT .    2.0D0 )GO TO 8040
                    IF ( STRTIM . GT .   15.0D0 )GO TO 8040
                    IF ( SIMTIM . LT .   10.0D0 )GO TO 8050
                    IF ( SIMTIM . GT .  164.0D0 )GO TO 8050
                    IF ( DT     . LT . DTMINV   )GO TO 8060
                    IF ( DT     . GT . DTMAXV   )GO TO 8060
C[    IF ( XMPH               .EQ.-2147483647.0 )STOP 'RUSERD XMPH   02'
                    IF ( XMPH   . LT .    0.0D0 )GO TO 8070
                    IF ( XMPH   . GT .   40.0D0 )GO TO 8070
                    IF ( XQDIST . LT .    4.0D0 )GO TO 8080
                    IF ( XQDIST . GT .   40.0D0 )GO TO 8080
                    IF ( CAREQL . LT .    0.0D0 )GO TO 8090
                    IF ( CAREQL . GT .    4.0D0 )GO TO 8090
                    IF ( CAREQM . LT .    0.0D0 )GO TO 8100
                    IF ( CAREQM . GT .    4.0D0 )GO TO 8100
                    IF ( CAREQA . LT .    0.0D0 )GO TO 8110
                    IF ( CAREQA . GT . 9999.9D0 )GO TO 8110
                    IF ( ICONTR . LT . ICUNCT   )GO TO 8120
                    IF ( ICONTR . GT . MCNTRL   )GO TO 8120
                    IF ((IWNOUT . NE . ILETTW). AND .
     *                  (IWNOUT . NE . ILETTN)  )GO TO 8910
                    IF ((IPTC   . NE . IYES  ). AND .
     *                  (IPTC   . NE . INO   )  )GO TO 8130
C[    IF ( IPOUT              .EQ.'~'           )STOP 'RUSERD IPOUT  03'
                    IF ((IPOUT  . NE . IYES(1:1)). AND .
     *                  (IPOUT  . NE . INO (1:1)))
     *                                           GO TO 8920
                    IF ((IPAP   . NE . IYES). AND .
     *                  (IPAP   . NE . INO )    )GO TO 8140
                    IF ( TLEAD  . LT .    0.5D0 )GO TO 8150
                    IF ( TLEAD  . GT .    3.0D0 )GO TO 8150
                    IF ( TLAG   . LT .    0.5D0 )GO TO 8160
                    IF ( TLAG   . GT .    3.0D0 )GO TO 8160
                    IF ((IPUNCH . NE . IYES). AND .
     *                  (IPUNCH . NE . INO )    )GO TO 8170
                    IF ((IPOLL  . NE . IANI). AND .
     *                  (IPOLL  . NE . INO ). AND .
     *                  (IPOLL  . NE . IPOL)    )GO TO 8180
                    IF ( NVMSM . LT .    0      )GO TO 7700
                    IF ( NVMSM . GT .  NVMSMM   )GO TO 7700
C[    IF ( MCSTOP             .EQ.'~'           )STOP 'RUSERD MCSTOP 04'
                    IF ((MCSTOP . NE . IYES). AND .
     *                  (MCSTOP . NE . INO )    )GO TO 7390
C[    IF ( IPDSCL             .EQ.'~'           )STOP 'RUSERD IPDSCL 04'
                    IF ( IPDSCL . LE .    0     )GO TO 7380
                    IF ( IPDSCL . GT .  100     )GO TO 7380
                    IF ( IABTMN . LT .    0     )GO TO 7370
                    IF ( IABTMN . GT . 9960     )GO TO 7370
                    IF ( IABTSD . LT .    0     )GO TO 7360
                    IF ( IABTSD . GT . 9960     )GO TO 7360
                    IF ((ISSAM  . NE . IYES). AND .
     *                  (ISSAM  . NE . INO )    )GO TO 7200
                    IF ((ISIMGO . NE . IYES). AND .
     *                  (ISIMGO . NE . INO )    )GO TO 7170
                    IF ( DZBTIM . LT .    1.0D0 )GO TO 7160
                    IF ( DZBTIM . GT .    9.9D0 )GO TO 7160
                    IF ( DZETIM . LT .    1.0D0 )GO TO 7150
                    IF ( DZETIM . GT .    9.9D0 )GO TO 7150
                    IF ( DZBTIM . LE . DZETIM   )GO TO 7140
                    IF ( HITLST . LT .    0     )GO TO 7130
                    IF ( HITLST . GT .  300     )GO TO 7130
 1010 CONTINUE
C-----SET PARAMATERS FOR INTERSECTION CONTROL
      ESIMGO   = .FALSE.
      FIG3     = .FALSE.
      FIG4     = .FALSE.
      FIG6     = .FALSE.
      FIG7     = .FALSE.
      NGROUP   = 0
      NOLP     = 0
      NPHPR(1) = 0
      NPHPR(2) = 0
      NRING    = 0
      GO TO ( 2010,2020,2030,2040,2050,2060,2070,2080,2090,2100,
     *        2110,2120,2130,2140,2150,2160,2170,2180 ) ,
     *                                           ICONTR
      GO TO 8120
 2010 CONTINUE
C-----ICUNCT   1 INTERSECTION CONTROL - UNCONTROLLED
      GO TO 3010
 2020 CONTINUE
C-----ICYELD   2 INTERSECTION CONTROL - YIELD SIGN
      GO TO 3010
 2030 CONTINUE
C-----ICLTAS   3 INTERSECTION CONTROL - LESS-THAN-ALL-WAY STOP SIGN
      GO TO 3010
 2040 CONTINUE
C-----ICAWST   4 INTERSECTION CONTROL - ALL-WAY STOP SIGN
      GO TO 3010
 2050 CONTINUE
C-----ICPSIG   5 INTERSECTION CONTROL - PRE-TIMED SIGNAL
      NRING = 1
      GO TO 3010
 2060 CONTINUE
C-----ICSACT   6 INTERSECTION CONTROL - SEMI-ACTUATED SIGNAL
      NRING = 1
      GO TO 3010
 2070 CONTINUE
C-----ICFACT   7 INTERSECTION CONTROL - FULL-ACTUATED SIGNAL
      NRING = 1
      GO TO 3010
 2080 CONTINUE
C-----ICTDF3   8 INTERSECTION CONTROL - TEXAS  DIAMOND FIG 3 SIGNAL
      FIG3 = .TRUE.
      GO TO 3010
 2090 CONTINUE
C-----ICTDF4   9 INTERSECTION CONTROL - TEXAS  DIAMOND FIG 4 SIGNAL
      FIG4 = .TRUE.
      GO TO 3010
 2100 CONTINUE
C-----ICTDF6  10 INTERSECTION CONTROL - TEXAS  DIAMOND FIG 6 SIGNAL
      FIG6 = .TRUE.
      GO TO 3010
 2110 CONTINUE
C-----ICTDF7  11 INTERSECTION CONTROL - TEXAS  DIAMOND FIG 7 SIGNAL
      FIG7 = .TRUE.
      GO TO 3010
 2120 CONTINUE
C-----ICDDF3  12 INTERSECTION CONTROL - DALLAS DIAMOND FIG 3 SIGNAL
      FIG3 = .TRUE.
      GO TO 2990
 2130 CONTINUE
C-----ICDDF4  13 INTERSECTION CONTROL - DALLAS DIAMOND FIG 4 SIGNAL
      FIG4 = .TRUE.
      GO TO 2990
 2140 CONTINUE
C-----ICDDF6  14 INTERSECTION CONTROL - DALLAS DIAMOND FIG 6 SIGNAL
      FIG6 = .TRUE.
      GO TO 2990
 2150 CONTINUE
C-----ICDDF7  15 INTERSECTION CONTROL - DALLAS DIAMOND FIG 7 SIGNAL
      FIG7 = .TRUE.
      GO TO 2990
 2160 CONTINUE
C-----ICNEMA  16 INTERSECTION CONTROL - NEMA SIGNAL
      ESIMGO   = ( ISIMGO . EQ . IYES )
      GO TO 3010
 2170 CONTINUE
C-----ICNEMV  17 INTERSECTION CONTROL - NEMA VOLUME DENSITY SIGNAL
      ESIMGO   = ( ISIMGO . EQ . IYES )
      GO TO 3010
 2180 CONTINUE
C-----ICHDWR  18 INTERSECTION CONTROL - HARDWARE-IN-THE-LOOP SIGNAL
      GO TO 3010
 2990 CONTINUE
      PTX2DL(1) = 2
      PTX2DL(2) = 4
      PTX2DL(3) = 1
      PTX2DL(4) = 0
      PTX2DL(5) = 5
      PTX2DL(6) = 6
      PTX2DL(7) = 8
      PTX2DL(8) = 0
      PDL2TX(1) = 3
      PDL2TX(2) = 1
      PDL2TX(3) = 0
      PDL2TX(4) = 2
      PDL2TX(5) = 5
      PDL2TX(6) = 6
      PDL2TX(7) = 0
      PDL2TX(8) = 7
      GO TO 3010
 3010 CONTINUE
      IF ( FIG3 .OR. FIG4 .OR. FIG6 .OR. FIG7 )  THEN
        LOLDF(1,1) = 1
        LOLDF(2,1) = 3
        NOLDF(1)   = 2
        LOLDF(1,2) = 5
        LOLDF(2,2) = 6
        NOLDF(2)   = 2
        NPHPR(1)   = 3
        NPHPR(2)   = 3
        NRING      = 2
      END IF
C-----CALCULATE SEVERAL SIMULATION PARAMETERS FROM THE USER DATA
      STRTIM = STRTIM*60.0D0
      SIMTIM = SIMTIM*60.0D0 + STRTIM
      BEGT20 = BEGT20*60.0D0
      ENDT20 = ENDT20*60.0D0
C[    IF ( IPOUT              .EQ.'~'           )STOP 'RUSERD IPOUT  04'
      LTPOUT = ( IPOUT . EQ . IYES(1:1) )
C[    IF ( ISTATS             .EQ.-2147483647   )STOP 'RUSERD ISTATS 01'
                    IF ( ISTATS . LE . 0 )       GO TO 3020
      WRITE (6,701) ISTATS
      GO TO 3030
 3020 CONTINUE
      ISTATS = 9999
 3030 CONTINUE
                    IF ( TPRINT . LE . 0.0D0 )   GO TO 3040
      WRITE (6,702) TPRINT
      GO TO 3050
 3040 CONTINUE
      TPRINT = 99999.99D0
 3050 CONTINUE
                    IF ( IXXX . EQ . ILETTX )    WRITE (6,703)
      DTSQ = DT*DT
      DTCU = DTSQ*DT
C[    IF ( XMPH               .EQ.-2147483647.0 )STOP 'RUSERD XMPH   03'
      XFPS = XMPH*MPH2FS
C[    IF ( ISTATS             .EQ.-2147483647   )STOP 'RUSERD ISTATS 02'
      TSTATS = ISTATS
                    IF ( IPUNCH . NE . IYES )    GO TO 4010
      IF ( NCSTA . GT . 0 )                      THEN
        CALL  OPENDD  ( ICS,STAFIL )
      END IF
      REWIND ICS
      CALL  TXINQ   ( ICS,' ',' ',STAFIL,ERR )
      NCSTA = ILNB( STAFIL )
 4010 CONTINUE
                    IF ( IPOLL . EQ . INO )      GO TO 4020
      IF ( NCPVA . GT . 0 )                      THEN
        CALL  OPENDD  ( NPD,PVAFIL )
      END IF
      REWIND NPD
      CALL  TXINQ   ( NPD,' ',' ',PVAFIL,ERR )
      NCPVA = ILNB( PVAFIL )
 4020 CONTINUE
C[    IF ( IPDSCL             .EQ.'~'           )STOP 'RUSERD IPDSCL 04'
      PDSCOL = DBLE( IPDSCL )/100.0D0
      ABTPMN = DBLE( IABTMN )
      ABTPSD = DBLE( IABTSD )
      IF ( ( ISSAM  . EQ . IYES ) . AND .
     *     ( NCSSAM . EQ . 0    ) )              THEN
        SSAM  ='ssam.trj'
        NCSSAM = ILNB( SSAM )
      END IF
C-----OPEN SURROGATE SAFETY ASSESSMENT METHODOLOGY DATA FILE FOR WRITING
      IF ( ISSAM . EQ . IYES )                   THEN
        OPEN ( ISS,FILE=SSAM,ACCESS='DIRECT',FORM='UNFORMATTED'
C|   1                                      ,STATUS='NEW'
C|   1                                      ,DEFAULTFILE='.trj'
     2                                                          )
        REWIND ISS
        CALL  TXINQ   ( ISS,' ',' ',SSAM,ERR )
        NCSSAM = ILNB( SSAM )
C-----  WRITE SSAM FORMAT RECORD
C-----  SSAMID = RECORD TYPE (0=FORMAT)
C-----  SSAMEN = ENDIAN ('L'=LITTLE ENDIAN; 'B'=BIG ENDIAN)
C-----  SSAMVN = VERSION
        SSAMID = 0
        SSAMEN = ICHAR( 'L' )
        SSAMVN = 1.04
        WRITE (ISS) SSAMID,SSAMEN,SSAMVN
      END IF
      RETURN
C-----PROCESS THE INPUT ERRORS AND STOP
 8040 CONTINUE
      WRITE (ERRMSG,804) STRTIM
      CALL  PRTERR  ( 'STOP 804 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  804
 8050 CONTINUE
      WRITE (ERRMSG,805) SIMTIM
      CALL  PRTERR  ( 'STOP 805 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  805
 8060 CONTINUE
      WRITE (ERRMSG,806) DT,DTMINV,DTMAXV
      CALL  PRTERR  ( 'STOP 806 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  806
 8070 CONTINUE
C[    IF ( XMPH               .EQ.-2147483647.0 )STOP 'RUSERD XMPH   04'
      WRITE (ERRMSG,807) XMPH
      CALL  PRTERR  ( 'STOP 807 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  807
 8080 CONTINUE
      WRITE (ERRMSG,808) XQDIST
      CALL  PRTERR  ( 'STOP 808 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  808
 8090 CONTINUE
      WRITE (ERRMSG,809) CAREQL
      CALL  PRTERR  ( 'STOP 809 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  809
 8100 CONTINUE
      WRITE (ERRMSG,810) CAREQM
      CALL  PRTERR  ( 'STOP 810 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  810
 8110 CONTINUE
      WRITE (ERRMSG,811) CAREQA
      CALL  PRTERR  ( 'STOP 811 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  811
 8120 CONTINUE
      WRITE (ERRMSG,812) ICONTR,MCNTRL
      CALL  PRTERR  ( 'STOP 812 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  812
 8910 CONTINUE
      WRITE (ERRMSG,891) IWNOUT
      CALL  PRTERR  ( 'STOP 891 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  891
 8130 CONTINUE
      WRITE (ERRMSG,813) IPTC
      CALL  PRTERR  ( 'STOP 813 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  813
 8920 CONTINUE
      WRITE (ERRMSG,892) IPOUT
      CALL  PRTERR  ( 'STOP 892 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  892
 8140 CONTINUE
      WRITE (ERRMSG,814) IPAP
      CALL  PRTERR  ( 'STOP 814 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  814
 8150 CONTINUE
      WRITE (ERRMSG,815) TLEAD
      CALL  PRTERR  ( 'STOP 815 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  815
 8160 CONTINUE
      WRITE (ERRMSG,816) TLAG
      CALL  PRTERR  ( 'STOP 816 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  816
 8170 CONTINUE
      WRITE (ERRMSG,817) IPUNCH
      CALL  PRTERR  ( 'STOP 817 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  817
 8180 CONTINUE
      WRITE (ERRMSG,818) IPOLL
      CALL  PRTERR  ( 'STOP 818 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  818
 7700 CONTINUE
      WRITE (ERRMSG,770) NVMSM,NVMSMM
      CALL  PRTERR  ( 'STOP 770 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  770
 7390 CONTINUE
      WRITE (ERRMSG,739) MCSTOP
      CALL  PRTERR  ( 'STOP 739 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  739
 7380 CONTINUE
      WRITE (ERRMSG,738) IPDSCL
      CALL  PRTERR  ( 'STOP 738 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  738
 7370 CONTINUE
      WRITE (ERRMSG,737) IABTMN
      CALL  PRTERR  ( 'STOP 737 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  737
 7360 CONTINUE
      WRITE (ERRMSG,736) IABTSD
      CALL  PRTERR  ( 'STOP 736 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  736
 7200 CONTINUE
      WRITE (ERRMSG,720) ISSAM
      CALL  PRTERR  ( 'STOP 720 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  720
 7170 CONTINUE
      WRITE (ERRMSG,717) ISIMGO
      CALL  PRTERR  ( 'STOP 717 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  717
 7160 CONTINUE
      WRITE (ERRMSG,716) DZBTIM
      CALL  PRTERR  ( 'STOP 716 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  716
 7150 CONTINUE
      WRITE (ERRMSG,715) DZETIM
      CALL  PRTERR  ( 'STOP 715 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  715
 7140 CONTINUE
      WRITE (ERRMSG,714) DZBTIM,DZETIM
      CALL  PRTERR  ( 'STOP 714 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  714
 7130 CONTINUE
      WRITE (ERRMSG,713) HITLST
      CALL  PRTERR  ( 'STOP 713 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RUSERD'                             )
      STOP  713
      END                                                               RUSERD
C
C
C
      SUBROUTINE RGEOPD
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ANIMAT'
      INCLUDE 'APPRO'
      INCLUDE 'ARC'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'CONFLT'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LINE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SDR'
      INCLUDE 'TITLE'
      INCLUDE 'USER'
      CHARACTER*1       JAFLAG
      CHARACTER*3       IDIA,JDIA
      INTEGER           I,IARC,ILINE,ILNB,ISDRC,ITEST,J,JA,JNTLNK,
     *                  JNTLNU,K,LL,LTURNT,MAXIB,MAXOB
      DOUBLE PRECISION  DISDT,DISLN
      DATA     JDIA   / 'DIA' /
  401 FORMAT(F4.2)
  501 FORMAT(I4,1X,A3,A5,A3)
  502 FORMAT(20I4)
  503 FORMAT(11I4,A1)
  504 FORMAT(51I1)
  505 FORMAT(20I4)
  506 FORMAT(A)
  600 FORMAT(//,
     *5X,39HMAXIMUM NUMBER OF VEHICLES IN SYSTEM IS,I4,/,
     *5X,45HHESITATION ADDED TO PIJR FOR FIRST VEHICLE IS,F4.1,
     *8H SECONDS)
  601 FORMAT(1X,A,//)
  602 FORMAT(21H LANE CONTROL FOR THE,I3,8H LANES =,50I2)
  603 FORMAT(/,
     *51H      WHERE  1 = OUTBOUND (OR BLOCKED INBOUND) LANE,/,
     *29H             2 = UNCONTROLLED,/,
     *27H             3 = YIELD SIGN,/,
     *26H             4 = STOP SIGN,/,
     *23H             5 = SIGNAL,/,
     *46H             6 = SIGNAL WITH LEFT  TURN ON RED,/,
     *46H             7 = SIGNAL WITH RIGHT TURN ON RED,//)
C1701 FORMAT(11H A TOTAL OF,I3,32H INBOUND AND OUTBOUND APPROACHES,/)
C1702 FORMAT(11H A TOTAL OF,I3,27H INBOUND AND OUTBOUND LANES,/)
C1703 FORMAT(11H A TOTAL OF,I3,28H SIGHT-DISTANCE RESTRICTIONS,/)
C1704 FORMAT(11H A TOTAL OF,I4,26H PATHS IN THE INTERSECTION,/)
C1705 FORMAT(11H A TOTAL OF,I5,29H INTERSECTION CONFLICT POINTS,/)
C1751 FORMAT(8H APPRO  ,I3,1X,A1,1X,39I3)
C1752 FORMAT(8H CONFLT ,I3,1X,12I4)
C1753 FORMAT(8H LANE   ,I3,1X,28I4)
C1754 FORMAT(8H PATH   ,I3,1X,27I4,2(/,30I4))
C1755 FORMAT(8H SDR    ,I3,1X,20I4,/,12X,20I4)
  721 FORMAT(
     *46H NUMBER OF SIGHT DISTANCE RESTRICTION ICANSE =,I3,
     *14H IS LT 1 OR GT,I3)
  722 FORMAT(
     *51H NUMBER OF SIGHT DISTANCE RESTRICTION COORDINATES =,I3,
     *14H IS LT 1 OR GT,I3)
  723 FORMAT(
     *18H NUMBER OF LINES =,I3,14H IS LT 1 OR GT,I3)
  724 FORMAT(
     *17H NUMBER OF ARCS =,I3,14H IS LT 1 OR GT,I3)
  741 FORMAT(
     *23H NUMBER OF APPROACHES =,I3,6H IS NE,I3)
  788 FORMAT(
     *35H NUMBER OF INTERSECTION CONFLICTS =,I4,14H IS LT 1 OR GT,I4)
  789 FORMAT(
     *42H NUMBER OF CONFLICTS FOR INTERSECTION PATH,I3,2H =,I3,
     *14H IS LT 1 OR GT,I3)
  790 FORMAT(
     *31H NUMBER OF INTERSECTION PATHS =,I3,14H IS LT 1 OR GT,I3)
  791 FORMAT(
     *40H NUMBER OF SIGHT DISTANCE RESTRICTIONS =,I3,14H IS LT 0 OR GT,
     *I3)
  792 FORMAT(
     *38H NUMBER OF INTERSECTION PATHS FOR LANE,I3,2H =,I3,
     *14H IS LT 1 OR GT,I2)
  793 FORMAT(
     *18H NUMBER OF LANES =,I3,14H IS LT 1 OR GT,I3)
  794 FORMAT(
     *51H NUMBER OF SIGHT DISTANCE RESTRICTIONS FOR APPROACH,I3,2H =,I3,
     *14H IS LT 0 OR GT,I3)
  795 FORMAT(
     *29H NUMBER OF LANES FOR APPROACH,I3,2H =,I3,14H IS LT 1 OR GT,I3)
  796 FORMAT(
     *18H APPROACH NUMBER =,I3,14H IS LT 1 OR GT,I3)
  797 FORMAT(
     *23H NUMBER OF APPROACHES =,I3,14H IS LT 1 OR GT,I3)
  798 FORMAT(
     *27H NUMBER OF OUTBOUND LANES =,I3,14H IS LT 1 OR GT,I2)
  799 FORMAT(
     *26H NUMBER OF INBOUND LANES =,I3,14H IS LT 1 OR GT,I2)
  819 FORMAT(
     *37H LANE CONTROL SPECIFIED FOR MORE THAN,I3,6H LANES)
  820 FORMAT(
     *5H LANE,I3,15H LANE CONTROL =,I2,16H IS LT 1 OR GT 7)
  821 FORMAT(
     *5H LANE,I3,15H LANE CONTROL =,I2,25H IS EQ 1 FOR INBOUND LANE)
  822 FORMAT(
     *5H LANE,I3,15H LANE CONTROL =,I2,26H IS NE 1 FOR OUTBOUND LANE)
  823 FORMAT(
     *5H LANE,I3,15H LANE CONTROL =,I2,8H IS GT 2,
     *42H FOR INTERSECTION TRAFFIC CONTROL TYPE = 1)
  824 FORMAT(
     *5H LANE,I3,15H LANE CONTROL =,I2,8H IS GT 3,
     *42H FOR INTERSECTION TRAFFIC CONTROL TYPE = 2)
  825 FORMAT(
     *5H LANE,I3,15H LANE CONTROL =,I2,8H IS GT 4,
     *42H FOR INTERSECTION TRAFFIC CONTROL TYPE = 3)
  826 FORMAT(
     *5H LANE,I3,15H LANE CONTROL =,I2,16H IS LT 3 OR GT 4,
     *42H FOR INTERSECTION TRAFFIC CONTROL TYPE = 4)
  827 FORMAT(
     *5H LANE,I3,15H LANE CONTROL =,I2,8H IS LT 3,
     *43H FOR INTERSECTION TRAFFIC CONTROL TYPE GE 5)
  828 FORMAT(
     *5H LANE,I3,40H SIGNAL WITH LEFT TURN ON RED SPECIFIED ,
     *26HFOR OTHER THAN MEDIAN LANE)
  829 FORMAT(
     *5H LANE,I3,41H SIGNAL WITH RIGHT TURN ON RED SPECIFIED ,
     *24HFOR OTHER THAN CURB LANE)
  895 FORMAT(
     *5H LANE,I3,45H DT TOO LARGE FOR LANE LENGTH AND SPEED LIMIT)
  896 FORMAT(
     *5H PATH,I4,45H DT TOO LARGE FOR PATH LENGTH AND SPEED LIMIT)
C
C-----SUBROUTINE RGEOPD READS THE GEOMETRY PROCESSOR DATA FROM THE
C-----GEOMETRY PROCESSOR TAPE AND READS THE LANE CONTROL INFORMATION
C-----FROM CARD 3 OF THE INPUT DIRECTLY TO THE SIMULATION PROCESSOR AND
C-----CHECKS FOR ERRORS
C
C[    JAFLAG     = '~'
C[    IDIA       = '~~~'
C[    I          = -2147483647
C[    ITEST      = -2147483647
C[    J          = -2147483647
C[    JA         = -2147483647
C[    JNTLNK     = -2147483647
C[    JNTLNU     = -2147483647
C[    K          = -2147483647
C[    LL         = -2147483647
C[    LTURNT     = -2147483647
C[    DISDT      = -2147483647.0
C[    DISLN      = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'RGEOPD'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----READ THE ARC INFORMATION
      READ (IGEOP,501) NARCS,IDIA,JVERSN,IMAGOP
      IF ( IDIA . EQ . JDIA )                    THEN
        DIAMON = .TRUE.
        HESFAC = 0.5D0*HESFAC
      ELSE
        DIAMON = .FALSE.
      END IF
                    IF ( JVERSN . EQ . '     ' ) JVERSN = 'V3.10'
      READ (JVERSN(2:5),401,ERR=9350) GVERSN
      WRITE (6,600) NVE,HESFAC
      IF ( ( NARCS.LT.0) . OR . (NARCS.GT.NAR) ) GO TO 7240
                    IF ( NARCS . EQ . 0 )        GO TO 1020
      DO 1010  I = 1 , NARCS
      READ (IGEOP,502) IARC,IARCX (IARC),IARCY(IARC),IARCAZ(IARC),
     *                      IARCSW(IARC),IARCR(IARC)
      LARCS(I) = IARC
 1010 CONTINUE
 1020 CONTINUE
C-----READ THE LINE INFORMATION
      READ (IGEOP,502) NLINES
      IF ( ( NLINES.LT.0) .OR. (NLINES.GT.NLI) ) GO TO 7230
                    IF ( NLINES . EQ . 0 )       GO TO 2020
      DO 2010  I = 1 , NLINES
      READ (IGEOP,502) ILINE,ILX1(ILINE),ILY1(ILINE),
     *                       ILX2(ILINE),ILY2(ILINE)
      LLINES(I) = ILINE
 2010 CONTINUE
 2020 CONTINUE
C-----READ THE IMAGE FILE DATA, IF ANY
      IF ( IMAGOP . EQ . 'YES' )                 THEN
        READ (IGEOP,506) IMAGL1
        READ (IGEOP,506) IMAGL2
      END IF
C-----READ THE APPROACH INDEXING INFORMATION
      MAXIB = 0
      MAXOB = 0
      READ (IGEOP,502) NIBA
      IF ( (NIBA.LT.1) . OR . (NIBA.GT.NIA) )    GO TO 7990
      READ (IGEOP,502) (LIBA(IAN),IAN=1,NIBA)
      DO 3010  IAN = 1 , NIBA
      IA = LIBA(IAN)
      LIBAR (IA) = IAN
      IATYPE(IA) = INBNDL
      MAXIB = MAX0( MAXIB,IA )
 3010 CONTINUE
      READ (IGEOP,502) NOBA,NLEGS,NFUT
      IF ( (NOBA.LT.1) . OR . (NOBA.GT.NOA) )    GO TO 7980
      READ (IGEOP,502) (LOBA(IAN),IAN=1,NOBA)
      DO 3020  IAN = 1 , NOBA
      IA = LOBA(IAN)
      LOBAR (IA) = IAN
      IATYPE(IA) = OUTBDL
      MAXOB = MAX0( MAXOB,IA )
 3020 CONTINUE
C-----READ THE NUMBER OF APPROACHES
      READ (IGEOP,502) NAPS
      IF ( (NAPS.LT.1) . OR . (NAPS.GT.NAP) )    GO TO 7970
      IF ( NAPS . NE . (NIBA+NOBA) )             GO TO 7410
C1    WRITE (6,701) NAPS
C-----READ THE INFORMATION FOR EACH APPROACH
      DO 3050  I = 1 , NAPS
 3030 CONTINUE
C-----READ THE APPROACH INFORMATION
      READ (IGEOP,503) JA,IAAZIM(JA),IAPX(JA),IAPY(JA),ISLIM(JA),
     *                 NLANES(JA),NSDR(JA),IALEFT(JA),IARGHT(JA),
     *                 JNTLNK,JNTLNU,JAFLAG
C[    IF ( JA                 .EQ.-2147483647   )STOP 'RGEOPD JA     01'
      IF ( (JA.LT.1) . OR . (JA.GT.NAP) )        GO TO 7961
      IF ( (NLANES(JA).LT.1  ) . OR .
     *     (NLANES(JA).GT.NAL) )                 GO TO 7950
      IF ( (NSDR(JA).LT.0    ) . OR .
     *     (NSDR(JA).GT.NIA-1) )                 GO TO 7940
      IF ( GVERSN . LT . 3.12 )                  THEN
        IF ( (.NOT. DIAMON) )                    IAFLAG(JA) = ILETTL
      ELSE
C[      IF ( JAFLAG           .EQ.'~'           )STOP 'RGEOPD JAFLAG 01'
        IAFLAG(JA) = JAFLAG
C[      IF ( JNTLNK           .EQ.-2147483647   )STOP 'RGEOPD JNTLNK 01'
        INTLNK(JA) = JNTLNK
C[      IF ( JNTLNU           .EQ.-2147483647   )STOP 'RGEOPD JNTLNU 01'
        INTLNU(JA) = JNTLNU
      END IF
      IF ( IAFLAG(JA) . EQ . ILETTI )            THEN
        IATYPE(JA) = DINBDL
      END IF
      READ (IGEOP,502) (LLANES(K,JA),K=1,NLANES(JA))
      DO 3035  K = 1 , NLANES(JA)
      ISNL(LLANES(K,JA)) = K
 3035 CONTINUE
                    IF ( NSDR(JA) . EQ . 0 )     GO TO 3040
      READ (IGEOP,502) (ISDRN(K,JA),ISDRA(K,JA),K=1,NSDR(JA))
 3040 CONTINUE
C1    WRITE (6,751) JA,IAFLAG(JA),NLANES(JA),NSDR(JA),IALEFT(JA),
C1   *              (LLANES(K,JA),K=1,NLANES(JA)),
C1   *              (ISDRN(K,JA),ISDRA(K,JA),K=1,NSDR(JA))
C[    IF ( JA                 .EQ.-2147483647   )STOP 'RGEOPD JA     02'
C-----END OF APPROACH LOOP
 3050 CONTINUE
C-----IF NLEGS NOT SPECIFIED THEN CALCULATE NLEGS AND NFUT
      IF ( NLEGS . EQ . 0 )                      THEN
        IF ( DIAMON )                            THEN
          NLEGS = 6
          NFUT  = 0
          IF ( IAFLAG(1) . EQ . ILETTI )         NFUT = NFUT + 1
          IF ( IAFLAG(6) . EQ . ILETTI )         NFUT = NFUT + 2
        ELSE
          NLEGS = MAX0( MAXIB,NIBA,INT( (MAXOB+1)/2 ),NOBA )
          NFUT  = 0
        END IF
      END IF
C-----READ THE NUMBER OF LANES
      READ (IGEOP,502) NRLAN
      IF ( (NRLAN.LT.1) . OR . (NRLAN.GT.NLA) )  GO TO 7930
C-----READ THE LANE CONTROL INFORMATION FROM CARD 3 OF THE INPUT
C-----DIRECTLY TO THE SIMULATION PROCESSOR
      READ (INPUT,504) (LCONTR(I),I=1,NRLAN),ITEST
      CALL  PHEADR  ( 6 )
      WRITE (6,601) STITLE
      WRITE (6,602) NRLAN,(LCONTR(I),I=1,NRLAN)
      WRITE (6,603)
C1    WRITE (6,702) NRLAN
C[    IF ( ITEST              .EQ.-2147483647   )STOP 'RGEOPD ITEST  01'
                    IF ( ITEST . NE . 0 )        GO TO 8190
C-----READ THE INFORMATION FOR EACH LANE
      DO 4030  I = 1 , NRLAN
C-----READ THE LANE INFORMATION
      READ (IGEOP,502) LWID(I),LTURN(I),NPINT(I),NLL(I),NLR(I),
     *                 ISNA(I),(LGEOM(LL,I),LL=1,4),IDX(I),IBLN(I),
     *                 LAVT(I)
      IF ( LAVT(I) . EQ . 0 )                    THEN
C-----  SET DEFAULT LANE ALLOWED VEHICLE TYPE FOR BICYCLES, EMERGENCY
C-----  VEHICLES, AND NORMAL VEHICLES
        LAVT(I) = LAVTB + LAVTE + LAVTV
      END IF
      AAVT(ISNA(I)) = IOR( AAVT(ISNA(I)),LAVT(I) )
                    IF ( IBLN(I) . NE . 0 )      LLANER(IBLN(I)) = I
C-----CHECK THE LANE CONTROL FOR ERRORS
                    IF ( LCONTR(I) .LT. LCOUTB ) GO TO 8200
                    IF ( LCONTR(I) .GT. LCSRTR ) GO TO 8200
      IF ( ( LCONTR(I) .EQ. LCOUTB ) . AND . ( LTURN(I) .NE. 0 ) )
     *                                           GO TO 8210
      IF ( ( LCONTR(I) .GT. LCOUTB ) . AND . ( LTURN(I) .EQ. 0 ) )
     *                                           GO TO 8220
                    IF ( LCONTR(I) .EQ. LCOUTB ) GO TO 4010
            IF ( LGEOM(3,I) . EQ . LGEOM(4,I) )  GO TO 4010
      IF ( ( ICONTR .EQ. ICUNCT ) . AND . ( LCONTR(I) .GT. LCUNCT ) )
     *                                           GO TO 8230
      IF ( ( ICONTR .EQ. ICYELD ) . AND . ( LCONTR(I) .GT. LCYELD ) )
     *                                           GO TO 8240
      IF ( ( ICONTR .EQ. ICLTAS ) . AND . ( LCONTR(I) .GT. LCSTOP ) )
     *                                           GO TO 8250
      IF ( ( ICONTR .EQ. ICAWST ) . AND . ( LCONTR(I) .LT. LCYELD ) )
     *                                           GO TO 8260
      IF ( ( ICONTR .EQ. ICAWST ) . AND . ( LCONTR(I) .GT. LCSTOP ) )
     *                                           GO TO 8260
      IF ( ( ICONTR .GE. ICPSIG ) . AND . ( LCONTR(I) .LT. LCYELD ) )
     *                                           GO TO 8270
      IF ( ( LCONTR(I) .EQ. LCSLTR ) . AND . ( NLL(I) .NE. 0 ) )
     *                                           GO TO 8280
      IF ( ( LCONTR(I) .EQ. LCSRTR ) . AND . ( NLR(I) .NE. 0 ) )
     *                                           GO TO 8290
 4010 CONTINUE
      LTURNT = LTURN(I)
                    IF ( LTURNT . GE . LTURNU )  LTURNT = LTURNT-LTURNU
                    IF ( LTURNT . GE . LTURNL )  LTURNT = LTURNT-LTURNL
                    IF ( LTURNT . LE . LTURNR )  GO TO 4015
      DISDT = ISLIM(ISNA(I))*DT
      DISLN = MAX0( LGEOM(4,I),LGEOM(2,I) ) - LGEOM(1,I)
      IF ((DT.GT.0.5D0) . AND . (DISDT.GT.DISLN))GO TO 8950
 4015 CONTINUE
      NIBL = MAX0( NIBL,IBLN(I) )
                    IF ( NPINT(I) . EQ . 0 )     GO TO 4020
      IF ( (NPINT(I).LT.1).OR.(NPINT(I).GT.NLP) )GO TO 7920
      READ (IGEOP,502) (LINTP(K,I),K=1,NPINT(I))
 4020 CONTINUE
      ILTYPE(I) = IATYPE(ISNA(I))
C1    WRITE (6,753) I,LWID(I),LTURN(I),NPINT(I),NLL(I),NLR(I),ISNA(I),
C1   *              (LGEOM(LL,I),LL=1,4),IBLN(I),
C1   *              (LINTP(K,I),K=1,NPINT(I))
C-----END OF LANE LOOP
 4030 CONTINUE
C-----COUNT THE NUMBER OF INBOUND LANES AT THE INTERSECTION, CALCULATE
C-----THE LOCATION ON EACH INBOUND LANE WHICH IS WITHIN 50 FT OF THE
C-----INTERSECTION, THE LOCATION ON EACH INBOUND LANE WHICH IS WITHIN
C-----200 FT OF THE INTERSECTION, AND THE NUMBER OF 50 FT SECTIONS
C-----WITHIN 200 FT OF THE INTERSECTION
      NIBLAI = 0
      DO 4050 IAN = 1 , NIBA
      IA = LIBA(IAN)
      DO 4040 ILN = 1 , NLANES(IA)
      IL = LLANES(ILN,IA)
C-----OPEN ALL 1=3, 2=4, AND 2>1
      IF ( LGEOM(1,IL) . EQ . LGEOM(3,IL) . AND .
     *     LGEOM(2,IL) . EQ . LGEOM(4,IL) . AND .
     *     LGEOM(2,IL) . GT . LGEOM(1,IL) )      THEN
        MINLLN(IA) = MIN( MINLLN(IA),LGEOM(1,IL) )
        MAXLLN(IA) = MAX( MAXLLN(IA),LGEOM(4,IL) )
      END IF
C-----BEG BLOCKED AND END OPEN 1=2, 3>2, AND 4>3
      IF ( LGEOM(1,IL) . EQ . LGEOM(2,IL) . AND .
     *     LGEOM(3,IL) . GT . LGEOM(2,IL) . AND .
     *     LGEOM(4,IL) . GT . LGEOM(3,IL) )      THEN
        MINLLN(IA) = MIN( MINLLN(IA),LGEOM(3,IL) )
        MAXLLN(IA) = MAX( MAXLLN(IA),LGEOM(4,IL) )
      END IF
C-----BEG OPEN AND END BLOCKED 3=4, 3>2, AND 2>1
      IF ( LGEOM(3,IL) . EQ . LGEOM(4,IL) . AND .
     *     LGEOM(3,IL) . GT . LGEOM(2,IL) . AND .
     *     LGEOM(2,IL) . GT . LGEOM(1,IL) )      THEN
        MINLLN(IA) = MIN( MINLLN(IA),LGEOM(1,IL) )
        MAXLLN(IA) = MAX( MAXLLN(IA),LGEOM(2,IL) )
      END IF
C-----BEG OPEN, MIDDLE BLOCKED, AND END OPEN 4>3, 3>2, AND 2>1
      IF ( LGEOM(4,IL) . GT . LGEOM(3,IL) . AND .
     *     LGEOM(3,IL) . GT . LGEOM(2,IL) . AND .
     *     LGEOM(2,IL) . GT . LGEOM(1,IL) )      THEN
        MINLLN(IA) = MIN( MINLLN(IA),LGEOM(1,IL) )
        MAXLLN(IA) = MAX( MAXLLN(IA),LGEOM(4,IL) )
      END IF
      IF ( LGEOM(3,IL) . NE . LGEOM(4,IL) )      THEN
        NIBLAI         = NIBLAI + 1
        LVILAI(ILN,IA) = MAX0( LGEOM(3,IL),LGEOM(4,IL)-50  )
        LVILNI(ILN,IA) = MAX0( LGEOM(3,IL),LGEOM(4,IL)-200 )
        MVILNI(ILN,IA) = MAX0( (LGEOM(4,IL)-LVILNI(ILN,IA)+25)/50,1 )
      END IF
 4040 CONTINUE
 4050 CONTINUE
C-----READ THE NUMBER OF SIGHT DISTANCE RESTRICTIONS
      READ (IGEOP,502) NUMSDR,MSS,NSDRC
                    IF ( MSS . EQ . 0 )          MSS = 40
      IF ( (NUMSDR.LT.0) . OR . (NUMSDR.GT.NSR) )GO TO 7910
      IF ( (MSS   .LT.0) . OR . (MSS   .GT.NSS) )GO TO 7220
      IF ( (NSDRC .LT.0) . OR . (NSDRC .GT.NSR) )GO TO 7210
                    IF ( NUMSDR . EQ . 0 )       GO TO 5020
C1    CALL  PHEADR  ( 6 )
C1    WRITE (6,601) STITLE
C1    WRITE (6,703) NUMSDR
C-----READ THE INFORMATION FOR EACH SIGHT DISTANCE RESTRICTION
      DO 5010  I = 1 , NUMSDR
C-----READ THE SIGHT DISTANCE RESTRICTION INFORMATION
      READ (IGEOP,502) (ICANSE(J,I),J=1,NSS)
C1    WRITE (6,755)  I,(ICANSE(J,I),J=1,NSS)
C-----END OF SIGHT DISTANCE RESTRICTION LOOP
 5010 CONTINUE
 5020 CONTINUE
                    IF ( NSDRC . LE . 0 )        GO TO 5040
C-----WRITE THE COORDINATE INFORMATION FOR EACH SIGHT DISTANCE
C-----RESTRICTION
      DO 5030  I = 1 , NSDRC
      READ (IGEOP,502) ISDRC,IXSDRC(ISDRC),IYSDRC(ISDRC)
      LSDRC(I) = ISDRC
 5030 CONTINUE
 5040 CONTINUE
C-----READ THE NUMBER OF INTERSECTION PATHS
      READ (IGEOP,502) NPATHS
      IF ( (NPATHS.LT.1) . OR . (NPATHS.GT.NPA) )GO TO 7900
C1    CALL  PHEADR  ( 6 )
C1    WRITE (6,601) STITLE
C1    WRITE (6,704) NPATHS
 6010 CONTINUE
C-----READ THE INFORMATION FOR EACH INTERSECTION PATH
      DO 6030  I = 1 , NPATHS
C-----READ THE INTERSECTION PATH INFORMATION
      READ (IGEOP,505) IIA(I) ,IIL(I) ,IOA(I),IOL(I) ,
     *                 IXL1(I),IYL1(I),LL1(I),JXL1(I),JYL1(I),
     *                 IXA1(I),IYA1(I),LA1(I),IRA1(I),IBA1(I),IDA1(I),
     *                 IXA2(I),IYA2(I),LA2(I),IRA2(I),IBA2(I),IDA2(I),
     *                 IXL2(I),IYL2(I),LL2(I),JXL2(I),JYL2(I),
     *                 LENP(I),IPT(I),LIMP(I),IOPT(I),
     *                 ILCH(I),LIBL(I),LOBL(I),NGEOCP(I),PAVT(I)
      IF ( PAVT(I) . EQ . 0 )                    THEN
C-----  SET DEFAULT INTERSECTION PATH ALLOWED VEHICLE TYPE FOR BICYCLES,
C-----  EMERGENCY VEHICLES, AND NORMAL VEHICLES
        PAVT(I) = LAVTB + LAVTE + LAVTV
      END IF
      LENPT (I) = LL1(I)
      RADMAX(I) = MAX0( IRA1(I),IRA2(I) )
      RADMIN(I) = IRA1(I)
      IF ( (IRA2(I).GT.0        ) . AND .
     *     (IRA2(I).LT.RADMIN(I)) )              RADMIN(I) = IRA2(I)
      IF ( RADMIN(I) . EQ . 0 )                  RADMIN(I) = POSBIG
                    IF ( NGEOCP(I) . EQ . 0 )    GO TO 6020
      IF ( (NGEOCP(I).LT.0  ) . OR .
     *     (NGEOCP(I).GT.NCP) )                  GO TO 7890
      READ (IGEOP,502) (IGEOCP(K,I),K=1,NGEOCP(I))
 6020 CONTINUE
C-----FIX THE LOBL FOR INTERSECTION PATHS THAT LINK TO DIAMOND
C-----INTERCHANGE INTERNAL LANES PROCESSED BY GEOPRO BEFORE VERSION 3.12
C-----NOTE: IOA AND IOL ARE NOT FIXED FOR ANY VERSION
      IF ( GVERSN . LT . 3.12 )                  THEN
        IF ( IAFLAG(IOA(I)) . EQ . ILETTI )      THEN
          LOBL(I) = LLANES(IOL(I),INTLNK(IOA(I)))
        END IF
      END IF
C1    WRITE (6,754) I,IOA(I),
C1   *              IXL1(I),IYL1(I),JXL1(I),JYL1(I),
C1   *              IXA1(I),IYA1(I),IRA1(I),IBA1(I),IDA1(I),
C1   *              IXA2(I),IYA2(I),IRA2(I),IBA2(I),IDA2(I),
C1   *              IXL2(I),IYL2(I),JXL2(I),JYL2(I),
C1   *              LENP(I),IPT(I),LIMP(I),IOPT(I),
C1   *              ILCH(I),LIBL(I),LOBL(I),NGEOCP(I),
C1   *              (IGEOCP(K,I),K=1,NGEOCP(I))
                    IF ( IPT(I) . NE . 2 )       GO TO 6030
      DISDT = LIMP(I)*DT
      IF ((DT.GT.0.5D0) .AND. (DISDT.GT.LENP(I)))GO TO 8961
C-----END OF INTERSECTION PATH LOOP
 6030 CONTINUE
C-----READ THE NUMBER OF INTERSECTION CONFLICTS
      READ (IGEOP,502) NOCONF
      IF ( (NOCONF.LT.1) . OR . (NOCONF.GT.NCO) )GO TO 7880
C1    CALL  PHEADR  ( 6 )
C1    WRITE (6,601) STITLE
C1    WRITE (6,705) NOCONF
C-----READ THE INFORMATION FOR EACH INTERSECTION CONFLICT
      DO 7010  I = 1 , NOCONF
C-----READ THE INTERSECTION CONFLICT INFORMATION
      READ (IGEOP,502) ICONP(1,I),ICONP(2,I),ICONA(1,I),ICONA(2,I),
     *                 ICOND(1,I),ICOND(2,I),ICONAN(I),ICONI(1,I),
     *                 ICONI(2,I)
C1    WRITE (6,752) I,ICONP(1,I),ICONP(2,I),ICONA(1,I),ICONA(2,I),
C1   *              ICOND(1,I),ICOND(2,I),ICONAN(I),ICONI(1,I),
C1   *              ICONI(2,I)
C-----END OF INTERSECTION CONFLICT LOOP
 7010 CONTINUE
      RETURN
C-----PROCESS THE INPUT ERRORS AND STOP
 7210 CONTINUE
      WRITE (ERRMSG,721) NSDRC,NSR
      CALL  PRTERR  ( 'STOP 721 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  721
 7220 CONTINUE
      WRITE (ERRMSG,722) MSS,NSS
      CALL  PRTERR  ( 'STOP 722 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  722
 7230 CONTINUE
      WRITE (ERRMSG,723) NLINES,NLI
      CALL  PRTERR  ( 'STOP 723 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  723
 7240 CONTINUE
      WRITE (ERRMSG,724) NARCS,NAR
      CALL  PRTERR  ( 'STOP 724 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  724
 7410 CONTINUE
      WRITE (ERRMSG,741) NAPS,(NIBA+NOBA)
      CALL  PRTERR  ( 'STOP 741 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  741
 7880 CONTINUE
      WRITE (ERRMSG,788) NOCONF,NCO
      CALL  PRTERR  ( 'STOP 788 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  788
 7890 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      01'
      WRITE (ERRMSG,789) I,NGEOCP(I),NCP
      CALL  PRTERR  ( 'STOP 789 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  789
 7900 CONTINUE
      WRITE (ERRMSG,790) NPATHS,NPA
      CALL  PRTERR  ( 'STOP 790 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  790
 7910 CONTINUE
      WRITE (ERRMSG,791) NUMSDR,NSR
      CALL  PRTERR  ( 'STOP 791 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  791
 7920 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      02'
      WRITE (ERRMSG,792) I,NPINT(I),NLP
      CALL  PRTERR  ( 'STOP 792 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  792
 7930 CONTINUE
      WRITE (ERRMSG,793) NRLAN,NLA
      CALL  PRTERR  ( 'STOP 793 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  793
 7940 CONTINUE
C[    IF ( JA                 .EQ.-2147483647   )STOP 'RGEOPD JA     03'
      WRITE (ERRMSG,794) JA,NSDR(JA),NIA-1
      CALL  PRTERR  ( 'STOP 794 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  794
 7950 CONTINUE
C[    IF ( JA                 .EQ.-2147483647   )STOP 'RGEOPD JA     04'
      WRITE (ERRMSG,795) JA,NLANES(JA),NAL
      CALL  PRTERR  ( 'STOP 795 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  795
 7961 CONTINUE
C[    IF ( JA                 .EQ.-2147483647   )STOP 'RGEOPD JA     05'
      WRITE (ERRMSG,796) JA,NAP
      CALL  PRTERR  ( 'STOP 796 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  796
 7970 CONTINUE
      WRITE (ERRMSG,797) NAPS,NAP
      CALL  PRTERR  ( 'STOP 797 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  797
 7980 CONTINUE
      WRITE (ERRMSG,798) NOBA,NOA
      CALL  PRTERR  ( 'STOP 798 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  798
 7990 CONTINUE
      WRITE (ERRMSG,799) NIBA,NIA
      CALL  PRTERR  ( 'STOP 799 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  799
 8190 CONTINUE
      WRITE (ERRMSG,819) NRLAN
      CALL  PRTERR  ( 'STOP 819 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  819
 8200 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      03'
      WRITE (ERRMSG,820) I,LCONTR(I)
      CALL  PRTERR  ( 'STOP 820 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  820
 8210 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      04'
      WRITE (ERRMSG,821) I,LCONTR(I)
      CALL  PRTERR  ( 'STOP 821 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  821
 8220 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      05'
      WRITE (ERRMSG,822) I,LCONTR(I)
      CALL  PRTERR  ( 'STOP 822 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  822
 8230 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      06'
      WRITE (ERRMSG,823) I,LCONTR(I)
      CALL  PRTERR  ( 'STOP 823 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  823
 8240 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      07'
      WRITE (ERRMSG,824) I,LCONTR(I)
      CALL  PRTERR  ( 'STOP 824 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  824
 8250 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      08'
      WRITE (ERRMSG,825) I,LCONTR(I)
      CALL  PRTERR  ( 'STOP 825 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  825
 8260 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      09'
      WRITE (ERRMSG,826) I,LCONTR(I)
      CALL  PRTERR  ( 'STOP 826 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  826
 8270 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      10'
      WRITE (ERRMSG,827) I,LCONTR(I)
      CALL  PRTERR  ( 'STOP 827 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  827
 8280 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      11'
      WRITE (ERRMSG,828) I
      CALL  PRTERR  ( 'STOP 828 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  828
 8290 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      12'
      WRITE (ERRMSG,829) I
      CALL  PRTERR  ( 'STOP 829 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  829
 8950 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      13'
      WRITE (ERRMSG,895) I
      CALL  PRTERR  ( 'STOP 895 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  895
 8961 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RGEOPD I      14'
      WRITE (ERRMSG,896) I
      CALL  PRTERR  ( 'STOP 896 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RGEOPD'                             )
      STOP  896
 9350 CONTINUE
      CALL  PRTERR  ( 'STOP 935 - ' //
     *                'ERROR READING GVERSN FROM JVERSN - ' //
     *                'RGEOPD'                                 )
      STOP  935
      END                                                               RGEOPD
C
C
C
      SUBROUTINE RCAMSD
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PHASES'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
C1    INCLUDE 'TITLE'
      INCLUDE 'TXDSIG'
      INCLUDE 'USER'
      CHARACTER*1       IISIGN(4),IITURN(3),LANESS(3*NIL),IC,ILETTC,
     *                  ILETTP
      CHARACTER*3       JSISET(-1:25)
      CHARACTER*9       IPTURN(ITURNR)
      INTEGER           I,ICAM1,IGARP,II,ILNB,ISVAL(3,4,3),ITC,J,JBLN,
     *                  JGARP,JJ,K,KK,MCONTR,MOV,MPHASE,NINT,NLC
      DATA     IISIGN / 'G','A','R','P' /
      DATA     IITURN / 'L','S','R' /
      DATA     ILETTC / 'C' /
      DATA     ILETTP / 'P' /
      DATA     IPTURN / 'U-TURN --','LEFT ----','STRAIGHT ','RIGHT ---'/
C-----         ISVAL    LGG SGG RGG LAG SAG RAG LRG SRG RRG LPG SPG RPG
C-----                  LGA SGA RGA LAA SAA RAA LRA SRA RRA LPA SPA RPA
C-----                  LGR SGR RGR LAR SAR RAR LRR SRR RRR LPR SPR RPR
      DATA     ISVAL  /  1,  1,  1,  7, 13, 19,  9, 15, 21, 23, -1, -1,
     *                   5, 11, 17,  2,  2,  2, 10, 16, 22, 24, -1, -1,
     *                   6, 12, 18,  8, 14, 20,  3,  3,  3, 25, -1, -1/
      DATA     JSISET / 'ERR','UNS',
     *                  'AG ','AA ','AR ','AP ','LGA','LGR','LAG','LAR',
     *                  'LRG','LRA','SGA','SGR','SAG','SAR','SRG','SRA',
     *                  'RGA','RGR','RAG','RAR','RRG','RRA','LPG','LPA',
     *                  'LPR'                                          /
  501 FORMAT(20I4)
  502 FORMAT(I2,F3.0,75A1)
C1601 FORMAT(1X,A,//)
  512 FORMAT(1X,A1,F3.0,75A1)
  602 FORMAT(11H A TOTAL OF,I3,18H CAM STACK ENTRIES,/)
  603 FORMAT(6H ENTRY,I3,6H PHASE,I3,7H TIME =,F6.2,1X,25(1X,3A1))
  604 FORMAT(6H ENTRY,I3,6H PHASE,I3,1X,25(1X,3A1))
  605 FORMAT(4H THE,I3,
     *       47H ENTRIES DESIGNATED BY LETTERS ARE FOR OVERLAPS,/)
  614 FORMAT(6H ENTRY,I3,8H OVRLAP ,A,1X,25(1X,3A1))
  615 FORMAT(/,
     *       49H TIME FOR NEXT PHASE(S) BY MOVEMENT WITH THE SAME,
     *       49H SIGNAL INDICATION (GREEN AND PROTECTED GREEN ARE,
     *       21H CONSIDERED THE SAME),/)
  616 FORMAT(6H ENTRY,I3,6H PHASE,I3,7H TIME =,F6.2,1X,25(1X,A3))
  617 FORMAT(19X,A8,4H----,25I4)
  779 FORMAT(
     *10H CAM STACK,I3,24H SIGNAL PHASE NUMBER = (,A,
     *57H) DOES NOT HAVE THE CHARACTER (A) THROUGH (P) IN COLUMN 2)
  780 FORMAT(
     *10H CAM STACK,I3,24H SIGNAL PHASE NUMBER = (,A,
     *49H) DOES NOT HAVE A DIGIT OR BLANK IN COLUMN 1 OR 2)
  830 FORMAT(
     *30H NUMBER OF CAM STACK ENTRIES =,I4,14H IS LT 4 OR GT,I3)
  831 FORMAT(
     *10H CAM STACK,I3,20H SIGNAL PHASE NUMBER,I2,2H =,I3,
     *14H IS LT 1 OR GT,I2)
  832 FORMAT(
     *10H CAM STACK,I3,13H PHASE TIME =,F6.2,8H IS LT 1)
  833 FORMAT(
     *10H CAM STACK,I3,5H LANE,I3,13H INBOUND LANE,I3,
     *20H FIRST CHARACTER = (,A1,28H) IS NOT (L) (S) (R) (A) (U),
     *11H (N) OR ( ))
  834 FORMAT(
     *10H CAM STACK,I3,5H LANE,I3,13H INBOUND LANE,I3,
     *21H SECOND CHARACTER = (,A1,27H) IS NOT (G) (A) (R) (P) (N,
     *12H) (C) OR ( ))
  835 FORMAT(
     *10H CAM STACK,I3,5H LANE,I3,13H INBOUND LANE,I3,
     *20H THIRD CHARACTER = (,A1,28H) IS NOT (G) (A) (R) (S) (P),
     *7H OR ( ))
  836 FORMAT(
     *10H CAM STACK,I3,5H LANE,I3,13H INBOUND LANE,I3,
     *20H FIRST CHARACTER = (,A1,22H) SECOND CHARACTER = (,A1,
     *21H) THIRD CHARACTER = (,A1,27H) IS AN ILLEGAL COMBINATION)
  837 FORMAT(
     *10H CAM STACK,I3,5H LANE,I3,13H INBOUND LANE,I3,
     *21H SECOND CHARACTER = (,A1,27H) IS NOT (G) (A) (R) OR (P),
     *27H WHEN FIRST CHARACTER = (A))
  838 FORMAT(
     *10H CAM STACK,I3,5H LANE,I3,13H INBOUND LANE,I3,
     *47H FIRST CHARACTER = (A) AND SECOND CHARACTER = (,A1,
     *25H) BUT THIRD CHARACTER = (,A1,12H) IS NOT ( ))
  839 FORMAT(
     *10H CAM STACK,I3,5H LANE,I3,13H INBOUND LANE,I3,
     *47H FIRST CHARACTER = ( ) BUT SECOND CHARACTER = (,A1,
     *17H) IS NOT ( ) ALSO)
  840 FORMAT(
     *10H CAM STACK,I3,5H LANE,I3,13H INBOUND LANE,I3,
     *49H FIRST CHARACTER = ( ) AND SECOND CHARACTER = ( ),
     *24H BUT THIRD CHARACTER = (,A1,17H) IS NOT ( ) ALSO)
  841 FORMAT(
     *5H LANE,I3,13H INBOUND LANE,I3,22H FIRST CHARACTER = ( ),
     *53H AND SECOND CHARACTER = ( ) AND THIRD CHARACTER = ( ),
     *16H FOR CAM STACK 1)
  842 FORMAT(
     *10H CAM STACK,I3,5H LANE,I3,13H INBOUND LANE,I3,
     *20H FIRST CHARACTER = (,A1,22H) SECOND CHARACTER = (,A1,
     *21H) THIRD CHARACTER = (,A1,34H) IS ILLEGAL FOR UNSIGNALIZED LANE)
  885 FORMAT(
     *22H NUMBER OF OVERLAPS IS,I3,11H MUST BE LE,I3)
C
C-----SUBROUTINE RCAMSD READS THE CAM STACK INFORMATION FROM THE INPUT
C-----DIRECTLY TO THE SIMULATION PROCESSOR AND CHECKS FOR ERRORS
C
C[    DO  I = 1 , 3*NIL
C[    LANESS(I)  = '~'
C[    END DO
C[    IC         = '~'
C[    I          = -2147483647
C[    ICAM1      = -2147483647
C[    II         = -2147483647
C[    J          = -2147483647
C[    JBLN       = -2147483647
C[    JJ         = -2147483647
C[    K          = -2147483647
C[    KK         = -2147483647
C[    MCONTR     = -2147483647
C[    NLC        = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'RCAMSD'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IF ( ICONTR . LT . ICNEMA )                THEN
C-----  PRETIMED, SEMI-ACT, FULL-ACT, TEX-DIA, AND DAL-DIA
        MPHASE = NPH
        MOV    = NOV
C-----  READ THE NUMBER OF CAM STACK POSITIONS
        READ (INPUT,501) NCAMSP
        IF ( FIG3 .OR. FIG4 .OR. FIG6 .OR. FIG7 )THEN
          LOLP(1) = NCAMSP + 1
          LOLP(2) = NCAMSP + 3
          NOLP    = 2
        END IF
      ELSE
C-----  NEMA AND HARDWARE
        IF ( ICONTR . LT . ICHDWR )              THEN
C-----    NEMA
          MPHASE = NPN
          MOV    = NON
C-----    READ THE NUMBER OF CAMSTACKS, SIGNAL PHASES, OVERLAPS, RINGS,
C-----    AND GROUPS (GROUPS ARE SEPARATED BY BARRIERS)
          READ (INPUT,501) NCAMSP,NPHASE,NOLP,NRING,NGROUP
        ELSE
C-----    HARDWARE
          MPHASE = HPH
          MOV    = HOV
C-----    READ THE NUMBER OF CAMSTACKS, SIGNAL PHASES AND OVERLAPS
          READ (INPUT,501) NCAMSP,NPHASE,NOLP
          NRING  = 0
          NGROUP = 0
          NPRCNT = IDNINT( 1.0/DT )
        END IF
C-----  ADJUST NCAMSP TO BE THE LAST POSITION FOR PHASES
        NCAMSP = NCAMSP - 2*NOLP
      END IF
C1    CALL  PHEADR  ( 6 )
C1    WRITE (6,601) STITLE
      WRITE (6,602) NCAMSP + 2*NOLP
      IF ( (NCAMSP.       LT.4  ) . OR .
     *     (NCAMSP+2*NOLP.GT.NCM) )              GO TO 8300
      NLC = NIBL*3
      IF ( NOLP . GT . 0 )                       THEN
        WRITE (6,605) 2*NOLP
        IF ( NOLP . GT . MOV )                   GO TO 8850
      END IF
      ICAM1 = ICHAR( 'A' ) - 1
C-----READ THE INFORMATION FOR EACH CAM STACK POSITION
      DO 7020  I = 1 , NCAMSP + 2*NOLP
      READ (INPUT,FMT) NLINE
      CALL  TOUPR   ( NLINE )
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      01'
      IF ( I . LE . NCAMSP )                     THEN
        IF ( ((NLINE(1:1).LT.'0').OR.(NLINE(1:1).GT.'9')) . AND .
     *        (NLINE(1:1).NE.' ') )              GO TO 7800
        IF ( ((NLINE(2:2).LT.'0').OR.(NLINE(2:2).GT.'9')) . AND .
     *        (NLINE(2:2).NE.' ') )              GO TO 7800
C[      IF ( I                .EQ.-2147483647   )STOP 'RCAMSD I      02'
C[      IF ( NLC              .EQ.-2147483647   )STOP 'RCAMSD NLC    01'
        READ (NLINE,502) ICAMPH(I),TCAMSP(I),(LANESS(J),J=1,NLC)
        ICAMPH(I) = PDL2TX(ICAMPH(I))
                    IF ( ICONTR . GT . ICPSIG )  GO TO 1010
C[      IF ( NLC              .EQ.-2147483647   )STOP 'RCAMSD NLC    02'
C[      DO 987  J = 1 , NLC
C[      IF ( LANESS(J)        .EQ.'~'           )STOP 'RCAMSD LANESS 03'
C[987   CONTINUE
C[      IF ( I                .EQ.-2147483647   )STOP 'RCAMSD I      03'
C[      IF ( NLC              .EQ.-2147483647   )STOP 'RCAMSD NLC    03'
        WRITE (6,603) I,PTX2DL(ICAMPH(I)),TCAMSP(I),(LANESS(J),J=1,NLC)
        NPHASE = MAX0( NPHASE,ICAMPH(I) )
        GO TO 1020
      ELSE
        IF ( (NLINE(2:2) . LT . 'A') . OR .
     *       (NLINE(2:2) . GT . 'P') )           GO TO 7790
C
C ----- OVERLAP CAMSTACK ENTRIES ARE NOT CHECKED TO SEE IF
C ----- THERE ARE EXACTLY 2 ENTRIES FOR EACH OVERLAP OR THAT
C ----- THE OVERLAP IDENTIFIER IN COLUMN 2 IS CORRECT
C ----- THEY ARE ASSUMED TO BE IN CORRECT ORDER:
C ----- ALPHABETICAL PER OVERLAP LETTER, WITH GREEN THEN YELLOW
C
C[      IF ( I                .EQ.-2147483647   )STOP 'RCAMSD I      04'
C[      IF ( NLC              .EQ.-2147483647   )STOP 'RCAMSD NLC    04'
        READ (NLINE,512) IC,TCAMSP(I),(LANESS(J),J=1,NLC)
C[      IF ( NLC              .EQ.-2147483647   )STOP 'RCAMSD NLC    05'
C[      DO 986  J = 1 , NLC
C[      IF ( LANESS(J)        .EQ.'~'           )STOP 'RCAMSD LANESS 02'
C[986   CONTINUE
C[      IF ( I                .EQ.-2147483647   )STOP 'RCAMSD I      05'
C[      IF ( IC               .EQ.'~'           )STOP 'RCAMSD IC     01'
C[      IF ( NLC              .EQ.-2147483647   )STOP 'RCAMSD NLC    06'
        WRITE (6,614) I,IC,(LANESS(J),J=1,NLC)
C
C ----- SET ICAMPH: 1=A, 2=B, 3=C, AND 4=D
C
C[      IF ( IC               .EQ.'~'           )STOP 'RCAMSD IC     01'
C[      IF ( ICAM1            .EQ.-2147483647   )STOP 'RCAMSD ICAM1  01'
        ICAMPH(I) = ICHAR( IC ) - ICAM1
        GO TO 1025
      END IF
 1010 CONTINUE
C[    IF ( NLC                .EQ.-2147483647   )STOP 'RCAMSD NLC    07'
C[    DO 985  J = 1 , NLC
C[    IF ( LANESS(J)          .EQ.'~'           )STOP 'RCAMSD LANESS 03'
C[985 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      06'
C[    IF ( NLC                .EQ.-2147483647   )STOP 'RCAMSD NLC    08'
      WRITE (6,604) I,PTX2DL(ICAMPH(I)),(LANESS(J),J=1,NLC)
 1020 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      07'
                    IF ( ICAMPH(I) .LT. 1      ) GO TO 8310
                    IF ( ICAMPH(I) .GT. MPHASE ) GO TO 8310
 1025 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      08'
      IF ( ( ICONTR .EQ. ICPSIG ) . AND . ( TCAMSP(I) .LT. 1.0D0 ) )
     *                                           GO TO 8320
      K = 1
C-----CHECK EACH LANE FOR THREE CHARACTER SIGNAL SETTING
      DO 7010  J = 1 , NRLAN
C-----IF THIS IS NOT AN INBOUND LANE THEN SKIP TO THE NEXT LANE
      JBLN = IBLN(J)
                    IF ( JBLN . EQ . 0 )         GO TO 7010
      MCONTR = LCONTR(J)
C-----IF THE FIRST CHARACTER = ( ) THEN GO TO 5010 AND USE THE SIGNAL
C-----SETTING FROM THE LAST CAM STACK POSITION FOR THIS LANE
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      01'
C[    IF ( LANESS(K)          .EQ.'~'           )STOP 'RCAMSD LANESS 04'
                    IF ( LANESS(K).EQ.IBLNK1 )   GO TO 5010
C-----IF THE FIRST CHARACTER = (A) THEN GO TO 4010 AND CHECK THE SECOND
C-----CHARACTER FOR (G) (A) (R) OR (P)
                    IF ( LANESS(K).EQ.ILETTA )   GO TO 4010
C-----IF THE THREE CHARACTERS = (UNS) THEN GO TO 6020 WITH ISISET=0
C[    IF ( LANESS(K  )        .EQ.'~'           )STOP 'RCAMSD LANESS 05'
C[    IF ( LANESS(K+1)        .EQ.'~'           )STOP 'RCAMSD LANESS 06'
C[    IF ( LANESS(K+2)        .EQ.'~'           )STOP 'RCAMSD LANESS 07'
C[    IF ( MCONTR             .EQ.-2147483647   )STOP 'RCAMSD MCONTR 01'
      IF ( ( LANESS(K  ) . EQ . ILETTU ) . AND .
     *     ( LANESS(K+1) . EQ . ILETTN ) . AND .
     *     ( LANESS(K+2) . EQ . ILETTS ) . AND .
     *     ( MCONTR      . LE . LCSTOP ) )       GO TO 6020
C-----IF THE THREE CHARACTERS = (NCP) THEN GO TO 6020 WITH ISISET=3
      IF ( ( LANESS(K  ) . EQ . ILETTN ) . AND .
     *     ( LANESS(K+1) . EQ . ILETTC ) . AND .
     *     ( LANESS(K+2) . EQ . ILETTP ) . AND .
     *     ( ICONTR      . GE . ICTDF3 ) . AND .
     *     ( MCONTR      . GE . LCSIGX ) )       THEN
C[      IF ( I                .EQ.-2147483647   )STOP 'RCAMSD I      09'
C[      IF ( JBLN             .EQ.-2147483647   )STOP 'RCAMSD JBLN   01'
        ISISET(I,JBLN) = 3
        GO TO 6020
      END IF
C-----CHECK FIRST CHARACTER FOR (L) (S) OR (R)
      DO 1030  II = 1 , 3
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      02'
C[    IF ( LANESS(K)          .EQ.'~'           )STOP 'RCAMSD LANESS 08'
            IF ( LANESS(K  ).EQ.IITURN(II) )     GO TO 1040
 1030 CONTINUE
      GO TO 8330
 1040 CONTINUE
C-----CHECK SECOND CHARACTER FOR (G) (A) (R) OR (P)
      DO 2010  JJ = 1 , 4
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      03'
C[    IF ( LANESS(K+1)        .EQ.'~'           )STOP 'RCAMSD LANESS 09'
            IF ( LANESS(K+1).EQ.IISIGN(JJ) )     GO TO 2020
 2010 CONTINUE
      GO TO 8340
 2020 CONTINUE
C-----CHECK THIRD CHARACTER FOR (G) (A) OR (R)
      DO 3010  KK = 1 , 3
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      04'
C[    IF ( LANESS(K+2)        .EQ.'~'           )STOP 'RCAMSD LANESS 10'
            IF ( LANESS(K+2).EQ.IISIGN(KK) )     GO TO 3020
 3010 CONTINUE
      GO TO 8350
 3020 CONTINUE
C-----SET SIGNAL SETTING NUMBER FOR THIS CAM STACK POSITION AND INBOUND
C-----LANE BASED ON THE THREE CHARACTER SIGNAL CODE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      10'
C[    IF ( II                 .EQ.-2147483647   )STOP 'RCAMSD II     01'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   02'
C[    IF ( JJ                 .EQ.-2147483647   )STOP 'RCAMSD JJ     01'
C[    IF ( KK                 .EQ.-2147483647   )STOP 'RCAMSD KK     01'
      ISISET(I,JBLN) = ISVAL(II,JJ,KK)
                    IF ( ISISET(I,JBLN).LE.0 )   GO TO 8360
      GO TO 6010
 4010 CONTINUE
C-----CHECK THE SECOND CHARACTER FOR (G) (A) (R) OR (P) WHEN THE FIRST
C-----CHARACTER = (A)
      DO 4020  II = 1 , 4
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      05'
C[    IF ( LANESS(K+1)        .EQ.'~'           )STOP 'RCAMSD LANESS 11'
            IF ( LANESS(K+1).EQ.IISIGN(II) )     GO TO 4030
 4020 CONTINUE
      GO TO 8370
 4030 CONTINUE
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      06'
C[    IF ( LANESS(K+2)        .EQ.'~'           )STOP 'RCAMSD LANESS 12'
                    IF ( LANESS(K+2).NE.IBLNK1 ) GO TO 8380
C-----SET SIGNAL SETTING NUMBER FOR THIS CAM STACK POSITION AND INBOUND
C-----LANE BASED ON THE SECOND CHARACTER WHEN FIRST CHARACTER = (A)
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      11'
C[    IF ( II                 .EQ.-2147483647   )STOP 'RCAMSD II     02'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   03'
      ISISET(I,JBLN) = II
      GO TO 6010
 5010 CONTINUE
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      07'
C[    IF ( LANESS(K+1)        .EQ.'~'           )STOP 'RCAMSD LANESS 13'
                    IF ( LANESS(K+1).NE.IBLNK1 ) GO TO 8390
C[    IF ( LANESS(K+2)        .EQ.'~'           )STOP 'RCAMSD LANESS 14'
                    IF ( LANESS(K+2).NE.IBLNK1 ) GO TO 8400
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      12'
                    IF ( I . EQ . 1 )            GO TO 8410
C-----SET SIGNAL SETTING NUMBER FOR THIS CAM STACK POSITION AND INBOUND
C-----LANE TO THE SIGNAL SETTING NUMBER FOR THE LAST CAM STACK POSITION
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   04'
      ISISET(I,JBLN) = ISISET(I-1,JBLN)
 6010 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      13'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   05'
C[    IF ( MCONTR             .EQ.-2147483647   )STOP 'RCAMSD MCONTR 01'
      IF ( ( MCONTR         .LE. LCSTOP ) . AND .
     *     ( ISISET(I,JBLN) .NE. 0      ) )      GO TO 8420
 6020 CONTINUE
C-----INCREMENT POINTER FOR NEXT THREE CHARACTERS
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      08'
      K = K + 3
C-----END OF LANE LOOP
 7010 CONTINUE
C-----END OF CAM STACK INFORMATION LOOP
 7020 CONTINUE
C-----INITIALIZE SIGNAL SETTINGS FOR PRE-TIMED SIGNAL
      ICAMPC = 1
      ICAMPO = NCAMSP
      ICPHAS = ICAMPH(ICAMPC)
      TP = 0.0D0
      TR = TCAMSP(ICAMPC)
      TRLAST(1) = TR
C-----IF PRE-TIMED SIGNAL CONTROLLER THEN CALCULATE THE TIME REMAINING
C-----IN FUTURE PHASES WITH THE SAME SIGNAL INDICATION FOR INBOUND LANE
C-----AND TURN CODE
      IF ( ICONTR . EQ . ICPSIG )                THEN
        DO 7060  JBLN = 1      , NIBL
        DO 7050  ITC  = ITURNU , ITURNR
        DO 7040  I    = 1      , NCAMSP
        TRTCMN(JBLN,ITC,I) = 0.0D0
        TRTCMX(JBLN,ITC,I) = 0.0D0
        CALL  SIGARP  ( ISISET(I,JBLN),ITC,IGARP )
                    IF ( IGARP . EQ . 4 )        IGARP = 1
        J = I + 1
                    IF ( J . GT . NCAMSP )       J = 1
        DO 7030  WHILE ( J . NE . I )
        CALL  SIGARP  ( ISISET(J,JBLN),ITC,JGARP )
                    IF ( JGARP . EQ . 4 )        JGARP = 1
                    IF ( IGARP . NE . JGARP )    GO TO 7040
        TRTCMN(JBLN,ITC,I) = TRTCMN(JBLN,ITC,I) + TCAMSP(J)
        TRTCMX(JBLN,ITC,I) = TRTCMN(JBLN,ITC,I)
        J = J + 1
                    IF ( J . GT . NCAMSP )       J = 1
 7030   CONTINUE
 7040   CONTINUE
 7050   CONTINUE
 7060   CONTINUE
        WRITE (6,615)
        DO 7080  I   = 1      , NCAMSP
        WRITE (6,616) I,PTX2DL(ICAMPH(I)),TCAMSP(I),
     *                (JSISET(ISISET(I,JBLN)),JBLN=1,NIBL)
        DO 7070  ITC = ITURNU , ITURNR
        WRITE (6,617) IPTURN(ITC),(NINT(TRTCMN(JBLN,ITC,I)),JBLN=1,NIBL)
 7070   CONTINUE
 7080   CONTINUE
      END IF
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 7790 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      15'
      WRITE (ERRMSG,779) I,NLINE(1:2)
      CALL  PRTERR  ( 'STOP 779 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  779
 7800 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      15'
      WRITE (ERRMSG,780) I,NLINE(1:2)
      CALL  PRTERR  ( 'STOP 780 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  780
 8300 CONTINUE
      WRITE (ERRMSG,830) NCAMSP+2*NOLP,NCM
      CALL  PRTERR  ( 'STOP 830 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  830
 8310 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      16'
      WRITE (ERRMSG,831) I,PTX2DL(ICAMPH(I)),ICAMPH(I),MPHASE
      CALL  PRTERR  ( 'STOP 831 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  831
 8320 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      17'
      WRITE (ERRMSG,832) I,TCAMSP(I)
      CALL  PRTERR  ( 'STOP 832 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  832
 8330 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      18'
C[    IF ( J                  .EQ.-2147483647   )STOP 'RCAMSD J      01'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   06'
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      09'
C[    IF ( LANESS(K)          .EQ.'~'           )STOP 'RCAMSD LANESS 15'
      WRITE (ERRMSG,833) I,J,JBLN,LANESS(K)
      CALL  PRTERR  ( 'STOP 833 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  833
 8340 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      19'
C[    IF ( J                  .EQ.-2147483647   )STOP 'RCAMSD J      02'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   07'
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      10'
C[    IF ( LANESS(K+1)        .EQ.'~'           )STOP 'RCAMSD LANESS 16'
      WRITE (ERRMSG,834) I,J,JBLN,LANESS(K+1)
      CALL  PRTERR  ( 'STOP 834 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  834
 8350 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      20'
C[    IF ( J                  .EQ.-2147483647   )STOP 'RCAMSD J      03'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   08'
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      11'
C[    IF ( LANESS(K+2)        .EQ.'~'           )STOP 'RCAMSD LANESS 16'
      WRITE (ERRMSG,835) I,J,JBLN,LANESS(K+2)
      CALL  PRTERR  ( 'STOP 835 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  835
 8360 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      21'
C[    IF ( J                  .EQ.-2147483647   )STOP 'RCAMSD J      04'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   09'
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      12'
C[    IF ( LANESS(K  )        .EQ.'~'           )STOP 'RCAMSD LANESS 17'
C[    IF ( LANESS(K+1)        .EQ.'~'           )STOP 'RCAMSD LANESS 18'
C[    IF ( LANESS(K+2)        .EQ.'~'           )STOP 'RCAMSD LANESS 19'
      WRITE (ERRMSG,836) I,J,JBLN,LANESS(K),LANESS(K+1),LANESS(K+2)
      CALL  PRTERR  ( 'STOP 836 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  836
 8370 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      22'
C[    IF ( J                  .EQ.-2147483647   )STOP 'RCAMSD J      05'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   10'
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      13'
C[    IF ( LANESS(K+1)        .EQ.'~'           )STOP 'RCAMSD LANESS 20'
      WRITE (ERRMSG,837) I,J,JBLN,LANESS(K+1)
      CALL  PRTERR  ( 'STOP 837 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  837
 8380 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      23'
C[    IF ( J                  .EQ.-2147483647   )STOP 'RCAMSD J      06'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   11'
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      14'
C[    IF ( LANESS(K+1)        .EQ.'~'           )STOP 'RCAMSD LANESS 21'
C[    IF ( LANESS(K+2)        .EQ.'~'           )STOP 'RCAMSD LANESS 22'
      WRITE (ERRMSG,838) I,J,JBLN,LANESS(K+1),LANESS(K+2)
      CALL  PRTERR  ( 'STOP 838 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  838
 8390 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      24'
C[    IF ( J                  .EQ.-2147483647   )STOP 'RCAMSD J      07'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   12'
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      15'
C[    IF ( LANESS(K+1)        .EQ.'~'           )STOP 'RCAMSD LANESS 23'
      WRITE (ERRMSG,839) I,J,JBLN,LANESS(K+1)
      CALL  PRTERR  ( 'STOP 839 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  839
 8400 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      25'
C[    IF ( J                  .EQ.-2147483647   )STOP 'RCAMSD J      08'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   13'
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      16'
C[    IF ( LANESS(K+2)        .EQ.'~'           )STOP 'RCAMSD LANESS 24'
      WRITE (ERRMSG,840) I,J,JBLN,LANESS(K+2)
      CALL  PRTERR  ( 'STOP 840 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  840
 8410 CONTINUE
C[    IF ( J                  .EQ.-2147483647   )STOP 'RCAMSD J      09'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   14'
      WRITE (ERRMSG,841) J,JBLN
      CALL  PRTERR  ( 'STOP 841 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  841
 8420 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RCAMSD I      26'
C[    IF ( J                  .EQ.-2147483647   )STOP 'RCAMSD J      10'
C[    IF ( JBLN               .EQ.-2147483647   )STOP 'RCAMSD JBLN   15'
C[    IF ( K                  .EQ.-2147483647   )STOP 'RCAMSD K      17'
C[    IF ( LANESS(K  )        .EQ.'~'           )STOP 'RCAMSD LANESS 25'
C[    IF ( LANESS(K+1)        .EQ.'~'           )STOP 'RCAMSD LANESS 26'
C[    IF ( LANESS(K+2)        .EQ.'~'           )STOP 'RCAMSD LANESS 27'
      WRITE (ERRMSG,842) I,J,JBLN,LANESS(K),LANESS(K+1),LANESS(K+2)
      CALL  PRTERR  ( 'STOP 842 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  842
 8850 CONTINUE
      WRITE (ERRMSG,885) NOLP,MOV
      CALL  PRTERR  ( 'STOP 885 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RCAMSD'                             )
      STOP  885
      END                                                               RCAMSD
C
C
C
      SUBROUTINE RPHASD
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LOOPS'
      INCLUDE 'PHASES'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SIGCAM'
      INCLUDE 'TITLE'
      INCLUDE 'TXDSIG'
      INCLUDE 'USER'
      CHARACTER*1       IC
      CHARACTER*3       DENVOL
      CHARACTER*4       CHAR4
      CHARACTER*6       CHAR6
      CHARACTER*7       CHAR7
      CHARACTER*18      IMEMN
      CHARACTER*38      OPTNDL(NOP),OPTNTX(NOP),TIMRDL(NTM),TIMRTX(NTM)
      CHARACTER*50      MSG864
      LOGICAL           PHEADL
      INTEGER           I,ICAM1,IERR,IGROUP,ILNB,INUM,IOVDEF(NON),
     *                  IPHASE,IT1,IT2,ITEST,IUSED(NPN),J,JDEPH,JJ,JP,
     *                  JPHPS,JPP1,JPP2,JRING,K,KK,L,M,MCAM,MOV,MPHASE,
     *                  N,NCAM,NN,NVLDEN,OV,P
      DOUBLE PRECISION  PEDMIN,TEST
      DATA     TIMRDL / 'PHASES 1-5 CLEARANCE GREEN ------4-6-7',
     *                  'PHASES 2-8 ADVANCE GREEN ------3-4-6-7',
     *                  'PHASES 4-6 ADVANCE GREEN ------3-4-6-7',
     *                  'PHASE   4  TRANSFER GAP ---------4-6-7',
     *                  'PHASE   8  TRANSFER GAP ---------4-6-7',
     *                  'PHASES 2-6 ADVANCE GREEN MIN ------6--',
     *                  'PHASES 2-6 ADVANCE GREEN MAX ------6--',
     *                  'PHASES 4-8 ADVANCE GREEN ----------6--',
     *                  'PHASES 2-6 ADVANCE GREEN MIN --------7',
     *                  'PHASES 2-6 ADVANCE GREEN MAX --------7',
     *                  'PHASES 4-8 ADVANCE GREEN ------------7',
     *                  'PHASES 1-5 CLEARANCE GREEN ----3------'/
      DATA     TIMRTX / 'PHASES 3-5 CLEARANCE GREEN ------4-6-7',
     *                  'PHASES 1-7 ADVANCE GREEN ------3-4-6-7',
     *                  'PHASES 2-6 ADVANCE GREEN ------3-4-6-7',
     *                  'PHASE   2  TRANSFER GAP ---------4-6-7',
     *                  'PHASE   7  TRANSFER GAP ---------4-6-7',
     *                  'PHASES 1-6 ADVANCE GREEN MIN ------6--',
     *                  'PHASES 1-6 ADVANCE GREEN MAX ------6--',
     *                  'PHASES 2-7 ADVANCE GREEN ----------6--',
     *                  'PHASES 1-6 ADVANCE GREEN MIN --------7',
     *                  'PHASES 1-6 ADVANCE GREEN MAX --------7',
     *                  'PHASES 2-7 ADVANCE GREEN ------------7',
     *                  'PHASES 3-5 CLEARANCE GREEN ----3------'/
      DATA     OPTNDL / 'ENABLE D1  DURING PHASES 1-8 ---------',
     *                  'ENABLE D12 DURING PHASES 1-8 ---------',
     *                  'ENABLE D5  DURING PHASES 4-5 ---------',
     *                  'ENABLE D56 DURING PHASES 4-5 ---------',
     *                  'TERMINATE LOGIC FOR PHASES 4-8 -------',
     *                  'TERMINATE LOGIC FOR PHASES 4-8 -------',
     *                  'FIGURE 6 OPTION A (2-6 TIMING) -------',
     *                  'FIGURE 6 OPTION B (4-8 TIMING) -------',
     *                  'FIGURE 6 OPTION C (PHASE 6 SKIPPING) -',
     *                  'FIGURE 7 OPTION A (2-6 TIMING) -------',
     *                  'FIGURE 7 OPTION B (4-8 TIMING) -------',
     *                  'FIGURE 7 OPTION C (PHASE 2 SKIPPING) -'/
      DATA     OPTNTX / 'ENABLE D3  DURING PHASES 3-7 ---------',
     *                  'ENABLE D13 DURING PHASES 3-7 ---------',
     *                  'ENABLE D5  DURING PHASES 2-5 ---------',
     *                  'ENABLE D56 DURING PHASES 2-5 ---------',
     *                  'TERMINATE LOGIC FOR PHASES 2-7 -------',
     *                  'TERMINATE LOGIC FOR PHASES 2-7 -------',
     *                  'FIGURE 6 OPTION A (1-6 TIMING) -------',
     *                  'FIGURE 6 OPTION B (2-7 TIMING) -------',
     *                  'FIGURE 6 OPTION C (PHASE 6 SKIPPING) -',
     *                  'FIGURE 7 OPTION A (1-6 TIMING) -------',
     *                  'FIGURE 7 OPTION B (2-7 TIMING) -------',
     *                  'FIGURE 7 OPTION C (PHASE 1 SKIPPING) -'/
  501 FORMAT(20I4)
  502 FORMAT(I2,4F5.1,F6.1,5(1X,A3),2I4,7I2)
  503 FORMAT(I2,F3.0,F4.1,3F3.0,2F4.1,F3.1,2F3.0,I2,8A3,F4.1,3F3.0,F4.1,
     *       A7,I5,F6.2,40I2)
C-----       JP TMI  TVI  TM1   |     |    |     |  |   |    |     |
C-----       |  |  |    | TM2   |     |    |     |  |   |    |     |
C-----       |  |  |    | T2S   |     |    |     |  |   |    |     |
C-----       |  |  |    |       TCI   |    |     |  |   |    |     |
C-----       |  |  |    |       TAR   |    |     |  |   |    |     |
C-----       |  |  |    |             TRR  |     |  |   |    |     |
C-----       |  |  |    |                  TWK   |  |   |    |     |
C-----       |  |  |    |                  TPC   |  |   |    |     |
C-----       |  |  |    |                        IDEPH  |    |     |
C-----       |  |  |    |                           ISTO     |     |
C-----       |  |  |    |                           IMXR     |     |
C-----       |  |  |    |                           IMNR     |     |
C-----       |  |  |    |                           IPRCL    |     |
C-----       |  |  |    |                           IPRCY    |     |
C-----       |  |  |    |                           IMEM     |     |
C-----       |  |  |    |                           ICNDSV   |     |
C-----       |  |  |    |                           DENVOL   |     |
C-----       |  |  |    |                               TIIADD     |
C-----       |  |  |    |                                    TIIMAX|
C-----       |  |  |    |                                    TVITTR|
C-----       |  |  |    |                                    TVITBR|
C-----       |  |  |    |                                         TVIMIN
C-----       PEDDIS|    |
C-----          PEDVOL  |
C-----             PEDPAR
C-----                  J,(LLD(M    ,JP),M=1,J)
C-----                  K,(LLD(J+M  ,JP),M=1,K)
C-----                  L,(LLD(J+K+M,JP),M=1,L)
  504 FORMAT(I2,76X,
     *       A7,I5,F6.2,40I2)
C-----       JP |  |    |
C-----       PEDDIS|    |
C-----          PEDVOL  |
C-----             PEDPAR
C-----                  J,(LLD(M    ,JP),M=1,J)
C-----                  K,(LLD(J+M  ,JP),M=1,K)
C-----                  L,(LLD(J+K+M,JP),M=1,L)
  505 FORMAT(20I2)
  506 FORMAT(1X,A1,20I2)
  601 FORMAT(1X,A,//)
  602 FORMAT(11H A TOTAL OF,I3,14H SIGNAL PHASES)
  603 FORMAT(
     *54H DILEMMA ZONE BEGIN TIME (SECONDS) ----------------- =,F6.1,/,
     *54H DILEMMA ZONE END   TIME (SECONDS) ----------------- =,F6.1)
  604 FORMAT(
     *54H HARDWARE-IN-THE-LOOP SIGNAL SLEEP TIME (SECONDS) -- =,I4)
  605 FORMAT(
     *54H ENABLE SIMULTANEOUS GAP OUT FOR NEMA (YES/NO) ----- =,3X,A3)
  606 FORMAT(//,
     *45H SEMI-ACTUATED SIGNAL MAIN STREET INFORMATION,/,
     *58H MAIN STREET PHASE NUMBER -------------------------- =   1,/,
     *54H MAIN STREET MINIMUM GREEN INTERVAL (SECONDS) ------ =,F6.1,/,
     *54H MAIN STREET YELLOW CHANGE INTERVAL (SECONDS) ------ =,F6.1,/,
     *54H MAIN STREET RED CLEARANCE INTERVAL (SECONDS) ------ =,F6.1,/,
     *54H MAIN STREET NUMBER OF PHASES CLEARED TO ----------- =,I4,/,
     *54H MAIN STREET LIST OF PHASES CLEARED TO ------------- =,7I4)
  607 FORMAT(//,
     *54H SIGNAL PHASE NUMBER ------------------------------- =,I4,/,
     *54H INITIAL INTERVAL (SECONDS) ------------------------ =,F6.1,/,
     *54H VEHICLE INTERVAL (SECONDS) ------------------------ =,F6.1,/,
     *54H YELLOW CHANGE INTERVAL (SECONDS) ------------------ =,F6.1,/,
     *54H RED CLEARANCE INTERVAL (SECONDS) ------------------ =,F6.1,/,
     *54H MAXIMUM EXTENSION AFTER DEMAND ON RED (SECONDS) --- =,F6.1)
  608 FORMAT(
     *54H SKIP PHASE SWITCH POSITION (ON/OFF) --------------- =,3X,A3,/,
     *54H RECALL SWITCH POSITION (ON/OFF) ------------------- =,3X,A3,/,
     *54H MINOR MOVEMENT CONTROLLER OPTION (YES/NO) --------- =,3X,A3,/,
     *54H DUAL LEFTS TO BE FOLLOWED BY 2 SINGLE LEFTS (YES/NO)=,3X,A3)
  609 FORMAT(//,
     *54H SIGNAL PHASE NUMBER ------------------------------- =,I4,/,
     *54H WALK (SECONDS) ------------------------------------ =,F6.1,/,
     *54H PEDESTRIAN CLEARANCE (FLASHING DON'T WALK) (SECS) - =,F6.1,/,
     *54H ENABLE PEDESTRIAN RECALL (YES/NO) ----------------- =,3X,A3,/,
     *54H ENABLE PEDESTRIAN RECYCLE (YES/NO) ---------------- =,3X,A3)
  610 FORMAT(//,
     *54H SIGNAL PHASE NUMBER ------------------------------- =,I4,/,
     *54H MIN GREEN (SECONDS) ------------------------------- =,F6.1,/,
     *54H PASSAGE TIME (SECONDS) ---------------------------- =,F6.1,/,
     *54H MAXIMUM 1 (SECONDS) ------------------------------- =,F6.1,/,
     *54H MAXIMUM 2 (SECONDS) ------------------------------- =,F6.1,/,
     *54H TIME TO SWITCH FROM MAXIMUM 1 TO MAXIMUM 2 (MINUTES)=,F6.1,/,
     *54H YELLOW CHANGE (SECONDS) --------------------------- =,F6.1,/,
     *54H RED CLEARANCE (SECONDS) --------------------------- =,F6.1,/,
     *54H RED REVERT (SECONDS) ------------------------------ =,F6.1,/,
     *54H WALK (SECONDS) ------------------------------------ =,F6.1,/,
     *54H PEDESTRIAN CLEARANCE (FLASHING DON'T WALK) (SECS) - =,F6.1,/,
     *54H DUAL ENTRY PHASE NUMBER (0=SINGLE ENTRY PERMITTED)  =,I4,/,
     *54H PROVISION FOR STORING DEMAND (YES/NO) ------------- =,3X,A3,/,
     *54H ENABLE MAXIMUM RECALL (YES/NO) -------------------- =,3X,A3,/,
     *54H ENABLE MINIMUM RECALL (YES/NO) -------------------- =,3X,A3,/,
     *54H ENABLE PEDESTRIAN RECALL (YES/NO) ----------------- =,3X,A3,/,
     *54H ENABLE PEDESTRIAN RECYCLE (YES/NO) ---------------- =,3X,A3,/,
     *54H PLACE CALL ON MAXIMUM TIME OUT (YES/NO) ----------- =,3X,A3,A,
     *                                                                /,
     *54H ENABLE CONDITIONAL SERVICE (YES/NO) --------------- =,3X,A3)
  611 FORMAT(//,
     *54H SIGNAL PHASE NUMBER ------------------------------- =,I4)
  612 FORMAT(
     *54H ENABLE VOLUME DENSITY OPERATION (YES/NO) ---------- =,3X,A3)
  613 FORMAT(
     *54H VOLUME DENSITY ADDED INITIAL PER ACTUATION (SECONDS)=,F6.1,/,
     *54H VOLUME DENSITY MAXIMUM INITIAL (SECONDS) ---------- =,F6.1,/,
     *54H VOLUME DENSITY TIME TO REDUCE (SECONDS) ----------- =,F6.1,/,
     *54H VOLUME DENSITY TIME BEFORE REDUCTION (SECONDS) ---- =,F6.1,/,
     *54H VOLUME DENSITY MINIMUM GAP (SECONDS) -------------- =,F6.1)
  614 FORMAT(
     *54H PEDESTRIAN MOVEMENT HEADWAY FREQUENCY DISTRIBUTION  =,1X,A7,/,
     *54H PEDESTRIAN MOVEMENT VOLUME (PEDESTRIANS PER HOUR) - =,I4)
  615 FORMAT(
     *54H PEDESTRIAN MOVEMENT PARAMETER - NO PARAMETER ------  )
  616 FORMAT(
     *54H PEDESTRIAN MOVEMENT PARAMETER - INT OF MEAN**2/VAR  =,I4)
  617 FORMAT(
     *54H PEDESTRIAN MOVEMENT PARAMETER - MEAN**2/VARIANCE -- =,F7.2)
  618 FORMAT(
     *54H PEDESTRIAN MOVEMENT PARAMETER - STANDARD DEVIATION  =,F7.2)
  619 FORMAT(
     *54H PEDESTRIAN MOVEMENT PARAMETER - MIN HEADWAY (SECS)  =,F7.2)
  620 FORMAT(
     *54H NUMBER OF PHASES CLEARED TO ----------------------- =,I4,/,
     *54H LIST OF PHASES CLEARED TO ------------------------- =,7I4)
  621 FORMAT(
     *54H DETECTOR CONNECTION TYPE (AND/OR) ----------------- =,3X,A3)
  622 FORMAT(
     *54H NUMBER OF DETECTORS CONNECTED TO PHASE ------------ =,I4)
  623 FORMAT(
     *54H LIST OF CALL & EXTEND DETECTORS CONNECTED TO PHASE  =,7I4)
  624 FORMAT(
     *54H LIST OF CALL & EXTEND DETECTORS CONNECTED TO PHASE  =,5I4,/,
     *54X                                                      ,5I4)
  625 FORMAT(
     *54H LIST OF CALL ONLY     DETECTORS CONNECTED TO PHASE  =,7I4)
  626 FORMAT(
     *54H LIST OF CALL ONLY     DETECTORS CONNECTED TO PHASE  =,5I4,/,
     *54X                                                      ,5I4)
  627 FORMAT(
     *54H LIST OF EXTEND ONLY   DETECTORS CONNECTED TO PHASE  =,7I4)
  628 FORMAT(
     *54H LIST OF EXTEND ONLY   DETECTORS CONNECTED TO PHASE  =,5I4,/,
     *54X                                                      ,5I4)
  629 FORMAT(
     *54H LIST OF DETECTORS CONNECTED TO PHASE -------------- =,7I4)
  630 FORMAT(
     *54H LIST OF DETECTORS CONNECTED TO PHASE -------------- =,5I4,/,
     *54X                                                      ,5I4)
  631 FORMAT(/,
     *34H PHASE TIMING SET FOR ALL-RED REST)
C 632 FORMAT(1H )
  633 FORMAT(/,
     *27H INITIAL INTERVAL FOR PHASE,I3,8H RESET =,F6.1,
     *45H SECONDS SO THAT DUAL LEFT PHASE WOULD HAVE A,
     *25H MINIMUM GREEN INTERVAL =,F6.1,8H SECONDS)
  634 FORMAT(/,
     *36H YELLOW CHANGE INTERVAL FOR PHASE,I3,
     *8H RESET =,F6.1,32H SECONDS SO THAT DUAL LEFT PHASE,
     *29H WOULD HAVE THE MAXIMUM VALUE)
  635 FORMAT(/,
     *33H RED CLEARANCE INTERVAL FOR PHASE,I3,
     *8H RESET =,F6.1,32H SECONDS SO THAT DUAL LEFT PHASE,
     *29H WOULD HAVE THE MAXIMUM VALUE)
  636 FORMAT(/,
     *48H MAXIMUM EXTENSION AFTER DEMAND ON RED FOR PHASE,I3,
     *8H RESET =,F6.1,32H SECONDS SO THAT DUAL LEFT PHASE,
     *29H WOULD HAVE THE MINIMUM VALUE)
  637 FORMAT(/,
     *44H TEXAS DIAMOND CONTROLLER TIMERS:    FIGURES)
  638 FORMAT(1X,I2,3X,A,4H- = ,F4.1)
  639 FORMAT(/,
     *34H TEXAS DIAMOND CONTROLLER OPTIONS:)
  640 FORMAT(1X,I2,3X,A,4H- = ,A)
C-----THERE IS/ARE N RING/RINGS WITH N PHASE GROUP/GROUPS PER RING
  641 FORMAT(/,
     *6H THERE,A,I2,A,5H WITH,I2,6H PHASE,A,9H PER RING)
  642 FORMAT(///,11H A TOTAL OF,I3,9H OVERLAPS,/)
  643 FORMAT(/,
     *43H OVERLAP IDENTIFIER ------------------- =  ,A1)
  644 FORMAT(41H NUMBER OF PHASES IN DEFINITION LIST -- =,I3)
  645 FORMAT(41H LIST OF PHASES IN DEFINITION LIST ---- =,(10I3))
  646 FORMAT(
     *23H WARNING - SIGNAL PHASE,I3,17H WALK (SECONDS) =,F8.1,
     *57H WHEN PEDESTRIAN MOVEMENT VOLUME (PEDESTRIANS PER HOUR) =,I6)
  718 FORMAT(
     *13H SIGNAL PHASE,I3,
     *55H PEDESTRIAN MOVEMENT HEADWAY FREQUENCY DISTRIBUTION = (,A,1H)/,
     *58H IS NOT (CONSTAN), (ERLANG), (GAMMA), (LOGNRML), (NEGEXP),,
     *24H (SNEGEXP), OR (UNIFORM))
  719 FORMAT(
     *13H SIGNAL PHASE,I3,
     *52H PEDESTRIAN MOVEMENT VOLUME (PEDESTRIANS PER HOUR) =,I6,
     *8H IS LT 0)
  727 FORMAT(
     *13H SIGNAL PHASE,I3,26H DUAL ENTRY PHASE NUMBER =,I3,
     *14H IS LT 0 OR GT,I3)
  728 FORMAT(
     *13H SIGNAL PHASE,I3,
     *55H PEDESTRIAN CLEARANCE (FLASHING DON'T WALK) (SECONDS) =,F8.1,
     *6H IS LT,F4.1,12H OR GT 255.0)
  729 FORMAT(
     *13H SIGNAL PHASE,I3,17H WALK (SECONDS) =,F8.1,
     *6H IS LT,F4.1,12H OR GT 255.0)
  730 FORMAT(
     *13H SIGNAL PHASE,I3,23H RED REVERT (SECONDS) =,F8.1,
     *20H IS LT 2.0 OR GT 6.0)
  731 FORMAT(
     *13H SIGNAL PHASE,I3,
     *55H TIME TO SWITCH FROM MAXIMUM 1 TO MAXIMUM 2 (MINUTES) =,F8.1,
     *22H IS LT 0.0 OR GT 166.0)
  732 FORMAT(
     *13H SIGNAL PHASE,I3,22H MAXIMUM 2 (SECONDS) =,F8.1,
     *22H IS LT 1.0 OR GT 255.0)
  733 FORMAT(
     *13H SIGNAL PHASE,I3,22H MAXIMUM 1 (SECONDS) =,F8.1,
     *22H IS LT 1.0 OR GT 255.0)
  734 FORMAT(
     *13H SIGNAL PHASE,I3,22H MIN GREEN (SECONDS) =,F8.1,
     *7H IS LT ,F3.1,12H OR GT 255.0)
  771 FORMAT(
     *13H SIGNAL PHASE,I3,36H ENABLE VOLUME DENSITY OPERATION = (,
     *A3,23H) IS NOT (YES) OR (NO ))
  772 FORMAT(
     *13H SIGNAL PHASE,I3,
     *40H ENABLE VOLUME DENSITY OPERATION = (YES),
     *53H FOR NEMA CONTROLLER WITHOUT VOLUME DENSITY OPERATION)
  773 FORMAT(
     *13H SIGNAL PHASE,I3,
     *45H VOLUME DENSITY ADDED INITIAL PER ACTUATION =,F7.3,
     *21H IS LT 0.0 OR GT 25.5)
  774 FORMAT(
     *13H SIGNAL PHASE,I3,
     *33H VOLUME DENSITY MAXIMUM INITIAL =,F6.1,
     *6H IS LT,F6.1,12H OR GT 255.0)
  775 FORMAT(
     *13H SIGNAL PHASE,I3,
     *29H VOLUME DENSITY MINIMUM GAP =,F7.3,
     *16H IS LT 0.0 OR GT,F7.3)
  776 FORMAT(
     *13H SIGNAL PHASE,I3,
     *39H VOLUME DENSITY TIME BEFORE REDUCTION =,F6.1,
     *22H IS LT 1.0 OR GT 255.0)
  777 FORMAT(
     *13H SIGNAL PHASE,I3,
     *32H VOLUME DENSITY TIME TO REDUCE =,F6.1,
     *22H IS LT 1.0 OR GT 255.0)
  778 FORMAT(
     *55H ENABLE VOLUME DENSITY OPERATION = (NO ) FOR ALL PHASES,
     *50H FOR NEMA CONTROLLER WITH VOLUME DENSITY OPERATION)
  781 FORMAT(
     *6H PHASE,I3,47H AND ITS DUAL ENTRY PHASE ARE NOT IN SAME GROUP,
     *19H AND DIFFERENT RING)
  782 FORMAT(
     *6H PHASE,I3,
     *51H IS LISTED MORE THAN ONCE IN THE PREFERRED SEQUENCE)
  787 FORMAT(
     *46H NUMBER OF PHASES TO CLEAR TO FOR SIGNAL PHASE,I3,2H =,I3,
     *14H IS LT 1 OR GT,I3)
  843 FORMAT(
     *26H NUMBER OF SIGNAL PHASES =,I3,14H IS LT 2 OR GT,I3)
  844 FORMAT(
     *20H SIGNAL PHASE NUMBER,I3,2H =,I3,14H IS LT 1 OR GT,I3)
  845 FORMAT(
     *41H MORE THAN 1 SET OF DATA FOR SIGNAL PHASE,I3)
  846 FORMAT(
     *13H SIGNAL PHASE,I3,24H IS NOT IN THE CAM STACK)
  847 FORMAT(
     *13H SIGNAL PHASE,I3,27H YELLOW CHANGE (SECONDS) =,F8.1,
     *21H IS LT 3.0 OR GT 25.5)
  848 FORMAT(
     *13H SIGNAL PHASE,I3,26H RED CLEARANCE (SECONDS) =,F8.1,
     *21H IS LT 0.0 OR GT 25.5)
  849 FORMAT(
     *13H SIGNAL PHASE,I3,
     *40H MAXIMUM EXTENSION AFTER DEMAND ON RED =,F8.1,
     *22H IS LT 1.0 OR GT 255.0)
  850 FORMAT(
     *13H SIGNAL PHASE,I3,31H SKIP PHASE SWITCH POSITION = (,A3,
     *29H) IS NOT (ON ) (OFF) OR (   ))
  851 FORMAT(
     *13H SIGNAL PHASE,I3,27H RECALL SWITCH POSITION = (,A3,
     *29H) IS NOT (ON ) (OFF) OR (   ))
  852 FORMAT(
     *13H SIGNAL PHASE,I3,37H MINOR MOVEMENT CONTROLLER OPTION = (,A3,
     *29H) IS NOT (YES) (NO ) OR (   ))
  853 FORMAT(
     *13H SIGNAL PHASE,I3,
     *48H DUAL LEFTS TO BE FOLLOWED BY 2 SINGLE LEFTS = (,A3,
     *29H) IS NOT (YES) (NO ) OR (   ))
  854 FORMAT(
     *13H SIGNAL PHASE,I3,29H DETECTOR CONNECTION TYPE = (,A3,
     *29H) IS NOT (AND) (OR ) OR (   ))
  855 FORMAT(
     *13H SIGNAL PHASE,I3,32H NUMBER OF DETECTORS FOR PHASE =,I3,
     *14H IS LT 1 OR GT,I3)
  856 FORMAT(
     *13H SIGNAL PHASE,I3,33H IS ACTUATED BUT HAS NO DETECTORS,
     *39H AND THE RECALL SWITCH POSITION = (OFF))
  857 FORMAT(
     *13H SIGNAL PHASE,I3,36H RECALL SWITCH POSITION = (ON ) BUT,
     *27H NUMBER OF LOOP DETECTORS =,I3,8H IS NE 0)
  858 FORMAT(
     *13H SIGNAL PHASE,I3,16H DETECTOR NUMBER,I3,4H = 0)
  859 FORMAT(
     *13H SIGNAL PHASE,I3,35H POSITIVE CONNECTED DETECTOR IS NOT,
     *14H FIRST ON LIST)
  860 FORMAT(
     *13H SIGNAL PHASE,I3,30H NUMBER OF PHASES CLEARED TO =,I3,
     *16H IS LT 1 OR GT 7)
  861 FORMAT(
     *13H SIGNAL PHASE,I3,
     *60H DUAL LEFTS TO BE FOLLOWED BY 2 SINGLE LEFTS = (YES) BUT THE,
     *30H NUMBER OF PHASES CLEARED TO =,I3,8H IS LT 3)
  862 FORMAT(
     *13H SIGNAL PHASE,I3,24H CAN NOT CLEAR TO ITSELF)
  863 FORMAT(
     *13H SIGNAL PHASE,I3,19H PHASE CLEARED TO =,I3,
     *24H IS NOT IN THE CAM STACK)
  864 FORMAT(
     *13H SIGNAL PHASE,I3,35H NUMBER OF ENTRIES IN THE CAM STACK,
     *2H =,I3,A,I3)
  865 FORMAT(
     *13H SIGNAL PHASE,I3,
     *60H DUAL LEFTS TO BE FOLLOWED BY 2 SINGLE LEFTS = (YES) BUT THE,
     *25H FIRST PHASE CLEARED TO =,I3,7H IS NOT,I3)
  866 FORMAT(
     *13H SIGNAL PHASE,I3,
     *60H DUAL LEFTS TO BE FOLLOWED BY 2 SINGLE LEFTS = (YES) BUT THE,
     *26H SECOND PHASE CLEARED TO =,I3,7H IS NOT,I3)
  867 FORMAT(
     *13H SIGNAL PHASE,I3,2H =,I3,
     *35H IS IN THE CAM STACK FOR THE SIGNAL,
     *30H BUT NO OTHER DATA WAS ENTERED)
  868 FORMAT(
     *13H SIGNAL PHASE,I3,36H DID NOT HAVE THE ALL-RED REST PHASE,
     *52H AS THE LAST PHASE ON ITS LIST OF PHASES TO CLEAR TO)
  886 FORMAT(
     *24H OVERLAP IDENTIFIER IS (,A1,11H(   MUST BE,8(2H (,A,1H),:))
  887 FORMAT(
     *34H NUMBER OF PHASES ON THE OVERLAP (,A,
     *20H) DEFINITION LIST IS,I3,/,
     *43H MUST BE GE 0 AND LE THE NUMBER OF PHASES (,I3,1H))
  888 FORMAT(
     *25H A PHASE ON THE OVERLAP (,A,
     *46H) DEFINITION LIST IS NOT ON THE LIST OF PHASES)
  889 FORMAT(
     *46H ERROR IN TEXAS DIAMOND CONTROLLER TIMER LIST:,/,
     *7H TIMER(,I2,4H) = ,A)
  890 FORMAT(
     *47H ERROR IN TEXAS DIAMOND CONTROLLER OPTION LIST:,/,
     *8H OPTION(,I2,5H) = (,A,25H)  MUST BE (ON ) OR (OFF))
  897 FORMAT(
     *9H OVERLAP ,A,47H DOES NOT HAVE TWO CONSECUTIVE CAMSTACK ENTRIES)
  898 FORMAT(
     *13H SIGNAL PHASE,I3,1X,A,23H MUST BE (YES) OR (NO ))
  899 FORMAT(
     *16H A PHASE IN RING,I2,6H GROUP,I2,
     *29H IS NOT ON THE LIST OF PHASES)
C
C-----SUBROUTINE RPHASD READS THE SIGNAL PHASE INFORMATION FROM THE
C-----INPUT DIRECTLY TO THE SIMULATION PROCESSOR AND CHECKS FOR ERRORS
C
C[    DENVOL     = '~~~'
C[    DO  I = 1 , NPN
C[    IUSED (I)  = -2147483647
C[    END DO
C[    I          = -2147483647
C[    ICAM1      = -2147483647
C[    IERR       = -2147483647
C[    IGROUP     = -2147483647
C[    INUM       = -2147483647
C[    IPHASE     = -2147483647
C[    IRING      = -2147483647
C[    IT1        = -2147483647
C[    IT2        = -2147483647
C[    ITEST      = -2147483647
C[    J          = -2147483647
C[    JDEPH      = -2147483647
C[    JJ         = -2147483647
C[    JP         = -2147483647
C[    JPHPS      = -2147483647
C[    JPP1       = -2147483647
C[    JPP2       = -2147483647
C[    JRING      = -2147483647
C[    K          = -2147483647
C[    KK         = -2147483647
C[    M          = -2147483647
C[    MCAM       = -2147483647
C[    N          = -2147483647
C[    NCAM       = -2147483647
C[    NN         = -2147483647
C[    NVLDEN     = -2147483647
C[    OV         = -2147483647
C[    P          = -2147483647
C[    TEST       = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'RPHASD'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      ANYPED = .FALSE.
      NLOOPS = 0
      NVLDEN = 0
      NPEDS  = 0
      PHEADL = .FALSE.
      IF ( ICONTR . LT . ICNEMA )                THEN
C-----  PRETIMED, SEMI-ACT, FULL-ACT, AND TEX-DIA
        MPHASE = NPH
        MOV    = NOV
C-----  READ THE NUMBER OF SIGNAL PHASES
        READ (INPUT,501) NPHASE
      ELSE
C-----  NEMA AND HARDWARE
        IF ( ICONTR . LT . ICHDWR )              THEN
C-----    NEMA
          MPHASE = NPN
          MOV    = NON
        ELSE
C-----    HARDWARE
          MPHASE = HPH
          MOV    = HOV
        END IF
      END IF
      CALL  PHEADR  ( 6 )
      WRITE (6,601) STITLE
      WRITE (6,602) NPHASE
      IF ( (NPHASE.LT.2).OR.(NPHASE.GT.MPHASE) ) GO TO 8430
      IF ( ICONTR . GE . ICPSIG )                THEN
        IF ( .NOT. PHEADL )                      THEN
          WRITE (6,'(/)')
          PHEADL = .TRUE.
        END IF
        WRITE (6,603)  DZBTIM,DZETIM
        IF ( ICONTR .EQ. ICHDWR )                THEN
          WRITE (6,604)  HITLST
        END IF
      END IF
      IF ( ( ICONTR .GE. ICNEMA ) . AND . ( ICONTR .LE. ICNEMV ) )
     *                                           THEN
        IF ( .NOT. PHEADL )                      THEN
          WRITE (6,'(/)')
          PHEADL = .TRUE.
        END IF
        IF ( ESIMGO )                            THEN
          WRITE (6,605) IYES
        ELSE
          WRITE (6,605) INO//' '
        END IF
      END IF
      DO 1010  I = 1 , MPHASE
      IUSED(I) = 0
 1010 CONTINUE
C-----READ THE INFORMATION FOR EACH SIGNAL PHASE
      DO 2130  I = 1 , NPHASE
C-----READ THE SIGNAL PHASE INFORMATION
      IF ( ICONTR . LT . ICNEMA )                THEN
C-----  PRETIMED, SEMI-ACT, FULL-ACT, TEX-DIA, AND DAL-DIA
        READ (INPUT,502) JP,TII   (PDL2TX(JP)),TVI   (PDL2TX(JP)),
     *                      TCI   (PDL2TX(JP)),TAR   (PDL2TX(JP)),
     *                      TMX   (PDL2TX(JP)),
     *                      ISKP  (PDL2TX(JP)),IREC  (PDL2TX(JP)),
     *                      IMINOR(PDL2TX(JP)),IDUALL(PDL2TX(JP)),
     *                      IANDOR(PDL2TX(JP)),N,
     *                      NN,(LPHNXT(M,PDL2TX(JP)),M=1,NN)
C[      IF ( JP               .EQ.-2147483647   )STOP 'RPHASD JP     01'
        P = PDL2TX(JP)
        IF ( ( P.LT.1 ) . OR . ( P.GT.MPHASE ) ) GO TO 8440
C-----  SET THE DEFAULTS FOR THE SIGNAL PHASE INFORMATION
                    IF ( ISKP  (P) .EQ. IBLNK1 ) ISKP  (P) = IOFF
                    IF ( IREC  (P) .EQ. IBLNK1 ) IREC  (P) = IOFF
                    IF ( IMINOR(P) .EQ. IBLNK1 ) IMINOR(P) = INO
                    IF ( IDUALL(P) .EQ. IBLNK1 ) IDUALL(P) = INO
                    IF ( IANDOR(P) .EQ. IBLNK1 ) IANDOR(P) = JOR
        DENVOL = INO
        IPRCL (P) = INO
        TWK   (P) = 0.0D0
        PEDVOL(P) = 0
        PDEXCL(P) = .FALSE.
C[      IF ( NN               .EQ.-2147483647   )STOP 'RPHASD NN     01'
        IF ( (NN.LT.0).OR.(NN.GT.MPHASE-1) )     GO TO 7870
C-----  IF TEX-DIA OR DAL-DIA THEN GO TO 2010
        IF ( ( ICONTR .GE. ICTDF3 ) . AND . ( ICONTR .LE. ICDDF7 ) )
     *                                           GO TO 2010
        IF ( (NN.LT.1).OR.(NN.GT.MPHASE-1) )     GO TO 7870
      ELSE
C-----  NEMA AND HARDWARE
        IF ( ICONTR . LT . ICHDWR )           THEN
C-----    NEMA
          READ (INPUT,503) JP,TMI   (JP),TVI   (JP),TM1   (JP),
     *                        TM2   (JP),T2S   (JP),TCI   (JP),
     *                        TAR   (JP),TRR   (JP),TWK   (JP),
     *                        TPC   (JP),IDEPH (JP),ISTO  (JP),
     *                        IMXR  (JP),IMNR  (JP),IPRCL (JP),
     *                        IPRCY (JP),IMEM  (JP),ICNDSV(JP),
     *                        DENVOL    ,TIIADD(JP),TIIMAX(JP),
     *                        TVITTR(JP),TVITBR(JP),TVIMIN(JP),
     *                        PEDDIS(JP),PEDVOL(JP),PEDPAR(JP),
     *                        J,(LLD(M    ,JP),M=1,J),
     *                        K,(LLD(J+M  ,JP),M=1,K),
     *                        L,(LLD(J+K+M,JP),M=1,L)
        ELSE
C-----    HARDWARE
          READ (INPUT,504) JP,PEDDIS(JP),PEDVOL(JP),PEDPAR(JP),
     *                        J,(LLD(M    ,JP),M=1,J),
     *                        K,(LLD(J+M  ,JP),M=1,K),
     *                        L,(LLD(J+K+M,JP),M=1,L)
C-----    SET VALUES FOR HARDWARE-IN-THE-LOOP SIGNAL SO IT WILL PASS
C-----    TESTS
          TMI   (JP) = 0.0D0
          TVI   (JP) = 0.0D0
          TM1   (JP) = 0.0D0
          TM2   (JP) = 0.0D0
          T2S   (JP) = 0.0D0
          TCI   (JP) = 0.0D0
          TAR   (JP) = 0.0D0
          TRR   (JP) = 0.0D0
          TWK   (JP) = PEDVOL(JP)
          TPC   (JP) = 0.0D0
          IDEPH (JP) = 0
          ISTO  (JP) = INO
          IMXR  (JP) = INO
          IMNR  (JP) = INO
          IPRCL (JP) = INO
          IPRCY (JP) = INO
          IMEM  (JP) = INO
          ICNDSV(JP) = INO
          DENVOL     = INO
          TIIADD(JP) = 0.0D0
          TIIMAX(JP) = 0.0D0
          TVITTR(JP) = 0.0D0
          TVITBR(JP) = 0.0D0
          TVIMIN(JP) = 0.0D0
          TMX   (JP) = TM1(JP)
        END IF
C-----  JP     = Phase Number
C-----  TMI    = Min Green (seconds)
C-----  TVI    = Passage Time (seconds)
C-----  TM1    = Maximum 1 (seconds)
C-----  TM2    = Maximum 2 (seconds)
C-----  T2S    = Time to Switch from Maximum 1 to Maximum 2 (minutes)
C-----  TCI    = Yellow Change (seconds)
C-----  TAR    = Red Clearance (seconds)
C-----  TRR    = Red Revert (seconds)
C-----  TWK    = Walk (seconds)
C-----  TPC    = Pedestrian Clearance (seconds)
C-----  IDEPH  = Dual Entry Phase Number
C-----  ISTO   = Provision for Storing Demand
C-----  IMXR   = Enable Maximum Recall
C-----  IMNR   = Enable Minimum Recall
C-----  IPRCL  = Enable Pedestrian Recall
C-----  IPRCY  = Enable Pedestrian Recycle
C-----  IMEM   = Place Call on Maximum Time Out
C-----  ICNDSV = Enable Conditional Service
C-----  VOLDEN = Use Volume Density Options (DENVOL=YES/NO)
C-----  TIIADD = Volume Density Added Initial per Actuation (seconds)
C-----  TIIMAX = Volume Density Maximum Initial (seconds)
C-----  TVITTR = Volume Density Time to Reduce (seconds)
C-----  TVITBR = Volume Density Time Before Reduction (seconds)
C-----  TVIMIN = Volume Density Minimum Gap (seconds)
C-----  PEDDIS = Pedestrian Distribution Name
C-----  PEDVOL = Pedestrian Distribution Volume (peds per hour)
C-----  PEDPAR = Pedestrian Distribution Parameter
C-----  J,LLD  = Number and list of call and extend detectors (normal)
C-----  K,LLD  = Number and list of call            detectors
C-----  L,LLD  = Number and list of          extend detectors
C
C-----  LDEPH  = is phase a Dual Entry Phase
C-----           not used directly in SIMPRO
C[      IF ( JP               .EQ.-2147483647   )STOP 'RPHASD JP     01'
        P = JP
        IF ( ( P.LT.1 ) . OR . ( P.GT.MPHASE ) ) GO TO 8440
        IANDOR(P) = JOR
        PDEXCL(P) = .FALSE.
        NLDF  (P) = J
        NLDC  (P) = K
        NLDE  (P) = L
C-----  N - TOTAL NUMBER OF DETECTORS
        N = J + K + L
C[      IF ( N                .EQ.-2147483647   )STOP 'RPHASD N      01'
        IF ( ( N.LT.0 ) . OR . ( N.GT.NPL ) )    GO TO 8550
        IF ( ( ICONTR .GE. ICNEMA ) . AND . ( ICONTR .LE. ICNEMV ) )
     *                                           THEN
C-----    SET THE DEFAULTS FOR THE NEMA SIGNAL PHASE INFORMATION
                    IF ( ISTO  (P) .EQ. IBLNK1 ) ISTO  (P) = IYES
                    IF ( IMXR  (P) .EQ. IBLNK1 ) IMXR  (P) = INO
                    IF ( IMNR  (P) .EQ. IBLNK1 ) IMNR  (P) = INO
                    IF ( IPRCL (P) .EQ. IBLNK1 ) IPRCL (P) = INO
                    IF ( IPRCY (P) .EQ. IBLNK1 ) IPRCY (P) = INO
                    IF ( IMEM  (P) .EQ. IBLNK1 ) IMEM  (P) = IYES
                    IF ( ICNDSV(P) .EQ. IBLNK1 ) ICNDSV(P) = INO
C[          IF ( DENVOL       .EQ.'~~~'         )STOP 'RUSERD DENVOL 01'
                    IF ( DENVOL    .EQ. IBLNK1 ) DENVOL    = INO
          PDEXCL(P) = (TMI(P) . LE . 0.0D0)
          TMX(P) = TM1(P)
C-----    DETECTOR CALL AND EXTEND DATA
C-----    DETECTOR CONNECTED TO MORE THAN ONE PHASE GETS LAST VALUE
C-----    CALL AND EXTEND
          DO 1020  M = 1 , J
          LDCLEX(LLD(M,P)) = LDCNEX
 1020     CONTINUE
C-----    CALL
          DO 1030  M = 1 , K
          LDCLEX(LLD(J+M,P)) = LDCALL
 1030     CONTINUE
C-----    EXTEND
          DO 1040  M = 1 , L
          IF ( LDCLEX(LLD(J+K+M,P)) . EQ . 0 )   THEN
            LDCLEX(LLD(J+K+M,P)) = LDEXTN
          END IF
 1040     CONTINUE
        END IF
      END IF
                    IF ( ICONTR . GE . ICFACT )  GO TO 2010
                    IF ( P . NE . 1 )            GO TO 2010
C-----SET THE SIGNAL INFORMATION FOR MAIN STREET PHASE OF THE SEMI-
C-----ACTUATED SIGNAL
      TVI   (1) = 0.0D0
      TMX   (1) = 0.0D0
      ISKP  (1) = IOFF
      IREC  (1) = ION
      IMINOR(1) = INO
      IDUALL(1) = INO
      IANDOR(1) = JOR
      TMI   (1) = TII(1) + TVI(1)
      INUM = 3
C[    IF ( NN                 .EQ.-2147483647   )STOP 'RPHASD NN     02'
      WRITE (6,606) TII(1),TCI(1),TAR(1),NN,(LPHNXT(M,1),M=1,NN)
                                                 GO TO 2020
 2010 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     03'
      IF ( ICONTR . LT . ICNEMA )                THEN
        WRITE (6,607) JP,TII(P),TVI(P),TCI(P),TAR(P),TMX(P)
      END IF
      IF ( ICONTR . LE . ICFACT )                THEN
C[      IF ( NN               .EQ.-2147483647   )STOP 'RPHASD NN     03'
        WRITE (6,608) ISKP(P),IREC(P),
     *                IMINOR(P),IDUALL(P)
        INUM = 3
      END IF
      IF ( ( ICONTR .GE. ICTDF3 ) . AND . ( ICONTR .LE. ICDDF7 ) )
     *                                           THEN
        INUM = 5
        N = 0
        NN = 1
      END IF
      IF ( ( ICONTR .GE. ICNEMA ) . AND . ( ICONTR .LE. ICNEMV ) )
     *                                           THEN
        IF ( PDEXCL(P) )                         THEN
          WRITE (6,609) JP,TWK   (P),TPC   (P),IPRCL (P),IPRCY (P)
        ELSE
          IF ( IMEM(P) . EQ . INO )              THEN
            IMEM(P) = IYES
            IMEMN   = ' CHANGED BY SIMPRO'
          ELSE
            IMEMN   = '                  '
          END IF
          WRITE (6,610) JP,TMI   (P),TVI   (P),TM1   (P),TM2   (P),
     *                     T2S   (P),TCI   (P),TAR   (P),TRR   (P),
     *                     TWK   (P),TPC   (P),IDEPH (P),ISTO  (P),
     *                     IMXR  (P),IMNR  (P),IPRCL (P),IPRCY (P),
     *                     IMEM  (P),IMEMN    ,ICNDSV(P)
        END IF
        IF ( ICONTR . EQ . ICNEMA )              THEN
          INUM = 3
        ELSE
          INUM = 2
        END IF
        NN = 0
      END IF
      IF ( ICONTR .GE. ICHDWR )                  THEN
        WRITE (6,611) JP
        INUM = 3
        NN = 0
      END IF
      IF ( ICONTR . EQ . ICNEMV )                THEN
C[      IF ( DENVOL           .EQ.'~~~'         )STOP 'RUSERD DENVOL 02'
        WRITE (6,612) DENVOL
        IF ( DENVOL . EQ . IYES )                THEN
          WRITE (6,613) TIIADD(P),TIIMAX(P),TVITTR(P),TVITBR(P),
     *                  TVIMIN(P)
        END IF
      ELSE
        DENVOL = INO
      END IF
C-----CHECK AND INITIALIZE PEDESTRIAN DATA
      IF ( PEDVOL(P) . LT . 0 )                  GO TO 7190
C-----WARN THE USER IF WALK>0 AND PEDVOL=0 OR WALK=0 AND PEDVOL>0
      IF ( ( ( TWK   (P) . GT . 0.0D0 ) . AND .
     *       ( PEDVOL(P) . EQ . 0     ) ) . OR .
     *     ( ( TWK   (P) . EQ . 0.0D0 ) . AND .
     *       ( PEDVOL(P) . GT . 0     ) ) )      THEN
        WRITE (WRNMSG,646) JP,TWK(P),PEDVOL(P)
        WRITE (SER,FMT)
        WRITE (SER,FMT) WRNMSG(1:ILNB( WRNMSG ))
        CALL  PRTWRN  ( WRNMSG )
      END IF
      PDTRIP(P) = .FALSE.
      PDCALL(P) = .FALSE.
      PEDTMN(P) = 0.0D0
      PEDTIM(P) = 0.0D0
      IF ( ( TWK(P) . GT . 0.0D0        ) . AND .
     *     ( ( IPRCL (P) . EQ . IYES  ) . OR .
     *       ( PEDVOL(P) . GT . 0     ) ) )      THEN
          ANYPED  = .TRUE.
          PEDS(P) = .TRUE.
          NPEDS = NPEDS + 1
      END IF
      IF ( PEDVOL(P) . GT . 0 )                  THEN
        WRITE (6,614) PEDDIS(P),PEDVOL(P)
        PEDTMN(P) = 3600.0/DBLE( PEDVOL(P) )
        IF      ( PEDDIS(P) .EQ. "CONSTAN" )     THEN
C-----    PRINT CONSTANT PARAMETER
          WRITE (6,615)
        ELSE IF ( PEDDIS(P) .EQ. "ERLANG"  )     THEN
C-----    PRINT ERLANG PARAMETER
          WRITE (6,616) IDNINT( PEDPAR(P) )
        ELSE IF ( PEDDIS(P) .EQ. "GAMMA"   )     THEN
C-----    PRINT GAMMA PARAMETER
          WRITE (6,617) PEDPAR(P)
        ELSE IF ( PEDDIS(P) .EQ. "LOGNRML" )     THEN
C-----    PRINT LOG NORMAL PARAMETER
          WRITE (6,618) PEDPAR(P)
        ELSE IF ( PEDDIS(P) .EQ. "NEGEXP"  )     THEN
C-----    PRINT NEGATIVE EXPONENTIAL PARAMETER
          WRITE (6,615)
        ELSE IF ( PEDDIS(P) .EQ. "SNEGEXP" )     THEN
C-----    PRINT SHIFTED NEGATIVE EXPONENTIAL PARAMETER
          WRITE (6,619) PEDPAR(P)
        ELSE IF ( PEDDIS(P) .EQ. "UNIFORM" )     THEN
C-----    PRINT UNIFORM PARAMETER
          WRITE (6,618) PEDPAR(P)
        ELSE
          GO TO 7180
        END IF
      END IF
      IF ( ICONTR . LE .  ICFACT )               THEN
        IF ( NN . GT . 0 )                       THEN
          WRITE (6,620) NN,(LPHNXT(M,P),M=1,NN)
        END IF
      END IF
 2020 CONTINUE
C-----CHECK THE SIGNAL PHASE INFORMATION FOR ERRORS
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      04'
C[    IF ( IUSED(P)           .EQ.-2147483647   )STOP 'RPHASD IUSED  01'
                    IF ( IUSED(P) . NE . 0 )     GO TO 8450
      IUSED(P) = 1
C-----FIND THE FIRST CAM STACK POSITION WITH THIS SIGNAL PHASE NUMBER
      DO 2030  M = 1 , NCAMSP
                    IF ( P . EQ . ICAMPH(M) )    GO TO 2040
 2030 CONTINUE
      GO TO 8460
 2040 CONTINUE
C[    IF ( M                  .EQ.-2147483647   )STOP 'RPHASD M      01'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      05'
      ICAMPS(P) = M
C-----SET THE VALUES FOR SEVERAL OF THE SIGNAL PHASE PARAMETERS
      LPHASE(I) = P
      TII(P) = DMAX1( TII(P),0.0D0 )
      TVI(P) = DMAX1( TVI(P),DBLE( DT ) )
      IF       ( ICONTR . LT . ICNEMA )          THEN
        TMI(P) = TII(P) + TVI(P)
      ELSE IF  ( ICONTR . LT . ICHDWR )          THEN
        TII(P) = TMI(P) - TVI(P)
      END IF
C[    IF ( NN                 .EQ.-2147483647   )STOP 'RPHASD NN     04'
      NPHNXT(P) = NN
      IF ( ICONTR .LT. ICHDWR )                  THEN
C-----CHECK THE SIGNAL PHASE INFORMATION FOR ERRORS
        IF ( PEDS(P) )                           THEN
C-----PHASE HAS PEDESTRIANS TMI = 0 MEANS EXCLUSIVE PEDS
          IF ( ( TMI(P) . LT .   0.0D0 ) . OR .
     *         ( TMI(P) . GT . 255.0D0 ) )       GO TO 7340
        ELSE
          IF ( ( TMI(P) . LT .   1.0D0 ) . OR .
     *         ( TMI(P) . GT . 255.0D0 ) )       GO TO 7340
        END IF
        IF ( ( TCI(P) . LT .   3.0D0 ) . OR .
     *       ( TCI(P) . GT .  25.5D0 ) )         GO TO 8470
        IF ( ( TAR(P) . LT .   0.0D0 ) . OR .
     *       ( TAR(P) . GT .  25.5D0 ) )         GO TO 8480
        IF ( ( ( ICONTR . GT . ICSACT           ) . OR .
     *       ( P      . GT . 1                ) ) . AND .
     *     ( ( TMX(P) . LT .   1.0D0 ) . OR .
     *       ( TMX(P) . GT . 255.0D0 )        ) )GO TO 8490
        IF ( ICONTR . LE . ICFACT )              THEN
          IF ( ( ISKP  (P) . NE . ION  ) . AND .
     *         ( ISKP  (P) . NE . IOFF ) )       GO TO 8500
          IF ( ( IREC  (P) . NE . ION  ) . AND .
     *         ( IREC  (P) . NE . IOFF ) )       GO TO 8510
          IF ( ( IMINOR(P) . NE . IYES ) . AND .
     *         ( IMINOR(P) . NE .  INO ) )       GO TO 8520
          IF ( ( IDUALL(P) . NE . IYES ) . AND .
     *         ( IDUALL(P) . NE . INO  ) )       GO TO 8530
          IF ( ( IANDOR(P) . NE . JAND ) . AND .
     *         ( IANDOR(P) . NE . JOR  ) )       GO TO 8540
C[        IF ( N               EQ.-2147483647   )STOP 'RPHASD N      03'
          IF ( ( IREC  (P) . EQ . IOFF ) . AND .
     *         ( N         . EQ . 0    ) )       GO TO 8560
          IF ( ( IREC  (P) . EQ . ION  ) . AND .
     *         ( N         . NE . 0    ) )       GO TO 8570
        END IF
      END IF
      IF ( ( ICONTR .GE. ICNEMA ) . AND . ( ICONTR .LE. ICNEMV ) )
     *                                           THEN
        IF ( ( TM1   (P) . LT .   1.0D0 ) . OR .
     *       ( TM1   (P) . GT . 255.0D0 ) )      GO TO 7330
        IF ( ( TM2   (P) . LT .   1.0D0 ) . OR .
     *       ( TM2   (P) . GT . 255.0D0 ) )      GO TO 7320
        IF ( ( T2S   (P) . LT .   0.0D0 ) . OR .
     *       ( T2S   (P) . GT . 166.0D0 ) )      GO TO 7310
        IF ( ( TRR   (P) . LT .   2.0D0 ) . OR .
     *       ( TRR   (P) . GT .   6.0D0 ) )      GO TO 7300
        IF ( ( TWK   (P) . GT .   0.0D0) . OR .
     *       ( TPC   (P) . GT .   0.0D0) )       THEN
          PEDMIN = 1.0D0
        ELSE
          PEDMIN = 0.0D0
        END IF
        IF ( ( TWK   (P) . LT . PEDMIN  ) . OR .
     *       ( TWK   (P) . GT . 255.0D0 ) )      GO TO 7290
        IF ( ( TPC   (P) . LT . PEDMIN  ) . OR .
     *       ( TPC   (P) . GT . 255.0D0 ) )      GO TO 7280
        IF ( ( IDEPH (P) . LT . 0       ) . OR .
     *       ( IDEPH (P) . GT . NPHASE  ) )      GO TO 7270
        IF ( ( ISTO  (P) . NE . IYES    ) . AND .
     *       ( ISTO  (P) . NE . INO     ) )      THEN
          NLINE = 'ENABLE PROVISION FOR STORING A DEMAND' //
     *            ' = ('//ISTO(P)//')'
          GO TO 8980
        END IF
        IF ( ( IMXR  (P) . NE . IYES    ) . AND .
     *       ( IMXR  (P) . NE . INO     ) )      THEN
          NLINE = 'ENABLE PLACEMENT OF MAXIMUM RECALL' //
     *            ' = ('//IMXR(P)//')'
          GO TO 8980
        END IF
        IF ( ( IMNR  (P) . NE . IYES    ) . AND .
     *       ( IMNR  (P) . NE . INO     ) )      THEN
          NLINE = 'ENABLE PLACEMENT OF MINIMUM RECALL' //
     *            ' = ('//IMNR(P)//')'
          GO TO 8980
        END IF
        IF ( ( IPRCL (P) . NE . IYES    ) . AND .
     *       ( IPRCL (P) . NE . INO     ) )       THEN
          NLINE = 'ENABLE PEDESTRIAN RECALL'//
     *            ' = ('//IPRCL(P)//')'
          GO TO 8980
        END IF
        IF ( ( IPRCY (P) . NE . IYES    ) . AND .
     *       ( IPRCY (P) . NE . INO     ) )       THEN
          NLINE = 'ENABLE PEDESTRIAN RECYCLE'//
     *            ' = ('//IPRCY(P)//')'
          GO TO 8980
        END IF
        IF ( ( IMEM  (P) . NE . IYES    ) . AND .
     *       ( IMEM  (P) . NE . INO     ) )       THEN
          NLINE = 'ENABLE PLACEMENT OF CALL ON MAX-OUT'//
     *            ' = ('//IMEM(P)//')'
          GO TO 8980
        END IF
        IF ( ( ICNDSV(P) . NE . IYES    ) . AND .
     *       ( ICNDSV(P) . NE . INO     ) )       THEN
          NLINE = 'ENABLE CONDITIONAL SERVICE'//
     *            ' = ('//ICNDSV(P)//')'
          GO TO 8980
        END IF
      END IF
C[    IF ( DENVOL             .EQ.'~~~'         )STOP 'RUSERD DENVOL 03'
      IF ((DENVOL.NE.IYES). AND .(DENVOL.NE.INO))GO TO 7710
      IF ( DENVOL . EQ . IYES )                  THEN
        IF ( ICONTR . NE . ICNEMV )              GO TO 7720
C[      IF ( NVLDEN           .EQ.-2147483647   )STOP 'RPHASD NVLDEN 01'
        NVLDEN = NVLDEN + 1
        VOLDEN(P) = .TRUE.
        IF ( ( TIIADD(P) . LT .   0.0D0 ) . OR .
     *       ( TIIADD(P) . GT .  25.5D0 ) )      GO TO 7730
        IF ( ( TIIMAX(P) . LT . TMI(P)  ) . OR .
     *       ( TIIMAX(P) . GT . 255.0D0 ) )      GO TO 7740
        IF ( ( TVITTR(P) . LT .   1.0D0 ) . OR .
     *       ( TVITTR(P) . GT . 255.0D0 ) )      GO TO 7770
        IF ( ( TVITBR(P) . LT .   1.0D0 ) . OR .
     *       ( TVITBR(P) . GT . 255.0D0 ) )      GO TO 7760
        IF ( ( TVIMIN(P) . LT .   0.0D0 ) . OR .
     *       ( TVIMIN(P) . GT . TVI(P)  ) )      GO TO 7750
        TVISLP(P) = (TVI(P)-TVIMIN(P))/TVITTR(P)
      ELSE
        VOLDEN(P) = .FALSE.
        TIIADD(P) = 0.0D0
        TIIMAX(P) = TMI(P)
        TVIMIN(P) = TVI(P)
        TVITBR(P) = 0.0D0
        TVITTR(P) = 0.0D0
        TVISLP(P) = 0.0D0
      END IF
      TIIVEH(P) = 0.0D0
      TVIBEG(P) = TIMERR
      IF ( IDEPH(P) . GT . 0 )                   THEN
        LDEPH(IDEPH(P)) = .TRUE.
      END IF
C[    IF ( N                  .EQ.-2147483647   )STOP 'RPHASD N      04'
                    IF ( N . EQ .  0 )           GO TO 2060
      IF ( (N.LT.1) . OR . (N.GT.NPL) )          GO TO 8550
C-----READ THE LIST OF DETECTORS FOR THIS SIGNAL PHASE
      IF ( ICONTR . LE . ICFACT )                THEN
        READ (INPUT,501) (LLD(M,P),M=1,N)
      END IF
      NLD(P) = N
      IF ( N . GT . 0 )                          THEN
        IF ( ICONTR . LT . ICNEMA )              THEN
          WRITE (6,621) IANDOR(P)
        END IF
        WRITE (6,622) N
        IF ( ( ICONTR .GE. ICNEMA ) . AND . ( ICONTR .LE. ICHDWR ) )
     *                                           THEN
          IF ( NLDF(P) . GT . 0 )                THEN
            IF ( NLDF(P) . LE . 7 )              THEN
              WRITE (6,623) (LLD(M,P),M=1,NLDF(P))
            ELSE
              WRITE (6,624) (LLD(M,P),M=1,NLDF(P))
            END IF
          END IF
          IF ( NLDC(P) . GT . 0 )                THEN
            IF ( NLDC(P) . LE . 7 )              THEN
              WRITE (6,625) (LLD(NLDF(P)+M,P),M=1,NLDC(P))
            ELSE
              WRITE (6,626) (LLD(NLDF(P)+M,P),M=1,NLDC(P))
            END IF
          END IF
          IF ( NLDE(P) . GT . 0 )                THEN
            IF ( NLDE(P) . LE . 7 )              THEN
              WRITE (6,627) (LLD(NLDF(P)+NLDC(P)+M,P),M=1,NLDE(P))
            ELSE
              WRITE (6,628) (LLD(NLDF(P)+NLDC(P)+M,P),M=1,NLDE(P))
            END IF
          END IF
        ELSE
          IF ( N . LE . 7 )                      THEN
            WRITE (6,629) (LLD(M,P),M=1,N)
          ELSE
            WRITE (6,630) (LLD(M,P),M=1,N)
          END IF
        END IF 
      END IF
      NLOOPS = NLOOPS + N
      ITEST = 0
      DO 2050  M = 1 , N
                    IF ( LLD(M,P).EQ.0 )         GO TO 8580
                    IF ( LLD(M,P).GT.0 )         ITEST = 1
 2050 CONTINUE
      IF ((ITEST            .EQ.1). AND .
     *    (LLD(1,P).LT.0))                       GO TO 8590
                    IF ( ITEST . EQ . 1 )        GO TO 2060
C-----SET THE SIGNAL INFORMATION FOR ALL-RED REST PHASE OF THE FULL-
C-----ACTUATED SIGNAL
      IARRPH = P
      TII   (P) = 0.0D0
      TVI   (P) = DT
      TCI   (P) = 0.0D0
      TAR   (P) = 0.0D0
      TMX   (P) = 0.0D0
      ISKP  (P) = ION
      IREC  (P) = IOFF
      IMINOR(P) = INO
      IDUALL(P) = INO
      IANDOR(P) = JAND
      TMI   (P) = TII(P) + TVI(P)
      WRITE (6,631)
 2060 CONTINUE
      IF ( ( ICONTR .GE. ICTDF3 ) . AND . ( ICONTR .LE. ICHDWR ) )
     *                                           GO TO 2090
C[    IF ( NN                 .EQ.-2147483647   )STOP 'RPHASD NN     05'
                    IF ( NN . LT . 1 )           GO TO 8600
                    IF ( NN . GT . 7 )           GO TO 8600
      IF ( (IDUALL(P).EQ.IYES). AND . (NN.LT.3) )GO TO 8610
C-----CHECK TO MAKE SURE THAT THIS PHASE NUMBER IS NOT ON ITS OWN LIST
C-----OF PHASES THAT IT CAN CLEAR TO AND THAT EACH PHASE THAT IT CAN
C-----CLEAR TO IS IN THE CAM STACK
      DO 2080  M = 1 , NN
                    IF ( P . EQ . LPHNXT(M,P) )  GO TO 8620
      DO 2070  KK = 1 , NCAMSP
            IF ( LPHNXT(M,P) . EQ . ICAMPH(KK) ) GO TO 2080
 2070 CONTINUE
      GO TO 8630
 2080 CONTINUE
 2090 CONTINUE
      IT1 = ICAMPS(P)
C-----CHECK TO MAKE SURE THAT THERE IS A CAM STACK POSITION FOR THE
C-----GREEN INTERVAL, THE YELLOW CHANGE INTERVAL FOR EACH PHASE THAT
C-----THIS PHASE CAN CLEAR TO, AND THE RED CLEARANCE INTERVAL (IF TAR(P)
C-----GT 0.0, NOT FOR NEMA CONTROLLER) FOR THIS PHASE
C[    IF ( IT1                .EQ.-2147483647   )STOP 'RPHASD IT1    01'
      DO 2100  M = IT1 , NCAMSP
                    IF ( ICAMPH(M) . NE . P )    GO TO 2110
 2100 CONTINUE
      M = NCAMSP + 1
 2110 CONTINUE
C[    IF ( IT1                .EQ.-2147483647   )STOP 'RPHASD IT1    02'
C[    IF ( M                  .EQ.-2147483647   )STOP 'RPHASD M      02'
      NCAM = M - IT1
      MCAM = 1 + NPHNXT(P)
                    IF ( TAR(P) . GT . 0.0D0 )   MCAM = MCAM + 1
      IF ( ICONTR . LT . ICNEMA )                THEN
        IF ( NCAM . NE . MCAM )                  THEN
          MSG864 = ' IS NE 1+(NUMBER OF PHASES CLEARED TO)+(ALL-RED) ='
          GO TO 8640
        END IF
      ELSE
        IF ( NCAM . NE . 2 )                     THEN
          MCAM=2
          MSG864 = ' IS NE'
          GO TO 8640
        END IF
      END IF
                    IF ( ICONTR . GE . ICTDF3 )  GO TO 2120
                    IF ( IDUALL(P) . NE . IYES ) GO TO 2120
C-----CHECK TO MAKE SURE THAT THE FIRST PHASE THAT THIS PHASE CAN CLEAR
C-----TO IS (THIS PHASE NUMBER+1) WHEN THE DUAL LEFT OPTION = (ON )
      JPP1 = P + 1
                    IF ( LPHNXT(1,P) .NE. JPP1 ) GO TO 8650
C-----CHECK TO MAKE SURE THAT THE SECOND PHASE THAT THIS PHASE CAN CLEAR
C-----TO IS (THIS PHASE NUMBER+2) WHEN THE DUAL LEFT OPTION = (ON )
      JPP2 = P + 2
                    IF ( LPHNXT(2,P) .NE. JPP2 ) GO TO 8660
 2120 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RPHASD I      01'
C[    IF ( INUM               .EQ.-2147483647   )STOP 'RPHASD INUM   01'
      IF ( (((I/INUM)*INUM).EQ.I     ) .AND.
     *     (I              .NE.NPHASE) )         THEN
C       WRITE (6,632)
        WRITE (6,FMT) CHAR( 12 )
      END IF
C-----END OF SIGNAL PHASE LOOP
 2130 CONTINUE
C-----CHECK TO MAKE SURE THAT DATA WAS ENTERED FOR EACH SIGNAL PHASE IN
C-----THE CAM STACK
      DO 3020  I = 1 , NCAMSP
      DO 3010  M = 1 , NPHASE
            IF ( ICAMPH(I) . EQ . LPHASE(M) )    GO TO 3020
 3010 CONTINUE
      GO TO 8670
 3020 CONTINUE
                    IF ( ICONTR . GE . ICTDF3 )  GO TO 3070
C-----CHECK EACH SIGNAL PHASE FOR DUAL LEFT OPTION
      DO 3060  I = 1 , NPHASE
      JP = LPHASE(I)
                    IF ( IDUALL(JP) . NE . IYES )GO TO 3060
      JPP1 = JP + 1
      JPP2 = JP + 2
C-----CHECK TO MAKE SURE THAT THE MINIMUM GREEN INTERVAL (TII+TVI) FOR
C-----THE DUAL LEFT PHASE IS EQ TO THE MINIMUM OF THE MINIMUM GREEN
C-----INTERVAL FOR THE FIRST 2 PHASES THAT THIS PHASE CAN CLEAR TO
      TEST = TII(JP)
      TII(JP) = DMIN1( TII(JPP1)+TVI(JPP1),TII(JPP2)+TVI(JPP2) )-TVI(JP)
                    IF ( TEST . EQ . TII(JP) )   GO TO 3030
      TEST = TII(JP) + TVI(JP)
      WRITE (6,633) PTX2DL(JP),TII(JP),TEST
 3030 CONTINUE
C-----CHECK TO MAKE SURE THAT THE YELLOW CHANGE INTERVAL FOR THE DUAL
C-----LEFT PHASE IS EQ TO THE MAXIMUM OF THE YELLOW CHANGE INTERVAL
C-----FOR THE FIRST 2 PHASES THAT THIS PHASE CAN CLEAR TO
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     09'
      TEST = TCI(JP)
C[    IF ( JPP1               .EQ.-2147483647   )STOP 'RPHASD JPP1   01'
C[    IF ( JPP2               .EQ.-2147483647   )STOP 'RPHASD JPP2   01'
      TCI(JP) = DMAX1( TCI(JPP1),TCI(JPP2) )
                    IF ( TEST . EQ . TCI(JP) )   GO TO 3040
      WRITE (6,634) PTX2DL(JP),TCI(JP)
 3040 CONTINUE
C-----CHECK TO MAKE SURE THAT THE RED CLEARANCE INTERVAL FOR THE DUAL
C-----LEFT PHASE IS EQ TO THE MAXIMUM OF THE RED CLEARANCE INTERVAL FOR
C-----THE FIRST 2 PHASES THAT THIS PHASE CAN CLEAR TO
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     10'
      TEST = TAR(JP)
C[    IF ( JPP1               .EQ.-2147483647   )STOP 'RPHASD JPP1   02'
C[    IF ( JPP2               .EQ.-2147483647   )STOP 'RPHASD JPP2   02'
      TAR(JP) = DMAX1( TAR(JPP1),TAR(JPP2) )
                    IF ( TEST . EQ . TAR(JP) )   GO TO 3050
      WRITE (6,635) PTX2DL(JP),TAR(JP)
 3050 CONTINUE
C-----CHECK TO MAKE SURE THAT THE MAXIMUM EXTENSION AFTER DEMAND ON RED
C-----FOR THE DUAL LEFT PHASE IS EQ TO THE MINIMUM OF THE MAXIMUM
C-----EXTENSION AFTER DEMAND ON RED FOR THE FIRST 2 PHASES THAT THIS
C-----PHASE CAN CLEAR TO
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     11'
      TEST = TMX(JP)
C[    IF ( JPP1               .EQ.-2147483647   )STOP 'RPHASD JPP1   03'
C[    IF ( JPP2               .EQ.-2147483647   )STOP 'RPHASD JPP2   03'
      TMX(JP) = DMIN1( TMX(JPP1),TMX(JPP2) )
                    IF ( TEST . EQ . TMX(JP) )   GO TO 3060
      WRITE (6,636) PTX2DL(JP),TMX(JP)
C-----END OF DUAL LEFT PHASE LOOP
 3060 CONTINUE
 3070 CONTINUE
      IF ( ICONTR . GE . ICTDF3 )                THEN
        IF ( ICONTR . LE . ICDDF7 )              NLOOPS = 10
        ICAMPC = NCM + 1 + 2*MOV
        ICAMPO = ICAMPC + 1
      ELSE
C-----  INITIALIZE THE SIGNAL SETTINGS FOR THE ACTUATED SIGNAL
        ICPHAS = LPHASE(1)
        ICAMPC = ICAMPS(ICPHAS)
        NOLDF(2) = 2
        ICAMPO = NCAMSP
        TP = 0.0D0
        TR = TII(ICPHAS) + TVI(ICPHAS)
        TRLAST(1) = TR
      END IF
      IF ( ( ICONTR .GE. ICTDF3 ) . AND . ( ICONTR .LE. ICDDF7 ) )
     *                                           THEN
        CALL  PHEADR  ( 6 )
        WRITE (6,601) STITLE
        READ (INPUT,FMT) NLINE
        IT2 = 0
        WRITE (6,637)
        DO 3080  I = 1 , NTM
C[      IF ( IT2              .EQ.-2147483647   )STOP 'RPHASD IT2    01'
        IT1 = IT2 + 1
        IERR = I
        IF ( (I.EQ.4) .OR. (I.EQ.5) )            THEN
C[        IF ( IT2            .EQ.-2147483647   )STOP 'RPHASD IT2    02'
          IT2 = IT2 + 3
C[        IF ( I              .EQ.-2147483647   )STOP 'RPHASD I      02'
C[        IF ( IT1            .EQ.-2147483647   )STOP 'RPHASD IT1    03'
          READ (NLINE(IT1:IT2),'(F3.1)',ERR=8890) TMRSET(I)
        ELSE
C[        IF ( IT2            .EQ.-2147483647   )STOP 'RPHASD IT2    03'
          IT2 = IT2 + 4
C[        IF ( I              .EQ.-2147483647   )STOP 'RPHASD I      03'
C[        IF ( IT1            .EQ.-2147483647   )STOP 'RPHASD IT1    04'
          READ (NLINE(IT1:IT2),'(F4.1)',ERR=8890) TMRSET(I)
        END IF
C[      IF ( I                .EQ.-2147483647   )STOP 'RPHASD I      04'
        IF ( TMRSET(I) . LT . 0.0D0 )            GO TO 8890
        IF ( ( ICONTR .GE. ICTDF3 ) . AND . ( ICONTR .LE. ICTDF7 ) )
     *                                           THEN
          WRITE (6,638) I,TIMRTX(I),TMRSET(I)
        END IF
        IF ( ( ICONTR .GE. ICDDF3 ) . AND . ( ICONTR .LE. ICDDF7 ) )
     *                                           THEN
          WRITE (6,638) I,TIMRDL(I),TMRSET(I)
        END IF
 3080   CONTINUE
        READ (INPUT,FMT) NLINE
        IT2 = 0
        WRITE (6,639)
        DO 3090  I = 1 , NOP
C[      IF ( IT2              .EQ.-2147483647   )STOP 'RPHASD IT2    04'
        IT1 = IT2 + 1
        IT2 = IT2 + 3
        IF ( NLINE(IT1:IT2) . EQ . ION )         THEN
C[      IF ( I                .EQ.-2147483647   )STOP 'RPHASD I      05'
          OPTN(I) = .TRUE.
          IF ( ( ICONTR .GE. ICTDF3 ) . AND . ( ICONTR .LE. ICTDF7 ) )
     *                                           THEN
            WRITE (6,640) I,OPTNTX(I),ION
          END IF
          IF ( ( ICONTR .GE. ICDDF3 ) . AND . ( ICONTR .LE. ICDDF7 ) )
     *                                           THEN
            WRITE (6,640) I,OPTNDL(I),ION
          END IF
          GO TO 3090
        END IF
C[      IF ( IT1              .EQ.-2147483647   )STOP 'RPHASD IT1    05'
C[      IF ( IT2              .EQ.-2147483647   )STOP 'RPHASD IT2    05'
        IF ( NLINE(IT1:IT2) . EQ . IOFF )        THEN
C[        IF ( I              .EQ.-2147483647   )STOP 'RPHASD I      06'
          OPTN(I) = .FALSE.
          IF ( ( ICONTR .GE. ICTDF3 ) . AND . ( ICONTR .LE. ICTDF7 ) )
     *                                           THEN
            WRITE (6,640) I,OPTNTX(I),IOFF
          END IF
          IF ( ( ICONTR .GE. ICDDF3 ) . AND . ( ICONTR .LE. ICDDF7 ) )
     *                                           THEN
            WRITE (6,640) I,OPTNDL(I),IOFF
          END IF
          GO TO 3090
        END IF
C[      IF ( I                .EQ.-2147483647   )STOP 'RPHASD I      07'
        IERR = I
        GO TO 8900
 3090   CONTINUE
      END IF
      IF ( ( ICONTR .GE. ICNEMA ) . AND . ( ICONTR .LE. ICNEMV ) )
     *                                           THEN
        CALL  PHEADR  ( 6 )
        WRITE (6,601) STITLE
C-----  THERE IS/ARE N RING/RINGS WITH N PHASE GROUP/GROUPS PER RING
        IF ( NRING . EQ . 1 )                    THEN
          CHAR4 = ' IS'
          CHAR6 = ' RING'
        ELSE
          CHAR4 = ' ARE'
          CHAR6 = ' RINGS'
        END IF
        IF ( NGROUP . EQ . 1 )                   THEN
          CHAR7 = ' GROUP'
        ELSE
          CHAR7 = ' GROUPS'
        END IF
        WRITE (6,641) CHAR4(1:ILNB( CHAR4 )),NRING,
     *                CHAR6(1:ILNB( CHAR6 )),NGROUP,
     *                CHAR7(1:ILNB( CHAR7 ))
C-----  READ THE PREFERRED SEQUENCE OF PHASES IN EACH GROUP OF EACH RING
        DO 4030  IRING = 1 , NRING
        DO 4020  IGROUP = 1 , NGROUP
        READ (INPUT,505) (LPHPS(IRING,IGROUP,I),I=1,NPHASE)
        DO 4010  I = NPHASE , 1 , -1
        IF ( LPHPS(IRING,IGROUP,I) . GT . 0 )    THEN
          NPHPS(IRING,IGROUP) = I
          GO TO 4020
        END IF
 4010   CONTINUE
 4020   CONTINUE
 4030   CONTINUE
        WRITE (6,FMT)
        CALL  PRINRG  ( NPHPS,LPHPS,NRING,NGROUP,NRG,NGR,' ',I,-6 )
        DO 4040  I = 1 , MPHASE
        IUSED(I) = 0
 4040   CONTINUE
        DO 4090  IRING = 1 , NRING
        DO 4080  IGROUP = 1 , NGROUP
        DO 4070  I = 1 , NPHPS(IRING,IGROUP)
        JPHPS = LPHPS(IRING,IGROUP,I)
        DO 4050  M = 1 , NPHASE
                  IF ( JPHPS . EQ . LPHASE(M) )  GO TO 4060
 4050   CONTINUE
        GO TO 8990
 4060   CONTINUE
C[      IF ( JPHPS            .EQ.-2147483647   )STOP 'RPHASD JPHPS  01'
        IF ( IUSED(JPHPS) . NE . 0 )             GO TO 7820
        IUSED(JPHPS) = 1
        PHRNG(JPHPS) = IRING
        PHGRP(JPHPS) = IGROUP
 4070   CONTINUE
 4080   CONTINUE
 4090   CONTINUE
C-----  NUMBER AND LIST OF PHASES THAT CAN RUN CONCURRENTLY
        DO 4096  M = 1 , NPHASE
        DO 4093  I = 1 , NPHASE
        IF ( I .EQ. M)                           GO TO 4093
        IF ( ( PHGRP(LPHASE(M)) . EQ . PHGRP(LPHASE(I) ) ) . AND .
     *       ( PHRNG(LPHASE(M)) . NE . PHRNG(LPHASE(I) ) ) )
     *                                           THEN
          KK = NPHRC(M) + 1 
          NPHRC(   M) = KK
          LPHRC(KK,M) = LPHASE(I)
        END IF
 4093   CONTINUE
 4096   CONTINUE
        DO 4170  M = 1 , NPHASE
        IPHASE = LPHASE(M)
        DO 4120  IRING = 1 , NRING
        DO 4110  IGROUP = 1 , NGROUP
        DO 4100  KK = 1 , NPHPS(IRING,IGROUP)
        IF ( IPHASE.EQ.LPHPS(IRING,IGROUP,KK) )  GO TO 4130
 4100   CONTINUE
 4110   CONTINUE
 4120   CONTINUE
C-----  PHASE IS NOT IN THE PREFERRED SEQUENCE
        GO TO 4170
 4130   CONTINUE
C-----  FOUND PHASE IN THE PREFERRED SEQUENCE
C[      IF ( IPHASE           .EQ.-2147483647   )STOP 'RPHASD IPHASE 01'
        JDEPH = IDEPH(IPHASE)
        IF ( JDEPH . GT . 0 )                    THEN
          DO 4150  JRING = 1 , NRING
C[        IF ( IRING          .EQ.-2147483647   )STOP 'RPHASD IRING  01'
          IF ( JRING . EQ . IRING )              GO TO 4150
C[        IF ( IGROUP         .EQ.-2147483647   )STOP 'RPHASD IGROUP 01'
          DO 4140  KK = 1 , NPHPS(JRING,IGROUP)
          IF ( JDEPH.EQ.LPHPS(JRING,IGROUP,KK) ) GO TO 4160
 4140     CONTINUE
 4150     CONTINUE
          GO TO 7810
        END IF
 4160   CONTINUE
 4170   CONTINUE
        IF ( NOLP . GT . 0 )                     THEN
C-----    READ THE OVERLAP DEFINITIONS
          WRITE (6,642) NOLP
          DO 5010  I = 1 , MOV
          IOVDEF(I) = 0
 5010     CONTINUE
          ICAM1 = ICHAR( 'A' ) - 1
          DO 5100  M = 1 , NOLP
          READ (INPUT,506) IC,(IOVDEF(I),I=1,MOV)
          WRITE (6,643) IC
          JJ = ICHAR( IC ) - ICAM1
          IF ( (JJ.LT.1) . OR . (JJ.GT.MOV) )    GO TO 8860
          DO 5020  I = 1 , 2*NOLP , 2
          IF ( ICAMPH(NCAMSP+I) . EQ . JJ )      THEN
            IF ( ICAMPH(NCAMSP+I+1) . NE . JJ )  GO TO 8970
            LOLP(M) = NCAMSP + I
            GO TO 5030
          END IF
 5020     CONTINUE
          GO TO 8970
 5030     CONTINUE
          DO 5040  I = MOV , 1, -1
          IF ( IOVDEF(I) . NE . 0 )              THEN
            OV = I
            GO TO 5050
          END IF
 5040     CONTINUE
          OV = 0
 5050     CONTINUE
C[        IF ( OV             .EQ.-2147483647   )STOP 'RPHASD OV     01'
          WRITE (6,644) OV
          IF ( (OV.LT.0) . OR . (OV.GT.NPHASE) ) GO TO 8870
          NOLDF(M) = OV
          WRITE (6,645) (IOVDEF(I),I=1,OV)
          DO 5080  I = 1 , OV
          IT1 = IOVDEF(I)
          DO 5060  IPHASE = 1 , NPHASE
                    IF ( IT1.EQ.LPHASE(IPHASE) ) GO TO 5070
 5060     CONTINUE
          GO TO 8880
 5070     CONTINUE
 5080     CONTINUE
C[        IF ( OV             .EQ.-2147483647   )STOP 'RPHASD OV     02'
          DO 5090  I = 1 , OV
          LOLDF(I,M) = IOVDEF(I)
 5090     CONTINUE
 5100     CONTINUE
        END IF
      END IF
      IF ( ICONTR . LE . ICFACT )                THEN
        IF ( IARRPH . EQ . 0 )                   GO TO 6020
        DO 6010  IPHASE = 1 , NPHASE
        IF ( IPHASE . EQ . IARRPH )              GO TO 6010
        N = NPHNXT(IPHASE)
        IF ( LPHNXT(N,IPHASE) . NE . IARRPH )    GO TO 8680
 6010   CONTINUE
 6020   CONTINUE
      END IF
C[    IF ( NVLDEN             .EQ.-2147483647   )STOP 'RPHASD NVLDEN 02'
      IF ((ICONTR.EQ.ICNEMV).AND.(NVLDEN.EQ.0))  GO TO 7780
      RETURN
C-----PROCESS THE INPUT ERRORS AND STOP
 7180 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,718) JP,PEDDIS(P)
      CALL  PRTERR  ( 'STOP 718 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  718
 7190 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,719) JP,PEDVOL(P)
      CALL  PRTERR  ( 'STOP 719 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  719
 7270 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,727) JP,IDEPH(P),NPHASE
      CALL  PRTERR  ( 'STOP 727 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  727
 7280 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,728) JP,TPC(P),PEDMIN
      CALL  PRTERR  ( 'STOP 728 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  728
 7290 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,729) JP,TWK(P),PEDMIN
      CALL  PRTERR  ( 'STOP 729 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  729
 7300 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,730) JP,TRR(P)
      CALL  PRTERR  ( 'STOP 730 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  730
 7310 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,731) JP,T2S(P)
      CALL  PRTERR  ( 'STOP 731 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  731
 7320 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,732) JP,TM2(P)
      CALL  PRTERR  ( 'STOP 732 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  732
 7330 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,733) JP,TM1(P)
      CALL  PRTERR  ( 'STOP 733 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  733
 7340 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      IF ( PEDS(P) )                             THEN
        WRITE (ERRMSG,734) JP,TMI(P),0.0
      ELSE
        WRITE (ERRMSG,734) JP,TMI(P),1.0
      END IF
      CALL  PRTERR  ( 'STOP 734 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  734
 7710 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( DENVOL             .EQ.'~~~'         )STOP 'RUSERD DENVOL 04'
      WRITE (ERRMSG,771) JP,DENVOL
      CALL  PRTERR  ( 'STOP 771 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  771
 7720 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
      WRITE (ERRMSG,772) JP
      CALL  PRTERR  ( 'STOP 772 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  772
 7730 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,773) JP,TIIADD(P)
      CALL  PRTERR  ( 'STOP 773 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  773
 7740 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,774) JP,TIIMAX(P),TMI(P)
      CALL  PRTERR  ( 'STOP 774 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  774
 7750 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,775) JP,TVIMIN(P),TVI(P)
      CALL  PRTERR  ( 'STOP 775 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  775
 7760 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,776) JP,TVITBR(P)
      CALL  PRTERR  ( 'STOP 776 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  776
 7770 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,777) JP,TVITTR(P)
      CALL  PRTERR  ( 'STOP 777 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  777
 7780 CONTINUE
      WRITE (ERRMSG,778)
      CALL  PRTERR  ( 'STOP 778 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  778
 7810 CONTINUE
C[    IF ( JDEPH              .EQ.-2147483647   )STOP 'RPHASD JDEPH  01'
      WRITE (ERRMSG,781) JDEPH
      CALL  PRTERR  ( 'STOP 781 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  781
 7820 CONTINUE
C[    IF ( JPHPS              .EQ.-2147483647   )STOP 'RPHASD JPHPS  02'
      WRITE (ERRMSG,782) JPHPS
      CALL  PRTERR  ( 'STOP 782 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  782
 7870 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
C[    IF ( NN                 .EQ.-2147483647   )STOP 'RPHASD NN     06'
      WRITE (ERRMSG,787) JP,NN,MPHASE-1
      CALL  PRTERR  ( 'STOP 787 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  787
 8430 CONTINUE
      WRITE (ERRMSG,843) NPHASE,MPHASE
      CALL  PRTERR  ( 'STOP 843 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  843
 8440 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     13'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,844) JP,P,MPHASE
      CALL  PRTERR  ( 'STOP 844 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  844
 8450 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     14'
      WRITE (ERRMSG,845) JP
      CALL  PRTERR  ( 'STOP 845 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  845
 8460 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     15'
      WRITE (ERRMSG,846) JP
      CALL  PRTERR  ( 'STOP 846 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  846
 8470 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     16'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,847) JP,TCI(P)
      CALL  PRTERR  ( 'STOP 847 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  847
 8480 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     17'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,848) JP,TAR(P)
      CALL  PRTERR  ( 'STOP 848 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  848
 8490 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     18'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,849) JP,TMX(P)
      CALL  PRTERR  ( 'STOP 849 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  849
 8500 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     19'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,850) JP,ISKP(P)
      CALL  PRTERR  ( 'STOP 850 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  850
 8510 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     20'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,851) JP,IREC(P)
      CALL  PRTERR  ( 'STOP 851 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  851
 8520 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     21'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,852) JP,IMINOR(P)
      CALL  PRTERR  ( 'STOP 852 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  852
 8530 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     22'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,853) JP,IDUALL(P)
      CALL  PRTERR  ( 'STOP 853 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  853
 8540 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     23'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,854) JP,IANDOR(P)
      CALL  PRTERR  ( 'STOP 854 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  854
 8550 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     24'
C[    IF ( N                  .EQ.-2147483647   )STOP 'RPHASD N      05'
      WRITE (ERRMSG,855) JP,N,NPL
      CALL  PRTERR  ( 'STOP 855 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  855
 8560 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     25'
      WRITE (ERRMSG,856) JP
      CALL  PRTERR  ( 'STOP 856 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  856
 8570 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     26'
C[    IF ( N                  .EQ.-2147483647   )STOP 'RPHASD N      06'
      WRITE (ERRMSG,857) JP,N
      CALL  PRTERR  ( 'STOP 857 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  857
 8580 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     27'
C[    IF ( N                  .EQ.-2147483647   )STOP 'RPHASD N      07'
      WRITE (ERRMSG,858) JP,N
      CALL  PRTERR  ( 'STOP 858 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  858
 8590 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     28'
      WRITE (ERRMSG,859) JP
      CALL  PRTERR  ( 'STOP 859 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  859
 8600 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     29'
C[    IF ( NN                 .EQ.-2147483647   )STOP 'RPHASD NN     07'
      WRITE (ERRMSG,860) JP,NN
      CALL  PRTERR  ( 'STOP 860 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  860
 8610 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     30'
C[    IF ( NN                 .EQ.-2147483647   )STOP 'RPHASD NN     08'
      WRITE (ERRMSG,861) JP,NN
      CALL  PRTERR  ( 'STOP 861 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  861
 8620 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     31'
      WRITE (ERRMSG,862) JP
      CALL  PRTERR  ( 'STOP 862 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  862
 8630 CONTINUE
C[    IF ( M                  .EQ.-2147483647   )STOP 'RPHASD M      03'
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     32'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,863) JP,LPHNXT(M,P)
      CALL  PRTERR  ( 'STOP 863 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  863
 8640 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     33'
C[    IF ( MCAM               .EQ.-2147483647   )STOP 'RPHASD MCAM   01'
C[    IF ( NCAM               .EQ.-2147483647   )STOP 'RPHASD NCAM   01'
      WRITE (ERRMSG,864) JP,NCAM,MSG864(1:ILNB( MSG864 )),MCAM
      CALL  PRTERR  ( 'STOP 864 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  864
 8650 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     34'
C[    IF ( JPP1               .EQ.-2147483647   )STOP 'RPHASD JPP1   04'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,865) JP,LPHNXT(1,P),JPP1
      CALL  PRTERR  ( 'STOP 865 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  865
 8660 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     35'
C[    IF ( JPP2               .EQ.-2147483647   )STOP 'RPHASD JPP2   04'
C[    IF ( P                  .EQ.-2147483647   )STOP 'RPHASD P      08'
      WRITE (ERRMSG,866) JP,LPHNXT(2,P),JPP2
      CALL  PRTERR  ( 'STOP 866 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  866
 8670 CONTINUE
      WRITE (ERRMSG,867) PTX2DL(ICAMPH(I)),ICAMPH(I)
      CALL  PRTERR  ( 'STOP 867 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  867
 8680 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RPHASD I      08'
      WRITE (ERRMSG,868) I
      CALL  PRTERR  ( 'STOP 868 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  868
 8860 CONTINUE
C[    IF ( ICAM1              .EQ.-2147483647   )STOP 'RPHASD ICAM1  01'
C[    IF ( JJ                 .EQ.-2147483647   )STOP 'RPHASD JJ     01'
      WRITE (ERRMSG,886) CHAR( ICAM1+JJ ),
     *                   (CHAR( I ),I=ICAM1+1,ICAM1+MOV)
      CALL  PRTERR  ( 'STOP 886 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  886
 8870 CONTINUE
C[    IF ( ICAM1              .EQ.-2147483647   )STOP 'RPHASD ICAM1  02'
C[    IF ( JJ                 .EQ.-2147483647   )STOP 'RPHASD JJ     02'
      WRITE (ERRMSG,887) CHAR( ICAM1+JJ ),OV,NPHASE
      CALL  PRTERR  ( 'STOP 887 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  887
 8880 CONTINUE
C[    IF ( ICAM1              .EQ.-2147483647   )STOP 'RPHASD ICAM1  03'
C[    IF ( JJ                 .EQ.-2147483647   )STOP 'RPHASD JJ     03'
      WRITE (ERRMSG,888) CHAR( ICAM1+JJ )
      CALL  PRTERR  ( 'STOP 888 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  888
 8890 CONTINUE
C[    IF ( IERR               .EQ.-2147483647   )STOP 'RPHASD IERR   01'
C[    IF ( IT1                .EQ.-2147483647   )STOP 'RPHASD IT1    06'
C[    IF ( IT2                .EQ.-2147483647   )STOP 'RPHASD IT2    06'
      WRITE (ERRMSG,889) IERR,NLINE(IT1:IT2)
      CALL  PRTERR  ( 'STOP 889 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  889
 8900 CONTINUE
C[    IF ( IERR               .EQ.-2147483647   )STOP 'RPHASD IERR   02'
C[    IF ( IT1                .EQ.-2147483647   )STOP 'RPHASD IT1    07'
C[    IF ( IT2                .EQ.-2147483647   )STOP 'RPHASD IT2    07'
      WRITE (ERRMSG,890) IERR,NLINE(IT1:IT2)
      CALL  PRTERR  ( 'STOP 890 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  890
 8970 CONTINUE
C[    IF ( ICAM1              .EQ.-2147483647   )STOP 'RPHASD ICAM1  04'
C[    IF ( JJ                 .EQ.-2147483647   )STOP 'RPHASD JJ     04'
      WRITE (ERRMSG,897) CHAR (ICAM1+JJ)
      CALL  PRTERR  ( 'STOP 897 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  897
 8980 CONTINUE
C[    IF ( JP                 .EQ.-2147483647   )STOP 'RPHASD JP     12'
      WRITE (ERRMSG,898) JP,NLINE(1:ILNB( NLINE ))
      CALL  PRTERR  ( 'STOP 898 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  898
 8990 CONTINUE
C[    IF ( IRING              .EQ.-2147483647   )STOP 'RPHASD IRING  02'
C[    IF ( IGROUP             .EQ.-2147483647   )STOP 'RPHASD IGROUP 02'
      WRITE (ERRMSG,899) IRING,IGROUP
      CALL  PRTERR  ( 'STOP 899 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RPHASD'                             )
      STOP  899
      END                                                               RPHASD
C
C
C
      SUBROUTINE RLOOPD
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
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
      INCLUDE 'TITLE'
      INCLUDE 'USER'
      INTEGER           I,ID,II,ILDLN,ILNB,IUSED(NLS),J,JJ,JL,K,MGEOM3,
     *                  MGEOM4,MLANES,N
      LOGICAL           LDOK
  501 FORMAT(20I4)
C 502 FORMAT(I2,1X,2A,1X,10I4)
C 502 FORMAT(I2,1X,A,1X,11I4)
  502 FORMAT(I2,1X,A,1X,13I4)
  503 FORMAT(5(A8,2I4))
  601 FORMAT(1X,A,//)
  602 FORMAT(11H A TOTAL OF,I3,10H DETECTORS)
  603 FORMAT(//,
     *       31H DETECTOR NUMBER ------------ =,I5,/,
     *       31H DETECTOR TYPE -------------- =,1X,A,/,
     *       31H STARTING POSITION (FEET) --- =,I5,/,
     *       31H STOPPING POSITION (FEET) --- =,I5,/,
     *       31H APPROACH NUMBER ------------ =,I5,/,
     *       31H NUMBER OF LANES ------------ =,I5,:,/,
     *       31H LIST OF LANE NUMBERS ------- =,6I5)
  604 FORMAT(31H DETECTOR DELAY (SECONDS) --- =,I5,/,
     *       31H DETECTOR EXTEND (SECONDS) -- =,F7.1)
  605 FORMAT(31H NUMBER OF VEHICLE CLASSES -- =,I5)
  606 FORMAT(20H DETECTOR CLASS NAME,I3,10H ----- = (,A8,
     *       11H) LENGTH GT,I4,7H AND LE,I4)
  735 FORMAT(
     *9H DETECTOR,I3,14H, BOTH DELAY (,I5,14H) AND EXTEND (,F7.1,
     *19H) CAN NOT BE GT 0.0)
  742 FORMAT(
     *14H VEHICLE CLASS,I3,1X,A8,13H MIN LENGTH =,I4,
     *19H IS NE MAX LENGTH =,I4,27H FOR PREVIOUS VEHICLE CLASS)
  743 FORMAT(
     *14H VEHICLE CLASS,I3,1X,A8,13H MIN LENGTH =,I4,
     *19H IS GE MAX LENGTH =,I4)
  744 FORMAT(
     *14H VEHICLE CLASS,I3,1X,A8,13H MAX LENGTH =,I4,10H IS NE 999)
  745 FORMAT(
     *14H VEHICLE CLASS,I3,1X,A8,13H MIN LENGTH =,I4,8H IS NE 0)
  746 FORMAT(
     *54H NUMBER OF VEHICLE CLASSES FOR CLASSIFICATION DETECTOR,I3,2H =,
     *I3,14H IS LT 1 OR GT,I3)
  785 FORMAT(
     *34H NUMBER OF LANES FOR LOOP DETECTOR,I3,2H =,I3,
     *14H IS LT 1 OR GT,I2)
  786 FORMAT(
     *23H LOOP DETECTOR NUMBER =,I3,14H IS LT 1 OR GT,I3)
  869 FORMAT(
     *22H NUMBER OF DETECTORS =,I3,14H IS LT 1 OR GT,I3)
  870 FORMAT(
     *18H DETECTOR NUMBER =,I3,14H IS LT 1 OR GT,I3)
  871 FORMAT(
     *37H MORE THAN 1 SET OF DATA FOR DETECTOR,I3)
  872 FORMAT(
     *9H DETECTOR,I3,18H DETECTOR TYPE = (,A,
     *58H) IS NOT (CLASSIFY), (PRESENCE), (PULSE   ), OR (        ))
  873 FORMAT(
     *9H DETECTOR,I3,20H STARTING POSITION =,I5,8H IS LT 0)
  874 FORMAT(
     *9H DETECTOR,I3,20H STOPPING POSITION =,I5,
     *26H IS LT STARTING POSITION =,I5)
  875 FORMAT(
     *9H DETECTOR,I3,18H APPROACH NUMBER =,I3,
     *37H IS NOT ON LIST OF INBOUND APPROACHES)
  876 FORMAT(
     *9H DETECTOR,I3,25H NUMBER OF LANE NUMBERS =,I4,
     *14H IS LT 1 OR GT,I3)
  877 FORMAT(
     *9H DETECTOR,I3,14H LANE NUMBER =,I4,
     *43H IS LT 1 OR GT NUMBER OF LANES FOR APPROACH,I3,2H =,I2)
  878 FORMAT(
     *9H APPROACH,I3,29H NUMBER OF DETECTORS FOR LANE,I2,2H =,I2,
     *7H IS GT ,I1)
  879 FORMAT(
     *9H DETECTOR,I3,9H APPROACH,I3,5H LANE,I2,
     *37H IS NOT AVAILABLE AT THE INTERSECTION)
  880 FORMAT(
     *9H DETECTOR,I3,20H STOPPING POSITION =,I5,
     *31H IS GT END OF LANE FOR APPROACH,I3,5H LANE,I2,2H =,I5)
  881 FORMAT(
     *9H DETECTOR,I3,34H IS ON LIST OF DETECTORS FOR PHASE,I2,
     *30H BUT NO OTHER DATA WAS ENTERED)
  882 FORMAT(
     *9H DETECTOR,I3,36H DATA WAS ENTERED BUT DID NOT APPEAR,
     *51H ON THE LIST OF DETECTORS FOR ANY PHASE AS POSITIVE)
C
C-----SUBROUTINE RLOOPD READS THE DETECTOR INFORMATION FROM THE INPUT
C-----DIRECTLY TO THE SIMULATION PROCESSOR AND CHECKS FOR ERRORS
C
C[    DO  I = 1 , NLS
C[    IUSED (I)  = -2147483647
C[    END DO
C[    I          = -2147483647
C[    ID         = -2147483647
C[    II         = -2147483647
C[    ILDLN      = -2147483647
C[    J          = -2147483647
C[    JJ         = -2147483647
C[    JL         = -2147483647
C[    K          = -2147483647
C[    MGEOM3     = -2147483647
C[    MGEOM4     = -2147483647
C[    MLANES     = -2147483647
C[    N          = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'RLOOPD'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----READ THE NUMBER OF DETECTORS
      READ (INPUT,501) NLOOPS
      CALL  PHEADR  ( 6 )
      WRITE (6,601) STITLE
      WRITE (6,602) NLOOPS
      IF ( (NLOOPS.LT.1) . OR . (NLOOPS.GT.NLS) )GO TO 8690
      DO 1010  I = 1 , NLS
      IUSED(I) = 0
 1010 CONTINUE
C-----READ THE INFORMATION FOR EACH DETECTOR
      DO 2030  I = 1 , NLOOPS
C-----READ THE DETECTOR INFORMATION
      READ (INPUT,502) ID,ITYPLD(ID),     LDSTRT(ID),LDSTOP(ID),LDA(ID),
     *                 NLDLN(ID),(LLDLN(K,ID),K=1,NLDLN(ID)),DETCLK(ID),
     *                 J,K
      IF ( ( ICONTR .GE. ICNEMA ) . AND . ( ICONTR .LE. ICHDWR ) )
     *                                           THEN
        LDDELY(ID) = DBLE( J )
        LDEXTD(ID) = DBLE( K ) / 10.0D0
      ELSE
        LDDELY(ID) = 0.0D0
        LDEXTD(ID) = 0.0D0
      END IF
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     01'
      IF ( ( ID.LT.1 ) . OR . ( ID.GT.NLS ) )    GO TO 7860
      IF ( ( NLDLN(ID) . LT . 1  ) . OR .
     *     ( NLDLN(ID) . GT . NAL) )             GO TO 7850
C-----SET THE DEFAULTS FOR THE DETECTOR INFORMATION
                    IF ( ITYPLD(ID).NE.IBLNK1 )  GO TO 2010
      ITYPLD(ID) = IPRES
 2010 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     02'
      IF ( DIAMON )                              THEN
        IF ( ( (LDA(ID).EQ.3) .AND. (NFUT.EQ.1.OR.NFUT.EQ.3) ) . OR .
     *       ( (LDA(ID).EQ.8) .AND. (NFUT.EQ.2.OR.NFUT.EQ.3) ) )
     *                                           THEN
          DO 2015  K = 1 , NLDLN(ID)
          LLDLN(K,ID) = LLDLN(K,ID) + 1
 2015     CONTINUE
        END IF
      END IF
      WRITE (6,603) ID,ITYPLD(ID),     LDSTRT(ID),LDSTOP(ID),LDA(ID),
     *              NLDLN(ID),(LLDLN(K,ID),K=1,NLDLN(ID))
      IF ( ( ICONTR .GE. ICNEMA ) . AND . ( ICONTR .LE. ICHDWR ) )
     *                                           THEN
        WRITE (6,604) NINT( LDDELY(ID) ),LDEXTD(ID)
      END IF
      IF ( ITYPLD(ID) . EQ . ICLAS )             THEN
        WRITE (6,605) DETCLK(ID)
      END IF
C-----CHECK THE DETECTOR INFORMATION FOR ERRORS
                    IF ( ID . LT .   1 )         GO TO 8700
                    IF ( ID . GT . NLS )         GO TO 8700
C[    IF ( IUSED(ID)          .EQ.-2147483647   )STOP 'RLOOPD IUSED  01'
                    IF ( IUSED(ID) . NE . 0 )    GO TO 8710
      IUSED(ID) = 1
      IF ( ( ITYPLD(ID) . NE . ICLAS ) . AND .
     *     ( ITYPLD(ID) . NE . IPULS ) . AND .
     *     ( ITYPLD(ID) . NE . IPRES ) )
     *                                           GO TO 8720
            IF ( LDSTRT(ID) . LT . 0          )  GO TO 8730
            IF ( LDSTOP(ID) . LT . LDSTRT(ID) )  GO TO 8740
      IF ( ( ITYPLD(ID) . EQ . ICLAS ) . AND .
     *     ( NLDLN (ID) . GT . 1     ) )         GO TO 7850
      IF ( ( LDDELY(ID) . NE . 0.0D0 ) . AND . 
     *     ( LDEXTD(ID) . NE . 0.0D0 ) )         GO TO 7350
      STRTLD(ID) = LDSTRT(ID)
      STOPLD(ID) = LDSTOP(ID)
C[    IF ( I                  .EQ.-2147483647   )STOP 'RLOOPD I      01'
      LLOOPS(I) = ID
      LDCROS(ID) = .FALSE.
      LDTRIP(ID) = .FALSE.
      LDCLER(ID) = .FALSE.
      VDCNT (ID) = .FALSE.
C-----CHECK TO MAKE SURE THAT THE DETECTOR APPROACH NUMBER IS ON THE
C-----LIST OF INBOUND APPROACHES
      IF ( ( NLDLN(ID)      . GT . 0 ) . AND .
     *     ( LIBAR(LDA(ID)) . LE . 0 ) )         GO TO 8750
      IF ( ( ICONTR .LT. ICTDF3 ) .AND. ( NLDLN(ID) .LT. 1 ) )
     *                                           GO TO 8760
                    IF ( NLDLN(ID) . GT . NAL )  GO TO 8760
      MLANES = NLANES(LDA(ID))
C-----PROCESS EACH LANE THAT THE DETECTOR OCCUPIES
      DO 2020  K = 1 , NLDLN(ID)
      ILDLN = LLDLN(K,ID)
                    IF ( ILDLN . LT . 1 )        GO TO 8770
                    IF ( ILDLN . GT . MLANES )   GO TO 8770
      JL = LLANES(ILDLN,LDA(ID))
C-----ADD THE DETECTOR FOR LANE JL
      NLDL(JL) = NLDL(JL) + 1
                    IF ( NLDL(JL) . GT . NLO )   GO TO 8780
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     03'
      LLDL(NLDL(JL),JL) = ID
      MGEOM3 = LGEOM(3,JL)
      MGEOM4 = LGEOM(4,JL)
                    IF ( MGEOM3     .EQ. MGEOM4 )GO TO 8790
                    IF ( LDSTOP(ID) .GT. MGEOM4 )GO TO 8800
C-----END OF LANE LOOP
 2020 CONTINUE
      IF ( ITYPLD(ID) . EQ . ICLAS )             THEN
        IF ( ( DETCLK(ID) . LT . 1   ) . OR .
     *       ( DETCLK(ID) . GT . LDC ) )         GO TO 7460
        READ (INPUT,503) (DETCLN(J,ID),DETCLL(J,ID),DETCLU(J,ID),
     *                    J=1,DETCLK(ID))
        DO 2025  J = 1 , DETCLK(ID)
        WRITE (6,606) J,DETCLN(J,ID),DETCLL(J,ID),DETCLU(J,ID)
        IF ( ( J            .EQ. 1 ) . AND .
     *       ( DETCLL(J,ID) .NE. 0 ) )           GO TO 7450
        IF ( ( J            .EQ. DETCLK(ID) ) . AND .
     *       ( DETCLU(J,ID) .NE. 999        ) )  GO TO 7440
        IF ( DETCLL(J,ID) .GE. DETCLU(J,ID) )    GO TO 7430
        IF ( ( J           .GT.1              ) . AND .
     *       ( DETCLL(J,ID).NE.DETCLU(J-1,ID) ) )GO TO 7420
 2025   CONTINUE
      END IF
C[    IF ( I                  .EQ.-2147483647   )STOP 'RLOOPD I      02'
      IF ( ((I/6)*6.EQ.I).AND.(I.NE.NLOOPS) )    THEN
C       WRITE (6,604)
        WRITE (6,FMT) CHAR( 12 )
      END IF
C-----END OF DETECTOR LOOP
 2030 CONTINUE
      IF ( ( ICONTR .GE. ICTDF3 ) . AND . ( ICONTR .LE. ICDDF7 ) )
     *                                           GO TO 4040
C-----CHECK EACH SIGNAL PHASE TO MAKE SURE THAT DATA WAS ENTERED FOR
C-----EACH DETECTOR THAT WAS DECLARED FOR THAT SIGNAL PHASE
      DO 3030  II = 1 , NPHASE
      I = LPHASE(II)
      N = NLD(I)
                    IF ( N . LE . 0 )            GO TO 3030
      DO 3020  J = 1 , N
      JL = IABS(LLD(J,I))
      DO 3010  K = 1 , NLOOPS
                    IF ( JL . EQ . LLOOPS(K) )   GO TO 3020
 3010 CONTINUE
      GO TO 8810
 3020 CONTINUE
 3030 CONTINUE
C-----CHECK EACH DETECTOR TO MAKE SURE THAT IT APPEARED ON AT LEAST ONE
C-----OF THE LIST OF DETECTORS FOR A SIGNAL PHASE AS POSITIVE
      DO 4030  I = 1 , NLOOPS
      JL = LLOOPS(I)
                    IF ( ITYPLD(JL) .EQ. ICLAS ) GO TO 4030
      LDOK = .FALSE.
      DO 4020  JJ = 1 , NPHASE
      J = LPHASE(JJ)
      N = NLD(J)
                    IF ( N . LE . 0 )            GO TO 4020
      DO 4010  K = 1 , N
      IF ( JL . EQ . LLD(K,J) )                  THEN
        LDOK = .TRUE.
        IF ( ( LDCLEX(JL) . EQ . LDCALL ) . OR .
     *       ( LDCLEX(JL) . EQ . LDCNEX ) )      THEN
C-----    DETECTOR IS LDCALL=1=CALL OR LDCNEX=3=CALL&EXTEND
          LDPHCL(JL) = J
        END IF
      END IF
 4010 CONTINUE
 4020 CONTINUE
                    IF ( LDOK )                  GO TO 4030
      GO TO 8820
 4030 CONTINUE
 4040 CONTINUE
      RETURN
C-----PROCESS THE INPUT ERRORS AND STOP
 7350 CONTINUE
      WRITE (ERRMSG,735) ID,NINT( LDDELY(ID) ),LDEXTD(ID)
      CALL  PRTERR  ( 'STOP 735 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  735
 7420 CONTINUE
      WRITE (ERRMSG,742) J,DETCLN(J,ID),DETCLL(J,ID),DETCLU(J-1,ID)
      CALL  PRTERR  ( 'STOP 742 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  742
 7430 CONTINUE
      WRITE (ERRMSG,743) J,DETCLN(J,ID),DETCLL(J,ID),DETCLU(J,ID)
      CALL  PRTERR  ( 'STOP 743 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  743
 7440 CONTINUE
      WRITE (ERRMSG,744) J,DETCLN(J,ID),DETCLU(J,ID)
      CALL  PRTERR  ( 'STOP 744 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  744
 7450 CONTINUE
      WRITE (ERRMSG,745) J,DETCLN(J,ID),DETCLL(J,ID)
      CALL  PRTERR  ( 'STOP 745 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  745
 7460 CONTINUE
      WRITE (ERRMSG,746) ID,DETCLK(ID),LDC
      CALL  PRTERR  ( 'STOP 746 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  746
 7850 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     04'
      IF ( ITYPLD(ID) . EQ . ICLAS )             THEN
        I = 1
      ELSE
        I = NAL
      END IF
      WRITE (ERRMSG,785) ID,NLDLN(ID),I
      CALL  PRTERR  ( 'STOP 785 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  785
 7860 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     05'
      WRITE (ERRMSG,786) ID,NLS
      CALL  PRTERR  ( 'STOP 786 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  786
 8690 CONTINUE
      WRITE (ERRMSG,869) NLOOPS,NLS
      CALL  PRTERR  ( 'STOP 869 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  869
 8700 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     06'
      WRITE (ERRMSG,870) ID,NLS
      CALL  PRTERR  ( 'STOP 870 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  870
 8710 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     07'
      WRITE (ERRMSG,871) ID
      CALL  PRTERR  ( 'STOP 871 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  871
 8720 CONTINUE
      WRITE (ERRMSG,872) ID,ITYPLD(ID)
      CALL  PRTERR  ( 'STOP 872 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  872
 8730 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     09'
      WRITE (ERRMSG,873) ID,LDSTRT(ID)
      CALL  PRTERR  ( 'STOP 873 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  873
 8740 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     10'
      WRITE (ERRMSG,874) ID,LDSTOP(ID),LDSTRT(ID)
      CALL  PRTERR  ( 'STOP 874 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  874
 8750 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     11'
      WRITE (ERRMSG,875) ID,LDA(ID)
      CALL  PRTERR  ( 'STOP 875 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  875
 8760 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     12'
      WRITE (ERRMSG,876) ID,NLDLN(ID),NAL
      CALL  PRTERR  ( 'STOP 876 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  876
 8770 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     13'
C[    IF ( ILDLN              .EQ.-2147483647   )STOP 'RLOOPD ILDLN  01'
C[    IF ( MLANES             .EQ.-2147483647   )STOP 'RLOOPD MLANES 01'
      WRITE (ERRMSG,877) ID,ILDLN,LDA(ID),MLANES
      CALL  PRTERR  ( 'STOP 877 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  877
 8780 CONTINUE
C[    IF ( ILDLN              .EQ.-2147483647   )STOP 'RLOOPD ILDLN  02'
C[    IF ( JL                 .EQ.-2147483647   )STOP 'RLOOPD JL     01'
      WRITE (ERRMSG,878) LDA(ID),ILDLN,NLDL(JL),NLO
      CALL  PRTERR  ( 'STOP 878 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  878
 8790 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     14'
C[    IF ( ILDLN              .EQ.-2147483647   )STOP 'RLOOPD ILDLN  03'
      WRITE (ERRMSG,879) ID,LDA(ID),ILDLN
      CALL  PRTERR  ( 'STOP 879 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  879
 8800 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     15'
C[    IF ( ILDLN              .EQ.-2147483647   )STOP 'RLOOPD ILDLN  04'
C[    IF ( MGEOM4             .EQ.-2147483647   )STOP 'RLOOPD MGEOM4 01'
      WRITE (ERRMSG,880) ID,LDSTOP(ID),LDA(ID),ILDLN,MGEOM4
      CALL  PRTERR  ( 'STOP 880 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  880
 8810 CONTINUE
C[    IF ( I                  .EQ.-2147483647   )STOP 'RLOOPD I      03'
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     16'
      WRITE (ERRMSG,881) JL,I
      CALL  PRTERR  ( 'STOP 881 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  881
 8820 CONTINUE
C[    IF ( ID                 .EQ.-2147483647   )STOP 'RLOOPD ID     17'
      WRITE (ERRMSG,882) JL
      CALL  PRTERR  ( 'STOP 882 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RLOOPD'                             )
      STOP  882
      END                                                               RLOOPD
C
C
C
      SUBROUTINE RDVPRD
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'CURAND'
      INCLUDE 'INDEX'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'TITLE'
      INCLUDE 'USER'
      TYPE (VEHCL),     POINTER :: VEHC
      CHARACTER*1       IFUT
      CHARACTER*3       VEMERG
      INTEGER           I,IAMAX(NVC),IDCHAR(NDC),IDMAX(NVC),ILNB,
     *                  IVCHAR(NVC),IVMAX(NVC),J,JVN
      DOUBLE PRECISION  PIJRMI,PIJRMX
  401 FORMAT(F4.2)
  501 FORMAT(2I4,A5,I8,I3)
  502 FORMAT(20I4)
  503 FORMAT(10F5.1)
  504 FORMAT(F6.2,I2,I1,I3,2I2,2I1,A1,F7.2,I4,6F7.2,I6,1X,A3,3F7.3)
  505 FORMAT(10A8)
  506 FORMAT(I2,4(I3,3I2,2I3,2I2),:(/,2X,4(I3,3I2,2I3,2I2)))
C1701 FORMAT(38H-LENGTH OF VEHICLES (FT) -------------,15I6)
C7702 FORMAT(38H LENGTH OF VEHICLES (FT) -------------,15I6)
C1703 FORMAT(38H VEHICLE OPERATIONAL FACTOR ----------,15I6)
C1704 FORMAT(38H MAXIMUM DECELERATION (FT/SEC/SEC) ---,15I6)
C1705 FORMAT(38H MAXIMUM ACCELERATION (FT/SEC/SEC) ---,15I6)
C1706 FORMAT(38H MAXIMUM VELOCITY (FT/SEC) -----------,15I6)
C1707 FORMAT(38H MINIMUM TURNING RADIUS (FT) ---------,15I6)
C1708 FORMAT(38H DRIVER OPERATIONAL FACTOR -----------,5I6)
C1709 FORMAT(38H DRIVER REACTION TIME (SEC) ----------,6F6.1)
C1710 FORMAT()
C1711 FORMAT(13H QUEUE BUFFER,I3,9H  VEHICLE,I5,10H  READIN =,F10.2,7I5)
  712 FORMAT(
     *31H NUMBER OF VEHICLE ATTRIBUTES =,I3,11H - NVEHCL =,I3,
     *14H IS NOT NVCD =,I3)
  783 FORMAT(
     *27H NUMBER OF DRIVER CLASSES =, I3,6H IS LT,I2,6H OR GT,I2)
  784 FORMAT(
     *28H NUMBER OF VEHICLE CLASSES =,I3,6H IS LT,I3,6H OR GT,I3)
  883 FORMAT(
     *15H AVERAGE PIJR =,F4.1,21H IS LT MINIMUM PIJR =,F4.1)
C
C-----SUBROUTINE RDVPRD READS THE DRIVER-VEHICLE PROCESSOR DATA FROM THE
C-----DRIVER-VEHICLE PROCESSOR TAPE, INITIALIZES THE QUEUE BUFFERS, AND
C-----CHECKS FOR ERRORS
C
C[    DO  I = 1 , NVC
C[    IAMAX (I)  = -2147483647
C[    IDMAX (I)  = -2147483647
C[    IVCHAR(I)  = -2147483647
C[    IVMAX (I)  = -2147483647
C[    END DO
C[    DO  I = 1 , NDC
C[    IDCHAR(I)  = -2147483647
C[    END DO
C[    IFUT       = '~'
C[    I          = -2147483647
C[    J          = -2147483647
C[    PIJRMI     = -2147483647.0
C[    PIJRMX     = -2147483647.0
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'RDVPRD'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
      IEF = .FALSE.
C-----READ THE NUMBER OF VEHICLE AND DRIVER CLASSES, VERSION NUMBER,
C-----AND RANDOM NUMBER SEED
      READ (IVEHP,501) NVEHCL,NDRICL,KVERSN,ISEED,NVEHAT
      IF ( (NVEHCL.LT.NVCD).OR.(NVEHCL.GT.NVC) ) GO TO 7840
      IF ( (NDRICL.LT.NDCD).OR.(NDRICL.GT.NDC) ) GO TO 7830
                    IF ( KVERSN . EQ . '     ' ) KVERSN = 'V3.10'
      READ (KVERSN(2:5),401,ERR=9360) DVERSN
                    IF ( ISEED  . LE . 0 )       ISEED  = 42081
                    IF ( NVEHAT . LE . 0 )       NVEHAT =     6
      ISTART = ISEED
      NEWSED = .TRUE.
      NEXTRN = 1
C-----READ AND ECHO-PRINT THE VEHICLE CHARACTERISTICS
      READ  (IVEHP,502)   (LENV  (I),I=1,NVEHCL)
C1    WRITE (6    ,701)   (LENV  (I),I=1,NVEHCL)
C7    WRITE (PPP  ,702)   (LENV  (I),I=1,NVEHCL)
      READ  (IVEHP,502)   (IVCHAR(I),I=1,NVEHCL)
C1    WRITE (6    ,703)   (IVCHAR(I),I=1,NVEHCL)
      READ  (IVEHP,502)   (IDMAX (I),I=1,NVEHCL)
C1    WRITE (6    ,704)   (IDMAX (I),I=1,NVEHCL)
      READ  (IVEHP,502)   (IAMAX (I),I=1,NVEHCL)
C1    WRITE (6    ,705)   (IAMAX (I),I=1,NVEHCL)
      READ  (IVEHP,502)   (IVMAX (I),I=1,NVEHCL)
C1    WRITE (6    ,706)   (IVMAX (I),I=1,NVEHCL)
      READ  (IVEHP,502)   (IRMIN (I),I=1,NVEHCL)
C1    WRITE (6    ,707)   (IRMIN (I),I=1,NVEHCL)
      IF ( NVEHAT . GE . 10 )                    THEN
        READ  (IVEHP,505) (CLASSV(I),I=1,NVEHCL)
        READ  (IVEHP,502) (HEIGHT(I),I=1,NVEHCL)
        READ  (IVEHP,502) (WIDV  (I),I=1,NVEHCL)
        DO 1005  I = 1 , NVEHCL
        READ  (IVEHP,506) NUNITS(I),
     *                    (ULEN  (J,I),UWID  (J,I),UDRWSQ(J,I),
     *                     UFPD  (J,I),URPD  (J,I),URHPD (J,I),
     *                     UTRLEN(J,I),UTRWID(J,I),J=1,NUNITS(I))
 1005   CONTINUE
      ELSE
        IF ( NVEHCL . NE . NVCD )                GO TO 7120
        NVEHAT       = 10
        LENV  (  11) = 72
        LENV  (  12) = 72
        CLASSV(   1) = 'AUTO'
        CLASSV(   2) = 'AUTO'
        CLASSV(   3) = 'AUTO'
        CLASSV(   4) = 'AUTO'
        CLASSV(   5) = 'SU2'
        CLASSV(   6) = 'SU2'
        CLASSV(   7) = 'SU3'
        CLASSV(   8) = 'SU3'
        CLASSV(   9) = '2-S1'
        CLASSV(  10) = '2-S1'
        CLASSV(  11) = '3-S2'
        CLASSV(  12) = '3-S2'
        HEIGHT(   1) =  4
        HEIGHT(   2) =  4
        HEIGHT(   3) =  5
        HEIGHT(   4) =  6
        HEIGHT(   5) = 12
        HEIGHT(   6) = 12
        HEIGHT(   7) = 12
        HEIGHT(   8) = 12
        HEIGHT(   9) = 13
        HEIGHT(  10) = 13
        HEIGHT(  11) = 13
        HEIGHT(  12) = 13
        WIDV  (   1) =  6
        WIDV  (   2) =  6
        WIDV  (   3) =  7
        WIDV  (   4) =  7
        WIDV  (   5) =  8
        WIDV  (   6) =  8
        WIDV  (   7) =  8
        WIDV  (   8) =  8
        WIDV  (   9) =  8
        WIDV  (  10) =  8
        WIDV  (  11) =  8
        WIDV  (  12) =  8
        NUNITS(   1) =  1
        NUNITS(   2) =  1
        NUNITS(   3) =  1
        NUNITS(   4) =  1
        NUNITS(   5) =  1
        NUNITS(   6) =  1
        NUNITS(   7) =  1
        NUNITS(   8) =  1
        NUNITS(   9) =  2
        NUNITS(  10) =  2
        NUNITS(  11) =  2
        NUNITS(  12) =  2
        ULEN  (1, 1) = 14
        ULEN  (1, 2) = 15
        ULEN  (1, 3) = 16
        ULEN  (1, 4) = 18
        ULEN  (1, 5) = 32
        ULEN  (1, 6) = 32
        ULEN  (1, 7) = 32
        ULEN  (1, 8) = 32
        ULEN  (1, 9) = 20
        ULEN  (1,10) = 20
        ULEN  (1,11) = 28
        ULEN  (1,12) = 28
        UWID  (1, 1) =  6
        UWID  (1, 2) =  6
        UWID  (1, 3) =  7
        UWID  (1, 4) =  7
        UWID  (1, 5) =  8
        UWID  (1, 6) =  8
        UWID  (1, 7) =  8
        UWID  (1, 8) =  8
        UWID  (1, 9) =  8
        UWID  (1,10) =  8
        UWID  (1,11) =  8
        UWID  (1,12) =  8
        UDRWSQ(1, 1) =  1
        UDRWSQ(1, 2) =  1
        UDRWSQ(1, 3) =  1
        UDRWSQ(1, 4) =  1
        UDRWSQ(1, 5) =  1
        UDRWSQ(1, 6) =  1
        UDRWSQ(1, 7) =  1
        UDRWSQ(1, 8) =  1
        UDRWSQ(1, 9) =  1
        UDRWSQ(1,10) =  1
        UDRWSQ(1,11) =  1
        UDRWSQ(1,12) =  1
        UFPD  (1, 1) =  3
        UFPD  (1, 2) =  3
        UFPD  (1, 3) =  3
        UFPD  (1, 4) =  3
        UFPD  (1, 5) =  3
        UFPD  (1, 6) =  3
        UFPD  (1, 7) =  4
        UFPD  (1, 8) =  4
        UFPD  (1, 9) =  4
        UFPD  (1,10) =  4
        UFPD  (1,11) =  4
        UFPD  (1,12) =  4
        URPD  (1, 1) = 10
        URPD  (1, 2) = 11
        URPD  (1, 3) = 12
        URPD  (1, 4) = 14
        URPD  (1, 5) = 23
        URPD  (1, 6) = 23
        URPD  (1, 7) = 25
        URPD  (1, 8) = 25
        URPD  (1, 9) = 17
        URPD  (1,10) = 17
        URPD  (1,11) = 23
        URPD  (1,12) = 23
        URHPD (1, 1) =  0
        URHPD (1, 2) =  0
        URHPD (1, 3) =  0
        URHPD (1, 4) =  0
        URHPD (1, 5) =  0
        URHPD (1, 6) =  0
        URHPD (1, 7) =  0
        URHPD (1, 8) =  0
        URHPD (1, 9) = 17
        URHPD (1,10) = 17
        URHPD (1,11) = 22
        URHPD (1,12) = 22
        UTRLEN(1, 1) =  0
        UTRLEN(1, 2) =  0
        UTRLEN(1, 3) =  0
        UTRLEN(1, 4) =  0
        UTRLEN(1, 5) =  0
        UTRLEN(1, 6) =  0
        UTRLEN(1, 7) =  0
        UTRLEN(1, 8) =  0
        UTRLEN(1, 9) =  0
        UTRLEN(1,10) =  0
        UTRLEN(1,11) =  0
        UTRLEN(1,12) =  0
        UTRWID(1, 1) =  0
        UTRWID(1, 2) =  0
        UTRWID(1, 3) =  0
        UTRWID(1, 4) =  0
        UTRWID(1, 5) =  0
        UTRWID(1, 6) =  0
        UTRWID(1, 7) =  0
        UTRWID(1, 8) =  0
        UTRWID(1, 9) =  0
        UTRWID(1,10) =  0
        UTRWID(1,11) =  0
        UTRWID(1,12) =  0
        ULEN  (2, 9) = 46
        ULEN  (2,10) = 46
        ULEN  (2,11) = 53
        ULEN  (2,12) = 53
        UWID  (2, 9) =  8
        UWID  (2,10) =  8
        UWID  (2,11) =  8
        UWID  (2,12) =  8
        UDRWSQ(2, 9) =  2
        UDRWSQ(2,10) =  2
        UDRWSQ(2,11) =  2
        UDRWSQ(2,12) =  2
        UFPD  (2, 9) =  3
        UFPD  (2,10) =  3
        UFPD  (2,11) =  3
        UFPD  (2,12) =  3
        URPD  (2, 9) = 38
        URPD  (2,10) = 38
        URPD  (2,11) = 43
        URPD  (2,12) = 43
        URHPD (2, 9) =  0
        URHPD (2,10) =  0
        URHPD (2,11) =  0
        URHPD (2,12) =  0
        UTRLEN(2, 9) =  0
        UTRLEN(2,10) =  0
        UTRLEN(2,11) =  0
        UTRLEN(2,12) =  0
        UTRWID(2, 9) =  0
        UTRWID(2,10) =  0
        UTRWID(2,11) =  0
        UTRWID(2,12) =  0
      END IF
      DO 1008  I = 1 , NVEHCL
      VEHC=>VEHCLA(I)
      VEHC%TYPE           = CLASSV(I)
      VEHC%HEIGHT         = HEIGHT(I)
      VEHC%LEN            = LENV  (I)
      VEHC%UNITS          = NUNITS(I)
      VEHLEN(I)           = LENV  (I)
      VEHWID(I)           = WIDV  (I)
      DO 1007  J = 1 , NUNITS(I)
      VEHC%UNIT(J)%LEN    = ULEN  (J,I)
      VEHC%UNIT(J)%WID    = UWID  (J,I)
      VEHC%UNIT(J)%UDRGSQ = UDRWSQ(J,I)
      VEHC%UNIT(J)%FPD    = UFPD  (J,I)
      VEHC%UNIT(J)%RPD    = URPD  (J,I)
      VEHC%UNIT(J)%HPD    = URHPD (J,I)
      VEHC%UNIT(J)%TRLEN  = UTRLEN(J,I)
      VEHC%UNIT(J)%TRWID  = UTRWID(J,I)
 1007 CONTINUE
 1008 CONTINUE
C-----READ AND ECHO-PRINT THE DRIVER CHARACTERISTICS
      READ  (IVEHP,502)   (IDCHAR(I),I=1,NDRICL)
C1    WRITE (6  ,708)     (IDCHAR(I),I=1,NDRICL)
      READ  (IVEHP,503)   (PIJR(I),I=1,NDRICL),APIJR
C1    WRITE (6  ,709)     (PIJR(I),I=1,NDRICL),APIJR
C1    WRITE (6  ,710)
      DCHRMN = 1.0D9
      DCHRMX = 0.0D0
      PIJRMI = 1.0D9
      PIJRMX = 0.0D0
      TLEAD = TLEAD - APIJR
      TLAG  = TLAG  - APIJR
C-----COMPUTE DRIVER PARAMETERS FOR THE SIMULATION
      DO 1010  I = 1 , NDRICL
C[    IF ( IDCHAR(I)          .EQ.-2147483647   )STOP 'RDVPRD IDCHAR 01'
      DCHAR(I) = IDCHAR(I)/100.0D0
      DCHRMN = DMIN1( DCHRMN,DCHAR(I) )
      DCHRMX = DMAX1( DCHRMX,DCHAR(I) )
C[    IF ( PIJRMI             .EQ.-2147483647.0 )STOP 'RDVPRD PIJRMI 01'
      PIJRMI = DMIN1( PIJR(I),PIJRMI )
C[    IF ( PIJRMX             .EQ.-2147483647.0 )STOP 'RDVPRD PIJRMX 01'
      PIJRMX = DMAX1( PIJR(I),PIJRMX )
      IPIJR(I) = MAX0( IDINT(PIJR(I)/DT+0.5D0),1 )
      PIJR(I) = IPIJR(I)*DT
 1010 CONTINUE
C[    IF ( PIJRMI             .EQ.-2147483647.0 )STOP 'RDVPRD PIJRMI 02'
                    IF ( APIJR . LT . PIJRMI )   GO TO 8830
      MPRTM = 2.0D0*PIJRMX/DT + 0.5D0
C-----COMPUTE VEHICLE PARAMETERS FOR THE SIMULATION
      DO 1020  I = 1 , NVEHCL
C[    IF ( IDMAX(I)           .EQ.-2147483647   )STOP 'RDVPRD IDMAX  01'
      DMAX(I) = -DUTOL*IDMAX(I)
      DMAXAV  = MIN( DMAXAV,DMAX(I) )
C[    IF ( IAMAX(I)           .EQ.-2147483647   )STOP 'RDVPRD IAMAX  01'
      AMAX(I) = AUTOL*IAMAX(I)
      AMAXAV  = MAX( AMAXAV,AMAX(I) )
C[    IF ( IVMAX(I)           .EQ.-2147483647   )STOP 'RDVPRD IVMAX  01'
      VMAX(I) = IVMAX(I)
C[    IF ( IVCHAR(I)          .EQ.-2147483647   )STOP 'RDVPRD IVCHAR 01'
      VCHAR(I) = IVCHAR(I)/100.0D0
C OLD IF      (   LENV(I) . LE . 15   )          THEN
C OLD   WIDV(I) = 6
C OLD ELSE IF ( ( LENV(I) . GT . 15 ) . AND .
C OLD*          ( LENV(I) . LT . 20 ) )          THEN
C OLD   WIDV(I) = 7
C OLD ELSE
C OLD   WIDV(I) = 8
C OLD END IF
 1020 CONTINUE
C-----INITIALIZE THE QUEUE BUFFERS
      DO 2010  I = 1 , NIL
C-----READ THE DRIVER-VEHICLE INFORMATION, IF END-OF-FILE THEN GO TO
C-----2020 AND SET IEF FLAG
C-----READ QTIME,IVEHCL,IDRICL,DESVEL,NOBAPD,IA,ILN,IPRTLO,IFUT,
C-----     QSTTIM,FSTPIA,QSTPOS,QSTDTM,QGOTIM,QGOATM,QRRTIM,QRRATM,
C-----     JVN,VEMERG,ENTVEL,ENTACC,ENTSLP
      READ (IVEHP,504,END=2020) QTIME(I),(IBUF(J,I),J=1,IBUFPL),IFUT,
     *                          QSTTIM(I),IBUF(IBUFFS,I),QSTPOS(I),
     *                          QSTDTM(I),
     *                          QGOTIM(I),QGOATM(I),
     *                          QRRTIM(I),QRRATM(I),
     *                          JVN,VEMERG,
     *                          ENTVEL(I),ENTACC(I),ENTSLP(I)
C[    IF ( IFUT               .EQ.'~'           )STOP 'RDVPRD IFUT   01'
      IF ( IFUT . EQ . 'F' )                     THEN
        IF ( GVERSN . LT . 3.12 )                GO TO 9370
        IF ( DVERSN . LT . 3.12 )                GO TO 9380
        IBUF(IBUFDO,I) = -IBUF(IBUFDO,I)
      END IF
                    IF ( JVN    . EQ . 0      )  JVN    = NUMV
                    IF ( VEMERG . EQ . IBLNK1 )  VEMERG = INO
      IEMERG(I) = (VEMERG . EQ . IYES)
C-----SET THE SEQUENTIAL VEHICLE NUMBER FOR THIS VEHICLE
      IBUF(IBUFVN,I) = JVN
      NUMV = NUMV + 1
C2                  IF ( IBUF(IBUFPL,I) .EQ. 0 ) GO TO 102
C1                  IF ( TIME . LT . TPRINT )    GO TO 102
C1    WRITE (6,711) I,IBUF(IBUFVN,I),QTIME(I),(IBUF(J,I),J=1,IBUFPL)
C1102 CONTINUE
C-----INCREMENT THE NUMBER OF VEHICLES IN THE QUEUE BUFFERS
      IQF = IQF + 1
C-----END OF QUEUE BUFFER LOOP
 2010 CONTINUE
      I = NIL
      RETURN
 2020 CONTINUE
C-----SET END-OF-FILE FLAG AND FLAG QUEUE BUFFER I UNUSED
      IEF = .TRUE.
C[    IF ( I                  .EQ.-2147483647   )STOP 'RDVPRD I      01'
      QTIME(I) = -1.0D0
      RETURN
C-----PROCESS THE INPUT ERROR AND STOP
 7120 CONTINUE
      WRITE (ERRMSG,712) NVEHAT,NVEHCL,NVCD
      CALL  PRTERR  ( 'STOP 712 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RDVPRD'                             )
      STOP  712
 7830 CONTINUE
      WRITE (ERRMSG,783) NDRICL,NDCD,NDC
      CALL  PRTERR  ( 'STOP 783 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RDVPRD'                             )
      STOP  783
 7840 CONTINUE
      WRITE (ERRMSG,784) NVEHCL,NVCD,NVC
      CALL  PRTERR  ( 'STOP 784 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RDVPRD'                             )
      STOP  784
 8830 CONTINUE
C[    IF ( PIJRMI             .EQ.-2147483647.0 )STOP 'RDVPRD PIJRMI 03'
      WRITE (ERRMSG,883) APIJR,PIJRMI
      CALL  PRTERR  ( 'STOP 883 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RDVPRD'                             )
      STOP  883
 9360 CONTINUE
      CALL  PRTERR  ( 'STOP 936 - ' //
     *                'ERROR READING DVERSN FROM KVERSN - ' //
     *                'RDVPRD'                                 )
      STOP  936
 9370 CONTINUE
      CALL  PRTERR  ( 'STOP 937 - ' //
     *                'GEOPRO VERSION LT 3.12 FOR FREE U-TURNS - ' //
     *                'LOGIN/RDVPRD'                                  )
      STOP  937
 9380 CONTINUE
      CALL  PRTERR  ( 'STOP 938 - ' //
     *                'DVPRO VERSION LT 3.12 FOR FREE U-TURNS - ' //
     *                'LOGIN/RDVPRD'                                 )
      STOP  938
      END                                                               RDVPRD
C
C
C
      SUBROUTINE RVMSM
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'TITLE'
      INCLUDE 'USER'
      INTEGER           I,ILNB,J
      DOUBLE PRECISION  POSEND
  501 FORMAT(2I4,3F7.2, I4  ,2I2,2F7.2,I6,A7,2F6.2)
  601 FORMAT(1X,A,//)
  602 FORMAT(11H A TOTAL OF,I4,32H VEHICLE MESSAGE SYSTEM MESSAGES)
  603 FORMAT(//,
     *       ' MESSAGE NUMBER --------------------- = ',I6)
  604 FORMAT(' MESSAGE TYPE ----------------------- = ',I6,3X,
     *       ' DRIVER DMS')
  605 FORMAT(' MESSAGE TYPE ----------------------- = ',I6,3X,
     *       ' DRIVER IVDMS')
  606 FORMAT(' MESSAGE TYPE ----------------------- = ',I6,3X,
     *       ' VEHICLE IVDMS')
  607 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' ACCEL/DECEL TO SPEED XX USING NORMAL ACCEL/DECEL' )
  608 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' ACCEL/DECEL TO SPEED XX USING MAX VEHICLE ACCEL/DECEL' )
  609 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' STOP AT THE INTERSECTION STOP LINE')
  610 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' STOP AT LOCATION XX')
  611 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' STOP IMMEDIATELY USING MAX VEHICLE DECEL')
  612 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' STOP IMMEDIATELY USING COLLISION DECEL')
  613 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' CHANGE LANES TO THE LEFT')
  614 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' CHANGE LANES TO THE RIGHT')
  615 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' FORCED GO')
  616 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' FORCED RUN THE RED SIGNAL')
  617 FORMAT(' MESSAGE ---------------------------- = ',I6,3X,
     *       ' DISTRACTED DRIVER')
  618 FORMAT(' MESSAGE PARAMETER - COLLISION DECEL  = ',F9.2,
     *       ' FT/SEC/SEC')
  619 FORMAT(' MESSAGE PARAMETER - SPEED ---------- = ',F9.2,
     *       ' MPH')
  620 FORMAT(' MESSAGE PARAMETER - POSITION ------- = ',F9.2,
     *       ' FEET')
  621 FORMAT(' MESSAGE START TIME ----------------- = ',F9.2,
     *       ' SECONDS',/,
     *       ' MESSAGE ACTIVE TIME ---------------- = ',F9.2,
     *       ' SECONDS')
  622 FORMAT(' MESSAGE APPROACH ------------------- = ',I6)
  623 FORMAT(' MESSAGE INTERSECTION PATH ---------- = ',I6)
  624 FORMAT(' MESSAGE LANE BEGIN ----------------- = ',I6,/,
     *       ' MESSAGE LANE END ------------------- = ',I6)
  625 FORMAT(' MESSAGE POSITION BEGIN ------------- = ',F9.2,' FEET',/,
     *       ' MESSAGE POSITION END --------------- = ',F9.2,' FEET')
  626 FORMAT(' MESSAGE VEHICLE NUMBER ------------- = ALL VEHICLES')
  627 FORMAT(' MESSAGE VEHICLE NUMBER ------------- = ',I6)
  628 FORMAT(' MESSAGE REACTION TIME DISTRIBUTION - = ',A,/,
     *       ' MESSAGE REACTION TIME MEAN --------- = ',F9.2,
     *       ' SECONDS')
  629 FORMAT(' MESSAGE REACTION TIME PARAMETER ---- = ',F9.2)
  769 FORMAT('END-OF-FILE READING VEHICLE MESSAGE SYSTEM MESSAGE ',I4)
  768 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,' TYPE = ',I4,
     *       ' IS NOT ',I1,', ',I1,', OR ',I1)
  767 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,' MESSAGE = ',I4,
     *       ' IS NOT ',I1,', ',I1,', ',I1,', ',I1,', ',I1,', ',I1,', ',
     *       I1,', ',I1,', ',I1,', ',I2,', OR ',I2)
  766 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,' MESSAGE = ',I4,
     *       ' IS ',I1,', ',I1,', ',I1,', ',I2,', OR ',I2,
     *       ' - INVALID FOR VEHICLE IVDMS MESSAGE')
  765 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,' MESSAGE = ',I4,
     *       ' MESSAGE COLLISION DECELERATION = ',F7.2,
     *       ' FT/SEC/SEC IS LT ',F7.2,' OR GE 0.00 FT/SEC/SEC')
  764 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,' MESSAGE = ',I4,
     *       ' MESSAGE SPEED = ',F7.2' MPH IS LT 0.00 OR GT ',F7.2,
     *       ' MPH')
  763 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,' MESSAGE = ',I4,
     *       ' MESSAGE POSITION = ',F7.2' FEET IS LT 0.00 OR GT ',F7.2,
     *       ' FEET')
  762 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' MESSAGE START TIME = ',F7.2,' IS LT 0.00 OR GT ',F7.2,
     *       ' SECONDS')
  761 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' MESSAGE START TIME = ',F7.2,
     *       ' IS LT PREVIOUS MESSAGE START TIME = ',F7.2,' SECONDS')
  760 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' MESSAGE ACTIVE TIME = ',F7.2,' IS LT 0.00 SECONDS')
  759 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' MESSAGE APPROACH NUMBER = ',I4,' IS INVALID')
  758 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' MESSAGE CHANGE LANES LEFT/RIGHT',
     *       ' NOT ALLOWED FOR MESSAGE INTERSCTION PATH NUMBER = ',I4)
  757 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' MESSAGE INTERSCTION PATH NUMBER = ',I4,' IS INVALID')
  756 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' MESSAGE APPROACH NUMBER = ',I4,
     *       ' LANE BEGIN = ',I2,' IS LT 1 OR GT ',I2)
  755 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' MESSAGE APPROACH NUMBER = ',I4,
     *       ' LANE END = ',I2,' IS LT 1 OR GT ',I2)
  754 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' MESSAGE APPROACH NUMBER = ',I4,
     *       ' LANE END = ',I2,' IS LT LANE BEGIN = ',I2)
  753 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' MESSAGE APPROACH NUMBER = ',I4,
     *       ' NUMBER OF LANES = ',I2,' IS LT 2 FOR CHANGE LEFT/RIGHT')
  752 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' POSITION BEGIN = ',F7.2,' IS LT 0.00 OR GT ',F7.2,
     *       ' FEET')
  751 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' POSITION END = ',F7.2,' IS LT POSITION BEGIN = ',F7.2,
     *       ' OR GT ',F7.2,' FEET')
  750 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' VEHICLE NUMBER = ',I6,' IS LT 0')
  749 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' REACTION TIME DISTRIBUTION NAME = (',A,')',
     *       ' IS NOT (CONSTAN), (ERLANG), (GAMMA), (LOGNRML),',
     *       ' (NEGEXP), (SNEGEXP), OR (UNIFORM)')
  748 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' REACTION TIME DISTRIBUTION MEAN = ',F6.2,
     *       ' IS LT 0.00 SECONDS')
  747 FORMAT('VEHICLE MESSAGE SYSTEM PRIORITY ',I2,
     *       ' NOT FOUND IN IVMSPR ARRAY')
  740 FORMAT('VEHICLE MESSAGE SYSTEM MESSAGE ',I4,
     *       ' REACTION TIME DISTRIBUTION PARAMETER = ',F6.2,
     *       ' IS LT 0.00')
C
C-----SUBROUTINE RVMSM READS THE VEHICLE MESSAGE SYSTEM DATA FROM THE
C-----INPUT DIRECTLY TO THE SIMULATION PROCESSOR AND CHECKS FOR ERRORS
C
C[    I          = -2147483647
C*    NRNAME = NRNAME + 1
C*    IRNAME(NRNAME) = 'RVMSM'
C*                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
                    IF ( NVMSM . LE . 0 )        RETURN
      CALL  PHEADR  ( 6 )
      WRITE (6,601) STITLE
      WRITE (6,602) NVMSM
      DO 1070  I = 1 , NVMSM
C-----READ THE VEHICLE MESSAGE SYSTEM DATA FROM THE INPUT DIRECTLY TO
C-----THE SIMULATION PROCESSOR
C-----IVMSMT(I) I4   VMS MESSAGE TYPE
C-----IVMSMG(I) I4   VMS MESSAGE
C-----DVMSMP(I) F7.2 VMS MESSAGE PARAMETER
C-----DVMSST(I) F7.2 VMS MESSAGE STARTING TIME
C-----DVMSAT(I) F7.2 VMS MESSAGE ACTIVE TIME
C-----IVMSAP(I) I4   VMS MESSAGE APPROACH (+) OR INTERSECTION PATH (-)
C-----IVMSLB(I) I2   VMS MESSAGE LANE BEGIN
C-----IVMSLE(I) I2   VMS MESSAGE LANE END
C-----DVMSPB(I) F7.2 VMS MESSAGE POSITION BEGIN
C-----DVMSPE(I) F7.2 VMS MESSAGE POSITION END
C-----IVMSVN(I) I6   VMS MESSAGE VEHICLE NUMBER (0=ALL)
C-----CVMSDN(I) A7   VMS MESSAGE REACTION TIME DISTRIBUTION NAME
C-----DVMSDM(I) F6.2 VMS MESSAGE REACTION TIME DISTRIBUTION MEAN
C-----DVMSDP(I) F6.2 VMS MESSAGE REACTION TIME DISTRIBUTION PARAMETER
      READ (INPUT,501,END=7690) IVMSMT(I),IVMSMG(I),DVMSMP(I),DVMSST(I),
     *                          DVMSAT(I),IVMSAP(I),IVMSLB(I),IVMSLE(I),
     *                          DVMSPB(I),DVMSPE(I),IVMSVN(I),CVMSDN(I),
     *                          DVMSDM(I),DVMSDP(I)
C-----PRINT VEHICLE MESSAGE SYSTEM MESSAGE NUMBER
      WRITE (6,603)  I
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE TYPE FOR ERRORS
C-----VMSTDD DRIVER  DMS
C-----VMSTDI DRIVER  IVDMS
C-----VMSTVI VEHICLE IVDMS
      IF ( ( IVMSMT(I) . NE . VMSTDD ) . AND .
     *     ( IVMSMT(I) . NE . VMSTDI ) . AND .
     *     ( IVMSMT(I) . NE . VMSTVI ) )         GO TO 7680
C-----PRINT VEHICLE MESSAGE SYSTEM MESSAGE TYPE
      IF ( IVMSMT(I) . EQ . VMSTDD )             WRITE (6,604) IVMSMT(I)
      IF ( IVMSMT(I) . EQ . VMSTDI )             WRITE (6,605) IVMSMT(I)
      IF ( IVMSMT(I) . EQ . VMSTVI )             WRITE (6,606) IVMSMT(I)
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE FOR ERRORS
C-----VMSMAN ACCELERATE OR DECELERATE TO SPEED XX USING NORMAL
C-----       ACCELERATION OR DECELERATION
C-----VMSMAM ACCELERATE OR DECELERATE TO SPEED XX USING MAXIMUM
C-----       VEHICLE ACCELERATION OR DECELERATION
C-----VMSMSI STOP AT THE INTERSECTION STOP LINE
C-----VMSMSL STOP AT LOCATION XX
C-----VMSMSM STOP IMMEDIATELY USING MAXIMUM VEHICLE DECELERATION
C-----VMSMSC STOP IMMEDIATELY USING COLLISION DECELERATION
C-----VMSMCL CHANGE LANES TO THE LEFT
C-----VMSMCR CHANGE LANES TO THE RGHT
C-----VMSMGO FORCED GO
C-----VMSMRR FORCED RUN THE RED SIGNAL
C-----VMSMDD DISTRACTED DRIVER
      IF ( ( IVMSMG(I) . NE . VMSMAN ) . AND .
     *     ( IVMSMG(I) . NE . VMSMAM ) . AND .
     *     ( IVMSMG(I) . NE . VMSMSI ) . AND .
     *     ( IVMSMG(I) . NE . VMSMSL ) . AND .
     *     ( IVMSMG(I) . NE . VMSMSM ) . AND .
     *     ( IVMSMG(I) . NE . VMSMSC ) . AND .
     *     ( IVMSMG(I) . NE . VMSMCL ) . AND .
     *     ( IVMSMG(I) . NE . VMSMCR ) . AND .
     *     ( IVMSMG(I) . NE . VMSMGO ) . AND .
     *     ( IVMSMG(I) . NE . VMSMRR ) . AND .
     *     ( IVMSMG(I) . NE . VMSMDD ) )         GO TO 7670
C-----PRINT VEHICLE MESSAGE SYSTEM MESSAGE
      IF ( IVMSMG(I) . EQ . VMSMAN )             WRITE (6,607) IVMSMG(I)
      IF ( IVMSMG(I) . EQ . VMSMAM )             WRITE (6,608) IVMSMG(I)
      IF ( IVMSMG(I) . EQ . VMSMSI )             WRITE (6,609) IVMSMG(I)
      IF ( IVMSMG(I) . EQ . VMSMSL )             WRITE (6,610) IVMSMG(I)
      IF ( IVMSMG(I) . EQ . VMSMSM )             WRITE (6,611) IVMSMG(I)
      IF ( IVMSMG(I) . EQ . VMSMSC )             WRITE (6,612) IVMSMG(I)
      IF ( IVMSMG(I) . EQ . VMSMCL )             WRITE (6,613) IVMSMG(I)
      IF ( IVMSMG(I) . EQ . VMSMCR )             WRITE (6,614) IVMSMG(I)
      IF ( IVMSMG(I) . EQ . VMSMGO )             WRITE (6,615) IVMSMG(I)
      IF ( IVMSMG(I) . EQ . VMSMRR )             WRITE (6,616) IVMSMG(I)
      IF ( IVMSMG(I) . EQ . VMSMDD )             WRITE (6,617) IVMSMG(I)
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE TYPE AND MESSAGE COMBINATIONS
      IF ( IVMSMT(I) . EQ . VMSTVI )             THEN
C-----  VEHICLE MESSAGE SYSTEM MESSAGE TYPE VEHICLE IVDMS IS NOT ALLOWED
C-----  FOR:
C-----  VMSMCL CHANGE LANES TO THE LEFT
C-----  VMSMCR CHANGE LANES TO THE RGHT
C-----  VMSMGO FORCED GO
C-----  VMSMRR FORCED RUN THE RED SIGNAL
C-----  VMSMDD DISTRACTED DRIVER
        IF ( ( IVMSMG(I) . EQ . VMSMCL ) . OR .
     *       ( IVMSMG(I) . EQ . VMSMCR ) . OR .
     *       ( IVMSMG(I) . EQ . VMSMGO ) . OR .
     *       ( IVMSMG(I) . EQ . VMSMRR ) . OR .
     *       ( IVMSMG(I) . EQ . VMSMDD ) )       GO TO 7660
      END IF
C-----CHECK VEHICLE MESSAGE SYSTEM PARAMETER DECELERATION FOR ERRORS
C-----FOR:
C-----VMSMSC STOP IMMEDIATELY USING COLLISION DECELERATION
      IF ( IVMSMG(I) . EQ . VMSMSC )             THEN
C-----  PRINT VEHICLE MESSAGE SYSTEM PARAMETER
        WRITE (6,618) DVMSMP(I)
        IF ( ( DVMSMP(I) . LT . -DECCOL ) . OR .
     *       ( DVMSMP(I) . GE . 0.0D0   ) )      GO TO 7650
      END IF
C-----CHECK VEHICLE MESSAGE SYSTEM PARAMETER SPEED FOR ERRORS FOR:
C-----VMSMAN ACCELERATE OR DECELERATE TO SPEED XX USING NORMAL
C-----       ACCELERATION OR DECELERATION
C-----VMSMAM ACCELERATE OR DECELERATE TO SPEED XX USING MAXIMUM
C-----       VEHICLE ACCELERATION OR DECELERATION
      IF ( ( IVMSMG(I) . EQ . VMSMAN ) . OR .
     *     ( IVMSMG(I) . EQ . VMSMAM ) )         THEN
C-----  PRINT VEHICLE MESSAGE SYSTEM PARAMETER
        WRITE (6,619) DVMSMP(I)
        IF ( ( DVMSMP(I) . LT . 0.0D0  ) . OR .
     *       ( DVMSMP(I) . GT . VELMAX ) )       GO TO 7640
      END IF
C-----CHECK VEHICLE MESSAGE SYSTEM PARAMETER POSITION FOR ERRORS FOR:
C-----VMSMSL STOP AT LOCATION XX
      IF ( IVMSMG(I) . EQ . VMSMSL )             THEN
C-----  PRINT VEHICLE MESSAGE SYSTEM PARAMETER
        WRITE (6,620) DVMSMP(I)
        IF ( ( DVMSMP(I) . LT . 0.0D0  ) . OR .
     *       ( DVMSMP(I) . GT . POSMAX ) )       GO TO 7630
      END IF
C-----PRINT VEHICLE MESSAGE SYSTEM MESSAGE START TIME, ACTIVE TIME, AND
C-----APPROACH/INTERSECTION
      WRITE (6,621) DVMSST(I),DVMSAT(I)
      IF ( IVMSAP(I) . GE . 0 )                  THEN
        WRITE (6,622) IVMSAP(I)
      ELSE
        WRITE (6,623) -IVMSAP(I)
      END IF
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE START TIME FOR ERRORS
      IF ( ( DVMSST(I) . LT . 0.0D0  ) . OR .
     *     ( DVMSST(I) . GT . SIMTIM ) )         GO TO 7620
      IF ( ( I         . GT . 1           ) . AND .
     *     ( DVMSST(I) . LT . DVMSST(I-1) ) )    GO TO 7610
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE ACTIVE TIME FOR ERRORS
      IF ( DVMSAT(I) . LT . 0.0D0  )             GO TO 7600
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE APPROACH/PATH FOR ERRORS
      POSEND = -1.0D0
      IF ( IVMSAP(I) . GE . 0 )                  THEN
C-----  CHECK APPROACH NUMBER
        DO 1010  IAN = 1 , NIBA
        IA = LIBA(IAN)
        IF ( IVMSAP(I) . EQ . IA )               GO TO 1040
 1010   CONTINUE
        DO 1020  IAN = 1 , NOBA
        IA = LOBA(IAN)
        IF ( IVMSAP(I) . EQ . IA )               GO TO 1040
 1020   CONTINUE
        GO TO 7590
      ELSE
        IVMSLB(I) = 0
        IVMSLE(I) = 0
C-----  CHECK INTERSECTION PATH NUMBER
C-----  INTERSECTION PATH IS NOT ALLOWED FOR:
C-----  VMSMCL CHANGE LANES TO THE LEFT
C-----  VMSMCR CHANGE LANES TO THE RGHT
        IF ( ( IVMSMG(I) . EQ . VMSMCL ) . OR .
     *       ( IVMSMG(I) . EQ . VMSMCR ) )       GO TO 7580
        DO 1030  IP = 1 , NPATHS
        IF ( -IVMSAP(I) . EQ . IP )              THEN
          POSEND = LENP(IP)
          GO TO 1060
        END IF
 1030   CONTINUE
        GO TO 7570
      END IF
 1040 CONTINUE
C-----PRINT VEHICLE MESSAGE SYSTEM BEGIN AND END LANE
      WRITE (6,624) IVMSLB(I),IVMSLE(I)
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE LANES FOR ERRORS
      IF ( (IVMSLB(I) . LT . 1         ) . OR .
     *     (IVMSLB(I) . GT . NLANES(IA)) )       GO TO 7560
      IF ( (IVMSLE(I) . LT . 1         ) . OR .
     *     (IVMSLE(I) . GT . NLANES(IA)) )       GO TO 7550
      IF (  IVMSLE(I) . LT . IVMSLB(I)   )       GO TO 7540
      IF ( ( IVMSMG(I) . EQ . VMSMCL     ) . OR .
     *     ( IVMSMG(I) . EQ . VMSMCR     ) )     THEN
        IF ( NLANES(IA) . LT . 2 )               GO TO 7530
      END IF
      DO 1050  ILN = IVMSLB(I) , IVMSLE(I)
      IL = LLANES(ILN,IA)
      IF ( LGEOM(3,IL) . EQ . LGEOM(4,IL) )      THEN
        POSEND = DMAX1( POSEND,DBLE( LGEOM(2,IL) ) )
      ELSE
        POSEND = DMAX1( POSEND,DBLE( LGEOM(4,IL) ) )
      END IF
 1050 CONTINUE
 1060 CONTINUE
C-----PRINT VEHICLE MESSAGE SYSTEM POSITION
      WRITE (6,625) DVMSPB(I),DVMSPE(I)
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE POSITIONS FOR ERRORS
      IF ( (DVMSPB(I) . LT . 0.0D0    ) . OR .
     *     (DVMSPB(I) . GT . POSEND   ) )        GO TO 7520
      IF ( (DVMSPE(I) . LT . DVMSPB(I)) . OR .
     *     (DVMSPE(I) . GT . POSEND   ) )        GO TO 7510
C-----PRINT VEHICLE MESSAGE SYSTEM VEHICLE NUMBER
      IF ( (IVMSMT(I) . EQ . VMSTDI) . OR .
     *     (IVMSMT(I) . EQ . VMSTVI) )           THEN
        IF ( IVMSVN(I) . EQ . 0 )                THEN
          WRITE (6,626)
        ELSE
          WRITE (6,627)  IVMSVN(I)
        END IF
      ELSE
        IVMSVN(I) = 0
      END IF
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE VEHICLE NUMBER FOR ERRORS
      IF ( IVMSVN(I) . LT . 0         )          GO TO 7500
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE DISTRIBUTION NAME FOR ERRORS
      IF ( CVMSDN(I) . EQ . 'CONSTAN' )          GO TO 1065
      IF ( CVMSDN(I) . EQ . 'ERLANG'  )          GO TO 1065
      IF ( CVMSDN(I) . EQ . 'GAMMA'   )          GO TO 1065
      IF ( CVMSDN(I) . EQ . 'LOGNRML' )          GO TO 1065
      IF ( CVMSDN(I) . EQ . 'NEGEXP'  )          GO TO 1065
      IF ( CVMSDN(I) . EQ . 'SNEGEXP' )          GO TO 1065
      IF ( CVMSDN(I) . EQ . 'UNIFORM' )          GO TO 1065
      GO TO 7490
 1065 CONTINUE
C-----PRINT VEHICLE MESSAGE SYSTEM MESSAGE DISTRIBUTION NAME AND MEAN
      WRITE (6,628)  CVMSDN(I),DVMSDM(I)
      IF ( CVMSDN(I) . EQ . 'CONSTAN' )          GO TO 1067
C-----PRINT VEHICLE MESSAGE SYSTEM MESSAGE DISTRIBUTION PARAMETER
      WRITE (6,629)  DVMSDP(I)
 1067 CONTINUE
C-----CHECK VEHICLE MESSAGE SYSTEM MESSAGE DISTRIBUTION MEAN FOR ERRORS
      IF ( DVMSDM(I) . LT . 0.0D0     )          GO TO 7480
      IF ( DVMSDP(I) . LT . 0.0D0     )          GO TO 7400
C-----CONVERT SPEED FROM MPH TO FPS
      IF ( (IVMSMG(I) . EQ . VMSMAN) . OR .
     *     (IVMSMG(I) . EQ . VMSMAM) )           THEN
        DVMSMP(I) = DVMSMP(I)*MPH2FS
      END IF
 1070 CONTINUE
C-----SET VEHICLE MESSAGE SYSTEM PRIORITIES
      DO 2020  I = 1 , NVMSMV
      DO 2010  J = 1 , NVMSMV
                    IF ( IVMSPR(J) . EQ . I )    GO TO 2020
 2010 CONTINUE
      GO TO 7470
 2020 CONTINUE
      RETURN
C-----PROCESS THE INPUT ERRORS AND STOP
 7690 CONTINUE
      WRITE (ERRMSG,769) I
      CALL  PRTERR  ( 'STOP 769 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  769
 7680 CONTINUE
      WRITE (ERRMSG,768) I,IVMSMT(I),VMSTDD,VMSTDI,VMSTVI
      CALL  PRTERR  ( 'STOP 768 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  768
 7670 CONTINUE
      WRITE (ERRMSG,767) I,IVMSMG(I),VMSMAN,VMSMAM,VMSMSI,VMSMSL,VMSMSM,
     *                               VMSMSC,VMSMCL,VMSMCR,VMSMGO,VMSMRR,
     *                               VMSMDD
      CALL  PRTERR  ( 'STOP 767 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                             )
      STOP  767 
 7660 CONTINUE
      WRITE (ERRMSG,766) I,IVMSMG(I),VMSMCL,VMSMCR,VMSMGO,VMSMRR,VMSMDD
      CALL  PRTERR  ( 'STOP 766 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  766
 7650 CONTINUE
      WRITE (ERRMSG,765) I,IVMSMG(I),DVMSMP(I),-DECCOL
      CALL  PRTERR  ( 'STOP 765 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  765
 7640 CONTINUE
      WRITE (ERRMSG,764) I,IVMSMG(I),DVMSMP(I),VELMAX
      CALL  PRTERR  ( 'STOP 764 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  764
 7630 CONTINUE
      WRITE (ERRMSG,763) I,IVMSMG(I),DVMSMP(I),POSMAX
      CALL  PRTERR  ( 'STOP 763 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  763
 7620 CONTINUE
      WRITE (ERRMSG,762) I,DVMSST(I),SIMTIM
      CALL  PRTERR  ( 'STOP 762 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  762
 7610 CONTINUE
      WRITE (ERRMSG,761) I,DVMSST(I),DVMSST(I-1)
      CALL  PRTERR  ( 'STOP 761 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  761
 7600 CONTINUE
      WRITE (ERRMSG,760) I,DVMSAT(I)
      CALL  PRTERR  ( 'STOP 760 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  760
 7590 CONTINUE
      WRITE (ERRMSG,759) I,IVMSAP(I)
      CALL  PRTERR  ( 'STOP 759 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  759
 7580 CONTINUE
      WRITE (ERRMSG,758) I,-IVMSAP(I)
      CALL  PRTERR  ( 'STOP 758 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  758
 7570 CONTINUE
      WRITE (ERRMSG,757) I,-IVMSAP(I)
      CALL  PRTERR  ( 'STOP 757 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  757
 7560 CONTINUE
      WRITE (ERRMSG,756) I,IVMSAP(I),IVMSLB(I),NLANES(IA)
      CALL  PRTERR  ( 'STOP 756 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  756
 7550 CONTINUE
      WRITE (ERRMSG,755) I,IVMSAP(I),IVMSLE(I),NLANES(IA)
      CALL  PRTERR  ( 'STOP 755 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  755
 7540 CONTINUE
      WRITE (ERRMSG,754) I,IVMSAP(I),IVMSLE(I),IVMSLB(I)
      CALL  PRTERR  ( 'STOP 754 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  754
 7530 CONTINUE
      WRITE (ERRMSG,753) I,IVMSAP(I),NLANES(IA)
      CALL  PRTERR  ( 'STOP 753 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  753
 7520 CONTINUE
      WRITE (ERRMSG,752) I,DVMSPB(I),POSEND
      CALL  PRTERR  ( 'STOP 752 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  752
 7510 CONTINUE
      WRITE (ERRMSG,751) I,DVMSPE(I),DVMSPB(I),POSEND
      CALL  PRTERR  ( 'STOP 751 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  751
 7500 CONTINUE
      WRITE (ERRMSG,750) I,IVMSVN(I)
      CALL  PRTERR  ( 'STOP 750 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  750
 7490 CONTINUE
      WRITE (ERRMSG,749) I,CVMSDN(I)
      CALL  PRTERR  ( 'STOP 749 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  749
 7480 CONTINUE
      WRITE (ERRMSG,748) I,DVMSDM(I)
      CALL  PRTERR  ( 'STOP 748 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  748
 7470 CONTINUE
      WRITE (ERRMSG,747) I
      CALL  PRTERR  ( 'STOP 747 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  747
 7400 CONTINUE
      WRITE (ERRMSG,740) I,DVMSDP(I)
      CALL  PRTERR  ( 'STOP 740 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'RVMSM'                              )
      STOP  740
      END                                                               RVMSM
