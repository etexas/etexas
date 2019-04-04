      SUBROUTINE INITDT() BIND(C, name='initModel')
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
      INTEGER           DEG,I,IARC,ILINE,ILNB,ILOOP,ISDRC,J,JA,JTYPLD,K,
     *                  LL,N,NC,NC1,NCPV(PARNSP),NOUT,NUMREP,P
C8    INTEGER           ISIG8
C+    INTEGER           ISIGP
      INTEGER*4         SSAMAX,SSAMAY,SSAMIX,SSAMIY
      REAL*4            ADD,ADDAZ,SSAMSC,XPOS,YPOS,ZERO
      DOUBLE PRECISION  POSPTH,TIMVAL
      DATA NCPV / PARNSP*-1 /
      DATA ZERO / 0.000001D0 /

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
C-----SUBROUTINE INITDT INITIALIZES THE PARAMETERS FOR THE SIMULATION
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
      CALL  CONFFL  ( PARNSP,PAR,PARVAL,NCPV,'simpro' )
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
C-----Harmonia mod--------------------------------------------BAMauldon 10/15/2013      
C-----IF ( REPRUN . NE . INO )                   THEN
C-----NC = ILNB( REPRUN )
C-----IF ( NC . GT . 0 )                       THEN
C-----WRITE (IFORM,'(2H(I,I2.2,1H))') NC
C-----READ (REPRUN,IFORM,ERR=8930) NUMREP
C-----IF ( ( NUMREP . LT . 1              ) . OR .
C-----*         ( NUMREP . GT . MIN0( 99,NRP ) ) )GO TO 8940
C-----WRITE (STITLE(56:59),'(2H R,I2.2)') NUMREP
C-----END IF
C-----END IF
C-----End Harmonia mod--------------------------------------------------------------
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
      END                                                               INITDT
C
C
C
      SUBROUTINE CONFFL(PARNUM,PAR,PARVAL,NCPV,PROG)
C
C ----- THIS SUBROUTINE GETS POSITIONAL AND KEYWORD (NOT VAX VMS)
C ----- PARAMETERS FROM THE COMMAND FILE.
C ----- POSITIONAL PARAMETERS ARE NUMBERED LEFT TO RIGHT, WITH
C ----- FIRST PARAMETER TO RIGHT OF PROGRAM NAME AS 1.
C ----- KEYWORD PARAMETERS HAVE THE FORM: KEYWORD=PARAMETER .
C ----- KEYWORD PARAMETERS ARE NOT CONSIDERED WHEN NUMBERING
C ----- POSITIONAL PARAMETERS.
C
C   PARNUM - MAXIMUM NUMBER OF PARAMETERS
C   PAR    - DIMENSIONED CHARACTER ARRAY WITH KEYWORDS
C   PARVAL - DIMENSIONED CHARACTER ARRAY TO RETURN THE PARAMETER VALUES
C   NCPV   - NUMBER OF CHARACTERS IN RETURNED <PARVAL>
C          -1 - PARAMETER NOT FOUND
C           0 - KEYWORD FOUND WITHOUT PARAMETER TO RIGHT OF =
C          >0 - NUMBER OF CHARACTERS
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER           PARNUM           ,NCPV(PARNUM)
      CHARACTER*(*)            PAR(PARNUM)
      CHARACTER*(*)                PARVAL(PARNUM)
      CHARACTER*(*)                            PROG
      CHARACTER*1 SDSEP
C?    PARAMETER  (SDSEP='/')
      PARAMETER  (SDSEP='\')                                            CCODE=C#
C}    PARAMETER  (SDSEP='/')
C%    PARAMETER  (SDSEP=':')
      CHARACTER*10 CLPAR,CLPARU
      CHARACTER*60 VALPAR,CLPARM
      INTEGER      I,ILNB,J,K,NCCL,NCEQ,NCP,NCPAR,NP,NPP,NPT
C{    INTEGER*4 IARGC
C
C   GET THE SIZE OF THE PROGRAM NAME STRING PASSED IN.
C   THIS PROGRAM NAME WILL BE THE NAME OF THE CONFIGURATION FILE TOO.
C
      NCP=ILNB( PROG )
      IF(NCP.EQ.0)THEN
        WRITE(*,'(A)')'Error in CONFFL: program name not specified'
        STOP 900
      END IF
C
C
C   FILL PARNUM ARRAY WITH DEFAULT RETURN VALUES OF -1
C
      DO 10 I=1,PARNUM
      NCPV(I)=-1
   10 CONTINUE
C
C
C ----- THE LOGICAL NAME "NCLP" MUST BE SET TO NUMBER OF
C ----- COMMAND LINE PARAMETERS
C
C
      OPEN(9,FILE=PROG(1:NCP)//'.par',STATUS='OLD',
     *       ACTION='READ',ERR=901)
      NPT=PARNUM
      NPP=0
      DO 100 NP=1,NPT
C
C ----- FOR VAX VMS, THE LOGICAL NAME "CLPi" MUST BE SET
C ----- FOR EACH OF THE i COMMAND LINE PARAMETERS
C
      READ(9,'(A)',END=102,ERR=902)CLPARM
      NCCL=ILNB( CLPARM )
      IF(NCCL.EQ.0)GO TO 100
      CLPARM=CLPARM(1:NCCL)
      CLPAR=CLPARM
      CLPARU=CLPAR
      CALL TOUPR(CLPARU)
      VALPAR=' '
      DO 40 J=1,PARNUM
      NCP=ILNB( PAR(J) )
      IF((NCCL.EQ.NCP) .AND. (CLPARU(1:NCCL).EQ.PAR(J)(1:NCP)))THEN
C
C ----- FOUND KEYWORD WITHOUT =
C
        NCPV(J)=0
        GO TO 100
      END IF
   40 CONTINUE
      NCEQ=INDEX(CLPARM,'=')
C
C ----- DOS REMOVES THE = BEFORE SENDING COMMAND LINE PARAMETERS
C ----- TO AN APPLICATIOM
C ----- + IS USED FOR DOS TO SEPARATE KEYWORDS FROM PARAMETERS
C
      IF(NCEQ.EQ.0)NCEQ=INDEX(CLPARM,'+')
      IF(NCEQ.EQ.1)THEN
        WRITE(*,'(A,I2,A)')'Error in CONFFL for '//
     *                     PROG(1:ILNB( PROG ))//
     *                     ': parameter ',NP,' = "'//
     *                     CLPARM(1:ILNB( CLPARM ))//
     *                     '" starts with "'//CLPARM(1:1)//'"'
        STOP 903
      END IF
      IF(NCEQ.GT.0)THEN
        CLPAR=CLPARM(1:NCEQ-1)
        NCPAR=ILNB( CLPAR )
        CLPARU=CLPAR
        CALL TOUPR(CLPARU)
        NCPAR=MAX0(NCPAR,1)
        IF(NCEQ+1.LE.LEN( CLPARM ))VALPAR=CLPARM(NCEQ+1:)
C
C ----- LOOK FOR KEYWORD MATCH
C
        DO 50 J=1,PARNUM
        NCP=ILNB( PAR(J) )
        IF((NCPAR.EQ.NCP) .AND. (CLPARU(1:NCPAR).EQ.PAR(J)(1:NCP)))THEN
C
C ----- MATCHES ONE OF THE KEYWORDS
C
          PARVAL(J)=VALPAR
          IF(PAR(J).EQ.'GDVS00' .OR.
     *       PAR(J).EQ.'SYS_DAT'.OR.
     *       PAR(J).EQ.'WRK_DIR')THEN
            K=ILNB( PARVAL(J) )
            IF(PARVAL(J)(K:K).NE.SDSEP)PARVAL(J)=PARVAL(J)(1:K)//SDSEP
          END IF
          NCPV(J)=ILNB( PARVAL(J) )
          GO TO 100
        END IF
   50   CONTINUE
        WRITE(*,'(A,I2,A)')'Error in CONFFL for '//
     *                     PROG(1:ILNB( PROG ))//
     *                     ': parameter ',NP,' = "'//
     *                     CLPARM(1:ILNB( CLPARM ))//
     *                     '" - keyword = "'//CLPARU(1:NCPAR)//
     *                     '" is not valid'
        STOP 904
      END IF
C
C ----- POSITIONAL PARAMETER
C
      NPP=NPP+1
      PARVAL(NPP)=CLPARM
      NCPV(NPP)=ILNB( PARVAL(NPP) )
  100 CONTINUE
  102 CONTINUE
      CLOSE(9)
      RETURN
  901 CONTINUE
      WRITE(*,'(A)')'Error in CONFFL opening '//
     *              'command line parameter file '//
     *              PROG(1:NCP)//'.par'
      STOP 901
  902 CONTINUE
      WRITE(*,'(A,I2,A)')'Error in CONFFL reading line ',NP,
     *                   ' from command line parameter file '//
     *                   PROG(1:NCP)//'.par'
      STOP 902
      END                                                               CONFFL
C
C
C
      SUBROUTINE INITEX() BIND(C, name='initExecution')
C
C ----- THIS SUBROUTINE INITIALIZES THE EXECUTION LOOP AND SHOULD
C ----- BE CALLED ONCE BEFORE SIMDT. THIS CODE WAS MOVED FROM THE 
C ----- EXEC SUBROUTINE.

      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'INTER'
      INCLUDE 'PHASES'
      INCLUDE 'QUE'
      INCLUDE 'USER'
      INTEGER           IPRCNT
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
      END                                                               INITEX
C
C
C
      SUBROUTINE GTCONS(PINTS) BIND(C, name='getConstantsData')
C
C ----- THIS SUBROUTINE IS USED TO OBTAIN CONSTANT DATA FROM SIMPRO
C ----- USING AN INTEGER ARRAY.
C
      IMPLICIT          NONE                                            CCODE=C.

      INCLUDE 'PARAMS'

      INTEGER      PINTS(*)

      PINTS(1) = NCM
      PINTS(2) = NRG
      PINTS(3) = NON
      PINTS(4) = NIL

      RETURN
      END                                                               GTCONS
C
C
C
       SUBROUTINE GTMAP(PINTS, PIBLN, PISNA, PLIBAR, PLOBAR, PLCONT,
     *                 PLGEOM, PLTURN, PLWID, PBLX, PBLY, PELX, PELY,
     *				   PISLIM, PNLANES) 
     *                 BIND(C, name='getMAPData')
C
C ----- THIS SUBROUTINE IS USED TO OBTAIN MAP DATA FROM SIMPRO
C
      IMPLICIT          NONE                                            CCODE=C.

      INCLUDE 'PARAMS'
      INCLUDE 'INTER'
      INCLUDE 'TYPES'
      INCLUDE 'LANE'
      INCLUDE 'LNPATH'
      INCLUDE 'APPRO'

      INTEGER               PINTS(2)
      INTEGER               PILTYP(50)
      INTEGER               PIBLN(50)
      INTEGER               PISNA(50)
      INTEGER               PLIBAR(16)
      INTEGER               PLOBAR(16)
      INTEGER               PLCONT(50)
      INTEGER               PLGEOM(4, 50)
      INTEGER               PLTURN(50)
      INTEGER               PLWID(50)
      INTEGER				PISLIM(NLA)
      INTEGER               PNLANES(NAP)
      INTEGER               PNAL
      INTEGER				I
      DOUBLE PRECISION      PBLX(50)
      DOUBLE PRECISION      PBLY(50)
      DOUBLE PRECISION      PELX(50)
      DOUBLE PRECISION      PELY(50)

      PINTS(1) = NRLAN
      PINTS(2) = NIBA
      PIBLN = IBLN
      PISNA = ISNA
      PLIBAR = LIBAR
      PLOBAR = LOBAR
      PLCONT = LCONTR
      PLGEOM = LGEOM
      PLTURN = LTURN
      PLWID = LWID
      PBLX = BASELX
      PBLY = BASELY
      PELX = ENDLNX
      PELY = ENDLNY
      
      DO 1770 I=1, NLA
      PISLIM(I) = ISLIM(I)

 1770 CONTINUE
 
      DO 1780 I=1, NAP
      PNLANES(I) = NLANES(I)
 1780 CONTINUE

      RETURN
      END                                                               GTMAP
C
C
C
      SUBROUTINE GTMAP2(PLLANES, PLIBA) 
     * BIND(C, name='getMoreMAPData')
      IMPLICIT          NONE                                            CCODE=C.
      
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'INTER'
      
      INTEGER PLLANES(NAL, NAP)
      INTEGER PLIBA(NIBA)
      INTEGER I, J
      
      DO 1010 I=1, NAL
        DO 1020 J=1, NAP
          PLLANES(I,J) = LLANES(I, J)
 1020 CONTINUE
 1010 CONTINUE
 
      DO 1030 I=1, NIBA
        PLIBA(I) = LIBA(I)
 1030 CONTINUE
      
      RETURN
      END                                                               GTMAP2
C
C
C
      SUBROUTINE GTSPAT(PDBLS, PTCAMS, PINTS, PICAMP, PINTER, PISISE)
     *                  BIND(C, name='getSPATData')
C
C ----- THIS SUBROUTINE IS USED TO OBTAIN SIGNAL DATA FROM SIMPRO
C
      IMPLICIT          NONE                                            CCODE=C.

      INCLUDE 'PARAMS'
      INCLUDE 'INTER'
      INCLUDE 'SIGCAM'

      INTEGER               PINTS(8)
      INTEGER               PICAMP(72)
      INTEGER               PINTER(4)
      INTEGER               PISISE((72+2+16+16), 25)
      DOUBLE PRECISION      PDBLS(2)
      DOUBLE PRECISION      PTCAMS(72)

      PINTS(1) = ICONTR
      PINTS(2) = NIBL
      PINTS(3) = IARRPH
      PINTS(4) = ICAMPC
      PINTS(5) = ICAMPO
      PINTS(6) = ICPHAS
      PINTS(7) = IGO
      PINTS(8) = NCAMSP

      PDBLS(1) = TP
      PDBLS(2) = TR

      PTCAMS = TCAMSP

      PICAMP = ICAMPH
      PINTER = INTER
      PISISE = ISISET

      RETURN
      END                                                               GTSPAT
C
C
C
      SUBROUTINE INJMSG ( JVMSMT,JVMSMG,RVMSMP,RVMSST,RVMSAT,JVMSAP,
     *                    JVMSLB,JVMSLE,RVMSPB,RVMSPE,JVMSVN,JVMSDT,
     *                    RVMSDM,RVMSDP                              )
C
C-----SUBROUTINE INJMSG INSERTS VEHICLE MESSAGE SYSTEM MESSAGES
C
C-----JVMSMT INTEGER     VMS MESSAGE TYPE
C-----JVMSMG INTEGER     VMS MESSAGE
C-----RVMSMP DOUBLE      VMS MESSAGE PARAMETER (MPH FOR SPEED)
C-----RVMSST DOUBLE      VMS MESSAGE STARTING TIME
C-----RVMSAT DOUBLE      VMS MESSAGE ACTIVE TIME
C-----JVMSAP INTEGER     VMS MESSAGE APPROACH (+) OR INTERSECTION PATH (-)
C-----JVMSLB INTEGER     VMS MESSAGE LANE BEGIN
C-----JVMSLE INTEGER     VMS MESSAGE LANE END
C-----RVMSPB DOUBLE      VMS MESSAGE POSITION BEGIN
C-----RVMSPE DOUBLE      VMS MESSAGE POSITION END
C-----JVMSVN INTEGER     VMS MESSAGE VEHICLE NUMBER (O=ALL)
C-----JVMSDT INTEGER     VMS MESSAGE REACTION TIME DISTRIBUTION TYPE
C-----RVMSDM DOUBLE      VMS MESSAGE REACTION TIME DISTRIBUTION MEAN
C-----RVMSDP DOUBLE      VMS MESSAGE REACTION TIME DISTRIBUTION PARAMETER
C
      IMPLICIT NONE                                                     CCODE=C.
      CHARACTER*7       SVMSDN
      INTEGER, VALUE :: JVMSAP,JVMSLB,JVMSLE,JVMSMG,
     *                  JVMSMT,JVMSVN,JVMSDT
      DOUBLE PRECISION, VALUE :: RVMSAT,RVMSDM,RVMSDP,RVMSMP,RVMSPB,
     *                  RVMSPE,RVMSST

      SVMSDN = 'CONSTAN'
	  IF ( JVMSDT .EQ. 1 )             SVMSDN = 'CONSTAN'
	  IF ( JVMSDT .EQ. 2 )             SVMSDN = 'ERLANG'
	  IF ( JVMSDT .EQ. 3 )             SVMSDN = 'GAMMA'
	  IF ( JVMSDT .EQ. 4 )             SVMSDN = 'LOGNRML'
	  IF ( JVMSDT .EQ. 5 )             SVMSDN = 'NEGEXP'
	  IF ( JVMSDT .EQ. 6 )             SVMSDN = 'SNEGEXP'
	  IF ( JVMSDT .EQ. 7 )             SVMSDN = 'UNIFORM'
      CALL IVMSG( JVMSMT,JVMSMG,RVMSMP,RVMSST,RVMSAT,JVMSAP,
     *            JVMSLB,JVMSLE,RVMSPB,RVMSPE,JVMSVN,SVMSDN,
     *            RVMSDM,RVMSDP)

      RETURN
      END                                                               INJMSG
      
C
C
C
      SUBROUTINE GTVEHC ( PFBX, PFBY, PIQ, PINUSE, PCLASS, PLANE,
     * PVEL, PLEN, PHEAD, PANGLE, PWID, 
     * PACC, PDECBR, PHEIGHT, PVTYPE) BIND(C, name='getVehicleData')
C
C-----SUBROUTINE GTVEHC GETS THE ARRAY OF VEHICLEs
C
C-----NVC       99 NUMBER OF VEHICLE CLASSES
C-----NVE      500 NUMBER OF VEHICLES IN THE SYSTEM AT ONE TIME
C
C-----DATA FOR A VEHICLE
C-----FBX,FBY - COORDINATES OF FRONT BUMPER
C-----LENLP - DISTANCE FROM FRONT TO REAR BUMPER ALONG LANE AND/OR PATH
C-----LNCHGD - LANE CHANGE LATERAL DISTANCE
C-----FBS - STATUS OF FRONT BUMPER
C-----      2 - ON INBOUND LANE
C-----      3 - ON FIRST OR ONLY PATH
C-----     -3 - ON SECOND PATH
C-----      4 - ON OUTBOUND LANE
C-----     -4 - ON INTERNAL LANE
C-----INUSE - IS THIS STRUCTURE MEMBER CURRENTLY IN USE
C-----LTPDST - 0 LEFT TURN PULLOUT NOT ACTIVE
C-----         >0 LEFT TURN PULLOUT ACTIVE, LTPDST IS LENGTH OF LANE
C-----CLASS - VEHICLE CLASS
C-----UNITS - NUMBER OF UNITS FOR THIS VEHICLE
C-----LNCHGC - IS LANE CHANGE CLEANUP IN PROGRESS
C-----LNCHGC - IS LANE CHANGE IN PROGRESS
C-----      TYPE VEH
C-----        SEQUENCE
C-----        DOUBLE PRECISION FBX
C-----        DOUBLE PRECISION FBY
C-----        DOUBLE PRECISION LENLP
C-----        DOUBLE PRECISION LNCHGD
C-----        INTEGER FBS
C-----        INTEGER INUSE
C-----        INTEGER LTPDST
C-----        INTEGER PLACEHOLDER
C-----        INTEGER*2 CLASS
C-----        INTEGER*2 UNITS
C-----        LOGICAL*2 LNCHGC
C-----        LOGICAL*2 LNCHG
C-----        TYPE (VEHUNI) :: UNIT(MNU)
C-----        TYPE (LNPAVE) :: LNPA
C-----      END TYPE VEH
      IMPLICIT NONE                                                     CCODE=C.

      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'QUE'
      INCLUDE 'CLASS'

      INTEGER           I, PIQ(0:NVEP1), PINUSE(NVE), PLANE(NVE) 
      INTEGER*2         PCLASS(NVE)
      DOUBLE PRECISION  PFBX(NVE), PFBY(NVE), PVEL(NVE), PLEN(NVE), 
     * PHEAD(NVE), PANGLE(NVE), PWID(NVE), PACC(NVE), PDECBR
      INTEGER           IGETLP, PHEIGHT(NVE), PVTYPE(NVE)		

      PDECBR = DECBRK
      PIQ = IQ
      PVEL = IVEL
      PACC = IACC
      PHEAD = HEADNG
      PANGLE = STEERA

C
C-----LOOP OVER EACH VEHICLE IN THE ARRAY AND TRANSLATE TO RETURN ARRAYS
C
      DO 1010 I=1, NVE
C
C-----COPY VALUES FROM ARRAY INDEX TO OUTPUT ARRAYS
C
          PFBX(I) = VEHCLE(I)%FBX
          PFBY(I) = VEHCLE(I)%FBY
          PINUSE(I) = VEHCLE(I)%INUSE
          PCLASS(I) = VEHCLE(I)%CLASS
          PLEN(I) = VEHCLA(IVEHCL(I))%LEN
          PWID(I) = WIDV(IVEHCL(I))
C         PLANE(I) = VEHCLE(I)%LNPA%ILP(1,1)
		  PLANE(I) = LPRES(I)
		  PHEIGHT(I) = VEHCLA(IVEHCL(I))%HEIGHT
		  IF ( VEHCLA(IVEHCL(I))%TYPE(1:1) .EQ. "AUTO" )	PVTYPE(I) = 0
		  IF ( VEHCLA(IVEHCL(I))%TYPE .EQ. "SU2" )			PVTYPE(I) = 1
		  IF ( VEHCLA(IVEHCL(I))%TYPE .EQ. "3S-2" )			PVTYPE(I) = 2
C		  WRITE(0,*) VEHCLA(IVEHCL(I))%TYPE
		  
 1010 CONTINUE

      RETURN
      END                                                               GTVEHC
C
C
C
      SUBROUTINE GTCLAS ( PCLSTR )
C
C-----SUBROUTINE GTCLAS GETS THE ARRAY OF VEHICLE CLASS STRINGS
C
      IMPLICIT NONE                                                     CCODE=C.

      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'

      INTEGER                I
      CHARACTER*8            PCLSTR(NVC)
C
C-----LOOP OVER EACH VEHICLE IN THE ARRAY AND TRANSLATE TO RETURN ARRAYS
C
      DO 1010 I=1, NVEHCL
C
C-----COPY VALUES FROM ARRAY INDEX TO OUTPUT ARRAYS
C
          PCLSTR(I) = VEHCLA(I)%TYPE

 1010 CONTINUE

      RETURN
      END                                                               GTCLAS
C
C
C
      SUBROUTINE GTVINF ( PVID, PAORP, PLNBEG, PLNEND, PPSBEG, PPSEND)
C
C-----SUBROUTINE GTVINF RETURNS INFORMATION FOR A SPECIFIC VEHICLE
C
      IMPLICIT NONE                                                     CCODE=C.

      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INCLUDE 'LANE'

      INTEGER           PVID, PAORP, PLNBEG, PLNEND
      DOUBLE PRECISION  PPSBEG, PPSEND
      INTEGER           IGETLP

C
C-----FIND THE LANE INFORMATION FOR THE CURRENT VEHICLE
C

      PLNBEG = IGETLP(VEHCLE(PVID)%LNPA, 0, 0)
      PLNEND = PLNBEG
      PAORP = ISNA(PLNBEG)
      PPSBEG = LGEOM(PLNBEG, 1)
      PPSEND = LGEOM(PLNBEG, 4)

      RETURN
      END                                                               GTVINF
C
C
C
      SUBROUTINE GTDTCT ( PPULSE, PLOOPS, PNLDLN, PSTRT, PSTOP)
     *          BIND(C, name='getDetectorGeometry')
C
C-----SUBROUTINE GTDTCT RETURNS DETECTOR GEOMETRY INFORMATION
C
      USE ISO_C_BINDING 
      IMPLICIT NONE                                                     CCODE=C.

      INCLUDE 'PARAMS'
      INCLUDE 'LOOPS'
      INCLUDE 'CHARAC'
      
      INTEGER           I
      
      LOGICAL           PPULSE(NLS)
      INTEGER           PLOOPS(NLS)
      INTEGER           PNLDLN(NLS)
      INTEGER           PSTRT(NLS)
      INTEGER           PSTOP(NLS)
      
      DO 1010 I=1, NLS
C
C-----DETERMINE LOOP DETECTOR TYPE
C
      IF ( ITYPLD(I) . EQ . IPULS )                  THEN
        PPULSE(I) = .TRUE.
      ELSE
        PPULSE(I) = .FALSE.
      END IF

 1010 CONTINUE

      
      
      PLOOPS = LLOOPS
      PNLDLN = NLDLN
      PSTRT = LDSTRT
      PSTOP = LDSTOP
C
C-----FIND THE DETECTOR INFORMATION
C

      RETURN
      END                                                               GTDTCT
C
C
C
      SUBROUTINE GTDTST ( PLOOPS, PCLER, PCROS, PTRIP, PSTAT, PNLDLN, 
     *     PSTRT, PSTOP, PITYPE, PLLDLN, PLDA)
     *          BIND(C, name='getDetectorInfo')
C
C-----SUBROUTINE GTDTST RETURNS DETECTOR LOCATION AND TYPE INFORMATION
C
      USE ISO_C_BINDING 
      IMPLICIT NONE                                                     CCODE=C.

      INCLUDE 'PARAMS'
      INCLUDE 'LOOPS'
      INCLUDE 'CHARAC'
      INCLUDE 'LANE'
      INCLUDE 'INTER'
      
      INTEGER  I
      
      INTEGER (C_INT), INTENT(OUT) :: PLOOPS(NLS)
      LOGICAL (C_BOOL), INTENT(OUT) :: PCLER(NLS)
      LOGICAL (C_BOOL), INTENT(OUT) :: PCROS(NLS)
      LOGICAL (C_BOOL), INTENT(OUT) :: PTRIP(NLS)
      INTEGER (C_INT), INTENT(OUT) :: PSTAT(NLS)
      INTEGER (C_INT), INTENT(OUT) :: PNLDLN(NLS)
      INTEGER (C_INT), INTENT(OUT) :: PSTRT(NLS)
      INTEGER (C_INT), INTENT(OUT) :: PSTOP(NLS)
      LOGICAL (C_BOOL), INTENT(OUT) :: PITYPE(NLS)
      INTEGER (C_INT), INTENT(OUT) :: PLLDLN(NAL, NLS)
      INTEGER (C_INT), INTENT(OUT) :: PLDA(NLS)

      
      PLOOPS = LLOOPS
      PCLER = LDCLER
      PCROS = LDCROS
      PTRIP = LDTRIP
      PSTAT = LDSTAT
      
      PNLDLN = NLDLN
      PSTRT = LDSTRT
      PSTOP = LDSTOP
      
      PLLDLN = LLDLN
      PLDA = LDA
      
      DO 20 I=1, NLS
C
C-----DETERMINE LOOP DETECTOR TYPE
C
      IF ( ITYPLD(I) . EQ . IPULS )                  THEN
        PITYPE(I) = .TRUE.
      ELSE
        PITYPE(I) = .FALSE.
      END IF

 20   CONTINUE

      RETURN
      END                                                               GTDTST
C
C
C
      SUBROUTINE HLDSIG ( PSEC)
     *          BIND(C, name='holdSignalChange')
C
C-----SUBROUTINE HLDSIG HOLDS THE SIGNAL CHANGE FOR A GIVEN TIME PERIOD
C
C----- PSEC - THE NUMBER OF SECONDS TO HOLD THE SIGNAL CHANGE IN ITS
C-----        CURRENT STATE
      IMPLICIT NONE                                                     CCODE=C.

      INCLUDE 'PARAMS'
      INCLUDE 'SIGCAM'

      DOUBLE PRECISION,VALUE :: PSEC

      TR = TR + PSEC

      RETURN
      END                                                               HLDSIG
C
C
C
      SUBROUTINE CHGSIG ( PSEC)
     *          BIND(C, name='changeSignal')
C
C-----SUBROUTINE CHGSIG CHANGE THE SIGNAL IN THE SPECIFIED TIME PERIOD
C
C----- PSEC - THE NUMBER OF SECONDS TO WAIT TO CHANGE THE SIGNAL TO ITS
C-----        NEXT STATE
      IMPLICIT NONE                                                     CCODE=C.

      INCLUDE 'PARAMS'
      INCLUDE 'SIGCAM'

      DOUBLE PRECISION,VALUE :: PSEC

      TR = PSEC

      RETURN
      END                                                               CHGSIG
C
C
C
      SUBROUTINE GTQUE ( PLQUE, PMQUE)
     *          BIND(C, name='getQueueLengths')
C
C-----SUBROUTINE GTQUE GET QUEUE LENGTH STATISTICS
C
      USE ISO_C_BINDING 
      IMPLICIT NONE                                                     CCODE=C.

      INCLUDE 'PARAMS'
      INCLUDE 'SUMST2'

      INTEGER (C_INT), INTENT(OUT) :: PLQUE(NIS, NAL)
      INTEGER (C_INT), INTENT(OUT) :: PMQUE(NIS, NAL)

      PLQUE = LQUEUE
      PMQUE = MQUEUE

      RETURN
      END                                                               CHGSIG
C      
C      
C
      SUBROUTINE GT1202 ( DETEC, PNPEDS, PEDETT, VEHDET, VEHSTA)
     *          BIND(C, name='getNTCIP1202')

C-----THIS SUBROUTINE IS TO RETRIEVE NTCIP1202 MESSAGES FROM SIMPRO
C
      IMPLICIT NONE
      
      INCLUDE 'PARAMS'
      INCLUDE 'PHASES' 
      INCLUDE 'INTER' 
      INCLUDE 'LOOPS' 
      
      INTEGER              DETEC(1)
      INTEGER              PNPEDS(1)
      LOGICAL              PEDETT(NPN)
      INTEGER              VEHDET(5, NLS)
      INTEGER              VEHSTA(1, NLS)
      


              
C      DETEC = NLOOPS
C      PNPEDS = NPEDS
C      
C      PEDETT = PEDS
C      
C
C      VEHDET(1) = LDCLEX
C      VEHDET(2) = LDPHCL
C      VEHDET(4) = LDDELY
C      VEHDET(5) = LDEXTD
      
C      VEHSTA(1) = LDTRIP
      
      
      
      RETURN
      END              
      
      SUBROUTINE GTPHASE (DETEC, PNPEDS, PHASET, PHASEG, OVRGRP)
     *      BIND(C, name='getNTCIPPhaseData')
      
      IMPLICIT NONE
      
      INCLUDE               'PARAMS'
      INCLUDE               'PHASES'
      INCLUDE               'INTER'
      INCLUDE               'SIGCAM'
      INCLUDE               'TXDSIG'               
     
      INTEGER               DETEC(1)
      INTEGER               PNPEDS(1)
      INTEGER            PHASET(23, 255)
      INTEGER              PHASEG(9, 255)
      INTEGER              OVRGRP(1, 255)          
          
          
C      DETEC = NLOOPS
C      PNPEDS = NPEDS
C      
      
C      PHASET(1) = IP
C      PHASET(2) = TWK
C      PHASET(3) = TPC
C      PHASET(4) = TMI
C      PHASET(5) = TVI
C      PHASET(6) = TM1
C      PHASET(7) = TM2
C      PHASET(8) = TCI
C      PHASET(9) = TAR
C      PHASET(10) = TRR
C      PHASET(11) = TIIADD
C      PHASET(12) = TIIMAX
C      PHASET(13) = TVITBR
C      PHASET(15) = TVITTR
C      PHASET(17) = TVIMIN
C      PHASET(20) = 4
C      PHASET(21) = TIIVEH
C      PHASET(22) = PHRNG
C      PHASET(23) = NPHRC
	  
      
C      PHASEG(1) = IP
C      PHASEG(2) = PHINT
C      PHASEG(3) = PHINT
C      PHASEG(4) = PHINT
C      PHASEG(5) = PEDINT
C      PHASEG(6) = PEDINT
C      PHASEG(7) = PEDINT
C      PHASEG(8) = CLPH
C      PHASEG(9) = PDCALL
          
C      OVRGRP(1) = IOVRLP
      
      RETURN
      END
C
C-----Get the DT size and the total simulation time
C
      SUBROUTINE GTDT (PDT, PSIMTI)
     *      BIND(C, name='getDTData')
      

      IMPLICIT NONE                                                     CCODE=C
      
      INCLUDE 'PARAMS'
      INCLUDE 'USER'
      
      DOUBLE PRECISION      PDT, PSIMTI
      
      PDT = DT
      PSIMTI = SIMTIM
      
      RETURN
      END                                                               GTDT
C
C
C-----Get the vehicle injection errors
C
      SUBROUTINE GETVDIERR (PERRTIM, PERRIVN, PERRNUM, PERRCNT)
     *      BIND(C, name='getInjectionErrors')
      

      IMPLICIT NONE                                                     CCODE=C
      
      INCLUDE 'PARAMS'
      INCLUDE 'USER'
      INCLUDE 'QUE'
      
      DOUBLE PRECISION      PERRTIM(NIL)
      INTEGER PERRIVN(NIL), PERRNUM(NIL), PERRCNT
      
      DO 2010 PERRCNT=1,VDIERRCNT
        PERRTIM(PERRCNT) = VDIERRTIM(PERRCNT)
        PERRIVN(PERRCNT) = VDIERRIVN(PERRCNT)
        PERRNUM(PERRCNT) = VDIERRNUM(PERRCNT)
      
        VDIERRTIM(PERRCNT) = 0
        VDIERRIVN(PERRCNT) = 0
        VDIERRNUM(PERRCNT) = 0
 2010   CONTINUE
      PERRCNT = VDIERRCNT
      VDIERRCNT = 0
      RETURN
      END                                                               GTDT
C
C
C
      SUBROUTINE VDIPUT ( IVIDTIM,IVIDDT, INVID,
     *                    IVIDQIT,IVIDVCN,IVIDDCN,
     *                    IVIDDSP,IVIDOBN,IVIDIBN,
     *                    IVIDILN,IVIDPLO,IVIDFUT,
     *                    IVIDFST,IVIDFSL,IVIDFSP,IVIDFSD,
     *                    IVIDFGT,IVIDFGA,
     *                    IVIDFRT,IVIDFRA,
     *                    IVIDIVN,IVIDEMV,
     *                    IVIDSPD,IVIDACC,IVIDSLP )
     * BIND(C, name='injectVehicles')
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'QUE'
      INCLUDE 'USER'
      DOUBLE PRECISION, VALUE :: IVIDDT,IVIDTIM
      DOUBLE PRECISION IVIDACC(NIL),IVIDFGA(NIL),
     * IVIDFGT(NIL),IVIDFRA(NIL),IVIDFRT(NIL),IVIDFSD(NIL),
     * IVIDFSP(NIL),IVIDFST(NIL),IVIDQIT(NIL),IVIDSLP(NIL),
     * IVIDSPD(NIL)
      INTEGER, VALUE :: INVID
      INTEGER NVID
      INTEGER IVIDDCN(NIL),IVIDDSP(NIL),
     * IVIDEMV(NIL),IVIDFSL(NIL),IVIDFUT(NIL),
     * IVIDIBN(NIL),IVIDILN(NIL),IVIDIVN(NIL),IVIDOBN(NIL),
     * IVIDPLO(NIL),IVIDVCN(NIL)
      IF ( INVID . GT . NIL )       GO TO 9790
      
      DO 2010 NVID=1,NIL
        VDIQIT(NVID) = IVIDQIT(NVID)
        VDIVCN(NVID) = IVIDVCN(NVID)
        VDIDCN(NVID) = IVIDDCN(NVID)
        VDIDSP(NVID) = IVIDDSP(NVID)
        VDIOBN(NVID) = IVIDOBN(NVID)
        VDIIBN(NVID) = IVIDIBN(NVID)
        VDIILN(NVID) = IVIDILN(NVID)
        VDIPLO(NVID) = IVIDPLO(NVID)
        VDIFUT(NVID) = IVIDFUT(NVID)
        VDIFST(NVID) = IVIDFST(NVID)
        VDIFSL(NVID) = IVIDFSL(NVID)
        VDIFSP(NVID) = IVIDFSP(NVID)
        VDIFSD(NVID) = IVIDFSD(NVID)
        VDIFGT(NVID) = IVIDFGT(NVID)
        VDIFGA(NVID) = IVIDFGA(NVID)
        VDIFRT(NVID) = IVIDFRT(NVID)
        VDIFRA(NVID) = IVIDFRA(NVID)
        VDIIVN(NVID) = IVIDIVN(NVID)
        VDIEMV(NVID) = IVIDEMV(NVID)
        VDISPD(NVID) = IVIDSPD(NVID)
        VDIACC(NVID) = IVIDACC(NVID)
        VDISLP(NVID) = IVIDSLP(NVID)
 2010   CONTINUE
      VDIPUTN = INVID
      RETURN
C-----PROCESS THE EXECUTION ERRORS AND STOP
 9790 CONTINUE
      CALL  ABORTR  ( 'STOP 979 - INVID GT NIL - VDIPUT' )
      STOP  979
      END                                                               VDIPUT
