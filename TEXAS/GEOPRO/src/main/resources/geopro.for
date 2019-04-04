      PROGRAM GEOPRO
C            ( INPUT,OUTPUT,TAPE8,PLOT )
C                I     L      T8  PLOT   PRE  C   LGEO
C                SYSDAT
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
C-----READ      ERROR STOP NUMBERS ARE 801-881
C-----EXECTION  ERROR STOP NUMBERS ARE 901-919
C
      IMPLICIT NONE                                                     CCODE=C.
C
C *** PARAMETER VALUES FOR VARIABLE DIMENSION LIMITATIONS
C
      INCLUDE 'PARAMS'
C
C *** COLEASE COMMON BLOCKS CHANGED TO REGULAR COMMON BLOCKS
C
      INCLUDE 'APPRO'
      INCLUDE 'ARC'
      INCLUDE 'CONFLT'
      INCLUDE 'LANE'
      INCLUDE 'LINE'
      INCLUDE 'PATH'
      INCLUDE 'SDR'
C
C *** REGULAR COMMON BLOCKS
C
      INCLUDE 'CONSTN'
      INCLUDE 'CWDDIR'
      INCLUDE 'DATA'
      INCLUDE 'GEOCP'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'INDEX'
      INCLUDE 'OUTPT'
      INCLUDE 'PLOTTR'
      INCLUDE 'RADIAN'
      INCLUDE 'SDRC'
      INCLUDE 'TITLE'
      INCLUDE 'USFILE'
C
C ----- IOPLTR - UNIT NUMBER FOR CHANGE PLOT PAPER MESSAGE
C                0 - PAPER CHANGE REQUEST PUT PLOTTER IN COMMAND STREAM
C
      COMMON / PLTRIO / IOPLTR,NPF                                      CCODE=C]
      INTEGER           IOPLTR,NPF                                      CCODE=C]
C<    COMMON / PLTRIO / IOPLTR,NPF
C<    INTEGER           IOPLTR,NPF
C:    COMMON / PLTRIO / IOPLTR,NPF
C:    INTEGER           IOPLTR,NPF
C-----INITIALIZE CONSTANTS
      CALL  INITCN
      CALL  EXEC
      END                                                               GEOPRO
C
C
C
      BLOCK DATA
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'OUTPT'
      INCLUDE 'PLOTTR'
      INCLUDE 'RADIAN'
      INCLUDE 'SDRC'
      INCLUDE 'TITLE'
      COMMON / PLTRIO / IOPLTR,NPF                                      CCODE=C]
      INTEGER           IOPLTR,NPF                                      CCODE=C]
C<    COMMON / PLTRIO / IOPLTR,NPF
C<    INTEGER           IOPLTR,NPF
C:    COMMON / PLTRIO / IOPLTR,NPF
C:    INTEGER           IOPLTR,NPF
C-----COMMON / APPRO  /
      DATA     IAFLAG / NAP*' ' /
      DATA     ISFLAG / 4*'R',4*'L',4*'R',4*'L' /
      DATA     INTLNK / NAP*0 /
      DATA     INTLNU / NAP*0 /
C-----COMMON / GEOCOM /
      DATA     NCONFS / 0 /
      DATA     NIBL   / 0 /
      DATA     NOBL   / 0 /
      DATA     NPATHS / 0 /
      DATA     NSDRS  / 0 /
C-----COMMON / GEOVLC /
      DATA     ERRMSG / ' ' /
C-----COMMON / OUTPT  /
      DATA     LINES  / 60 /
      DATA     MODELT /  8 /
      DATA     NER    / 90 /
      DATA     NPAGE  /  1 /
      DATA     NTABL  /  1 /
C-----COMMON / PLOTTR /
      DATA     MAXXA  /    0 /
      DATA     MAXXI  /    0 /
      DATA     MAXXL  /    0 /
      DATA     MAXXR  /    0 /
      DATA     MAXYA  /    0 /
      DATA     MAXYI  /    0 /
      DATA     MAXYL  /    0 /
      DATA     MAXYR  /    0 /
      DATA     MINXA  / 9999 /
      DATA     MINXI  / 9999 /
      DATA     MINXL  / 9999 /
      DATA     MINXR  / 9999 /
      DATA     MINYA  / 9999 /
      DATA     MINYI  / 9999 /
      DATA     MINYL  / 9999 /
      DATA     MINYR  / 9999 /
C-----COMMON / RADIAN /
      DATA     XROUND / 0.500001D0 /
      DATA     ZERO   / 0.000001D0 /
C-----COMMON / TITLE  /
      DATA     IVERSN / 'V6.00' /
C-----COMMON / PLTRIO /
C:    DATA     NPF    / 7 /
      END                                                               BLKDAT
C
C
C
      SUBROUTINE EXEC
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CWDDIR'
      INCLUDE 'GEOVAL'
      INCLUDE 'OUTPT'
      INCLUDE 'TITLE'
      INCLUDE 'USFILE'
      COMMON / PLTRIO / IOPLTR,NPF                                      CCODE=C]
      INTEGER           IOPLTR,NPF                                      CCODE=C]
C<    COMMON / PLTRIO / IOPLTR,NPF
C<    INTEGER           IOPLTR,NPF
C:    COMMON / PLTRIO / IOPLTR,NPF
C:    INTEGER           IOPLTR,NPF
      INTEGER           NCPV  (PARNGP)
      CHARACTER*10      PAR   (PARNGP)
      CHARACTER*60      PARVAL(PARNGP),
     *                              IFILE  ,            LFILE  ,
     *                              T8FILE ,            PLFILE ,
     *                              PFILE  ,            CFILE  ,
     *                              LGEOFL ,            SYSDAT ,
     *                              WRKDIR ,            PAUSND ,
     *                              DUMLDV ,            DUMT9F ,
     *                              DUMREP
      EQUIVALENCE       (PARVAL( 1),IFILE ),(PARVAL( 2),LFILE ),
     *                  (PARVAL( 3),T8FILE),(PARVAL( 4),PLFILE),
     *                  (PARVAL( 5),PFILE ),(PARVAL( 6),CFILE ),
     *                  (PARVAL( 7),LGEOFL),(PARVAL( 8),SYSDAT),
     *                  (PARVAL( 9),WRKDIR),(PARVAL(10),PAUSND),
     *                  (PARVAL(11),DUMLDV),(PARVAL(12),DUMT9F),
     *                  (PARVAL(13),DUMREP)
      DIMENSION         NOUT(20)
      INTEGER           NOUT
      CHARACTER*3       IOFORM,TAG,TAGU
      CHARACTER*7       STATLF,STATT8
      CHARACTER*60      TDFILE,DEFILE
      CHARACTER*60      DEFFN,GDFILE,DEFILN
C|    CHARACTER*60      T8FILEU
      LOGICAL           EXI,STDTAG
      INTEGER           I,IRET,IS1,IS2,NC,NCGD,NCLF,NCT8,NDFIO,NIN
C|    INTEGER*4         ISTAT,SYS$SETDFPROT
C|    INTEGER*2         CURPRO,TMPPRO
      INTEGER           ILNB
C|    DATA     CURPRO / 1 /
C|    DATA     TMPPRO / 'F000'X /
C-----THE FIRST 9 PARAMETERS ARE FOR GEOPRO
C-----THE LAST  3 PARAMETERS ARE FOR GDVPRO PROCESSING
C-----LDV (DUMLDV), T9 (DUMT9F), AND REP (DUMREP) ARE DUMMY PARAMETERS
      DATA     NCPV   / -1           ,-1           ,-1           ,
     *                  -1           ,-1           ,-1           ,
     *                  -1           ,-1           ,-1           ,
     *                  -1           ,-1           ,-1           ,
     *                  -1             /
      DATA     PAR    / 'I'          ,'L'          ,'T8'         ,
     *                  'PLOT'       ,'PRE'        ,'C'          ,
     *                  'LGEO'       ,'SYS_DAT'    ,'WRK_DIR'    ,
     *                  'PAUSEND'    ,'LDV'        ,'T9'         ,
     *                  'REP'          /
      DATA     PARVAL / ' '          ,'geolist.txt',' '          ,
     *                  ' '          ,' '          ,' '          ,
     *                  ' '          ,' '          ,' '          ,
     *                  'YES'        ,'DUMLDV'     ,'DUMT9F'     ,
     *                  'DUMREP'       /
      DATA     TDFILE / ' ' /
      DATA     DEFILE / ' ' /
      DATA     USFILE / ' ' /
      DATA     IOFORM / '(A)' /
      DATA     STATLF / 'UNKNOWN' /
      DATA     STATT8 / 'UNKNOWN' /
  601 FORMAT(41HGeometry Processor for the TEXAS Traffic ,
     *       20HSimulation Package (,A5,1H),/,
     *       29HGEOPRO Copyright (c) 1989 by ,
     *       33HThe University of Texas at Austin,/)
C
C-----SUBROUTINE EXEC CONTROLS THE CALLING OF THE OTHER SUBROUTINES
C-----TO PROCESS THE INTERSECTION
C
C
C-----GET FILE NAMES FROM COMMAND LINE
C
C-----IFILE  - INPUT FILE (UNIT <NIN>)
C-----LFILE  - DATA LISTING FILE (UNIT <6>)
C-----T8FILE - OUTPUT FILE (UNIT <MODELT>)
C-----PFILE  - PREPROCESSOR (CONFIGURATION 2) FILE (UNIT <NDFIO>)
C-----CFILE  - CONVERTED (CONFIGURATION 1) FILE (UNIT <NIN>)
C
      WRITE (*,601) IVERSN
      CALL  CLFILE  ( PARNGP,PAR,PARVAL,NCPV,'geopro' )
      CWDDIR=WRKDIR
      CALL  TOUPR   ( PAUSND )
C
C-----KEYWORD "LGEO" ALSO FOR LISTING FILE NAME
C
                    IF ( PARVAL(7) . NE . ' ' )  LFILE = PARVAL(7)
C
C-----READ INITIALIZATION DATA
C
      IRET=1
      NIO=0
      CALL  GDVS0   ( IRET,1,NOUT,NIO,I,I,IS1,IS2,SYSDAT,
     *                ' ',TDFILE,DEFILE,USFILE,'NO','NO','NO','NO' )
                    IF ( IRET . EQ . -1 )        GO TO 8600
      GDFILE=' '
      IF(ILNB( CFILE  ).EQ.0)CFILE='gdv'
C%    IF(ILNB( CWDDIR ).GT.0)USFILE=CWDDIR
      IF(ILNB( PLFILE ).EQ.0)PLFILE='geoplot'                           CCODE=C]
C|   1                                       //'.dat'
      IF(ILNB( T8FILE ).EQ.0)T8FILE=DEFFN(MODELT)
C%    IF(ILNB( CFILE  ).GT.0)CALL PCFS(CFILE ,USFILE,CFILE )
C%    IF(ILNB( DEFILE ).GT.0)CALL PCFS(DEFILE,USFILE,DEFILE)
C%    IF(ILNB( IFILE  ).GT.0)CALL PCFS(IFILE ,USFILE,IFILE )
C%    IF(ILNB( LFILE  ).GT.0)CALL PCFS(LFILE ,USFILE,LFILE )
C%    IF(ILNB( PLFILE ).GT.0)CALL PCFS(PLFILE,USFILE,PLFILE)
      NDFIO=NOUT(2)+1
C
C-----NIN - UNIT FOR STANDARD INPUT
C-----NIO - UNIT FOR USER INPUT/OUTPUT
C
      NIN=NOUT(1)-1
      NOUT(1)=NIO
C|    STATLF='NEW'
C<    IOPLTR=NIO
                    IF ( IFILE . NE . ' ' )      THEN
C
C-----USE INPUT FILE FROM COMMAND LINE
C
        OPEN(NIN,FILE=IFILE,STATUS='OLD')
C|      STATT8='NEW'
        GO TO 1000
      END IF
      CALL  GDVCON  ( NIN,TAG,NOUT,GDFILE,PFILE,CFILE,DEFILE,USFILE )
      TAGU=TAG
      CALL  TOUPR   ( TAGU )
      IF ( TAGU . EQ . 'NUL' )                   THEN
        WRITE (*,IOFORM) 'A usable input data file not specified.'
        GO TO 2010
      END IF
      IF ( GDFILE . NE . ICBL )                  THEN
        NCGD=ILNB( GDFILE )
        WRITE (*,'(3A/A)') 'G & D-V Preprocessor file "',
     *                     GDFILE(1:NCGD),'" has been',
     *                     'converted for input to '//
     *                     'the Geometry & Driver-Vehicle Processors.'
      END IF
      IF ( TAG . EQ . ICBL )                     THEN
C|      STATT8='NEW'
        GO TO 1000
      END IF
C|    STATT8='UNKNOWN'
      STDTAG=.FALSE.
      IF(TAG(1:1).GE.'3'.AND.TAG(1:1).LE.'7'.AND.
     *   TAG(3:3).GE.'2'.AND.TAG(3:3).LE.'7'    )THEN
        IF(TAG(2:2).EQ.'T')                      TAG(2:2)='t'
        IF(TAG(2:2).EQ.'X')                      TAG(2:2)='x'
        IF(TAG(2:2).EQ.'t')                      STDTAG=.TRUE.
        IF(TAG(2:2).EQ.'x')                      STDTAG=.TRUE.
      END IF
      IF(TAG(3:3).GE.'1'.AND.TAG(3:3).LE.'3'    )THEN
        IF(TAG(1:2).EQ.'EX')                     TAG(1:2)='ex'
        IF(TAG(1:2).EQ.'ex')                     STDTAG=.TRUE.
      END IF
      IF ( STDTAG )                              THEN
        CALL  TXINQ  ( 0,'gpo'//TAG,TDFILE,T8FILE,EXI )
C
C-----WRITE GPO OUTPUT DATA TO FILE NAMED TO REFER TO A
C-----STANDARD LIBRARY FILE
C
      ELSE
C
C-----WRITE GPO OUTPUT DATA TO FILE NAMED TO REFER TO A
C-----USER-GROUP FILE
C
        CALL  TXINQ  ( 0,'gpo'//TAG,USFILE,T8FILE,EXI )
      END IF
 1000 CONTINUE
C
C---------- START OF CODE FOR RM FORTRAN ----------
C
C ----- WON'T WORK WITHOUT IT
C
C
C     READ (NIN,IOFORM)
C     REWIND(NIN)
C
C ---------- END OF CODE FOR RM FORTRAN ----------
C
      INQUIRE(NIN,NAME=IFILE)
      NC=ILNB( IFILE )
      IF ( NC . EQ . 0 )                         THEN
        CLOSE (NIN,STATUS='DELETE')
        WRITE (*,IOFORM) 'Input data file not specified.'
        GO TO 2010
      END IF
C%    CALL PCFS(IFILE,USFILE,IFILE)
      CALL  SHONAM  ( 'Geometry input data file name is',IFILE,78 )
      NCLF=ILNB( LFILE )
      IF ( NCLF . GT . 0 )                       THEN
        OPEN(6,FILE=LFILE,STATUS=STATLF
C|   *                                 ,DEFAULTFILE='.lst'
     *                                                    )
      END IF
      NCT8=ILNB( T8FILE )
C|    T8FILEU=T8FILE
C|    CALL  TOUPR   ( T8FILEU )
C|    IF ( INDEX(T8FILEU,'GPO').GE.1 )ISTAT=SYS$SETDFPROT(TMPPRO,CURPRO)
      IF ( NCT8 . GT . 0 )                       THEN
C
C---------- START OF CODE FOR RM FORTRAN ----------
C
C ----- WON'T WORK WITHOUT IT
C
C       INQUIRE(FILE=T8FILE,EXIST=EXI)
C       IF ( EXI )                               THEN
C         OPEN(MODELT,FILE=T8FILE)
C         CLOSE(MODELT,STATUS='DELETE')
C       END IF
C
C ---------- END OF CODE FOR RM FORTRAN ----------
C
        OPEN(MODELT,FILE=T8FILE,STATUS=STATT8
C|   *                                       ,CARRIAGECONTROL='LIST'
C?   *                                       ,CARRIAGECONTROL='LIST'
C~   *                                       ,CARRIAGECONTROL='LIST'
C%   *                                       ,CARRIAGECONTROL='LIST'
     *                                                              )
      ELSE
        DEFILN=DEFFN(MODELT)
        OPEN (MODELT,FILE=DEFILN,STATUS=STATT8
C|   *                            ,CARRIAGECONTROL='LIST',READONLY
C?   *                            ,CARRIAGECONTROL='LIST',READONLY
C~   *                            ,CARRIAGECONTROL='LIST'
C%   *                            ,CARRIAGECONTROL='LIST',ACTION='READ'
     *                                                                 )
      END IF
C|    TMPPRO=0
C|    T8FILEU=T8FILE
C|    CALL  TOUPR   ( T8FILEU )
C|    IF ( INDEX(T8FILEU,'GPO').GE.1 )ISTAT=SYS$SETDFPROT(CURPRO,TMPPRO)
      REWIND(MODELT)
      INQUIRE(MODELT,NAME=T8FILE)
C-----READ INPUT DATA AND CHECK FOR ERRORS
      CALL  READIN  ( NIN )
C-----WRITE THE TITLE FOR GEOPRO, THE ARC INFORMATION, AND THE LINE
C-----INFORMATION ONTO TAPE MODELT FOR SIMPRO
      CALL  WRITAL
C-----FIND THE X AND Y COORDINATES FOR A POINT AT THE MIDDLE AND END
C-----OF EACH INBOUND LANE AND AT THE MIDDLE AND START OF EACH OUTBOUND
C-----LANE THAT IS AVAILABLE AT THE INTERSECTION, FIND THE BOUNDARIES
C-----FOR PLOTTING, AND FIND THE PLOT SCALE FACTORS
      CALL  FNDXYP
C-----FIND THE SIGHT DISTANCE RESTRICTIONS BETWEEN THE INBOUND
C-----APPROACHES
      CALL  FNDSDR
C-----WRITE THE APPROACH INFORMATION ONTO TAPE MODELT FOR SIMPRO
      CALL  WRITAP
C-----INITIALIZE PLOTTING
      CALL  INIPLT  ( PLFILE )
C-----FIND THE INTERSECTION PATHS WITHIN THE INTERSECTION
      CALL  FNDPTH
      IF ( IPLOT . NE . 4 )                      THEN
        IF ( PLFILE . EQ . ' ' )                 THEN
          INQUIRE(FILE=JPLOT,NAME=PLFILE)
C%        IF(ILNB( PLFILE ).GT.0)CALL PCFS(PLFILE,USFILE,PLFILE)
        END IF
        CALL  SHONAM  ( 'Geometry plot file is',PLFILE,78 )
        CALL  PLOT    ( 0.0,0.0,999 )                                   CCODE=C]
C<      CALL  PLOT    ( 0.0,0.0,999 )
C:      CALL  PLOT    ( 0.0,0.0,999 )
      END IF
C-----CHECK EACH INBOUND LANE THAT IS AVAILABLE AT THE INTERSECTION TO
C-----SEE IF AN INTERSECTION PATH WAS CALCULATED FOR EACH TURNING
C-----MOVEMENT SPECIFIED FOR THE INBOUND LANE
      CALL  CHKPTH
C-----WRITE THE LANE INFORMATION AND THE SIGHT DISTANCE RESTRICTION
C-----INFORMATION ONTO TAPE MODELT FOR SIMPRO
      CALL  WRITLA
C-----FIND THE INTERSECTION CONFLICTS BETWEEN THE INTERSECTION PATHS
      CALL  FNDCON
C-----SORT THE INTERSECTION CONFLICTS FOR EACH INTERSECTION PATH BY THE
C-----DISTANCE DOWN THE INTERSECTION PATH TO THE INTERSECTION CONFLICT
      CALL  SRTCON
C-----WRITE THE INTERSECTION PATH INFORMATION ONTO TAPE MODELT FOR
C-----SIMPRO
      CALL  WRITPA
C-----CROSS INDEX THE INTERSECTION CONFLICTS WITH THE INTERSECTION PATHS
      CALL  NDXCON
C-----WRITE THE CONFLICT INFORMATION ONTO TAPE MODELT FOR SIMPRO
      CALL  WRITCO
      ENDFILE (MODELT)
C-----FINISHED PROCESSING
C
C-----GET NAME OF FILE WITH GEO INPUT DATA LISTING
C
      INQUIRE(6,NAME=LFILE)
      CALL  SHONAM  ( 'Geometry Processor input data listing on',LFILE,
     *                78 )
      CLOSE(6)
      CALL  NOTF    ( NDFIO,'Geometry Processor output data',
     *                'geo'//TAG,T8FILE )
      IF ( TAG . EQ . ICBL )                     THEN
        CLOSE(MODELT)
        GO TO 1040
      END IF
C
C-----MAKE PERMANENT COPY AND CLOSE GEO OUTPUT DATA FILE
C
      STDTAG=.FALSE.
      IF(TAG(1:1).GE.'3'.AND.TAG(1:1).LE.'7'.AND.
     *   TAG(3:3).GE.'2'.AND.TAG(3:3).LE.'7'    )THEN
        IF(TAG(2:2).EQ.'T')                      TAG(2:2)='t'
        IF(TAG(2:2).EQ.'X')                      TAG(2:2)='x'
        IF(TAG(2:2).EQ.'t')                      STDTAG=.TRUE.
        IF(TAG(2:2).EQ.'x')                      STDTAG=.TRUE.
      END IF
      IF(TAG(3:3).GE.'1'.AND.TAG(3:3).LE.'3'    )THEN
        IF(TAG(1:2).EQ.'EX')                     TAG(1:2)='ex'
        IF(TAG(1:2).EQ.'ex')                     STDTAG=.TRUE.
      END IF
      IF ( STDTAG )                              THEN
        CALL  TXCLOS  ( MODELT,GDFILE,TDFILE )
      ELSE
        CALL  TXCLOS  ( MODELT,GDFILE,USFILE )
      END IF
 1040 CONTINUE
      WRITE (*,IOFORM) 'Geometry data for TEXAS Model have '//
     *                 'been processed.'
 2010 CONTINUE
      IF ( PAUSND . EQ . 'YES' )                 THEN
        CALL EXIT ( 0 )
      END IF
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 8600 CONTINUE
      CALL  PRTERR ( 'STOP 860 - ' //
     *               'ERROR IN GDVS0 CALL - ' //
     *               'EXEC'                      )
      STOP  860
      END                                                               EXEC
C
C
C
      SUBROUTINE READIN ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'GEOCOM'
      INCLUDE 'OUTPT'
      INCLUDE 'TITLE'
      INTEGER           NIN
  501 FORMAT(A,1X,F4.2)
C
C-----SUBROUTINE READIN READS INPUT DATA AND CHECKS FOR ERRORS
C
C-----READ 80 CHARACTER TITLE FOR GEOPRO
      READ(NIN,501) ITITLE,RVERSN
      CALL  GETCDT  ( ITITLE(61:80) )
                    IF ( RVERSN . EQ . 0.0D0 )   RVERSN = 5.99D0
      NLINE = 0
      CALL  HEADER
C-----READ THE NUMBER AND LIST OF INBOUND AND OUTBOUND APPROACHES AND
C-----CHECK FOR ERRORS
      CALL  READIO  ( NIN )
C-----READ THE APPROACH INFORMATION AND CHECK FOR ERRORS
      CALL  READAP  ( NIN )
C-----FIND THE APPROACH TO THE LEFT AND THE APPROACH TO THE RIGHT FOR
C-----EACH INBOUND APPROACH
      CALL  APPLAR  ( NIBA,LIBA )
C-----FIND THE APPROACH TO THE LEFT AND THE APPROACH TO THE RIGHT FOR
C-----EACH OUTBOUND APPROACH
      CALL  APPLAR  ( NOBA,LOBA )
C-----READ THE ARC INFORMATION AND CHECK FOR ERRORS
      CALL  READAI  ( NIN )
C-----READ THE LINE INFORMATION AND CHECK FOR ERRORS
      CALL  READLI  ( NIN )
C-----READ SIGHT DISTANCE RESTRICTION COORDINATE INFORMATION AND
C-----CHECK FOR ERRORS
      CALL  READSI  ( NIN )
C-----READ THE GEOMETRY PROCESSOR OPTIONS AND CHECK FOR ERRORS
      CALL  READOP  ( NIN )
      RETURN
      END                                                               READIN
C
C
C
      SUBROUTINE HEADER
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'OUTPT'
      INCLUDE 'TITLE'
  601 FORMAT(1H ,41HGEOMETRY PROCESSOR FOR THE TEXAS TRAFFIC ,
     *       20HSIMULATION PACKAGE (,A5,11H)      PAGE,I3,/,
     *       10X,29HGEOPRO COPYRIGHT (c) 1989 BY ,
     *       33HTHE UNIVERSITY OF TEXAS AT AUSTIN,/)
  602 FORMAT(1X,A80,//)
C
C-----SUBROUTINE HEADER SKIPS TO THE TOP OF A NEW PAGE, PRINTS THE
C-----HEADER MESSAGE, AND PRINTS THE TITLE FOR GEOPRO
C
      IF ( NLINE . GT . 0 )                      THEN
        WRITE (6,'(A)') CHAR( 12 )
      END IF
      WRITE (6,601) IVERSN,NPAGE
      NLINE = 3
      NPAGE = NPAGE + 1
      WRITE (6,602) ITITLE
      NLINE = NLINE + 3
      RETURN
      END                                                               HEADER
C
C
C
      SUBROUTINE READIO ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'DATA'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'INDEX'
      INCLUDE 'OUTPT'
      CHARACTER*3       PLTOPT
      CHARACTER*4       IDIA
      INTEGER           NIN,NTEST,PLTVML,PRTVCL,TOTTIM
      INTEGER           ILNB
      REAL              MINHWD,PLTSIZ
  501 FORMAT(20I4)
  502 FORMAT(2I4,F4.1,4I4,A3,F5.2,A3,I2)
  601 FORMAT(8X,5HTABLE,I3,33H  -  LISTING OF INBOUND APPROACH ,
     *       7HNUMBERS,//)
  602 FORMAT(16X,I6)
  603 FORMAT(//,12X,37HTOTAL NUMBER OF INBOUND APPROACHES = ,I2,///)
  604 FORMAT(8X,5HTABLE,I3,34H  -  LISTING OF OUTBOUND APPROACH ,
     *       7HNUMBERS,//)
  605 FORMAT(16X,I6)
  606 FORMAT(//,12X,38HTOTAL NUMBER OF OUTBOUND APPROACHES = ,I2)
  607 FORMAT(///,12X,47HTOTAL NUMBER OF INBOUND AND OUTBOUND APPROACHES,
     *       3H = ,I2,///)
  801 FORMAT(31H NUMBER OF INBOUND APPROACHES =,I3,
     *       14H IS LE 0 OR GT,I2)
  802 FORMAT(17H INBOUND APPROACH,I3,3H = ,I3,14H IS LE 0 OR GT,I3)
  803 FORMAT(17H INBOUND APPROACH,I3,3H = ,I3,21H IS EQUAL TO INBOUND ,
     *       8HAPPROACH,I3,3H = ,I3)
  804 FORMAT(32H NUMBER OF OUTBOUND APPROACHES =,I3,
     *       14H IS LE 0 OR GT,I2)
  805 FORMAT(18H OUTBOUND APPROACH,I3,2H =,I3,14H IS LE 0 OR GT,I3)
  806 FORMAT(18H OUTBOUND APPROACH,I3,2H =,I3,21H IS EQUAL TO OUTBOUND,
     *       9H APPROACH,I3,3H = ,I3)
  807 FORMAT(17H INBOUND APPROACH,I3,2H =,I3,21H IS EQUAL TO OUTBOUND,
     *       9H APPROACH,I3,3H = ,I3)
  808 FORMAT(23H NUMBER OF APPROACHES =,I3,14H IS LT 3 OR GT,I3)
  809 FORMAT(53H NUMBER OF INBOUND APPROACHES PLUS NUMBER OF OUTBOUND,
     *       13H APPROACHES =,I3,29H IS NE NUMBER OF APPROACHES =,I3)
  866 FORMAT(28H NUMBER OF VEHICLE CLASSES =,I3,6H IS LT,I3,6H OR GT,I3)
  867 FORMAT(27H NUMBER OF DRIVER CLASSES =, I3,6H IS LT,I2,6H OR GT,I2)
  869 FORMAT(22H IMAGE FILE OPTION = (,A,23H) IS NOT (YES) OR (NO ))
  876 FORMAT(31H NUMBER OF VEHICLE ATTRIBUTES =,I3,15H IS NOT 6 OR 10)
C
C-----SUBROUTINE READIO READS THE NUMBER AND LIST OF INBOUND AND
C-----OUTBOUND APPROACHES AND CHECK FOR ERRORS
C
C-----READ NUMBER OF INBOUND APPROACHES
      READ(NIN,'(I4,A4)') NIBA,IDIA
      IF ( IDIA . EQ . ' DIA' )                  THEN
        DIAMON = .TRUE.
      ELSE
        DIAMON = .FALSE.
      END IF
                    IF ( NIBA . LE .  0 )        GO TO 8010
                    IF ( NIBA . GT .  NIA )      GO TO 8010
            IF ( NLINE+NIBA+9 . GT . LINES )     CALL  HEADER
      WRITE (6,601) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
C-----READ LIST OF INBOUND APPROACHES
      READ(NIN,501) (LIBA(IAN),IAN=1,NIBA)
      WRITE (6,602) (LIBA(IAN),IAN=1,NIBA)
      NLINE = NLINE + NIBA
      DO 1020  IAN = 1 , NIBA
                    IF ( LIBA(IAN) . LE .  0 )   GO TO 8020
                    IF ( LIBA(IAN) . GT . NAP )  GO TO 8020
                    IF ( NIBA . EQ . 1 )         GO TO 1020
                    IF ( IAN . EQ . NIBA )       GO TO 1020
C-----CHECK IF APPROACH IS DUPLICATED ON LIST OF INBOUND APPROACHES
      DO 1010  JAN = IAN+1 , NIBA
                    IF ( LIBA(IAN).EQ.LIBA(JAN) )GO TO 8030
 1010 CONTINUE
 1020 CONTINUE
      WRITE (6,603) NIBA
      NLINE = NLINE + 6
C-----READ NUMBER OF OUTBOUND APPROACHES
      READ(NIN,501) NOBA,NLEGS,NFUT
                    IF ( NOBA . LE .  0 )        GO TO 8040
                    IF ( NOBA . GT .  NOA )      GO TO 8040
            IF ( NLINE+NOBA+13 . GT . LINES )    CALL  HEADER
      WRITE (6,604) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
C-----READ LIST OF OUTBOUND APPROACHES
      READ(NIN,501) (LOBA(IAN),IAN=1,NOBA)
      WRITE (6,605) (LOBA(IAN),IAN=1,NOBA)
      NLINE = NLINE + NOBA
      DO 1040  IAN = 1 , NOBA
                    IF ( LOBA(IAN) . LE .  0 )   GO TO 8050
                    IF ( LOBA(IAN) . GT . NAP )  GO TO 8050
                    IF ( NOBA . EQ . 1 )         GO TO 1040
                    IF ( IAN . EQ . NOBA )       GO TO 1040
C-----CHECK IF APPROACH IS DUPLICATED ON LIST OF OUTBOUND APPROACHES
      DO 1030  JAN = IAN+1 , NOBA
                    IF ( LOBA(IAN).EQ.LOBA(JAN) )GO TO 8060
 1030 CONTINUE
 1040 CONTINUE
      WRITE (6,606) NOBA
      NLINE = NLINE + 3
C-----CHECK IF APPROACH NUMBER IS ON LIST OF INBOUND APPROACHES AND
C-----ALSO ON LIST OF OUTBOUND APPROACHES
      DO 1060  IAN = 1 , NIBA
      DO 1050  JAN = 1 , NOBA
                    IF ( LIBA(IAN).EQ.LOBA(JAN) )GO TO 8070
 1050 CONTINUE
 1060 CONTINUE
C-----READ NUMBER OF APPROACHES AND OTHER DATA
      READ(NIN,502) NAPS,TOTTIM,MINHWD,NVEHCL,NDRICL,PLTVML,PRTVCL,
     *              PLTOPT,PLTSIZ,IMAGOP,NVEHAT
                    IF (  NVEHCL . EQ . 0      ) NVEHCL = NVCD
                    IF (  NDRICL . EQ . 0      ) NDRICL = NDCD
                    IF (  IMAGOP . EQ . '   '  ) IMAGOP = 'NO '
                    IF (  NVEHAT . EQ . 0      ) NVEHAT = 6
                    IF (  NAPS   . LT . 3      ) GO TO 8080
                    IF (  NAPS   . GT . NAP    ) GO TO 8080
                    IF (  NVEHCL . LT . NVCD   ) GO TO 8660
                    IF (  NVEHCL . GT . NVC    ) GO TO 8660
                    IF (  NDRICL . LT . NDCD   ) GO TO 8670
                    IF (  NDRICL . GT . NDC    ) GO TO 8670
                    IF ( (IMAGOP . NE . 'YES') . AND .
     *                   (IMAGOP . NE . 'NO ') ) GO TO 8690
                    IF ( (NVEHAT . NE .  6   ) . AND .
     *                   (NVEHAT . NE . 10   ) ) GO TO 8760
      NTEST = NIBA + NOBA
                    IF ( NTEST  . NE . NAPS  )   GO TO 8090
      WRITE (6,607) NAPS
      NLINE = NLINE + 7
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 8010 CONTINUE
      WRITE (ERRMSG,801) NIBA,NIA
      CALL  PRTERR  ( 'STOP 801 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  801
 8020 CONTINUE
      WRITE (ERRMSG,802) IAN,LIBA(IAN),NAP
      CALL  PRTERR  ( 'STOP 802 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  802
 8030 CONTINUE
      WRITE (ERRMSG,803) IAN,LIBA(IAN),JAN,LIBA(JAN)
      CALL  PRTERR  ( 'STOP 803 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  803
 8040 CONTINUE
      WRITE (ERRMSG,804) NOBA,NOA
      CALL  PRTERR  ( 'STOP 804 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  804
 8050 CONTINUE
      WRITE (ERRMSG,805) IAN,LOBA(IAN),NAP
      CALL  PRTERR  ( 'STOP 805 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  805
 8060 CONTINUE
      WRITE (ERRMSG,806) IAN,LOBA(IAN),JAN,LOBA(JAN)
      CALL  PRTERR  ( 'STOP 806 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  806
 8070 CONTINUE
      WRITE (ERRMSG,807) IAN,LIBA(IAN),JAN,LOBA(JAN)
      CALL  PRTERR  ( 'STOP 807 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  807
 8080 CONTINUE
      WRITE (ERRMSG,808) NAPS,NAP
      CALL  PRTERR  ( 'STOP 808 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  808
 8090 CONTINUE
      WRITE (ERRMSG,809) NTEST,NAPS
      CALL  PRTERR  ( 'STOP 809 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  809
 8660 CONTINUE
      WRITE (ERRMSG,866) NVEHCL,NVCD,NVC
      CALL  PRTERR  ( 'STOP 866 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  866
 8670 CONTINUE
      WRITE (ERRMSG,867) NDRICL,NDCD,NDC
      CALL  PRTERR  ( 'STOP 867 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  867
 8690 CONTINUE
      WRITE (ERRMSG,869) IMAGOP
      CALL  PRTERR  ( 'STOP 869 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  869
 8760 CONTINUE
      WRITE (ERRMSG,876) NVEHAT
      CALL  PRTERR  ( 'STOP 876 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READIO'                             )
      STOP  876
      END                                                               READIO
C
C
C
      SUBROUTINE READAP ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'LANE'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'INDEX'
      INCLUDE 'OUTPT'
      INCLUDE 'RADIAN'
      DIMENSION         IUSED(NAP),LINTS(2,NAP),NEXTL(5),NEXTTC(4),
     *                  NEXTVT(4)
      INTEGER           IUSED     ,LINTS       ,NEXTL
      CHARACTER*1       NEXTTC,NEXTVT
      CHARACTER*1       AVTB,AVTE,AVTR,AVTV,ILT,IRT,IST,IUT,JSFLAG,
     *                  NB,NE,NI,NL,NR,NS,NU,NV,NX
      CHARACTER*3       IYES,NYES
      CHARACTER*4       ITEST,NBLANK
      CHARACTER*13      IMCTYP
      CHARACTER*80      ZLINE,ZLINEU
      INTEGER           I,IVTP,IZ,J,JBLN,KA,KAN,LGEOM1,LLTYPE,LTEST,NIN,
     *                  NINTS,PERFUT,WIDLMN,WIDLMX
      LOGICAL           ANYDEF,ANYFUT,FREEUT,IBAP
      INTEGER           IAND,ILNB
      DATA     NB     / 'B' /
      DATA     NE     / 'E' /
      DATA     NI     / 'I' /
      DATA     NL     / 'L' /
      DATA     NR     / 'R' /
      DATA     NS     / 'S' /
      DATA     NU     / 'U' /
      DATA     NV     / 'V' /
      DATA     NX     / 'X' /
      DATA     NYES   / 'YES' /
      DATA     NBLANK / '    ' /
  501 FORMAT(5I4,2I2,2I3,A1,A4,42X,A3,I3)
  502 FORMAT(5I4,1X,4A1,9X,4A1,2X,5I4,1X,4A1,9X,4A1)
  503 FORMAT(A)
  601 FORMAT(8X,5HTABLE,I3,26H  -  LISTING OF APPROACHES,//)
  602 FORMAT(12X,35HAPPROACH NUMBER -------------------,I5,/,
     *       12X,35HAPPROACH AZIMUTH ------------------,I5,/,
     *       12X,35HBEGINNING CENTERLINE X COORDINATE -,I5,/,
     *       12X,35HBEGINNING CENTERLINE Y COORDINATE -,I5,/,
     *       12X,35HSPEED LIMIT (MPH) -----------------,I5,/,
     *       12X,35HNUMBER OF DEGREES FOR STRAIGHT ----,I5,/,
     *       12X,35HNUMBER OF DEGREES FOR U-TURN ------,I5,/,
     *       12X,35HNUMBER OF LANES -------------------,I5,:,/,
     *       12X,35HNUMBER OF VARYING TRAFFIC PERIODS -,I5)
  603 FORMAT(/,
     *       12X,51HLANE IL IBLN WIDTH ---LANE GEOMETRY--- LEGAL TURNS ,
     *       13HALLOWED TYPES)
  604 FORMAT(12X,    I3, I4, I4,   I5, 2X,     4I5,    3X, 1H(,4A1,1H),
     *       7X,1H(,4A1,1H),4X,A)
  605 FORMAT(/)
  606 FORMAT(12X,29HTOTAL NUMBER OF APPROACHES = ,I2,///)
  810 FORMAT(16H APPROACH NUMBER,I3,14H IS LE 0 OR GT,I3)
  811 FORMAT(16H APPROACH NUMBER,I3,23H IS USED MORE THAN ONCE)
  812 FORMAT(16H APPROACH NUMBER,I3,10H AZIMUTH =,I4,15H IS LT 0 OR GE ,
     *       3H360)
  813 FORMAT(16H APPROACH NUMBER,I3,15H X COORDINATE =,I5,9H IS LT 0 ,
     *       10HOR GT 9999)
  814 FORMAT(16H APPROACH NUMBER,I3,15H Y COORDINATE =,I5,9H IS LT 0 ,
     *       10HOR GT 9999)
  815 FORMAT(16H APPROACH NUMBER,I3,14H SPEED LIMIT =,I3,9H IS LT 10,
     *       9H OR GT 80)
  816 FORMAT(16H APPROACH NUMBER,I3,18H NUMBER OF LANES =,I2,6H IS LE,
     *       8H 0 OR GT,I2)
  817 FORMAT(16H APPROACH NUMBER,I3,30H NUMBER OF DEGREES FOR STRAIGH,
     *       4HT = ,I3,17H IS LT 0 OR GT 45)
  818 FORMAT(16H APPROACH NUMBER,I3,30H NUMBER OF DEGREES FOR U-TURN ,
     *       2H= ,I3,17H IS LT 0 OR GT 45)
  819 FORMAT(16H APPROACH NUMBER,I3,30H IS NOT ON INBOUND OR OUTBOUND,
     *       6H LISTS)
  820 FORMAT(16H APPROACH NUMBER,I3,32H IS ON INBOUND LIST YET HAS OUTB,
     *       19HOUND DATA SPECIFIED)
  821 FORMAT(27H NUMBER OF INBOUND LANES = ,I3,6H IS GT,I3)
  822 FORMAT(16H APPROACH NUMBER,I3,32H IS ON OUTBOUND LIST YET HAS INB,
     *       19HOUND DATA SPECIFIED)
  823 FORMAT(28H NUMBER OF OUTBOUND LANES = ,I3,6H IS GT,I3)
  824 FORMAT(16H APPROACH NUMBER,I3,32H IS OUTBOUND YET HAS DATA FOR PE,
     *       53HRCENT OF EACH VEHICLE CLASS MAKING THE TRAFFIC STREAM)
  825 FORMAT(16H APPROACH NUMBER,I3,12H LANE NUMBER,I3,
     *       13H LANE WIDTH =,I3,6H IS LT,I2,6H OR GT,I3)
  826 FORMAT(16H APPROACH NUMBER,I3,12H LANE NUMBER,I3,
     *       14H LANE GEOMETRY,I3,2H =,I5,15H IS LT 0 OR GT ,I4)
  827 FORMAT(16H APPROACH NUMBER,I3,12H LANE NUMBER,I3,
     *       30H LANE GEOMETRY ORDER INCORRECT)
  828 FORMAT(16H APPROACH NUMBER,I3,12H LANE NUMBER,I3,
     *       18H LANE GEOMETRY 1 =,I5,
     *       38H IS NE LANE GEOMETRY 1 FOR LAST LANE =,I5)
  829 FORMAT(16H APPROACH NUMBER,I3,12H LANE NUMBER,I3,
     *       14H TURN CODE = (,A1,25H) IS NOT ( ), (X), OR (U))
  830 FORMAT(16H APPROACH NUMBER,I3,12H LANE NUMBER,I3,
     *       14H TURN CODE = (,A1,25H) IS NOT ( ), (X), OR (L))
  831 FORMAT(16H APPROACH NUMBER,I3,12H LANE NUMBER,I3,
     *       14H TURN CODE = (,A1,25H) IS NOT ( ), (X), OR (S))
  832 FORMAT(16H APPROACH NUMBER,I3,12H LANE NUMBER,I3,
     *       14H TURN CODE = (,A1,25H) IS NOT ( ), (X), OR (R))
  833 FORMAT(16H APPROACH NUMBER,I3,12H LANE NUMBER,I3,
     *       23H NO TURN CODE SPECIFIED)
  834 FORMAT(25H INFORMATION FOR APPROACH,I3,17H IS NOT SPECIFIED)
  861 FORMAT(16H APPROACH NUMBER,I3,14H SIDE FLAG = (,A1,
     *       31H) IS NOT (L) OR (R) FOR DIAMOND)
  862 FORMAT(16H APPROACH NUMBER,I3,12H LANE NUMBER,I3,
     *       41H IS NOT 1 FOR TURN CODE = (U) FOR DIAMOND)
  863 FORMAT(37H NUMBER OF INTERNAL APPROACH PAIRS = ,I3,7H IS GT ,I3,
     *       12H FOR DIAMOND)
  864 FORMAT(50H NUMBER OF INTERNAL APPROACH PAIRS = 0 FOR DIAMOND)
  865 FORMAT(41H LEFT OR RIGHT SIDE NOT SPECIFIED FOR ALL,
     *       36H APPROACHES FOR DIAMOND WITH U-TURNS)
  868 FORMAT(37H NUMBER OF VARYING TRAFFIC PERIODS = ,I3,
     *       15H IS LT 0 OR GT ,I2)
  877 FORMAT(37H ALLOWED VEHICLE TYPE FOR BICYCLE = (,A,
     *       19H) IS NOT ( ) OR (B))
  878 FORMAT(47H ALLOWED VEHICLE TYPE FOR EMERGENCY VEHICLE = (,A,
     *       19H) IS NOT ( ) OR (E))
  879 FORMAT(42H ALLOWED VEHICLE TYPE FOR RAIL VEHICLE = (,A,
     *       19H) IS NOT ( ) OR (R))
  880 FORMAT(44H ALLOWED VEHICLE TYPE FOR NORMAL VEHICLE = (,A,
     *       19H) IS NOT ( ) OR (V))
  881 FORMAT(25H ALLOWED VEHICLE TYPE = (,A,14H) IS NOT VALID)
C
C-----SUBROUTINE READAP READS THE APPROACH INFORMATION AND CHECKS FOR
C-----ERRORS
C
                    IF ( NLINE+22 . GT . LINES ) CALL  HEADER
      WRITE (6,601) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
      IL    = 0
      JBLN  = 0
      NINTS = 0
      ANYDEF = .FALSE.
      ANYFUT = .FALSE.
      DO 1010  IZ = 1 , NAP
      IUSED(IZ) = 0
 1010 CONTINUE
C-----READ INFORMATION FOR EACH APPROACH
      DO 2090  I = 1 , NAPS
C-----READ APPROACH INFORMATION
      READ (NIN,503)   ZLINE
      READ (ZLINE,501) IA,IAAZIM(IA),IAPX(IA),IAPY(IA),ISLIM(IA),
     *                 NVTP(IA),NLANES(IA),NDEGST(IA),NDEGUT(IA),
     *                 JSFLAG,ITEST,IYES,PERFUT
                    IF ( NDEGST(IA) . EQ . 0 )   NDEGST(IA) = 20
                    IF ( NDEGUT(IA) . EQ . 0 )   NDEGUT(IA) = 10
      IF ( DIAMON )                              THEN
        IF ( JSFLAG . EQ . ' ' )                 THEN
          JSFLAG = ISFLAG(IA)
          ANYDEF = .TRUE.
        END IF
        IAFLAG(IA) = JSFLAG
        ISFLAG(IA) = JSFLAG
      ELSE
        IAFLAG(IA) = NL
        ISFLAG(IA) = NL
      END IF
      IBAP = .FALSE.
      DO  IAN = 1 , NIBA
                    IF ( IA . EQ . LIBA(IAN) )   IBAP = .TRUE.
      END DO
      LTEST = NLINE + NLANES(IA) + 11
                    IF ( IBAP )                  LTEST = LTEST + 1
                    IF ( I . EQ . NAPS )         LTEST = LTEST + 4
                    IF ( LTEST . GT . LINES )    CALL  HEADER
      IF ( IBAP )                                THEN
        WRITE (6,602) IA,IAAZIM(IA),IAPX(IA),IAPY(IA),ISLIM(IA),
     *                NDEGST(IA),NDEGUT(IA),NLANES(IA),NVTP(IA)
        NLINE = NLINE + 9
      ELSE
        WRITE (6,602) IA,IAAZIM(IA),IAPX(IA),IAPY(IA),ISLIM(IA),
     *                NDEGST(IA),NDEGUT(IA),NLANES(IA)
        NLINE = NLINE + 8
      END IF
      WRITE (6,603)
      NLINE = NLINE + 2
            IF ( IA         . LE .    0 )        GO TO 8100
            IF ( IA         . GT .  NAP )        GO TO 8100
            IF ( IUSED(IA)  . NE .    0 )        GO TO 8110
            IF ( IAAZIM(IA) . LT .    0 )        GO TO 8120
            IF ( IAAZIM(IA) . GE .  360 )        GO TO 8120
            IF ( IAPX(IA)   . LT .    0 )        GO TO 8130
            IF ( IAPX(IA)   . GT . 9999 )        GO TO 8130
            IF ( IAPY(IA)   . LT .    0 )        GO TO 8140
            IF ( IAPY(IA)   . GT . 9999 )        GO TO 8140
            IF ( ISLIM(IA)  . LT .   10 )        GO TO 8150
            IF ( ISLIM(IA)  . GT .   80 )        GO TO 8150
            IF ( NLANES(IA) . LE .    0 )        GO TO 8160
            IF ( NLANES(IA) . GT .  NAL )        GO TO 8160
            IF ( NDEGST(IA) . LT .    0 )        GO TO 8170
            IF ( NDEGST(IA) . GT .   45 )        GO TO 8170
            IF ( NDEGUT(IA) . LT .    0 )        GO TO 8180
            IF ( NDEGUT(IA) . GT .   45 )        GO TO 8180
            IF ((ISFLAG(IA) . NE .   NL). AND .
     *          (ISFLAG(IA) . NE .   NR))        GO TO 8610
            IF ( NVTP(IA)   . LT .    0 )        GO TO 8680
            IF ( NVTP(IA)   . GT .  NVT )        GO TO 8680
C-----CHECK IF APPROACH IS ON LIST OF INBOUND APPROACHES
      DO 1030  IAN = 1 , NIBA
                    IF ( IA . EQ . LIBA(IAN) )   GO TO 1050
 1030 CONTINUE
C-----CHECK IF APPROACH IS ON LIST OF OUTBOUND APPROACHES
      DO 1040  IAN = 1 , NOBA
                    IF ( IA . EQ . LOBA(IAN) )   GO TO 1060
 1040 CONTINUE
      GO TO 8190
 1050 CONTINUE
C-----APPROACH IS INBOUND
                    IF ( ITEST . EQ . NBLANK )   GO TO 8200
      NIBL = NIBL + NLANES(IA)
      LLTYPE = 1
                    IF ( NIBL . GT . NIL )       GO TO 8210
      GO TO 1070
 1060 CONTINUE
C-----APPROACH IS OUTBOUND
                    IF ( ITEST . NE . NBLANK )   GO TO 8220
      NOBL = NOBL + NLANES(IA)
      LLTYPE = 2
                    IF ( NOBL . GT . NOL )       GO TO 8230
 1070 CONTINUE
      IUSED(IA) = 1
      ISLIM(IA) = ISLIM(IA)*MPH2FS + XROUND
      NSDR(IA) = 0
      ILN = 1
C     LGEOM1 = -1
                    IF ( IYES . NE . NYES )      GO TO 2010
                    IF ( LLTYPE . EQ . 2 )       GO TO 8240
C-----DUMMY READ PERCENT OF EACH VEHICLE CLASS MAKING UP THE TRAFFIC
C-----STREAM
      DO 2005  J = 1 , NVEHCL , 15
      READ (NIN,503)  ZLINE
 2005 CONTINUE
 2010 CONTINUE
      IL = IL + 1
C-----READ LANE INFORMATION (NEXTL IS FOR SECOND LANE ON CARD)
      READ (NIN,503)   ZLINE
      READ (ZLINE,502) LWID(IL),(LGEOM(IZ,IL),IZ=1,4),IUT,ILT,IST,IRT,
     *                 AVTB,AVTE,AVTR,AVTV,NEXTL,NEXTTC,NEXTVT
 2030 CONTINUE
      IF ( (AVTB.EQ.NBLANK) .AND. (AVTE.EQ.NBLANK) .AND.
     *     (AVTR.EQ.NBLANK) .AND. (AVTV.EQ.NBLANK) )
     *                                           THEN
        AVTB = NB
        AVTE = NE
        AVTV = NV
      END IF
      IBLN(IL) = 0
      NPINT(IL) = 0
      LGEOM1 = LGEOM(1,IL)
                    IF ( LLTYPE . EQ . 2 )       GO TO 2040
      JBLN = JBLN + 1
      IBLN(IL) = JBLN
 2040 CONTINUE
      IMCTYP = ' '
      IF ( NLANES(IA) . GT . 1 )                 THEN
        IF      ( ILN . EQ . 1          )        THEN
          IMCTYP='(MEDIAN LANE)'
        ELSE IF ( ILN . EQ . NLANES(IA) )        THEN
          IMCTYP='(CURB LANE)'
        END IF
      END IF
      WRITE (6,604) ILN,IL,IBLN(IL),LWID(IL),(LGEOM(IZ,IL),IZ=1,4),IUT,
     *              ILT,IST,IRT,AVTB,AVTE,AVTR,AVTV,IMCTYP
      NLINE = NLINE + 1
C-----CHECK LANE GEOMETRY
      DO 2050  IZ = 1 , 4
            IF ( LGEOM(IZ,IL) . LT . 0      )    GO TO 8260
            IF ( LGEOM(IZ,IL) . GT . POSMAX )    GO TO 8260
 2050 CONTINUE
C-----OPEN ALL 1=3, 2=4, AND 2>1
      IF ( LGEOM(1,IL) . EQ . LGEOM(3,IL) . AND .
     *     LGEOM(2,IL) . EQ . LGEOM(4,IL) . AND .
     *     LGEOM(2,IL) . GT . LGEOM(1,IL) )      GO TO 2060
C-----BEG BLOCKED AND END OPEN 1=2, 3>2, AND 4>3
      IF ( LGEOM(1,IL) . EQ . LGEOM(2,IL) . AND .
     *     LGEOM(3,IL) . GT . LGEOM(2,IL) . AND .
     *     LGEOM(4,IL) . GT . LGEOM(3,IL) )      GO TO 2060
C-----BEG OPEN AND END BLOCKED 3=4, 3>2, AND 2>1
      IF ( LGEOM(3,IL) . EQ . LGEOM(4,IL) . AND .
     *     LGEOM(3,IL) . GT . LGEOM(2,IL) . AND .
     *     LGEOM(2,IL) . GT . LGEOM(1,IL) )      GO TO 2060
C-----BEG OPEN, MIDDLE BLOCKED, AND END OPEN 4>3, 3>2, AND 2>1
      IF ( LGEOM(4,IL) . GT . LGEOM(3,IL) . AND .
     *     LGEOM(3,IL) . GT . LGEOM(2,IL) . AND .
     *     LGEOM(2,IL) . GT . LGEOM(1,IL) )      GO TO 2060
      GO TO 8270
 2060 CONTINUE
      IF ( ILN.NE.1.AND.LGEOM(1,IL).NE.LGEOM1.AND.LLTYPE.EQ.1 )
     *                                           GO TO 8280
C-----CHECK TURNING MOVEMENTS THAT ARE LEGAL
      LTURN(IL) = 0
      IF ( (IUT.NE.NBLANK).AND.(IUT.NE.NX).AND.(IUT.NE.NU) )
     *                                           GO TO 8290
      IF ( (ILT.NE.NBLANK).AND.(ILT.NE.NX).AND.(ILT.NE.NL) )
     *                                           GO TO 8300
      IF ( (IST.NE.NBLANK).AND.(IST.NE.NX).AND.(IST.NE.NS) )
     *                                           GO TO 8310
      IF ( (IRT.NE.NBLANK).AND.(IRT.NE.NX).AND.(IRT.NE.NR) )
     *                                           GO TO 8320
      IF (  IUT . EQ . NU )                      THEN
C-----  ALLOW U-TURNS
        LTURN(IL) = LTURN(IL) + LTURNU
      END IF
      IF (  ILT . EQ . NL )                      THEN
C-----  ALLOW LEFT TURNS
        LTURN(IL) = LTURN(IL) + LTURNL
      END IF
      IF (  IST . EQ . NS )                      THEN
C-----  ALLOW STRAIGHTS
        LTURN(IL) = LTURN(IL) + LTURNS
      END IF
      IF (  IRT . EQ . NR )                      THEN
C-----  ALLOW RIGHT TURNS
        LTURN(IL) = LTURN(IL) + LTURNR
      END IF
      IF ( LTURN(IL).LE.0 . AND . LGEOM(3,IL).NE.LGEOM(4,IL) . AND .
     *     LLTYPE.EQ.1 )                         GO TO 8330
      IF ( LTURN(IL).LE.0 . AND . LGEOM(1,IL).NE.LGEOM(2,IL) . AND .
     *     LLTYPE.EQ.2 )                         GO TO 8330
C-----CHECK LANE ALLOWED VEHICLE TYPES
      IF ( (AVTB.NE.NBLANK) .AND. (AVTB.NE.NB) ) GO TO 8770
      IF ( (AVTE.NE.NBLANK) .AND. (AVTE.NE.NE) ) GO TO 8780
      IF ( (AVTR.NE.NBLANK) .AND. (AVTR.NE.NR) ) GO TO 8790
      IF ( (AVTV.NE.NBLANK) .AND. (AVTV.NE.NV) ) GO TO 8800
      LAVT(IL) = 0
      IF (  AVTB . EQ . NB )                     THEN
C-----  ALLOW BICYCLES
        LAVT(IL) = LAVT(IL) + LAVTB
      END IF
      IF (  AVTE . EQ . NE )                     THEN
C-----  ALLOW EMERGENCY VEHICLES
        LAVT(IL) = LAVT(IL) + LAVTE
      END IF
      IF (  AVTR . EQ . NR )                     THEN
C-----  ALLOW RAIL VEHICLES
        LAVT(IL) = LAVT(IL) + LAVTR
      END IF
      IF (  AVTV . EQ . NV )                     THEN
C-----  ALLOW NORMAL VEHICLES
        LAVT(IL) = LAVT(IL) + LAVTV
      END IF
      IF ( LAVT(IL) . EQ . 0 )                   GO TO 8810
      IF ( LAVT(IL) . EQ . LAVTB )               THEN
        WIDLMN = LWBMIN
        WIDLMX = LWBMAX
      ELSE
        WIDLMN = LWVMIN
        WIDLMX = LWVMAX
      END IF
                    IF ( LWID(IL) . LT . WIDLMN )GO TO 8250
                    IF ( LWID(IL) . GT . WIDLMX )GO TO 8250
      IF ( ( DIAMON        ) . AND .
     *     ( IUT . EQ . NU ) . AND .
     *     ( ILN . NE . 1  ) )                   GO TO 8620
      LLANES(ILN,IA) = IL
      ISNA(IL) = IA
      LTYPE(IL) = LLTYPE
C-----FIND LANE TO THE LEFT AND THE RIGHT
      NLL(IL) = IL - 1
                    IF ( ILN . EQ . 1 )          NLL(IL) = 0
      NLR(IL) = IL + 1
                    IF ( ILN . EQ . NLANES(IA) ) NLR(IL) = 0
      ILN = ILN + 1
                    IF ( (ILN/2)*2 . NE . ILN )  GO TO 2080
                    IF ( ILN . GT . NLANES(IA) ) GO TO 2080
C-----PROCESS SECOND LANE ON CARD
      IL = IL + 1
      LWID(IL) = NEXTL(1)
      LGEOM(1,IL) = NEXTL(2)
      LGEOM(2,IL) = NEXTL(3)
      LGEOM(3,IL) = NEXTL(4)
      LGEOM(4,IL) = NEXTL(5)
      IUT  = NEXTTC(1)
      ILT  = NEXTTC(2)
      IST  = NEXTTC(3)
      IRT  = NEXTTC(4)
      AVTB = NEXTVT(1)
      AVTE = NEXTVT(2)
      AVTR = NEXTVT(3)
      AVTV = NEXTVT(4)
      GO TO 2030
 2080 CONTINUE
                    IF ( ILN . LE . NLANES(IA) ) GO TO 2010
C-----END OF LANE LOOP
C
C-----DUMMY VARYING TRAFFIC PERIODS
C
      DO 2086  IVTP = 1 , NVTP(IA)
      READ (NIN,503)  ZLINE
      ZLINEU=ZLINE
      CALL  TOUPR   ( ZLINEU )
      IF ( ZLINEU(78:80) . EQ . NYES )           THEN
        DO 2084  J = 1 , NVEHCL , 15
        READ (NIN,503)  ZLINE
 2084   CONTINUE
      END IF
 2086 CONTINUE
      WRITE (6,605)
      NLINE = NLINE + 2
C-----END OF APPROACH LOOP
 2090 CONTINUE
C-----CHECK IF INFORMATION FOR EACH INBOUND APPROACH WAS SPECIFIED
      DO 3010  IAN = 1 , NIBA
      IA = LIBA(IAN)
                    IF ( IUSED(IA) . EQ . 0 )    GO TO 8340
 3010 CONTINUE
C-----CHECK IF INFORMATION FOR EACH OUTBOUND APPROACH WAS SPECIFIED
      DO 3020  IAN = 1 , NOBA
      IA = LOBA(IAN)
                    IF ( IUSED(IA) . EQ . 0 )    GO TO 8340
 3020 CONTINUE
      WRITE (6,606) NAPS
      NLINE = NLINE + 4
                    IF ( .NOT. DIAMON )          RETURN
C-----FIND DIAMOND INTERNAL INBOUND AND INTERNAL OUTBOUND APPROACHES
      DO 4030  IAN = 1 , NIBA
      IA = LIBA(IAN)
      DO 4020  JAN = 1 , NOBA
      JA = LOBA(JAN)
            IF ( IAAZIM(IA) . NE . IAAZIM(JA) )  GO TO 4020
            IF ( IAPX  (IA) . NE . IAPX  (JA) )  GO TO 4020
            IF ( IAPY  (IA) . NE . IAPY  (JA) )  GO TO 4020
            IF ( ISLIM (IA) . NE . ISLIM (JA) )  GO TO 4020
            IF ( NLANES(IA) . NE . NLANES(JA) )  GO TO 4020
            IF ( ISFLAG(IA) . EQ . ISFLAG(JA) )  GO TO 4020
      DO 4010  ILN = 1 , NLANES(IA)
      JLN = ILN
      IL = LLANES(ILN,IA)
      JL = LLANES(JLN,JA)
            IF ( LWID   (IL) . NE . LWID   (JL) )GO TO 4020
            IF ( LGEOM(1,IL) . NE . LGEOM(1,JL) )GO TO 4020
            IF ( LGEOM(2,IL) . NE . LGEOM(2,JL) )GO TO 4020
            IF ( LGEOM(3,IL) . NE . LGEOM(3,JL) )GO TO 4020
            IF ( LGEOM(4,IL) . NE . LGEOM(4,JL) )GO TO 4020
 4010 CONTINUE
      NINTS = NINTS + 1
                    IF ( NINTS . GT . NAP )      GO TO 8630
      LINTS(1,NINTS) = IA
      LINTS(2,NINTS) = JA
      IAFLAG(IA) = NI
      IAFLAG(JA) = NI
 4020 CONTINUE
 4030 CONTINUE
                    IF ( NINTS . EQ . 0 )        GO TO 8640
      DO 4050  I = 1 , NINTS
      IA = LINTS(1,I)
      JA = LINTS(2,I)
      IF ( ( IAND( LTURN(LLANES(1,IA)),LTURNU ).EQ.LTURNU ) . AND .
     *     ( IAND( LTURN(LLANES(1,JA)),LTURNU ).EQ.LTURNU ) )
     *                                           THEN
        FREEUT = .TRUE.
        ANYFUT = .TRUE.
      ELSE
        FREEUT = .FALSE.
      END IF
      DO 4040  KAN = 1 , NOBA
      KA = LOBA(KAN)
      IF ( ISFLAG(KA) . EQ . ISFLAG(IA) )        THEN
        IF ( FREEUT )                            THEN
          IF ( ( IAND( LTURN(LLANES(1,KA)),LTURNU ).EQ.LTURNU ) . AND .
     *         ( IAFLAG(KA)                        .NE.NI     ) )
     *                                           INTLNU(KA) = JA
        ELSE
          INTLNK(KA) = JA
        END IF
      END IF
 4040 CONTINUE
 4050 CONTINUE
                    IF ( ANYFUT . AND . ANYDEF ) GO TO 8650
      DO 4060  I = 1 , NINTS
      INTLNK(LINTS(2,I)) = LINTS(1,I)
 4060 CONTINUE
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 8100 CONTINUE
      WRITE (ERRMSG,810) IA,NAP
      CALL  PRTERR  ( 'STOP 810 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  810
 8110 CONTINUE
      WRITE (ERRMSG,811) IA
      CALL  PRTERR  ( 'STOP 811 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  811
 8120 CONTINUE
      WRITE (ERRMSG,812) IA,IAAZIM(IA)
      CALL  PRTERR  ( 'STOP 812 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  812
 8130 CONTINUE
      WRITE (ERRMSG,813) IA,IAPX(IA)
      CALL  PRTERR  ( 'STOP 813 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  813
 8140 CONTINUE
      WRITE (ERRMSG,814) IA,IAPY(IA)
      CALL  PRTERR  ( 'STOP 814 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  814
 8150 CONTINUE
      WRITE (ERRMSG,815) IA,ISLIM(IA)
      CALL  PRTERR  ( 'STOP 815 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  815
 8160 CONTINUE
      WRITE (ERRMSG,816) IA,NLANES(IA),NAL
      CALL  PRTERR  ( 'STOP 816 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  816
 8170 CONTINUE
      WRITE (ERRMSG,817) IA,NDEGST(IA)
      CALL  PRTERR  ( 'STOP 817 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  817
 8180 CONTINUE
      WRITE (ERRMSG,818) IA,NDEGUT(IA)
      CALL  PRTERR  ( 'STOP 818 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  818
 8190 CONTINUE
      WRITE (ERRMSG,819) IA
      CALL  PRTERR  ( 'STOP 819 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  819
 8200 CONTINUE
      WRITE (ERRMSG,820) IA
      CALL  PRTERR  ( 'STOP 820 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  820
 8210 CONTINUE
      WRITE (ERRMSG,821) NIBL,NIL
      CALL  PRTERR  ( 'STOP 821 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  821
 8220 CONTINUE
      WRITE (ERRMSG,822) IA
      CALL  PRTERR  ( 'STOP 822 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  822
 8230 CONTINUE
      WRITE (ERRMSG,823) NOBL,NOL
      CALL  PRTERR  ( 'STOP 823 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  823
 8240 CONTINUE
      WRITE (ERRMSG,824) IA
      CALL  PRTERR  ( 'STOP 824 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  824
 8250 CONTINUE
      WRITE (ERRMSG,825) IA,ILN,LWID(IL),WIDLMN,WIDLMX
      CALL  PRTERR  ( 'STOP 825 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  825
 8260 CONTINUE
      WRITE (ERRMSG,826) IA,ILN,IZ,LGEOM(IZ,IL),POSMAX
      CALL  PRTERR  ( 'STOP 826 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  826
 8270 CONTINUE
      WRITE (ERRMSG,827) IA,ILN
      CALL  PRTERR  ( 'STOP 827 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  827
 8280 CONTINUE
      WRITE (ERRMSG,828) IA,ILN,LGEOM(1,IL),LGEOM1
      CALL  PRTERR  ( 'STOP 828 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  828
 8290 CONTINUE
      WRITE (ERRMSG,829) IA,ILN,IUT
      CALL  PRTERR  ( 'STOP 829 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  829
 8300 CONTINUE
      WRITE (ERRMSG,830) IA,ILN,ILT
      CALL  PRTERR  ( 'STOP 830 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  830
 8310 CONTINUE
      WRITE (ERRMSG,831) IA,ILN,IST
      CALL  PRTERR  ( 'STOP 831 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  831
 8320 CONTINUE
      WRITE (ERRMSG,832) IA,ILN,IRT
      CALL  PRTERR  ( 'STOP 832 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  832
 8330 CONTINUE
      WRITE (ERRMSG,833) IA,ILN
      CALL  PRTERR  ( 'STOP 833 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  833
 8340 CONTINUE
      WRITE (ERRMSG,834) IA
      CALL  PRTERR  ( 'STOP 834 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  834
 8610 CONTINUE
      WRITE (ERRMSG,861) IA,ISFLAG(IA)
      CALL  PRTERR  ( 'STOP 861 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  861
 8620 CONTINUE
      WRITE (ERRMSG,862) IA,ILN
      CALL  PRTERR  ( 'STOP 862 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  862
 8630 CONTINUE
      WRITE (ERRMSG,863) NINTS,NAP
      CALL  PRTERR  ( 'STOP 863 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  863
 8640 CONTINUE
      WRITE (ERRMSG,864)
      CALL  PRTERR  ( 'STOP 864 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  864
 8650 CONTINUE
      WRITE (ERRMSG,865)
      CALL  PRTERR  ( 'STOP 865 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  865
 8680 CONTINUE
      WRITE (ERRMSG,868) NVTP(IA),NVT
      CALL  PRTERR  ( 'STOP 868 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  868
 8770 CONTINUE
      WRITE (ERRMSG,877) AVTB
      CALL  PRTERR  ( 'STOP 877 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  877
 8780 CONTINUE
      WRITE (ERRMSG,878) AVTE
      CALL  PRTERR  ( 'STOP 878 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  878
 8790 CONTINUE
      WRITE (ERRMSG,879) AVTR
      CALL  PRTERR  ( 'STOP 879 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  879
 8800 CONTINUE
      WRITE (ERRMSG,880) AVTV
      CALL  PRTERR  ( 'STOP 880 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  880
 8810 CONTINUE
      WRITE (ERRMSG,881) AVTB//AVTE//AVTR//AVTV
      CALL  PRTERR  ( 'STOP 881 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAP'                             )
      STOP  881
      END                                                               READAP
C
C
C
      SUBROUTINE APPLAR ( NBA,LBA )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'INDEX'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INTEGER           NBA
      DIMENSION         LBA(NBA)
      INTEGER           LBA
      INTEGER           IMAXAZ,IMINAZ,JAAZIM,KAAZIM,LAAZIM
C
C-----SUBROUTINE APPLAR FINDS THE APPROACH TO THE LEFT AND THE APPROACH
C-----TO THE RIGHT FOR EACH APPROACH ON THE LBA LIST
C
C-----PROCESS EACH APPROACH ON THE LBA LIST
      DO 1030  IAN = 1 , NBA
      IA = LBA(IAN)
      IALEFT(IA) = 0
      IARGHT(IA) = 0
      JAAZIM = IAAZIM(IA)
      IMINAZ = 180 - NDEGST(IA)
      IMAXAZ = 180 + NDEGST(IA)
C-----CHECK AGAINST EACH OTHER APPROACH ON THE LBA LIST
      DO 1020  JAN = 1 , NBA
                    IF ( IAN . EQ . JAN )        GO TO 1020
      JA = LBA(JAN)
      IF ( DIAMON )                              THEN
        IF ( ISFLAG(IA) . NE . ISFLAG(JA) )      GO TO 1020
      END IF
      KAAZIM = IAAZIM(JA)
                    IF ( KAAZIM . LT . JAAZIM )  KAAZIM = KAAZIM + 360
      LAAZIM = KAAZIM - JAAZIM
                    IF ( LAAZIM . GE . IMINAZ )  GO TO 1010
C-----APPROACH TO THE LEFT HAS THE MINIMUM AZIMUTH DIFFERENCE
      IMINAZ = LAAZIM
      IALEFT(IA) = JA
 1010 CONTINUE
                    IF ( LAAZIM . LE . IMAXAZ )  GO TO 1020
C-----APPROACH TO THE RIGHT HAS THE MAXIMUM AZIMUTH DIFFERENCE
      IMAXAZ = LAAZIM
      IARGHT(IA) = JA
C-----END OF OTHER APPROACH LOOP
 1020 CONTINUE
C-----END OF APPROACH LOOP
 1030 CONTINUE
      RETURN
      END                                                               APPLAR
C
C
C
      SUBROUTINE READAI ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ARC'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'OUTPT'
      DIMENSION         IUSED(NAR)
      INTEGER           IUSED
      CHARACTER*17      IRTTYP
      INTEGER           I,IARC,IZ,LTEST,NIN
      INTEGER           ILNB
  501 FORMAT(20I4)
  601 FORMAT(8X,5HTABLE,I3,40H  -  LISTING OF ARCS (FOR PLOTTING ONLY),
     *       //)
  602 FORMAT(12X,35HARC NUMBER ------------------------,I5,/,
     *       12X,35HCENTER X COORDINATE ---------------,I5,/,
     *       12X,35HCENTER Y COORDINATE ---------------,I5,/,
     *       12X,35HBEGINNING AZIMUTH -----------------,I5,/,
     *       12X,35HSWEEP ANGLE -----------------------,I5,/,
     *       12X,35HRADIUS OF ARC ---------------------,I5,/,
     *       12X,35HROTATION FROM BEGINNING AZIMUTH ---,A,//)
  603 FORMAT(12X,23HTOTAL NUMBER OF ARCS = ,I2,///)
  835 FORMAT(18H NUMBER OF ARCS = ,I3,14H IS LT 0 OR GT,I3)
  836 FORMAT(11H ARC NUMBER,I3,3H = ,I3,14H IS LE 0 OR GT,I3)
  837 FORMAT(11H ARC NUMBER,I3,23H IS USED MORE THAN ONCE)
  838 FORMAT(11H ARC NUMBER,I3,15H X COORDINATE =,I5,13H IS LT 0 OR G,
     *       6HT 9999)
  839 FORMAT(11H ARC NUMBER,I3,15H Y COORDINATE =,I5,13H IS LT 0 OR G,
     *       6HT 9999)
  840 FORMAT(11H ARC NUMBER,I3,10H AZIMUTH =,I4,18H IS LT 0 OR GE 360)
  841 FORMAT(11H ARC NUMBER,I3,20H NUMBER OF DEGREES =,I4,8H IS LT -,
     *       13H360 OR GT 360)
  842 FORMAT(11H ARC NUMBER,I3,9H RADIUS =,I6,18H IS LE 0 OR GT 999)
C
C-----SUBROUTINE READAI READ THE ARC INFORMATION AND CHECKS FOR ERRORS
C
C-----READ NUMBER OF ARCS
      READ(NIN,501) NARCS
                    IF ( NARCS . EQ .   0 )      GO TO 1040
                    IF ( NARCS . LT .   0 )      GO TO 8350
                    IF ( NARCS . GT . NAR )      GO TO 8350
                    IF ( NLINE+16 . GT . LINES ) CALL  HEADER
      WRITE (6,601) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
      DO 1010  IZ = 1 , NAR
      IUSED(IZ) = 0
 1010 CONTINUE
C-----READ INFORMATION FOR EACH ARC
      DO 1030  I = 1, NARCS
C-----READ ARC INFORMATION
      READ(NIN,501) IARC,IARCX(IARC),IARCY(IARC),IARCAZ(IARC),
     *           IARCSW(IARC),IARCR(IARC)
      LTEST = NLINE + 9
                    IF ( I . EQ . NARCS )        LTEST = LTEST + 4
                    IF ( LTEST . GT . LINES )    CALL  HEADER
      IF ( IARCSW(IARC) . GE . 0 )               THEN
        IRTTYP = 'CLOCKWISE'
      ELSE
        IRTTYP = 'COUNTER CLOCKWISE'
      END IF
      WRITE (6,602) IARC,IARCX(IARC),IARCY(IARC),IARCAZ(IARC),
     *              IARCSW(IARC),IARCR(IARC),IRTTYP
      NLINE = NLINE + 9
                    IF ( IARC . LE .   0 )       GO TO 8360
                    IF ( IARC . GT . NAR )       GO TO 8360
            IF ( IUSED(IARC)  . NE .    0 )      GO TO 8370
            IF ( IARCX(IARC)  . LT .    0 )      GO TO 8380
            IF ( IARCX(IARC)  . GT . 9999 )      GO TO 8380
            IF ( IARCY(IARC)  . LT .    0 )      GO TO 8390
            IF ( IARCY(IARC)  . GT . 9999 )      GO TO 8390
            IF ( IARCAZ(IARC) . LT .    0 )      GO TO 8400
            IF ( IARCAZ(IARC) . GE .  360 )      GO TO 8400
            IF ( IARCSW(IARC) . LT . -360 )      GO TO 8410
            IF ( IARCSW(IARC) . GT . +360 )      GO TO 8410
            IF ( IARCR(IARC)  . LE .    0 )      GO TO 8420
            IF ( IARCR(IARC)  . GT .  999 )      GO TO 8420
      LARCS(I) = IARC
      IUSED(IARC) = 1
C-----END OF ARC LOOP
 1030 CONTINUE
      WRITE (6,603) NARCS
      NLINE = NLINE + 4
 1040 CONTINUE
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 8350 CONTINUE
      WRITE (ERRMSG,835) NARCS,NAR
      CALL  PRTERR  ( 'STOP 835 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAI'                             )
      STOP  835
 8360 CONTINUE
      WRITE (ERRMSG,836) I,IARC,NAR
      CALL  PRTERR  ( 'STOP 836 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAI'                             )
      STOP  836
 8370 CONTINUE
      WRITE (ERRMSG,837) IARC
      CALL  PRTERR  ( 'STOP 837 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAI'                             )
      STOP  837
 8380 CONTINUE
      WRITE (ERRMSG,838) IARC,IARCX(IARC)
      CALL  PRTERR  ( 'STOP 838 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAI'                             )
      STOP  838
 8390 CONTINUE
      WRITE (ERRMSG,839) IARC,IARCY(IARC)
      CALL  PRTERR  ( 'STOP 839 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAI'                             )
      STOP  839
 8400 CONTINUE
      WRITE (ERRMSG,840) IARC,IARCAZ(IARC)
      CALL  PRTERR  ( 'STOP 840 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAI'                             )
      STOP  840
 8410 CONTINUE
      WRITE (ERRMSG,841) IARC,IARCSW(IARC)
      CALL  PRTERR  ( 'STOP 841 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAI'                             )
      STOP  841
 8420 CONTINUE
      WRITE (ERRMSG,842) IARC,IARCR(IARC)
      CALL  PRTERR  ( 'STOP 842 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READAI'                             )
      STOP  842
      END                                                               READAI
C
C
C
      SUBROUTINE READLI ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'LINE'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'OUTPT'
      DIMENSION         IUSED(NLI)
      INTEGER           IUSED
      INTEGER           I,ILINE,IZ,LTEST,NIN
      INTEGER           ILNB
  501 FORMAT(20I4)
  601 FORMAT(8X,5HTABLE,I3,41H  -  LISTING OF LINES (FOR PLOTTING ONLY),
     *       //)
  602 FORMAT(12X,35HLINE NUMBER -----------------------,I5,/,
     *       12X,35HSTART X COORDINATE ----------------,I5,/,
     *       12X,35HSTART Y COORDINATE ----------------,I5,/,
     *       12X,35HEND X COORDINATE ------------------,I5,/,
     *       12X,35HEND Y COORDINATE ------------------,I5,//)
  603 FORMAT(12X,24HTOTAL NUMBER OF LINES = ,I2,///)
  843 FORMAT(19H NUMBER OF LINES = ,I3,14H IS LT 0 OR GT,I4)
  844 FORMAT(12H LINE NUMBER,I3,3H = ,I3,14H IS LE 0 OR GT,I4)
  845 FORMAT(12H LINE NUMBER,I3,23H IS USED MORE THAN ONCE)
  846 FORMAT(12H LINE NUMBER,I3,25H BEGINNING X COORDINATE =,I5,2H I,
     *       17HS LT 0 OR GT 9999)
  847 FORMAT(12H LINE NUMBER,I3,25H BEGINNING Y COORDINATE =,I5,2H I,
     *       17HS LT 0 OR GT 9999)
  848 FORMAT(12H LINE NUMBER,I3,22H ENDING X COORDINATE =,I5,6H IS LT,
     *       13H 0 OR GT 9999)
  849 FORMAT(12H LINE NUMBER,I3,22H ENDING Y COORDINATE =,I5,6H IS LT,
     *       13H 0 OR GT 9999)
C
C-----SUBROUTINE READLI READS THE LINE INFORMATION AND CHECKS FOR ERRORS
C
C-----READ NUMBER OF LINES
      READ(NIN,501) NLINES
                    IF ( NLINES . EQ .   0 )     GO TO 1040
                    IF ( NLINES . LT .   0 )     GO TO 8430
                    IF ( NLINES . GT . NLI )     GO TO 8430
                    IF ( NLINE+14 . GT . LINES ) CALL  HEADER
      WRITE (6,601) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
      DO 1010  IZ = 1 , NLI
      IUSED(IZ) = 0
 1010 CONTINUE
C-----READ INFORMATION FOR EACH LINE
      DO 1030  I = 1, NLINES
C-----READ LINE INFORMATION
      READ(NIN,501)ILINE,ILX1(ILINE),ILY1(ILINE),ILX2(ILINE),ILY2(ILINE)
      LTEST = NLINE + 7
                    IF ( I . EQ . NLINES )       LTEST = LTEST + 4
                    IF ( LTEST . GT . LINES )    CALL  HEADER
      WRITE (6,602) ILINE,ILX1(ILINE),ILY1(ILINE),ILX2(ILINE),
     *              ILY2(ILINE)
      NLINE = NLINE + 7
                    IF ( ILINE . LE .   0 )      GO TO 8440
                    IF ( ILINE . GT . NLI )      GO TO 8440
            IF ( IUSED(ILINE) . NE .    0 )      GO TO 8450
            IF ( ILX1(ILINE)  . LT .    0 )      GO TO 8460
            IF ( ILX1(ILINE)  . GT . 9999 )      GO TO 8460
            IF ( ILY1(ILINE)  . LT .    0 )      GO TO 8470
            IF ( ILY1(ILINE)  . GT . 9999 )      GO TO 8470
            IF ( ILX2(ILINE)  . LT .    0 )      GO TO 8480
            IF ( ILX2(ILINE)  . GT . 9999 )      GO TO 8480
            IF ( ILY2(ILINE)  . LT .    0 )      GO TO 8490
            IF ( ILY2(ILINE)  . GT . 9999 )      GO TO 8490
      LLINES(I) = ILINE
      IUSED(ILINE) = 1
C-----END OF LINE LOOP
 1030 CONTINUE
      WRITE (6,603) NLINES
      NLINE = NLINE + 4
 1040 CONTINUE
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 8430 CONTINUE
      WRITE (ERRMSG,843) NLINES,NLI
      CALL  PRTERR  ( 'STOP 843 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READLI'                             )
      STOP  843
 8440 CONTINUE
      WRITE (ERRMSG,844) I,ILINE,NLI
      CALL  PRTERR  ( 'STOP 844 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READLI'                             )
      STOP  844
 8450 CONTINUE
      WRITE (ERRMSG,845) ILINE
      CALL  PRTERR  ( 'STOP 845 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READLI'                             )
      STOP  845
 8460 CONTINUE
      WRITE (ERRMSG,846) ILINE,ILX1(ILINE)
      CALL  PRTERR  ( 'STOP 846 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READLI'                             )
      STOP  846
 8470 CONTINUE
      WRITE (ERRMSG,847) ILINE,ILY1(ILINE)
      CALL  PRTERR  ( 'STOP 847 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READLI'                             )
      STOP  847
 8480 CONTINUE
      WRITE (ERRMSG,848) ILINE,ILX2(ILINE)
      CALL  PRTERR  ( 'STOP 848 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READLI'                             )
      STOP  848
 8490 CONTINUE
      WRITE (ERRMSG,849) ILINE,ILY2(ILINE)
      CALL  PRTERR  ( 'STOP 849 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READLI'                             )
      STOP  849
      END                                                               READLI
C
C
C
      SUBROUTINE READSI ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'GEOVAL'
      INCLUDE 'OUTPT'
      INCLUDE 'SDRC'
      DIMENSION         IUSED(NSR)
      INTEGER           IUSED
      INTEGER           I,IZ,J,LTEST,NIN
      INTEGER           ILNB
  501 FORMAT(20I4)
  601 FORMAT(8X,5HTABLE,I3,22H  -  LISTING OF SIGHT ,
     *       32HDISTANCE RESTRICTION COORDINATES,//)
  602 FORMAT(12X,35HSIGHT DISTANCE RESTRICTION NUMBER -,I5,/,
     *       12X,35HX COORDINATE ----------------------,I5,/,
     *       12X,35HY COORDINATE ----------------------,I5,//)
  603 FORMAT(12X,25HTOTAL NUMBER OF POINTS = ,I2,///)
  850 FORMAT(41H NUMBER OF SIGHT DISTANCE RESTRICTIONS = ,I3,8H IS LT 0,
     *       6H OR GT,I3)
  851 FORMAT(34H SIGHT DISTANCE RESTRICTION NUMBER,I3,3H = ,I3,6H IS LE,
     *       8H 0 OR GT,I3)
  852 FORMAT(34H SIGHT DISTANCE RESTRICTION NUMBER,I3,14H IS USED MORE ,
     *       9HTHAN ONCE)
  853 FORMAT(27H SIGHT DISTANCE RESTRICTION,I3,15H X COORDINATE =,I5,
     *       19H IS LT 0 OR GT 9999)
  854 FORMAT(27H SIGHT DISTANCE RESTRICTION,I3,15H Y COORDINATE =,I5,
     *       19H IS LT 0 OR GT 9999)
C
C-----SUBROUTINE READSI READS THE SIGHT DISTANCE RESTRICTION
C-----COORDINATE INFORMATION AND CHECKS FOR ERRORS
C
C-----READ NUMBER OF SIGHT DISTANCE RESTRICTION COORDINATES
      READ(NIN,501) NSDRC
                    IF ( NSDRC . EQ .   0 )      GO TO 1030
                    IF ( NSDRC . LT .   0 )      GO TO 8500
                    IF ( NSDRC . GT . NSR )      GO TO 8500
                    IF ( NLINE+12 . GT . LINES ) CALL  HEADER
      WRITE (6,601) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
      DO 1010  IZ = 1 , NSR
      IUSED(IZ) = 0
 1010 CONTINUE
C-----READ INFORMATION FOR SIGHT DISTANCE RESTRICTION COORDINATES
      DO 1020  I = 1 , NSDRC
C-----READ SIGHT DISTANCE RESTRICTION COORDINATE INFORMATION
      READ(NIN,501) J,IXSDRC(J),IYSDRC(J)
      LTEST = NLINE + 5
                    IF ( I . EQ . NSDRC )        LTEST = LTEST + 4
                    IF ( LTEST . GT . LINES )    CALL  HEADER
      WRITE (6,602) J,IXSDRC(J),IYSDRC(J)
      NLINE = NLINE + 5
                    IF ( J . LE .  0 )           GO TO 8510
                    IF ( J . GT . NSR )          GO TO 8510
                    IF ( IUSED(J) . NE . 0 )     GO TO 8520
                    IF ( IXSDRC(J) . LT .    0 ) GO TO 8530
                    IF ( IXSDRC(J) . GT . 9999 ) GO TO 8530
                    IF ( IYSDRC(J) . LT .    0 ) GO TO 8540
                    IF ( IYSDRC(J) . GT . 9999 ) GO TO 8540
      LSDRC(I) = J
      IUSED(J) = 1
C-----END OF SIGHT DISTANCE RESTRICTION COORDINATE LOOP
 1020 CONTINUE
      WRITE (6,603) NSDRC
      NLINE = NLINE + 4
 1030 CONTINUE
      RETURN
C-----PROCESS INPUT ERROR AND STOP
 8500 CONTINUE
      WRITE (ERRMSG,850) NSDRC,NSR
      CALL  PRTERR  ( 'STOP 850 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READSI'                             )
      STOP  850
 8510 CONTINUE
      WRITE (ERRMSG,851) I,J,NSR
      CALL  PRTERR  ( 'STOP 851 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READSI'                             )
      STOP  851
 8520 CONTINUE
      WRITE (ERRMSG,852) J
      CALL  PRTERR  ( 'STOP 852 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READSI'                             )
      STOP  852
 8530 CONTINUE
      WRITE (ERRMSG,853) J,IXSDRC
      CALL  PRTERR  ( 'STOP 853 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READSI'                             )
      STOP  853
 8540 CONTINUE
      WRITE (ERRMSG,854) J,IYSDRC
      CALL  PRTERR  ( 'STOP 854 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READSI'                             )
      STOP  854
      END                                                               READSI
C
C
C
      SUBROUTINE READOP ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'DATA'
      INCLUDE 'GEOVAL'
      INCLUDE 'OUTPT'
      LOGICAL           NINEOF
      CHARACTER*3       DRVDAT,DRVMIX,VEHDAT
      CHARACTER*8       NNOPLT,NOPT1,NPLTI,NPLTR,NPRIM,NSEPAR,NBLANK,
     *                  NPLT,NSAME,JPATH,JSAME
      CHARACTER*80      ZLINE
      INTEGER           I,J,NCARDS,NCFV,NCFV10,NCFV20,NIN,NUNITS
      INTEGER           ILNB
      REAL              R,SA,SI
      DATA     NBLANK / '        ' /
      DATA     NNOPLT / 'NOPLOT  ' /
      DATA     NOPT1  / 'OPTION1 ' /
      DATA     NPLT   / 'PLOT    ' /
      DATA     NPLTI  / 'PLOTI   ' /
      DATA     NPLTR  / 'PLOTR   ' /
      DATA     NPRIM  / 'PRIMARY ' /
      DATA     NSAME  / 'SAME    ' /
      DATA     NSEPAR / 'SEPARATE' /
  501 FORMAT(3(A8,2X),3F10.2,2I5)
  502 FORMAT(3(A8,2X),3F10.2,I5,F5.0)
  503 FORMAT(A)
  504 FORMAT(I2)
  601 FORMAT(8X,5HTABLE,I3,28H  -  LISTING OF OPTIONS AND ,
     *       15HADDITIONAL DATA,//)
  602 FORMAT(12X,A7,15H PATHS SELECTED,/)
  603 FORMAT(12X,34HPLOT SELECTED USING BALL POINT PEN,/)
  604 FORMAT(12X,27HPLOT SELECTED USING INK PEN,/)
  605 FORMAT(12X,16HNO PLOT SELECTED,/)
  606 FORMAT(12X,40HAPPROACH PATHS PLOTTED ON THE SAME FRAME,/)
  607 FORMAT(12X,41HAPPROACH PATHS PLOTTED ON SEPARATE FRAMES,/)
  608 FORMAT(12X,35HAPPROACH SCALE FACTOR FROM INPUT IS,F7.1,8H FEET PE,
     *       6HR INCH,//,12X,39HINTERSECTION SCALE FACTOR FROM INPUT IS,
     *       F7.1,14H FEET PER INCH,/)
  609 FORMAT(12X,47HA STRAIGHT LINE WILL BE USED FOR A PATH WITH A ,
     *       9HRADIUS GT,F8.2,3H FT,/)
  610 FORMAT(12X,46HPROGRAM CHECKS TO SEE IF THE CENTER TO CENTER ,
     *       8HDISTANCE,/,16X,35HBETWEEN VEHICLES BECOMES LESS THAN ,
     *       11HOR EQUAL TO,I3,5H FEET/)
  611 FORMAT(12X,19HPLOT PAPER WIDTH = ,I2,7H INCHES,/)
  612 FORMAT(12X,A,17H PLOTTER SELECTED,/)
  613 FORMAT(12X,12HPLOT SIZE = ,F5.2,7H INCHES,/)
  614 FORMAT(12X,38HNON-DEVICE-SPECIFIC PLOT FILE SELECTED,/)
  615 FORMAT(12X,30HIMAGE FILE OPTION --------- = ,A,/,
     *       12X,30HIMAGE FILE NAME ----------- = ,A,/,
     *       12X,30HIMAGE FILE TYPE ----------- = ,A,/,
     *       12X,30HIMAGE FILE ATTACHMENT ----- = ,A,/,
     *       12X,30HIMAGE FILE X TRANSLATION -- = ,1X,A,/,
     *       12X,30HIMAGE FILE Y TRANSLATION -- = ,1X,A,/,
     *       12X,30HIMAGE FILE ROTATION (-=CCW) = ,2X,A,/,
     *       12X,30HIMAGE FILE SCALE ---------- = ,A,/)
  616 FORMAT(12X,20HIMAGE FILE OPTION = ,A,/)
  855 FORMAT(16H PATH OPTION = (,A8,30H) IS NE (        )OR(PRIMARY ),
     *       12HOR(OPTION1 ))
  856 FORMAT(16H PLOT OPTION = (,A8,30H) IS NE (        )OR(PLOT    ),
     *       36HOR(PLOTI   )OR(PLOTR   )OR(NOPLOT  ))
  857 FORMAT(21H PATH PLOT OPTION = (,A8,26H) IS NE (        )OR(SAME ,
     *       16H   )OR(SEPARATE))
  858 FORMAT(18H CLOSE DISTANCE = ,I3,17H IS LT 6 OR GT 20)
  859 FORMAT(20H PLOT PAPER WIDTH = ,I3,15H IS NE 12 OR 30)
  870 FORMAT(53H END-OF-FILE ON INPUT BEFORE READING DRIVER MIX CARDS)
  871 FORMAT(55H END-OF-FILE ON INPUT BEFORE READING VEHICLE DATA CARDS)
  872 FORMAT(54H END-OF-FILE ON INPUT BEFORE READING DRIVER DATA CARDS)
  873 FORMAT(54H END-OF-FILE ON INPUT BEFORE READING IMAGE FILE LINE 1)
  874 FORMAT(47H END-OF-FILE ON INPUT READING IMAGE FILE LINE 1)
  875 FORMAT(47H END-OF-FILE ON INPUT READING IMAGE FILE LINE 2)
C
C-----SUBROUTINE READOP READS THE GEOMETRY PROCESSOR OPTIONS AND CHECKS
C-----FOR ERRORS
C
      NINEOF = .FALSE.
                    IF ( NLINE+7 . GT . LINES )  CALL  HEADER
      WRITE (6,601) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
C-----READ GEOPRO OPTIONS
      READ (NIN,502,END=1010)  JPATH,JPLOT,JSAME,SA,SI,R,ICLOSE,XPAPER  CCODE=C]
                    IF ( NLINE . GT . 99999 )    GO TO  990             CCODE=C]
      GO TO 1015                                                        CCODE=C]
  990 CONTINUE
      READ (NIN,501,END=1010)  JPATH,JPLOT,JSAME,SA,SI,R,ICLOSE,IPAPER
      GO TO 1015
 1010 CONTINUE
      NINEOF = .TRUE.
      JPATH  = NBLANK
      JPLOT  = NBLANK
      JSAME  = NBLANK
      SA     = 0.0
      SI     = 0.0
      R      = 0.0
      ICLOSE = 0
      XPAPER = 0.0
 1015 CONTINUE
C-----PROCESS PATH OPTION - DEFAULT IS (PRIMARY )
                    IF ( JPATH . EQ . NBLANK )   GO TO 1020
                    IF ( JPATH . EQ . NPRIM )    GO TO 1030
                    IF ( JPATH . EQ . NOPT1 )    GO TO 1040
      GO TO 8550
 1020 CONTINUE
      JPATH = NPRIM
 1030 CONTINUE
C-----PATH OPTION IS (PRIMARY )
      IPATH = 1
      GO TO 1050
 1040 CONTINUE
C-----PATH OPTION IS (OPTION1 )
      IPATH = 2
 1050 CONTINUE
      WRITE (6,602) JPATH
      NLINE = NLINE + 2
C-----PROCESS PLOT OPTION - DEFAULT IS (PLOT    )
                    IF ( JPLOT . EQ . NBLANK )   GO TO 2010
                    IF ( JPLOT . EQ . NPLT )     GO TO 2010
                    IF ( JPLOT . EQ . NNOPLT )   GO TO 2040
                    IF ( NLINE . GT . 99999 )    GO TO 2009             CCODE=C]
      GO TO 8560                                                        CCODE=C]
C<    GO TO 8560
C:    GO TO 8560
 2009 CONTINUE
                    IF ( JPLOT . EQ . NPLTI )    GO TO 2020
                    IF ( JPLOT . EQ . NPLTR )    GO TO 2030
      GO TO 8560
 2010 CONTINUE
C-----PLOT OPTION IS (PLOT    )
      IPLOT = 1
      WRITE (6,614)                                                     CCODE=C]
                    IF ( NLINE . GT . 99999 )    GO TO 2019             CCODE=C]
      GO TO 2090                                                        CCODE=C]
C<    WRITE (6,612) 'H. P.'
C<    GO TO 2090
C:    WRITE (6,612) 'ZETA'
C:    GO TO 2090
 2019 CONTINUE
      WRITE (6,603)
      GO TO 2090
 2020 CONTINUE
C-----PLOT OPTION IS (PLOTI   )
      IPLOT = 2
      WRITE (6,604)
      GO TO 2090
 2030 CONTINUE
C-----PLOT OPTION IS (PLOTR   )
      IPLOT = 3
      WRITE (6,603)
      GO TO 2090
 2040 CONTINUE
C-----PLOT OPTION IS (NOPLOT  )
      IPLOT = 4
      WRITE (6,605)
 2090 CONTINUE
      NLINE = NLINE + 2
                    IF ( IPLOT . EQ . 4 )        GO TO 4010
                    IF ( NLINE+6 . GT . LINES )  CALL  HEADER
C-----PROCESS PATH PLOT OPTION - DEFAULT IS (SEPARATE)
            IF ( JSAME(1:7) . EQ . NSEPAR(1:7) ) JSAME = NSEPAR
                    IF ( JSAME . EQ . NBLANK )   GO TO 3020
                    IF ( JSAME . EQ . NSAME  )   GO TO 3010
                    IF ( JSAME . EQ . NSEPAR )   GO TO 3020
      GO TO 8570
 3010 CONTINUE
C-----PATH PLOT OPTION IS (SAME    )
      ISAME = 1
      WRITE (6,606)
      GO TO 3030
 3020 CONTINUE
C-----PATH PLOT OPTION IS (SEPARATE)
      ISAME = 2
      WRITE (6,607)
 3030 CONTINUE
      NLINE = NLINE + 2
C-----PROCESS PLOT SCALE FACTOR FOR APPROACH AND INTERSECTION
      WRITE (6,608) SA,SI
      NLINE = NLINE + 4
      SCALEA = DBLE(SA)
      SCALEI = DBLE(SI)
      SCALEL = SCALEI
      SCALER = SCALEI
 4010 CONTINUE
C-----PROCESS MAXIMUM PATH RADIUS - DEFAULT IS 500.0
                    IF ( R . EQ .   0.0 )        R = 500.0
      R = AMIN1( AMAX1( R,100.0 ),9999.0 )
                    IF ( NLINE+2 . GT . LINES  ) CALL  HEADER
      WRITE (6,609) R
      NLINE = NLINE + 2
      RADIUS = DBLE(R)
C-----PROCESS CLOSE DISTANCE - DEFAULT IS 10
                    IF ( ICLOSE . EQ .  0 )      ICLOSE = 10
                    IF ( ICLOSE . LT .  6 )      GO TO 8580
                    IF ( ICLOSE . GT . 20 )      GO TO 8580
                    IF ( NLINE+3 . GT . LINES )  CALL  HEADER
      WRITE (6,610) ICLOSE
      NLINE = NLINE + 3
                    IF ( IPLOT . EQ . 4 )        GO TO 4020
                    IF ( XPAPER . LE . 0.0   )   XPAPER =  7.5          CCODE=C]
                    IF ( NLINE  . GT . 99999 )   GO TO 4014             CCODE=C]
      GO TO 4015                                                        CCODE=C]
C<                  IF ( IPAPER . LE .  0 )      IPAPER =  8
C<                  IF ( IPAPER . GT . 11 )      IPAPER = 11
C<    GO TO 4015
C:                  IF ( IPAPER . LE .  0 )      IPAPER = 12
C:                  IF ( IPAPER . GT . 12 )      IPAPER = 12
C:    GO TO 4015
 4014 CONTINUE
                    IF ( IPAPER . EQ . 0 )       IPAPER = 30
                    IF ( IPLOT . EQ . 3 )        IPAPER = 12
      IF ( IPAPER.NE.12 . AND . IPAPER.NE.30 )   GO TO 8590
 4015 CONTINUE
                    IF ( NLINE+2 . GT . LINES )  CALL  HEADER
      WRITE (6,613) XPAPER                                              CCODE=C]
      NLINE = NLINE + 2                                                 CCODE=C]
                    IF ( NLINE   . GT . 99999 )  GO TO 4017             CCODE=C]
      GO TO 4020                                                        CCODE=C]
 4017 CONTINUE
      WRITE (6,611) IPAPER
      NLINE = NLINE + 2
 4020 CONTINUE
C-----DUMMY READ USER-SPECIFIED VEHICLE AND DRIVER DATA
      DRVMIX = 'NO'
      VEHDAT = 'NO'
      DRVDAT = 'NO'
      IF ( .NOT. NINEOF )                        THEN
        IF ( (NVEHCL.LE.15) .AND. (NDRICL.LE.5) )THEN
          NCARDS = 1
        ELSE
          NCARDS = 1 + INT( (NVEHCL+25)/26 ) + 1
        END IF
        DO 5010  I = 1 , NCARDS
        READ (NIN,503,END=5020) ZLINE
        IF ( I . EQ . 1 )                        THEN
          DRVMIX = ZLINE(1:3)
          VEHDAT = ZLINE(4:6)
          DRVDAT = ZLINE(7:9)
        END IF
 5010   CONTINUE
        GO TO 5030
      END IF
 5020 CONTINUE
      NINEOF = .TRUE.
 5030 CONTINUE
      CALL  TOUPR   ( DRVMIX )
      CALL  TOUPR   ( VEHDAT )
      CALL  TOUPR   ( DRVDAT )
C ----- PROCESS DRIVER MIX CARDS, IF ANY
      IF ( DRVMIX . EQ . 'YES' )                 THEN
        IF ( NINEOF )                            GO TO 8700
        DO 6010 I = 1 , NVEHCL
        READ(NIN,503,END=6040)
 6010   CONTINUE
      END IF
C ----- PROCESS VEHICLE DATA CARDS, IF ANY
C ----- LENGTH, OPER_CHAR, MAX_DECEL, MAX_ACCEL, MAX_VEL, AND MIN_RADIUS
C   1   gdvsim GDV_Veh_Length         - 1 card for for every 20 vehicles
C   2   gdvsim GDV_Veh_OperChar       - 1 card for for every 20 vehicles
C   3   gdvsim GDV_Veh_MaxDecel       - 1 card for for every 20 vehicles
C   4   gdvsim GDV_Veh_MaxAccel       - 1 card for for every 20 vehicles
C   5   gdvsim GDV_Veh_MaxVel         - 1 card for for every 20 vehicles
C   6   gdvsim GDV_Veh_MinRad         - 1 card for for every 20 vehicles
C ----- CLASSIFICATION, HEIGHT, WIDTH, AND UNITS
C   7   gdvsim GDV_Veh_Classification - 1 card for for every 10 vehicles
C   8   gdvsim GDV_Veh_Height         - 1 card for for every 20 vehicles
C   9   gdvsim GDV_Veh_Width          - 1 card for for every 20 vehicles
C  10   gdvsim GDV_Veh_Units          - 1 card for for every  4 unit/veh
      IF ( VEHDAT . EQ . 'YES' )                 THEN
        IF ( NINEOF )                            GO TO 8710
        NCFV10 = (NVEHCL+ 9)/10
        NCFV20 = (NVEHCL+19)/20
        NCFV   = 6*NCFV20
        IF ( NVEHAT . GE .  7 )                  NCFV = NCFV + NCFV10
        IF ( NVEHAT . GE .  8 )                  NCFV = NCFV + NCFV20
        IF ( NVEHAT . GE .  9 )                  NCFV = NCFV + NCFV20
        DO 6020  I = 1 , NCFV
        READ(NIN,503,END=6040)
 6020   CONTINUE
        IF ( NVEHAT . GE . 10 )                  THEN
          DO 6026  I = 1 , NVEHCL
          READ(NIN,504,END=6040) NUNITS
          IF ( NUNITS . LE . 4 )                 GO TO 6026
          DO 6023  J = 5 , NUNITS , 4
          READ(NIN,503,END=6040)
 6023     CONTINUE
 6026     CONTINUE
        END IF
      END IF
C ----- PROCESS DRIVER DATA CARDS, IF ANY
C ----- OPER_CHAR AND PIJR TIME
      IF ( DRVDAT . EQ . 'YES' )                 THEN
        IF ( NINEOF )                            GO TO 8720
        DO 6030 I = 1 , 2
        READ(NIN,503,END=6040)
 6030   CONTINUE
        GO TO 6050
      END IF
      GO TO 6050
 6040 CONTINUE
      NINEOF = .TRUE.
 6050 CONTINUE
C-----PROCESS IMAGE FILE DATA, IF ANY
      IF ( IMAGOP . EQ . 'YES' )                 THEN
        IF ( NINEOF )                            GO TO 8730
        READ (NIN,503,END=8740) IMAGL1
        READ (NIN,503,END=8750) IMAGL2
                    IF ( NLINE+9 . GT . LINES )  CALL  HEADER
        WRITE (6,615) IMAGOP,
     *                IMAGL1(1:ILNB( IMAGL1 )),
     *                IMAGL2( 1: 3),
     *                IMAGL2( 4:11),
     *                IMAGL2(12:18),
     *                IMAGL2(19:25),
     *                IMAGL2(26:41),
     *                IMAGL2(42:59)
        NLINE = NLINE + 9
      ELSE
                    IF ( NLINE+2 . GT . LINES )  CALL  HEADER
        WRITE (6,616) IMAGOP
        NLINE = NLINE + 2
      END IF
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 8550 CONTINUE
      WRITE (ERRMSG,855) JPATH
      CALL  PRTERR  ( 'STOP 855 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  855
 8560 CONTINUE
      WRITE (ERRMSG,856) JPLOT
      CALL  PRTERR  ( 'STOP 856 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  856
 8570 CONTINUE
      WRITE (ERRMSG,857) JSAME
      CALL  PRTERR  ( 'STOP 857 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  857
 8580 CONTINUE
      WRITE (ERRMSG,858) ICLOSE
      CALL  PRTERR  ( 'STOP 858 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  858
 8590 CONTINUE
      WRITE (ERRMSG,859) IPAPER
      CALL  PRTERR  ( 'STOP 859 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  859
 8700 CONTINUE
      WRITE (ERRMSG,870)
      CALL  PRTERR  ( 'STOP 870 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  870
 8710 CONTINUE
      WRITE (ERRMSG,871)
      CALL  PRTERR  ( 'STOP 871 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  871
 8720 CONTINUE
      WRITE (ERRMSG,872)
      CALL  PRTERR  ( 'STOP 872 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  872
 8730 CONTINUE
      WRITE (ERRMSG,873)
      CALL  PRTERR  ( 'STOP 873 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  873
 8740 CONTINUE
      WRITE (ERRMSG,874)
      CALL  PRTERR  ( 'STOP 874 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  874
 8750 CONTINUE
      WRITE (ERRMSG,875)
      CALL  PRTERR  ( 'STOP 875 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READOP'                             )
      STOP  875
      END                                                               READOP
C
C
C
      SUBROUTINE WRITAL
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ARC'
      INCLUDE 'LINE'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'OUTPT'
      INCLUDE 'TITLE'
      CHARACTER*4       IDIA
      INTEGER           IARC,IARCN,ILINE,ILINEN
      INTEGER           ILNB
  601 FORMAT(A80)
  602 FORMAT(I4,A4,A5,A3)
  603 FORMAT(20I4)
  604 FORMAT(A)
C
C-----SUBROUTINE WRITAL WRITES THE TITLE FOR GEOPRO, THE ARC
C-----INFORMATION, AND THE LINE INFORMATION ONTO TAPE MODELT FOR SIMPRO
C
      REWIND(MODELT)
C-----WRITE THE TITLE FOR GEOPRO ONTO MODELT
      WRITE (MODELT,601) ITITLE
C-----WRITE THE ARC INFORMATION ONTO MODELT
      IF ( DIAMON )                              THEN
        IDIA = ' DIA'
      ELSE
        IDIA = '    '
      END IF
      WRITE (MODELT,602) NARCS,IDIA,IVERSN,IMAGOP
                    IF ( NARCS . LE . 0 )        GO TO 1020
      DO 1010  IARCN = 1 , NARCS
      IARC = LARCS(IARCN)
      WRITE (MODELT,603) IARC,IARCX(IARC),IARCY(IARC),IARCAZ(IARC),
     *                   IARCSW(IARC),IARCR(IARC)
 1010 CONTINUE
 1020 CONTINUE
C-----WRITE THE LINE INFORMATION ONTO MODELT
      WRITE (MODELT,603) NLINES
                    IF ( NLINES . LE . 0 )       GO TO 2020
      DO 2010  ILINEN = 1 , NLINES
      ILINE = LLINES(ILINEN)
      WRITE (MODELT,603) ILINE,ILX1(ILINE),ILY1(ILINE),ILX2(ILINE),
     *                   ILY2(ILINE)
 2010 CONTINUE
 2020 CONTINUE
C-----WRITE IMAGE FILE DATA, IF ANY
      IF ( IMAGOP . EQ . 'YES' )                 THEN
        WRITE (MODELT,604) IMAGL1(1:ILNB( IMAGL1 ))
        WRITE (MODELT,604) IMAGL2(1:ILNB( IMAGL2 ))
      END IF
      RETURN
      END                                                               WRITAL
C
C
C
      SUBROUTINE FNDXYP
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'LANE'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'INDEX'
      INCLUDE 'OUTPT'
      INCLUDE 'PLOTTR'
      INCLUDE 'RADIAN'
      INTEGER           NSCALE
      PARAMETER       ( NSCALE = 24 )
      DIMENSION         SCALEF(NSCALE)
      DOUBLE PRECISION  SCALEF
      DOUBLE PRECISION  DAI,DW,DXI,DYI
      INTEGER           I,IX,IY,LGEOM1,LGEOM2,LGEOM3,LGEOM4
      REAL              SA,SI
      DATA     SCALEF /   10.0D0,  15.0D0,  20.0D0,  25.0D0,
     *                    30.0D0,  40.0D0,  50.0D0,  75.0D0,
     *                   100.0D0, 150.0D0, 200.0D0, 250.0D0,
     *                   300.0D0, 400.0D0, 500.0D0, 750.0D0,
     *                  1000.0D0,1500.0D0,2000.0D0,2500.0D0,
     *                  3000.0D0,4000.0D0,5000.0D0,7500.0D0 /
  601 FORMAT(12X,35HAPPROACH SCALE FACTOR TO BE USED IS,F7.1,5H FEET,
     *       9H PER INCH,//,12X,36HINTERSECTION SCALE FACTOR TO BE USED,
     *       3H IS,F7.1,14H FEET PER INCH)
  602 FORMAT(/)
C
C-----SUBROUTINE FNDXYP FINDS THE X AND Y COORDINATES FOR A POINT AT THE
C-----MIDDLE AND END OF EACH INBOUND LANE AND AT THE MIDDLE AND START OF
C-----EACH OUTBOUND LANE THAT IS AVAILABLE AT THE INTERSECTION, FINDS
C-----THE BOUNDARIES FOR PLOTTING, AND FINDS THE PLOT SCALE FACTORS
C
C-----PROCESS EACH INBOUND APPROACH
      DO 1040  IAN = 1 , NIBA
      IA = LIBA(IAN)
      DXI = 0.0D0
C-----PROCESS EACH LANE OF THE INBOUND APPROACH
      DO 1030  ILN = 1 , NLANES(IA)
      IL = LLANES(ILN,IA)
      DW = 0.5D0*DBLE(LWID(IL))
      DXI = DXI + DW
      LGEOM3 = LGEOM(3,IL)
      LGEOM4 = LGEOM(4,IL)
      DYI = LGEOM4
      IXAPP(IL) = -1
      IYAPP(IL) = -1
                    IF ( LGEOM3 . EQ . LGEOM4 )  GO TO 1010
C-----FIND THE X AND Y COORDINATES FOR THE END OF THE LANE
      CALL  XROTAI  ( DXI,DYI,IAAZIM(IA),IAPX(IA),IAPY(IA),IXAPP(IL),
     *                IYAPP(IL) )
      DAI = DYI - 5.0D0
C-----FIND THE X AND Y COORDINATES FOR THE LOCATION OF THE TURN
C-----DIRECTION ARROWS
      CALL  XROTAI  ( DXI,DAI,IAAZIM(IA),IAPX(IA),IAPY(IA),LTDIRX(IL),
     *                LTDIRY(IL) )
 1010 CONTINUE
      LGEOM1 = LGEOM(1,IL)
C-----FIND THE BOUNDARIES FOR THE APPROACH PLOT
      CALL  XROTAI  ( DXI-DW,DBLE(LGEOM1),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXA = MIN0( MINXA,IX )
      MAXXA = MAX0( MAXXA,IX )
      MINYA = MIN0( MINYA,IY )
      MAXYA = MAX0( MAXYA,IY )
      CALL  XROTAI  ( DXI+DW,DBLE(LGEOM1),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXA = MIN0( MINXA,IX )
      MAXXA = MAX0( MAXXA,IX )
      MINYA = MIN0( MINYA,IY )
      MAXYA = MAX0( MAXYA,IY )
                    IF ( LGEOM3 . EQ . LGEOM4 )  GO TO 1020
C-----FIND THE BOUNDARIES FOR THE INTERSECTION PLOT
      CALL  XROTAI  ( DXI-DW,DBLE(LGEOM4),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXI = MIN0( MINXI,IX )
      MAXXI = MAX0( MAXXI,IX )
      MINYI = MIN0( MINYI,IY )
      MAXYI = MAX0( MAXYI,IY )
      CALL  XROTAI  ( DXI+DW,DBLE(LGEOM4),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXI = MIN0( MINXI,IX )
      MAXXI = MAX0( MAXXI,IX )
      MINYI = MIN0( MINYI,IY )
      MAXYI = MAX0( MAXYI,IY )
                    IF ( .NOT. DIAMON )          GO TO 1020
                    IF ( ISFLAG(IA) . NE . 'L' ) GO TO 1015
C-----FIND THE BOUNDARIES FOR THE INTERSECTION PLOT DIAMOND LEFT SIDE
      CALL  XROTAI  ( DXI-DW,DBLE(LGEOM4),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXL = MIN0( MINXL,IX )
      MAXXL = MAX0( MAXXL,IX )
      MINYL = MIN0( MINYL,IY )
      MAXYL = MAX0( MAXYL,IY )
      CALL  XROTAI  ( DXI+DW,DBLE(LGEOM4),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXL = MIN0( MINXL,IX )
      MAXXL = MAX0( MAXXL,IX )
      MINYL = MIN0( MINYL,IY )
      MAXYL = MAX0( MAXYL,IY )
 1015 CONTINUE
                    IF ( ISFLAG(IA) . NE . 'R' ) GO TO 1020
C-----FIND THE BOUNDARIES FOR THE INTERSECTION PLOT DIAMOND RIGHT SIDE
      CALL  XROTAI  ( DXI-DW,DBLE(LGEOM4),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXR = MIN0( MINXR,IX )
      MAXXR = MAX0( MAXXR,IX )
      MINYR = MIN0( MINYR,IY )
      MAXYR = MAX0( MAXYR,IY )
      CALL  XROTAI  ( DXI+DW,DBLE(LGEOM4),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXR = MIN0( MINXR,IX )
      MAXXR = MAX0( MAXXR,IX )
      MINYR = MIN0( MINYR,IY )
      MAXYR = MAX0( MAXYR,IY )
 1020 CONTINUE
C-----FIND THE DISTANCE TO THE CENTER OF THE LANE FROM THE CENTER LINE
C-----OF THE APPROACH
      IDX(IL) = DXI + XROUND
      DXI = DXI + LWID(IL) - DW
C-----END OF LANE LOOP
 1030 CONTINUE
C-----END OF INBOUND APPROACH LOOP
 1040 CONTINUE
C-----PROCESS EACH OUTBOUND APPROACH
      DO 2040  IAN = 1 , NOBA
      IA = LOBA(IAN)
      DXI = 0.0D0
C-----PROCESS EACH LANE OF OUTBOUND APPROACH
      DO 2030  ILN = 1 , NLANES(IA)
      IL = LLANES(ILN,IA)
      DW = 0.5D0*DBLE(LWID(IL))
      DXI = DXI + DW
      LGEOM1 = LGEOM(1,IL)
      LGEOM2 = LGEOM(2,IL)
      DYI = LGEOM1
      IXAPP(IL) = -1
      IYAPP(IL) = -1
                    IF ( LGEOM1 . EQ . LGEOM2 )  GO TO 2010
C-----FIND THE X AND Y COORDINATES FOR THE START OF THE LANE
      CALL  XROTAI  ( DXI,DYI,IAAZIM(IA),IAPX(IA),IAPY(IA),IXAPP(IL),
     *                IYAPP(IL) )
      DAI = DYI + 15.0D0
C-----FIND THE X AND Y COORDINATES FOR THE LOCATION OF THE TURN
C-----DIRECTION ARROWS
      CALL  XROTAI  ( DXI,DAI,IAAZIM(IA),IAPX(IA),IAPY(IA),LTDIRX(IL),
     *                LTDIRY(IL) )
 2010 CONTINUE
      LGEOM4 = LGEOM(4,IL)
C-----FIND THE BOUNDARIES FOR THE APPROACH PLOT
      CALL  XROTAI  ( DXI-DW,DBLE(LGEOM4),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXA = MIN0( MINXA,IX )
      MAXXA = MAX0( MAXXA,IX )
      MINYA = MIN0( MINYA,IY )
      MAXYA = MAX0( MAXYA,IY )
      CALL  XROTAI  ( DXI+DW,DBLE(LGEOM4),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXA = MIN0( MINXA,IX )
      MAXXA = MAX0( MAXXA,IX )
      MINYA = MIN0( MINYA,IY )
      MAXYA = MAX0( MAXYA,IY )
                    IF ( LGEOM1 . EQ . LGEOM2 )  GO TO 2020
C-----FIND THE BOUNDARIES FOR THE INTERSECTION PLOT
      CALL  XROTAI  ( DXI-DW,DBLE(LGEOM1),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXI = MIN0( MINXI,IX )
      MAXXI = MAX0( MAXXI,IX )
      MINYI = MIN0( MINYI,IY )
      MAXYI = MAX0( MAXYI,IY )
      CALL  XROTAI  ( DXI+DW,DBLE(LGEOM1),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXI = MIN0( MINXI,IX )
      MAXXI = MAX0( MAXXI,IX )
      MINYI = MIN0( MINYI,IY )
      MAXYI = MAX0( MAXYI,IY )
                    IF ( .NOT. DIAMON )          GO TO 2020
                    IF ( ISFLAG(IA) . NE . 'L' ) GO TO 2015
C-----FIND THE BOUNDARIES FOR THE INTERSECTION PLOT DIAMOND LEFT SIDE
      CALL  XROTAI  ( DXI-DW,DBLE(LGEOM1),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXL = MIN0( MINXL,IX )
      MAXXL = MAX0( MAXXL,IX )
      MINYL = MIN0( MINYL,IY )
      MAXYL = MAX0( MAXYL,IY )
      CALL  XROTAI  ( DXI+DW,DBLE(LGEOM1),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXL = MIN0( MINXL,IX )
      MAXXL = MAX0( MAXXL,IX )
      MINYL = MIN0( MINYL,IY )
      MAXYL = MAX0( MAXYL,IY )
 2015 CONTINUE
                    IF ( ISFLAG(IA) . NE . 'R' ) GO TO 2020
C-----FIND THE BOUNDARIES FOR THE INTERSECTION PLOT DIAMOND RIGHT SIDE
      CALL  XROTAI  ( DXI-DW,DBLE(LGEOM1),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXR = MIN0( MINXR,IX )
      MAXXR = MAX0( MAXXR,IX )
      MINYR = MIN0( MINYR,IY )
      MAXYR = MAX0( MAXYR,IY )
      CALL  XROTAI  ( DXI+DW,DBLE(LGEOM1),IAAZIM(IA),IAPX(IA),
     *                IAPY(IA),IX,IY )
      MINXR = MIN0( MINXR,IX )
      MAXXR = MAX0( MAXXR,IX )
      MINYR = MIN0( MINYR,IY )
      MAXYR = MAX0( MAXYR,IY )
 2020 CONTINUE
C-----FIND THE DISTANCE TO THE CENTER OF THE LANE FROM THE CENTER LINE
C-----OF THE APPROACH
      IDX(IL) = DXI + XROUND
      DXI = DXI + LWID(IL) - DW
C-----END OF LANE LOOP
 2030 CONTINUE
C-----END OF OUTBOUND APPROACH LOOP
 2040 CONTINUE
C-----ADD 1 FOOT BORDERS FOR APPROACH PLOT BOUNDARIES
      MINXA = MINXA - 1
      MINYA = MINYA - 1
      MAXXA = MAXXA + 1
      MAXYA = MAXYA + 1
C-----ADD 1 FOOT BORDERS FOR INTERSECTION PLOT BOUNDARIES AND ENSURE
C-----THAT AT LEAST THE LAST 20 FEET OF EACH INBOUND LANE AND THE FIRST
C-----20 FEET OF EACH OUTBOUND LANE WILL BE PLOTTED
      MINXI = MINXI - 21
      MINYI = MINYI - 21
      MAXXI = MAXXI + 21
      MAXYI = MAXYI + 21
      IF ( DIAMON )                              THEN
        MINXL = MINXL - 21
        MINYL = MINYL - 21
        MAXXL = MAXXL + 21
        MAXYL = MAXYL + 21
        MINXR = MINXR - 21
        MINYR = MINYR - 21
        MAXXR = MAXXR + 21
        MAXYR = MAXYR + 21
      END IF
                    IF ( IPLOT . EQ . 4 )        GO TO 4040
      PWID = XPAPER                                                     CCODE=C]
                    IF ( NLINE . GT . 99999 )    GO TO 3003             CCODE=C]
      GO TO 3005                                                        CCODE=C]
 3003 CONTINUE
C<    IF ( IPAPER . LE . 8 )                     THEN
C<      PWID = IPAPER - 0.5
C<      GO TO 3005
C<    END IF
      PWID = IPAPER - 1
 3005 CONTINUE
                    IF ( SCALEA . LE . 0.0D0 )   GO TO 3010
C-----CHECK APPROACH PLOT SCALE FACTOR FROM INPUT
      XSIZEA = (MAXXA-MINXA)/SCALEA
      YSIZEA = (MAXYA-MINYA)/SCALEA
      CSIZEA = XSIZEA/80.0D0
      IF ( YSIZEA+ 8.0D0*CSIZEA.LE.DBLE(PWID) . AND .
     *     XSIZEA              .LE.DBLE(PWID) )  GO TO 3030
 3010 CONTINUE
C-----FIND APPROACH PLOT SCALE FACTOR THAT WILL MAKE THE PLOT AS LARGE
C-----AS POSSIBLE ON THE PLOT PAGE
      DO 3020  I = 1 , NSCALE
      SCALEA = SCALEF(I)
      XSIZEA = (MAXXA-MINXA)/SCALEA
      YSIZEA = (MAXYA-MINYA)/SCALEA
      CSIZEA = XSIZEA/80.0D0
      IF ( YSIZEA+ 8.0D0*CSIZEA.LE.DBLE(PWID) . AND .
     *     XSIZEA              .LE.DBLE(PWID) )  GO TO 3030
 3020 CONTINUE
      GO TO 9010
 3030 CONTINUE
                    IF ( SCALEI . LE . 0.0D0 )   GO TO 4010
C-----CHECK INTERSECTION PLOT SCALE FACTOR FROM INPUT
      XSIZEI = (MAXXI-MINXI)/SCALEI
      YSIZEI = (MAXYI-MINYI)/SCALEI
      CSIZEI = XSIZEI/80.0D0
      IF ( YSIZEI+10.0D0*CSIZEI.LE.DBLE(PWID) . AND .
     *     XSIZEI              .LE.DBLE(PWID) )  GO TO 4030
 4010 CONTINUE
C-----FIND INTERSECTION SCALE FACTOR THAT WILL MAKE THE PLOT AS LARGE
C-----AS POSSIBLE ON THE PLOT PAGE
      DO 4020  I = 1 , NSCALE
      SCALEI = SCALEF(I)
      XSIZEI = (MAXXI-MINXI)/SCALEI
      YSIZEI = (MAXYI-MINYI)/SCALEI
      CSIZEI = XSIZEI/80.0D0
      IF ( YSIZEI+10.0D0*CSIZEI.LE.DBLE(PWID) . AND .
     *     XSIZEI              .LE.DBLE(PWID) )  GO TO 4030
 4020 CONTINUE
      GO TO 9020
 4030 CONTINUE
                    IF ( .NOT. DIAMON )          GO TO 4038
                    IF ( SCALEL . LE . 0.0D0 )   GO TO 4032
C-----CHECK INTERSECTION PLOT SCALE FACTOR FROM INPUT
      XSIZEL = MAX0(MAXXL-MINXL,MAXXR-MINXR)/SCALEL
      YSIZEL = MAX0(MAXYL-MINYL,MAXYR-MINYR)/SCALEL
      CSIZEL = XSIZEL/80.0D0
      IF ( YSIZEL+10.0D0*CSIZEL.LE.DBLE(PWID) . AND .
     *     XSIZEL              .LE.DBLE(PWID) )  GO TO 4036
 4032 CONTINUE
C-----FIND INTERSECTION SCALE FACTOR THAT WILL MAKE THE PLOT AS LARGE
C-----AS POSSIBLE ON THE PLOT PAGE FOR DIAMOND LEFT SIDE
      DO 4034  I = 1 , NSCALE
      SCALEL = SCALEF(I)
      XSIZEL = MAX0(MAXXL-MINXL,MAXXR-MINXR)/SCALEL
      YSIZEL = MAX0(MAXYL-MINYL,MAXYR-MINYR)/SCALEL
      CSIZEL = XSIZEL/80.0D0
      IF ( YSIZEL+10.0D0*CSIZEL.LE.DBLE(PWID) . AND .
     *     XSIZEL              .LE.DBLE(PWID) )  GO TO 4036
 4034 CONTINUE
      GO TO 9020
 4036 CONTINUE
      SCALER = SCALEL
      XSIZER = XSIZEL
      YSIZER = YSIZEL
      CSIZER = CSIZEL
 4038 CONTINUE
C-----PRINT APPROACH AND INTERSECTION PLOT SCALE FACTOR TO BE USED
                    IF ( NLINE+5 . GT . LINES )  CALL  HEADER
      SA = SCALEA
      SI = SCALEI
      WRITE (6,601) SA,SI
      NLINE = NLINE + 3
 4040 CONTINUE
      WRITE (6,602)
      NLINE = NLINE + 2
      RETURN
C-----PROCESS THE EXECUTION ERRORS AND STOP
 9010 CONTINUE
      CALL  ABORTR  ( 'STOP 901 - ' //
     *                'NO SCALE FACTOR ON SCALEF LIST WILL ALLOW ' //
     *                'THE APPROACH TO BE PLOTTED - ' //
     *                'FNDXYP'                                        )
      STOP  901
 9020 CONTINUE
      CALL  ABORTR  ( 'STOP 902 - ' //
     *                'NO SCALE FACTOR ON SCALEF LIST WILL ALLOW ' //
     *                'THE INTERSECTION TO BE PLOTTED - ' //
     *                'FNDXYP'                                        )
      STOP  902
      END                                                               FNDXYP
C
C
C
      SUBROUTINE FNDSDR
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'LANE'
      INCLUDE 'SDR'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'INDEX'
      INCLUDE 'OUTPT'
      INCLUDE 'SDRC'
      DOUBLE PRECISION  DUM,DX1,DX2,DY1,XFROM,XINT,XSDR,X1,X2,X3,
     *                  X4,YFROM,YINT,YSDR,Y1,Y2,Y3,Y4
      INTEGER           I,IAZIM,IMAXL,INDEX,ISDRC,ISDRCN,ISEE,ISTART,
     *                  ISTOP,ITEST,IXCLAP,IYCLAP,JMAXL,MAXSEE,NSDRAP
      INTEGER           ILNB,LDOWN,LTOL
  601 FORMAT(8X,5HTABLE,I3,39H  -  LISTING OF SIGHT DISTANCE RESTRICT,
     *       11HION ENTRIES,//)
  602 FORMAT(12X,32HSIGHT DISTANCE RESTRICTION ENTRY,I3,10H IS NUMBER,
     *       I2,13H FOR APPROACH,I3,/,15X,21HAND INVOLVES APPROACH,I3,/)
  603 FORMAT(15X,8HAPPROACH,I3,5H FROM,I5,3H TO,I5,9H CAN SEE ,
     *       8HAPPROACH,I3,5H FROM,I5,3H TO,I5)
  604 FORMAT(/)
  904 FORMAT(53HNUMBER OF SIGHT DISTANCE RESTRICTIONS FOR APPROACH IS,
     *       3H GT,I3)
  905 FORMAT(48HTOTAL NUMBER OF SIGHT DISTANCE RESTRICTION IS GT,I3)
C
C-----SUBROUTINE FNDSDR FINDS THE SIGHT DISTANCE RESTRICTIONS BETWEEN
C-----THE INBOUND APPROACHES
C
                    IF ( NSDRC . LE . 0 )        RETURN
      NSDRS = 1
C-----PROCESS EACH INBOUND APPROACH
      DO 3020  IAN = 1 , NIBA
      IA = LIBA(IAN)
      DX1 = 0.0D0
      IMAXL = 0
C-----FIND THE CENTER OF THE LANES FOR THE APPROACH AND THE MAXIMUM LANE
C-----LENGTH DOWN THE APPROACH
      DO 1010  ILN = 1 , NLANES(IA)
      IL = LLANES(ILN,IA)
      DX1 = DX1 + 0.5D0*DBLE(LWID(IL))
      IF ( LGEOM(3,IL) . EQ . LGEOM(4,IL) )      GO TO 1010
      IMAXL = MAX0( IMAXL,LGEOM(4,IL) )
 1010 CONTINUE
      IAZIM = IAAZIM(IA)
      IXCLAP = IAPX(IA)
      IYCLAP = IAPY(IA)
      NSDRAP = 0
C-----CHECK AGAINST EACH OTHER INBOUND APPROACH
      DO 3010  JAN = 1 , NIBA
                    IF ( IAN . EQ . JAN )        GO TO 3010
      JA = LIBA(JAN)
C-----IF THE APPROACHES ARE NOT ON THE SAME SIDE THEN THERE IS NO
C-----SIGHT DISTANCE RESTRICTION BETWEEN THESE APPROACHES
            IF ( ISFLAG(IA) . NE . ISFLAG(JA) )  GO TO 3010
C-----IF THE APPROACHES GO IN PARALLEL DIRECTIONS THEN THERE IS NO
C-----SIGHT DISTANCE RESTRICTION BETWEEN THESE APPROACHES
            IF ( IAAZIM(JA) . EQ . IAZIM     )   GO TO 3010
            IF ( IAAZIM(JA) . EQ . IAZIM+360 )   GO TO 3010
            IF ( IAAZIM(JA) . EQ . IAZIM-360 )   GO TO 3010
            IF ( IAAZIM(JA)+360 . EQ . IAZIM )   GO TO 3010
            IF ( IAAZIM(JA)-360 . EQ . IAZIM )   GO TO 3010
            IF ( IAAZIM(JA) . EQ . IAZIM+180 )   GO TO 3010
            IF ( IAAZIM(JA) . EQ . IAZIM-180 )   GO TO 3010
            IF ( IAAZIM(JA)+180 . EQ . IAZIM )   GO TO 3010
            IF ( IAAZIM(JA)-180 . EQ . IAZIM )   GO TO 3010
      DX2 = 0.0D0
      JMAXL = 0
C-----FIND THE CENTER OF THE LANES FOR THE APPROACH BEING CHECKED
C-----AGAINST AND THE MAXIMUM LANE LENGTH DOWN THAT APPROACH
      DO 1020  ILN = 1 , NLANES(JA)
      IL = LLANES(ILN,JA)
      DX2 = DX2 + 0.5D0*DBLE(LWID(IL))
      IF ( LGEOM(3,IL) . EQ . LGEOM(4,IL) )      GO TO 1020
      JMAXL = MAX0( JMAXL,LGEOM(4,IL) )
 1020 CONTINUE
C-----FIND THE INTERSECTION OF THE TWO APPROACHES
      CALL  XROTAX  ( DX2,0.0D0 ,IAAZIM(JA),IAPX(JA),IAPY(JA),X2,Y2 )
      CALL  XROTAX  ( DX2,POSMAX,IAAZIM(JA),IAPX(JA),IAPY(JA),X3,Y3 )
      CALL  XROTAX  ( DX1,0.0D0 ,IAZIM,IXCLAP,IYCLAP,X1,Y1 )
      CALL  XROTAX  ( DX1,POSMAX,IAZIM,IXCLAP,IYCLAP,X4,Y4 )
      ITEST = LTOL( X1,Y1,X4,Y4,X2,Y2,X3,Y3,XINT,YINT,DUM,DUM )
                    IF ( ITEST . NE . 1 )        GO TO 9030
      X3 = XINT
      Y3 = YINT
C-----FIND THE MAXIMUM DISTANCE DOWN THE OTHER APPROACH THAT CAN BE SEEN
C-----FROM THE CENTER OF EVERY 25 FOOT SECTION DOWN THE APPROACH BEING
C-----PROCESSED
      INDEX = 0
      DY1 = -12.5D0
 1030 CONTINUE
      DY1 = DY1 + 25.0D0
      INDEX = INDEX + 1
      CALL  XROTAX  ( DX1,DY1,IAZIM,IXCLAP,IYCLAP,XFROM,YFROM )
      MAXSEE = 0
C-----CHECK EACH SIGHT DISTANCE RESTRICTION COORDINATE WHILE AT THIS
C-----SECTION
      DO 1040  ISDRCN = 1 , NSDRC
      ISDRC = LSDRC(ISDRCN)
      XSDR = IXSDRC(ISDRC)
      YSDR = IYSDRC(ISDRC)
      ISEE = LDOWN( XFROM,YFROM,XSDR,YSDR,X2,Y2,X3,Y3 )
      MAXSEE = MAX0( MAXSEE,ISEE )
 1040 CONTINUE
      ICANSE(INDEX,NSDRS) = MIN0( MAXSEE,JMAXL )
            IF ( DY1+12.6D0 . LT . DBLE(IMAXL) ) GO TO 1030
C-----IF YOU CAN SEE THE START OF THE OTHER APPROACH FROM EACH 25 FOOT
C-----SECTION ON THE APPROACH BEING PROCESSED THEN THERE IS NO SIGHT
C-----DISTANCE RESTRICTION BETWEEN THESE APPROACHES
      DO 1050  I = 1 , INDEX
            IF ( ICANSE(I,NSDRS) . NE . 0 )      GO TO 2010
 1050 CONTINUE
      GO TO 3010
 2010 CONTINUE
C-----THERE IS A SIGHT DISTANCE RESTRICTION
                    IF ( NSDRS . NE . 1 )        GO TO 2020
                    IF ( NLINE+INDEX+8.GT.LINES )CALL  HEADER
      WRITE (6,601) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
 2020 CONTINUE
C-----ADD SIGHT DISTANCE RESTRICTION FOR THE APPROACH BEING PROCESSED
      NSDRAP = NSDRAP + 1
                    IF ( NSDRAP . GT . NAS )     GO TO 9040
      INDEX = INDEX + 1
                    IF ( INDEX . GT . NSS )      GO TO 2040
      DO 2030  I = INDEX , NSS
      ICANSE(I,NSDRS) = 0
 2030 CONTINUE
 2040 CONTINUE
      NSDR(IA) = NSDRAP
      ISDRN(NSDRAP,IA) = NSDRS
      ISDRA(NSDRAP,IA) = JA
C-----PRINT SIGHT DISTANCE RESTRICTION
      INDEX = INDEX - 1
                    IF ( NLINE+INDEX+5.GT.LINES )CALL  HEADER
      WRITE (6,602) NSDRS,NSDRAP,IA,JA
      NLINE = NLINE + 3
      ISTART = -25
      ISTOP = 0
      DO 2050  I = 1 , INDEX
      ISTART = ISTART + 25
      ISTOP = MIN0( ISTOP+25,IMAXL )
      WRITE (6,603) IA,ISTART,ISTOP,JA,ICANSE(I,NSDRS),JMAXL
      NLINE = NLINE + 1
 2050 CONTINUE
      NSDRS = NSDRS + 1
                    IF ( NSDRS . GT . NSR )      GO TO 9050
      WRITE (6,604)
      NLINE = NLINE + 2
C-----END OF OTHER APPROACH LOOP
 3010 CONTINUE
C-----END OF APPROACH LOOP
 3020 CONTINUE
      NSDRS = NSDRS - 1
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9030 CONTINUE
      CALL  ABORTR  ( 'STOP 903 - ' //
     *                'APPROACHES DO NOT INTERSECT - ' //
     *                'FNDSDR'                            )
      STOP  903
 9040 CONTINUE
      WRITE (ERRMSG,904) NAS
      CALL  ABORTR  ( 'STOP 904 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'FNDSDR'                             )
      STOP  904
 9050 CONTINUE
      NSDRS = NSR
      WRITE (ERRMSG,905) NSR
      CALL  ABORTR  ( 'STOP 905 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'FNDSDR'                             )
      STOP  905
      END                                                               FNDSDR
C
C
C
      FUNCTION   LTOL   ( X1,Y1,X2,Y2,X3,Y3,X4,Y4,XI1,YI1,XI2,YI2 )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  CLOSE,XBA,XBB,XI1,XI2,XMA,XMB,X1,X2,X3,X4,YI1,
     *                  YI2,Y1,Y2,Y3,Y4
      INTEGER           LTOL
      DATA     CLOSE  / 1.000001D0 /
C
C-----FUNCTION LTOL TESTS IF LINE A FROM (X1,Y1) TO (X2,Y2) INTERSECTS
C-----WITH LINE B FROM (X3,Y3) TO (X4,Y4)  (LTOL=0=NO, LTOL=1=YES, AND
C-----LTOL=2=PARALLEL AND SAME)
C
      LTOL = 0
C-----IF LINE A VERTICAL THEN GO TO 1010
                    IF ( DABS(X2-X1).LE.ZERO )   GO TO 1010
      XMA = (Y2-Y1)/(X2-X1)
      XBA = Y1 - X1*XMA
C-----IF LINE B VERTICAL THEN GO TO 1020
                    IF ( DABS(X4-X3).LE.ZERO )   GO TO 1020
      XMB = (Y4-Y3)/(X4-X3)
      XBB = Y3 - X3*XMB
C-----IF THE SLOPE OF LINE A IS EQUAL TO THE SLOPE OF LINE B THEN LINE A
C-----IS PARALLEL TO LINE B THUS GO TO 2010
                    IF ( DABS(XMA-XMB).LE.ZERO ) GO TO 2010
C-----FIND THE INTERSECTION OF LINE A AND LINE B
      XI1 = (XBB-XBA)/(XMA-XMB)
      YI1 = XMA*XI1 + XBA
      GO TO 1030
 1010 CONTINUE
C-----IF LINE B IS ALSO VERTICAL THEN LINE A IS PARALLEL TO LINE B THUS
C-----GO TO 3010
                    IF ( DABS(X4-X3).LE.ZERO )   GO TO 3010
      XMB = (Y4-Y3)/(X4-X3)
      XBB = Y3 - X3*XMB
C-----FIND THE INTERSECTION OF LINE A AND LINE B
      XI1 = X1
      YI1 = XMB*XI1 + XBB
      GO TO 1030
 1020 CONTINUE
C-----FIND THE INTERSECTION OF LINE A AND LINE B
      XI1 = X3
      YI1 = XMA*XI1 + XBA
 1030 CONTINUE
C-----IF (XI1,YI1) DOES NOT LIE BETWEEN (X1,Y1) AND (X2,Y2) THEN THE
C-----POINT OF INTERSECTION DOES NOT LIE ON LINE A THUS RETURN (LTOL=0)
            IF ( (XI1-X1)*(XI1-X2).GT.ZERO )     RETURN
            IF ( (YI1-Y1)*(YI1-Y2).GT.ZERO )     RETURN
C-----IF (XI1,YI1) DOES NOT LIE BETWEEN (X3,Y3) AND (X4,Y4) THEN THE
C-----POINT OF INTERSECTION DOES NOT LIE ON LINE B THUS RETURN (LTOL=0)
            IF ( (XI1-X3)*(XI1-X4).GT.ZERO )     RETURN
            IF ( (YI1-Y3)*(YI1-Y4).GT.ZERO )     RETURN
C-----LINE A INTERSECTS LINE B
      LTOL = 1
      RETURN
 2010 CONTINUE
C-----LINE A IS PARALLEL TO LINE B THUS FIND THE X AND Y COORDINATES FOR
C-----THE PARTS OF THE LINES THAT OVERLAP
      XI1 = DMAX1( DMIN1( X1,X2 ),DMIN1( X3,X4 ) )
      XI2 = DMIN1( DMAX1( X1,X2 ),DMAX1( X3,X4 ) )
C-----IF THE MINIMUM X COORDINATE IS GREATER THAN THE MAXIMUM X
C-----COORDINATE THEN RETURN (LTOL=0)
                    IF ( XI1-CLOSE . GT . XI2 )  RETURN
      YI1 = DMAX1( DMIN1( Y1,Y2 ),DMIN1( Y3,Y4 ) )
      YI2 = DMIN1( DMAX1( Y1,Y2 ),DMAX1( Y3,Y4 ) )
C-----IF THE MINIMUM Y COORDINATE IS GREATER THAN THE MAXIMUM Y
C-----COORDINATE THEN RETURN (LTOL=0)
                    IF ( YI1-CLOSE . GT . YI2 )  RETURN
C-----IF THE PERPENDICULAR DISTANCE BETWEEN THE LINES IS NOT CLOSE THEN
C-----RETURN (LTOL=0) ELSE THE LINES ARE PARALLEL AND THE SAME THUS
C-----RETURN (LTOL=2)
      IF ( DABS(XBA-XBB)*DCOS(DATAN(0.5D0*(XMA+XMB))) . GT . CLOSE )
     *                                           RETURN
      LTOL = 2
      RETURN
 3010 CONTINUE
C-----LINE A AND LINE B ARE VERTICAL THUS FIND THE X AND Y COORDINATES
C-----FOR THE PARTS OF THE LINES THAT OVERLAP
      YI1 = DMAX1( DMIN1( Y1,Y2 ),DMIN1( Y3,Y4 ) )
      YI2 = DMIN1( DMAX1( Y1,Y2 ),DMAX1( Y3,Y4 ) )
C-----IF THE MINIMUM Y COORDINATE IS GREATER THAN THE MAXIMUM Y
C-----COORDINATE THEN RETURN (LTOL=0)
                    IF ( YI1-CLOSE . GT . YI2 )  RETURN
C-----IF THE X INTERCEPT OF THE LINES IS DIFFERENT THEN RETURN (LTOL=0)
C-----ELSE THE LINES ARE PARALLEL AND THE SAME THUS RETURN (LTOL=2)
      IF ( DABS(0.5D0*(X1+X2)-0.5D0*(X3+X4)) . GT . CLOSE )
     *                                           RETURN
      XI1 = 0.25D0*(X1+X2+X3+X4)
      XI2 = XI1
      LTOL = 2
      RETURN
      END                                                               LTOL
C
C
C
      FUNCTION   LDOWN  ( X1,Y1,XSDR,YSDR,X2,Y2,X3,Y3 )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  XBA,XBB,XINT,XMA,XMB,XSDR,X1,X2,X3,YINT,YSDR,Y1,
     *                  Y2,Y3
      INTEGER           LDOWN
C
C-----FUNCTION LDOWN FINDS THE DISTANCE FROM (X2,Y2) TO (XINT,YINT) IF
C-----LINE A FROM (X1,Y1) THROUGH (XSDR,YSDR) INTERSECTS WITH LINE B
C-----FROM (X2,Y2) TO (X3,Y3)  (LDOWN=0=NO INTERSECTION)
C
      LDOWN = 0
C-----IF LINE A VERTICAL THEN GO TO 1010
                    IF ( DABS(XSDR-X1).LE.ZERO ) GO TO 1010
      XMA = (YSDR-Y1)/(XSDR-X1)
      XBA = Y1 - X1*XMA
C-----IF LINE B VERTICAL THEN GO TO 1020
                    IF ( DABS(X3-X2).LE.ZERO )   GO TO 1020
      XMB = (Y3-Y2)/(X3-X2)
      XBB = Y2 - X2*XMB
C-----IF THE SLOPE OF LINE A IS EQUAL TO THE SLOPE OF LINE B THEN LINE A
C-----IS PARALLEL TO LINE B AND THERE IS NO INTERSECTION
                    IF ( DABS(XMA-XMB).LE.ZERO ) RETURN
C-----FIND THE INTERSECTION OF LINE A AND LINE B
      XINT = (XBB-XBA)/(XMA-XMB)
      YINT = XMA*XINT + XBA
      GO TO 1030
 1010 CONTINUE
C-----IF LINE B IS ALSO VERTICAL THEN LINE A IS PARALLEL TO LINE B AND
C-----THERE IS NO INTERSECTION
                    IF ( DABS(X3-X2).LE.ZERO )   RETURN
      XMB = (Y3-Y2)/(X3-X2)
      XBB = Y2 - X2*XMB
C-----FIND THE INTERSECTION OF LINE A AND LINE B
      XINT = X1
      YINT = XMB*XINT + XBB
      GO TO 1030
 1020 CONTINUE
C-----FIND THE INTERSECTION OF LINE A AND LINE B
      XINT = X2
      YINT = XMA*XINT + XBA
 1030 CONTINUE
C-----IF (XSDR,YSDR) DOES NOT LIE BETWEEN (X1,Y1) AND (XINT,YINT) THEN
C-----THE POINT OF SIGHT DISTANCE RESTRICTION DOES NOT LIE BETWEEN THE
C-----DRIVER AND THE OTHER APPROACH AND THERE IS NO INTERSECTION
            IF ( (XSDR-X1)*(XSDR-XINT).GT.ZERO ) RETURN
            IF ( (YSDR-Y1)*(YSDR-YINT).GT.ZERO ) RETURN
C-----IF (XINT,YINT) DOES NOT LIE BETWEEN (X2,Y2) AND (X3,Y3) THEN THE
C-----POINT OF INTERSECTION DOES NOT LIE ON LINE B
            IF ( (XINT-X2)*(XINT-X3).GT.ZERO )   RETURN
            IF ( (YINT-Y2)*(YINT-Y3).GT.ZERO )   RETURN
C-----FIND THE DISTANCE FROM (X2,Y2) TO (XINT,YINT)
      LDOWN = DSQRT((X2-XINT)**2+(Y2-YINT)**2) + XROUND
      RETURN
      END                                                               LDOWN
C
C
C
      SUBROUTINE WRITAP
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'GEOCOM'
      INCLUDE 'INDEX'
      INCLUDE 'OUTPT'
      INTEGER           I,ISD
  601 FORMAT(20I4)
  602 FORMAT(11I4,A1)
C
C-----SUBROUTINE WRITAP WRITES THE APPROACH INFORMATION ONTO TAPE MODELT
C-----FOR SIMPRO
C
C-----WRITE THE NUMBER AND LIST OF INBOUND APPROACHES ONTO MODELT
      WRITE (MODELT,601) NIBA
      WRITE (MODELT,601) (LIBA(I),I=1,NIBA)
C-----WRITE THE NUMBER AND LIST OF OUTBOUND APPROACHES ONTO MODELT
      WRITE (MODELT,601) NOBA,NLEGS,NFUT
      WRITE (MODELT,601) (LOBA(I),I=1,NOBA)
C-----WRITE THE NUMBER OF APPROACHES ONTO MODELT
      WRITE (MODELT,601) NAPS
C-----WRITE THE INFORMATION FOR EACH INBOUND APPROACH ONTO MODELT
      DO 1010  IAN = 1 , NIBA
      IA = LIBA(IAN)
C-----WRITE THE INBOUND APPROACH INFORMATION ONTO MODELT
      WRITE (MODELT,602) IA,IAAZIM(IA),IAPX(IA),IAPY(IA),ISLIM(IA),
     *                   NLANES(IA),NSDR(IA),IALEFT(IA),IARGHT(IA),
     *                   INTLNK(IA),INTLNU(IA),IAFLAG(IA)
      WRITE (MODELT,601) (LLANES(ILN,IA),ILN=1,NLANES(IA))
                    IF ( NSDR(IA) . LE . 0 )     GO TO 1010
      WRITE (MODELT,601) (ISDRN(ISD,IA),ISDRA(ISD,IA),ISD=1,NSDR(IA))
 1010 CONTINUE
C-----WRITE THE INFORMATION FOR EACH OUTBOUND APPROACH ONTO MODELT
      DO 2010  IAN = 1 , NOBA
      IA = LOBA(IAN)
C-----WRITE THE OUTBOUND APPROACH INFORMATION ONTO MODELT
      WRITE (MODELT,602) IA,IAAZIM(IA),IAPX(IA),IAPY(IA),ISLIM(IA),
     *                   NLANES(IA),NSDR(IA),IALEFT(IA),IARGHT(IA),
     *                   INTLNK(IA),INTLNU(IA),IAFLAG(IA)
      WRITE (MODELT,601) (LLANES(ILN,IA),ILN=1,NLANES(IA))
                    IF ( NSDR(IA) . LE . 0 )     GO TO 2010
      WRITE (MODELT,601) (ISDRN(ISD,IA),ISDRA(ISD,IA),ISD=1,NSDR(IA))
 2010 CONTINUE
      RETURN
      END                                                               WRITAP
C
C
C
      SUBROUTINE INIPLT ( PLFILE )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'GEOVAL'
      INCLUDE 'PLOTTR'
      COMMON / PLTRIO / IOPLTR,NPF                                      CCODE=C]
      INTEGER           IOPLTR,NPF                                      CCODE=C]
C<    COMMON / PLTRIO / IOPLTR,NPF
C<    INTEGER           IOPLTR,NPF
C:    COMMON / PLTRIO / IOPLTR,NPF
C:    INTEGER           IOPLTR,NPF
      CHARACTER*(*)     PLFILE
C\    INTEGER           IDUM,IREL,LPLOT,LPLOTW
C IBM DIMENSION         IBUF(1024)
C IBM INTEGER           IBUF
C\    DATA     LPLOT  / L"PLOT" /
C\    DATA     LPLOTW / L"PLOTW" /
C
C-----SUBROUTINE INIPLT INITIALIZES PLOTTING
C
      GO TO ( 1010,2010,3010,4010 ) , IPLOT
 1010 CONTINUE
C-----PLOT OPTION IS (PLOT    )
      PWID = XPAPER                                                     CCODE=C]
      CALL  PLOTS   ( 0.0,PWID,PLFILE )                                 CCODE=C]
      INQUIRE(NPF,NAME=PLFILE)                                          CCODE=C]
C<    IF ( PLFILE . EQ . ' ' )                   PLFILE=JPLOT
C<    CALL  PLOTS   ( 0.0,0.0,PLFILE )
C<    INQUIRE(NPF,NAME=PLFILE)
C:    CALL  PLOTS   ( 0.0,0.0,NPF )
C:    INQUIRE(NPF,NAME=PLFILE)
C\    IF ( IPAPER . EQ . 12 )                    THEN
C\      CALL  PLOTS  ( IREL,IDUM,LPLOT )
C\      JPLOT='PLOT'
C\      GO TO 3020
C\    END IF
C\    IF ( IPAPER . EQ . 30 )                    THEN
C\      CALL  PLOTS  ( IREL,IDUM,LPLOTW  )
C\      JPLOT='PLOTW'
C\    END IF
C IBM CALL  PLOTS   ( IBUF,1024,8 )
      GO TO 3020
 2010 CONTINUE
C-----PLOT OPTION IS (PLOTI   )
C\    GO TO 1010
C     GO TO 1010
C     IF ( IPAPER . EQ . 12 )
C    *CALL  BGNPLT  ( 5LPLOTI  )
C     IF ( IPAPER . EQ . 30 )
C    *CALL  BGNPLT  ( 6LPLOTWI )
C IBM CALL  PLOTS   ( IBUF,1024,8 )
C IBM CALL  NEWPEN  ( 2 )
C IBM GO TO 3020
 3010 CONTINUE
C     CALL  BGNPLT  ( 5LPLOTR  )
      GO TO 1010
 3020 CONTINUE
C-----DRAW THE APPROACH PLOT
      CALL  DRWAPR
C-----DRAW THE INTERSECTION PLOT
      CALL  DRWINT  ( MINXI,MAXXI,MINYI,MAXYI,
     *                SCALEI,XSIZEI,YSIZEI,CSIZEI,' ' )
 4010 CONTINUE
C-----PLOT OPTION IS NOPLOT
      RETURN
      END                                                               INIPLT
C
C
C
      SUBROUTINE DRWAPR
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'ARC'
      INCLUDE 'LANE'
      INCLUDE 'LINE'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'INDEX'
      INCLUDE 'PLOTTR'
      INCLUDE 'SDRC'
      INCLUDE 'TITLE'
      DIMENSION         ISCALE(2)
      CHARACTER*16      ISCALE
      DOUBLE PRECISION  X,XBRDR,X1,X2,Y,YBRDR,Y1,Y2
      CHARACTER*6       SCALET
      INTEGER           IARC,IARCN,ILINE,ILINEN,ISDRC,ISDRCN,IX1,IX2,
     *                  NCIT,NCT
      REAL              XPAGE,YPAGE
      INTEGER           ILNB
      DATA     ISCALE / 'SCALE FACTOR IS ',' FEET PER INCH' /
C
C-----SUBROUTINE DRWAPR DRAWS THE APPROACH PLOT
C
                    IF ( IPLOT . EQ . 4 )        RETURN
C-----SET PLOT PARAMETERS FOR APPROACH PLOT
      SCALE = SCALEA
      XMIN = MINXA
      YMIN = MINYA
      XMAX = MAXXA
      YMAX = MAXYA
C-----FIND APPROACH PLOT BORDERS
      PWID = XPAPER                                                     CCODE=C]
                    IF ( IPLOT . GT . 99999 )    GO TO  999             CCODE=C]
      GO TO 1000                                                        CCODE=C]
  999 CONTINUE
C<    IF ( IPAPER . LE . 8 )                     THEN
C<      PWID = IPAPER - 0.5
C<      GO TO 1000
C<    END IF
      PWID = IPAPER - 1
 1000 CONTINUE
      XBRDR = 0.5D0*(PWID-XSIZEA)
      YBRDR = 0.5D0*(PWID-YSIZEA-8.0D0*CSIZEA)
      CALL  NEWPEN   ( 1 )                                              CCODE=C]
C-----RE-ORIGIN PLOT SO (XMIN,YMIN) WILL BE (0.0,0.0)
      X0 = XSIZEA + XBRDR
      Y0 = YBRDR + 4.0D0*CSIZEA
      CALL  PLOT    ( SNGL(XBRDR),SNGL(Y0),-3 )
C-----DRAW THE PLOT SCALE FACTOR MESSAGE AT BOTTOM OF PLOT
      WRITE (SCALET,'(F6.1)') SNGL(SCALE)
      NCT = 3
      IF ( SNGL(SCALE) .GE.    10.0 )            NCT = 4
      IF ( SNGL(SCALE) .GE.   100.0 )            NCT = 5
      IF ( SNGL(SCALE) .GE.  1000.0 )            NCT = 6
      IF ( SNGL(SCALE) .GE. 10000.0 )            NCT = 7
      XPAGE = 0.5D0*XSIZEA - 0.5D0*(31+NCT)*CSIZEA
      YPAGE = -3.0D0*CSIZEA
      CALL  SYMBOL  ( XPAGE,YPAGE,SNGL(CSIZEA),
C:   *                                         %REF(
     *                  ISCALE(1)//SCALET(7-NCT:)//ISCALE(2)(1:15)
C:   *                                                 )
     *                                                   ,0.0, 31+NCT )
C-----DRAW THE TITLE FOR GEOPRO AT THE TOP OF THE PLOT
      XPAGE = 0.5D0*XSIZEA - 40.0D0*CSIZEA
      YPAGE = YSIZEA + 2.0D0*CSIZEA
      NCIT=ILNB( ITITLE )
      CALL  SYMBOL  ( XPAGE,YPAGE,SNGL(CSIZEA),
C:   *                                         %REF(
     *                                              ITITLE
C:   *                                                    )
     *                                                     ,0.0,NCIT )
C-----DRAW EACH INBOUND APPROACH
      DO 1060  IAN = 1 , NIBA
      IA = LIBA(IAN)
      IX1 = 0
C-----DRAW EACH LANE OF THE INBOUND APPROACH
      DO 1050  ILN = 1 , NLANES(IA)
      IL = LLANES(ILN,IA)
      IX2 = IX1 + LWID(IL)
      IF ( LGEOM(1,IL) . NE . LGEOM(3,IL) )      GO TO 1010
C-----DRAW A BOX FROM LGEOM(1) TO LGEOM(4) FOR THE INBOUND LANE
      CALL  DRWBOX  ( IA,IX1,IX2,LGEOM(1,IL),LGEOM(4,IL) )
      GO TO 1040
 1010 CONTINUE
      IF ( LGEOM(3,IL) . NE . LGEOM(4,IL) )      GO TO 1030
 1020 CONTINUE
C-----DRAW A BOX FROM LGEOM(1) TO LGEOM(2) FOR THE INBOUND LANE
      CALL  DRWBOX  ( IA,IX1,IX2,LGEOM(1,IL),LGEOM(2,IL) )
      GO TO 1040
 1030 CONTINUE
C-----DRAW A BOX FROM LGEOM(3) TO LGEOM(4) FOR THE INBOUND LANE
      CALL  DRWBOX  ( IA,IX1,IX2,LGEOM(3,IL),LGEOM(4,IL) )
      IF ( LGEOM(1,IL) . NE . LGEOM(2,IL) )      GO TO 1020
 1040 CONTINUE
      IX1 = IX2
C-----END OF LANE LOOP
 1050 CONTINUE
C-----END OF INBOUND APPROACH LOOP
 1060 CONTINUE
C-----DRAW EACH OUTBOUND APPROACH
      DO 2060  IAN = 1 , NOBA
      IA = LOBA(IAN)
      IF ( DIAMON )                              THEN
        IF ( IAFLAG(IA) . EQ . 'I' )             GO TO 2060
      END IF
      IX1 = 0
C-----DRAW EACH LANE OF THE OUTBOUND APPROACH
      DO 2050  ILN = 1 , NLANES(IA)
      IL = LLANES(ILN,IA)
      IX2 = IX1 + LWID(IL)
      IF ( LGEOM(1,IL) . NE . LGEOM(3,IL) )      GO TO 2010
C-----DRAW A BOX FROM LGEOM(1) TO LGEOM(4) FOR THE OUTBOUND LANE
      CALL  DRWBOX  ( IA,IX1,IX2,LGEOM(1,IL),LGEOM(4,IL) )
      GO TO 2040
 2010 CONTINUE
      IF ( LGEOM(3,IL) . NE . LGEOM(4,IL) )      GO TO 2030
 2020 CONTINUE
C-----DRAW A BOX FROM LGEOM(1) TO LGEOM(2) FOR THE OUTBOUND LANE
      CALL  DRWBOX  ( IA,IX1,IX2,LGEOM(1,IL),LGEOM(2,IL) )
      GO TO 2040
 2030 CONTINUE
C-----DRAW A BOX FROM LGEOM(3) TO LGEOM(4) FOR THE OUTBOUND LANE
      CALL  DRWBOX  ( IA,IX1,IX2,LGEOM(3,IL),LGEOM(4,IL) )
      IF ( LGEOM(1,IL) . NE . LGEOM(2,IL) )      GO TO 2020
 2040 CONTINUE
      IX1 = IX2
C-----END OF LANE LOOP
 2050 CONTINUE
C-----END OF OUTBOUND APPROACH LOOP
 2060 CONTINUE
                    IF ( NARCS . LE . 0 )        GO TO 3020
C-----DRAW EACH ARC
      DO 3010  IARCN = 1 , NARCS
      IARC = LARCS(IARCN)
      CALL  DRWARC  ( IARCX(IARC),IARCY(IARC),IARCAZ(IARC),IARCSW(IARC),
     *                IARCR(IARC) )
 3010 CONTINUE
 3020 CONTINUE
                    IF ( NLINES . LE . 0 )       GO TO 4020
C-----DRAW EACH LINE
      DO 4010  ILINEN = 1 , NLINES
      ILINE = LLINES(ILINEN)
      X1 = ILX1(ILINE)
      Y1 = ILY1(ILINE)
      X2 = ILX2(ILINE)
      Y2 = ILY2(ILINE)
      CALL  DRWLIN  ( X1,Y1,X2,Y2 )
 4010 CONTINUE
 4020 CONTINUE
                    IF ( NSDRC . LE . 0 )        GO TO 5020
C-----DRAW EACH SIGHT DISTANCE RESTRICTION COORDINATE
      DO 5010  ISDRCN = 1 , NSDRC
      ISDRC = LSDRC(ISDRCN)
      X = IXSDRC(ISDRC)
      Y = IYSDRC(ISDRC)
C-----IF THE COORDINATES LIE OFF THE PLOT PAGE THEN SKIP THE POINT
                    IF ( X . LT . XMIN )         GO TO 5010
                    IF ( X . GT . XMAX )         GO TO 5010
                    IF ( Y . LT . YMIN )         GO TO 5010
                    IF ( Y . GT . YMAX )         GO TO 5010
C-----DRAW A 5 FOOT STAR AT COORDINATE
      XPAGE = (X-XMIN)/SCALE
      YPAGE = (Y-YMIN)/SCALE
      CALL  SYMBOL  ( XPAGE,YPAGE,SNGL(5.0D0/SCALE),
C|   *                                              CHAR(
     *                                              CHAR(               CCODE=C{
C%   *                                              CHAR(
     *                                                   11
C|   *                                                     )
     *                                                     )            CCODE=C{
C%   *                                                     )
     *                                                      ,0.0,-1 )
 5010 CONTINUE
 5020 CONTINUE
      RETURN
      END                                                               DRWAPR
C
C
C
      SUBROUTINE DRWPTH ( KP )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'APPRO'
      INCLUDE 'PATH'
      INCLUDE 'INDEX'
      INCLUDE 'DATA'
      INCLUDE 'GEOVAL'
      DOUBLE PRECISION  X1,X2,Y1,Y2
      INTEGER           IDELTR,IPTHOF,KP
      REAL              ANGLE,DIST,DISTX,DISTY
      SAVE              IPTHOF
      DATA     IPTHOF / 0 /
C
C-----SUBROUTINE DRWPTH DRAWS AN INTERSECTION PATH ON THE PLOT PAGE
C
                    IF ( IPLOT . EQ . 4 )        RETURN
      CALL  PLOT ( 0.0,0.0,3 )                                          CCODE=C]
                    IF ( IPTURN(KP) .EQ. LTURNU )CALL NEWPEN ( 1 )      CCODE=C]
                    IF ( IPTURN(KP) .EQ. LTURNL )CALL NEWPEN ( 2 )      CCODE=C]
                    IF ( IPTURN(KP) .EQ. LTURNS )CALL NEWPEN ( 3 )      CCODE=C]
                    IF ( IPTURN(KP) .EQ. LTURNR )CALL NEWPEN ( 4 )      CCODE=C]
                    IF ( ( LL1(KP) . GT . 0 ) .OR.
     1                   ( LL2(KP) . GT . 0 ) )  THEN
C-----PREPARE TO MOVE STRAIGHT SECTIONS
                      IF ( IPTURN(KP).EQ.LTURNU )DIST = -(IPTHOF+IPTHOF)
                      IF ( IPTURN(KP).EQ.LTURNL )DIST = -IPTHOF
                      IF ( IPTURN(KP).EQ.LTURNS )DIST =  0.0
                      IF ( IPTURN(KP).EQ.LTURNR )DIST =  IPTHOF
                    END IF
                    IF ( LL1(KP) . LE . 0 )      GO TO 1010
C-----DRAW SECTION 1 (LINE 1) OF THE INTERSECTION PATH ON THE PLOT PAGE
      X1 = IXL(1,KP)
      Y1 = IYL(1,KP)
      X2 = JXL(1,KP)
      Y2 = JYL(1,KP)
                    IF ( DIST .NE. 0.0 )         THEN
C-----MOVE THE U 2 FEET, LEFTS AND RIGHTS 1 FOOT FROM THE STRAIGHTS
                      ANGLE = FLOAT ( - IAAZIM ( IIA ( KP ) ) )*DEG2RD
                      DISTX = DIST * COS ( ANGLE )
                      DISTY = DIST * SIN ( ANGLE )
                      X1 = X1 + DISTX
                      Y1 = Y1 + DISTY
                      X2 = X2 + DISTX
                      Y2 = Y2 + DISTY
                    END IF
      CALL  DRWLIN  ( X1,Y1,X2,Y2 )
 1010 CONTINUE
                    IF ( LA1(KP) . LE . 0 )      GO TO 2010
C-----DRAW SECTION 2 (ARC 1) OF THE INTERSECTION PATH ON THE PLOT PAGE
C-----MOVE THE U 2 FEET, LEFT AND RIGHTS 1 FOOT FROM THE STRAIGHTS
                    IF ( IPTURN(KP) .EQ. LTURNU )THEN
                      IDELTR = -2 * IPTHOF
                      IF( IDA(1,KP) .GT. 0 )     IDELTR = -IDELTR
                    END IF
                    IF ( IPTURN(KP) .EQ. LTURNL )THEN
                      IDELTR = -IPTHOF
                      IF( IDA(1,KP) .GT. 0 )     IDELTR = -IDELTR
                    END IF
                    IF ( IPTURN(KP) .EQ. LTURNS )IDELTR = 0
                    IF ( IPTURN(KP) .EQ. LTURNR )THEN
                      IDELTR = -IPTHOF
                      IF( IDA(1,KP) .LT. 0 )     IDELTR = -IDELTR
                    END IF
      CALL  DRWARC  ( IXA(1,KP),IYA(1,KP),IBA(1,KP),IDA(1,KP),
     1                IRA(1,KP) + IDELTR )
 2010 CONTINUE
                    IF ( LA2(KP) . LE . 0 )      GO TO 3010
C-----DRAW SECTION 3 (ARC 2) OF THE INTERSECTION PATH ON THE PLOT PAGE
                    IF ( IPTURN(KP) .EQ. LTURNU )THEN
                      IDELTR = -2 * IPTHOF
                      IF( IDA(2,KP) .GT. 0 )     IDELTR = -IDELTR
                    END IF
                    IF ( IPTURN(KP) .EQ. LTURNL )THEN
                      IDELTR = -IPTHOF
                      IF( IDA(2,KP) .GT. 0 )     IDELTR = -IDELTR
                    END IF
                    IF ( IPTURN(KP) .EQ. LTURNS )IDELTR = 0
                    IF ( IPTURN(KP) .EQ. LTURNR )THEN
                      IDELTR = -IPTHOF
                      IF( IDA(2,KP) .LT. 0 )     IDELTR =  -IDELTR
                    END IF
      CALL  DRWARC  ( IXA(2,KP),IYA(2,KP),IBA(2,KP),IDA(2,KP),
     1                IRA(2,KP) + IDELTR )
 3010 CONTINUE
                    IF ( LL2(KP) . LE . 0 )      GO TO 4010
C-----DRAW SECTION 4 (LINE 2) OF THE INTERSECTION PATH ON THE PLOT PAGE
      X1 = IXL(2,KP)
      Y1 = IYL(2,KP)
      X2 = JXL(2,KP)
      Y2 = JYL(2,KP)
                    IF ( DIST .NE. 0.0 )         THEN
C-----MOVE THE U 2 FEET, LEFTS AND RIGHTS 1 FOOT FROM THE STRAIGHTS
                      ANGLE = FLOAT ( - IAAZIM ( IOA ( KP ) ) )*DEG2RD
                      DISTX = DIST * COS ( ANGLE )
                      DISTY = DIST * SIN ( ANGLE )
                      X1 = X1 + DISTX
                      Y1 = Y1 + DISTY
                      X2 = X2 + DISTX
                      Y2 = Y2 + DISTY
                   END IF
      CALL  DRWLIN  ( X1,Y1,X2,Y2 )
 4010 CONTINUE
      RETURN
      END                                                               DRWPTH
C
C
C
      SUBROUTINE DRWBOX ( KA,IX1,IX2,IL1,IL2 )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'GEOVAL'
      DOUBLE PRECISION  X1,X2,X3,X4,Y1,Y2,Y3,Y4
      INTEGER           IL1,IL2,IX1,IX2,KA
C
C-----SUBROUTINE DRWBOX DRAWS A BOX FROM IL1 TO IL2 FOR A LANE
C
                    IF ( IPLOT . EQ . 4 )        RETURN
C-----FIND THE COORDINATES OF THE EDGES OF THE BOX FOR THE LANE
      CALL  IROTAX  ( IX1,IL1,IAAZIM(KA),IAPX(KA),IAPY(KA),X1,Y1 )
      CALL  IROTAX  ( IX2,IL1,IAAZIM(KA),IAPX(KA),IAPY(KA),X2,Y2 )
      CALL  IROTAX  ( IX2,IL2,IAAZIM(KA),IAPX(KA),IAPY(KA),X3,Y3 )
      CALL  IROTAX  ( IX1,IL2,IAAZIM(KA),IAPX(KA),IAPY(KA),X4,Y4 )
C-----DRAW THE BOX FOR THE LANE
      CALL  DRWLIN  ( X1,Y1,X2,Y2 )
      CALL  DRWLIN  ( X2,Y2,X3,Y3 )
      CALL  DRWLIN  ( X3,Y3,X4,Y4 )
      CALL  DRWLIN  ( X4,Y4,X1,Y1 )
      RETURN
      END                                                               DRWBOX
C
C
C
      SUBROUTINE DRWLIN ( X1,Y1,X2,Y2 )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'GEOVAL'
      INCLUDE 'PLOTTR'
      DOUBLE PRECISION  D,DIST,DMIN,XDMIN,XINT,XX1,XX2,X1,X2,YDMIN,YINT,
     *                  YY1,YY2,Y1,Y2
      INTEGER           LTEST
      REAL              XPAGE,YPAGE
      INTEGER           LTOL
C
C-----SUBROUTINE DRWLIN DRAWS A LINE ON THE PLOT PAGE
C
                    IF ( IPLOT . EQ . 4 )        RETURN
      XX1 = X1
      YY1 = Y1
      XX2 = X2
      YY2 = Y2
C-----IF THE START OF THE LINE IS OFF THE PLOT PAGE THEN GO TO 2010
                    IF ( XX1 . LT . XMIN )       GO TO 2010
                    IF ( XX1 . GT . XMAX )       GO TO 2010
                    IF ( YY1 . LT . YMIN )       GO TO 2010
                    IF ( YY1 . GT . YMAX )       GO TO 2010
 1010 CONTINUE
C-----IF THE END OF THE LINE IS OFF THE PLOT PAGE THEN GO TO 3010
                    IF ( XX2 . LT . XMIN )       GO TO 3010
                    IF ( XX2 . GT . XMAX )       GO TO 3010
                    IF ( YY2 . LT . YMIN )       GO TO 3010
                    IF ( YY2 . GT . YMAX )       GO TO 3010
 1020 CONTINUE
C-----MOVE PEN TO THE START OF THE LINE WITH THE PEN UP
      XPAGE = (XX1-XMIN)/SCALE
      YPAGE = (YY1-YMIN)/SCALE
      CALL  PLOT    ( XPAGE,YPAGE,3 )
C-----MOVE PEN TO THE END OF THE LINE WITH THE PEN DOWN
      XPAGE = (XX2-XMIN)/SCALE
      YPAGE = (YY2-YMIN)/SCALE
      CALL  PLOT    ( XPAGE,YPAGE,2 )
      RETURN
 2010 CONTINUE
C-----THE FIRST POINT IS OFF THE PLOT PAGE THUS FIND THE INTERSECTION
C-----OF THE LINE WITH THE BOUNDARY NEAREST THE FIRST POINT
      DMIN = 1.0D+38
C-----FIND THE INTERSECTION WITH THE BOTTOM EDGE
      LTEST = LTOL( XX1,YY1,XX2,YY2,XMIN,YMIN,XMAX,YMIN,XINT,YINT,D,D )
                    IF ( LTEST . NE . 1 )        GO TO 2020
      DIST = DSQRT((XX1-XINT)**2+(YY1-YINT)**2)
                    IF ( DIST . GE . DMIN )      GO TO 2020
      DMIN = DIST
      XDMIN = XINT
      YDMIN = YINT
 2020 CONTINUE
C-----FIND THE INTERSECTION WITH THE RIGHT EDGE
      LTEST = LTOL( XX1,YY1,XX2,YY2,XMAX,YMIN,XMAX,YMAX,XINT,YINT,D,D )
                    IF ( LTEST . NE . 1 )        GO TO 2030
      DIST = DSQRT((XX1-XINT)**2+(YY1-YINT)**2)
                    IF ( DIST . GE . DMIN )      GO TO 2030
      DMIN = DIST
      XDMIN = XINT
      YDMIN = YINT
 2030 CONTINUE
C-----FIND THE INTERSECTION WITH THE TOP EDGE
      LTEST = LTOL( XX1,YY1,XX2,YY2,XMAX,YMAX,XMIN,YMAX,XINT,YINT,D,D )
                    IF ( LTEST . NE . 1 )        GO TO 2040
      DIST = DSQRT((XX1-XINT)**2+(YY1-YINT)**2)
                    IF ( DIST . GE . DMIN )      GO TO 2040
      DMIN = DIST
      XDMIN = XINT
      YDMIN = YINT
 2040 CONTINUE
C-----FIND THE INTERSECTION WITH THE LEFT EDGE
      LTEST = LTOL( XX1,YY1,XX2,YY2,XMIN,YMAX,XMIN,YMIN,XINT,YINT,D,D )
                    IF ( LTEST . NE . 1 )        GO TO 2050
      DIST = DSQRT((XX1-XINT)**2+(YY1-YINT)**2)
                    IF ( DIST . GE . DMIN )      GO TO 2050
      DMIN = DIST
      XDMIN = XINT
      YDMIN = YINT
 2050 CONTINUE
C-----IF THE MINIMUM DISTANCE IS STILL A LARGE NUMBER THEN RETURN
C-----ELSE SET POINT ONE TO THE CLOSEST COORDINATES
                    IF ( DMIN . EQ . 1.0D+38 )   RETURN
      XX1 = XDMIN
      YY1 = YDMIN
      GO TO 1010
 3010 CONTINUE
C-----THE SECOND POINT IS OFF THE PLOT PAGE THUS FIND THE INTERSECTION
C-----OF THE LINE WITH THE BOUNDARY NEAREST THE SECOND POINT
      DMIN = 1.0D+38
C-----FIND THE INTERSECTION WITH THE BOTTOM EDGE
      LTEST = LTOL( XX1,YY1,XX2,YY2,XMIN,YMIN,XMAX,YMIN,XINT,YINT,D,D )
                    IF ( LTEST . NE . 1 )        GO TO 3020
      DIST = DSQRT((XX2-XINT)**2+(YY2-YINT)**2)
                    IF ( DIST . GE . DMIN )      GO TO 3020
      DMIN = DIST
      XDMIN = XINT
      YDMIN = YINT
 3020 CONTINUE
C-----FIND THE INTERSECTION WITH THE RIGHT EDGE
      LTEST = LTOL( XX1,YY1,XX2,YY2,XMAX,YMIN,XMAX,YMAX,XINT,YINT,D,D )
                    IF ( LTEST . NE . 1 )        GO TO 3030
      DIST = DSQRT((XX2-XINT)**2+(YY2-YINT)**2)
                    IF ( DIST . GE . DMIN )      GO TO 3030
      DMIN = DIST
      XDMIN = XINT
      YDMIN = YINT
 3030 CONTINUE
C-----FIND THE INTERSECTION WITH THE TOP EDGE
      LTEST = LTOL( XX1,YY1,XX2,YY2,XMAX,YMAX,XMIN,YMAX,XINT,YINT,D,D )
                    IF ( LTEST . NE . 1 )        GO TO 3040
      DIST = DSQRT((XX2-XINT)**2+(YY2-YINT)**2)
                    IF ( DIST . GE . DMIN )      GO TO 3040
      DMIN = DIST
      XDMIN = XINT
      YDMIN = YINT
 3040 CONTINUE
C-----FIND THE INTERSECTION WITH THE LEFT EDGE
      LTEST = LTOL( XX1,YY1,XX2,YY2,XMIN,YMAX,XMIN,YMIN,XINT,YINT,D,D )
                    IF ( LTEST . NE . 1 )        GO TO 3050
      DIST = DSQRT((XX2-XINT)**2+(YY2-YINT)**2)
                    IF ( DIST . GE . DMIN )      GO TO 3050
      DMIN = DIST
      XDMIN = XINT
      YDMIN = YINT
 3050 CONTINUE
C-----IF THE MINIMUM DISTANCE IS STILL A LARGE NUMBER THEN RETURN
C-----ELSE SET POINT TWO TO THE CLOSEST COORDINATES
                    IF ( DMIN . EQ . 1.0D+38 )   RETURN
      XX2 = XDMIN
      YY2 = YDMIN
      GO TO 1020
      END                                                               DRWLIN
C
C
C
      SUBROUTINE DRWARC ( IXARC,IYARC,IAZARC,ISWARC,IRARC )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'GEOVAL'
      INCLUDE 'PLOTTR'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  X,Y
      INTEGER           IAZARC,IPEN,IRARC,ISWARC,IXARC,IYARC
      REAL              ADD,ADDAZ,DEG,XPAGE,YPAGE
C
C-----SUBROUTINE DRWARC DRAWS AN ARC ON THE PLOT PAGE
C
                    IF ( IPLOT . EQ . 4 )        RETURN
C-----THE STEP INCREMENT FOR THE AZIMUTH IS THE MINIMUM OF ONE-TENTH OF
C-----THE TOTAL SWEEP ANGLE AND 5 DEGREES
      ADDAZ = SIGN( AMIN1( IABS(ISWARC)/10.0,5.0 ),FLOAT(ISWARC) )
      ADD = -ADDAZ
 1010 CONTINUE
C-----IF FINISHED PLOTTING THE ARC THEN RETURN
      IF ( ABS(ADD).GE.FLOAT(IABS(ISWARC)) . OR .
     *     ABS(ADD-ISWARC) . LE . ZERO     )     RETURN
      IPEN = 3
 1020 CONTINUE
C-----FIND THE AZIMUTH OF A POINT ON THE ARC
      ADD = ADD + ADDAZ
      IF ( ABS(ADD).GE.FLOAT(IABS(ISWARC)) . OR .
     *     ABS(ADD-ISWARC) . LE . ZERO     )     ADD = ISWARC
C-----FIND THE X AND Y COORDINATES OF A POINT ON THE ARC
      DEG = 90 - (IAZARC+ADD)
      X = IXARC + IRARC*DCOS(DEG*DEG2RD)
      Y = IYARC + IRARC*DSIN(DEG*DEG2RD)
C-----IF THE POINT IS OFF THE PLOT PAGE THEN GO TO 1010
                    IF ( X . LT . XMIN )         GO TO 1010
                    IF ( X . GT . XMAX )         GO TO 1010
                    IF ( Y . LT . YMIN )         GO TO 1010
                    IF ( Y . GT . YMAX )         GO TO 1010
C-----MOVE TO THE POINT WITH THE PEN UP (IPEN=3) OR DOWN (IPEN=2)
      XPAGE = (X-XMIN)/SCALE
      YPAGE = (Y-YMIN)/SCALE
      CALL  PLOT    ( XPAGE,YPAGE,IPEN )
      IPEN = 2
C-----IF FINISHED PLOTTING THE ARC THEN RETURN
      IF ( ABS(ADD).GE.FLOAT(IABS(ISWARC)) . OR .
     *     ABS(ADD-ISWARC) . LE . ZERO     )     RETURN
      GO TO 1020
      END                                                               DRWARC
C
C
C
      SUBROUTINE DRWINT ( MINXP,MAXXP,MINYP,MAXYP,
     *                    SCALEP,XSIZEP,YSIZEP,CSIZEP,LRDESC )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'ARC'
      INCLUDE 'LANE'
      INCLUDE 'LINE'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'PLOTTR'
      INCLUDE 'SDRC'
      INCLUDE 'TITLE'
      CHARACTER*(*)     LRDESC
      DIMENSION         ISCALE(2)
      CHARACTER*16      ISCALE
      DOUBLE PRECISION  X,XBRDR,X1,X2,Y,YBRDR,Y1,Y2
      CHARACTER*6       SCALET
      INTEGER           IAL,IAR,IARC,IARCN,IAS,IFACT,ILINE,ILINEN,ISDRC,
     *                  ISDRCN,IX1,IX2,KA,KAN,KL,KLN,MAXXP,MAXYP,MINXP,
     *                  MINYP,NCIT,NCT
      REAL              XPAGE,YPAGE
      DOUBLE PRECISION  CSIZEP,SCALEP,XSIZEP,YSIZEP
      INTEGER           IAND,ILNB
      DATA     ISCALE / 'SCALE FACTOR IS ',' FEET PER INCH' /
C
C-----SUBROUTINE DRWINT DRAWS THE INTERSECTION PLOT
C-----(MAY NOT USE /INDEX/ BECAUSE CALLED BY FNDPTH)
C
                    IF ( IPLOT . EQ . 4 )        RETURN
C-----SET PLOT PARAMETERS FOR INTERSECTION PLOT
      SCALE = SCALEP
      XMIN = MINXP
      YMIN = MINYP
      XMAX = MAXXP
      YMAX = MAXYP
C-----RE-ORIGIN THE PLOT PAST THE LAST PLOT PAGE
C     CALL  PLOT    ( 0.0,0.0,-999 )
C\    CALL  PLOT    ( 0.0,0.0,-999 )
      CALL  PLOT    ( 0.0,0.0,-999 )                                    CCODE=C]
C<    CALL  PLOT    ( 0.0,0.0,-999 )
C:    CALL  PLOT    ( 0.0,0.0,-999 )
C IBM CALL  PLOT    ( SNGL(X0+4.0D0),SNGL(-Y0),-3 )
C-----FIND THE INTERSECTION PLOT BORDERS
      PWID = XPAPER                                                     CCODE=C]
                    IF ( IPLOT . GT . 99999 )    GO TO  999             CCODE=C]
      GO TO 1000                                                        CCODE=C]
  999 CONTINUE
C<    IF ( IPAPER . LE . 8 )                     THEN
C<      PWID = IPAPER - 0.5
C<      GO TO 1000
C<    END IF
      PWID = IPAPER - 1
 1000 CONTINUE
      XBRDR = 0.5D0*(PWID-XSIZEP)
      YBRDR = 0.5D0*(PWID-YSIZEP-10.0D0*CSIZEP)
      CALL  NEWPEN   ( 1 )                                              CCODE=C]
C-----RE-ORIGIN THE PLOT SO (XMIN,YMIN) WILL BE (0.0,0.0)
      X0 = XSIZEP + XBRDR
      Y0 = YBRDR + 5.0D0*CSIZEP
      CALL  PLOT    ( SNGL(XBRDR),SNGL(Y0),-3 )
C-----DRAW THE PLOT SCALE FACTOR MESSAGE AT THE BOTTOM OF THE PLOT
      WRITE (SCALET,'(F6.1)') SNGL(SCALE)
      NCT = 3
      IF ( SNGL(SCALE) .GE.    10.0 )            NCT = 4
      IF ( SNGL(SCALE) .GE.   100.0 )            NCT = 5
      IF ( SNGL(SCALE) .GE.  1000.0 )            NCT = 6
      IF ( SNGL(SCALE) .GE. 10000.0 )            NCT = 7
      XPAGE = 0.5D0*XSIZEP - 0.5D0*(31+NCT)*CSIZEP
      YPAGE = -3.0D0*CSIZEP
      CALL  SYMBOL  ( XPAGE,YPAGE,SNGL(CSIZEP),
C:   *                                         %REF(
     *                  ISCALE(1)//SCALET(7-NCT:)//ISCALE(2)(1:15)
C:   *                                                 )
     *                                                   ,0.0, 31+NCT )
C-----DRAW THE LEFT OR RIGHT DESIGNATION
      NCT = ILNB( LRDESC )
                    IF ( NCT . EQ . 0 )          GO TO 1005
      XPAGE = 0.5D0*XSIZEP - 0.5D0*NCT*CSIZEP
      YPAGE = -5.0D0*CSIZEP
      CALL  SYMBOL  ( XPAGE,YPAGE,SNGL(CSIZEP),
C:   *                                         %REF(
     *                                            LRDESC(1:NCT)
C:   *                                                       )
     *                                                        ,0.0,NCT )
 1005 CONTINUE
C-----DRAW THE TITLE FOR GEOPRO AT THE TOP OF THE PLOT
      XPAGE = 0.5D0*XSIZEP - 40.0D0*CSIZEP
      YPAGE = YSIZEP + 2.0D0*CSIZEP
      NCIT=ILNB( ITITLE )
      CALL  SYMBOL  ( XPAGE,YPAGE,SNGL(CSIZEP),
C:   *                                         %REF(
     *                                              ITITLE
C:   *                                                    )
     *                                                     ,0.0,NCIT )
C-----DRAW EACH INBOUND APPROACH
      DO 1060  KAN = 1 , NIBA
      KA = LIBA(KAN)
      IX1 = 0
C-----DRAW EACH LANE OF THE INBOUND APPROACH
      DO 1050  KLN = 1 , NLANES(KA)
      KL = LLANES(KLN,KA)
      IX2 = IX1 + LWID(KL)
      IF ( LGEOM(1,KL) . NE . LGEOM(3,KL) )      GO TO 1010
C-----DRAW A BOX FROM LGEOM(1) TO LGEOM(4) FOR THE INBOUND LANE
      CALL  DRWBOX  ( KA,IX1,IX2,LGEOM(1,KL),LGEOM(4,KL) )
      GO TO 1040
 1010 CONTINUE
      IF ( LGEOM(3,KL) . NE . LGEOM(4,KL) )      GO TO 1030
 1020 CONTINUE
C-----DRAW A BOX FROM LGEOM(1) TO LGEOM(2) FOR THE INBOUND LANE
      CALL  DRWBOX  ( KA,IX1,IX2,LGEOM(1,KL),LGEOM(2,KL) )
      GO TO 1040
 1030 CONTINUE
C-----DRAW A BOX FROM LGEOM(3) TO LGEOM(4) FOR THE INBOUND LANE
      CALL  DRWBOX  ( KA,IX1,IX2,LGEOM(3,KL),LGEOM(4,KL) )
      IF ( LGEOM(1,KL) . NE . LGEOM(2,KL) )      GO TO 1020
 1040 CONTINUE
      IX1 = IX2
C-----DRAW THE LANE TURN CODE ARROWS FOR THE INBOUND LANE
      IAL = IAAZIM(KA) - 90
      IAS = IAAZIM(KA)
      IAR = IAAZIM(KA) + 90
            IF ( IAND( LTURN(KL),(LTURNL+LTURNR) ) . EQ .
     *                           (LTURNL+LTURNR) )
     *                                           THEN
C-----IF BOTH LEFT AND RIGHT, OFFSET SO THEY DON'T OVERLAP
              IFACT=1
            ELSE
              IFACT=0
            END IF
            IF ( IAND( LTURN(KL),LTURNU ) . EQ . LTURNU )
     *                                           THEN
C-----PEN 1 IS READY FROM DRAWING THE APPROACH
              CALL  DRWUTA ( IAS,KL )
            END IF
            IF ( IAND( LTURN(KL),LTURNL ) . EQ . LTURNL )
     *                                           THEN
              CALL  PLOT  ( 0.0,0.0,3 )                                 CCODE=C]
              CALL  NEWPEN ( 2 )                                        CCODE=C]
              CALL  DRWARR ( IAL,KL,IFACT )
            END IF
            IF ( IAND( LTURN(KL),LTURNS ) . EQ . LTURNS )
     *                                           THEN
              CALL  PLOT  ( 0.0,0.0,3 )                                 CCODE=C]
              CALL  NEWPEN ( 3 )                                        CCODE=C]
              CALL  DRWARR ( IAS,KL,0 )
            END IF
            IF ( IAND( LTURN(KL),LTURNR ) . EQ . LTURNR )
     *                                           THEN
              CALL  PLOT  ( 0.0,0.0,3 )                                 CCODE=C]
              CALL  NEWPEN ( 4 )                                        CCODE=C]
              CALL  DRWARR ( IAR,KL,IFACT )
            END IF
      CALL  PLOT  ( 0.0,0.0,3 )                                         CCODE=C]
      CALL  NEWPEN ( 1 )                                                CCODE=C]
C-----END OF LANE LOOP
 1050 CONTINUE
C-----END OF INBOUND APPROACH LOOP
 1060 CONTINUE
C-----DRAW EACH OUTBOUND APPROACH
      DO 2060  KAN = 1 , NOBA
      KA = LOBA(KAN)
      IX1 = 0
C-----DRAW EACH LANE OF THE OUTBOUND APPROACH
      DO 2050  KLN = 1 , NLANES(KA)
      KL = LLANES(KLN,KA)
      IX2 = IX1 + LWID(KL)
      IF ( DIAMON )                              THEN
        IF ( IAFLAG(KA) . EQ . 'I' )             GO TO 2040
      END IF
      IF ( LGEOM(1,KL) . NE . LGEOM(3,KL) )      GO TO 2010
C-----DRAW A BOX FROM LGEOM(1) TO LGEOM(4) FOR THE OUTBOUND LANE
      CALL  DRWBOX  ( KA,IX1,IX2,LGEOM(1,KL),LGEOM(4,KL) )
      GO TO 2040
 2010 CONTINUE
      IF ( LGEOM(3,KL) . NE . LGEOM(4,KL) )      GO TO 2030
 2020 CONTINUE
C-----DRAW A BOX FROM LGEOM(1) TO LGEOM(2) FOR THE OUTBOUND LANE
      CALL  DRWBOX  ( KA,IX1,IX2,LGEOM(1,KL),LGEOM(2,KL) )
      GO TO 2040
 2030 CONTINUE
C-----DRAW A BOX FROM LGEOM(3) TO LGEOM(4) FOR THE OUTBOUND LANE
      CALL  DRWBOX  ( KA,IX1,IX2,LGEOM(3,KL),LGEOM(4,KL) )
      IF ( LGEOM(1,KL) . NE . LGEOM(2,KL) )      GO TO 2020
 2040 CONTINUE
      IX1 = IX2
C-----DRAW THE LANE TURN CODE ARROWS FOR THE OUTBOUND LANE
      IAL = IAAZIM(KA) - 90
      IAS = IAAZIM(KA)
      IAR = IAAZIM(KA) + 90
            IF ( IAND( LTURN(KL),(LTURNL+LTURNR) ) . EQ .
     *                           (LTURNL+LTURNR) )
     *                                           THEN
C-----IF BOTH LEFT AND RIGHT, OFFSET SO THEY DON'T OVERLAP
              IFACT=1
            ELSE
              IFACT=0
            END IF
            IF ( IAND( LTURN(KL),LTURNU ) . EQ . LTURNU )
     *                                           THEN
C-----PEN 1 IS READY FROM DRAWING THE APPROACH
              CALL  DRWUTA ( IAS,KL )
            END IF
            IF ( IAND( LTURN(KL),LTURNL ) . EQ . LTURNL )
     *                                           THEN
              CALL  PLOT ( 0.0,0.0,3 )                                  CCODE=C]
              CALL  NEWPEN ( 2 )                                        CCODE=C]
              CALL  DRWARR ( IAL,KL,IFACT )
            END IF
            IF ( IAND( LTURN(KL),LTURNS ) . EQ . LTURNS )
     *                                           THEN
              CALL  PLOT ( 0.0,0.0,3 )                                  CCODE=C]
              CALL  NEWPEN ( 3 )                                        CCODE=C]
              CALL  DRWARR ( IAS,KL,0 )
            END IF
            IF ( IAND( LTURN(KL),LTURNR ) . EQ . LTURNR )
     *                                           THEN
              CALL  PLOT ( 0.0,0.0,3 )                                  CCODE=C]
              CALL  NEWPEN ( 4 )                                        CCODE=C]
              CALL  DRWARR ( IAR,KL,IFACT )
            END IF
      CALL  PLOT ( 0.0,0.0,3 )                                          CCODE=C]
      CALL  NEWPEN ( 1 )                                                CCODE=C]
C-----END OF LANE LOOP
 2050 CONTINUE
C-----END OF OUTBOUND APPROACH LOOP
 2060 CONTINUE
                    IF ( NARCS . LE . 0 )        GO TO 3020
C-----DRAW EACH ARC
      DO 3010  IARCN = 1 , NARCS
      IARC = LARCS(IARCN)
      CALL  DRWARC  ( IARCX(IARC),IARCY(IARC),IARCAZ(IARC),IARCSW(IARC),
     *                IARCR(IARC) )
 3010 CONTINUE
 3020 CONTINUE
                    IF ( NLINES . LE . 0 )       GO TO 4020
C-----DRAW EACH LINE
      DO 4010  ILINEN = 1 , NLINES
      ILINE = LLINES(ILINEN)
      X1 = ILX1(ILINE)
      Y1 = ILY1(ILINE)
      X2 = ILX2(ILINE)
      Y2 = ILY2(ILINE)
      CALL  DRWLIN  ( X1,Y1,X2,Y2 )
 4010 CONTINUE
 4020 CONTINUE
                    IF ( NSDRC . LE . 0 )        GO TO 5020
C-----DRAW EACH SIGHT DISTANCE RESTRICTION COORDINATE
      DO 5010  ISDRCN = 1 , NSDRC
      ISDRC = LSDRC(ISDRCN)
      X = IXSDRC(ISDRC)
      Y = IYSDRC(ISDRC)
C-----IF THE COORDINATES LIE OFF THE PLOT PAGE THEN SKIP THE POINT
                    IF ( X . LT . XMIN )         GO TO 5010
                    IF ( X . GT . XMAX )         GO TO 5010
                    IF ( Y . LT . YMIN )         GO TO 5010
                    IF ( Y . GT . YMAX )         GO TO 5010
C-----DRAW A 5 FOOT STAR AT THE COORDINATE
      XPAGE = (X-XMIN)/SCALE
      YPAGE = (Y-YMIN)/SCALE
      CALL  SYMBOL  ( XPAGE,YPAGE,SNGL(5.0D0/SCALE),
C|   *                                              CHAR(
     *                                              CHAR(               CCODE=C{
C%   *                                              CHAR(
     *                                                   11
C|   *                                                     )
     *                                                     )            CCODE=C{
C%   *                                                     )
     *                                                      ,0.0,-1 )
 5010 CONTINUE
 5020 CONTINUE
      RETURN
      END                                                               DRWINT
C
C
C
      SUBROUTINE DRWARR ( IANGLE,ILANE,IFACT )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'GEOVAL'
      INCLUDE 'PLOTTR'
      DOUBLE PRECISION  XBOT,XLEFT,XRGHT,XTOP,YBOT,YLEFT,
     *                  YRGHT,YTOP
      INTEGER           IANGLE,ICX,ICY,IFACT,ILANE
      REAL              ANGLE,DIST,DISTX,DISTY
C
C-----SUBROUTINE DRWARR DRAWS AN ARROW POINTING IN THE IANGLE DIRECTION
C
                    IF ( IPLOT . EQ . 4 )        RETURN
      ICX = LTDIRX(ILANE)
      ICY = LTDIRY(ILANE)
C-----FIND THE COORDINATES OF THE ARROW POINTING IN THE IANGLE DIRECTION
      CALL  XROTAX  (  0.0D0,-3.5D0,IANGLE,ICX,ICY,XBOT ,YBOT  )
      CALL  XROTAX  (  0.0D0, 3.5D0,IANGLE,ICX,ICY,XTOP ,YTOP  )
      CALL  XROTAX  ( -0.5D0, 2.5D0,IANGLE,ICX,ICY,XLEFT,YLEFT )
      CALL  XROTAX  (  0.5D0, 2.5D0,IANGLE,ICX,ICY,XRGHT,YRGHT )
      IF(IFACT.NE.0)THEN
C-----OFFSET THE LEFT & RIGHT ARROWS SO NOT ON TOP OF EACH OTHER
        DIST=0.5D0*IFACT*1.75D0
        ANGLE=FLOAT(IANGLE)*DEG2RD
        DISTX=DIST*COS(ANGLE)
        DISTY=DIST*SIN(ANGLE)
        XBOT=XBOT+DISTX
        YBOT=YBOT+DISTY
        XTOP=XTOP+DISTX
        YTOP=YTOP+DISTY
        XLEFT=XLEFT+DISTX
        YLEFT=YLEFT+DISTY
        XRGHT=XRGHT+DISTX
        YRGHT=YRGHT+DISTY
      END IF
C-----DRAW THE ARROW POINTING IN THE IANGLE DIRECTION
      CALL  DRWLIN  ( XBOT,YBOT,XTOP,YTOP )
      CALL  DRWLIN  ( XTOP,YTOP,XLEFT,YLEFT )
      CALL  DRWLIN  ( XTOP,YTOP,XRGHT,YRGHT )
      RETURN
      END                                                               DRWARR
C
C
C
      SUBROUTINE DRWUTA ( IAU,ILANE )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'INDEX'
      INCLUDE 'GEOVAL'
      INCLUDE 'PLOTTR'
      DOUBLE PRECISION  UX1,UX2,UX3,UX4,UX5,UX6,UY1,UY2,UY3,UY4,UY5,UY6
      INTEGER           IAU,ICX,ICY,ILANE
C
C-----SUBROUTINE DRWUTA DRAWS A U-TURN ARROW FOR A LANE
C
                    IF ( IPLOT . EQ . 4 )        RETURN
      ICX = LTDIRX(ILANE)
      ICY = LTDIRY(ILANE)
C-----FIND THE COORDINATES OF THE U-TURN ARROW
      CALL  XROTAX  (  2.0D0,-2.0D0,IAU,ICX,ICY,UX1,UY1 )
      CALL  XROTAX  (  2.0D0, 0.0D0,IAU,ICX,ICY,UX2,UY2 )
      CALL  XROTAX  ( -2.0D0, 0.0D0,IAU,ICX,ICY,UX3,UY3 )
      CALL  XROTAX  ( -2.0D0,-3.0D0,IAU,ICX,ICY,UX4,UY4 )
      CALL  XROTAX  ( -2.5D0,-2.0D0,IAU,ICX,ICY,UX5,UY5 )
      CALL  XROTAX  ( -1.5D0,-2.0D0,IAU,ICX,ICY,UX6,UY6 )
C-----DRAW A U-TURN ARROW FOR THE LANE
      CALL  DRWLIN  ( UX1,UY1,UX2,UY2 )
      CALL  DRWARC  ( ICX,ICY,IAU+90,-180,2 )
      CALL  DRWLIN  ( UX3,UY3,UX4,UY4 )
      CALL  DRWLIN  ( UX4,UY4,UX5,UY5 )
      CALL  DRWLIN  ( UX4,UY4,UX6,UY6 )
      RETURN
      END                                                               DRWUTA
C
C
C
      SUBROUTINE FNDPTH
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'DATA'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'INDEX'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'PLOTTR'
      INTEGER           IP
C
C-----SUBROUTINE FNDPTH FINDS THE INTERSECTION PATHS WITHIN THE
C-----INTERSECTION
C
C-----PROCESS EACH INBOUND APPROACH
      DO 2040  IAN = 1 , NIBA
                    IF ( IAN.EQ.1.OR.ISAME.EQ.2 )
     *CALL  DRWINT  ( MINXI,MAXXI,MINYI,MAXYI,
     *                SCALEI,XSIZEI,YSIZEI,CSIZEI,' ' )
      IA = LIBA(IAN)
      JAZIM = IAAZIM(IA)
      NLANEI = NLANES(IA)
C-----PROCESS EACH LANE OF THE INBOUND APPROACH
      DO 2030  ILN = 1 , NLANEI
      IL = LLANES(ILN,IA)
C-----PROCESS EACH OUTBOUND APPROACH
      DO 2020  JAN = 1 , NOBA
      JA = LOBA(JAN)
      IF ( DIAMON )                              THEN
        IF ( ISFLAG(IA) . NE . ISFLAG(JA) )      GO TO 2020
      END IF
      KAZIM = IAAZIM(JA)
      NLANEJ = NLANES(JA)
C-----PROCESS EACH LANE OF THE OUTBOUND APPROACH
      DO 2010  JLN = 1 , NLANEJ
      JL = LLANES(JLN,JA)
C-----CALCULATE AN INTERSECTION PATH WITHIN THE INTERSECTION AND CHECK
C-----ITS LEGALITY
      CALL  CALPTH
C-----IF THE PATH COULD NOT BE CALCULATED THEN GO TO THE NEXT OUTBOUND
C-----LANE
                    IF ( IFLAG . NE . 0 )        GO TO 2010
C-----IF THE PATH DOES NOT HAVE A COMMON ALLOWED VEHICLE TYPE THEN GO TO
C-----THE NEXT OUTBOUND LANE
      IF ( IAND(  LAVT(IL),LAVT(JL)  ) . EQ . 0 )GO TO 2010
C-----IF THE PATH OPTION IS PRIMARY AND THE PATH OPTION CALCULATED FOR
C-----THE PATH IS NOT PRIMARY THEN GO TO THE NEXT OUTBOUND LANE
            IF ( IPATH.EQ.1 . AND . JOPT.NE.0 )  GO TO 2010
C-----ADD THE INTERSECTION PATH FOR THE INBOUND LANE
      CALL  ADDPTH  ( NPATHS )
                    IF ( IPLOT . EQ . 4 )        GO TO 2010
C-----DRAW THE INTERSECTION PATH OF THE PLOT PAGE
      CALL  DRWPTH  ( NPATHS )
C-----END OF OUTBOUND LANE LOOP
 2010 CONTINUE
C-----END OF OUTBOUND APPROACH LOOP
 2020 CONTINUE
C-----END OF INBOUND LANE LOOP
 2030 CONTINUE
C-----END OF INBOUND APPROACH LOOP
 2040 CONTINUE
                    IF ( NPATHS . LE . 0 )       GO TO 9060
                    IF ( IPLOT . EQ . 4 )        RETURN
                    IF ( .NOT. DIAMON )          RETURN
      CALL  DRWINT  ( MINXL,MAXXL,MINYL,MAXYL,
     *                SCALEL,XSIZEL,YSIZEL,CSIZEL,
     *                'LEFT SIDE OF DIAMOND' )
      DO 3010  IP = 1 , NPATHS
      CALL  DRWPTH  ( IP )
 3010 CONTINUE
      CALL  DRWINT  ( MINXR,MAXXR,MINYR,MAXYR,
     *                SCALER,XSIZER,YSIZER,CSIZER,
     *                'RIGHT SIDE OF DIAMOND' )
      DO 3020  IP = 1 , NPATHS
      CALL  DRWPTH  ( IP )
 3020 CONTINUE
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9060 CONTINUE
      CALL  ABORTR  ( 'STOP 906 - ' //
     *                'NUMBER OF PATHS IS LE 0 - ' //
     *                'FNDPTH'                        )
      STOP  906
      END                                                               FNDPTH
C
C
C
      SUBROUTINE CALPTH
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'APPRO'
      INCLUDE 'LANE'
      INCLUDE 'DATA'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'INDEX'
      INCLUDE 'RADIAN'
      INTEGER           IANGLE,ILNI,ILNO,IXC2,IXC3,IYC2,IYC3,KANGLE,
     *                  LAZIM,LN,LNI,LNJ,MAZIM,TAVT
      INTEGER           IAND
C
C-----SUBROUTINE CALPTH CALCULATES AN INTERSECTION PATH WITHIN THE
C-----INTERSECTION AND CHECKS ITS LEGALITY
C
      IFLAG = 1
C-----IF THE INBOUND LANE IS NOT AVAILABLE AT THE INTERSECTION THEN
C-----RETURN WITH IFLAG EQUAL 1
                    IF ( IXAPP(IL) . LT . 0 )    RETURN
                    IF ( IYAPP(IL) . LT . 0 )    RETURN
C-----IF THE OUTBOUND LANE IS NOT AVAILABLE AT THE INTERSECTION THEN
C-----RETURN WITH IFLAG EQUAL 1
                    IF ( IXAPP(JL) . LT . 0 )    RETURN
                    IF ( IYAPP(JL) . LT . 0 )    RETURN
C-----IF THE INBOUND LANE AND THE OUTBOUND LANE DO NOT HAVE A SHARED
C-----ALLOWED VEHICLE TYPE THEN RETURN WITH IFLAG EQUAL 1
      TAVT = IAND( LAVT(IL),LAVT(JL) )
                    IF ( TAVT . EQ . 0 )         RETURN
      IFLAG = 0
C-----ROTATE THE COORDINATES OF THE INBOUND LANE AND THE OUTBOUND LANE
C-----SO THAT THE INBOUND LANE IS POINTING NORTH (0 AZIMUTH)
      CALL  IROTX   ( IXAPP(IL),IYAPP(IL),-JAZIM,XI,YI )
      CALL  IROTX   ( IXAPP(JL),IYAPP(JL),-JAZIM,XO,YO )
C-----FIND THE PARAMETERS FOR CALCULATING THE INTERSECTION PATH
      ADX = DABS( XI-XO )
      ADY = DABS( YI-YO )
      LAZIM = JAZIM + 180
      MAZIM = KAZIM
                    IF ( LAZIM . GE . 360 )      LAZIM = LAZIM - 360
                    IF ( MAZIM . LT . LAZIM )    MAZIM = MAZIM + 360
      IANGLE = MAZIM - LAZIM
                    IF ( IANGLE . LT . 180 )     JANGLE = 180 - IANGLE
                    IF ( IANGLE . GE . 180 )     JANGLE = IANGLE - 180
      RA2 = 0.0D0
                    IF ( JANGLE . EQ .   0 )     GO TO 1010
                    IF ( JANGLE . EQ . 180 )     GO TO 1020
                    IF ( XO - XI )               2010 , 2010 , 3010
 1010 CONTINUE
C-----CALCULATE A STRAIGHT PATH
      KTURN = LTURNS
            IF ( XO.LT.XI . AND . ADX.GT.ZERO )  CALL  STRLFT
            IF ( XO.EQ.XI .  OR . ADX.LE.ZERO )  CALL  STRSTR
            IF ( XO.GT.XI . AND . ADX.GT.ZERO )  CALL  STRRGH
      CALL  XROTI   ( XC2,YC2,JAZIM,IXC2,IYC2 )
      CALL  XROTI   ( XC3,YC3,JAZIM,IXC3,IYC3 )
      IF ( RA2 .GT.RADIUS                   . OR .
     *     IXC2.LT.-999 . OR . IXC2.GT.9999 . OR .
     *     IYC2.LT.-999 . OR . IYC2.GT.9999 . OR .
     *     RA3 .GT.RADIUS                   . OR .
     *     IXC3.LT.-999 . OR . IXC3.GT.9999 . OR .
     *     IYC3.LT.-999 . OR . IYC3.GT.9999 )    CALL  STRSTR
      GO TO 4010
 1020 CONTINUE
C-----CALCULATE A U-TURN PATH
      KTURN = LTURNU
                    IF ( XI . GE . XO )          CALL  UTURNL
                    IF ( XI . LT . XO )          CALL  UTURNR
      GO TO 4010
 2010 CONTINUE
C-----CALCULATE A LEFT TURN PATH
      KTURN = LTURNL
                    IF ( JANGLE - 90 )           2020 , 2030 , 2030
 2020 CONTINUE
C-----LEFT TURN IS LESS THAN 90 DEGREES
            IF ( JANGLE . LE . NDEGST(IA) )      KTURN = LTURNS
      RC = ADX / ( 1.0D0 - DCOS(JANGLE*DEG2RD) )
      YC = RC*DSIN(JANGLE*DEG2RD)
                    IF ( ADY . GE . YC )         CALL  LTLTGE
                    IF ( ADY . LT . YC )         CALL  LTLTLT
      CALL  XROTI   ( XC2,YC2,JAZIM,IXC2,IYC2 )
      CALL  XROTI   ( XC3,YC3,JAZIM,IXC3,IYC3 )
      IF ( RA2 .GT.RADIUS                   . OR .
     *     IXC2.LT.-999 . OR . IXC2.GT.9999 . OR .
     *     IYC2.LT.-999 . OR . IYC2.GT.9999 . OR .
     *     RA3 .GT.RADIUS                   . OR .
     *     IXC3.LT.-999 . OR . IXC3.GT.9999 . OR .
     *     IYC3.LT.-999 . OR . IYC3.GT.9999 )    CALL  STRSTR
      GO TO 4010
 2030 CONTINUE
C-----LEFT TURN IS GREATER THAN OR EQUAL 90 DEGREES
            IF ( JANGLE . GE . 180-NDEGUT(IA) )  KTURN = LTURNU
      KANGLE = 180 - JANGLE
      RC = ADX / ( 1.0D0 + DCOS(KANGLE*DEG2RD) )
      YC = RC*DSIN(KANGLE*DEG2RD)
            IF ( ADY.GE.YC . AND . YO.GE.YI )    CALL  LTGEGE
            IF ( ADY.LT.YC .  OR . YO.LT.YI )    CALL  LTGELT
      GO TO 4010
 3010 CONTINUE
C-----CALCULATE A RIGHT TURN PATH
      KTURN = LTURNR
                    IF ( JANGLE - 90 )           3020 , 3030 , 3030
 3020 CONTINUE
C-----RIGHT TURN IS LESS THAN 90 DEGREES
            IF ( JANGLE . LE . NDEGST(IA) )      KTURN = LTURNS
      RC = ADX / ( 1.0D0 - DCOS(JANGLE*DEG2RD) )
      YC = RC*DSIN(JANGLE*DEG2RD)
                    IF ( ADY . GE . YC )         CALL  RTLTGE
                    IF ( ADY . LT . YC )         CALL  RTLTLT
      CALL  XROTI   ( XC2,YC2,JAZIM,IXC2,IYC2 )
      CALL  XROTI   ( XC3,YC3,JAZIM,IXC3,IYC3 )
      IF ( RA2 .GT.RADIUS                   . OR .
     *     IXC2.LT.-999 . OR . IXC2.GT.9999 . OR .
     *     IYC2.LT.-999 . OR . IYC2.GT.9999 . OR .
     *     RA3 .GT.RADIUS                   . OR .
     *     IXC3.LT.-999 . OR . IXC3.GT.9999 . OR .
     *     IYC3.LT.-999 . OR . IYC3.GT.9999 )    CALL  STRSTR
      GO TO 4010
 3030 CONTINUE
C-----RIGHT TURN IS GREATER THAN OR EQUAL TO 90 DEGREES
            IF ( JANGLE . GE . 180-NDEGUT(IA) )  KTURN = LTURNU
      KANGLE = 180 - JANGLE
      RC = ADX / ( 1.0D0 + DCOS(KANGLE*DEG2RD) )
      YC = RC*DSIN(KANGLE*DEG2RD)
            IF ( ADY.GE.YC . AND . YO.GE.YI )    CALL  RTGEGE
            IF ( ADY.LT.YC .  OR . YO.LT.YI )    CALL  RTGELT
 4010 CONTINUE
      IF ( DIAMON . AND . ( KTURN.EQ.LTURNU ) )  IFLAG = 1
C-----IF THE INTERSECTION PATH COULD NOT BE CALCULATED THEN RETURN
                    IF ( IFLAG . NE . 0 )        RETURN
      IF ( ( DIAMON                           ) . AND .
     *     ( IAND( LTURN(IL),LTURNU ).EQ.LTURNU ) . AND .
     *     ( IAND( LTURN(JL),LTURNU ).EQ.LTURNU ) . AND .
     *     ( KTURN                   .EQ.LTURNL ) )
     *                                           KTURN = LTURNU
C-----IF THE TURN CODE OF THE PATH DOES NOT MATCH THE TURN CODE OF THE
C-----INBOUND LANE AND THE OUTBOUND LANE THEN RETURN WITH IFLAG EQUAL 1
            IF ( IAND( LTURN(IL),KTURN ) .EQ. 0 )IFLAG = 1
            IF ( IAND( LTURN(JL),KTURN ) .EQ. 0 )IFLAG = 1
                    IF ( IFLAG . NE . 0 )        RETURN
C-----CHECK THE LANE CHANGE OPTION AND THE PATH OPTION
      ILNI = 0
      ILNO = 0
      JOPT = 0
      JLCH = 0
C-----IF THE PATH IS A U-TURN THEN RETURN AND DO NOT CHECK THE LANE
C-----CHANGE OPTION OR THE PATH OPTION
                    IF ( KTURN . EQ . LTURNU )   RETURN
C-----IF THE PATH IS A RIGHT TURN THEN GO TO 4060
                    IF ( KTURN . EQ . LTURNR )   GO TO 4060
C-----COUNT THE NUMBER OF INBOUND LANES TO THE LEFT OF THE IL LANE WITH
C-----A MATCHING TURN CODE AND ALLOWED VEHICLE TYPE FOR THE PATH
C-----(INSIDE TO OUTSIDE)
      DO 4020  LNI = 1 , NLANEI
      LN = LLANES(LNI,IA)
            IF ( IAND( KTURN,LTURN(LN) ) .EQ. 0 )GO TO 4020
            IF ( IAND( TAVT ,LAVT (LN) ) .EQ. 0 )GO TO 4020
      ILNI = ILNI + 1
                    IF ( LN . EQ . IL )          GO TO 4030
 4020 CONTINUE
      GO TO 9070
 4030 CONTINUE
C-----COUNT THE NUMBER OF OUTBOUND LANES TO THE LEFT OF THE JL LANE WITH
C-----A MATCHING TURN CODE AND ALLOWED VEHICLE TYPE FOR THE PATH
C-----(INSIDE TO OUTSIDE)
      DO 4040  LNJ = 1 , NLANEJ
      LN = LLANES(LNJ,JA)
            IF ( IAND( KTURN,LTURN(LN) ) .EQ. 0 )GO TO 4040
            IF ( IAND( TAVT ,LAVT (LN) ) .EQ. 0 )GO TO 4040
      ILNO = ILNO + 1
                    IF ( LN . EQ . JL )          GO TO 4050
 4040 CONTINUE
      GO TO 9080
 4050 CONTINUE
C-----GO TO 5010 AND CHECK THE PATH OPTION
      GO TO 5010
 4060 CONTINUE
C-----COUNT THE NUMBER OF INBOUND LANES TO THE RIGHT OF THE IL LANE WITH
C-----A MATCHING TURN CODE AND ALLOWED VEHICLE TYPE FOR THE PATH
C-----(OUTSIDE TO INSIDE)
      DO 4070  LNI = NLANEI , 1 , -1
      LN = LLANES(LNI,IA)
            IF ( IAND( KTURN,LTURN(LN) ) .EQ. 0 )GO TO 4070
            IF ( IAND( TAVT ,LAVT (LN) ) .EQ. 0 )GO TO 4070
      ILNI = ILNI + 1
                    IF ( LN . EQ . IL )          GO TO 4080
 4070 CONTINUE
      GO TO 9070
 4080 CONTINUE
C-----FIND THE LANE NUMBER OF THE FIRST OUTBOUND LANE WITH A TURN CODE
C-----THAT MATCHES THE TURN CODE AND ALLOWED VEHICLE TYPE FOR THE PATH
C-----(OUTSIDE TO INSIDE)
      DO 4090  LNJ = NLANEJ , 1 , -1
      LN = LLANES(LNJ,JA)
            IF ( IAND( KTURN,LTURN(LN) ) .EQ. 0 )GO TO 4090
            IF ( IAND( TAVT ,LAVT (LN) ) .EQ. 0 )GO TO 4090
      ILNO = ILNO + 1
                    IF ( LN . EQ . JL )          GO TO 4100
 4090 CONTINUE
      GO TO 9080
 4100 CONTINUE
 5010 CONTINUE
C-----IF NOT THE SAME RELATIVE LANE NUMBER THEN THERE IS A LANE CHANGE
                    IF ( ILNO . NE . ILNI )      JLCH = 1
C-----IF NOT THE SAME RELATIVE LANE NUMBER THEN THE PATH IS OPTION1
                    IF ( ILNO . NE . ILNI )      JOPT = 1
C-----IF MORE THAN 1 LANE CHANGED THEN THE PATH IS ILLEGAL
                    IF ( ILNO . LT . ILNI-1 )    IFLAG = 1
                    IF ( ILNO . GT . ILNI+1 )    IFLAG = 1
      RETURN
C-----PROCESS THE EXECUTION ERRORS AND STOP
 9070 CONTINUE
      CALL  ABORTR  ( 'STOP 907 - ' //
     *                'PATH TURN CODE DOES NOT MATCH ANY TURN ' //
     *                'CODE FOR INBOUND APPROACH - ' //
     *                'CALPTH'                                     )
      STOP  907
 9080 CONTINUE
      CALL  ABORTR  ( 'STOP 908 - ' //
     *                'PATH TURN CODE DOES NOT MATCH ANY TURN ' //
     *                'CODE FOR OUTBOUND APPROACH - ' //
     *                'CALPTH'                                     )
      STOP  908
      END                                                               CALPTH
C
C
C
      SUBROUTINE STRLFT
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  ANGLE
      INTEGER           MAXVEL
C
C-----SUBROUTINE STRLFT CALCULATES AN INTERSECTION PATH THAT IS A
C-----STRAIGHT THROUGH MOVEMENT THAT IS A REVERSE CIRCULAR CURVE THAT
C-----VEERS LEFT (EXACTLY 0 DEGREES)
C
C-----ADX GREATER THAN OR EQUAL ADY IS TOO MUCH TRANSVERSE CHANGE
C-----WITHIN THE INTERSECTION
                    IF ( ADX . GE . ADY )        GO TO 1010
C-----SECTION 1 (LINE 1) IS NOT USED
      CALL  ZEROP1
C-----CALCULATE SECTION 2 (ARC 1) AS A REVERSE CIRCULAR CURVE
      RA2 = ( ADX**2+ADY**2 )/( 4.0D0*ADX )
      XC2 = XI - RA2
      YC2 = YI
      ANGLE = RAD2DG*DATAN(ADY/(2.0D0*RA2-ADX))
      JANGLE = DMAX1( 1.0D0,ANGLE+XROUND )
      L2 = ANGLE*RA2*DEG2RD + XROUND
      JB2 = 90
      JD2 = -JANGLE
C-----CALCULATE SECTION 3 (ARC 2) AS A REVERSE CIRCULAR CURVE
      RA3 = RA2
      XC3 = XO + RA3
      YC3 = YO
      L3 = L2
      JB3 = 270 - JANGLE
      JD3 = JANGLE
C-----SECTION 4 (LINE 2) IS NOT USED
      CALL  ZEROP4
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
      RETURN
 1010 CONTINUE
      IFLAG = 1
      RETURN
      END                                                               STRLFT
C
C
C
      SUBROUTINE STRSTR
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
C
C-----SUBROUTINE STRSTR CALCULATES AN INTERSECTION PATH THAT IS A
C-----STRAIGHT THROUGH MOVEMENT THAT GOES STRAIGHT FROM THE INBOUND LANE
C-----TO THE OUTBOUND LANE
C
C-----CALCULATE SECTION 1 (LINE 1) FROM THE INBOUND LANE TO THE OUTBOUND
C-----LANE
      X11 = XI
      Y11 = YI
      L1 = DSQRT(ADX**2+ADY**2) + XROUND
      X12 = XO
      Y12 = YO
C-----SECTION 2 (ARC 1) IS NOT USED
      CALL  ZEROP2
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----SECTION 4 (LINE 2) IS NOT USED
      CALL  ZEROP4
C-----SET A HIGH MAXIMUM SPEED FOR THE INTERSECTION PATH SO THAT THE
C-----SPEED LIMIT OF THE INBOUND AND THE OUTBOUND APPROACH WILL GOVERN
      JSPEED = 999
      RETURN
      END                                                               STRSTR
C
C
C
      SUBROUTINE STRRGH
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  ANGLE
      INTEGER           MAXVEL
C
C-----SUBROUTINE STRRGH CALCULATES AN INTERSECTION PATH AS A STRAIGHT
C-----STRAIGHT THROUGH MOVEMENT THAT IS A REVERSE CIRCULAR CURVE THAT
C-----VEERS RIGHT (EXACTLY 0 DEGREES)
C
C-----ADX GREATER THAN OR EQUAL ADY IS TOO MUCH TRANSVERSE CHANGE
C-----WITHIN THE INTERSECTION
                    IF ( ADX . GE . ADY )        GO TO 1010
C-----SECTION 1 (LINE 1) IS NOT USED
      CALL  ZEROP1
C-----CALCULATE SECTION 2 (ARC 1) AS A REVERSE CIRCULAR CURVE
      RA2 = ( ADX**2+ADY**2 )/( 4.0D0*ADX )
      XC2 = XI + RA2
      YC2 = YI
      ANGLE = RAD2DG*DATAN(ADY/(2.0D0*RA2-ADX))
      JANGLE = DMAX1( 1.0D0,ANGLE+XROUND )
      L2 = ANGLE*RA2*DEG2RD + XROUND
      JB2 = 270
      JD2 = JANGLE
C-----CALCULATE SECTION 3 (ARC 2) AS A REVERSE CIRCULAR CURVE
      RA3 = RA2
      XC3 = XO - RA3
      YC3 = YO
      L3 = L2
      JB3 = 90 + JANGLE
      JD3 = -JANGLE
C-----SECTION 4 (LINE 2) IS NOT USED
      CALL  ZEROP4
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
      RETURN
 1010 CONTINUE
      IFLAG = 1
      RETURN
      END                                                               STRRGH
C
C
C
      SUBROUTINE UTURNL
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      INTEGER           MAXVEL
C
C-----SUBROUTINE UTURNL CALCULATES AN INTERSECTION PATH THAT IS A U-TURN
C-----THAT GOES LEFT (EXACTLY 180 DEGREES)
C
C-----CALCULATE SECTION 1 (LINE 1) AS A LINE FROM THE INBOUND LANE TO
C-----THE START OF SECTION 2 (ARC 1)
      X11 = XI
      Y11 = YI
      L1 = ADY + XROUND
      X12 = XI
      Y12 = YI + ADY
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC FROM THE END OF SECTION 1
C-----(LINE 1) TO THE START OF SECTION 4 (LINE 2)
      RA2 = ADX / 2.0D0
      XC2 = XI - RA2
      YC2 = YI
                    IF ( YO . GT . YI )          YC2 = YO
      L2 = JANGLE*RA2*DEG2RD + XROUND
      JB2 = 90
      JD2 = -JANGLE
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----CALCULATE SECTION 4 (LINE 2) AS A LINE FROM THE END OF SECTION 2
C-----(ARC 1) TO THE OUTBOUND LANE
      X41 = XO
      Y41 = YO + ADY
      L4 = ADY + XROUND
      X42 = XO
      Y42 = YO
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
C-----IF THE INBOUND LANE IS ABOVE THE OUTBOUND LANE THEN SECTION 1
C-----(LINE 1) IS NOT USED
                    IF ( YI . GE . YO )          CALL  ZEROP1
C-----IF THE OUTBOUND LANE IS ABOVE THE INBOUND LANE THEN SECTION 4
C-----(LINE 2) IS NOT USED
                    IF ( YO . GE . YI )          CALL  ZEROP4
      RETURN
      END                                                               UTURNL
C
C
C
      SUBROUTINE UTURNR
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      INTEGER           MAXVEL
C
C-----SUBROUTINE UTURNR CALCULATES AN INTERSECTION PATH THAT IS A U-TURN
C-----THAT GOES RIGHT (EXACTLY 180 DEGREES)
C
C-----CALCULATE SECTION 1 (LINE 1) AS A LINE FROM THE INBOUND LANE TO
C-----THE START OF SECTION 2 (ARC 1)
      X11 = XI
      Y11 = YI
      L1 = ADY + XROUND
      X12 = XI
      Y12 = YI + ADY
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC FROM THE END OF SECTION 1
C-----(LINE 1) TO THE START OF SECTION 4 (LINE 4)
      RA2 = ADX / 2.0D0
      XC2 = XI + RA2
      YC2 = YI
                    IF ( YO . GT . YI )          YC2 = YO
      L2 = JANGLE*RA2*DEG2RD + XROUND
      JB2 = 270
      JD2 = JANGLE
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----CALCULATE SECTION 4 (LINE 2) AS A LINE FROM THE END OF SECTION 2
C-----(ARC 1) TO THE OUTBOUND LANE
      X41 = XO
      Y41 = YO + ADY
      L4 = ADY + XROUND
      X42 = XO
      Y42 = YO
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
C-----IF THE INBOUND LANE IS ABOVE THE OUTBOUND LANE THEN SECTION 1
C-----(LINE 1) IS NOT USED
                    IF ( YI . GE . YO )          CALL  ZEROP1
C-----IF THE OUTBOUND LANE IS ABOVE THE INBOUND LANE THEN SECTION 4
C-----(LINE 2) IS NOT USED
                    IF ( YO . GE . YI )          CALL  ZEROP4
      RETURN
      END                                                               UTURNR
C
C
C
      SUBROUTINE LTLTGE
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  DY
      INTEGER           MAXVEL
C
C-----SUBROUTINE LTLTGE CALCULATES AN INTERSECTION PATH THAT IS A LEFT
C-----TURN LT 90 DEGREES AND ADY GE YC WITH RADIUS RC
C
C-----CALCULATE SECTION 1 (LINE 1) AS A LINE FROM THE INBOUND LANE TO
C-----THE START OF SECTION 2 (ARC 1)
      X11 = XI
      Y11 = YI
      DY = ADY - YC
      L1 = DY + XROUND
      X12 = XI
      Y12 = YI + DY
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC WITH RADIUS RC FROM THE END
C-----OF SECTION 1 (LINE 1) TO THE OUTBOUND LANE
      RA2 = RC
      XC2 = XI - RA2
      YC2 = YI + DY
      L2 = JANGLE*RA2*DEG2RD + XROUND
      JB2 = 90
      JD2 = -JANGLE
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----SECTION 4 (LINE 2) IS NOT USED
      CALL  ZEROP4
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
C-----IF THE LENGTH OF SECTION 1 (LINE 1) IS LE 0 THEN SECTION 1 IS NOT
C-----USED
                    IF ( L1 . LE . 0 )           CALL  ZEROP1
      RETURN
      END                                                               LTLTGE
C
C
C
      SUBROUTINE LTLTLT
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  A,ANGLE2,ANGLE3,B,C,COSJA,DY,RADICL,SINJA
      INTEGER           KANGL2,KANGL3,KANGLE
      INTEGER           MAXVEL
C
C-----SUBROUTINE LTLTLT CALCULATES AN INTERSECTION PATH THAT IS A LEFT
C-----TURN LT 90 DEGREES AND ADY LT YC
C
C-----CALCULATE SECTION 4 (LINE 2) AS A LINE FROM THE END OF SECTION 2
C-----(ARC 1) TO THE START OF THE OUTBOUND LANE
      X42 = XO
      Y42 = YO
      DY = YC - ADY
      L4 = DY + XROUND
      KANGLE = 90 - JANGLE
      X41 = XO + DY*DCOS(KANGLE*DEG2RD)
      Y41 = YO - DY*DSIN(KANGLE*DEG2RD)
C-----IF THE START OF SECTION 4 (LINE 2) IS TO THE RIGHT OR BELOW THE
C-----INBOUND LANE THEN GO TO 1010 AND CALCULATE A REVERSE CURVE
                    IF ( X41 . GE . XI )         GO TO 1010
                    IF ( Y41 . LE . YI )         GO TO 1010
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC FROM THE INBOUND LANE TO THE
C-----START OF SECTION 4 (LINE 2)
      RA2 = XI-X41 + (Y41-YI)/DTAN(JANGLE*DEG2RD)
      XC2 = XI - RA2
      YC2 = YI
      L2 = JANGLE*RA2*DEG2RD + XROUND
      JB2 = 90
      JD2 = -JANGLE
C-----SECTION 1 (LINE 1) IS NOT USED
      CALL  ZEROP1
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
C-----IF THE LENGTH OF SECTION 4 (LINE 2) IS LE 0 THEN SECTION 4 IS NOT
C-----USED
                    IF ( L4 . LE . 0 )           CALL  ZEROP4
      RETURN
 1010 CONTINUE
C-----CALCULATE A REVERSE CURVE
C-----SECTION 1 (LINE 1) IS NOT USED
      CALL  ZEROP1
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC FROM THE INBOUND LANE TO THE
C-----START OF SECTION 3 (ARC 2)
      SINJA = DSIN(JANGLE*DEG2RD)
      COSJA = DCOS(JANGLE*DEG2RD)
      A = 2.0D0 - 2.0D0*COSJA
      B = 2.0D0*ADX*(1.0D0+COSJA) - 2.0D0*ADY*SINJA
      C = ADX**2 + ADY**2
      C = -C
      RADICL = B**2 - 4.0D0*A*C
C-----IF RADICL IS LT 0.0 THEN THE PATH CAN NOT BE CALCULATED
                    IF ( RADICL . LT . 0.0D0 )   GO TO 2010
      RA2 = (-B+DSQRT(RADICL))/(2.0D0*A)
      XC2 = XI - RA2
      YC2 = YI
      ANGLE2 = RAD2DG*DATAN((RA2*SINJA+ADY)/(RA2+RA2*COSJA-ADX))
      KANGL2 = DMAX1( 1.0D0,ANGLE2+XROUND )
      L2 = ANGLE2*RA2*DEG2RD + XROUND
      JB2 = 90
      JD2 = -KANGL2
C-----CALCULATE SECTION 3 (ARC 2) AS AN ARC FROM THE END OF SECTION 2
C-----(ARC 2) TO THE OUTBOUND LANE
      RA3 = RA2
      XC3 = XO + RA3*COSJA
      YC3 = YO + RA3*SINJA
      ANGLE3 = ANGLE2 - JANGLE
      KANGL3 = DMAX1( 1.0D0,ANGLE3+XROUND )
      L3 = ANGLE3*RA3*DEG2RD + XROUND
      JB3 = 270 - JANGLE - KANGL3
      JD3 = KANGL3
C-----SECTION 4 (LINE 2) IS NOT USED
      CALL  ZEROP4
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
      RETURN
 2010 CONTINUE
      IFLAG = 1
      RETURN
      END                                                               LTLTLT
C
C
C
      SUBROUTINE LTGEGE
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  DY
      INTEGER           MAXVEL
C
C-----SUBROUTINE LTGEGE CALCULATES AN INTERSECTION PATH THAT IS A LEFT
C-----TURN GE 90 DEGREES AND ADY GE YC WITH RADIUS RC
C
C-----CALCULATE SECTION 1 (LINE 1) AS A LINE FROM THE INBOUND LANE TO
C-----THE START OF SECTION 2 (ARC 1)
      X11 = XI
      Y11 = YI
      DY = ADY - YC
      L1 = DY + XROUND
      X12 = XI
      Y12 = YI + DY
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC WITH RADIUS RC FROM THE END
C-----OF SECTION 1 (LINE 1) TO THE OUTBOUND LANE
      RA2 = RC
      XC2 = XI - RA2
      YC2 = YI + DY
      L2 = JANGLE*RA2*DEG2RD + XROUND
      JB2 = 90
      JD2 = -JANGLE
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----SECTION 4 (LINE 2) IS NOT USED
      CALL  ZEROP4
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
C-----IF THE LENGTH OF SECTION 1 (LINE 1) IS LE 0 THEN SECTION 1 IS NOT
C-----USED
                    IF ( L1 . LE . 0 )           CALL  ZEROP1
      RETURN
      END                                                               LTGEGE
C
C
C
      SUBROUTINE LTGELT
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  DY
      INTEGER           KANGLE
      INTEGER           MAXVEL
C
C-----SUBROUTINE LTGELT CALCULATES AN INTERSECTION PATH THAT IS A LEFT
C-----TURN GE 90 DEGREES AND ADY LT YC
C
C-----CALCULATE SECTION 4 (LINE 2) AS A LINE FROM THE END OF SECTION 2
C-----TO THE OUTBOUND LANE
      X42 = XO
      Y42 = YO
C     DY = YI + YC - YO
C-----CHANGED 07-MAY-2001 BY TWR BASED UPON AN ERROR FOUND IN IGIDS
      DY = YC + ADY
                    IF ( YO . GE . YI )          DY = YC - ADY
      L4 = DY + XROUND
      KANGLE = JANGLE - 90
      X41 = XO + DY*DCOS(KANGLE*DEG2RD)
      Y41 = YO + DY*DSIN(KANGLE*DEG2RD)
C-----IF THE START OF SECTION 4 (LINE 2) IS TO THE RIGHT OR BELOW THE
C-----INBOUND LANE THEN THE PATH CAN NOT BE CALCULATED
                    IF ( X41 . GE . XI )         GO TO 2010
                    IF ( Y41 . LE . YI )         GO TO 2010
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC FROM THE INBOUND LANE TO THE
C-----START OF SECTION 4 (LINE 2)
C     RA2 = XI - X41
C                   IF ( JANGLE . EQ . 90 )      GO TO 1010
C     KANGLE = 180 - JANGLE
C     RA2 = RA2 - (Y41-YI)/DTAN(KANGLE*DEG2RD)
C1010 CONTINUE
C-----CHANGED 07-MAY-2001 BY TWR BASED UPON AN ERROR FOUND IN IGIDS
      RA2 = (ADX-DY*DCOS((JANGLE-90 )*DEG2RD))
     1    /(1.0D0+DCOS((180-JANGLE)*DEG2RD))
      XC2 = XI - RA2
      YC2 = YI
      L2 = JANGLE*RA2*DEG2RD + XROUND
      JB2 = 90
      JD2 = -JANGLE
C-----SECTION 1 (LINE 1) IS NOT USED
      CALL  ZEROP1
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
C-----IF THE LENGTH OF SECTION 4 (LINE 2) IS LE 0 THEN SECTION 4 IS NOT
C-----USED
                    IF ( L4 . LE . 0 )           CALL  ZEROP4
      RETURN
 2010 CONTINUE
      IFLAG = 1
      RETURN
      END                                                               LTGELT
C
C
C
      SUBROUTINE RTLTGE
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  DY
      INTEGER           MAXVEL
C
C-----SUBROUTINE RTLTGE CALCULATES AN INTERSECTION PATH THAT IS A RIGHT
C-----TURN LT 90 DEGREES AND ADY GE YC WITH RADIUS RC
C
C-----CALCULATE SECTION 1 (LINE 1) AS A LINE FROM THE INBOUND LANE TO
C-----THE START OF SECTION 2 (ARC 1)
      X11 = XI
      Y11 = YI
      DY = ADY - YC
      L1 = DY + XROUND
      X12 = XI
      Y12 = YI + DY
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC WITH RADIUS RC FROM THE END
C-----OF SECTION 1 TO THE OUTBOUND LANE
      RA2 = RC
      XC2 = XI + RA2
      YC2 = YI + DY
      L2 = JANGLE*RA2*DEG2RD + XROUND
      JB2 = 270
      JD2 = JANGLE
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----SECTION 4 (LINE 2) IS NOT USED
      CALL  ZEROP4
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
C-----IF THE LENGTH OF SECTION 1 (LINE 1) IS LE 0 THEN SECTION 1 IS NOT
C-----USED
                    IF ( L1 . LE . 0 )           CALL  ZEROP1
      RETURN
      END                                                               RTLTGE
C
C
C
      SUBROUTINE RTLTLT
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  A,ANGLE2,ANGLE3,B,C,COSJA,DY,RADICL,SINJA
      INTEGER           KANGL2,KANGL3,KANGLE
      INTEGER           MAXVEL
C
C-----SUBROUTINE RTLTLT CALCULATES AN INTERSECTION PATH THAT IS A RIGHT
C-----TURN LT 90 DEGREES AND ADY LT YC
C
C-----CALCULATE SECTION 4 (LINE 2) AS A LINE FROM THE END OF SECTION 2
C-----(ARC 1) TO THE OUTBOUND LANE
      X42 = XO
      Y42 = YO
      DY = YC - ADY
      L4 = DY + XROUND
      KANGLE = 90 - JANGLE
      X41 = XO - DY*DCOS(KANGLE*DEG2RD)
      Y41 = YO - DY*DSIN(KANGLE*DEG2RD)
C-----IF THE START OF SECTION 4 (LINE 2) IS TO THE LEFT OR BELOW THE
C-----INBOUND LANE THEN GO TO 1010 AND CALCULATE REVERSE CURVES
                    IF ( X41 . LE . XI )         GO TO 1010
                    IF ( Y41 . LE . YI )         GO TO 1010
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC FROM THE INBOUND LANE TO THE
C-----START OF SECTION 4 (LINE 2)
      RA2 = X41 - XI + (Y41-YI)/DTAN(JANGLE*DEG2RD)
      XC2 = XI + RA2
      YC2 = YI
      L2 = JANGLE*RA2*DEG2RD + XROUND
      JB2 = 270
      JD2 = JANGLE
C-----SECTION 1 (LINE 1) IS NOT USED
      CALL  ZEROP1
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
C-----IF THE LENGTH OF SECTION 4 (LINE 2) IS LE 0 THEN SECTION 4 IS NOT
C-----USED
                    IF ( L4 . LE . 0 )           CALL  ZEROP4
      RETURN
 1010 CONTINUE
C-----CALCULATE REVERSE CURVES
C-----SECTION 1 (LINE 1) IS NOT USED
      CALL  ZEROP1
C-----CALCULATE SECTION 2 (ARC 1) AS A REVERSE CURVE FROM THE INBOUND
C-----LANE TO THE START OF SECTION 3 (ARC 2)
      SINJA = DSIN(JANGLE*DEG2RD)
      COSJA = DCOS(JANGLE*DEG2RD)
      A = 2.0D0 - 2.0D0*COSJA
      B = 2.0D0*ADX*(1.0D0+COSJA) - 2.0D0*ADY*SINJA
      C = ADX**2 + ADY**2
      C = -C
      RADICL = B**2 - 4.0D0*A*C
C-----IF RADICL LT 0.0 THEN THE REVERSE CURVE CAN NOT BE CALCULATED
                    IF ( RADICL . LT . 0.0D0 )   GO TO 2010
      RA2 = (-B+DSQRT(RADICL))/(2.0D0*A)
      XC2 = XI + RA2
      YC2 = YI
      ANGLE2 = RAD2DG*DATAN((RA2*SINJA+ADY)/(RA2+RA2*COSJA-ADX))
      KANGL2 = DMAX1( 1.0D0,ANGLE2+XROUND )
      L2 = ANGLE2*RA2*DEG2RD + XROUND
      JB2 = 270
      JD2 = KANGL2
C-----CALCULATE SECTION 3 (ARC 2) AS A REVERSE CURVE FROM THE END OF
C-----SECTION 2 (ARC 1) TO THE OUTBOUND LANE
      RA3 = RA2
      XC3 = XO - RA3*COSJA
      YC3 = YO + RA3*SINJA
      ANGLE3 = ANGLE2 - JANGLE
      KANGL3 = DMAX1( 1.0D0,ANGLE3+XROUND )
      L3 = ANGLE3*RA3*DEG2RD + XROUND
      JB3 = 90 + JANGLE + KANGL3
      JD3 = -KANGL3
C-----SECTION 4 (LINE 2) IS NOT USED
      CALL  ZEROP4
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
      RETURN
 2010 CONTINUE
      IFLAG = 1
      RETURN
      END                                                               RTLTLT
C
C
C
      SUBROUTINE RTGEGE
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  DY
      INTEGER           MAXVEL
C
C-----SUBROUTINE RTGEGE CALCULATES AN INTERSECTION PATH THAT IS A RIGHT
C-----TURN GE 90 DEGREES AND ADY GE YC WITH RADIUS RC
C
C-----CALCULATE SECTION 1 (LINE 1) AS A LINE FROM THE INBOUND LANE TO
C-----THE START OF SECTION 2 (ARC 1)
      X11 = XI
      Y11 = YI
      DY = ADY - YC
      L1 = DY + XROUND
      X12 = XI
      Y12 = YI + DY
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC WITH RADIUS RC FROM THE END
C-----OF SECTION 1 (LINE 1) TO THE OUTBOUND LANE
      RA2 = RC
      XC2 = XI + RA2
      YC2 = YI + DY
      L2 = JANGLE*RA2*DEG2RD + XROUND
      JB2 = 270
      JD2 = JANGLE
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----SECTION 4 (LINE 2) IS NOT USED
      CALL  ZEROP4
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
C-----IF THE LENGTH OF SECTION 1 (LINE 1) IS LE 0 THEN SECTION 1 IS NOT
C-----USED
                    IF ( L1 . LE . 0 )           CALL  ZEROP1
      RETURN
      END                                                               RTGEGE
C
C
C
      SUBROUTINE RTGELT
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  DY
      INTEGER           KANGLE
      INTEGER           MAXVEL
C
C-----SUBROUTINE RTGELT CALCULATES AN INTERSECTION PATH THAT IS A RIGHT
C-----TURN GE 90 DEGREES AND ADY LT YC
C
C-----CALCULATE SECTION 4 (LINE 2) AS A LINE FROM THE END OF SECTION 2
C-----(ARC 1) TO THE OUTBOUND LANE
      X42 = XO
      Y42 = YO
      DY = YI + YC - YO
      L4 = DY + XROUND
      KANGLE = JANGLE - 90
      X41 = XO - DY*DCOS(KANGLE*DEG2RD)
      Y41 = YO + DY*DSIN(KANGLE*DEG2RD)
C-----IF THE START OF SECTION 4 (LINE 2) IS TO THE LEFT OR BELOW THE
C-----INBOUND LANE THEN THE PATH CAN NOT BE CALCULATED
                    IF ( X41 . LE . XI )         GO TO 2010
                    IF ( Y41 . LE . YI )         GO TO 2010
C-----SECTION 3 (ARC 2) IS NOT USED
      CALL  ZEROP3
C-----CALCULATE SECTION 2 (ARC 1) AS AN ARC FROM THE INBOUND LANE TO THE
C-----START OF SECTION 4 (LINE 2)
      RA2 = X41 - XI
                    IF ( JANGLE . EQ . 90 )      GO TO 1010
      KANGLE = 180 - JANGLE
      RA2 = RA2 - (Y41-YI)/DTAN(KANGLE*DEG2RD)
 1010 CONTINUE
      XC2 = XI + RA2
      YC2 = YI
      L2 = JANGLE*RA2*DEG2RD + XROUND
      JB2 = 270
      JD2 = JANGLE
C-----SECTION 1 (LINE 1) IS NOT USED
      CALL  ZEROP1
C-----CALCULATE THE MAXIMUM VELOCITY FOR THE INTERSECTION PATH BASED ON
C-----THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF THE PATH
      JSPEED = MAXVEL( RA2 )
C-----IF THE LENGTH OF SECTION 4 (LINE 2) IS LE 0 THEN SECTION 4 IS NOT
C-----USED
                    IF ( L4 . LE . 0 )           CALL  ZEROP4
      RETURN
 2010 CONTINUE
      IFLAG = 1
      RETURN
      END                                                               RTGELT
C
C
C
      SUBROUTINE ZEROP1
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'DATA'
C
C-----SUBROUTINE ZEROP1 ZEROES OUT THE PARAMETERS FOR SECTION 1 OF THE
C-----INTERSECTION PATH (LINE 1)
C
      X11 = 0.0D0
      Y11 = 0.0D0
      L1  = 0
      X12 = 0.0D0
      Y12 = 0.0D0
      RETURN
      END                                                               ZEROP1
C
C
C
      SUBROUTINE ZEROP2
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'DATA'
C
C-----SUBROUTINE ZEROP2 ZEROES OUT THE PARAMETERS FOR SECTION 2 OF THE
C-----INTERSECTION PATH (ARC 1)
C
      XC2 = 0.0D0
      YC2 = 0.0D0
      RA2 = 0.0D0
      L2  = 0
      JB2 = 0
      JD2 = 0
      RETURN
      END                                                               ZEROP2
C
C
C
      SUBROUTINE ZEROP3
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'DATA'
C
C-----SUBROUTINE ZEROP3 ZEROES OUT THE PARAMETERS FOR SECTION 3 OF THE
C-----INTERSECTION PATH (ARC 2)
C
      XC3 = 0.0D0
      YC3 = 0.0D0
      RA3 = 0.0D0
      L3  = 0
      JB3 = 0
      JD3 = 0
      RETURN
      END                                                               ZEROP3
C
C
C
      SUBROUTINE ZEROP4
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'DATA'
C
C-----SUBROUTINE ZEROP4 ZEROES OUT THE PARAMETERS FOR SECTION 4 OF THE
C-----INTERSECTION PATH (LINE 2)
C
      X41 = 0.0D0
      Y41 = 0.0D0
      L4  = 0
      X42 = 0.0D0
      Y42 = 0.0D0
      RETURN
      END                                                               ZEROP4
C
C
C
      FUNCTION   MAXVEL ( R )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      INCLUDE 'DATA'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  A,AL,AP,B,BL,BP,C,CP,R,VELMPH
      INTEGER           MAXVEL
      DATA     AL     / +0.19000000D0 /
      DATA     AP     / +0.49671329D0 /
      DATA     BL     / -0.00100000D0 /
      DATA     BP     / -0.01403629D0 /
      DATA     CP     / +0.00013951D0 /
C
C-----SUBROUTINE MAXVEL FINDS THE MAXIMUM VELOCITY FOR AN INTERSECTION
C-----PATH BASED ON THE MAXIMUM SAFE SIDE FRICTION AND THE RADIUS OF
C-----THE INTERSECTION PATH
C
                    IF ( R . LT . 0.0D0 )        GO TO 2010
C-----FIND THE MAXIMUM VELOCITY USING THE LINEAR EQUATION FOR MAXIMUM
C-----SAFE SIDE FRICTION
      A = 1.0D0
      B = -15.0D0*R*BL
      C = -15.0D0*R*AL
      VELMPH = (-B+DSQRT(B**2-4.0D0*A*C))/(2.0D0*A)
C-----IF THE MAXIMUM VELOCITY IS GT 46.7 THEN THE LINEAR EQUATION FOR
C-----MAXIMUM SAFE SIDE FRICTION WAS VALID AND GO TO 1010
                    IF ( VELMPH . GT . 46.7D0 )  GO TO 1010
C-----CALCULATE THE MAXIMUM VELOCITY USING THE PARABOLIC EQUATION FOR
C-----MAXIMUM SAFE SIDE FRICTION
      A = 1.0D0 - 15.0D0*R*CP
      B = -15.0D0*R*BP
      C = -15.0D0*R*AP
      VELMPH = (-B+DSQRT(B**2-4.0D0*A*C))/(2.0D0*A)
 1010 CONTINUE
C-----CONVERT THE MAXIMUM VELOCITY FROM MPH TO FPS
      MAXVEL = MPH2FS*VELMPH + XROUND
      RETURN
 2010 CONTINUE
      IFLAG = 1
      MAXVEL = 0
      RETURN
      END                                                               MAXVEL
C
C
C
      SUBROUTINE ADDPTH ( IT1 )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'DATA'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'INDEX'
      INCLUDE 'RADIAN'
      INTEGER           IT1,NP
      INTEGER           ILNB,TAVT
  909 FORMAT(21HNUMBER OF PATHS IS GT,I4)
  910 FORMAT(31HNUMBER OF PATHS FROM LANE IS GT,I2)
  919 FORMAT(39HINTERSECTION PATH FROM INBOUND APPROACH,I3,5H LANE,I2,
     *       21H TO OUTBOUND APPROACH,I3,5H LANE,I2,
     *       46H DOES NOT HAVE A MATCHING ALLOWED VEHICLE TYPE)
C
C-----SUBROUTINE ADDPTH ADDS INTERSECTION PATHS FOR A LANE
C
C-----SET INTERSECTION PATH ALLOWED VEHICLE TYPES
      TAVT = IAND( LAVT(IL),LAVT(JL) )
                    IF ( TAVT . EQ . 0 )         GO TO 9190
      NPATHS = NPATHS + 1
      IT1=NPATHS
                    IF ( NPATHS . GT . NPA )     GO TO 9090
C-----SET UP INDEXES FOR THE INTERSECTION PATHS
      IIA   (NPATHS) = IA
      IIL   (NPATHS) = ILN
      LIBL  (NPATHS) = IL
      IOA   (NPATHS) = JA
      IOL   (NPATHS) = JLN
      LOBL  (NPATHS) = JL
      NGEOCP(NPATHS) = 0
      PAVT  (NPATHS) = TAVT
C-----STORE PARAMETERS FOR SECTION 1 (LINE 1) OF THE INTERSECTION PATH
      CALL  XROTI   ( X11,Y11,JAZIM,IXL(1,NPATHS),IYL(1,NPATHS) )
      LL1(NPATHS) = L1
      CALL  XROTI   ( X12,Y12,JAZIM,JXL(1,NPATHS),JYL(1,NPATHS) )
C-----STORE PARAMETERS FOR SECTION 2 (ARC 1) OF THE INTERSECTION PATH
      CALL  XROTI   ( XC2,YC2,JAZIM,IXA(1,NPATHS),IYA(1,NPATHS) )
      LA1(NPATHS) = L2
      IRA(1,NPATHS) = RA2 + XROUND
      CALL  AJAZIM  ( JAZIM,JB2,IBA(1,NPATHS),JD2,IDA(1,NPATHS),L2 )
C-----STORE PARAMETERS FOR SECTION 3 (ARC 2) OF THE INTERSECTION PATH
      CALL  XROTI   ( XC3,YC3,JAZIM,IXA(2,NPATHS),IYA(2,NPATHS) )
      LA2(NPATHS) = L3
      IRA(2,NPATHS) = RA3 + XROUND
      CALL  AJAZIM  ( JAZIM,JB3,IBA(2,NPATHS),JD3,IDA(2,NPATHS),L3 )
C-----STORE PARAMETERS FOR SECTION 4 (LINE 2) OF THE INTERSECTION PATH
      CALL  XROTI   ( X41,Y41,JAZIM,IXL(2,NPATHS),IYL(2,NPATHS) )
      LL2(NPATHS) = L4
      CALL  XROTI   ( X42,Y42,JAZIM,JXL(2,NPATHS),JYL(2,NPATHS) )
C-----STORE OTHER PARAMETERS FOR THE INTERSECTION PATH
      LENP(NPATHS) = L1 + L2 + L3 + L4
      IPTURN(NPATHS) = KTURN
      LIMP(NPATHS) = MIN0( JSPEED,ISLIM(IA),ISLIM(JA) )
      IOPT(NPATHS) = JOPT
      ILCH(NPATHS) = JLCH
C-----ADD THE INTERSECTION PATH FOR THE INBOUND LANE
      NPINT(IL) = NPINT(IL) + 1
                    IF ( NPINT(IL) . GT . NLP )  GO TO 9100
      NP = NPINT(IL)
      LINTP(NP,IL) = NPATHS
C-----ADD THE INTERSECTION PATH FOR THE OUTBOUND LANE
      NPINT(JL) = NPINT(JL) + 1
                    IF ( NPINT(JL) . GT . NLP )  GO TO 9100
      NP = NPINT(JL)
      LINTP(NP,JL) = NPATHS
      RETURN
C-----PROCESS THE EXECUTION ERRORS AND STOP
 9090 CONTINUE
      NPATHS = NPA
      WRITE (ERRMSG,909) NPA
      CALL  ABORTR  ( 'STOP 909 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'ADDPTH'                             )
      STOP  909
 9100 CONTINUE
      WRITE (ERRMSG,910) NLP
      CALL  ABORTR  ( 'STOP 910 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'ADDPTH'                             )
      STOP  910
 9190 CONTINUE
      WRITE (ERRMSG,919) IA,ILN,JA,JLN
      CALL  ABORTR  ( 'STOP 919 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'ADDPTH'                             )
      STOP  919
      END                                                               ADDPTH
C
C
C
      SUBROUTINE AJAZIM ( JAZIML,JB2OR3,IBAL,JD2OR3,IDAL,L2OR3 )
      IMPLICIT NONE                                                     CCODE=C.
      INTEGER           IBAL,IDAL,JAZIML,JB2OR3,JD2OR3,L2OR3
C
C-----SUBROUTINE AJAZIM ADDS JAZIML TO JB2OR3 AND MAKES IT FALL IN THE
C-----RANGE FROM 0 TO 359 DEGREES AND SETS IDAL TO JD2OR3 WHEN THE
C-----LENGTH OF THE ARC (L2OR3) IS GT 0
C
C-----ADD JAZIML TO JB2OR3 AND MAKE IT FALL IN THE RANGE FROM 0 TO 359
C-----DEGREES
      IBAL = JAZIML + JB2OR3
 1010 CONTINUE
                    IF ( IBAL . LT .   0 )       IBAL = IBAL + 360
                    IF ( IBAL . GE . 360 )       IBAL = IBAL - 360
                    IF ( IBAL . LT .   0 )       GO TO 1010
                    IF ( IBAL . GE . 360 )       GO TO 1010
C-----SET IDAL TO JD2OR3
      IDAL = JD2OR3
C-----IF THE LENGTH OF THE ARC (L2OR3) IS GT 0 THEN RETURN
                    IF ( L2OR3 . GT . 0 )        RETURN
C-----SET IBAL AND IDAL TO 0 AND RETURN
      IBAL = 0
      IDAL = 0
      RETURN
      END                                                               AJAZIM
C
C
C
      SUBROUTINE CHKPTH
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'INDEX'
      DIMENSION         LTURNC(4),LTURNI(4)
      CHARACTER*1       LTURNC
      INTEGER           LTURNI
      INTEGER           IPINT,ITEST,KP,LTURNT
      INTEGER           IAND,ILNB
C-----LTURNR     1 LTURN, IPT, AND IPTURN CODE - RIGHT TURN
C-----LTURNS     2 LTURN, IPT, AND IPTURN CODE - STRAIGHT
C-----LTURNL     4 LTURN, IPT, AND IPTURN CODE - LEFT TURN
C-----LTURNU     8 LTURN, IPT, AND IPTURN CODE - U-TURN
      DATA     LTURNI / LTURNR,LTURNS,LTURNL,LTURNU /
      DATA     LTURNC / 'R'   ,'S'   ,'L'   ,'U'    /
  911 FORMAT(16HINBOUND APPROACH,I2,2H =,I3,
     *       41H - NO PATH INTO THE INTERSECTION FOR LANE,I2)
  912 FORMAT(16HINBOUND APPROACH,I2,2H =,I3,
     *       41H - NO PATH INTO THE INTERSECTION FOR LANE,I2,
     *       21H WITH A TURN CODE = (,A1,1H))
C
C-----SUBROUTINE CHKPTH CHECKS EACH INBOUND LANE THAT IS AVAILABLE AT
C-----THE INTERSECTION TO SEE IF AN INTERSECTION PATH WAS CALCULATED FOR
C-----EACH TURNING MOVEMENT SPECIFIED FOR THE INBOUND LANE
C
C-----PROCESS EACH INBOUND APPROACH
      DO 1040  IAN = 1 , NIBA
      IA = LIBA(IAN)
C-----PROCESS EACH LANE OF THE INBOUND APPROACH
      DO 1030  ILN = 1 , NLANES(IA)
      IL = LLANES(ILN,IA)
C-----IF THE INBOUND LANE IS NOT AVAILABLE AT THE INTERSECTION THEN
C-----PROCESS THE NEXT INBOUND LANE
                    IF ( IXAPP(IL) . LT . 0 )    GO TO 1030
                    IF ( IYAPP(IL) . LT . 0 )    GO TO 1030
                    IF ( NPINT(IL) . LE . 0 )    GO TO 9110
C-----TEST THE INBOUND LANE FOR EACH TURN CODE POSSIBLE
      DO 1020  ITEST = 1 , 4
      LTURNT = LTURNI(ITEST)
C-----IF THE INBOUND LANE DID NOT HAVE THE TURN CODE SELECTED THEN
C-----PROCESS THE NEXT TURN CODE POSSIBLE
            IF ( IAND( LTURNT,LTURN(IL) ).EQ.0 ) GO TO 1020
C-----CHECK EACH INTERSECTION PATH FROM THIS INBOUND LANE TO SEE IF AT
C-----LEAST ONE OF THE INTERSECTION PATHS HAS THE TEST TURN CODE
      DO 1010  IPINT = 1 , NPINT(IL)
      KP = LINTP(IPINT,IL)
C-----IF THE TURN CODES MATCH THEN PROCESS THE NEXT TURN CODE POSSIBLE
      IF ( IAND( IPTURN(KP),LTURNT ) . NE . 0 )  GO TO 1020
C-----END OF INTERSECTION PATH LOOP
 1010 CONTINUE
      GO TO 9120
C-----END OF TEST TURN CODE LOOP
 1020 CONTINUE
C-----END OF INBOUND LANE LOOP
 1030 CONTINUE
C-----END OF INBOUND APPROACH LOOP
 1040 CONTINUE
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9110 CONTINUE
      WRITE (ERRMSG,911) IAN,IA,ILN
      CALL  ABORTR  ( 'STOP 911 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'CHKPTH'                             )
      STOP  911
 9120 CONTINUE
      WRITE (ERRMSG,912) IAN,IA,ILN,LTURNC(ITEST)
      CALL  ABORTR  ( 'STOP 912 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'CHKPTH'                             )
      STOP  912
      END                                                               CHKPTH
C
C
C
      SUBROUTINE WRITLA
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'LANE'
      INCLUDE 'SDR'
      INCLUDE 'SDRC'
      INCLUDE 'GEOCOM'
      INCLUDE 'OUTPT'
      INTEGER           I,ISDRC,ISDRS,KL,NUMLAN
  601 FORMAT(20I4)
C
C-----SUBROUTINE WRITLA WRITES THE LANE INFORMATION AND THE SIGHT
C-----DISTANCE RESTRICTION INFORMATION ONTO TAPE MODELT FOR SIMPRO
C
      NUMLAN = NIBL + NOBL
      WRITE (MODELT,601) NUMLAN
C-----WRITE THE INFORMATION FOR EACH LANE
      DO 1010  KL = 1 , NUMLAN
                    IF ( LTYPE(KL) . EQ . 2 )    LTURN(KL) = 0
      WRITE (MODELT,601) LWID(KL),LTURN(KL),NPINT(KL),NLL(KL),NLR(KL),
     *                   ISNA(KL),(LGEOM(I,KL),I=1,4),IDX(KL),IBLN(KL),
     *                   LAVT(KL)
                    IF ( NPINT(KL) . LE . 0 )    GO TO 1010
      WRITE (MODELT,601) (LINTP(I,KL),I=1,NPINT(KL))
 1010 CONTINUE
      WRITE (MODELT,601) NSDRS,NSS,NSDRC
                    IF ( NSDRS . LE . 0 )        GO TO 2020
C-----WRITE THE INFORMATION FOR EACH SIGHT DISTANCE RESTRICTION
      DO 2010  ISDRS = 1 , NSDRS
      WRITE (MODELT,601) (ICANSE(I,ISDRS),I=1,NSS)
 2010 CONTINUE
 2020 CONTINUE
                    IF ( NSDRC . LE . 0 )        GO TO 2040
C-----WRITE THE COORDINATE INFORMATION FOR EACH SIGHT DISTANCE
C-----RESTRICTION
      DO 2030  I = 1 , NSDRC
      ISDRC = LSDRC(I)
      WRITE (MODELT,601) ISDRC,IXSDRC(ISDRC),IYSDRC(ISDRC)
 2030 CONTINUE
 2040 CONTINUE
      RETURN
      END                                                               WRITLA
C
C
C
      SUBROUTINE FNDCON
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'LANE'
      INCLUDE 'PATH'
      INCLUDE 'GEOCP'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INTEGER           IBAND,IFS,IZ,JCLOSE,KCLOSE,MIBL,MLCH,MOA,MOBL,
     *                  MPTURN,NC
C
C-----SUBROUTINE FNDCON FINDS THE INTERSECTION CONFLICTS BETWEEN THE
C-----INTERSECTION PATHS
C
C-----CHECK EACH INTERSECTION PATH EXCEPT THE LAST
      DO 7010  IP = 1 , NPATHS-1
C-----SET THE INTERSECTION PATH AS THE MAIN INTERSECTION PATH IN THE
C-----BAND
      MXL(1,1) = IXL (1,IP)
      MXL(2,1) = IXL (2,IP)
      MYL(1,1) = IYL (1,IP)
      MYL(2,1) = IYL (2,IP)
      NXL(1,1) = JXL (1,IP)
      NXL(2,1) = JXL (2,IP)
      NYL(1,1) = JYL (1,IP)
      NYL(2,1) = JYL (2,IP)
      MXA(1,1) = IXA (1,IP)
      MXA(2,1) = IXA (2,IP)
      MYA(1,1) = IYA (1,IP)
      MYA(2,1) = IYA (2,IP)
      MLL(1  ) = LL1 (  IP)
      MAL(1  ) = LA1 (  IP)
      MAL(2  ) = LA2 (  IP)
      MLL(2  ) = LL2 (  IP)
      MBA(1,1) = IBA (1,IP)
      MBA(2,1) = IBA (2,IP)
      MDA(1,1) = IDA (1,IP)
      MDA(2,1) = IDA (2,IP)
      MRA(1,1) = IRA (1,IP)
      MRA(2,1) = IRA (2,IP)
      MIA      = IIA (  IP)
      MIBL     = LIBL(  IP)
      MOA      = IOA (  IP)
      MOBL     = LOBL(  IP)
      MLCH     = ILCH(  IP)
      MPTURN   = IPTURN(IP)
C-----BUILD A BAND 1 FOOT TO THE LEFT AND TO THE RIGHT OF THE MAIN
C-----INTERSECTION PATH
      DO 1010  IZ = 2 , 3
      MXL(1,IZ) = 0
      MXL(2,IZ) = 0
      MYL(1,IZ) = 0
      MYL(2,IZ) = 0
      NXL(1,IZ) = 0
      NXL(2,IZ) = 0
      NYL(1,IZ) = 0
      NYL(2,IZ) = 0
      MXA(1,IZ) = 0
      MXA(2,IZ) = 0
      MYA(1,IZ) = 0
      MYA(2,IZ) = 0
      MBA(1,IZ) = 0
      MBA(2,IZ) = 0
      MDA(1,IZ) = 0
      MDA(2,IZ) = 0
      MRA(1,IZ) = 0
      MRA(2,IZ) = 0
 1010 CONTINUE
      CALL  BAND    ( 2,1,-1 )
      CALL  BAND    ( 3,1,+1 )
      JCLOSE = -1
C-----CHECK AGAINST EACH INTERSECTION PATH THAT HAS A HIGHER NUMBER
C-----THAN THE INTERSECTION PATH BEING CHECKED
      DO 6010  JP = IP+1 , NPATHS
C-----IF THE INTERSECTION PATHS ORIGINATE FROM THE SAME INBOUND APPROACH
C-----AND THE SAME INBOUND LANE THEN SKIP THIS INTERSECTION PATH
      IF ( MIA.EQ.IIA(JP).AND.MIBL.EQ.LIBL(JP) ) GO TO 6010
      KCLOSE = MIN( LWID(LIBL(IP))-2,LWID(LOBL(IP))-2,
     *              LWID(LIBL(JP))-2,LWID(LOBL(JP))-2,7 )
C-----IF EITHER OF THE INTERSECTION PATHS CHANGES LANES THEN SKIP THE
C-----NEXT TEST
                    IF ( MLCH . NE . 0 )         GO TO 1020
                    IF ( ILCH(JP) . NE . 0 )     GO TO 1020
C-----IF THE INTERSECTION PATHS ORIGINATE FROM THE SAME INBOUND APPROACH
C-----AND GO TO DIFFERENT OUTBOUND LANES THEN SKIP THIS INTERSECTION
C-----PATH
C OLD IF ( MIA.EQ.IIA(JP).AND.MOBL.NE.LOBL(JP) ) GO TO 6010
 1020 CONTINUE
C-----IF EITHER OF THE INTERSECTION PATHS IS A STRAIGHT THROUGH MOVEMENT
C-----OR A RIGHT TURN THEN GO TO 1030 AND BUILD THE KCLOSE BANDS
                    IF ( MPTURN     .LE. LTURNS )GO TO 1030
                    IF ( IPTURN(JP) .LE. LTURNS )GO TO 1030
C-----IF THE INTERSECTION PATHS GO TO THE SAME OUTBOUND APPROACH BUT GO
C-----TO DIFFERENT OUTBOUND LANES THEN GO TO 1030 AND BUILD THE KCLOSE
C-----BANDS
      IF ( MOA.EQ.IOA(JP).AND.MOBL.NE.LOBL(JP) ) GO TO 1030
C-----BOTH INTERSECTION PATHS ARE U-TURN OR LEFT TURNS THUS IF THE
C-----ICLOSE BANDS ARE ALREADY BUILT THEN GO TO 1050 ELSE BUILD THE
C-----ICLOSE BANDS
                    IF ( JCLOSE . EQ . ICLOSE )  GO TO 1050
      JCLOSE = ICLOSE
      GO TO 1040
 1030 CONTINUE
C-----ONE OF THE INTERSECTION PATHS IS A STRAIGHT THROUGH MOVEMENT OR
C-----A RIGHT TURN THUS IF THE KCLOSE BANDS ARE ALREADY BUILT THEN GO
C-----TO 1050 ELSE BUILD THE KCLOSE BANDS
                    IF ( JCLOSE . EQ . KCLOSE )  GO TO 1050
      JCLOSE = KCLOSE
 1040 CONTINUE
      DO 1045  IZ = 4 , 5
      MXL(1,IZ) = 0
      MXL(2,IZ) = 0
      MYL(1,IZ) = 0
      MYL(2,IZ) = 0
      NXL(1,IZ) = 0
      NXL(2,IZ) = 0
      NYL(1,IZ) = 0
      NYL(2,IZ) = 0
      MXA(1,IZ) = 0
      MXA(2,IZ) = 0
      MYA(1,IZ) = 0
      MYA(2,IZ) = 0
      MBA(1,IZ) = 0
      MBA(2,IZ) = 0
      MDA(1,IZ) = 0
      MDA(2,IZ) = 0
      MRA(1,IZ) = 0
      MRA(2,IZ) = 0
 1045 CONTINUE
      CALL  BAND    ( 4,JCLOSE,-1 )
      CALL  BAND    ( 5,JCLOSE,+1 )
 1050 CONTINUE
      NC = 0
C-----CHECK EACH BAND OF THE INTERSECTION PATH STARTING WITH THE MAIN
C-----INTERSECTION PATH, THEN THE 1 FOOT BANDS, AND FINALLY THE ICLOSE
C-----BANDS
      DO 5010  IBAND = 1 , 5
C-----CHECK THE FIRST AND SECOND LINE AND ARC
      DO 4010  IFS = 1 , 2
                    IF ( MLL(IFS) . EQ . 0 )     GO TO 3010
                    IF ( LL1(JP) . EQ . 0 )      GO TO 2010
C-----CHECK BAND IBAND OF LINE IFS OF THE INTERSECTION PATH FOR
C-----CONFLICTS WITH LINE 1 OF THE OTHER INTERSECTION PATH
      CALL  CLTOLC  ( IFS,IBAND,1,NC )
 2010 CONTINUE
                    IF ( LA1(JP) . EQ . 0 )      GO TO 2020
C-----CHECK BAND IBAND OF LINE IFS OF THE INTERSECTION PATH FOR
C-----CONFLICTS WITH ARC 1 OF THE OTHER INTERSECTION PATH
      CALL  CLTOAC  ( IFS,IBAND,1,NC )
 2020 CONTINUE
                    IF ( LA2(JP) . EQ . 0 )      GO TO 2030
C-----CHECK BAND IBAND OF LINE IFS OF THE INTERSECTION PATH FOR
C-----CONFLICTS WITH ARC 2 OF THE OTHER INTERSECTION PATH
      CALL  CLTOAC  ( IFS,IBAND,2,NC )
 2030 CONTINUE
                    IF ( LL2(JP) . EQ . 0 )      GO TO 3010
C-----CHECK BAND IBAND OF LINE IFS OF THE INTERSECTION PATH FOR
C-----CONFLICTS WITH LINE 2 OF THE OTHER INTERSECTION PATH
      CALL  CLTOLC  ( IFS,IBAND,2,NC )
 3010 CONTINUE
                    IF ( MAL(IFS) . EQ . 0 )     GO TO 4010
                    IF ( LL1(JP) . EQ . 0 )      GO TO 3020
C-----CHECK BAND IBAND OF ARC IFS OF THE INTERSECTION PATH FOR
C-----CONFLICTS WITH LINE 1 OF THE OTHER INTERSECTION PATH
      CALL  CATOLC  ( IFS,IBAND,1,NC )
 3020 CONTINUE
                    IF ( LA1(JP) . EQ . 0 )      GO TO 3030
C-----CHECK BAND IBAND OF ARC IFS OF THE INTERSECTION PATH FOR
C-----CONFLICTS WITH ARC 1 OF THE OTHER INTERSECTION PATH
      CALL  CATOAC  ( IFS,IBAND,1,NC )
 3030 CONTINUE
                    IF ( LA2(JP) . EQ . 0 )      GO TO 3040
C-----CHECK BAND IBAND OF ARC IFS OF THE INTERSECTION PATH FOR
C-----CONFLICTS WITH ARC 2 OF THE OTHER INTERSECTION PATH
      CALL  CATOAC  ( IFS,IBAND,2,NC )
 3040 CONTINUE
                    IF ( LL2(JP) . EQ . 0 )      GO TO 4010
C-----CHECK BAND IBAND OF ARC IFS OF THE INTERSECTION PATH FOR
C-----CONFLICTS WITH LINE 2 OF THE OTHER INTERSECTION PATH
      CALL  CATOLC  ( IFS,IBAND,2,NC )
C-----END OF FIRST OR SECOND ARC OR LINE LOOP
 4010 CONTINUE
C-----IF A CONFLICT WAS DETECTED THEN GO TO THE NEXT INTERSECTION PATH
                    IF ( NC . NE . 0 )           GO TO 5020
C-----END OF BAND LOOP
 5010 CONTINUE
 5020 CONTINUE
C-----END OF OTHER INTERSECTION PATH LOOP
 6010 CONTINUE
C-----END OF INTERSECTION PATH LOOP
 7010 CONTINUE
                    IF ( NCONFS . LE . 0 )       GO TO 9130
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9130 CONTINUE
      CALL  ABORTR  ( 'STOP 913 - ' //
     *                'TOTAL NUMBER OF CONFLICTS IS LE 0 - ' //
     *                'FNDCON'                                  )
      STOP  913
      END                                                               FNDCON
C
C
C
      SUBROUTINE BAND   ( IB,IDIST,ILR )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'GEOCP'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  BEARX,BEARY
      INTEGER           IAZ1,IAZ2,IB,IDIST,ILR
      DOUBLE PRECISION  AZIM36
C
C-----SUBROUTINE BAND BUILDS A BAND IDIST DISTANCE FROM THE MAIN
C-----INTERSECTION PATH EITHER LEFT OR RIGHT OF THE MAIN INTERSECTION
C-----PATH DEPENDING UPON ILR
C
                    IF ( MLL(1) . LE . 0 )       GO TO 1010
C-----BUILD A BAND FOR SECTION 1 (LINE 1) OF THE INTERSECTION PATH
      BEARX = NXL(1,1) - MXL(1,1)
      BEARY = NYL(1,1) - MYL(1,1)
      IAZ1 = AZIM36( BEARY,BEARX ) +  ILR*90 + XROUND
      CALL  XROTAI  ( 0.0D0,DBLE(IDIST),IAZ1,MXL(1,1),MYL(1,1),
     *                MXL(1,IB),MYL(1,IB) )
      CALL  XROTAI  ( 0.0D0,DBLE(IDIST),IAZ1,NXL(1,1),NYL(1,1),
     *                NXL(1,IB),NYL(1,IB) )
 1010 CONTINUE
                    IF ( MAL(1) . LE . 0 )       GO TO 2010
C-----BUILD A BAND FOR SECTION 2 (ARC 1) OF THE INTERSECTION PATH
      MXA(1,IB) = MXA(1,1)
      MYA(1,IB) = MYA(1,1)
      MBA(1,IB) = MBA(1,1)
      MDA(1,IB) = MDA(1,1)
      MRA(1,IB) = MRA(1,1) - ILR*(ISIGN( 1,MDA(1,IB))*IDIST ) + XROUND
 2010 CONTINUE
                    IF ( MAL(2) . LE . 0 )       GO TO 3010
C-----BUILD A BAND FOR SECTION 3 (ARC 2) OF THE INTERSECTION PATH
      MXA(2,IB) = MXA(2,1)
      MYA(2,IB) = MYA(2,1)
      MBA(2,IB) = MBA(2,1)
      MDA(2,IB) = MDA(2,1)
      MRA(2,IB) = MRA(2,1) - ILR*(ISIGN( 1,MDA(2,IB))*IDIST ) + XROUND
 3010 CONTINUE
                    IF ( MLL(2) . LE . 0 )       RETURN
C-----BUILD A BAND FOR SECTION 4 (LINE 2) OF THE INTERSECTION PATH
      BEARX = NXL(2,1) - MXL(2,1)
      BEARY = NYL(2,1) - MYL(2,1)
      IAZ2 = AZIM36( BEARY,BEARX ) +  ILR*90 + XROUND
      CALL  XROTAI  ( 0.0D0,DBLE(IDIST),IAZ2,MXL(2,1),MYL(2,1),
     *                MXL(2,IB),MYL(2,IB) )
      CALL  XROTAI  ( 0.0D0,DBLE(IDIST),IAZ2,NXL(2,1),NYL(2,1),
     *                NXL(2,IB),NYL(2,IB) )
      RETURN
      END                                                               BAND
C
C
C
      SUBROUTINE CLTOLC ( IFS,IBAND,JFS,NC )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'PATH'
      INCLUDE 'GEOCP'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  AZ1,AZ2,X1,X2,X3,X4,Y1,Y2,Y3,Y4
      INTEGER           IBAND,IFS,IL1,IL2,ITEST,JFS,NC
      DOUBLE PRECISION  AZIM36
      INTEGER           LTOL
C
C-----SUBROUTINE CLTOLC CHECKS FOR INTERSECTION CONFLICTS BETWEEN THE
C-----LINE PORTION OF THE INTERSECTION PATH BEING CHECKED AND THE LINE
C-----PORTION OF THE INTERSECTION PATH BEING CHECKED AGAINST
C
      X1 = MXL(IFS,IBAND)
      Y1 = MYL(IFS,IBAND)
      X2 = NXL(IFS,IBAND)
      Y2 = NYL(IFS,IBAND)
      X3 = IXL(JFS,JP)
      Y3 = IYL(JFS,JP)
      X4 = JXL(JFS,JP)
      Y4 = JYL(JFS,JP)
C-----TEST IF LINE A FROM (X1,Y1) TO (X2,Y2) FOR THE INTERSECTION PATH
C-----BEING CHECKED INTERSECTS WITH LINE B FROM (X3,Y3) TO (X4,Y4) FOR
C-----THE INTERSECTION PATH BEING CHECKED AGAINST
      ITEST = LTOL( X1,Y1,X2,Y2,X3,Y3,X4,Y4,XINT1,YINT1,XINT2,YINT2 )
                    IF ( ITEST . EQ . 0 )        RETURN
C-----FIND THE PARAMETERS FOR THE FIRST INTERSECTION CONFLICT
      IL1 = DSQRT((XINT1-MXL(IFS,IBAND))**2+(YINT1-MYL(IFS,IBAND))**2) +
     *      XROUND
      AZ1 = AZIM36( Y2-Y1,X2-X1 )
                    IF ( IFS . EQ . 1 )          GO TO 1010
      IL1 = IL1 + MLL(1) + MAL(1) + MAL(2)
 1010 CONTINUE
      IL2 = DSQRT((XINT1-IXL(JFS,JP))**2+(YINT1-IYL(JFS,JP))**2)+XROUND
      AZ2 = AZIM36( Y4-Y3,X4-X3 )
                    IF ( JFS . EQ . 1 )          GO TO 1020
      IL2 = IL2 + LL1(JP) + LA1(JP) + LA2(JP)
 1020 CONTINUE
C-----ADD THE INTERSECTION CONFLICT BETWEEN THE INTERSECTION PATHS
      CALL  ADDCON  ( IP,MIA,IL1,AZ1,JP,IIA(JP),IL2,AZ2,NC )
C-----IF THERE WAS ONLY ONE INTERSECTION CONFLICT BETWEEN LINE A AND
C-----LINE B THEN RETURN ELSE FIND THE PARAMETERS FOR THE INTERSECTION
C-----CONFLICT
                    IF ( ITEST . EQ . 1 )        RETURN
      IL1 = DSQRT((XINT2-MXL(IFS,IBAND))**2+(YINT2-MYL(IFS,IBAND))**2) +
     *      XROUND
                    IF ( IFS . EQ . 1 )          GO TO 2010
      IL1 = IL1 + MLL(1) + MAL(1) + MAL(2)
 2010 CONTINUE
      IL2 = DSQRT((XINT2-IXL(JFS,JP))**2+(YINT2-IYL(JFS,JP))**2)+XROUND
                    IF ( JFS . EQ . 1 )          GO TO 2020
      IL2 = IL2 + LL1(JP) + LA1(JP) + LA2(JP)
 2020 CONTINUE
C-----ADD THE INTERSECTION CONFLICT BETWEEN THE INTERSECTION PATHS
      CALL  ADDCON  ( IP,MIA,IL1,AZ1,JP,IIA(JP),IL2,AZ2,NC )
      RETURN
      END                                                               CLTOLC
C
C
C
      SUBROUTINE ADDCON ( INP,INA,INL,ANI,JNP,JNA,JNL,ANJ,NNC )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONFLT'
      INCLUDE 'PATH'
      INCLUDE 'GEOCOM'
      INCLUDE 'GEOVAL'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  ANI,ANJ
      INTEGER           IANGLE,ICON,INA,INL,INP,JNA,JNL,JNP,KP,LP,
     *                  MGEOCP,NNC
      INTEGER           ILNB
  914 FORMAT(31HTOTAL NUMBER OF CONFLICTS IS GT,I5)
  915 FORMAT(28HNUMBER OF CONFLICTS FOR PATH,I4,8H OR PATH,I4,
     *       6H IS GT,I3)
C
C-----SUBROUTINE ADDCON ADDS INTERSECTION CONFLICTS BETWEEN TWO
C-----INTERSECTION PATHS
C
      IANGLE = ANJ - ANI + XROUND
 1010 CONTINUE
                    IF ( IANGLE . LE . 0 )       IANGLE = IANGLE + 360
                    IF ( IANGLE . GE . 360 )     IANGLE = IANGLE - 360
                    IF ( IANGLE . LT . 0 )       GO TO 1010
                    IF ( IANGLE . GT . 360 )     GO TO 1010
                    IF ( NCONFS . LE . 0 )       GO TO 2020
C-----CHECK TO SEE IF THERE IS ALREADY AN INTERSECTION CONFLICT BETWEEN
C-----THESE TWO INTERSECTION PATHS THAT ARE CLOSE TOGETHER
      DO 2010  ICON = 1 , NCONFS
      KP = -1
            IF ( ICONP(1,ICON) . EQ . INP )      KP = 1
            IF ( ICONP(2,ICON) . EQ . INP )      KP = 2
C-----IF THE INTERSECTION CONFLICT DOES NOT INVOLVE INTERSECTION PATH
C-----INP THEN GO TO 2010 AND SKIP TO THE NEXT INTERSECTION CONFLICT
                    IF ( KP . LE . 0 )           GO TO 2010
      LP = 3 - KP
C-----IF THE INTERSECTION CONFLICT DOES NOT INVOLVE INTERSECTION PATH
C-----JNP THEN GO TO 2010 AND SKIP TO THE NEXT INTERSECTION CONFLICT
            IF ( ICONP(LP,ICON) . NE . JNP )     GO TO 2010
C-----IF THE DISTANCES TO THE INTERSECTION CONFLICT ARE GT ICLOSE THEN
C-----GO TO 2010 AND SKIP TO THE NEXT INTERSECTION CONFLICT
      IF ( IABS(ICOND(KP,ICON)-INL).GT.ICLOSE )  GO TO 2010
      IF ( IABS(ICOND(LP,ICON)-JNL).GT.ICLOSE )  GO TO 2010
C-----AVERAGE THE INTERSECTION CONFLICTS AND RE-STORE
      ICOND(KP,ICON) = 0.5D0*(ICOND(KP,ICON)+INL) + XROUND
      ICOND(LP,ICON) = 0.5D0*(ICOND(LP,ICON)+JNL) + XROUND
                    IF ( KP . EQ . 2 )           IANGLE = 360 - IANGLE
                    IF ( IANGLE . EQ . 360 )     IANGLE = 0
      ICONAN(ICON) = 0.5D0*(ICONAN(ICON)+IANGLE) + XROUND
C-----RETURN WITHOUT ADDING THE INTERSECTION CONFLICT
      RETURN
 2010 CONTINUE
 2020 CONTINUE
      NNC = NNC + 1
      NCONFS = NCONFS + 1
                    IF ( NCONFS . GT . NCO )     GO TO 9140
C-----ADD INTERSECTION CONFLICT FOR INTERSECTION PATH BEING CHECKED
C-----(INP)
      MGEOCP = NGEOCP(INP)
      MGEOCP = MGEOCP + 1
                    IF ( MGEOCP . GT . NCP )     GO TO 9150
      IGEOCP(MGEOCP,INP) = NCONFS
      NGEOCP(INP) = MGEOCP
C-----ADD INTERSECTION CONFLICT FOR INTERSECTION PATH BEING CHECKED
C-----AGAINST (JNP)
      MGEOCP = NGEOCP(JNP)
      MGEOCP = MGEOCP + 1
                    IF ( MGEOCP . GT . NCP )     GO TO 9150
      IGEOCP(MGEOCP,JNP) = NCONFS
      NGEOCP(JNP) = MGEOCP
C-----SET PARAMETERS FOR INTERSECTION CONFLICT NCONFS
      ICONP(1,NCONFS) = INP
      ICONP(2,NCONFS) = JNP
      ICONA(1,NCONFS) = INA
      ICONA(2,NCONFS) = JNA
      ICOND(1,NCONFS) = INL
      ICOND(2,NCONFS) = JNL
      ICONAN (NCONFS) = IANGLE
      ICONI(1,NCONFS) = 0
      ICONI(2,NCONFS) = 0
      RETURN
C-----PROCESS THE EXECUTION ERRORS AND STOP
 9140 CONTINUE
      NCONFS = NCO
      WRITE (ERRMSG,914) NCO
      CALL  ABORTR  ( 'STOP 914 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'ADDCON'                             )
      STOP  914
 9150 CONTINUE
      WRITE (ERRMSG,915) INP,JNP,NCP
      CALL  ABORTR  ( 'STOP 915 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'ADDCON'                             )
      STOP  915
      END                                                               ADDCON
C
C
C
      SUBROUTINE CLTOAC ( IFS,IBAND,JFS, NC )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'PATH'
      INCLUDE 'GEOCP'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  A,B,C,RADICL,X,XB,XM
      INTEGER           IBAND,IFS,JFS,NC
C
C-----SUBROUTINE CLTOAC CHECKS FOR INTERSECTION CONFLICTS BETWEEN THE
C-----LINE PORTION OF THE INTERSECTION PATH BEING CHECKED AND THE ARC
C-----PORTION OF THE INTERSECTION PATH BEING CHECKED AGAINST
C
C-----IF THE LINE IS VERTICAL THEN GO TO 1050
      IF ( IABS(NXL(IFS,IBAND)-MXL(IFS,IBAND)) . LE . 0 )
     *                                           GO TO 1050
C-----FIND THE SLOPE AND THE Y INTERCEPT OF THE LINE
      XM = DBLE(NYL(IFS,IBAND)-MYL(IFS,IBAND)) /
     *     DBLE(NXL(IFS,IBAND)-MXL(IFS,IBAND))
      XB = MYL(IFS,IBAND) - MXL(IFS,IBAND)*XM
C-----FIND THE POINT(S) OF INTERSECTION BETWEEN THE LINE AND THE ARC
      A = 1.0D0 + XM**2
      B = -2.0D0*IXA(JFS,JP) + 2.0D0*XM*XB - 2.0D0*IYA(JFS,JP)*XM
      C = IXA(JFS,JP)**2 + IYA(JFS,JP)**2 + XB**2 - IRA(JFS,JP)**2 -
     *    2.0D0*IYA(JFS,JP)*XB
      RADICL = B**2 - 4.0D0*A*C
                    IF ( DABS(RADICL).LE.ZERO )  GO TO 1010
                    IF ( RADICL )                2010 , 1010 , 1030
 1010 CONTINUE
C-----FIND 1 POINT OF INTERSECTION BETWEEN THE LINE AND THE ARC
      XINT1 = -B/(2.0D0*A)
      YINT1 = XM*XINT1 + XB
 1020 CONTINUE
C-----ADD 1 POINT OF INTERSECTION BETWEEN THE LINE AND THE ARC
      CALL  ADDLA   ( IFS,IBAND,JFS,NC,1 )
      RETURN
 1030 CONTINUE
C-----FIND 2 POINTS OF INTERSECTION BETWEEN THE LINE AND THE ARC
      XINT1 = (-B+DSQRT(RADICL))/(2.0D0*A)
      YINT1 = XM*XINT1 + XB
      XINT2 = (-B-DSQRT(RADICL))/(2.0D0*A)
      YINT2 = XM*XINT2 + XB
 1040 CONTINUE
C-----ADD 2 POINTS OF INTERSECTION BETWEEN THE LINE AND THE ARC
      CALL  ADDLA   ( IFS,IBAND,JFS,NC,2 )
      RETURN
 1050 CONTINUE
C-----FIND THE INTERSECTION BETWEEN THE VERTICAL LINE AND THE ARC
      X = 0.5D0*(MXL(IFS,IBAND)+NXL(IFS,IBAND))
      A = 1.0D0
      B = -2.0D0*IYA(JFS,JP)
      C = IYA(JFS,JP)**2 + (X-IXA(JFS,JP))**2 - IRA(JFS,JP)**2
      RADICL = B**2  - 4.0D0*A*C
                    IF ( DABS(RADICL).LE.ZERO )  GO TO 1060
                    IF ( RADICL )                2010 , 1060 , 1070
 1060 CONTINUE
C-----FIND 1 POINT OF INTERSECTION BETWEEN THE VERTICAL LINE AND THE ARC
      XINT1 = X
      YINT1 = -B/(2.0D0*A)
      GO TO 1020
 1070 CONTINUE
C-----FIND 2 POINTS OF INTERSECTION BETWEEN THE VERTICAL LINE AND THE
C-----ARC
      XINT1 = X
      YINT1 = (-B+DSQRT(RADICL))/(2.0D0*A)
      XINT2 = X
      YINT2 = (-B-DSQRT(RADICL))/(2.0D0*A)
      GO TO 1040
 2010 CONTINUE
      RETURN
      END                                                               CLTOAC
C
C
C
      SUBROUTINE ADDLA  ( IFS,IBAND,JFS,NC,NUM )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'PATH'
      INCLUDE 'GEOCP'
      INCLUDE 'GEOVAL'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  AZIM1,AZIM2,AZ11,AZ12,AZ21,AZ22,BEARX,BEARY,DA1,
     *                  DA2,X,XBEAR,YBEAR
      INTEGER           IBAND,IFS,IL1,IL2,ITEST1,ITEST2,JFS,JTEST1,
     *                  JTEST2,NC,NUM,NUMPTS
      DOUBLE PRECISION  AZIM36
      INTEGER           ICHKA,ICHKL
C
C-----SUBROUTINE ADDLA ADDS INTERSECTION CONFLICTS BETWEEN THE LINE
C-----PORTION OF THE INTERSECTION PATH BEING CHECKED AND THE ARC PORTION
C-----OF THE INTERSECTION PATH BEING CHECKED AGAINST
C
      NUMPTS = NUM
 1010 CONTINUE
C-----CHECK IF THE FIRST POINT OF INTERSECTION LIES ON THE LINE
      ITEST1 = ICHKL( MXL(IFS,IBAND),MYL(IFS,IBAND),NXL(IFS,IBAND),
     *         NYL(IFS,IBAND),XINT1,YINT1 )
C-----CHECK IF THE FIRST POINT OF INTERSECTION LIES ON THE ARC
      BEARX = NXL(IFS,IBAND) - MXL(IFS,IBAND)
      BEARY = NYL(IFS,IBAND) - MYL(IFS,IBAND)
      AZ11 = AZIM36( BEARY,BEARX )
      XBEAR = XINT1 - IXA(JFS,JP)
      YBEAR = YINT1 - IYA(JFS,JP)
      AZIM1 = AZIM36( YBEAR,XBEAR )
      AZ12 = AZIM1 + ISIGN( 90,IDA(JFS,JP) )
      ITEST2 = ICHKA( AZIM1,IBA(JFS,JP),IDA(JFS,JP),DA1 )
      JTEST1 = 1
      JTEST2 = 1
                    IF ( NUMPTS . EQ . 1 )       GO TO 1020
C-----CHECK IF THE SECOND POINT OF INTERSECTION LIES ON THE LINE
      JTEST1 = ICHKL( MXL(IFS,IBAND),MYL(IFS,IBAND),NXL(IFS,IBAND),
     *         NYL(IFS,IBAND),XINT2,YINT2 )
C-----CHECK IF THE SECOND POINT OF INTERSECTION LIES ON THE ARC
      BEARX = NXL(IFS,IBAND) - MXL(IFS,IBAND)
      BEARY = NYL(IFS,IBAND) - MYL(IFS,IBAND)
      AZ21 = AZIM36( BEARY,BEARX )
      XBEAR = XINT2 - IXA(JFS,JP)
      YBEAR = YINT2 - IYA(JFS,JP)
      AZIM2 = AZIM36( YBEAR,XBEAR )
      AZ22 = AZIM2 + ISIGN( 90,IDA(JFS,JP) )
      JTEST2 = ICHKA( AZIM2,IBA(JFS,JP),IDA(JFS,JP),DA2 )
 1020 CONTINUE
C-----IF NEITHER POINT OF INTERSECTION LIES ON BOTH THE LINE AND THE ARC
C-----THEN RETURN
      IF ( (ITEST1.NE.0.OR .ITEST2.NE.0) . AND .
     *     (JTEST1.NE.0.OR .JTEST2.NE.0) )       RETURN
C-----IF ONLY THE FIRST POINT OF INTERSECTION LIES ON BOTH THE LINE AND
C-----THE ARC THEN ADD THE FIRST POINT OF INTERSECTION
      IF ( (ITEST1.EQ.0.AND.ITEST2.EQ.0) . AND .
     *     (JTEST1.NE.0.OR .JTEST2.NE.0) )       GO TO 2010
C-----IF ONLY THE SECOND POINT OF INTERSECTION LIES ON BOTH THE LINE AND
C-----THE ARC THEN ADD THE SECOND POINT OF INTERSECTION
      IF ( (ITEST1.NE.0.OR .ITEST2.NE.0) . AND .
     *     (JTEST1.EQ.0.AND.JTEST2.EQ.0) )       GO TO 3010
C-----IF THIS IS NOT THE MAIN INTERSECTION PATH THEN GO TO 4010
                    IF ( IBAND . NE . 1 )        GO TO 4010
C-----IF THE DISTANCE BETWEEN THE 2 POINTS OF CONFLICT ON THE MAIN
C-----INTERSECTION PATH IS LE ICLOSE THEN GO TO 4010
      X = DSQRT((XINT1-XINT2)**2+(YINT1-YINT2)**2)
                    IF ( X.LE.DBLE(ICLOSE) )     GO TO 4010
 2010 CONTINUE
C-----ADD FIRST POINT OF INTERSECTION AS AN INTERSECTION CONFLICT
      IL1 = DSQRT((XINT1-MXL(IFS,IBAND))**2+(YINT1-MYL(IFS,IBAND))**2) +
     *      XROUND
                    IF ( IFS . EQ . 1 )          GO TO 2020
      IL1 = IL1 + MLL(1) + MAL(1) + MAL(2)
 2020 CONTINUE
      IL2 = IRA(JFS,JP)*DABS(DA1)*DEG2RD + LL1(JP) + XROUND
                    IF ( JFS .EQ . 1 )           GO TO 2030
      IL2 = IL2 + LA1(JP)
 2030 CONTINUE
      CALL  ADDCON  ( IP,MIA,IL1,AZ11,JP,IIA(JP),IL2,AZ12,NC )
C-----IF THE SECOND POINT OF INTERSECTION DOES NOT LIE ON THE LINE OR
C-----THE ARC THEN RETURN
      IF ( JTEST1.NE.0 . OR . JTEST2.NE.0 )      RETURN
 3010 CONTINUE
C-----ADD THE SECOND POINT OF INTERSECTION AS AN INTERSECTION CONFLICT
      IL1 = DSQRT((XINT2-MXL(IFS,IBAND))**2+(YINT2-MYL(IFS,IBAND))**2) +
     *      XROUND
                    IF ( IFS . EQ . 1 )          GO TO 3020
      IL1 = IL1 + MLL(1) + MAL(1) + MAL(2)
 3020 CONTINUE
      IL2 = IRA(JFS,JP)*DABS(DA2)*DEG2RD + LL1(JP) + XROUND
                    IF ( JFS .EQ . 1 )           GO TO 3030
      IL2 = IL2 + LA1(JP)
 3030 CONTINUE
      CALL  ADDCON  ( IP,MIA,IL1,AZ21,JP,IIA(JP),IL2,AZ22,NC )
      RETURN
 4010 CONTINUE
C-----COMBINE THE 2 POINTS OF INTERSECTION AND CHECK AGAIN
      XINT1 = 0.5D0*(XINT1+XINT2)
      YINT1 = 0.5D0*(YINT1+YINT2)
      NUMPTS = 1
      GO TO 1010
      END                                                               ADDLA
C
C
C
      FUNCTION   ICHKL  ( IX1,IY1,IX2,IY2,XINT,YINT )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  XINT,YINT
      INTEGER           IX1,IX2,IY1,IY2
      INTEGER           ICHKL
C
C-----FUNCTION ICHKL CHECKS TO SEE IF (XINT,YINT) LIES BETWEEN (IX1,IY1)
C-----AND (IX2,IY2)  (ICHKL=0=YES AND ICHKL=1=NO)
C
      ICHKL = 1
            IF ( (XINT-IX1)*(XINT-IX2).GT.ZERO ) RETURN
            IF ( (YINT-IY1)*(YINT-IY2).GT.ZERO ) RETURN
      ICHKL = 0
      RETURN
      END                                                               ICHKL
C
C
C
      FUNCTION   ICHKA  ( AZIM,NBA,NDA,DA )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  AZIM,BZIM,DA
      INTEGER           NBA,NDA
      INTEGER           ICHKA
C
C-----FUNCTION ICHKA CHECKS TO SEE IF AZIM LIES BETWEEN NBA AND NBA+NDA
C-----AND RETURNS DA
C
      ICHKA = 1
      DA = 0.0D0
      BZIM = AZIM
                    IF ( NDA )                   1010 , 9160 , 2010
 1010 CONTINUE
C-----NDA NEGATIVE
C-----IF BZIM IS VERY CLOSE TO NBA THEN RETURN WITH ICHKA=0
            IF ( DABS(BZIM-NBA) . LE . XROUND)   GO TO 3010
C-----MAKE BZIM LT NBA
                    IF ( BZIM.LT.DBLE(NBA) )     GO TO 1020
      BZIM = BZIM - 360.0D0
      GO TO 1010
 1020 CONTINUE
      DA = BZIM - NBA
C-----IF DA IS VERY CLOSE TO NDA THEN RETURN WITH ICHKA=0
                    IF ( DABS(DA-NDA).LE.XROUND )GO TO 3010
C-----IF DA IS GE NDA THEN RETURN WITH ICHKA=0
                    IF ( DA . GE . DBLE(NDA) )   GO TO 3010
      RETURN
 2010 CONTINUE
C-----NDA IS POSITIVE
C-----IF BZIM IS VERY CLOSE TO NBA THEN RETURN WITH ICHKA=0
            IF ( DABS(BZIM-NBA) . LE . XROUND)   GO TO 3010
C-----MAKE BZIM GT NBA
                    IF ( BZIM.GT.DBLE(NBA) )     GO TO 2020
      BZIM = BZIM + 360.0D0
      GO TO 2010
 2020 CONTINUE
      DA = BZIM - NBA
C-----IF DA IS VERY CLOSE TO NDA THEN RETURN WITH ICHKA=0
                    IF ( DABS(DA-NDA).LE.XROUND )GO TO 3010
C-----IF DA LE NDA THEN RETURN WITH ICHKA=0
                    IF ( DA . LE . DBLE(NDA) )   GO TO 3010
      RETURN
 3010 CONTINUE
      ICHKA = 0
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9160 CONTINUE
      CALL  ABORTR  ( 'STOP 916 - ' //
     *                'SWEEP ANGLE FOR ARC PORTION OF PATH IS EQ 0'
     *                // ' - ' //
     *                'ICHKA'                                       )
      STOP  916
      END                                                               ICHKA
C
C
C
      SUBROUTINE CATOLC ( IFS,IBAND,JFS,NC )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'PATH'
      INCLUDE 'GEOCP'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  A,B,C,RADICL,X,XB,XM
      INTEGER           IBAND,IFS,JFS,NC
C
C-----SUBROUTINE CATOLC CHECKS FOR INTERSECTION CONFLICTS BETWEEN THE
C-----ARC PORTION OF THE INTERSECTION PATH BEING CHECKED AND THE LINE
C-----PORTION OF THE INTERSECTION PATH BEING CHECKED AGAINST
C
C-----IF THE LINE IS VERTICAL THEN GO TO 1050
      IF ( IABS(JXL(JFS,JP)-IXL(JFS,JP)).LE.0 )  GO TO 1050
C-----FIND THE SLOPE AND THE Y INTERCEPT OF THE LINE
      XM = DBLE ( JYL(JFS,JP) - IYL(JFS,JP) ) /
     *     DBLE ( JXL(JFS,JP) - IXL(JFS,JP) )
      XB = IYL(JFS,JP) - IXL(JFS,JP)*XM
C-----FIND THE POINT(S) OF INTERSECTION BETWEEN THE ARC AND THE LINE
      A = 1.0D0 + XM**2
      B = - 2.0D0*MXA(IFS,IBAND) + 2.0D0*XM*XB
     *    - 2.0D0*MYA(IFS,IBAND)*XM
      C = MXA(IFS,IBAND)**2 + MYA(IFS,IBAND)**2 + XB**2 -
     *    MRA(IFS,IBAND)**2 - 2.0D0*MYA(IFS,IBAND)*XB
      RADICL = B**2 - 4.0D0*A*C
                    IF ( DABS(RADICL).LE.ZERO )  GO TO 1010
                    IF ( RADICL )                2010 , 1010 , 1030
 1010 CONTINUE
C-----FIND 1 POINT OF INTERSECTION BETWEEN THE ARC AND THE LINE
      XINT1 = -B/(2.0D0*A)
      YINT1 = XM*XINT1 + XB
 1020 CONTINUE
C-----ADD 1 POINT OF INTERSECTION BETWEEN THE ARC AND THE LINE
      CALL  ADDAL   ( IFS,IBAND,JFS,NC,1 )
      RETURN
 1030 CONTINUE
C-----FIND 2 POINTS OF INTERSECTION BETWEEN THE ARC AND THE LINE
      XINT1 = (-B+DSQRT(RADICL))/(2.0D0*A)
      YINT1 = XM*XINT1 + XB
      XINT2 = (-B-DSQRT(RADICL))/(2.0D0*A)
      YINT2 = XM*XINT2 + XB
 1040 CONTINUE
C-----ADD 2 POINTS OF INTERSECTION BETWEEN THE ARC AND THE LINE
      CALL  ADDAL   ( IFS,IBAND,JFS,NC,2 )
      RETURN
 1050 CONTINUE
C-----FIND THE INTERSECTION BETWEEN THE ARC AND THE VERTICAL LINE
      X = 0.5D0*(IXL(JFS,JP)+JXL(JFS,JP))
      A = 1.0D0
      B = -2.0D0*MYA(IFS,IBAND)
      C = MYA(IFS,IBAND)**2 + (X-MXA(IFS,IBAND))**2 - MRA(IFS,IBAND)**2
      RADICL = B**2 - 4.0D0*A*C
                    IF ( DABS(RADICL).LE.ZERO )  GO TO 1060
                    IF ( RADICL )                2010 , 1060 , 1070
 1060 CONTINUE
C-----FIND 1 POINT OF INTERSECTION BETWEEN THE ARC AND THE VERTICAL LINE
      XINT1 = X
      YINT1 = -B/(2.0D0*A)
      GO TO 1020
 1070 CONTINUE
C-----FIND 2 POINTS OF INTERSECTION BETWEEN THE ARC AND THE VERTICAL
C-----LINE
      XINT1 = X
      YINT1 = (-B+DSQRT(RADICL))/(2.0D0*A)
      XINT2 = X
      YINT2 = (-B-DSQRT(RADICL))/(2.0D0*A)
      GO TO 1040
 2010 CONTINUE
      RETURN
      END                                                               CATOLC
C
C
C
      SUBROUTINE ADDAL  ( IFS,IBAND,JFS,NC,NUM )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'PATH'
      INCLUDE 'GEOCP'
      INCLUDE 'GEOVAL'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  AZIM1,AZIM2,AZ11,AZ12,AZ21,AZ22,BEARX,BEARY,DA1,
     *                  DA2,X,XBEAR,YBEAR
      INTEGER           IBAND,IFS,IL1,IL2,ITEST1,ITEST2,JFS,JTEST1,
     *                  JTEST2,NC,NUM,NUMPTS
      DOUBLE PRECISION  AZIM36
      INTEGER           ICHKA,ICHKL
C
C-----SUBROUTINE ADDAL ADDS INTERSECTION CONFLICTS BETWEEN THE ARC
C-----PORTION OF THE INTERSECTION PATH BEING CHECKED AND THE LINE
C-----PORTION OF THE INTERSECTION PATH BEING CHECKED AGAINST
C
      NUMPTS = NUM
 1010 CONTINUE
C-----CHECK IF THE FIRST POINT OF INTERSECTION LIES ON THE ARC
      BEARX = JXL(JFS,JP) - IXL(JFS,JP)
      BEARY = JYL(JFS,JP) - IYL(JFS,JP)
      AZ12 = AZIM36( BEARY,BEARX )
      XBEAR = XINT1 - MXA(IFS,IBAND)
      YBEAR = YINT1 - MYA(IFS,IBAND)
      AZIM1 = AZIM36( YBEAR,XBEAR )
      AZ11 = AZIM1 + ISIGN( 90,MDA(IFS,IBAND) )
      ITEST1 = ICHKA( AZIM1,MBA(IFS,IBAND),MDA(IFS,IBAND),DA1 )
C-----CHECK IF THE FIRST POINT OF INTERSECTION LIES ON THE LINE
      ITEST2 = ICHKL( IXL(JFS,JP),IYL(JFS,JP),JXL(JFS,JP),JYL(JFS,JP),
     *                XINT1,YINT1 )
      JTEST1 = 1
      JTEST2 = 1
                    IF ( NUMPTS . EQ . 1 )       GO TO 1020
C-----CHECK IF THE SECOND POINT OF INTERSECTION LIES ON THE ARC
      BEARX = JXL(JFS,JP) - IXL(JFS,JP)
      BEARY = JYL(JFS,JP) - IYL(JFS,JP)
      AZ22 = AZIM36( BEARY,BEARX )
      XBEAR = XINT2 - MXA(IFS,IBAND)
      YBEAR = YINT2 - MYA(IFS,IBAND)
      AZIM2 = AZIM36( YBEAR,XBEAR )
      AZ21 = AZIM2 + ISIGN( 90,MDA(IFS,IBAND) )
      JTEST1 = ICHKA( AZIM2,MBA(IFS,IBAND),MDA(IFS,IBAND),DA2 )
C-----CHECK IF THE SECOND POINT OF INTERSECTION LIES ON THE LINE
      JTEST2 = ICHKL( IXL(JFS,JP),IYL(JFS,JP),JXL(JFS,JP),JYL(JFS,JP),
     *                XINT2,YINT2 )
 1020 CONTINUE
C-----IF NEITHER POINT OF INTERSECTION LIES ON BOTH THE ARC AND THE LINE
C-----THEN RETURN
      IF ( (ITEST1.NE.0.OR .ITEST2.NE.0) . AND .
     *     (JTEST1.NE.0.OR .JTEST2.NE.0) )       RETURN
C-----IF ONLY THE FIRST POINT OF INTERSECTION LIES ON BOTH THE ARC AND
C-----THE LINE THEN ADD THE FIRST POINT OF INTERSECTION
      IF ( (ITEST1.EQ.0.AND.ITEST2.EQ.0) . AND .
     *     (JTEST1.NE.0.OR .JTEST2.NE.0) )       GO TO 2010
C-----IF ONLY THE SECOND POINT OF INTERSECTION LIES ON BOTH THE ARC AND
C-----THE LINE THEN ADD THE SECOND POINT OF INTERSECTION
      IF ( (ITEST1.NE.0.OR .ITEST2.NE.0) . AND .
     *     (JTEST1.EQ.0.AND.JTEST2.EQ.0) )       GO TO 3010
C-----IF THIS IS NOT THE MAIN INTERSECTION PATH THEN GO TO 4010
                    IF ( IBAND . NE . 1 )        GO TO 4010
C-----IF THE DISTANCE BETWEEN THE 2 POINTS OF CONFLICT ON THE MAIN
C-----INTERSECTION PATH IS LE ICLOSE THEN GO TO 4010
      X = DSQRT((XINT1-XINT2)**2+(YINT1-YINT2)**2)
                    IF ( X.LE.DBLE(ICLOSE) )     GO TO 4010
 2010 CONTINUE
C-----ADD FIRST POINT OF INTERSECTION AS AN INTERSECTION CONFLICT
      IL1 = MRA(IFS,1)*DABS(DA1)*DEG2RD + MLL(1) + XROUND
                    IF ( IFS .EQ . 1 )           GO TO 2020
      IL1 = IL1 + MAL(1)
 2020 CONTINUE
      IL2 = DSQRT((XINT1-IXL(JFS,JP))**2+(YINT1-IYL(JFS,JP))**2)+XROUND
                    IF ( JFS . EQ . 1 )          GO TO 2030
      IL2 = IL2 + LL1(JP) + LA1(JP) + LA2(JP)
 2030 CONTINUE
C-----IF THE SECOND POINT OF INTERSECTION DOES NOT LIE ON THE ARC OR
C-----THE LINE THEN RETURN
      CALL  ADDCON  ( IP,MIA,IL1,AZ11,JP,IIA(JP),IL2,AZ12,NC )
      IF ( JTEST1.NE.0 . OR . JTEST2.NE.0 )      RETURN
 3010 CONTINUE
C-----ADD THE SECOND POINT OF INTERSECTION AS AN INTERSECTION CONFLICT
      IL1 = MRA(IFS,1)*DABS(DA2)*DEG2RD + MLL(1) + XROUND
                    IF ( IFS . EQ . 1 )          GO TO 3020
      IL1 = IL1 + MAL(1)
 3020 CONTINUE
      IL2 = DSQRT((XINT2-IXL(JFS,JP))**2+(YINT2-IYL(JFS,JP))**2)+XROUND
                    IF ( JFS . EQ . 1 )          GO TO 3030
      IL2 = IL2 + LL1(JP) + LA1(JP) + LA2(JP)
 3030 CONTINUE
      CALL  ADDCON  ( IP,MIA,IL1,AZ21,JP,IIA(JP),IL2,AZ22,NC )
      RETURN
 4010 CONTINUE
C-----COMBINE THE 2 POINTS OF INTERSECTION AND CHECK AGAIN
      XINT1 = 0.5D0*(XINT1+XINT2)
      YINT1 = 0.5D0*(YINT1+YINT2)
      NUMPTS = 1
      GO TO 1010
      END                                                               ADDAL
C
C
C
      SUBROUTINE CATOAC ( IFS,IBAND,JFS,NC )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'PATH'
      INCLUDE 'GEOCP'
      INCLUDE 'RADIAN'
      DIMENSION         IAZIMS(4)
      INTEGER           IAZIMS
      DOUBLE PRECISION  A,ANGRAD,AZIM,B,C,DUMMY,RADICL,R1,R1SQ,R2,R2SQ,
     *                  X1,X2,X2X1SQ,Y1,Y1SQ,Y2,Y2SQ,Y2Y1SQ
      INTEGER           I,IAZIM,IBAND,IFS,ITEST,JFLAG,JFS,NAZIMS,NC
      INTEGER           BND360,ICHKA
      DOUBLE PRECISION  XVAL
C
C-----SUBROUTINE CATOAC CHECKS FOR CONFLICTS BETWEEN THE ARC PORTION OF
C-----THE INTERSECTION PATH BEING CHECKED AND THE ARC PORTION OF THE
C-----INTERSECTION PATH BEING CHECKED AGAINST
C
      R1 = MRA(IFS,IBAND)
                    IF ( R1 . LE . ZERO )        RETURN
      R2 = IRA(JFS,JP)
                    IF ( R2 . LE . ZERO )        RETURN
      X1 = MXA(IFS,IBAND)
      X2 = IXA(JFS,JP)
      Y1 = MYA(IFS,IBAND)
      Y2 = IYA(JFS,JP)
      IF ( MXA(IFS,IBAND) . EQ . IXA(JFS,JP) . AND .
     *     MYA(IFS,IBAND) . EQ . IYA(JFS,JP) )   GO TO 4010
      X2X1SQ = (X2-X1)**2
      Y2Y1SQ = (Y2-Y1)**2
      Y1SQ = Y1**2
      Y2SQ = Y2**2
      R1SQ = R1**2
      R2SQ = R2**2
C-----CALCULATE THE POINT(S) OF INTERSECTION OF THE TWO ARCS
      A = 4.0D0*(X2X1SQ+Y2Y1SQ)
      B = 4.0D0*(Y2-Y1)*(R2SQ-R1SQ+Y1SQ-Y2SQ) -
     *    4.0D0*X2X1SQ*(Y1+Y2)
      C = ((R2SQ-R1SQ)-(Y2SQ-Y1SQ))**2 + X2X1SQ*
     *     (-2.0D0*R2SQ-2.0D0*R1SQ+2.0D0*Y1SQ+2.0D0*Y2SQ+X2X1SQ)
                    IF ( A . EQ . 0.0D0 )        GO TO 4010
      RADICL = B**2 - 4.0D0*A*C
                    IF ( DABS(RADICL).LE.ZERO )  GO TO 1010
                    IF ( RADICL )                5010 , 1010 , 2010
 1010 CONTINUE
C-----ONE Y COORDINATE FOR THE POINT(S) OF INTERSECTION
      YINT1 = -B/(2.0D0*A)
      YINT2 = YINT1
      RADICL = R1SQ - (YINT1-Y1)**2
                    IF ( DABS(RADICL).LE.ZERO )  GO TO 1020
                    IF ( RADICL )                5010 , 1020 , 1040
 1020 CONTINUE
C-----ONE X COORDINATE FOR ONE Y COORDINATE FOR THE POINT OF
C-----INTERSECTION
      XINT1 = X1
 1030 CONTINUE
C-----ADD 1 POINT OF INTERSECTION AS AN INTERSECTION CONFLICT
      CALL  ADDAA   ( IFS,IBAND,JFS,NC,1 )
      RETURN
 1040 CONTINUE
C-----POSSIBLY TWO X COORDINATES FOR ONE Y COORDINATE FOR THE POINTS OF
C-----INTERSECTION
      XINT1 = XVAL( X1,Y1,R1,X2,Y2,R2,RADICL,YINT1,+1,JFLAG )
                    IF ( JFLAG . NE . 0 )        GO TO 1050
      XINT2 = XVAL( X1,Y1,R1,X2,Y2,R2,RADICL,YINT2,-1,JFLAG )
                    IF ( JFLAG . NE . 0 )        GO TO 1030
            IF ( DABS(XINT1-XINT2) . LE . ZERO ) GO TO 1030
      GO TO 3010
 1050 CONTINUE
C-----THE FIRST X COORDINATE DOES NOT LIE ON EITHER ARC OF A CIRCLE THUS
C-----CHECK THE SECOND X COORDINATE
      XINT1 = XVAL( X1,Y1,R1,X2,Y2,R2,RADICL,YINT1,-1,JFLAG )
                    IF ( JFLAG . NE . 0 )        GO TO 5010
      GO TO 1030
 2010 CONTINUE
C-----TWO Y COORDINATES FOR THE POINT(S) OF INTERSECTION
      YINT1 = (-B+DSQRT(RADICL))/(2.0D0*A)
      YINT2 = (-B-DSQRT(RADICL))/(2.0D0*A)
      RADICL = R1SQ - (YINT1-Y1)**2
                    IF ( DABS(RADICL).LE.ZERO )  RADICL = 0.0D0
                    IF ( RADICL . LT . 0.0D0 )   GO TO 5010
C-----FIRST X COORDINATE FOR TWO Y COORDINATES FOR THE POINTS OF
C-----INTERSECTION
      XINT1 = XVAL( X1,Y1,R1,X2,Y2,R2,RADICL,YINT1,+1,JFLAG )
                    IF ( JFLAG . NE . 0 )        GO TO 5010
      RADICL = R1SQ - (YINT2-Y1)**2
                    IF ( DABS(RADICL).LE.ZERO )  RADICL = 0.0D0
                    IF ( RADICL . LT . 0.0D0 )   GO TO 5010
C-----SECOND X COORDINATE FOR TWO Y COORDINATES FOR THE POINTS OF
C-----INTERSECTION
      XINT2 = XVAL( X1,Y1,R1,X2,Y2,R2,RADICL,YINT2,+1,JFLAG )
                    IF ( JFLAG . NE . 0 )        GO TO 5010
 3010 CONTINUE
C-----ADD TWO POINTS OF INTERSECTION AS INTERSECTION CONFLICTS
      CALL  ADDAA   ( IFS,IBAND,JFS,NC,2 )
      RETURN
 4010 CONTINUE
C-----BOTH OF THE ARCS HAVE THE SAME CENTER COORDINATES
                    IF ( DABS(R1-R2).GT.ZERO )   GO TO 5010
c     GO TO 9170
C-----BOTH OF THE ARCS HAVE THE SAME RADIUS
      NAZIMS = 0
C-----IF THE BEG AZIMUTH POINT FOR THE ARC PORTION OF THE INTERSECTION
C-----PATH BEING CHECKED         LIES ON THE ARC PORTION OF THE
C-----INTERSECTION PATH BEING CHECK AGAINST THEN ADD THE AZIMUTH POINT
      IAZIM = BND360( MBA(IFS,IBAND)                )
      AZIM  = IAZIM
      ITEST = ICHKA( AZIM,IBA(JFS,JP)   ,IDA(JFS,JP)   ,DUMMY )
      IF ( ITEST . EQ . 0 )                      THEN
        CALL  ADDAZM  ( IAZIM,IAZIMS,NAZIMS )
      END IF
C-----IF THE END AZIMUTH POINT FOR THE ARC PORTION OF THE INTERSECTION
C-----PATH BEING CHECKED         LIES ON THE ARC PORTION OF THE
C-----INTERSECTION PATH BEING CHECK AGAINST THEN ADD THE AZIMUTH POINT
      IAZIM = BND360( MBA(IFS,IBAND)+MDA(IFS,IBAND) )
      AZIM  = IAZIM
      ITEST = ICHKA( AZIM,IBA(JFS,JP)   ,IDA(JFS,JP)   ,DUMMY )
      IF ( ITEST . EQ . 0 )                      THEN
        CALL  ADDAZM  ( IAZIM,IAZIMS,NAZIMS )
      END IF
C-----IF THE BEG AZIMUTH POINT FOR THE ARC PORTION OF THE INTERSECTION
C-----PATH BEING CHECKED AGAINST LIES ON THE ARC PORTION OF THE
C-----INTERSECTION PATH BEING CHECK         THEN ADD THE AZIMUTH POINT
      IAZIM = BND360( IBA(JFS,JP)                   )
      AZIM  = IAZIM
      ITEST = ICHKA( AZIM,MBA(IFS,IBAND),MDA(IFS,IBAND),DUMMY )
      IF ( ITEST . EQ . 0 )                      THEN
        CALL  ADDAZM  ( IAZIM,IAZIMS,NAZIMS )
      END IF
C-----IF THE END AZIMUTH POINT FOR THE ARC PORTION OF THE INTERSECTION
C-----PATH BEING CHECKED AGAINST LIES ON THE ARC PORTION OF THE
C-----INTERSECTION PATH BEING CHECK         THEN ADD THE AZIMUTH POINT
      IAZIM = BND360( IBA(JFS,JP)   +IDA(JFS,JP)    )
      AZIM  = IAZIM
      ITEST = ICHKA( AZIM,MBA(IFS,IBAND),MDA(IFS,IBAND),DUMMY )
      IF ( ITEST . EQ . 0 )                      THEN
        CALL  ADDAZM  ( IAZIM,IAZIMS,NAZIMS )
      END IF
                    IF ( NAZIMS . EQ . 0 )       GO TO 5010
C-----ADD EACH AZIMUTH POINT AS AN INTERSECTION CONFLICT
      DO 4020  I = 1 , NAZIMS
      IAZIM = IAZIMS(I)
      ANGRAD = DEG2RD*(90-IAZIM)
      XINT1 = X1 + R1*DCOS(ANGRAD)
      YINT1 = Y1 + R1*DSIN(ANGRAD)
      XINT2 = X2 + R2*DCOS(ANGRAD)
      YINT2 = Y2 + R2*DSIN(ANGRAD)
      XINT1 = 0.5D0*(XINT1+XINT2)
      YINT1 = 0.5D0*(YINT1+YINT2)
      CALL  ADDAA   ( IFS,IBAND,JFS,NC,1 )
 4020 CONTINUE
 5010 CONTINUE
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
C9170 CONTINUE
C     CALL  ABORTR  ( 'STOP 917 - CIRCLES ARE IDENTICAL - CATOAC' )
C     STOP  917
      END                                                               CATOAC
C
C
C
      SUBROUTINE ADDAZM ( IAZIM,IAZIMS,NAZIMS )
      IMPLICIT NONE                                                     CCODE=C.
      DIMENSION         IAZIMS(4)
      INTEGER           IAZIMS
      INTEGER           I,IAZIM,NAZIMS
C
C-----SUBROUTINE ADDAZM ADD THE AZIMUTH IAZIM TO THE LIST IAZIMS
C
                    IF ( NAZIMS . EQ . 0 )       GO TO 2010
      DO 1010  I = 1 , NAZIMS
                    IF ( IAZIM . EQ . IAZIMS(I) )RETURN
 1010 CONTINUE
 2010 CONTINUE
      NAZIMS = NAZIMS + 1
      IAZIMS(NAZIMS) = IAZIM
      RETURN
      END                                                               ADDAZM
C
C
C
      FUNCTION   XVAL   ( X1,Y1,R1,X2,Y2,R2,RADICL,YVAL,IISIGN,JFLAG )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  RA,RADICL,RB,R1,R2,X1,X2,YVAL,Y1,Y2
      INTEGER           IISIGN,JFLAG
      DOUBLE PRECISION  XVAL
C
C-----FUNCTION XVAL FINDS THE X COORDINATE OF THE INTERSECTION OF TWO
C-----ARCS FOR A GIVEN YVAL COORDINATE (JFLAG=0=OK AND JFLAG=1=NOT ON
C-----EITHER ARC OF CIRCLE
C
      JFLAG = 1
C-----FIND ONE OF THE VALUES FOR XVAL AND CHECK IF IT IS ON BOTH ARCS
      XVAL = X1 + IISIGN*DSQRT(RADICL)
C-----IF THE DISTANCE FROM (XVAL,YVAL) TO (X1,Y1) IS NOT R1 THEN XVAL IS
C-----NOT ON ARC 1 AND THE OTHER VALUE FOR XVAL SHOULD BE USED
      RA = DSQRT((XVAL-X1)**2+(YVAL-Y1)**2)
                    IF ( DABS(RA-R1).GT.ZERO )   GO TO 1010
C-----IF THE DISTANCE FROM (XVAL,YVAL) TO (X2,Y2) IS NOT R2 THEN XVAL IS
C-----NOT ON ARC 2 AND THE OTHER VALUE FOR XVAL SHOULD BE USED
      RB = DSQRT((XVAL-X2)**2+(YVAL-Y2)**2)
                    IF ( DABS(RB-R2).GT.ZERO )   GO TO 1010
      JFLAG = 0
      RETURN
 1010 CONTINUE
C-----FIND THE OTHER VALUE FOR XVAL AND CHECK IF IT IS ON BOTH ARCS
      XVAL = X1 - IISIGN*DSQRT(RADICL)
C-----IF THE DISTANCE FROM (XVAL,YVAL) TO (X1,Y1) IS NOT R1 THEN XVAL IS
C-----NOT ON ARC 1 THUS RETURN (JFLAG=1)
      RA = DSQRT((XVAL-X1)**2+(YVAL-Y1)**2)
                    IF ( DABS(RA-R1).GT.ZERO )   RETURN
C-----IF THE DISTANCE FROM (XVAL,YVAL) TO (X2,Y2) IS NOT R2 THEN XVAL IS
C-----NOT ON ARC 2 THUS RETURN (JFLAG=1)
      RB = DSQRT((XVAL-X2)**2+(YVAL-Y2)**2)
                    IF ( DABS(RB-R2).GT.ZERO )   RETURN
      JFLAG = 0
      RETURN
      END                                                               XVAL
C
C
C
      FUNCTION   BND360 ( IANGLE )
      IMPLICIT NONE                                                     CCODE=C.
      INTEGER           BND360,IANGLE
C
C-----FUNCTION BND360 BOUNDS IANGLE BETWEEN 0 AND 360
C
      BND360 = IANGLE
 1010 CONTINUE
      IF ( BND360 . LT .   0 )                   THEN
        BND360 = BND360 + 360
        GO TO 1010
      END IF
 2010 CONTINUE
      IF ( BND360 . GE . 360 )                   THEN
        BND360 = BND360 - 360
        GO TO 2010
      END IF
      RETURN
      END                                                               BND360
C
C
C
      SUBROUTINE ADDAA  ( IFS,IBAND,JFS,NC,NUM )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'PATH'
      INCLUDE 'GEOCP'
      INCLUDE 'GEOVAL'
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  AZIM11,AZIM12,AZIM21,AZIM22,AZ11,AZ12,AZ21,AZ22,
     *                  DA11,DA12,DA21,DA22,X,XBEAR1,XBEAR2,YBEAR1,
     *                  YBEAR2
      INTEGER           IBAND,IFS,IL1,IL2,ITEST1,ITEST2,JFS,JTEST1,
     *                  JTEST2,NC,NUM,NUMPTS
      DOUBLE PRECISION  AZIM36
      INTEGER           ICHKA
C
C-----SUBROUTINE ADDAA ADDS INTERSECTION CONFLICTS BETWEEN THE ARC
C-----PORTION OF THE INTERSECTION PATH BEING CHECKED AND THE ARC PORTION
C-----OF THE INTERSECTION PATH BEING CHECKED AGAINST
C
      NUMPTS = NUM
 1010 CONTINUE
C-----CHECK IF THE FIRST POINT OF INTERSECTION LIES ON THE ARC PORTION
C-----OF THE INTERSECTION PATH BEING CHECKED
      XBEAR1 = XINT1 - MXA(IFS,IBAND)
      YBEAR1 = YINT1 - MYA(IFS,IBAND)
      AZIM11 = AZIM36( YBEAR1,XBEAR1 )
      AZ11 = AZIM11 + ISIGN( 90,MDA(IFS,IBAND) )
      ITEST1 = ICHKA( AZIM11,MBA(IFS,IBAND),MDA(IFS,IBAND),DA11 )
C-----CHECK IF THE FIRST POINT OF INTERSECTION LIES ON THE ARC PORTION
C-----OF THE INTERSECTION PATH BEING CHECKED AGAINST
      XBEAR2 = XINT1 - IXA(JFS,JP)
      YBEAR2 = YINT1 - IYA(JFS,JP)
      AZIM12 = AZIM36( YBEAR2,XBEAR2 )
      AZ12 = AZIM12 + ISIGN( 90,IDA(JFS,JP) )
      ITEST2 = ICHKA( AZIM12,IBA(JFS,JP),IDA(JFS,JP),DA12 )
      JTEST1 = 1
      JTEST2 = 1
                    IF ( NUMPTS . EQ . 1 )       GO TO 1020
C-----CHECK IF THE SECOND POINT OF INTERSECTION LIES ON THE ARC PORTION
C-----OF THE INTERSECTION PATH BEING CHECKED
      XBEAR1 = XINT2 - MXA(IFS,IBAND)
      YBEAR1 = YINT2 - MYA(IFS,IBAND)
      AZIM21 = AZIM36( YBEAR1,XBEAR1 )
      AZ21 = AZIM21 + ISIGN( 90,MDA(IFS,IBAND) )
      JTEST1 = ICHKA( AZIM21,MBA(IFS,IBAND),MDA(IFS,IBAND),DA21 )
C-----CHECK IF THE SECOND POINT OF INTERSECTION LIES ON THE ARC PORTION
C-----OF THE INTERSECTION PATH BEING CHECKED AGAINST
      XBEAR2 = XINT2 - IXA(JFS,JP)
      YBEAR2 = YINT2 - IYA(JFS,JP)
      AZIM22 = AZIM36( YBEAR2,XBEAR2 )
      AZ22 = AZIM22 + ISIGN( 90,IDA(JFS,JP) )
      JTEST2 = ICHKA( AZIM22,IBA(JFS,JP),IDA(JFS,JP),DA22 )
 1020 CONTINUE
C-----IF NEITHER POINT OF INTERSECTION LIES ON BOTH THE ARC PORTION OF
C-----THE INTERSECTION PATH BEING CHECKED AND THE ARC PORTION OF THE
C-----INTERSECTION PATH BEING CHECKED AGAINST THEN RETURN
      IF ( (ITEST1.NE.0.OR .ITEST2.NE.0) . AND .
     *     (JTEST1.NE.0.OR .JTEST2.NE.0) )       RETURN
C-----IF ONLY THE FIRST POINT OF INTERSECTION LIES ON BOTH THE ARC
C-----PORTION OF THE INTERSECTION PATH BEING CHECKED AND THE ARC PORTION
C-----OF THE INTERSECTION PATH BEING CHECKED AGAINST THEN ADD THE FIRST
C-----POINT OF INTERSECTION
      IF ( (ITEST1.EQ.0.AND.ITEST2.EQ.0) . AND .
     *     (JTEST1.NE.0.OR .JTEST2.NE.0) )       GO TO 2010
C-----IF ONLY THE SECOND POINT OF INTERSECTION LIES ON BOTH THE ARC
C-----PORTION OF THE INTERSECTION PATH BEING CHECKED AND THE ARC PORTION
C-----OF THE INTERSECTION PATH BEING CHECKED AGAINST THEN ADD THE SECOND
C-----POINT OF INTERSECTION
      IF ( (ITEST1.NE.0.OR .ITEST2.NE.0) . AND .
     *     (JTEST1.EQ.0.AND.JTEST2.EQ.0) )       GO TO 3010
C-----IF THIS IS NOT THE MAIN INTERSECTION PATH THEN GO TO 4010
                    IF ( IBAND . NE . 1 )        GO TO 4010
C-----IF THE DISTANCE BETWEEN THE 2 POINTS OF CONFLICT ON THE MAIN
C-----INTERSECTION PATH IS LE ICLOSE THEN GO TO 4010
      X = DSQRT((XINT1-XINT2)**2+(YINT1-YINT2)**2)
                    IF ( X.LE.DBLE(ICLOSE) )     GO TO 4010
 2010 CONTINUE
C-----ADD FIRST POINT OF INTERSECTION AS AN INTERSECTION CONFLICT
      IL1 = MRA(IFS,1)*DABS(DA11)*DEG2RD + MLL(1) + XROUND
                    IF ( IFS . EQ . 1 )          GO TO 2020
      IL1 = IL1 + MAL(1)
 2020 CONTINUE
      IL2 = IRA(JFS,JP)*DABS(DA12)*DEG2RD + LL1(JP) + XROUND
                    IF ( JFS .EQ. 1 )            GO TO 2030
      IL2 = IL2 + LA1(JP)
 2030 CONTINUE
      CALL  ADDCON  ( IP,MIA,IL1,AZ11,JP,IIA(JP),IL2,AZ12,NC )
C-----IF THE SECOND POINT OF INTERSECTION DOES NOT LIE ON THE ARC
C-----PORTION OF THE INTERSECTION PATH BEING CHECKED OR THE ARC PORTION
C-----OF THE INTERSECTION PATH BEING CHECKED AGAINST THEN RETURN
      IF ( JTEST1.NE.0 . OR . JTEST2.NE.0 )      RETURN
 3010 CONTINUE
C-----ADD THE SECOND POINT OF INTERSECTION AS AN INTERSECTION CONFLICT
      IL1 = MRA(IFS,1)*DABS(DA21)*DEG2RD + MLL(1) + XROUND
                    IF ( IFS . EQ . 1 )          GO TO 3020
      IL1 = IL1 + MAL(1)
 3020 CONTINUE
      IL2 = IRA(JFS,JP)*DABS(DA22)*DEG2RD + LL1(JP) + XROUND
                    IF ( JFS . EQ . 1 )          GO TO 3030
      IL2 = IL2 + LA1(JP)
 3030 CONTINUE
      CALL  ADDCON  ( IP,MIA,IL1,AZ21,JP,IIA(JP),IL2,AZ22,NC )
      RETURN
 4010 CONTINUE
C-----COMBINE THE 2 POINTS OF INTERSECTION AND CHECK AGAIN
      XINT1 = 0.5D0*(XINT1+XINT2)
      YINT1 = 0.5D0*(YINT1+YINT2)
      NUMPTS = 1
      GO TO 1010
      END                                                               ADDAA
C
C
C
      SUBROUTINE SRTCON
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONFLT'
      INCLUDE 'PATH'
      INCLUDE 'GEOCOM'
      DIMENSION         IDIST(NCP)
      INTEGER           IDIST
      INTEGER           I,ICON,ITEMP,J,JCON,KP,KPN
C
C-----SUBROUTINE SRTCON SORTS THE INTERSECTION CONFLICTS FOR EACH
C-----INTERSECTION PATH BY THE DISTANCE DOWN THE INTERSECTION PATH TO
C-----THE INTERSECTION CONFLICT
C
C-----PROCESS EACH INTERSECTION PATH
      DO 3020  KP = 1 , NPATHS
                    IF ( NGEOCP(KP) . LE . 1 )   GO TO 3020
C-----FIND THE DISTANCE DOWN THE INTERSECTION PATH TO EACH INTERSECTION
C-----CONFLICT AND TEMPORARILY STORE IN ARRAY IDIST
      DO 1010  ICON = 1 , NGEOCP(KP)
      JCON = IGEOCP(ICON,KP)
      KPN = 1
            IF ( ICONP(2,JCON) . EQ . KP )       KPN = 2
      IDIST(ICON) = ICOND(KPN,JCON)
 1010 CONTINUE
C-----SORT THE DISTANCE DOWN THE INTERSECTION PATH TO THE INTERSECTION
C-----CONFLICT (IDIST) AND CARRY ARRAY IGEOCP FROM ENTRY KP OF ENTITY
C-----PATH USING A BUBBLE SORT
C-----SET THE SORT INDEX TO THE SECOND ELEMENT IN THE LIST
      I = 2
 2010 CONTINUE
C-----IF THE SORT INDEX IS GT THE NUMBER IN THE LIST THEN THE SORT IS
C-----FINISHED
                    IF ( I . GT . NGEOCP(KP) )   GO TO 3020
C-----IF THE ELEMENT IS OUT OF ORDER THEN BUBBLE IT UP TO ITS PROPER
C-----POSITION IN THE LIST
            IF ( IDIST(I) . LT . IDIST(I-1) )    GO TO 2020
C-----CHECK THE NEXT ELEMENT DOWN THE LIST
      I = I + 1
      GO TO 2010
 2020 CONTINUE
C-----SAVE THE INDEX OF THE NEXT ELEMENT TO BE CHECKED AFTER THIS
C-----ELEMENT HAS BEEN BUBBLED TO ITS PROPER POSITION IN THE LIST
      J = I + 1
 2030 CONTINUE
C-----SWAP ELEMENT I AND ELEMENT I-1 OF ARRAY IDIST AND IGEOCP
      ITEMP          = IGEOCP(I-1,KP)
      IGEOCP(I-1,KP) = IGEOCP(I,KP)
      IGEOCP(I,KP)   = ITEMP
      ITEMP          = IDIST(I-1)
      IDIST(I-1)     = IDIST(I)
      IDIST(I)       = ITEMP
C-----CHECK NEXT ELEMENT ABOVE TO SEE IF THE ELEMENT HAS BEEN BUBBLED TO
C-----ITS PROPER POSITION IN THE LIST
      I = I - 1
C-----IF THE START OF THE LIST HAS BEEN REACHED THEN END BUBBLING THIS
C-----ELEMENT
                    IF ( I . EQ . 1 )            GO TO 2040
C-----IF THE ELEMENT IS STILL NOT IN ITS PROPER POSITION IN THE LIST
C-----THEN SWAP THE ELEMENTS AND CHECK AGAIN
            IF ( IDIST(I) . LT . IDIST(I-1))     GO TO 2030
 2040 CONTINUE
C-----SET THE INDEX TO THE NEXT ELEMENT TO BE CHECKED AND START CHECKING
C-----DOWN THE LIST AGAIN
      I = J
      GO TO 2010
C-----END OF INTERSECTION PATH LOOP
 3020 CONTINUE
      RETURN
      END                                                               SRTCON
C
C
C
      SUBROUTINE WRITPA
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'PATH'
      INCLUDE 'GEOCOM'
      INCLUDE 'OUTPT'
      CHARACTER*4       AVTYPS
      CHARACTER*8       ITRTYP
      INTEGER           J,KP,LTEST
  601 FORMAT(8X,5HTABLE,I3,21H  -  LISTING OF PATHS,//)
  602 FORMAT(20I4)
  603 FORMAT(12X,4HPATH,I4,19H GOES FROM APPROACH,I3,5H LANE,I2,
     *       12H TO APPROACH,I3,5H LANE,I2,/,
     *       15X,16HLENGTH OF PATH =,I4,25H FEET AND SPEED OF PATH =,I3,
     *       16H FEET PER SECOND,/,
     *       15X,42HVEHICLE TYPES ALLOWED TO USE THIS PATH = (,A,1H),/,
     *       15X,21HNUMBER OF CONFLICTS =,I3,23H AND TURN CODE FOR PATH,
     *       4H IS ,A)
  604 FORMAT(15X,48HCONFLICT ENTRY NUMBERS ORDERED BY DISTANCE DOWN ,
     *       13HTHIS PATH ARE)
  605 FORMAT(18X,10I5)
  606 FORMAT(/)
  607 FORMAT(12X,34HTOTAL NUMBER OF PATHS CALCULATED =,I4,//)
C
C-----SUBROUTINE WRITPA WRITES THE INTERSECTION PATH INFORMATION ONTO
C-----TAPE MODELT FOR SIMPRO
C
                    IF ( NLINE+16.GT.LINES )     CALL  HEADER
      WRITE (6,601) NTABL
      NTABL = NTABL + 1
      NLINE = NLINE + 3
      WRITE (MODELT,602) NPATHS
C-----WRITE THE INFORMATION FOR EACH INTERSECTION PATH
      DO 1020  KP = 1 , NPATHS
      IF ( DIAMON )                              THEN
        IF ( IAFLAG(IOA(KP)) . EQ . 'I' )        THEN
          LOBL(KP) = LLANES(IOL(KP),INTLNK(IOA(KP)))
        END IF
      END IF
C-----WRITE THE INTERSECTION PATH INFORMATION
      WRITE (MODELT,602) IIA(KP),IIL(KP),IOA(KP),IOL(KP),
     *      IXL(1,KP),IYL(1,KP),LL1(KP),JXL(1,KP),JYL(1,KP),
     *      IXA(1,KP),IYA(1,KP),LA1(KP),IRA(1,KP),IBA(1,KP),IDA(1,KP),
     *      IXA(2,KP),IYA(2,KP),LA2(KP),IRA(2,KP),IBA(2,KP),IDA(2,KP),
     *      IXL(2,KP),IYL(2,KP),LL2(KP),JXL(2,KP),JYL(2,KP),
     *      LENP(KP),IPTURN(KP),LIMP(KP),IOPT(KP),ILCH(KP),LIBL(KP),
     *      LOBL(KP),NGEOCP(KP),PAVT(KP)
      LTEST = NLINE + 7 + (NGEOCP(KP)+9)/10
                    IF ( KP . EQ . NPATHS )      LTEST = LTEST + 3
                    IF ( LTEST . GT . LINES )    CALL  HEADER
      ITRTYP = ' '
                    IF ( IPTURN(KP).EQ.LTURNU )  ITRTYP = 'U-TURN'
                    IF ( IPTURN(KP).EQ.LTURNL )  ITRTYP = 'LEFT'
                    IF ( IPTURN(KP).EQ.LTURNS )  ITRTYP = 'STRAIGHT'
                    IF ( IPTURN(KP).EQ.LTURNR )  ITRTYP = 'RIGHT'
      WRITE (6,603) KP,IIA(KP),IIL(KP),IOA(KP),IOL(KP),LENP(KP),
     *              LIMP(KP),AVTYPS( PAVT(KP) ),NGEOCP(KP),ITRTYP
      NLINE = NLINE + 4
                    IF ( NGEOCP(KP) . LE . 0 )   GO TO 1010
      WRITE (MODELT,602) (IGEOCP(J,KP),J=1,NGEOCP(KP))
      WRITE (6,604)
      WRITE (6,605) (IGEOCP(J,KP),J=1,NGEOCP(KP))
      NLINE = NLINE + 1 + (NGEOCP(KP)+9)/10
 1010 CONTINUE
      WRITE (6,606)
      NLINE = NLINE + 2
C-----END OF INTERSECTION PATH LOOP
 1020 CONTINUE
      WRITE (6,607) NPATHS
      NLINE = NLINE + 3
      RETURN
      END                                                               WRITPA
C
C
C
      SUBROUTINE NDXCON
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONFLT'
      INCLUDE 'PATH'
      INCLUDE 'GEOCOM'
      INTEGER           I12,ICON,JCON,KP
C
C-----SUBROUTINE NDXCON CROSS INDEXES THE INTERSECTION CONFLICTS WITH
C-----THE INTERSECTION PATHS
C
C-----PROCESS EACH INTERSECTION CONFLICT
      DO 2010  ICON = 1 , NCONFS
C-----PROCESS EACH INTERSECTION PATH INVOLVED IN THE INTERSECTION
C-----CONFLICT
      DO 1030  I12 = 1 , 2
      KP = ICONP(I12,ICON)
                    IF ( NGEOCP(KP) . LE . 0 )   GO TO 9180
C-----SEARCH EACH INTERSECTION CONFLICT FOR THIS INTERSECTION PATH AND
C-----FIND INTERSECTION CONFLICT ICON ON THE IGEOCP ARRAY
      DO 1010  JCON = 1 , NGEOCP(KP)
            IF ( ICON . EQ . IGEOCP(JCON,KP) )   GO TO 1020
 1010 CONTINUE
      GO TO 9180
 1020 CONTINUE
C-----SAVE THE INDEX JCON FOR THIS INTERSECTION CONFLICT
      ICONI(I12,ICON) = JCON
C-----END OF INTERSECTION PATH LOOP
 1030 CONTINUE
C-----END OF INTERSECTION CONFLICT LOOP
 2010 CONTINUE
      RETURN
C-----PROCESS THE EXECUTION ERROR AND STOP
 9180 CONTINUE
      CALL  ABORTR  ( 'STOP 918 - ' //
     *                'CONFLICT WAS NOT FOUND ON IGEOCP LIST FOR ' //
     *                'PATH - ' //
     *                'NDXCON'                                        )
      STOP  918
      END                                                               NDXCON
C
C
C
      SUBROUTINE WRITCO
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONFLT'
      INCLUDE 'GEOCOM'
      INCLUDE 'OUTPT'
      INTEGER           I,IADD,ICON
  600 FORMAT(8X,5HTABLE,I3,25H  -  LISTING OF CONFLICTS,/)
  601 FORMAT(12X,40HCONFLICT  PATH1 PATH2  APPR1 APPR2  DIST,
     *       29H1 DIST2  ANGLE  INDEX1 INDEX2)
  602 FORMAT(20I4)
  603 FORMAT(12X,I5,2X,2I6,1X,2I6,1X,2I6,I8,2I7)
  604 FORMAT(//,12X,27HTOTAL NUMBER OF CONFLICTS =,I5,//)
C
C-----SUBROUTINE WRITCO WRITES THE INTERSECTION CONFLICT INFORMATION
C-----ONTO TAPE MODELT FOR SIMPRO
C
                    IF ( NLINE+10 . GT . LINES ) CALL  HEADER
      WRITE (6,600) NTABL
      WRITE (6,601)
      NLINE = NLINE + 3
      NTABL = NTABL + 1
      WRITE (MODELT,602) NCONFS
      IADD = 1
C-----WRITE THE INFORMATION FOR EACH INTERSECTION CONFLICT
      DO 1010  ICON = 1 , NCONFS
C-----WRITE THE INTERSECTION CONFLICT INFORMATION
      WRITE (MODELT,602) (ICONP(I,ICON),I=1,2),(ICONA(I,ICON),I=1,2),
     *      (ICOND(I,ICON),I=1,2),ICONAN(ICON),(ICONI(I,ICON),I=1,2)
                    IF ( ICON . GT . NCONFS-4 )  IADD = NCONFS-ICON+6
      IF ( NLINE+IADD.GT.LINES )                 THEN
        CALL  HEADER
        WRITE (6,601)
        NLINE = NLINE + 1
      END IF
      WRITE (6,603) ICON,(ICONP(I,ICON),I=1,2),(ICONA(I,ICON),I=1,2),
     *      (ICOND(I,ICON),I=1,2),ICONAN(ICON),(ICONI(I,ICON),I=1,2)
      NLINE = NLINE + 1
 1010 CONTINUE
      WRITE (6,604) NCONFS
      NLINE = NLINE + 5
      RETURN
      END                                                               WRITCO
C
C
C
      SUBROUTINE XROTX  ( X,Y,IAZIM,RX,RY )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      DOUBLE PRECISION  COSA,RX,RY,SINA,X,Y,DIAZIM
      INTEGER           IALAST,IAZIM
      SAVE              COSA,IALAST,SINA
      DATA     COSA   / 1.0D0 /
      DATA     IALAST / 0 /
      DATA     SINA   / 0.0D0 /
C
C-----SUBROUTINE XROTX ROTATES A REAL VECTOR BY AN AZIMUTH AND RETURNS
C-----A REAL VECTOR
C
                    IF ( IAZIM . EQ . IALAST )   GO TO 1010
      DIAZIM=DBLE(IAZIM)*DEG2RD
      SINA = DSIN(DIAZIM)
      COSA = DCOS(DIAZIM)
      IALAST = IAZIM
 1010 CONTINUE
      RX =  X*COSA + Y*SINA
      RY = -X*SINA + Y*COSA
      RETURN
      END                                                               XROTX
C
C
C
      SUBROUTINE XROTI  ( X,Y,IAZIM,IRX,IRY )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'RADIAN'
      DOUBLE PRECISION  RX,RY,X,Y
      INTEGER           IAZIM,IRX,IRY
C
C-----SUBROUTINE XROTI ROTATES A REAL VECTOR BY AN AZIMUTH AND RETURNS
C-----AN INTEGER VECTOR
C
      CALL  XROTX   ( X,Y,IAZIM,RX,RY )
      IRX = SIGN( SNGL(DABS(RX)+XROUND),SNGL(RX) )
      IRY = SIGN( SNGL(DABS(RY)+XROUND),SNGL(RY) )
                    IF ( DABS(RX) . LT . XROUND )IRX = 0
                    IF ( DABS(RY) . LT . XROUND )IRY = 0
      RETURN
      END                                                               XROTI
C
C
C
      SUBROUTINE IROTX  ( IX,IY,IAZIM,RX,RY )
      IMPLICIT NONE                                                     CCODE=C.
      DOUBLE PRECISION  RX,RY,X,Y
      INTEGER           IAZIM,IX,IY
C
C-----SUBROUTINE IROTX ROTATES AN INTEGER VECTOR BY AN AZIMUTH AND
C-----RETURNS A REAL VECTOR
C
      X = IX
      Y = IY
      CALL  XROTX   ( X,Y,IAZIM,RX,RY )
      RETURN
      END                                                               IROTX
C
C
C
      SUBROUTINE XROTAX ( X,Y,IAZIM,IAX,IAY,RX,RY )
      IMPLICIT NONE                                                     CCODE=C.
      DOUBLE PRECISION  RX,RY,X,Y
      INTEGER           IAX,IAY,IAZIM
C
C-----SUBROUTINE XROTAX ROTATES A REAL VECTOR BY AN AZIMUTH, ADDS AN
C-----INTEGER COORDINATE, AND RETURNS A REAL COORDINATE
C
      CALL  XROTX   ( X,Y,IAZIM,RX,RY )
      RX = IAX + RX
      RY = IAY + RY
      RETURN
      END                                                               XROTAX
C
C
C
      SUBROUTINE XROTAI ( X,Y,IAZIM,IAX,IAY,IRX,IRY )
      IMPLICIT NONE                                                     CCODE=C.
      DOUBLE PRECISION  X,Y
      INTEGER           IAX,IAY,IAZIM,IRX,IRY
C
C-----SUBROUTINE XROTAI ROTATES A REAL VECTOR BY AN AZIMUTH, ADDS AN
C-----INTEGER COORDINATE, AND RETURNS AN INTEGER COORDINATE
C
      CALL  XROTI   ( X,Y,IAZIM,IRX,IRY )
      IRX = IAX + IRX
      IRY = IAY + IRY
      RETURN
      END                                                               XROTAI
C
C
C
      SUBROUTINE IROTAX ( IX,IY,IAZIM,IAX,IAY,RX,RY )
      IMPLICIT NONE                                                     CCODE=C.
      DOUBLE PRECISION  RX,RY
      INTEGER           IAX,IAY,IAZIM,IX,IY
C
C-----SUBROUTINE IROTAX ROTATES AN INTEGER VECTOR BY AN AZIMUTH, ADDS AN
C-----INTEGER COORDINATE, AND RETURNS A REAL COORDINATE
C
      CALL  IROTX   ( IX,IY,IAZIM,RX,RY )
      RX = IAX + RX
      RY = IAY + RY
      RETURN
      END                                                               IROTAX
C
C
C
      FUNCTION   AZIM36 ( Y,X )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      DOUBLE PRECISION  X,Y
      DOUBLE PRECISION  ATAN36,AZIM36
C
C-----FUNCTION AZIM36 FINDS THE ARC TANGENT OF A COORDINATE AND RETURNS
C-----THE AZIMUTH FROM 0 TO 360 DEGREES (NORTH ZERO AND CLOCKWISE
C-----POSITIVE)
C
      AZIM36 = RAD2DG*ATAN36( X,Y )
      RETURN
      END                                                               AZIM36
C
C
C
      FUNCTION   ATAN36 ( Y,X )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'CONSTN'
      DOUBLE PRECISION  X,Y
      DOUBLE PRECISION  ATAN36
C
C-----FUNCTION ATAN36 FINDS THE ARC TANGENT OF A COORDINATE AND RETURNS
C-----THE ANGLE FROM 0 TO 360 DEGREES (EAST ZERO AND COUNTER-CLOCKWISE
C-----POSITIVE)
C
      ATAN36 = PIT2
            IF ( Y.EQ.0.0D0.AND.X.GE.0.0D0 )     ATAN36 = 0.0D0
            IF ( X.EQ.0.0D0.AND.Y.GT.0.0D0 )     ATAN36 = PID2
            IF ( Y.EQ.0.0D0.AND.X.LT.0.0D0 )     ATAN36 = PI
            IF ( X.EQ.0.0D0.AND.Y.LT.0.0D0 )     ATAN36 = PIT1P5
                    IF ( ATAN36 . NE . PIT2 )    RETURN
      ATAN36 = DATAN( Y/X )
                    IF ( X . LT . 0.0D0 )        ATAN36 = ATAN36 + PI
            IF ( X.GT.0.0D0.AND.Y.LT.0.0D0 )     ATAN36 = ATAN36 + PIT2
      RETURN
      END                                                               ATAN36
C
C
C
      SUBROUTINE ABORTR ( MESAGE )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'GEOVAL'
      INCLUDE 'OUTPT'
      CHARACTER*(*)     MESAGE
C
C-----SUBROUTINE ABORTR PRINTS THE ERROR MESSAGE
C
C-----PRINT THE ERROR MESSAGE
      CALL  PRTERR  ( MESAGE )
                    IF ( IPLOT . EQ . 4 )        RETURN
C-----END THE PLOT
C     CALL  PLOT    ( 0.0,0.0,999 )
C\    CALL  PLOT    ( 0.0,0.0,999 )
C:    CALL  PLOT    ( 0.0,0.0,999 )
      RETURN
      END                                                               ABORTR
C
C
C
      SUBROUTINE PRTERR ( MESAGE )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'OUTPT'
      CHARACTER*(*)     MESAGE
      INTEGER           ILNB
      INTEGER           NCMES
  601 FORMAT(A)
      NCMES = MAX0( ILNB( MESAGE ),1 )
      OPEN  (NER,FILE='error.txt',ACCESS='APPEND',STATUS='UNKNOWN')
      WRITE (NER,601) MESAGE(1:NCMES)
      ENDFILE NER
      CLOSE (NER,STATUS='KEEP')
      WRITE (6,601)
      WRITE (6,601) MESAGE(1:NCMES)
      WRITE (*,601)
      WRITE (*,601) MESAGE(1:NCMES)
      RETURN
      END                                                               PRTERR
C
C
C
C     FUNCTION   IAND   ( I,J )
C     IMPLICIT NONE                                                     CODE=C.
C     INCLUDE 'PARAMS'
C     INTEGER           I,ILT,IRT,IST,IUT,J
C     INTEGER           IAND
C     IUT = ( I/LTURNU ) + ( J/LTURNU )
C     ILT = (-( (I/LTURNL)/LTURNS*LTURNS ) + I/LTURNL ) +
C    *      (-( (J/LTURNL)/LTURNS*LTURNS ) + J/LTURNL )
C     IST = (-( (I/LTURNS)/LTURNS*LTURNS ) + I/LTURNS ) +
C    *      (-( (J/LTURNS)/LTURNS*LTURNS ) + J/LTURNS )
C     IRT = (-(I/LTURNS*LTURNS) + I ) + (-(J/LTURNS*LTURNS) + J )
C     IAND = 0
C                   IF ( IUT . EQ . 2 )          IAND =        LTURNU
C                   IF ( ILT . EQ . 2 )          IAND = IAND + LTURNL
C                   IF ( IST . EQ . 2 )          IAND = IAND + LTURNS
C                   IF ( IRT . EQ . 2 )          IAND = IAND + LTURNR
C     RETURN
C     END                                                               IAND
C
C
C
      FUNCTION AVTYPS ( AVTVAL )
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      CHARACTER*4       AVTYPS
      INTEGER           AVTVAL
C-----SET ALLOWED VEHICLE TYPES STRING
      AVTYPS = '    '
C-----SET BICYCLE
      IF ( IAND( AVTVAL,LAVTB ) . NE . 0 )       AVTYPS(1:1) = 'B'
C-----SET EMERGENCY VEHICLE
      IF ( IAND( AVTVAL,LAVTE ) . NE . 0 )       AVTYPS(2:2) = 'E'
C-----SET RAIL VEHICLE
      IF ( IAND( AVTVAL,LAVTR ) . NE . 0 )       AVTYPS(3:3) = 'R'
C-----SET NORMAL VEHICLE
      IF ( IAND( AVTVAL,LAVTV ) . NE . 0 )       AVTYPS(4:4) = 'V'
      RETURN
      END                                                               AVTYPS
