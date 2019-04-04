      PROGRAM    DVPRO
C                       ( INPUT,OUTPUT,TAPE9 )
C                           I     L     T9     PRE   C     LDV
C                           SYSDAT REP
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
C-----Harmonia mod. BAMauldon 10/2013
C----- Injected random seed (JSEED) and number of executions from java
C----- Added NREP and JSEED parameters to the command line and JRANDOM 32
C-----  bit integer. JRandom contains the random seed. 
C----- 
C----- end Harmina mod -----------------------------------------------------------
C
C-----DRIVER-VEHICLE PROCESSOR FOR THE TEXAS TRAFFIC SIMULATION PACKAGE
C
C-----READ      ERROR STOP NUMBERS ARE 793-799
C-----READ      ERROR STOP NUMBERS ARE 801-899
C-----EXECUTION ERROR STOP NUMBERS ARE 901-906
C
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHTEX'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'CURAND'
      INCLUDE 'CWDDIR'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LITCON'
      INCLUDE 'OUTPUT'
      INCLUDE 'PARVAL'
      INCLUDE 'STATS'
      INCLUDE 'TITLE'
      INCLUDE 'USFILE'
      DIMENSION         NOUT(20)
      INTEGER           NOUT
      CHARACTER*3       IOFORM,TAG,TAGU
      CHARACTER*7       STATLF,STATT9
      CHARACTER*60      DEFILE,TDFILE
      CHARACTER*60      DEFFN,GDFILE,DEFILN
      INTEGER           I,IS1,IS2,N,NC,NCGD,NCLF,NCT9,NDFIO,NIN
      LOGICAL           EXI,STDTAG
      INTEGER           ILNB
C|    CHARACTER*60      T9FILEU
C|    INTEGER*2         CURPRO,TMPPRO
C|    INTEGER*4         ISTAT
C|    INTEGER*4         SYS$SETDFPROT
C|    DATA     CURPRO / 1 /
      DATA     IOFORM / '(A)' /
      DATA     STATLF / 'UNKNOWN' /
      DATA     STATT9 / 'UNKNOWN' /
C|    DATA     TMPPRO / 'F000'X /
  601 FORMAT(47HDriver-Vehicle Processor for the TEXAS Traffic ,
     *       20HSimulation Package (,A5,1H),/,
     *       28HDVPRO Copyright (c) 1989 by ,
     *       33HThe University of Texas at Austin,/)
C
C----- UNIT#     R/W    FILNAM       USAGE
C----- *         WRITE               DVPRO STANDARD CONSOLE (TTY)
C----- 5=NIN     READ                DVPRO STANDARD INPUT
C----- 6=NIO     WRITE  dvplist.txt  DVPRO STANDARD OUTPUT
C----- 9=MODELT  WRITE  fort9        DVPRO OUTPUT (VEHICLE STREAM) FOR SIMPRO
C-----23=NDFIO   INQUIRE
C-----90=NER     WRITE  error.txt    DVPRO AUTOMATED RUN ERROR FILE
C-----91=NWR     WRITE  warning.txt  DVPRO AUTOMATED RUN WARNING FILE
C
C-----IFILE  - INPUT FILE (UNIT <NIN>)
C-----PFILE  - PREPROCESSOR (CONFIGURATION 2) FILE (UNIT <NDFIO>)
C-----CFILE  - CONVERTED (CONFIGURATION 1) FILE (UNIT <NIN>)
C-----LFILE  - DATA LISTING FILE (UNIT <6>)
C-----T9FILE - OUTPUT FILE (UNIT <MODELT>)
C
C-----INITIALIZE CONSTANTS
      CALL  INITCN
      WRITE (*,601) IVERSN
      CALL  CLFILE  ( PARNDV,PAR,PARVAL,NCPV,'dvpro' )
      CWDDIR=WRKDIR
      CALL  TOUPR   ( PAUSND )
C-----Harmonia mod-----------------------------------------------Harmonia B A Mauldon 10/10/2013     
      READ(JSEED, '(I8)') JRANDM 
C-----End--------------------------------------------------------Harmonia     
        
C
C-----KEYWORD "LDV" ALSO FOR LISTING FILE NAME
C
      IF ( PARVAL(6) . NE . ICBL )               LFILE=PARVAL(6)
C
C-----READ INITIALIZATION DATA
C
      N   = 1
      NIO = 0
      CALL  GDVS0   ( N,1,NOUT,NIO,I,I,IS1,IS2,SYSDAT,
     *                ICBL,TDFILE,DEFILE,USFILE,CNO,CNO,CNO,CNO )
      IF ( N . EQ . -1 )                         GO TO 8630
      GDFILE = ICBL
      IF(ILNB( CFILE  ).EQ.0) CFILE ='gdv'
C%    IF(ILNB( CWDDIR ).GT.0) USFILE=CWDDIR
      IF(ILNB( T9FILE ).EQ.0) T9FILE=DEFFN( MODELT )
C%    IF(ILNB( CFILE  ).GT.0) CALL PCFS   ( CFILE ,USFILE,CFILE  )
C%    IF(ILNB( DEFILE ).GT.0) CALL PCFS   ( DEFILE,USFILE,DEFILE )
C%    IF(ILNB( IFILE  ).GT.0) CALL PCFS   ( IFILE ,USFILE,IFILE  )
C%    IF(ILNB( LFILE  ).GT.0) CALL PCFS   ( LFILE ,USFILE,LFILE  )
      NDFIO =  NOUT(2) + 2
C
C-----NIN - UNIT FOR STANDARD INPUT
C-----NIO - UNIT FOR USER INPUT/OUTPUT
C
      NIN    = NOUT(1) - 1
      NOUT(1)= NIO
C|    STATLF = 'NEW'
      
      OPEN(NIO,FILE='log.out')
      WRITE(NIO,*) JSEED
      WRITE(NIO,*) JRANDM
      CLOSE(NIO)
      
      IF ( IFILE . NE . ICBL )                   THEN
C
C-----USE INPUT FILE FROM COMMAND LINE
C
        OPEN(NIN,FILE=IFILE
C|   *                     ,READONLY     ,STATUS='UNKNOWN'
C?   *                     ,READONLY
     *                     ,ACTION='READ',STATUS='UNKNOWN'              CCODE=C{
C%   *                     ,ACTION='READ',STATUS='UNKNOWN'
     *                                                    )
C|      STATT9='NEW'
        GO TO 1000
      END IF
      CALL  GDVCON  ( NIN,TAG,NOUT,GDFILE,PFILE,CFILE,DEFILE,USFILE )
      TAGU=TAG
      CALL  TOUPR   ( TAGU )
      IF ( TAGU . EQ . 'NUL' )                   GO TO 8640
      IF ( GDFILE . NE . ICBL )                  THEN
        NCGD=ILNB( GDFILE )
        WRITE (*,'(3A/A)') 'G & D-V Preprocessor file "',
     *        GDFILE(:NCGD),'" has been','converted for input to '//
     *        'the Geometry and Driver-Vehicle Processors.'
      END IF
      IF ( TAG . EQ . ICBL )                     THEN
C|      STATT9='NEW'
        GO TO 1000
      END IF
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
        CALL TXINQ ( 0,'dvo'//TAG,TDFILE,T9FILE,EXI )
C
C-----WRITE DVO OUTPUT DATA TO FILE NAMED TO REFER TO A
C-----STANDARD LIBRARY FILE
C
      ELSE
C
C-----WRITE DVO OUTPUT DATA TO FILE NAMED TO REFER TO A
C-----USER-GROUP FILE
C
        CALL TXINQ ( 0,'dvo'//TAG,USFILE,T9FILE,EXI )
      END IF
 1000 CONTINUE
C
C---------- START OF CODE FOR RM FORTRAN ----------
C
C-----WON'T WORK WITHOUT IT
C
C     READ (NIN,IOFORM)
C     REWIND(NIN)
C
C---------- END OF CODE FOR RM FORTRAN ----------
C
      INQUIRE(NIN,NAME=IFILE)
      NC   = ILNB( IFILE )
      IF ( NC . EQ . 0 )                         GO TO 8650
C%    CALL  PCFS   ( IFILE,USFILE,IFILE )
      CALL  SHONAM ( 'Driver-Vehicle input data file name is',IFILE,78)
      NCLF = ILNB( LFILE )
      IF ( NCLF . GT . 0 )                       THEN
        OPEN(6,FILE=LFILE,STATUS=STATLF
C|   *                                 ,DEFAULTFILE='.lst'
     *                                                    )
      END IF
      NCT9 = ILNB( T9FILE )
C|    T9FILEU=T9FILE
C|    CALL  TOUPR  ( T9FILEU )
C|    IF ( INDEX(T9FILEU,'DVO').GE.1 )ISTAT=SYS$SETDFPROT(TMPPRO,CURPRO)
      IF ( NCT9 . GT . 0 )                       THEN
        OPEN(MODELT,FILE=T9FILE,STATUS=STATT9
C|   *                                       ,CARRIAGECONTROL='LIST'
C~   *                                       ,CARRIAGECONTROL='LIST'
C?   *                                       ,CARRIAGECONTROL='LIST'
C%   *                                       ,CARRIAGECONTROL='LIST'
     *                                                              )
      ELSE
        DEFILN=DEFFN( MODELT )
        OPEN (MODELT,FILE=DEFILN,STATUS=STATT9
C|   *                                        ,CARRIAGECONTROL='LIST'
C~   *                                        ,CARRIAGECONTROL='LIST'
C?   *                                        ,CARRIAGECONTROL='LIST'
C%   *                                        ,CARRIAGECONTROL='LIST'
     *                                                               )
      END IF
C|    TMPPRO=0
C|    T9FILEU=T9FILE
C|    CALL  TOUPR  ( T9FILEU )
C|    IF ( INDEX(T9FILEU,'DVO').GE.1 )ISTAT=SYS$SETDFPROT(CURPRO,TMPPRO)
      REWIND(MODELT)
      CALL  READIN  ( NIN )
      CALL  WRITDV
      CALL  BIASLT
      CALL  GENHED
      CALL  GENDV   ( NIN )
      CALL  PNOTES
      CALL  PSUMDV
      CALL  PSTATS
      INQUIRE(6,NAME=LFILE)
      CALL  SHONAM ( 'Driver-Vehicle Processor input data '//
     *               'listing on',LFILE,78)
      CLOSE(6)
      INQUIRE(MODELT,NAME=T9FILE)
      CALL  NOTF   ( NDFIO,'Driver-Vehicle Processor output data',
     *               'dvo'//TAG,T9FILE)
      IF ( TAG . EQ . ICBL )                     THEN
        CLOSE(MODELT)
        GO TO 1040
      END IF
C
C-----MAKE PERMANENT COPY AND CLOSE D-V OUTPUT DATA FILE
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
        CALL TXCLOS ( MODELT,T9FILE,TDFILE )
      ELSE
        CALL TXCLOS ( MODELT,T9FILE,USFILE )
      END IF
 1040 CONTINUE
      WRITE (*,IOFORM)
     *       'Driver-Vehicle Data for TEXAS Model have been processed.'
      IF ( PAUSND . EQ . CYES )                  THEN
        CALL EXIT ( 0 )
      END IF
      GO TO 9999
C-----PROCESS INPUT ERRORS AND STOP
 8630 CONTINUE
      ERRMSG = 'STOP 863 - ERROR WHEN ACCESSING FILE "gdvs00" - DVPRO'
      CALL  ABORTR ( ERRMSG )
      STOP  863
 8640 CONTINUE
      ERRMSG = 'STOP 864 - USABLE INPUT DATA FILE NOT SPECIFIED - DVPRO'
      CALL  ABORTR ( ERRMSG )
      STOP  864
 8650 CONTINUE
      CLOSE (NIN,STATUS='DELETE')
      ERRMSG = 'STOP 865 - INPUT DATA FILE NOT SPECIFIED - DVPRO'
      CALL  ABORTR ( ERRMSG )
      STOP  865
 9999 CONTINUE
      END                                                               DVPRO
C
C
C
      BLOCK DATA
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHTEX'
      INCLUDE 'CLASS'
      INCLUDE 'CURAND'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LITCON'
      INCLUDE 'OUTPUT'
      INCLUDE 'PARVAL'
      INCLUDE 'STATS'
      INCLUDE 'TITLE'
      INTEGER           NDCNVC,NALNIA,NANINV,NIANV2 ,NOANIA,NONIVT,
     *                  NVCNIA,NVCX  ,NDCX  ,NIANVT,
     *                  NVCXDC       ,NVTNIA       ,NALNAP
      PARAMETER ( 
     *           NALNAP=NAL*NAP                                     ,
     *           NALNIA=NAL             *NIA                        ,
     *           NANINV=NALNIA                                *NVT  ,
     *           NDCNVC=        NDC             *NVC                ,
     *           NDCX  =        NDC-NDCD                            ,
     *           NIANVT=                 NIA                  *NVT  ,
     *           NIANV2 =                NIANVT                   *2,
     *           NOANIA=                 NIA*NOA                    ,
     *           NONIVT=                 NIA*NOA              *NVT  ,
     *           NVCNIA=                 NIA    *NVC          *NVT  ,
     *           NVCX  =                         NVC-NVCD           ,
     *           NVCXDC=        NDC                      *NVCX      ,
     *           NVTNIA=                 NIA                  *NVT   )
C-----COMMON / APPRO  /
      DATA     AAVT   /    NAP*0       /
      DATA     ALOWUT /    NAP*.FALSE. /
      DATA     FREEUT / NOANIA*.FALSE. /
      DATA     IAFLAG /    NAP*ICBL    /
      DATA     IBAP   /    NAP*.FALSE. /
      DATA     ILT    /    NLA*' '     /
      DATA     ILUB   /    NLA*' '     /
      DATA     ILUE   /    NLA*' '     /
      DATA     ILUR   /    NLA*' '     /
      DATA     ILUV   /    NLA*' '     /
      DATA     INTLNK / 8     * 0,
     *                  3     *13,
     *                          5,
     *                          4,
     *                  3     *12      /
      DATA     IRT    /    NLA*' '     /
      DATA     ISFLAG / 4*'R',4*'L',4*'R',4*'L' /
      DATA     IST    /    NLA*' '     /
      DATA     IUT    /    NLA*' '     /
      DATA     IVOLT  /    NIA*0       /
      DATA     LLANES / NALNAP*0       /
      DATA     LAVT   /    NLA*0       /
      DATA     MVA    / NVTNIA*0       /
      DATA     NVA    / NVTNIA*0       /
      DATA     NGWRTT /    NIA*0       /
C-----COMMON / CHTEX  /
      DATA     CDISTN / 'CONSTAN','ERLANG','GAMMA','LOGNRML',
     *                  'NEGEXP','SNEGEXP','UNIFORM' /
      DATA     CNO    / 'NO'    /
      DATA     CYES   / 'YES'   /
      DATA     CYESDL / NDC*'NO' /
      DATA     CYESVL / NVC*'NO' /
C-----COMMON / CLASS  /
      DATA     IAMAX  /  14,  8,  9, 11,  7,  6,  6,  5,  4,  3,  5,  4,
     *                   NVCX*0 /
      DATA     IDMAX  /  10,  9,  9,  8,  7,  5,  7,  5,  6,  4,  6,  4,
     *                   NVCX*0 /
      DATA     IRMIN  /  11, 11, 13, 14, 28, 28, 28, 28, 22, 22, 22, 22,
     *                   NVCX*0 /
      DATA     IVCHAR / 115, 90,100,110, 85, 80, 80, 75, 70, 65, 75, 70,
     *                   NVCX*0 /
      DATA     IVMAX  / 205,120,135,150,100, 85,100, 85, 95, 75,100, 80,
     *                   NVCX*0 /
      DATA     LENV   /  14, 15, 16, 18, 32, 32, 32, 32, 60, 60, 72, 72,
     *                   NVCX*0 /
C-----CLASSIFICATIONS, HEIGHTS, WIDTHS, AND UNIT INFO ARE NEW VEHICLE
C-----ATTRIBUTES
      DATA     CLASSV /   4*'AUTO',2*'SU2',2*'SU3',2*'2S-1',2*'3S-2',
     *                   NVCX*' ' /
      DATA     WIDV   /   6,  6,  7,  7,  8,  8,  8,  8,  8,  8,  8,  8,
     *                   NVCX*0 /
      DATA     HEIGHT /   4,  4,  5,  6, 12, 12, 12, 12, 13, 13, 13, 13,
     *                   NVCX*0 /
      DATA     NUNITS /   8*1,4*2,NVCX*0 /
      DATA     IDCHAR /  110, 100,  85, NDCX*0   /
      DATA     PIJR   /  0.5, 1.0, 1.5, NDCX*0.0 /
C-----PERCENT OF DRIVER CLASS IN VEHICLE CLASS - XPERD(IDRICL,IVEHCL)
      DATA     XPERD  / 50.0,40.0,10.0,NDCX*0.0,
     *                  30.0,40.0,30.0,NDCX*0.0,
     *                  35.0,35.0,30.0,NDCX*0.0,
     *                  25.0,45.0,30.0,NDCX*0.0,
     *                  40.0,30.0,30.0,NDCX*0.0,
     *                  40.0,30.0,30.0,NDCX*0.0,
     *                  40.0,30.0,30.0,NDCX*0.0,
     *                  40.0,30.0,30.0,NDCX*0.0,
     *                  50.0,40.0,10.0,NDCX*0.0,
     *                  50.0,40.0,10.0,NDCX*0.0,
     *                  50.0,40.0,10.0,NDCX*0.0,
     *                  50.0,40.0,10.0,NDCX*0.0,
     *                  NVCXDC*0.0               /
C-----COMMON / CURAND /
      DATA     ISEED  / 42081,13747,00291,77043,
     *                  50123,33145,83447,60695 /
C-----                  32257,99381,23145,19905 OLD VALUES FOR 09-12
      DATA     NEWSED / .TRUE. /
C-----COMMON / DVDATA /
      DATA     IEOF   /        .FALSE. /
      DATA     MAYENT / NALNIA*.FALSE. /
      DATA     QTLAST / NALNIA*-5.0    /
C-----PERCENT OF VEHICLE CLASS IN TRAFFIC STREAM - XPERV(IVEHCL,IVT,IAN)
      DATA     XPERV
     *    /1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     1     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     1     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0, 
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     1     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     1     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     1     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     1     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     1     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     1     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     1     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     1     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0,
     *     1.5,22.5,23.3,44.7,2.6,2.6,0.2,0.2,0.2,0.2,1.0,1.0,NVCX*0.0/
      DATA     ZERO   / 0.001 /
C-----COMMON / INDEX  /
      DATA     XPERL  / NANINV*0.0 /
      DATA     NSTOP  / 0 /
      DATA     NWARN  / 0 /
      DATA     NGELIM / 0 /
C-----COMMON / INTER  /
      DATA     LIBAR  / NAP*0 /
      DATA     LOBAR  / NAP*0 /
C-----COMMON / OUTPUT /
      DATA     LINES  / 45 /
      DATA     MODELT / 9 /
      DATA     NLINE  / 0 /
      DATA     NLINEH / 6 /
      DATA     NOTEU   / NND*0 /
      DATA     NPAGE  / 1 /
      DATA     NTABL  / 1 /
C-----COMMON / PARVAL /
C-----THE FIRST 9 PARAMETERS ARE FOR DVPRO
C-----THE LAST  3 PARAMETERS ARE FOR GDVPRO PROCESSING
C-----LGEO (DUMLGE), T8 (DUMT8F), AND PLOT (DUMPLT) ARE DUMMY PARAMETERS
      DATA     NCPV   / -1       ,-1       ,-1    ,-1       ,-1       ,
     *                  -1       ,-1       ,-1    ,-1       ,-1       ,
     *                  -1       ,-1       ,-1    ,-1 /
      DATA     PAR    / 'I'      ,'L'      ,'T9'  ,'PRE'    ,'C'      ,
     *                  'LDV'    ,'SYS_DAT','REP' ,
     *                  'JSEED'  , 'WRK_DIR','PAUSEND',
     *                  'LGEO'   ,'T8'     ,'PLOT'/
      DATA     PARVAL / ICBL     ,'dvplist.txt'   ,ICBL     ,ICBL     ,
     *                  ICBL     ,ICBL     ,ICBL  ,'NO'     ,
     *                  '  '     ,  ICBL     ,
     *                  'YES'    ,'DUMLGE' ,'DUMT8F'        ,'DUMPLT' /
C-----COMMON / STATS  /
      DATA     SPERD  / NDCNVC*0.0 /
      DATA     SPERL  / NANINV*0.0 /
      DATA     SPERT  / NONIVT*0.0 /
      DATA     SPERUT / NIANV2*0.0 /
      DATA     SPERV  / NVCNIA*0.0 /
C-----COMMON / TITLE  /
      DATA     IVERSN / 'V6.00' /
      END                                                               BLKDAT
C
C
C
      SUBROUTINE READIN ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHTEX'
      INCLUDE 'INDEX'
      INCLUDE 'LITCON'
      INCLUDE 'OUTPUT'
      INCLUDE 'PARVAL'
      INCLUDE 'TITLE'
      CHARACTER*5       IFORM
      INTEGER           NC,NIN
      INTEGER           ILNB
  501 FORMAT(A)
  867 FORMAT(11HSTOP 867 - 
     *       14HREPRUN VALUE =,I6,31H IS LESS THAN 1 OR GREATER THAN,I3,
     *        9H - READIN)
C
C-----SUBROUTINE READIN READS INPUT DATA AND CHECKS FOR ERRORS
C
C-----READ 80 CHARACTER TITLE FOR DVPRO
      READ (NIN,501) ITITLE
      CALL  GETCDT  ( ITITLE(61:80) )
      ITITLE(60:60) = ICBL
      NUMREP = 1
C-----Harmonia mod-Removed--------------------------------------------BAMauldon 10/14/2013      
C-----IF ( REPRUN . NE . CNO    )                THEN
C-----NC = ILNB( REPRUN )
C-----IF ( NC . GE . 1 )                       THEN
C-----WRITE (IFORM,'(2H(I,I2.2,1H))') NC
C-----READ (REPRUN,IFORM,ERR=8660) NUMREP
C-----IF ( NUMREP.LT.1 . OR . NUMREP.GT.NRP )GO TO 8670
C----- WRITE (ITITLE(56:59),'(2H R,I2.2)') NUMREP
C-----END IF
C-----END IF
C-----End Harmonia mod---------------------------------------------------------------------
      CALL  HEADER
C-----READ THE NUMBER AND LIST OF INBOUND AND OUTBOUND APPRAOCHES AND
C-----CHECK FOR ERRORS
      CALL  READIO  ( NIN )
C-----READ THE NUMBER OF APPROACHES AND DRIVER-VEHICLE PROCESSOR OPTIONS
C-----AND CHECK FOR ERRORS
      CALL  READOP  ( NIN )
C-----READ THE APPROACH INFORMATION AND CHECK FOR ERRORS
      CALL  READAP  ( NIN )
C-----DUMMY READ GEOMETRY PROCESSOR DATA
      CALL  READGP  ( NIN )
C-----READ YES OPTIONS
      CALL  READYO  ( NIN )
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 8660 CONTINUE
      ERRMSG = 'STOP 866 - '                                          //
     *         'READ ERROR READING REPRUN VALUE FROM COMMAND LINE - ' //
     *         'READIN'
      CALL  ABORTR ( ERRMSG )
      STOP  866
 8670 CONTINUE
      WRITE (ERRMSG,867) NUMREP,NRP
      CALL  ABORTR ( ERRMSG )
      STOP  867
      END                                                               READIN
C
C
C
      SUBROUTINE HEADER
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'OUTPUT'
      INCLUDE 'TITLE'
  601 FORMAT(1H ,43HDRIVER-VEHICLE PROCESSOR FOR TEXAS TRAFFIC ,
     *       20HSIMULATION PACKAGE (,A5,1H),3X,4HPAGE,I3,/,
     *       10X,28HDVPRO COPYRIGHT (c) 1989 BY ,
     *       33HTHE UNIVERSITY OF TEXAS AT AUSTIN,/)
  602 FORMAT(1X,A80,//)
C
C-----SUBROUTINE HEADER SKIPS TO THE TOP OF A NEW PAGE, PRINTS THE
C-----HEADER MESSAGE, AND PRINTS THE TITLE FOR DVPRO
C
      IF ( NLINE . GT . 0 )                      THEN
        WRITE (6,'(A)') CHAR( 12 )
C-----  CHAR( 12 ) = FORM FEED = FF (START NEW PAGE WITH STANDARD HEADER)
C-----  CHAR IS A STANDARD (BUILT IN) FORTRAN FUNCTION
      END IF
      WRITE (6,601) IVERSN,NPAGE
      NLINE = 3
      NPAGE = NPAGE + 1
      WRITE (6,602) ITITLE
      NLINE  = NLINE + 3
      NLINEH = NLINE
      RETURN
      END                                                               HEADER
C
C
C
      SUBROUTINE READIO ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHTEX'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'OUTPUT'
      CHARACTER*4       IDIA
      INTEGER           NIN
  501 FORMAT(20I4)
  601 FORMAT(8X,5HTABLE,I3,33H  -  LISTING OF INBOUND APPROACH ,
     *       7HNUMBERS,//)
  602 FORMAT(16X,I6)
  603 FORMAT(//,12X,37HTOTAL NUMBER OF INBOUND APPROACHES = ,I2)
  604 FORMAT(8X,5HTABLE,I3,34H  -  LISTING OF OUTBOUND APPROACH ,
     *       7HNUMBERS,//)
  605 FORMAT(16X,I6)
  606 FORMAT(//,12X,38HTOTAL NUMBER OF OUTBOUND APPROACHES = ,I2)
  801 FORMAT('STOP 801 - ',
     *       'NUMBER OF INBOUND APPROACHES = ',I3,' IS LE 0 OR GT ',I1,
     *       ' - READIO')
  802 FORMAT('STOP 802 - ',
     *       'INBOUND APPROACH',I3,' = ',I3,' IS LE 0 OR GT ',I2,
     *       ' - READIO')
  803 FORMAT('STOP 803 - ',
     *       'INBOUND APPROACH'I3,' = ',I3,' IS EQUAL TO INBOUND ',
     *       'APPROACH',I3,' = ',I3,
     *       ' - READIO')
  804 FORMAT('STOP 804 - ',
     *       'NUMBER OF OUTBOUND APPROACHES =',I3,' IS LE 0 OR GT ',I1,
     *       ' - READIO')
  805 FORMAT('STOP 805 - ',
     *       'OUTBOUND APPROACH',I3,' = ',I3,' IS LE 0 OR GT ',I2,
     *       ' - READIO')
  806 FORMAT('STOP 806 - ',
     *       'OUTBOUND APPROACH',I3,' = ',I3,' IS EQUAL TO OUTBOUND ',
     *       'APPROACH',I3,' = ',I3,
     *       ' - READIO')
  807 FORMAT('STOP 807 - ',
     *       'INBOUND APPROACH',I3,' = ',I3,' IS EQUAL TO OUTBOUND ',
     *       'APPROACH',I3,' = ',I3,
     *       ' - READIO')
C
C-----SUBROUTINE READIO READS THE NUMBER AND LIST OF INBOUND AND
C-----OUTBOUND APPROACHES AND CHECKS FOR ERRORS
C
C-----READ NUMBER OF INBOUND APPROACHES
      READ (NIN,'(I4,A4)') NIBA,IDIA
      IF ( IDIA . EQ . ' DIA' )                  THEN
        DIAMON = .TRUE.
      ELSE
        DIAMON = .FALSE.
      END IF
                    IF ( NIBA . LE .  0   )      GO TO 8010
                    IF ( NIBA . GT .  NIA )      GO TO 8010
      IF ( NLINE+NIBA+9 . GT . LINES )           THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 3
        END IF
      END IF
      WRITE (6,601) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
C-----READ LIST OF INBOUND APPROACHES
      READ (NIN,501) (LIBA(IAN),IAN=1,NIBA)
      WRITE (6,602)  (LIBA(IAN),IAN=1,NIBA)
      NLINE = NLINE + NIBA
      DO 1020  IAN = 1 , NIBA
                    IF ( LIBA(IAN) . LE . 0   )  GO TO 8020
                    IF ( LIBA(IAN) . GT . NAP )  GO TO 8020
        IA        = LIBA(IAN)
        LIBAR(IA) = IAN
                    IF ( NIBA . EQ . 1    )      GO TO 1020
                    IF ( IAN  . EQ . NIBA )      GO TO 1020
C-----CHECK IF APPROACH IS DUPLICATED ON LIST OF INBOUND APPROACHES
      DO 1010  JAN = IAN+1 , NIBA
                    IF ( LIBA(IAN).EQ.LIBA(JAN) )GO TO 8030
 1010 CONTINUE
 1020 CONTINUE
      WRITE (6,603) NIBA
      NLINE = NLINE + 6
C-----READ NUMBER OF OUTBOUND APPROACHES
      READ (NIN,501) NOBA,NLEGS,NFUT
                    IF ( NOBA . LE .  0   )      GO TO 8040
                    IF ( NOBA . GT .  NOA )      GO TO 8040
      IF ( NLINE+NOBA+13 . GT . LINES )          THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 3
        END IF
      END IF
      WRITE (6,604) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
C-----READ LIST OF OUTBOUND APPROACHES
      READ (NIN,501) (LOBA(IAN),IAN=1,NOBA)
      WRITE (6,605)  (LOBA(IAN),IAN=1,NOBA)
      NLINE = NLINE + NOBA
      DO 1040  IAN = 1 , NOBA
                    IF ( LOBA(IAN) . LE . 0   )  GO TO 8050
                    IF ( LOBA(IAN) . GT . NAP )  GO TO 8050
        IA        = LOBA(IAN)
        LOBAR(IA) = IAN
                    IF ( NOBA . EQ . 1    )      GO TO 1040
                    IF ( IAN  . EQ . NOBA )      GO TO 1040
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
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 8010 CONTINUE
      WRITE (ERRMSG,801) NIBA,NIA
      CALL  ABORTR ( ERRMSG )
      STOP  801
 8020 CONTINUE
      WRITE (ERRMSG,802) IAN,LIBA(IAN),NAP
      CALL  ABORTR ( ERRMSG )
      STOP  802
 8030 CONTINUE
      WRITE (ERRMSG,803) IAN,LIBA(IAN),JAN,LIBA(JAN)
      CALL  ABORTR ( ERRMSG )
      STOP  803
 8040 CONTINUE
      WRITE (ERRMSG,804) NOBA,NOA
      CALL  ABORTR ( ERRMSG )
      STOP  804
 8050 CONTINUE
      WRITE (ERRMSG,805) IAN,LOBA(IAN),NAP
      CALL  ABORTR ( ERRMSG )
      STOP  805
 8060 CONTINUE
      WRITE (ERRMSG,806) IAN,LOBA(IAN),JAN,LOBA(JAN)
      CALL  ABORTR ( ERRMSG )
      STOP  806
 8070 CONTINUE
      WRITE (ERRMSG,807) IAN,LIBA(IAN),JAN,LOBA(JAN)
      CALL  ABORTR ( ERRMSG )
      STOP  807
      END                                                               READIO
C
C
C
      SUBROUTINE READOP ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHTEX'
      INCLUDE 'CLASS'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'OUTPUT'
      INTEGER           NIN
  501 FORMAT(2I4,F4.1,2I4,2F4.0,A3,F5.2,A3,I2)
  601 FORMAT(///,12X,47HTOTAL NUMBER OF INBOUND AND OUTBOUND APPROACHES,
     *       3H = ,I2)
  602 FORMAT(8X,5HTABLE,I3,37H  -  DRIVER-VEHICLE PROCESSOR OPTIONS,///,
     *       12X,39HTIME FOR GENERATING VEHICLES (MIN) ----,I5,/,
     *       12X,39HMINIMUM HEADWAY FOR VEHICLES (SEC) ----,F7.1,/,
     *       12X,39HNUMBER OF VEHICLE CLASSES -------------,I5,/,
     *       12X,39HNUMBER OF DRIVER CLASSES --------------,I5,/,
     *       12X,39HPERCENT OF LEFT  TURNS IN MEDIAN LANE -,F6.0,/,
     *       12X,39HPERCENT OF RIGHT TURNS IN CURB   LANE -,F6.0)
  808 FORMAT('STOP 808 - ',
     *       'NUMBER OF APPROACHES = ',I3,' IS LT 2 OR GT ',I2,
     *       ' - READIO')
  809 FORMAT('STOP 809 - ',
     *       'NUMBER OF INBOUND APPROACHES PLUS NUMBER OF OUTBOUND ',
     *       'APPROACHES = ',I3,' IS NE NUMBER OF APPROACHES = ',I3,
     *       ' - READOP')
  810 FORMAT('STOP 810 - ',
     *       'TIME FOR GENERATING VEHICLES =',I3,' IS LT 12 OR GT',I4,
     *       ' - READOP')
  811 FORMAT('STOP 811 - ',
     *       'MINIMUN HEADWAY BETWEEN VEHICLES =',F4.1,' IS LT 1.0 OR ',
     *       'GT 5.0 - READOP')
  812 FORMAT('STOP 812 - ',
     *       'NUMBER OF VEHICLE CLASSES =',I3,' IS LT',I3,' OR GT',I3,
     *       ' - READOP')
  813 FORMAT('STOP 813 - ',
     *       'NUMBER OF DRIVER CLASSES =',I3,7H IS LT',I2,' OR GT',I2,
     *       ' - READOP')
  814 FORMAT('STOP 814 - ',
     *       'PERCENT OF LEFT TURNS IN MEDIAN LANE =',F7.1,' IS LT 50.'
     *       '0 OR GT 100.0',
     *       ' - READOP')
  815 FORMAT('STOP 815 - ',
     *       'PERCENT OF RIGHT TURNS IN CURB LANE =',F7.1,' IS LT 50.0',
     *       ' OR GT 100.0',
     *       ' - READOP')
  887 FORMAT('STOP 887 - ',
     *       'NUMBER OF VEHICLE ATTRIBUTES (NVEHAT) =',I3,' IS NOT 6',
     *       ' OR 10, INTERNAL DATA INCONSISTENCY -',
     *       ' MIX OF OLD AND NEW INPUT FILES',
     *       ' - READOP')
C
C-----SUBROUTINE READOP READS THE NUMBER OF APPROACHES AND THE DRIVER-
C-----VEHICLE PROCESSOR OPTIONS AND CHECKS FOR ERRORS
C
C-----READ NUMBER OF APPROACHES AND DRIVER-VEHICLE PROCESSOR OPTIONS
      READ (NIN,501) NAPS,ITSIM,HMIN,NVEHCL,NDRICL,FPERL(1,1),
     *               FPERR(1,1),PLTOPT,PLTSIZ,IMAGOP,NVEHAT
                    IF ( ITSIM      . EQ . 0 )   ITSIM      = 20
                    IF ( HMIN       . EQ . 0.0D0)HMIN       = 1.0D0
                    IF ( NVEHCL     . EQ . 0 )   NVEHCL     = NVCD
                    IF ( NDRICL     . EQ . 0 )   NDRICL     = NDCD
                    IF ( FPERL(1,1) . LE . 0.0 ) FPERL(1,1) = 80.0
                    IF ( FPERR(1,1) . LE . 0.0 ) FPERR(1,1) = 80.0
                    IF ( IMAGOP     . NE . CYES )IMAGOP     = CNO
                    IF ( NVEHAT     . EQ . 0    )NVEHAT     = 6
      DO  IAN = 2 , NIA
        FPERL(1,IAN) = FPERL(1,1)
        FPERR(1,IAN) = FPERR(1,1)
      END DO
C-----ECHO-PRINT VALUES
      WRITE (6,601) NAPS
      NLINE = NLINE + 7
      IF ( NLINE+12 . GT . LINES )               THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 3
        END IF
      END IF
      WRITE (6,602) NTABL,ITSIM,HMIN,NVEHCL,NDRICL,FPERL(1,1),FPERR(1,1)
      NLINE = NLINE + 12
      NTABL = NTABL + 1
C-----CHECK FOR ERRORS
                    IF ( NAPS      .LT.    2   ) GO TO 8080
                    IF ( NAPS      .GT. NAP    ) GO TO 8080
                    IF ( NIBA+NOBA .NE. NAPS   ) GO TO 8090
                    IF ( ITSIM     .LT.   12   ) GO TO 8100
                    IF ( ITSIM     .GT. MSTMIN ) GO TO 8100
                    IF ( HMIN      .LT.    1.0 ) GO TO 8110
                    IF ( HMIN      .GT.    5.0 ) GO TO 8110
                    IF ( NVEHCL    .LT. NVCD   ) GO TO 8120
                    IF ( NVEHCL    .GT. NVC    ) GO TO 8120
                    IF ( NDRICL    .LT. NDCD   ) GO TO 8130
                    IF ( NDRICL    .GT. NDC    ) GO TO 8130
                    IF ( FPERL(1,1).LT.   50.0 ) GO TO 8140
                    IF ( FPERL(1,1).GT.  100.0 ) GO TO 8140
                    IF ( FPERR(1,1).LT.   50.0 ) GO TO 8150
                    IF ( FPERR(1,1).GT.  100.0 ) GO TO 8150
      IF ( NVEHAT.NE.6 . AND . NVEHAT.NE.10    ) GO TO 8870
      SIMTIM = ITSIM*60
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 8080 CONTINUE
      WRITE (ERRMSG,808) NAPS,NAP
      CALL  ABORTR ( ERRMSG )
      STOP  808
 8090 CONTINUE
      WRITE (ERRMSG,809) NIBA+NOBA,NAPS
      CALL  ABORTR ( ERRMSG )
      STOP  809
 8100 CONTINUE
      WRITE (ERRMSG,810) ITSIM,MSTMIN
      CALL  ABORTR ( ERRMSG )
      STOP  810
 8110 CONTINUE
      WRITE (ERRMSG,811) HMIN
      CALL  ABORTR ( ERRMSG )
      STOP  811
 8120 CONTINUE
      WRITE (ERRMSG,812) NVEHCL,NVCD,NVC
      CALL  ABORTR ( ERRMSG )
      STOP  812
 8130 CONTINUE
      WRITE (ERRMSG,813) NDRICL,NDCD,NDC
      CALL  ABORTR ( ERRMSG )
      STOP  813
 8140 CONTINUE
      WRITE (ERRMSG,814) FPERL(1,1)
      CALL  ABORTR ( ERRMSG )
      STOP  814
 8150 CONTINUE
      WRITE (ERRMSG,815) FPERR(1,1)
      CALL  ABORTR ( ERRMSG )
      STOP  815
 8870 CONTINUE
      WRITE (ERRMSG,887) NVEHAT
      CALL  ABORTR ( ERRMSG )
      STOP  887
      END                                                               READOP
C
C
C
      SUBROUTINE READAP ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHTEX'
      INCLUDE 'CLASS'
      INCLUDE 'CONSTN'
      INCLUDE 'CURAND'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LITCON'
      INCLUDE 'OUTPUT'
      INTEGER           CWARNC
      PARAMETER       ( CWARNC = 52 )
      DIMENSION         IUSEED(NAL),IUSED(NAP),YPERT(NOA)
      INTEGER           IUSEED,IUSED,IVTP,K
      REAL              YPERT
      CHARACTER*1       JSFLAG
      CHARACTER*3       IYES
      CHARACTER*7       CDIST
      CHARACTER*13      CMORC
      CHARACTER*21      CPDIST
      CHARACTER*(CWARNC)CWARN
      INTEGER           I,IZ,J,JDIST,JVOL,KGEOM,KVOL,LPDIST,LTEST,LWARN,
     1                  MDEGST,MDEGUT,NIN,WIDLMN,WIDLMX,IC1,LEG
      LOGICAL           ANYDEF,ANYFUT
      REAL              DPERL,FUTPER,PDIST,SUM,X85PER,XMEANS
  501 FORMAT(5I4,2I2,2I3,A1,A7,I5,F6.2,2F5.1,6F3.0,A3,F3.0)
  502 FORMAT(15F5.1)
  503 FORMAT(5I4,1X,4A1,I5,F4.0,4A1,2X,5I4,1X,4A1,I5,F4.0,4A1)
  504 FORMAT(A)
  505 FORMAT(I3,9F3.0,1X,A7,I5,F6.2,2F5.1,6F3.0,A3)
  601 FORMAT(8X,5HTABLE,I3,26H  -  LISTING OF APPROACHES,//)
  602 FORMAT(11X,39HAPPROACH NUMBER -----------------------,I5,3X,A,/,
     *       11X,39HAPPROACH AZIMUTH ----------------------,I5,/,
     *       11X,39HNUMBER OF DEGREES FOR STRAIGHT --------,I5,/,
     *       11X,39HNUMBER OF LANES -----------------------,I5,:,/,
     *       11X,39HNUMBER OF VARYING TRAFFIC PERIODS -----,I5)
  603 FORMAT(13H  PARAMETER =,F8.2)
  604 FORMAT(11X,39HHEADWAY DISTRIBUTION NAME -------------,1X,A7,A)
  605 FORMAT(11X,39HEQUIVALENT HOURLY VOLUME (VPH) --------,I5,:,/,
     *       11X,39HAPPROACH MEAN SPEED (MPH) -------------,F7.1,/,
     *       11X,39HAPPROACH 85 PERCENTILE SPEED (MPH) ----,F7.1)
  606 FORMAT(11X,39H    OUTBOUND APPROACH NUMBER ----------,8I5)
  607 FORMAT(11X,39HPERCENT GOING TO OUTBOUND APPROACHES --,1X,8F5.0)
  608 FORMAT(11X,39HUSER SUPPLIED PERCENT OF VEHICLES -----,3X,A)
  609 FORMAT(11X,39HPERCENT USING FREE U-TURN AT DIAMOND --,F6.0)
  610 FORMAT(11X,39H    VEHICLE CLASS NUMBER --------------,16I5,
     *       6(:,/,50X,16I5))
  611 FORMAT(11X,39HUSER SUPPLIED PERCENT OF VEHICLES -----,2X,16F5.1,
     *       6(:,/,50X,2X,16F5.1))
  612 FORMAT(11X,39HPROGRAM SUPPLIED PERCENT OF VEHICLES --,2X,16F5.1,
     *       6(:,/,50X,2X,16F5.1))
  613 FORMAT(11X,39HSEED FOR RANDOM NUMBERS ---------------,I8)
  614 FORMAT(11X,35HPERCENT OF TRAFFIC ENTERING ON LANE,I2,2H =,I5,
     *       3X,A,1X,A)
  615 FORMAT(11X,29HTOTAL NUMBER OF APPROACHES = ,I2)
C 616 FORMAT(11X,/)
  617 FORMAT(11X,26HALLOWED TURN CODES ON LANE,I2,11H ----------,2X,4A1,
     *       2X,A)
  618 FORMAT(11X,26HALLOWED LANE USE   ON LANE,I2,11H ----------,2X,4A1,
     *       2X,A)
  620 FORMAT( 8X,45H**VARYING TRAFFIC PERIOD (VTP) INFORMATION:**,/,
     *       10X,51HPERIOD  LENGTH  % LEFT  %RIGHT  % FREE  %ENTERING L,
     *           51HANE NO.  HEADWAY  VOLUM  HEADW   MEAN    85%  % TO ,
     *           19HOUTBOUND APPR  USER,/,
     *       10X,40HNUMBER  (MIN.)  - MOST  - MOST  U-TURN  ,6I3,
     *           39H  DISTRIB  (VPH)  PARAM  SPEED  SPEED  ,6I3,
     *            6H  VEH%)
  621 FORMAT(7X,'VTP',I6,4I8,2X,6I3,2X,A7,I7,F7.2,2F7.1,2X,6I3,3X,A3)
  814 FORMAT('STOP 814 - ',
     *       'PERCENT OF LEFT TURNS IN MEDIAN LANE =',F7.1,
     *       ' IS LT 50.0 OR GT 100.0 - READAP')
  815 FORMAT('STOP 815 - ',
     *       'PERCENT OF RIGHT TURNS IN CURB LANE =',F7.1,
     *       ' IS LT 50.0 OR GT 100.0 - READAP')
  816 FORMAT('STOP 816 - APPROACH NUMBER',I3,' IS LE 0 OR GT ',I2,
     *       ' - READAP')
  817 FORMAT('STOP 817 - APPROACH NUMBER',I3,' IS USED MORE THAN ONCE',
     *       ' - READAP')
  818 FORMAT('STOP 818 - ',
     *       'APPROACH NUMBER',I3,' AZIMUTH =',I4,' IS LT 0 OR GT 360',
     *       ' - READAP')
  819 FORMAT('STOP 819 - ',
     *       'APPROACH NUMBER',I3,' NUMBER OF LANES =',I2,
     *       'IS LE 0 OR GT',I2,' - READAP')
  820 FORMAT('STOP 820 - ',
     *       'APPROACH NUMBER',I3,'IS NOT ON INBOUND OR OUTBOUND LISTS',
     *       ' - READAP')
  821 FORMAT('STOP 821 - ',
     *       'APPROACH NUMBER',I3,' NUMBER OF DEGREES FOR STRAIGHT =',
     *        I4,' IS LT 0 OR GT 45 - READAP')
  822 FORMAT('STOP 822 - ',
     *       'APPROACH NUMBER',I3,' HEADWAY DISTRIBUTION NAME (',A,
     *       ') IS NOT (CONSTAN)OR(ERLANG )OR(GAMMA  )OR(LOGNRM',
     *       'L)OR',/,63X,'(NEGEXP )OR(SNEGEXP)OR(UNIFORM) - READAP')
c  823 FORMAT('STOP 823 - ',
c     *       'APPROACH NUMBER',I3,' HAS ZERO VOLUME WITH A VALID DIST',
c     *       'RIBUTION NAME - READAP')
  824 FORMAT('STOP 824 - ',
     *       'APPROACH NUMBER',I3,' PARAMETER FOR DISTRIBUTION =',F7.2,
     *       ' IS LE 0.0 - READAP')
  825 FORMAT('STOP 825 - ',
     *       'APPROACH NUMBER',I3,' PARAMETER FOR ERLANG DISTRIBUTION ',
     *       '=',F7.2,' IS NOT AN INTEGER VALUE - READAP')
  826 FORMAT('STOP 826 - ',
     *       'APPROACH NUMBER',I3,' PARAMETER FOR GAMMA DISTRIBUTION =',
     *        F7.2,' IS LT 1.0 - READAP')
  827 FORMAT('STOP 827 - ',
     *       'APPROACH NUMBER',I3,' PARAMETER FOR SHIFTED NEGATIVE EXP',
     *       'ONENTIAL DISTRIBUTION =',F7.2,' IS GE MEAN HEADWAY =',
     *        F7.2,' - READAP')
  828 FORMAT('STOP 828 - ',
     *       'APPROACH NUMBER',I3,' EQUIVALENT HOURLY VOLUME =',I5,
     *       ' IS LT 0 OR GT',I5,' - READAP')
  829 FORMAT('STOP 829 - ',
     *       'APPROACH NUMBER',I3,' APPROACH MEAN SPEED =',F6.1,
     *       ' IS LE 10.0 OR GT 80.0 MPH - READAP')
  830 FORMAT('STOP 830 - ',
     *       'APPROACH NUMBER',I3,' APPROACH 85 PERCENTILE SPEED =',
     *        F6.1,' IS LT APPROACH MEAN SPEED =',F6.1,' OR GT 90.0',
     *       ' - READAP')
  831 FORMAT('STOP 831 - ',
     *       'APPROACH NUMBER',I3,' APPROACH TURNING PERCENTAGES SUM =',
     *        F6.1,' IS NOT 100.0 - READAP')
  832 FORMAT('STOP 832 - ',
     *       'APPROACH NUMBER',I3,' USER SUPPLIED PERCENT OF VEHICLES ',
     *       'OPTION = (',A,') IS NOT (YES)OR(NO ) - READAP')
  833 FORMAT('STOP 833 - ',
     *       'APPROACH NUMBER',I3,' NUMBER OF VEHICLE CLASSES =',I3,
     *       ' IS NOT',I3,' WHEN ASKING FOR PROGRAM SUPPLIED PERCENT ',
     *       'OF VEHICLES IN TRAFFIC STREAM - READAP')
  834 FORMAT('STOP 834 - ',
     *       'APPROACH NUMBER',I3,' USER SUPPLIED PERCENT OF VEHICLES ',
     *       'MAKING UP THE TRAFFIC STREAM SUM =',F6.1,' IS NOT 100.0',
     *       ' - READAP')
  835 FORMAT('STOP 835 - ',
     *       'APPROACH NUMBER',I3,' LANE',I2,' DOES NOT START AT THE S',
     *       'AME LGEOM(1) AS THE FIRST LANE (',I4,') - READAP')
  836 FORMAT('STOP 836 - ',
     *       'APPROACH NUMBER',I3,' HAS VEHICLES ENTERING ON LANE NUMB',
     *       'ER',I2,' THAT DOES NOT EXIST AT THE BEGINNING OF THE APP',
     *       'ROACH - READAP')
  837 FORMAT('STOP 837 - ',
     *       'APPROACH NUMBER',I3,' PERCENT OF VEHICLES IN EACH LANE S',
     *       'UM =',F6.1,' IS NOT 100.0',
     *       ' - READAP')
  838 FORMAT('STOP 838 - ',
     *       'APPROACH NUMBER',I3,' HAS A MEAN SPEED =',F7.1,' AND A 8',
     *       '5 PERCENTILE SPEED =',F7.1,/,'WHICH GIVES ONE STANDARD D',
     *       'EVIATION =',F7.1,' WHICH IS GREATER THAN THE MEAN',
     *       ' - READAP')
  839 FORMAT('STOP 839 - ',
     *       'APPROACH NUMBER',I3,' ON OUTBOUND LIST YET HAS INBOUND D',
     *       'ATA SPECIFIED - READAP')
  840 FORMAT('STOP 840 - ',
     *       'APPROACH NUMBER,I3,32H IS ON OUTBOUND LIST YET HAS PER,
     *       52HCENT OF EACH VEHICLE CLASS MAKING THE TRAFFIC STREAM',
     *       ' - READAP')
  841 FORMAT('STOP 841 - APPROACH NUMBER,I3,29H HAS NO INFORMATION SPE',
     *       'CIFIED - READAP')
  868 FORMAT('STOP 868 - ',
     *       'APPROACH NUMBER',I3,' PERCENT USING FREE U-TURN AT DIAMO',
     *       'ND = ',F7.3,' IS LT 0.0 OR GT 100.0 - READAP')
  869 FORMAT('STOP 869 - ',
     *       'APPROACH NUMBER',I3,' SIDE FLAG = (',A1,') IS NOT (L) OR',
     *       ' (R) FOR DIAMOND - READAP')
  870 FORMAT('STOP 870 - ',
     *       'LEFT OR RIGHT SIDE NOT SPECIFIED FOR ALL APPROACHES FOR ',
     *       'DIAMOND WITH U-TURNS - READAP')
  871 FORMAT('STOP 871 - ',
     *       'NUMBER OF VARYING TRAFFIC PERIODS =',I3,' IS LT 0 OR GT ',
     *        I2,' - READAP')
  872 FORMAT('STOP 872 - ',
     *       'NUMBER OF VARYING TRAFFIC PERIODS =',I3,' WHICH SHOULD B',
     *       'E  0 (ZERO) FOR APPROACH',I3,' - READAP')
  873 FORMAT('STOP 873 - ',
     *       'LENGTH OF VARYING TRAFFIC PERIOD NUMBER',I3,' = ',I3,
     *       ' IS LT 5 OR GT',I4,' - READAP')
  874 FORMAT('STOP 874 - ',
     *       'LENGTH OF VARYING TRAFFIC PERIOD NUMBER',I3,' = ',I3,
     *       ' AND LENGTHS SUMMED ARE GT',I4,' - READAP')
  876 FORMAT('STOP 876 - ',
     *       'APPROACH',I3,' LANE',I2,' DOES NOT HAVE ANY TURN CODE ',
     *       'DESIGNATION - READAP')
  877 FORMAT('STOP 877 - ',
     *       'APPROACH',I3,' LANE',I2,' HAS TURN CODE DESIGNATION <',A1,
     *       '> (FOR U TURN) WHICH IS NOT A <U> OR < > - READAP')
  878 FORMAT('STOP 878 - ',
     *       'APPROACH',I3,' LANE',I2,' HAS TURN CODE DESIGNATION <',A1,
     *       '> (FOR LEFT TURN) WHICH IS NOT A <L> OR < > - READAP')
  879 FORMAT('STOP 879 - ',
     *       'APPROACH',I3,' LANE',I2,' HAS TURN CODE DESIGNATION <',A1,
     *       '> (FOR STRAIGHT) WHICH IS NOT A <S> OR < > - READAP')
  880 FORMAT('STOP 880 - ',
     *       'APPROACH',I3,' LANE',I2,' HAS TURN CODE DESIGNATION <',A1,
     *       '> (FOR RIGHT TURN) WHICH IS NOT A <R> OR < > - READAP')
  881 FORMAT('STOP 881 - ',
     *       'APPROACH',I3,' LANE',I2,' HAS LANE USE DESIGNATION <',A1,
     *       '> (FOR BICYCLE) WHICH IS NOT A <B> OR < > - READAP')
  882 FORMAT('STOP 882 - ',
     *       'APPROACH',I3,' LANE',I2,' HAS LANE USE DESIGNATION <',A1,
     *       '> (FOR EMERGENCY) WHICH IS NOT A <E> OR < > - READAP')
  883 FORMAT('STOP 883 - ',
     *       'APPROACH',I3,' LANE',I2,' HAS LANE USE DESIGNATION <',A1,
     *       '> (FOR RAIL) WHICH IS NOT A <R> OR < > - READAP')
  884 FORMAT('STOP 884 - ',
     *       'APPROACH',I3,' LANE',I2,' HAS LANE USE DESIGNATION <',A1,
     *       '> (FOR VEHICLE) WHICH IS NOT A <V> OR < > - READAP')
  885 FORMAT('STOP 885 - ',
     *       'APPROACH NUMBER',I3,' LANE NUMBER',I3,' LANE WIDTH =',I3,
     *       ' IS LT',I2,' OR GT',I3,' - READAP')
C 
C-----SUBROUTINE READAP READS THE APPROACH INFORMATION AND CHECKS FOR
C-----ERRORS
C
      CWARN = ' WARNING - THIS LANE WILL HAVE NO GENERATED VEHICLES'
      IF ( NLINE+2*INT( (NVEHCL+15)/16 )+23 . GT . LINES )
     *                                           THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 3
        END IF
      END IF
      WRITE (6,601) NTABL
      NLINE = NLINE + 3
      NTABL = NTABL + 1
      DO 1010  I = 1 , NAP
      IUSED(I) = 0
 1010 CONTINUE
      IL = 0
      ANYDEF = .FALSE.
      ANYFUT = .FALSE.
C-----READ INFORMATION FOR EACH APPROACH
      DO 4040  I = 1 , NAPS
C-----READ REGULAR (OLD ONE TIME PERIOD TRAFFIC) INTO FIRST VARYING
C-----TRAFFIC (TIME) PERIOD
      IVTP = 1
C-----READ APPROACH INFORMATION
      DO 1015  J = 7 , NOA
      YPERT(J) = 0.0
 1015 CONTINUE
      READ (NIN,501) IA,IAAZIM(IA),IAPX(IA),IAPY(IA),ISLIM(IA),NVTP(IA),
     *               NLANES(IA),MDEGST,MDEGUT,JSFLAG,CDIST,JVOL,PDIST,
     *               XMEANS,X85PER,(YPERT(J),J=1,6),IYES,FUTPER
                    IF ( MDEGST . EQ . 0 )       MDEGST = 20
                    IF ( MDEGUT . EQ . 0 )       MDEGUT = 10
                    IF ( IYES . EQ . ICBL )      IYES = CNO
C-----2 BLANK LINES
C-----APPROACH NUMBER
C-----APPROACH AZIMUTH
C-----NUMBER OF DEGREES FOR STRAIGHT
C-----NUMBER OF LANES
C-----ALLOWED TURN CODES ON LANE
C-----ALLOWED LANE USE ON LANE
      LTEST = NLINE + 6 + 2*NLANES(IA)
      DO 1020  IAN = 1 , NIBA
      CNOTE(1:) = '[LEG XX, '
      IC1 = 10
      IF ( IA . EQ . LIBA(IAN) )                 THEN
        CNOTE(IC1:) = 'INBOUND LANE(S), '
        IC1 = IC1 + 17
C-----  NOT ALL DIAMOND INTERCHANGE IAs ON LIBA HAVE GENERATED TRAFFIC
        IF ( DIAMON )                            THEN
C 
C               Diamond Interchange Numbering Convention
C 
C        |               /|\                |              |
C        6                1                 |              |
C       \|/               |               6 | 4          1 | 2
C <-5->     <-7->  <-0->     <-2->      ----|--------------|----
C        |               /|\              5 | 4          1 | 3
C        4                3                 |              |
C       \|/               |                 |              |
C            Leg Numbers               Curb Return Radius Numbers
C 
C 
C      | /|\           | /|\                 |     FUT 2    /|\
C      8 16            1  9                  8 -16->   -6--> 9
C     \|/ |           \|/ |                 \|/              |
C <-15-    <--5-  <-12-    <-2--      <-15-    <--5-  <-12-    <-2--
C --7->    -13->  --4->    -10->      --7->    -13->  --4->    -10->
C      | /|\           | /|\                 |              /|\
C     14  6           11  3                 14 <--1-   <-11- 3
C     \|/ |           \|/ |                 \|/    FUT 1     |
C         Approach Numbers                    Approach Numbers
C      (without free u-turns)               (with free u-turns)     
C
C-----    IA = 4 = INTERNAL APPROACH TO RIGHT SIDE OF DIAMOND; LEG 0
          IF ( IA . EQ . 4 )                     THEN
            CNOTE(IC1:) = 'INTERNAL TO RIGHT SIDE OF DIAMOND )'
            IC1 = IC1 + 35
            GO TO 1020
          END IF
C-----    IA = 5 = INTERNAL APPROACH TO LEFT  SIDE OF DIAMOND; LEG 7
          IF ( IA . EQ . 5 )                     THEN
            CNOTE(IC1:) = 'INTERNAL TO LEFT SIDE OF DIAMOND )'
            IC1 = IC1 + 34
            GO TO 1020
          END IF
C-----    NFUT = 0 = NO  FREE UTURNS
C-----    NFUT = 1 = ONE FREE UTURN , FUT#1   (IA=1)   ON DIAMOND; LEG 3
C-----    NFUT = 2 = ONE FREE UTURN , FUT#2   (IA=6)   ON DIAMOND; LEG 6
C-----    NFUT = 3 = TWO FREE UTURNS, FUT#1&2 (IA=1&6) ON DIAMOND
          IF ( NFUT . EQ . 1 .AND. IA . EQ . 1 ) THEN
            CNOTE(IC1:) = 'ONE FREE UTURN, FUT#1 ON DIAMOND)'
            IC1 = IC1 + 33
            GO TO 1020
          END IF
          IF ( NFUT . EQ . 2 .AND. IA . EQ . 6 ) THEN
            CNOTE(IC1:) = 'ONE FREE UTURN, FUT#2 ON DIAMOND)'
            IC1 = IC1 + 33
            GO TO 1020
          END IF
          IF ( NFUT . EQ . 3 .AND. 
     *       ( IA   . EQ . 1 .OR . IA . EQ . 6 ))THEN
            CNOTE(IC1:) = 'TWO FREE UTURNS, FUT#1&2 ON DIAMOND)'
            IC1 = IC1 + 36
            GO TO 1020
          END IF
          IF ( JSFLAG . EQ . 'R' )               THEN
            CNOTE(IC1:) = 'TO RIGHT SIDE OF DIAMOND)'
            IC1 = IC1 + 25
          END IF
          IF ( JSFLAG . EQ . 'L' )               THEN
            CNOTE(IC1:) = 'TO LEFT SIDE OF DIAMOND)'
            IC1 = IC1 + 24
          END IF
          IF ( ( (IA.EQ.1) . AND . (IAND( NFUT,1 ).EQ.0) ) . OR .
     *         (  IA.EQ.2                                ) . OR .
     *         (  IA.EQ.3                                ) . OR .
     *         ( (IA.EQ.6) . AND . (IAND( NFUT,2 ).EQ.0) ) . OR .
     *         (  IA.EQ.7                                ) . OR .
     *         (  IA.EQ.8                                ) )
     *                                           THEN
            IBAP(IA) = .TRUE.
          ELSE
            IBAP(IA) = .FALSE.
          END IF
        ELSE
C-----    IBAP=TRUE MEANS TRAFFIC ENTERS THE SIMULATION ON THIS APPROACH
C-----    IE - IT IS A TRUE INBOUND APPROACH, NOT AN INTERNAL APPROACH
          IBAP(IA) = .TRUE.
C       END OF IF DIAMON IS NEXT LINE
        END IF
C-----  NUMBER OF VARYING TRAFFIC PERIODS
C-----  EQUIVALENT HOURLY VOLUME
        LTEST = LTEST + 2
        IF ( JVOL . GT . 0 )                     THEN
C-----    HEADWAY DISTRIBUTION NAME
C-----    APPROACH MEAN SPEED
C-----    APPROACH 85 PERCENTILE SPEED
C-----        OUTBOUND APPROACH NUMBER
C-----    PERCENT GOING TO OUTBOUND APPROACHES
C-----    USER SUPPLIED PERCENT OF VEHICLES
C-----        VEHICLE CLASS NUMBER
C-----    USER SUPPLIED PERCENT OF VEHICLES
C-----    SEED FOR RANDOM NUMBERS
C-----    PERCENT OF TRAFFIC ENTERING ON LANE 
          LTEST = LTEST + 7 + 2*INT( (NVEHCL+15)/16 ) + NLANES(IA)
        END IF
        EXIT
      ELSE
        CNOTE(IC1:) = 'OUTBOUND LANE(S), '
        IC1 = IC1 + 18
        LEG = IA
C     END OF IF LIBA IS NEXT LINE
      END IF
 1020 CONTINUE
      IF ( DIAMON )                            THEN
        IF ( IA .EQ. 1 . OR . IA .EQ.  9 ) LEG = 1
        IF ( IA .EQ. 2 . OR . IA .EQ. 10 ) LEG = 2
        IF ( IA .EQ. 3 . OR . IA .EQ. 11 ) LEG = 3
        IF ( IA .EQ. 4 . OR . IA .EQ. 12 ) LEG = 0
        IF ( IA .EQ. 5 . OR . IA .EQ. 13 ) LEG = 7
        IF ( IA .EQ. 6 . OR . IA .EQ. 14 ) LEG = 4
        IF ( IA .EQ. 7 . OR . IA .EQ. 15 ) LEG = 5
        IF ( IA .EQ. 8 . OR . IA .EQ. 16 ) LEG = 6
      ELSE
C-----TEMP DESIGNATION FOR NON-DIA LEG
        DO  J = 1 , NIBA
          IF ( IA.EQ.J . OR . IA.EQ.(J+NIBA) )   LEG = J
        END DO
      END IF
      IC1 = MAX0( IC1-2,1)
      WRITE(CNOTE(5:7),505) LEG
      CNOTE(IC1:) = ']'
                    IF ( I . EQ . NAPS )         LTEST = LTEST + 3
      IF ( LTEST . GT . LINES )                  THEN
        CALL  HEADER
      ELSE
        IF ( ( I     . GT . 1      ) . AND .
     *       ( NLINE . GT . NLINEH ) )           THEN
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 2
        END IF
      END IF
C-----ECHO-PRINT OF DATA
      IF ( IBAP(IA) )                            THEN
        WRITE (6,602) IA,CNOTE(1:IC1),IAAZIM(IA),MDEGST,NLANES(IA),
     *                NVTP(IA)
        NLINE = NLINE + 5
      ELSE
        WRITE (6,602) IA,CNOTE(1:IC1),IAAZIM(IA),MDEGST,NLANES(IA)
        NLINE = NLINE + 4
      END IF
                    IF ( IA         . EQ .   0 ) GO TO 8160
                    IF ( IABS( IA ) . GT . NAP ) GO TO 8160
                    IF ( IUSED (IA) . NE .   0 ) GO TO 8170
                    IF ( IAAZIM(IA) . LT .   0 ) GO TO 8180
                    IF ( IAAZIM(IA) . GT . 360 ) GO TO 8180
                    IF ( MDEGST     . LT .   0 ) GO TO 8210
                    IF ( MDEGST     . GT .  45 ) GO TO 8210
                    IF ( NLANES(IA) . LE .   0 ) GO TO 8190
                    IF ( NLANES(IA) . GT . NAL ) GO TO 8190
                    IF ( NVTP  (IA) . LT .   0 ) GO TO 8710
                    IF ( NVTP  (IA) . GT . NVT ) GO TO 8710
C-----ADD CHECKS FOR DIAMOND INTERNAL APPROACHES HAVING NORMAL INBOUND
C-----APPROACH TRAFFIC DATA
                    IF ( IBAP(IA) )              GO TO 1025
                    IF ( NVTP(IA)   . NE .   0 ) GO TO 8720
 1025 CONTINUE
C-----CHECK IF APPROACH IS ON LIST OF INBOUND APPROACHES
      DO 1030  IAN = 1 , NIBA
                    IF ( IA . EQ . LIBA(IAN) )   GO TO 2010
 1030 CONTINUE
C-----CHECK IF APPROACH IS ON LIST OF OUTBOUND APPROACHES
      DO 1040  IAN = 1 , NOBA
                    IF ( IA . EQ . LOBA(IAN) )   GO TO 4010
 1040 CONTINUE
      GO TO 8200
 2010 CONTINUE
C
C-----APPROACH IS INBOUND
C
      IF ( CDIST . EQ . CDISTN(1) . OR .
     *     CDIST . EQ . CDISTN(5) )              THEN
C-------DISTRIBUTION DOES NOT HAVE A PARAMETER, SET LENGTH TO 1
        CPDIST = ICBL
        LPDIST = 1
      ELSE
        WRITE (CPDIST,603) PDIST
        LPDIST = 21
      END IF
      IF ( CDIST . NE . 'XXXXXXX' )              THEN
        IF ( JVOL . GT . 0 )                     THEN
          WRITE (6,604) CDIST,CPDIST(1:LPDIST)
          NLINE = NLINE + 1
        END IF
      END IF
      JDIST=0
      IF ( CDIST . EQ . 'XXXXXXX' )              THEN
C-----INTERNAL APPROACH, DON'T GENERATE TRAFFIC FOR THIS APPROACH
        JDIST=-1
        GO TO 2040
      END IF
C-----BLANK DISTRIBUTION NAME IS ONLY VALID WITH 0 VOLUME
      IF ( (CDIST.EQ.ICBL) .AND. (JVOL.EQ.0) )   GO TO 2040
      DO 2020  J = 1 , 7
      JDIST=J
      IF ( CDIST . EQ . CDISTN(J) )              GO TO 2030
 2020 CONTINUE
      GO TO 8220
 2030 CONTINUE
      IF ( JVOL . GT . 0 )                       GO TO 2080
 2040 CONTINUE
      JVOL = 0
      WRITE (6,605) JVOL
      NLINE = NLINE + 1
C-----BLANK DISTRIBUTION NAME AND ZERO VOLUME FOR THIS APPROACH, OR
C-----INTERNAL APPROACH OF DIAMOND, OR ZERO VOLUME FOR THIS APPROACH
C-----READ VEHICLE CLASS PERCENTS
      IF ( IYES . EQ . CYES )                    THEN
        READ (NIN,502) (XPERV(J,IVTP,IAN),J=1,NVEHCL)
      END IF
C-----READ LANE DATA FOR INBOUND APPROACHES WITH ZERO VOLUME
      READ (NIN,503) (LWID(IL+J),(LGEOM(IZ,IL+J),IZ=1,4),
     *               IUT(IL+J),ILT(IL+J),IST(IL+J),IRT(IL+J),IUSEED(J),
     *               XPERL(J,IVTP,IAN),ILUB(IL+J),ILUE(IL+J),ILUR(IL+J),
     *               ILUV(IL+J),J=1,NLANES(IA))
      DO 2070  J = 1 , NLANES(IA)
      IL = IL + 1
      LLANES(J,IA) = IL
      CMORC = ICBL
      IF ( NLANES(IA) . GT . 1 )                 THEN
        IF      ( J . EQ . 1          )          THEN
          CMORC='(MEDIAN LANE)'
        ELSE IF ( J . EQ . NLANES(IA) )          THEN
          CMORC='(CURB LANE)'
        END IF
      END IF
C-----CHECK TURN CODE FIELDS FOR LANE IL
      WRITE (6,617) J,IUT(IL),ILT(IL),IST(IL),IRT(IL),CMORC
      NLINE = NLINE + 1
C-----IF LANE TURN CODE IS ALL XXXXs OK-LANE UNAVAILABLE AT INTERSECTION
      IF ( IUT(IL).EQ.'X' .AND. ILT(IL).EQ.'X' .AND.
     *     IST(IL).EQ.'X' .AND. IRT(IL).EQ.'X' ) GO TO 2050
C-----IF LANE TURN CODE IS ALL BLANKS, THEN ERROR
      IF ( IUT(IL).EQ.ICBL .AND.
     *     ILT(IL).EQ.ICBL .AND.
     *     IST(IL).EQ.ICBL .AND.
     *     IRT(IL).EQ.ICBL )                     GO TO 8760
C-----CHECK IUT, ILT, IST, AND IRT FOR CORRECT LETTER OR BLANK
      IF ( IUT(IL).NE.ICBL .AND. IUT(IL).NE.'U' )GO TO 8770
      IF ( ILT(IL).NE.ICBL .AND. ILT(IL).NE.'L' )GO TO 8780
      IF ( IST(IL).NE.ICBL .AND. IST(IL).NE.'S' )GO TO 8790
      IF ( IRT(IL).NE.ICBL .AND. IRT(IL).NE.'R' )GO TO 8800
 2050 CONTINUE
C-----CHECK LANE USE FIELDS FOR LANE IL
C-----IF LANE USE IS ALL BLANKS, SET TO DEFAULT LANE USE OF B, E, AND V
      IF ( ILUB(IL).EQ.ICBL.AND.ILUE(IL).EQ.ICBL.AND.
     *     ILUR(IL).EQ.ICBL.AND.ILUV(IL).EQ.ICBL)THEN
        ILUB(IL) = 'B'
        ILUE(IL) = 'E'
        ILUV(IL) = 'V'
      END  IF
      WRITE (6,618) J,ILUB(IL),ILUE(IL),ILUR(IL),ILUV(IL),CMORC
      NLINE = NLINE + 1
      IF ( ILUB(IL).NE.ICBL.AND.ILUB(IL).NE.'B' )GO TO 8810
      IF ( ILUE(IL).NE.ICBL.AND.ILUE(IL).NE.'E' )GO TO 8820
      IF ( ILUR(IL).NE.ICBL.AND.ILUR(IL).NE.'R' )GO TO 8830
      IF ( ILUV(IL).NE.ICBL.AND.ILUV(IL).NE.'V' )GO TO 8840
      IF ( LGEOM(1,IL) . EQ . LGEOM(2,IL) )      GO TO 2060
C-----IF LGEOM1 LT LGEOM2, VEHICLES MAY ENTER (LOGIN) THIS LANE
      MAYENT(IAN,J) = .TRUE.
      GO TO 2070
 2060 CONTINUE
      MAYENT(IAN,J) = .FALSE.
 2070 CONTINUE
      GO TO 3084
 2080 CONTINUE
C
C-----PROCESS NORMAL (NEEDS GENERATED TRAFFIC) IBAP (INBOUND) APPROACHES
C
                    IF ( JDIST . EQ . 1 )        GO TO 3010
                    IF ( JDIST . EQ . 5 )        GO TO 3010
                    IF ( PDIST . LE . 0.0 )      GO TO 8240
      IF ( JDIST . EQ . 2 . AND .
     *     ABS( PDIST-INT( PDIST ) ) .GT.  ZERO )GO TO 8250
      IF ( JDIST.EQ.3 . AND . PDIST.LT.1.0 )     GO TO 8260
      TMEAN = 3600.0/JVOL
      IF ( JDIST.EQ.6 . AND . PDIST.GE.TMEAN)    GO TO 8270
 3010 CONTINUE
      WRITE (6,605) JVOL,XMEANS,X85PER
      NLINE = NLINE + 3
      WRITE (6,606) (LOBA (J),J=1,NOBA)
      NLINE = NLINE + 1
      IF ( DIAMON )                              THEN
C-----FOR DIAMOND, FIND INTERNAL OUTBOUND APPROACHES
C-----THEY WILL BE FIRST IN LIST OF APPROACHES LOBA(1) AND LOBA(2)
C-----THEY WILL HAVE NO OUTBOUND TRAFFIC 
C-----IF MORE THAN 6 OUTBOUNDS, DATA WILL BE IN YPERT(1) AND YPERT(2)
C-----MOVE THEM TO THEIR PROPER PLACES          YPERT(7) AND YPERT(8)
C-----SET INTERNAL OUTBOUND APPROACH DATA TO ZERO
        DO  K = 1 , 2
          IF ((LOBA(K).EQ.12).OR.(LOBA(K).EQ.13))THEN
            YPERT(K+6) = YPERT(K)
            YPERT(K)   = 0.0D0
          END IF
        END DO
      END IF
      WRITE (6,607) (YPERT(J),J=1,NOBA)
      NLINE = NLINE + 1
                    IF ( JVOL   . LT .      0 )  GO TO 8280
                    IF ( JVOL   . GT . MXTVOL )  GO TO 8280
                    IF ( XMEANS . LE .   10.0 )  GO TO 8290
                    IF ( XMEANS . GT .   80.0 )  GO TO 8290
                    IF ( X85PER . LT . XMEANS )  GO TO 8300
                    IF ( X85PER . GT .   90.0 )  GO TO 8300
      SUM = 0.0
      DO 3020  JAN = 1 , NOBA
      SUM = SUM + YPERT(JAN)
 3020 CONTINUE
            IF ( ABS( SUM-100.0 ) . GT . ZERO )  GO TO 8310
      WRITE (6,608) IYES
      NLINE = NLINE + 1
      IF ( IYES.NE.CYES . AND . IYES.NE.CNO )    GO TO 8320
      WRITE (6,610) (J,J=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
                    IF ( IYES . EQ . CNO )       GO TO 3030
      READ (NIN,502) (XPERV(J,IVTP,IAN),J=1,NVEHCL)
      WRITE (6,611)  (XPERV(J,IVTP,IAN),J=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      GO TO 3040
 3030 CONTINUE
                    IF ( NVEHCL . NE . NVCD )    GO TO 8330
      WRITE (6,612) (XPERV(J,IVTP,IAN),J=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
 3040 CONTINUE
      SUM = 0.0
      DO 3050  J = 1 , NVEHCL
      SUM = SUM + XPERV(J,IVTP,IAN)
 3050 CONTINUE
            IF ( ABS( SUM-100.0 ) . GT . ZERO )  GO TO 8340
C-----READ LANE DATA FOR INBOUND APPROACHES WITH NON-ZERO VOLUME
C-----READ PERCENT OF TRAFFIC STREAM FOR EACH LANE
      READ (NIN,503) (LWID(IL+J),(LGEOM(IZ,IL+J),IZ=1,4),
     *               IUT(IL+J),ILT(IL+J),IST(IL+J),IRT(IL+J),IUSEED(J),
     *               XPERL(J,IVTP,IAN),ILUB(IL+J),ILUE(IL+J),ILUR(IL+J),
     *               ILUV(IL+J),J=1,NLANES(IA))
      DO 3055  J = 1 , NLANES(IA)
      IL = IL + 1
      LLANES(J,IA) = IL
      CMORC = ICBL
      IF ( NLANES(IA) . GT . 1 )                 THEN
        IF      ( J . EQ . 1          )          THEN
          CMORC='(MEDIAN LANE)'
        ELSE IF ( J . EQ . NLANES(IA) )          THEN
          CMORC='(CURB LANE)'
        END IF
      END IF
      WRITE (6,617) J,IUT(IL),ILT(IL),IST(IL),IRT(IL),CMORC
      NLINE = NLINE + 1
C-----IF LANE TURN CODE IS ALL XXXXs OK-LANE UNAVAILABLE AT INTERSECTION
      IF ( IUT(IL).EQ.'X' .AND. ILT(IL).EQ.'X' .AND.
     *     IST(IL).EQ.'X' .AND. IRT(IL).EQ.'X' ) GO TO 3054
C-----IF LANE TURN CODE IS ALL BLANKS, THEN ERROR
      IF ( IUT(IL).EQ.ICBL .AND.
     *     ILT(IL).EQ.ICBL .AND.
     *     IST(IL).EQ.ICBL .AND.
     *     IRT(IL).EQ.ICBL )                     GO TO 8760
C-----CHECK IUT, ILT, IST, AND IRT FOR CORRECT LETTER OR BLANK
      IF ( IUT(IL).NE.ICBL .AND. IUT(IL).NE.'U' )GO TO 8770
      IF ( ILT(IL).NE.ICBL .AND. ILT(IL).NE.'L' )GO TO 8780
      IF ( IST(IL).NE.ICBL .AND. IST(IL).NE.'S' )GO TO 8790
      IF ( IRT(IL).NE.ICBL .AND. IRT(IL).NE.'R' )GO TO 8800
 3054 CONTINUE
C-----IF LANE USE IS ALL BLANKS, SET TO DEFAULT LANE USE OF B, E, AND V
      IF ( ILUB(IL).EQ.ICBL.AND.ILUE(IL).EQ.ICBL.AND.
     *     ILUR(IL).EQ.ICBL.AND.ILUV(IL).EQ.ICBL)THEN
        ILUB(IL) = 'B'
        ILUE(IL) = 'E'
        ILUV(IL) = 'V'
      END  IF
      WRITE (6,618) J,ILUB(IL),ILUE(IL),ILUR(IL),ILUV(IL),CMORC
      NLINE = NLINE + 1
      IF ( ILUB(IL).NE.ICBL.AND.ILUB(IL).NE.'B' )GO TO 8810
      IF ( ILUE(IL).NE.ICBL.AND.ILUE(IL).NE.'E' )GO TO 8820
      IF ( ILUR(IL).NE.ICBL.AND.ILUR(IL).NE.'R' )GO TO 8830
      IF ( ILUV(IL).NE.ICBL.AND.ILUV(IL).NE.'V' )GO TO 8840
 3055 CONTINUE
      IF ( DIAMON )                              THEN
        IF ( (IA.EQ.3) . OR . (IA.EQ.8) )        THEN
          IF ( LLANES(1,IA) . GT . 0 )           THEN
            IF ( IUT(LLANES(1,IA)) . EQ . 'U' )  THEN
              WRITE (6,609) FUTPER
              NLINE = NLINE + 1
              IF ( FUTPER . LT .   0.0 )         GO TO 8680
              IF ( FUTPER . GT . 100.0 )         GO TO 8680
              XPERUT(1,IVTP,IAN) = FUTPER
              XPERUT(2,IVTP,IAN) = 100.0 - FUTPER
            END IF
          END IF
        END IF
      END IF
C
C-----SEED FOR RANDOM NUMBERS IS WITH DATA FOR FIRST LANE (IUSEED(1))
C
                    IF ( IUSEED(1) . GT . 0 )    ISEED(IAN)=IUSEED(1)
C-----TEXAS removed --------------------------------------------------------------Harmonia B. A. Mauldon                                       
      ISEED(IAN) = (NUMREP-1)*100000 + ISEED(IAN)
C-----End-----------------------------------------------------------------------------------------------
C-----Harmonia mod----------------------------------------------------------------Harmonia B.A.Mauldon 10/1/2013

      ISEED(IAN) = (JRANDM)+ISEED(IAN)
C-----End-------------------------------------------------------------------------Harmonia B.A.Mauldon                 
      WRITE (6,613) ISEED(IAN)
      NLINE = NLINE + 1
      JL = LLANES(1,IA)
      KGEOM = LGEOM(1,JL)
      DO 3070  J = 1 , NLANES(IA)
      JL = LLANES(J,IA)
            IF ( LGEOM(1,JL) . NE . KGEOM )      GO TO 8350
      CMORC = ICBL
      IF ( NLANES(IA) . GT . 1 )                 THEN
        IF      ( J . EQ . 1          )          THEN
          CMORC='(MEDIAN LANE)'
        ELSE IF ( J . EQ . NLANES(IA) )          THEN
          CMORC='(CURB LANE)'
        END IF
      END IF
      LWARN = 1
      IF ( LGEOM(1,JL).LT.LGEOM(2,JL) . AND .
     *     XPERL(J,IVTP,IAN).LE.0.0 )            LWARN = CWARNC
      WRITE (6,614) J,NINT( XPERL(J,IVTP,IAN) ),CMORC,CWARN(1:LWARN)
      NLINE = NLINE + 1
      IF ( LWARN . EQ . CWARNC )                 THEN
        WRITE (WRNMSG,614) J,NINT( XPERL(J,IVTP,IAN) ),CMORC,
     *                     CWARN(1:LWARN)
        CALL  PRTWRN  ( WRNMSG,.TRUE. )
      END IF
      IF ( LGEOM(1,JL) . EQ . LGEOM(2,JL) )      GO TO 3060
C-----IF LGEOM1 LT LGEOM2, VEHICLES MAY ENTER (LOGIN) THIS LANE
      MAYENT(IAN,J) = .TRUE.
      GO TO 3070
 3060 CONTINUE
      MAYENT(IAN,J) = .FALSE.
            IF ( XPERL(J,IVTP,IAN) . GT . 0.0 )  GO TO 8360
 3070 CONTINUE
      SUM = 0.0
      DO 3080  J = 1 , NLANES(IA)
      SUM = SUM + XPERL(J,IVTP,IAN)
 3080 CONTINUE
            IF ( ABS( SUM-100.0 ) . GT . ZERO )  GO TO 8370
 3084 CONTINUE
      NDEGST(IAN) = MDEGST
C-----SET OLD VARIABLES READ IN INTO NEW MULTI-TIME PERIOD VARIABLES
      IVOL  (IVTP,IAN) = JVOL
      IDIST (IVTP,IAN) = JDIST
      PARAM (IVTP,IAN) = PDIST
      VMEAN (IVTP,IAN) = MPH2FS*XMEANS
      VSIGMA(IVTP,IAN) = MPH2FS*(X85PER-XMEANS)/1.0364334
            IF ( VSIGMA(IVTP,IAN) . LT . ZERO )  VSIGMA(IVTP,IAN) = 0.0
      IF ( ( VMEAN (IVTP,IAN) . GT . 0.0D0           ) . AND .
     *     ( VSIGMA(IVTP,IAN) . GE . VMEAN(IVTP,IAN) ) )
     *                                           GO TO 8380
      DO 3090  JAN = 1 , NOBA
      XPERT(JAN,IVTP,IAN) = YPERT(JAN)
 3090 CONTINUE
      GO TO 4030
 4010 CONTINUE
C
C-----APPROACH IS OUTBOUND
C
                    IF ( CDIST . NE . ICBL )     GO TO 8390
      IF ( IYES.NE.CYES . AND . IYES.NE.CNO )    GO TO 8320
                    IF ( IYES . EQ . CYES )      GO TO 8400
C-----READ LANE INFORMATION
      READ (NIN,503) (LWID(IL+J),(LGEOM(IZ,IL+J),IZ=1,4),
     *               IUT(IL+J),ILT(IL+J),IST(IL+J),IRT(IL+J),IUSEED(J),
     *               DPERL,ILUB(IL+J),ILUE(IL+J),ILUR(IL+J),
     *               ILUV(IL+J),J=1,NLANES(IA))
      DO 4025  J = 1 , NLANES(IA)
      IL = IL + 1
      LLANES(J,IA) = IL
      CMORC = ICBL
      IF ( NLANES(IA) . GT . 1 )                 THEN
        IF      ( J . EQ . 1          )          THEN
          CMORC='(MEDIAN LANE)'
        ELSE IF ( J . EQ . NLANES(IA) )          THEN
          CMORC='(CURB LANE)'
        END IF
      END IF
      WRITE (6,617) J,IUT(IL),ILT(IL),IST(IL),IRT(IL),CMORC
      NLINE = NLINE + 1
C-----IF LANE TURN CODE IS ALL XXXXs OK-LANE UNAVAILABLE AT INTERSECTION
      IF ( IUT(IL).EQ.'X' .AND. ILT(IL).EQ.'X' .AND.
     *     IST(IL).EQ.'X' .AND. IRT(IL).EQ.'X' ) GO TO 4024
C-----IF LANE TURN CODE IS ALL BLANKS, THEN ERROR
      IF ( IUT(IL).EQ.ICBL .AND.
     *     ILT(IL).EQ.ICBL .AND.
     *     IST(IL).EQ.ICBL .AND.
     *     IRT(IL).EQ.ICBL )                     GO TO 8760
C-----CHECK IUT, ILT, IST, AND IRT FOR CORRECT LETTER OR BLANK
      IF ( IUT(IL).NE.ICBL .AND. IUT(IL).NE.'U' )GO TO 8770
      IF ( ILT(IL).NE.ICBL .AND. ILT(IL).NE.'L' )GO TO 8780
      IF ( IST(IL).NE.ICBL .AND. IST(IL).NE.'S' )GO TO 8790
      IF ( IRT(IL).NE.ICBL .AND. IRT(IL).NE.'R' )GO TO 8800
 4024 CONTINUE
C-----IF LANE USE IS ALL BLANKS, SET TO DEFAULT LANE USE OF B, E, AND V
      IF ( ILUB(IL).EQ.ICBL.AND.ILUE(IL).EQ.ICBL.AND.
     *     ILUR(IL).EQ.ICBL.AND.ILUV(IL).EQ.ICBL)THEN
        ILUB(IL) = 'B'
        ILUE(IL) = 'E'
        ILUV(IL) = 'V'
      END  IF
      WRITE (6,618) J,ILUB(IL),ILUE(IL),ILUR(IL),ILUV(IL),CMORC
      NLINE = NLINE + 1
      IF ( ILUB(IL).NE.ICBL.AND.ILUB(IL).NE.'B' )GO TO 8810
      IF ( ILUE(IL).NE.ICBL.AND.ILUE(IL).NE.'E' )GO TO 8820
      IF ( ILUR(IL).NE.ICBL.AND.ILUR(IL).NE.'R' )GO TO 8830
      IF ( ILUV(IL).NE.ICBL.AND.ILUV(IL).NE.'V' )GO TO 8840
 4025 CONTINUE
 4030 CONTINUE
C
C-----INFORMATION FOR ALL (INBOUND & OUTBOUND) APPROACHES
C
      IUSED (IA) = 1
      IF ( DIAMON )                              THEN
        IF ( IUT(LLANES(1,IA)) . EQ . 'U' )      THEN
          ALOWUT(IA) = .TRUE.
          ANYFUT     = .TRUE.
        END IF
        IF ( JSFLAG . EQ . ICBL )                THEN
          JSFLAG = ISFLAG(IA)
          ANYDEF = .TRUE.
        END IF
        IAFLAG(IA) = JSFLAG
        ISFLAG(IA) = JSFLAG
      ELSE
        IAFLAG(IA) = 'L'
        ISFLAG(IA) = 'L'
      END IF
      IF ( ISFLAG(IA) . NE . 'L' . AND .
     *     ISFLAG(IA) . NE . 'R' )               GO TO 8690
C
C-----CHECK LANE WIDTH AGAINST LANE USE CODES
C-----CHECK ALLOWED VEHICLE TYPES
C
      DO  J = 1 , NLANES(IA)
        IL = LLANES(J,IA)
        LAVT(IL) = 0
        IF (  ILUB(IL) . EQ . 'B' )              THEN
C-----    ALLOW BICYCLES
          LAVT(IL) = LAVT(IL) + LAVTB
        END IF
        IF (  ILUE(IL) . EQ . 'E' )              THEN
C-----    ALLOW EMERGENCY VEHICLES
          LAVT(IL) = LAVT(IL) + LAVTE
        END IF
        IF (  ILUR(IL) . EQ . 'R' )              THEN
C-----    ALLOW RAIL VEHICLES
          LAVT(IL) = LAVT(IL) + LAVTR
        END IF
        IF (  ILUV(IL) . EQ . 'V' )              THEN
C-----    ALLOW NORMAL VEHICLES
          LAVT(IL) = LAVT(IL) + LAVTV
        END IF
        IF ( LAVT(IL) . EQ . 0     )             GO TO 7930
        AAVT(IA) = IOR( AAVT(IA),LAVT(IL) )
        IF ( LAVT(IL) . EQ . LAVTB )             THEN
          WIDLMN = LWBMIN
          WIDLMX = LWBMAX
        ELSE
          WIDLMN = LWVMIN
          WIDLMX = LWVMAX
        END IF
                    IF ( LWID(IL) . LT . WIDLMN )GO TO 8850
                    IF ( LWID(IL) . GT . WIDLMX )GO TO 8850
      END DO
C
C-----CHECK FOR AND PROCESS VARYING TRAFFIC STREAMS FOR THIS APPROACH 
C
      IF ( IBAP(IA) )                            THEN
        IF ( NVTP(IA) . LE . 0 )                 THEN
          NVTP(IA) = 1
          LVTP(IVTP,IAN) = ITSIM
          EVTP(IVTP,IAN) = SIMTIM
          GO TO 4036
        ELSE
          IF ( NLINE+2+3 . GT . LINES )          THEN
            CALL  HEADER
          END IF
          WRITE (6,620)  (LLANES(J,IA),J=1,6),(LOBA(J), J=1,6)
          NLINE = NLINE + 3
        END IF
        EVTP(1,IAN) = 0.0D0
        KVOL = 0
        DO 4032  J = 7 , NOA
        YPERT(J) = 0.0
 4032   CONTINUE
        DO 4035  IVTP = 1 , NVTP(IA)
          READ (NIN,505) LVTP(IVTP,IAN),FPERL(IVTP,IAN),FPERR(IVTP,IAN),
     *                   FUTPER,(XPERL(J,IVTP,IAN),J=1,6),CDIST,JVOL,
     *                   PDIST,XMEANS,X85PER,(YPERT(J),J=1,6),IYES
C-----  ECHO-PRINT OF DATA
                    IF ( NLINE+1 . GT . LINES )  CALL  HEADER
        WRITE (6,621) IVTP,LVTP(IVTP,IAN),INT( FPERL(IVTP,IAN) ),
     *                INT( FPERR(IVTP,IAN )),INT( FUTPER ),
     *                (INT( XPERL(J,IVTP,IAN) ),J=1,6),CDIST,JVOL,
     *                PDIST,XMEANS,X85PER,(INT( YPERT(J) ),J=1,6),IYES
        NLINE = NLINE + 1
            IF ( LVTP(IVTP,IAN)  . LT .      5 ) GO TO 8730
            IF ( LVTP(IVTP,IAN)  . GT .  ITSIM ) GO TO 8730
C-----  SET END OF VTP
        EVTP(IVTP,IAN) = EVTP(IVTP,IAN) + DBLE( LVTP(IVTP,IAN)*60 )
            IF ( EVTP(IVTP,IAN)  . GT . SIMTIM ) GO TO 8740
        IF ( IVTP . LT . NVTP(IA) )              THEN
          EVTP(IVTP+1,IAN) = EVTP(IVTP,IAN)
        END IF
            IF ( FPERL(IVTP,IAN) . LT .   50.0 ) GO TO 8140
            IF ( FPERL(IVTP,IAN) . GT .  100.0 ) GO TO 8140
            IF ( FPERR(IVTP,IAN) . LT .   50.0 ) GO TO 8150
            IF ( FPERR(IVTP,IAN) . GT .  100.0 ) GO TO 8150
        KVOL = KVOL + JVOL
        IF ( DIAMON )                            THEN
          IF ( (IA.EQ.3) . OR . (IA.EQ.8) )      THEN
            IF ( LLANES(1,IA) . GT . 0 )         THEN
              IF ( IUT(LLANES(1,IA)) . EQ . 'U' )THEN
                WRITE (6,609) FUTPER
                NLINE = NLINE + 1
                IF ( FUTPER . LT .   0.0 )       GO TO 8680
                IF ( FUTPER . GT . 100.0 )       GO TO 8680
                XPERUT(1,IVTP,IAN) = FUTPER
                XPERUT(2,IVTP,IAN) = 100.0 - FUTPER
              END IF
            END IF
          END IF
        END IF
        SUM = 0.0
        DO  J = 1 , NLANES(IA)
          SUM = SUM + XPERL(J,IVTP,IAN)
        END DO
              IF ( ABS( SUM-100.0 ) . GT . ZERO )GO TO 8370
        IF ( CDIST . EQ . CDISTN(1) . OR .
     *       CDIST . EQ . CDISTN(5) )            THEN
          CPDIST = ICBL
          LPDIST = 1
        ELSE
C 603   FORMAT(13H  PARAMETER =,F8.2)
C         WRITE (CPDIST,603) PDIST
C         LPDIST = 21
        END IF
C 604   FORMAT(12X,39HNUMBER OF DEGREES FOR STRAIGHT --------,I5,:,/,
C    *         12X,39HHEADWAY DISTRIBUTION NAME -------------,1X,A7,A)
C         WRITE (6,604) MDEGST,CDIST,CPDIST(1:LPDIST)
C         NLINE = NLINE + 2
        IF ( JVOL . EQ . 0 )                     THEN
C 605   FORMAT(12X,39HEQUIVALENT HOURLY VOLUME (VPH) --------,I5,:,/,
C    *         12X,39HAPPROACH MEAN SPEED (MPH) -------------,F7.1,/,
C    *         12X,39HAPPROACH 85 PERCENTILE SPEED (MPH) ----,F7.1)
C         WRITE (6,605) JVOL
C         NLINE = NLINE + 1
        END IF
        DO  J = 1 , 7
          JDIST=J
          IF ( CDIST . EQ . CDISTN(J) )          GO TO 4033
        END DO
        GO TO 8220
 4033   CONTINUE
                      IF ( JDIST . EQ . 1 )      GO TO 4034
                      IF ( JDIST . EQ . 5 )      GO TO 4034
                      IF ( PDIST . LE . 0.0 )    GO TO 8240
        IF ( JDIST . EQ . 2 . AND .
     *       ABS( PDIST-INT( PDIST ) ).GT.ZERO ) GO TO 8250
        IF ( JDIST .EQ. 3 .AND. PDIST .LT. 1.0 ) GO TO 8260
        TMEAN = 3600.0/JVOL
        IF ( JDIST .EQ. 6 .AND. PDIST .GE. TMEAN)GO TO 8270
 4034   CONTINUE
        IF ( DIAMON )                            THEN
          DO  K = 1 , 2
            IF (LOBA(K).EQ.12.OR.LOBA(K).EQ.13)  THEN
              YPERT(K+6) = YPERT(K)
              YPERT(K)   = 0.0D0
            END IF
          END DO
        END IF
                      IF ( IYES . NE . CYES )    GO TO 4037
        READ (NIN,502) (XPERV(J,IVTP,IAN),J=1,NVEHCL)
        WRITE (6,610)  (J,J=1,NVEHCL)
        WRITE (6,611)  (XPERV(J,IVTP,IAN),J=1,NVEHCL)
        NLINE = NLINE + 2*INT( (NVEHCL+15)/16 )
 4037   CONTINUE
        IVOL  (IVTP,IAN) = JVOL
        IDIST (IVTP,IAN) = JDIST
        PARAM (IVTP,IAN) = PDIST
        VMEAN (IVTP,IAN) = MPH2FS*XMEANS
        VSIGMA(IVTP,IAN) = MPH2FS*(X85PER-XMEANS)/1.0364334
              IF ( VSIGMA(IVTP,IAN) . LT . ZERO )  VSIGMA(IVTP,IAN) = 0.0
        IF ( ( VMEAN (IVTP,IAN) . GT . 0.0D0           ) . AND .
     *       ( VSIGMA(IVTP,IAN) . GE . VMEAN(IVTP,IAN) ) )
     *                                           GO TO 8380
        DO  JAN = 1 , NOBA
          XPERT(JAN,IVTP,IAN) = YPERT(JAN)
        END DO
 4035   CONTINUE
        IF ( KVOL . LE . 0 )                     THEN
          WRNMSG = '***** WARNING - ALL VARYING TRAFFIC PERIODS HAVE' //
     *             ' ZERO VOLUME FOR THIS APPROACH - REVIEW WARNING'  //
     *             ' MESSAGES, CORRECT INPUT AND RERUN AS NEEDED -'   //
     *             ' READAP *****'
          CALL  PRTWRN  ( WRNMSG,.FALSE. )
        END  IF
 4036   CONTINUE
C END IF ( IBAP(IA) )                            THEN
      END IF
C-----END OF APPROACH LOOP
 4040 CONTINUE
C-----CHECK IF INFORMATION FOR EACH INBOUND APPROACH WAS SPECIFIED
      DO 5010  IAN = 1 , NIBA
      IA = LIBA(IAN)
                    IF ( IUSED(IA) . EQ . 0 )    GO TO 8410
 5010 CONTINUE
C-----CHECK IF INFORMATION FOR EACH OUTBOUND APPROACH WAS SPECIFIED
      DO 5020  IAN = 1 , NOBA
      IA = LOBA(IAN)
                    IF ( IUSED(IA) . EQ . 0 )    GO TO 8410
 5020 CONTINUE
      IF ( NLINE+3 . GT . LINES )                THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 2
        END IF
      END IF
      WRITE (6,615) NAPS
      NLINE = NLINE + 4
                    IF ( .NOT. DIAMON )          RETURN
                    IF ( ANYFUT . AND . ANYDEF ) GO TO 8700
C-----FIND DIAMOND INTERNAL INBOUND AND INTERNAL OUTBOUND APPROACHES
      DO 6030  IAN = 1 , NIBA
      IA = LIBA(IAN)
      DO 6020  JAN = 1 , NOBA
      JA = LOBA(JAN)
            IF ( IAAZIM(IA) . NE . IAAZIM(JA) )  GO TO 6020
            IF ( IAPX  (IA) . NE . IAPX  (JA) )  GO TO 6020
            IF ( IAPY  (IA) . NE . IAPY  (JA) )  GO TO 6020
            IF ( ISLIM (IA) . NE . ISLIM (JA) )  GO TO 6020
            IF ( NLANES(IA) . NE . NLANES(JA) )  GO TO 6020
            IF ( ISFLAG(IA) . EQ . ISFLAG(JA) )  GO TO 6020
      DO 6010  ILN = 1 , NLANES(IA)
      JLN = ILN
      IL = LLANES(ILN,IA)
      JL = LLANES(JLN,JA)
            IF ( LWID   (IL) . NE . LWID   (JL) )GO TO 6020
            IF ( LGEOM(1,IL) . NE . LGEOM(1,JL) )GO TO 6020
            IF ( LGEOM(2,IL) . NE . LGEOM(2,JL) )GO TO 6020
            IF ( LGEOM(3,IL) . NE . LGEOM(3,JL) )GO TO 6020
            IF ( LGEOM(4,IL) . NE . LGEOM(4,JL) )GO TO 6020
 6010 CONTINUE
      IAFLAG(IA) = 'I'
      IAFLAG(JA) = 'I'
 6020 CONTINUE
 6030 CONTINUE
C-----FIND DIAMOND FREE U-TURN PATHS
      DO 6050  IAN = 1 , NIBA
      IA = LIBA(IAN)
                    IF ( IAFLAG(IA) . EQ . 'I' ) GO TO 6050
                    IF ( .NOT. ALOWUT(IA) )      GO TO 6050
      DO 6040  JAN = 1 , NOBA
      JA = LOBA(JAN)
            IF ( ISFLAG(IA) . EQ . ISFLAG(JA) )  GO TO 6040
                    IF ( IAFLAG(JA) . EQ . 'I' ) GO TO 6040
                    IF ( .NOT. ALOWUT(JA) )      GO TO 6040
      FREEUT(JAN,IAN) = .TRUE.
 6040 CONTINUE
 6050 CONTINUE
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 7930 CONTINUE
      CALL  ABORTR  ( 'STOP 793 - '                         //
     *                'ALLOWED VEHICLE TYPE = ('            //
     *                ILUB(IL)//ILUE(IL)//ILUR(IL)//ILUV(IL)//
     *                ') IS NOT VALID - READAP'                )
      STOP  793
 8140 CONTINUE
      WRITE (ERRMSG,814) FPERL(IVTP,IA)
      CALL  ABORTR ( ERRMSG )
      STOP  814
 8150 CONTINUE
      WRITE (ERRMSG,815) FPERR(IVTP,IA)
      CALL  ABORTR ( ERRMSG )
      STOP  815
 8160 CONTINUE
      WRITE (ERRMSG,816) IA,NAP
      CALL  ABORTR ( ERRMSG )
      STOP  816
 8170 CONTINUE
      WRITE (ERRMSG,817) IA
      CALL  ABORTR ( ERRMSG )
      STOP  817
 8180 CONTINUE
      WRITE (ERRMSG,818) IA,IAAZIM(IA)
      CALL  ABORTR ( ERRMSG )
      STOP  818
 8190 CONTINUE
      WRITE (ERRMSG,819) IA,NLANES(IA),NAL
      CALL  ABORTR ( ERRMSG )
      STOP  819
 8200 CONTINUE
      WRITE (ERRMSG,820) IA
      CALL  ABORTR ( ERRMSG )
      STOP  820
 8210 CONTINUE
      WRITE (ERRMSG,821) IA,MDEGST
      CALL  ABORTR ( ERRMSG )
      STOP  821
 8220 CONTINUE
      WRITE (ERRMSG,822) IA,CDIST
      CALL  ABORTR ( ERRMSG )
      STOP  822
c8230 CONTINUE
c     WRITE (ERRMSG,823) IA
c     CALL  ABORTR ( ERRMSG )
c     STOP  823
 8240 CONTINUE
      WRITE (ERRMSG,824) IA,PDIST
      CALL  ABORTR ( ERRMSG )
      STOP  824
 8250 CONTINUE
      WRITE (ERRMSG,825) IA,PDIST
      CALL  ABORTR ( ERRMSG )
      STOP  825
 8260 CONTINUE
      WRITE (ERRMSG,826) IA,PDIST
      CALL  ABORTR ( ERRMSG )
      STOP  826
 8270 CONTINUE
      WRITE (ERRMSG,827) IA,PDIST,TMEAN
      CALL  ABORTR ( ERRMSG )
      STOP  827
 8280 CONTINUE
      WRITE (ERRMSG,828) IA,JVOL,MXTVOL
      CALL  ABORTR ( ERRMSG )
      STOP  828
 8290 CONTINUE
      WRITE (ERRMSG,829) IA,XMEANS
      CALL  ABORTR ( ERRMSG )
      STOP  829
 8300 CONTINUE
      WRITE (ERRMSG,830) IA,X85PER,XMEANS
      CALL  ABORTR ( ERRMSG )
      STOP  830
 8310 CONTINUE
      WRITE (ERRMSG,831) IA,SUM
      CALL  ABORTR ( ERRMSG )
      STOP  831
 8320 CONTINUE
      WRITE (ERRMSG,832) IA,IYES
      CALL  ABORTR ( ERRMSG )
      STOP  832
 8330 CONTINUE
      WRITE (ERRMSG,833) IA,NVEHCL,NVCD
      CALL  ABORTR ( ERRMSG )
      STOP  833
 8340 CONTINUE
      WRITE (ERRMSG,834) IA,SUM
      CALL  ABORTR ( ERRMSG )
      STOP  834
 8350 CONTINUE
      WRITE (ERRMSG,835) IA,J,KGEOM
      CALL  ABORTR ( ERRMSG )
      STOP  835
 8360 CONTINUE
      WRITE (ERRMSG,836) IA,J
      CALL  ABORTR ( ERRMSG )
      STOP  836
 8370 CONTINUE
      WRITE (ERRMSG,837) IA,SUM
      CALL  ABORTR ( ERRMSG )
      STOP  837
 8380 CONTINUE
      WRITE (ERRMSG,838) IA,XMEANS,X85PER,VSIGMA(IVTP,IAN)
      CALL  ABORTR ( ERRMSG )
      STOP  838
 8390 CONTINUE
      WRITE (ERRMSG,839) IA
      CALL  ABORTR ( ERRMSG )
      STOP  839
 8400 CONTINUE
      WRITE (ERRMSG,840) IA
      CALL  ABORTR ( ERRMSG )
      STOP  840
 8410 CONTINUE
      WRITE (ERRMSG,841) IA
      CALL  ABORTR ( ERRMSG )
      STOP  841
 8680 CONTINUE
      WRITE (ERRMSG,868) IA,FUTPER
      CALL  ABORTR ( ERRMSG )
      STOP  868
 8690 CONTINUE
      WRITE (ERRMSG,869) IA,ISFLAG(IA)
      CALL  ABORTR ( ERRMSG )
      STOP  869
 8700 CONTINUE
      WRITE (ERRMSG,870)
      CALL  ABORTR ( ERRMSG )
      STOP  870
 8710 CONTINUE
      WRITE (ERRMSG,871) NVTP(IA),NVT
      CALL  ABORTR ( ERRMSG )
      STOP  871
 8720 CONTINUE
      WRITE (ERRMSG,872) NVTP(IA),IA
      CALL  ABORTR ( ERRMSG )
      STOP  872
 8730 CONTINUE
      WRITE (ERRMSG,873) NVTP(IA),LVTP(IVTP,IAN),ITSIM
      CALL  ABORTR ( ERRMSG )
      STOP  873
 8740 CONTINUE
      WRITE (ERRMSG,874) NVTP(IA),INT( EVTP(IVTP,IAN)/60.0 ),ITSIM
      CALL  ABORTR ( ERRMSG )
      STOP  874
 8760 CONTINUE
      WRITE (ERRMSG,876) IA,J
      CALL  ABORTR ( ERRMSG )
      STOP  876
 8770 CONTINUE
      WRITE (ERRMSG,877) IA,J,IUT(IL)
      CALL  ABORTR ( ERRMSG )
      STOP  877
 8780 CONTINUE
      WRITE (ERRMSG,878) IA,J,ILT(IL)
      CALL  ABORTR ( ERRMSG )
      STOP  878
 8790 CONTINUE
      WRITE (ERRMSG,879) IA,J,IST(IL)
      CALL  ABORTR ( ERRMSG )
      STOP  879
 8800 CONTINUE
      WRITE (ERRMSG,880) IA,J,IRT(IL)
      CALL  ABORTR ( ERRMSG )
      STOP  880
 8810 CONTINUE
      WRITE (ERRMSG,881) IA,J,ILUB(IL)
      CALL  ABORTR ( ERRMSG )
      STOP  881
 8820 CONTINUE
      WRITE (ERRMSG,882) IA,J,ILUE(IL)
      CALL  ABORTR ( ERRMSG )
      STOP  882
 8830 CONTINUE
      WRITE (ERRMSG,883) IA,J,ILUR(IL)
      CALL  ABORTR ( ERRMSG )
      STOP  883
 8840 CONTINUE
      WRITE (ERRMSG,884) IA,J,ILUV(IL)
      CALL  ABORTR ( ERRMSG )
      STOP  884
 8850 CONTINUE
      WRITE (ERRMSG,885) IA,J,LWID(IL),WIDLMN,WIDLMX
      CALL  ABORTR ( ERRMSG )
      STOP  885
      END                                                               READAP
C
C
C
      SUBROUTINE READGP ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHTEX'
      INCLUDE 'OUTPUT'
      INTEGER           I,NARCS,NIN,NLINES,NSDRC
  501 FORMAT(20I4)
  502 FORMAT(A)
  842 FORMAT('STOP 842 - ',
     *       'NUMBER OF ARCS =',I3,' IS LT 0 OR GT',I3,' - READGP')
  843 FORMAT('STOP 843 - ',
     *       'NUMBER OF LINES =',I3,' IS LT 0 OR GT',I4,' - READGP')
  844 FORMAT('STOP 844 - ',
     *       'NUMBER OF SIGHT DISTANCE RESTRICTION COORDINATES =',I3,
     *       ' IS LT 0 OR GT',I3,' - READGP')
C
C-----SUBROUTINE READGP DUMMY READS THE GEOMETRY PROCESSOR DATA
C
C-----READ NUMBER OF ARCS
      READ (NIN,501) NARCS
                    IF ( NARCS . LT . 0   )      GO TO 8420
                    IF ( NARCS . EQ . 0   )      GO TO 5010
                    IF ( NARCS . GT . NAR )      GO TO 8420
C-----DUMMY READ INFORMATION FOR EACH ARC
      DO 5020  I = 1 , NARCS
      READ (NIN,502)
 5020 CONTINUE
 5010 CONTINUE
C-----READ NUMBER OF LINES
      READ (NIN,501) NLINES
                    IF ( NLINES . LT . 0   )     GO TO 8430
                    IF ( NLINES . EQ . 0   )     GO TO 5040
                    IF ( NLINES . GT . NLI )     GO TO 8430
C-----DUMMY READ INFORMATION FOR EACH LINE
      DO 5030  I = 1 , NLINES
      READ (NIN,502)
 5030 CONTINUE
 5040 CONTINUE
C-----READ NUMBER OF SIGHT DISTANCE RESTRICTION COORDINATES
      READ (NIN,501) NSDRC
                    IF ( NSDRC . LT . 0   )      GO TO 8440
                    IF ( NSDRC . EQ . 0   )      GO TO 5060
                    IF ( NSDRC . GT . NSR )      GO TO 8440
C-----DUMMY READ INFORMATION FOR SIGHT DISTANCE RESTRICTION COORDINATES
      DO 5050  I = 1 , NSDRC
      READ (NIN,502)
 5050 CONTINUE
 5060 CONTINUE
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 8420 CONTINUE
      WRITE (ERRMSG,842) NARCS,NAR
      CALL  ABORTR ( ERRMSG )
      STOP  842
 8430 CONTINUE
      WRITE (ERRMSG,843) NLINES,NLI
      CALL  ABORTR ( ERRMSG )
      STOP  843
 8440 CONTINUE
      WRITE (ERRMSG,844) NSDRC,NSR
      CALL  ABORTR ( ERRMSG )
      STOP  844
      END                                                               READGP
C
C
C
      SUBROUTINE READYO ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHTEX'
      INCLUDE 'CLASS'
      INCLUDE 'DVDATA'
      INCLUDE 'LITCON'
      INCLUDE 'OUTPUT'
      CHARACTER*3       CYESD,CYESP,CYESV
      CHARACTER*25      CUORP
      INTEGER           DRWSQN(MNU),DSQMAX,DSQMIN,I,ILNB,J,LTEST,NIN,
     *                  SUMLEN,UFPDMX,ULENMX,ULENSM
      REAL              SUM
  501 FORMAT(26A)
  502 FORMAT(15F5.1)
  503 FORMAT(20I4)
  504 FORMAT(10A8)
  505 FORMAT(I2,4(I3,3I2,2I3,2I2),:(/,2X,4(I3,3I2,2I3,2I2)))
  601 FORMAT(8X,5HTABLE,I3,
     *       45H  -  DRIVER AND VEHICLE CLASS CHARACTERISTICS,///,
     *       11X,39HUSER SUPPLIED DRIVER CLASS SPLIT ------,3X,A,/,
     *       11X,39HUSER SUPPLIED VEHICLE CHARACTERISTICS -,3X,A,/,
     *       11X,39HUSER SUPPLIED DRIVER  CHARACTERISTICS -,3X,A)
  602 FORMAT(19X,31HVEHICLE CLASS NUMBER ----------,16I5,
     *       6(:,/,50X,16I5))
  603 FORMAT(11X,39HVEHICLE LOGOUT SUMMARY REQUESTED ------,1X,16(2X,A),
     *       6(:,/,51X,16(2X,A)))
  604 FORMAT(19X,31HDRIVER CLASS NUMBER -----------, 9I5)
  605 FORMAT(11X,39HDRIVER  LOGOUT SUMMARY REQUESTED ------,1X, 9(2X,A))
  606 FORMAT(11X,18HDRIVER CLASS SPLIT,2X,A25)
  607 FORMAT(15X,20HVEHICLE CLASS NUMBER,I3,1X,11(1H-),2X,9F5.1)
  608 FORMAT(11X,23HVEHICLE CHARACTERISTICS,2X,A25)
  609 FORMAT(15X,35HLENGTH OF VEHICLES (FT) -----------,16I5,
     *       6(:,/,50X,16I5))
  610 FORMAT(15X,35HVEHICLE OPERATIONAL FACTOR --------,16I5,
     *       6(:,/,50X,16I5))
  611 FORMAT(15X,35HMAXIMUM DECELERATION (FT/SEC/SEC) -,16I5,
     *       6(:,/,50X,16I5))
  612 FORMAT(15X,35HMAXIMUM ACCELERATION (FT/SEC/SEC) -,16I5,
     *       6(:,/,50X,16I5))
  613 FORMAT(15X,35HMAXIMUM VELOCITY (FT/SEC) ---------,16I5,
     *       6(:,/,50X,16I5))
  614 FORMAT(15X,35HMINIMUM INSIDE TURNING RADIUS (FT) ,16I5,
     *       6(:,/,50X,16I5))
  615 FORMAT(11X,22HDRIVER CHARACTERISTICS,2X,A25)
  616 FORMAT(15X,35HDRIVER OPERATIONAL FACTOR ---------, 9I5)
  618 FORMAT(15X,35HDRIVER REACTION TIME (SEC) --------, 9F5.1)
  619 FORMAT(15X,35HWIDTH OF VEHICLES (FT) ------------,16I5,
     *       6(:,/,50X,16I5))
  620 FORMAT(15X,35HHEIGTH OF VEHICLES (FT) -----------,16I5,
     *       6(:,/,50X,16I5))
  621 FORMAT(15X,35HCLASSIFICATION --------------------,8(1X,A8),
     *       12(:,/,50X,8(1X,A8)))
  622 FORMAT(15X,35HNUMBER OF UNITS FOR VEHICLE -------,16I5,
     *       6(:,/,50X,16I5))
  623 FORMAT(19X,31HVEHICLE CLASS NUMBER ----------,I5,7I9,
     *       12(:,/,46X,8I9))
  624 FORMAT(15X,49HVEHICLE  UNIT   UNIT   UNIT  DRAWING FRONT  REAR ,
     *           17HHITCH  TRANSITION,/,
     *       15X,49H CLASS  NUMBER LENGTH WIDTH SEQUENCE POINT POINT ,
     *           18HPOINT LENGTH WIDTH,/,
     *       15X,49H NUMBER         (FT)   (FT)           (FT)  (FT) ,
     *           18H (FT)  (FT)   (FT),/,
     *       15X,49H------- ------ ------ ----- -------- ----- ----- ,
     *           18H----- ------ -----)
  625 FORMAT(15X,I5,I7,I8,I6,I7,I8,3I6,I7)
  626 FORMAT(15X,5X,I7,I8,I6,I7,I8,3I6,I7)
  794 FORMAT('DRAWING SEQUENCE FOR VEHICLE CLASS',I3,' =',I3,
     *       ' IS NOT USED')
  795 FORMAT('DRAWING SEQUENCE MINIMUM FOR VEHICLE CLASS',I3,' =',I3,
     *       ' IS NOT ',I3)
  796 FORMAT('VEHICLE LENGTH FROM UNIT LENGTHS FOR VEHICLE CLASS',I3,
     *       ' =',I4,' IS NOT VEHICLE LENGTH =',I4)
  797 FORMAT('TRANSITION LENGTH FOR VEHICLE CLASS',I3,' UNIT',I3,' =',
     *       I3,' IS GT O WHEN TRANSITION WIDTH = 0')
  798 FORMAT('TRANSITION WIDTH FOR VEHICLE CLASS',I3,' UNIT',I3,' =',I3,
     *       ' IS GT O WHEN TRANSITION LENGTH = 0')
  799 FORMAT('FRONT POINT FOR VEHICLE CLASS',I3,' UNIT',I3,' =',I4,
     *       ' IS GE REAR HITCH POINT =',I4)
  845 FORMAT('STOP 845 - ',
     *       'USER SUPPLY DRIVER CLASS SPLIT = (',A,') IS NOT ',
     *       '(YES) OR (NO ) - READYO')
  846 FORMAT('STOP 846 - ',
     *       'USER SUPPLY VEHICLE CHARACTERISTICS = (',A,') IS NOT ',
     *       '(YES) OR (NO ) - READYO')
  847 FORMAT('STOP 847 - ',
     *       'USER SUPPLY DRIVER CHARACTERISTICS = (',A,') IS NOT ',
     *       '(YES) OR (NO ) - READYO')
  848 FORMAT('STOP 848 - ',
     *       'VEHICLE CLASS',I3,' LOGOUT SUMMARY REQUESTED = (',A,') ',
     *       'IS NOT (YES) OR (NO ) - READYO')
  849 FORMAT('STOP 849 - ',
     *       'DRIVER CLASS',I2,' LOGOUT SUMMARY REQUESTED = (',A,') ',
     *       'IS NOT (YES) OR (NO ) - READYO')
  850 FORMAT('STOP 850 - ',
     *       'NUMBER OF VEHICLE CLASSES =',I3,' IS NOT',I3,1X,
     *       'WHEN DEFAULT DRIVER CLASS SPLITS ARE REQUESTED - READYO')
  851 FORMAT('STOP 851 - ',
     *       'NUMBER OF DRIVER CLASSES =',I2,' IS NOT',I2,1X,
     *       'WHEN DEFAULT DRIVER CLASS SPLITS ARE REQUESTED - READYO')
  852 FORMAT('STOP 852 - ',
     *       'DRIVER CLASS SPLITS FOR VEHICLE CLASS',I3,' SUM =',
     *        F6.1,' IS NOT 100.0 - READYO')
  853 FORMAT('STOP 853 - ',
     *       'NUMBER OF VEHICLE CLASSES =',I3,' IS NOT',I3,' WHEN ',
     *       'DEFAULT VEHICLE CHARACTERISTICS ARE REQUESTED - READYO')
  854 FORMAT('STOP 854 - ',
     *       'LENGTH FOR VEHICLE CLASS',I3,' =',I4,' IS LT',I2,' OR GT',
     *       I4,' - READYO')
  855 FORMAT('STOP 855 - ',
     *       'DRIVER FACTOR FOR VEHICLE CLASS',I3,' =',I4,' IS LT ',
     *       '50 OR GT 150 - READYO')
  856 FORMAT('STOP 856 - ',
     *       'DECELERATION MAXIMUM FOR VEHICLE CLASS',I3,' =',I4,' IS ',
     *       'LT 4 OR GT 16 - READYO')
  857 FORMAT('STOP 857 - ',
     *       'ACCELERATION MAXIMUM FOR VEHICLE CLASS',I3,' =',I4,' IS ',
     *       'LT 1 OR GT 18 - READYO')
  858 FORMAT('STOP 858 - ',
     *       'VELOCITY MAXIMUM FOR VEHICLE CLASS',I3,' =',I4,' IS ',
     *       'LT 8 OR GT 235 - READYO')
  859 FORMAT('STOP 859 - ',
     *       'MINIMUM TURNING RADIUS FOR VEHICLE CLASS',I3,' =',I4,1X,
     *       'IS LT 3 OR GT 300 - READYO')
  860 FORMAT('STOP 860 - ',
     *       'NUMBER OF DRIVER CLASSES =',I3,' IS NOT',I2,' WHEN ',
     *       'DEFAULT DRIVER CHARACTERISTICS ARE REQUESTED - READYO')
  861 FORMAT('STOP 861 - ',
     *       'DRIVER FACTOR FOR DRIVER CLASS',I3,' =',I4,' IS LT 50 OR',
     *       ' GT 150 - READYO')
  862 FORMAT('STOP 862 - ',
     *       'PIJR TIME FOR DRIVER CLASS',I3,' =',F6.2,' IS LT 0.25 OR',
     *       ' GT 5.00 - READYO')
  886 FORMAT('NUMBER OF VEHICLE ATTRIBUTES =',I3,' - NVEHCL =',I3,
     *       ' IS NOT NVCD =',I3)
C  887 FORMAT('STOP 887 - ',
C     *       'NUMBER OF VEHICLE ATTRIBUTES (NVEHAT) =',I3,' IS NOT 6',
C     *       ' OR 10, INTERNAL DATA INCONSISTENCY -',
C     *       ' MIX OF OLD AND NEW INPUT FILES',
C     *       ' - READOP')
  888 FORMAT('MAXIMUM VEHICLE WIDTH FOR VEHICLE CLASS',I3,' =',I3,
     *       ' IS LT',I3,' OR GT',I3)
  889 FORMAT('VEHICLE HEIGHT FOR VEHICLE CLASS',I3,' =',I3,
     *       ' IS LT',I3,' OR GT',I3)
  890 FORMAT('NUMBER OF UNITS FOR VEHICLE CLASS',I3,' =',I3,
     *       ' IS LT',I3,' OR GT',I3)
  891 FORMAT('UNIT LENGTH FOR VEHICLE CLASS',I3,' UNIT',I3,' =',I4,
     *       ' IS LT',I4,' OR GT',I4)
  892 FORMAT('UNIT WIDTH FOR VEHICLE CLASS',I3,' UNIT',I3,' =',I3,
     *       ' IS LT',I3,' OR GT',I3)
  893 FORMAT('DRAWING SEQUENCE FOR VEHICLE CLASS',I3,' UNIT',I3,' =',I3,
     *       ' IS LT',I3,' OR GT',I3)
  894 FORMAT('FRONT POINT FOR VEHICLE CLASS',I3,' UNIT',I3,' =',I4,
     *       ' IS LT',I4,' OR GT',I4)
  895 FORMAT('REAR POINT FOR VEHICLE CLASS',I3,' UNIT',I3,' =',I4,
     *       ' IS LT',I4,' OR GT',I4)
  896 FORMAT('REAR HITCH POINT FOR VEHICLE CLASS',I3,' UNIT',I3,' =',I4,
     *       ' IS LT',I4,' OR GT',I4)
  897 FORMAT('TRANSITION LENGTH FOR VEHICLE CLASS',I3,' UNIT',I3,' =',
     *       I4,' IS LT',I4,' OR GT',I4)
  898 FORMAT('TRANSITION WIDTH FOR VEHICLE CLASS',I3,' UNIT',I3,' =',
     *       I3,' IS LT',I3,' OR GT',I3)
  899 FORMAT('FRONT POINT FOR VEHICLE CLASS',I3,' UNIT',I3,' =',I4,
     *       ' IS GE REAR POINT =',I4)
C
C-----SUBROUTINE READYO READS THE YES OPTIONS AND CHECKS FOR ERRORS
C
C-----DUMMY READ GEOMETRY PROCESSOR OPTIONS
      READ (NIN,501,END=1010)
C-----READ THE YES OPTIONS
      IF ( (NDRICL.LE.5) .AND. (NVEHCL.LE.15) )  THEN
        READ (NIN,501,END=1010)  CYESP,CYESV,CYESD,
     *                           (CYESVL(I),I=1,15),
     *                           (CYESDL(I),I=1, 5)
      ELSE
        READ (NIN,501,END=1010)  CYESP,CYESV,CYESD
        READ (NIN,501,END=1010)  (CYESVL(I),I=1,NVEHCL)
        READ (NIN,501,END=1010)  (CYESDL(I),I=1,NDRICL)
      END IF
C      IF ( IMAGOP . EQ . CYES )                  THEN
C        READ (NIN,501,END=1010)  IMAGL1
C        READ (NIN,501,END=1010)  IMAGL2
C      END IF
      GO TO 1020
 1010 CONTINUE
      IEOF = .TRUE.
 1020 CONTINUE
C-----SET DEFAULT VALUES FOR YES OPTIONS
                    IF ( CYESP . EQ . ICBL )     CYESP = CNO
                    IF ( CYESV . EQ . ICBL )     CYESV = CNO
                    IF ( CYESD . EQ . ICBL )     CYESD = CNO
      DO 1030  I = 1 , NVEHCL
            IF ( CYESVL(I) . EQ . ICBL )         CYESVL(I) = CNO
 1030 CONTINUE
      DO 1040  I = 1 , NDRICL
            IF ( CYESDL(I) . EQ . ICBL )         CYESDL(I) = CNO
 1040 CONTINUE
C-----TABLE  N  -  DRIVER AND VEHICLE CLASS CHARACTERISTICS
C-----2 BLANK LINES
C-----USER SUPPLIED DRIVER CLASS SPLIT
C-----USER SUPPLIED VEHICLE CHARACTERISTICS
C-----USER SUPPLIED DRIVER CHARACTERISTICS
C-----        VEHICLE CLASS NUMBER
C-----VEHICLE LOGOUT SUMMARY REQUESTED
C-----        DRIVER CLASS NUMBER
C-----DRIVER  LOGOUT SUMMARY REQUESTED
      IF ( NLINE+6+2*INT( (NVEHCL+15)/16 )+2 . GT . LINES )
     *                                           THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 3
        END IF
      END IF
C-----ECHO PRINT YES OPTIONS
      WRITE (6,601) NTABL,CYESP,CYESV,CYESD
      NLINE = NLINE + 6
      NTABL = NTABL + 1
      WRITE (6,602) (I,I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,603) (CYESVL(J),J=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,604) (I,I=1,NDRICL)
      WRITE (6,605) (CYESDL(I),I=1,NDRICL)
      NLINE = NLINE + 2
      IF ( CYESP.NE.CYES . AND . CYESP.NE.CNO )  GO TO 8450
      IF ( CYESV.NE.CYES . AND . CYESV.NE.CNO )  GO TO 8460
      IF ( CYESD.NE.CYES . AND . CYESD.NE.CNO )  GO TO 8470
      DO 2010  I = 1 , NVEHCL
      IF ( CYESVL(I).NE.CYES . AND . CYESVL(I).NE.CNO )
     *                                           GO TO 8480
 2010 CONTINUE
      DO 2020  I = 1 , NDRICL
      IF ( CYESDL(I).NE.CYES . AND . CYESDL(I).NE. CNO )
     *                                           GO TO 8490
 2020 CONTINUE
C-----2 BLANK LINES
C-----DRIVER CLASS SPLIT
C-----        DRIVER CLASS NUMBER
C-----    VEHICLE CLASS NUMBER
      IF ( NLINE+4+NVEHCL . GT . LINES )         THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 2
        END IF
      END IF
      IF ( CYESP . EQ . CYES )                   THEN
        CUORP = '(USER SUPPLIED VALUES)'
      ELSE
        CUORP = '(PROGRAM SUPPLIED VALUES)'
      END IF
      WRITE (6,606) CUORP
      NLINE = NLINE + 1
                    IF ( CYESP . EQ . CNO )      GO TO 3020
      DO 3010  I = 1 , NVEHCL
C-----READ PERCENT OF DRIVER CLASS IN VEHICLE CLASS (XPERD)
      READ (NIN,502) (XPERD(J,I),J=1,NDRICL)
 3010 CONTINUE
      GO TO 3030
 3020 CONTINUE
                    IF ( NVEHCL . NE . NVCD )    GO TO 8500
                    IF ( NDRICL . NE . NDCD )    GO TO 8510
 3030 CONTINUE
C-----PRINT DEFAULT OR USER SUPPLIED VALUES OF XPERD
      WRITE (6,604) (I,I=1,NDRICL)
      NLINE = NLINE + 1
      DO 3050  I = 1 , NVEHCL
      WRITE (6,607) I,(XPERD(J,I),J=1,NDRICL)
      NLINE = NLINE + 1
      SUM = 0.0
      DO 3040  J = 1 , NDRICL
      SUM = SUM + XPERD(J,I)
 3040 CONTINUE
            IF ( ABS( SUM-100.0 ) . GT . ZERO )  GO TO 8520
 3050 CONTINUE
C-----2 BLANK LINES
C-----VEHICLE CHARACTERISTICS
C-----    VEHICLE CLASS NUMBER
C-----LENGTH OF VEHICLES
C-----WIDTH OF VEHICLES
C-----HEIGTH OF VEHICLES
C-----VEHICLE OPERATIONAL FACTOR
C-----MAXIMUM DECELERATION
C-----MAXIMUM ACCELERATION
C-----MAXIMUM VELOCITY
C-----MINIMUM INSIDE TURNING RADIUS
C-----NUMBER OF UNITS FOR VEHICLE
C-----    VEHICLE CLASS NUMBER
C-----CLASSIFICATION
C-----2 BLANK LINES
C-----VEHICLE  UNIT   UNIT   UNIT  DRAWING FRONT  REAR HITCH  TRANSITION
C----- CLASS  NUMBER LENGTH WIDTH SEQUENCE POINT POINT POINT LENGTH WIDTH
C----- NUMBER         (FT)   (FT)           (FT)  (FT)  (FT)  (FT)   (FT)
C------------ ------ ------ ----- -------- ----- ----- ----- ------ -----
C-----   12     12     123    12     12     123   123   123    12     12
      LTEST = NLINE + 3 + 10*INT( (NVEHCL+15)/16 )
     *                  +  2*INT( (NVEHCL+ 7)/ 8 )
     *              + 6 + NVEHCL
      IF ( LTEST . GT . LINES )                  THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 2
        END IF
      END IF
      IF ( CYESV . EQ . CYES )                   THEN
        CUORP = '(USER SUPPLIED VALUES)'
      ELSE
        CUORP = '(PROGRAM SUPPLIED VALUES)'
      END IF
      WRITE (6,608) CUORP
      NLINE = NLINE + 1
C-----SET ADDITIONAL VEHICLE ATTRIBUTES FOR NVEHAT = 10
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
                    IF ( CYESV . EQ . CNO )      GO TO 4010
C-----READ IN VEHICLE CHARACTERISTICS
      READ (NIN,503) (LENV  (I),I=1,NVEHCL)
      READ (NIN,503) (IVCHAR(I),I=1,NVEHCL)
      READ (NIN,503) (IDMAX (I),I=1,NVEHCL)
      READ (NIN,503) (IAMAX (I),I=1,NVEHCL)
      READ (NIN,503) (IVMAX (I),I=1,NVEHCL)
      READ (NIN,503) (IRMIN (I),I=1,NVEHCL)
C
C-----READ IN NEW VEHICLE ATTRIBUTES
C
      IF ( NVEHAT . GE . 10 )                    THEN
        READ (NIN,504) (CLASSV(I),I=1,NVEHCL)
        READ (NIN,503) (HEIGHT(I),I=1,NVEHCL)
        READ (NIN,503) (WIDV  (I),I=1,NVEHCL)
C
C-----READ IN UNIT DATA FOR EACH VEHICLE CLASS
C
        DO 3060  I = 1 , NVEHCL
        READ (NIN,505) NUNITS(I),
     *                 (ULEN  (J,I),UWID  (J,I),UDRWSQ(J,I),
     *                  UFPD  (J,I),URPD  (J,I),URHPD (J,I),
     *                  UTRLEN(J,I),UTRWID(J,I),J=1,NUNITS(I))
 3060   CONTINUE
      ELSE
        IF ( NVEHCL . NE . NVCD )                GO TO 8860
        NVEHAT = 10
      END IF
      GO TO 4020
 4010 CONTINUE
                    IF ( NVEHCL . NE . NVCD )    GO TO 8530
        NVEHAT = 10
 4020 CONTINUE
C-----PRINT VEHICLE CHARACTERISTICS
      WRITE (6,602) (I        ,I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,609) (LENV  (I),I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,619) (WIDV  (I),I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,620) (HEIGHT(I),I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,610) (IVCHAR(I),I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,611) (IDMAX (I),I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,612) (IAMAX (I),I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,613) (IVMAX (I),I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,614) (IRMIN (I),I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,622) (NUNITS(I),I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+15)/16 )
      WRITE (6,623) (I,I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+ 7)/ 8 )
      WRITE (6,621) (CLASSV(I),I=1,NVEHCL)
      NLINE = NLINE + INT( (NVEHCL+ 7)/ 8 )
C
C-----PRINT UNIT DATA FOR EACH VEHICLE CLASS
C
      WRITE (6,501)
      WRITE (6,501)
      WRITE (6,624)
      NLINE = NLINE + 5
      DO 4040  I = 1 , NVEHCL
      DO 4030  J = 1 , NUNITS(I)
      IF ( NLINE+NUNITS(I) . GT . LINES )        THEN
        CALL  HEADER
        WRITE (6,624)
        NLINE = NLINE + 4
      END IF
      IF ( J . EQ . 1 )                          THEN
        WRITE (6,625) I,J,ULEN  (J,I),UWID  (J,I),UDRWSQ(J,I),
     *                    UFPD  (J,I),URPD  (J,I),URHPD (J,I),
     *                    UTRLEN(J,I),UTRWID(J,I)
      ELSE
        WRITE (6,626)   J,ULEN  (J,I),UWID  (J,I),UDRWSQ(J,I),
     *                    UFPD  (J,I),URPD  (J,I),URHPD (J,I),
     *                    UTRLEN(J,I),UTRWID(J,I)
      END IF
      NLINE = NLINE + 1
 4030 CONTINUE
 4040 CONTINUE
      DO 4080  I = 1 , NVEHCL
            IF ( LENV  (I) . LT . MINLEN )       GO TO 8540
            IF ( LENV  (I) . GT . MAXLEN )       GO TO 8540
            IF ( WIDV  (I) . LT . MINWID )       GO TO 8880
            IF ( WIDV  (I) . GT . MAXWID )       GO TO 8880
            IF ( HEIGHT(I) . LT . MINHIG )       GO TO 8890
            IF ( HEIGHT(I) . GT . MAXHIG )       GO TO 8890
            IF ( IVCHAR(I) . LT .     50 )       GO TO 8550
            IF ( IVCHAR(I) . GT .    150 )       GO TO 8550
            IF ( IDMAX (I) . LT .      4 )       GO TO 8560
            IF ( IDMAX (I) . GT .     16 )       GO TO 8560
            IF ( IAMAX (I) . LT .      1 )       GO TO 8570
            IF ( IAMAX (I) . GT .     18 )       GO TO 8570
            IF ( IVMAX (I) . LT .      8 )       GO TO 8580
            IF ( IVMAX (I) . GT .    235 )       GO TO 8580
            IF ( IRMIN (I) . LT .      3 )       GO TO 8590
            IF ( IRMIN (I) . GT .    300 )       GO TO 8590
            IF ( NUNITS(I) . LT .      1 )       GO TO 8900
            IF ( NUNITS(I) . GT . MNU    )       GO TO 8900
      DO 4050  J = 1 , MNU
      DRWSQN(J) = 0
 4050 CONTINUE
      DSQMAX =  0
      DSQMIN = 99
      SUMLEN =  0
      ULENMX =  0
      ULENSM =  0
      DO 4060  J = 1 , NUNITS(I)
      ULENMX = LENV(I) - ULENSM - 3*(NUNITS(I)-J)
      IF ( J . GE . 2 )                          THEN
        ULENMX = ULENMX + UFPD(J,I)
      END IF
      ULENMX = MIN0( ULENMX ,LENV(I),MUL       )
      UFPDMX = MIN0( 99     ,MUL-1,ULEN(J,I)-1 )
            IF ( ULEN  (J,I) . LT .           1 )GO TO 8910
            IF ( ULEN  (J,I) . GT . ULENMX      )GO TO 8910
            IF ( UWID  (J,I) . LT . MINWID      )GO TO 8920
            IF ( UWID  (J,I) . GT . WIDV  (I)   )GO TO 8920
            IF ( UDRWSQ(J,I) . LT .           1 )GO TO 8930
            IF ( UDRWSQ(J,I) . GT . NUNITS(I)   )GO TO 8930
            IF ( UFPD  (J,I) . LT .           0 )GO TO 8940
            IF ( UFPD  (J,I) . GT . UFPDMX      )GO TO 8940
            IF ( URPD  (J,I) . LT .           1 )GO TO 8950
            IF ( URPD  (J,I) . GT . ULEN  (J,I) )GO TO 8950
            IF ( URHPD (J,I) . LT .           0 )GO TO 8960
            IF ( URHPD (J,I) . GT . ULEN  (J,I) )GO TO 8960
            IF ( UTRLEN(J,I) . LT .           0 )GO TO 8970
            IF ( UTRLEN(J,I) . GT . ULEN  (J,I) )GO TO 8970
            IF ( UTRWID(J,I) . LT .           0 )GO TO 8980
            IF ( UTRWID(J,I) . GT . UWID  (J,I) )GO TO 8980
            IF ( UFPD  (J,I) . GE . URPD  (J,I) )GO TO 8990
      IF ( J . LT . NUNITS(I) )                  THEN
            IF ( UFPD  (J,I) . GE . URHPD (J,I) )GO TO 7990
      END IF
      IF ( UTRLEN(J,I) . EQ . 0 )                THEN
            IF ( UTRWID(J,I) . GT .           0 )GO TO 7980
      END IF
      IF ( UTRWID(J,I) . EQ . 0 )                THEN
            IF ( UTRLEN(J,I) . GT .           0 )GO TO 7970
      END IF
      IF ( J . EQ . 1 )                          THEN
        ULENSM = ULENSM + URHPD(J,I)
      ELSE
        ULENSM = ULENSM + URHPD(J,I) - UFPD(J,I)
      END IF
      IF ( NUNITS(I) . EQ . 1 )                  THEN
        SUMLEN = ULEN(J,I)
      ELSE
        IF      ( J . EQ . 1                    )THEN
          SUMLEN = URHPD (J,I)
        ELSE IF ( (J.GE.2).AND.(J.LT.NUNITS(I)) )THEN
          SUMLEN = SUMLEN + URHPD(J,I) - UFPD(J,I)
        ELSE IF ( J . EQ . NUNITS(I)            )THEN
          SUMLEN = SUMLEN + ULEN (J,I) - UFPD(J,I)
        END IF
      END IF
      DSQMAX = MAX0( DSQMAX,UDRWSQ(J,I) )
      DSQMIN = MIN0( DSQMIN,UDRWSQ(J,I) )
      DRWSQN(UDRWSQ(J,I)) = DRWSQN(UDRWSQ(J,I)) + 1
 4060 CONTINUE
                    IF ( SUMLEN . NE . LENV(I) ) GO TO 7960
                    IF ( DSQMIN . NE . 1       ) GO TO 7950
      DO 4070  J = 1 , DSQMAX
                    IF ( DRWSQN(J) . EQ . 0 )    GO TO 7940
 4070 CONTINUE
 4080 CONTINUE
C-----2 BLANK LINES
C-----DRIVER CHARACTERISTICS
C-----        DRIVER CLASS NUMBER
C-----    DRIVER OPERATIONAL FACTOR
C-----    DRIVER REACTION TIME
      IF ( NLINE+6 . GT . LINES )                THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 2
        END IF
      END IF
      IF ( CYESD . EQ . CYES )                   THEN
        CUORP = '(USER SUPPLIED VALUES)'
      ELSE
        CUORP = '(PROGRAM SUPPLIED VALUES)'
      END IF
      WRITE (6,615) CUORP
      NLINE = NLINE + 1
                    IF ( CYESD . EQ . CNO )      GO TO 5010
C-----READ IN DRIVER CHARACTERISTICS
      READ (NIN,503) (IDCHAR(I),I=1,NDRICL)
      READ (NIN,502) (PIJR  (I),I=1,NDRICL)
      GO TO 5020
 5010 CONTINUE
                    IF ( NDRICL . NE . NDCD )    GO TO 8600
 5020 CONTINUE
C-----PRINT DRIVER CHARACTERISTICS
      WRITE (6,604) (I        ,I=1,NDRICL)
      WRITE (6,616) (IDCHAR(I),I=1,NDRICL)
      WRITE (6,618) (PIJR  (I),I=1,NDRICL)
      NLINE = NLINE + 3
      DO 5030  I = 1 , NDRICL
                    IF ( IDCHAR(I) . LT .  50 )  GO TO 8610
                    IF ( IDCHAR(I) . GT . 150 )  GO TO 8610
                    IF ( PIJR  (I) . LT . 0.25 ) GO TO 8620
                    IF ( PIJR  (I) . GT . 5.00 ) GO TO 8620
 5030 CONTINUE
      IF ( IMAGOP . EQ . CYES )                  THEN
        READ (NIN,501)  IMAGL1
        READ (NIN,501)  IMAGL2
      END IF
      RETURN
C-----PROCESS INPUT ERRORS AND STOP
 7940 CONTINUE
      WRITE (ERRMSG,794) I,J
      CALL  ABORTR  ( 'STOP 794 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  794
 7950 CONTINUE
      WRITE (ERRMSG,795) I,DSQMIN,1
      CALL  ABORTR  ( 'STOP 795 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  795
 7960 CONTINUE
      WRITE (ERRMSG,796) I,SUMLEN,LENV(I)
      CALL  ABORTR  ( 'STOP 796 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  796
 7970 CONTINUE
      WRITE (ERRMSG,797) I,J,UTRLEN(J,I)
      CALL  ABORTR  ( 'STOP 797 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  797
 7980 CONTINUE
      WRITE (ERRMSG,798) I,J,UTRWID(J,I)
      CALL  ABORTR  ( 'STOP 798 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  798
 7990 CONTINUE
      WRITE (ERRMSG,799) I,J,UFPD(J,I),URHPD(J,I)
      CALL  ABORTR  ( 'STOP 799 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  799
 8450 CONTINUE
      WRITE (ERRMSG,845) CYESP
      CALL  ABORTR ( ERRMSG )
      STOP  845
 8460 CONTINUE
      WRITE (ERRMSG,846) CYESV
      CALL  ABORTR ( ERRMSG )
      STOP  846
 8470 CONTINUE
      WRITE (ERRMSG,847) CYESD
      CALL  ABORTR ( ERRMSG )
      STOP  847
 8480 CONTINUE
      WRITE (ERRMSG,848) I,CYESVL(I)
      CALL  ABORTR ( ERRMSG )
      STOP  848
 8490 CONTINUE
      WRITE (ERRMSG,849) I,CYESDL(I)
      CALL  ABORTR ( ERRMSG )
      STOP  849
 8500 CONTINUE
      WRITE (ERRMSG,850) NVEHCL,NVCD
      CALL  ABORTR ( ERRMSG )
      STOP  850
 8510 CONTINUE
      WRITE (ERRMSG,851) NDRICL,NDCD
      CALL  ABORTR ( ERRMSG )
      STOP  851
 8520 CONTINUE
      WRITE (ERRMSG,852) I,SUM
      CALL  ABORTR ( ERRMSG )
      STOP  852
 8530 CONTINUE
      WRITE (ERRMSG,853) NVEHCL,NVCD
      CALL  ABORTR ( ERRMSG )
      STOP  853
 8540 CONTINUE
      WRITE (ERRMSG,854) I,LENV  (I),MINLEN,MAXLEN
      CALL  ABORTR ( ERRMSG )
      STOP  854
 8550 CONTINUE
      WRITE (ERRMSG,855) I,IVCHAR(I)
      CALL  ABORTR ( ERRMSG )
      STOP  855
 8560 CONTINUE
      WRITE (ERRMSG,856) I,IDMAX (I)
      CALL  ABORTR ( ERRMSG )
      STOP  856
 8570 CONTINUE
      WRITE (ERRMSG,857) I,IAMAX (I)
      CALL  ABORTR ( ERRMSG )
      STOP  857
 8580 CONTINUE
      WRITE (ERRMSG,858) I,IVMAX (I)
      CALL  ABORTR ( ERRMSG )
      STOP  858
 8590 CONTINUE
      WRITE (ERRMSG,859) I,IRMIN (I)
      CALL  ABORTR ( ERRMSG )
      STOP  859
 8600 CONTINUE
      WRITE (ERRMSG,860) NDRICL,NDCD
      CALL  ABORTR ( ERRMSG )
      STOP  860
 8610 CONTINUE
      WRITE (ERRMSG,861) I,IDCHAR(I)
      CALL  ABORTR ( ERRMSG )
      STOP  861
 8620 CONTINUE
      WRITE (ERRMSG,862) I,PIJR  (I)
      CALL  ABORTR ( ERRMSG )
      STOP  862
 8860 CONTINUE
      WRITE (ERRMSG,886) NVEHAT,NVEHCL,NVCD
      CALL  ABORTR  ( 'STOP 886 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  886
 8880 CONTINUE
      WRITE (ERRMSG,888) I,WIDV(I),MINWID,MAXWID
      CALL  ABORTR  ( 'STOP 888 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  888
 8890 CONTINUE
      WRITE (ERRMSG,889) I,HEIGHT(I),MINHIG,MAXHIG
      CALL  ABORTR  ( 'STOP 889 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  889
 8900 CONTINUE
      WRITE (ERRMSG,890) I,NUNITS(I),1,MNU
      CALL  ABORTR  ( 'STOP 890 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  890
 8910 CONTINUE
      WRITE (ERRMSG,891) I,J,ULEN(J,I),1,ULENMX
      CALL  ABORTR  ( 'STOP 891 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  891
 8920 CONTINUE
      WRITE (ERRMSG,892) I,J,UWID(J,I),MINWID,WIDV(I)
      CALL  ABORTR  ( 'STOP 892 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  892
 8930 CONTINUE
      WRITE (ERRMSG,893) I,J,UDRWSQ(J,I),1,NUNITS(I)
      CALL  ABORTR  ( 'STOP 893 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  893
 8940 CONTINUE
      WRITE (ERRMSG,894) I,J,UFPD(J,I),0,UFPDMX
      CALL  ABORTR  ( 'STOP 894 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  894
 8950 CONTINUE
      WRITE (ERRMSG,895) I,J,URPD(J,I),1,ULEN(J,I)
      CALL  ABORTR  ( 'STOP 895 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  895
 8960 CONTINUE
      WRITE (ERRMSG,896) I,J,URHPD(J,I),0,ULEN(J,I)
      CALL  ABORTR  ( 'STOP 896 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  896
 8970 CONTINUE
      WRITE (ERRMSG,897) I,J,UTRLEN(J,I),0,ULEN(J,I)
      CALL  ABORTR  ( 'STOP 897 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  897
 8980 CONTINUE
      WRITE (ERRMSG,898) I,J,UTRWID(J,I),0,UWID(J,I)
      CALL  ABORTR  ( 'STOP 898 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  898
 8990 CONTINUE
      WRITE (ERRMSG,899) I,J,UFPD(J,I),URPD(J,I)
      CALL  ABORTR  ( 'STOP 899 - ' //
     *                ERRMSG(1:ILNB( ERRMSG )) // ' - ' //
     *                'READYO'                             )
      STOP  899
      END                                                               READYO
C
C
C
      SUBROUTINE WRITDV
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CLASS'
      INCLUDE 'CURAND'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'OUTPUT'
      INCLUDE 'TITLE'
      INTEGER           ID,IV,IVTP,J
      REAL              APIJR,DVCHAR,DVCMAX,DVCMIN,FACT,PERV,SUMP,TTV,
     *                  VCHAR,VMMS,VMPS,VOLIAN,VSIG
  501 FORMAT(A)
  502 FORMAT(2I4,A5,I8,I3)
  503 FORMAT(20I4)
  504 FORMAT(15F5.1)
  505 FORMAT(10A8)
  506 FORMAT(I2,4(I3,3I2,2I3,2I2),:(/,2X,4(I3,3I2,2I3,2I2)))
C
C-----SUBROUTINE WRITDV CALCULATES MINIMUM AND MAXIMUM SPEEDS FOR EACH
C-----DRIVER AND VEHICLE CLASS BASED ON ONE STANDARD DEVIATION AWAY FROM
C-----THE MEAN SPEED FOR EACH APPROACH.  WRITDV ALSO WRITES THE VEHICLE
C-----AND DRIVER CHARACTERISTICS ONTO TAPE FOR SIMPRO.
C
C-----DETERMINE THE MINIMUM/MAXIMUM DRIVER-VEHICLE CHARACTERISTIC
      DVCMIN = 2.25
      DVCMAX = 0.25
      DO 1020  IV = 1 , NVEHCL
      VCHAR = IVCHAR(IV)/10000.0
      DO 1010  ID = 1 , NDRICL
      DVCHAR = IDCHAR(ID)*VCHAR
      DVCMIN = AMIN1( DVCMIN,DVCHAR )
      DVCMAX = AMAX1( DVCMAX,DVCHAR )
 1010 CONTINUE
 1020 CONTINUE
      SUMP = 0.0
      TTV = 0.0
      IVTP=1
C-----CALCULATE THE MINIMUM AND MAXIMUM SPEEDS ALLOWABLE FOR EACH DRIVER
C-----AND VEHICLE CLASS BASED ON ONE STANDARD DEVIATION AWAY FROM THE
C-----MEAN SPEED FOR EACH APPROACH.  THIS CODE ALSO CALCULATES THE
C-----AVERAGE PIJR TIME FOR ALL DRIVER-VEHICLE UNITS
      DO 2030  IAN = 1 , NIBA
      IA = LIBA(IAN)
C-----PROCESS EACH TIME PERIOD FOR THIS APPROACH
      DO 2025  IVTP = 1 , NVTP(IA)
      VOLIAN = IVOL(IVTP,IAN)
      TTV = TTV + VOLIAN
      VSIG = VSIGMA(IVTP,IAN)
      VMMS = VMEAN(IVTP,IAN) - VSIG
      VMPS = VMEAN(IVTP,IAN) + VSIG
      DO 2020  IV = 1 , NVEHCL
      PERV = XPERV(IV,IVTP,IAN)/10000.0
      VCHAR = IVCHAR(IV)/10000.0
      DO 2010  ID = 1 , NDRICL
      SUMP = SUMP + PIJR(ID)*PERV*XPERD(ID,IV)*VOLIAN
      DVCHAR = IDCHAR(ID)*VCHAR
      IF ( DVCHAR . LT . 1.0 )         FACT = -(DVCHAR-1.0)/(DVCMIN-1.0)
      IF ( DVCHAR . GE . 1.0 )         FACT = +(DVCHAR-1.0)/(DVCMAX-1.0)
      IF ( VSIG . LE . 0.0 )           FACT = 0.0
      VMIN(IVTP,IAN,ID,IV) = VMMS + FACT*VSIG
      VMAX(IVTP,IAN,ID,IV) = VMPS + FACT*VSIG
 2010 CONTINUE
 2020 CONTINUE
 2025 CONTINUE
 2030 CONTINUE
      APIJR = SUMP/TTV
C-----WRITE ONTO TAPE FOR SIMPRO THE VEHICLE AND DRIVER CHARACTERISTICS
      WRITE (MODELT,501)   ITITLE
      WRITE (MODELT,502)   NVEHCL,NDRICL,IVERSN,ISEED(1),NVEHAT
      WRITE (MODELT,503)   (LENV  (IV),IV=1,NVEHCL)
      WRITE (MODELT,503)   (IVCHAR(IV),IV=1,NVEHCL)
      WRITE (MODELT,503)   (IDMAX (IV),IV=1,NVEHCL)
      WRITE (MODELT,503)   (IAMAX (IV),IV=1,NVEHCL)
      WRITE (MODELT,503)   (IVMAX (IV),IV=1,NVEHCL)
      WRITE (MODELT,503)   (IRMIN (IV),IV=1,NVEHCL)
      IF ( NVEHAT . GE . 10 )                    THEN
        WRITE (MODELT,505) (CLASSV(IV),IV=1,NVEHCL)
        WRITE (MODELT,503) (HEIGHT(IV),IV=1,NVEHCL)
        WRITE (MODELT,503) (WIDV  (IV),IV=1,NVEHCL)
        DO 3010  IV = 1 , NVEHCL
        WRITE (MODELT,506) NUNITS(IV),
     *                     (ULEN  (J,IV),UWID  (J,IV),UDRWSQ(J,IV),
     *                      UFPD  (J,IV),URPD  (J,IV),URHPD (J,IV),
     *                      UTRLEN(J,IV),UTRWID(J,IV),J=1,NUNITS(IV))
 3010   CONTINUE
      END IF
      WRITE (MODELT,503)  (IDCHAR(ID),ID=1,NDRICL)
      WRITE (MODELT,504)  (PIJR  (ID),ID=1,NDRICL),APIJR
      RETURN
      END                                                               WRITDV
C
C
C
      SUBROUTINE BIASLT
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      DIMENSION         XPERTS(3,NVT,NIA)
      REAL              XPERTS
      INTEGER           IANGLE,IAZIM,ITURN,JAZIM,MDEGST,NL,IVTP
      REAL              FPER,SUM
C-----PROCESS EACH INBOUND APPROACH
      DO 1050  IAN = 1 , NIBA
      IA = LIBA(IAN)
C-----PROCESS EACH VARYING TRAFFIC PERIOD FOR THIS APPROACH
      DO 1045  IVTP = 1 , NVTP(IA)
      XPERTS(1,IVTP,IAN) = 0.0
      XPERTS(2,IVTP,IAN) = 0.0
      XPERTS(3,IVTP,IAN) = 0.0
      MDEGST = NDEGST(IAN)
      IAZIM = IAAZIM(IA) + 180
                    IF ( IAZIM . GT . 360 )      IAZIM = IAZIM - 360
C-----PROCESS EACH OUTBOUND APPROACH
      DO 1040  JAN = 1 , NOBA
C-----MUST DO WHOLE LOOP EACH TIME OR MESSES UP ITURN USAGE AFTER 1035
      JA = LOBA(JAN)
C-----FIND THE ANGLE FROM THE INBOUND APPROACH TO THE OUTBOUND APPROACH
      JAZIM = IAAZIM(JA)
                    IF ( JAZIM . LT . IAZIM )    JAZIM = JAZIM + 360
      IANGLE = JAZIM - IAZIM
C-----IF THE ANGLE IS BETWEEN 0 AND 180-NDEGST THEN GO TO 1010
                    IF ( IANGLE.LT.180-MDEGST )  GO TO 1010
C-----IF THE ANGLE IS BETWEEN 180-NDEGST AND 180+NDEGST THEN GO TO 1020
                    IF ( IANGLE.LE.180+MDEGST )  GO TO 1020
C-----APPROACH JAN IS A RIGHT TURN FOR APPROACH IAN
      ITURN = DTURNR
      GO TO 1030
 1010 CONTINUE
C-----APPROACH JAN IS A U-TURN OR A LEFT TURN FOR APPROACH IAN
      ITURN = DTURNL
      GO TO 1030
 1020 CONTINUE
C-----APPROACH JAN IS A STRAIGHT THROUGH MOVEMENT FOR APPROACH IAN
      ITURN = DTURNS
 1030 CONTINUE
      IITURN(JAN,IAN) = ITURN
 1035 CONTINUE
C           IF ( IVOL(IVTP,IAN) . EQ . 0 )       GO TO 1040
C-----SUM THE TURNING PERCENTAGES BY TURN CODE
      XPERTS(ITURN,IVTP,IAN) = XPERTS(ITURN,IVTP,IAN) + 
     * XPERT(JAN,IVTP,IAN)
 1040 CONTINUE
 1045 CONTINUE
 1050 CONTINUE
C-----PROCESS EACH INBOUND APPROACH
      DO 2040  IAN = 1 , NIBA
      IA = LIBA(IAN)
      DO 2035  IVTP = 1, NVTP(IA)
                    IF ( IVOL(IVTP,IAN) .EQ. 0 ) GO TO 2040
      NL = NLANES(IA)
      FPER = FPERL(IVTP,IAN)/100.0
      IF ( NL . EQ . 1 )                         THEN
        FPER = 1.00
      ELSE
        IF ( XPERL(2,IVTP,IAN) . LE . 0.0 )      FPER = 1.00
      END IF
      SUM = 0.0
C-----PROCESS EACH LANE OF INBOUND APPROACH FROM MEDIAN TO CURB
      DO 2010  ILN = 1 , NL
C-----MAXIMIZE MEDIAN LANE OCCUPANCY FOR U-TURNS AND LEFT TURNS
C     XPERLO(ILN,1,IAN) = AMIN1( XPERL(ILN,IAN),FPER*XPERTS(1,IAN)-SUM )
      XPERLO(ILN,1,IVTP,IAN) = AMIN1( XPERL(ILN,IVTP,IAN),
     * FPER*XPERTS(1,IVTP,IAN)-SUM )
      FPER = 1.00
      SUM = SUM + XPERLO(ILN,1,IVTP,IAN)
 2010 CONTINUE
      FPER = FPERR(IVTP,IAN)/100.0
      IF ( NL . EQ . 1 )                         THEN
        FPER = 1.00
      ELSE
        IF ( XPERL(NL-1,IVTP,IAN) . LE . 0.0 )   FPER = 1.00
      END IF
      SUM = 0.0
C-----PROCESS EACH LANE OF INBOUND APPROACH FROM CURB TO MEDIAN
      DO 2020  ILN = 1 , NL
      JLN = NL - ILN + 1
C-----MAXIMIZE CURB LANE OCCUPANCY FOR RIGHT TURNS
      XPERLO(JLN,3,IVTP,IAN) =
     *  AMIN1( XPERL(JLN,IVTP,IAN)-XPERLO(JLN,1,IVTP,IAN),
     *         FPER*XPERTS(3,IVTP,IAN)-SUM                 )
      FPER = 1.0
      SUM = SUM + XPERLO(JLN,3,IVTP,IAN)
 2020 CONTINUE
C-----PROCESS EACH LANE OF INBOUND APPROACH
      DO 2030  ILN = 1 , NL
C-----DISTRIBUTE STRAIGHT THROUGH MOVEMENTS TO SATISFY LANE OCCUPANCY
      XPERLO(ILN,2,IVTP,IAN) = XPERL(ILN,IVTP,IAN)
     *                 - XPERLO(ILN,1,IVTP,IAN) - XPERLO(ILN,3,IVTP,IAN)
C-----FACTOR XPERLO SO THAT IT RANGES FROM 0.00 TO 100.0
      IF ( XPERTS(1,IVTP,IAN) . EQ . 0.0 )            THEN
        XPERLO(ILN,1,IVTP,IAN) = 0.0
      ELSE
        XPERLO(ILN,1,IVTP,IAN) = 100.0*XPERLO(ILN,1,IVTP,IAN)
     *   /XPERTS(1,IVTP,IAN)
      END IF
      IF ( XPERTS(2,IVTP,IAN) . EQ . 0.0 )            THEN
        XPERLO(ILN,2,IVTP,IAN) = 0.0
      ELSE
        XPERLO(ILN,2,IVTP,IAN) = 100.0*XPERLO(ILN,2,IVTP,IAN)
     *   /XPERTS(2,IVTP,IAN)
      END IF
      IF ( XPERTS(3,IVTP,IAN) . EQ . 0.0 )            THEN
        XPERLO(ILN,3,IVTP,IAN) = 0.0
      ELSE
        XPERLO(ILN,3,IVTP,IAN) = 100.0*XPERLO(ILN,3,IVTP,IAN)
     *   /XPERTS(3,IVTP,IAN)
      END IF
 2030 CONTINUE
 2035 CONTINUE
 2040 CONTINUE
      RETURN
      END                                                               BIASLT
C
C
C
      SUBROUTINE GENHED
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHTEX'
      INCLUDE 'CURAND'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LITCON'
      INCLUDE 'OUTPUT'
      DOUBLE PRECISION  SRAN
      INTEGER           IDNUM,IDVOL,IDVOLB,IDVOLH,IDVOLL,ISUMIV,ISUMNG,
     *                  ISUMVG,ITER,ITER1,ITER2,IVOLGN,IVOLIA,IVTP,
     *                  IV,IVIA,IVIA1,LVTPIAN,NVAIANT,ISUMNGA,ISUMVGA,
     *                  ISUMIVA
      CHARACTER*3       IPERIOD
      CHARACTER*7       IDISTNM
      DATA     IDISTNM  / 7H        /
      REAL              FACTOR,PERDIF,VOLTSB,VOLTSH,VOLTSL,VOLTST
  501 FORMAT(A)
  601 FORMAT(8X,5HTABLE,I3,36H  -  GENERATION OF APPROACH HEADWAYS,///,
     *       12X,25H          VARYING TRAFFIC,/,
     *       12X,51HAPPROACH  ---------------  DISTRIBUTION    NUMBER  ,
C    *           30H   VOLUME    INPUT    PERCENT ,/,
     *           43H   VOLUME    INPUT    PERCENT     NUMBER OF,/,
     *       12X,51H NUMBER    PERIOD   TIME       NAME      GENERATED ,
     *           43H GENERATED  VOLUME  DIFFERENCE   ITERATIONS,/)
C    *           20H   ITER  ITER1 ITER2,/)
C 602 FORMAT(14X,  I3,  7X ,A3,I9,5X,A,3I11,3X,F7.2,2X,3I6)
  602 FORMAT(14X,  I3,  7X ,A3,I9,5X,A,3I11,3X,F7.2,I11)
  603 FORMAT(14X,5HTOTAL,5X,A3,I9, 12X,3I11,3X,F7.2)
  901 FORMAT('STOP 901 - '
     *       'APPROACH NUMBER',I3,' VARYING TRAFFIC PERIOD',I3,
     *       ' - LOW VOLUME NOT FOUND IN 20 TRYS - GENHED')
  902 FORMAT('STOP 902 - '
     *       'APPROACH NUMBER',I3,' VARYING TRAFFIC PERIOD',I3,
     *       ' - HIGH VOLUME NOT FOUND IN 20 TRYS - GENHED')
  903 FORMAT('STOP 903 - '
     *       'APPROACH NUMBER',I3,' HAS MORE THAN ',I4,' VEHICLES'
     *       ' - GENHED')
C
C-----SUBROUTINE GENHED GENERATES APPROACH HEADWAYS UNDER SPECIFIED
C-----DISTRIBUTIONS USING THE ASSOCIATED LOCATION AND DISPERSION
C-----PARAMETERS
C
C-----ISUMNG  = Sum of Number of Generated Vehicles
C-----ISUMVG  = Sum of Volume of Generated Vehicles
C-----ISUMIV  = Sum of Volume of Input     Volumes
C
      ISUMNG  = 0
      ISUMVG  = 0
      ISUMIV  = 0
      IF ( NLINE+NIBA+11 . GT . LINES )          THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 3
        END IF
      END IF
      WRITE (6,601) NTABL
      NTABL = NTABL + 1
      NLINE = NLINE + 6
C-----BEGIN INBOUND APPROACH LOOP FOR HEADWAY GENERATION
      DO 3100  IAN = 1 , NIBA
      IA = LIBA(IAN)
      LVTPIAN = 0
      NVAIANT = 0
      ISUMNGA = 0
      ISUMVGA = 0
      ISUMIVA = 0
C-----PROCESS EACH VARYING TRAFFIC PERIOD FOR THIS APPROACH
      DO 3095  IVTP = 1 , NVTP(IA)
            IF ( IDIST(IVTP,IAN) . EQ . -1 )     GO TO 3100
      IVOLIA = IVOL(IVTP,IAN)
      IDNUM = IDIST(IVTP,IAN)
      IF ( IVOLIA . EQ . 0 )                     THEN
        IF ( IDNUM . GT . 0 )                    THEN
          WRITE (IPERIOD,'(I3)') IVTP
          WRITE(6,602) LIBA(IAN),IPERIOD,LVTP(IVTP,IAN),CDISTN(IDNUM),
     *                 0,0,0,0.0
        ELSE
         WRITE (6,602) LIBA(IAN),IPERIOD,LVTP(IVTP,IAN),ICBL,
     *                 0,0,0,0.0
        END IF
        NVAIAN  = 0
        NVAIANT = NVAIANT + NVAIAN
        GO TO 3095
      END IF
      PARIAN = PARAM(IVTP,IAN)
      VOLTST = 0.9*FLOAT( IVOLIA )
      IDVOLL =  100000000
      VOLTSL = VOLTST
      IDVOLB =  100000000
      VOLTSB = VOLTST
      IDVOLH = -100000000
      VOLTSH = VOLTST
      ITER   = 0
      ITER1  = 0
      ITER2  = 0
      GO TO 2010
 1010 CONTINUE
      IF ( ITER . EQ . 20 )                      THEN
        VOLTST = VOLTSB
        GO TO 2010
      END IF
      IDVOL = IVOLIA - IVOLGN
      IF ( IABS( IDVOL ) . LE . IABS( IDVOLB ) ) THEN
        IDVOLB = IDVOL
        VOLTSB = VOLTST
      END IF
      IF ( IDVOL . GE . 0 )                      THEN
        IF ( IDVOL . LE . IDVOLL )               THEN
          IDVOLL = IDVOL
          VOLTSL = VOLTST
        END IF
      ELSE
        IF ( IDVOL . GE . IDVOLH )               THEN
          IDVOLH = IDVOL
          VOLTSH = VOLTST
        END IF
      END IF
      IF ( ITER . EQ . 1 )                       THEN
        IF ( ITER1 . EQ . 20 )                   GO TO 9010
        IF ( IDVOLL . EQ .  100000000 )          THEN
          VOLTST = 0.9*VOLTST
          GO TO 2020
        END IF
        VOLTST = 1.1*FLOAT( IVOLIA )
        GO TO 2010
      END IF
      IF ( ITER . EQ . 2 )                       THEN
        IF ( ITER2 . EQ . 20 )                   GO TO 9020
        IF ( IDVOLH . EQ . -100000000 )          THEN
          VOLTST = 1.1*VOLTST
          GO TO 2020
        END IF
      END IF
      FACTOR = FLOAT( IDVOLL )/FLOAT( IDVOLL+IABS( IDVOLH ) )
      VOLTST = VOLTSL + (VOLTSH-VOLTSL)*FACTOR
 2010 CONTINUE
      ITER = ITER + 1
 2020 CONTINUE
                    IF ( ITER . EQ . 1 )         ITER1 = ITER1 + 1
                    IF ( ITER . EQ . 2 )         ITER2 = ITER2 + 1
                    IF ( VOLTST . LT . 1.0 )     VOLTST = 1.0
      TMEAN = 3600.0/VOLTST
C
C-----RANDOM NUMBER SEED
C
      IS(IAN)=ISEED(IAN)
      NEWSED = .TRUE.
      IF ( IVTP . EQ . 1 )                       THEN
        IVIA = 2
        QTIME(1,IAN) = 2.0D+00*SRAN( )*TMEAN
        IV = IVIA
      ELSE
        IF ( ITER . EQ . 1 )                     THEN
          IVIA1 = IVIA + NVA(IVTP-1,IAN)
        END IF
        IVIA = IVIA1
      END IF
      ENDQTM = EVTP(IVTP,IAN)
      GO TO ( 3010,3020,3030,3040,3050,3060,3070 ) , IDNUM
 3010 CONTINUE
      QTIME(IV,IAN) = 0.0
      CALL  CONST   ( IV,QTIME(1,IAN) )
      GO TO 3080
 3020 CONTINUE
      CALL  ERLANG  ( IV,QTIME(1,IAN) )
      GO TO 3080
 3030 CONTINUE
      CALL  GAMMA   ( IV,QTIME(1,IAN) )
      GO TO 3080
 3040 CONTINUE
      CALL  LGNRML  ( IV,QTIME(1,IAN) )
      GO TO 3080
 3050 CONTINUE
      CALL  NEGEXP  ( IV,QTIME(1,IAN) )
      GO TO 3080
 3060 CONTINUE
      CALL  SNEGEX  ( IV,QTIME(1,IAN) )
      GO TO 3080
 3070 CONTINUE
      CALL  UNIFRM  ( IV,QTIME(1,IAN) )
 3080 CONTINUE
                    IF ( NVAIAN . LT . 0 )       GO TO 9030
C-----PRINT GENERATED VOLUME INFORMATION
      IVOLGN = NINT( (FLOAT( NVAIAN )*3600.0 )
     *         / (FLOAT( LVTP(IVTP,IAN) )*60.0) )
      PERDIF = 100.0*FLOAT( IVOLGN-IVOLIA )/FLOAT( IVOLIA )
                    IF ( ITER . EQ . 21 )        GO TO 3090
            IF ( IABS( IVOLGN-IVOLIA ) . LE . 1 )GO TO 3090
      GO TO 1010
 3090 CONTINUE
      WRITE (IPERIOD,'(I3)') IVTP
      WRITE (6,602) LIBA(IAN),IPERIOD,LVTP(IVTP,IAN),CDISTN(IDNUM),
C    *              NVAIAN,IVOLGN,IVOLIA,PERDIF,ITER,ITER1,ITER2
     *              NVAIAN,IVOLGN,IVOLIA,PERDIF,ITER
      NVA(IVTP,IAN) = NVAIAN
      IV      = IV      + NVAIAN
      ISUMNGA = ISUMNGA + NVAIAN
      ISUMVGA = ISUMVGA + (IVOLGN*LVTP(IVTP,IAN))/ITSIM
      ISUMIVA = ISUMIVA + (IVOLIA*LVTP(IVTP,IAN))/ITSIM
      MVA(IVTP,IAN) = MVA(MAX0(1,IVTP-1),IAN) + NVAIAN
      LVTPIAN = LVTPIAN + LVTP(IVTP,IAN)
      NVAIANT = NVAIANT + NVAIAN
 3095 CONTINUE
                    IF ( . NOT . IBAP(IA) )      GO TO 3100
      ISUMNG  = ISUMNG + ISUMNGA
      ISUMVG  = ISUMVG + ISUMVGA
      ISUMIV  = ISUMIV + ISUMIVA
                    IF ( NVTP(IA) . LE . 1 )     GO TO 3097
      IPERIOD = 'ALL'
      IF ( NVAIANT . GT . 0 )                    THEN
        PERDIF = 100.0*FLOAT( ISUMVGA-ISUMIVA )/FLOAT( ISUMIVA )
      ELSE
        PERDIF = 0.0
      END  IF
      WRITE (6,602) LIBA(IAN),IPERIOD,LVTPIAN,IDISTNM,NVAIANT,
     *              ISUMVGA,ISUMIVA,PERDIF
 3097 CONTINUE
      WRITE (6,602)
 3100 CONTINUE
                    IF ( ISUMNG . EQ . 0 )       RETURN
      PERDIF = 100.0*FLOAT( ISUMVG-ISUMIV )/FLOAT( ISUMIV )
      WRITE (6,603) IPERIOD,ITSIM,ISUMNG,ISUMVG,ISUMIV,PERDIF
      NLINE = NLINE + NIBA + 5
      RETURN
C-----PROCESS EXECUTION ERROR AND STOP
 9010 CONTINUE
      WRITE (ERRMSG,901) LIBA(IAN),IVTP
      CALL  ABORTR ( ERRMSG )
      STOP  901
 9020 CONTINUE
      WRITE (ERRMSG,902) LIBA(IAN),IVTP
      CALL  ABORTR ( ERRMSG )
      STOP  902
 9030 CONTINUE
      WRITE (ERRMSG,903) LIBA(IAN),NVP
      CALL  ABORTR ( ERRMSG )
      STOP  903
      END                                                               GENHED
C
C
C
      SUBROUTINE CONST  ( IV,QTIMS )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      DIMENSION         QTIMS(NVP)
      DOUBLE PRECISION  QTIMS
      INTEGER           I,IV
      DO 1010  I = IV , NVP
      QTIMS(I) = QTIMS(I-1) + DBLE( TMEAN )
                    IF ( QTIMS(I) . GT .  ENDQTM )GO TO 1020
 1010 CONTINUE
      NVAIAN = -1
      RETURN
 1020 CONTINUE
      NVAIAN = I - IV + 1
      RETURN
      END                                                               CONST
C
C
C
      SUBROUTINE ERLANG ( IV,QTIMS )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      DIMENSION         QTIMS(NVP)
      DOUBLE PRECISION  QTIMS,SRAN
      INTEGER           I,J,K,IV
      REAL              ALPHA,THEAD,TR
      K = NINT( PARIAN )
      ALPHA = REAL( K )/TMEAN
      DO 1020  I = IV , NVP
      TR = 1.0
      DO 1010  J = 1 , K
      TR = TR*SNGL( SRAN( ) )
 1010 CONTINUE
      THEAD = -ALOG( TR )/ALPHA
      QTIMS(I) = QTIMS(I-1) + DBLE( THEAD )
                    IF ( QTIMS(I) . GT .  ENDQTM )GO TO 1030
 1020 CONTINUE
      NVAIAN = -1
      RETURN
 1030 CONTINUE
      NVAIAN = I - IV + 1
      RETURN
      END                                                               ERLANG
C
C
C
      SUBROUTINE GAMMA  ( IV,QTIMS )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      DIMENSION         QTIMS(NVP)
      DOUBLE PRECISION  QTIMS,SRAN
      INTEGER           I,J,K,K1,K2,IV
      REAL              A,ALPHA,Q,THEAD,TR
      A = PARIAN
      ALPHA = A/TMEAN
      K1 = A
      K2 = A + 1.0
      Q = A - K1
      DO 1020  I = IV , NVP
      TR = 1.0
      K = K2
                    IF ( SRAN( ) . GT . Q )      K = K1
      DO 1010  J = 1 , K
      TR = TR*SRAN( )
 1010 CONTINUE
      THEAD = -ALOG( TR )/ALPHA
      QTIMS(I) = QTIMS(I-1) + THEAD
                    IF ( QTIMS(I) . GT .  ENDQTM )GO TO 1030
 1020 CONTINUE
      NVAIAN = -1
      RETURN
 1030 CONTINUE
      NVAIAN = I - IV + 1
      RETURN
      END                                                               GAMMA
C
C
C
      SUBROUTINE LGNRML ( IV,QTIMS )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      DIMENSION         QTIMS(NVP)
      DOUBLE PRECISION  QTIMS,SUM,SRAN
      INTEGER           I,J,IV
      REAL              EX,EY,STDX,STDY,THEAD,VARY
      EX = TMEAN
      STDX = PARIAN
      VARY = ALOG( (STDX**2/(EX**2))+1.0 )
      STDY = SQRT( VARY )
      EY = ALOG( EX ) - 0.5*VARY
      DO 1020  I = IV , NVP
      SUM = 0.0D+00
      DO 1010  J = 1 , 12
      SUM = SUM + SRAN( )
 1010 CONTINUE
      THEAD = EXP( EY+STDY*(SUM-6.0D+00) )
      QTIMS(I) = QTIMS(I-1) + THEAD
                    IF ( QTIMS(I) . GT .  ENDQTM )GO TO 1030
 1020 CONTINUE
      NVAIAN = -1
      RETURN
 1030 CONTINUE
      NVAIAN = I - IV + 1
      RETURN
      END                                                               LGNRML
C
C
C
      SUBROUTINE NEGEXP ( IV,QTIMS )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      DIMENSION         QTIMS(NVP)
      DOUBLE PRECISION  QTIMS,SRAN
      INTEGER           I,IV
      REAL              THEAD
      DO 1010  I = IV , NVP
      THEAD = -ALOG( SNGL( SRAN( ) ) )*TMEAN
      QTIMS(I) = QTIMS(I-1) + THEAD
                    IF ( QTIMS(I) . GT .  ENDQTM )GO TO 1020
 1010 CONTINUE
      NVAIAN = -1
      RETURN
 1020 CONTINUE
      NVAIAN = I - IV + 1
      RETURN
      END                                                               NEGEXP
C
C
C
      SUBROUTINE SNEGEX ( IV,QTIMS )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      DIMENSION         QTIMS(NVP)
      DOUBLE PRECISION  QTIMS,SRAN
      INTEGER           I,IV
      REAL              TAU,TBAR,THEAD
      TAU = PARIAN
      TBAR = TMEAN - TAU
      DO 1010  I = IV , NVP
      THEAD = -ALOG( SNGL( SRAN( ) ) )*TBAR + TAU
      QTIMS(I) = QTIMS(I-1) + THEAD
                    IF ( QTIMS(I) . GT .  ENDQTM )GO TO 1020
 1010 CONTINUE
      NVAIAN = -1
      RETURN
 1020 CONTINUE
      NVAIAN = I - IV + 1
      RETURN
      END                                                               SNEGEX
C
C
C
      SUBROUTINE UNIFRM ( IV,QTIMS )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHTEX'
      INCLUDE 'CONSTN'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'LITCON'
      DIMENSION         QTIMS(NVP)
      DOUBLE PRECISION  QTIMS,SRAN
      INTEGER           I,IV
      REAL              A,B,BMA,THEAD
      A = TMEAN - SQRT3*PARIAN
      B = TMEAN + SQRT3*PARIAN
      BMA = B - A
      DO 1010  I = IV , NVP
      THEAD = A + BMA*SNGL( SRAN( ) )
      QTIMS(I) = QTIMS(I-1) + DBLE( THEAD )
                    IF ( QTIMS(I) . GT .  ENDQTM )GO TO 1020
 1010 CONTINUE
      NVAIAN = -1
      RETURN
 1020 CONTINUE
      NVAIAN = I - IV + 1
      RETURN
      END                                                               UNIFRM
C
C
C
      SUBROUTINE GENDV  ( NIN )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHTEX'
      INCLUDE 'CLASS'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LITCON'
      INCLUDE 'OUTPUT'
      INCLUDE 'STATS'
      INTEGER           NSPRDA                ,NSPRLO
      PARAMETER       ( NSPRDA=NDC*NVC*NVT*NIA,NSPRLO=NAL*  3*NVT*NIA )
      DIMENSION         INEXTV(NIA),SPRDA(NDC,NVC,NVT,NIA),
     *                  SPRLO(NAL,3,NVT,NIA),TPER(NVC)
      REAL              FSTMSV,FSPSSV,FSDTSV,FGTMSV,FGATSV,FRTMSV,
     *                  FRATSV,POSCHK
      INTEGER           FSAPSV
      INTEGER           INEXTV,XRELMI
      REAL              SPRDA,SPRLO,TPER
      CHARACTER*1       IFUTGV,IFUTSV
      CHARACTER*3       VEMERG
      INTEGER           IANGV,IANSV,IC1,IDRCGV,IDRCSV,IFUT,IIBAGV,
     *                  IIBASV,ILNGV,ILNSV,IOBAGV,IOBASV,IPLOGV,IPLOSV,
     *                  ITER,ITIMGV,ITIMSV,ITRNGV,ITRNSV,IV,IVELGV,
     *                  IVELSV,IVHCGV,IVHCSV,IVN,IVTP,IVTPGV,JANSV,
     *                  NIN,NOTEN,NUMGEN,NSTOPL,OANGV,VEHTYP
      LOGICAL           ISPLHD,ITABL
      REAL              HEAD,QBIG,QTIMGV,QTIMSV,VELGV,VFACT,VRANG,
     *                  QTPRSV,HMINCK
C;    INTEGER           I
      DATA     SPRDA  / NSPRDA*0.0 /
      DATA     SPRLO  / NSPRLO*0.0 /
      DATA     XRELMI / 3 /
      DATA     NSTOPL / 0 /
  501 FORMAT(F10.2,7I5,A1,F7.2,I4,6F7.2,A3)
  502 FORMAT(I6,I2,I1,I3,2I2,2I1,A1,F7.2,I4,6F7.2,I6,1X,A3)
  503 FORMAT(I6,I2,I1,I3,2I2,2I1,A1,    53X      ,I6,1X,A3)
  601 FORMAT(I7,F8.2,I5    ,I6    ,I7    ,I5    ,I5,I4,I5    ,5X,A1 ,
     *       F10.2,I4,3(F9.2,F8.2),1X,A3,1X,A)
  602 FORMAT(I7,F8.2,4X,1HX,5X,1HX,6X,1HX,4X,1HX,I5,I4,4X,1HX,5X,1HX,
     *       10X  ,4X,3(9X  ,8X  ),1X,3X,1X,A)
C
C-----SUBROUTINE GENDV GENERATES EACH INDIVIDUAL DRIVER-VEHICLE UNIT FOR
C-----SIMPRO, READS IN SPECIAL VEHICLES (IF ANY), CHECKS THE SPECIAL
C-----VEHICLE'S LOGIN ATTRIBUTES, WRITES ALL CORRECT DRIVER-VEHICLE
C-----UNITS ONTO A TAPE FOR SIMPRO, AND COLLECTS STATISTICAL DATA
C
C-----INITIALIZE VARIABLES AND ARRAYS
      ISPLHD = .TRUE.
      ITABL = .FALSE.
C-----MAKE SURE HEADWAY CHECK DOES NOT FAIL DUE TO ROUNDING, SMALLEST
C-----DT IS 0.01, SO USE HALF OF THAT FOR ROUNDING CHECK
      HMINCK = SNGL( HMIN - 0.005D+00 )
      LVTOT = 0
      NSREAD = 0
      QBIG = 1.00E38
      QTIMSV = QBIG
      QTPRSV = QTIMSV
      IVN = 0
      IC1 = 1
      IVTP = 1
      DO 1010  IAN = 1 , NIBA
      INEXTV(IAN) = 1
      NSWRIT(IAN) = 0
      NGWRIT(IVTP,IAN) = 0
C;    WRITE (6,777) IAN,(XPERT(I,1,IAN),I=1,NOBA)
C;777 FORMAT(* IAN=*I2* XPERT=*6F8.3)
C;    WRITE (6,778) IAN,(XPERV(I,1,IAN),I=1,NVEHCL)
C;778 FORMAT(*     *I2*XPERV=*15F8.3)
 1010 CONTINUE
C-----CHECK TO SEE IF THERE ARE ANY SPECIAL VEHICLES AND READ ONE
                    IF ( IEOF )                  GO TO 2010
      READ (NIN,501,END=1020) QTIMSV,IVHCSV,IDRCSV,IVELSV,IOBASV,
     *                        IIBASV,ILNSV ,IPLOSV,IFUTSV,
     *                        FSTMSV,FSAPSV,FSPSSV,FSDTSV,
     *                        FGTMSV,FGATSV,FRTMSV,FRATSV,VEMERG
      CALL  TOUPR   ( IFUTSV )
C-----NOTE 2 = SPECIAL VEHICLE AS READ IN
      NOTEN = 2
      NOTEU(NOTEN) = 1
      CNOTE(IC1:) = '2,'
      IC1 = IC1 + 2
      NSREAD = NSREAD + 1
      QTPRSV = 0.0
      GO TO 2010
 1020 CONTINUE
      IEOF = .TRUE.
      QTIMSV = QBIG
C-----START OF SORTING LOOP TO WRITE VEHICLES OUT INCREASING IN TIME
 2010 CONTINUE
      QTIMGV = QBIG
      IVTPGV   = 0
C-----FIND INBOUND APPROACH ASSOCIATED WITH LOWEST QUEUE-IN TIME FOR
C-----THE GENERATED VEHICLES
      DO 2020  IAN = 1 , NIBA
      IA = LIBA(IAN)
                    IF ( .NOT. IBAP(IA) )        GO TO 2020
      IV = INEXTV(IAN)
      DO 2015  IVTP = 1 , NVTP(IA)
            IF ( IV . GT . MVA(IVTP,IAN) )       GO TO 2014
            IF ( QTIME(IV,IAN) . GE . QTIMGV )   GO TO 2020
      IVTPGV = IVTP
      QTIMGV = QTIME(IV,IAN)
C-----IANGV IS THE APPROACH WITH SMALLEST QUEUE-IN TIME
      IANGV = IAN
      GO TO 2020
 2014 CONTINUE
 2015 CONTINUE
 2020 CONTINUE
      IVTP = IVTPGV
C-----IF NO MORE SPECIAL VEHICLES GO TO 5010 AND GENERATE LOGIN
C-----ATTRIBUTES FOR GENERATED VEHICLE WITH SMALLEST QUEUE-IN TIME
                    IF ( IEOF )                  GO TO 5010
 3010 CONTINUE
C-----START CHECKING SPECIAL VEHICLE'S LOGIN PARAMETERS
                    IF ( QTIMSV . GT . SIMTIM )  GO TO 3020
C-----IF GENERATED QUEUE-IN TIME IS LESS THAN THE NEXT SPECIAL VEHICLE'S
C-----QUEUE-IN TIME THEN GO TO 5010 AND INSERT GENERATED VEHICLE
                    IF ( QTIMGV . LT . QTIMSV )  GO TO 5010
 3020 CONTINUE
                    IF ( ISPLHD )                GO TO 3030
      WRITE (6,601)
      NLINE = NLINE + 1
 3030 CONTINUE
C-----PRINT SPECIAL VEHICLE AS READ IN
      CALL  GENDVH  ( ITABL,3 )
      WRITE (6,601) 0,QTIMSV,IVHCSV,IDRCSV,IVELSV,IOBASV,IIBASV,ILNSV,
     *              IPLOSV,IFUTSV,FSTMSV,FSAPSV,FSPSSV,FSDTSV,
     *              FGTMSV,FGATSV,FRTMSV,FRATSV,VEMERG,
     *              CNOTE(1:MAX0(IC1-2,1))
      NLINE = NLINE + 1
      ISPLHD = .TRUE.
      IC1 = 1
      CNOTE = ICBL
C-----IF THIS SPECIAL VEHICLE'S LOGIN PARAMETERS HAVE ALREADY BEEN
C-----CHECKED AND ONLY THE HEADWAY WAS CHANGED TO MEET A MINIMUM OF
C-----HMIN SECONDS THEN GO TO 3080 AND CHECK HEADWAY AGAIN
                    IF ( NOTEN . EQ . 25 )           GO TO 3080
C-----SET ERROR INFO FOR ALL FIELDS AND GO TO 4010 AND PRINT SPECIAL
C-----VEHICLE
      IF ( QTIMSV.LT.QTPRSV.OR.QTIMSV.GT.SIMTIM )THEN
C-----  NOTE 3 = QUEUE-IN TIME LESS THAN PREVIOUS SPECIAL VEHICLE OR
C-----           GREATER THAN SIMULATION TIME - SPECIAL VEHICLE ERROR
        NOTEN = 3
        NOTEU(NOTEN) = 1
        CNOTE(IC1:) = '3,'
        IC1 = IC1 + 2
        NSTOP = NSTOP + 1
      END IF
      IF ( IVHCSV.LE.  0 .OR. IVHCSV.GT.NVEHCL ) THEN
C-----  NOTE 4 = VEHICLE CLASS INCORRECT - SPECIAL VEHICLE ERROR
        NOTEN = 4
        NOTEU(NOTEN) = 1
        CNOTE(IC1:) = '4,'
        IC1 = IC1 + 2
        NSTOP = NSTOP + 1
      END IF
C-----SET VEHICLE TYPE
      VEHTYP = 0
      IF ( ( CLASSV(IVHCSV) . EQ . 'BC'   ) . OR .
     *     ( CLASSV(IVHCSV) . EQ . 'BC-1' ) )    THEN
C-----  SET BICYCLE
        VEHTYP = VEHTYP + LAVTB
      END IF
      IF ( VEMERG . EQ . CYES )                  THEN
C-----  SET EMERGENCY VEHICLE
        VEHTYP = VEHTYP + LAVTE
      END IF
      IF ( CLASSV(IVHCSV) . EQ . 'RAIL' )        THEN
C-----  SET RAIL VEHICLE
        VEHTYP = VEHTYP + LAVTR
      END IF
      IF ( VEHTYP . EQ . 0 )                     THEN
C-----  SET NORMAL VEHICLE
        VEHTYP = LAVTV
      END IF
      IF ( IDRCSV.LE.  0 .OR. IDRCSV.GT.NDRICL ) THEN
C-----  NOTE 5 = DRIVER  CLASS INCORRECT - SPECIAL VEHICLE ERROR
        NOTEN = 5
        NOTEU(NOTEN) = 1
        CNOTE(IC1:) = '5,'
        IC1 = IC1 + 2
        NSTOP = NSTOP + 1
      END IF
      IF ( IVELSV.LE.  0 .OR. IVELSV.GT.MDS   )  THEN
C-----  NOTE 6 = QUESTIONABLE DESIRED SPEED - SPECIAL VEHICLE ERROR
        NOTEN = 6
        NOTEU(NOTEN) = 1
        CNOTE(IC1:) = '6,'
        IC1 = IC1 + 2
        NSTOP = NSTOP + 1
      END IF
      DO 3040  JANSV = 1 , NOBA
                    IF ( IOBASV.EQ.LOBA(JANSV) ) GO TO 3050
 3040 CONTINUE
C-----NOTE 7 = LINKING OUTBOUND APPROACH NUMBER INCORRECT - SPECIAL
C-----         VEHICLE ERROR
      JANSV = 0
      NOTEN = 7
      NOTEU(NOTEN) = 1
      CNOTE(IC1:) = '7,'
      IC1 = IC1 + 2
      NSTOP = NSTOP + 1
 3050 CONTINUE
      DO 3060  IANSV = 1 , NIBA
                    IF ( IIBASV.EQ.LIBA(IANSV) ) GO TO 3070
 3060 CONTINUE
C-----NOTE 8 = INBOUND APPROACH NUMBER INCORRECT - SPECIAL VEHCLE ERROR
      IANSV = 0
      NOTEN = 8
      NOTEU(NOTEN) = 1
      CNOTE(IC1:) = '8,'
      IC1 = IC1 + 2
      NSTOP = NSTOP + 1
 3070 CONTINUE
C-----HAVE GOOD INBOUND APPROACH NUMBER
                    IF ( IBAP(IIBASV) )          GO TO 3073
C-----NOTE 31 = TRAFFIC MAY NOT LOGIN (ENTER) THE SIMULATED SYSTEM ON
C-----          THIS INBOUND APPROACH - SPECIAL VEHICLE ERROR
      NOTEN = 31
      NOTEU(NOTEN) = 1
      CNOTE(IC1:) = '31,'
      IC1 = IC1 + 3
      NSTOP = NSTOP + 1
 3073 CONTINUE
      IF ( IANSV . GT . 0 )                      THEN
C-----  HAVE GOOD INBOUND APPROACH NUMBER THAT VEHICLES MAY ENTER ON
C-----  (NOT AN INTERNAL INBOUND APPROACH OF A DIAMOND)
        IF ( ILNSV . LE . 0 . OR . 
     *       ILNSV . GT . NLANES(IIBASV) )       THEN
C-----    NOTE 10 = LANE NUMBER INCORRECT - SPECIAL VEHICLE ERROR
          ILNSV = 0
          NOTEN = 10
          NOTEU(NOTEN) = 1
          CNOTE(IC1:) = '10,'
          IC1 = IC1 + 3
          NSTOP = NSTOP + 1
        ELSE
C-----    HAVE GOOD INBOUND APPROACH NUMBER THAT VEHICLES MAY ENTER ON
C-----    HAVE GOOD INBOUND LANE NUMBER
          IF ( .NOT. MAYENT(IANSV,ILNSV) )       THEN
C-----      NOTE 11 = LANE DOES NOT EXIST AT THE BEGINNING OF THE
C-----                APPROACH - SPECIAL VEHICLE ERROR
            NOTEN = 11
            NOTEU(NOTEN) = 1
            CNOTE(IC1:) = '11,'
            IC1 = IC1 + 3
            NSTOP = NSTOP + 1
          END IF
C-----    HAVE GOOD INBOUND APPROACH NUMBER THAT VEHICLES MAY ENTER ON
C-----    HAVE GOOD INBOUND LANE NUMBER AND VEHICLES MAY ENTER ON IT
          IL = LLANES(ILNSV,IANSV)
          IF ( IAND( LAVT(IL),VEHTYP ) . EQ . 0 )THEN
C-----      NOTE 27 = INBOUND LANE DOES NOT ALLOW VEHICLE TYPE - SPECIAL
C-----                VEHICLE ERROR
            NOTEN = 27
            NOTEU(NOTEN) = 1
            CNOTE(IC1:) = '27,'
            IC1 = IC1 + 3
            NSTOP = NSTOP + 1
          END IF
          IF ( WIDV(IVHCSV) . GT . LWID(IL) )    THEN
C-----      NOTE 30 = VEHICLE TOO WIDE FOR INBOUND LANE - SPECIAL
C-----                VEHICLE ERROR
            NOTEN = 30
            NOTEU(NOTEN) = 1
            CNOTE(IC1:) = '30,'
            IC1 = IC1 + 3
            NSTOP = NSTOP + 1
          END IF
        END IF
        IF ( JANSV . GT . 0 )                    THEN
C-----    HAVE GOOD OUTBOUND APPROACH NUMBER
          IF ( IAND( AAVT(IOBASV),VEHTYP ).EQ.0 )THEN
C-----      NOTE 28 = OUTBOUND APPROACH DOES NOT ALLOW VEHICLE TYPE -
C-----                SPECIAL VEHICLE ERROR
            NOTEN = 28
            NOTEU(NOTEN) = 1
            CNOTE(IC1:) = '28,'
            IC1 = IC1 + 3
            NSTOP = NSTOP + 1
          END IF
          IVTP = 0
          IF ( QTIMSV.LT.QTPRSV .OR.
     *         QTIMSV.GT.SIMTIM )                GO TO 3079
C-----    FIND WHICH IVTP VARYING TRAFFIC PERIOD THIS VEHICLE WILL ENTER
C-----    THE SIMULATION
          DO 3075  IVTP = 1 , NVTP(IIBASV)
            IF ( QTIMSV . LE . EVTP(IVTP,IANSV) )GO TO 3076
 3075     CONTINUE
C-----    STOP HERE-PROGRAM ERROR-CAN NOT FIND IVTP
          GO TO 9040
 3076     CONTINUE
          IF ( XPERT(JANSV,IVTP,IANSV).LE.0.0 )  THEN
C-----      NOTE 9 = QUESTIONABLE OUTBOUND APPROACH (TURN PERCENT) -
C-----               SPECIAL VEHICLE WARNING
            NOTEN = 9
            NOTEU(NOTEN) = 1
            CNOTE(IC1:) = '9,'
            IC1 = IC1 + 2
            NWARN = NWARN + 1
          END IF
          IF ( IAFLAG(IIBASV).EQ.IAFLAG(IOBASV) )THEN
            ITRNSV = IITURN(JANSV,IANSV)
          ELSE
            ITRNSV = IITURN(LOBAR(INTLNK(IOBASV)),IANSV)
          END IF
          DO 3077  ILN = 1 , NLANES(IIBASV)
            IL = LLANES(ILN,IIBASV)
            IF ( ( ITRNSV  . EQ . DTURNL ) . AND .
     *           ( IUT(IL) . EQ . 'U'    ) )     GO TO 3078
            IF ( ( ITRNSV  . EQ . DTURNL ) . AND .
     *           ( ILT(IL) . EQ . 'L'    ) )     GO TO 3078
            IF ( ( ITRNSV  . EQ . DTURNS ) . AND .
     *           ( IST(IL) . EQ . 'S'    ) )     GO TO 3078
            IF ( ( ITRNSV  . EQ . DTURNR ) . AND .
     *           ( IRT(IL) . EQ . 'R'    ) )     GO TO 3078
 3077     CONTINUE
C-----    NOTE 29 = TURN MOVMENT NOT AVAILABLE ON INBOUND APPROACH -
C-----              SPECIAL VEHICLE ERROR
          NOTEN = 29
          NOTEU(NOTEN) = 1
          CNOTE(IC1:) = '29,'
          IC1 = IC1 + 3
          NSTOP = NSTOP + 1
 3078     CONTINUE
        END IF
      END IF
 3079 CONTINUE
C-----IF PRINT INDIVIDUAL WEHICLE STATS ON LOGOUT HAS ANY VALUE EXCEPT
C-----FOR ZERO, SET IT TO ONE AND DO NOT CHECK
                    IF ( IPLOSV . NE . 0 )       IPLOSV = 1
C-----CHECK FREE U-TURN INPUT VALIDITY AND AVAILABILITY
C-----N=NOT APPLICABLE, F=USE FREE UTURN & O=OTHER(2 LEFTS THRU DIAMOND)
      IF ( IFUTSV . NE .  'N' . AND .
     *     IFUTSV . NE .  'F' . AND .
     *     IFUTSV . NE .  'O' )                  THEN
C-----  NOTE 12 = FREE U-TURN PARAMETER IS NOT (N), (F), OR (O) -
C-----            SPECIAL VEHICLE ERROR
        NOTEN = 12
        NOTEU(NOTEN) = 1
        CNOTE(IC1:) = '12,'
        IC1 = IC1 + 3
        NSTOP = NSTOP + 1
      END IF
C     DIAMOND CAN ONLY HAVE 'F' OR 'O'
C     IF FREEUT(JANSV,IANSV) THEN IFUTSV='F' OR 'O'
C     NON-DIAMOND CAN ONLY HAVE 'N'
      IF ( DIAMON )                              THEN
        IF ( (IANSV.GT.0) . AND . (JANSV.GT.0) ) THEN
          IF ( FREEUT(JANSV,IANSV)       )       THEN
            IF ( IFUTSV . NE . 'O' .AND .
     *           IFUTSV . NE . 'F' )             THEN
C-----        NOTE 13 = FREE U-TURN IS AVAILABLE ON THIS DIAMOND, BUT
C-----                  FREE U-TURN PARAMETER IS NOT (F) OR (O) -
C-----                  SPECIAL VEHICLE ERROR
              NOTEN = 13
              NOTEU(NOTEN) = 1
              CNOTE(IC1:) = '13,'
              IC1 = IC1 + 3
              NSTOP = NSTOP + 1
            END IF
          ELSE
            IF ( IFUTSV . NE . 'O' )             THEN
C-----        NOTE 14 = DIAMOND FREE U-TURN IS NOT AVAILABLE FOR THESE
C-----                  LEGS AND FREE U-TURN PARAMETER IS NOT (O) -
C-----                  SPECIAL VEHICLE ERROR
              NOTEN = 14
              NOTEU(NOTEN) = 1
              CNOTE(IC1:) = '14,'
              IC1 = IC1 + 3
              NSTOP = NSTOP + 1
            END IF
          END IF
        END IF
      ELSE
        IF ( IFUTSV . NE .  'N' )                THEN
C-----    NOTE 15 = FREE U-TURN PARAMETER IS NOT (N) FOR A STANDARD
C-----              INTERSECTION - SPECIAL VEHICLE ERROR
          NOTEN = 15
          NOTEU(NOTEN) = 1
          CNOTE(IC1:) = '15,'
          IC1 = IC1 + 3
          NSTOP = NSTOP + 1
        END IF
      END IF
C
C-----CHECK VMS MSG INFO ON SPECIAL VEHICLES
C
C  FSTMSV,FSLCSV,FSLPSV,FSPSSV,FSDTSV,  FGTMSV,FGATSV,  FRTMSV,FRATSV
C  FSTMSV,      ,FSAPSV,FSPSSV,FSDTSV,  FGTMSV,FGATSV,  FRTMSV,FRATSV
C
      IF ( FSTMSV .NE. 0.0 )                     THEN
C-----  FSTMSV = 0 MEANS FORCED STOP IS NOT ACTIVE FOR THIS VEHICLE
        IF ( FSTMSV . LT . QTIMSV .OR.
     *       FSTMSV . GT . SIMTIM )              THEN
C-----    NOTE 16 = FORCED STOP TIME LESS THAN VEHICLE'S QUEUE-IN TIME
C-----              OR GREATER THAN SIMULATION TIME - SPECIAL VEHICLE
C-----              ERROR
          NOTEN = 16
          NOTEU(NOTEN) = 1
          CNOTE(IC1:) = '16,'
          IC1 = IC1 + 3
          NSTOP = NSTOP + 1
        END IF
        IF ( FSAPSV . LT . 0 )                   THEN
C-----    INTERSECTION PATH - PATH NUMBER IS NEGATIVE
          POSCHK = POSMAX
          IF ( -FSAPSV . GT . NPA )              THEN
C-----      NOTE 18 = FORCED STOP INTERSECTION PATH NUMBER IS INCORRECT
C-----                (GREATER THAN MAXIMUM NUMBER OF PATHS) - SPECIAL
C-----                VEHICLE ERROR
            FSAPSV = 0
            NOTEN = 18
            NOTEU(NOTEN) = 1
            CNOTE(IC1:) = '18,'
            IC1 = IC1 + 3
            NSTOP = NSTOP + 1
          END IF
        ELSE
C-----    APPROACH (NOT LEG) NUMBER (BOTH IN AND OUT BOUND)
          IF ( FSAPSV.LT.1 .OR. FSAPSV.GT.NAPS ) THEN
C-----      NOTE 17 = FORCED STOP APPROACH (NOT LEG) NUMBER IS INCORRECT
C-----                (LESS THAN 1 OR GREATER THAN NUMBER OF APPROACHES)
C-----                - SPECIAL VEHICLE ERROR
            FSAPSV = 0
            NOTEN = 17
            NOTEU(NOTEN) = 1
            CNOTE(IC1:) = '17,'
            IC1 = IC1 + 3
            NSTOP = NSTOP + 1
          ELSE
            IF      ( LIBAR(FSAPSV).GT.0 . AND .
     *                IBAP (FSAPSV)      . AND .
     *                FSAPSV.NE.IIBASV   )       THEN
C-----        NOTE 32 = FORCED STOP APPROACH (NOT LEG) NUMBER IS
C-----                  INCORRECT (NOT INBOUND APPROACH NUMBER) -
C-----                  SPECIAL VEHICLE ERROR
              FSAPSV = 0
              NOTEN = 32
              NOTEU(NOTEN) = 1
              CNOTE(IC1:) = '32,'
              IC1 = IC1 + 3
              NSTOP = NSTOP + 1
            ELSE
              POSCHK = 0.0
              DO  ILN = 1 , NLANES(FSAPSV)
                IL = LLANES(ILN,FSAPSV)
                POSCHK = AMAX1( POSCHK,FLOAT( LGEOM(4,IL) ) )
              END DO
            END IF
          END IF
        END IF
        IF ( FSPSSV . LT .    0.0 .OR.
     *       FSPSSV . GT . POSCHK )              THEN
C-----    NOTE 19 = FORCED STOP POSITION LESS THAN 0 OR GREATER THAN
C-----              LONGEST LANE FOR THIS APPROACH - SPECIAL VEHICLE
C-----              ERROR
          NOTEN = 19
          NOTEU(NOTEN) = 1
          CNOTE(IC1:) = '19,'
          IC1 = IC1 + 3
          NSTOP = NSTOP + 1
        END IF
        IF ( FSDTSV . LT . 0.0 .OR.
     *       FSDTSV . GT . SIMTIM-FSTMSV )       THEN
C-----    NOTE 20 = FORCED STOP DWELL TIME LESS THAN 0 OR GREATER THAN
C-----              REMAINING SIMULATION TIME - SPECIAL VEHICLE ERROR
          NOTEN = 20
          NOTEU(NOTEN) = 1
          CNOTE(IC1:) = '20,'
          IC1 = IC1 + 3
          NSTOP = NSTOP + 1
        END IF
      END IF
      IF ( FGTMSV .NE. 0.0 )                     THEN
C-----  FGTMSV = 0 MEANS FORCED GO IS NOT ACTIVE FOR THIS VEHICLE
        IF ( FGTMSV . LT . QTIMSV .OR.
     *       FGTMSV . GT . SIMTIM )              THEN
C-----    NOTE 21 = FORCED GO TIME LESS THAN VEHICLE'S QUEUE-IN TIME OR
C-----              GREATER THAN SIMULATION TIME - SPECIAL VEHICLE ERROR
          NOTEN = 21
          NOTEU(NOTEN) = 1
          CNOTE(IC1:) = '21,'
          IC1 = IC1 + 3
          NSTOP = NSTOP + 1
        END IF
        IF ( FGATSV . LT . 0.0 .OR.
     *       FGATSV . GT . SIMTIM-FGTMSV )       THEN
C-----    NOTE 22 = FORCED GO ACTIVE TIME LESS THAN 0 OR GREATER THAN
C-----              REMAINING SIMULATION TIME - SPECIAL VEHICLE ERROR
          NOTEN = 22
          NOTEU(NOTEN) = 1
          CNOTE(IC1:) = '22,'
          IC1 = IC1 + 3
          NSTOP = NSTOP + 1
        END IF
      END IF
      IF ( FRTMSV .NE. 0.0 )                     THEN
C-----  FRTMSV = 0 MEANS FORCED RUN RED IS NOT ACTIVE FOR THIS VEHICLE
        IF ( FRTMSV . LT . QTIMSV .OR.
     *       FRTMSV . GT . SIMTIM )              THEN
C-----    NOTE 23 = FORCED RUN RED SIGNAL TIME LESS THAN VEHICLE'S
C-----              QUEUE-IN TIME OR GREATER THAN SIMULATION TIME -
C-----              SPECIAL VEHICLE ERROR
          NOTEN = 23
          NOTEU(NOTEN) = 1
          CNOTE(IC1:) = '23,'
          IC1 = IC1 + 3
          NSTOP = NSTOP + 1
        END IF
        IF ( FRATSV . LT . 0.0 .OR.
     *       FRATSV . GT . SIMTIM-FRTMSV )       THEN
C-----    NOTE 24 = FORCED RUN RED SIGNAL ACTIVE TIME LESS THAN 0 OR
C-----              GREATER THAN REMAINING SIMULATION TIME - SPECIAL
C-----              VEHICLE ERROR
          NOTEN = 24
          NOTEU(NOTEN) = 1
          CNOTE(IC1:) = '24,'
          IC1 = IC1 + 3
          NSTOP = NSTOP + 1
        END IF
      END IF
      IF ( VEMERG.NE.CYES . AND . VEMERG.NE.CNO) THEN
C-----  NOTE 37 = EMERGENCY VEHICLE IS NOT (YES) OR (NO) - SPECIAL
C-----            VEHICLE ERROR
        NOTEN = 37
        NOTEU(NOTEN) = 1
        CNOTE(IC1:) = '37,'
        IC1 = IC1 + 3
        NSTOP = NSTOP + 1
      END IF
 3080 CONTINUE
C-----SPECIAL VEHICLE'S LOGIN PARAMETERS CHECKED, NOW CHECK THE
C-----HEADWAY TO SEE IF VEHICLE MAY BE WRITTEN ONTO TAPE FOR SIMPRO
                    IF ( NSTOP . GT . NSTOPL )   GO TO 4010
      HEAD = SNGL( QTIMSV - QTLAST(IANSV,ILNSV) )
                    IF ( HEAD  . LT . HMINCK )   GO TO 4030
C-----NOTE 26 = SPECIAL VEHICLE AS INSERTED
      NOTEN = 26
      NOTEU(NOTEN) = 1
      CNOTE(IC1:) = '26,'
      IC1 = IC1 + 3
      QTLAST(IANSV,ILNSV) = QTIMSV
C-----WRITE SPECIAL VEHICLE ONTO TAPE FOR SIMPRO
      ITIMSV = NINT( QTIMSV*100.0 )
      IVN = IVN + 1
      WRITE (MODELT,502) ITIMSV,IVHCSV,IDRCSV,IVELSV,IOBASV,IIBASV,
     *                   ILNSV,IPLOSV,IFUTSV,FSTMSV,FSAPSV,FSPSSV,
     *                   FSDTSV,FGTMSV,FGATSV,FRTMSV,FRATSV,IVN,VEMERG
      LVTOT = LVTOT + LENV(IVHCSV) + XRELMI
      NSWRIT(IANSV) = NSWRIT(IANSV) + 1
 4010 CONTINUE
C-----PRINT SPECIAL VEHICLE AND ITS NOTEU (POSSIBLY ERROR CODE) AND READ
C-----NEXT SPECIAL VEHICLE AND IF NO MORE GO TO 4020 AND SET IEOF FLAG
      CALL  GENDVH  ( ITABL,2 )
      WRITE (6,601) IVN,QTIMSV,IVHCSV,IDRCSV,IVELSV,IOBASV,IIBASV,ILNSV,
     *              IPLOSV,IFUTSV,FSTMSV,FSAPSV,FSPSSV,FSDTSV,
     *              FGTMSV,FGATSV,FRTMSV,FRATSV,VEMERG,
     *              CNOTE(1:MAX0(IC1-2,1))
      WRITE (6,601)
      QTPRSV = QTIMSV
      ISPLHD = .TRUE.
      NLINE = NLINE + 2
      READ (NIN,501,END=4020) QTIMSV,IVHCSV,IDRCSV,IVELSV,IOBASV,
     *                        IIBASV,ILNSV,IPLOSV,IFUTSV,
     *                        FSTMSV,FSAPSV,FSPSSV,FSDTSV,
     *                        FGTMSV,FGATSV,FRTMSV,FRATSV,VEMERG
      CALL  TOUPR   ( IFUTSV )
      IC1 = 1
      NSTOPL = NSTOP
      CNOTE = ICBL
C-----NOTE 2 = SPECIAL VEHICLE AS READ IN
      NOTEN = 2
      NOTEU(NOTEN) = 1
      CNOTE(IC1:) = '2,'
      IC1 = IC1 + 2
      NSREAD = NSREAD + 1
      GO TO 3010
C-----SET IEOF FLAG AND GO TO 5010 AND CHECK ON GENERATED VEHICLES TO BE
C-----WRITTEN ONTO TAPE FOR SIMPRO
 4020 CONTINUE
      IEOF = .TRUE.
      GO TO 5010
C-----RESET SPECIAL VEHICLE'S QUEUE-IN TIME TO HAVE HMIN SEC HEADWAY
 4030 CONTINUE
      QTIMSV = QTLAST(IANSV,ILNSV) + DBLE( HMIN )
C-----NOTE 25 = HEADWAY LESS THAN 12.3  SECONDS FROM PREVIOUS VEHICLE ON
C-----          SAME APPROACH AND LANE - SPECIAL VEHICLE WARNING -
C-----          HEADWAY INCREASED TO 12.3 SECONDS
      NOTEN = 25
      NOTEU(NOTEN) = 1
      CNOTE(IC1:) = '25,'
      IC1 = IC1 + 3
      NWARN = NWARN + 1
C-----GO TO 3010 AND CHECK FOR NEXT VEHICLE TO BE QUEUED IN
      GO TO 3010
C-----START OF GENERATION OF GENERATED VEHICLES LOGIN ATTRIBUTES
 5010 CONTINUE
C-----IF MINIMUM QUEUE-IN TIME IS VERY LARGE GO TO 6010 AND ENDFILE
C-----TAPE FOR SIMPRO
                    IF ( QTIMGV . GE . QBIG )    GO TO 6010
      IAN    =      IANGV
      IIBAGV = LIBA(IANGV)
                    IF ( .NOT. IBAP(IIBAGV) )    GO TO 9050
      ITER = 0
 5012 CONTINUE
      ITER = ITER + 1
C-----ATTRIBUTES ARE GENERATED UNDER DISCRETE MULTINOMIAL DISTRIBUTION
      CALL  FIXPER  ( XPERT(1,IVTP,IANGV),NVA(IVTP,IANGV),
     *                SPERT(1,IVTP,IANGV),TPER(1),NOBA )
      CALL  DISCRT  ( TPER(1),NOBA,OANGV )
      IOBAGV = LOBA(OANGV)
      IF ( IAFLAG(IIBAGV) . EQ . IAFLAG(IOBAGV) )THEN
        ITRNGV = IITURN(OANGV,IANGV)
      ELSE
        ITRNGV = IITURN(LOBAR(INTLNK(IOBAGV)),IANGV)
      END IF
      DO 5014  ILN = 1 , NLANES(IIBAGV)
        IL = LLANES(ILN,IIBAGV)
        IF ( ITRNGV.EQ.DTURNL.AND.IUT(IL).EQ.'U')GO TO 5016
        IF ( ITRNGV.EQ.DTURNL.AND.ILT(IL).EQ.'L')GO TO 5016
        IF ( ITRNGV.EQ.DTURNS.AND.IST(IL).EQ.'S')GO TO 5016
        IF ( ITRNGV.EQ.DTURNR.AND.IRT(IL).EQ.'R')GO TO 5016
 5014 CONTINUE
                    IF ( ITER . LT . 100 )       GO TO 5012
C-----NOTE 33 = TURN MOVMENT NOT AVAILABLE ON INBOUND APPROACH -
C-----          GENERATED VEHICLE IGNORED
      NOTEU(33) = 1
      NWARN   = NWARN  + 1
      NGELIM  = NGELIM + 1
      CALL  GENDVH  ( ITABL,2 )
      WRITE (6,601)
      NLINE = NLINE + 1
      WRITE (6,602) 0,QTIMGV,IIBAGV,0,'33'
      NLINE = NLINE + 1
      INEXTV(IANGV) = INEXTV(IANGV) + 1
C-----GO TO 2010 AND CHECK TO FIND APPROACH WITH MINIMUM QUEUE-IN TIME
      GO TO 2010
 5016 CONTINUE
      NUMGEN = NINT( NVA(IVTP,IANGV)*XPERT(OANGV,IVTP,IANGV)/100.0 )
C;    IF ( NUMGEN . LE .0 ) WRITE (6,717) IANGV,OANGV,
C;   *                                    XPERT(OANGV,IVTP,IANGV)
C;717 FORMAT(* IANGV=*I2* OANGV=*I2* XPERT(OANGV,IVTP,IANGV)=*F8.2)
      IF ( FREEUT(OANGV,IANGV) )                 THEN
        CALL  FIXPER  ( XPERUT(1,IVTP,IANGV),NUMGEN,
     *                  SPERUT(1,IVTP,IANGV),TPER(1),2 )
        CALL  DISCRT  ( TPER(1),2,IFUT )
        IF ( IFUT . EQ . 1 )                     THEN
          IFUTGV = 'F'
        ELSE
          IFUTGV = 'O'
        END IF
      ELSE
        IFUTGV = ICBL
      END IF
      ITER = 0
 5020 CONTINUE
      ITER = ITER + 1
      CALL  FIXPER  ( XPERLO(1,ITRNGV,IVTP,IANGV),NUMGEN,
     *                SPRLO(1,ITRNGV,IVTP,IANGV),
     *                TPER(1),NLANES(IIBAGV) )
      CALL  DISCRT  ( TPER(1),NLANES(IIBAGV),ILNGV )
                    IF ( MAYENT(IANGV,ILNGV) )   GO TO 5022
                    IF ( ITER . LT . 100 )       GO TO 5020
C-----NOTE 34 = LANE DOES NOT EXIST AT THE BEGINNING OF THE APPROACH -
C-----          GENERATED VEHICLE IGNORED
      NOTEU(34) = 1
      NWARN   = NWARN  + 1
      NGELIM  = NGELIM + 1
      CALL  GENDVH  ( ITABL,2 )
      WRITE (6,601)
      NLINE = NLINE + 1
      WRITE (6,602) 0,QTIMGV,IIBAGV,0,'34'
      NLINE = NLINE + 1
      INEXTV(IANGV) = INEXTV(IANGV) + 1
C-----GO TO 2010 AND CHECK TO FIND APPROACH WITH MINIMUM QUEUE-IN TIME
      GO TO 2010
 5022 CONTINUE
C-----CHECK HEADWAYS BETWEEN VEHICLES ON THE SAME APPROACH AND LANE SO
C-----THAT THEY ARE ARE MINIMUM OF HMIN SECONDS APART.  IF HMIN IS
C-----VIOLATED THEN TRY TO GENERATE AN ALTERNATE LANE (100 CHANCES)
      HEAD = SNGL( QTIMGV - QTLAST(IANGV,ILNGV) )
                    IF ( HEAD . GE . HMIN )      GO TO 5030
                    IF ( ITER . LT . 100 )       GO TO 5020
C-----NOTE 1 = HEADWAY LESS THAN 12.3 SECONDS FROM PREVIOUS VEHICLE FOR
C-----         THIS APPROACH AND ITS LANE(S) - GENERATED VEHICLE IGNORED
      NOTEU(1) = 1
      NWARN   = NWARN  + 1
      NGELIM  = NGELIM + 1
      CALL  GENDVH  ( ITABL,2 )
                    IF ( NOTEN . NE . 23    )        GO TO 5025
                    IF ( .NOT. ISPLHD )          GO TO 5025
      WRITE (6,601)
      NLINE = NLINE + 1
 5025 CONTINUE
      WRITE (6,602) 0,QTIMGV,IIBAGV,ILNGV,'1'
      ISPLHD = .FALSE.
      NLINE = NLINE + 1
      INEXTV(IANGV) = INEXTV(IANGV) + 1
C-----GO TO 2010 AND CHECK TO FIND APPROACH WITH MINIMUM QUEUE-IN TIME
      GO TO 2010
 5030 CONTINUE
      ITER = 0
 5031 CONTINUE
      ITER = ITER + 1
      CALL  FIXPER ( XPERV(1,IVTP,IANGV),NVA(IVTP,IANGV),
     *               SPERV(1,IVTP,IANGV),TPER(1),NVEHCL )
      CALL  DISCRT  ( TPER(1),NVEHCL,IVHCGV )
C-----SET VEHICLE TYPE
      VEHTYP = 0
      IF ( ( CLASSV(IVHCGV) . EQ . 'BC'   ) . OR .
     *     ( CLASSV(IVHCGV) . EQ . 'BC-1' ) )    THEN
C-----  SET BICYCLE
        VEHTYP = VEHTYP + LAVTB
      END IF
C     IF ( VEMERG . EQ . CYES )                  THEN
C-----  SET EMERGENCY VEHICLE (SPECIAL VEHICLES ONLY)
C       VEHTYP = VEHTYP + LAVTE
C     END IF
      IF ( CLASSV(IVHCGV) . EQ . 'RAIL' )        THEN
C-----  SET RAIL VEHICLE
        VEHTYP = VEHTYP + LAVTR
      END IF
      IF ( VEHTYP . EQ . 0 )                     THEN
C-----  SET NORMAL VEHICLE
        VEHTYP = LAVTV
      END IF
      IL = LLANES(ILNGV,IIBAGV)
      IF ( IAND( LAVT(IL)    ,VEHTYP ) . EQ . VEHTYP )
     *                                           GO TO 5032
                    IF ( ITER . LT . 100 )       GO TO 5031
C-----NOTE 35 = INBOUND LANE DOES NOT ALLOW VEHICLE TYPE - GENERATED
C-----          VEHICLE IGNORED
      NOTEU(35) = 1
      NWARN   = NWARN  + 1
      NGELIM  = NGELIM + 1
      CALL  GENDVH  ( ITABL,2 )
      WRITE (6,601)
      NLINE = NLINE + 1
      WRITE (6,602) 0,QTIMGV,IIBAGV,0,'35'
      NLINE = NLINE + 1
      INEXTV(IANGV) = INEXTV(IANGV) + 1
C-----GO TO 2010 AND CHECK TO FIND APPROACH WITH MINIMUM QUEUE-IN TIME
      GO TO 2010
 5032 CONTINUE
      IF ( IAND( AAVT(IOBAGV),VEHTYP ) . EQ . VEHTYP )
     *                                           GO TO 5033
                    IF ( ITER . LT . 100 )       GO TO 5031
C-----NOTE 36 = OUTBOUND APPROACH DOES NOT ALLOW VEHICLE TYPE -
C-----          GENERATED VEHICLE IGNORED
      NOTEU(36) = 1
      NWARN   = NWARN  + 1
      NGELIM  = NGELIM + 1
      CALL  GENDVH  ( ITABL,2 )
      WRITE (6,601)
      NLINE = NLINE + 1
      WRITE (6,602) 0,QTIMGV,IIBAGV,0,'36'
      NLINE = NLINE + 1
      INEXTV(IANGV) = INEXTV(IANGV) + 1
C-----GO TO 2010 AND CHECK TO FIND APPROACH WITH MINIMUM QUEUE-IN TIME
      GO TO 2010
 5033 CONTINUE
      IF ( WIDV(IVHCGV) . LE . LWID(IL) )        GO TO 5034
                    IF ( ITER . LT . 100 )       GO TO 5031
 5034 CONTINUE
      SPERT(OANGV,IVTP,IANGV)       = SPERT(OANGV,IVTP,IANGV)       +1.0
      SPERL(ILNGV,IVTP,IANGV)       = SPERL(ILNGV,IVTP,IANGV)       +1.0
      SPRLO(ILNGV,ITRNGV,IVTP,IANGV)= SPRLO(ILNGV,ITRNGV,IVTP,IANGV)+1.0
      IF ( IFUTGV . NE . ICBL )                  THEN
        SPERUT(IFUT,IVTP,IANGV) = SPERUT(IFUT,IVTP,IANGV) + 1.0
      END IF
      SPERV(IVHCGV,IVTP,IANGV)      = SPERV(IVHCGV,IVTP,IANGV)      +1.0
      NUMGEN = NINT( NVA(IVTP,IANGV)*XPERV(IVHCGV,IVTP,IANGV)/100.0 )
C;    IF ( NUMGEN.LE.0 ) WRITE (6,718) IANGV,IVHCGV,XPERV(IVHCGV,IANGV)
C;718 FORMAT(* IANGV=*I2* IVHCGV=*I2* XPERV(IVHCGV,IANGV)=*F8.2)
      CALL  FIXPER  ( XPERD(1,IVHCGV),NUMGEN,SPRDA(1,IVHCGV,IVTP,IANGV),
     *                TPER(1),NDRICL )
      CALL  DISCRT  ( TPER(1),NDRICL,IDRCGV )
      SPERD(IDRCGV,IVHCGV)           =SPERD(IDRCGV,IVHCGV)           +1.
      SPRDA(IDRCGV,IVHCGV,IVTP,IANGV)=SPRDA(IDRCGV,IVHCGV,IVTP,IANGV)+1.
      IPLOGV = 0
                    IF ( CYESVL(IVHCGV).EQ.CYES )IPLOGV = 1
                    IF ( CYESDL(IDRCGV).EQ.CYES )IPLOGV = 1
C-----ARRIVING SPEED IS GENERATED UNDER NORMAL DISTRIBUTION AND MUST BE
C-----WITHIN ONE STANDARD DEVIATION OF APPROACH'S MEAN SPEED WITH A
C-----SLIGHT VARIATION TO ACCOUNT FOR DIFFERENT DRIVERS AND VEHICLES
      ITER = 0
 5040 CONTINUE
      ITER = ITER + 1
      CALL  NORMAL  ( VMEAN(IVTP,IANGV),VSIGMA(IVTP,IANGV),VELGV )
      IF ( ITER . LE . 100 )                     THEN
        IF ( VELGV . LT . VMIN(IVTP,IANGV,IDRCGV,IVHCGV) )
     *                                           GO TO 5040
        IF ( VELGV . GT . VMAX(IVTP,IANGV,IDRCGV,IVHCGV) )
     *                                           GO TO 5040
      ELSE
        VRANG = VMAX(IVTP,IANGV,IDRCGV,IVHCGV)
     *        - VMIN(IVTP,IANGV,IDRCGV,IVHCGV)
        IF ( VSIGMA(IVTP,IANGV) . EQ . 0.0 )     THEN
          VELGV = VMIN(IVTP,IANGV,IDRCGV,IVHCGV) + VRANG*0.5
        ELSE
          VFACT = (VELGV-VMEAN(IVTP,IANGV))/VSIGMA(IVTP,IANGV)
          VFACT = AMIN1(1.0,AMAX1(-1.0,VFACT))
          VELGV = VMIN(IVTP,IANGV,IDRCGV,IVHCGV) + VRANG*0.5*(VFACT+1.0)
        END IF
      END IF
      IVELGV = NINT( VELGV )
C-----WRITE GENERATED DRIVER-VEHICLE UNIT ONTO TAPE FOR SIMPRO
      ITIMGV = NINT( QTIMGV*100.0 )
      IVN = IVN + 1
      WRITE (MODELT,503) ITIMGV,IVHCGV,IDRCGV,IVELGV,IOBAGV,IIBAGV,
     *                   ILNGV,IPLOGV,IFUTGV,IVN,CNO
      LVTOT = LVTOT + LENV(IVHCGV) + XRELMI
      QTLAST(IANGV,ILNGV) = QTIMGV
      INEXTV(IANGV) = INEXTV(IANGV) + 1
      NGWRIT(IVTP,IANGV) = NGWRIT(IVTP,IANGV) + 1
      GO TO 2010
C-----WRITE AN END OF FILE ONTO TAPE FOR SIMPRO
 6010 CONTINUE
      ENDFILE(MODELT)
      WRITE (6,601)
      NLINE = NLINE + 1
      RETURN
 9040 CONTINUE
      ERRMSG = 'STOP 904 - VARYING TRAFFIC PERIOD PROBLEM - GENDV'
      CALL  ABORTR ( ERRMSG )
      STOP  904
 9050 CONTINUE
      ERRMSG = 'STOP 905 - INBOUND APPROACH IS NOT REAL INBOUND - GENDV'
      CALL  ABORTR ( ERRMSG )
      STOP  905
      END                                                               GENDV
C
C
C
      SUBROUTINE GENDVH ( ITABL,I )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'OUTPUT'
      INTEGER           I
      LOGICAL           ITABL
  501 FORMAT(A)
  601 FORMAT(8X,5HTABLE,I3,41H  -  LISTING OF GENERATED VEHICLES IGNORE,
     *32HD AND SPECIAL VEHICLES PROCESSED,//)
  602 FORMAT(
     *62H                                   APPROACH  INB  LOG        -,
     *62H--------FORCED STOP--------  ---FORCED GO---  ---FORCED RUN-- ,

     * 9HEMERGENCY,/,
     *62H VEHICLE         VEH  DRIVER SPEED -------- LANE  OUT  FREE  T,
     *62HIME  +APP/ POSITION   DWELL  TIME     ACTIVE  ---RED SIGNAL-- ,
     *3HVEH       ,/,
     *62H NUMBER   QTIME CLASS  CLASS (FPS) OUT  INB  NUM PRINT  UT   (,
     *62HSEC) -PATH     (FT)   (SEC)  (SEC)     (SEC)  TIME     ACTIVE ,
     * 9H    NOTES,/,
     *62H ------ ------- ----- ------ ----- ---   -- ---- ----- ----  -,
     *62H------ ---  ------- -------  ------- -------  ------- ------- ,
     * 9H--- -----)
C
C-----SUBROUTINE GENDVH PRINTS THE TABLE AND TABLE HEADINGS THE FIRST
C-----TIME IT IS CALLED AND FROM THEN ON ONLY CHECKS TO SEE IF A NEW
C-----PAGE HEADING IS NEEDED BEFORE PRINTING OUT A VEHICLE AND ITS NOTE
C
                    IF ( ITABL )                 GO TO 1010
      IF ( NLINE+12 . GT . LINES )               THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 3
        END IF
      END IF
      ITABL = .TRUE.
      WRITE (6,601) NTABL
      WRITE (6,602)
      NLINE = NLINE + 7
      NTABL = NTABL + 1
      RETURN
 1010 CONTINUE
      IF ( NLINE+I . GT . LINES )                THEN
        CALL  HEADER
        WRITE (6,602)
        NLINE = NLINE + 4
      END IF
      RETURN
      END                                                               GENDVH
C
C
C
      SUBROUTINE FIXPER ( GPER,NUMTGN,AGEN,TPER,NCLS )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INTEGER           NCLS
      DIMENSION         AGEN(NCLS),GPER(NCLS),TPER(NCLS)
      REAL              AGEN      ,GPER      ,TPER
      INTEGER           I,IGENS,NUMGEN,NUMTGN
      REAL              SPER
C
C-----GPER   - GOAL PERCENTAGE PER CLASS
C-----NUMTGN - NUMBER TO GENERATE (TOTAL)
C-----AGEN   - NUMBER ALREADY GENERATED PER CLASS
C-----TPER   - TRIAL PERCENTAGE PER CLASS - CORRECTED (OUTPUT TO DISCRT)
C-----NCLS   - NUMBER OF CLASSES
C
                    IF ( NCLS . EQ . 1 )         GO TO 2010
      IGENS = 0
      DO 1000  I = 1 , NCLS
      IGENS = IGENS + AGEN(I)
 1000 CONTINUE
                    IF ( IGENS . LE . NCLS )     GO TO 2010
      NUMGEN = NUMTGN
                    IF ( IGENS . LT . NUMGEN )   GO TO 1005
      NUMGEN = IGENS + 1
 1005 CONTINUE
      IF ( FLOAT( IGENS )/FLOAT( NUMGEN ).LT..1 )GO TO 2010
C;    WRITE (6,701) NCLS,NCLS,(GPER(I),I=1,NCLS),NCLS,
C;   *              (AGEN(I),I=1,NCLS),IGENS,NUMGEN
C;701 FORMAT(* NCLS=*I2* GPER=*=F6.2* AGEN=*=F5.0/* IGENS=*I4,
C;   *       * NUM=*I4)
      SPER = 0.0
      DO 1010  I = 1 , NCLS
      TPER(I) = AMAX1( GPER(I)*NUMGEN-AGEN(I)*100.0,0.0 )
      SPER = SPER + TPER(I)
 1010 CONTINUE
                    IF ( SPER . EQ . 0.0 )       RETURN
      DO 1020  I = 1 , NCLS
      TPER(I) = TPER(I)/SPER*100.0
 1020 CONTINUE
C;    WRITE (6,702) (TPER(I),I=1,NCLS)
C;702 FORMAT(* TPER=*15F6.2)
      RETURN
 2010 CONTINUE
      DO 2020  I = 1 , NCLS
      TPER(I) = GPER(I)
 2020 CONTINUE
      RETURN
      END                                                               FIXPER
C
C
C
      SUBROUTINE DISCRT ( XPER,NUM,I )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      DIMENSION         XPER(NVC)
      REAL              XPER
      INTEGER           I,NUM
      DOUBLE PRECISION  RANNUM,SUM
      DOUBLE PRECISION  SRAN
C
C-----SUBROUTINE DISCRT GENERATES A DISCRETE MULTINOMIAL RANDOM DEVIATE
C-----FOR A GIVEN PERCENTAGE ( 0.00 TO 100.0)
C
      RANNUM = SRAN( )*100.0D+00
      SUM = 0.0D+00
      DO 1010  I = 1 , NUM
      SUM = SUM + DBLE( XPER(I) )
                    IF ( SUM . GE . RANNUM )     RETURN
 1010 CONTINUE
      I = NUM
      RETURN
      END                                                               DISCRT
C
C
C
      SUBROUTINE NORMAL ( PMEAN,PSIGMA,VEL )
      IMPLICIT NONE                                                     CCODE=C.
      INTEGER           I
      DOUBLE PRECISION  SRAN,SUM
      REAL              PMEAN,PSIGMA,VEL
C
C-----SUBROUTINE NORMAL GENERATES NORMALLY DISTRIBUTED RANDOM DEVIATES
C
      SUM = 0.0D+00
      DO 1010  I = 1 , 12
      SUM = SUM + SRAN( )
 1010 CONTINUE
      VEL = PMEAN + PSIGMA*SNGL( (SUM-6.0D+00) )
      RETURN
      END                                                               NORMAL
C
C
C
      SUBROUTINE PNOTES
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CHTEX'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'OUTPUT'
      INTEGER           I,LTEST
  600 FORMAT(12X,32HNOTE  EXPLANATION OF THE NOTE(S),/)
  601 FORMAT(13X,21H 1  HEADWAY LESS THAN,F4.1,21H SECONDS FROM PREVIOU,
     *       55HS VEHICLE FOR THIS APPROACH AND ITS LANE(S) - GENERATED,
     *       16H VEHICLE IGNORED)
  602 FORMAT(13X,30H 2  SPECIAL VEHICLE AS READ IN)
  603 FORMAT(13X,51H 3  QUEUE-IN TIME LESS THAN PREVIOUS SPECIAL VEHICL,
     *       55HE OR GREATER THAN SIMULATION TIME - SPECIAL VEHICLE ERR,
     *        2HOR)
  604 FORMAT(13X,51H 4  VEHICLE CLASS INCORRECT - SPECIAL VEHICLE ERROR)
  605 FORMAT(13X,51H 5  DRIVER  CLASS INCORRECT - SPECIAL VEHICLE ERROR)
  606 FORMAT(13X,51H 6  QUESTIONABLE DESIRED SPEED - SPECIAL VEHICLE ER,
     *       3HROR)
  607 FORMAT(13X,51H 7  LINKING OUTBOUND APPROACH NUMBER INCORRECT - SP,
     *       19HECIAL VEHICLE ERROR)
  608 FORMAT(13X,51H 8  INBOUND APPROACH NUMBER INCORRECT - SPECIAL VEH,
     *       10HICLE ERROR)
  609 FORMAT(13X,51H 9  QUESTIONABLE OUTBOUND APPROACH (TURN PERCENT) -,
     *       24H SPECIAL VEHICLE WARNING)
  610 FORMAT(13X,49H10  LANE NUMBER INCORRECT - SPECIAL VEHICLE ERROR)
  611 FORMAT(13X,51H11  LANE DOES NOT EXIST AT THE BEGINNING OF THE APP,
     *       29HROACH - SPECIAL VEHICLE ERROR)
  612 FORMAT(13X,51H12  FREE U-TURN PARAMETER IS NOT (N), (F), OR (O) -,
     *       22H SPECIAL VEHICLE ERROR)
  613 FORMAT(13X,51H13  FREE U-TURN IS AVAILABLE ON THIS DIAMOND, BUT F,
     *       55HREE U-TURN PARAMETER IS NOT (F) OR (O) - SPECIAL VEHICL,
     *        7HE ERROR)
  614 FORMAT(13X,51H14  DIAMOND FREE U-TURN IS NOT AVAILABLE FOR THESE ,
     *       55HLEGS AND FREE U-TURN PARAMETER IS NOT (O) - SPECIAL VEH,
     *       10HICLE ERROR)
  615 FORMAT(13X,51H15  FREE U-TURN PARAMETER IS NOT (N) FOR A STANDARD,
     *       37H INTERSECTION - SPECIAL VEHICLE ERROR)
  616 FORMAT(13X,51H16  FORCED STOP TIME LESS THAN VEHICLE'S QUEUE-IN T,
     *       55HIME OR GREATER THAN SIMULATION TIME - SPECIAL VEHICLE E,
     *        4HRROR)
  617 FORMAT(13X,51H17  FORCED STOP APPROACH (NOT LEG) NUMBER IS INCORR,
     *       54HECT (LESS THAN 1 OR GREATER THAN NUMBER OF APPROACHES),
     *       /,17X,23H- SPECIAL VEHICLE ERROR)
  618 FORMAT(13X,51H18  FORCED STOP INTERSECTION PATH NUMBER IS INCORRE,
     *       55HCT (GREATER THAN MAXIMUM NUMBER OF PATHS) - SPECIAL VEH,
     *       10HICLE ERROR)
  619 FORMAT(13X,51H19  FORCED STOP POSITION LESS THAN 0 OR GREATER THA,
     *       55HN LONGEST LANE FOR THIS APPROACH - SPECIAL VEHICLE ERRO,
     *        1HR)
  620 FORMAT(13X,51H20  FORCED STOP DWELL TIME LESS THAN 0 OR GREATER T,
     *       53HHAN REMAINING SIMULATION TIME - SPECIAL VEHICLE ERROR)
  621 FORMAT(13X,51H21  FORCED GO TIME LESS THAN VEHICLE'S QUEUE-IN TIM,
     *       55HE OR GREATER THAN SIMULATION TIME - SPECIAL VEHICLE ERR,
     *        2HOR)
  622 FORMAT(13X,51H22  FORCED GO ACTIVE TIME LESS THAN 0 OR GREATER TH,
     *       52HAN REMAINING SIMULATION TIME - SPECIAL VEHICLE ERROR)
  623 FORMAT(13X,51H23  FORCED RUN RED SIGNAL TIME LESS THAN VEHICLE'S ,
     *       55HQUEUE-IN TIME OR GREATER THAN SIMULATION TIME - SPECIAL,
     *       14H VEHICLE ERROR)
  624 FORMAT(13X,51H24  FORCED RUN RED SIGNAL ACTIVE TIME LESS THAN 0 O,
     *       55HR GREATER THAN REMAINING SIMULATION TIME - SPECIAL VEHI,
     *        9HCLE ERROR)
  625 FORMAT(13X,21H25  HEADWAY LESS THAN,F4.1,21H SECONDS FROM PREVIOU,
     *       35HS VEHICLE ON SAME APPROACH AND LANE,
     *       /,17X,48H- SPECIAL VEHICLE WARNING - HEADWAY INCREASED TO,
     *       F4.1,8H SECONDS)
  626 FORMAT(13X,31H26  SPECIAL VEHICLE AS INSERTED)
  627 FORMAT(13X,51H27  INBOUND LANE DOES NOT ALLOW VEHICLE TYPE - SPEC,
     *       17HIAL VEHICLE ERROR)
  628 FORMAT(13X,51H28  OUTBOUND APPROACH DOES NOT ALLOW VEHICLE TYPE -,
     *       22H SPECIAL VEHICLE ERROR)
  629 FORMAT(13X,51H29  TURN MOVMENT NOT AVAILABLE ON INBOUND APPROACH ,
     *       23H- SPECIAL VEHICLE ERROR)
  630 FORMAT(13X,51H30  VEHICLE TOO WIDE FOR INBOUND LANE - SPECIAL VEH,
     *       10HICLE ERROR)
  631 FORMAT(13X,51H31  TRAFFIC MAY NOT LOGIN (ENTER) THE SIMULATED SYS,
     *       52HTEM ON THIS INBOUND APPROACH - SPECIAL VEHICLE ERROR)
  632 FORMAT(13X,51H32  FORCED STOP APPROACH (NOT LEG) NUMBER IS INCORR,
     *       55HECT (NOT INBOUND APPROACH NUMBER) - SPECIAL VEHICLE ERR,
     *        2HOR)
  633 FORMAT(13X,51H33  TURN MOVMENT NOT AVAILABLE ON INBOUND APPROACH ,
     *       27H- GENERATED VEHICLE IGNORED)
  634 FORMAT(13X,51H34  LANE DOES NOT EXIST AT THE BEGINNING OF THE APP,
     *       33HROACH - GENERATED VEHICLE IGNORED)
  635 FORMAT(13X,51H35  INBOUND LANE DOES NOT ALLOW VEHICLE TYPE - GENE,
     *       21HRATED VEHICLE IGNORED)
  636 FORMAT(13X,51H36  OUTBOUND APPROACH DOES NOT ALLOW VEHICLE TYPE -,
     *       26H GENERATED VEHICLE IGNORED)
  637 FORMAT(13X,51H37  EMERGENCY VEHICLE IS NOT (YES) OR (NO) - SPECIA,
     *       15HL VEHICLE ERROR)
  638 FORMAT(/,1X,131(1H-),/,
     *       17X,51HNOTES 1, 9, 25, AND 33-36 INDICATE WARNING(S) ABOUT,
     *       15H VEHICLE STREAM,/,
     *       17X,51HNOTES 2 AND 26 GIVE INFORMATION ABOUT SPECIAL VEHIC,
     *        3HLES,/,
     *       15X,51H**NOTES 3-8, 10-24, 27-32, AND 37 INDICATE "FATAL" ,
     *       50HSPECIAL VEHICLE ERROR(S) - CORRECT INPUT AND RERUN,/,
     *        1X,131(1H-),//)
C
C-----SUBROUTINE PNOTES PRINTS THE EXPLANATION OF THE NOTES ASSOCIATED
C-----WITH THE WRITING AND CHECKING OF DRIVER-VEHICLE UNITS ONTO A TAPE
C-----FOR SIMPRO
C
C-----COUNT UP NUMBER OF NOTE LINES TO BE PRINTED
      LTEST = 0
      DO 1010  I = 1 , NND
                    IF ( NOTEU(I) . NE . 0 )     LTEST = LTEST + 1
 1010 CONTINUE
C-----ADD IN EXTRA LINE(S) FOR 2 LINE NOTE(S)
                    IF ( NOTEU(17) . NE . 0 )    LTEST = LTEST + 1
                    IF ( NOTEU(25) . NE . 0 )    LTEST = LTEST + 1
C-----IF NOT ANY NOTES TO BE PRINTED GO TO 2010 AND RETURN
                    IF ( LTEST . EQ . 0 )        GO TO 2010
      LTEST = LTEST + 10
C-----PRINT ANY PERTINENT NOTES
            IF ( NLINE+LTEST . GT . LINES )      CALL  HEADER
      WRITE (6,600)
                    IF ( NOTEU( 1) . NE . 0 )    WRITE (6,601) HMIN
                    IF ( NOTEU( 2) . NE . 0 )    WRITE (6,602)
                    IF ( NOTEU( 3) . NE . 0 )    WRITE (6,603)
                    IF ( NOTEU( 4) . NE . 0 )    WRITE (6,604)
                    IF ( NOTEU( 5) . NE . 0 )    WRITE (6,605)
                    IF ( NOTEU( 6) . NE . 0 )    WRITE (6,606)
                    IF ( NOTEU( 7) . NE . 0 )    WRITE (6,607)
                    IF ( NOTEU( 8) . NE . 0 )    WRITE (6,608)
                    IF ( NOTEU( 9) . NE . 0 )    WRITE (6,609)
                    IF ( NOTEU(10) . NE . 0 )    WRITE (6,610)
                    IF ( NOTEU(11) . NE . 0 )    WRITE (6,611)
                    IF ( NOTEU(12) . NE . 0 )    WRITE (6,612)
                    IF ( NOTEU(13) . NE . 0 )    WRITE (6,613)
                    IF ( NOTEU(14) . NE . 0 )    WRITE (6,614)
                    IF ( NOTEU(15) . NE . 0 )    WRITE (6,615)
                    IF ( NOTEU(16) . NE . 0 )    WRITE (6,616)
                    IF ( NOTEU(17) . NE . 0 )    WRITE (6,617)
                    IF ( NOTEU(18) . NE . 0 )    WRITE (6,618)
                    IF ( NOTEU(19) . NE . 0 )    WRITE (6,619)
                    IF ( NOTEU(20) . NE . 0 )    WRITE (6,620)
                    IF ( NOTEU(21) . NE . 0 )    WRITE (6,621)
                    IF ( NOTEU(22) . NE . 0 )    WRITE (6,622)
                    IF ( NOTEU(23) . NE . 0 )    WRITE (6,623)
                    IF ( NOTEU(24) . NE . 0 )    WRITE (6,624)
                    IF ( NOTEU(25) . NE . 0 )    WRITE (6,625) HMIN,HMIN
                    IF ( NOTEU(26) . NE . 0 )    WRITE (6,626)
                    IF ( NOTEU(27) . NE . 0 )    WRITE (6,627)
                    IF ( NOTEU(28) . NE . 0 )    WRITE (6,628)
                    IF ( NOTEU(29) . NE . 0 )    WRITE (6,629)
                    IF ( NOTEU(30) . NE . 0 )    WRITE (6,630)
                    IF ( NOTEU(31) . NE . 0 )    WRITE (6,631)
                    IF ( NOTEU(32) . NE . 0 )    WRITE (6,632)
                    IF ( NOTEU(33) . NE . 0 )    WRITE (6,633)
                    IF ( NOTEU(34) . NE . 0 )    WRITE (6,634)
                    IF ( NOTEU(35) . NE . 0 )    WRITE (6,635)
                    IF ( NOTEU(36) . NE . 0 )    WRITE (6,636)
                    IF ( NOTEU(37) . NE . 0 )    WRITE (6,637)
      WRITE (6,638)
      NLINE = NLINE + LTEST
 2010 CONTINUE
      IF ( NWARN . GT . 0 )                      THEN
        WRNMSG = '***** WARNING - POSSIBLE '   //
     *           'GENERATED VEHICLES IGNORED ' //
     *           'OR SPECIAL VEHICLE ERROR(S)' //
     *           ' - REVIEW WARNING MESSAGES,' //
     *           ' CORRECT INPUT AND RERUN AS' //
     *           ' NEEDED - PNOTES *****'
        CALL  PRTWRN ( WRNMSG,.FALSE. )
      END IF
      IF ( NSTOP . GT . 0 )                      THEN
        ERRMSG = '***** STOP 875 - *FATAL* '   //
     *           'SPECIAL VEHICLE ERROR(S) - ' //
     *           'CORRECT INPUT AND RERUN - '  //
     *           'PNOTES *****'
        CALL  ABORTR ( ERRMSG )
        STOP  875
      END IF
      RETURN
      END                                                               PNOTES
C
C
C
      SUBROUTINE PSUMDV
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHTEX'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'OUTPUT'
      INTEGER           IDENSE,LTEST,NGTOT,NGVOL,NSELIM,NSTOT,
     *                  NSVOL,NTOTAL,NTVOL,IVTP
  501 FORMAT(A)
  601 FORMAT( 8X, 5HTABLE,I3,27H  -  FINAL APPROACH VOLUMES,///,
     *       25X,17HSPECIAL  VEHICLES,7X,19HGENERATED  VEHICLES,8X,
     *           15HTOTAL  VEHICLES,/,23X,3(22(1H-),3X),/,
     *       12X,11HAPPROACH   ,3(25HNUMBER FOR  VOLUME FOR   ),/,
     *       12X,11H NUMBER    ,3(25HSIMULATION  SIMULATION   ),/)
  602 FORMAT(  14X,I3,   3X,3(2I11,3X))
  603 FORMAT(/,14X,6HTOTAL ,3(2I11,3X),/)
  604 FORMAT(7H ***** ,I5,23H SPECIAL VEHICLES WERE ,A)
  605 FORMAT(12X,37HTHE INTERSECTION HAS A JAM DENSITY OF,I4,
     *       18H VEHICLES PER MILE)
  606 FORMAT(7H ***** ,I5,35H GENERATED VEHICLES WERE ELIMINATED,/)
  906 FORMAT('STOP 906 - '
     *       'APPROACH NUMBER',I3,' HAS NO VEHICLES - PSUMDV')
C
C-----SUBROUTINE PSUMDV PRINTS THE SUMMARY STATISTICS OF THE VEHICLES
C-----ACTUALLY WRITTEN ONTO A TAPE FOR SIMPRO
C
      IF ( NLINE+NIBA+10 . GT . LINES )          THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 3
        END IF
      END IF
      WRITE (6,601) NTABL
      NTABL = NTABL + 1
      NLINE = NLINE + 8
      NGTOT = 0
      NSTOT = 0
      DO 1010  IAN = 1 , NIBA
C-----START INBOUND APPROACH LOOP
      IA = LIBA(IAN)
                    IF ( .NOT. IBAP(IA) )        GO TO 1010
C-----SUM NUMBER GENERATED FOR EACH APPROACH-VARYING TRAFFIC PERIOD
      DO 1000  IVTP = 1 , NVTP(IA)
      IVOLT (IAN) = IVOLT (IAN) + IVOL  (IVTP,IAN)
      NGWRTT(IAN) = NGWRTT(IAN) + NGWRIT(IVTP,IAN)
 1000 CONTINUE
      IF ( IVOLT (IAN) . LE . 0 )                THEN
        WRITE (6,602) LIBA(IAN),0,0,0,0,0,0
        GO TO 1010
      END IF
      NTOTAL = NSWRIT(IAN) + NGWRTT(IAN)
      NSVOL  = NINT( NSWRIT(IAN)*3600.0/SIMTIM )
      NGVOL  = NINT( NGWRTT(IAN)*3600.0/SIMTIM )
      NTVOL  = NINT( NTOTAL     *3600.0/SIMTIM )
C-----PRINT STATISTICS FOR INBOUND APPROACH IAN
      WRITE (6,602) LIBA(IAN),NSWRIT(IAN),NSVOL,NGWRTT(IAN),NGVOL,
     *              NTOTAL,NTVOL
C-----IF THERE WERE NOT ANY VEHICLES WRITTEN ONTO THE TAPE FOR SIMPRO
C-----FOR INBOUND APPROACH IAN THEN GO TO 9060 AND PRINT ERROR MESSAGE
                    IF( NTOTAL .LE. 0 )          GO TO 9060
      NGTOT = NGTOT + NGWRTT(IAN)
      NSTOT = NSTOT + NSWRIT(IAN)
 1010 CONTINUE
C-----CALCULATE TOTALS FOR THE INTERSECTION AND PRINT THE TOTALS
      NTOTAL = NGTOT + NSTOT
      NSVOL  = NINT( NSTOT *3600.0/SIMTIM )
      NGVOL  = NINT( NGTOT *3600.0/SIMTIM )
      NTVOL  = NINT( NTOTAL*3600.0/SIMTIM )
      WRITE (6,603) NSTOT,NSVOL,NGTOT,NGVOL,NTOTAL,NTVOL
      NLINE = NLINE + NIBA + 2
C-----IF NO SPECIAL VEHICLES THEN GO TO 1020 AND PRINT JAM DENSITY
      LTEST = 5
                    IF ( NSREAD . LE . 0 )       LTEST = 2
                    IF ( NLINE+LTEST.GT.LINES )  CALL  HEADER
                    IF ( NSREAD . LE . 0 )       GO TO 1020
      NSELIM = NSREAD - NSTOT
      WRITE (6,604) NSREAD,'READ IN'
      WRITE (6,604) NSELIM,'ELIMINATED'
      WRITE (6,602)
 1020 CONTINUE
      LTEST = 4
                    IF ( NGELIM . LE . 0 )       LTEST = 2
                    IF ( NLINE+LTEST.GT.LINES )  CALL  HEADER
                    IF ( NGELIM . LE . 0 )       GO TO 1030
      WRITE (6,606) NGELIM
      WRITE (6,602)
 1030 CONTINUE
C-----CALCULATE AND PRINT THE JAM DENSITY FOR THE INTERSECTION
      IDENSE = NINT( 5280.0*FLOAT( NTOTAL )/FLOAT( LVTOT ) )
      WRITE (6,605) IDENSE
      NLINE = NLINE + LTEST
      RETURN
C-----PROCESS EXECUTION ERROR AND STOP
 9060 CONTINUE
      WRITE (ERRMSG,906) LIBA(IAN)
      CALL  ABORTR ( ERRMSG )
      STOP  906
      END                                                               PSUMDV
C
C
C
      SUBROUTINE PSTATS
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHTEX'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'OUTPUT'
      INCLUDE 'STATS'
      INTEGER           CWARNC
      PARAMETER       ( CWARNC = 52 )
      DIMENSION         FPERUT(NOA)
      CHARACTER*5       FPERUT
      CHARACTER*13      CMORC
      CHARACTER*(CWARNC)CWARN
      INTEGER           ID,IV,LWARN,NTEST,NUMV,IVTP
      LOGICAL           FUTPRT
      REAL              SUM,SUMF
  501 FORMAT(F5.1)
  601 FORMAT(8X,5HTABLE,I3,29H  -  STATISTICS OF GENERATION,///,
     *       11X,19HAPPROACH STATISTICS,/,11X,19(1H-),/)
  602 FORMAT(11X,39HAPPROACH NUMBER -----------------------,I5)
  603 FORMAT(15X,35HOUTBOUND APPROACH NUMBER ----------,8I5)
  604 FORMAT(11X,39HPERCENT GOING TO OUTBOUND APPROACHES --,1X,8F5.1)
  605 FORMAT(11X,39HPERCENT MAKING FREE U-TURN ------------,1X,8A5)
  606 FORMAT(15X,35HVEHICLE CLASS NUMBER --------------,16I5,
     *       6(:,/,50X,16I5))
  607 FORMAT(11X,39HGENERATION PERCENT OF VEHICLES --------,1X,16F5.1,
     *       6(:,/,2X,50X,16F5.1))
  608 FORMAT(11X,35HPERCENT OF TRAFFIC ENTERING ON LANE,I2,2H -,F6.1,
     *       2X,A,1X,A)
  609 FORMAT(11X,29HDRIVER CLASS SPLIT STATISTICS,/,11X,29(1H-),//,
     *       11X,40HDRIVER CLASS NUMBER --------------------, 9I6)
  610 FORMAT(11X,20HVEHICLE CLASS NUMBER,I3,2H (,I5,11H VEH) -----,
     *       9F6.1)
  611 FORMAT()
C
C-----SUBROUTINE PSTATS CALCULATES AND PRINTS BY EACH INBOUND APPROACH
C-----THE GENERATED PERCENTAGES FOR THE LOGIN ATTRIBUTES PREVIOUSLY
C-----SPECIFIED BY THE USER (OR DEFAULT VALUES)
C
      CWARN = ' WARNING - THIS LANE WILL HAVE NO GENERATED VEHICLES'
C-----CHECK TO SEE IF THERE ARE ANY GENERATED VEHICLES TO COMPUTE
C-----STATISTICS OF GENERATION
      NUMV = 0
      DO 1004  IAN = 1 , NIBA
      NUMV = NUMV + NGWRTT(IAN)
      IA = LIBA(IAN)
      DO 1000  JAN  = 1 , NOBA
      DO 1000  IVTP = 2 , NVTP(IA)
      SPERT(JAN,1,IAN) = SPERT(JAN,1,IAN) + SPERT(JAN,IVTP,IAN)
 1000 CONTINUE
      DO 1001  IV   = 1 , NVEHCL
      DO 1001  IVTP = 2 , NVTP(IA)
      SPERV(IV ,1,IAN) = SPERV(IV,1,IAN) + SPERV(IV,IVTP,IAN)
 1001 CONTINUE
      DO 1002  ILN  = 1 , NLANES(IA)
      DO 1002  IVTP = 2 , NVTP(IA)
      SPERL(ILN,1,IAN) = SPERL(ILN,1,IAN) + SPERL(ILN,IVTP,IAN)
 1002 CONTINUE
 1004 CONTINUE
                    IF ( NUMV . LE . 0 )         RETURN
      NTEST = NLINE + NLANES(LIBA(1)) + 13
      DO 1005  JAN = 1 , NOBA
                    IF ( FREEUT(JAN,1) )         NTEST = NTEST + 1
 1005 CONTINUE
      IF ( NTEST .GT. LINES )                    THEN
        CALL  HEADER
      ELSE
        IF ( NLINE . GT . NLINEH )               THEN
          WRITE (6,501)
          WRITE (6,501)
          WRITE (6,501)
          NLINE = NLINE + 3
        END IF
      END IF
      WRITE (6,601) NTABL
      NTABL = NTABL + 1
      NLINE = NLINE + 6
C-----PRINT APPROACH STATISTICS BY EACH INBOUND APPROACH
      DO 4010  IAN = 1 , NIBA
      IVTP = 1
C-----IF NO GENERATED VEHICLES FOR THIS APPROACH GO TO 4010 AND PROCESS
C-----OTHER APPROACHES
                    IF ( NGWRTT(IAN) . LE . 0 )  GO TO 4010
      IA = LIBA(IAN)
      NTEST = NLINE + NLANES(IA) + 2*INT( (NVEHCL+15)/16 ) + 5
      DO 1007  JAN = 1 , NOBA
                    IF ( FREEUT(JAN,IAN) )       NTEST = NTEST + 1
 1007 CONTINUE
                    IF ( NTEST .GT. LINES )      CALL  HEADER
      WRITE (6,602) IA
C-----SUM NUMBER OF VEHICLES GOING TO EACH OUTBOUND APPROACH
      SUM = 0.0
      DO 1010  JAN = 1 , NOBA
      SUM = SUM + SPERT(JAN,IVTP,IAN)
 1010 CONTINUE
C-----CALCULATE THE PERCENTAGE GOING TO EACH OUTBOUND APPROACH
      FUTPRT = .FALSE.
      DO 1020  JAN = 1 , NOBA
      IF ( SUM . EQ . 0.0 )                      THEN
        SPERT(JAN,IVTP,IAN) = 0.0
      ELSE
        SPERT(JAN,IVTP,IAN) = 100.0*SPERT(JAN,IVTP,IAN)/SUM
      END IF
      IF ( FREEUT(JAN,IAN) )                     THEN
        FUTPRT = .TRUE.
        SUMF = SPERUT(1,IVTP,IAN) + SPERUT(2,IVTP,IAN)
        IF ( SUMF . EQ . 0.0 )                   THEN
          WRITE (FPERUT(JAN),501) 0.0
        ELSE
          WRITE (FPERUT(JAN),501) 100.0*SPERUT(1,IVTP,IAN)/SUMF
        END IF
      ELSE
        FPERUT(JAN) = ICBL
      END IF
 1020 CONTINUE
C-----PRINT THE PERCENTAGES GOING TO EACH OUTBOUND APPROACH
      WRITE (6,603) (LOBA(JAN),JAN=1,NOBA)
      WRITE (6,604) (SPERT(JAN,IVTP,IAN),JAN=1,NOBA)
      IF ( FUTPRT )                              THEN
      WRITE (6,605) (FPERUT(JAN),JAN=1,NOBA)
      END IF
C-----SUM THE NUMBER OF VEHICLES OF EACH VEHICLE CLASS GENERATED
      SUM = 0.0
      DO 2010  IV = 1 , NVEHCL
      SUM = SUM + SPERV(IV,IVTP,IAN)
 2010 CONTINUE
C-----CALCULATE THE PERCENTAGE
      DO 2020  IV = 1 , NVEHCL
      SPERV(IV,IVTP,IAN) = 100.0*SPERV(IV,IVTP,IAN)/SUM
 2020 CONTINUE
C-----PRINT THE PERCENTAGE OF EACH VEHICLE CLASS GENERATED
      WRITE (6,606) (IV,IV=1,NVEHCL)
      WRITE (6,607) (SPERV(IV,IVTP,IAN),IV=1,NVEHCL)
C-----SUM THE NUMBER OF VEHICLES ENTERING ON EACH LANE
      SUM = 0.0
      DO 3010  ILN = 1 , NLANES(IA)
      SUM = SUM + SPERL(ILN,IVTP,IAN)
 3010 CONTINUE
C-----CALCULATE AND PRINT THE PERCENTAGE OF VEHICLES ENTERING EACH LANE
      DO 3020  ILN = 1 , NLANES(IA)
      JL = LLANES(ILN,IA)
      SPERL(ILN,IVTP,IAN) = 100.0*SPERL(ILN,IVTP,IAN)/SUM
      CMORC = ICBL
      IF ( NLANES(IA) . GT . 1 )                 THEN
        IF      ( ILN . EQ . 1          )        THEN
          CMORC='(MEDIAN LANE)'
        ELSE IF ( ILN . EQ . NLANES(IA) )        THEN
          CMORC='(CURB LANE)'
        END IF
      END IF
      LWARN = 1
      IF ( LGEOM(1,JL).LT.LGEOM(2,JL) . AND .
     *     SPERL(ILN,IVTP,IAN).LE.0.0 )          LWARN = CWARNC
      WRITE (6,608) ILN,SPERL(ILN,IVTP,IAN),CMORC,CWARN(1:LWARN)
      NLINE = NLINE + 1
      IF ( LWARN . EQ . CWARNC )                 THEN
        WRITE (WRNMSG,608) ILN,SPERL(ILN,IVTP,IAN),CMORC,CWARN(1:LWARN)
        CALL  PRTWRN  ( WRNMSG,.TRUE. )
      END IF
 3020 CONTINUE
      WRITE (6,611)
      WRITE (6,611)
      NLINE = NLINE + NLANES(IA) + 2*INT( (NVEHCL+15)/16 ) + 5
      IF ( FUTPRT )                              NLINE = NLINE + 1
C-----END OF INBOUND APPROACH LOOP
 4010 CONTINUE
C-----PRINT DRIVER CLASS SPLIT STATISTICS (XPERD)
            IF ( NLINE+12+5 . GT . LINES )       CALL  HEADER
      WRITE (6,609) (ID,ID=1,NDRICL)
      WRITE (6,611)
      NLINE = NLINE + 5
C-----SUM THE NUMBER OF VEHICLES GENERATED UNDER EACH VEHICLE AND DRIVER
C-----CLASS
      DO 6010  IV = 1 , NVEHCL
      SUM = 0.0
      DO 5010  ID = 1 , NDRICL
      SUM = SUM + SPERD(ID,IV)
 5010 CONTINUE
                    IF ( SUM . LE . 0.0 )        GO TO 5030
C-----CALCULATE THE PERCENTAGE OF DRIVER TYPES IN EACH VEHICLE CLASS
      DO 5020  ID = 1 , NDRICL
      SPERD(ID,IV) = 100.0*SPERD(ID,IV)/SUM
 5020 CONTINUE
 5030 CONTINUE
C-----PRINT PERCENTAGE OF DRIVER TYPES GENERATED FOR EACH VEHICLE CLASS
                    IF ( NLINE+1 . GT . LINES )  CALL  HEADER
      WRITE (6,610) IV,NINT( SUM ),(SPERD(ID,IV),ID=1,NDRICL)
      NLINE = NLINE + 1
 6010 CONTINUE
                    IF ( NLINE+3 . GT . LINES )  RETURN
      WRITE (6,611)
      WRITE (6,611)
      WRITE (6,611)
      NLINE = NLINE + 3
      RETURN
      END                                                               PSTATS
C
C
C
      FUNCTION   SRAN   ( )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CURAND'
      INCLUDE 'INDEX'
      DOUBLE PRECISION  SRAN
C
C-----FROM KNUTH VOL2 P172 - SUBTRACTIVE RANDOM NUMBER GENERATOR
C
      IF ( NEWSED )                              THEN
C
C-----NEW SEED, INITIALIZE AND LOAD
C
        NEWSED = .FALSE.
        CALL IN55
        NEXTRN(IAN) = 1
        GO TO 1010
      END IF
      NEXTRN(IAN) = NEXTRN(IAN) + 1
      IF ( NEXTRN(IAN) . GT . 55 )                 THEN
C
C-----RELOAD
C
        CALL IRN55
        NEXTRN(IAN) = 1
      END IF
 1010 CONTINUE
      SRAN = DBLE( IASRAN(NEXTRN(IAN),IAN) )*1.0D-9
      RETURN
      END                                                               SRAN
C
C
C
      SUBROUTINE IN55
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CURAND'
      INCLUDE 'INDEX'
      INTEGER           I,II,J,K
C
C-----THIS SUBROUTINE INITIALIZES THE ARRAY IASRAN(55,IAN) WHERE IASRAN
C-----IS AN ARRAY OF RANDOM INTEGERS BETWEEN 0 AND 1,000,000,000.
C-----THIS FUNCTION MUST BE CALLED BEFORE CALLING THE RANDOM NUMBER
C-----GENERATOR IRN55, OR TO INTRODUCE A NEW "SEED'.
C
      IASRAN(55,IAN) = IS(IAN)
      J = IS(IAN)
      K = 1
      DO 1 I = 1 , 54
      II = MOD(21*I,55)
      IASRAN(II,IAN) = K
      K = J - K
      IF ( K . LT . 0 )                          K=K+1000000000
      J = IASRAN(II,IAN)
    1 CONTINUE
      CALL  IRN55
      CALL  IRN55
      CALL  IRN55
      RETURN
      END                                                               IN55
C
C
C
      SUBROUTINE IRN55
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CURAND'
      INCLUDE 'INDEX'
      INTEGER           I,J
C
C-----SUBTRACTIVE METHOD RANDOM NUMBER GEN. AFTER KNUTH VOL2 P172
C-----REGENERATES AN ARRAY IASRAN(55,IAN) OF RANDOM INTEGERS BETWEEN
C-----0 AND 1,000,000,000
C
      DO 1 I = 1 , 24
      J = IASRAN(I,IAN) - IASRAN(I+31,IAN)
      IF ( J . LT . 0 )                          J=J+1000000000
      IASRAN(I,IAN) = J
    1 CONTINUE
      DO 2 I = 25 , 55
      J = IASRAN(I,IAN) - IASRAN(I-24,IAN)
      IF ( J . LT . 0 )                          J=J+1000000000
      IASRAN(I,IAN) = J
    2 CONTINUE
      RETURN
      END                                                               IRN55
C
C
C
      SUBROUTINE ABORTR ( MESAGE )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHTEX'
      INCLUDE 'CLASS'
      INCLUDE 'CURAND'
      INCLUDE 'CWDDIR'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LITCON'
      INCLUDE 'OUTPUT'
      INCLUDE 'PARVAL'
      INCLUDE 'STATS'
      INCLUDE 'TITLE'
      INCLUDE 'USFILE'
      CHARACTER*(*) MESAGE
      INTEGER       ILNB  ,NCMES
  600 FORMAT(A)
  601 FORMAT(6X,47HDRIVER-VEHICLE PROCESSOR FOR THE TEXAS TRAFFIC ,
     *       20HSIMULATION PACKAGE (,A5,1H),/,
     *       9X,28HDVPRO COPYRIGHT (C) 1989 BY ,
     *       33HTHE UNIVERSITY OF TEXAS AT AUSTIN,//,1X,A,//,
     *       48H DRIVER-VEHICLE PROCESSOR FOR THE TEXAS TRAFFIC ,
     *       34HSIMULATION PACKAGE *** ABORTED ***,///)
  602 FORMAT(1X,A,///)
  603 FORMAT(47HDriver-Vehicle Processor for the TEXAS Traffic ,
     *       20HSimulation Package (,A5,1H),/,
     *       28HDVPRO Copyright (C) 1989 by ,
     *       33HThe University of Texas at Austin,//,   A,//,
     *       47HDriver-Vehicle Processor for the TEXAS Traffic ,
     *       34HSimulation Package *** ABORTED ***,///)
C-----
C-----WRITE OUT MESSAGE TO OUTPUT(6), TTY(*), AND error,txt(90)
C-----
      NCMES = MAX0( ILNB( MESAGE ),1 )
      WRITE (  6,600) CHAR( 12 )
      WRITE (  6,601) IVERSN,ITITLE
C     WRITE (  *,603) IVERSN,ITITLE
      CALL  PRTERR  ( MESAGE )
      RETURN
      END                                                               ABORTR
C
C
C
      SUBROUTINE PRTERR ( MESAGE )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHTEX'
      INCLUDE 'CLASS'
      INCLUDE 'CURAND'
      INCLUDE 'CWDDIR'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LITCON'
      INCLUDE 'OUTPUT'
      INCLUDE 'PARVAL'
      INCLUDE 'STATS'
      INCLUDE 'TITLE'
      INCLUDE 'USFILE'
      CHARACTER*(*) MESAGE
      INTEGER       ILNB  ,NCMES
      INTEGER       NER
      DATA          NER   / 90 /
  601 FORMAT(A)
C-----
C-----WRITE OUT MESSAGE TO  error,txt(90), OUTPUT(6) AND TTY(*)
C-----
      NCMES = MAX0( ILNB( MESAGE ),1 )
      OPEN   (NER,FILE='error.txt',ACCESS='APPEND',STATUS='UNKNOWN')
      WRITE  (NER,601) MESAGE(1:NCMES)
      ENDFILE NER
      CLOSE  (NER,STATUS='KEEP')
C
      WRITE  (6,601)
      WRITE  (6,601) MESAGE(1:NCMES)
      WRITE  (6,601)
      WRITE  (6,601)
C
      WRITE  (*,601)
      WRITE  (*,601) MESAGE(1:NCMES)
      WRITE  (*,601)
      WRITE  (*,601)
      RETURN
      END                                                               PRTERR
C
C
C
      SUBROUTINE PRTWRN ( MESAGE,SKIPW6 )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'APPRO'
      INCLUDE 'CHTEX'
      INCLUDE 'CLASS'
      INCLUDE 'CURAND'
      INCLUDE 'CWDDIR'
      INCLUDE 'DVDATA'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LITCON'
      INCLUDE 'OUTPUT'
      INCLUDE 'PARVAL'
      INCLUDE 'STATS'
      INCLUDE 'TITLE'
      INCLUDE 'USFILE'
      CHARACTER*(*) MESAGE
      INTEGER       ILNB  ,NCMES
      INTEGER       NWR
      LOGICAL       SKIPW6
      DATA          NWR   / 91 /
  601 FORMAT(A)
C-----
C-----WRITE OUT MESSAGE TO  warning,txt(91), OUTPUT(6) AND TTY(*)
C-----
      NCMES = MAX0( ILNB( MESAGE ),1 )
      OPEN   (NWR,FILE='warning.txt',ACCESS='APPEND',STATUS='UNKNOWN')
      WRITE  (NWR,601) MESAGE(1:NCMES)
      ENDFILE NWR
      CLOSE  (NWR,STATUS='KEEP')
C
      IF ( .NOT. SKIPW6 )                        THEN
        WRITE  (6,601)
        WRITE  (6,601) MESAGE(1:NCMES)
        WRITE  (6,601)
        WRITE  (6,601)
        NLINE = NLINE + 4
      END IF
C
      WRITE  (*,601)
      WRITE  (*,601) MESAGE(1:NCMES)
      WRITE  (*,601)
      WRITE  (*,601)
      RETURN
      END                                                               PRTWRN