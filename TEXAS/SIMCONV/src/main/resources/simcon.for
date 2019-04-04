      SUBROUTINE SIMCON(JOF,TAG,GDFILE)
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
C ----- THIS SUBROUTINE CONVERTS SIMULATION DATA FROM CONFIGURATION 1 (C1)
C ----- TO CONFIGURATION 2 (C2).  A C1 FILE IS COMPATABLE WITH THE
C ----- SIMULATION PROCESSOR (SIMPRO) AS DEVELOPED IN PROJ 184.
C ----- A C2 FILE IS CREATED BY THE PREPROCESOR DEVELOPED IN PROJ 361.
C ----- AFTER EXECUTION OF THIS SUBROUTINE, A C1 FILE IS ACCESSABLE
C ----- ON UNIT ABS<JOF>.  IF <JOF> IS NEGATIVE, A COMPLETION
C ----- MESSAGE IS DISPLAYED ON THE TERMINAL.
C
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INTEGER           JOF
      CHARACTER*3           TAG
      CHARACTER*(*)             GDFILE
      CHARACTER*7 UNKN
      PARAMETER  (UNKN='UNKNOWN')
      INCLUDE 'CMPR01'
      INCLUDE 'CMTX01'
      INCLUDE 'CMTXSR'
      INCLUDE 'CMCH01'
      INCLUDE 'CWDDIR'
      INCLUDE 'IVERSN'
      INCLUDE 'USFILE'
      COMMON / LOOPSC / DETCLN(LDC,NLS)
      CHARACTER*8       DETCLN
      COMMON / LOOPSI / DETCLK(NLS),DETCLL(LDC,NLS),DETCLU(LDC,NLS)
      INTEGER           DETCLK     ,DETCLL         ,DETCLU
      INTEGER       NCPV  (PARNSV)
      CHARACTER*10  PAR   (PARNSV)
      CHARACTER*130 PARVAL(PARNSV),
     1              PFILE  ,           CFILEP ,
     2              SYSDAT ,           WRKDIR ,
     3              PAUSND
      EQUIVALENCE  (PARVAL(1),PFILE ),(PARVAL(2),CFILEP),
     1             (PARVAL(3),SYSDAT),(PARVAL(4),WRKDIR),
     2             (PARVAL(5),PAUSND)
      CHARACTER SSC*2,SSCA*2,CCTEMP*3,SSCU*2,
     1          STAT*6,IC*8,ICU*8,
     2          FMT4*4,FMT2*4,FMT3*4,
     3          TLINE*50,NLINES(NLS)*2,DEFFN*60,
     4          PHOLTX*3,DETLEG*2,UCONT*2,UNS*3,TEMPC1*1
      CHARACTER ILINEU*80,NLINEU*130,NLINESU*2,DETLEGU*2
      CHARACTER*60 FNAME,DNAME
      CHARACTER*130 ERRMSG
      LOGICAL BB,AB,BC,AC,BL,AL,BS,AS,BR,AR,BLUNLT,EXCLT
      LOGICAL ERR,EXI
      LOGICAL   ANYSET,JCUNCT,JCYELD,JCLTAS,JCAWST,JCPSIG,JCSACT,JCFACT,
     *          JCTDIA,JCNEMA,JCHDWR
      INTEGER   NPHPS         ,LPHPS           ,NOLDEF   ,
     1          LOLDEF
      DIMENSION NPHPS(NRG,NGR),LPHPS(NRG,NGR,NPN),NOLDEF(NON),
     1          LOLDEF(NPN,NON)
      INTEGER   IUBLI             ,IUBLO             ,IOFF             ,
     1          NLNS         ,NINLNS         ,LANGLE         ,
     2          LLENG        ,NTDDET
      DIMENSION IUBLI(NAL,0:NLGP1),IUBLO(NAL,0:NLGP1),IOFF(NAL,0:NLGP1),
     1          NLNS(0:NLGP1),NINLNS(0:NLGP1),LANGLE(0:NLGP1),
     2          LLENG(0:NLGP1),NTDDET(10),PHOLTX(NPHOL)
      INTEGER   LISTCT         ,NCCT       ,
     1
     2                    IT3S    ,IT4S    ,IT5S    ,IDFFS    ,
     3          ILENS   ,LIAD          ,LOAD         ,LPD        ,
     4          LPSD         ,ILPSD    ,IUWDTH
      CHARACTER*130
     1          LLINE   ,ILINE
      REAL
     1                            PTG   ,PTY   ,ALLRED
      LOGICAL   ARREST
C-----NEMA WILL ONLY HAVE 1 PHASE IN CLEAR TO LIST
      DIMENSION LISTCT(NPH-1,NPN),NCCT(NPN),
     1          LLINE(NPN),ILINE(NPHOL),PTG(NPN),PTY(NPN),ALLRED(NPN),
     2          ARREST(NPN),IT3S(NLS),IT4S(NLS),IT5S(NLS),IDFFS(NLS),
     3          ILENS(NLS),LIAD(0:NLGP1),LOAD(0:NLGP1),LPD(0:NLGP1),
     4          LPSD(0:NLGP1,2),IUWDTH(2)
      INTEGER   I,IDFF,IG,II,III,ILEN,ILNB,IO,IO1,IOF,IOFFST,IP,IPH,
     1          IPLUS,IR,IREC,IT1,IT2,IT3,IT4,IT5,IT6,ITAM1,J,JJ,JREC,K,
     2          K1,K1L,K1R,KK,L3LN1,L6LN1,LREC,LSTREC,N,NALLR,NC,NC1,
     3          NCA,NCARDS,NCB,NCCF,NCCTT,NCFN,NCI,NCSTAT,NCTEX,NDET1,
     4          NINACT,NINLNT,NFL,NFW,NGROUP,NLEGS,NLSTL,NOLPA,NPHASS,
     5          NPHAST,NR,NRING,NRR,IT1L,IT1R,IT1T
      REAL      XFACT,SVER
      INTEGER              NIO             ,NSFIO ,
     1                     ICYC            ,NPHA            ,NDET ,
     2                     IFIGUR          ,NOLP
      REAL       DETEXT 
      DIMENSION  DETEXT(NLS)
      INTEGER    DETDLY
      DIMENSION  DETDLY(NLS)
      CHARACTER*3 PEDDAT,TDETC(NPL+3)
      INTEGER                 NDETCN,       NDETCC,       NDETCE
      INTEGER    LDETC,       LDETCN,       LDETCC,       LDETCE
      DIMENSION  LDETC(NPL+3),LDETCN(NPL+3),LDETCC(NPL+3),LDETCE(NPL+3)
      EQUIVALENCE (MOUT(1),NIO   ),(MOUT(2),NSFIO),
     1            (MOUT(8),ICYC  ),(MOUT(9),NPHA ),(MOUT(13),NDET),
     2            (MOUT(8),IFIGUR),(MOUT(8),NOLP )
C-----VMS MESSAGES VARIABLES
      INTEGER          IVMSMT,IVMSMG
      DOUBLE PRECISION DVMSMP,DVMSST,DVMSAT
      CHARACTER*1      IVMSLC
      INTEGER          IVMSLP,IVMSLB,IVMSLE
      DOUBLE PRECISION DVMSPB,DVMSPE
      INTEGER          IVMSVN
      CHARACTER*7      CVMSDN
      DOUBLE PRECISION DVMSDM,DVMSDP
      INTEGER          IVMSAP
  501 FORMAT(2I4,3F7.2,A1,I3,2I2,2F7.2,I6,A7,2F6.2)
  502 FORMAT(2I4,3F7.2, I4  ,2I2,2F7.2,I6,A7,2F6.2)
  901 FORMAT('SIMCON ERROR - VMS MESSAGE LANE OR PATH NUMBER =',I3,
     *       ' IS LT 1 OR GT ',I2)
  902 FORMAT('SIMCON ERROR - VMS MESSAGE LANE OR PATH = (',A,
     *       ') IS NOT (I), (L), (O), (P), OR (R)')
  903 FORMAT('SIMCON ERROR - INTERSECTION CONTROL = (',A,
     *       ') IS NOT VALID')
  904 FORMAT('SIMCON ERROR - NUMBER OF PHASES TO CLEAR TO FOR PHASE ',
     *        I2,' = ',I2,' IS LT 1 OR GT ',I2)
      DATA ALLRED / NPN*0.0     /
      DATA ARREST / NPN*.FALSE. /
      DATA DETDLY / NLS * 0     /
      DATA DETEXT / NLS * 0.0   /
      DATA ERR    / .FALSE.     /
      DATA NINLNS / NLGP2*0     /
      DATA NLNS   / NLGP2*0     /
      DATA NOLDEF / NON*0       /
      DATA UNS    / 'UNS'       /
C
C ----- COMMAND LINE PARAMETERS
C
      DATA NCPV   / -1   ,-1 ,-1       ,-1       ,-1        /
      DATA PAR    / 'PRE','C','SYS_DAT','WRK_DIR','PAUSEND' /
      DATA PARVAL / ' '  ,' ',' '      ,' '      ,'YES'     /
      DATA FMT4,FMT2,FMT3 / '(I4)','(I2)','(I3)' /
C
C ----- APPROACH NUMBERS FOR DIAMOND (MUST BE SAME IN SIMCON)
C
      DATA (LIAD(I),I=0,NLGP1) /  4, 1, 2, 3, 6, 7, 8, 5 /
      DATA (LOAD(I),I=0,NLGP1) / 12, 9,10,11,14,15,16,13 /
C
C ----- PREVIOUS LEGS FOR DIAMOND (MUST BE SAME IN SIMCON)
C
      DATA (LPD(I),I=0,NLGP1) / 3,0,1,2,7,4,5,6 /
C
C ----- LEG PROCESSING SEQUENCE FOR DIAMOND ( MUST BE SAME IN SIMCON)
C

C ----- 1 WAY FRONTAGE ROADS
      DATA (LPSD(I,1),I=0,NLGP1) / 1,2,3,4,5,6,0,7 /
C ----- 2 WAY FRONTAGE ROADS
      DATA (LPSD(I,2),I=0,NLGP1) / 0,1,2,3,4,5,6,7 /
C
C ----- DETECTOR NUMBERS FOR TEXAS DIAMOND CONTROLLER DETECTORS
C
      DATA (NTDDET(I),I=1,10) / 1,2,12,3,13,5,15,6,7,17 /
  884 FORMAT(' SIMCOMV ERROR IN GDVS0 CALL')
  885 FORMAT(' SIMCONV ERROR - DETECTOR ',I2,
     *       ' NUMBER OF CLASSIFICATIONS = ',I4,' IS LT 1 OR GT ',I4)
  720 FORMAT(' SIMCONV ERROR - DETECTOR CONNECTION TYPE = "',A,'"',
     *       ' IS NOT "C&E", "CAL", OR "EXT"')
      IF(JOF.LT.0)WRITE(*,'(/2A/)')'SIMCONV ',IVERSN
      ITAM1=ICHAR( 'A' )-1
C
C ----- PFILE  - PREPROCESSOR (CONFIGURATION 2) FILE (UNIT <NDFIO>)
C ----- CFILEP - CONVERTED (CONFIGURATION 1) FILE (UNIT <NIN>)
C
C
C ----- GET FILE NAMES FROM COMMAND LINE
C
      CALL CLFILE(PARNSV,PAR,PARVAL,NCPV,'simconv')
      CWDDIR=WRKDIR
      CALL TOUPR( PAUSND )
C
C ----- READ INITIALIZATION DATA
C
      N=2
      NIO=0
      FNAME=' '
      CALL GDVS0(N,1,MOUT,NIO,N,N,N,N,SYSDAT,
     1           ' ',TDFILE,FNAME,USFILE,'NO','NO','NO','NO')
      IF(N.EQ.-1)THEN
        TAG='NUL'
        WRITE(ERRMSG,884)
        CALL  PRTERR  ( ERRMSG )
        GO TO 9990
      END IF
C     NLINE=CFILEP
C     CALL TXGFND(NLINE,'sim',CFILEP)
      IF(ILNB( CFILEP ).EQ.0)CFILEP='sim'
C%    IF(ILNB( CWDDIR ).GT.0)USFILE=CWDDIR
C%    IF(ILNB( CFILEP ).GT.0)CALL PCFS(CFILEP,USFILE,CFILEP)
C%    IF(ILNB( FNAME  ).GT.0)CALL PCFS(FNAME ,USFILE,FNAME )
C%    IF(ILNB( PFILE  ).GT.0)CALL PCFS(PFILE ,USFILE,PFILE )
      NSFIO=NSFIO+5
      NOUT(1)=NIO
      NOUT(2)=MOUT(2)
      TEXFN=DEFFN(NSFIO)
      NCTEX=ILNB( TEXFN )
      NCI=ILNB( PFILE )
      EXI=.FALSE.
      IF(NCI.GT.0)THEN
C
C ----- READ FROM PREPROCESSOR FILE SPECIFIED ON COMMAND LINE
C
C
C ----- IS FILE IN USER FILE STORAGE AREA
C
        CALL TXGFND(USFILE,FNAME,DNAME)
        CALL TXINQ(0,PFILE,DNAME,NLINE,EXI)
        IF(EXI)THEN
          PFILE=NLINE
          GO TO 80
        END IF
        NLINE=' '
C
C ----- IS FILE IN DEFAULT AREA
C
        CALL TXGFND(' ',FNAME,DNAME)
        CALL TXINQ(0,PFILE,DNAME,NLINE,EXI)
        IF(EXI)THEN
          PFILE=NLINE
          GO TO 80
        END IF
        NLINE=' '
        WRITE(*,'(A)')
        GO TO 180
      END IF
      IF(NCTEX.EQ.0)THEN
        CALL OPENRO(NSFIO,' ',UNKN,DIR,FMTD,80,ERR)
        IF(ERR)GO TO 100
        GO TO 200
      END IF
      PFILE=TEXFN
      INQUIRE(FILE=TEXFN,EXIST=EXI,NAME=PFILE)
      IF(.NOT.EXI)GO TO 100
   80 CONTINUE
C
C ----- FILE WITH INFO POINTING TO SIMULATION INPUT DATA
C
      CALL OPENRO(NSFIO,PFILE,'OLD',DIR,FMTD,80,ERR)
      IF(.NOT.ERR)GO TO 200
  100 CONTINUE
C
C ----- TAG=NUL TELLS CALLER THAT A DATA FILE IS NOT AVAILABLE
C
      TAG='NUL'
      IF(NCI.LE.0)THEN
C
C ----- FILE WITH POINTER TO DATA COULD NOT BE ACCESSED
C
        IF(JOF.LT.0)
     1    WRITE(*,'(/A)')'A Simulation Preprocessor file '//
     2                   'was not specified.'
        IF(NCTEX.EQ.0)THEN
          CALL OPENFI(NSFIO,' ',ERR)
        ELSE
          CALL OPENFI(NSFIO,PFILE,ERR)
        END IF
        CLOSE(NSFIO,STATUS='DELETE')
        GO TO 9999
      END IF
      EXI=.TRUE.
  180 CONTINUE
C
C ----- FILE FROM COMMAND LINE COULD NOT BE ACCESSED
C
      TAG='NUL'
      IF(JOF.LT.0)THEN
        NC=ILNB( PFILE )
        IF(EXI)THEN
          WRITE(*,'(3A)')'File "',PFILE(1:NC),'" is not a '//
     1                   'usable Simulation Preprocessor file.'
        ELSE
          CALL SHONAM('Did not find Simulation Preprocessor file',PFILE,
     1                78)
        END IF
      END IF
      GO TO 9999
  200 CONTINUE
      TAG=' '
      IOF=IABS(JOF)
C
C------ READ SIM HEADER LINE
C
      READ(NSFIO,IOFORM,REC=1,ERR=100)NLINE
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF((NLINEU(1:5).NE.' #%&$') .OR. (NLINEU(10:12).NE.'SIM'))THEN
        WRITE(*,'(/A)')'Header in Simulation Preprocessor file '//
     1                 'is invalid'
        GO TO 180
      END IF
      IF(NLINEU(6:9).EQ.'FILE')THEN
C
C ----- FOUND POINTER TO DATA FILE
C
        FNAME=NLINE(20:)
        NCFN=ILNB( FNAME )
        IF(NLINEU(13:14).EQ.'C1')THEN
C
C ----- FILE POINTED TO IS A C1 FILE
C
          GDFILE=' '
          CALL TXINQ(0,FNAME(1:NCFN),' ',CFILEP,EXI)
          CALL OPENRO(IOF,CFILEP,'OLD','SEQUENTIAL',FMTD,0,ERR)
          IF(ERR)THEN
            WRITE(*,'(A)')
            CALL SHONAM('Converted Simulation data file is',CFILEP,78)
            CALL OPNERR(CFILEP)
            GO TO 9510
          END IF
          GO TO 9000
        END IF
        STAT='KEEP'
        NCSTAT=4
        CALL OPENRO(NSFIO,FNAME(1:NCFN),UNKN,DIR,FMTD,80,ERR)
        INQUIRE(NSFIO,NAME=GDFILE)
        GO TO 650
      END IF
      IF(NLINEU(6:9).NE.'DATA')THEN
        WRITE(*,'(/A)')'Header in Simulation Preprocessor file '//
     1                 'is invalid'
        GO TO 180
      END IF
C
C ----- FOUND A C2 DATA FILE
C
      IF(NCI.GT.0)THEN
        STAT='KEEP'
        NCSTAT=4
        GDFILE=PFILE
      ELSE
        STAT='DELETE'
        NCSTAT=6
C
C------ READ SIM HEADER LINE
C
        READ(NSFIO,IOFORM,REC=1)NLINE
        GO TO 660
      END IF
  650 CONTINUE
C
C------ READ SIM HEADER LINE
C
      READ(NSFIO,IOFORM,REC=1)NLINE
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(NLINEU(1:14).NE.' #%&$DATASIMC2')THEN
        PFILE=GDFILE
        WRITE(*,'(/A)')'Header in Simulation Preprocessor file '//
     1                 'is invalid.'
        GO TO 180
      END IF
      NC=ILNB( GDFILE )
      WRITE(*,'(3A/A/)')'Simulation Preprocessor file "',
     1                  GDFILE(1:NC),'"','is being converted for '//
     2                  'input to the Simulation Processor.'
  660 CONTINUE
C
C ----- DATA FILE IS IN SECOND CONFIGURATION
C
C ----- MOUT( 6) - LINE WITH TITLE
C ----- MOUT( 7) - FIRST LINE WITH PHASE TIMING DATA
C ----- MOUT( 8) - PRETIMED CYCLE LENGTH
C                    0 - DATA IS BEING INPUT AS TIME INTERVALS
C                    GREATER THAN 0 - DATA IS BEING INPUT IN PERCENT OF
C                                     CYCLE LENGTH
C -----                           OR
C -----            TEXAS DIAMOND FIGURE NUMBER
C -----                           OR
C -----            NEMA NUMBER OF OVERLAPS
C ----- MOUT( 9) - NUMBER OF SIGNAL CONTROLLER PHASES
C ----- MOUT(10) - FIRST LINE OF SIGNAL SEQUENCE DATA
C ----- MOUT(11) - NEMA OR TEX-DIA - LINE BEFORE PHASE AND OVERLAP TEXT
C -----            OTHERS          - FIRST LINE OF CLEAR-TO DATA
C ----- MOUT(12) - FIRST LINE OF DETECTOR DATA
C ----- MOUT(13) - NUMBER OF DETECTORS
C ----- MOUT(14) - FIRST LINE OF DETECTOR CONNECTION DATA
C ----- MOUT(15) - LAST LINE OF SIMULATION DATA  (VMS MESSAGES)
C -----            NEXT LINE IS THE START OF G&D-V REF. DATA. 
C -----            IF NEGATIVE, THIS IS A DIAMOND
C ----- MOUT(16) - LAST LINE OF G&D-V REF. DATA
C
      READ(NLINE(16:),'(11I5)')(MOUT(I),I=6,16)
      IF(MOUT(15).LT.0)THEN
C
C ----- THIS IS A DIAMOND INTERSECTION
C
        DIAMON=.TRUE.
        MOUT(15)=-MOUT(15)
      ELSE
        DIAMON=.FALSE.
      END IF
C
C------ READ VERSION CARD
C
      IF ( MOUT(6) . EQ . 2 )                    THEN
        SVER = 5.99
      ELSE
        READ(NSFIO,'(1X,F4.2)',REC=2)SVER
      END IF
C
C ----- READ TITLE CARD
C
      READ(NSFIO,IOFORM,REC=MOUT(6))NLINE
      NRR=MOUT(6)+1
C
C ----- OPEN UNIT <IOF> FOR DATA THAT IS BEING CONVERTED TO C1.
C
      CALL OPENDD(IOF,CFILEP)
      REWIND IOF
C
C ----- WRITE TITLE CARD
C
      WRITE(NLINE(81:85),'(1HV,F4.2)')SVER
      WRITE(IOF,IOFORM,ERR=670)NLINE(1:85)
      GO TO 675
  670 CONTINUE
      WRITE(*,'(A)')
      CALL SHONAM('Converted Simulation data file is',CFILEP,78)
      CALL OPNERR(CFILEP)
      GO TO 9510
  675 CONTINUE
C
C ----- READ PARAMETER DATA
C
      NLINE=' '
C
C-----GDVSIM FIELDS
C
C-----TX_FMT_SIM_PAR_OPT_START_TIME         1: 4 STRTIM F4.1
C-----TX_FMT_SIM_PAR_OPT_SIM_TIME           5: 9 SIMTIM F5.1
C-----TX_FMT_SIM_PAR_OPT_DT                10:13 DT     F4.2
C-----TX_FMT_SIM_PAR_OPT_INTER_CONTROL     14:21 ICONTR A8
C-----TX_FMT_SIM_PAR_OPT_TURN_SUM          22:24 IPTC   A3
C-----TX_FMT_SIM_PAR_OPT_APPRO_SUM         25:27 IPAP   A3
C-----TX_FMT_SIM_PAR_OPT_COMPRESSED        28:30 IPUNCH A3
C-----TX_FMT_SIM_PAR_OPT_PVA_DATA          31:33 IPOLL  A3
C-----TX_FMT_SIM_PAR_OPT_PVA_END           34:37 ENDT20 I4
C-----TX_FMT_SIM_PAR_OPT_WIDE              38:40 IWNOUT A3
C-----TX_FMT_SIM_PAR_OPT_LT_PULLOUT        41:43 IPOUT  A3
C-----TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT 44:46 ISIMGO A3
C-----TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES  47:50 NVMSM  I4
C-----TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT 51:53 DZBTIM F3.1
C-----TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT 54:56 DZETIM F3.1
C-----TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME   57:59 HITLST I3
C
C-----TX_FMT_SIM_PAR_OPT_2_DS_SPEED         1: 2 XMPH   I2
C-----TX_FMT_SIM_PAR_OPT_2_QUEUE_DIST       3: 5 XQDIST I3
C-----TX_FMT_SIM_PAR_OPT_2_LAMBDA           6:10 CAREQL F5.3
C-----TX_FMT_SIM_PAR_OPT_2_MU              11:15 CAREQM F5.3
C-----TX_FMT_SIM_PAR_OPT_2_ALPHA           16:20 CAREQA I5
C-----TX_FMT_SIM_PAR_OPT_2_T_LEAD          21:24 TLEAD  F4.2
C-----TX_FMT_SIM_PAR_OPT_2_T_LAG           25:28 TLAG   F4.2
C-----TX_FMT_SIM_PAR_OPT_2_HESITATION_FACT 29:32 HESFAC F4.1
C-----TX_FMT_SIM_PAR_OPT_2_MAJCOL_STOP     33:35 MCSTOP A3
C-----TX_FMT_SIM_PAR_OPT_2_MAJCOL_PER_DSPD 36:38 IPDSCL I3
C-----TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_MN 39:42 IABTMN I4
C-----TX_FMT_SIM_PAR_OPT_2_MAJCOL_EVASA_SD 43:46 IABTSD I4
C-----TX_FMT_SIM_PAR_OPT_2_SSAM_FILE       47:49 ISSAM  A3
C
C-----SIMPRO FIELDS
C
C-----STRTIM   1:  4 F4.1
C-----SIMTIM   5: 10 F6.1
C-----DT      11: 15 F5.2
C-----XMPH    16: 18 F3.0
C-----XQDIST  19: 21 F3.0
C-----CAREQL  22: 27 F6.3
C-----CAREQM  28: 33 F6.3
C-----CAREQA  34: 38 F5.0
C-----ICONTR  39: 40 I2
C-----IWNOUT  41: 41 A1
C-----IPTC    42: 44 A3
C-----IPOUT   45: 45 A1
C-----IPAP    46: 48 A3
C-----        49: 49 1X
C-----IPUNCH  50: 52 A3
C-----        53: 53 1X
C-----IPOLL   54: 56 A3
C-----TLEAD   57: 61 F5.2
C-----TLAG    62: 66 F5.2
C-----ENDT20  67: 70 F4.0
C-----ISTATS  71: 73 I3    NOT USED
C-----TPRINT  74: 79 F6.1  NOT USED
C-----IXXX    80: 80 A1    NOT USED
C-----HESFAC  81: 84 F4.1
C-----XTMAX   85: 88 F4.1  NOT USED
C-----NVMSM   89: 92 I4
C-----MCSTOP  93: 95 A3
C-----IPDSCL  96: 98 I3
C-----IABTMN  99:102 I4
C-----IABTSD 103:106 I4
C-----ISSAM  107:109 A3
C-----ISIMGO 110:112 A3
C-----DZBTIM 113:115 F3.1
C-----DZETIM 116:118 F3.1
C-----HITLST 119:121 I3
C
C-----READ GDVSIM TX_FMT_SIM_PAR_OPT FIELDS
      READ(NSFIO,'(10A,2X,A,2X,6A)',REC=NRR)
     1  NLINE(  1:  4),NLINE(  6: 10),NLINE( 12: 15),IC            ,
     2  NLINE( 42: 44),NLINE( 46: 48),NLINE( 50: 52),NLINE( 54: 56),
     3  NLINE( 67: 70),NLINE( 41: 41),NLINE( 45: 45),NLINE(110:112),
     4  NLINE( 89: 92),NLINE(113:115),NLINE(116:118),NLINE(119:121)
      ICU=IC
      CALL  TOUPR   ( ICU )
C-----|UNCONTRL|YIELD|STOP|ALL-STOP|PRETIMED|SEMI-ACT|FULL-ACT|NEMA|TEX-DIA|HARDWARE|
C-----JCUNCT   1 INTERSECTION CONTROL - UNCONTROLLED
C-----JCYELD   2 INTERSECTION CONTROL - YIELD SIGN
C-----JCLTAS   3 INTERSECTION CONTROL - LESS-THAN-ALL-WAY STOP SIGN
C-----JCAWST   4 INTERSECTION CONTROL - ALL-WAY STOP SIGN
C-----JCPSIG   5 INTERSECTION CONTROL - PRE-TIMED SIGNAL
C-----JCSACT   6 INTERSECTION CONTROL - SEMI-ACTUATED SIGNAL
C-----JCFACT   7 INTERSECTION CONTROL - FULL-ACTUATED SIGNAL
C-----ICTDF3   8 INTERSECTION CONTROL - TEXAS  DIAMOND FIG 3 SIGNAL
C-----ICTDF4   9 INTERSECTION CONTROL - TEXAS  DIAMOND FIG 4 SIGNAL
C-----ICTDF6  10 INTERSECTION CONTROL - TEXAS  DIAMOND FIG 6 SIGNAL
C-----ICTDF7  11 INTERSECTION CONTROL - TEXAS  DIAMOND FIG 7 SIGNAL
C-----ICDDF3  12 INTERSECTION CONTROL - DALLAS DIAMOND FIG 3 SIGNAL NOT USED
C-----ICDDF4  13 INTERSECTION CONTROL - DALLAS DIAMOND FIG 4 SIGNAL NOT USED
C-----ICDDF6  14 INTERSECTION CONTROL - DALLAS DIAMOND FIG 6 SIGNAL NOT USED
C-----ICDDF7  15 INTERSECTION CONTROL - DALLAS DIAMOND FIG 7 SIGNAL NOT USED
C-----JCNEMA  16 INTERSECTION CONTROL - NEMA SIGNAL
C-----ICNEMV  17 INTERSECTION CONTROL - NEMA VOLUME DENSITY SIGNAL
C-----JCHDWR  18 INTERSECTION CONTROL - HARDWARE IN THE LOOP
      JCUNCT = (ICU(1:8).EQ.'UNCONTRL')
      JCYELD = (ICU(1:5).EQ.'YIELD'   )
      JCLTAS = (ICU(1:4).EQ.'STOP'    )
      JCAWST = (ICU(1:8).EQ.'ALL-STOP')
      JCPSIG = (ICU(1:8).EQ.'PRETIMED')
      JCSACT = (ICU(1:8).EQ.'SEMI-ACT')
      JCFACT = (ICU(1:8).EQ.'FULL-ACT')
      JCTDIA = (ICU(1:7).EQ.'TEX-DIA' )
      JCNEMA = (ICU(1:4).EQ.'NEMA'    )
      JCHDWR = (ICU(1:8).EQ.'HARDWARE')
      ANYSET = JCUNCT.OR.JCYELD.OR.JCLTAS.OR.JCAWST.OR.JCPSIG.OR.
     *         JCSACT.OR.JCFACT.OR.JCTDIA.OR.JCNEMA.OR.JCHDWR
      IF(.NOT.ANYSET)THEN
        TAG='NUL'
        WRITE(ERRMSG,903) IC
        CALL  PRTERR  ( ERRMSG )
        GO TO 9990
      END IF
      IF(DIAMON)THEN
C
C ----- FREE U-TURN LANES
C ----- TYPE OF INTERSECTION CONTROL
C
C ----- UCONT - INTERSECTION CONTROL IN SIMPRO TERMS:
C ----- 1 = OUTBOUND OR BLOCKED INBOUND LANE
C ----- 2 = UNCONTROLLED
C ----- 3 = YIELD SIGN
C ----- 4 = STOP  SIGN
C
        UCONT='31'
        IF(JCUNCT)THEN
          UCONT='21'
        END IF
        IF(JCAWST)THEN
          UCONT='31'
        END IF
      END IF
      READ(NLINE(11:15),'(F5.2)')DT
      XFACT=ICYC/(100.*DT)
      NRR=NRR+1
C-----READ GDVSIM TX_FMT_SIM_PAR_OPT2 FIELDS
      READ(NSFIO,'(13A)',REC=NRR)
     1  NLINE( 17: 18),NLINE( 19: 21),NLINE( 23: 27),NLINE( 29: 33),
     2  NLINE( 34: 38),NLINE( 58: 61),NLINE( 63: 66),NLINE( 81: 84),
     3  NLINE( 93: 95),NLINE( 96: 98),NLINE( 99:102),NLINE(103:106),
     4  NLINE(107:109)
      NPHAST=NPHA
      NPHASS=NPHA
      IF(JCUNCT)NLINE(39:40)=' 1'
      IF(JCYELD)NLINE(39:40)=' 2'
      IF(JCLTAS)NLINE(39:40)=' 3'
      IF(JCAWST)NLINE(39:40)=' 4'
      IF(JCPSIG)NLINE(39:40)=' 5'
      IF(JCSACT)NLINE(39:40)=' 6'
      IF(JCFACT)NLINE(39:40)=' 7'
      IF(JCTDIA)THEN
        IF(MOUT(8).EQ.3)NLINE(39:40)=' 8'
        IF(MOUT(8).EQ.4)NLINE(39:40)=' 9'
        IF(MOUT(8).EQ.6)NLINE(39:40)='10'
        IF(MOUT(8).EQ.7)NLINE(39:40)='11'
        NPHAST=6
        NPHASS=8
      END IF
      IF(JCNEMA)NLINE(39:40)='16'
      IF(JCHDWR)NLINE(39:40)='18'
      IF(JCNEMA.OR.JCHDWR)THEN
        IF(SVER.LT.6.00)THEN
          WRITE(*,'(/A)')'Old NEMA signal controller information found.'
          WRITE(*,'(A)' )'You must use gdvsim to convert to new NEMA.'
          GO TO 9999
        END IF
        LREC=MOUT(7)-1
        JREC=1
C
C ----- PHASE TIMING DATA
C
        DO 700 I=1,NPHAST
        ILINE(I)=' '
        READ(NSFIO,'(A,A)',REC=LREC+JREC)ILINE(I)(3:78),PEDDAT
        JREC=JREC+1
        CALL  TOUPR   ( PEDDAT )
        IF(PEDDAT.EQ.'YES')THEN
          READ(NSFIO,'(A)',REC=LREC+JREC)ILINE(I)(79:96)
          JREC=JREC+1
        END IF
  700   CONTINUE
C-----  IF ANY PHASE USES VOLUME-DENSITY OPERATION THEN SET INTERSECTION
C-----  CONTROL FOR NEMA VOLUME DENSITY
        DO 710 I=1,NPHAST
        ILINEU=ILINE(I)
        CALL  TOUPR ( ILINEU )
C-----  ILINE READ STARTING AT INDEX 3 RATHER THAN 1 THUS 57+2=59
        IF(ILINEU(59:61).EQ.'YES')NLINE(39:40)='17'
  710   CONTINUE
      END IF
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(NLINEU(41:41).EQ.'Y')THEN
        NLINE(41:41)='W'
      ELSE
        NLINE(41:41)='N'
      END IF
      WRITE(IOF,IOFORM)NLINE(1:ILNB( NLINE ))
C
C ----- READ G&D-V REF. DATA
C
      NR=MOUT(15)+2
      IF(DIAMON)THEN
        NFL=0
        NLSTL=NLGP1
        READ(NSFIO,'(I1,I2,2I5)',REC=NR)NLEGS,NINLNT,IUWDTH(1),IUWDTH(2)
        NR=NR+1
        READ(NSFIO,'(8(I2,I1,I3,I4))',REC=NR)
     1      (NLNS(I),NINLNS(I),LANGLE(I),LLENG(I),I=NFL,NLSTL)
      ELSE
        READ(NSFIO,'(I1,I2,6(I2,I1,I3,I4))',REC=NR)NLEGS,NINLNT,
     1      (NLNS(I),NINLNS(I),LANGLE(I),LLENG(I),I=1,NLEGS)
        IUWDTH(1)=0
        IUWDTH(2)=0
        NFL=1
        NLSTL=NLEGS
        DO 740 I=1,6
        LIAD(I)=I
        LOAD(I)=I+NLEGS
  740   CONTINUE
      END IF
      DO 750 I=NFL,NLSTL
      NR=NR+1
      READ(NSFIO,'(6(A4,2I4))',REC=NR)
     1     (MVIL(J,I),IUBLI(J,I),IUBLO(J,I),J=1,NINLNS(I))
      NR=NR+1
      READ(NSFIO,'(6I4)',REC=NR)(IOFF(J,I),J=1,NINLNS(I))
  750 CONTINUE
      IF (DIAMON)THEN
        IF((NINLNS(1).GT.0)         .AND. (NINLNS(4).GT.0) .AND.
     *     (NLNS(3)-NINLNS(3).GT.0) .AND. (NLNS(6)-NINLNS(6).GT.0))THEN
C
C ----- 2 WAY FRONTAGE ROADS
C
          IF (IUWDTH(1) + IUWDTH(2).GT.0)THEN
C ----- RFI ERROR
C     BOTH 2 WAY FRONTAGE ROADS AND FREE UTURN LANE NOT ALLOWED
          END IF
          ILPSD = 2
        ELSE
C
C ----- 1 WAY FRONTAGE ROADS
C
          IF((NINLNS(1) + NINLNS(4) + (NLNS(3)-NINLNS(3)) + 
     *       (NLNS(6)-NINLNS(6))) .GT. 0)THEN
C ----- RFI ERROR
C     IF 1 FRONTAGE WAY IS 1 WAY, ALL MUST BE 1 WAY
          END IF
          ILPSD = 1
        END IF
 
      END IF
C
C ----- READ LANE CONTROL DATA
C
      NRR=NRR+1
      READ(NSFIO,IOFORM,REC=NRR)NLINE
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(JCNEMA.OR.JCHDWR)THEN
        IREC=MOUT(11)+1
C
C ----- PHASE AND OVERLAP NAME TEXT
C
        READ(NSFIO,'(2X,20A3)',REC=IREC)(PHOLTX(I),I=1,NPHA)
        IF (NOLP .GT. 0)THEN
          IREC=IREC+1
          READ(NSFIO,'(2X,20A3)',REC=IREC)(PHOLTX(I),I=NPHA+1,NPHA+NOLP)
        END IF
        NOLPA=NOLP
        NRING=0
        NGROUP=0
        IF(JCNEMA)THEN
C
C -----   READ NUMBER AND LIST OF PHASES IN EACH RING AND GROUP
C
          IREC=IREC+1
          READ(NSFIO,'(40I2)',REC=IREC)NRING,NGROUP,
     1                                 ((NPHPS(I,J),(LPHPS(I,J,K),
     2                                 K=1,NPHPS(I,J)),J=1,NGROUP),
     3                                 I=1,NRING)
C
C -----   SKIP THE NEMA PHASE DIAGAM
C
          IREC=IREC+1
C
C -----   OVERLAPS INACTIVE ?
C
          DO 760 IO=1,NOLP
C
C -----   LIST OF PHASES ON OVERLAP DEFINITION LIST
C
          READ(NSFIO,'(40I2)',REC=IREC+IO)(LOLDEF(IO1,IO),IO1=1,NPHA-1)
          NOLDEF(IO)=0
          DO 755 IO1=1,NPHA-1
C -----   NUMBER OF PHASES ON OVERLAP DEFINITION LIST
          IF(LOLDEF(IO1,IO).EQ.0)GO TO 756
          NOLDEF(IO)=IO1
  755     CONTINUE
  756     CONTINUE
C         IF(NOLDEF(IO).EQ.0)THEN
C
C -----     OVERLAP IS INACTIVE (NO PHASES ON DEFINITION LIST)
C
C           NOLPA=NOLPA-1
C         END IF
  760     CONTINUE
        END IF
        NPHASS=NPHA+NOLP
      END IF
C ----- LANE CONTROL DATA ORDER
C       INPUT      IR L1 L2 L3 L4 L5 L6 IL
C       SUBSCRIPT   0  1  2  3  4  5  6  7
C
C ----- OUTPUT     L1 L2 L3 L4 L5 L6 IR IL
C
      K=1
      DO 820 II=NFL,NLSTL
C
C ----- PROCESS EACH LEG (I) IN THE SPECIFIED SEQUENCE
C
      IF(DIAMON)THEN
        I=LPSD(II,ILPSD)
        IP=LPD(I)
      ELSE
        I=II
        IP=I-1
        IF(IP.LT.NFL)IP=NLSTL
      END IF
      K1=0
      DO 790 J=NFL,I-1
C
C ----- K1 IS COLUMN FOR START OF INPUT (PREPROCESSOR) DATA FOR THIS LEG
C
      K1=K1+NINLNS(J)
  790 CONTINUE
      K1=K1+K1-1
C
C ----- K IS COLUMN NUMBER FOR CONVERTED DATA
C
      DO 810 J=1,NINLNS(I)
      K1=K1+2
      IF(DIAMON .AND. (J.EQ.1))THEN
C
C ----- PREPARE TO SWAP ENTRANCE AND EXIT CONTROL FOR FREE U-TURN LANES
C
        IF((I.EQ.3) .AND. (IUWDTH(1).GT.0))THEN
          L3LN1=K
          IF(NLINEU(K1:K1).EQ.'U')THEN
            TLINE(K:K)=UCONT(1:1)
            K = K + 1
            GO TO 810
          END IF
        END IF
        IF((I.EQ.6) .AND. (IUWDTH(2).GT.0))THEN
          L6LN1=K
          IF(NLINEU(K1:K1).EQ.'U')THEN
            TLINE(K:K)=UCONT(1:1)
            K = K + 1
            GO TO 810
          END IF
        END IF
      END IF
      IF((I.EQ.0) .OR. (I.EQ.NLGP1))THEN
C
C ----- SKIP HERE, PUT AT END
C
        IF (I.EQ.0) THEN
C
C----- INBOUND TO CENTER R
C
          IF (J.EQ.1)K1R = K1
        ELSE
C
C----- INBOUND TO CENTER L
C
          IF (J.EQ.1)K1L = K1
        END IF
        GO TO 810
      END IF
      CALL LNCNTI(K,K1,NLINEU,TLINE)
  810 CONTINUE
C
C ----- OUTBOUND LANES FROM PREVIOUS LEG
C
      IF((IP.EQ.0) .OR. (IP.EQ.NLGP1))THEN
C
C ----- SKIP HERE, PUT AT END
C
        GO TO 820
      END IF
      CALL LNCNTO(K,NLNS(IP)-NINLNS(IP),TLINE)
  820 CONTINUE

C
C----- INBOUND TO CENTER R
C
      K1 = K1R
      DO J = 1,NINLNS(0)
        CALL LNCNTI(K,K1,NLINEU,TLINE)
        K1 = K1 + 2
      END DO
C----- OUTBOUND FROM CENTER R
      IP = 0
      CALL LNCNTO(K,NLNS(IP)-NINLNS(IP),TLINE)
C
C----- INBOUND TO CENTER L
C
      K1 = K1L
      DO J = 1,NINLNS(NLGP1)
        CALL LNCNTI(K,K1,NLINEU,TLINE)
        K1 = K1 + 2
      END DO
C----- OUTBOUND FROM CENTER L
      IP = NLGP1
      CALL LNCNTO(K,NLNS(IP)-NINLNS(IP),TLINE)
      IF(DIAMON)THEN
C
C ----- FREE U-TURN LANES
C ----- SWAP THE CONTROL AT THE ENTRANCE END WITH CONTROL AT EXIT END
C
        IF(IUWDTH(1).GT.0)THEN
C
C ----- LEG 3 TO LEG 4
C
          TEMPC1=TLINE(L3LN1:L3LN1)
          TLINE(L3LN1:L3LN1)=UCONT(1:1)
          TLINE(K:K+1)=TEMPC1//UCONT(2:2)
          K=K+2
C
C ----- PREPARE FOR SWAPPING CAMSTACK ENTRIES
C
          L3LN1=NINLNS(1)+NINLNS(2)
          L3LN1=L3LN1*3+6
        END IF
        IF(IUWDTH(2).GT.0)THEN
C
C ----- LEG 6 TO LEG 1
C
          TEMPC1=TLINE(L6LN1:L6LN1)
          TLINE(L6LN1:L6LN1)=UCONT(1:1)
          TLINE(K:K+1)=TEMPC1//UCONT(2:2)
          K=K+2
C
C ----- PREPARE FOR SWAPPING CAMSTACK ENTRIES
C
          L6LN1=NINLNS(1)+NINLNS(2)+NINLNS(3)+NINLNS(4)+NINLNS(5)
          L6LN1=L6LN1*3+6
        END IF
      END IF
      WRITE(IOF,IOFORM)TLINE(1:K-1)
      IF(NRR.EQ.MOUT(15))GO TO 8000
      IF(JCUNCT.OR.JCYELD.OR.JCLTAS.OR.JCAWST)GO TO 8000
      NRR=NRR+1
C
C ----- BUILD CAM STACK
C
      IREC=MOUT(10)-1
      IOFFST=0
      NC1=NINLNT+NINLNT+1
      DO 830 I=1,NPHASS
C
C ----- READ SIGNAL SEQUENCE DATA
C
      READ(NSFIO,IOFORM,REC=IREC+I+IOFFST)MLINE(I)
      IF(JCNEMA .OR. JCHDWR)THEN
       GO TO 830
      END IF
C
C ----- CHECK FOR ALL RED REST PHASE
C
      IF(JCTDIA)THEN
        IF(I.LE.NPHAST)ARREST(I)=.FALSE.
      ELSE
        ARREST(I)=BLUNLT(MLINE(I),1,NINLNT*2-1)
      END IF
  830 CONTINUE
      IREC=MOUT(11)-1
      NCCTT=0
      IT3=1
      DO 860 II=1,NPHAST
      IF(JCTDIA .OR. JCNEMA .OR. JCHDWR)THEN
        NCCT(II)=1
      ELSE
C
C ----- READ "CLEAR TO" DATA
C
        NFW=2
        READ(NSFIO,IOFORM,REC=IREC+II)NLINE(57:)
        IT3=ILNB( NLINE(57:) )+NFW-1
        IT3=IT3/NFW
        IF((IT3.LT.1).OR.(IT3.GT.(NPH-1)))THEN
          TAG='NUL'
          WRITE(ERRMSG,904)II,IT3,(NPH-1)
          CALL  PRTERR  ( ERRMSG )
          GO TO 9990
        END IF
        NCCT(II)=IT3
C
C ----- CONVERT FROM LETTERS TO NUMBERS WITH A=>1, B=>2, ETC
C
        IT1=57
        NLINEU=NLINE
        CALL  TOUPR   ( NLINEU )
        DO 840 KK=1,IT3
        IF((NLINEU(IT1:IT1).GE.'0').AND.(NLINEU(IT1:IT1).LE.'9'))THEN
          READ(NLINEU(IT1:IT1),'(I1)')IT2
        ELSE
          IT2=ICHAR( NLINEU(IT1:IT1) )-ITAM1
        END IF
        LISTCT(KK,II)=IT2
        IT1=IT1+NFW
  840   CONTINUE
      END IF
      IF((JCNEMA.OR.JCTDIA.OR.JCHDWR) .AND. (II.GT.NPHAST))GO TO 860
      IF(.NOT.ARREST(II))NCCTT=NCCTT+IT3
  860 CONTINUE
      IF(JCNEMA .OR. JCHDWR)THEN
C
C ----- THIS IGNORES TEXT PHASE NUMBERING FROM THE INPUT FILE, USES NUMBERS
C         WRITE(LLINE(1),'(1X,A1,3X,25A3)')PHOLTX(1),('AR ',II=1,NINLNT)
C
        WRITE(LLINE(1),'(I2,3X,25A3)')1,('AR ',II=1,NINLNT)
      ELSE
        WRITE(LLINE(1),'(I2,3X,25A3)')1,('AR ',II=1,NINLNT)
      END IF
      LREC=MOUT(7)-1
      NALLR=0
      DO 870 I=1,NPHAST
      IF(JCPSIG)THEN
        IF(ICYC.EQ.0)THEN
C
C ----- READ TIME INTERVALS FOR PRETIMED
C
          READ(NSFIO,'(F4.1,2F3.1)',REC=LREC+I)PTG(I),PTY(I),ALLRED(I)
        ELSE
C
C ----- READ PERCENTS FOR PRETIMED AND CONVERT TO TIME INTERVALS
C ----- ROUND TO "DT"
C
          READ(NSFIO,'(I4,2I3)',REC=LREC+I)IT1,IT2,IT3
          IT1=IT1*XFACT+0.5
          PTG(I)=IT1*DT
          IT1=IT2*XFACT+0.5
          PTY(I)=IT1*DT
          IT1=IT3*XFACT+0.5
          ALLRED(I)=IT1*DT
        END IF
      ELSE
C
C ----- PREPARE ACTUATED SIGNAL TIMING DATA
C
        IF(JCNEMA .OR. JCHDWR)THEN
C
C ----- THIS IGNORES LEFT JUSTIFIED TEXT PHASE NUMBERING FROM INPUT FILE
C         WRITE(ILINE(I)(1:3),'(1X,A2)')PHOLTX(I)
C
C ----- PHASE NUMBERS MUST BE RIGHT JUSTIFIED IN OUTPUT
C
          WRITE(ILINE(I)(1:2),'(I2)')I
        ELSE
          IF(JCTDIA .AND. (I.GT.3))THEN
C
C ----- PHASES 5, 6 AND 7
C
            IPH=I+1
          ELSE
            IPH=I
          END IF
          WRITE(ILINE(I),FMT2)IPH
        END IF
        IF((I.EQ.1) .AND. JCSACT)THEN
C
C ----- UNACTUATED PHASE OF SEMI ACTUATED
C
          READ(NSFIO,'(A4,4X,2A3)',REC=LREC+I)ILINE(1)(4:7),
     1        ILINE(1)(15:17),ILINE(1)(20:22)
        ELSE IF(.NOT.(JCNEMA .OR. JCHDWR))THEN
          READ(NSFIO,'(2A4,2A3,A4,4A3)',REC=LREC+I)ILINE(I)(4:7),
     1            ILINE(I)(9:12),ILINE(I)(15:17),ILINE(I)(20:22),
     2            ILINE(I)(25:28),ILINE(I)(30:32),ILINE(I)(34:36),
     3            ILINE(I)(38:40),ILINE(I)(42:44)
        END IF
        IF(.NOT.(JCTDIA .OR. JCNEMA .OR. JCHDWR))THEN
C
C ----- PUT "CLEAR TO" DATA IN OUTPUT TEXT LINE
C
          WRITE(ILINE(I)(53:),'(I4,7I2)')NCCT(I),
     1                                   (LISTCT(II,I),II=1,NCCT(I))
        END IF
        READ(ILINE(I)(20:22),'(F3.1)')ALLRED(I)
      END IF
      IF(ARREST(I))GO TO 870
      IF(ALLRED(I).NE.0.0)NALLR=NALLR+1
  870 CONTINUE
      IF(JCNEMA .OR. JCHDWR)THEN
        DO I=NPHAST+1,NPHASS
C
C ----- PUT OVERLAP LETTERS IN OUTPUT TEXT LINE
C
          WRITE(ILINE(I)(1:2),'(1X,A1)')PHOLTX(I)
        END DO
C
C ----- NUMBER OF CAM STACK POSITIONS, PHASES, OVERLAPS, RINGS, GROUPS
C
        IF(JCNEMA)THEN
          WRITE(IOF,'(5I4)')(NPHAST+NOLPA)*2,NPHA,NOLPA,NRING,NGROUP
        ELSE
          WRITE(IOF,'(3I4)')(NPHAST+NOLPA)*2,NPHA,NOLPA
        END IF
      ELSE
C
C ----- WRITE NUMBER OF CAM STACK POSITIONS
C
        WRITE(IOF,FMT4)NPHAST+NALLR+NCCTT
      END IF
      DO 4000 I=1,NPHASS
C      IF(JCNEMA .AND. (I.GT.NPHAST))THEN
C
C ----- IT THIS AN INACTIVE OVERLAP ?
C
C        IF(NOLDEF(I-NPHAST).EQ.0)GO TO 3900
C      END IF
      IT1=3
      IT2=5
      IF(I.LE.NPHAST)THEN
        DO 880 II=1,NCCT(I)
        IF(II.EQ.1)THEN
C
C -----COPY THE PHASE NUMBER TO EACH CAMSTACK CLEARANCE POSITION
C
          LLINE(2)=LLINE(1)(1:2)
        ELSE
          LLINE(II+1)=LLINE(1)
        END IF
  880   CONTINUE
      ELSE
C
C ----- NEMA
C ----- COPY THE OVERLAP LETTER TO CAMSTACK CLEARANCE POSITION
C
        LLINE(2)=LLINE(1)(1:2)
      END IF
      IT1R = 0
      IT1L = 0
      IF (DIAMON) THEN
C
C ----- PUT IR CAMSTACK POSITIONS TO RIGHT OF LEG 6 AND LEFT OF IL
C
        DO J = 1,NLEGS
          IT1R = IT1R +NINLNS(J) 
        END DO
        IT1R = IT1R * 3
        IT1R = IT1R + 3
        IT1L = NINLNS(0) * 3
        IT1L = IT1L + IT1R
        IT1T = 0
      END IF
      DO 3000 JJ=NFL,NLSTL
      IF(DIAMON)THEN
        J=LPSD(JJ,ILPSD)
        IF (JJ .EQ. 0) THEN
          IT1T = IT1
          IT1 = IT1R
        ELSE IF (JJ .EQ. NLGP1) THEN
          IT1T = IT1
          IT1 = IT1L
        ELSE
          IF (IT1T.GT.0)IT1 = IT1T
          IT1T = 0
        END IF
      ELSE
        J=JJ
      END IF
      IT3=0 
      DO 900 III=NFL,J-1
C
C ----- IT3 IS COLUMN FOR START OF PREPROCESSOR DATA FOR THIS LEG
C
      IT3=IT3+NINLNS(III)
  900 CONTINUE
      IT3=IT3+IT3-1
      DO 2800 K=1,NINLNS(J)
      IT3=IT3+2
      IT4=IT3+1
      SSC=MLINE(I)(IT3:IT4)
      IT1=IT1+3
      IT2=IT1+2
      SSCU=SSC
      CALL  TOUPR   ( SSCU )
      IF(SSCU.EQ.' ')THEN
        BB=.TRUE.
        NCB=0
        GO TO 2000
      END IF
      IF(SSCU.EQ.'UN')THEN
        LLINE(1)(IT1:IT2)=UNS
        GO TO 2000
      END IF
C
C ----- GET SIGNAL STATUS "B"EFORE THE CLEARANCE
C
      CALL CSLV(SSC,NCB,BB,BC,BL,BS,BR)
      IF(NCB.EQ.1)THEN
C
C ----- SINGLE CHARACTER SIGNAL CODE
C
        IF(BC)THEN
          LLINE(1)(IT1:IT2)='AG'
          GO TO 2020
        END IF
        IF(BL)THEN
          LLINE(1)(IT1:IT2)='LPR'
          GO TO 2020
        END IF
        IF(BR)THEN
          LLINE(1)(IT1:IT2)='RGR'
          GO TO 2020
        END IF
        LLINE(1)(IT1:IT2)='SGR'
        GO TO 2020
      END IF
C
C ----- TWO CHARACTER SIGNAL CODE
C
      IF(BC)THEN
        IF(BL)THEN
          IF(EXCLT(MVIL(K,J)))THEN
            LLINE(1)(IT1:IT2)='LPR'
          ELSE
            LLINE(1)(IT1:IT2)='LPG'
          END IF
          GO TO 2020
        END IF
        LLINE(1)(IT1:IT2)='AG'
        GO TO 2020
      END IF
      IF(BL)THEN
        IF(BS)LLINE(1)(IT1:IT2)='RRG'
        IF(BR)LLINE(1)(IT1:IT2)='SRG'
        GO TO 2020
      END IF
      LLINE(1)(IT1:IT2)='LRG'
      GO TO 2020
 2000 CONTINUE
      IF (I.LE. NPHAST ) THEN
        DO 2010 II=2,NCCT(I)+1
        LLINE(II)(IT1:IT2)=' '
 2010   CONTINUE
      END IF
      GO TO 2800
 2020 CONTINUE
C
C ----- YELLOW SIGNAL INDICATION FOR EACH PHASE THAT CAN BE CLEARED TO
C
      IF(I.LE.NPHAST)THEN
        IF(ARREST(I))GO TO 2800
      END IF
      IF(I.LE.NPHAST)THEN
        J = NCCT(I)
      ELSE
        J = 1
      END IF
      DO 2700 II=1,J
      IF(BB)GO TO 2500
      IF(JCTDIA .OR. JCNEMA .OR. JCHDWR)THEN
C
C ----- ASSUME ALL INDICATIONS ARE RED WHEN PHASE IS NOT ACTIVE
C
        SSCA=' '
      ELSE
        IT5=LISTCT(II,I)
        SSCA=MLINE(IT5)(IT3:IT4)
      END IF
      IF(SSC.EQ.SSCA)GO TO 2500
C
C ----- GET SIGNAL STATUS "A"FTER THE CLEARANCE
C
      CALL CSLV(SSCA,NCA,AB,AC,AL,AS,AR)
      IF(AC .AND. (.NOT.BL))GO TO 2500
      IF(AB .AND. (NCB.EQ.2))GO TO 2510
      IF((NCB.EQ.1) .AND. (NCA.EQ.2))THEN
        IF(BC.AND.AC)GO TO 2500
        IF(BL.AND.AL)GO TO 2500
        IF(BS.AND.AS)GO TO 2500
        IF(BR.AND.AR)GO TO 2500
      END IF
      IF(NCB.EQ.2)GO TO 2100
      IF(BC)THEN
        IF(AB)GOTO 2510
        IF(NCA.EQ.1)THEN
          IF(AL)GO TO 2540
          IF(AS)GO TO 2580
          IF(AR)GO TO 2610
        ELSE
          IF(AL)THEN
            IF(AS)GO TO 2620
            IF(AR)GO TO 2590
          END IF
          IF(AR.AND.AS)GO TO 2550
        END IF
      END IF
      IF(BL)THEN
        IF(AC .AND. (NCA.EQ.1))GO TO 2550
        GO TO 2560
      END IF
      IF(BS)GO TO 2600
      GO TO 2630
 2100 CONTINUE
      IF(BC)THEN
        IF(BL)THEN
          IF(NCA.EQ.1)THEN
            IF(AC)GO TO 2530
            IF(AL)GO TO 2540
            IF(AS)GO TO 2580
            GO TO 2610
          END IF
          IF(AC)GO TO 2550
          IF(AL)THEN
            IF(AS)GO TO 2620
            GO TO 2590
          END IF
          GO TO 2550
        END IF
        IF(BS)THEN
          IF(AR.AND.AS)GO TO 2500
          IF(AS)GO TO 2620
          IF(AR)GO TO 2590
          GO TO 2510
        END IF
        IF(BR)THEN
          IF(NCA.EQ.1)THEN
            IF(AL)GO TO 2510
            IF(AS)GO TO 2620
            GO TO 2590
          END IF
          IF(AL)THEN
            IF(AS)GO TO 2580
            GO TO 2610
          END IF
          GO TO 2500
        END IF
      END IF
      IF(AS)GO TO 2580
      IF(AR)GO TO 2610
      GO TO 2510
C
C ----- SET YELLOW FIELDS
C
 2500 CCTEMP=' '
      GO TO 2690
 2510 CCTEMP='AA'
      GO TO 2690
 2530 CONTINUE
      CCTEMP='LAG'
      GO TO 2690
 2540 CONTINUE
      IF(EXCLT(MVIL(K,J)))THEN
        CCTEMP=' '
      ELSE
        CCTEMP='LGA'
      END IF
      GO TO 2690
 2550 CONTINUE
      IF(EXCLT(MVIL(K,J)))THEN
        CCTEMP='LAG'
      ELSE
        CCTEMP=' '
      END IF
      GO TO 2690
 2560 CCTEMP='LAR'
      GO TO 2690
 2580 CCTEMP='SGA'
      GO TO 2690
 2590 CCTEMP='SAG'
      GO TO 2690
 2600 CCTEMP='SAR'
      GO TO 2690
 2610 CCTEMP='RGA'
      GO TO 2690
 2620 CCTEMP='RAG'
      GO TO 2690
 2630 CCTEMP='RAR'
 2690 LLINE(II+1)(IT1:IT2)=CCTEMP
 2700 CONTINUE
 2800 CONTINUE
 3000 CONTINUE
      IF(JCPSIG)THEN
C
C ----- PUT TIME INTERVALS IN CAM STACK FOR PRETIMED
C
        IT1=PTG(I)+0.5
        WRITE(LLINE(1)(3:5),FMT3)IT1
        IT1=PTY(I)+0.5
        WRITE(LLINE(2)(3:5),FMT3)IT1
        DO 3100 J=3,NCCT(I)+1
        LLINE(J)(3:5)=LLINE(2)(3:5)
 3100   CONTINUE
      END IF
      IF(I.GT.NPHAST)THEN
        IT1=2
      ELSE
        IT1=1
        IF(.NOT.ARREST(I))IT1=IT1+NCCT(I)
      END IF
      IF(DIAMON)THEN
C
C ----- FREE U-TURN LANES
C
        IF(IUWDTH(1).GT.0)THEN
C
C ----- INTERNAL LANES FROM LEG 3 TO LEG 4
C ----- SWAP THE ENTRANCE AND EXIT CAMSTACK ENTRY
C
          IT6=IT2+1
          IT2=IT2+3
          LLINE(1)(IT6:IT2)=LLINE(1)(L3LN1:L3LN1+2)
          LLINE(1)(L3LN1:L3LN1+2)=UNS
          DO 3150 II=2,IT1
          LLINE(II)(IT6:IT2)=LLINE(II)(L3LN1:L3LN1+2)
          LLINE(II)(L3LN1:L3LN1+2)=UNS
 3150     CONTINUE
        END IF
        IF(IUWDTH(2).GT.0)THEN
C
C ----- INTERNAL LANES FROM LEG 6 TO LEG 1
C ----- SWAP THE ENTRANCE AND EXIT CAMSTACK ENTRY
C
          IT6=IT2+1
          IT2=IT2+3
          LLINE(1)(IT6:IT2)=LLINE(1)(L6LN1:L6LN1+2)
          LLINE(1)(L6LN1:L6LN1+2)=UNS
          DO 3160 II=2,IT1
          LLINE(II)(IT6:IT2)=LLINE(II)(L6LN1:L6LN1+2)
          LLINE(II)(L6LN1:L6LN1+2)=UNS
 3160     CONTINUE
        END IF
      END IF
      WRITE(IOF,IOFORM)LLINE(1)(1:ILNB( LLINE(1) ))
      WRITE(IOF,IOFORM)(LLINE(II)(1:ILNB( LLINE(1) )),II=2,IT1)
      IT1=3
      IT2=5
      DO 3200 II=1,NINLNT
      IT1=IT1+3
      IT2=IT2+3
      IF(LLINE(1)(IT1:IT2).NE.UNS)LLINE(1)(IT1:IT2)='AR '
 3200 CONTINUE
      IF(DIAMON)THEN
C
C ----- FREE U-TURN LANES
C
        IF(IUWDTH(1).GT.0)THEN
C
C ----- INTERNAL LANES LEG 3 TO LEG 4
C
          IT1=IT1+3
          IT2=IT2+3
          IF(LLINE(1)(IT1:IT2).NE.UNS)LLINE(1)(IT1:IT2)='AR '
        END IF
        IF(IUWDTH(2).GT.0)THEN
C
C ----- INTERNAL LANES LEG 6 TO LEG 1
C
          IT1=IT1+3
          IT2=IT2+3
          IF(LLINE(1)(IT1:IT2).NE.UNS)LLINE(1)(IT1:IT2)='AR '
        END IF
      END IF
      IF(JCNEMA)GO TO 3240
      IF((.NOT.ARREST(I)) .AND. (ALLRED(I).GT.0.))THEN
C
C ----- PUT ALL RED INTERVAL INTO CAM STACK
C
        IF(JCPSIG)THEN
          IT1=ALLRED(I)+0.5
          WRITE(IOF,'(A,I3,A)')LLINE(1)(1:2),IT1,LLINE(1)(6:IT2)
        ELSE
          WRITE(IOF,'(A)')LLINE(1)(1:IT2)
        END IF
      END IF
 3240 CONTINUE
      IF(DIAMON)THEN
C
C ----- UN-SWAP THE ENTRANCE AND EXIT CAMSTACK ENTRY
C
        IF(IUWDTH(2).GT.0)THEN
          LLINE(1)(L6LN1:L6LN1+2)=LLINE(1)(IT6:IT2)
          LLINE(1)(IT6:IT2)=UNS
          IT6=IT6-3
          IT2=IT2-3
        END IF
        IF(IUWDTH(1).GT.0)THEN
          LLINE(1)(L3LN1:L3LN1+2)=LLINE(1)(IT6:IT2)
          LLINE(1)(IT6:IT2)=UNS
          IF(IUWDTH(2).GT.0)IT2=IT2+3
        END IF
      END IF
C
C ----- PUT NEXT PHASE NUMBER IN CAMSTACK
C
      IF(JCTDIA)THEN
        IF(I.GE.6)THEN
C
C ----- USE LETTERS FOR OVERLAPS
C
          IF(I.EQ.6)WRITE(LLINE(1)(1:2),IOFORM)' A'
          IF(I.EQ.7)WRITE(LLINE(1)(1:2),IOFORM)' B'
          GO TO 4000
        END IF
        IF(I.GE.3)THEN
C
C ----- NEXT PHASE IS 5, 6 OR 7
C
          WRITE(LLINE(1)(1:2),'(I2)')I+2
          GO TO 4000
        END IF
      END IF
      IF(.NOT.(JCNEMA .OR. JCHDWR))THEN
        WRITE(LLINE(1)(1:2),'(I2)')I+1
        GO TO 4000
      END IF
 3900 CONTINUE
C
C ----- NEMA
C
      IF ((I+1) .GT. NPHOL) GO TO 4000
      IF ((I+1) .LE. NPHAST) THEN
C
C ----- THIS IGNORES LEFT JUSTIFIED TEXT PHASE NUMBERING FROM INPUT FILE
C         WRITE(LLINE(1)(1:3),'(1X,A2)')PHOLTX(I+1)
C
C ----- PHASE NUMBERS MUST BE RIGHT JUSTIFIED IN OUTPUT
C
        WRITE(LLINE(1)(1:2),'(I2)')I+1
      ELSE
C
C ----- OVERLAP LETTERS
C
        WRITE(LLINE(1)(1:2),'(1X,A1)')PHOLTX(I+1)
      END IF
 4000 CONTINUE
      IF(JCPSIG)GO TO 8000
C
C ----- WRITE ACTUATED CONTROLLER TIMING DATA
C
      IF(.NOT.(JCNEMA .OR. JCHDWR))WRITE(IOF,FMT4)NPHAST
      IREC=MOUT(14)-1
      DO 4500 I=1,NPHAST
      IT1=0
      IF(JCTDIA)GO TO 4400
      IF((I.EQ.1) .AND. JCSACT)GO TO 4400
C
C ----- DETECTOR CONNECTION DATA
C
      IF ( JCNEMA .OR. JCHDWR )                  THEN
        READ(NSFIO,'(16(I2,A3))',REC=IREC+I)
     *    (LDETC(J),TDETC(J),J=1,NPL+3)
        NDETCN = 0
        NDETCC = 0
        NDETCE = 0
        DO J = 1 , NPL+3
          IF ( LDETC(J) .EQ. 0 )                 EXIT
          IF ( TDETC(J) .EQ. "   " )             TDETC(J) = "C&E"
          IF      ( TDETC(J) .EQ. "C&E" )        THEN
            NDETCN = NDETCN + 1
            LDETCN(NDETCN) = LDETC(J)
          ELSE IF ( TDETC(J) .EQ. "CAL" )        THEN
            NDETCC = NDETCC + 1
            LDETCC(NDETCC) = LDETC(J)
          ELSE IF ( TDETC(J) .EQ. "EXT" )        THEN
            NDETCE = NDETCE + 1
            LDETCE(NDETCE) = LDETC(J)
          ELSE
            TAG='NUL'
            WRITE(ERRMSG,720) TDETC(J)
            CALL  PRTERR  ( ERRMSG )
            GO TO 9990
          END IF
        END DO
C ----- WRITE PHASE DATA RECORD
        WRITE(IOF,'(A,99I2)')ILINE(I)(1:96),
     *                       NDETCN,(LDETCN(J),J=1,NDETCN),
     *                       NDETCC,(LDETCC(J),J=1,NDETCC),
     *                       NDETCE,(LDETCE(J),J=1,NDETCE)
        GO TO 4500
      END IF
      READ(NSFIO,'(A3,10I3)',REC=IREC+I)ILINE(I)(46:48),
     1                                  (LDETCN(J),J=1,10)
      DO 4300 J=1,10
      IF(LDETCN(J).EQ.0)GO TO 4310
      IT1=J
 4300 CONTINUE
 4310 CONTINUE
      WRITE(ILINE(I)(51:52),FMT2)IT1
 4400 CONTINUE
      WRITE(IOF,IOFORM)ILINE(I)(1:ILNB( ILINE(I) ))
      IF(IT1.EQ.0)GO TO 4500
C
C ----- CONNECTIONS ARE IN ASCENDING ORDER
C ----- PUT POSITIVE CONNECTIONS FIRST
C
      IT2=-3
      IT4=0
      DO 4450 J=1,IT1
C
C ----- POSITIVE CONNECTIONS
C
      IF(NCCT(J).LT.0)THEN
        IT4=J
        GO TO 4450
      END IF
      IT2=IT2+4
      IT3=IT2+3
      WRITE(NLINE(IT2:IT3),FMT4)LDETCN(J)
 4450 CONTINUE
      DO 4460 J=1,IT4
C
C ----- NEGATIVE CONNECTIONS
C
      IT2=IT2+4
      IT3=IT2+3
 4460 WRITE(NLINE(IT2:IT3),FMT4)LDETCN(J)
      WRITE(IOF,IOFORM)NLINE(1:IT3)
 4500 CONTINUE
      IF(JCNEMA)THEN
C
C ----- WRITE NUMBER AND LIST OF PHASES IN EACH RING AND GROUP
C
        DO 4660 IR=1,NRING
        DO 4650 IG=1,NGROUP
        WRITE(IOF,'(16I2)')(LPHPS(IR,IG,II),II=1,NPHPS(IR,IG))
 4650   CONTINUE
 4660   CONTINUE
C
C ----- OVERLAP DEFINITIONS
C
        DO 4680 IO=1,NOLP
C        IF(NOLDEF(IO).EQ.0)GO TO 4680
        WRITE(IOF,'(1X,A1,99I2)')PHOLTX(NPHA+IO),
     1                           (LOLDEF(IO1,IO),IO1=1,NOLDEF(IO))
 4680   CONTINUE
      END IF
      IF(JCTDIA)THEN
        IPLUS=0
        IF(IFIGUR.EQ.4)IPLUS=1
        IF(IFIGUR.EQ.6)IPLUS=2
        IF(IFIGUR.EQ.7)IPLUS=3
C
C ----- TEXAS DIAMOND CONTROLLER INTERVALS
C
        READ(NSFIO,IOFORM,REC=MOUT(11)+IPLUS)NLINE
        NC=MAX0(1,ILNB( NLINE ))
        WRITE(IOF,IOFORM)NLINE(1:NC)
C
C ----- TEXAS DIAMOND CONTROLLER OPTIONS
C
        IPLUS=IPLUS+4
        READ(NSFIO,IOFORM,REC=MOUT(11)+IPLUS)NLINE
        NC=MAX0(1,ILNB( NLINE ))
        WRITE(IOF,IOFORM)NLINE(1:NC)
      END IF
C
C ----- DETECTOR DATA
C
      IREC=MOUT(12)-1
C
C ----- CHECK FOR INACTIVE DETECTORS
C
      NINACT=0
      JREC = 1
      DO 4900 I=1,NDET
      IF(DIAMON)THEN
        READ(NSFIO,'(A2,2I1,I5,I3,A2,I3,F4.1,I2)',REC=IREC+JREC)
     1       DETLEG,IT4S(I),IT5S(I),IDFFS(I),ILENS(I),NLINES(I),
     2       DETDLY(I),DETEXT(I),DETCLK(I)
        JREC=JREC+1
        IT3S(I)=0
        DETLEGU=DETLEG
        CALL  TOUPR   ( DETLEGU )
        IF(DETLEGU.EQ.'IR')IT3S(I)=0
        IF(DETLEGU.EQ.' 1')IT3S(I)=1
        IF(DETLEGU.EQ.' 2')IT3S(I)=2
        IF(DETLEGU.EQ.' 3')IT3S(I)=3
        IF(DETLEGU.EQ.' 4')IT3S(I)=4
        IF(DETLEGU.EQ.' 5')IT3S(I)=5
        IF(DETLEGU.EQ.' 6')IT3S(I)=6
        IF(DETLEGU.EQ.'IL')IT3S(I)=7
        NLINESU=NLINES(I)
        CALL  TOUPR   ( NLINESU )
        IF(NLINESU.EQ.'IN')NINACT=NINACT+1
      ELSE
        READ(NSFIO,'(3I1,I5,I3,A2,I3,F4.1,I2)',REC=IREC+JREC)
     1       IT3S(I),IT4S(I),IT5S(I),IDFFS(I),ILENS(I),NLINES(I),
     2       DETDLY(I),DETEXT(I),DETCLK(I)
        JREC=JREC+1
        NLINESU=NLINES(I)
        CALL  TOUPR   ( NLINESU )
        IF(NLINESU.EQ.'IN')NINACT=NINACT+1
        IF(NLINESU.EQ.'CL')THEN
          IF ( ( DETCLK(I) . LT . 1   ) . OR .
     *         ( DETCLK(I) . GT . LDC ) )        THEN
            TAG='NUL'
            WRITE(ERRMSG,885) I,DETCLK(I),LDC
            CALL  PRTERR  ( ERRMSG )
            GO TO 9990
          END IF
          DO 4800 J=1,DETCLK(I),5
            READ(NSFIO,'(5(A8,2I4))',REC=IREC+JREC)
     *        (DETCLN(K,I),DETCLL(K,I),DETCLU(K,I),
     *        K=J,MIN0(DETCLK(I),J+4))
            JREC=JREC+1
 4800     CONTINUE
        END IF
      END IF
 4900 CONTINUE
      NDET1=NDET-NINACT
      WRITE(IOF,FMT4)NDET1
      DO 5000 I=1,NDET
C
C ----- <IT3> IS APPROACH NUMBER
C ----- <IT4> IS NUMBER OF FIRST LANE COVERED
C ----- <IT5> IS HOW MANY LANES COVERED
C ----- <IDFF> DETECTOR OFFSET FROM NOMINAL LANE TERMINAL (+ IS TOWARD CENTER)
C ----- <ILEN> DETECTOR LENGTH (FROM <IDFF>, IN UPSTREAM DIRECTION)
C
      IF(JCTDIA)THEN
        WRITE(NLINE,FMT2)NTDDET(I)
      ELSE
        NLINESU=NLINES(I)
        CALL  TOUPR   ( NLINESU )
        IF(NLINESU.EQ.'IN')GO TO 5000
        WRITE(NLINE,FMT2)I
      END IF
      NLINESU=NLINES(I)
      CALL  TOUPR   ( NLINESU )
      IF(NLINESU.EQ.'CL')THEN
        NLINE(4:11)='CLASSIFY'
      ELSE IF(NLINESU.EQ.'PR')THEN
        NLINE(4:11)='PRESENCE'
      ELSE
        NLINE(4:11)='PULSE'
      END IF
C
C ----- LEG NUMBER
C
      IT3=IT3S(I)
      IDFF=LLENG(IT3)+IDFFS(I)
      ILEN=IDFF-ILENS(I)
C
C ----- CHANGE TO INBOUND APPROACH NUMBER
C ----- FOR NON-DIAMOND, LEG AND INBOUND APPROACH ARE THE SAME
C
      IF(DIAMON)IT3=LIAD(IT3)
      IT5=IT5S(I)
      IT4=IT4S(I)
      WRITE(IOF,'(A,1X,13I4)')NLINE(1:11),ILEN,IDFF,IT3,IT5,
     1     (J,J=IT4,IT4+IT5-1),DETCLK(I),DETDLY(I),NINT( DETEXT(I)*10 )
      IF(NLINE(4:11).EQ.'CLASSIFY')THEN
        WRITE(IOF,'(5(A8,2I4))')(DETCLN(J,I),DETCLL(J,I),DETCLU(J,I),
     *       J=1,DETCLK(I))
      END IF
 5000 CONTINUE
 8000 CONTINUE
C
C-----PROCEESS VMS MESSAGES
C
      LSTREC = MOUT(15)
C-----|UNCONTRL|YIELD|STOP|ALL-STOP|PRETIMED|SEMI-ACT|FULL-ACT|NEMA|TEX-DIA|HARDWARE|
      IF(JCUNCT)LSTREC=MOUT( 6)+3
      IF(JCYELD)LSTREC=MOUT( 6)+3
      IF(JCLTAS)LSTREC=MOUT( 6)+3
      IF(JCAWST)LSTREC=MOUT( 6)+3
      IF(JCPSIG)LSTREC=MOUT(11)+MOUT( 9)-1
      IF(JCSACT)LSTREC=MOUT(14)+MOUT( 9)-1
      IF(JCFACT)LSTREC=MOUT(14)+MOUT( 9)-1
      IF(JCNEMA)LSTREC=MOUT(14)+MOUT( 9)-1
      IF(JCTDIA)LSTREC=MOUT(12)+MOUT(13)-1
      IF(JCHDWR)LSTREC=MOUT(14)+MOUT( 9)-1
      NCARDS=MOUT(15)-LSTREC
      DO 8010 I=1,NCARDS
      READ(NSFIO,IOFORM,REC=LSTREC+I)NLINE
      IF(ILNB( NLINE ).EQ.0)GO TO 8010
      READ(NLINE,501)IVMSMT,IVMSMG,DVMSMP,DVMSST,DVMSAT,IVMSLC,IVMSLP,
     *               IVMSLB,IVMSLE,DVMSPB,DVMSPE,IVMSVN,CVMSDN,DVMSDM,
     *               DVMSDP
      CALL  TOUPR  ( IVMSLC )
      IF(IVMSLC.EQ.'I')THEN
        IF((IVMSLP.LT.1).OR.(IVMSLP.GT.NLEGS))THEN
          TAG='NUL'
          WRITE(ERRMSG,901)IVMSLP,NLEGS
          CALL  PRTERR  ( ERRMSG )
          GO TO 9990
        END IF
        IVMSAP=LIAD(IVMSLP)
      ELSE IF(IVMSLC.EQ.'L')THEN
        IVMSAP=LIAD(NLGP1)
      ELSE IF(IVMSLC.EQ.'O')THEN
        IF((IVMSLP.LT.1).OR.(IVMSLP.GT.NLEGS))THEN
          TAG='NUL'
          WRITE(ERRMSG,901)IVMSLP,NLEGS
          CALL  PRTERR  ( ERRMSG )
          GO TO 9990
        END IF
        IVMSAP=LOAD(IVMSLP)
      ELSE IF(IVMSLC.EQ.'P')THEN
        IVMSAP=-IVMSLP
      ELSE IF(IVMSLC.EQ.'R')THEN
        IVMSAP=LIAD(0)
      ELSE
        TAG='NUL'
        WRITE(ERRMSG,902)IVMSLC
        CALL  PRTERR  ( ERRMSG )
        GO TO 9990
      END IF
      WRITE(IOF,502) IVMSMT,IVMSMG,DVMSMP,DVMSST,DVMSAT,IVMSAP,
     *               IVMSLB,IVMSLE,DVMSPB,DVMSPE,IVMSVN,CVMSDN,DVMSDM,
     *               DVMSDP
 8010 CONTINUE
      ENDFILE(IOF)
      CLOSE(NSFIO,STATUS=STAT(1:NCSTAT))
C
C ----- REOPEN UNIT <NSFIO> WITH DEFAULT NAME FOR POINTER
C ----- TO FILE CONVERTED TO C1
C
      CALL OPENDF(NSFIO,TEXFN,80,ERR)
      IF(ERR)THEN
        CALL OPNERR(TEXFN)
        GO TO 9510
      END IF
      INQUIRE(IOF,NAME=FNAME)
      NCFN=ILNB( FNAME )
      WRITE(NSFIO,IOFORM,REC=1)' #%&$FILESIMC1    ='//FNAME(1:NCFN)
      WRITE(NSFIO,IOFORM,REC=2)GDFILE
 9000 CONTINUE
      IF(JOF.GT.0)GO TO 9990
      WRITE(*,'(A)')
      CALL SHONAM('Converted Simulation data file is',FNAME,78)
      REWIND(IOF)
      NLINE=' '
      READ(IOF,'(A)',END=9500)NLINE
      GO TO 9550
 9500 CONTINUE
      NCCF=ILNB( CFILEP )
      WRITE(*,'(3A/)')'Simulation data file "',CFILEP(1:NCCF),
     1                '" is empty.'
 9510 CONTINUE
      GO TO 9999
 9550 CONTINUE
      WRITE(*,'(A,/,A)')'Simulation converted data title text:',NLINE
      WRITE(*,IOFORM)' '
 9990 CONTINUE
      REWIND(IOF)
 9999 CONTINUE
      IF(PAUSND.EQ.'YES')THEN
        CALL EXIT ( 0 )
      END IF
      RETURN
      END
C
C
C
      FUNCTION BLUNLT(NLINE,IT1,IT2)
C ----- THIS LOGICAL FUNCTION CHECKS CONSECUTIVE 2-CHARACTER FIELDS
C ----- IN <NLINE> TO MAKE SURE THAT THEY CONTAIN ONLY "UN" OR
C ----- ARE BLANK, AND SETS <BLUNLT> TO .TRUE. IF SO. OTHERWISE, TO
C ----- .FALSE.  <IT1> IS THE CHARACTER POSITION OF THE START OF
C ----- THE FIRST FIELD TO BE CHECKED, <IT2> OF THE LAST FIELD.
 
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  BLUNLT
      CHARACTER*(*)   NLINE
      INTEGER               IT1,IT2
      CHARACTER*1 IBL
      PARAMETER  (IBL=' ')
      CHARACTER*80 NLINEU
      INTEGER  I
      IF(IT2.LT.IT1)GO TO 9000
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(NLINEU(IT1:IT2+1).EQ.IBL)GO TO 9000
      DO 100 I=IT1,IT2,2
      IF(NLINEU(I:I+1).EQ.IBL)GO TO 100
      IF(NLINEU(I:I+1).EQ.'UN')GO TO 100
      GO TO 9900
  100 CONTINUE
 9000 BLUNLT=.TRUE.
      GO TO 9990
 9900 BLUNLT=.FALSE.
 9990 RETURN
      END
C
C
C
      SUBROUTINE CSLV(SSC,NC,B,C,L,S,R)
C ----- THIS SUBROUTINE CHECKS THE CHARACTER VARIABLE <SSC> TO SEE
C ----- IF IT IS BLANK OR CONTAINS CHARACTER(S) "C", "L", "S" OR
C ----- "R". THIS IS RETURNED IN THE LOGICAL VARIABLES <B>, <C>,
C ----- <L>, <S>, OR <R>. THE NUMBER OF CHARACTERS IS RETURNED IN
C ----- THE INTEGER VARIABLE <NC>.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*2     SSC
      INTEGER             NC
      LOGICAL                B,C,L,S,R
      CHARACTER*2 SSCU
      INTEGER     ILNB
      B=.FALSE.
      C=.FALSE.
      L=.FALSE.
      S=.FALSE.
      R=.FALSE.
      NC=ILNB( SSC )
      IF(NC.EQ.0)THEN
        B=.TRUE.
        GO TO 9990
      END IF
      SSCU=SSC
      CALL  TOUPR   ( SSCU )
      IF(INDEX(SSCU(1:NC),'C').GT.0)THEN
        C=.TRUE.
        IF(NC.EQ.1)GO TO 9990
      END IF
      IF(INDEX(SSCU(1:NC),'L').GT.0)THEN
        L=.TRUE.
        IF(NC.EQ.1)GO TO 9990
      END IF
      IF(INDEX(SSCU(1:NC),'S').GT.0)S=.TRUE.
      IF(INDEX(SSCU(1:NC),'R').GT.0)R=.TRUE.
 9990 CONTINUE
      RETURN
      END
C
C
C
      FUNCTION EXCLT(TC)
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  EXCLT
      CHARACTER*(*)  TC
C
C ----- TC - CHARACTER VARIABLE WITH TURN CODES("U" "L" "S" "R")
C ----- EXCLT - SET TO .TRUE. IF <TC> HAS ONLY "L", OTHERWISE .FALSE.
C
      CHARACTER*1 TCU
      INTEGER     ILNB
      IF(ILNB( TC ).GT.1)GO TO 1000
      TCU=TC
      CALL  TOUPR   ( TCU )
      IF(INDEX(TCU,'L').EQ.0)GO TO 1000
      EXCLT=.TRUE.
      GO TO 9990
 1000 CONTINUE
      EXCLT=.FALSE.
 9990 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE LNCNTO(K,NLNSOB,TLINE)
      IMPLICIT  NONE                                                    CCODE=C.
      INTEGER K,NLNSOB,K2
      CHARACTER*(*) TLINE
C
C ----- LANE CONTROL FOR OUTBOUND LANES
C
      K2=K+NLNSOB-1
      IF(K2.GE.K)THEN
        TLINE(K:K2)='11111111'
        K=K2+1
      END IF
      RETURN
      END
C
C
C
      SUBROUTINE LNCNTI(K,K1,NLINEU,TLINE)
      IMPLICIT  NONE                                                    CCODE=C.
      INTEGER K,K1
      CHARACTER*(*)NLINEU,TLINE
C
C ----- LANE CONTROL FOR INBOUND LANES
C
      IF(NLINEU(K1:K1+1).EQ.'SI')THEN
        TLINE(K:K)='5'
        GO TO 100
      END IF
      IF(NLINEU(K1:K1).EQ.'R')THEN
        TLINE(K:K)='7'
        GO TO 100
      END IF
      IF(NLINEU(K1:K1).EQ.'B')THEN
        TLINE(K:K)='1'
        GO TO 100
      END IF
      IF(NLINEU(K1:K1).EQ.'U')THEN
        TLINE(K:K)='2'
        GO TO 100
      END IF
      IF(NLINEU(K1:K1).EQ.'Y')THEN
        TLINE(K:K)='3'
        GO TO 100
      END IF
      IF(NLINEU(K1:K1+1).EQ.'ST')THEN
        TLINE(K:K)='4'
        GO TO 100
      END IF
      IF(NLINEU(K1:K1).EQ.'L')TLINE(K:K)='6'
  100 CONTINUE
      K=K+1
      RETURN
      END
C
C
C
      SUBROUTINE PRTERR ( MESAGE )
      IMPLICIT NONE                                                     CCODE=C.
      CHARACTER*(*)     MESAGE
      INTEGER           ILNB
      INTEGER           NCMES
  601 FORMAT(A)
      NCMES = MAX0( ILNB( MESAGE ),1 )
      OPEN  (90,FILE='error.txt',ACCESS='APPEND',STATUS='UNKNOWN')
      WRITE (90,601) MESAGE(1:NCMES)
      ENDFILE 90
      CLOSE (90,STATUS='KEEP')
      WRITE (*  ,601)
      WRITE (*  ,601) MESAGE(1:NCMES)
      RETURN
      END                                                               PRTERR
