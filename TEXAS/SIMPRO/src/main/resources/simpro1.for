      PROGRAM SIMPRO
C\   *     (TAPE5=65,OUTPUT=513,TAPE7=65,TAPE8=65,TAPE9=65
C\   *     ,POSDAT=513,TAPE99=65,TTY=65,TAPE6=OUTPUT)
C           I        L          STA      T8       T9
C           PVA        ERR                           PRE      C
C           SYSDAT
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
      INCLUDE 'ABIAS'
      INCLUDE 'ANIMAT'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'CONCHK'
      INCLUDE 'CONFLT'
      INCLUDE 'CONSTN'
      INCLUDE 'CURAND'
      INCLUDE 'CWDDIR'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INCLUDE 'LOOPS'
      INCLUDE 'PATH'
      INCLUDE 'PHASES'
C6    INCLUDE 'PRTPVA'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SDR'
      INCLUDE 'SIGCAM'
      INCLUDE 'SUMST2'
C8    INCLUDE 'TAPE10'
C=    INCLUDE 'TESTER'
      INCLUDE 'TITLE'
      INCLUDE 'TXDSIG'
      INCLUDE 'USER'
      INCLUDE 'USFILE'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
  601 FORMAT(43HSimulation Processor for the TEXAS Traffic ,
     *       20HSimulation Package (,A5,1H),/,
     *       29HSIMPRO Copyright (C) 1989 by ,
     *       33HThe University of Texas at Austin,/)
  602 FORMAT(A)
C
C----- UNIT#  RW PARVAL FILNAM      USAGE
C----- 1=NGD   R                    GDVS0 FILE FOR GDVS00
C----- 1=IPP   W                    POS FOR C6 POS/VEL/ACC VS TIME PLOT
C----- 2=IPV   W                    VEL FOR C6 POS/VEL/ACC VS TIME PLOT
C----- 3=IPA   W                    ACC FOR C6 POS/VEL/ACC VS TIME PLOT
C----- 4=PPP   W                    C7 = PAGE PLOT OF POSITION
C----- 5=INPUT R IFILE  SIM         SIMPRO STANDARD INPUT
C----- 6       W LFILE  SIMPLST.TXT SIMPRO STANDARD OUTPUT
C----- 6=NIO   W                    TERMINAL OUTPUT (NOT USED)
C----- 7=ICS   W STAFIL SIMSTAT     SIMPRO COMPRESSED STATISTICS
C----- 8=IGEOP R T8FILE FORT8       GEOPRO INPUT FILE TO SIMPRO
C----- 9=IVEHP R T9FILE FORT9       DVPRO  INPUT FILE TO SIMPRO
C-----10=IQD   W                    C8 = WRITE DISCHARGE HEADWAYS, QUEUE
C-----                                   AND STOP DELAY, ORIGINAL QUEUE
C-----                                   POSITION, AND SIGNAL INFO
C-----11=IDH   W                    C+ = DISCHARGE HEADWAYS
C-----20=NPD   W PVAFIL POSDAT      SIMPRO ANIMATION/POLLUTION FILE
C-----22=NGP   R                    OPNSIF FILE FOR GEOPRO
C-----23=NDV   R                    OPNSIF FILE FOR DVPRO
C-----26=NSP   R                    OPNSIF FILE FOR SIMPRO
C-----30=ISS   W SSAM   SSAM.TRJ    SURROGATE SAFETY ASSESSMENT METHODOLOGY
C-----33=TC3   W                    C; = TEMPORARY TEST CODE FOR ***CON
C-----44=NWV   W                    WRITE VEHICLE ON LOGOUT
C-----66=TC6   W                    C; = TEMPORARY TEST CODE FOR CARFOL
C-----77=DET   W                    DETECTORS
C-----90=NER   W        ERROR.TXT   ERROR.TXT FILE
C-----91=NWR   W        WARNING.TXT WARNING.TXT FILE
C-----98=NVD   R VEHDAT             OPNSRO FILE FOR VEHDAT
C-----99=SER   W ERRFIL SIMERR.TXT  SIMPRO ERROR FILE
C
C-----READ      ERROR STOP NUMBERS ARE 712-782
C-----DIMENSION ERROR STOP NUMBERS ARE 783-799
C-----READ      ERROR STOP NUMBERS ARE 801-899
C-----EXECUTION ERROR STOP NUMBERS ARE 901-979
C
C-----FOR SYMBOLIC DEBUGGING, EVERY SUBROUTINE SHOULD INCLUDE THE
C-----FOLLOWING COMMON BLOCKS (EXCEPT SUMMARY AND CALLED BY SUMMARY):
C-----CLASS     VEHICLE PROPERTIES
C-----INDEX     IA, IL, IP, IV, ETC.
C-----QUE       IQ
C-----USER      TIME, ETC.
C
C-----INITIALIZE CONSTANTS
      CALL  INITCN
      WRITE (*,601) IVERSN
C-----INITIALIZE THE PARAMETERS FOR THE SIMULATION
      CALL  INITAL
C-----RUN THE SIMULATION
      CALL  EXEC
C-----PRINT THE SUMMARY STATISTICS
      CALL  SUMARY
      WRITE (*,602) 'TEXAS Model traffic simulation processing is '//
     *              'complete.'
      IF ( PAUSND . EQ . 'YES' )                 THEN
        CALL EXIT ( 0 )
      END IF
      END                                                               SIMPRO
C
C
C
      BLOCK DATA
C
C-----USER DEFINED BLOCK DATA
C
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'ANIMAT'
      INCLUDE 'APPRO'
      INCLUDE 'ARC'
      INCLUDE 'CHARAC'
      INCLUDE 'CID'
      INCLUDE 'CLASS'
      INCLUDE 'CONCHK'
      INCLUDE 'CONFLT'
      INCLUDE 'CURAND'
      INCLUDE 'CWDDIR'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'LINE'
      INCLUDE 'LOOPS'
      INCLUDE 'PATH'
      INCLUDE 'PHASES'
C6    INCLUDE 'PRTPVA'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SDR'
      INCLUDE 'SIGCAM'
      INCLUDE 'SUMST2'
C8    INCLUDE 'TAPE10'
C=    INCLUDE 'TESTER'
      INCLUDE 'TITLE'
      INCLUDE 'TXDSIG'
      INCLUDE 'USER'
      INCLUDE 'USFILE'
C;    COMMON / TWRD   / DVILNI(NAL,NAP),FVILNI(NAL,NAP),GVILNI(NAL,NAP),
C;   *                  HVILNI(NAL,NAP),TVILNI(NAL,NAP)
C;    DOUBLE PRECISION  DVILNI         ,FVILNI         ,GVILNI         ,
C;   *                  HVILNI         ,TVILNI
C;    COMMON / TWR4   / KVILNI(NAL,NAP)
C;    INTEGER*4         KVILNI
      INTEGER           LDCNLS,MIANAP,MNUNVC,MPNNPN,NALNAP,NALNLS,
     *                  NCOT2 ,NCPNPA,NIANAL,NILIBS,NILITU,NILTCM,
     *                  NISNAL,NISNTS,NLAT4 ,NLONLA,NLPNLA,NPCNPC,
     *                  NPHNON,NPHNOV,NPLNPN,NRGNGR,NRNBNP,NSISET,
     *                  NSSNSR,NVEP2 ,RGGRPN
      PARAMETER       ( LDCNLS = LDC*NLS             )
      PARAMETER       ( MIANAP = (NIA-1)*NAP         )
      PARAMETER       ( MNUNVC = MNU*NVC             )
      PARAMETER       ( MPNNPN = (NPN-1)*NPN         )
      PARAMETER       ( NALNAP = NAL*NAP             )
      PARAMETER       ( NALNLS = NAL*NLS             )
      PARAMETER       ( NCOT2  = NCO*2               )
      PARAMETER       ( NCPNPA = NCP*NPA             )
      PARAMETER       ( NIANAL = NIA*NAL             )
      PARAMETER       ( NILIBS = NIL*IBUFFS          )
      PARAMETER       ( NILITU = NIL*ITURNR          )
      PARAMETER       ( NILTCM = NIL*ITURNR*NCM      )
      PARAMETER       ( NISNAL = NIS*NAL             )
      PARAMETER       ( NISNTS = NIS*NTS             )
      PARAMETER       ( NLAT4  = NLA*4               )
      PARAMETER       ( NLONLA = NLO*NLA             )
      PARAMETER       ( NLPNLA = NLP*NLA             )
      PARAMETER       ( NPCNPC = NPC*NPC             )
      PARAMETER       ( NPHNON = NPH*NON             )
      PARAMETER       ( NPHNOV = NPH*NOV             )
      PARAMETER       ( NPLNPN = NPL*NPN             )
      PARAMETER       ( NRGNGR = NRG*NGR             )
      PARAMETER       ( RGGRPN = NRG*NGR*NPN         )
      PARAMETER       ( NRNBNP = NRGNGR*NPH          )
      PARAMETER       ( NSISET = (NCM+2+NON+NON)*NIL )
      PARAMETER       ( NSSNSR = NSS*NSR             )
      PARAMETER       ( NVEP2  = NVE+2               )
      INTEGER           I
C-----COMMON / ABIAS  /
      DATA     ACCNEW         /         0.0D0                  /
      DATA     ACCOLD         /         0.0D0                  /
      DATA     DESVEL         /         0.0D0                  /
      DATA     ENDLN          /         0.0D0                  /
      DATA     LENVAP         /         0.0D0                  /
      DATA     OLDDTI         /         0.0D0                  /
      DATA     OLDDTS         /         0.0D0                  /
      DATA     POSNEW         /         0.0D0                  /
      DATA     POSOLD         /         0.0D0                  /
      DATA     PVACC          /         0.0D0                  /
      DATA     PVPOS          /         0.0D0                  /
      DATA     PVSLP          /         0.0D0                  /
      DATA     PVVEL          /         0.0D0                  /
      DATA     RELEND         /         0.0D0                  /
      DATA     RELPOS         /         0.0D0                  /
      DATA     RELVEL         /         0.0D0                  /
      DATA     SLPBLK         /         0.0D0                  /
      DATA     SLPCON         /         0.0D0                  /
      DATA     SLPMAX         /         6.0D0                  /
      DATA     SLPNEW         /         0.0D0                  /
      DATA     SLPNOF         /         0.0D0                  /
      DATA     SLPOLD         /         0.0D0                  /
      DATA     VELNEW         /         0.0D0                  /
      DATA     VELOLD         /         0.0D0                  /
      DATA     XREL           /         0.0D0                  /
      DATA     XRELMI         /         3.0D0                  /
      DATA     XRELMX         /         8.0D0                  /
C-----COMMON / ANIMAT /
      DATA     LLANER         / NIL   * 0                      /
C-----COMMON / APPROC /
      DATA     IAFLAG         / 3     *'R',
     *                          2     *'I',
     *                          3     *'L',
     *                          3     *'R',
     *                          2     *'I',
     *                          3     *'L'                     /
C-----COMMON / APPROI /
      DATA     AAVT           / NAP   * 0                      /
      DATA     DZB2BG         / NAP   * 0                      /
      DATA     DZB2BS         / NAP   * 0                      /
      DATA     DZE2BG         / NAP   * 0                      /
      DATA     DZE2BS         / NAP   * 0                      /
      DATA     DZS2EG         / NAP   * 0                      /
      DATA     DZS2ES         / NAP   * 0                      /
      DATA     IALEFT         / NAP   * 0                      /
      DATA     IATYPE         / NAP   * 0                      /
      DATA     INTLNK         / 8     * 0,
     *                          3     *13,
     *                                  5,
     *                                  4,
     *                          3     *12                      /
      DATA     INTLNU         /    NAP* 0                      /
      DATA     ISDRA          / MIANAP* 0                      /
      DATA     ISDRN          / MIANAP* 0                      /
      DATA     ISLIM          /    NAP* 0                      /
      DATA     KVILAI         /         0                      /
      DATA     LLANES         / NALNAP* 0                      /
      DATA     LVILAI         / NALNAP* 0                      /
      DATA     LVILNI         / NALNAP* 0                      /
      DATA     MAXLLN         /    NAP* 0                      /
      DATA     MINLLN         /    NAP* 0                      /
      DATA     MVILNI         / NALNAP* 0                      /
      DATA     NIBLAI         /         0                      /
      DATA     NLANES         /    NAP* 0                      /
      DATA     NSDR           /    NAP* 0                      /
      DATA     NVIL           / NALNAP* 0                      /
      DATA     NVILAI         / NALNAP* 0                      /
      DATA     NVILNI         / NALNAP* 0                      /
C-----COMMON / APSSAM /
      DATA     IAAZIM         / NAP   * 0                      /
      DATA     IAPX           / NAP   * 0                      /
      DATA     IAPY           / NAP   * 0                      /
      DATA     IARGHT         / NAP   * 0                      /
C-----COMMON / ARCI   /
      DATA     IARCAZ         / NAR   * 0                      /
      DATA     IARCR          / NAR   * 0                      /
      DATA     IARCSW         / NAR   * 0                      /
      DATA     IARCX          / NAR   * 0                      /
      DATA     IARCY          / NAR   * 0                      /
C-----COMMON / CHARAC /
      DATA     IANI           / 'ANI'                          /
      DATA     IBLNK1         / ' '                            /
      DATA     ICLAS          / 'CLASSIFY'                     /
      DATA     ILETTA         / 'A'                            /
      DATA     ILETTI         / 'I'                            /
      DATA     ILETTL         / 'L'                            /
      DATA     ILETTN         / 'N'                            /
      DATA     ILETTR         / 'R'                            /
      DATA     ILETTS         / 'S'                            /
      DATA     ILETTU         / 'U'                            /
      DATA     ILETTW         / 'W'                            /
      DATA     ILETTX         / 'X'                            /
      DATA     INO            / 'NO'                           /
      DATA     IOFF           / 'OFF'                          /
      DATA     ION            / 'ON'                           /
      DATA     IPOL           / 'POL'                          /
      DATA     IPRES          / 'PRESENCE'                     /
      DATA     IPULS          / 'PULSE'                        /
      DATA     IUNCLL         / 'UNCLAS-L'                     /
      DATA     IUNCLU         / 'UNCLAS-U'                     /
      DATA     IYES           / 'YES'                          /
      DATA     JAND           / 'AND'                          /
      DATA     JOR            / 'OR'                           /
      DATA     NLINE          / ' '                            /
C-----COMMON / CIDB   /
      DATA     BIT            /      Z'01',
     *                               Z'02',
     *                               Z'04',
     *                               Z'08',
     *                               Z'10',
     *                               Z'20',
     *                               Z'40',
     *                               Z'80'                     /
      DATA     CIDIN          / 73    * 0                      /
      DATA     CIDINP         / 73    * 0                      /
      DATA     CIDOUP         / 73    * 0                      /
      DATA     CIDOUT         / 73    * 0                      /
C-----COMMON / CIDI   /
      DATA     CIDID          /         0                      /
      DATA     CIDIDS         / 10    * 0                      /
      DATA     LDHDWR         / NLS   * 0                      /
      DATA     NCIDS          /         0                      /
      DATA     SIGO           / NOV   * 0                      /
      DATA     SIGOP          / NOV   * 0                      /
      DATA     SIGP           / NPH   * 0                      /
      DATA     SIGPP          / NPH   * 0                      /
      DATA     SIGV           / NPH   * 0                      /
      DATA     SIGVP          / NPH   * 0                      /
      DATA     TICKDL         /         0                      /
      DATA     TICKDT         /         0                      /
      DATA     TICKNX         /         0                      /
      DATA     TICKST         /         0                      /
C-----COMMON / CIDL   /
      DATA     CIDOVL         / NOV   *.FALSE.                 /
      DATA     CIDPED         / NPH   *.FALSE.                 /
      DATA     CIDPH          / NPH   *.FALSE.                 /
C-----COMMON / CIDP   /
      DATA     CloseOnlineCIDsPfn      / 0                     /
      DATA     GetOnlineCIDsPfn        / 0                     /
      DATA     GetPhasePfn             / 0                     /
      DATA     hDLL                    / 0                     /
      DATA     ReadCIDPfn              / 0                     /
      DATA     WriteCIDPfn             / 0                     /
C-----COMMON / CLASSC /
      DATA     CLASSV         / NVC   *'        '              /
C-----COMMON / CLASSD /
      DATA     AMAX           / NVC   *0.0D0                   /
      DATA     AMAXAV         /        0.0D0                   /
      DATA     DCHAR          / NDC   *0.0D0                   /
      DATA     DCHRMN         /        0.0D0                   /
      DATA     DCHRMX         /        0.0D0                   /
      DATA     DMAX           / NVC   *0.0D0                   /
      DATA     DMAXAV         /        0.0D0                   /
      DATA     PIJR           / NDC   *0.0D0                   /
      DATA     VCHAR          / NVC   *0.0D0                   /
      DATA     VMAX           / NVC   *0.0D0                   /
C-----COMMON / CLASSI /
      DATA     HEIGHT         /    NVC* 0                      /
      DATA     IPIJR          /    NDC* 0                      /
      DATA     IRMIN          /    NVC* 0                      /
      DATA     LENMAX         /         0                      /
      DATA     LENV           /    NVC* 0                      /
      DATA     NDRICL         /         0                      /
      DATA     NUNITS         /    NVC* 0                      /
      DATA     NVEHAT         /         6                      /
      DATA     NVEHCL         /         0                      /
      DATA     UDRWSQ         / MNUNVC* 0                      /
      DATA     UFPD           / MNUNVC* 0                      /
      DATA     ULEN           / MNUNVC* 0                      /
      DATA     URHPD          / MNUNVC* 0                      /
      DATA     URPD           / MNUNVC* 0                      /
      DATA     UTRLEN         / MNUNVC* 0                      /
      DATA     UTRWID         / MNUNVC* 0                      /
      DATA     UWID           / MNUNVC* 0                      /
      DATA     WIDMAX         /         0                      /
      DATA     WIDV           /    NVC* 0                      /
C-----COMMON / CONCHD /
      DATA     AO             /         0.0D0                  /
      DATA     AONOF          /         0.0D0                  /
      DATA     P              /         0.0D0                  /
      DATA     PO             /         0.0D0                  /
      DATA     PONOF          /         0.0D0                  /
      DATA     SO             /         0.0D0                  /
      DATA     SONOF          /         0.0D0                  /
      DATA     VO             /         0.0D0                  /
      DATA     VONOF          /         0.0D0                  /
C-----COMMON / CONCHI /
      DATA     IVPRTV         /         0                      /
      DATA     JDCONF         /         0                      /
      DATA     JDNOF          /         0                      /
      DATA     JSLIM          /         0                      /
      DATA     JSPD           /         0                      /
      DATA     JSPDP          /         0                      /
      DATA     JVCONF         /         0                      /
      DATA     JVNOF          /         0                      /
      DATA     JVPRTV         /         0                      /
      DATA     LGEOM4         /         0                      /
      DATA     MIMP           /         0                      /
      DATA     NOFSPD         /         0                      /
C-----COMMON / CONFLT /
      DATA     ICONA          / NCOT2 * 0                      /
      DATA     ICONAN         / NCO   * 0                      /
      DATA     ICOND          / NCOT2 * 0                      /
      DATA     ICONI          / NCOT2 * 0                      /
      DATA     ICONP          / NCOT2 * 0                      /
      DATA     ICONV          / NCOT2 * 0                      /
C-----COMMON / CURANI /
      DATA     IASRAN         / 55    * 0                      /
      DATA     ISTART         /         0                      /
      DATA     ISEED          /         0                      /
      DATA     NEXTRN         /         0                      /
C-----COMMON / CURANL /
      DATA     NEWSED         / .FALSE.                        /
C-----COMMON / CWDDIR /
      DATA     CWDDIR         / ' '                            /
C-----COMMON / DIAMON /
      DATA     DIAMON         / .FALSE.                        /
C-----COMMON / INDEXC /
      DATA     IPFLAG         / '          '                   /
      DATA     JPFLAG         / '          '                   /
      DATA     KPFLAG         / '          '                   /
C-----COMMON / INDEXI /
      DATA     IA             /         0                      /
      DATA     IAN            /         0                      /
      DATA     IL             /         0                      /
      DATA     ILN            /         0                      /
      DATA     IP             /         0                      /
      DATA     IV             /         0                      /
      DATA     IVN            /         0                      /
      DATA     IVPV           /         0                      /
      DATA     IX             /         0                      /
      DATA     JPRTM          /         0                      /
      DATA     LOGTMP         /         0                      /
C-----COMMON / INTERD /
      DATA     TVATIN         / NIL   * 0.0D0                  /
C-----COMMON / INTERI /
      DATA     DTLAG          /         0                      /
      DATA     DTLAGS         /         0                      /
      DATA     ICONTR         /         0                      /
      DATA     LARCS          / NAR   * 0                      /
      DATA     LIBA           / NIA   * 0                      /
      DATA     LIBAR          / NAP   *-1                      /
      DATA     LLINES         / NLI   * 0                      /
      DATA     LOBA           / NOA   * 0                      /
      DATA     LOBAR          / NAP   *-1                      /
      DATA     LSDRC          / NSR   * 0                      /
      DATA     LVATIN         / NIL   * 0                      /
      DATA     MSS            /        40                      /
      DATA     NAPS           /         0                      /
      DATA     NARCS          /         0                      /
      DATA     NFUT           /         0                      /
      DATA     NIBA           /         0                      /
      DATA     NIBL           /         0                      /
      DATA     NLEGS          /         0                      /
      DATA     NLINES         /         0                      /
      DATA     NLOOPS         /         0                      /
      DATA     NOBA           /         0                      /
      DATA     NOCONF         /         0                      /
      DATA     NPATHS         /         0                      /
      DATA     NRLAN          /         0                      /
      DATA     NSDRC          /         0                      /
      DATA     NUMSDR         /         0                      /
      DATA     NVATIN         /         0                      /
      DATA     NVIA           / NAP   * 0                      /
      DATA     NVIBA          /         0                      /
      DATA     NVIN           /         0                      /
      DATA     NVIP           / NPA   * 0                      /
      DATA     NVMSM          /         0                      /
      DATA     NVOBA          /         0                      /
      DATA     NVSY           /         0                      /
C-----COMMON / INTERL /
      DATA     EVCCON         / .FALSE.                        /
      DATA     SMJCOL         / .FALSE.                        /
C-----COMMON / INTERR /
      DATA     XMAXIN         /         0.0                    /
      DATA     XMININ         /         0.0                    /
      DATA     YMAXIN         /         0.0                    /
      DATA     YMININ         /         0.0                    /
C-----COMMON / LANECD /
      DATA     AVSF           /         0.0D0                  /
      DATA     AVSR           /         0.0D0                  /
      DATA     FACTOR         /         2.0D0                  /
      DATA     PVSF           /         0.0D0                  /
      DATA     PVSR           /         0.0D0                  /
      DATA     SLPLCH         /         0.0D0                  /
      DATA     SVSF           /         0.0D0                  /
      DATA     SVSR           /         0.0D0                  /
      DATA     VVSF           /         0.0D0                  /
      DATA     VVSR           /         0.0D0                  /
C-----COMMON / LANECI /
      DATA     ISIDE          /         0                      /
      DATA     NCQ            /         0                      /
      DATA     NOSF           /         0                      /
      DATA     NOSFS          /         0                      /
      DATA     NOSR           /         0                      /
      DATA     NOSRS          /         0                      /
C-----COMMON / LANEI  /
      DATA     IBLN           /    NLA* 0                      /
      DATA     IFVL           /    NLA* 0                      /
      DATA     ILTYPE         /    NLA* 0                      /
      DATA     ILVL           /    NLA* 0                      /
      DATA     ISNA           /    NLA* 0                      /
      DATA     LAVT           /    NLA* 0                      /
      DATA     LCONTR         /    NLA* 0                      /
      DATA     LGEOM          / NLAT4 * 0                      /
      DATA     LINTP          / NLPNLA* 0                      /
      DATA     LLDL           / NLONLA* 0                      /
      DATA     LTURN          /    NLA* 0                      /
      DATA     LWID           /    NLA* 0                      /
      DATA     NLDL           /    NLA* 0                      /
      DATA     NLL            /    NLA* 0                      /
      DATA     NLR            /    NLA* 0                      /
      DATA     NPINT          /    NLA* 0                      /
C-----COMMON / LANEL  /
      DATA     LMJCOL         /    NLA*.FALSE.                 /
C-----COMMON / LNSSAM /
      DATA     IDX            /    NLA* 0                      /
      DATA     ISNL           /    NLA* 0                      /
C-----COMMON / LINEI  /
      DATA     ILX1           / NLI   * 0                      /
      DATA     ILX2           / NLI   * 0                      /
      DATA     ILY1           / NLI   * 0                      /
      DATA     ILY2           / NLI   * 0                      /
C-----COMMON / LOGICV /
C OLD DATA     LFALSE         /         2                      /
C OLD DATA     LTRUE          /         1                      /
C-----COMMON / LOOPSC /
      DATA     DETCLN         / LDCNLS*'        '              /
      DATA     DETCLS         /    NLS*'        '              /
      DATA     ITYPLD         /    NLS*'PRESENCE'              /
C-----COMMON / LOOPSD /
      DATA     DETACC         /    NLS* 0.0D0                  /
      DATA     DETALV         /    NLS* 0.0D0                  /
      DATA     DETLEN         /    NLS* 0.0D0                  /
      DATA     DETP1          /    NLS* 0.0D0                  /
      DATA     DETP2          /    NLS* 0.0D0                  /
      DATA     DETP3          /    NLS* 0.0D0                  /
      DATA     DETP4          /    NLS* 0.0D0                  /
      DATA     DETSLP         /    NLS* 0.0D0                  /
      DATA     DETSLV         /    NLS* 0.0D0                  /
      DATA     DETTFB         /    NLS* 0.0D0                  /
      DATA     DETTFE         /    NLS* 0.0D0                  /
      DATA     DETTRE         /    NLS* 0.0D0                  /
      DATA     DETVBE         /    NLS* 0.0D0                  /
      DATA     DETVEL         /    NLS* 0.0D0                  /
      DATA     DETVLV         /    NLS* 0.0D0                  /
      DATA     LDDELY         /    NLS* 0.0D0                  /
      DATA     LDEXTD         /    NLS* 0.0D0                  /
      DATA     STOPLD         /    NLS* 0.0D0                  /
      DATA     STRTLD         /    NLS* 0.0D0                  /
C-----COMMON / LOOPSI /
      DATA     DETCLK         /    NLS* 0                      /
      DATA     DETCLL         / LDCNLS* 0                      /
      DATA     DETCLU         / LDCNLS* 0                      /
      DATA     DETIV          /    NLS* 0                      /
      DATA     LDA            /    NLS* 0                      /
      DATA     LDCLEX         /    NLS* 0                      /
      DATA     LDPHCL         /    NLS* 0                      /
      DATA     LDSTAT         /    NLS*-1                      /
      DATA     LDSTOP         /    NLS* 0                      /
      DATA     LDSTRT         /    NLS* 0                      /
      DATA     LLDLN          / NALNLS* 0                      /
      DATA     LLOOPS         /    NLS* 0                      /
      DATA     NLDLN          /    NLS* 0                      /
C-----COMMON / LOOPSL /
      DATA     LDCLER         /    NLS*.FALSE.                 /
      DATA     LDCROS         /    NLS*.FALSE.                 /
      DATA     LDTRIP         /    NLS*.FALSE.                 /
      DATA     VDCNT          /    NLS*.FALSE.                 /
C-----COMMON / PASSAM /
      DATA     IBA1           /    NPA* 0                      /
      DATA     IBA2           /    NPA* 0                      /
      DATA     IDA1           /    NPA* 0                      /
      DATA     IDA2           /    NPA* 0                      /
      DATA     IIA            /    NPA* 0                      /
      DATA     IIL            /    NPA* 0                      /
      DATA     IOA            /    NPA* 0                      /
      DATA     IOL            /    NPA* 0                      /
      DATA     IRA1           /    NPA* 0                      /
      DATA     IRA2           /    NPA* 0                      /
      DATA     IXA1           /    NPA* 0                      /
      DATA     IXA2           /    NPA* 0                      /
      DATA     IXL1           /    NPA* 0                      /
      DATA     IXL2           /    NPA* 0                      /
      DATA     IYA1           /    NPA* 0                      /
      DATA     IYA2           /    NPA* 0                      /
      DATA     IYL1           /    NPA* 0                      /
      DATA     IYL2           /    NPA* 0                      /
      DATA     JXL1           /    NPA* 0                      /
      DATA     JXL2           /    NPA* 0                      /
      DATA     JYL1           /    NPA* 0                      /
      DATA     JYL2           /    NPA* 0                      /
      DATA     LA1            /    NPA* 0                      /
      DATA     LA2            /    NPA* 0                      /
      DATA     LL1            /    NPA* 0                      /
      DATA     LL2            /    NPA* 0                      /
C-----COMMON / PATHI  /
      DATA     ICPSET         / NCPNPA* 0                      /
      DATA     IFVP           /    NPA* 0                      /
      DATA     IGEOCP         / NCPNPA* 0                      /
      DATA     ILCH           /    NPA* 0                      /
      DATA     ILVP           /    NPA* 0                      /
      DATA     IOPT           /    NPA* 0                      /
      DATA     IPT            /    NPA* 0                      /
      DATA     LENP           /    NPA* 0                      /
      DATA     LENPT          /    NPA* 0                      /
      DATA     LIBL           /    NPA* 0                      /
      DATA     LIMP           /    NPA* 0                      /
      DATA     LOBL           /    NPA* 0                      /
      DATA     NCPSET         /    NPA* 0                      /
      DATA     NGEOCP         /    NPA* 0                      /
      DATA     PAVT           /    NPA* 0                      /
      DATA     RADMAX         /    NPA* 0                      /
      DATA     RADMIN         /    NPA* 0                      /
C-----         ITURNU     1 ITURN CODE - U-TURN
C-----         ITURNL     2 ITURN CODE - LEFT TURN
C-----         ITURNS     3 ITURN CODE - STRAIGHT
C-----         ITURNR     4 ITURN CODE - RIGHT TURN
C-----         LTURNR     1 LTURN, IPT, AND IPTURN CODE - RIGHT TURN
C-----         LTURNS     2 LTURN, IPT, AND IPTURN CODE - STRAIGHT
C-----         LTURNL     4 LTURN, IPT, AND IPTURN CODE - LEFT TURN
C-----         LTURNU     8 LTURN, IPT, AND IPTURN CODE - U-TURN
C-----         RITURN IS INDEXED BY IPT AND RETURNS THE ITURN CODE
C-----         IPT  (IP) = R=1 S=2 L=4 U=8
C-----         ITURN(IV) = R=4 S=3 L=2 U=1
      DATA     RITURN         /         ITURNR,
     *                                  ITURNS,
     *                                  0     ,
     *                                  ITURNL,
     *                                  0     ,
     *                                  0     ,
     *                                  0     ,
     *                                  ITURNU                 /
C-----COMMON / PATHL  /
      DATA     PMJCOL         /    NPA*.FALSE.                 /
C-----COMMON / PHASEC /
      DATA     IANDOR         /    NPN*'OR '                   /
      DATA     ICNDSV         /    NPN*'   '                   /
      DATA     IDUALL         /    NPN*'NO '                   /
      DATA     IMINOR         /    NPN*'NO '                   /
      DATA     IPRCL          /    NPN*'   '                   /
      DATA     IPRCY          /    NPN*'   '                   /
      DATA     IREC           /    NPN*'OFF'                   /
      DATA     ISKP           /    NPN*'OFF'                   /
      DATA     PEDDIS         /    NPN*'       '               /
C-----COMMON / PHASED /
      DATA     DZBTIM         /         0.0D0                  /
      DATA     DZETIM         /         0.0D0                  /
      DATA     EOM            / NRG   *TIMERR                  /
      DATA     PEDPAR         /    NPN* 0.0D0                  /
      DATA     PEDTIM         /    NPN* 0.0D0                  /
      DATA     PEDTMN         /    NPN* 0.0D0                  /
      DATA     T2S            /    NPN* 0.0D0                  /
      DATA     TAIMID         /    NPN* 0.0D0                  /
      DATA     TAR            /    NPN* 0.0D0                  /
      DATA     TCI            /    NPN* 0.0D0                  /
      DATA     TGAPPC         / NPC   * 0.0D0                  /
      DATA     TGAPPH         /    NPN* 0.0D0                  /
      DATA     TGRMID         /    NPN* 0.0D0                  /
      DATA     TII            /    NPN* 0.0D0                  /
      DATA     TIIADD         /    NPN* 0.0D0                  /
      DATA     TIIMAX         /    NPN* 0.0D0                  /
      DATA     TIIVEH         /    NPN* 0.0D0                  /
      DATA     TMAXPC         / NPC   * 0.0D0                  /
      DATA     TMAXPH         /    NPN* 0.0D0                  /
      DATA     TMI            /    NPN* 0.0D0                  /
      DATA     TMX            /    NPN* 0.0D0                  /
      DATA     TM1            /    NPN* 0.0D0                  /
      DATA     TM2            /    NPN* 0.0D0                  /
      DATA     TPC            /    NPN* 0.0D0                  /
      DATA     TPDEXC         /    NPN* 0.0D0                  /
      DATA     TRR            /    NPN* 0.0D0                  /
      DATA     TTIMPC         / NPC   * 0.0D0                  /
      DATA     TTIMPH         /    NPN* 0.0D0                  /
      DATA     TVI            /    NPN* 0.0D0                  /
      DATA     TVIBEG         /    NPN*TIMERR                  /
      DATA     TVIMIN         /    NPN* 0.0D0                  /
      DATA     TVISLP         /    NPN* 0.0D0                  /
      DATA     TVITBR         /    NPN* 0.0D0                  /
      DATA     TVITTR         /    NPN* 0.0D0                  /
      DATA     TWK            /    NPN* 0.0D0                  /
C-----COMMON / PHASEI /
      DATA     HITLST         /         0                      /
      DATA     ICAMPS         /    NPN* 0                      /
      DATA     (IPH(I,1),I=1,NPC)
     *                        /         1,1,1,2,2,2,3,3,3      /
      DATA     (IPH(I,2),I=1,NPC)
     *                        /         5,6,7,5,6,7,5,6,7      /
      DATA     LLD            / NPLNPN* 0                      /
      DATA     LPHASE         /    NPN* 0                      /
      DATA     LPHNXT         / MPNNPN* 0                      /
      DATA     LPHRC          / MPNNPN* 0                      /
      DATA     NAIMAX         /    NPN* 0                      /
      DATA     NAIMID         /    NPN* 0                      /
      DATA     NAIMIN         /    NPN* 0                      /
      DATA     NEXTPH         /         0                      /
      DATA     NGAPPC         / NPC   * 0                      /
      DATA     NGAPPH         /    NPN* 0                      /
      DATA     NGRMAX         /    NPN* 0                      /
      DATA     NGRMID         /    NPN* 0                      /
      DATA     NGRMIN         /    NPN* 0                      /
      DATA     NGROUP         /         0                      /
      DATA     NLD            /    NPN* 0                      /
      DATA     NLDC           /    NPN* 0                      /
      DATA     NLDE           /    NPN* 0                      /
      DATA     NLDF           /    NPN* 0                      /
      DATA     NMAXPC         / NPC   * 0                      /
      DATA     NMAXPH         /    NPN* 0                      /
      DATA     NPDEXC         /    NPN* 0                      /
      DATA     NPEDS          /         0                      /
      DATA     NPHASE         /         0                      /
      DATA     NPHNXT         /    NPN* 0                      /
      DATA     NPHPR          /    NRG* 0                      /
      DATA     NPHRC          /    NPN* 0                      /
      DATA     NRING          /         0                      /
      DATA     NTIMPC         / NPC   * 0                      /
      DATA     NTIMPH         /    NPN* 0                      /
      DATA     PDL2TX         /         1, 2, 3, 4, 5, 6, 7, 8,
     *                                  9,10,11,12,13,14,15,16 /
      DATA     PDSTAT         /    NPN* 0                      /
      DATA     PEDINT         /    NPN* 0                      /
      DATA     PEDVOL         /    NPN* 0                      /
      DATA     PHGRP          /    NPN* 0                      /
      DATA     PHRNG          /    NPN* 0                      /
      DATA     PTX2DL         /         1, 2, 3, 4, 5, 6, 7, 8,
     *                                  9,10,11,12,13,14,15,16 /
C-----COMMON / PHASEL /
      DATA     ANYPED         /        .FALSE.                 /
      DATA     ESIMGO         /        .FALSE.                 /
      DATA     IDOR           / NRGNGR*.FALSE.                 /
      DATA     MAGSAT         /        .TRUE.                  /
      DATA     NEWPDA         /        .FALSE.                 /
      DATA     NEWPSI         /        .FALSE.                 /
      DATA     NEWTDA         /        .FALSE.                 /
      DATA     NEWTSI         /        .FALSE.                 /
      DATA     PDCALL         /    NPN*.FALSE.                 /
      DATA     PDEXCL         /    NPN*.FALSE.                 /
      DATA     PDTRIP         /    NPN*.FALSE.                 /
      DATA     PEDS           /    NPN*.FALSE.                 /
      DATA     VOLDEN         /    NPN*.FALSE.                 /
C-----COMMON / PRTPVA /
C6    DATA     DISTAD         / NVE   * 0.0D0                  /
C-----COMMON / QUED   /
      DATA     ENTACC         / NIL   * 0.0D0                  /
      DATA     ENTSLP         / NIL   * 0.0D0                  /
      DATA     ENTVEL         / NIL   * 0.0D0                  /
      DATA     QGOATM         / NIL   * 0.0D0                  /
      DATA     QGOTIM         / NIL   * 0.0D0                  /
      DATA     QRRATM         / NIL   * 0.0D0                  /
      DATA     QRRTIM         / NIL   * 0.0D0                  /
      DATA     QSTDTM         / NIL   * 0.0D0                  /
      DATA     QSTPOS         / NIL   * 0.0D0                  /
      DATA     QSTTIM         / NIL   * 0.0D0                  /
      DATA     QTIME          / NIL   *-1.0D0                  /
      DATA     VDIACC         / NIL   * 0.0D0                  /
      DATA     VDIFGA         / NIL   * 0.0D0                  /
      DATA     VDIFGT         / NIL   * 0.0D0                  /
      DATA     VDIFRA         / NIL   * 0.0D0                  /
      DATA     VDIFRT         / NIL   * 0.0D0                  /
      DATA     VDIFSD         / NIL   * 0.0D0                  /
      DATA     VDIFSP         / NIL   * 0.0D0                  /
      DATA     VDIFST         / NIL   * 0.0D0                  /
      DATA     VDIQIT         / NIL   * 0.0D0                  /
      DATA     VDISLP         / NIL   * 0.0D0                  /
      DATA     VDISPD         / NIL   * 0.0D0                  /
C-----COMMON / QUEI   /
      DATA     IBUF           / NILIBS* 0                      /
      DATA     IQ             / NVEP2 * 0                      /
      DATA     IQF            /         0                      /
      DATA     IQFVDI         /         0                      /
      DATA     IQQ            /         0                      /
      DATA     LQ             / NIANAL* 0                      /
      DATA     NUMV           /         1                      /
      DATA     VDIERRCNT      /         0                      /
C-----COMMON / QUEL   /
      DATA     IEF            / .FALSE.                        /
C-----COMMON / RUTINC /
C*    DATA     IRNAME         / 61    *'      '                /
C*    DATA     MSG            / 'STOP 999 - NRNAME GT 60'      /
C-----COMMON / RUTINI /
C*    DATA     NRNAME         /         0                      /
C*    DATA     NRNAMM         /        60                      /
C-----COMMON / SDR    /
      DATA     ICANSE         / NSSNSR* 0                      /
      DATA     IXSDRC         /    NSR* 0                      /
      DATA     IYSDRC         /    NSR* 0                      /
C-----COMMON / SIGCAD /
      DATA     TCAMSP         / NCM   * 0.0D0                  /
      DATA     TP             /         0.0D0                  /
      DATA     TPT            / NRG   * 0.0D0                  /
      DATA     TR             /         0.0D0                  /
      DATA     TRBYTC         / NILTCM* 0.0D0                  /
      DATA     TRLAST         / NRG   * 0.0D0                  /
      DATA     TRT            / NRG   * 0.0D0                  /
      DATA     TRTCMN         / NILTCM* 0.0D0                  /
      DATA     TRTCMX         / NILTCM* 0.0D0                  /
C-----COMMON / SIGCAI /
      DATA     GARP           / NILITU* 0                      /
      DATA     IARRPH         /         0                      /
      DATA     ICAMPC         /         0                      /
      DATA     ICAMPH         / NCM   * 0                      /
      DATA     ICAMPO         /         0                      /
      DATA     ICPHAS         /         0                      /
      DATA     IGO            /         0                      /
      DATA     INTER          / NRG   * INTERG                 /
      DATA     ISISET         / NSISET* 0                      /
      DATA     NCAMSP         /         0                      /
      DATA     PHINT          / NPN   * 0                      /
C-----COMMON / SIGCAL /
      DATA     NEWSSG         /        .FALSE.                 /
C-----COMMON / SUMSTD /
      DATA     ADESPD         / NISNTS* 0.0D0                  /
      DATA     ASPEED         / NISNTS* 0.0D0                  /
      DATA     DMPH           / NISNTS* 0.0D0                  /
      DATA     PLVDV          / NIS   * 0.0D0                  /
      DATA     QD             / NISNTS* 0.0D0                  /
      DATA     SD             / NISNTS* 0.0D0                  /
      DATA     STIME          / NISNTS* 0.0D0                  /
      DATA     TD             / NISNTS* 0.0D0                  /
      DATA     TMTIME         / 5     * 0.0D0                  /
      DATA     VMAXA          / NISNTS* 0.0D0                  /
      DATA     VMAXD          / NISNTS* 0.0D0                  /
      DATA     VMT            / NISNTS* 0.0D0                  /
      DATA     XFPS           /         0.0D0                  /
      DATA     XQDIST         /         0.0D0                  /
C-----COMMON / SUMSTI /
      DATA     MNVSY          /         0                      /
      DATA     MQUEUE         / NISNAL* 0                      /
      DATA     NBANG          / NIS   * 0                      /
      DATA     NDMPH          / NISNTS* 0                      /
      DATA     NELIM          / NIS   * 0                      /
      DATA     NLVDV          / NIS   * 0                      /
      DATA     NQD            / NISNTS* 0                      /
      DATA     NSD            / NISNTS* 0                      /
      DATA     NTD            / NISNTS* 0                      /
      DATA     NUMPRO         / NISNTS* 0                      /
      DATA     NUMPST         /         0                      /
      DATA     NUMPSU         /         0                      /
C-----COMMON / SUMST4 /
      DATA     LQUEUE         / NISNAL* 0                      /
      DATA     NVSYA          /         0                      /
C-----COMMON / TAP10D /
C8    DATA     TQUEUE         / NVE   * 0.0D0                  /
C-----COMMON / TAP10I /
C8    DATA     NQUEUE         / NVE   * 0                      /
C-----COMMON / TESTEC /
C=    DATA     LDTXT          / NLS   *'Loop '                 /
C=    DATA     STANAM         /        ' PC15 ',
C=   *                                 ' PC16 ',
C=   *                                 ' PC17 ',
C=   *                                 ' PC25 ',
C=   *                                 ' PC26 ',
C=   *                                 ' PC27 ',
C=   *                                 ' PC35 ',
C=   *                                 ' PC36 ',
C=   *                                 ' PC37 '                /
C-----COMMON / TESTED /
C=    DATA     TMRVAT         / NTM   * 0.0D0                  /
C-----COMMON / TESTEI /
C=    DATA     ICAR           /         0                      /
C=    DATA     ICCA           /         0                      /
C=    DATA     ICCI           /         0                      /
C=    DATA     ICDE           /         0                      /
C=    DATA     ICGR           /         0                      /
C=    DATA     ICHO           /         0                      /
C=    DATA     ICII           /         0                      /
C=    DATA     ICLD           /         0                      /
C=    DATA     ICMX           /         0                      /
C=    DATA     ICNX           /         0                      /
C=    DATA     ICPCT          /        -1                      /
C=    DATA     ICPH           /         0                      /
C=    DATA     ICRN           /         0                      /
C=    DATA     ICSE           /         0                      /
C=    DATA     ICVI           /         0                      /
C=    DATA     INPCT          /        -1                      /
C=    DATA     IOPCT          /        -1                      /
C=    DATA     IOVRLT         / NON   *-1                      /
C=    DATA     IPGZ           /         0                      /
C=    DATA     IRADD          /         0                      /
C=    DATA     IRDTCT         /         0                      /
C=    DATA     IRMAX          /         0                      /
C=    DATA     IROPTS         /         0                      /
C=    DATA     IROVLP         /         0                      /
C=    DATA     IRPHAS         /         0                      /
C=    DATA     IRRING         /         0                      /
C=    DATA     IRSIGS         /         0                      /
C=    DATA     IRSTAT         /         0                      /
C=    DATA     IRTIME         /         0                      /
C=    DATA     IRTMRS         /         0                      /
C-----COMMON / TESTEL /
C=    DATA     CROSST         /        .FALSE.                 /
C=    DATA     DABT           /        .FALSE.                 /
C=    DATA     IDORT          / NRGNGR*.FALSE.                 /
C=    DATA     LTEMP          /        .FALSE.                 /
C=    DATA     PAGING         /        .FALSE.                 /
C=    DATA     SERVET         / NRG   *.FALSE.                 /
C=    DATA     STATET         / NPC   *.TRUE.                  /
C-----COMMON / TITLEC /
      DATA     DTITLE         / ' '                            /
      DATA     GTITLE         / ' '                            /
      DATA     IVERSN         / 'V6.01'                        /
      DATA     IXXX           / ' '                            /
      DATA     JVERSN         / '     '                        /
      DATA     KVERSN         / '     '                        /
      DATA     STITLE         / ' '                            /
C-----COMMON / TITLER /
      DATA     DVERSN         /         0.0                    /
      DATA     GVERSN         /         0.0                    /
      DATA     SVERSN         /         0.0                    /
C-----COMMON / TWRD   /
C;    DATA     DVILNI         / NALNAP* 0.0D0                  /
C;    DATA     FVILNI         / NALNAP* 0.0D0                  /
C;    DATA     GVILNI         / NALNAP* 0.0D0                  /
C;    DATA     HVILNI         / NALNAP* 0.0D0                  /
C;    DATA     TVILNI         / NALNAP* 0.0D0                  /
C-----COMMON / TWR4   /
C;    DATA     KVILNI         / NALNAP* 0                      /
C-----COMMON / TXDSIC /
      DATA     GMT            / NPN   *' '                     /
      DATA     IMEM           / NPN   *'YES'                   /
      DATA     IMNR           / NPN   *'NO '                   /
      DATA     IMXR           / NPN   *'NO '                   /
      DATA     ISTO           / NPN   *'YES'                   /
C-----COMMON / TXDSID /
      DATA     TIMRCR         / NPN   *-9.99D0                 /
      DATA     TMRSET         / NTM   * 0.0D0                  /
      DATA     TMRVAL         / NTM   * 0.0D0                  /
C-----COMMON / TXDSII / 
      DATA     ICAMCT         / NRG   * 0                      /
      DATA     ICPC           /         0                      /
      DATA     ICPHAT         / NRG   * 0                      /
      DATA     IDEPH          / NPN   * 0                      /
      DATA     IFPR           / NRG   * 0                      /
      DATA     ILPR           / NRG   * 0                      /
      DATA     INPC           /         0                      /
      DATA     IOPC           /         0                      /
      DATA     IOPHAS         / NRG   * 0                      /
      DATA     IOVRLP         / NON   * 0                      /
      DATA     IPC            / NPCNPC* 0                      /
      DATA     IPHPS          / NGR   * 8                      /
      DATA     IPS            / NPCNPC* 0                      /
      DATA     IRING          /         0                      /
      DATA     LOLDF          / NPHNON* 0                      /
      DATA     LOLP           / NON   * 0                      /
      DATA     LPHPS          / RGGRPN* 0                      /
      DATA     NEXTPT         / NRG   * 0                      /
      DATA     NEXTTT         / NRG   * 0                      /
      DATA     NOLDF          / NON   * 0                      /
      DATA     NOLP           /         0                      /
      DATA     NPHPS          / NRGNGR* 0                      /
      DATA     NPS            / NPC   * 0                      /
C-----COMMON / TXDSIL /
      DATA     CLPH           / NPN   *.FALSE.                 /
      DATA     CPH            / NPN   *.FALSE.                 /
      DATA     DAB            /        .FALSE.                 /
      DATA     FIG3           /        .FALSE.                 /
      DATA     FIG4           /        .FALSE.                 /
      DATA     FIG6           /        .FALSE.                 /
      DATA     FIG7           /        .FALSE.                 /
      DATA     FIRSIG         /        .TRUE.                  /
      DATA     HOLD           / NPN   *.FALSE.                 /
      DATA     LDEPH          / NPN   *.FALSE.                 /
      DATA     NEXT           / NPN   *.FALSE.                 /
      DATA     OPH            / NPN   *.FALSE.                 /
      DATA     OPTN           / NOP   *.FALSE.                 /
      DATA     REST           / NPC   *.FALSE.                 /
      DATA     SEL            / NPN   *.FALSE.                 /
      DATA     STATE          / NPC   *.FALSE.                 /
C-----COMMON / USERC  /
      DATA     ERRMSG         / ' '                            /
      DATA     ESC            / ' '                            /
      DATA     FMT            / '(A)'                          /
      DATA     IMAGL1         / ' '                            /
      DATA     IMAGL2         / ' '                            /
      DATA     IMAGOP         / '   '                          /
      DATA     IPAP           / 'YES'                          /
      DATA     IPOLL          / 'NO '                          /
      DATA     IPTC           / 'YES'                          /
      DATA     IPUNCH         / 'YES'                          /
      DATA     ISSAM          / 'NO '                          /
      DATA     IWNOUT         / 'W'                            /
      DATA     PAR            / 'I'          ,
     *                          'L'          ,
     *                          'STA'        ,
     *                          'T8'         ,
     *                          'T9'         ,
     *                          'PVA'        ,
     *                          'ERR'        ,
     *                          'SYS_DAT'    ,
     *                          'REP'        ,
     *                          'SSAM'       ,
     *                          'WRK_DIR'    ,
     *                          'PAUSEND'    ,
     *                          'VEHDAT'                       /
      DATA     PARVAL         / ' '          ,
     *                          'simplst.txt',
     *                          'simstat'    ,
     *                          ' '          ,
     *                          ' '          ,
     *                          'posdat'     ,
     *                          'simerr.txt' ,
     *                          ' '          ,
     *                          'NO'         ,
     *                          ' '          ,
     *                          ' '          ,
     *                          'YES'        ,
     *                          ' '                            /
      DATA     WRNMSG         / ' '                            /
C-----COMMON / USERD  /
      DATA     ABTPMN         /         0.0D0                  /
      DATA     ABTPSD         /         0.0D0                  /
      DATA     APIJR          /         0.0D0                  /
      DATA     AUTOL          /         1.7D0                  /
      DATA     BEGT20         /         0.0D0                  /
      DATA     CAREQA         /         0.0D0                  /
      DATA     CAREQL         /         0.0D0                  /
      DATA     CAREQM         /         0.0D0                  /
      DATA     DT             /         0.0D0                  /
      DATA     DTCU           /         0.0D0                  /
      DATA     DTMAX          /         1.0D0                  /
      DATA     DTSQ           /         0.0D0                  /
      DATA     DUTOL          /         2.666666666666667D0    /
      DATA     ENDT20         /         5.0D0                  /
      DATA     HESFAC         /         2.0D0                  /
      DATA     PDSCOL         /         0.0D0                  /
      DATA     SIMTIM         /         0.0D0                  /
      DATA     STRTIM         /         0.0D0                  /
      DATA     TIME           /         0.0D0                  /
      DATA     TLAG           /         0.0D0                  /
      DATA     TLEAD          /         0.0D0                  /
      DATA     TPRINT         /         0.0D0                  /
      DATA     TSTATS         /         0.0D0                  /
C-----COMMON / USERI  /
      DATA     DET            /        77                      /
      DATA     IBLATR         /         0                      /
      DATA     ICS            /         7                      /
      DATA     IDH            /.       11                      /
      DATA     IGEOP          /         8                      /
      DATA     INPUT          /         5                      /
      DATA     IPA            /         3                      /
      DATA     IPP            /         1                      /
      DATA     IPV            /         2                      /
      DATA     IQD            /        10                      /
      DATA     ISS            /        30                      /
      DATA     IVEHP          /         9                      /
      DATA     IVMSMB         /         1                      /
      DATA     MPRTM          /        15                      /
      DATA     NCERR          /         0                      /
      DATA     NCIF           /         0                      /
      DATA     NCLF           /         0                      /
      DATA     NCOLS          /         0                      /
      DATA     NCPVA          /         0                      /
      DATA     NCSSAM         /         0                      /
      DATA     NCSTA          /         0                      /
      DATA     NCT8           /         0                      /
      DATA     NCT9           /         0                      /
      DATA     NCVD           /         0                      /
      DATA     NDV            /        23                      /
      DATA     NER            /        90                      /
      DATA     NFRTS          /         0                      /
      DATA     NGD            /         1                      /
      DATA     NGP            /        22                      /
      DATA     NIO            /         0                      /
      DATA     NPD            /        20                      /
      DATA     NPRCNT         /         0                      /
      DATA     NRNS           /         0                      /
      DATA     NROWS          /         0                      /
      DATA     NRTS           /         0                      /
      DATA     NRTSM1         /         0                      /
      DATA     NSP            /        26                      /
      DATA     NVD            /        98                      /
      DATA     NWR            /        91                      /
      DATA     NWV            /        44                      /
      DATA     PPP            /         4                      /
      DATA     SER            /        99                      /
      DATA     TC3            /        33                      /
      DATA     TC6            /        66                      /
C-----COMMON / USERL  /
      DATA     FIRSTM         /       .TRUE.                   /
      DATA     LTPOUT         /       .FALSE.                  /
      DATA     MAJCBP         /       .FALSE.                  /
      DATA     STOPMC         /       .FALSE.                  /
C-----COMMON / VMSMC  /
      DATA     CVMSDN         / NVMSMM*'       '               /
C-----COMMON / VMSMD  /
      DATA     DVMSAT         / NVMSMM* 0.0D0                  /
      DATA     DVMSDM         / NVMSMM* 0.0D0                  /
      DATA     DVMSDP         / NVMSMM* 0.0D0                  /
      DATA     DVMSMP         / NVMSMM* 0.0D0                  /
      DATA     DVMSPB         / NVMSMM* 0.0D0                  /
      DATA     DVMSPE         / NVMSMM* 0.0D0                  /
      DATA     DVMSST         / NVMSMM* 0.0D0                  /
C-----COMMON / VMSMI  /
      DATA     IVMSAP         / NVMSMM* 0                      /
      DATA     IVMSLB         / NVMSMM* 0                      /
      DATA     IVMSLE         / NVMSMM* 0                      /
      DATA     IVMSMC(VMSMSC) /        VMSCSC                  /
      DATA     IVMSMC(VMSMDD) /        VMSCDD                  /
      DATA     IVMSMC(VMSMSM) /        VMSCSM                  /
      DATA     IVMSMC(VMSMSL) /        VMSCSL                  /
      DATA     IVMSMC(VMSMSI) /        VMSCSI                  /
      DATA     IVMSMC(VMSMRR) /        VMSCRR                  /
      DATA     IVMSMC(VMSMGO) /        VMSCGO                  /
      DATA     IVMSMC(VMSMAM) /        VMSCAM                  /
      DATA     IVMSMC(VMSMAN) /        VMSCAN                  /
      DATA     IVMSMC(VMSMCL) /        VMSCCL                  /
      DATA     IVMSMC(VMSMCR) /        VMSCCR                  /
      DATA     IVMSMG         / NVMSMM* 0                      /
      DATA     IVMSMT         / NVMSMM* 0                      /
      DATA     IVMSPR(VMSMSC) /        VMSPSC                  /
      DATA     IVMSPR(VMSMDD) /        VMSPDD                  /
      DATA     IVMSPR(VMSMSM) /        VMSPSM                  /
      DATA     IVMSPR(VMSMSL) /        VMSPSL                  /
      DATA     IVMSPR(VMSMSI) /        VMSPSI                  /
      DATA     IVMSPR(VMSMRR) /        VMSPRR                  /
      DATA     IVMSPR(VMSMGO) /        VMSPGO                  /
      DATA     IVMSPR(VMSMAM) /        VMSPAM                  /
      DATA     IVMSPR(VMSMAN) /        VMSPAN                  /
      DATA     IVMSPR(VMSMCL) /        VMSPCL                  /
      DATA     IVMSPR(VMSMCR) /        VMSPCR                  /
      DATA     IVMSVN         / NVMSMM* 0                      /
C-----COMMON / USFILE /
      DATA     USFILE         /        ' '                     /
      END                                                               BLKDAT
C
C
C
      SUBROUTINE ABORTR ( MESAGE )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'ABIAS'
      INCLUDE 'ANIMAT'
      INCLUDE 'APPRO'
      INCLUDE 'CHARAC'
      INCLUDE 'CLASS'
      INCLUDE 'CONCHK'
      INCLUDE 'CONFLT'
      INCLUDE 'DIAMON'
      INCLUDE 'INDEX'
      INCLUDE 'INTER'
      INCLUDE 'LANE'
      INCLUDE 'LANECH'
      INCLUDE 'LOOPS'
      INCLUDE 'PATH'
      INCLUDE 'PHASES'
C6    INCLUDE 'PRTPVA'
      INCLUDE 'QUE'
C*    INCLUDE 'RUTINE'
      INCLUDE 'SDR'
      INCLUDE 'SIGCAM'
      INCLUDE 'SUMST2'
C8    INCLUDE 'TAPE10'
C=    INCLUDE 'TESTER'
      INCLUDE 'TITLE'
      INCLUDE 'TXDSIG'
      INCLUDE 'USER'
      INCLUDE 'VEHD'
      INCLUDE 'VEHF'
      INCLUDE 'VEHIL'
      CHARACTER*(*)     MESAGE
      INTEGER           ILNB,NCMES
C*    INTEGER           I
  601 FORMAT(3(1X,A,/),/,
     *       44H SIMULATION PROCESSOR FOR THE TEXAS TRAFFIC ,
     *       26HSIMULATION PACKAGE ABORTED,//,1X,A,/)
  602 FORMAT(43HSimulation Processor for the TEXAS Traffic ,
     *       20HSimulation Package (,A5,1H),/,
     *       29HSIMPRO Copyright (C) 1989 by ,
     *       33HThe University of Texas at Austin,//,3(   A,/),/,
     *       43HSimulation Processor for the TEXAS Traffic ,
     *       26HSimulation Package ABORTED,//,1X,A,/)
  603 FORMAT(' TIME=',F7.2,:,
     *       '  IV=',I4,' IVN=',I4,
     *       '  IL=',I3,' ILN=',I3,
     *       '  IA=',I3,' IAN=',I3,
     *       '  IP=',I3,' MININT=',L1)
C*604 FORMAT(18(1X,A6))
  605 FORMAT(' POSOLD=',F8.2,' VELOLD=',F7.2,' ACCOLD=',F9.3,
     *       ' SLPOLD=',F10.3,/,
     *       ' POSNEW=',F8.2,' VELNEW=',F7.2,' ACCNEW=',F9.3,
     *       ' SLPNEW=',F10.3,/,
     *       ' LPREV=',I4,' LPRES=',I4,' LNEXT=',I4,' INT2P=',I4)
      NCMES = MAX0( ILNB( MESAGE ),1 )
      CALL  PHEADR  ( 6 )
      WRITE (6,601)        GTITLE,DTITLE,STITLE,MESAGE(1:NCMES)
      WRITE (*,602) IVERSN,GTITLE,DTITLE,STITLE,MESAGE(1:NCMES)
      IF ( IV . EQ . 0 )                         THEN
        WRITE (6,603) TIME
        WRITE (*,603) TIME
      ELSE
        WRITE (6,603) TIME,IV,IQ(IV),IL,ILN,IA,IAN,IP,MININT(IV)
        WRITE (*,603) TIME,IV,IQ(IV),IL,ILN,IA,IAN,IP,MININT(IV)
      END IF
C*    WRITE (6,604) (IRNAME(I),I=1,NRNAME)
C*    WRITE (*,604) (IRNAME(I),I=1,NRNAME)
      IF ( IV . GT . 0 )                         THEN
        WRITE (6,605) POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                LPREV(IV),LPRES(IV),LNEXT(IV),INT2P(IV)
        WRITE (*,605) POSOLD,VELOLD,ACCOLD,SLPOLD,
     *                POSNEW,VELNEW,ACCNEW,SLPNEW,
     *                LPREV(IV),LPRES(IV),LNEXT(IV),INT2P(IV)
      END IF
      FIRSTM = .FALSE.
      CALL  PRTERR  ( MESAGE )
C*          IF ( MESAGE(1:12) . EQ . MSG(1:12) ) STOP  999
      RETURN
      END                                                               ABORTR
C
C
C
      SUBROUTINE PHEADR ( IUNIT )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
      INCLUDE 'TITLE'
      INCLUDE 'USER'
      LOGICAL           FIRST(99)
      INTEGER           IUNIT
      SAVE     FIRST
      DATA     FIRST  / 99*.TRUE. /
  601 FORMAT(6X,43HSIMULATION PROCESSOR FOR THE TEXAS TRAFFIC ,
     *       20HSIMULATION PACKAGE (,A5,1H),/,
     *       9X,29HSIMPRO COPYRIGHT (C) 1989 BY ,
     *       33HTHE UNIVERSITY OF TEXAS AT AUSTIN,/)
      IF ( .NOT. FIRST(IUNIT) )                  THEN
        WRITE (IUNIT,FMT) CHAR( 12 )
      END IF
      WRITE (IUNIT,601) IVERSN
      FIRST(IUNIT) = .FALSE.
      RETURN
      END                                                               PHEADR
C
C
C
      SUBROUTINE PRTERR ( MESAGE )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
      INCLUDE 'TITLE'
      INCLUDE 'USER'
      CHARACTER*(*)     MESAGE
      INTEGER           ILNB,NCMES
  601 FORMAT(A)
      IF ( FIRSTM )                              THEN
        CALL  PHEADR  ( 6   )
        FIRSTM = .FALSE.
      END IF
      NCMES = MAX0( ILNB( MESAGE ),1 )
      OPEN  (NER,FILE='error.txt',ACCESS='APPEND',STATUS='UNKNOWN')
      WRITE (NER,601) MESAGE(1:NCMES)
      ENDFILE NER
      CLOSE (NER,STATUS='KEEP')
      WRITE (6  ,601)
      WRITE (6  ,601) MESAGE(1:NCMES)
      WRITE (SER,601)
      WRITE (SER,601) MESAGE(1:NCMES)
      WRITE (*  ,601)
      WRITE (*  ,601) MESAGE(1:NCMES)
      RETURN
      END                                                               PRTERR
C
C
C
      SUBROUTINE PRTWRN ( MESAGE )
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CLASS'
      INCLUDE 'INDEX'
      INCLUDE 'QUE'
      INCLUDE 'TITLE'
      INCLUDE 'USER'
      CHARACTER*(*)     MESAGE
      INTEGER           ILNB,NCMES
  601 FORMAT(A)
      NCMES = MAX0( ILNB( MESAGE ),1 )
      OPEN  (NWR,FILE='warning.txt',ACCESS='APPEND',STATUS='UNKNOWN')
      WRITE (NWR,601) MESAGE(1:NCMES)
      ENDFILE NWR
      CLOSE (NWR,STATUS='KEEP')
      WRITE (SER,601)
      WRITE (SER,601) MESAGE(1:NCMES)
      RETURN
      END                                                               PRTWRN
C
C
C
C>    SUBROUTINE EXTIME ( I )
C>    IMPLICIT NONE
C>    INCLUDE 'PARAMS'
C>    INCLUDE 'CLASS'
C>    INCLUDE 'INDEX'
C>    INCLUDE 'QUE'
C>    INCLUDE 'RUTINE'
C>    INCLUDE 'SUMST2'
C>    INCLUDE 'USER'
C>    INTEGER           I,ITM
C
C-----SUBROUTINE EXTIME GETS THE TM TIME FOR THIS JOB
C
C>    NRNAME = NRNAME + 1
C>    IRNAME(NRNAME) = 'EXTIME'
C>                  IF ( NRNAME . GT . NRNAMM )  CALL ABORTR ( MSG )
C-----GET THE TM TIME FOR THIS JOB (CDC)
C>    CALL  JOBINFO ( 0,ITM )
C>    TMTIME(I) = ITM/1000.0D0
C-----GET CPU TIME FOR THIS JOB (IBM)
C>    RETURN
C>    END                                                               EXTIME
