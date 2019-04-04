      SUBROUTINE GDVCON(JOF,TAG,NOUT,GDFILE,PFILE,CFILE,DEFILE,USFILE)
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
C ----- THIS SUBROUTINE CONVERTS GEOMETRY AND DRIVER-VEHICLE DATA
C ----- FROM CONFIGURATION 2 (C2) TO CONFIGURATION 1 (C1).
C ----- A C1 FILE IS COMPATABLE WITH THE GEOMETRY PROCESSOR AND DRIVER-VEHICLE
C ----- PROCESSOR DEVELOPED IN PROJ. 184. A C2 FILE IS CREATED BY THE
C ----- PREPROCESSOR DEVELOPED IN PROJ 361.  AFTER EXECUTION, A
C ----- C1 FILE IS OPEN ON UNIT ABS<JOF>.  IF <JOF> IS
C ----- NEGATIVE, A COMPLETION MESSAGE IS DISPLAYED ON THE TERMINAL.
C ----- IF <GDFILE>="ECHO", ADDITIONAL STATUS MESSAGES ARE DISPLAYED.
C ----- IF <PFILE> NE " ", THEN FILE <PFILE> IS ASSUMED TO BE THE
C ----- NAME OF A PREPROCESSSOR DATA FILE.  <DEFILE> IS THE DEFAULT
C ----- PREPROCESSOR FILE NAME AND EXTENSION.  <USFILE> IS USED TO SUPPLY
C ----- PARTS OF THE PREPROCESSOR FULL FILENAME THAT ARE NOT IN <PFILE>
C ----- OR <DEFILE>.
C ----- THE DEFAULT DATA OR REFERENCE FILE IS THE FORTRAN DEFAULT FILE
C ----- NAME FOR UNIT <MOUT(2)>.  IF <CFILE> NE " ", THE C1 DATA IS
C ----- WRITTEN TO FILE <CFILE>.  THE DEFAULT FILE NAME FOR THE C1 DATA
C ----- IS "GDV".  IF A C2 FILE IS SUCESSFULLY CONVERTED, THE C2 FILE
C ----- NAME IS RETURNED IN <GDFILE>.  FOR A SUCCESSFUL CONVERSION,
C ----- <TAG> RETURNS THE STANDARD LIBRARY ID, THE USER-GROUP LIBRARY
C ----- NUMBER OR "   " FOR A FILE FROM THE COMMAND LINE.  "NUL" IS
C ----- RETURNED IF A C2 FILE IS NOT OPEN.
C
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'IVERSN'
      INTEGER           JOF    ,NOUT(*)
      CHARACTER*3           TAG
      CHARACTER *(*)                 GDFILE,PFILE,CFILE,DEFILE,USFILE
      CHARACTER*6 DIR
      CHARACTER*1              ICY
      CHARACTER*9                      FMTD
      CHARACTER*7                                       UNKN
      PARAMETER  (DIR='DIRECT',ICY='Y',FMTD='FORMATTED',UNKN='UNKNOWN')
      CHARACTER*3  DRVDAT,DRVMIX,IMAGEF,IMAGEU,IOFORM,LVTPP(12,6),TAGU,
     *             VEHDAT
      CHARACTER*4  AZI(0:NLGP1),ILSLIL,ILSLIR,IUAVT(2),SLIMI(0:NLGP1),
     *             SLIMO(0:NLGP1)
      CHARACTER*5  FMTI3
      CHARACTER*6  IFMT,ISUANG(0:NLGP1),STAT
      CHARACTER*15 HDR
      CHARACTER*24 ULINE
      CHARACTER*40 ILIDR(6),ILIDL(6),ILODR(6),ILODL(6)
      CHARACTER*60 ILAPIR,ILAPIL,ILAPOR,ILAPOL,TEXFN,DEFFN,FNAME
      CHARACTER*60 PFILE1,PFILE2,GDFILEU
      CHARACTER*80 NLINE,LID(6),LOD(6),NLINEU,LVTP(12),PVTP(12,7),LVTPU
      INTEGER   LIA       ,LOA       ,MIL         ,MOL         ,
     1          LLENI         ,LLENO                   ,IXRC    ,
     2          IYRC    ,IRRC    ,IBAZI    ,ISANG    ,
     3          IXLN1     ,IYLN1     ,IXLN2     ,IYLN2     ,
     4          IXC1         ,IYC1         ,IXC2         ,IYC2         ,
     5          IPCT         ,MED         ,LPD         ,LIAD         ,
     6          LOAD         ,IUWDTH   ,IULEN     ,IURAD     ,IUSPA   ,
     7          IUPCT   ,IXUR   ,IXUL   ,IYU
     8                                 ,NVTP
      INTEGER*4                                             ISEED
      REAL
     1                                        XTEMP    ,
     2
     3
     4
     5
     6
     7
     8          ANGL         ,SETB
      CHARACTER*1                               DESTD
      DIMENSION LIA(NLGP2),LOA(NLGP2),MIL(0:NLGP1),MOL(0:NLGP1),
     1          LLENI(0:NLGP1),LLENO(0:NLGP1),XTEMP(NVC),IXRC(16),
     2          IYRC(16),IRRC(16),IBAZI(16),ISANG(16),
     3          IXLN1(NLI),IYLN1(NLI),IXLN2(NLI),IYLN2(NLI),ISEED(12),
     4          IXC1(0:NLGP1),IYC1(0:NLGP1),IXC2(0:NLGP1),IYC2(0:NLGP1),
     5          IPCT(0:NLGP1),MED(0:NLGP1),LPD(0:NLGP1),LIAD(0:NLGP1),
     6          LOAD(0:NLGP1),IUWDTH(2),IULEN(2,2),IURAD(2,2),IUSPA(2),
     7          IUPCT(2),IXUR(2),IXUL(2),IYU(2),DESTD(NLGP2+NLGP2),
     8          ANGL(0:NLGP1),SETB(2,2),NVTP(0:NLGP1)
      INTEGER*4 ISE
      LOGICAL ECHO,PARLG,NEGCRR,DIAMON,EXI,EXI1,CLDATA,ERR
      INTEGER CLOFF(0:NLGP1),CRR(NLG),ICRRD(0:NLGP1)
      INTEGER ISDRL(20),ISDSB(20),ISDOFF(20)
      INTEGER I,ICPXC,ICPXP,ICPYC,ICPYP,ICRRT,ICSPA,ICSPO2,IDIST,IJ1,
     1        IJ2,ILMW,ILNB,ILNIL,ILNILL,ILNILR,ILNIR,ILNOLL,ILNOL,
     2        ILNOLR,ILNOR,ILXIL,ILXIR,ILXOL,ILXOR,IM1,IM2,INDX1,INDX2,
     3        IOF,IOFC,IOFCP,IOFPT,IOFT,IP,IPCTU,IR,IR1,IR2,IREC,ISDSBT,
     4        ISIGN,ISLW,ISLWP,IT1,IT2,IT3,IT4,IT5,ITCRR,ITEMP,IVTP,IXL,
     5        IXOFF,IXOFFT,IYL,IYOFF,J,JJ,LSTOF,LSTOFP,MXOF,MXOFIL,
     6        MXOFIR,MXOFOL,MXOFOR,MXOFP,NUNAPR,NC,NCARDS,NCCF,NCFN,
     7        NCFV,NCFV10,NCFV20,NCGD,NCI,NCSTAT,NCTEX,NDRICL,NDFIO,
     8        NFUT,NIL1,NILS,NINT,NLEGS,NLNE,NOL1,NOLS,NOTBAP,NRCG,
     9        NRCM1,NRCU,NSD,NUNITS,NVEHAT,NVEHCL,RECVTP
      REAL    A,A1,ALPHA,AP,B,BP,CLOFT,CLOFPT,COSTHP,COSTHT,CSPAO2,D,D1,
     1        DELTHT,PPW,R,SDANG,SDX,SDY,SETBT,SINTHP,SINTHT,T1,THETA,
     2        THT,THTP,X1,XC,XCRRC,XL,XM,XMP,XT,XT2,XT3,XX,XX1,XX2,YC,
     3        YCRRC,YL,YR,YT
C-----SPECIAL VEHICLE VARIABLES
      DOUBLE PRECISION QTIMSV
      INTEGER          IVHCSV,IDRCSV,IVELSV,IOBLSV,IIBLSV,ILNSV,IPLOSV
      CHARACTER*1      IFUTSV
      DOUBLE PRECISION FSTMSV
      CHARACTER*1      FSLCSV
      INTEGER          FSLPSV
      DOUBLE PRECISION FSPSSV,FSDTSV,FGTMSV,FGATSV,FRTMSV,FRATSV
      CHARACTER*3      EMERGV
      INTEGER          IOBASV,IIBASV,FSAPSV
  501 FORMAT(F7.2 ,I2,I1,I3,2I2,2I1,A1,F7.2,A1,I3,6F7.2,A3)
  502 FORMAT(F10.2,7I5,A1,F7.2, I4  ,6F7.2,A3)
  901 FORMAT('GDVCON ERROR IOBLSV =',I3,' IS LT 1 OR GT ',I2)
  902 FORMAT('GDVCON ERROR IIBLSV =',I3,' IS LT 1 OR GT ',I2)
  903 FORMAT('GDVCON ERROR FSLPSV =',I3,' IS LT 1 OR GT ',I2)
  904 FORMAT('GDVCON ERROR FSLCSV = (',A,
     *       ') IS NOT (I), (L), (O), (P), OR (R)')
  905 FORMAT('GDVCON ERROR EMERGV = (',A,') IS NOT (YES) OR (NO)')
  906 FORMAT('GDVCON ERROR NVEHAT = ',I2,' IS LT 10 WHEN NVEHCL = ',I2,
     *       ' IS NOT ',I2)
      DATA FMTI3,IOFORM,IFMT   / '(9I3)','(A)','(20I4)' /
      DATA NLNE,NRCG           / 2*0 /
      DATA DIAMON,CLDATA       / 2*.FALSE. /
      DATA LID                 /  6*' ' /
      DATA LOD                 /  6*' ' /
      DATA LVTP                / 12*' ' /
      DATA LVTPU               /    ' ' /
      DATA NLINE               /    ' ' /
      DATA NLINEU              /    ' ' /
      DATA PVTP                / 84*' ' /
      DATA (IPCT(I),I=0,NLGP1) / NLGP2*0 /
      DATA (NVTP(I),I=0,NLGP1) / NLGP2*0 /
      DATA (IYC2(I),I=0,NLGP1) / NLGP2*-32000 /
      DATA (IUWDTH(I),I=1,2)   / 2*0 /
      DATA (IUSPA (I),I=1,2)   / 2*0 /
      DATA (IUPCT (I),I=1,2)   / 2*0 /
      DATA ((IULEN(I,J),I=1,2),J=1,2) / 4*0 /
      DATA ((IURAD(I,J),I=1,2),J=1,2) / 4*0 /
C
C ----- FOR FREE U-TURN APPROACHES FOR DESTD(1) AND DESTD(6), 
C ----- MUST SWITCH LEFT/RIGHT INTERSECTION DESTINATION LATER
C
      DATA (DESTD(I),I=1,NLGP2+NLGP2) / 'R','R','R','R','L','L','L',
     1                                  'L','R','R','R','R','L','L',
     2                                  'L','L' /
C
C ----- CURB RETURN INDEX FOR DIAMOND
C ----- THESE WILL BE CHANGED FOR NON-DIAMOND
C
      DATA (ICRRD(I),I=0,NLGP1) / 1,1,2,3,4,5,6,4 /
C
C ----- APPROACH NUMBERS FOR DIAMOND
C ----- THESE WILL BE CHANGED FOR NON-DIAMOND
C
C                   Diamond Interchange Numbering Convention
C  
C            |               /|\                |              |
C            6                1                 |              |
C           \|/               |               6 | 4          1 | 2
C     <-5->     <-7->  <-0->     <-2->      ----|--------------|----
C            |               /|\              5 | 4          1 | 3
C            4                3                 |              |
C           \|/               |                 |              |
C  
C                Leg Numbers               Curb Return Radius Numbers
C  
C  
C          | /|\           | /|\                 |     FUT 2    /|\
C          8 16            1  9                  8 -16->   -6--> 9
C         \|/ |           \|/ |                 \|/              |
C     <-15-    <--5-  <-12-    <-2--      <-15-    <--5-  <-12-    <-2--
C     --7->    -13->  --4->    -10->      --7->    -13->  --4->    -10->
C          | /|\           | /|\                 |              /|\
C         14  6           11  3                 14 <--1-   <-11- 3
C         \|/ |           \|/ |                 \|/    FUT 1     |
C  
C             Approach Numbers                    Approach Numbers
C          (without free u-turns)               (with free u-turns)
C
      DATA (LIAD(I),I=0,NLGP1) /  4, 1, 2, 3, 6, 7, 8, 5 /
      DATA (LOAD(I),I=0,NLGP1) / 12, 9,10,11,14,15,16,13 /
C
C ----- PREVIOUS LEGS FOR DIAMOND
C
      DATA (LPD(I),I=0,NLGP1) / 3,0,1,2,7,4,5,6 /
C
C ----- STANDARD SEED FOR RANDOM NUMBERS
C ----- SHOULD BE SAME IN GDVDATA(BTDFCD) AND DVPRO
C
      DATA (ISEED(I),I=1,12) / 42081,13747,00290,77043,50123,33145,
     1                         83447,60695,32257,99381,23145,19905 /
C-----INITIALIZE CONSTANTS
      CALL  INITCN
      NDFIO=NOUT(2)
      GDFILEU=GDFILE
      CALL  TOUPR   ( GDFILEU )
      IF(GDFILEU(1:4).EQ.'ECHO')THEN
        ECHO=.TRUE.
      ELSE
        ECHO=.FALSE.
      END IF
      GDFILE=' '
      TEXFN=DEFFN(NDFIO)
      NCTEX=ILNB( TEXFN )
      IVERSN = 'V6.00'
      IF(ECHO)WRITE(*,'(/2A)')'GDVCONV ',IVERSN
      NCI=ILNB( PFILE )
      IF(NCI.GT.0)THEN
C
C ----- READ FROM PREPROCESSOR FILE SPECIFIED ON COMMAND LINE
C
        CLDATA=.TRUE.
C
C ----- IS PREPROCESSOR FILE IN DEFAULT FILE STORAGE AREA
C
        CALL TXINQ(0,PFILE,DEFILE,PFILE1,EXI1)
        IF(EXI1)THEN
          EXI=.TRUE.
          PFILE=PFILE1
          GO TO 80
        END IF
C
C ----- IS PREPROCESSOR FILE IN USER_DAT FILE STORAGE AREA
C
        CALL TXINQ(0,USFILE,DEFILE,NLINE,EXI)
        CALL TXINQ(0,PFILE,NLINE,PFILE2,EXI)
        IF(EXI)THEN
          PFILE=PFILE2
          GO TO 80
        END IF
        NLINE=' '
        WRITE(*,'(A)')
        GO TO 100
      END IF
      IF(NCTEX.EQ.0)THEN
        CALL OPENRO(NDFIO,' ',UNKN,DIR,FMTD,80,ERR)
        EXI=.TRUE.
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
      CALL OPENRO(NDFIO,PFILE,'OLD',DIR,FMTD,80,ERR)
      IF(.NOT.ERR)GO TO 200
  100 CONTINUE
C
C ----- TAG=NUL TELLS CALLER THAT A C2 FILE IS NOT AVAILABLE
C
      TAG='NUL'
      IF(NCI.GT.0)THEN
C
C ----- FILE FROM COMMAND LINE COULD NOT BE ACCESSED
C
        IF(ECHO)THEN
          IF(EXI)THEN
            NC=ILNB( PFILE )
            WRITE(*,'(3A)')'File "',PFILE(1:NC),'" is not a '//
     1                     'usable G & D-V Preprocessor file.'
          ELSE
            IF(PFILE1.NE.PFILE2)THEN
              CALL SHONAM('Did not find G & D-V Preprocessor file',
     1                    PFILE1,78)
            END IF
            CALL SHONAM('Did not find G & D-V Preprocessor file',
     1                  PFILE2,78)
          END IF
        END IF
      ELSE
C
C ----- FILE WITH POINTER TO DATA COULD NOT BE ACCESSED
C
        IF(ECHO)WRITE(*,'(/A)')'A G & D-V Preprocessor file was not '//
     1                         'specified.'
        IF(NCTEX.EQ.0)THEN
          CALL OPENFI(NDFIO,' ',ERR)
        ELSE
          CALL OPENFI(NDFIO,PFILE,ERR)
        END IF
        CLOSE(NDFIO,STATUS='DELETE')
      END IF
      GO TO 9999
  200 CONTINUE
      TAG='   '
      IOF=IABS(JOF)
      READ(NDFIO,IOFORM,REC=1,ERR=100)NLINE
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF((NLINEU(1:5).NE.' #%&$') .OR. (NLINEU(10:12).NE.'GDV'))THEN
        EXI=.TRUE.
        WRITE(*,'(/A)')'Header in G & D-V Preprocessor file is invalid.'
        GO TO 100
      END IF
      IF(NLINEU(6:9).EQ.'FILE')THEN
C
C ----- FOUND POINTER TO DATA FILE
C
        TAG=NLINE(13:15)
        FNAME=NLINE(20:)
        NCFN=ILNB( FNAME )
        IF(NLINEU(16:17).EQ.'C1')THEN
C
C ----- FILE POINTED TO IS A C1 FILE
C
          GDFILE=' '
          CALL OPENRO(IOF,FNAME,'OLD','SEQUENTIAL',FMTD,0,ERR)
          IF(ERR)THEN
            WRITE(*,'(A)')
            CALL SHONAM('Converted Geometry & Driver-Vehicle '//
     1                  'data file is',FNAME,78)
            CALL OPNERR(FNAME)
            GO TO 9510
          END IF
          GO TO 9000
        END IF
        STAT='KEEP'
        NCSTAT=4
        CALL OPENRO(NDFIO,FNAME(1:NCFN),UNKN,DIR,FMTD,80,ERR)
        GDFILE=FNAME
        NCGD=NCFN
        GO TO 650
      END IF
      IF(NLINEU(6:9).NE.'DATA')THEN
        EXI=.TRUE.
        WRITE(*,'(/A)')'Header in G & D-V Preprocessor file is invalid.'
        GO TO 100
      END IF
C
C ----- FOUND A C2 DATA FILE
C
      IF(NCI.GT.0)THEN
        STAT='KEEP'
        NCSTAT=4
        GDFILE=PFILE
        NCGD=ILNB( GDFILE )
      ELSE
        STAT='DELETE'
        NCSTAT=6
        GO TO 660
      END IF
  650 CONTINUE
      IF(ECHO)THEN
        WRITE(*,'(A)')
        CALL SHONAM('Geometry & Driver-Vehicle Preprocessor '//
     1              'file is',GDFILE,78)
      END IF
  660 CONTINUE
      READ(NDFIO,IOFORM,REC=1)NLINE
C
C ----- DATA FILE IS IN SECOND CONFIGURATION
C
      READ(NLINE(16:),'(11I5)')(NOUT(I),I=6,16)
      HDR=NLINE(1:15)
      NOUT(4)=NOUT(6)
      IF(NOUT(8).LT.-1)THEN
        DIAMON=.TRUE.
        NOUT(8)=-NOUT(8)
      END IF
      NFUT=0
C
C-----  READ VERSION CARD
C       gdvsim GDV_Ver
C
      IF(NOUT(6).GE.3)THEN
        READ(NDFIO,IOFORM,REC=2)IVERSN
      END IF
C
C ----- READ TITLE CARD
C       gdvsim TX_Title
C
      READ(NDFIO,IOFORM,REC=NOUT(6))NLINE
C
C ----- OPEN UNIT <IOF> FOR DATA  BEING CONVERTED TO CONFIGURATION 1
C
      CALL OPENTF(IOF,CFILE,ERR)
      IF(ERR)THEN
        WRITE(*,'(A)')
        CALL SHONAM('Converted Geometry & Driver-Vehicle '//
     1              'data file is',CFILE,78)
        CALL OPNERR(CFILE)
        GO TO 9510
      END IF
      REWIND IOF
C
C ----- WRITE TITLE CARD
C
      WRITE(IOF,IOFORM,ERR=670)NLINE//IVERSN
      GO TO 675
  670 CONTINUE
      WRITE(*,'(A)')
      CALL SHONAM('Converted Geometry & Driver-Vehicle '//
     1            'data file is',CFILE,78)
      CALL OPNERR(CFILE)
      GO TO 9510
  675 CONTINUE
C
C ----- READ PARAMETER-OPTION CARD
C       gdvsim GDV_Par_Opt
C
      READ(NDFIO,IOFORM,REC=NOUT(6)+1)NLINE
      NC=ILNB( NLINE )
      IMAGEF = NLINE(37:39)
      IMAGEU = IMAGEF
      CALL  TOUPR   ( IMAGEU )
C
C ----- NUM LEGS, NUM VEHICLE CLASSES, NUM DRIVER CLASSES, & NUM VEHICLE
C ----- ATTRIBUTES
C
      READ(NLINE,'(I4,8X,2I4,19X,I2)')NLEGS,NVEHCL,NDRICL,NVEHAT
                    IF ( NVEHAT . EQ . 0 )       NVEHAT = 6
C
C ----- READ LEG DATA FOR EACH LEG
C
      J=0
      NOTBAP=0
      IF(DIAMON)THEN
C
C ----- READ GEOMETRY DATA FOR INTERNAL LANES
C       gdvsim GDV_DiamondLeg
C
C   ICSPA  - SPACING BETWEEN INTERSECTION CENTERS
C   ILNIR  - NUMBER OF LANES      INBOUND TO CENTER R
C   ILNIL  -                                 CENTER L
C   ILSLIR - SPEED LIMIT ON LANES INBOUND TO CENTER R
C   ILSLIL -                                 CENTER L
C   ILMW   - MEDIAN WIDTH
C
        READ(NDFIO,'(4X,3I4,2A,I4)',REC=NOUT(8))ICSPA,ILNIR,
     1                                   ILNIL,ILSLIR,ILSLIL,ILMW
C
C ----- BUILD LEG GEOMETRY DATA FOR THE TWO PSEUDO-LEGS:
C                                               \
C   LEG 0 - APPROACH  4 INBOUND TO CENTER R       \
C           APPROACH 12 OUTBOUND FROM CENTER R      \  W O W !
C   LEG 7 - APPROACH  5 INBOUND TO CENTER L
C           APPROACH 13 OUTBOUND FROM CENTER L
C
        CSPAO2=0.5*ICSPA
        ICSPO2=ICSPA/2
        ILNOR=ILNIL
        ILNOL=ILNIR
        AZI(0)=' 270'
        ANGL(0)=PI
        LLENI(0)=ICSPA
        LLENO(0)=ICSPA
        MIL(0)=ILNIR
        MOL(0)=ILNOR
        SLIMI(0)=ILSLIR
        SLIMO(0)=ILSLIL
        CLOFF(0)=0
        MED(0)=ILMW
        ISUANG(0)=' 40 40'
        NVTP(0)=0
        AZI(NLGP1)='  90'
        ANGL(NLGP1)=0.0
        LLENI(NLGP1)=ICSPA
        LLENO(NLGP1)=ICSPA
        MIL(NLGP1)=ILNIL
        MOL(NLGP1)=ILNOL
        SLIMI(NLGP1)=ILSLIL
        SLIMO(NLGP1)=ILSLIR
        CLOFF(NLGP1)=0
        MED(NLGP1)=ILMW
        ISUANG(NLGP1)=' 40 40'
        NVTP(NLGP1)=0
        IF(ILNIR.GT.0)THEN
          J=J+1
          LIA(J)=LIAD(0)
          NOTBAP=NOTBAP+1
          LOA(NOTBAP)=LOAD(NLGP1)
        END IF
        IF(ILNIL.GT.0)THEN
          J=J+1
          LIA(J)=LIAD(NLGP1)
          NOTBAP=NOTBAP+1
          LOA(NOTBAP)=LOAD(0)
        END IF
        INDX1=0
        INDX2=NLGP1
        IF(IABS(NOUT(8))-NOUT(7).GE.3)THEN
C
C ----- READ DATA FOR FREE U-TURN
C       gdvsim GDV_FreeUTurns
C ----- FIRST SUBSCRIPT = 1   FREE U-TURN FROM LEG 3 TO LEG 4
C -----                   2   FREE U-TURN FROM LEG 6 TO LEG 1
C ----- SECOND SUBSCRIPT = 1  ENTRANCE
C -----                    2  EXIT
C
          READ(NDFIO,'(I2,6I3,A)',REC=NOUT(7)+1)IUWDTH(1)  ,IUSPA(1)  ,
     1                                          IULEN (1,1),IURAD(1,1),
     2                                          IULEN (1,2),IURAD(1,2),
     3                                          IUPCT (1)  ,IUAVT(1)
          READ(NDFIO,'(I2,6I3,A)',REC=NOUT(7)+2)IUWDTH(2)  ,IUSPA(2)  ,
     1                                          IULEN (2,1),IURAD(2,1),
     2                                          IULEN (2,2),IURAD(2,2),
     3                                          IUPCT (2)  ,IUAVT(2)
          NFUT = 0
          IF ( IUWDTH(1) . GT . 0 )  NFUT = NFUT + 1
          IF ( IUWDTH(2) . GT . 0 )  NFUT = NFUT + 2
        END IF
      ELSE
        INDX1=1
        INDX2=NLEGS
        AZI(0)='    '
        ANGL(0)=0.0
        LLENI(0)=0
        LLENO(0)=0
        MIL(0)=0
        MOL(0)=0
        SLIMI(0)='   0'
        SLIMO(0)='   0'
        CLOFF(0)=0
        MED(0)=0
        ISUANG(0)='      '
        NVTP(0)=0
        AZI(NLGP1)='    '
        ANGL(NLGP1)=0.0
        LLENI(NLGP1)=0
        LLENO(NLGP1)=0
        MIL(NLGP1)=0
        MOL(NLGP1)=0
        SLIMI(NLGP1)='   0'
        SLIMO(NLGP1)='   0'
        CLOFF(NLGP1)=0
        MED(NLGP1)=0
        ISUANG(NLGP1)='      '
        NVTP(NLGP1)=0
        DO 677 I=0,NLGP1
C
C ----- NOT CONSISTENT WITH THE ORIGINAL USE OF LOAD
C ----- USED TO SAVE STORAGE
C
        LOAD(I)=-1
  677   CONTINUE
      END IF
      DO 680 I=1,NLEGS
C
C ----- READ LEG DATA
C       gdvsim Leg_Geo
C
      READ(NDFIO,'(4X,A4,4I4,2A4,2I4,A6,I2)',REC=NOUT(8+I))
     1    AZI(I),LLENI(I),LLENO(I),MIL(I),MOL(I),SLIMI(I),SLIMO(I),
     2    CLOFF(I),MED(I),ISUANG(I),NVTP(I)
      READ(AZI(I),IFMT)IT1
      IF(IT1.LT.0)IT1=IT1+360
      IF(IT1.GT.360)IT1=IT1-360
      WRITE(AZI(I),IFMT)IT1
      IT1=90-IT1
      IF(IT1.LT.0)IT1=IT1+360
      ANGL(I)=IT1*DEG2RD
      NIL1=MIL(I)
      NOL1=MOL(I)
C
C ----- TURN BAY LANES FOR FREE U-TURN
C
      IF((I.EQ.1) .AND. (IUWDTH(2).GT.0))NOL1=NOL1+1
      IF((I.EQ.3) .AND. (IUWDTH(1).GT.0))NIL1=NIL1+1
      IF((I.EQ.4) .AND. (IUWDTH(1).GT.0))NOL1=NOL1+1
      IF((I.EQ.6) .AND. (IUWDTH(2).GT.0))NIL1=NIL1+1
      IF(NIL1.GT.0)THEN
        J=J+1
        IF(DIAMON)THEN
          LIA(J)=LIAD(I)
        ELSE
          LIA(J)=I
          LIAD(I)=I
        END IF
      END IF
      IF(NOL1.GT.0)THEN
        NOTBAP=NOTBAP+1
        IF(DIAMON)THEN
          LOA(NOTBAP)=LOAD(I)
        ELSE
          LOA(NOTBAP)=I+NLEGS
          LOAD(I)=I+NLEGS
        END IF
      END IF
  680 CONTINUE
      IF(IUWDTH(1).GT.0)THEN
C
C ----- INBOUND AND OUTBOUND APPROACH FOR FREE U-TURN FROM LEG 3 TO LEG 4
C
        J=J+1
        LIA(J)=1
        NOTBAP=NOTBAP+1
        LOA(NOTBAP)=11
        DESTD(1) = 'L'
      END IF
      IF(IUWDTH(2).GT.0)THEN
C
C ----- INBOUND AND OUTBOUND APPROACH FOR FREE U-TURN FROM LEG 6 TO LEG 1
C
        J=J+1
        LIA(J)=6
        NOTBAP=NOTBAP+1
        LOA(NOTBAP)=16
        DESTD(6) = 'R'
      END IF
C
C                 DIAMOND INTERSECTION NUMBERING CONVENTION
C
C          |               /|\                |              |
C          6                1                 |              |
C         \|/               |               6 | 4          1 | 2
C   <-5->     <-7->  <-0->     <-2->      ----|--------------|----
C          |               /|\              5 | 4          1 | 3
C          4                3                 |              |
C         \|/               |                 |              |
C
C              LEG NUMBERS               CURB RETURN RADIUS NUMBERS
C
C
C          |               /|\                  |               /|\
C          8                9                   8  -16->  -6-->  9
C         \|/               |                  \|/               |
C   <-15-     <--5-  <-12-     <-2--     <-15-     <--5-  <-12-     <-2--
C   --7->     -13->  --4->     -10->     --7->     -13->  --4->     -10->
C          |               /|\                  |               /|\
C         14                3                  14  <--1-  <-11-  3
C         \|/               |                  \|/               |
C
C           APPROACH NUMBERS                     APPROACH NUMBERS
C                                               (WITH FREE U-TURNS)
C
C
C ----- WRITE NUMBER OF INBOUND APPROACHES
C
      IF(DIAMON)THEN
        WRITE(IOF,'(I4,A4)')J,' DIA'
      ELSE
        WRITE(IOF,IFMT)J
      END IF
C
C ----- WRITE LIST OF INBOUND APPROACHES
C
      WRITE(IOF,IFMT)(LIA(I),I=1,J)
C
C ----- WRITE NUMBER OF OUTBOUND APPROACHES
C
      WRITE(IOF,IFMT)NOTBAP,NLEGS,NFUT
C
C ----- WRITE LIST OF OUTBOUND APPROACHES
C
      WRITE(IOF,IFMT)(LOA(I),I=1,NOTBAP)
C
C ----- PUT NUMBER OF APPROACHES INTO DATA LINE
C
      NUNAPR=J+NOTBAP
      WRITE(NLINE(1:4),IFMT)NUNAPR
C
C ----- WRITE PARAMETER-OPTIONS
C
      WRITE(IOF,IOFORM)NLINE(1:NC)
C
C ----- INTERSECTION CENTER OFFSETS SO COORDINATES WILL BE POSITIVE
C
      IXOFFT=XYCNTR
      IXOFF=IXOFFT
      IYOFF=IXOFFT
C
C ----- READ CURB RETURN RADII
C       gdvsim Curb_Ret
C
      READ(NDFIO,'(6I4)',REC=NOUT(7))(CRR(I),I=1,NLEGS)
C
C ----- PROCESSING SEQUENCE REQUIRE PRE-CALCULATING THESE
C
      ISLW=0
      DO 685 I=1,MIL(0)
C
C ----- READ LANE WIDTH FROM LANES INBOUND TO CENTER R
C       gdvsim GDV_DiamondLane
C
      READ(NDFIO,'(I4)',REC=NOUT(8)+I)IT1
      ISLW=ISLW+IT1
  685 CONTINUE
      IYU(1)=IYOFF-NINT( 0.5*MED(0) )-ISLW-IUWDTH(1)-IUSPA(1)
      ISLW=0
      DO 690 I=1,MIL(NLGP1)
C
C ----- READ LANE WIDTH FROM LANES INBOUND TO CENTER L
C       gdvsim GDV_DiamondLane
C
      READ(NDFIO,'(I4)',REC=NOUT(8)+MIL(0)+I)IT1
      ISLW=ISLW+IT1
  690 CONTINUE
      IYU(2)=IYOFF+NINT( 0.5*MED(0) )+ISLW+IUWDTH(2)+IUSPA(2)
      D=IUWDTH(2)-0.5*MED(6)-CLOFF(6)
      R=IURAD(2,1)
      ALPHA=ANGL(6)
      YR=IYU(2)+R
      XC=IXOFFT-CSPAO2
      YC=IXOFFT
      THETA=PI-ALPHA
      T1=PID2-THETA
      D1=YR-YC-(D+R)*SIN(T1)
      X1=XC-D1*TAN(ALPHA-PID2)
      IXUL(2)=X1+(D+R)*COS(ABS(T1))
      SETB(2,1)=D1/COS(ALPHA-PID2)
C
C ----- MAIN LOOP, PROCESS INBOUND AND PREVIOUS (CCW) OUTBOUND
C
      DO 800 I=INDX1,INDX2
      IF(DIAMON)THEN
        IP=LPD(I)
        ICRRT=ICRRD(I)
        IF(I.EQ.0)THEN
          IXOFF=IXOFFT+ICSPO2
        ELSE
          IF(I.EQ.4)IXOFF=IXOFFT-ICSPO2
        END IF
      ELSE
        IF(I.EQ.1)THEN
          IP=NLEGS
        ELSE
          IP=I-1
        END IF
        ICRRT=I
        ICRRD(I)=I
      END IF
      CLOFT=CLOFF(I)
      CLOFPT=CLOFF(IP)
      IF((I.EQ.3) .AND. (IUWDTH(1).GT.0))CLOFT=CLOFT-IUWDTH(1)
      IF((I.EQ.6) .AND. (IUWDTH(2).GT.0))CLOFT=CLOFT-IUWDTH(2)
      IF((IP.EQ.4) .AND. (IUWDTH(1).GT.0))CLOFPT=CLOFPT+IUWDTH(1)
      IF((IP.EQ.1) .AND. (IUWDTH(2).GT.0))CLOFPT=CLOFPT+IUWDTH(2)
      NIL1=MIL(I)
      NOL1=MOL(IP)
      THT=ANGL(I)
      THTP=ANGL(IP)
      SINTHT=SIN(THT)
      COSTHT=COS(THT)
      SINTHP=SIN(THTP)
      COSTHP=COS(THTP)
      ISLW=0
      MXOF=0
      NILS=1
      IF((I.EQ.3) .AND. (IUWDTH(1).GT.0))THEN
C
C ----- ADD THE FREE U-TURN ENTRANCE BAY
C
        NIL1=NIL1+1
        NILS=2
        ISLW=IUWDTH(1)
        IF(THTP.GT.PI)THEN
          DELTHT=THTP-THT
        ELSE
          DELTHT=PIT2-THT+THTP
        END IF
        D=IUWDTH(1)-0.5*MED(3)-CLOFF(3)
        R=IURAD(1,1)
        ALPHA=ANGL(3)
        YR=IYU(1)-R
        XC=IXOFFT+CSPAO2
        YC=IXOFFT
        THETA=PIT2-ALPHA
        T1=PID2-THETA
        D1=YC-YR-(D+R)*SIN(T1)
        X1=XC+D1*TAN(ALPHA-PIT1P5)
        IXUR(1)=X1-(D+R)*COS(ABS(T1))
        SETB(1,1)=D1/COS(ALPHA-PIT1P5)
      END IF
      IF((I.EQ.6) .AND. (IUWDTH(2).GT.0))THEN
C
C ----- ADD THE FREE U-TURN ENTRANCE BAY
C
        NIL1=NIL1+1
        NILS=2
        ISLW=IUWDTH(2)
      END IF
      IF(NIL1.EQ.0)THEN
        IOFC=0
        LSTOF=0
      ELSE
        IREC=NOUT(I+8)
        DO 715 IT1=NILS,NIL1
C
C ----- READ CURRENT INBOUND LANE DATA
C
C ----- INTERNAL LANES (0 OR 7):
C       gdvsim GDV_DiamondLane
C   COL  1- 4   LANE WIDTH
C        5- 6   MOVEMENT CODE        RIGHT INTERSECTION
C        7- 8   MOVEMENT CODE        LEFT  INTERSECTION
C        9-12   USEABLE LENGTH FROM  RIGHT INTERSECTION
C       13-16   USEABLE LENGTH FROM  LEFT  INTERSECTION
C       17-20   OFFSET DISTANCE NEAR RIGHT INTERSECTION
C       21-24   OFFSET DISTANCE NEAR LEFT  INTERSECTION
C       25-28   VEHICLE TYPES ALLOWED TO USE THIS LANE
C
C ----- EXTERNAL LANES (1 THRU 6):
C       gdvsim Lane_Data
C   COL  1- 4   LANE WIDTH
C        5- 8   MOVEMENT CODE
C        9-12   USEABLE LENGTH AT INTERSECTION END
C       13-16   USEABLE LENGTH AT OUTER        END
C       17-20   STOP LINE OFFSET
C       21-24   PERCENT OF TRAFFIC TO ENTER ON THIS LANE
C       25-28   VEHICLE TYPES ALLOWED TO USE THIS LANE
C
        IF(I.EQ.0)THEN
C
C ----- READ FROM LANES INBOUND TO CENTER R
C       gdvsim GDV_DiamondLane
C
          LID(IT1)=' '
          READ(NDFIO,'(A,2X,A)',REC=NOUT(8)+IT1)
     1         LID(IT1)(1:6),LID(IT1)(9:28)
          GO TO 708
        END IF
        IF(I.EQ.NLGP1)THEN
C
C ----- READ FROM LANES INBOUND TO CENTER L
C       gdvsim GDV_DiamondLane
C
          LID(IT1)=' '
          READ(NDFIO,'(A,2X,6A)',REC=NOUT(8)+MIL(0)+IT1)
     1         LID(IT1)(1:4),LID(IT1)(5:6),LID(IT1)(13:16),
     2         LID(IT1)(9:12),LID(IT1)(21:24),LID(IT1)(17:20),
     3         LID(IT1)(25:28)
          GO TO 708
        END IF
        GO TO 710
  708   CONTINUE
C
C ----- AT END WHERE TRAFFIC ENTERS LANES
C
        READ(LID(IT1),'(I4,16X,I4)')IT2,LSTOF
        IF(LSTOF.GT.MXOF)MXOF=LSTOF
        GO TO 712
  710   CONTINUE
C
C ----- READ EXTERNAL LANES (1 THRU 6):
C       gdvsim Lane_Data
C
        IREC=IREC+1
        READ(NDFIO,IOFORM,REC=IREC)LID(IT1)
        READ(LID(IT1),'(I4)')IT2
  712   CONTINUE
C
C   LSTOF - STOP LINE OFFSET FOR CURB LANE
C   IOFC - STOP LINE OFFSET FOR MEDIAN LANE
C   ISLW - SUM OF LANE WIDTHS
C   MXOF - MAX OFFSET AT END WHERE TRAFFIC ENTERS (INTERNAL LANES ONLY)
C
        ISLW=ISLW+IT2
  715   CONTINUE
      END IF
      ISLWP=0
      MXOFP=0
      IOFCP=0
      LSTOFP=0
      NOLS=1
      IF((IP.EQ.4) .AND. (IUWDTH(1).GT.0))THEN
C
C ----- ADD THE FREE U-TURN EXIT BAY
C
        NOL1=NOL1+1
        NOLS=2
        ISLWP=IUWDTH(1)
        D=IUWDTH(1)-0.5*MED(4)+CLOFF(4)
        R=IURAD(1,2)
        ALPHA=ANGL(4)
        YR=IYU(1)-R
        XC=IXOFFT-CSPAO2
        YC=IXOFFT
        THETA=ALPHA-PI
        T1=PID2-THETA
        D1=YC-YR-(D+R)*SIN(T1)
        X1=XC+D1*TAN(ALPHA-PIT1P5)
        IXUL(1)=X1+(D+R)*COS(T1)
        SETB(1,2)=D1/COS(ALPHA-PIT1P5)
      END IF
      IF((IP.EQ.1) .AND. (IUWDTH(2).GT.0))THEN
C
C ----- ADD THE FREE U-TURN EXIT BAY
C
        NOL1=NOL1+1
        NOLS=2
        ISLWP=IUWDTH(2)
        D=IUWDTH(2)-0.5*MED(1)+CLOFF(1)
        R=IURAD(2,2)
        ALPHA=ANGL(1)
        YR=IYU(2)+R
        XC=IXOFFT+CSPAO2
        YC=IXOFFT
        THETA=ALPHA
        T1=PID2-THETA
        D1=YR-YC-(D+R)*SIN(T1)
        X1=XC+D1*TAN(PID2-ALPHA)
        IXUR(2)=X1-(D+R)*COS(ABS(T1))
        SETB(2,2)=D1/COS(ALPHA-PID2)
      END IF
      IREC=NOUT(IP+8)+MIL(IP)
      DO 720 IT1=NOLS,NOL1
C
C ----- READ PREVIOUS OUTBOUND LANE DATA
C
C ----- INTERNAL LANES (0 OR 7):
C       gdvsim GDV_DiamondLane
C   COL  1- 4   LANE WIDTH
C        5- 6   MOVEMENT CODE        RIGHT INTERSECTION
C        7- 8   MOVEMENT CODE        LEFT  INTERSECTION
C        9-12   USEABLE LENGTH FROM  RIGHT INTERSECTION
C       13-16   USEABLE LENGTH FROM  LEFT  INTERSECTION
C       17-20   OFFSET DISTANCE NEAR RIGHT INTERSECTION
C       21-24   OFFSET DISTANCE NEAR LEFT  INTERSECTION
C       25-28   VEHICLE TYPES ALLOWED TO USE THIS LANE
C
C ----- EXTERNAL LANES (1 THRU 6):
C       gdvsim Lane_Data
C   COL  1- 4   LANE WIDTH
C        5- 8   MOVEMENT CODE
C        9-12   USEABLE LENGTH AT INTERSECTION END
C       13-16   USEABLE LENGTH AT OUTER        END
C       17-20   STOP LINE OFFSET
C       21-24   PERCENT OF TRAFFIC TO ENTER ON THIS LANE
C       25-28   VEHICLE TYPES ALLOWED TO USE THIS LANE
C
      IF(IP.EQ.0)THEN
C
C ----- READ FROM LANES INBOUND TO CENTER L
C       gdvsim GDV_DiamondLane
C
        LOD(IT1)=' '
        READ(NDFIO,'(A,2X,A)',REC=NOUT(8)+MIL(0)+IT1)
     1       LOD(IT1)(1:6),LOD(IT1)(9:28)
        GO TO 718
      END IF
      IF(IP.EQ.NLGP1)THEN
C
C ----- READ FROM LANES INBOUND TO CENTER R
C       gdvsim GDV_DiamondLane
C
        LOD(IT1)=' '
        READ(NDFIO,'(A,2X,6A)',REC=NOUT(8)+IT1)
     1       LOD(IT1)(1:4),LOD(IT1)(5:6),LOD(IT1)(13:16),
     2       LOD(IT1)(9:12),LOD(IT1)(21:24),LOD(IT1)(17:20),
     3       LOD(IT1)(25:28)
        GO TO 718
      END IF
C
C ----- READ EXTERNAL LANES (1 THRU 6):
C       gdvsim Lane_Data
C
      IREC=IREC+1
      READ(NDFIO,IOFORM,REC=IREC)LOD(IT1)
  718 CONTINUE
      READ(LOD(IT1),'(I4,12X,I4)')IT2,LSTOFP
      IF(IT1.EQ.1)IOFCP=LSTOFP
      IF(LSTOFP.GT.MXOFP)MXOFP=LSTOFP
      ISLWP=ISLWP+IT2
  720 CONTINUE
      IF(DIAMON)GO TO 730
C
C ----- CHECK FOR PARALLEL (+/- 20 DEG) LEGS
C
      XT2=ABS(THTP-THT)
      IF(XT2.LT.0.35)GO TO 725
      XT3=ABS(XT2-PI)
      IF(XT3.LT.0.35)GO TO 725
      XT3=ABS(XT2-PIT2)
      IF(XT3.GE.0.35)GO TO 730
  725 CONTINUE
C
C ----- LEGS ARE PARALLEL (+/- 20 DEG), ASSUME CURB RETURN RADIUS TO
C ----- BE USED AS A SETBACK FROM THE INTERSECTION CENTER FOR LOCATING
C ----- THE NOMINAL STOPLINE (NEGATIVE CRR WILL DO THIS).
C
      PARLG=.TRUE.
      ITCRR=ABS(CRR(ICRRT))
      ITCRR=-ITCRR
      CRR(ICRRT)=ITCRR
      GO TO 734
  730 CONTINUE
      PARLG=.FALSE.
C
C ----- CHECK FOR NEGATIVE CURB RETURN RADIUS
C
      ITCRR=CRR(ICRRT)
      IF(ITCRR.GE.0)GO TO 736
  734 CONTINUE
C
C ----- PROCESS NEGATIVE CRR FOR INBOUND LANE
C
      SETBT=IABS(ITCRR)
      NEGCRR=.TRUE.
      XX1=CLOFT+0.5*MED(I)
      XX=THT-PID2
      XT=IXOFF-XX1*COS(XX)
      YT=IYOFF-XX1*SIN(XX)
      XT=XT-ITCRR*COSTHT
      YT=YT-ITCRR*SINTHT
      GO TO 774
  736 CONTINUE
      NEGCRR=.FALSE.
C
C ----- CALCULATE PERPENDICULAR FROM LEG CL TO CRR CENTER
C
      XT2=THTP-THT
      IF(XT2.LT.0.)XT2=XT2+PIT2
      ISIGN=1
      IF(XT2.GT.PI)ISIGN=-1
      A=CLOFT+0.5*MED(I)+ISIGN*ITCRR
      AP=CLOFPT-0.5*MED(IP)-ISIGN*ITCRR
      A=A+ISLW
      AP=AP-ISLWP
C
C ----- CHECK FOR INFINITE (+/- .5 DEG) SLOPE OF LEG CL'S
C
      IF(ABS(THT-PID2).LT.0.01)THEN
        A1=-A
        GO TO 752
      END IF
      IF(ABS(THT-PIT1P5).LT.0.01)GO TO 750
      IF(ABS(XT2).LT..01)THEN
        A1=-AP
        GO TO 742
      END IF
      IF(ABS(THTP-PIT1P5).GE.0.01)GO TO 760
C
C ----- PREVIOUS LEG ANGLE CLOSE TO 0 OR 180, SLOPE CLOSE TO INFINITY
C
      A1=AP
  742 CONTINUE
      XCRRC=A1
      YCRRC=A1*TAN(THT)+A/COSTHT
      GO TO 770
  750 CONTINUE
C
C ----- CURRENT LEG ANGLE CLOSE TO 0 OR 180, SLOPE CLOSE TO INFINITY
C
      A1=A
  752 CONTINUE
      XCRRC=A1
      YCRRC=A1*TAN(THTP)+AP/COSTHP
      GO TO 770
  760 CONTINUE
C
C ----- CALCULATE SLOPE OF LEG CL'S
C
      XM=TAN(THT)
      XMP=TAN(THTP)
C
C ----- CALCULATE Y INTERCEPT OF LEG CL'S
C
      B=A/COSTHT
      BP=AP/COSTHP
C
C ----- CALCULATE CURB RETURN RADIUS CENTER
C
      XCRRC=(BP-B)/(XM-XMP)
      YCRRC=XM*XCRRC+B
  770 CONTINUE
C
C ----- CALCULATE X & Y COORDINATE FOR CURRENT INBOUND APPROACH
C
      XCRRC=XCRRC+IXOFF
      YCRRC=YCRRC+IYOFF
      XX1=THT-PID2
      XX2=ISIGN*ITCRR+ISLW
      XT=XCRRC+XX2*COS(XX1)
      YT=YCRRC+XX2*SIN(XX1)
      IF(((I.EQ.3) .AND. (IUWDTH(1).GT.0)) .OR.
     1   ((I.EQ.6) .AND. (IUWDTH(2).GT.0)))THEN
C
C ----- PERPENDICULAR FROM NOMINAL STOP LINE TO INTERSECTION CENTER
C
        SETBT=SQRT((XT-IXOFF)**2+(YT-IYOFF)**2-0.5*(MED(I)+CLOFF(I))**2)
      END IF
  774 CONTINUE
      IF((I.EQ.3) .AND. (IUWDTH(1).GT.0))THEN
        IOFT=NINT( SETBT-SETB(1,1) )
        WRITE(LID(1),'(I4,A4,4I4)')IUWDTH(1),'U   ',IULEN(1,1),0,
     1                             IOFT,0
        IF(IOFT.GT.MXOF)MXOF=IOFT
      END IF
      IF((I.EQ.6) .AND. (IUWDTH(2).GT.0))THEN
        IOFT=NINT( SETBT-SETB(2,1) )
        WRITE(LID(1),'(I4,A4,4I4)')IUWDTH(2),'U   ',IULEN(2,1),0,
     1                             IOFT,0
        IF(IOFT.GT.MXOF)MXOF=IOFT
      END IF
      READ(LID(1)(17:20),IFMT)IOFC
      IF(NIL1.GT.1)THEN
        READ(LID(NIL1)(17:20),IFMT)LSTOF
      ELSE
        LSTOF=IOFC
      END IF
      IF(MED(I).GT.0)THEN
C
C ----- ONE END POINT FOR LINE TO CLOSE MEDIAN
C
        IXC2(I)=NINT( XT-IOFC*COSTHT )
        IYC2(I)=NINT( YT-IOFC*SINTHT )
      ELSE
        IF(NIL1.EQ.0)THEN
C
C ----- END OF CURB RETURN
C ----- TO BE TIED TO CORNER OF OUTBOUND LANES OF CURRENT LEG
C
          IXC2(I)=NINT( XT )
          IYC2(I)=NINT( YT )
        END IF
        IF(MOL(I).EQ.0)THEN
C
C ----- CORNER OF INBOUND LANE 1
C ----- TO BE TIED TO END OF NEXT CURB RETURN
C
          IXC1(I)=NINT( XT-IOFC*COSTHT )
          IYC1(I)=NINT( YT-IOFC*SINTHT )
        END IF
      END IF
      IF((I.EQ.3) .AND. (IUWDTH(1).GT.0))THEN
        IXC1(I)=IXC1(I)+NINT( IUWDTH(1)*COS(THT+PID2) )
        IYC1(I)=IYC1(I)+NINT( IUWDTH(1)*SIN(THT+PID2) )
      END IF
      IF((I.EQ.6) .AND. (IUWDTH(2).GT.0))THEN
        IXC1(I)=IXC1(I)+NINT( IUWDTH(2)*COS(THT+PID2) )
        IYC1(I)=IYC1(I)+NINT( IUWDTH(2)*SIN(THT+PID2) )
      END IF
      XL=XT+LLENI(I)*COSTHT
      YL=YT+LLENI(I)*SINTHT
      XX=THT+PID2
      XT=XT+ISLW*COS(XX)
      YT=YT+ISLW*SIN(XX)
      ICPXC=NINT( XT )
      ICPYC=NINT( YT )
      IF(LSTOF.LT.0)THEN
C
C ----- FIND LANE EXTENSION LINE ENDPOINTS
C
        NLNE=NLNE+1
        IXLN1(NLNE)=NINT( XT )
        IYLN1(NLNE)=NINT( YT )
        IXLN2(NLNE)=NINT( XT-LSTOF*COSTHT )
        IYLN2(NLNE)=NINT( YT-LSTOF*SINTHT )
      END IF
      IF(NIL1.EQ.0)GO TO 780
      IXL=NINT( XL )
      IYL=NINT( YL )
C
C ----- SET UP APPROACH CARD DATA FOR CURRENT INBOUND APPROACH
C
      READ(AZI(I),IFMT)IT1
      IT1=IT1+180
      IF(IT1.GE.360)IT1=IT1-360
      WRITE(NLINE,'(4I4,4X,2I2)')LIAD(I),IT1,IXL,IYL,NVTP(I),NIL1
      NLINE(17:20)=SLIMI(I)
      NLINE(25:30)=ISUANG(I)
      IF(DIAMON)THEN
        NLINE(31:31)=DESTD(LIAD(I))
      ELSE
        NLINE(31:31)='L'
      END IF
      IF((I.GT.0) .AND. (I.LE.NLEGS))THEN
C
C ----- PUT TRAFFIC DISTRIBUTION FUNCTION DATA INTO DATA LINE
C       gdvsim Traf_Hdway
C
        IT1=NOUT(I+8)+MIL(I)+MOL(I)+1
        READ(NDFIO,'(A28,A3,I5)',REC=IT1)NLINE(32:59),NLINE(78:80),ISE
        READ(NLINE(44:59),'(F6.0,2F5.0)')(XTEMP(IT2),IT2=1,3)
        WRITE(NLINE(44:59),'(F6.2,2F5.1)')(XTEMP(IT2),IT2=1,3)
C
C ----- TRAFFIC DESTINATION DATA
C       gdvsim Traf_Dest
C
        READ(NDFIO,FMTI3,REC=IT1+INT((NVEHCL+14)/15)+1)
     *                          (IPCT(IT2),IT2=1,NLEGS)
C
C ----- VARYING TRAFFIC PERIODS
C       gdvsim Varying_Traffic_Period
C
        RECVTP = IT1 + INT((NVEHCL+14)/15) + 1
        DO 776  IVTP=1,NVTP(I)
        RECVTP = RECVTP + 1
        READ(NDFIO,IOFORM,REC=RECVTP)LVTP(IVTP)
        DO  JJ = 1 , 6
          LVTPP(IVTP,JJ) = LVTP(IVTP)((57+(3*JJ)):(59+(3*JJ)))
        END DO
        LVTPU=LVTP(IVTP)
        CALL  TOUPR   ( LVTPU )
        IF(LVTPU(78:80).EQ.'YES')THEN
          DO 775  IT5 = 1 , INT((NVEHCL+14)/15)
C
C ----- VARYING TRAFFIC PERIODS TRAFFIC MIX
C       gdvsim Traf_Mix
C
          RECVTP = RECVTP + 1
          READ(NDFIO,IOFORM,REC=RECVTP)PVTP(IVTP,IT5)
  775     CONTINUE
        END IF
  776   CONTINUE
        NLINE(60:77)=' '
        IT2=60
        IT3=IT2+2
        IF(I.EQ.INDX1)THEN
C
C ----- NO DESTINATION DATA FOR FREE U-TURN APPROACHES
C
          IF(IUWDTH(1).GT.0)NOTBAP=NOTBAP-1
          IF(IUWDTH(2).GT.0)NOTBAP=NOTBAP-1
        END IF
        DO 778 IJ1=1,NOTBAP
C
C ----- ONLY FOR LEGS WITH OUTBOUND LANES
C
        IT4=LOA(IJ1)
        DO 777 IJ2=0,NLGP1
            IF(LOAD(IJ2).EQ.IT4)THEN
              IF(.NOT.(DIAMON.AND.(IJ2 .EQ. 0).OR.(IJ2 .EQ. NLGP1)))THEN
C
C ----- NOT INTERNAL APPROACHES
C
                WRITE(NLINE(IT2:IT3),FMTI3)IPCT(IJ2)
                DO  IVTP=1,NVTP(I)
                  LVTP(IVTP)(IT2:IT3) = LVTPP(IVTP,IJ2)
                END DO
              END IF
              IF (IT2 .EQ. 75) THEN
C
C ----- GO BACK AND FILL IN THE SPACES LEFT BY SKIPPING INTERNAL LANES
C ----- MAKE PROVISION FOR ADDITIONAL DATA WITH 2 WAY FRONTAGE ROADS
C ----- AND KEEP COMPATIBILITY WITH 1 WAY FRONTQGE ROAD DATA
C
                IT2=60
              ELSE
                IT2=IT2+3
              END IF
              IT3=IT2+2
            END IF
  777   CONTINUE
  778   CONTINUE
C
C ----- WRITE INBOUND APPROACH CARD
C
 
        IPCTU=0
C
C ----- PERCENT OF U-TURNERS USING FREE U-TURN
C
        IF(I.EQ.3)IPCTU=IUPCT(1)
        IF(I.EQ.6)IPCTU=IUPCT(2)
        WRITE(IOF,'(A80,I3)')NLINE,IPCTU
        IF(IPCTU.GT.0)THEN
          DO  IVTP=1,NVTP(I)
            LVTP(IVTP)(13:30) = '  0' // LVTP(IVTP)(13:27)
          END DO
        END IF
      ELSE
C
C ----- PUT IN DUMMY DATA FOR TRAFFIC DISTRIBUTION FUNCTION NAME
C
        NLINE(32:38)='XXXXXXX'
C
C ----- SAVE DATA FOR LATER
C
        IF(I.EQ.0)THEN
          ILAPIR=NLINE
          ILXIR=IXL
          MXOFIR=MXOF
        ELSE
          ILAPIL=NLINE
          ILXIL=IXL
          MXOFIL=MXOF
        END IF
      END IF
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(NLINEU(78:78).EQ.ICY)THEN
C
C ----- PROCESS TRAFFIC MIX DATA
C       gdvsim Traf_Mix
C
        DO 779  IT5 = 1 , NVEHCL , 15
        READ(NDFIO,'(15F5.1)',REC=IT1+1+IT5/15)
     *                       (XTEMP(IT2),IT2=IT5,MIN0(IT5+14,NVEHCL))
        WRITE(IOF,'(15F5.1)')(XTEMP(IT2),IT2=IT5,MIN0(IT5+14,NVEHCL))
  779   CONTINUE
      END IF
      IF((I.GT.0) .AND. (I.LE.NLEGS))THEN
C
C ----- PROCESS CURRENT INBOUND LANE CARDS
C
        IF(ISE.LE.0)ISE=ISEED(I)
        CALL LCD(ISE,NIL1,LID,LLENI(I),0,IOF)
C
C ----- VARYING TRAFFIC PERIODS
C
      DO 796  IVTP=1,NVTP(I)
      WRITE(IOF,IOFORM)LVTP(IVTP)
      LVTPU=LVTP(IVTP)
      CALL  TOUPR   ( LVTPU )
      IF(LVTPU(78:80).EQ.'YES')THEN
        DO 794  IT5 = 1 , INT((NVEHCL+14)/15)
        WRITE(IOF,IOFORM)PVTP(IVTP,IT5)
  794   CONTINUE
      END IF
  796 CONTINUE
      ELSE
C
C ----- SAVE DATA FOR LATER
C
        IF(I.EQ.0)THEN
          ILNILR=NIL1
          CALL CTOC(LID,ILIDR,NIL1)
        ELSE
          ILNILL=NIL1
          CALL CTOC(LID,ILIDL,NIL1)
        END IF
      END IF
  780 CONTINUE
C
C ----- CALCULATE X & Y FOR PREVIOUS OUTBOUND APPROACH
C
      IF(.NOT.NEGCRR)THEN
        XX=THTP+PID2
        XX1=ISIGN*ITCRR+ISLWP
        XL=XCRRC+XX1*COS(XX)
        YL=YCRRC+XX1*SIN(XX)
      ELSE
        XL=IXOFF-ITCRR*COSTHP
        XX=THTP-PID2
        XX1=0.5*MED(IP)-CLOFPT
        XL=XL+XX1*COS(XX)
        YL=IYOFF-ITCRR*SINTHP
        YL=YL+XX1*SIN(XX)
      END IF
      XT=XL
      YT=YL
      IF(MXOFP.GT.0)THEN
C
C ----- ADJUST X & Y FOR MAX POSITIVE STOP LINE OFFSET
C
        XL=XL-MXOFP*COSTHP
        YL=YL-MXOFP*SINTHP
      END IF
      IF(MED(IP).GT.0)THEN
C
C ----- CORNER OF OUTBOUND LANE 1
C ----- ONE END POINT FOR LINE TO CLOSE MEDIAN
C
        IXC1(IP)=NINT( XL-(IOFCP-MXOFP)*COSTHP )
        IYC1(IP)=NINT( YL-(IOFCP-MXOFP)*SINTHP )
      ELSE
        IF(MIL(IP).EQ.0)THEN
C
C ----- CORNER OF OUTBOUND LANE 1
C ----- TO BE TIED TO END OF PREVIOUS CURB RETURN
C
          IXC1(IP)=NINT( XL+(IOFCP-MXOFP)*COSTHP )
          IYC1(IP)=NINT( YL+(IOFCP-MXOFP)*SINTHP )
        END IF
        IF(NOL1.EQ.0)THEN
C
C ----- END OF CURB RETURN
C ----- TO BE TIED INTO CORNER OF INBOUND LANES ON PREVIOUS LEG
C
          IXC2(IP)=NINT( XT )
          IYC2(IP)=NINT( YT )
        END IF
      END IF
      IF((IP.EQ.4) .AND. (IUWDTH(1).GT.0))THEN
        IXC1(IP)=IXC1(IP)+NINT( IUWDTH(1)*COS(THTP-PID2) )
        IYC1(IP)=IYC1(IP)+NINT( IUWDTH(1)*SIN(THTP-PID2) )
      END IF
      IF((IP.EQ.1) .AND. (IUWDTH(2).GT.0))THEN
        IXC1(IP)=IXC1(IP)+NINT( IUWDTH(2)*COS(THTP-PID2) )
        IYC1(IP)=IYC1(IP)+NINT( IUWDTH(2)*SIN(THTP-PID2) )
      END IF
      IF(NOL1.EQ.0)GO TO 790
      IXL=NINT( XL )
      IYL=NINT( YL )
      XX=THTP-PID2
      XT=XL+ISLWP*COS(XX)
      YT=YL+ISLWP*SIN(XX)
      IF(((IP.EQ.1) .AND. (IUWDTH(2).GT.0)) .OR.
     1   ((IP.EQ.4) .AND. (IUWDTH(1).GT.0)))THEN
C
C ----- PERPENDICULAR FROM NOMINAL STOP LINE TO INTERSECTION CENTER
C
        SETBT=SQRT((XL-IXOFF)**2+(YL-IYOFF)**2-
     1             (0.5*MED(IP)-CLOFF(IP))**2)
      END IF
      ICPXP=NINT( XT )
      ICPYP=NINT( YT )
      IF(LSTOFP.LT.0)THEN
C
C ----- FIND LANE EXTENSION LINE ENDPOINTS
C
        NLNE=NLNE+1
        IXLN1(NLNE)=NINT( XT )
        IYLN1(NLNE)=NINT( YT )
        XT=XT-FLOAT(LSTOFP)*COSTHP
        YT=YT-FLOAT(LSTOFP)*SINTHP
        IXLN2(NLNE)=NINT( XT )
        IYLN2(NLNE)=NINT( YT )
      END IF
      IF(PARLG.AND.NEGCRR)THEN
C
C ----- NEARLY PARALLEL LEGS AND NEGATIVE CRR,  CONNECT WITH LINE
C
        NLNE=NLNE+1
        IXLN1(NLNE)=ICPXP
        IYLN1(NLNE)=ICPYP
        IXLN2(NLNE)=ICPXC
        IYLN2(NLNE)=ICPYC
      END IF
C
C ----- SET UP APPROACH CARD DATA FOR PREVIOUS OUTBOUND APPROACH
C
      WRITE(NLINE,'(I4,4X,2I4,4X,I4)')LOAD(IP),IXL,IYL,NOL1
      NLINE(5:8)=AZI(IP)
      NLINE(17:20)=SLIMO(IP)
      IF(DIAMON)THEN
        NLINE(31:31)=DESTD(LOAD(IP))
      ELSE
        NLINE(31:31)='L'
      END IF
      IF((IP.GT.0) .AND. (IP.LE.NLEGS))THEN
        WRITE(IOF,IOFORM)NLINE(1:31)
C
C ----- PROCESS PREVIOUS OUTBOUND LANE CARDS
C
        IF((IP.EQ.1) .AND. (IUWDTH(2).GT.0))THEN
          IOFPT=NINT( SETBT-SETB(2,2) )
          WRITE(LOD(1),'(I4,A4,4I4)')IUWDTH(2),'U   ',IULEN(2,2),0,
     1                               IOFPT,0
          IF(IOFPT.GT.MXOFP)MXOFP=IOFPT
        END IF
        IF((IP.EQ.4) .AND. (IUWDTH(1).GT.0))THEN
          IOFPT=NINT( SETBT-SETB(1,2) )
          WRITE(LOD(1),'(I4,A4,4I4)')IUWDTH(1),'U   ',IULEN(1,2),0,
     1                               IOFPT,0
          IF(IOFPT.GT.MXOFP)MXOFP=IOFPT
        END IF
        CALL LCD(-1,NOL1,LOD,LLENO(IP),MXOFP,IOF)
      ELSE
C
C ----- SAVE DATA FOR LATER
C
        IF(IP.EQ.0)THEN
          ILAPOR=NLINE
          ILXOR=IXL
          ILNOLR=NOL1
          MXOFOR=MXOFP
          CALL CTOC(LOD,ILODR,NOL1)
        ELSE
          ILAPOL=NLINE
          ILXOL=IXL
          ILNOLL=NOL1
          MXOFOL=MXOFP
          CALL CTOC(LOD,ILODL,NOL1)
        END IF
      END IF
  790 CONTINUE
      IF(ITCRR.GT.0)THEN
C
C ----- BUILD CURB RETURN ARC DATA
C
        NRCG=NRCG+1
        IXRC(NRCG)=NINT( XCRRC )
        IYRC(NRCG)=NINT( YCRRC )
        IRRC(NRCG)=ITCRR
        IT1=180-NINT( RAD2DG*THT )
        IF(IT1.LT.0)IT1=IT1+360
        IT2=NINT( RAD2DG*(THTP-THT) )
        IF(THTP.LT.THT)IT2=IT2+360
        IT2=180-IT2
        ISANG(NRCG)=IT2
        IF(IT2.LT.0)IT1=IT1-180
        IF(IT1.LT.0)IT1=IT1+360
        IBAZI(NRCG)=IT1
      END IF
  800 CONTINUE
      IF(DIAMON)THEN
C
C ----- PROCESS APPROACH AND LANE CARDS FOR INTERNAL LANES
C
        IF(ILNIR.GT.0)THEN
C
C ----- APPROACH INBOUND TO CENTER R
C
          ITEMP=ILXOL-ILXIR
          WRITE(ILAPIR(9:12),IFMT)ILXOL
          WRITE(IOF,IOFORM)ILAPIR
          ITEMP=LLENI(0)-ITEMP
          CALL LCD(-2,ILNIR,ILIDR,ITEMP,MXOFIR,IOF)
        END IF
        IF(ILNOR.GT.0)THEN
C
C ----- APPROACH OUTBOUND FROM CENTER R
C
          ITEMP=ILXIL-ILXOR
          WRITE(IOF,IOFORM)ILAPOR
          ITEMP=LLENO(0)-ITEMP
          CALL LCD(-3,ILNOR,ILODR,ITEMP,MXOFOR,IOF)
        END IF
        IF(ILNIL.GT.0)THEN
C
C ----- APPROACH INBOUND TO CENTER L
C
          ITEMP=ILXIL-ILXOR
          WRITE(ILAPIL(9:12),IFMT)ILXOR
          WRITE(IOF,IOFORM)ILAPIL
          ITEMP=LLENI(NLGP1)-ITEMP
          CALL LCD(-2,ILNIL,ILIDL,ITEMP,MXOFIL,IOF)
        END IF
        IF(ILNIR.GT.0)THEN
C
C ----- APPROACH OUTBOUND FROM CENTER L
C
          ITEMP=ILXOL-ILXIR
          WRITE(IOF,IOFORM)ILAPOL
          ITEMP=LLENO(NLGP1)-ITEMP
          CALL LCD(-3,ILNOL,ILODL,ITEMP,MXOFOL,IOF)
        END IF
        IF(IUWDTH(1).GT.0)THEN
C
C ----- FREE U-TURN FROM LEG 3 TO LEG 4
C ----- ARCS FOR EDGES OF U-TURN LANE
C
C ----- ENTRY END
C
          NRCG=NRCG+1
          IXRC(NRCG)=IXUR(1)
          IYRC(NRCG)=IYU(1)-IURAD(1,1)
          IRRC(NRCG)=IURAD(1,1)
          IBAZI(NRCG)=0
          ISANG(NRCG)=360-NINT( RAD2DG*ANGL(3) )
          NRCG=NRCG+1
          IXRC(NRCG)=IXUR(1)
          IYRC(NRCG)=IYU(1)-IURAD(1,1)
          IRRC(NRCG)=IURAD(1,1)+IUWDTH(1)
          IBAZI(NRCG)=0
          ISANG(NRCG)=ISANG(NRCG-1)
C
C ----- EXIT END
C
          NRCG=NRCG+1
          IXRC(NRCG)=IXUL(1)
          IYRC(NRCG)=IYU(1)-IURAD(1,2)
          IRRC(NRCG)=IURAD(1,2)
          IBAZI(NRCG)=0
          ISANG(NRCG)=180-NINT( RAD2DG*ANGL(4) )
          NRCG=NRCG+1
          IXRC(NRCG)=IXUL(1)
          IYRC(NRCG)=IYU(1)-IURAD(1,2)
          IRRC(NRCG)=IURAD(1,2)+IUWDTH(1)
          IBAZI(NRCG)=0
          ISANG(NRCG)=ISANG(NRCG-1)
          ITEMP=IXUR(1)-IXUL(1)
C
C ----- U-TURN APPROACH INBOUND TO LEG 4
C
          WRITE(ULINE,'(I4,A)')IUWDTH(1),'U      0   0   0   0'
          WRITE(IOF,'(I4,A,2I4,A,I4,3A)')1,AZI(0),IXUR(1),IYU(1),
     1                                   SLIMI(3),1,ISUANG(0),DESTD(1),
     2                                   'XXXXXXX'
          CALL LCD(-2,1,ULINE,ITEMP,0,IOF)
C
C ----- U-TURN APPROACH OUTBOUND FROM LEG 3
C
          WRITE(IOF,'(I4,A,2I4,A,I4,6X,A)')11,AZI(0),IXUR(1),IYU(1),
     1                                     SLIMI(3),1,DESTD(11)
          CALL LCD(-3,1,ULINE,ITEMP,0,IOF)
        END IF
        IF(IUWDTH(2).GT.0)THEN
C
C ----- FREE U-TURN FROM LEG 6 TO LEG 1
C ----- ARCS FOR EDGES OF U-TURN LANE
C
C ----- ENTRY END
C
          NRCG=NRCG+1
          IXRC(NRCG)=IXUL(2)
          IYRC(NRCG)=IYU(2)+IURAD(2,1)
          IRRC(NRCG)=IURAD(2,1)
          IBAZI(NRCG)=180
          ISANG(NRCG)=180-NINT( RAD2DG*ANGL(6) )
          NRCM1=NRCG
          NRCG=NRCG+1
          IXRC(NRCG)=IXUL(2)
          IYRC(NRCG)=IYRC(NRCM1)
          IRRC(NRCG)=IURAD(2,1)+IUWDTH(2)
          IBAZI(NRCG)=180
          ISANG(NRCG)=ISANG(NRCM1)
C
C ----- EXIT END
C
          NRCG=NRCG+1
          IXRC(NRCG)=IXUR(2)
          IYRC(NRCG)=IYU(2)+IURAD(2,2)
          IRRC(NRCG)=IURAD(2,2)
          IBAZI(NRCG)=180
          ISANG(NRCG)=-NINT( RAD2DG*ANGL(1) )
          NRCM1=NRCG
          NRCG=NRCG+1
          IXRC(NRCG)=IXUR(2)
          IYRC(NRCG)=IYRC(NRCM1)
          IRRC(NRCG)=IURAD(2,2)+IUWDTH(2)
          IBAZI(NRCG)=180
          ISANG(NRCG)=ISANG(NRCM1)
          ITEMP=IXUR(2)-IXUL(2)
C
C ----- U-TURN APPROACH INBOUND TO LEG 1
C
          WRITE(ULINE,'(I4,A)')IUWDTH(2),'U      0   0   0   0'
          WRITE(IOF,'(I4,A,2I4,A,I4,3A)')6,AZI(NLGP1),IXUL(2),IYU(2),
     1                                   SLIMI(6),1,ISUANG(NLGP1),
     2                                   DESTD(6),'XXXXXXX'
          CALL LCD(-2,1,ULINE,ITEMP,0,IOF)
C
C ----- U-TURN APPROACH OUTBOUND FROM LEG 6
C
          WRITE(IOF,'(I4,A,2I4,A,I4,6X,A)')16,AZI(NLGP1),IXUL(2),IYU(2),
     1                                     SLIMI(6),1,DESTD(16)
          CALL LCD(-3,1,ULINE,ITEMP,0,IOF)
        END IF
      END IF
C
C ----- PROCESS USER SUPPLIED ARC DATA
C       gdvsim Arc_Hdr
C       gdvsim Arc_Data
C
      IR1=NOUT(15)
      READ(NDFIO,IFMT,REC=IR1)IR2
      NRCU=IR2
      IF(NRCG+NRCU.GT.NAR)NRCU=NAR-NRCG
      IF(NRCU.LT.0)NRCU=0
      WRITE(IOF,IFMT)NRCG+NRCU
      IXOFF=IXOFFT
      IF(NRCU.EQ.0)GO TO 822
      DO 820 I=1,IR2
      READ(NDFIO,IOFORM,REC=IR1+I)NLINE(1:24)
      READ(NLINE(5:12),IFMT)IT2,IT3
C TWR IT2=IT2+IXOFF
C TWR IT3=IT3+IYOFF
      WRITE(NLINE(5:12),IFMT)IT2,IT3
      WRITE(IOF,IOFORM)NLINE(1:24)
  820 CONTINUE
  822 CONTINUE
C
C ----- WRITE CURB RETURN ARC DATA
C
      DO 830 I=1,NRCG
      WRITE(IOF,IFMT)NRCU+I,IXRC(I),IYRC(I),IBAZI(I),ISANG(I),IRRC(I)
  830 CONTINUE
      DO 855 I=INDX1,INDX2
      IF(IYC2(I).NE.-32000)THEN
        IF(NLNE.GE.NLI)GO TO 857
C
C ----- LINES TO CLOSE MEDIANS
C
        IF((IXC1(I).EQ.IXC2(I)) .AND. (IYC1(I).EQ.IYC2(I)))GO TO 855
        NLNE=NLNE+1
        IXLN1(NLNE)=IXC1(I)
        IYLN1(NLNE)=IYC1(I)
        IXLN2(NLNE)=IXC2(I)
        IYLN2(NLNE)=IYC2(I)
      END IF
  855 CONTINUE
  857 CONTINUE
C
C ----- PROCESS USER SUPPLIED LINE DATA
C       gdvsim Line_Hdr
C       gdvsim Line_Data
C
      IR1=IR1+IR2+1
      READ(NDFIO,IFMT,REC=IR1)IR2
      NRCU=IR2
      IF(NRCU+NLNE.GT.NLI)NRCU=NLI-NLNE
      IF(NRCU.LT.0)NRCU=0
      WRITE(IOF,IFMT)NRCU+NLNE
      IF(NRCU.LE.0)GO TO 862
      DO 860 I=1,NRCU
      READ(NDFIO,IOFORM,REC=IR1+I)NLINE(1:20)
      READ(NLINE(5:20),IFMT)IT2,IT3,IT4,IT5
C TWR IT2=IT2+IXOFF
C TWR IT3=IT3+IYOFF
C TWR IT4=IT4+IXOFF
C TWR IT5=IT5+IYOFF
      WRITE(IOF,'(A4,4I4)')NLINE(1:4),IT2,IT3,IT4,IT5
  860 CONTINUE
  862 CONTINUE
      DO 870 I=1,NLNE
C
C ----- WRITE LANE EXTENSION LINES
C
      WRITE(IOF,IFMT)I+NRCU,IXLN1(I),IYLN1(I),IXLN2(I),IYLN2(I)
  870 CONTINUE
C
C ----- PROCESS USER SUPPLIED SIGHT DISTANCE RESTRICTION DATA
C       gdvsim SDR_Hdr
C       gdvsim SDR_Data
C
      IR1=IR1+IR2+1
      READ(NDFIO,IFMT,REC=IR1)NRCU
      NSD=NRCU
      IF(NRCU.EQ.0)GO TO 878
      DO 875 I=1,NRCU
      READ(NDFIO,'(I2,I3,I4)',REC=IR1+I)ISDRL(I),ISDSB(I),ISDOFF(I)
      IF(ISDRL(I).EQ.0)NSD=NSD-1
  875 CONTINUE
  878 CONTINUE
      WRITE(IOF,IFMT)NSD
      NSD=0
      DO 880 I=1,NRCU
      IF(ISDRL(I).EQ.0)GO TO 880
      NSD=NSD+1
      ISDSBT=IABS(ISDSB(I))
      SDANG=ANGL(ISDRL(I))
      SDX=ISDSBT*COS(SDANG)
      SDY=ISDSBT*SIN(SDANG)
      SDANG=SDANG+PID2
      IF(SDANG.GT.PIT2)SDANG=SDANG-PIT2
      SDX=SDX+(ISDOFF(I)+CLOFF(ISDRL(I)))*COS(SDANG)
      IF(DIAMON)THEN
        IF(ISDRL(I).LE.3)THEN
          SDX=SDX+CSPAO2
        ELSE
          SDX=SDX-CSPAO2
        END IF
      END IF
      SDY=SDY+(ISDOFF(I)+CLOFF(ISDRL(I)))*SIN(SDANG)
      WRITE(IOF,IFMT)NSD,IXOFF+NINT( SDX ),IYOFF+NINT( SDY )
  880 CONTINUE
C
C ----- PROCESS PLOT DATA
C       gdvsim GDV_Plot_Opts
C
      IR1=IR1+NRCU+1
      READ(NDFIO,'(3A7,3I4,I2,F5.0)',REC=IR1)NLINE(1:10),
     1     NLINE(11:20),NLINE(21:30),IXL,IYL,IR,IDIST,PPW
      IF(NLINE(21:30).EQ.'SEPARAT   ')NLINE(21:30)='SEPARATE  '
      WRITE(NLINE(31:70),'(3F10.2,I5,F5.2)')FLOAT(IXL),FLOAT(IYL),
     1                                      FLOAT(IR),IDIST,PPW
      WRITE(IOF,IOFORM)NLINE(1:70)
C
C ----- PROCESS DRVMIX, VEHDAT, DRVDAT, VEHLO, AND DRVLO DATA
C       gdvsim GDV_Par_Opt_2
C
      IF((NVEHCL.LE.15).AND.(NDRICL.LE.5))THEN
        NCARDS=1
      ELSE
        NCARDS=1+INT((NVEHCL+25)/26)+1
      END IF
      DRVMIX = 'NO'
      VEHDAT = 'NO'
      DRVDAT = 'NO'
      DO 881 I=1,NCARDS
      IR1=IR1+1
      READ(NDFIO,IOFORM,REC=IR1)NLINE
      IF(I.EQ.1)THEN
        DRVMIX = NLINE(1:3)
        VEHDAT = NLINE(4:6)
        DRVDAT = NLINE(7:9)
      END IF
      NC=ILNB( NLINE )
      WRITE(IOF,IOFORM)NLINE(1:NC)
  881 CONTINUE
      CALL  TOUPR   ( DRVMIX )
      CALL  TOUPR   ( VEHDAT )
      CALL  TOUPR   ( DRVDAT )
C ----- SET NUMBER OF REMAINING CARDS IN INPUT FILE
      NCARDS=NOUT(16)-IR1
C
C ----- PROCESS DRIVER MIX CARDS, IF ANY
C       gdvsim GDV_DriverMix
C
      IF(DRVMIX.EQ.'YES')THEN
        NCARDS=MAX(NCARDS-NVEHCL,0)
        DO 882 I=1,NVEHCL
        IR1=IR1+1
        READ(NDFIO,IOFORM,REC=IR1)NLINE
        NC=ILNB( NLINE )
        WRITE(IOF,IOFORM)NLINE(1:NC)
  882   CONTINUE
      END IF
C
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
C
      IF(VEHDAT.EQ.'YES')THEN
        IF((NVEHAT.LT.10).AND.(NVEHCL.NE.NVCD))THEN
          TAG='NUL'
          WRITE(*,906)NVEHAT,NVEHCL,NVCD
          GO TO 9990
        END IF
        NCFV10=(NVEHCL+ 9)/10
        NCFV20=(NVEHCL+19)/20
        NCFV  =6*NCFV20
        IF(NVEHAT.GE. 7)NCFV=NCFV+NCFV10
        IF(NVEHAT.GE. 8)NCFV=NCFV+NCFV20
        IF(NVEHAT.GE. 9)NCFV=NCFV+NCFV20
        DO 883 I=1,NCFV
        IR1=IR1+1
        READ(NDFIO,IOFORM,REC=IR1)NLINE
        NC=ILNB( NLINE )
        WRITE(IOF,IOFORM)NLINE(1:NC)
        NCARDS=MAX(NCARDS-1,0)
  883   CONTINUE
        IF(NVEHAT.GE.10)THEN
          DO 885 I=1,NVEHCL
          IR1=IR1+1
          READ(NDFIO,IOFORM,REC=IR1)NLINE
          NC=ILNB( NLINE )
          WRITE(IOF,IOFORM)NLINE(1:NC)
          NCARDS=MAX(NCARDS-1,0)
          READ(NLINE,'(I2)')NUNITS
          IF(NUNITS.LE.4)GO TO 885
          DO 884 J=5,NUNITS,4
          IR1=IR1+1
          READ(NDFIO,IOFORM,REC=IR1)NLINE
          NC=ILNB( NLINE )
          WRITE(IOF,IOFORM)NLINE(1:NC)
          NCARDS=MAX(NCARDS-1,0)
  884     CONTINUE
  885     CONTINUE
        END IF
      END IF
C
C ----- PROCESS DRIVER DATA CARDS, IF ANY
C ----- OPER_CHAR AND PIJR TIME
C       gdvsim GDV_Drv_OperChar
C       gdvsim GDV_Drv_PIJRTime
C
      IF(DRVDAT.EQ.'YES')THEN
        NCARDS=MAX(NCARDS-2,0)
        DO 888 I=1,2
        IR1=IR1+1
        READ(NDFIO,IOFORM,REC=IR1)NLINE
        NC=ILNB( NLINE )
        WRITE(IOF,IOFORM)NLINE(1:NC)
  888   CONTINUE
      END IF
C
C ----- PROCESS IMAGE FILE, IF ANY
C ----- IMAGE FILE DATA COMES AFTER ANY SPECIAL VEHICLES IN GDVDATA FILE
C ----- IMAGE FILE DATA IS PUT BEFORE ANY SPECIAL VEHICLES IN GDV FILE
C       gdvsim GDV_Image_File
C
      IF(IMAGEU.EQ.'YES')THEN
        NCARDS=MAX(NCARDS-2,0)
        IM1=IR1+NCARDS+1
        READ(NDFIO,IOFORM,REC=IM1)NLINE
        NC=ILNB( NLINE )
        WRITE(IOF,IOFORM)NLINE(1:NC)
        IM2=IM1+1
        READ(NDFIO,IOFORM,REC=IM2)NLINE
        NC=ILNB( NLINE )
        WRITE(IOF,IOFORM)NLINE(1:NC)
      END IF
C
C ----- PROCESS SPECIAL VEHICLE DATA, IF ANY
C       gdvsim GDV_SpecialVeh
C
      DO 890 I=1,NCARDS
      IR1=IR1+1
      READ(NDFIO,IOFORM,REC=IR1)NLINE
      READ(NLINE,501)QTIMSV,IVHCSV,IDRCSV,IVELSV,IOBLSV,IIBLSV,
     *               ILNSV,IPLOSV,IFUTSV,
     *               FSTMSV,FSLCSV,FSLPSV,FSPSSV,FSDTSV,
     *               FGTMSV,FGATSV,
     *               FRTMSV,FRATSV,
     *               EMERGV
      IF((IOBLSV.LT.1).OR.(IOBLSV.GT.NLEGS))THEN
        TAG='NUL'
        WRITE(*,901)IOBLSV,NLEGS
        GO TO 9990
      END IF
      IF((IIBLSV.LT.1).OR.(IIBLSV.GT.NLEGS))THEN
        TAG='NUL'
        WRITE(*,902)IIBLSV,NLEGS
        GO TO 9990
      END IF
      IOBASV=LOAD(IOBLSV)
      IIBASV=LIAD(IIBLSV)
      IF(FSTMSV.GT.0.0)THEN
        CALL  TOUPR  ( FSLCSV )
        IF(FSLCSV.EQ.'I')THEN
          IF((FSLPSV.LT.1).OR.(FSLPSV.GT.NLEGS))THEN
            TAG='NUL'
            WRITE(*,903)FSLPSV,NLEGS
            GO TO 9990
          END IF
          FSAPSV=LIAD(FSLPSV)
        ELSE IF(FSLCSV.EQ.'L')THEN
          FSAPSV=LIAD(NLGP1)
        ELSE IF(FSLCSV.EQ.'O')THEN
          IF((FSLPSV.LT.1).OR.(FSLPSV.GT.NLEGS))THEN
            TAG='NUL'
            WRITE(*,903)FSLPSV,NLEGS
            GO TO 9990
          END IF
          FSAPSV=LOAD(FSLPSV)
        ELSE IF(FSLCSV.EQ.'P')THEN
          FSAPSV=-FSLPSV
        ELSE IF(FSLCSV.EQ.'R')THEN
          FSAPSV=LIAD(0)
        ELSE
          TAG='NUL'
          WRITE(*,904)FSLCSV
          GO TO 9990
        END IF
      END IF
      CALL  TOUPR   ( EMERGV )
      IF(EMERGV.EQ.'   ')EMERGV='NO '
      IF((EMERGV.NE.'YES').AND.(EMERGV.NE.'NO '))THEN
        TAG='NUL'
        WRITE(*,905)EMERGV
        GO TO 9990
      END IF
      WRITE(IOF,502) QTIMSV,IVHCSV,IDRCSV,IVELSV,IOBASV,IIBASV,
     *               ILNSV,IPLOSV,IFUTSV,
     *               FSTMSV,FSAPSV,FSPSSV,FSDTSV,
     *               FGTMSV,FGATSV,
     *               FRTMSV,FRATSV,
     *               EMERGV
  890 CONTINUE
C
C ----- STORE CONVERTED COORDINATES IN G&D-V C2 FILE
C
C     NOUT(8)=NOUT(16)+1
C     WRITE(NDFIO,'(12I6)',REC=NOUT(8))(IXC2(I),IYC2(I),I=1,NLEGS)
C     IF(DIAMON)NOUT(8)=-NOUT(8)
C     WRITE(NDFIO,'(A,11I5)',REC=1)HDR,(NOUT(I),I=6,16)
 
 
      ENDFILE(IOF)
      CLOSE(NDFIO,STATUS=STAT(1:NCSTAT))
      INQUIRE(IOF,NAME=FNAME)
      IF(CLDATA)THEN
        TAG='   '
        GO TO 9000
      END IF
      NCFN=ILNB( FNAME )
C
C ----- REOPEN UNIT 21 WITH SYSTEM DEFAULT NAME FOR POINTER TO
C ----- FILE CONVERTED TO CONFIGURATION 1
C
      CALL OPENDF(NDFIO,TEXFN,80,ERR)
      IF(ERR)THEN
        CALL OPNERR(TEXFN)
        GO TO 9510
      END IF
      TAGU=TAG
      CALL  TOUPR   ( TAGU )
      WRITE(NDFIO,IOFORM,REC=1)' #%&$FILEGDV'//TAGU//'C1 ='//
     1                         FNAME(1:NCFN)
      WRITE(NDFIO,IOFORM,REC=2)GDFILE(1:NCGD)
      CLOSE(NDFIO,STATUS='KEEP')
 9000 CONTINUE
      IF(JOF.GT.0)GO TO 9990
      WRITE(*,'(A)')
      CALL SHONAM('Converted Geometry & Driver-Vehicle '//
     1            'data file is',FNAME,78)
      REWIND(IOF)
      NLINE='  '
      READ(IOF,'(A)',END=9500)NLINE
      GO TO 9550
 9500 CONTINUE
      NCCF=ILNB( CFILE )
      WRITE(*,'(3A/)')'File "',CFILE(1:NCCF),'" is empty.'
 9510 CONTINUE
      TAG='NUL'
      GO TO 9990
 9550 CONTINUE
      WRITE(*,'(/A/A/)')'Converted data title text:',NLINE
 9990 CONTINUE
      REWIND(IOF)
 9999 CONTINUE
      RETURN
      END                                                               GDVCON
C
C
C
      SUBROUTINE CTOC(CIN,COUT,N)
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)   CIN(*),
     1                    COUT(*)
      INTEGER                  N
      INTEGER I
      DO 100 I=1,N
      COUT(I)=CIN(I)
  100 CONTINUE
      RETURN
      END                                                               CTOC
C
C
C
      SUBROUTINE LCD(I,ILN,LD,LLEN,LOFF,IOF)
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER*4      I
      INTEGER          ILN   ,LLEN,LOFF,IOF
      CHARACTER*(*)        LD(*)
      CHARACTER*1  ICB,ICE,ICL,ICR,ICS,ICU,ICV,ICX,NBLANK
      PARAMETER   (ICB   ='B')
      PARAMETER   (ICE   ='E')
      PARAMETER   (ICL   ='L')
      PARAMETER   (ICR   ='R')
      PARAMETER   (ICS   ='S')
      PARAMETER   (ICU   ='U')
      PARAMETER   (ICV   ='V')
      PARAMETER   (ICX   ='X')
      PARAMETER   (NBLANK=' ')
      CHARACTER*3  IOFORM
      PARAMETER   (IOFORM='(A)')
      CHARACTER*6  IFMT
      PARAMETER   (IFMT='(20I4)')
      CHARACTER*80 LDU
      CHARACTER*80 NLINE
      LOGICAL   INTERNAL,OUTBOUND
      INTEGER   IBG1,IBG2,IEN1,IEN2,ILNB,ILUL1,ILUL2,IT1,IT3,IT4,LTO1,
     1          LTO2,NC
      INTERNAL=.FALSE.
      OUTBOUND=.FALSE.
      NC=41
      IF(I.EQ.-1)OUTBOUND=.TRUE.
      IF(I.LE.-2)INTERNAL=.TRUE.
      IF(I.EQ.-3)OUTBOUND=.TRUE.
      DO 500 IT1=1,ILN
      IF(NC.EQ.41)THEN
C
C ----- FIRST LANE ON LINE
C
        NC=1
        NLINE=NBLANK
      ELSE
C
C ----- SECOND LANE ON LINE
C
        NC=41
      END IF
      NLINE(NC:NC+3)=LD(IT1)(1:4)
C
C   LTO1 - LANE TERMINAL OFFSET AT END WHERE TRAFFIC ENTERS LANE
C   LTO2 -                                           EXITS
C   ILUL1 - LENGTH OF USEABLE LANE AT END WHERE TRAFFIC ENTERS LANE
C   ILUL2 -                                             EXITS
C
      IF(OUTBOUND)THEN
        IF(INTERNAL)THEN
          READ(LD(IT1)(9:24),IFMT)ILUL1,ILUL2,LTO1,LTO2
        ELSE
          READ(LD(IT1)(9:20),IFMT)ILUL1,ILUL2,LTO1
          LTO2=0
        END IF
      ELSE
        IF(INTERNAL)THEN
          READ(LD(IT1)(9:24),IFMT)ILUL2,ILUL1,LTO2,LTO1
        ELSE
          READ(LD(IT1)(9:20),IFMT)ILUL2,ILUL1,LTO2
          LTO1=0
        END IF
      END IF
      IBG1=LOFF-LTO1
      IEN2=LLEN+LTO2
      IF(ILUL2.EQ.0.AND.ILUL1.EQ.0)THEN
        IBG2=IBG1
        IEN1=IEN2
      ELSE
        IEN1=ILUL1+IBG1
        IBG2=IEN2-ILUL2
      END IF
  450 CONTINUE
      WRITE(NLINE(NC+4:NC+19),IFMT)IBG1,IEN1,IBG2,IEN2
C
C ----- PUT TURN CODES IN SPECIFIC COLUMNS
C
      NLINE(NC+21:NC+24)=NBLANK
      IT3=NC+21
      LDU=LD(IT1)
      CALL  TOUPR   ( LDU )
      IT4=INDEX(LDU(5:8),ICU)
      IF(IT4.GT.0)NLINE(IT3:IT3)=ICU
      IT3=IT3+1
      IT4=INDEX(LDU(5:8),ICL)
      IF(IT4.GT.0)NLINE(IT3:IT3)=ICL
      IT3=IT3+1
      IT4=INDEX(LDU(5:8),ICS)
      IF(IT4.GT.0)NLINE(IT3:IT3)=ICS
      IT3=IT3+1
      IT4=INDEX(LDU(5:8),ICR)
      IF(IT4.GT.0)NLINE(IT3:IT3)=ICR
      IT4=INDEX(LDU(5:8),ICX)
      IF(IT4.GT.0)NLINE(NC+21:NC+24)='XXXX'
C
C ----- PUT VEHICLES ALLOWED TO USE THIS LANE IN SPECIFIC COLUMNS
C
      NLINE(NC+34:NC+37)=NBLANK
      IT3=NC+34
      IT4=INDEX(LDU(25:28),ICB)
      IF(IT4.GT.0)NLINE(IT3:IT3)=ICB
      IT3=IT3+1
      IT4=INDEX(LDU(25:28),ICE)
      IF(IT4.GT.0)NLINE(IT3:IT3)=ICE
      IT3=IT3+1
      IT4=INDEX(LDU(25:28),ICR)
      IF(IT4.GT.0)NLINE(IT3:IT3)=ICR
      IT3=IT3+1
      IT4=INDEX(LDU(25:28),ICV)
      IF(IT4.GT.0)NLINE(IT3:IT3)=ICV
      IF(.NOT.OUTBOUND)NLINE(NC+30:NC+33)=LD(IT1)(21:24)
      IF(NC.EQ.1)THEN
C
C ----- LANE 1, 3 OR 5
C
        IF((.NOT.INTERNAL) .AND. (.NOT.OUTBOUND)
     1                     .AND. (IT1.EQ.1))   THEN
C
C ----- PUT IN RANDOM NUMBER SEED
C
          WRITE(NLINE(26:30),'(I5)')I
        ELSE
          IF(IT1.EQ.3)NLINE(26:30)=NBLANK
        END IF
      ELSE
        WRITE(IOF,IOFORM)NLINE(1:ILNB( NLINE ))
      END IF
  500 CONTINUE
      IF(NC.EQ.1)WRITE(IOF,IOFORM)NLINE(1:ILNB( NLINE ))
      RETURN
      END                                                               LCD
