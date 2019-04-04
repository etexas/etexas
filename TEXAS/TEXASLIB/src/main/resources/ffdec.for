      FUNCTION FFDEC(IFNUM,NF,NFT,NLINE,IFEND,MLINE,IDEF,NIO)
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
C    THIS LOGICAL FUNCTION PROCESSES FREE FIELD INPUT OF INTEGER
C    OR DECIMAL NUMBERS, SEPARATED BY COMMAS, IN THE CHARACTER
C    VARIABLE <NLINE>.  THESE ARE ENCODED INTO FIELDS IN THE
C    80 CHARACTER VARIABLE <MLINE> ACCORDING TO THE FORMAT
C    SPECS. IN THE DIMENSIONED 4-CHARACTER VARIABLE <NFT>.
C    THE NUMBER OF THE LAST CHARACTER ENCODED IN <MLINE> IS RETURNED
C    IN <IFEND>.  THE DEFAULTS FOR EACH FIELD WILL BE TAKEN FROM
C    THE LEFTMOST CHARACTERS OF THE DIMENSIONED CHARACTER
C    VARIABLE <IDEF>.
C    <FFDEC> IS SET TO <TRUE> IF NO ERRORS ARE FOUND IN THE INPUT,
C    OTHERWISE <FFDEC> IS SET TO <FALSE>.
C
C    <IFNUM> IS THE NUMBER OF THE FIRST FIELD TO BE PROCESSED
C    <NF> IS THE NUMBER OF THE LAST FIELD TO BE PROCESSED.
C
C   SPECIAL DEFAULTS:
C     "CC...CI" - FIELD SET TO "I", RIGHT JUSTIFIED
C     "DD...DD" IF FIRST CHARACTER IS "C", FIELD SET TO "CONSTAN"
C                                     "E"               "ERLANG"
C                                     "G"               "GAMMA"
C                                     "L"               "LOGNRML"
C                                     "N"               "NEGEXP"
C                                     "S"               "SNEGEXP"
C                                     "U"               "UNIFORM"
C     "EE...EE" IF FIRST CHAR(S) = "U", FIELD SET TO "UNCONTRL"
C                                  "Y"               "YIELD"
C                                  "ST"              "STOP"
C                                  "A"               "ALL-STOP"
C                                  "P"               "PRETIMED"
C                                  "SE"              "SEMI-ACT"
C                                  "F"               "FULL-ACT"
C                                  "N"               "NEMA"
C               CAN'T BE CHANGED WHEN REVISING EXISTING  FILE.
C
C     "FFF" IF FIRST CHARACTER IS "Y", FIELD SET TO "YES"
C                                 "N",              "NO"
C     "GG" IF FIRST CHAR(S) = "B", FIELD SET TO "BL"
C                             "U"               "UN"
C                             "Y"               "YI"
C                             "ST"              "ST"
C                             "SI"              "SI"
C                             "L"               "LT"
C                             "R"               "RT"
C
C     "HH" WILL BE SET TO "BL" FOR BLOCKED LANE AS PER G&D-V REF. DATA
C
C     "III" IF FIRST CHARACTERS ARE "ON", FIELD SET TO "ON"
C                                   "OF"               "OFF"
C
C     "J..J" - FIELD NOT USED, SET TO "-..-"
C
C     "KK" - FIELD MUST STAY "UN" FOR UNSIGNALIZED
C
C     "LL" - FIELD CAN CONTAIN ONLY "C", "L", "S", "R", A COMBINATION
C            OF THE PRECEEDING (EXCEPT "LR" OR "LS"), "UN","XX" OR BE BLANK
C
C     "MM" - FIELD MUST BE "PR", "PU" OR "IN"
C
C     "NNN" IF FIRST CHARACTER IS "A", FIELD SET TO "AND"
C                                 "O"               "OR"
C     "PP...PP" - THIS FIELD CAN'T BE CHANGED WHEN REVISING
C
C     "QQ...QQ" IF FIRST CHAR(S) = "U", FIELD SET TO "UNCONTRL"
C                                  "Y"               "YIELD"
C                                  "ST"              "STOP"
C                                  "A"               "ALL-STOP"
C                                  "P"               "PRETIMED"
C                                  "T"               "TEX-DIA"
C                                  "N"               "NEMA"
C               CAN'T BE CHANGED WHEN REVISING EXISTING  FILE.
C
C     "RR...RR" - THIS FIELD CAN'T BE CHANGED
C
C     "SS...SS" - SPECIAL LEG ANGLE PROCESSING IN NOILDA
C                 OTHERWISE SAME AS XX...XX
C
C     "TT" - FIELD CAN CONTAIN ONLY "IR", "2", "3", "5", "6", OR "IL"
C            FOR LEG WHERE DETECTORS ARE LOCATED,NEMA CONTROLLER AT DIAMOND
C     "UUU" IF FIRST CHARACTER IS "Y", FIELD SET TO "YES"
C                                 "N",              "NO"
C                                 "P",              "POL"
C     "ZZ...ZZ" - SPECIAL PROCESSING IN NOILDA,
C                 0 IS ALLOWED, EVEN IF OUT OF RANGE
C                 OTHERWISE SAME AS XX...XX
C
C     "XX...XX" - BYPASS DEFAULT PROCESSING
C     "       " - FIELD LEFT BLANK, PRINTED MESSAGE
C
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  FFDEC
      INTEGER        IFNUM,NF          ,IFEND           ,NIO
      CHARACTER*4             NFT(*)
      CHARACTER*(*)               NLINE      ,MLINE,IDEF(*)
      CHARACTER*1 COMMA    ,PERIOD
      PARAMETER  (COMMA=',',PERIOD='.')
      CHARACTER*1 IC1    ,IC2
      PARAMETER  (IC1='1',IC2='2')
      CHARACTER*3 IOFORM
      PARAMETER  (IOFORM='(A)')
      INCLUDE 'CMPR01'
      INCLUDE 'CMTX01'
      CHARACTER*20 IDEFT
      CHARACTER*80 MLINEU,NLINEU
      LOGICAL  NOILDA,IL,LSTFLG
      CHARACTER IFMT*4,FMT1*9,FMT2*24,FMT3*7,FMT4*13,TEMP*40
      CHARACTER XXXXX*20,NBLANK*20
      CHARACTER*20 CCCCC,HHHHH,IIIII,JJJJJ,KKKKK,LLLLL
      CHARACTER*20 MMMMM,NNNNN,PPPPP,QQQQQ,RRRRR,SSSSS,TTTTT,UUUUU
      CHARACTER*20 ZZZZZ
      CHARACTER*4  NFTT,NFTTU
      CHARACTER*20 DDDDD,EEEEE,FFFFF,GGGGG,DASH
      INTEGER      I,IFIR,IFSIZE,IFSTRT,ILNB,IT1,IT2,IT3,IT4,IT5,IT6,
     1             IT7,IT8,ITC,ITEMP,IXSIZE,NC,NCNDF
      REAL         X
      DATA IFMT,FMT1,FMT3 / '(I )','(A,I ,3A)','(F  .0)' /
      DATA FMT2 / '(A,I ,A,I ,A,I ,A,I ,3A)' /
      DATA FMT4 / '(A,I ,A,I1,A)' /
      DATA DASH / '----------' /
      DATA CCCCC,XXXXX,NBLANK/ 'CCCCCCCCCC','XXXXXXXXXX','          ' /
      DATA DDDDD,EEEEE,FFFFF / 'DDDDDDDDDD','EEEEEEEEEE','FFFFFFFFFF' /
      DATA GGGGG,HHHHH,IIIII / 'GGGGGGGGGG','HHHHHHHHHH','IIIIIIIIII' /
      DATA JJJJJ,KKKKK       / 'JJJJJJJJJJ','KKKKKKKKKK'/
      DATA LLLLL,MMMMM,NNNNN / 'LLLLLLLLLL','MMMMMMMMMM','NNNNNNNNNN' /
      DATA PPPPP,QQQQQ,RRRRR / 'PPPPPPPPPP','QQQQQQQQQQ','RRRRRRRRRR' /
      DATA SSSSS,TTTTT,UUUUU / 'SSSSSSSSSS','TTTTTTTTTT','UUUUUUUUUU' /
      DATA ZZZZZ             / 'ZZZZZZZZZZ' /
      MLINEU=MLINE
      CALL  TOUPR   ( MLINEU )
      IFIR=IFNUM
      IL=.TRUE.
      FFDEC=.TRUE.
      NC=ILNB( NLINE )
      IF(NC.EQ.0)THEN
        LSTFLG=.TRUE.
      ELSE
        LSTFLG=.FALSE.
      END IF
      IFSTRT=1
      IF(IFIR.EQ.1)GO TO 170
      DO 165 I=1,IFIR-1
      IT1=2
      IT2=ICHAR( NFT(I)(3:3) )
      IF(IT2.GE.ICHAR( '0' ).AND.IT2.LE.ICHAR( '9' ))THEN
        IT1=3
        IFMT(3:3)=IC2
      ELSE
        IT1=2
        IFMT(3:3)=IC1
      END IF
      READ(NFT(I)(2:IT1),IFMT)ITEMP
  165 IFSTRT=IFSTRT+ITEMP
  170 IT1=1
  500 NFTT=NFT(IFIR)
      IDEFT=IDEF(IFIR)
      IT2=ICHAR( NFTT(3:3) )
      IF(IT2.GE.ICHAR( '0' ).AND.IT2.LE.ICHAR( '9' ))THEN
        IT3=3
        IFMT(3:3)=IC2
      ELSE
        IT3=2
        IFMT(3:3)=IC1
      END IF
      READ(NFTT(2:IT3),IFMT)IFSIZE
      IFEND=IFSTRT+IFSIZE-1
      IF(LSTFLG)GO TO 5000
      IF(NC.LT.IT1)THEN
        IT2=0
      ELSE
        IT2=INDEX(NLINE(IT1:NC),COMMA)
      END IF
      IF(IT2.EQ.0)THEN
        LSTFLG=.TRUE.
        IT2=NC
      ELSE
        IT2=IT2-2+IT1
C
C   CHECK FOR "(" IN THE FIELD
C
        IF(IT2.LT.IT1)GO TO 4600
        IT3=INDEX(NLINE(IT1:IT2),'(')
        IF(IT3.GT.0)THEN
C
C   IGNORE COMMAS WITHIN PARENTHESIS PAIRS
C
          IF(IT2.GE.IT1)THEN
            IT3=IT3+IT1
          ELSE
            IT3=0
            GO TO 4600
          END IF
          IT4=INDEX(NLINE(IT3:NC),')')
          IF(IT4.EQ.0)THEN
            CALL NCFMT(FMT1(5:),IFIR)
            IF(NIO.LT.0)THEN
              WRITE(*  ,FMT1)' Error in keyin, unclosed '//
     1                       'parenthesis in field ',IFIR,'.'
            ELSE
              WRITE(NIO,FMT1)' Error in keyin, unclosed '//
     1                       'parenthesis in field ',IFIR,'.'
            END IF
            FFDEC=.FALSE.
          END IF
          IT4=IT4+IT3
          IT2=INDEX(NLINE(IT4:NC),',')
          IF(IT2.EQ.0)THEN
            IT2=NC
            LSTFLG=.TRUE.
          ELSE
            IT2=IT2+IT4-2
          END IF
        END IF
 4600   CONTINUE
      END IF
C
C -----  NCNDF - NUMBER OF CHARACTERS IN THE KEYED IN DATA FIELD
C
      NCNDF=IT2-IT1+1
      IT3=IFSIZE-1
      IF(IT3.GE.1)THEN
        IF(IDEFT(1:IT3).EQ.CCCCC(1:IT3))GO TO 5015
      END IF
      IF((IDEFT(1:IFSIZE).EQ.PPPPP(1:IFSIZE)) .AND. REVISE)GO TO 5010
      IF(IDEFT(1:IFSIZE).EQ.RRRRR(1:IFSIZE))GO TO 5013
 4610 CONTINUE
      IF(IDEFT(1:IFSIZE).NE.JJJJJ(1:IFSIZE))GO TO 4620
C
C ----- UNUSABLE FIELD, BLOCK OUT
C
C     IF(NIO.LT.0)THEN
C       WRITE(*  ,'(A,I2,A,I2,3A/A,I2,3A)')
C    1            'IN FFDEC, MLINE(',IFSTRT,
C    2            ':',IFEND,'): "',MLINE(IFSTRT:IFEND),'"','DASH(1:',
C    3            IFSIZE,'): "',DASH(1:IFSIZE),'"'
C     ELSE
C       WRITE(NIO,'(A,I2,A,I2,3A/A,I2,3A)')
C    1            'IN FFDEC, MLINE(',IFSTRT,
C    2            ':',IFEND,'): "',MLINE(IFSTRT:IFEND),'"','DASH(1:',
C    3            IFSIZE,'): "',DASH(1:IFSIZE),'"'
C     END IF
      IF(MLINEU(IFSTRT:IFEND).NE.DASH(1:IFSIZE))
     1   MLINE(IFSTRT:IFEND)=DASH(1:IFSIZE)
      MLINEU=MLINE
      CALL  TOUPR   ( MLINEU )
C     IF(NIO.LT.0)THEN
C       WRITE(*  ,'(A,I2,A,I5)')'IN FFDEC, FIELD: ',IFIR,'   NCNDF:',
C    1                          NCNDF
C     ELSE
C       WRITE(NIO,'(A,I2,A,I5)')'IN FFDEC, FIELD: ',IFIR,'   NCNDF:',
C    1                          NCNDF
C     END IF
      IF(NCNDF.LE.0)GO TO 6030
      IF(DIA)THEN
        CALL NCFMT(FMT4(5:),IFIR)
        IF(NIO.LT.0)THEN
          WRITE(*  ,FMT4)'Field ',IFIR,' isn''t used for figure ',
     1                   MOUT(8),'.'
        ELSE
          WRITE(NIO,FMT4)'Field ',IFIR,' isn''t used for figure ',
     1                   MOUT(8),'.'
        END IF
      ELSE
        IF(NIO.LT.0)THEN
          WRITE(*  ,FMT1)'Phase 1 is unactuated.  Field ',IFIR,
     1                   ' isn''t used for this phase.'
        ELSE
          WRITE(NIO,FMT1)'Phase 1 is unactuated.  Field ',IFIR,
     1                   ' isn''t used for this phase.'
        END IF
      END IF
      IL=.FALSE.
      GO TO 6030
 4620 CONTINUE
      IF(NCNDF.GT.0)GO TO 5460
      IF(IDEFT(1:IFSIZE).EQ.UUUUU(1:IFSIZE))GO TO 5053
      IF(IDEFT(1:IFSIZE).EQ.FFFFF(1:IFSIZE))GO TO 5054
      IF(IDEFT(1:IFSIZE).EQ.DDDDD(1:IFSIZE))GO TO 5030
      IF(IDEFT(1:IFSIZE).EQ.EEEEE(1:IFSIZE))GO TO 5040
      IF(IDEFT(1:IFSIZE).EQ.QQQQQ(1:IFSIZE))GO TO 5140
      IF(IDEFT(1:IFSIZE).EQ.GGGGG(1:IFSIZE))GO TO 5020
      IF(IDEFT(1:IFSIZE).EQ.IIIII(1:IFSIZE))GO TO 5059
      IF(IDEFT(1:IFSIZE).EQ.KKKKK(1:IFSIZE))GO TO 6030
      IF(IDEFT(1:IFSIZE).EQ.LLLLL(1:IFSIZE))GO TO 6030
      IF(IDEFT(1:IFSIZE).EQ.MMMMM(1:IFSIZE))GO TO 5065
      IF(IDEFT(1:IFSIZE).EQ.NNNNN(1:IFSIZE))GO TO 5062
      IF(IDEFT(1:IFSIZE).NE.HHHHH(1:IFSIZE))GO TO 4800
      IF(IDEFT(1:IFSIZE).EQ.TTTTT(1:IFSIZE))GO TO 5070
 4700 CONTINUE
      IF(MLINEU(IFSTRT:IFEND).EQ.'BL')GO TO 6030
      MLINE(IFSTRT:IFEND)='BL'
      MLINEU=MLINE
      CALL  TOUPR   ( MLINEU )
      CALL NCFMT(FMT1(5:),IFIR)
      IF(NIO.LT.0)THEN
        WRITE(*  ,FMT1)'Field ',IFIR,' set to "BL" to be compatable '//
     1                 'with Geometry & Driver-Vehicle ref. data.'
      ELSE
        WRITE(NIO,FMT1)'Field ',IFIR,' set to "BL" to be compatable '//
     1                 'with Geometry & Driver-Vehicle ref. data.'
      END IF
      IL=.FALSE.
      GO TO 6030
 4725 CONTINUE
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(NLINEU(IT1:IT2).EQ.'UN')GO TO 6030
      CALL NCFMT(FMT1(5:),IFIR)
      IF(NIO.LT.0)THEN
        WRITE(*  ,FMT1)'Lane control data requires data in field ',
     1                 IFIR,' to be "UN", unsignalized.'
      ELSE
        WRITE(NIO,FMT1)'Lane control data requires data in field ',
     1                 IFIR,' to be "UN", unsignalized.'
      END IF
      IL=.FALSE.
      NLINE(IT1:IT2)='UN'
      GO TO 6030
 4750 CONTINUE
      IF(MLINEU(IFSTRT:IFEND).EQ.XXXXX(1:IFSIZE))GO TO 6030
      IF(MLINEU(IFSTRT:IFEND).EQ.SSSSS(1:IFSIZE))GO TO 6030
      IF(MLINEU(IFSTRT:IFEND).EQ.ZZZZZ(1:IFSIZE))GO TO 6030
      IF(MLINEU(IFSTRT:IFEND).EQ.'UN')GO TO 6030
      IF(MLINEU(IFSTRT:IFEND).EQ.' ')GO TO 6030
      IF(MLINEU(IFSTRT:IFSTRT).EQ.MLINEU(IFEND:IFEND))
     1     MLINE(IFEND:IFEND)=' '
      MLINEU=MLINE
      CALL  TOUPR   ( MLINEU )
      IF(MLINEU(IFEND:IFEND).EQ.' ')THEN
C
C ----- ONE CHARACTER
C
        IF(MLINEU(IFSTRT:IFSTRT).EQ.'C')GO TO 6030
        IF(MLINEU(IFSTRT:IFSTRT).EQ.'L')GO TO 6030
        IF(MLINEU(IFSTRT:IFSTRT).EQ.'S')GO TO 6030
        IF(MLINEU(IFSTRT:IFSTRT).EQ.'R')GO TO 6030
        GO TO 5430
      ELSE
C
C ----- TWO CHARACTERS
C
        IT6=INDEX(MLINEU(IFSTRT:IFEND), 'L')
        IT7=INDEX(MLINEU(IFSTRT:IFEND), 'S')
        IF((IT6+IT7).EQ.3)GO TO 5430
        IT8=INDEX(MLINEU(IFSTRT:IFEND), 'R')
        IF((IT6+IT8).EQ.3)GO TO 5430
        IT5=INDEX(MLINEU(IFSTRT:IFEND), 'C')+IT6+IT7+IT8
        IF(IT5.NE.3)GO TO 5430
      END IF
      GO TO 6030
 4800 CONTINUE
 5000 CONTINUE
C
C ----- NO MORE KEYED IN DATA, PROCESS DEFAULTS
C
      NCNDF=0
      IF(IDEFT(1:IFSIZE).EQ.JJJJJ(1:IFSIZE))GO TO 4610
      IF(IDEFT(1:IFSIZE).EQ.HHHHH(1:IFSIZE))GO TO 4700
      IF(IDEFT(1:IFSIZE).EQ.KKKKK(1:IFSIZE))GO TO 6030
      IF(IDEFT(1:IFSIZE).EQ.LLLLL(1:IFSIZE))GO TO 6030
      IXSIZE=IFSIZE
      IF(IXSIZE.GT.10)IXSIZE=10
      IF(IDEFT(1:IXSIZE).EQ.XXXXX(1:IXSIZE))GO TO 6020
      IF(IDEFT(1:IXSIZE).EQ.SSSSS(1:IXSIZE))GO TO 6020
      IF(IDEFT(1:IXSIZE).EQ.ZZZZZ(1:IXSIZE))GO TO 6020
      IF(IDEFT(1:IFSIZE).NE.NBLANK(1:1))GO TO 5440
      CALL NCFMT(FMT1(5:),IFIR)
      IF(NIO.LT.0)THEN
        WRITE(*  ,FMT1)'No default for field ',IFIR,
     1                 '.  Field left blank.'
      ELSE
        WRITE(NIO,FMT1)'No default for field ',IFIR,
     1                 '.  Field left blank.'
      END IF
      FFDEC=.FALSE.
      GO TO 5450
 5010 CONTINUE
      IF(NCNDF.LE.0)GO TO 6030
      CALL NCFMT(FMT1(5:),IFIR)
      IF(NIO.LT.0)THEN
        WRITE(*  ,FMT1)'Field ',IFIR,' can''t be changed when '//
     1                 'revising a file.'
      ELSE
        WRITE(NIO,FMT1)'Field ',IFIR,' can''t be changed when '//
     1                 'revising a file.'
      END IF
      IL=.FALSE.
      GO TO 6030
 5013 CONTINUE
      IF(NCNDF.LE.0)GO TO 6030
      CALL NCFMT(FMT1(5:),IFIR)
      IF(NIO.LT.0)THEN
        WRITE(*  ,FMT1)'Field ',IFIR,' is for reference only and '//
     1                 'can''t be changed.'
      ELSE
        WRITE(NIO,FMT1)'Field ',IFIR,' is for reference only and '//
     1                 'can''t be changed.'
      END IF
      IL=.FALSE.
      GO TO 6030
 5015 CONTINUE
      IF(MLINEU(IFSIZE:IFSIZE).NE.IDEFT(IFSIZE:IFSIZE))THEN
        MLINE(1:IFSIZE)=NBLANK(1:IT3)//IDEFT(IFSIZE:IFSIZE)
        MLINEU=MLINE
        CALL  TOUPR   ( MLINEU )
        IF(NIO.LT.0)THEN
          WRITE(*  ,IOFORM)'Field 1 set to the number of the leg'//
     1                     ' being processed.'
        ELSE
          WRITE(NIO,IOFORM)'Field 1 set to the number of the leg'//
     1                     ' being processed.'
        END IF
        FFDEC=.FALSE.
      ELSE
        IF(NCNDF.LE.0)GO TO 6030
        IF(NIO.LT.0)THEN
          WRITE(*  ,IOFORM)'Field 1 not changed.  Must '//
     1                     'be the number of the leg being processed.'
        ELSE
          WRITE(NIO,IOFORM)'Field 1 not changed.  Must '//
     1                     'be the number of the leg being processed.'
        END IF
        FFDEC=.FALSE.
      END IF
      GO TO 6030
 5020 IF(MLINEU(IFSTRT:IFSTRT).NE.'B')GO TO 5021
      MLINE(IFSTRT:IFEND)='BL'
      GO TO 6030
 5021 IF(MLINEU(IFSTRT:IFSTRT).NE.'U')GO TO 5022
      MLINE(IFSTRT:IFEND)='UN'
      GO TO 6030
 5022 IF(MLINEU(IFSTRT:IFSTRT).NE.'Y')GO TO 5023
      MLINE(IFSTRT:IFEND)='YI'
      GO TO 6030
 5023 IF(MLINEU(IFSTRT:IFSTRT+1).NE.'ST')GO TO 5024
      MLINE(IFSTRT:IFEND)='ST'
      GO TO 6030
 5024 IF(MLINEU(IFSTRT:IFSTRT+1).NE.'SI')GO TO 5025
      MLINE(IFSTRT:IFEND)='SI'
      GO TO 6030
 5025 IF(MLINEU(IFSTRT:IFSTRT).NE.'L')GO TO 5026
      MLINE(IFSTRT:IFEND)='LT'
      GO TO 6030
 5026 IF(MLINEU(IFSTRT:IFSTRT).NE.'R')GO TO 5400
      MLINE(IFSTRT:IFEND)='RT'
      GO TO 6030
 5030 IF(MLINEU(1:1).NE.'C')GO TO 5031
      MLINE(IFSTRT:IFEND)='CONSTAN'
      GO TO 6030
 5031 IF(MLINEU(1:1).NE.'E')GO TO 5032
      MLINE(IFSTRT:IFEND)='ERLANG'
      GO TO 6030
 5032 IF(MLINEU(1:1).NE.'G')GO TO 5033
      MLINE(IFSTRT:IFEND)='GAMMA'
      GO TO 6030
 5033 IF(MLINEU(1:1).NE.'L')GO TO 5034
      MLINE(IFSTRT:IFEND)='LOGNRML'
      GO TO 6030
 5034 IF(MLINEU(1:1).NE.'N')GO TO 5035
      MLINE(IFSTRT:IFEND)='NEGEXP'
      GO TO 6030
 5035 IF(MLINEU(1:1).NE.'S')GO TO 5036
      MLINE(IFSTRT:IFEND)='SNEGEXP'
      GO TO 6030
 5036 IF(MLINEU(1:1).NE.'U')GO TO 5400
      MLINE(IFSTRT:IFEND)='UNIFORM'
      GO TO 6030
 5040 IF(MLINEU(IFSTRT:IFSTRT).NE.'U')GO TO 5042
 5041 MLINE(IFSTRT:IFEND)='UNCONTRL'
      GO TO 6030
 5042 IF(MLINEU(IFSTRT:IFSTRT).NE.'Y')GO TO 5044
 5043 MLINE(IFSTRT:IFEND)='YIELD'
      GO TO 6030
 5044 IF(MLINEU(IFSTRT:IFSTRT+1).NE.'ST')GO TO 5046
 5045 MLINE(IFSTRT:IFEND)='STOP'
      GO TO 6030
 5046 IF(MLINEU(IFSTRT:IFSTRT).NE.'A')GO TO 5048
 5047 MLINE(IFSTRT:IFEND)='ALL-STOP'
      GO TO 6030
 5048 IF(MLINEU(IFSTRT:IFSTRT).NE.'P')GO TO 5050
      MLINE(IFSTRT:IFEND)='PRETIMED'
      GO TO 6030
 5050 IF(MLINEU(IFSTRT:IFSTRT+1).NE.'SE')GO TO 5051
      MLINE(IFSTRT:IFEND)='SEMI-ACT'
      GO TO 6030
 5051 IF(MLINEU(IFSTRT:IFSTRT).NE.'F')GO TO 5052
      MLINE(IFSTRT:IFEND)='FULL-ACT'
      GO TO 6030
 5052 IF(MLINEU(IFSTRT:IFSTRT).NE.'N')GO TO 5400
      MLINE(IFSTRT:IFEND)='NEMA'
      GO TO 6030
 5053 IF(MLINEU(IFSTRT:IFSTRT).NE.'P')GO TO 5054
      MLINE(IFSTRT:IFEND)='POL'
      GO TO 6030
 5054 IF(MLINEU(IFSTRT:IFSTRT).NE.'Y')GO TO 5055
      MLINE(IFSTRT:IFEND)='YES'
      GO TO 6030
 5055 IF(MLINEU(IFSTRT:IFSTRT).NE.'N')GO TO 5400
      MLINE(IFSTRT:IFEND)='NO'
      GO TO 6030
 5059 IF(MLINEU(IFSTRT:IFSTRT+1).NE.'OF')GO TO 5060
      MLINE(IFSTRT:IFEND)='OFF'
      GO TO 6030
 5060 IF(MLINEU(IFSTRT:IFSTRT+1).NE.'ON')GO TO 5400
      MLINE(IFSTRT:IFEND)='ON '
      GO TO 6030
 5062 IF(MLINEU(IFSTRT:IFSTRT).NE.'A')GO TO 5063
      MLINE(IFSTRT:IFEND)='AND'
      GO TO 6030
 5063 IF(MLINEU(IFSTRT:IFSTRT).NE.'O')GO TO 5400
      MLINE(IFSTRT:IFEND)='OR'
      GO TO 6030
 5065 IF(MLINEU(IFSTRT:IFEND).EQ.'PU')GO TO 6030
      IF(MLINEU(IFSTRT:IFEND).EQ.'PR')GO TO 6030
      IF(MLINEU(IFSTRT:IFSTRT).EQ.'I')THEN
        MLINE(IFSTRT:IFEND)='IN'
        GO TO 6030
      END IF
      GO TO 5400
 5070 CONTINUE
      IF(MLINEU(IFSTRT:IFEND).EQ.NBLANK(1:IFSIZE))GO TO 6030
      IF(MLINEU(IFSTRT:IFEND).EQ.'IR')GO TO 6030
      IF(MLINEU(IFSTRT:IFEND).EQ.'IL')GO TO 6030
      IF(MLINEU(IFSTRT:IFEND).EQ.' 2')GO TO 6030
      IF((MLINEU(IFSTRT:IFEND).EQ.'2 ') .OR.
     1   (MLINEU(IFSTRT:IFEND).EQ.'02'))THEN
        MLINE(IFSTRT:IFEND)=' 2'
        GO TO 6030
      END IF
      IF(MLINEU(IFSTRT:IFEND).EQ.' 3')GO TO 6030
      IF((MLINEU(IFSTRT:IFEND).EQ.'3 ') .OR.
     1   (MLINEU(IFSTRT:IFEND).EQ.'03'))THEN
        MLINE(IFSTRT:IFEND)=' 3'
        GO TO 6030
      END IF
      IF(MLINEU(IFSTRT:IFEND).EQ.' 5')GO TO 6030
      IF((MLINEU(IFSTRT:IFEND).EQ.'5 ') .OR.
     1   (MLINEU(IFSTRT:IFEND).EQ.'05'))THEN
        MLINE(IFSTRT:IFEND)=' 5'
        GO TO 6030
      END IF
      IF(MLINEU(IFSTRT:IFEND).EQ.' 6')GO TO 6030
      IF((MLINEU(IFSTRT:IFEND).EQ.'6 ') .OR.
     1   (MLINEU(IFSTRT:IFEND).EQ.'06'))THEN
        MLINE(IFSTRT:IFEND)=' 6'
        GO TO 6030
      END IF
      CALL NCFMT(FMT1(5:),IFIR)
      IF(NIO.LT.0)THEN
        WRITE(*  ,FMT1)'Leg in field ',IFIR,': "',
     1                 MLINE(IFSTRT:IFEND),
     2                 '" is not "IR","2","3","5","6", or '//
     3                 '"IL".  Field left blank.'
      ELSE
        WRITE(NIO,FMT1)'Leg in field ',IFIR,': "',
     1                 MLINE(IFSTRT:IFEND),
     2                 '" is not "IR","2","3","5","6", or '//
     3                 '"IL".  Field left blank.'
      END IF
      MLINE(IFSTRT:IFEND)=NBLANK(1:1)
      FFDEC=.FALSE.
      GO TO 6030
 5140 IF(MLINEU(IFSTRT:IFSTRT).NE.'U')GO TO 5142
 5141 MLINE(IFSTRT:IFEND)='UNCONTRL'
      GO TO 6030
 5142 IF(MLINEU(IFSTRT:IFSTRT).NE.'Y')GO TO 5144
 5143 MLINE(IFSTRT:IFEND)='YIELD'
      GO TO 6030
 5144 IF(MLINEU(IFSTRT:IFSTRT+1).NE.'ST')GO TO 5146
 5145 MLINE(IFSTRT:IFEND)='STOP'
      GO TO 6030
 5146 IF(MLINEU(IFSTRT:IFSTRT).NE.'A')GO TO 5148
 5147 MLINE(IFSTRT:IFEND)='ALL-STOP'
      GO TO 6030
 5148 IF(MLINEU(IFSTRT:IFSTRT).NE.'P')GO TO 5150
      MLINE(IFSTRT:IFEND)='PRETIMED'
      GO TO 6030
 5150 IF(MLINEU(IFSTRT:IFSTRT).NE.'T')GO TO 5152
      MLINE(IFSTRT:IFEND)='TEX-DIA'
      GO TO 6030
 5152 IF(MLINEU(IFSTRT:IFSTRT).NE.'N')GO TO 5400
      MLINE(IFSTRT:IFEND)='NEMA'
      GO TO 6030
 5400 IF(MLINEU(IFSTRT:IFEND).EQ.NBLANK(1:IFSIZE))GO TO 6030
 5430 CALL NCFMT(FMT1(5:),IFIR)
      IF(NIO.LT.0)THEN
        WRITE(*  ,FMT1)'Improper data entered for field ',IFIR,': "',
     1                 MLINE(IFSTRT:IFEND),'".  Field left blank.'
      ELSE
        WRITE(NIO,FMT1)'Improper data entered for field ',IFIR,': "',
     1                 MLINE(IFSTRT:IFEND),'".  Field left blank.'
      END IF
      MLINE(IFSTRT:IFEND)=NBLANK(1:1)
      FFDEC=.FALSE.
      GO TO 6030
 5440 IT3=IFSIZE-1
      IF(IT3.GE.1)THEN
        IF(IDEFT(1:IFSIZE).EQ.UUUUU(1:IFSIZE))GO TO 5053
        IF(IDEFT(1:IFSIZE).EQ.FFFFF(1:IFSIZE))GO TO 5054
        IF(IDEFT(1:IFSIZE).EQ.DDDDD(1:IFSIZE))GO TO 5030
        IF(IDEFT(1:IFSIZE).EQ.EEEEE(1:IFSIZE))GO TO 5040
        IF(IDEFT(1:IFSIZE).EQ.QQQQQ(1:IFSIZE))GO TO 5140
        IF(IDEFT(1:IFSIZE).EQ.GGGGG(1:IFSIZE))GO TO 5020
        IF(IDEFT(1:IFSIZE).EQ.IIIII(1:IFSIZE))GO TO 5059
        IF(IDEFT(1:IFSIZE).EQ.MMMMM(1:IFSIZE))GO TO 5065
        IF(IDEFT(1:IFSIZE).EQ.NNNNN(1:IFSIZE))GO TO 5062
        IF(IDEFT(1:IFSIZE).EQ.PPPPP(1:IFSIZE))GO TO 6030
        IF(IDEFT(1:IFSIZE).EQ.RRRRR(1:IFSIZE))GO TO 6030
        IF(IDEFT(1:IFSIZE).EQ.TTTTT(1:IFSIZE))GO TO 5070
        IF(IDEFT(1:IT3).EQ.CCCCC(1:IT3))THEN
          NCNDF=0
          GO TO 5015
        END IF
      END IF
      CALL NCFMT(FMT1(5:),IFIR)
C
C
C     IF(NIO.LT.0)THEN
C       WRITE(*  ,FMT1)'Default value used for field ',IFIR
C     ELSE
C       WRITE(NIO,FMT1)'Default value used for field ',IFIR
C     END IF
C
C
      FFDEC=.FALSE.
 5450 MLINE(IFSTRT:IFEND)=IDEFT(1:IFSIZE)
      MLINEU=MLINE
      CALL  TOUPR   ( MLINEU )
      GO TO 6020
 5460 CONTINUE
      IT3=IFEND+IT1-IT2
      NFTTU=NFTT
      CALL  TOUPR   ( NFTTU )
      IF(NFTTU(1:1).EQ.'A')THEN
C
C ----- PUT "A" DATA INTO FIELD
C
        IF(REVISE .AND. ((IDEFT(1:IFSIZE).EQ.EEEEE(1:IFSIZE))
     1                                 .OR.
     2                   (IDEFT(1:IFSIZE).EQ.QQQQQ(1:IFSIZE))))THEN
          IF(MLINEU(IFSTRT:IFSTRT).EQ.'U')GO TO 5480
          IF(MLINEU(IFSTRT:IFSTRT).EQ.'Y')GO TO 5480
          IF(MLINEU(IFSTRT:IFSTRT+1).EQ.'ST')GO TO 5480
          IF(MLINEU(IFSTRT:IFSTRT).EQ.'A')GO TO 5480
          NLINEU=NLINE
          CALL  TOUPR   ( NLINEU )
          IF(MLINEU(IFSTRT:IFSTRT).EQ.NLINEU(IT1:IT1))GO TO 6030
          IF(NIO.LT.0)THEN
            WRITE(*  ,IOFORM)'Type of intersection control can''t be '//
     1                       'changed when revising this existing file.'
          ELSE
            WRITE(NIO,IOFORM)'Type of intersection control can''t be '//
     1                       'changed when revising this existing file.'
          END IF
 5470     CALL NCFMT(FMT2(5:),IFIR)
          IF(NIO.LT.0)THEN
            WRITE(*  ,FMT2)'Field ',IFIR,' left unchanged.'
          ELSE
            WRITE(NIO,FMT2)'Field ',IFIR,' left unchanged.'
          END IF
          IL=.FALSE.
          GO TO 6030
 5480     CONTINUE
C
C ----- INTERSECTION IS NOW UNSIGNALIZED
C
          NLINEU=NLINE
          CALL  TOUPR   ( NLINEU )
          IF(MLINEU(IFSTRT:IFSTRT).EQ.NLINEU(IT1:IT1))GO TO 6030
          IF(NLINEU(IT1:IT1).EQ.'U')GO TO 5041
          IF(NLINEU(IT1:IT1).EQ.'Y')GO TO 5043
          IF(NLINEU(IT1:IT1+1).EQ.'ST')GO TO 5045
          IF(NLINEU(IT1:IT1).EQ.'A')GO TO 5047
          IF(NIO.LT.0)THEN
            WRITE(*  ,IOFORM)'An unsignalized intersection can''t be '//
     1                       'signalized by revising an existing file.'
          ELSE
            WRITE(NIO,IOFORM)'An unsignalized intersection can''t be '//
     1                       'signalized by revising an existing file.'
          END IF
          GO TO 5470
        END IF
        IF(IDEFT(1:IFSIZE).EQ.HHHHH(1:IFSIZE))THEN
          NLINEU=NLINE
          CALL  TOUPR   ( NLINEU )
          IF(NLINEU(IT1:IT1).EQ.'B')GO TO 5500
          CALL NCFMT(FMT1(5:),IFIR)
          IF(NIO.LT.0)THEN
            WRITE(*  ,FMT1)'Geometry & Driver-Vehicle reference data '//
     1                     'requires data in field ',IFIR,' be "BL".'
          ELSE
            WRITE(NIO,FMT1)'Geometry & Driver-Vehicle reference data '//
     1                     'requires data in field ',IFIR,' be "BL".'
          END IF
          IL=.FALSE.
 5500     MLINE(IFSTRT:IFEND)='BL'
          GO TO 6030
        END IF
        IF(IDEFT(1:IFSIZE).EQ.KKKKK(1:IFSIZE))GO TO 4725
        MLINE(IFSTRT:IFEND)=NLINE(IT1:IT2)
        MLINEU=MLINE
        CALL  TOUPR   ( MLINEU )
        IF(IFSIZE.LT.NCNDF)THEN
C
C ----- TRUNCATE "A" DATA TO FIT FIELD
C
          FFDEC=.FALSE.
          CALL NCFMT(FMT2(5:),IFIR)
          CALL NCFMT(FMT2(10:),IFSIZE)
          FMT2(15:15)=FMT2(10:10)
          CALL NCFMT(FMT2(20:),NCNDF)
          IT3=IT2-NCNDF+1
          IF(NIO.LT.0)THEN
            WRITE(*  ,FMT2)'Field ',IFIR,' size is ',IFSIZE,'. Only ',
     1                     IFSIZE,' of the ',NCNDF,
     2                     ' entered characters ("',
     3                     NLINE(IT3:IT2),'") were used.'
          ELSE
            WRITE(NIO,FMT2)'Field ',IFIR,' size is ',IFSIZE,'. Only ',
     1                     IFSIZE,' of the ',NCNDF,
     2                     ' entered characters ("',
     3                     NLINE(IT3:IT2),'") were used.'
          END IF
        END IF
        IF(IDEFT(1:IFSIZE).EQ.LLLLL(1:IFSIZE))GO TO 4750
        GO TO 6010
      END IF
      NFTTU=NFTT
      CALL  TOUPR   ( NFTTU )
      IF(IFSIZE.GE.NCNDF)THEN
        IT3=IFEND-NCNDF+1
        IT4=1
        IF(NFTTU(1:1).EQ.'F')THEN
C
C ----- CHECK FOR DEC PT IN "F" FIELD
C ----- TRY TO PUT IN DEC PT, IF REQ'D
C
          IT4=INDEX(NLINE(IT1:IT2),PERIOD)
          IF(IT4.EQ.0)THEN
C
C ----- THERE IS NO DEC PT IN "F" FIELD DATA
C
            IF(IFSIZE.EQ.NCNDF)GO TO 6005
            IT3=IT3-1
          END IF
        END IF
        IF(IT3.GT.IFSTRT)MLINE(IFSTRT:IT3-1)=NBLANK
        MLINE(IT3:IFEND)=NLINE(IT1:IT2)
        IF(IT4.EQ.0)MLINE(IFEND:IFEND)=PERIOD
        MLINEU=MLINE
        CALL  TOUPR   ( MLINEU )
        GO TO 6020
      END IF
      IF(NFTTU(1:1).EQ.'I')THEN
C
C ----- BLANK OUT INTEGER FIELDS WITH EXCESSIVE CHARACTERS
C
        CALL NCFMT(FMT2(5:),IFIR)
        CALL NCFMT(FMT2(10:),IFSIZE)
        CALL NCFMT(FMT2(15:),NCNDF)
        TEMP=NLINE(IT1:IT2)
        ITC=IT2-IT1+1
        IF(ITC.GT.40)ITC=40
        IF(NIO.LT.0)THEN
          WRITE(*  ,FMT2)'Field ',IFIR,' size is ',IFSIZE,' but ',NCNDF,
     1                   ' characters ("'//TEMP(1:ITC)//
     2                   '") entered.  '//'Field left blank.'
        ELSE
          WRITE(NIO,FMT2)'Field ',IFIR,' size is ',IFSIZE,' but ',NCNDF,
     1                   ' characters ("'//TEMP(1:ITC)//
     2                   '") entered.  '//'Field left blank.'
        END IF
        MLINE(IFSTRT:IFEND)=NBLANK
        GO TO 6030
      END IF
C
C ----- TRY TO FIT FLOATING PT NUMBER INTO FIELD
C
      IT3=INDEX(NLINE(IT1:IT2),PERIOD)
      IF(IT3.EQ.0)GO TO 6005
      WRITE(FMT3(3:4),'(I2)',ERR=6005)NCNDF
      READ(NLINE(IT1:IT2),FMT3,ERR=6005)X
      WRITE(MLINE(IFSTRT:IFEND),'('//NFTT//')',ERR=6005)X
      MLINEU=MLINE
      CALL  TOUPR   ( MLINEU )
      GO TO 6020
 6005 CALL NCFMT(FMT2(5:),IFIR)
      CALL NCFMT(FMT2(10:),NCNDF)
      TEMP=NLINE(IT1:IT2)
      ITC=IT2-IT1+1
      IF(ITC.GT.40)ITC=40
      IF(NIO.LT.0)THEN
        WRITE(*  ,FMT2)'Field ',IFIR,' left blank.  ',NCNDF,
     1                 ' characters entered ("'//TEMP(1:ITC)//
     2                 '") not per format('//NFTT//').'
      ELSE
        WRITE(NIO,FMT2)'Field ',IFIR,' left blank.  ',NCNDF,
     1                 ' characters entered ("'//TEMP(1:ITC)//
     2                 '") not per format('//NFTT//').'
      END IF
      MLINE(IFSTRT:IFEND)=NBLANK
      GO TO 6030
 6010 IF(IDEFT(1:IFSIZE).EQ.DDDDD(1:IFSIZE))GO TO 5030
      IF(IDEFT(1:IFSIZE).EQ.EEEEE(1:IFSIZE))GO TO 5040
      IF(IDEFT(1:IFSIZE).EQ.QQQQQ(1:IFSIZE))GO TO 5140
      IF(IDEFT(1:IFSIZE).EQ.FFFFF(1:IFSIZE))GO TO 5054
      IF(IDEFT(1:IFSIZE).EQ.UUUUU(1:IFSIZE))GO TO 5053
      IF(IDEFT(1:IFSIZE).EQ.GGGGG(1:IFSIZE))GO TO 5020
      IF(IDEFT(1:IFSIZE).EQ.IIIII(1:IFSIZE))GO TO 5059
      IF(IDEFT(1:IFSIZE).EQ.MMMMM(1:IFSIZE))GO TO 5065
      IF(IDEFT(1:IFSIZE).EQ.NNNNN(1:IFSIZE))GO TO 5062
      IF(IDEFT(1:IFSIZE).EQ.TTTTT(1:IFSIZE))GO TO 5070
 6020 IL=IL.AND.NOILDA(IFIR,IFIR,NFT,IDEF,MLINE,NIO)
      MLINEU=MLINE
      CALL  TOUPR   ( MLINEU )
 6030 IF(IFIR.GE.NF)GO TO 9990
      MLINEU=MLINE
      CALL  TOUPR   ( MLINEU )
      IFIR=IFIR+1
      IFSTRT=IFEND+1
      IT1=IT2+2
      GO TO 500
 9990 FFDEC=FFDEC.AND.IL
      RETURN
      END
