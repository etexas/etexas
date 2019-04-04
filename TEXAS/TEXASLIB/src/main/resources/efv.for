      FUNCTION EFV(MLINE,NLINE,TLINE,NF,NINLNS,FLAG,EK)
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
C ----- THIS LOGICAL FUNCTION EDITS A DATA FIELD IN
C ----- A VERTICAL FIELD COLUMN.
C ----- THE DATA FIELDS ARE DESCRIBED BY THE DATA IN COMMON BLOCK
C ----- /SRDATA/.
C
C   MLINE - DIMENSIONED CHARACTER VARIABLE WITH A MULTI-FIELD DATA
C           LINE IN EACH.
C   NLINE - CHARACTER VARIABLE WITH EDIT INSTRUCTIONS.
C   TLINE -
C   NF - NUMBER OF DATA LINES IN <MLINE> (MAX=30).
C   NINLNS - NUMBER OF INBOUND LANES
C   FLAG - IF NEGATIVE, USE TEXT FROM EK(2) AND UP FOR SECOND SUBSCRIPT
C   EK - EDIT REQUEST KEYWORD.
C        FOR SOME CASES, ARRAY ELEMENTS 2 THRU * ARE USED TO
C        DETERMINE THE SECOND SUBSCRIPT.
C   EFV - LOGICAL VARIABLES THAT RETURNS <TRUE> IF THE EDIT IS
C         SUCESSFUL, OTHERWISE <FALSE>.
C
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  EFV
      CHARACTER*(*)MLINE(*),
     1                   NLINE,TLINE               ,EK(*)
      INTEGER                        NF,NINLNS,FLAG
      INCLUDE 'CMPR01'
      INCLUDE 'CMTXSR'
      INTEGER   NFP
      PARAMETER(NFP=6)
      CHARACTER NFT1(30)*4,XDEF1(30)*10,XDEFT*10,EDREQ*10,EDREQU*10
      CHARACTER*80 EKU
      LOGICAL   DFF1,FFDEC
      REAL      RANGE1
      DIMENSION RANGE1(2,30)
      INTEGER   I,IFIELD,II,ILNB,IFSIZE,IT1,IT2,IT3,IT4,J,L,NC,NC1
      REAL      TR1,TR2,X
      II=-1
      IFIELD=1
      J=1
      IF(                        (FLAG.LT.0) .OR.
     1   ((ICODE.GE.23) .AND. (ICODE.LE.28)) .OR.
     2                         (ICODE.EQ.30) .OR.
     3                         (ICODE.EQ.31) .OR.
     4                         (ICODE.EQ.47) .OR.
     5                         (ICODE.EQ.48) .OR.
     6                         (ICODE.EQ.49))THEN
C
C ----- USE CHARACTER ARRAY TO FIND SECOND SUBSCRIPT
C
        CALL READIN(NLINE,EDREQ,L,IFIELD,J,L,X,TLINE,II,-299,EK(2))
      ELSE
C
C ----- NUMBERS FOR SECOND SUBSCRIPT
C
        CALL READIN(NLINE,EDREQ,L,IFIELD,J,L,X,TLINE,II,0,'dummy')
      END IF
      EDREQU=EDREQ
      CALL  TOUPR   ( EDREQU )
      IF(EDREQU.EQ.'READ ERROR')GO TO 9300
      NC=ILNB( EK(1) )
      NC1=NC
      IF(NC1.GT.10)NC1=10
      EKU=EK(1)
      CALL  TOUPR   ( EKU )
      IF(EDREQU(1:NC1).EQ.EKU(1:NC))THEN
C
C ----- GENERIC VERTICAL FIELD (COLUMN)EDIT REQUEST
C
        IF((NC.LT.10) .AND. (EDREQU(NC+1:NC+1).NE.' '))GO TO 9250
        IF(IFIELD.GT.IABS(NFIELD))GO TO 9350
        IF(J.GT.NF)GO TO 9350
        IF(IFIELD.EQ.1)THEN
          IT1=1
          IT2=NFS(1)
          GO TO 600
        END IF
        GO TO 500
      END IF
      IF(ICODE.NE.4)GO TO 9350
      IF(EDREQU(1:6).EQ.'WIDTH')THEN
C
C   WIDTH EDIT REQUEST
C
        IT1=1
        IT2=NFS(1)
        IFIELD=1
        GO TO 600
      END IF
      IF((EDREQU(1:4).EQ.'PER') .OR. (EDREQU(1:8).EQ.'PERCENT'))THEN
C
C   PERCENTAGE EDIT REQUEST
C
        IFIELD=6
        GO TO 500
      END IF
      EDREQU=EDREQ
      CALL  TOUPR   ( EDREQU )
      IF((EDREQU(1:5).EQ.'MOVE') .OR. (EDREQU(1:5).EQ.'MVMT') .OR.
     1   (EDREQU(1:8).EQ.'MOVEMENT'))THEN
C
C   MOVEMENT EDIT REQUEST
C
        IFIELD=2
        GO TO 500
      END IF
      IF((EDREQU(1:4).EQ.'OFF'). OR. (EDREQU(1:7).EQ.'OFFSET'))THEN
C
C   OFFSET OF STOPLINE REQUEST
C
        IFIELD=5
        GO TO 500
      END IF
C
C   REQUEST NOT RECOGNIZED
C
      GO TO 9350
  500 CONTINUE
      IT1=0
      DO 550 I=1,IFIELD-1
      IT1=IT1+NFS(I)
  550 CONTINUE
      IT2=IT1+NFS(IFIELD)
      IT1=IT1+1
  600 CONTINUE
      IFSIZE=IT2-IT1+1
      EDREQU=EDREQ
      CALL  TOUPR   ( EDREQU )
      EKU=EK(1)
      CALL  TOUPR   ( EKU )
      IF(EDREQU.EQ.EKU)THEN
C
C ----- SECOND SUBSCRIPT IS FOR ROW
C
        IF(.NOT.DFF1(TLINE,J,NF))GO TO 9300
      ELSE
C
C ----- FIRST SUBSCRIPT IS FOR ROW
C
        IF(.NOT.DFF1(TLINE,IFIELD,IABS(NFIELD)))GO TO 9300
      END IF
      IT4=0
      TR1=RANGE(1,IFIELD)
      TR2=RANGE(2,IFIELD)
      XDEFT=XDEF(IFIELD)
      DO 9210 I=1,NF
      RANGE1(1,I)=RANGE(1,I)
      RANGE1(2,I)=RANGE(2,I)
      IF((ICODE.EQ.4) .AND. (IFIELD.EQ.NFP))THEN
C
C ----- LANE DATA ENTERING PERCENTS
C
        CALL PCTRNG(I,NINLNS,RANGE(1,I),RANGE(2,I),TR1,TR2)
      ELSE
        RANGE(1,I)=TR1
        RANGE(2,I)=TR2
      END IF
      XDEF1(I)(1:IFSIZE)=XDEFT
      NFT1(I)=NFT(IFIELD)
C
C ----- CHANGE DATA FIELD COLUMN TO DATA LINE
C
      IT3=IT4+1
      IT4=IT4+IFSIZE
      NLINE(IT3:IT4)=MLINE(I)(IT1:IT2)
 9210 CONTINUE
      EFV=FFDEC(1,NF,NFT1,TLINE,NC,NLINE,XDEF1,-99)
      IT4=0
      DO 9220 I=1,NF
C
C ----- RESTORE DATA RANGES
C
      RANGE(1,I)=RANGE1(1,I)
      RANGE(2,I)=RANGE1(2,I)
C
C ----- CHANGE DATA LINE BACK TO FIELD COLUMN
C
      IT3=IT4+1
      IT4=IT4+IFSIZE
      MLINE(I)(IT1:IT2)=NLINE(IT3:IT4)
 9220 CONTINUE
      NLINE=' '
      IF(.NOT.EFV)GO TO 9250
      EDREQU=EDREQ
      CALL  TOUPR   ( EDREQU )
      IF(EDREQU(1:3).EQ.'PER')NLINE(1:7)='PERCENT'
      EFV=.TRUE.
      GO TO 9400
 9250 CONTINUE
C
C ----- ERROR IN REQUEST KEYWORD
C
      NLINE(1:10)='REQUEST ER'
      GO TO 9350
 9300 CONTINUE
      NLINE(1:10)='KEYIN ERRO'
 9350 CONTINUE
      EFV=.FALSE.
 9400 CONTINUE
 9990 RETURN
      END
