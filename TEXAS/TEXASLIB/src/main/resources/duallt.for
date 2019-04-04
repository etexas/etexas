      FUNCTION DUALLT(ILIST,MOL,I,NMPHA,DLTS)
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
C ----- THIS LOGICAL FUNCTION CHECKS TO SEE IF THE CURRENT PHASE
C ----- SEEMS TO BE DUAL LEFTS, FOLLOWED BY TWO SINGLE LEFTS
C ----- BY CHECKING MOVEMENT PATTERNS.
C ----- IF SO <DUALLT> IS SET TO .TRUE., OTHERWISE .FALSE.
C ----- IF THERE ARE ALSO DUAL STRAIGHTS FOLLOWING THE TWO SINGLE
C ----- LEFTS, <DLTS> IS SET TO .TRUE., OTHERWISE .FALSE.
C   I - PHASE BEING CHECKED
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  DUALLT
      INTEGER         ILIST(4,8),
     1                      MOL(8),
     2                          I,NMPHA
      LOGICAL                           DLTS
      INTEGER   NZERO  ,N1  ,N2  ,N3  ,N8
      PARAMETER(NZERO=0,N1=1,N2=2,N3=3,N8=8)
      LOGICAL   T       ,F
      CHARACTER*2                  ICUN
      CHARACTER*1                            ICBL    ,ICL
      PARAMETER(T=.TRUE.,F=.FALSE.,ICUN='UN',ICBL=' ',ICL='L')
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'CMTX01'
      INCLUDE 'CMTXRD'
      CHARACTER ITC*2,MVILU*4,ITCU*2
      LOGICAL SINGST,BLUNLT,SINGLT,EUALLT
      INTEGER             NPHA
      EQUIVALENCE(MOUT(9),NPHA)
      INTEGER   ISS,IT1,IT2,IT3,IT4,IT5,IT6,IT7,J,K,NL1,NL2
      GO TO 4000
      ENTRY EUALLT(I,DLTS)
C
C ----- THIS ENTRY POINT IS FOR CHECKING BY INSPECTING SIGNAL
C ----- SEQUENCE DATA
C
      IF(I.LT.NZERO)THEN
C
C ----- SIG SEQ DATA IS OFFSET IN <MLINE> BY <NPHOL>
C
        ISS=-I+NPHOL
      ELSE
        ISS=I
      END IF
      IF(NPHA.LT.N3)GO TO 9900
      NL1=NZERO
      NL2=NZERO
      IT1=-N1
      DO 1000 J=N1,NLEGS
      DO 500 K=N1,NINLNS(J)
      IT1=IT1+N2
      ITC=MLINE(ISS)(IT1:IT1+N1)
      IT2=K
      MVILU=MVIL(K,J)
      CALL  TOUPR   ( MVILU )
      IF(INDEX(MVILU,ICL).EQ.NZERO)GO TO 510
C
C ----- THERE IS "L" IN MOVEMENT CODE FOR LANE
C
      ITCU=ITC
      CALL  TOUPR   ( ITCU )
      IF(ITCU.NE.ICL)THEN
C
C ----- THERE ISN'T "L" IN SIGNAL DATA FOR THE LANE
C
        NL1=NZERO
        GO TO 510
      END IF
C
C ----- THERE IS "L" IN SIGNAL DATA FOR THE LANE
C
      NL1=J
  500 CONTINUE
      IF(NL1.GT.NZERO)GO TO 1010
      GO TO 1000
  510 CONTINUE
C
C ----- CHECK FOR NO ADDN'L SIGNALIZED MOVEMENTS ON THE LEG
C
      IT2=IT1+(NINLNS(J)-K)*N2
      IF(.NOT.BLUNLT(MLINE(ISS),IT1,IT2))GO TO 9900
      IT1=IT2
      IF(NL1.GT.NZERO)GO TO 1010
 1000 CONTINUE
      GO TO 9900
 1010 CONTINUE
      DO 2000 J=NL1+N1,NLEGS
      DO 1500 K=N1,NINLNS(J)
      IT1=IT1+N2
      ITC=MLINE(ISS)(IT1:IT1+N1)
      IT2=K
      MVILU=MVIL(K,J)
      CALL  TOUPR   ( MVILU )
      IF(INDEX(MVILU,ICL).EQ.NZERO)GO TO 1510
      ITCU=ITC
      CALL  TOUPR   ( ITCU )
      IF(ITCU.NE.ICL)THEN
        NL2=NZERO
        GO TO 1510
      END IF
      NL2=J
 1500 CONTINUE
      IF(NL2.GT.NZERO)GO TO 2010
      GO TO 2000
 1510 CONTINUE
      IT2=IT1+(NINLNS(J)-K)*N2
      IF(.NOT.BLUNLT(MLINE(ISS),IT1,IT2))GO TO 9900
      IT1=IT2
      IF(NL2.GT.NZERO)GO TO 2010
 2000 CONTINUE
      GO TO 9900
 2010 CONTINUE
      IF(.NOT.BLUNLT(MLINE(ISS),IT1,NINLNT*N2-N1))GO TO 9900
C
C ----- FOUND DUAL LEFTS, ARE NEXT PHASES TWO SINGLE LEFTS ?
C
      CALL ISSLT(ISS,NPHA,I)
      NLINE=MLINE(ISS)
      IT1=NZERO
      DO 2030 J=N1,NL1-N1
 2030 IT1=IT1+NINLNS(J)
      IT1=IT1+IT1+N1
      IT2=NZERO
      DO 2035 J=NL1,NL2-N1
 2035 IT2=IT2+NINLNS(J)
      IT2=IT1+IT2+IT2
      IT3=NINLNT*N2-N1
      IT4=IT2+NINLNS(NL2)*N2
      IT5=IT1+NINLNS(NL1)*N2
      IF(.NOT.SINGLT(MVIL(N1,NL1),NLINE(IT1:),NINLNS(NL1)))THEN
        IF(.NOT.SINGLT(MVIL(N1,NL2),NLINE(IT2:),NINLNS(NL2)))
     1     GO TO 9900
        IF(.NOT.BLUNLT(NLINE,N1,IT2-N2))GO TO 9990
        IF(.NOT.BLUNLT(NLINE,IT4,IT3))GO TO 9990
C
C ----- FOUND SINGLE LEFT CORRESPONDING TO SECOND (PER LEG NO. SEQ.)
C ----- DUAL LEFT ON FIRST PHASE FOLLOWING THE DUAL LEFT PHASE
C
        CALL ISSLT(ISS,NPHA,I)
        NLINE=MLINE(ISS)
        IF(.NOT.SINGLT(MVIL(N1,NL1),NLINE(IT1:),NINLNS(NL1)))
     1     GO TO 9900
        IF(.NOT.BLUNLT(NLINE,N1,IT1-N2))GO TO 9990
        IF(.NOT.BLUNLT(NLINE,IT5,IT3))GO TO 9990
C
C ----- FIRST LEFT ... SECOND FOLLOWING PHASE
C
      ELSE
        IF(.NOT.BLUNLT(NLINE,N1,IT1-N2))GO TO 9990
        IF(.NOT.BLUNLT(NLINE,IT5,IT3))GO TO 9990
C
C ----- FIRST LEFT ... FIRST FOLLOWING PHASE
C
        CALL ISSLT(ISS,NPHA,I)
        NLINE=MLINE(ISS)
        IF(.NOT.SINGLT(MVIL(N1,NL2),NLINE(IT2:),NINLNS(NL2)))
     1     GO TO 9900
        IF(.NOT.BLUNLT(NLINE,N1,IT2-N2))GO TO 9990
        IF(.NOT.BLUNLT(NLINE,IT4,IT3))GO TO 9990
C
C ----- SECOND LEFT ... SECOND FOLLOWING PHASE
C
      END IF
      DUALLT=T
C
C ----- CHECK FOR DUAL STRAIGHTS FOLLOWING THE TWO SINGLE LEFTS
C
      IT1=NZERO
      DO 4100 J=N1,NL1-N1
 4100 IT1=IT1+NINLNS(J)
      IT1=IT1*N2-N1
      CALL ISSLT(ISS,NPHA,I)
      NLINE=MLINE(ISS)
      IF(.NOT.BLUNLT(NLINE,N1,IT1))GO TO 8200
      IF(.NOT.SINGST(MVIL(N1,NL1),NLINE(IT1+N2:),NINLNS(NL1)))GO TO 8200
      IT1=IT1+NINLNS(NL1)*N2
      IT2=NZERO
      DO 4200 J=NL1+N1,NL2-N1
 4200 IT2=IT2+NINLNS(J)
      IT2=IT2*N2+IT1
      IT1=IT1+N2
      IF(.NOT.BLUNLT(NLINE,IT1,IT2))GO TO 8200
      IF(.NOT.SINGST(MVIL(N1,NL2),NLINE(IT2+N2:),NINLNS(NL2)))GO TO 8200
      IT2=IT2+NINLNS(NL2)*N2+N2
      IF(.NOT.BLUNLT(NLINE,IT2,NINLNT*N2-N1))GO TO 8200
      DLTS=T
      GO TO 9990
 4000 CONTINUE
      IF(NPHA.LT.N3)GO TO 9900
C
C ----- ARE BOTH MOVEMENTS LEFTS?
C
      IT1=ILIST(N1,I)
      IT5=MOL(IT1)
      IF(IT5.GT.NZERO)GO TO 9900
      IT2=ILIST(N2,I)
      IT6=MOL(IT2)
      IF(IT6.GT.NZERO)GO TO 9900
      IT3=I+N1
      IF(IT3.GT.NPHA)IT3=N1
      IT4=IT3+N1
      IF(IT4.GT.NPHA)IT4=N1
      IF((IT1.EQ.ILIST(N1,IT3)) .OR. (IT1.EQ.ILIST(N2,IT3)))THEN
        IF((IT2.NE.ILIST(N1,IT4)).AND.(IT2.NE.ILIST(N2,IT4)))GO TO 9900
        DUALLT=T
        GO TO 8000
      END IF
      IF((IT2.EQ.ILIST(N1,IT3)) .OR. (IT2.EQ.ILIST(N2,IT3)))THEN
        IF((IT1.NE.ILIST(N1,IT4)).AND.(IT1.NE.ILIST(N2,IT4)))GO TO 9900
        DUALLT=T
        GO TO 8000
      END IF
      GO TO 9900
 8000 IF(NPHA.LT.4)GO TO 8200
      IT7=IT4+N1
      IF(IT7.GT.NPHA)IT7=N1
      IT5=-IT5
      DO 8100 J=N1,N8
      IF(MOL(J).NE.IT5)GO TO 8100
      IT5=J
      GO TO 8120
 8100 CONTINUE
      GO TO 8200
 8120 CONTINUE
      IT6=-IT6
      DO 8140 J=N1,N8
      IF(MOL(J).NE.IT6)GO TO 8140
      IT6=J
      GO TO 8160
 8140 CONTINUE
      GO TO 8200
 8160 CONTINUE
      IF(((IT5.EQ.ILIST(N1,IT7)) .AND. (IT6.EQ.ILIST(N2,IT7))) .OR.
     1   ((IT6.EQ.ILIST(N1,IT7)) .AND. (IT5.EQ.ILIST(N2,IT7))))THEN
        DLTS=T
        GO TO 9990
      END IF
 8200 CONTINUE
      DLTS=F
      GO TO 9990
 9900 CONTINUE
      DUALLT=F
 9990 CONTINUE
      RETURN
      END
C
C
C
      FUNCTION SINGLT(MVIL,MLINE,NINLNS)
C
C ----- THIS LOGICAL FUNCTION CHECKS MOVEMENT CODES AND SIGNAL
C ----- INDICATIONS FOR A PHASE AND LEG TO SEE IF IT APPEARS TO
C ----- HAVE PROTECTED LEFTS AND ALSO THAT ALL MOVEMENTS (U-TURNS
C ----- EXCEPTED) ON THE LEG HAVE GREEN SIGNAL INDICATIONS.
C ----- IF SO, <SINGLT> IS SET TO .TRUE., OTHERWISE .FALSE.
C ----- THE PARAMETERS MUST BE SPECIFIC TO THE LEG AND PHASE OF
C ----- INTEREST.
C
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  SINGLT
      CHARACTER*4     MVIL(6)
      CHARACTER*(*)        MLINE
      INTEGER                    NINLNS
      INTEGER   NZERO  ,N1  ,N2
      CHARACTER*1                 ICC    ,ICL    ,ICS    ,ICR
      PARAMETER(NZERO=0,N1=1,N2=2,ICC='C',ICL='L',ICS='S',ICR='R')
      CHARACTER MVU*4,ML*2,MLU*2
      INTEGER   I,INDR,INDS,IT1
      IT1=-N1
      DO 1000 I=N1,NINLNS
      MVU=MVIL(I)
      CALL  TOUPR   ( MVU )
      IT1=IT1+N2
      ML=MLINE(IT1:IT1+N1)
      MLU=ML
      CALL  TOUPR   ( MLU )
      IF(MVU.EQ.ICL)THEN
C
C ----- MOVEMENT CODE IS "L", ONLY
C
        IF(MLU.NE.ICL)GO TO 9900
        GO TO 1000
      END IF
      IF(INDEX(MVU,ICL).GT.NZERO)THEN
C
C ----- MOVEMENT CODE HAS "L" & OTHERS
C
        IF(INDEX(MLU,ICL).EQ.NZERO)GO TO 9900
        INDS=INDEX(MVU,ICS)
        INDR=INDEX(MVU,ICR)
        IF((INDS.GT.NZERO) .OR. (INDR.GT.NZERO))THEN
          IF(INDEX(MLU,ICC).EQ.NZERO)GO TO 9900
        END IF
        GO TO 1000
      END IF
      IF(INDEX(MLU,ICC).GT.NZERO)GO TO 1000
      IF(MLU.EQ.'UN')GO TO 1000
      INDS=INDEX(MVU,ICS)
      IF((INDS.GT.NZERO) .AND. (INDEX(MLU,ICS).EQ.NZERO))GO TO 9900
      INDR=INDEX(MVU,ICR)
      IF((INDR.GT.NZERO) .AND. (INDEX(MLU,ICR).EQ.NZERO))GO TO 9900
 1000 CONTINUE
      SINGLT=.TRUE.
      GO TO 9990
 9900 SINGLT=.FALSE.
 9990 RETURN
      END
C
C
C
      SUBROUTINE ISSLT(ISS,NPHA,I)
C
C ----- THIS SUBROUTINE INCREMENTS A PHASE COUNTER AND, IF REQ'D,
C ----- ADJUSTS THE COUNTER TO "WRAP AROUND" TO THE FIRST PHASE.
C
      IMPLICIT          NONE                                            CCODE=C.
      INTEGER          ISS,NPHA,I
      ISS=ISS+1
      IF(I.LT.0)THEN
C
C ----- DATA FOR FIRST PHASE IS IN POSITION <NPHA> + 1
C
        IF(ISS.LE.NPHA+NPHA)GO TO 9990
      ELSE
C
C ----- DATA FOR FIRST PHASE IS IN POSITION 1
C
        IF(ISS.LE.NPHA)GO TO 9990
      END IF
      ISS=ISS-NPHA
 9990 RETURN
      END
C
C
C
      FUNCTION BLUNLT(NLINE,IT1,IT2)
C ----- THIS LOGICAL FUNCTION CHECKS CONSECUTIVE 2-CHARACTER FIELDS
C ----- IN <NLINE> TO MAKE SURE THAT THEY CONTAIN ONLY "UN" OR
C ----- ARE BLANK, AND SETS <BLUNLT> TO .TRUE. IF SO, OTHERWISE, TO
C ----- .FALSE.  <IT1> IS THE CHARACTER POSITION OF THE START OF
C ----- THE FIRST FIELD TO BE CHECKED, <IT2> OF THE LAST FIELD.
 
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  BLUNLT
      CHARACTER*(*)   NLINE
      INTEGER               IT1,IT2
      CHARACTER*1 IBL
      INTEGER             N1
      PARAMETER  (IBL=' ',N1=1)
      CHARACTER*80 NLINEU
      INTEGER     I
      IF(IT2.LT.IT1)GO TO 9000
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(NLINEU(IT1:IT2+N1).EQ.IBL)GO TO 9000
      DO 100 I=IT1,IT2,2
      IF(NLINEU(I:I+N1).EQ.IBL)GO TO 100
      IF(NLINEU(I:I+N1).EQ.'UN')GO TO 100
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
      FUNCTION SINGST(MVIL,NLINE,NINLNS)
      IMPLICIT          NONE                                            CCODE=C.
      LOGICAL  SINGST
      CHARACTER*4     MVIL(6)
      CHARACTER*(*)        NLINE
      INTEGER                    NINLNS
      INTEGER     NZERO  ,N1  ,N2
      CHARACTER*1                   ICS    ,ICR
      PARAMETER  (NZERO=0,N1=1,N2=2,ICS='S',ICR='R')
      CHARACTER NLINEU*80,MVILU*4
      INTEGER     I,IP1,IT1
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      IF(INDEX(NLINEU(1:NINLNS+NINLNS),'L').GT.NZERO)GO TO 9900
      IT1=-N1
      DO 1000 I=N1,NINLNS
      IT1=IT1+N2
      IP1=IT1+N1
      IF(INDEX(NLINEU(IT1:IP1),'C').GT.NZERO)GO TO 1000
      IF(NLINEU(IT1:IP1).EQ.'UN')GO TO 1000
      MVILU=MVIL(I)
      CALL  TOUPR   ( MVILU )
      IF((INDEX(MVILU,ICS).GT.NZERO) .AND.
     1   (INDEX(NLINEU(IT1:IP1),ICS).EQ.NZERO))GO TO 9900
      IF((INDEX(MVILU,ICR).GT.NZERO) .AND.
     1   (INDEX(NLINEU(IT1:IP1),ICR).EQ.NZERO))GO TO 9900
 1000 CONTINUE
      SINGST=.TRUE.
      GO TO 9990
 9900 SINGST=.FALSE.
 9990 RETURN
      END
