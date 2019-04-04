      SUBROUTINE YESNO(PROMPT,NLINE,ALT)
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
C   THIS SUBROUTINE PROMPTS THE USER FOR A KEYIN OF "YES", "NO"
C   OR ONE OF MORE ALTERNATE RESPONSES.
C
C   <PROMPT> - CHARACTER VARIABLE THAT CONTAINS THE PROMPT
C   <NLINE> - CHARACTER VARIABLE THAT IS SET TO THE KEYED IN RESPONSE.
C   <ALT> - A DIMENSIONED CHARACTER VARIABLE THAT CONTAINS THE
C           ALTERNATE RESPONSE INFORMATION.
C           IF THE FIRST TEXT STRING OF <ALT> = "NONE", THEN NO
C              ALTERNATE RESPONSE WILL BE ACCEPTED.
C           IF THE FIRST CHARACTER OF THE FIRST TEXT STRING
C              OF <ALT> = "$", THEN THE TEXT FOLLOWING THE "$"
C              WILL BE USED FOR THE ERROR MESSAGE AND EACH
C              OF THE NEXT TEXT STRINGS WILL BE AN ALTERNATE
C              RESPONSE TO THE PROMPT.  ANY ADDITIONAL TEXT
C              STRINGS IN <ALT> THAT BEGIN WITH "$" WILL ALSO
C              BE CONSIDERED PART OF THE ERROR MESSAGE.
C              THE LAST TEXT STRING MUST = "NONE".
C           IF THE FIRST TEXT STRING OF <ALT> IS NOT = "$..."
C              OR "NONE" THEN THE FIRST TEXT STRING IS
C              THE ONLY ALTERNATE RESPONSE.
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)    PROMPT,NLINE,ALT(*)
      CHARACTER*12 OAFORM
      PARAMETER   (OAFORM='(A:A:/A:A:A)')
      CHARACTER NYES*3,NNO*2,NLINEU*80
      INTEGER   I,ILNB,L,NC
      DATA NYES,NNO/'YES','NO'/
      WRITE(*,OAFORM)PROMPT
  100 CONTINUE
      READ(*,OAFORM,END=400)NLINE
      NLINEU=NLINE
      CALL  TOUPR   ( NLINEU )
      NC=MIN0(MAX0(ILNB( NLINE ),1),LEN( NYES ))
      IF(NLINEU(1:NC).EQ.NYES(1:NC))THEN
        NLINE=NYES
        GO TO 9990
      END IF
      NC=MIN0(MAX0(ILNB( NLINE ),1),LEN( NNO ))
      IF(NLINEU(1:NC).EQ.NNO (1:NC))THEN
        NLINE=NNO
        GO TO 9990
      END IF
      IF(ALT(1)(1:MIN0(LEN( ALT(1) ),4)).EQ.'NONE')THEN
C
C ----- NO ALTERNATE KEYINS SUPPLIED
C
        NC=MAX0(ILNB( NLINE ),1)
        WRITE(*,OAFORM)'Keyin "YES" or "NO", not: "',
     1                 NLINE(1:NC),'"'
      ELSE
C
C ----- CHECK FOR MULTIPLE ALTERNATES
C
        IF(ALT(1)(1:1).EQ.'$')THEN
C
C ----- PROCESS MULTIPLE ALTERNATES
C
          I=2
  310     CONTINUE
          L=ILNB( ALT(I) )
          IF(L.EQ.0)L=LEN( ALT(I) )
          IF(L.GT.LEN( NLINE ))L=LEN( NLINE )
          IF(ALT(I)(1:L).EQ.'NONE')GO TO 320
          IF(NLINEU(1:L).EQ.ALT(I)(1:L))GO TO 9990
          I=I+1
          GO TO 310
  320     CONTINUE
          NC=ILNB( ALT(1) )
          WRITE(*,OAFORM)'Keyin "YES", "NO", or ',ALT(1)(2:NC)
          I=2
  330     CONTINUE
          IF(ALT(I)(1:4).EQ.'NONE')GO TO 340
          IF(ALT(I)(1:1).EQ.'$')WRITE(*,OAFORM)ALT(I)(2:)
          I=I+1
          GO TO 330
        ELSE
C
C ----- PROCESS SINGLE ALTERNATE
C
          L=ILNB( ALT(1) )
          IF(NLINEU(1:L).EQ.ALT(1)(1:L))GO TO 9990
          NC=MAX0(ILNB( NLINE ),1)
          WRITE(*,OAFORM)'Keyin "YES", "NO", or "',ALT(1)(1:L),
     1                   '", not: "',NLINE(1:NC),'"'
        END IF
  340   CONTINUE
      END IF
      GO TO 100
  400 CONTINUE
      NLINE='EOF'
 9990 CONTINUE
      RETURN
      END
