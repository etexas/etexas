      SUBROUTINE NEWPG(LN1,LN2,LN3,IPW,IPN,IPNI,LFIO)
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
C   THIS SUBROUTINE PRINTS A HEADER FOR A NEW PAGE.
C
C   LN1 - CHARACTER VARIABLE WITH TEXT TO BE CENTERED ON FIRST
C         LINE.
C   LN2 - CHARACTER VARIABLE WITH TEXT TO BE CENTERED ON SECOND
C         LINE.
C   LN3 - CHARACTER VARIABLE WITH TEXT TO BE LEFT JUSTIFIED ON
C         FOURTH LINE.
C         IF <LN1>, <LN2> OR <LN3> IS BLANK , THE TEXT FROM THE
C         PREVIOUS CALL IS USED.
C
C   IPW - PAGE WIDTH.  IF 0, PAGE WIDTH FROM LAST CALL IS USED.
C
C   IPN - PAGE NUMBER TO BE PRINTED IN UPPER RIGHT CORNER.
C         IF 0, PAGE NUMBER FROM LAST CALL IS INCREMENTED
C         BY 1 AND PRINTED.
C         IF LESS THAN 0, PAGE NUMBER NOT PRINTED.
C
C   IPNI - MUST BE 1 OR 2. DETERMINES WHICH OF TWO PAGE
C          COUNTS IS TO BE USED.
C
C   LFIO - UNIT NUMBER OF OPEN PRINT FILE.
C
      IMPLICIT          NONE                                            CCODE=C.
      CHARACTER*(*)    LN1,LN2,LN3
      INTEGER                      IPW,IPN,IPNI,LFIO
      INTEGER   NZERO  ,N1  ,N4  ,N5  ,N6  ,N7  ,N17   ,N19
      PARAMETER(NZERO=0,N1=1,N4=4,N5=5,N6=6,N7=7,N17=17,N19=19)
      CHARACTER*4 FMTI
      PARAMETER  (FMTI='(I2)')
      INCLUDE 'CMPR01'
      INCLUDE 'CMCH01'
      INCLUDE 'IVERSN'
      CHARACTER FMT1*20,NBLANK*1
      CHARACTER*80 LK1,LK2,LK3
      INTEGER   KPN
      DIMENSION KPN(2)
      INTEGER   ILNB,IT1,KPRIO,KPW,NC,NC1,NC2,NC3,NSP1,NSPA
      REAL      X
      SAVE LK1,LK2,LK3,KPN,KPW,NC1,NC2,NC3,KPRIO
      DATA FMT1,NBLANK / '(A,  X,A,:,  X,A,I )' , ' ' /
      DATA LK1,LK2,LK3 / 3*' ' /
      DATA NC1,NC2,NC3/3*0/
      IF(IPW.GT.NZERO)KPW=IPW
      IF(LFIO.GT.NZERO)KPRIO=LFIO
      NC=ILNB( LN1 )
C
      IF(NC.GT.NZERO)THEN
        LK1=LN1
        NC1=ILNB( LK1 )
      END IF
C
      IF(NC1.EQ.NZERO)NC1=N1
      IF(NC1.GT.KPW)NC1=KPW
      NSPA=(KPW-NC1)/2
C
      IF(NSPA.EQ.NZERO)THEN
        FMT1(N6:N7)=NBLANK
      ELSE
        WRITE(FMT1(N4:N5),FMTI)NSPA
      END IF
C
      IF(IPN.LT.NZERO)THEN
C
C ----- DON'T PRINT PAGE NUMBER
C
        WRITE(KPRIO,FMT1)'1',LN1(1:NC1)
        GO TO 500
      ELSE
C
C ----- PRINT PAGE NUMBER
C
        IF(IPN.EQ.NZERO)THEN
C
C   INCREMENT PAGE NUMBER FROM LAST CALL
C
          KPN(IPNI)=KPN(IPNI)+N1
        ELSE
C
C   USE PAGE NUMBER IN CALL
C
          KPN(IPNI)=IPN
        END IF
C
        X=KPN(IPNI)
        IT1=LOG10(X)+N1
        WRITE(FMT1(N19:N19),'(I1)')IT1
        NSPA=KPW-NC1-NSPA-IT1-5
        IF(NSPA.LT.N1)THEN
          NC1=NC1+NSPA-N1
          NSPA=N1
        END IF
C
        WRITE(FMT1(12:13),FMTI)NSPA
        WRITE(KPRIO,FMT1)'1',LK1(1:NC1),'PAGE ',KPN(IPNI)
      END IF
C
  500 CONTINUE
      NC=ILNB( LN2 )
C
      IF(NC.GT.NZERO)THEN
        LK2=LN2
        NC2=ILNB( LK2 )
      END IF
      IF(NC2.EQ.NZERO)NC2=N1
C
      NSPA=(KPW-NC2)/2
C
      IF(NSPA.LT.N1)THEN
        WRITE(KPRIO,'(1X,4A)')LK2(1:KPW-8),' (',IVERSN,')'
        GO TO 1000
      ELSE
        WRITE(FMT1(N4:N5),FMTI)NSPA
        FMT1(N6:N7)='X,'
        NSP1=KPW-NSPA-NC2-N7
        IF(NSP1.LT.N1)THEN
          NC2=NC2+NSP1-N1
          NSP1=N1
        END IF
        WRITE(FMT1(12:13),FMTI)NSP1
        WRITE(KPRIO,FMT1)NBLANK,LK2(1:NC2),'('//IVERSN//')'
      END IF
C
 1000 CONTINUE
      NC=ILNB( LN3 )
C
      IF(NC.GT.NZERO)THEN
        LK3=LN3
        NC3=ILNB( LK3 )
      END IF
C
      IF(NC3.EQ.NZERO)NC3=N1
      IF(NC3.GT.KPW)NC3=KPW
      WRITE(KPRIO,'(1H0,A/1H0)')LK3(1:NC3)
      RETURN
      END
