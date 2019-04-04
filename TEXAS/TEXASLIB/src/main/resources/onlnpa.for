C *** ************************************************************** ***
C *** *                                                            * ***
C *** * COPYRIGHT (C) 2008 by Rioux Engineering, Austin, Texas USA * ***
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
C *** * or implied warranty.  Rioux Engineering makes no           * ***
C *** * representation about the suitability of this software for  * ***
C *** * any purpose and accepts no responsibility for its use.     * ***
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
      FUNCTION DIONLN (IL,X,Y)
C
C-----FIND DISTANCE FROM START OF LANE TO POINT ON LANE
C
C-----IL LANE NUMBER
C-----X,Y - COORDINATESS OF THE POINT
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INTEGER IL
      DOUBLE PRECISION X,Y
      DOUBLE PRECISION DIONLN
      DIONLN = DSQRT((BASELX(IL)-X)**2 + (BASELY(IL)-Y)**2)
      RETURN
      END                                                               LGONPA
C
C
C
      FUNCTION DIONPA (IP,CST,X,Y)
C
C-----FIND DISTANCE ALONG PATH FROM START OF PATH TO POINT ON PATH
C
C-----IP - PATH NUMBER
C-----CST - CURRENT STATUS OF THE POINT
C-----X,Y - COORDINATESS OF THE POINT
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INTEGER IP,CST
      DOUBLE PRECISION X,Y
      DOUBLE PRECISION DIONPA,CHORD,A
      DIONPA = 0.0D0
      IF (CST .LT. 1) THEN
C-----NOT YET ON THIS PATH
        RETURN
      END IF
      IF (CST .EQ. 1) THEN
C-----FIRST LINE OF PATH
        DIONPA = DIONPA +
     *           DSQRT((PTH1X1 (IP) - X)**2 + (PTH1Y1 (IP) - Y)**2)
        RETURN
      END  IF
      DIONPA = DIONPA + PTH1L (IP)
      IF (CST .EQ. 2) THEN
C-----ON FIRST ARC OF PATH
        CHORD = DSQRT((PTH2X1 (IP) - X)**2 + (PTH2Y1 (IP) - Y)**2)
C-----COSINE RULE
C-----B*B = C*C+A*A + 2*A*C*COS(BETA)
C-----COS(BETA) = (B*B - C*C - A*A) / (-2*A*C)
C-----COS(BETA) = (B*B-2*A*A) / (2*A*A)                   A = C = RADIUS
C-----COS(BETA) =
C-----BETA = INVERSE COS(((B*B) / (2*A*A)) -1)
        A = DACOS(DABS((CHORD*CHORD)/(2.0D0*PTH2R(IP)*PTH2R(IP))-1.0D0))
        DIONPA = DIONPA + DABS(A) * PTH2R (IP)
        RETURN
      END  IF
      DIONPA = DIONPA + PTH2L (IP)
      IF (CST .EQ. 3) THEN
C-----ON SECOND ARC OF PATH
        CHORD = DSQRT((PTH3X1 (IP) - X)**2 + (PTH3Y1 (IP) - Y)**2)
C-----COSINE RULE
C-----B*B = C*C+A*A + 2*A*C*COS(BETA)
C-----COS(BETA) = (B*B - C*C - A*A) / (2*A*C)
C-----BETA = INVERSE COS((B*B - C*C - A*A) / (2*A*C))
        A = DACOS(DABS((CHORD*CHORD)/(2.0D0*PTH2R(IP)*PTH2R(IP))-1.0D0))
        DIONPA = DIONPA + DABS(A) * PTH2R (IP)
        RETURN
      END  IF
      DIONPA = DIONPA + PTH3L (IP)
      IF (CST .EQ. 4) THEN
C-----SECOND LINE OF PATH
        DIONPA = DIONPA +
     *           DSQRT((PTH4X1 (IP) - X)**2 + (PTH4Y1 (IP) - Y)**2)
        RETURN
      END  IF
      IF ( CST .EQ.5 )                           THEN
C-----POINT IS PAST THIS PATH
        RETURN
      END  IF
      RETURN
      END                                                               LGONPA
C
C
C
      SUBROUTINE IPR2(VEHV,IVEHCL,UNITS,ILP,IPE)
C
C-----VEHICLE LOGGING IN
C
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (VEH), TARGET  :: VEHV
      DOUBLE PRECISION                      IPE
      INTEGER              IVEHCL,UNITS,ILP
      VEHV%CLASS = IVEHCL
      VEHV%FBS   = 2
      VEHV%INUSE  = 1
      VEHV%LNCHG  = .FALSE.
      VEHV%LNCHGC = .FALSE.
      VEHV%LNCHGD = 0.0D0
      VEHV%UNITS  = UNITS
      VEHV%UNIT%FPS = 0
      VEHV%UNIT%RPS = 0
      VEHV%UNIT%HPS = 0
      VEHV%UNIT%LNCHGD = 0.0D0
      CALL LPCLER (VEHV%LNPA)
      CALL LPPUSH (VEHV%LNPA,ILP,IPE,.TRUE.)
      RETURN
      END                                                               IPR2
C
C
C
      SUBROUTINE IPR3(VEHV,IPE,ILP)
C
C-----VEHICLE MOVING FROM LANE TO PATH
C
C-----VEHV - POINTER TO VEHICLE STRUCTURE
C-----ILP - NEW PATH
C-----IPE - DISTANCE DOWN PATH
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (VEH), TARGET  :: VEHV
      DOUBLE PRECISION     IPE
      INTEGER                  ILP
      IF (VEHV%LNCHG .OR. VEHV%LNCHGC)           THEN
C-----LANE CHANGE NOT COMPLETE, FORCE COMPLETION
        VEHV%LNCHGD      = 0.0D0
        VEHV%UNIT%LNCHGD = 0.0D0
        VEHV%LNCHG  = .FALSE.
        VEHV%LNCHGC = .FALSE.
      END IF
      IF (VEHV%FBS .EQ. 4)                       THEN
C-----FROM INTERNALS TO PATH
        VEHV%FBS = -3
      ELSE
C-----FROM INBOUND TO PATH
        VEHV%FBS = 3
      END IF
      CALL LPPUSH (VEHV%LNPA,ILP,IPE,.FALSE.)
      RETURN                                                            IPR3
      END
C
C
C
      SUBROUTINE IPR4(VEHV,IPE,ILP)
C
C-----VEHICLE MOVING FROM PATH TO LANE
C
C-----VEHV - POINTER TO VEHICLE STRUCTURE
C-----ILP - NEW LANE
C-----IPE - DISTANCE DOWN LANE
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (VEH), TARGET  :: VEHV
      DOUBLE PRECISION     IPE
      INTEGER                  ILP
      IF (VEHV%FBS .EQ. -3)                      THEN
C-----FROM PATH TO DIAMOND OUTBOUND
        VEHV%FBS = -4
      ELSE
C-----FROM PATH TO OUTBOUND/INTERNAL
        VEHV%FBS = 4
      END IF
      CALL LPPUSH (VEHV%LNPA,ILP,IPE,.TRUE.)
      RETURN
      END                                                               IPR4
C
C
C
      SUBROUTINE IPR5 (VEHV,TIME)
C
C-----VEHICLE LOGGING OUT
C
C-----VEHV - POINTER TO VEHICLE STRUCTURE
C-----TIME - TIME INTO SIMULATION
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (VEH), TARGET  :: VEHV
      DOUBLE PRECISION   TIME
      VEHV%INUSE = -TIME * 1.0D2
      CALL LPCLER (VEHV%LNPA)
      VEHV%LNCHGC = .FALSE.
      VEHV%LNCHGD = 0.0D0
      VEHV%FBS    = 0
      RETURN
      END                                                               IPR5
C
C
C
      SUBROUTINE IPR6 (VEHV,IPE,ILP,LAT)
C
C-----VEHICLE IS CHANGING LANES
C
C-----VEHV - POINTER TO VEHICLE STRUCTURE
C-----LAT - LANE CHANGE LATERAL DISTANCE
C-----ILP - LANE OR PATH VEHICLE IS CHANGING TO
C-----IPE - DISTANCE DOWN LANE OR PATH
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (VEH), TARGET  :: VEHV
      DOUBLE PRECISION      IPE    ,LAT
      INTEGER                   ILP
      INTEGER I,IGETLP
      IF (.NOT. VEHV%LNCHG)                      THEN
C----- THIS IS FIRST DT OF LANE CHANGE
        VEHV%LNCHG = .TRUE.
        CALL CHNGLN(VEHV%LNPA,ILP,IPE)
        DO I = 1, VEHV%UNITS
          IF (VEHV%LNCHGC)                       THEN
C-----LANE CHANGE CLEANUP STILL IN PROGRESS
            VEHV%UNIT(I)%LNCHGD = VEHV%UNIT(I)%LNCHGD + LAT
          ELSE
            VEHV%UNIT(I)%LNCHGD = LAT
          END IF
        END DO
        VEHV%LNCHGD = LAT
        VEHV%LNCHGC = .FALSE.
      END IF
      IF (LAT .NE. 0.0D0)                        THEN
C-----LANE CHANGE IN PROGRESS
        IF (VEHV%LNCHGD .NE. LAT)                THEN
          DO I = VEHV%UNITS, 2, -1
            VEHV%UNIT(I)%LNCHGD = VEHV%UNIT(I-1)%LNCHGD
          END DO
          VEHV%UNIT(1)%LNCHGD = VEHV%LNCHGD
          VEHV%LNCHGD = LAT
        END IF
      ELSE
C-----LANE CHANGE COMPLETED
        VEHV%LNCHG = .FALSE.
        IF (.NOT. VEHV%LNCHGC)                   THEN
C-----ONLY PROCESS THE FIRST LAT = 0
          VEHV%LNCHGC = .TRUE.
          ILP=IABS(IGETLP(VEHV%LNPA,0,0))
        END IF
      END IF
      RETURN
      END                                                               IPR6
C
C
C
      SUBROUTINE IPR6C (VEHV)
C
C-----CLEANING UP AFTER LANE CHANGE COMPLETED.
C
C-----VEHV - POINTER TO VEHICLE STRUCTURE
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (VEH), TARGET  :: VEHV
      INTEGER I,UNITS
      IF (VEHV%LNCHGC)                           THEN
        UNITS = VEHV%UNITS
        IF (VEHV%UNIT(UNITS)%LNCHGD .NE. 0.0D0)  THEN
          DO I = UNITS, 2, -1
            VEHV%UNIT(I)%LNCHGD = VEHV%UNIT(I-1)%LNCHGD
          END DO
          VEHV%UNIT(1)%LNCHGD = VEHV%LNCHGD
          VEHV%LNCHGD = 0.0D0
        END IF
        IF (VEHV%UNIT(UNITS)%LNCHGD .EQ. 0.0D0)  THEN
          VEHV%LNCHGC = .FALSE.
        END IF
      END IF
      RETURN
      END                                                               IPR6C
C
C
C
      SUBROUTINE INITAP (I,IA,IX,IY)
C
C-----INITIALIZE APPROACH DATA
C
C-----I - APPROACH NUMBER
C-----A - APPROACH AZIMUTH (DEGREES)
C-----IX - APPROACH BASE X
C-----IY - APPROACH BASE Y
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INTEGER I
      INTEGER    IA,IX,IY
      DOUBLE PRECISION X,AZTORA
C-----CONVERT APPROACH AZIMUTH TO RADIANS
      X=IA
      AZIA(I)=AZTORA(X)
C-----X, Y COORDINATES AT START OF APPROACH
      BASEAX(I)=IX
      BASEAY(I)=IY
      RETURN
      END                                                               INITAP
C
C
C
      SUBROUTINE INITLN (LN,IAPR,IAPRP,LGEOM1,LGEOM4,LWID,OUTBND,DIAMON)
C
C-----INITIALIZE LANE DATA FOR VEHICLE ARTICULATION
C
C-----LN - LANE NUMBER
C-----IAPR - APPROACH NUMBER
C-----IAPRP - PREVIOUS APPROACH NUMBER
C-----LGEOM1 - LANE END (DISTANCE DOWN APPROACH)
C-----LGEOM4 - LANE START (DISTANCE DOWN APPROACH)
C-----LWID - LANE WIDTH
C-----OUTBND -  IS THIS AN OUTBOUND LANE
C-----DIAMON - IS THIS A DIAMOND INTERSECTION
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INTEGER LN,IAPR,IAPRP,LGEOM1,LGEOM4,LWID
      LOGICAL                                  OUTBND,DIAMON
      INTEGER IRD2ZN
      DOUBLE PRECISION AT1,AT2,X,X1
      SAVE    X
      DATA X / 0.0 /
      IF(IAPRP.NE.IAPR)X=0.0
      IAPRP=IAPR
      X=X+LWID
      X1=X-LWID*0.5
      AT1=AZIA(IAPRP)
      XLNANG(LN)=AT1
      ANGNOL(LN)=IRD2ZN(AT1,VTANG,NOANGS)
      AT2=AT1-PID2
      XT1=COS(AT1)
      XT2=SIN(AT1)
C-----LANE X, Y STARTING COORDINATE
C-----FROM START OF LANE, NOT FROM POINT SPECIFIED BY LGEOM1
      BASELX(LN)=BASEAX(IAPR) + X1*COS(AT2)
      BASELY(LN)=BASEAY(IAPR) + X1*SIN(AT2)
      ENDLND(LN)=LGEOM4
C-----LANE X,Y END COORDINATES
C-----FROM POINT SPECIFIED BY LGEOM4
      ENDLNX(LN)=BASELX(LN)+LGEOM4*XT1
      ENDLNY(LN)=BASELY(LN)+LGEOM4*XT2
      IF (OUTBND .AND. (LGEOM1 .NE. 0))          THEN
C-----ADJUST FOR STOP LINE OFFSET ON OUTBOUND LANE
        ISLOFF(LN) = LGEOM1
        BASELX(LN) = BASELX(LN)+LGEOM1*XT1
        BASELY(LN) = BASELY(LN)+LGEOM1*XT2
      ELSE
        ISLOFF(LN) = 0
      END IF
C-----COMPENSATE FOR POSSIBLE GAP FROM END OF PATH
      BASELX(LN) = BASELX(LN)-LPGAP*XT1
      BASELY(LN) = BASELY(LN)-LPGAP*XT2
      ENDLNX(LN) = ENDLNX(LN)+LPGAP*XT1
      ENDLNY(LN) = ENDLNY(LN)+LPGAP*XT2
      IF (.NOT. OUTBND )                         THEN
C-----EXTEND PATH FOR POSSIBLE LEFT TURN PULLOUT
C-----OR OVERRUNNING OF STOPLINE
        ENDLNX(LN) = ENDLNX(LN)+2.0D1*XT1
        ENDLNY(LN) = ENDLNY(LN)+2.0D1*XT2
      END IF
      RETURN
      END                                                               INITLN
C
C
C
      SUBROUTINE INITPA (NPATHS)
C
C-----INITIALIZE PATH DATA
C
C-----NPATHS - NUMBER OF PATHS
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INTEGER NPATHS
C-----INITIALIZE SOME OTHER STUFF
      INTEGER I,IRD2ZN,IT1
      NOANGS = 360
      VTANG=DEG2RD
      DO 800 I=1,NPATHS
C-----FIRST STRAIGHT PART OF PATH
      IF(PTH1X1(I).EQ.0.0)                       THEN
        PTH1L(I)=0.0
        IT1=PATHFR(I)
        ANGPTH1(I)=XLNANG(IT1)
        ANGNOP1(I)=ANGNOL(IT1)
      ELSE
        CALL LINEP(PTH1X1(I),PTH1Y1(I),PTH1X2(I),PTH1Y2(I),PTH1L(I),XT1)
        PTH1A(I) = XT1
        ANGNOP1(I)=IRD2ZN(XT1,VTANG,NOANGS)
        ANGPTH1(I)=XT1
      END IF
C-----FIRST ARC OF PATH
C     ANGPTH2 REFERENCE ADDED TO STOP COMPILER WARNINGS
      ANGPTH2(I)=0.0
      IF(PTH2A2(I).EQ.0.0)                       THEN
        PTH2L (I)=0.0
        GAP12(I) = .FALSE.
      ELSE
        CALL ARCP(PTH2R(I),PTH2XC(I),PTH2YC(I),PTH2A1(I),PTH2A2(I),
     *            PTH2X1(I),PTH2Y1(I),PTH2X2(I),PTH2Y2(I),PTH2L(I),
     *            PTH2DR(I))
C-----LOOK FOR DISCONTINUTIES BETWEEN PATH SEGMENTS
        GAP12(I) = (DABS(PTH1X2(I) - PTH2X1(I)) .GT. NRZERO) .OR.
     1             (DABS(PTH1Y2(I) - PTH2Y1(I)) .GT. NRZERO)
      END IF
C-----SECOND ARC OF PATH
      ANGPTH3(I)=0.0
      IF(PTH3A2(I).EQ.0.0)                       THEN
        PTH3L(I)=0.0
        GAP23(I) = .FALSE.
      ELSE
        CALL ARCP(PTH3R(I),PTH3XC(I),PTH3YC(I),PTH3A1(I),PTH3A2(I),
     *            PTH3X1(I),PTH3Y1(I),PTH3X2(I),PTH3Y2(I),PTH3L(I),
     *            PTH3DR(I))
C-----LOOK FOR DISCONTINUTIES BETWEEN PATH SEGMENTS
        GAP23(I) = (DABS(PTH2X2(I) - PTH3X1(I)) .GT. NRZERO) .OR.
     1             (DABS(PTH2Y2(I) - PTH3Y1(I)) .GT. NRZERO)
      END IF
      PTH3SD(I)=PTH1L(I)+PTH2L(I)
C-----SECOND STRAIGHT PART OF PATH
      IF(PTH4X1(I).EQ.0.0)                       THEN
        PTH4L(I)=0.0
        GAP34(I) = .FALSE.
      ELSE
        CALL LINEP(PTH4X1(I),PTH4Y1(I),PTH4X2(I),PTH4Y2(I),PTH4L(I),XT1)
        PTH4A(I) = XT1
        ANGNOP4(I)=IRD2ZN(XT1,VTANG,NOANGS)
        ANGPTH4(I)=XT1
C-----LOOK FOR DISCONTINUTIES BETWEEN PATH SEGMENTS
        IF(PTH3A2(I).EQ.0.0)                     THEN
          GAP23(I) = (DABS(PTH2X2(I) - PTH4X1(I)) .GT. NRZERO) .OR.
     1               (DABS(PTH2Y2(I) - PTH4Y1(I)) .GT. NRZERO)
          GAP34(I) = .FALSE.
        ELSE
          GAP34(I) = (DABS(PTH3X2(I) - PTH4X1(I)) .GT. NRZERO) .OR.
     1               (DABS(PTH3Y2(I) - PTH4Y1(I)) .GT. NRZERO)
        END IF
      END IF
      PTH4SD(I)=PTH3SD(I)+PTH3L(I)
      PATHL(I) = PTH4SD(I) + PTH4L(I)
  800 CONTINUE
      RETURN
      END                                                               INITPA
C
C
C
      SUBROUTINE ONLNPA (VEHV,VEHC,ILP,IPE)
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (VEH), TARGET  :: VEHV
      TYPE (VEHCL), TARGET ::VEHC
      DOUBLE PRECISION                 IPE
      INTEGER                      ILP
      DOUBLE PRECISION DISTF,DISTR,DX,DY,IPESL,T,X,Y
      INTEGER I,IGOTO,ILPINF,ILPR,IRD2ZN,IUNIT,IVEHCL,XYS
      TYPE (VEHUNI),POINTER :: VU
      TYPE (CLSUNI),POINTER :: CU
      ILPINF = 1
      IVEHCL = VEHV%CLASS
      VEHV%LENLP = IPE
      IF(VEHV%FBS .EQ. 2)                        THEN
C-----FRONT ON INBOUND LANE
        CALL ONLNXY (IPE,ILP,VEHV%LNCHGD,X,Y)
        IGOTO = 1
        GO TO 500
      END IF
      IF(VEHV%FBS .EQ. -3)                       THEN
C-----FRONT ON PATH, AFTER INTERNALS
        CALL ONPAXY (IPE,ILP,X,Y,XYS)
        IGOTO = 2
        GO TO 500
      END  IF
      IF(VEHV%FBS .EQ. 3)                        THEN
C-----FRONT ON FIRST OR ONLY PATH
        CALL ONPAXY (IPE,ILP,X,Y,XYS)
        IGOTO = 3
        GO TO 500
      END  IF
      IF(VEHV%FBS.EQ. -4)                        THEN
C-----FRONT ON DIAMOND OUTBOUND LANE
        IPESL = IPE - ISLOFF(ILP)
        CALL ONLNXY (IPESL,ILP,VEHV%LNCHGD,X,Y)
        VEHV%LENLP = IPESL
        IGOTO = 4
        GO TO 500
      END  IF
      IF(VEHV%FBS .EQ. 4)                        THEN
C-----FRONT ON OUTBOUND LANE OR DIAMOND INTERNAL LANE
        IPESL = IPE - ISLOFF(ILP)
        CALL ONLNXY (IPESL,ILP,VEHV%LNCHGD,X,Y)
        VEHV%LENLP = IPESL
        IGOTO = 5
        GO TO 500
      END  IF
  500 CONTINUE
      VEHV%FBX = X
      VEHV%FBY = Y
C-----END OF CODE TO FIND POSITION OF VEHICLE FRONT BUMPER
      ILPR = ILP
C-----FIND POSITION OF FRONT POINT ON FRONT UNIT
      VU => VEHV%UNIT(1)
      CU => VEHC%UNIT(1)
      DISTF = CU%FPD
      CALL NONAME(IGOTO,X,Y,DISTF,VEHV%LNCHGD,
     *            VEHV,ILP,ILPINF,VU%FPS,VU%FPX,VU%FPY,
     *            .FALSE.)
      DO IUNIT = 1,VEHC%UNITS
C-----FIND POSITION OF REAR POINT OF EACH UNIT
        DISTR = CU%RPD - CU%FPD
        CALL NONAME(IGOTO,VU%FPX,VU%FPY,DISTR,VEHV%UNIT(IUNIT)%LNCHGD,
     *              VEHV,ILP,ILPINF,VU%RPS,VU%RPX,VU%RPY,
     *              IUNIT .EQ. VEHC%UNITS)
C-----CALCULATE ON-LANE OR ON-PATH X & Y COORDINATES OF UNIT CENTER
        X = 0.5*(VU%FPX+VU%RPX)
        Y = 0.5*(VU%FPY+VU%RPY)
        IF (IUNIT .EQ. 1)                        THEN
          DX = VEHV%FBX - VU%RPX
          DY = VEHV%FBY - VU%RPY
        ELSE
          DX = VU%FPX-VU%RPX
          DY = VU%FPY-VU%RPY
        END IF
        IF (DABS(DX) .LE. NRZERO)                THEN
C-----INFINITE SLOPE
          IF (DY .GT. 0.0D0)                     THEN
C-----GOING NORTH
            XT2 = PID2
          ELSE
C-----GOING SOUTH
            XT2 = PIT1P5
          END IF
        ELSE
          XT2 = ATAN2 (DY,DX)
        END IF
C-----ADJUST CENTER FOR FRONT AND REAR OVERHANG
        T = 0.5D0*(CU%FPD - (CU%LEN - CU%RPD))
        X = X + (T * COS(XT2))
        Y = Y + (T * SIN(XT2))
        VU%ANG = XT2
        VU%ANGNO = IRD2ZN(XT2,VTANG,NOANGS)
        IF (VU%ANGNO .EQ. 360)VU%ANGNO = 0
        VU%X = X
        VU%Y = Y
        IF (CU%HPD .GT. 0)                       THEN
C-----COORDINATES OF HITCH POINT
          T = CU%HPD - (CU%LEN / 2.0D0)
        ELSE
C-----LAST UNIT
C-----COORDINATES OF REAR BUMPER IN HITCH POINT
          T = (CU%LEN / 2.0D0)
        END IF
        X = X - (T * COS(XT2))
        Y = Y - (T * SIN(XT2))
C-----X, Y AND RPS NEEDED AS FRONT POINT DATA FOR NEXT CYCLE OF DO LOOP
        I = VU%RPS
        VU%HPX = X
        VU%HPY = Y
        IF (IUNIT .EQ. VEHC%UNITS)               THEN
          VEHV%LENLP = VEHV%LENLP + CU%LEN - CU%RPD
        ELSE
          VU => VEHV%UNIT(IUNIT+1)
          CU => VEHC%UNIT(IUNIT+1)
C-----USING HITCH POINT FOR FRONT POINT OF FOLLOWING UNIT
          VU%FPX = X
          VU%FPY = Y
          VU%FPS = I
        END IF
      END DO
      RETURN
      END                                                               ONLNPA
C
C
C
      SUBROUTINE ONLNXY (DIST,ILP,LNCHGD,XI,YI)
C
C-----FIND COORDINATES OF FRONT BUMPER ON LANE
C
C-----DIST - DISTANCE DOWN LANE
C-----ILP- PATH NUMBER
C-----LNCHGD - LANE CHANGE LATERAL DISTANCE
C-----XI, YI - RETURNED COORDINATES
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      DOUBLE PRECISION   DIST    ,LNCHGD
      INTEGER                 ILP
      DOUBLE PRECISION                   XI,YI
      DOUBLE PRECISION ANG
      XI = BASELX(ILP)
      YI = BASELY(ILP)
      ANG = XLNANG(ILP)
      XI = XI + DIST * DCOS(ANG)
      YI = YI + DIST * DSIN(ANG)
      IF (LNCHGD .NE. 0.0D0)                     THEN
        ANG = ANG - PID2
        XI = XI + LNCHGD * DCOS(ANG)
        YI = YI + LNCHGD * DSIN(ANG)
      END IF
      RETURN
      END                                                               ONLNXY
C
C
C
      SUBROUTINE ONPAXY (DIST,ILP,X,Y,XYS)
C
C-----FIND COORDINATES OF FRONT BUMPER ON PATH
C
C-----DIST - DISTANCE DOWN PATH
C-----ILP- PATH NUMBER
C-----X,Y - RETURNED COORDINATES
C-----XYS - STATUS (WHICH PATH SEGMENT) OF THE POINT
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      DOUBLE PRECISION   DIST
      INTEGER                 ILP    ,XYS
      DOUBLE PRECISION X,Y
      DOUBLE PRECISION ANG,DDIST
  901 FORMAT("NO LINES OR ARCS ON PATH ",I3," - ONPAXY")
      LOGICAL ONPALI
      DDIST = DIST
      XYS = 0
      IF (PTH1L(ILP) .GT. NRZERO)                THEN
        IF (ONPALI (DDIST,PTH1X1(ILP),PTH1Y1(ILP),PTH1L(ILP),
     1              PTH1A(ILP),X,Y))             THEN
          XYS = 1
          RETURN
        END IF
        DDIST = DDIST - PTH1L(ILP)
      END IF
      IF (PTH2L(ILP) .GT. NRZERO)                THEN
        IF (DDIST .LE. PTH2L(ILP))               THEN
          CALL ONPAAR (DDIST,PTH2XC(ILP),PTH2YC(ILP),PTH2R(ILP),
     1                 PTH2A1(ILP),PTH2A2(ILP),X,Y)
          XYS = 2
          RETURN
        END IF
        DDIST = DDIST - PTH2L(ILP)
      END IF
      IF (PTH3L(ILP) .GT. NRZERO)                THEN
        IF (DDIST .LE. PTH3L(ILP))               THEN
          CALL ONPAAR (DDIST,PTH3XC(ILP),PTH3YC(ILP),PTH3R(ILP),
     1                 PTH3A1(ILP),PTH3A2(ILP),X,Y)
          XYS = 3
          RETURN
        END IF
        DDIST = DDIST - PTH3L(ILP)
      END IF
      IF (PTH4L(ILP) .GT. NRZERO)                THEN
        IF (ONPALI (DDIST,PTH4X1(ILP),PTH4Y1(ILP),PTH4L(ILP),
     1              PTH4A(ILP),X,Y))             THEN
          XYS = 4
          RETURN
        END IF
        DDIST = DDIST - PTH4L(ILP)
      END IF
C-----PAST END OF LAST SEGMENT, BUT STILL LOGGED IN TO PATH
C-----EXTRAPOLATE POSITION ON A TANGENT TO END OF PATH
      XYS = 5
      IF (PTH4L(ILP) .GT. NRZERO)                THEN
        ANG = PTH4A(ILP)
        X = PTH4X2(ILP)
        Y = PTH4Y2(ILP)
      ELSE IF (PTH3L(ILP) .GT. NRZERO)           THEN
        ANG = PTH3A1(ILP) - PTH3A2(ILP)
        X = PTH3X2(ILP)
        Y = PTH3Y2(ILP)
      ELSE IF (PTH2L(ILP) .GT. NRZERO)           THEN
        ANG = PTH2A1(ILP) - PTH2A2(ILP)
        X = PTH2X2(ILP)
        Y = PTH2Y2(ILP)
      ELSE IF (PTH1L(ILP) .GT. NRZERO)           THEN
        ANG = PTH1A(ILP)
        X = PTH1X2(ILP)
        Y = PTH1Y2(ILP)
      ELSE
        CALL  PRTNER ( 99,"ERROR - NO LINES OR ARCS ON PATH - ONPAXY" )
        STOP
      END IF
      X = X + DDIST * DCOS(ANG)
      Y = Y + DDIST * DSIN(ANG)
      RETURN
      END                                                               ONPAXY
C
C
C
      FUNCTION ONPALI (DIST,PTHX,PTHY,PTHL,ANG,X,Y)
C
C-----IS POINT ON THIS (LINE) PATH SEGMENT
C-----IF SO,FIND COORDINATES OF POINT A GIVEN DISTANCE ALONG A LINE
C
C-----DIST - DISTANCE DOWN LINE
C-----PTHX,PTHY - COORDINATES OF START OF LINE
C-----PATHL - LENGTH OF LINE
C-----X,Y - RETURNED COORDINATES OF POINT ON LINE
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      DOUBLE PRECISION DIST,PTHX,PTHY,PTHL,ANG,X,Y
      LOGICAL ONPALI
      ONPALI = (DIST .LE. PTHL)
      IF (ONPALI)                                THEN
        X = PTHX + DIST * DCOS(ANG)
        Y = PTHY + DIST * DSIN(ANG)
      END IF
      RETURN
      END                                                               ONPALI
C
C
C
      SUBROUTINE ONPAAR (DIST,PTHXC,PTHYC,PTHR,PTHA1,PTHA2,X,Y)
C
C-----FIND COORDINATES OF POINT A GIVEN DISTANCE ALONG AN ARC
C
C-----DIST - DISTANCE ALONG ARC
C-----PTHXC,PTHYC - COORDINATES OF CENTER OF ARC
C-----PTHR - RADIUS OF ARC
C-----PTHA1,PTHA2 - ARC START ANGLE AND SWEEP ANGLE
C-----X,Y - RETURNED COORDINATES OF POINT ON ARC
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      DOUBLE PRECISION DIST,PTHXC,PTHYC,PTHR,PTHA1,PTHA2,X,Y
      DOUBLE PRECISION ANG
      IF (PTHA2 .GT. 0.0D0)                      THEN
        ANG = PTHA1 + (DIST / PTHR)
      ELSE
        ANG = PTHA1 - (DIST / PTHR )
      END IF
      IF (ANG .GT. PIT2)                         THEN
        ANG = ANG - PIT2
      ELSE IF (ANG .LT. -PIT2)                   THEN
        ANG = ANG + PIT2
      END IF
      X = PTHXC + PTHR * DCOS(ANG)
      Y = PTHYC + PTHR * DSIN(ANG)
      RETURN
      END                                                               ONPAAR
C
C
C
      SUBROUTINE NONAME(IGOTO,X,Y,R,LNCHGD,VEHV,ILP,ILPINC,CST,XI,YI,
     *                  LENLP)
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      INTEGER           IGOTO
      DOUBLE PRECISION        X,Y,R,LNCHGD
      INTEGER                                   ILP,ILPINC
      TYPE (VEH), TARGET  ::               VEHV
      INTEGER                                              CST
      DOUBLE PRECISION                                         XI,YI
      LOGICAL           LENLP
      CHARACTER*132 ERRMSG
      DOUBLE PRECISION ANG,DIONLN,DIONPA
      LOGICAL ONLANE,ONPATH
      INTEGER IGETLP, ILPT
  901 FORMAT('IGOTO = ',I3,' IS < 1 OR > 5',
     *        ' POINT NOT ON ANY LANE OR PATH - NONAME')
      GOTO (1580,1520,1560,1500,1540),IGOTO
      WRITE(ERRMSG,901)IGOTO
      CALL  PRTNER ( 99,ERRMSG )
      STOP
 1500 CONTINUE
C-----POINT ON DIAMOND OUTBOUND LANE ?
      IF(ONLANE(X,Y,R,LNCHGD,VEHV,ILP,ILPINC,
     1          CST,XI,YI))                      THEN
        IF (LENLP)                               THEN
          VEHV%LENLP = VEHV%LENLP - DIONLN(ILP,XI,YI)
        END IF
        GO TO 1600
      END IF
      IF(IGOTO .EQ. 4) IGOTO = 2
      ILPT = IABS(IGETLP(VEHV%LNPA,ILPINC,0))
      VEHV%LENLP = VEHV%LENLP + PATHL(ILPT)
      ILPINC = ILPINC + 1
 1520 CONTINUE
C-----POINT ON SECOND PATH ?
      IF(ONPATH(X,Y,R,VEHV,ILP,ILPINC,CST,XI,YI))THEN
        IF (LENLP)                               THEN
          VEHV%LENLP = VEHV%LENLP - DIONPA(ILP,CST,XI,YI)
        END IF
        GO TO 1600
      END IF
      IF(IGOTO .EQ. 2) IGOTO = 5
      ILPT = IABS(IGETLP(VEHV%LNPA,ILPINC,0))
      VEHV%LENLP = VEHV%LENLP + ENDLND(ILPT)
      ILPINC = ILPINC + 1
 1540 CONTINUE
C-----POINT ON OUTBOUND LANE OR DIAMOND INTERNAL LANE ?
      IF(ONLANE(X,Y,R,LNCHGD,VEHV,ILP,ILPINC,
     1          CST,XI,YI))                      THEN
        IF (LENLP)                               THEN
          VEHV%LENLP = VEHV%LENLP - DIONLN(ILP,XI,YI)
        END IF
        GO TO 1600
      END IF
      IF(IGOTO .EQ. 5) IGOTO = 3
      ILPT = IABS(IGETLP(VEHV%LNPA,ILPINC,0))
      VEHV%LENLP = VEHV%LENLP + PATHL(ILPT)
C-----LANE ENDS EXTENDED FOR POSSIBLE LANE PATH GAP
C-----ALLOW FOR THIS AT END OF LANE
      VEHV%LENLP = VEHV%LENLP - LPGAP
      ILPINC = ILPINC + 1
 1560 CONTINUE
C-----POINT ON FIRST PATH ?
      IF(ONPATH(X,Y,R,VEHV,ILP,ILPINC,CST,XI,YI))THEN
        IF (LENLP)                               THEN
          VEHV%LENLP = VEHV%LENLP - DIONPA(ILP,CST,XI,YI)
        END IF
        GO TO 1600
      END IF
      IF(IGOTO .EQ. 3) IGOTO = 1
      ILPT = IABS(IGETLP(VEHV%LNPA,ILPINC,0))
      VEHV%LENLP = VEHV%LENLP + ENDLND(ILPT)
      VEHV%LENLP = VEHV%LENLP + LPGAP
      ILPINC = ILPINC + 1
 1580 CONTINUE
C-----POINT ON INBOUND LANE ?
      IF(ONLANE(X,Y,R,LNCHGD,VEHV,ILP,ILPINC,
     1          CST,XI,YI))                      THEN
        IF (LENLP)                               THEN
          VEHV%LENLP = VEHV%LENLP - DIONLN(ILP,XI,YI)
        END IF
        GO TO 1600
      ELSE
C-----POINT IS BEFORE INBOUND LANE
        ANG = XLNANG(ILP)
        XI = X - R * DCOS(ANG)
        YI = Y - R * DSIN(ANG)
        IF (LENLP)                               THEN
          VEHV%LENLP = VEHV%LENLP + DIONLN(ILP,XI,YI)
        END IF
      END IF
 1600 CONTINUE
      RETURN
      END                                                               NONAME
C
C
C
      FUNCTION ONLANE (X,Y,R,LNCHGD,VEHV,ILP,ILPINC,CST,XI,YI)
C
C-----PROCESS VEHICLE UNIT WITH REFERENCE POINT ON LANE
C-----IS POINT ALSO ON THE LANE ?
C
C-----X, Y - COORDINATES OF REFERENCE POINT ON A LANE
C-----R - DISTANCE FROM REFERENCE POINT TO POINT
C-----LNCHGD - LANE CHANGE DISTANCE
C-----VEHCLE - THE VEHICLE STRUCTURE
C-----ILP  - LANE NUMBER
C-----ILPINC - NUMBER OF STEPS TO LOOK BACK FOR LANE/PATH NUMBER
C-----         FROM FRONT L/P TO CENTER L/P
C-----CST - CURRENT STATUS OF THE POINT
C-----XI,YI - IF RETURNING TRUE, RETURNED COORDINATES OF POINT
C
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (VEH), TARGET  ::        VEHV
      DOUBLE PRECISION X,Y,R,LNCHGD,                      XI,YI
      INTEGER                              ILP,ILPINC,CST
      DOUBLE PRECISION ANG,DIST,DX,DY

      INTEGER ILPP,IGETLP
      LOGICAL ARCLIN,ONLANE
      DOUBLE PRECISION X1,X2,Y1,Y2
      IF (R .LE. NRZERO)                         THEN
        XI = X
        YI = Y
        ONLANE = .TRUE.
        RETURN
      END IF
C-----DISTANCE FROM POINT TO START OF LANE
      DIST = DSQRT((X-BASELX(ILP))**2 + (Y-BASELY(ILP))**2)
      IF ((R-DIST) .GT. NRZERO)                   THEN
C-----POINT HAS NOT YET REACHED THIS LANE
                                                 GO TO 1000
      END IF
      X1 = BASELX(ILP)
      Y1 = BASELY(ILP)
      X2 = ENDLNX(ILP)
      Y2 = ENDLNY(ILP)
      IF (LNCHGD .NE. 0.0D0)                     THEN
        ANG = XLNANG(ILP) - PID2
        DX = LNCHGD * COS(ANG)
        DY = LNCHGD * SIN(ANG)
        X1 = X1 + DX
        Y1 = Y1 + DY
        X2 = X2 + DX
        Y2 = Y2 + DY
      END IF
      IF(ARCLIN (X,Y,R,X1,Y1,X2,Y2,.FALSE.,
     1           0.0D0,0.0D0,XI,YI))             THEN
C-----POINT IS ON THIS LANE
        IF((CST .GE. 1) .AND.
     1     (CST .LE. 4))                         THEN
C-----FOR FIRST TIME, COMING FROM PATH
          CST=5
        END IF
        ONLANE = .TRUE.
        RETURN
      END IF
 1000 CONTINUE
C-----POINT HAS NOT YET REACHED THIS LANE
      ILPP = IABS(IGETLP(VEHV%LNPA,ILPINC,0))
      IF (ILPP .EQ. 0)                           THEN
C-----THERE IS NO LANE/PATH BEFORE THIS LANE
        ONLANE = .FALSE.
        RETURN
      END IF
C-----IS STILL ON A PREVIOUS LANE/PATH
      ILP = ILPP
      ONLANE = .FALSE.
      RETURN
      END                                                               ONLANE
C
C
C
      FUNCTION ONPATH (X,Y,R,VEHV,ILP,ILPINC,CST,XI,YI)
C
C-----PROCESS VEHICLE UNIT WITH FRONT ON PATH
C-----IS POINT ON THIS PATH
C
C-----X, Y    - COORDINATES OF REFERENCE POINT ON A PATH
C-----R       - DISTANCE FROM REFERENCE POINT TO POINT
C-----VEHV    - THE VEHICLE STRUCTURE
C-----ILP     - LANE NUMBER
C-----ILPINC  - NUMBER OF STEPS TO LOOK BACK FOR LANE/PATH NUMBER
C-----          FROM FRONT L/P TO CENTER L/P
C-----CST     - CURRENT STATUS OF THE POINT
C-----XI,YI   - IF RETURNING TRUE, RETURNED COORDINATES OF POINT
C
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      DOUBLE PRECISION X,Y,R,                          XI,YI
      TYPE (VEH), TARGET  :: VEHV
      INTEGER                       ILP,ILPINC,CST
      CHARACTER*132 ERRMSG
      INTEGER IGETLP,DCST
      DOUBLE PRECISION DIST,XNEXT1,YNEXT1,XP,YP
      LOGICAL ARCARC,ARCLIN,ONPATH
  901 FORMAT('NO FIRST LINE OR ARC ON PATH',I3,' - ONPATH')
  902 FORMAT('CST = ',I3,' IS < 1 OR > 5',
     *       ' ON-PATH STATUS OF POINT IS INVALID - ONPATH')
      IF (R .LE. NRZERO)                         THEN
        XI = X
        YI = Y
        ONPATH = .TRUE.
        RETURN
      END IF
C-----DISTANCE FROM POINT TO START OF PATH
      IF (PTH1L(ILP) .GT. NRZERO)                THEN
C-----FIRST STRAIGHT SEGMENT
        XP = PTH1X1(ILP)
        YP = PTH1Y1(ILP)
        DCST = 0
      ELSE IF (PTH2R(ILP) .GT. NRZERO)           THEN
C-----FIRST ARC SEGMENT
        XP = PTH2X1(ILP)
        YP = PTH2Y1(ILP)
        DCST = 2
      ELSE
        WRITE(ERRMSG,901)ILP
        CALL  PRTNER ( 99,ERRMSG )
        STOP
      END IF
      DIST = DSQRT((X-XP)**2 + (Y-YP)**2)
      IF ((R - DIST) .GT. -NRZERO)               THEN
C-----POINT HAS NOT YET REACHED THIS PATH
C-----IS STILL ON A PREVIOUS LANE/PATH
        ILP = IABS(IGETLP(VEHV%LNPA,ILPINC,0))
        ONPATH = .FALSE.
        RETURN
      END IF
 1310 CONTINUE
      IF ((VEHV%FBS .LT. 0) .AND. (CST .EQ. 5))  THEN
C-----SECOND PATH, AFTER INTERNALS
        CST = 0
      END IF
      IF (CST .EQ. 0)                            THEN
        CST = CST + DCST
      ELSE IF (CST .EQ. 5)                       THEN
        CST = 2
      END IF
      GO TO (1320,1340,1360,1380,1400),CST
      IF (CST .NE. 0)                            THEN
        WRITE(ERRMSG,902) CST
        CALL  PRTNER ( 99,ERRMSG )
        STOP
      END IF
 1320 CONTINUE
      IF(ARCLIN (X,Y,R,PTH1X1(ILP),PTH1Y1(ILP),PTH1X2(ILP),
     1                PTH1Y2(ILP),GAP12(ILP),PTH2X1(ILP),
     2                PTH2Y1(ILP),XI,YI))        THEN
C-----POINT IS ON FIRST STRAIGHT PATH SEGMENT
        IF(CST.LT.1)                             THEN
C-----FOR FIRST TIME
          CST = 1
        END IF
        ONPATH = .TRUE.
        RETURN
      END IF
 1340 CONTINUE
      IF (PTH3R(ILP) .LE. NRZERO)                THEN
C-----NO SECOND ARC, CHECK FOR GAP TO SECOND LINE
        XNEXT1 = PTH4X1(ILP)
        YNEXT1 = PTH4Y1(ILP)
      ELSE
        XNEXT1 = PTH3X1(ILP)
        YNEXT1 = PTH3Y1(ILP)
      END IF
      IF(ARCARC (X,Y,R,PTH2XC(ILP),PTH2YC(ILP),PTH2R(ILP),PTH2A1(ILP),
     1           PTH2A2(ILP),PTH2X1(ILP),PTH2Y1(ILP),PTH2X2(ILP),
     2           PTH2Y2(ILP),GAP23(ILP),XNEXT1,YNEXT1,
     3           XI,YI))                         THEN
C-----POINT ON FIRST ARC
        IF(CST.LT.2)                             THEN
C-----FOR FIRST TIME
          CST=2
        END IF
        ONPATH = .TRUE.
        RETURN
      END IF
 1360 CONTINUE
      IF(ARCARC (X,Y,R,PTH3XC(ILP),PTH3YC(ILP),PTH3R(ILP),PTH3A1(ILP),
     1           PTH3A2(ILP),PTH3X1(ILP),PTH3Y1(ILP),PTH3X2(ILP),
     2           PTH3Y2(ILP),GAP34(ILP),PTH4X1(ILP),PTH4Y1(ILP),
     3           XI,YI))                         THEN
C-----POINT ON SECOND ARC
        IF(CST.LT.3)                             THEN
C-----FOR FIRST TIME
          CST=3
        END IF
        ONPATH = .TRUE.
        RETURN
      END IF
 1380 CONTINUE
      IF(ARCLIN (X,Y,R,PTH4X1(ILP),PTH4Y1(ILP),PTH4X2(ILP),
     1           PTH4Y2(ILP),.FALSE.,0.0D0,0.0D0,XI,YI))THEN
C-----POINT ON SECOND STRAIGHT PART OF PATH
        IF(CST.LT.4)                             THEN
C-----FOR FIRST TIME
          CST=4
        END IF
        ONPATH = .TRUE.
      ELSE
C-----POINT HAS NOT YET REACHED THIS PATH
C-----POINT IS ON PREVIOUS PATH/LANE
        ILP = IABS(IGETLP(VEHV%LNPA,ILPINC,0))
        ONPATH = .FALSE.
      END IF
      RETURN
 1400 CONTINUE
      ONPATH = .FALSE.
      RETURN
      END                                                               ONPATH
C
C
C
      SUBROUTINE LINEP(PTHX1,PTHY1,PTHX2,PTHY2,PTHL,ANGP)
C   PTHX1 - X COORDINATE OF START OF PATH.
C   PTHY1 - Y
C   PTHX2 - X               END
C   PTHY2 - Y
C   PTHL - CALCULATED LENGTH OF PATH.
C   ANGP - CALCULATED ANGLE OF THE PATH, RADIANS. SET TO -999 FOR
C          ZERO LENGTH PATH.
C
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      DOUBLE PRECISION PTHX1,PTHY1,PTHX2,PTHY2,PTHL,ANGP
      DOUBLE PRECISION      PTHLX,PTHLY
      PTHLX=PTHX2-PTHX1
      PTHLY=PTHY2-PTHY1
      PTHL=DSQRT(PTHLX*PTHLX+PTHLY*PTHLY)
      IF(PTHL.EQ.0.0D0)                          THEN
        ANGP=-9.99D2
        GO TO 9990
      END IF
      IF(PTHLX.EQ.0.0D0)                         THEN
        IF(PTHLY.GT.0.0D0)                       THEN
          ANGP=PID2
        ELSE
          ANGP=PIT1P5
          IF(ANGP.LT.0.0D0)ANGP=ANGP+PIT2
        END IF
      ELSE
        ANGP=ATAN2(PTHLY,PTHLX)
      END IF
      IF (DABS(ANGP) .LT. NRZERO)                THEN
        ANGP = 0.0D0
      END IF
 9990 CONTINUE
      RETURN
      END                                                               LINEP
C
C
C
      SUBROUTINE ARCP(PTHR,PTHXC,PTHYC,PTHA1,PTHA2,PTHX1,PTHY1,PTHX2,
     1                PTHY2,PTHLA,PTHDR)
C
C-----THIS SUBROUTINE FINDS THE BEGINNING AND ENDING COORDINATES, ARC
C-----LENGTH, CHORD LENGTH AND DIRECTION AT BEGINNING OF CIRCULAR ARC.
C
C-----PTHR - ARC RADIUS.
C-----PTHXC -  X COORDINATE OF ARC CENTER.
C-----PTHYC -  Y
C-----PTHA1 - ANGLE AT START OF ARC. DEGREES, POSITIVE CLOCKWISE FROM
C-----        12 O'CLOCK.
C-----        IS CONVERTED TO RADIANS,POSITIVE COUNTERCLOCKWISE FROM
C-----        3 O'CLOCK.
C-----PTHA2 - SWEEP ANGLE OF ARC, IN DEGREES. CONVERTED TO RADIANS
C-----PTHX1 - X COORDINATE OF ARC START POINT, CALCULATED.
C-----PTHY1 - Y
C-----PTHX2 - X COORDINATE OF ARC END POINT, CALCULATED.
C-----PTHY2 - Y
C-----PTHLA - LENGTH OF ARC, CALCULATED
C-----PTHLC - LENGTH OF CHORD, CALCULATED
C-----PTHDR - DIRECTION OF VEHICLE WHEN AT START OF PATH, RADIANS
C
      IMPLICIT  NONE                                                    CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      DOUBLE PRECISION PTHR,PTHXC,PTHYC,PTHA1,PTHA2,PTHX1,PTHY1,PTHX2,
     1                 PTHY2,PTHLA,PTHDR
      DOUBLE PRECISION AZTORA
C-----CHANGE TO RADIANS
      PTHA1=AZTORA(PTHA1)
      IF ((PTHA1 .LT.  NRZERO) .AND.
     *    (PTHA1 .GT. -NRZERO))                  THEN
        PTHA1 = 0.0D0
      END IF
C-----CHANGE TO RADIANS, POSITIVE COUNTERLOCKWISE
      PTHA2=PTHA2*DEG2RD
      PTHA2 = -PTHA2
      PTHX1=PTHXC+PTHR*DCOS(PTHA1)
      PTHY1=PTHYC+PTHR*DSIN(PTHA1)
      PTHX2=PTHXC+PTHR*DCOS(PTHA1+PTHA2)
      PTHY2=PTHYC+PTHR*DSIN(PTHA1+PTHA2)
C-----FIND DIRECTION OF VEHICLE WHEN ON START OF PATH
      IF (PTHA2.GT.NRZERO)                       THEN
        PTHDR=PTHA1-PID2
        IF (PTHDR .LT. -NRZERO)                  THEN
          PTHDR = PTHDR + PIT2
        END IF
      ELSE
        PTHDR=PTHDR+PID2
        IF (PTHDR .GT. PIT2)                     THEN
          PTHDR = PTHDR - PIT2
        END IF
      END IF
      PTHLA=PTHR*PTHA2
      PTHLA=ABS(PTHLA)
      RETURN
      END                                                               ARCP
C
C
C
      FUNCTION IRD2ZN(ANG,ANGZ,NZ)
C
C-----THIS FUNCTION FINDS THE ANGULAR ZONE IN WHICH A DIRECTED LINE
C-----IS LOCATED.  THE ZONES ARE NUMBERED COUNTERCLOCKWISE WITH
C-----ZONE 1 CENTERED AROUND A LINE AT <ANGZ> CCW FROM 3 O'CLOCK.
C
C-----ANG - ANGLE OF THE LINE, IN RADIANS, COUNTERCLOCKWISE FROM
C-----      3 O'CLOCK. MUST BE +/- 2 PI
C-----ANGZ - ARC OF THE ZONE, POSITIVE RADIANS.
C-----NZ - NUMBER OF ZONES, MUST BE COMPATIBLE WITH <ANGZ>.
C-----     IF NEGATIVE, RETURN THE ANGLE IN DEGREES
C
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      INTEGER          IRD2ZN
      DOUBLE PRECISION ANG,ANGZ
      INTEGER          NZ
      DOUBLE PRECISION A1
      A1=ANG
      IF(A1.LT.0.0)                              THEN
        A1=A1+PIT2
      ELSE
        IF(A1.GT.PIT2)A1=A1-PIT2
      END IF
      IF(NZ.LT.0)                                THEN
        IRD2ZN=IDNINT((A1*RAD2DG))
        GO TO 9900
      ELSE
        IRD2ZN=IDNINT(A1/ANGZ)
      END IF
      IF(IRD2ZN.EQ.0)IRD2ZN=NZ
 9900 CONTINUE
      RETURN
      END                                                               IRD2ZN
C
C
C
      FUNCTION AZTORA(AZIA)
C
C-----THIS FUNCTION CONVERTS AN AZIMUTH (DEGREES, CLOCKWISE FROM
C-----12 O'CLOCK) TO RADIANS (COUNTERCLOCKWISE FROM 3 O'CLOCK).
C
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'CONSTN'
      DOUBLE PRECISION AZTORA
      DOUBLE PRECISION AZIA
      AZTORA=90.0-AZIA
      IF(AZTORA.LT.0.)AZTORA=AZTORA+360.0
      AZTORA=AZTORA*DEG2RD
      RETURN
      END                                                               AZTORA
C
C
C
      SUBROUTINE LPPUSH (LNPA,ILP,DIST,LLP)
C
C-----THIS SUBROUTINE PUTS A LANE OR PATH NUMBER INTO AN ARRAY
C-----THE ARRAY WILL STORE UP TO 5 NUMBERS FOR EACH VEHICLE
C
C-----LNPA - LANE AND PATH DATA STRUCTURE FOR THE VEHICLE
C-----ILP - LANE/PATH NUMBER TO PUT IN NEXT AVAILABLE POSITION
C-----DIST - DISTANCE DOWN LANE OR PATH
C-----LLP - .TRUE. FOR LANE, .FALSE. FOR PATH
C

      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (LNPAVE)::LNPA
      DOUBLE PRECISION       DIST
      INTEGER            ILP
      LOGICAL                     LLP
      INTEGER I,J
      J = LNPA%ILPNOW + 1
      I = LNPA%ILNCHG(J) + 1
      LNPA%ILPNOW = J
      LNPA%ILP(I,J) = ILP
      LNPA%ILPDIS(I,J) = DIST
      LNPA%LLP(J) = LLP
      RETURN
      END                                                               LPPUSH
C
C
C
      SUBROUTINE CHNGLN (LNPA,ILP,DIST)
C
C-----THIS SUBROUTINE ADDS DATA FOR A LANE CHANGE ON AN APPROACH
C-----THE ARRAY WILL STORE UP TO LNCHMX LANES FOR EACH APPROACH
C-----NOT FOR CHANGING FROM LANE TO PATH OR FROM PATH TO LANE
C
C-----LNPA - LANE AND PATH DATA STRUCTURE FOR THE VEHICLE
C-----ILP - LANE/PATH NUMBER TO PUT IN NEXT AVAILABLE LOCATION
C-----DIST - DISTANCE DOWN LANE OR PATH
C

      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (LNPAVE)::LNPA
      DOUBLE PRECISION        DIST
      INTEGER             ILP
      CHARACTER*132 ERRMSG
      INTEGER I,J
  901 FORMAT('LANE CHANGE INDEX - I = ',I3,' IS <= 0 OR > ',I2,
     *       ' - CHNGLP')
      J = LNPA%ILPNOW
      I = LNPA%ILNCHG(J) + 1
      IF (ILP .EQ. LNPA%ILP(I,J))                  THEN
C-----LANE CHANGE INTO CURRENT LANE
C RFI THIS NEEDS TO BE PREVENTED SOMEWHERE ELSE
        RETURN
      END IF
      LNPA%ILNCHG(J) = I
      I = I + 1
      IF ((I .LE. 0 ) .OR. (I .GT. LNCHMX))        THEN
        WRITE(ERRMSG,901)I,LNCHMX
        CALL  PRTNER ( 99,ERRMSG )
        STOP
      END IF
      LNPA%ILP(I,J) = ILP
      LNPA%ILPDIS(I,J) = DIST
      RETURN
      END                                                               CHNGLP
C
C
C
      FUNCTION IGETLP (LNPA,INDXLP,INDXLC)
C
C-----THIS FUNCTION RETURNS A LANE OR PATH NUMBER FROM AN ARRAY
C-----  POSITIVE IF LANE, NEGATIVE IF PATH
C
C-----LNPA - LANE AND PATH DATA STRUCTURE FOR THE VEHICLE
C-----INDXLP - REFERENCE TO LANE/PATH NUMBER TO RETURN
C-----       0 - CURRENT LANE/PATH FOR FRONT OF VEHICLE
C-----       1 - FIRST   LANE/PATH BEHIND FRONT OF VEHICLE
C-----       2 - SECOND  LANE/PATH BEHIND FRONT OF VEHICLE
C-----       3 - THIRD   LANE/PATH BEHIND FRONT OF VEHICLE
C-----       4 - FOURTH  LANE/PATH BEHIND FRONT OF VEHICLE
C-----INDXLC - REFERENCE TO CHANGED LANES ON APPROACH
C-----       0 - CURRENT LANE FOR FRONT OF VEHICLE
C-----       1 - LANE BEFORE MOST RECENT LANE CHANGE
C-----       2 - LANE BEFORE SECOND MOST RECENT LANE CHANGE
C-----       3 - LANE BEFORE THIRD MOST RECENT LANE CHANGE
C-----       4 - ETC.

      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (LNPAVE)::LNPA
      INTEGER            INDXLP,INDXLC
      CHARACTER*132 ERRMSG
      INTEGER I,IGETLP,J
  901 FORMAT('LANE CHANGE INDEX - I = ',I3,' IS <= 0 OR > ',I2,
     *       ' - IGETLP')
      J = LNPA%ILPNOW - INDXLP
      IF ((J .LE. 0 ) .OR. (J .GT. 5))           THEN
        IGETLP = 0
        RETURN
      END IF
      IF (.NOT. LNPA%LLP(J) .AND. (INDXLC.NE.0)) THEN
C-----NO LANE CHANGES ON PATH
        CALL  PRTNER ( 99,'NO OCCUPIED LANE OR PATH - IGETLP' )
        STOP
      END IF
      I = LNPA%ILNCHG(J) + 1 - INDXLC
      IF ((I .LE. 0 ) .OR. (I .GT. LNCHMX))      THEN
        WRITE(ERRMSG,901)I,LNCHMX
        CALL  PRTNER ( 99,ERRMSG )
        STOP
      END IF
      IGETLP = LNPA%ILP(I,J)
      IF (.NOT. LNPA%LLP(J))                     THEN
        IGETLP = -IGETLP
      END IF
      RETURN
      END                                                               IGETLP
C
C
C
      FUNCTION IGETDI (LNPA,INDXLP,INDXLC)
C
C-----THIS FUNCTION RETURNS A DISTANCE DOWN A LANE OR PATH
C-----  POSITIVE IF LANE, NEGATIVE IF PATH
C
C-----LNPA - LANE AND PATH DATA STRUCTURE FOR THE VEHICLE
C-----INDXLP - REFERENCE TO LANE/PATH NUMBER TO RETURN
C-----       0 - CURRENT LANE/PATH FOR FRONT OF VEHICLE
C-----       1 - FIRST   LANE/PATH BEHIND FRONT OF VEHICLE
C-----       2 - SECOND  LANE/PATH BEHIND FRONT OF VEHICLE
C-----       3 - THIRD   LANE/PATH BEHIND FRONT OF VEHICLE
C-----       4 - FOURTH  LANE/PATH BEHIND FRONT OF VEHICLE
C-----INDXLC - REFERENCE TO CHANGED LANES ON APPROACH
C-----       0 - CURRENT LANE FOR FRONT OF VEHICLE
C-----       1 - LANE BEFORE MOST RECENT LANE CHANGE
C-----       2 - LANE BEFORE SECOND MOST RECENT LANE CHANGE
C-----       3 - LANE BEFORE THIRD MOST RECENT LANE CHANGE
C-----       4 - ETC.
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (LNPAVE)::LNPA
      INTEGER            INDXLP,INDXLC
      CHARACTER*132 ERRMSG
      INTEGER I,IGETDI,J
  901 FORMAT('LANE CHANGE INDEX - I = ',I3,' IS <= 0 OR > ',I2,
     *       ' - IGETDI')
      J = LNPA%ILPNOW - INDXLP
      IF ((J .LE. 0 ) .OR. (J .GT. 5))           THEN
        IGETDI = 0
        RETURN
      END IF
      IF (.NOT. LNPA%LLP(J) .AND. (INDXLC.NE.0)) THEN
C-----NO LANE CHANGES ON PATH
        CALL  PRTNER ( 99,'NO OCCUPIED LANE OR PATH - IGETDI' )
        STOP
      END IF
      I = LNPA%ILNCHG(J) + 1 - INDXLC
      IF ((I .LE. 0 ) .OR. (I .GT. LNCHMX))      THEN
        WRITE(ERRMSG,901)I,LNCHMX
        CALL  PRTNER ( 99,ERRMSG )
        STOP
      END IF
      IGETDI = LNPA%ILP(I,J)
      IF (.NOT. LNPA%LLP(J))                     THEN
        IGETDI = -IGETDI
      END IF
      RETURN
      END                                                               IGETDI
C
C
C
      SUBROUTINE LPCLER (LNPA)
C
C-----THIS SUBROUTINE CLEARS ALL LANE/PATH NUMBERS FOR A VEHICLE
C
C-----LNPA - LANE AND PATH DATA STRUCTURE FOR THE VEHICLE
C
      IMPLICIT NONE                                                     CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'TYPES'
      INCLUDE 'LNPATH'
      TYPE (LNPAVE)::LNPA
      LNPA%ILP    = 0
      LNPA%ILPDIS = 0.0D0
      LNPA%ILNCHG = 0
      LNPA%ILPNOW = 0
      RETURN
      END                                                               LPCLER
C
C
C
      FUNCTION ARCARC (XC1,YC1,R1,XC2,YC2,R2,A21,A22,X1,Y1,X2,Y2,GAP,
     1                 XN1,YN1,X,Y)
C
C-----FIND INTERSECTION POINT(S) OF 2 ARCS
C-----CHOOSE ONE TO USE
C
C-----(XC1,YC1),R1 - FIRST  CIRCLE CENTER,RADIUS
C-----(XC2,YC2),R2 - SECOND CIRCLE (PATH ARC) CENTER,RADIUS
C-----A21,A22 - SECOND CIRCLE (ARC PATH) START ANGLE,SWEEP ANGLE
C-----(X1,Y1) - BEGIN POINT OF SECOND CIRCLE
C-----(X2,Y2) - END POINT OF SECOND CIRCLE
C-----GAP - IS THERE BETWEEN THIS SEGMENT AND NEXT SEGMENT ?
C-----(XN1,YN1) - BEGIN POINT OF NEXT SEGMENT
C-----(X,Y) - RETURNED POINT OF INTERSECTION

      IMPLICIT  NONE                                                    CCODE=C.
      INCLUDE 'PARAMS'
      DOUBLE PRECISION XC1,YC1,R1,XC2,YC2,R2,A21,A22,X1,Y1,X2,Y2
      LOGICAL                                                    GAP
      DOUBLE PRECISION XN1,YN1,X,Y
      DOUBLE PRECISION A1,A2,A3,B1,B2,B3,C1,C2,C3,DX,DY,INTRCP,
     1                 SLOPE,T,XI(2),YI(2)
      LOGICAL ARCARC,NEARER,PTINSA,PTINRL,SOLVER
      IF ( R2 .EQ.0.0D0 ) THEN
C-----THIS PATH ARC DOES NOT EXITS
        ARCARC = .FALSE.
        RETURN
      END IF
      IF ((XC1 .EQ. XC2) .AND. (YC1 .EQ. YC2))   THEN
C-----CONCENTRIC, NO USEFUL INTERSECTION POINT
        ARCARC = .FALSE.
        RETURN
      END IF
      T = SQRT((XC2-XC1)**2 + (YC2-YC1)**2)
      IF (T .GT. (R1+R2))                        THEN
C-----TOO WIDELY SPACED TO INTERSECT
        ARCARC = .FALSE.
        RETURN
      END IF
      IF (R1 .LT. R2)                            THEN
        IF (T+R1 .LE. R2)                        THEN
C-----CIRCLE 1 ENTIRELY WITHIN CIRCLE 2
          ARCARC = .FALSE.
          RETURN
        END IF
      ELSE
        IF (T+R2 .LE. R1)                        THEN
C-----CIRCLE 2 ENTIRELY WITHIN CIRCLE 1
          ARCARC = .FALSE.
          RETURN
        END IF
      END IF
      IF (DABS(YC1-YC2) .LE. NRZERO)             THEN
C-----SLOPE OF COMMON CHORD IS INFINITE
C-----COSINE RULE
        DX = DABS(XC2-XC1)
        T = R2**2 - R1**2 - DX**2
        T = T/(-2.0D0 * DX * R1)
        T = DACOS(T)
        IF (XC2 .GT. XC1)                        THEN
          XI = XC1 + R1 * DCOS(T)
        ELSE
          XI = XC1 - R1 * DCOS(T)
        END IF
        DY = R1 * DSIN(T)
        YI(1) = YC1 + DY
        YI(2) = YC1 - DY
        ARCARC = .TRUE.
                                                 GO TO 1000
      END IF
C-----EQUATION OF FIRST CIRCLE
C-----X*X + Y*Y + A1*X + A2*Y + A3 = 0
      CALL ARCCOF (XC1,YC1,R1,A1,A2,A3)
C-----EQUATION OF SECOND CIRCLE
C-----X*X + Y*Y + B1*X + B2*Y + B3 = 0
      CALL ARCCOF (XC2,YC2,R2,B1,B2,B3)
C-----ELIMINATE X*X AND Y*Y BY SUBTRACTING SECOND FROM FIRST
C-----YIELDS EQUATION OF COMMON CHORD
C-----C1*X + C2*Y + C3 = 0
      C1 = A1-B1
      C2 = A2-B2
      C3 = A3-B3
C-----SOLVE EQUATION OF COMMON CHORD FOR Y
C-----C1*X + C2*Y + C3 = 0
C-----Y = -(((C1*X) + C3) / C2)
C-----Y = -(C1/C2)*X - (C3/C2)
      SLOPE = -C1/C2
      INTRCP = -C3/C2
      ARCARC = SOLVER (A1,A2,A3,SLOPE,INTRCP,XI,YI)
      IF (.NOT. ARCARC)                          THEN
        RETURN
      END IF
 1000 CONTINUE
      IF (NEARER (X1,Y1,XI,YI))                  THEN
        X = XI(1)
        Y = YI(1)
      ELSE
        X = XI(2)
        Y = YI(2)
      END IF
C-----IS CALCULATED POINT WITHIN RANGE OF PATH ARC (SECOND CIRCLE)
      ARCARC = PTINSA (X,Y,XC2,YC2,A21,A22)
      IF (.NOT.ARCARC .AND. GAP)                 THEN
C-----IS CALC. POINT IN GAP AFTER THIS SEGMENT AND BEFORE NEXT SEGMENT
C-----(WITHIN RANGE OF THE GAP)
        ARCARC = PTINRL (X,Y,X2,Y2,XN1,YN1)
      END IF
      RETURN
      END                                                               ARCARC
C
C
C
      FUNCTION ARCLIN (XC,YC,R,XL1,YL1,XL2,YL2,GAP,XN1,YN1,X,Y)
C
C-----FIND INTERSECTION POINT(S) OF CIRCLE AND LINE
C
C-----CIRCLE - RADIUS = R, CENTER = (XC,YC)
C-----LINE - POINTS ON LINE = (XL1,YL1),(XL2,YL2)
C-----GAP - IS THERE BETWEEN THIS SEGMENT AND NEXT SEGMENT ?
C-----(XN1,YN1) - BEGIN POINT OF NEXT SEGMENT
C-----RETURNED POINT OF INTERSECTION (X,Y)

      IMPLICIT  NONE                                                    CCODE=C.
      INCLUDE 'PARAMS'
      DOUBLE PRECISION XC,YC,R,XL1,YL1,XL2,YL2
      LOGICAL                                  GAP
      DOUBLE PRECISION                             XN1,YN1,X,Y
      DOUBLE PRECISION A1,B1,C1,C,D,E,DISTC,DY,PTTOLN,
     1                 SLOPE,XI(2),YI(2),YNTRCP
      LOGICAL ARCLIN,PTINCI,SOLVER,INCIR1,INCIR2,PTINRL
      IF ((XL1 .EQ.0.0D0).AND. (YL1 .EQ.0.0D0))  THEN
C-----THIS SEGMENT NOT PRESENT
        ARCLIN = .FALSE.
        RETURN
      END IF
      CALL LINCOF (XL1,YL1,XL2,YL2,A1,B1,C1)
C
C-----PERPINDICULAR DISTANCE FROM CENTER OF CIRCLE TO LINE
C
      DISTC = PTTOLN (XC,YC,A1,B1,C1)
      IF (DISTC .GT. R)                          THEN
C
C-----LINE IS OUTSIDE OF CIRCLE, NO INTERSECTION
C
        ARCLIN = .FALSE.
        RETURN
      END IF
      INCIR1 = PTINCI(XC,YC,R,XL1,YL1)
      INCIR2 = PTINCI(XC,YC,R,XL2,YL2)
      IF (INCIR1 .AND. INCIR2)                   THEN
C-----BOTH ENDS OF LINE ARE WITHIN CIRCLE, NO INTERSECTION
        ARCLIN = .FALSE.
        RETURN
      ELSE IF ((.NOT.INCIR1) .AND. (.NOT.INCIR2))THEN
C-----BOTH ENDS OF LINE ARE OUTSIDE CIRCLE
C-----LINE INTERSECTS WITH CIRCLE AT 2 POINTS
        IF (DABS(XL1-XL2) .LT. NRZERO)           THEN
C-----LINE HAS INFINITE SLOPE
          IF (XL1 .LT. XC)                       THEN
            DISTC = -DISTC
          END IF
          DY = R * DSIN(DACOS(DISTC/R))
          X = XL1
          IF (YL2 .GT. YL1)                      THEN
C-----GOING NORTH
            Y = YC - DY
          ELSE
C-----GOING SOUTH
            Y = YC + DY
          END IF
          GO TO 1000
        END IF
      ELSE IF (INCIR1)                           THEN
C-----THE ONLY INTERSECTION POINT IS AHEAD OF THIS UNIT AND INVALID
        ARCLIN = .FALSE.
        RETURN
      ELSE
C-----ENDING END IN, STARTING END OUT OF CIRCLE
        IF (DABS(XL1-XL2) .LT. NRZERO)           THEN
C-----LINE HAS INFINITE SLOPE
          DY = R * DSIN(DACOS(DISTC/R))
          IF (INCIR1)                            THEN
            ARCLIN = .FALSE.
          ELSE
            X = XL1
            IF (YL2 .GT. YL1)                    THEN
              Y = YC - DY
            ELSE
              Y = YC + DY
            END IF
            ARCLIN = .TRUE.
          END IF
          RETURN
        END IF
      END IF
C-----EQUATION OF LINE
C-----Y = SLOPE * X + YNTRCP
      SLOPE  = (YL2-YL1)/(XL2-XL1)
      YNTRCP = YL1 - (XL1*SLOPE)
C-----EQUATION OF CIRCLE
C-----X*X + Y*Y + D *X + E*Y + C = 0
      CALL ARCCOF (XC,YC,R,D,E,C)
      ARCLIN = SOLVER (D,E,C,SLOPE,YNTRCP,XI,YI)
      IF (ARCLIN)                                THEN
        IF (INCIR1 .AND. (.NOT.INCIR2))          THEN
          ARCLIN = .FALSE.
          RETURN
        ELSE
          IF (XL2 .LE. XL1)                      THEN
            IF (XI(1) .GT. XI(2))                THEN
              X = XI(1)
              Y = YI(1)
            ELSE
              X = XI(2)
              Y = YI(2)
            END IF
          ELSE
            IF (XI(1) .GT. XI(2))                THEN
              X = XI(2)
              Y = YI(2)
            ELSE
              X = XI(1)
              Y = YI(1)
            END IF
          END IF
        END IF
      END IF
 1000 CONTINUE
C-----IS CALCULATED POINT WITHIN RANGE OF PATH LINE
      ARCLIN = PTINRL (X,Y,XL1,YL1,XL2,YL2)
      IF (.NOT.ARCLIN .AND. GAP)                 THEN
C-----IS CALCULATED POINT IN GAP AFTER THIS SEGMENT AND BEFORE SEGMENT
C-----(WITHIN RANGE OF THE GAP)
        ARCLIN = PTINRL (X,Y,XL2,YL2,XN1,YN1)
      END IF
      RETURN
      END                                                               ARCLIN
C
C
C
      FUNCTION PTINRL (X,Y,XL1,YL1,XL2,YL2)
C
C-----IS A POINT WITHIN THE RANGE OF A LINE
C
C-----(X,Y) - COORDINATES OF THE POINT
C-----(XL1,YL1),(XL2,YL2) - COORDINATES OF ENDPOINTS OF LINE
      IMPLICIT  NONE                                                    CCODE=C.
      INCLUDE 'PARAMS'
      LOGICAL PTINRL
      DOUBLE PRECISION X,Y,XL1,YL1,XL2,YL2
      IF (XL2 .LE. XL1)                          THEN
        IF (((XL2-X) .GT. NRZERO) .OR.
     1      ((X-XL1) .GT. NRZERO))               THEN
          PTINRL = .FALSE.
          RETURN
        END IF
      ELSE
        IF (((XL1-X) .GT. NRZERO) .OR.
     1      ((X-XL2) .GT. NRZERO))               THEN
          PTINRL = .FALSE.
          RETURN
        END IF
      END IF
      IF (YL2 .LE. YL1)                          THEN
        IF (((YL2-Y) .GT. NRZERO) .OR.
     1      ((Y-YL1) .GT. NRZERO))               THEN
          PTINRL = .FALSE.
          RETURN
        END IF
      ELSE
        IF (((YL1-Y) .GT. NRZERO) .OR.
     1      ((Y-YL2) .GT. NRZERO))               THEN
          PTINRL = .FALSE.
          RETURN
        END IF
      END IF
      PTINRL = .TRUE.
      RETURN
      END                                                               PTINRL
C
C
C
      FUNCTION PTINSA (XP,YP,XC,YC,A1,A2)
C
C-----IS A POINT WITHIN THE SWEEP OF AN ARC
C
C-----(XP,YP) - COORDINATES OF THE POINT
C-----XC,YC - ARC CENTER (XC,YC)
C-----A1,A2 - ARC START ANGLE, SWEEP ANGLE
      IMPLICIT  NONE                                                    CCODE=C.
      INCLUDE 'PARAMS'
      INCLUDE 'CONSTN'
      LOGICAL PTINSA
      DOUBLE PRECISION XP,YP,XC,YC,A1,A2
      DOUBLE PRECISION AP
C-----ANGLE FROM CENTER OF ARC TO POINT
      AP = DATAN2((YP-YC),(XP-XC))
      IF (AP .LT. 0.0D0 ) AP = AP + PIT2
      IF (A2 .GT. -NRZERO)                       THEN
        IF (AP .LT. A1)                          THEN
          AP = AP + PIT2
        END IF
        PTINSA = (AP .GE. A1) .AND. (AP .LE. (A1 + A2))
      ELSE
        IF (AP .GT. A1)                          THEN
          AP = AP - PIT2
        END IF
        PTINSA = (AP .LE. A1) .AND. (AP .GE. (A1 + A2))
      END IF
      RETURN
      END                                                               PTINSA
C
C
C
      FUNCTION PTINCI(XC,YC,R,X,Y)
C
C-----IS POINT WITHIN CIRCLE ?
C
C-----CIRCLE - RADIUS = R, CENTER = (XC,YC)
C-----POINT - (X,Y)
C
      IMPLICIT  NONE                                                    CCODE=C.
      INCLUDE 'PARAMS'
      LOGICAL PTINCI
      DOUBLE PRECISION XC,YC,R,X,Y
      DOUBLE PRECISION D
      D = SQRT((XC-X)**2 + (YC-Y)**2)
      PTINCI = (D-R) .LE. -NRZERO
      RETURN
      END                                                               PTINCI
C
C
C
      FUNCTION PTTOLN (X,Y,A,B,C)
C
C-----MINIMUM DISTANCE FROM POINT TO LINE
C
C-----X,Y - COORDINATES OF POINT
C-----A,B,C - COEFFICIENTS FOR EQUATION OF LINE
C             A*X + B*Y + C = 0
      IMPLICIT  NONE                                                    CCODE=C.
      INCLUDE 'PARAMS'
      DOUBLE PRECISION PTTOLN,X,Y,A,B,C
      DOUBLE PRECISION T1,T2
      T1 = A*X + B*Y+ C
      T2 = DSQRT(A**2 + B**2)
      IF (T2 .EQ. 0.0)                           THEN
C RFI NOT DONE YET
        PTTOLN = 0.0D0
        RETURN
      END IF
      IF (C .EQ. 0.0D0)                          THEN
        IF (B .LT. 0.0D0)                        THEN
          T2 = -T2
        END IF
      ELSE
        IF (C .GE. 0.0D0)                        THEN
          T2 = -T2
        END IF
      END IF
      PTTOLN = DABS(T1/T2)
      IF (PTTOLN .LT. NRZERO)                    THEN
        PTTOLN = 0.0D0
      END IF
      RETURN
      END                                                                       PTTOLN

C
C
C
      FUNCTION NEARER(X,Y,XI,YI)
C
C-----IS (X,Y) NEARER TO (XI(1),YI(1)) THAN TO (XI(2),YI(2))
C
      IMPLICIT  NONE                                                    CCODE=C.
      DOUBLE PRECISION X,Y,XI(2),YI(2)
      LOGICAL NEARER
      DOUBLE PRECISION D1,D2
      D1 = DSQRT((X-XI(1))**2 + (Y-YI(1))**2)
      D2 = DSQRT((X-XI(2))**2 + (Y-YI(2))**2)
      NEARER = (D1 .LT. D2)
      RETURN
      END                                                               NEARER
C
C
C
      FUNCTION SOLVER (A,B,C,SLOPE,YNTRCP,XI,YI)
      IMPLICIT  NONE                                                    CCODE=C.
      DOUBLE PRECISION A,B,C,SLOPE,YNTRCP,XI(2),YI(2)
      DOUBLE PRECISION AQ,BQ,CQ,T
      LOGICAL SOLVER
C
C-----FINDS POINT(S) OF INTERSECTION OF CIRCLE AND LINE
C
C-----A,B,C - CONSTANTS IN EQUATION OF CIRCLE
C-----SLOPE,YNTRCP - SLOPE AND Y INTERCEPT IN EQUATION OF LINE
C-----XI,YI - RETURNED COORDINATES OF POINT(S) OF INTERSECTION
C-----EQUATION OF LINE
C-----Y = (SLOPE * X) + YNTRCP
C
C-----EQUATION OF CIRCLE
C-----X*X +
C----- Y*Y +
C-----  A*X +
C-----   B*Y +
C-----    C = 0
C
C-----PLUG EQUATION OF LINE INTO EQUATION OF CIRCLE
C-----X*X +
C----- [(SLOPE*X) + YNTRCP]*[(SLOPE*X) + YNTRCP] +
C-----  A*X +
C-----   B*[(SLOPE*X) + YNTRCP] +
C-----    C = 0
C
C-----PUT INTO QUADRATIC FORM: AQ*X*X + BQ*X + CQ = 0
C-----X*X + [(SLOPE)*(SLOPE)]*X*X + [2*YNTRCP*SLOPE]*X + (YNTRCP*YNTRCP) +
C-----               [A + B*SLOPE]*X + B*YNTRCP + C = 0
C
C-----USE QUADRATIC FORMULA TO SOLVE  FOR X1 AND X2
      AQ = 1 + (SLOPE*SLOPE)
      BQ = 2*YNTRCP*SLOPE + A + B*SLOPE
      CQ = YNTRCP*YNTRCP + B*YNTRCP + C
      T = BQ*BQ -(4.0D0*AQ*CQ)
      IF (T .LT. 0.0D0)                          THEN
        SOLVER = .FALSE.
        RETURN
      END IF
      XI(1) = (-BQ + DSQRT(T))/(2.0D0*AQ)
      XI(2) = (-BQ - DSQRT(T))/(2.0D0*AQ)
C-----PLUG X INTO EQUATION OF LINE AND SOLVE FOR Y1 AND Y2
      YI(1) = SLOPE * XI(1) + YNTRCP
      YI(2) = SLOPE * XI(2) + YNTRCP
      SOLVER = .TRUE.
      RETURN
      END                                                               SOLVER
C
C
C
      SUBROUTINE ARCCOF (XC,YC,R,A,B,C)
      IMPLICIT  NONE                                                    CCODE=C.
      DOUBLE PRECISION XC,YC,R,A,B,C
C
C-----FIND COEFFICIENTS OF EQUATION OF CIRCLE
C
C-----X*X + Y*Y + A*X + B*Y + C = 0
C-----RADIUS = R, CENTER = (XC,YC)
      A = -2.0D0 * XC
      B = -2.0D0 * YC
      C = (XC * XC) + (YC * YC) -(R * R)
      RETURN
      END                                                               ARCCOF
C
C
C
      SUBROUTINE LINCOF (X1,Y1,X2,Y2,A,B,C)
      IMPLICIT  NONE                                                    CCODE=C.
      DOUBLE PRECISION X1,Y1,X2,Y2,A,B,C
C
C-----FIND COEFFICIENTS FOR EQUATION OF LINE
C
C-----2 POINT FORM
C       A*X + B*Y + C = 0
C-----X*Y(1) - X*Y(2) - Y*X(1) +Y*X(2) + X(1)*Y(2) - X(2)*Y(1) = 0
C-----POINTS ON LINE (X1,Y1), (X2,Y2)
C
      A =  Y1 - Y2
      B = -X1 + X2
      C =  X1*Y2 - X2*Y1
      RETURN
      END                                                               LINCOF
C
C
C
      SUBROUTINE PRTNER ( NER,MESAGE )
      IMPLICIT NONE                                                     CCODE=C.
      CHARACTER*(*)     MESAGE
      INTEGER           ILNB
      INTEGER           NCMES,NER
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
      END                                                               PRTNER
