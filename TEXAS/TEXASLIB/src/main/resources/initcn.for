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
      SUBROUTINE INITCN
      IMPLICIT          NONE                                            CCODE=C.
      INCLUDE 'CONSTN'
C-----INITIALIZE CONSTANTS
      PI     = 4.0D0*DATAN( 1.0D0 )
      DEG2RD = PI/180.0D0
      FPS2MH = 60.0D0/88.0D0
      MPH2FS = 88.0D0/60.0D0
      ONED3  = 1.0D0/3.0D0
      ONED6  = 1.0D0/6.0D0
      PID2   = PI/2.0D0
      PIT1P5 = PI*1.5D0
      PIT2   = PI*2.0D0
      RAD2DG = 180.0D0/PI
      SQR2D2 = DSQRT( 2.0D0 )/2.0D0
      SQRT2P = DSQRT( PIT2 )
      SQRT3  = DSQRT( 3.0D0 )
      TWOD3  = 2.0D0/3.0D0
      RETURN
      END                                                               INITCN
