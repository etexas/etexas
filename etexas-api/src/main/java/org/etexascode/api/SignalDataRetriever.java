/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** *  COPYRIGHT (C) 2003 by The University of Texas at Austin   * ** *
 * * ** *                                                            * ** *
 * * ** * Permission is hereby granted to use, modify, copy, and     * ** *
 * * ** * distribute this software and its documentation for any     * ** *
 * * ** * purpose only without profit, provided that the above       * ** *
 * * ** * Copyright Notice appears in all copies and that both the   * ** *
 * * ** * Copyright Notice and this Permission Notice appears in     * ** *
 * * ** * every copy of supporting documentation.  No title to nor   * ** *
 * * ** * ownership of the software is transferred hereby.  The name * ** *
 * * ** * of The University of Texas at Austin shall not be used in  * ** *
 * * ** * advertising or publicity related to the distribution of    * ** *
 * * ** * the software without specific, written, prior permission.  * ** *
 * * ** * This software is provided as-delivered without expressed   * ** *
 * * ** * or implied warranty.  The University of Texas at Austin    * ** *
 * * ** * makes no representation about the suitability of this      * ** *
 * * ** * software for any purpose and accepts no responsibility for * ** *
 * * ** * its use.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** * This program is free software; you can redistribute it     * ** *
 * * ** * and/or modify it under the terms of the GNU General Public * ** *
 * * ** * License as published by the Free Software Foundation;      * ** *
 * * ** * either version 2 of the License, or (at your option) any   * ** *
 * * ** * later version.                                             * ** *
 * * ** *                                                            * ** *
 * * ** * This program is distributed in the hope that it will be    * ** *
 * * ** * useful, but WITHOUT ANY WARRANTY; without even the implied * ** *
 * * ** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ** *
 * * ** * PURPOSE.  See the GNU General Public License for more      * ** *
 * * ** * details.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** * You should have received a copy of the GNU General Public  * ** *
 * * ** * License along with this program; if not, write to the Free * ** *
 * * ** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ** *
 * * ** * Floor, Boston, MA 02110-1301, USA.                         * ** *
 * * ** *                                                            * ** *
 * * ** * For more information: http://www.gnu.org/licenses/gpl.html * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * #L%
 */

package org.etexascode.api;

/**
 * A class used to provide convenience access into the Simpro_SignalData class
 * 
 * @author bbadillo
 */
class SignalDataRetriever {

    /**
     * Data wrapped by this class which represents signal data from SIMPRO
     */
    private SimproJNA.SIMPRO_SignalData data;

    private static final int ISISET_SETSIZE = SimproJNA.NCM + 2 + SimproJNA.NON + SimproJNA.NON;

    /**
     * Wrapper class constructor
     */
    SignalDataRetriever() {
        this.data = new SimproJNA.SIMPRO_SignalData();
        SimproInterface.getSPATData(data);
    }

    /**
     * ICONTR : Intersection control Values: ICUNCT = 1 = Uncontrolled ICYELD = 2 = Yield Sign
     * ICLTAS = 3 = Less-than-all-way stop sign ICAWST = 4 = All-way stop sign ICPSIG = 5 =
     * PRE-TIMED SIGNAL ICSACT = 6 = Semi-actuated signal ICFACT = 7 = Full-actuated signal ICTDF3 =
     * 8 = TEXAS diamond fig 3 signal ICTDF4 = 9 = TEXAS diamond fig 4 signal ICTDF6 = 10 = TEXAS
     * diamond fig 6 signal ICTDF7 = 11 = TEXAS diamond fig 7 signal ICDDF3 = 12 = DALLAS diamond
     * fig 3 signal ICDDF4 = 13 = DALLAS diamond fig 4 signal ICDDF6 = 14 = DALLAS diamond fig 6
     * signal ICDDF7 = 15 = DALLAS diamond fig 7 signal ICNEMA = 16 = NEMA Signal ICNEMV = 17 = NEMA
     * volume density signal ICHDWR = 18 = Hardware-in-the-loop 19 = Unknown intersection control
     * 
     * @return The integer code for ICONTR
     */
    int getICONTR() {
        return this.data.intVals[SimproJNA.ICONTR];
    }

    int getNIBL() {
        return this.data.intVals[SimproJNA.NIBL];
    }

    int getIARRPH() {
        return this.data.intVals[SimproJNA.IARRPH];
    }

    int getICAMPC() {
        return this.data.intVals[SimproJNA.ICAMPC];
    }

    int getICAMPO() {
        return this.data.intVals[SimproJNA.ICAMPO];
    }

    int getICPHAS() {
        return this.data.intVals[SimproJNA.ICPHAS];
    }

    int getIGO() {
        return this.data.intVals[SimproJNA.IGO];
    }

    int getNCAMSP() {
        return this.data.intVals[SimproJNA.NCAMSP];
    }

    int getISISET(int i, int j) {
        return this.data.ISISET[(j - 1) * ISISET_SETSIZE + (i - 1)];
    }

    double getTR() {
        return this.data.doubleVals[SimproJNA.TR];
    }

    double getTCAMSP(int i) {
        return this.data.TCAMSP[i - 1];
    }
}
