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
 * A class used to provide convenience access into the Simpro_MapData class
 * 
 * @author bbadillo
 */
class MapDataRetriever {

    /**
     * Data wrapped by this class which represents map data from SIMPRO
     */
    private SimproJNA.SIMPRO_MapData data;

    /**
     * Wrapper class constructor
     */
    MapDataRetriever() {
        this.data = new SimproJNA.SIMPRO_MapData();
        SimproInterface.getMAPData(data);
    }

    int getNRLAN() {
        return this.data.intVals[SimproJNA.NRLAN];
    }

    int getIBLN(int i) {
        return this.data.IBLN[i - 1];
    }

    int getLIBAR(int i) {
        return this.data.LIBAR[i - 1];
    }

    int getLOBAR(int i) {
        return this.data.LOBAR[i - 1];
    }

    int getISNA(int i) {
        return this.data.ISNA[i - 1];
    }

    int getLWID(int i) {
        return this.data.LWID[i - 1];
    }

    int getLTURN(int i) {
        return this.data.LTURN[i - 1];
    }

    double getBASELX(int i) {
        return this.data.BASELX[i - 1];
    }

    double getBASELY(int i) {
        return this.data.BASELY[i - 1];
    }

    double getENDLNX(int i) {
        return this.data.ENDLNX[i - 1];
    }

    double getENDLNY(int i) {
        return this.data.ENDLNY[i - 1];
    }

    int[] getLGEOM() {
        return this.data.LGEOM;
    }

    int[] getISLIM() {
        return this.data.ISLIM;
    }

    int getNLANES(int approachId) {
        return this.data.NLANES[approachId - 1];
    }

    int getLIBA(int i) {
        return this.data.LIBA[i - 1];
    }

    int getNIBA() {
        return this.data.intVals[SimproJNA.NIBAINDEX];
    }

    int getLLANES(int lane, int approach) {
        return this.data.LLANES[(approach - 1) * SimproJNA.NAL + (lane - 1)];
    }

    int getLCONTR(int i) {
        return this.data.LCONTR[i - 1];
    }
}
