/**********************************************************************
 *** *                                                            * ***
 *** *  Copyright (c) 2012 Harmonia Holdings Group LLC            * ***
 *** *                                                            * ***
 *** * Permission is hereby granted to use, modify, copy, and     * ***
 *** * distribute this software and its documentation for any     * ***
 *** * purpose only without profit, provided that the above       * ***
 *** * Copyright Notice appears in all copies and that both the   * ***
 *** * Copyright Notice and this Permission Notice appears in     * ***
 *** * every copy of supporting documentation.  No title to nor   * ***
 *** * ownership of the software is transferred hereby.  The name * ***
 *** * of Harmonia Holdings Group LLC shall not be used in        * ***
 *** * advertising or publicity related to the distribution of    * ***
 *** * the software without specific, written, prior permission.  * ***
 *** * This software is provided as-delivered without expressed   * ***
 *** * or implied warranty.  Harmonia Holdings Group LLC          * ***
 *** * makes no representation about the suitability of this      * ***
 *** * software for any purpose and accepts no responsibility for * ***
 *** * its use.                                                   * ***
 *** *                                                            * ***
 *** ************************************************************** ***
 *** *                                                            * ***
 *** * This program is free software; you can redistribute it     * ***
 *** * and/or modify it under the terms of the GNU General Public * ***
 *** * License as published by the Free Software Foundation;      * ***
 *** * either version 2 of the License, or (at your option) any   * ***
 *** * later version.                                             * ***
 *** *                                                            * ***
 *** * This program is distributed in the hope that it will be    * ***
 *** * useful, but WITHOUT ANY WARRANTY; without even the implied * ***
 *** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ***
 *** * PURPOSE.  See the GNU General Public License for more      * ***
 *** * details.                                                   * ***
 *** *                                                            * ***
 *** * You should have received a copy of the GNU General Public  * ***
 *** * License along with this program; if not, write to the Free * ***
 *** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ***
 *** * Floor, Boston, MA 02110-1301, USA.                         * ***
 *** *                                                            * ***
 *** * For more information: http://www.gnu.org/licenses/gpl.html * ***
 *** *                                                            * ***
 **********************************************************************/
package org.etexascode.dcs.bindings;

import java.util.Iterator;
import org.etexascode.dcs.SignalController;
import org.etexascode.api.eTEXAS;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.datamodel.Signal;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalManager;

/**
 * A binding class which binds the DCS signal controller to the TEXAS signal controller
 *
 * @author bbadillo
 */
public class SignalControllerForETEXAS implements SignalController {

    /**
     * The TEXAS model which contains Public API methods
     */
    private eTEXAS texasSim;
    /**
     * The InterRep object that represents the intersection
     */
    private InterRep interRep;


    /**
     * Constructor used to wrap the TEXAS model public API object
     *
     * @param texasSim The TEXAS model public API object
     */
    public SignalControllerForETEXAS(eTEXAS texasSim, InterRep inputInterRep) {
        this.texasSim = texasSim;
        this.interRep = inputInterRep;
    }

    /**
     * Advance the signal controller to the next phase.
     */
    @Override
    public void changePhase() {
        texasSim.changeSignal(0.0);
    }

    /**
     * Continue to hold the current phase of the signal controller.
     */
    @Override
    public void holdPhase() {
        texasSim.changeSignal(1.5);
    }

    /**
     * Check to see if the signal controller is holding a green phase.
     *
     * @return Whether or not a signal controller is holding a green phase.
     */
    @Override
    public boolean isHoldingGreen() {

        SignalManager spatData = this.interRep.getSignalManager();
        Iterator<Integer> iterator = spatData.getKeys().iterator();


        boolean allGreen = true;
        while (iterator.hasNext()) {
            Integer nextID = iterator.next();
            Signal signal = spatData.getSignal(nextID);
            Iterator<Integer> indications = signal.getKeys().iterator();
            while (indications.hasNext()) {
                Color color = signal.getSignalIndicationById(indications.next()).getColorIndication();
                if (color.equals(Color.RED) || color.equals(Color.YELLOW)) {
                    allGreen = false;
                }
            }
        }

        return allGreen;
    }
}
