/**********************************************************************
 *** *                                                            * ***
 *** *  Copyright (c) 2011 Harmonia Holdings Group LLC            * ***
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
package org.etexascode.simulation.vehiclewriter;

import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.InterRepException;
import org.etexascode.interrep.model.Detector;
import org.etexascode.interrep.model.Vehicle;
import org.etexascode.j2735.BasicSafetyMessage;

/**
 *
 * @author bbadillo
 */
public class ModelManagerWithDetectorEmulation extends InterRep {

    public final static int LANE_ID = 3;
    public final static int DETECTOR_POS = 750;
    Detector detector;

    public ModelManagerWithDetectorEmulation() throws ClassNotFoundException {
        detector = new Detector(0);
        detector.addLaneID(LANE_ID);
        detector.setPulseDetectCap(true);
        detector.setPulse(0);
        getDetectorManager().addOrSetDetector(detector);
    }

    @Override
    public void update() {
        detector.setPulse(0);
        super.update();
        LinkedList<Detector> detectorList = new LinkedList<Detector>();
        detectorList.add(detector);
        fireDetectorUpdateEvent(detectorList);
    }

    @Override
    protected Vehicle updateBSM(BasicSafetyMessage bsm) throws InterRepException {
        Vehicle vehicle = super.updateBSM(bsm);
        if (vehicle.getLaneID() == LANE_ID) {
            if (vehicle.getDistToStopLine() < DETECTOR_POS) {
                Deque<Vehicle> completeVehicleInfo = getVehicleManager().getCompleteVehicleInfo(vehicle.getId());
                Iterator<Vehicle> iterator = completeVehicleInfo.iterator();
                if (iterator.hasNext()) {
                    iterator.next();
                    if (iterator.hasNext()) {
                        Vehicle pastPos = iterator.next();
                        if (pastPos.getDistToStopLine() >= DETECTOR_POS) {
                            int pulse = detector.getPulse();
                            detector.setPulse(pulse + 1);
                        }
                    }
                }
            }
        }
        return vehicle;
    }
}
