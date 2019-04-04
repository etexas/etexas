/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.test;

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorEvent;
import org.etexascode.interrep.datamodel.DetectorManager;

public class GenDetectorFunctions {

    public static DetectorEvent genDetEventTrue(int detectorId, double length) {
        DetectorEvent ret = new DetectorEvent();

        ret.setDetectorId(detectorId);
        ret.setPresence(true);
        ret.setPulse(1);
        ret.setLength(length);

        return ret;
    }

    public static Detector genDetector(int laneId, int x, int y, int detId) {
        Detector ret = new Detector();

        Polygon a = new Polygon();

        a.addPoint(x, y);
        a.addPoint(x + 50, y);
        a.addPoint(x + 50, y + 50);
        a.addPoint(x, y + 50);

        ret.setArea(a);

        List<Integer> l = new ArrayList<Integer>(1);
        l.add(laneId);
        ret.setLaneIDs(l);

        DetectorEvent de = genDetEventTrue(detId, 0);
        de.setPresence(false);
        de.setPulse(0);
        de.setSpeed(0);

        ret.setDetEvent(de);

        ret.setDetectorID(detId);

        return ret;
    }

    public static DetectorManager genDetManager() {
        DetectorManager ret = new DetectorManager();

        ret.addDetector(1, genDetector(1, -25, 1800, 1));
        ret.addDetector(2, genDetector(1, -25, 1875, 2));
        ret.addDetector(5, genDetector(5, 1725, 1725, 3));

        return ret;
    }
}
