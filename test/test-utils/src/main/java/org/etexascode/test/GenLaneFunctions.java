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

import java.util.Hashtable;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;

public class GenLaneFunctions {

    public static LaneManager genLaneManager() {
        LaneManager ret = new LaneManager();
        Hashtable<Integer, Lane> lanes = new Hashtable<Integer, Lane>();

        lanes.put(1, genLane(0, 1));
        lanes.put(2, genLane(1, 0));
        lanes.put(3, genLane(0, -1));
        lanes.put(4, genLane(-1, 0));
        lanes.put(5, genLane(1, 1));
        lanes.put(6, genLane(-1, 1));
        lanes.put(7, genLane(-1, -1));
        lanes.put(8, genLane(1, -1));

        ret.setLanes(lanes);
        return ret;
    }

    public static Lane genLane(int xDirect, int yDirect) {
        Lane ret = new Lane();

        for (int i = 0; i < 100; i++) {
            LaneNode ln = new LaneNode();

            ln.setWidth(500);
            ln.setX((1500 * xDirect) + (20 * i * xDirect));
            ln.setY((1500 * yDirect) + (20 * i * yDirect));

            ret.addLaneNode(ln);
        }

        return ret;
    }

    public static LaneNode genLaneNode(double x, double y, double width) {
        LaneNode ret = new LaneNode();

        ret.setX(x);
        ret.setY(y);
        ret.setWidth(width);

        return ret;
    }
}
