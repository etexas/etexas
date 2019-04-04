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
package org.etexascode.apps;

import static org.junit.Assert.assertEquals;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
public class UtilsMessageImportsTest {

    @Test
    public void testCenterLaneManagerOnReferencePoint() {
        LaneManager lm1 = new LaneManager();
        LaneManager lm2 = new LaneManager();

        Lane l1 = new Lane();
        Lane l2 = new Lane();

        LaneNode ln1 = new LaneNode();
        LaneNode ln2 = new LaneNode();

        LaneNode ln3 = new LaneNode();
        LaneNode ln4 = new LaneNode();

        l1.getLaneGeomList().add(ln1);
        l1.getLaneGeomList().add(ln2);

        l2.getLaneGeomList().add(ln3);
        l2.getLaneGeomList().add(ln4);

        lm2.setLatitude(0.006);
        lm2.setLongitude(0.006);

        double[] offsets = UtilsLatLongConversion.convertLatLongToCentimeterOffset(lm1.getLatitude(), lm1.getLongitude(), lm2.getLatitude(), lm2.getLongitude(), lm1.getGeoCalculatorType());

        ln3.setX(ln1.getX() + offsets[0]);
        ln3.setY(ln1.getY() + offsets[1]);
        ln4.setX(ln2.getX() + offsets[0]);
        ln4.setY(ln2.getY() + offsets[1]);

        UtilsMessageImports.centerLaneManagerOnReferencePoint(lm2, lm1.getLatitude(), lm1.getLongitude(), lm1.getGeoCalculatorType());
        assertEquals(lm1, lm2);
    }

}
