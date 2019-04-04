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
package com.harmonia.apps;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.etexascode.apps.MapDataUtil;
import org.etexascode.apps.UtilsMessageImports;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.j2735.MapData;
import org.etexascode.test.GenLaneFunctions;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt amauldon
 */
public class CircularMapTest {

    LaneManager newLaneManager = GenLaneFunctions.genLaneManager();

    List<Lane> start = null;

    List<Lane> finish = null;

    @Before
    public void setUp() throws Exception {
        for (Lane l : newLaneManager.getIterable()) {
            // checks to ensure we don't get any IndexOutOfBounds errors
            if (l.getApproachId() == 0) {
                l.setApproachId(1);
            }
        }

        MapData middle = MapDataProducerApp.createFormattedMapDataMessage(newLaneManager, (short)0, null, newLaneManager.getLatitude(), newLaneManager.getLongitude());
        start = MapDataUtil.getLaneVectorsFromMapData(middle);
        finish = UtilsMessageImports.importLanes(middle);
    }

    @Test
    public void testMapCircular() {
        for (Lane l : finish) {
            assertTrue(start.contains(l));
        }

        for (Lane l : start) {
            assertTrue(finish.contains(l));
        }

        assertEquals(start.size(), finish.size());
    }

}
