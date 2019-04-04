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

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.etexascode.apps.MapDataUtil;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.j2735.MapData;
import org.etexascode.test.GenLaneFunctions;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class MapDataUtilsTest {

    static LaneManager newLaneManager;

    List<Lane> laneVectorFromMD = null;

    static Map<Integer, Lane> laneMap;

    List<Lane> expectedLaneVector = new LinkedList<Lane>();

    @BeforeClass
    public static void setup() throws JAXBException {

        newLaneManager = GenLaneFunctions.genLaneManager();
        int i = 0;
        for (Lane l : newLaneManager.getIterable()) {

            l.setType(Lane.INBOUND);
            if (l.getApproachId() == 0) {
                l.setApproachId(1);
            }
        }
        laneMap = newLaneManager.getLanes();
    }

    @Before
    public void setUp() throws Exception {
        MapData middle = MapDataProducerApp.createFormattedMapDataMessage(newLaneManager, (short)0, null, newLaneManager.getLatitude(), newLaneManager.getLongitude());
        laneVectorFromMD = MapDataUtil.getLaneVectorsFromMapData(middle);

        for (Integer l : laneMap.keySet()) {
            expectedLaneVector.add(laneMap.get(l));
        }
    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testGetLaneVectorsFromMapData() {
        assertEquals(laneVectorFromMD.size(), expectedLaneVector.size());
        for (int i = 0; i < laneVectorFromMD.size(); i++) {
            assertTrue(laneVectorFromMD.contains(expectedLaneVector.get(i)));
        }
    }
}
