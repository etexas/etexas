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
package org.etexascode.apps.hybrid.defaultimplementation;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.j2735.Approach;
import org.etexascode.j2735.Approach.DrivingLanes;
import org.etexascode.j2735.ApproachObject;
import org.etexascode.j2735.DataParameters;
import org.etexascode.j2735.Intersection;
import org.etexascode.j2735.Intersection.Approaches;
import org.etexascode.j2735.MapData;
import org.etexascode.j2735.MapData.Intersections;
import org.etexascode.j2735.Position3D;
import org.etexascode.j2735.VehicleReferenceLane;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SimpleMapParserTest {

    SimpleMapParser smp = null;

    LaneManager emptylm = null;

    MapData map = null;

    @Before
    public void setup() {
        DataParameters dp = new DataParameters();

        VehicleReferenceLane vrl = new VehicleReferenceLane();
        vrl.setLaneNumber(new byte[] { 1 });
        vrl.getNodeList().add("aaaaaaaaaaa");

        DrivingLanes dl = new DrivingLanes();
        dl.getVehicleReferenceLane().add(vrl);

        Approach approach = new Approach();
        approach.setName("name");
        approach.setDrivingLanes(dl);
        approach.setId((short)1);

        ApproachObject ao = new ApproachObject();
        ao.setLaneWidth(10);
        ao.setApproach(approach);
        Approaches approaches = new Approaches();
        approaches.getApproachObject().add(ao);

        Position3D pos = new Position3D();
        pos.setLat(100);
        pos.setLong(200);
        Intersection intersect = new Intersection();
        intersect.setApproaches(approaches);
        intersect.setId(new byte[] { 0, 1 });
        intersect.setRefPoint(pos);
        Intersections is = new Intersections();
        is.getIntersection().add(intersect);

        map = new MapData();
        map.setCrc(new byte[] { 0, 0 });
        map.setDataParameters(dp);
        map.setIntersections(is);
        map.setLayerID((short)1);
        map.setMsgCnt((short)1);
        map.setMsgID("1");
        map.setName("name");

        smp = new SimpleMapParser();
        emptylm = new LaneManager();
    }

    @After
    public void teardown() {
        smp = null;
        emptylm = null;
    }

    @Test
    public void testParseLaneManager1() {
        LaneManager lm = smp.parseLaneManager(null, emptylm);
        assertTrue(lm == emptylm);
    }

    @Test
    public void testParseLaneManager2() {
        assertNull(smp.parseLaneManager(new Object[] {}, null));
    }

    @Test
    public void testParseLaneManager3() {
        Object mes[] = { map };
        LaneManager lm = smp.parseLaneManager(mes, null);
        assertNotNull(lm);
        assertNotNull(lm.getLaneById(1));
    }

    @Test
    public void testGetAppropriateMapMessage1() {
        assertNull(smp.getAppropriateMapMessage(new ArrayList<MapData>()));
    }

    @Test
    public void testGetAppropriateMapMessage2() {
        List<MapData> list = new ArrayList<MapData>();
        list.add(new MapData());
        assertNotNull(smp.getAppropriateMapMessage(list));
    }
}
