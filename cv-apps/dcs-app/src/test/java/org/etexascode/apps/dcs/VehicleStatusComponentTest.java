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
package org.etexascode.apps.dcs;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.junit.Before;
import org.junit.Test;
import org.powermock.reflect.Whitebox;

/**
 * Tests the vehicle status component class.
 * 
 * @author jrutherford
 */
public class VehicleStatusComponentTest {

    /** The vehicle status component. */
    private VehicleStatusComponent vsc;

    @Before
    public void setup() {
        List<LaneNode> node = new ArrayList<LaneNode>();
        node.add(new LaneNode(10, 10));
        Lane lane = new Lane();
        lane.setLaneGeomList(node);
        lane.setLaneId(17);
        lane.setType(Lane.INBOUND);

        Map<Integer, Lane> lanes = new HashMap<Integer, Lane>();
        lanes.put(17, lane);
        LaneManager laneMan = new LaneManager();
        laneMan.setLanes(lanes);
        laneMan.setElevation(100);
        laneMan.setIntersectionId(3);
        laneMan.setLatitude(24.3);
        laneMan.setLongitude(35.9);

        Vehicle veh = new Vehicle(34, 0.0, 0.0, 0.0, 0.0, 0.0);
        veh.setLaneID(17);
        veh.setSpeed(30.5);
        VehicleManager vehMan = new VehicleManager();
        vehMan.addVehicle(veh);

        vsc = new VehicleStatusComponent(laneMan, vehMan, new int[] { 17, 20 });
    }

    @Test
    public void testConstructor() {
        LaneManager lm = Whitebox.getInternalState(vsc, "laneManager");
        VehicleManager vm = Whitebox.getInternalState(vsc, "vehicleManager");

        assertTrue(lm.getLaneIds().size() == 1);
        assertTrue(vm.getAllVehicleIds().size() == 1);
    }

    @Test
    public void testUpdateManagers() {
        Map<Integer, Lane> lanes = new HashMap<Integer, Lane>();
        lanes.put(1, new Lane());
        lanes.put(2, new Lane());
        LaneManager laneMan = new LaneManager();
        laneMan.setLanes(lanes);
        laneMan.setElevation(100);
        laneMan.setIntersectionId(3);
        laneMan.setLatitude(24.3);
        laneMan.setLongitude(35.9);

        Vehicle veh1 = new Vehicle(34, 0.0, 0.0, 0.0, 0.0, 0.0);
        veh1.setLaneID(17);
        Vehicle veh2 = new Vehicle(35, 0.0, 0.0, 0.0, 0.0, 0.0);
        veh2.setLaneID(20);
        VehicleManager vehMan = new VehicleManager();
        vehMan.addVehicle(veh1);
        vehMan.addVehicle(veh2);

        InterRepInfoModel irim = new InterRepInfoModel(laneMan, vehMan, null, null, new ReferencePoint[] {}, 30.5, 0.5);
        vsc.updateManagers(irim);

        assertTrue(irim.getLmi().getLaneIds().size() == 2);
        assertTrue(irim.getVmi().getAllVehicleIds().size() == 2);
    }

    @Test
    public void testGetDilemmaZoneMatrix() {
        assertTrue(vsc.getDilemmaZoneMatrix().size() == 0);
    }

    @Test
    public void testPerformDetection() {
        vsc.performDetection(300.5);
        assertTrue(vsc.getDilemmaZoneMatrix().size() == 1);
    }
}
