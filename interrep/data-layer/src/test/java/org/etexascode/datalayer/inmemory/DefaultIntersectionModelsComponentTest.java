/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.datalayer.inmemory;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.etexascode.datalayer.interfaces.IIntersectionModelsComponent;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for the default intersection models component.
 * 
 * @author emyers
 */
public class DefaultIntersectionModelsComponentTest {

    /** The intersection models component. */
    private IIntersectionModelsComponent modelsComponent;

    /**
     * Creates a new default intersection models component before each test.
     */
    @Before
    public void init() {

        modelsComponent = new DefaultIntersectionModelsComponent();
    }

    /**
     * Tests the <code>getDetectors</code> method in the default intersection models component.
     */
    @Test
    public void testGetDetectors() {

        // add a new information model with a specific detector manager
        DetectorManager detectorManager = new DetectorManager();
        InterRepInfoModel model = new InterRepInfoModel(new LaneManager(), new VehicleManager(), new SignalManager(), detectorManager, new ReferencePoint[0], 0.0, 0.5);
        modelsComponent.putInterRepInfoModel(0, 0, model);

        // confirm the the detector manager was added
        assertTrue(modelsComponent.getDetectors(0, 0).equals(detectorManager));

        // confirm that a detector manager is returned for an invalid intersection ID
        assertNotNull(modelsComponent.getDetectors(0, Integer.MIN_VALUE));

        // confirm that a detector manager is returned when no detector manager exists
        model = new InterRepInfoModel(new LaneManager(), new VehicleManager(), new SignalManager(), null, new ReferencePoint[0], 0.0, 0.5);
        modelsComponent.putInterRepInfoModel(0, 0, model);
        assertNotNull(modelsComponent.getDetectors(0, 0));
    }

    /**
     * Tests the <code>getLaneInfoByVehicle</code> method in the default intersection models
     * component.
     */
    @Test
    public void testGetLaneInfoByVehicle() {

        // add a new information model with a specific lane
        Lane lane = new Lane();
        lane.setLaneId(1);
        Map<Integer, Lane> laneMap = new HashMap<Integer, Lane>();
        laneMap.put(1, lane);
        LaneManager laneManager = new LaneManager();
        laneManager.setLanes(laneMap);
        InterRepInfoModel model = new InterRepInfoModel(laneManager, new VehicleManager(), new SignalManager(), new DetectorManager(), new ReferencePoint[0], 0.0, 0.5);
        modelsComponent.putInterRepInfoModel(0, 0, model);

        // confirm that the lane can be retrieved with a vehicle in the lane
        Vehicle vehicle = new Vehicle(1, 100.0, 50.0, 0.0, 0.0, 0.0);
        vehicle.setLaneID(lane.getLaneId());
        assertTrue(modelsComponent.getLaneInfoByVehicle(0, vehicle).equals(lane));

        // confirm that the lane is not retrieved with an invalid vehicle
        vehicle.setLaneID(Integer.MIN_VALUE);
        assertNull(modelsComponent.getLaneInfoByVehicle(0, vehicle));

    }

    /**
     * Tests the <code>getSignalsByLane</code> method in the default intersection models component.
     */
    @Test
    public void testGetSignalsByLane() {

        // add a new information model with a specific signal
        Lane lane = new Lane();
        lane.setLaneId(1);
        SignalIndication signal = new SignalIndication();
        signal.setLaneId(lane.getLaneId());
        SignalManager signalManager = new SignalManager();
        signalManager.addSignal(signal);
        InterRepInfoModel model = new InterRepInfoModel(new LaneManager(), new VehicleManager(), signalManager, new DetectorManager(), new ReferencePoint[0], 0.0, 0.5);
        modelsComponent.putInterRepInfoModel(0, 0, model);

        // confirm that the signal can be retrieved with the corresponding lane
        assertTrue(modelsComponent.getSignalsByLane(0, lane).contains(signal));

        // confirm that the signal is not retrieved with an invalid lane
        lane.setLaneId(Integer.MIN_VALUE);
        assertNull(modelsComponent.getSignalsByLane(0, lane));
    }

    /**
     * Tests the <code>getVehicleInfoById</code> method in the default intersection models
     * component.
     */
    @Test
    public void testGetVehicleInfoById() {

        // add a new information model with a specific vehicle
        Vehicle vehicle = new Vehicle(1, 100.0, 50.0, 0.0, 0.0, 0.0);
        VehicleManager vehicleManager = new VehicleManager();
        vehicleManager.addVehicle(vehicle);
        InterRepInfoModel model = new InterRepInfoModel(new LaneManager(), vehicleManager, new SignalManager(), new DetectorManager(), new ReferencePoint[0], 0.0, 0.5);
        modelsComponent.putInterRepInfoModel(0, 0, model);

        // confirm that the vehicle can be retrieved
        assertTrue(modelsComponent.getVehicleInfoById(0, vehicle.getProperId()).equals(vehicle));

        // confirm that the vehicle is not retrieved with an invalid ID
        assertNull(modelsComponent.getVehicleInfoById(0, ""));
    }

    /**
     * Tests the <code>getVehicles</code> method in the default intersection models component.
     */
    @Test
    public void testGetVehicles() {

        // add a new information model with a specific vehicle manager
        VehicleManager vehicleManager = new VehicleManager();
        InterRepInfoModel model = new InterRepInfoModel(new LaneManager(), vehicleManager, new SignalManager(), new DetectorManager(), new ReferencePoint[0], 0.0, 0.5);
        modelsComponent.putInterRepInfoModel(0, 0, model);

        // confirm the the vehicle manager was added
        assertTrue(modelsComponent.getVehicles(0, 0).equals(vehicleManager));

        // confirm that a vehicle manager is returned for an invalid intersection ID
        assertNotNull(modelsComponent.getVehicles(0, Integer.MIN_VALUE));
    }

    /**
     * Tests the <code>putInterRepInfoModel</code> method in the default intersection models
     * component.
     */
    @Test
    public void testPutInterRepInfoModel() {

        // add a new information model
        InterRepInfoModel model = new InterRepInfoModel(new LaneManager(), new VehicleManager(), new SignalManager(), new DetectorManager(), new ReferencePoint[0], 0.0, 0.5);
        modelsComponent.putInterRepInfoModel(0, 0, model);

        // confirm that the information model as added
        assertTrue(modelsComponent.getInfoModel(0, 0).equals(model));
    }
}
