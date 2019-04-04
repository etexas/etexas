/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** *  COPYRIGHT (C) 2003 by The University of Texas at Austin   * ** *
 * * ** *                                                            * ** *
 * * ** * Permission is hereby granted to use, modify, copy, and     * ** *
 * * ** * distribute this software and its documentation for any     * ** *
 * * ** * purpose only without profit, provided that the above       * ** *
 * * ** * Copyright Notice appears in all copies and that both the   * ** *
 * * ** * Copyright Notice and this Permission Notice appears in     * ** *
 * * ** * every copy of supporting documentation.  No title to nor   * ** *
 * * ** * ownership of the software is transferred hereby.  The name * ** *
 * * ** * of The University of Texas at Austin shall not be used in  * ** *
 * * ** * advertising or publicity related to the distribution of    * ** *
 * * ** * the software without specific, written, prior permission.  * ** *
 * * ** * This software is provided as-delivered without expressed   * ** *
 * * ** * or implied warranty.  The University of Texas at Austin    * ** *
 * * ** * makes no representation about the suitability of this      * ** *
 * * ** * software for any purpose and accepts no responsibility for * ** *
 * * ** * its use.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** * This program is free software; you can redistribute it     * ** *
 * * ** * and/or modify it under the terms of the GNU General Public * ** *
 * * ** * License as published by the Free Software Foundation;      * ** *
 * * ** * either version 2 of the License, or (at your option) any   * ** *
 * * ** * later version.                                             * ** *
 * * ** *                                                            * ** *
 * * ** * This program is distributed in the hope that it will be    * ** *
 * * ** * useful, but WITHOUT ANY WARRANTY; without even the implied * ** *
 * * ** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ** *
 * * ** * PURPOSE.  See the GNU General Public License for more      * ** *
 * * ** * details.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** * You should have received a copy of the GNU General Public  * ** *
 * * ** * License along with this program; if not, write to the Free * ** *
 * * ** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ** *
 * * ** * Floor, Boston, MA 02110-1301, USA.                         * ** *
 * * ** *                                                            * ** *
 * * ** * For more information: http://www.gnu.org/licenses/gpl.html * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * #L%
 */
package org.etexascode.api;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.SimMetaData;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleDestinationCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.VehicleLaneChangeCommand;
import org.etexascode.interrep.datamodel.VehicleSpeedCommand;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.powermock.reflect.Whitebox;

/**
 * Tests the eTEXAS class.
 * 
 * @author emyers
 */
public class TestETEXAS {

    /** The flag indicating whether mocks have been initialized. */
    private boolean isInitialized;

    /** A mocked eTEXAS object. */
    private eTEXAS etexasMock;

    /** A mocked data model. */
    private ModelData modelDataMock;

    /** Mocked static data. */
    private StaticData staticDataMock;

    /** Mocked step data. */
    private StepData stepDataMock;

    /** A mocked generic vehicle command. */
    private VehicleCommand commandMock;

    /** A mocked vehicle destination command. */
    private VehicleDestinationCommand destinationCommandMock;

    /** A mocked vehicle land change command. */
    private VehicleLaneChangeCommand laneChangeCommandMock;

    /** A mocked vehicle speed command. */
    private VehicleSpeedCommand speedCommandMock;

    /** A mocked vehicle injection request. */
    private VehicleInjectionRequest injectionRequestMock;

    /**
     * Creates the mocks used for unit testing.
     */
    private void initializeMocks() {

        modelDataMock = Mockito.mock(ModelData.class);
        Whitebox.setInternalState(modelDataMock, "maxDT", 0L);

        Map<Integer, Lane> laneMap = new HashMap<Integer, Lane>();
        laneMap.put(1, new Lane());
        laneMap.put(2, new Lane());
        laneMap.get(1).setApproachId(1);
        laneMap.get(2).setApproachId(2);
        LaneManager laneManager = new LaneManager();
        laneManager.setLanes(laneMap);
        staticDataMock = Mockito.mock(StaticData.class);
        Mockito.when(staticDataMock.getLaneManager()).thenReturn(laneManager);
        Mockito.when(staticDataMock.getMetaData()).thenReturn(new SimMetaData());

        ArrayList<Vehicle> vehicles = new ArrayList<Vehicle>();
        vehicles.add(new Vehicle(1, 0.0, 0.0, 0.0, 0.0, 0.0));
        vehicles.get(0).setLaneID(1);
        stepDataMock = Mockito.mock(StepData.class);
        Mockito.when(stepDataMock.getVehicles()).thenReturn(vehicles);

        commandMock = Mockito.mock(VehicleCommand.class);
        Mockito.when(commandMock.getVehicleID()).thenReturn(1);
        destinationCommandMock = Mockito.mock(VehicleDestinationCommand.class);
        Mockito.when(destinationCommandMock.getVehicleID()).thenReturn(1);
        laneChangeCommandMock = Mockito.mock(VehicleLaneChangeCommand.class);
        Mockito.when(laneChangeCommandMock.getVehicleID()).thenReturn(1);
        speedCommandMock = Mockito.mock(VehicleSpeedCommand.class);
        Mockito.when(speedCommandMock.getVehicleID()).thenReturn(1);

        injectionRequestMock = Mockito.mock(VehicleInjectionRequest.class);

        isInitialized = true;
    }

    /**
     * Resets the eTEXAS mock before each test case.
     */
    @Before
    public void initializeTest() {

        if (!isInitialized) {

            initializeMocks();
        }

        etexasMock = Mockito.mock(eTEXAS.class);
        Mockito.when(etexasMock.getStaticData()).thenReturn(staticDataMock);
        Mockito.doCallRealMethod().when(etexasMock).addVehicleCommand(Mockito.any(VehicleCommand.class));
        Mockito.when(etexasMock.getVehicleCommands()).thenCallRealMethod();
        Mockito.doCallRealMethod().when(etexasMock).addVehicleInjectionRequest(Mockito.any(VehicleInjectionRequest.class));
        Whitebox.setInternalState(etexasMock, "modelData", modelDataMock);
    }

    /**
     * Tests the <code>addVehicleCommand(VehicleCommand)</code> method when no existing vehicle
     * commands are present.
     */
    @Test
    public void testAddVehicleCommand() {

        etexasMock.addVehicleCommand(commandMock);
        Assert.assertEquals(1, etexasMock.getVehicleCommands().size());
    }

    /**
     * Tests the <code>addVehicleCommand(VehicleCommand)</code> method when an existing vehicle
     * command of a different type is present.
     */
    @Test
    public void testAddVehicleCommand2() {

        etexasMock.addVehicleCommand(speedCommandMock);
        etexasMock.addVehicleCommand(laneChangeCommandMock);
        Assert.assertEquals(2, etexasMock.getVehicleCommands().size());
    }

    /**
     * Tests the <code>addVehicleCommand(VehicleCommand)</code> method when an existing vehicle
     * command for a different vehicle is present.
     */
    @Test
    public void testAddVehicleCommand3() {

        etexasMock.addVehicleCommand(speedCommandMock);
        etexasMock.addVehicleCommand(new VehicleSpeedCommand(2, VehicleSpeedCommand.MAX_ACCELERATE_TO_XX, 40));
        Assert.assertEquals(2, etexasMock.getVehicleCommands().size());
    }

    /**
     * Tests the <code>addVehicleCommand(VehicleCommand)</code> method when an identical vehicle
     * command is present and must be removed.
     */
    @Test
    public void testAddVehicleCommand4() {

        etexasMock.addVehicleCommand(speedCommandMock);
        etexasMock.addVehicleCommand(speedCommandMock);
        Assert.assertEquals(1, etexasMock.getVehicleCommands().size());
    }

    /**
     * Tests the <code>addVehicleInjectionRequest(VehicleInjectionRequest)</code> method when no
     * injection request is already present.
     */
    @Test
    @SuppressWarnings("unchecked")
    public void testAddVehicleInjectionRequest() {

        etexasMock.addVehicleInjectionRequest(injectionRequestMock);
        List<VehicleInjectionRequest> requests = (List<VehicleInjectionRequest>)Whitebox.getInternalState(etexasMock, "vehicleInjectionRequests");

        Assert.assertEquals(1, requests.size());
    }

    /**
     * Tests the <code>addVehicleInjectionRequest(VehicleInjectionRequest)</code> method when an
     * existing injection request is already present.
     */
    @Test
    @SuppressWarnings("unchecked")
    public void testAddVehicleInjectionRequest2() {

        etexasMock.addVehicleInjectionRequest(injectionRequestMock);
        etexasMock.addVehicleInjectionRequest(injectionRequestMock);
        List<VehicleInjectionRequest> requests = (List<VehicleInjectionRequest>)Whitebox.getInternalState(etexasMock, "vehicleInjectionRequests");

        Assert.assertEquals(2, requests.size());
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when no vehicles exist in the
     * step data.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage() throws Exception {

        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", commandMock, new StepData());

        Assert.assertNull(message);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when no vehicle in the step
     * data matches the ID of the vehicle command.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage2() throws Exception {

        VMSMessage message = Whitebox.invokeMethod(etexasMock, "convertVehicleCommandToVMSMessage",
                new VehicleSpeedCommand(2, VehicleSpeedCommand.MAX_ACCELERATE_TO_XX, 40), stepDataMock);

        Assert.assertNull(message);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a vehicle in the step
     * data matches the ID of the vehicle command.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage3() throws Exception {

        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", commandMock, stepDataMock);

        Assert.assertNotNull(message);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a vehicle matches a
     * speed command for maximum acceleration.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage4() throws Exception {

        Mockito.when(speedCommandMock.getSpeedCommand()).thenReturn(VehicleSpeedCommand.MAX_ACCELERATE_TO_XX);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", speedCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.ETEXAS_API_MAX_ACCELERATE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a vehicle matches a
     * speed command for maximum deceleration.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage5() throws Exception {

        Mockito.when(speedCommandMock.getSpeedCommand()).thenReturn(VehicleSpeedCommand.MAX_DECELERATE_TO_XX);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", speedCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.ETEXAS_API_MAX_DECELERATE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a vehicle matches a
     * speed command for normal acceleration.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage6() throws Exception {

        Mockito.when(speedCommandMock.getSpeedCommand()).thenReturn(VehicleSpeedCommand.NORMAL_ACCELERATE_TO_XX);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", speedCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.ETEXAS_API_NORMAL_ACCELERATE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a vehicle matches a
     * speed command for normal deceleration.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage7() throws Exception {

        Mockito.when(speedCommandMock.getSpeedCommand()).thenReturn(VehicleSpeedCommand.NORMAL_DECELERATE_TO_XX);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", speedCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.ETEXAS_API_NORMAL_DECELERATE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a vehicle matches a left
     * lane change command.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage8() throws Exception {

        Mockito.when(laneChangeCommandMock.getLaneCommand()).thenReturn(VehicleLaneChangeCommand.CHANGE_LANE_LEFT);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", laneChangeCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.ETEXAS_API_LEFT_LANE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a vehicle matches a
     * right lane change command.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage9() throws Exception {

        Mockito.when(laneChangeCommandMock.getLaneCommand()).thenReturn(VehicleLaneChangeCommand.CHANGE_LANE_RIGHT);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", laneChangeCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.ETEXAS_API_RIGHT_LANE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a vehicle matches a
     * straight destination command.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage10() throws Exception {

        Mockito.when(destinationCommandMock.getDestCommand()).thenReturn(VehicleDestinationCommand.STAY_STRAIGHT_AT_INTERSECTION);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", destinationCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.NONE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a vehicle matches a turn
     * left destination command.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage11() throws Exception {

        Mockito.when(destinationCommandMock.getDestCommand()).thenReturn(VehicleDestinationCommand.TURN_LEFT_AT_INTERSECTION);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", destinationCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.NONE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a vehicle matches a turn
     * right destination command.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage12() throws Exception {

        Mockito.when(destinationCommandMock.getDestCommand()).thenReturn(VehicleDestinationCommand.TURN_RIGHT_AT_INTERSECTION);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", destinationCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.NONE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a destination command is
     * not valid.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage13() throws Exception {

        Mockito.when(destinationCommandMock.getDestCommand()).thenReturn(Integer.MAX_VALUE);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", destinationCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.NONE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a lane change command is
     * not valid.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage14() throws Exception {

        Mockito.when(laneChangeCommandMock.getLaneCommand()).thenReturn(Integer.MAX_VALUE);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", laneChangeCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.NONE);
    }

    /**
     * Tests the <code>convertVehicleCommandToVMSMessage</code> method when a speed command is not
     * valid.
     * 
     * @throws Exception if an error occurs during object mocking
     */
    @Test
    public void testConvertVehicleCommandToVMSMessage15() throws Exception {

        Mockito.when(speedCommandMock.getSpeedCommand()).thenReturn(Integer.MAX_VALUE);
        VMSMessage message = Whitebox.invokeMethod(
                etexasMock, "convertVehicleCommandToVMSMessage", speedCommandMock, stepDataMock);

        Assert.assertEquals(message.getMessage(), VMSMessage.NONE);
    }
}
