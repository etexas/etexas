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

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.api.SimproJNA.SIMPRO_VehicleData;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.Vehicle.VEHICLE_TYPE;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

/**
 * @author dgolman
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({ UtilsStepData.class, SimproInterface.class })
public class TestUtilsStepData {

    private ModelData modelData;

    private SIMPRO_VehicleData data;

    private SignalDataRetriever signalDataRetriever;

    private List<Vehicle> expectedVehicles;

    private List<SignalIndication> expectedSignals = new ArrayList<SignalIndication>();

    private MapDataRetriever mapData;

    private SignalIndication[] expectedSignal0 = new SignalIndication[1];

    private SignalIndication[] expectedSignal1 = new SignalIndication[1];

    private SignalIndication[] expectedSignal2 = new SignalIndication[1];

    private SignalIndication[] expectedSignal3 = new SignalIndication[1];

    private SignalIndication[] expectedSignal4 = new SignalIndication[1];

    private SignalIndication[] expectedSignal5 = new SignalIndication[2];

    private SignalIndication[] expectedSignal6 = new SignalIndication[2];

    private SignalIndication[] expectedSignal7 = new SignalIndication[2];

    private SignalIndication[] expectedSignal8 = new SignalIndication[2];

    private SignalIndication[] expectedSignal9 = new SignalIndication[2];

    private SignalIndication[] expectedSignal10 = new SignalIndication[2];

    private SignalIndication[] expectedSignal11 = new SignalIndication[2];

    private SignalIndication[] expectedSignal12 = new SignalIndication[2];

    private SignalIndication[] expectedSignal13 = new SignalIndication[2];

    private SignalIndication[] expectedSignal14 = new SignalIndication[2];

    private SignalIndication[] expectedSignal15 = new SignalIndication[2];

    private SignalIndication[] expectedSignal16 = new SignalIndication[2];

    private SignalIndication[] expectedSignal17 = new SignalIndication[2];

    private SignalIndication[] expectedSignal18 = new SignalIndication[2];

    private SignalIndication[] expectedSignal19 = new SignalIndication[2];

    private SignalIndication[] expectedSignal20 = new SignalIndication[2];

    private SignalIndication[] expectedSignal21 = new SignalIndication[2];

    private SignalIndication[] expectedSignal22 = new SignalIndication[2];

    private SignalIndication[] expectedSignal23 = new SignalIndication[2];

    private SignalIndication[] expectedSignal24 = new SignalIndication[2];

    private SignalIndication[] expectedSignal25 = new SignalIndication[2];

    @Before
    public void setUp() throws Exception {
        modelData = PowerMockito.mock(ModelData.class);
        data = PowerMockito.mock(SIMPRO_VehicleData.class);
        signalDataRetriever = PowerMockito.mock(SignalDataRetriever.class);

        // Mock the construction of a signalDataRetriever.
        PowerMockito.whenNew(SignalDataRetriever.class).withNoArguments().thenReturn(signalDataRetriever);

        expectedVehicles = new ArrayList<Vehicle>();
        data.INUSE = new int[] { 1, 1, 1, 1, 1 };// ,1,1,1,1,1,1,0};
        data.IQ = new int[] { 1, 2, 3, 4, 0, 6, 7 };
        data.LEN = new double[] { 10.1, 20.2, 30.3, 40.4, 1.0 };// ,50.5,60.6,70.7,80.8,90.9,
                                                                // 100.0};
        data.PWID = new double[] { 2.2, 3.3, 4.4, 5.5, 1.0 }; // 6.6,7.7,8.8,9.9,10.10,11.11};
        data.VEL = new double[] { 2.4, 6.5, 3.9, 4.2, 1.0 };
        data.ACC = new double[] { 12.3, 4.5, 3.6, 4.2, 0.0 };
        data.FBX = new double[] { 1, 2, 3, 4, 5 };
        data.FBY = new double[] { 3, 2, 1, 0, -1 };
        data.PHEAD = new double[] { 6.5, 4.2, 10.4, 42.0, 0.0 };
        data.LANE = new int[] { 1, 2, 3, 4, -1 };
        data.PHEIGHT = new int[] { 100, 90, 80, 70, 60 };
        data.PTYPE = new int[] { 0, 1, 2, 0, 3 };

        modelData.centerX = 0;
        modelData.centerY = 0;

        generateExpectedVehicle(0);
        generateExpectedVehicle(1);
        generateExpectedVehicle(2);
        generateExpectedVehicle(4);

        generateExpectedSignals();
    }

    @After
    public void tearDown() {}

    @Test
    public void testPopulateVehicles() {
        // Mock the SIMPRO Interface to do nothing.
        PowerMockito.mockStatic(SimproInterface.class);
        PowerMockito.doNothing().when(SimproInterface.class);

        List<Vehicle> actualVehicles = UtilsStepData.populateVehicles(modelData, data);
        assertEquals(expectedVehicles, actualVehicles);
    }

    @Test
    public void testPopulateSignals() throws Exception {

        mapData = PowerMockito.mock(MapDataRetriever.class);

        PowerMockito.when(modelData.getMapData()).thenReturn(mapData);
        PowerMockito.when(mapData.getNRLAN()).thenReturn(3);
        PowerMockito.when(mapData.getIBLN(1)).thenReturn(1);
        PowerMockito.when(mapData.getIBLN(2)).thenReturn(2);
        PowerMockito.when(mapData.getIBLN(5)).thenReturn(0);

        SignalIndication si = new SignalIndication();
        si.setLaneId(1);
        si.setColorIndication(SignalIndication.Color.NONE);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.UNKNOWN);
        si.setTimeToChange(0.0);
        expectedSignals.add(si);
        si = new SignalIndication();
        si.setLaneId(2);
        si.setColorIndication(SignalIndication.Color.NONE);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.UNKNOWN);
        si.setTimeToChange(0.0);
        expectedSignals.add(si);

        List<SignalIndication> actualSignals = UtilsStepData.populateSignals(modelData);

        assertEquals(expectedSignals, actualSignals);
    }

    @Test
    public void testPopulateDetectors() {
        // Placeholder for when this method gets implemented.
        StepData stepData = PowerMockito.mock(StepData.class);

        UtilsStepData.populateDetectors(stepData);

        PowerMockito.verifyNoMoreInteractions(stepData);
    }

    @Test
    public void testGetTimeToChange() {
        double expected = 42.0;

        PowerMockito.when(signalDataRetriever.getTR()).thenReturn(1.1);
        PowerMockito.when(signalDataRetriever.getICAMPC()).thenReturn(1);
        PowerMockito.when(signalDataRetriever.getISISET(1, 0)).thenReturn(1);
        PowerMockito.when(signalDataRetriever.getISISET(2, 0)).thenReturn(1);
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(0);
        PowerMockito.when(signalDataRetriever.getNCAMSP()).thenReturn(3);
        PowerMockito.when(signalDataRetriever.getTCAMSP(2)).thenReturn(40.9);

        assertEquals(expected, UtilsStepData.getTimeToChange(signalDataRetriever, 0), .001);
    }

    @Test
    public void testGetSignalsFromLight0() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(0);
        assertEquals(expectedSignal0[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
    }

    @Test
    public void testGetSignalsFromLight1() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(1);
        assertEquals(expectedSignal1[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
    }

    @Test
    public void testGetSignalsFromLight2() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(2);
        assertEquals(expectedSignal2[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
    }

    @Test
    public void testGetSignalsFromLight3() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(3);
        assertEquals(expectedSignal3[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
    }

    @Test
    public void testGetSignalsFromLight4() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(4);
        assertEquals(expectedSignal4[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
    }

    @Test
    public void testGetSignalsFromLight5() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(5);
        assertEquals(expectedSignal5[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal5[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight6() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(6);
        assertEquals(expectedSignal6[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal6[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight7() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(7);
        assertEquals(expectedSignal7[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal7[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight8() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(8);
        assertEquals(expectedSignal8[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal8[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight9() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(9);
        assertEquals(expectedSignal9[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal9[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight10() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(10);
        assertEquals(expectedSignal10[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal10[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight11() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(11);
        assertEquals(expectedSignal11[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal11[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight12() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(12);
        assertEquals(expectedSignal12[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal12[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight13() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(13);
        assertEquals(expectedSignal13[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal13[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight14() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(14);
        assertEquals(expectedSignal14[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal14[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight15() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(15);
        assertEquals(expectedSignal15[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal15[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight16() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(16);
        assertEquals(expectedSignal16[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal16[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight17() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(17);
        assertEquals(expectedSignal17[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal17[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight18() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(18);
        assertEquals(expectedSignal18[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal18[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight19() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(19);
        assertEquals(expectedSignal19[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal19[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight20() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(20);
        assertEquals(expectedSignal20[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal20[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight21() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(21);
        assertEquals(expectedSignal21[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal21[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight22() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(22);
        assertEquals(expectedSignal22[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal22[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight23() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(23);
        assertEquals(expectedSignal23[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal23[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight24() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(24);
        assertEquals(expectedSignal24[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal24[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test
    public void testGetSignalsFromLight25() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(25);
        assertEquals(expectedSignal25[0], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[0]);
        assertEquals(expectedSignal25[1], UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0)[1]);
    }

    @Test(expected = AssertionError.class)
    public void testGetSignalsFromLight26() {
        PowerMockito.when(signalDataRetriever.getISISET(0, 0)).thenReturn(26);
        UtilsStepData.getSignalsFromLight(signalDataRetriever, 0, 0);
    }

    public void generateExpectedVehicle(int index) {
        Vehicle expectedVehicle = new Vehicle((data.IQ[index + 1]), (UtilsUnitConversion.convertFeetToCentimeters(data.LEN[index])), (UtilsUnitConversion.convertFeetToCentimeters(data.PWID[index])),
                (UtilsUnitConversion.convertFeetToCentimeters(data.FBX[index] - modelData.centerX)), (UtilsUnitConversion.convertFeetToCentimeters(data.FBY[index] - modelData.centerY)), 0.0);

        expectedVehicle.setSpeed(UtilsUnitConversion.convertFeetToMeters(data.VEL[index]));
        expectedVehicle.setAcceleration(UtilsUnitConversion.convertFeetToCentimeters(data.ACC[index]));
        expectedVehicle.setHeading(data.PHEAD[index]);
        expectedVehicle.setLaneID(data.LANE[index]);
        expectedVehicle.setHeight(UtilsUnitConversion.convertFeetToCentimeters(data.PHEIGHT[index]));
        if (data.PTYPE[index] == 0) {
            expectedVehicle.setType(VEHICLE_TYPE.CAR);
        }
        else if (data.PTYPE[index] == 1) {
            expectedVehicle.setType(VEHICLE_TYPE.BUS);
        }
        else if (data.PTYPE[index] == 2) {
            expectedVehicle.setType(VEHICLE_TYPE.TRACTOR_TRAILER);
        }
        expectedVehicle.setBrakePressed(false); // THIS NEEDS TO BE RETRIEVED.
        expectedVehicles.add(expectedVehicle);

    }

    public void generateExpectedSignals() {

        SignalIndication si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.NONE);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.UNKNOWN);
        expectedSignal0[0] = si;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal1[0] = si;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal2[0] = si;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.RED);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal3[0] = si;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal4[0] = si;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);

        SignalIndication si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.YELLOW);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal5[0] = si;
        expectedSignal5[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal6[0] = si;
        expectedSignal6[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.YELLOW);
        si2.setStateIndication(SignalIndication.State.FLASHING);
        si2.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        expectedSignal7[0] = si;
        expectedSignal7[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.RED);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.YELLOW);
        si2.setStateIndication(SignalIndication.State.FLASHING);
        si2.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        expectedSignal8[0] = si;
        expectedSignal8[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.RED);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.GREEN);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal9[0] = si;
        expectedSignal9[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.RED);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.YELLOW);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal10[0] = si;
        expectedSignal10[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.YELLOW);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal11[0] = si;
        expectedSignal11[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal12[0] = si;
        expectedSignal12[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.GREEN);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal13[0] = si;
        expectedSignal13[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.RED);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.YELLOW);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        expectedSignal14[0] = si;
        expectedSignal14[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        expectedSignal15[0] = si;
        expectedSignal15[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        expectedSignal16[0] = si;
        expectedSignal16[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.YELLOW);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal17[0] = si;
        expectedSignal17[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal18[0] = si;
        expectedSignal18[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.GREEN);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal19[0] = si;
        expectedSignal19[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal20[0] = si;
        expectedSignal20[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.RED);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.GREEN);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal21[0] = si;
        expectedSignal21[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.RED);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.YELLOW);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal22[0] = si;
        expectedSignal22[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.GREEN);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal23[0] = si;
        expectedSignal23[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.YELLOW);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal24[0] = si;
        expectedSignal24[1] = si2;

        si = new SignalIndication();
        si.setLaneId(0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si2 = new SignalIndication();
        si2.setLaneId(0);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal25[0] = si;
        expectedSignal25[1] = si2;
    }
}
