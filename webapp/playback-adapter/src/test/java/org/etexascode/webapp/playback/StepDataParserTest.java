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
package org.etexascode.webapp.playback;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.DetectorEvent;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class StepDataParserTest {

    String inputDirectory = "/playbackTestInput";

    PlaybackSimulator playsim;

    @Rule
    public ExpectedException exception = ExpectedException.none();

    private SignalIndication createSig(int id, SignalIndication.Color color, SignalIndication.Type type, SignalIndication.State state) {
        SignalIndication ret = new SignalIndication();
        ret.setLaneId(id);
        ret.setColorIndication(color);
        ret.setTypeIndication(type);
        ret.setStateIndication(state);
        return ret;
    }

    @Before
    public void setUp() {
        URL url = StepDataParserTest.class.getResource(inputDirectory);
        if (url == null) {
            System.out.println("Null URL.");
        }
        try {
            File input = new File(url.toURI());
            playsim = new PlaybackSimulator(input);
        }
        catch (Exception e) {
            fail("Couldn't create directory File.");
        }
    }

    @Test
    public void testConstructor() {
        new StepDataParser();
    }

    @Test
    public void testGetStepData() {
        StepData actual = playsim.getStepData(7);
        List<Vehicle> actualVehicles = actual.getVehicles();
        List<SignalIndication> actualSignals = actual.getSignalIndication();
        List<DetectorEvent> actualDetectors = actual.getDetectorEvents();

        List<Vehicle> expectedVehicles = new ArrayList<Vehicle>();
        List<SignalIndication> expectedSignals = new ArrayList<SignalIndication>();
        List<DetectorEvent> expectedDetectors = new ArrayList<DetectorEvent>();

        Vehicle veh1 = new Vehicle(9, (UtilsUnitConversion.convertFeetToCentimeters(12.0)), (UtilsUnitConversion.convertFeetToCentimeters(6.6)),
                (UtilsUnitConversion.convertFeetToCentimeters(-41.94)), (UtilsUnitConversion.convertFeetToCentimeters(1194.088)), 0.0);
        veh1.setSpeed(UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(0.0));
        veh1.setLaneID(41);
        veh1.setAcceleration(0.0);
        expectedVehicles.add(veh1);

        DetectorEvent det1 = new DetectorEvent();
        det1.setDetectorId(1);
        det1.setSpeed(UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(1.00));
        det1.setLength(UtilsUnitConversion.convertFeetToCentimeters(1.00));
        det1.setPresence(true);
        expectedDetectors.add(det1);

        expectedSignals.add(createSig(43, SignalIndication.Color.GREEN, SignalIndication.Type.BALL, SignalIndication.State.STEADY));
        expectedSignals.add(createSig(44, SignalIndication.Color.YELLOW, SignalIndication.Type.LEFT_ARROW, SignalIndication.State.STEADY));
        expectedSignals.add(createSig(38, SignalIndication.Color.RED, SignalIndication.Type.RIGHT_ARROW, SignalIndication.State.STEADY));
        expectedSignals.add(createSig(39, SignalIndication.Color.RED, SignalIndication.Type.STRAIGHT_ARROW, SignalIndication.State.STEADY));
        expectedSignals.add(createSig(40, SignalIndication.Color.RED, SignalIndication.Type.UTURN_ARROW, SignalIndication.State.STEADY));
        expectedSignals.add(createSig(25, SignalIndication.Color.NONE, SignalIndication.Type.STOP_SIGN, SignalIndication.State.STEADY));
        expectedSignals.add(createSig(26, SignalIndication.Color.NONE, SignalIndication.Type.YIELD_SIGN, SignalIndication.State.STEADY));

        for (int k = 0; k < actualVehicles.size(); ++k) {
            assertEquals(expectedVehicles.get(k), actualVehicles.get(k));
        }

        for (int k = 0; k < actualDetectors.size(); ++k) {
            assertEquals(expectedDetectors.get(k), actualDetectors.get(k));
        }

        for (int k = 0; k < actualSignals.size(); ++k) {
            assertEquals(expectedSignals.get(k), actualSignals.get(k));
        }

        if (actualVehicles.size() != expectedVehicles.size() || actualDetectors.size() != expectedDetectors.size() || actualSignals.size() != expectedSignals.size()) {
            fail("Parsed a different number of vehicles/detectors/signals than expected.");
        }
    }

    @Test
    public void testGetStepData2() {
        exception.expect(RuntimeException.class);
        exception.expectMessage("Colors must be:");
        playsim.getStepData(8);
    }

    @Test
    public void testGetStepData3() {
        exception.expect(RuntimeException.class);
        exception.expectMessage("Types must be:");
        playsim.getStepData(9);
    }

    @Test
    public void testGetStepData4() {
        exception.expect(RuntimeException.class);
        exception.expectMessage("invalid section type");
        playsim.getStepData(10);
    }

    @Test
    public void testGetStepData5() {
        exception.expect(RuntimeException.class);
        exception.expectMessage("not a valid step number");
        playsim.getStepData(11);
    }
}
