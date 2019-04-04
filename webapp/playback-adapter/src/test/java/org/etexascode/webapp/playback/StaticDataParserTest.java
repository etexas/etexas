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

import java.awt.Polygon;
import java.io.File;
import java.net.URL;
import java.util.Map;

import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.SimMetaData;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class StaticDataParserTest {

    @Rule
    public ExpectedException ee = ExpectedException.none();

    final double TOLERANCE = 0.00005;

    String inputDirectory = "/playbackTestInput";

    String inputDirectory2 = "/playbackTestInput3";

    PlaybackSimulator playsim;

    PlaybackSimulator playsim2;

    @Before
    public void setUp() {
        URL url = this.getClass().getResource(inputDirectory);
        URL url2 = this.getClass().getResource(inputDirectory2);
        if (url == null || url2 == null)
            System.out.println("Null URL.");
        try {
            File input = new File(url.toURI());
            playsim = new PlaybackSimulator(input);
            File input2 = new File(url2.toURI());
            playsim2 = new PlaybackSimulator(input2);
        }
        catch (Exception e) {
            fail("Couldn't create directory File.");
        }
    }

    @Test
    public void testParseStaticData() {
        StaticData sd = playsim.getStaticData();
        SimMetaData md = sd.getMetaData();

        assertEquals(getExpectedLaneManager(), sd.getLaneManager());
        assertEquals(UtilsUnitConversion.convertFeetToCentimeters(40.0), md.getSimWidth(), TOLERANCE);
        assertEquals(UtilsUnitConversion.convertFeetToCentimeters(40.0), md.getSimHeight(), TOLERANCE);
        assertEquals(7, md.getFirstStep());
        assertEquals(9642, md.getMaxSteps());
        assertEquals(0.1, md.getStepSize(), TOLERANCE);
        assertEquals(new SignalManager(), sd.getSignalManager());
        assertEquals(getExpectedDetectorManager(), sd.getDetectorManager());
    }

    @Test
    public void testParseStaticData2() {
        ee.expect(RuntimeException.class);
        playsim2.getStaticData();
    }

    private LaneManager getExpectedLaneManager() {
        LaneManager expectedLM = new LaneManager();
        expectedLM.setLatitude(0.0);
        expectedLM.setLongitude(0.0);

        Lane l1 = new Lane();
        l1.setLaneId(1);
        l1.setApproachId(1);
        l1.setType(Lane.INBOUND);
        Map<Integer, LaneMovement> movements = l1.getLaneMovements();
        LaneMovement m = new LaneMovement();
        m.setMovementId(1000);
        m.setMovement(Movement.STRAIGHT);
        movements.put(1000, m);
        l1.setType("Driving Lane");
        LaneNode ln1 = new LaneNode();
        ln1.setX(UtilsUnitConversion.convertFeetToCentimeters(0.0));
        ln1.setY(UtilsUnitConversion.convertFeetToCentimeters(1.0));
        ln1.setZ(UtilsUnitConversion.convertFeetToCentimeters(0.0));
        ln1.setWidth(UtilsUnitConversion.convertFeetToCentimeters(10.0));
        LaneNode ln2 = new LaneNode();
        ln2.setX(UtilsUnitConversion.convertFeetToCentimeters(0.0));
        ln2.setY(UtilsUnitConversion.convertFeetToCentimeters(2.0));
        ln2.setZ(UtilsUnitConversion.convertFeetToCentimeters(0.0));
        ln2.setWidth(UtilsUnitConversion.convertFeetToCentimeters(10.0));
        l1.addLaneNode(ln1);
        l1.addLaneNode(ln2);

        Lane l2 = new Lane();
        l2.setLaneId(99);
        l2.setApproachId(99);
        l2.setType(Lane.OUTBOUND);
        movements = l2.getLaneMovements();

        m = new LaneMovement();
        m.setMovementId(99000);
        m.setMovement(Movement.RIGHT_TURN);
        movements.put(99000, m);

        m = new LaneMovement();
        m.setMovementId(99001);
        m.setMovement(Movement.RIGHT_TURN_ON_RED);
        movements.put(99001, m);

        m = new LaneMovement();
        m.setMovementId(99002);
        m.setMovement(Movement.LEFT_TURN);
        movements.put(99002, m);

        m = new LaneMovement();
        m.setMovementId(99003);
        m.setMovement(Movement.LEFT_TURN_ON_RED);
        movements.put(99003, m);

        m = new LaneMovement();
        m.setMovementId(99004);
        m.setMovement(Movement.U_TURN);
        movements.put(99004, m);

        m = new LaneMovement();
        m.setMovementId(99005);
        m.setMovement(Movement.STRAIGHT);
        movements.put(99005, m);

        l2.setType("Driving Lane");
        ln1 = new LaneNode();
        ln1.setX(UtilsUnitConversion.convertFeetToCentimeters(-20.0));
        ln1.setY(UtilsUnitConversion.convertFeetToCentimeters(-20.0));
        ln1.setZ(UtilsUnitConversion.convertFeetToCentimeters(0.0));
        ln1.setWidth(UtilsUnitConversion.convertFeetToCentimeters(10.0));
        ln2 = new LaneNode();
        ln2.setX(UtilsUnitConversion.convertFeetToCentimeters(20.0));
        ln2.setY(UtilsUnitConversion.convertFeetToCentimeters(20.0));
        ln2.setZ(UtilsUnitConversion.convertFeetToCentimeters(0.0));
        ln2.setWidth(UtilsUnitConversion.convertFeetToCentimeters(10.0));
        l2.addLaneNode(ln1);
        l2.addLaneNode(ln2);

        expectedLM.getLanes().put(1, l1);
        expectedLM.getLanes().put(99, l2);

        return expectedLM;
    }

    private DetectorManager getExpectedDetectorManager() {
        DetectorManager expectedDM = new DetectorManager();

        Detector d = new Detector();
        d.setDetectorID(1);
        d.getLaneIDs().add(1);

        int x1 = (int)UtilsUnitConversion.convertFeetToCentimeters(10.0);
        int y1 = (int)UtilsUnitConversion.convertFeetToCentimeters(20.0);
        int x2 = (int)UtilsUnitConversion.convertFeetToCentimeters(30.0);
        int y2 = (int)UtilsUnitConversion.convertFeetToCentimeters(40.0);

        Polygon p = d.getArea();
        p.addPoint(x1, y1);
        p.addPoint(x1, y2);
        p.addPoint(x2, y2);
        p.addPoint(x2, y1);
        d.setArea(p);

        d.setPresenceDetectCap(true);
        d.setSpeedDetectCap(true);
        d.setLengthDetectCap(true);

        expectedDM.addDetector(1, d);

        return expectedDM;
    }

    @Test
    public void testConstructor() {
        StaticDataParser sdp = new StaticDataParser();
    }
}
