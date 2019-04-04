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

import static org.junit.Assert.assertEquals;

import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.junit.Before;
import org.junit.Test;

/**
 * Simple vehicle parser test.
 * 
 * @author jrutherford
 */
public class SimpleVehicleParserTest {

    BasicSafetyMessageVerbose bsm = null;

    @Before
    public void setup() {
        bsm = new BasicSafetyMessageVerbose();
        bsm.setId(new byte[] { 0, 0, 0, 0 });
        bsm.setLat(100);
        bsm.setLong(200);
        bsm.setElev(new byte[] { 0, 0 });
        bsm.setAccuracy(new byte[] { 0, 0, 0, 0 });
        bsm.setSpeed(new byte[] { 0, 0 });
        bsm.setAngle(new byte[] { 0, 0 });
        bsm.setAccelSet(new byte[] { 0, 0, 0, 0, 0, 0, 0 });
        bsm.setBrakes(new byte[] { 0, 0 });
    }

    @Test
    public void testParseVehicleManager1() {
        SimpleVehicleParser svp = new SimpleVehicleParser();
        Object mes[] = { 0, 0 };
        assertEquals(0, svp.parseVehicleManager(mes, new LaneManager(), 1).getAllVehicleIds().size());
    }

    @Test
    public void testParseVehicleManager2() {
        BasicSafetyMessage bsm = new BasicSafetyMessage();
        bsm.setBlob1(new byte[] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });

        SimpleVehicleParser svp = new SimpleVehicleParser();
        Object mes[] = { bsm, 0 };
        assertEquals(1, svp.parseVehicleManager(mes, new LaneManager(), 1).getAllVehicleIds().size());
    }

    @Test
    public void testParseVehicleManager3() {
        SimpleVehicleParser svp = new SimpleVehicleParser();
        Object mes[] = { bsm, 0 };
        assertEquals(1, svp.parseVehicleManager(mes, new LaneManager(), 1).getAllVehicleIds().size());
    }
}
