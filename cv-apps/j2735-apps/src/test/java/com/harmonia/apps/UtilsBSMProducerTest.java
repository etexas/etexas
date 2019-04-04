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

import java.io.UnsupportedEncodingException;

import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.test.GenVehicleFunctions;
import org.junit.Before;
import org.junit.Test;

public class UtilsBSMProducerTest {

    Vehicle vPrime = GenVehicleFunctions.genVehicle(26, -1600, 180, 180, 4, 2);

    BasicSafetyMessage bsmExpected;

    BasicSafetyMessage bsmActual;

    BasicSafetyMessageVerbose bsmVerboseExpected = new BasicSafetyMessageVerbose();

    BasicSafetyMessageVerbose bsmVerboseActual = new BasicSafetyMessageVerbose();

    @Before
    public void setUp() throws UnsupportedEncodingException {
        vPrime.setHeading(0.0);
        vPrime.setAcceleration(0.0);
        vPrime.setSpeed(0.0);

        new BSMVerboseProducerApp().getBasicSafetyMessageVerbose(.5, vPrime, true, (short)0, bsmVerboseExpected);
        new BSMVerboseProducerApp().getBasicSafetyMessageVerbose(.5, vPrime, true, (short)0, bsmVerboseActual);
        UtilsProducer.addExtendedData(.5, bsmVerboseExpected, vPrime);
        UtilsProducer.addExtendedData(.5, bsmVerboseActual, vPrime);
    }

    @Test
    public void testAddExtendedDataSafetyVerbose() {
        assertEquals(bsmVerboseExpected.getSafetyExt().getEvents(), bsmVerboseActual.getSafetyExt().getEvents());
        assertEquals(bsmVerboseExpected.getSafetyExt().getPathHistory().getItemCnt(), bsmVerboseActual.getSafetyExt().getPathHistory().getItemCnt());
        assertEquals(bsmVerboseExpected.getSafetyExt().getPathPrediction().getConfidence(), bsmVerboseActual.getSafetyExt().getPathPrediction().getConfidence());
        assertEquals(bsmVerboseExpected.getSafetyExt().getTheRTCM().getRtcmHeader()[0], bsmVerboseActual.getSafetyExt().getTheRTCM().getRtcmHeader()[0]);
    }

}
