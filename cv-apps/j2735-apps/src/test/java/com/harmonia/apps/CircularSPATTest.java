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

import java.util.List;

import org.etexascode.apps.UtilsMessageImports;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.j2735.SPAT;
import org.etexascode.test.GenSignalFunctions;
import org.junit.Before;
import org.junit.Test;

public class CircularSPATTest {

    SignalManager start = GenSignalFunctions.genSignalManager();

    SignalManager finish = new SignalManager();

    @Before
    public void setup() {

        SPATProducerApp spa = new SPATProducerApp();
        spa.getFormattedSPATMessage(0, start);
        SPAT message = spa.spatMessage;
        List<SignalIndication> signals = UtilsMessageImports.importSignal(message);
        // Waiting for Signal module changes
        for (SignalIndication s : signals) {
            finish.addSignal(s);
        }
    }

    @Test
    public void testSPATCircular() {
        assertEquals(start, finish);
    }
}
