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
package org.etexascode.wavesim;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TestWaveSimLayer {

    List<IWaveSim> sims = null;

    WaveSimLayer wsl = null;

    @Before
    public void setup() {
        sims = new ArrayList<IWaveSim>(3);

        sims.add(new TestWaveSim());
        sims.add(new TestWaveSim());
        sims.add(new TestWaveSim());

        wsl = new WaveSimLayer(sims);
    }

    @After
    public void teardown() {
        sims = null;
        wsl = null;
    }

    @Test
    public void testConstructor() {
        WaveSimLayer wsl2 = new WaveSimLayer(sims);
        assertEquals(wsl2.sims, sims);
    }

    @Test
    public void testWaveSim() {
        assertEquals(3, wsl.sims.size());

        for (IWaveSim sim : wsl.sims) {
            TestWaveSim tws = (TestWaveSim)sim;
            assertEquals(0, tws.stepSet);
        }

        wsl.waveSim(12);

        for (IWaveSim sim : wsl.sims) {
            TestWaveSim tws = (TestWaveSim)sim;
            assertEquals(12, tws.stepSet);
        }
    }

    public class TestWaveSim implements IWaveSim {

        public int stepSet = 0;

        @Override
        public void transmitMessages(int stepNum) {
            stepSet = stepNum;
        }
    }
}
