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
package com.harmonia.apps.exp;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class SPATProducerAppTest {

    List<ISignalIndication> timeSigs = null;

    IndsMeaning timeIm = null;

    @Before
    public void setup() {
        timeSigs = new ArrayList<ISignalIndication>(2);

        SignalIndication si = new SignalIndication();
        si.setTimeToChange(15.0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        timeSigs.add(si);

        si = new SignalIndication();
        si.setTimeToChange(20.0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        timeSigs.add(si);

        timeIm = new IndsMeaning(timeSigs, 42);
    }

    @After
    public void teardown() {
        timeSigs = null;
        timeIm = null;
    }

    @Test
    public void testGetCurrState() {
        byte[] res = SPATProducerAppExp.getCurrState(timeIm);
        byte[] exp = new byte[] { (byte)1 };
        assertEqualsByteArray(exp, res);
    }

    @Test
    public void testChop1() {
        byte[] res = SPATProducerAppExp.chop(new byte[] { (byte)0, (byte)0, (byte)0 });
        assertEquals(1, res.length);
        assertEquals((byte)0, res[0]);
    }

    @Test
    public void testChop2() {
        byte[] res = SPATProducerAppExp.chop(new byte[] { (byte)0, (byte)1, (byte)0 });
        byte[] exp = new byte[] { (byte)1, (byte)0 };
        assertEqualsByteArray(exp, res);
    }

    @Test
    public void testGetMinTimeRemaining() {
        byte[] res = SPATProducerAppExp.getMinTimeRemaining(timeIm);
        byte[] exp = new byte[] { (byte)0x00, (byte)0x0F };
        assertEqualsByteArray(exp, res);
    }

    @Test
    public void testGetMaxTimeRemaining() {
        byte[] res = SPATProducerAppExp.getMaxTimeRemaining(timeIm);
        byte[] exp = new byte[] { (byte)0x00, (byte)0x0F };
        assertEqualsByteArray(exp, res);
    }

    @Test
    public void testGetTime() {
        assertEquals(15.0, SPATProducerAppExp.getTime(timeSigs), 0.05);
    }

    private void assertEqualsByteArray(byte[] b1, byte[] b2) {
        assertEquals(b1.length, b2.length);

        for (int i = 0; i < b1.length; i++) {
            assertEquals(b1[i], b2[i]);
        }
    }
}
