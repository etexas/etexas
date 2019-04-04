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

package org.etexascode.interrep.datamodel;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * @author ttevendale
 */
public class SignalCommandTest {

    SignalCommand changeCom;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Before
    public void setUp() throws Exception {
        changeCom = new SignalCommand(SignalCommand.CHANGE_SIGNAL, 5);
    }

    @After
    public void tearDown() throws Exception {
        changeCom = null;
    }

    @Test
    public void testCreateChangeCommand() {
        double changeTime = 15;
        SignalCommand sigCom = SignalCommand.createChangeCommand(changeTime);
        assertTrue(SignalCommand.CHANGE_SIGNAL == sigCom.getSignalCommand());

        assertTrue(changeTime == sigCom.getTime());
    }

    @Test
    public void testCreateHoldCommand() {
        double holdTime = 12;
        SignalCommand sigCom = SignalCommand.createHoldCommand(holdTime);
        assertTrue(SignalCommand.HOLD_SIGNAL == sigCom.getSignalCommand());
        assertTrue(holdTime == sigCom.getTime());
    }

    @Test
    public void testSignalCommandConstructor() {
        int time = 5;
        SignalCommand sigCom = new SignalCommand(SignalCommand.CHANGE_SIGNAL, time);
        assertTrue(SignalCommand.CHANGE_SIGNAL == sigCom.getSignalCommand());
        assertTrue(time == sigCom.getTime());

        time = 7;
        sigCom = new SignalCommand(SignalCommand.HOLD_SIGNAL, time);
        assertTrue(SignalCommand.HOLD_SIGNAL == sigCom.getSignalCommand());
        assertTrue(time == sigCom.getTime());
        try {
            new SignalCommand(3, 4);
            thrown.expect(IllegalArgumentException.class);
        }
        catch (IllegalArgumentException e) {}
    }

    @Test
    public void testEquals() {
        SignalCommand diffSigCom = new SignalCommand(SignalCommand.HOLD_SIGNAL, 1);
        SignalCommand almostSameSigCom = new SignalCommand(SignalCommand.CHANGE_SIGNAL, 1);
        SignalCommand sameSigCom = new SignalCommand(SignalCommand.CHANGE_SIGNAL, 5);

        assertTrue(changeCom.equals(sameSigCom));
        assertFalse(changeCom.equals(diffSigCom));
        assertFalse(changeCom.equals(almostSameSigCom));
        assertFalse(changeCom.equals(new String()));
    }

    @Test
    public void testToString() {

        String sigStr = changeCom.toString();
        String testStr = String.format("SignalCommand [command=%d, time=%s]", changeCom.getSignalCommand(), Double.toString(changeCom.getTime()));
        assertTrue(testStr.equals(sigStr));
    }

}
