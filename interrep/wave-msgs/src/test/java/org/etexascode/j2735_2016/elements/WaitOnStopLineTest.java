/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.j2735_2016.elements;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the wait on stop line element.
 * 
 * @author ttevendale
 */
public class WaitOnStopLineTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        WaitOnStopLine waitOnStopLine = new WaitOnStopLine(false);
        String encodedWaitOnStopLine = waitOnStopLine.encodeUPER();
        assertTrue("0".equals(encodedWaitOnStopLine));

        waitOnStopLine = new WaitOnStopLine(true);
        encodedWaitOnStopLine = waitOnStopLine.encodeUPER();
        assertTrue("1".equals(encodedWaitOnStopLine));
    }

    @Test
    public void testDecodeUPER() {

        WaitOnStopLine waitOnStopLine = new WaitOnStopLine();

        String remainingBits = waitOnStopLine.decodeUPER("0");
        assertTrue("".equals(remainingBits));
        assertFalse(waitOnStopLine.getValue());

        remainingBits = waitOnStopLine.decodeUPER("1");
        assertTrue("".equals(remainingBits));
        assertTrue(waitOnStopLine.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        WaitOnStopLine waitOnStopLine = new WaitOnStopLine();
        thrown.expect(IllegalArgumentException.class);
        waitOnStopLine.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        WaitOnStopLine waitOnStopLine = new WaitOnStopLine();
        String remainingBits = waitOnStopLine.decodeUPER("00001");
        assertTrue("0001".equals(remainingBits));
        assertFalse(waitOnStopLine.getValue());
    }

    @Test
    public void testHashCode() {

        WaitOnStopLine waitOnStopLine = new WaitOnStopLine(true);
        WaitOnStopLine waitOnStopLine2 = new WaitOnStopLine(false);

        assertFalse(waitOnStopLine.hashCode() == waitOnStopLine2.hashCode());
        assertTrue(waitOnStopLine.hashCode() == waitOnStopLine.hashCode());
        assertTrue(waitOnStopLine2.hashCode() == waitOnStopLine2.hashCode());

        WaitOnStopLine waitOnStopLine3 = new WaitOnStopLine(waitOnStopLine.getValue());

        assertTrue(waitOnStopLine.hashCode() == waitOnStopLine3.hashCode());
        assertFalse(waitOnStopLine2.hashCode() == waitOnStopLine3.hashCode());
    }

    @Test
    public void testEquals() {

        WaitOnStopLine waitOnStopLine = new WaitOnStopLine(true);

        assertFalse(waitOnStopLine.equals(null));

        assertTrue(waitOnStopLine.equals(waitOnStopLine));

        WaitOnStopLine waitOnStopLine2 = new WaitOnStopLine(false);

        assertFalse(waitOnStopLine.equals(new String()));
        assertFalse(waitOnStopLine.equals(waitOnStopLine2));

        waitOnStopLine2.setValue(waitOnStopLine.getValue());
        assertTrue(waitOnStopLine.equals(waitOnStopLine2));
    }
}
