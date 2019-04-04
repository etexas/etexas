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
 * Unit tests for the lane direction element.
 * 
 * @author ttevendale
 */
public class LaneDirectionTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        LaneDirection directions = new LaneDirection();

        // min
        assertTrue("00".equals(directions.encodeUPER()));

        directions.setEgressPath(true);

        // some turned on
        assertTrue("01".equals(directions.encodeUPER()));

        directions.setIngressPath(true);

        // max
        assertTrue("11".equals(directions.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        LaneDirection directions = new LaneDirection();

        // min
        String remainingBits = directions.decodeUPER("00");
        assertTrue("".equals(remainingBits));
        assertFalse(directions.isIngressPath());
        assertFalse(directions.isEgressPath());

        // some turned on
        remainingBits = directions.decodeUPER("10");
        assertTrue("".equals(remainingBits));
        assertTrue(directions.isIngressPath());
        assertFalse(directions.isEgressPath());

        // max
        remainingBits = directions.decodeUPER("11");
        assertTrue("".equals(remainingBits));
        assertTrue(directions.isIngressPath());
        assertTrue(directions.isEgressPath());
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneDirection directions = new LaneDirection();
        thrown.expect(IllegalArgumentException.class);
        directions.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneDirection directions = new LaneDirection();
        String remainingBits = directions.decodeUPER("010110");
        assertTrue("0110".equals(remainingBits));
        assertFalse(directions.isIngressPath());
        assertTrue(directions.isEgressPath());
    }

    @Test
    public void testHashCode() {

        LaneDirection directions = new LaneDirection();
        directions.setIngressPath(true);
        directions.setEgressPath(true);

        LaneDirection directions2 = new LaneDirection();
        directions2.setIngressPath(false);
        directions2.setEgressPath(false);

        assertFalse(directions.hashCode() == directions2.hashCode());
        assertTrue(directions.hashCode() == directions.hashCode());
        assertTrue(directions2.hashCode() == directions2.hashCode());

        LaneDirection directions3 = new LaneDirection();
        directions3.setIngressPath(directions.isIngressPath());
        directions3.setEgressPath(directions.isEgressPath());

        assertTrue(directions.hashCode() == directions3.hashCode());
        assertFalse(directions2.hashCode() == directions3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneDirection directions = new LaneDirection();
        directions.setIngressPath(true);
        directions.setEgressPath(true);

        assertFalse(directions.equals(null));

        assertTrue(directions.equals(directions));

        LaneDirection directions2 = new LaneDirection();
        directions2.setIngressPath(false);
        directions2.setEgressPath(false);

        assertFalse(directions.equals(new String()));
        assertFalse(directions.equals(directions2));

        directions2.setIngressPath(directions.isIngressPath());
        assertFalse(directions.equals(directions2));

        directions2.setEgressPath(directions.isEgressPath());
        assertTrue(directions.equals(directions2));
    }
}
