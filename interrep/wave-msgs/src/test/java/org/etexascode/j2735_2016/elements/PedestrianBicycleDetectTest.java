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
 * Unit tests for the pedestrian bicycle detect element.
 * 
 * @author ttevendale
 */
public class PedestrianBicycleDetectTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        PedestrianBicycleDetect detect = new PedestrianBicycleDetect(false);
        String encodedDetect = detect.encodeUPER();
        assertTrue("0".equals(encodedDetect));

        detect = new PedestrianBicycleDetect(true);
        encodedDetect = detect.encodeUPER();
        assertTrue("1".equals(encodedDetect));
    }

    @Test
    public void testDecodeUPER() {

        PedestrianBicycleDetect detect = new PedestrianBicycleDetect();

        String remainingBits = detect.decodeUPER("0");
        assertTrue("".equals(remainingBits));
        assertFalse(detect.getValue());

        remainingBits = detect.decodeUPER("1");
        assertTrue("".equals(remainingBits));
        assertTrue(detect.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        PedestrianBicycleDetect detect = new PedestrianBicycleDetect();
        thrown.expect(IllegalArgumentException.class);
        detect.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        PedestrianBicycleDetect detect = new PedestrianBicycleDetect();
        String remainingBits = detect.decodeUPER("11101");
        assertTrue("1101".equals(remainingBits));
        assertTrue(detect.getValue());
    }

    @Test
    public void testHashCode() {

        PedestrianBicycleDetect detect = new PedestrianBicycleDetect(true);
        PedestrianBicycleDetect detect2 = new PedestrianBicycleDetect(false);

        assertFalse(detect.hashCode() == detect2.hashCode());
        assertTrue(detect.hashCode() == detect.hashCode());
        assertTrue(detect2.hashCode() == detect2.hashCode());

        PedestrianBicycleDetect detect3 = new PedestrianBicycleDetect(detect.getValue());

        assertTrue(detect.hashCode() == detect3.hashCode());
        assertFalse(detect2.hashCode() == detect3.hashCode());
    }

    @Test
    public void testEquals() {

        PedestrianBicycleDetect detect = new PedestrianBicycleDetect(true);

        assertFalse(detect.equals(null));

        assertTrue(detect.equals(detect));

        PedestrianBicycleDetect detect2 = new PedestrianBicycleDetect(false);

        assertFalse(detect.equals(new String()));
        assertFalse(detect.equals(detect2));

        detect2.setValue(detect.getValue());
        assertTrue(detect.equals(detect2));
    }
}
