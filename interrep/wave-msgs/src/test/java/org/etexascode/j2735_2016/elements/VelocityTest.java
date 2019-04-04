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
 * Unit tests for the velocity element.
 * 
 * @author ttevendale
 */
public class VelocityTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        Velocity velocity = new Velocity(Velocity.MIN);
        assertTrue(Velocity.MIN == velocity.getValue());

        thrown.expect(IllegalArgumentException.class);
        velocity = new Velocity(Velocity.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        Velocity velocity = new Velocity(Velocity.MAX);
        assertTrue(Velocity.MAX == velocity.getValue());

        thrown.expect(IllegalArgumentException.class);
        velocity = new Velocity(Velocity.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 7999;
        Velocity velocity = new Velocity(randomNum);
        assertTrue(randomNum == velocity.getValue());
    }

    @Test
    public void testSetValueMin() {

        Velocity velocity = new Velocity();
        velocity.setValue(Velocity.MIN);
        assertTrue(Velocity.MIN == velocity.getValue());

        thrown.expect(IllegalArgumentException.class);
        velocity.setValue(Velocity.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        Velocity velocity = new Velocity();
        velocity.setValue(Velocity.MAX);
        assertTrue(Velocity.MAX == velocity.getValue());

        thrown.expect(IllegalArgumentException.class);
        velocity.setValue(Velocity.MAX + 1);
    }

    @Test
    public void testSetValue() {

        Velocity velocity = new Velocity();
        int randomNum = 2012;
        velocity.setValue(randomNum);
        assertTrue(randomNum == velocity.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        Velocity velocity = new Velocity(Velocity.MIN);
        String encodedVelocity = velocity.encodeUPER();
        assertTrue("0000000000000".equals(encodedVelocity));

        // test max
        velocity = new Velocity(Velocity.MAX);
        encodedVelocity = velocity.encodeUPER();
        assertTrue("1111111111111".equals(encodedVelocity));

        int randomNumber = 541;
        velocity = new Velocity(randomNumber);
        encodedVelocity = velocity.encodeUPER();
        assertTrue("0001000011101".equals(encodedVelocity));
    }

    @Test
    public void testDecodeUPER() {

        Velocity velocity = new Velocity();

        // test min
        String remainingBits = velocity.decodeUPER("0000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Velocity.MIN == velocity.getValue());

        // test max
        remainingBits = velocity.decodeUPER("1111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(Velocity.MAX == velocity.getValue());

        int expectedNumber = 1789;
        remainingBits = velocity.decodeUPER("0011011111101");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == velocity.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        Velocity velocity = new Velocity();
        thrown.expect(IllegalArgumentException.class);
        velocity.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        Velocity velocity = new Velocity();
        String remainingBits = velocity.decodeUPER("00001000000000");
        assertTrue("0".equals(remainingBits));
        assertTrue(velocity.getValue() == 256);
    }

    @Test
    public void testHashCode() {

        Velocity velocity = new Velocity(8000);
        Velocity velocity2 = new Velocity(45);

        assertFalse(velocity.hashCode() == velocity2.hashCode());
        assertTrue(velocity.hashCode() == velocity.hashCode());
        assertTrue(velocity2.hashCode() == velocity2.hashCode());

        Velocity velocity3 = new Velocity(velocity.getValue());

        assertTrue(velocity.hashCode() == velocity3.hashCode());
        assertFalse(velocity2.hashCode() == velocity3.hashCode());
    }

    @Test
    public void testEquals() {

        Velocity velocity = new Velocity(8191);

        assertFalse(velocity.equals(null));

        assertTrue(velocity.equals(velocity));

        Velocity velocity2 = new Velocity(200);

        assertFalse(velocity.equals(new String()));
        assertFalse(velocity.equals(velocity2));

        velocity2.setValue(velocity.getValue());
        assertTrue(velocity.equals(velocity2));
    }
}
