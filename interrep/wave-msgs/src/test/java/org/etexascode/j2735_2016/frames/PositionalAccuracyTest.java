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
package org.etexascode.j2735_2016.frames;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.SemiMajorAxisAccuracy;
import org.etexascode.j2735_2016.elements.SemiMajorAxisOrientation;
import org.etexascode.j2735_2016.elements.SemiMinorAxisAccuracy;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the positional accuracy frame.
 * 
 * @author ttevendale
 */
public class PositionalAccuracyTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    PositionalAccuracy positionalAccuracy;

    String encodedBits;

    @Before
    public void init() {

        SemiMajorAxisAccuracy semiMajor = new SemiMajorAxisAccuracy(254);
        SemiMinorAxisAccuracy semiMinor = new SemiMinorAxisAccuracy(12);
        SemiMajorAxisOrientation orientation = new SemiMajorAxisOrientation(10);

        positionalAccuracy = new PositionalAccuracy(semiMajor, semiMinor, orientation);
        encodedBits = semiMajor.encodeUPER() + semiMinor.encodeUPER() + orientation.encodeUPER();
    }

    @Test
    public void testConstructor() {

        SemiMajorAxisAccuracy semiMajor = new SemiMajorAxisAccuracy(51);
        SemiMinorAxisAccuracy semiMinor = new SemiMinorAxisAccuracy(111);
        SemiMajorAxisOrientation orientation = new SemiMajorAxisOrientation(1000);

        PositionalAccuracy positionalAccuracy = new PositionalAccuracy(semiMajor, semiMinor, orientation);

        assertTrue(semiMajor.equals(positionalAccuracy.getSemiMajor()));
        assertTrue(semiMinor.equals(positionalAccuracy.getSemiMinor()));
        assertTrue(orientation.equals(positionalAccuracy.getOrientation()));
    }

    @Test
    public void testConstructorNullSemiMajor() {

        thrown.expect(NullPointerException.class);
        new PositionalAccuracy(null, new SemiMinorAxisAccuracy(), new SemiMajorAxisOrientation());
    }

    @Test
    public void testConstructorNullSemiMinor() {

        thrown.expect(NullPointerException.class);
        new PositionalAccuracy(new SemiMajorAxisAccuracy(), null, new SemiMajorAxisOrientation());
    }

    @Test
    public void testConstructorNullOrientation() {

        thrown.expect(NullPointerException.class);
        new PositionalAccuracy(new SemiMajorAxisAccuracy(), new SemiMinorAxisAccuracy(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int semiMajor = 151;
        int semiMinor = 15;
        int orientation = 233;

        PositionalAccuracy positionalAccuracy = new PositionalAccuracy(semiMajor, semiMinor, orientation);

        assertTrue(semiMajor == positionalAccuracy.getSemiMajor().getValue());
        assertTrue(semiMinor == positionalAccuracy.getSemiMinor().getValue());
        assertTrue(orientation == positionalAccuracy.getOrientation().getValue());
    }

    @Test
    public void testSetSemiMajor() {

        SemiMajorAxisAccuracy semiMajor = new SemiMajorAxisAccuracy(88);

        PositionalAccuracy positionalAccuracy = new PositionalAccuracy();
        positionalAccuracy.setSemiMajor(semiMajor);

        assertTrue(semiMajor.equals(positionalAccuracy.getSemiMajor()));

        thrown.expect(NullPointerException.class);
        positionalAccuracy.setSemiMajor(null);
    }

    @Test
    public void testSetSemiMajorPrimitive() {

        int semiMajor = 5;

        PositionalAccuracy positionalAccuracy = new PositionalAccuracy();
        positionalAccuracy.setSemiMajor(semiMajor);

        assertTrue(semiMajor == positionalAccuracy.getSemiMajor().getValue());
    }

    @Test
    public void testSetSemiMinor() {

        SemiMinorAxisAccuracy semiMinor = new SemiMinorAxisAccuracy(181);

        PositionalAccuracy positionalAccuracy = new PositionalAccuracy();
        positionalAccuracy.setSemiMinor(semiMinor);

        assertTrue(semiMinor.equals(positionalAccuracy.getSemiMinor()));

        thrown.expect(NullPointerException.class);
        positionalAccuracy.setSemiMinor(null);
    }

    @Test
    public void testSetSemiMinorPrimitive() {

        int semiMinor = 5;

        PositionalAccuracy positionalAccuracy = new PositionalAccuracy();
        positionalAccuracy.setSemiMinor(semiMinor);

        assertTrue(semiMinor == positionalAccuracy.getSemiMinor().getValue());
    }

    @Test
    public void testSetOrientation() {

        SemiMajorAxisOrientation orientation = new SemiMajorAxisOrientation(2001);

        PositionalAccuracy positionalAccuracy = new PositionalAccuracy();
        positionalAccuracy.setOrientation(orientation);

        assertTrue(orientation.equals(positionalAccuracy.getOrientation()));

        thrown.expect(NullPointerException.class);
        positionalAccuracy.setOrientation(null);
    }

    @Test
    public void testSetOrientationPrimitive() {

        int orientation = 5;

        PositionalAccuracy positionalAccuracy = new PositionalAccuracy();
        positionalAccuracy.setOrientation(orientation);

        assertTrue(orientation == positionalAccuracy.getOrientation().getValue());
    }

    @Test
    public void testEncodeUPER() {

        assertTrue(encodedBits.equalsIgnoreCase(positionalAccuracy.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        PositionalAccuracy decodedPositionalAccuracy = new PositionalAccuracy();
        decodedPositionalAccuracy.decodeUPER(encodedBits);
        assertTrue(positionalAccuracy.equals(decodedPositionalAccuracy));
    }

    @Test
    public void testHashCode() {

        int semiMajor = positionalAccuracy.getSemiMajor().getValue();
        int semiMinor = positionalAccuracy.getSemiMinor().getValue();
        int orientation = positionalAccuracy.getOrientation().getValue();

        PositionalAccuracy positionalAccuracy2 = new PositionalAccuracy(semiMajor + 1, semiMinor + 1, orientation + 1);

        assertFalse(positionalAccuracy.hashCode() == positionalAccuracy2.hashCode());
        assertTrue(positionalAccuracy.hashCode() == positionalAccuracy.hashCode());
        assertTrue(positionalAccuracy2.hashCode() == positionalAccuracy2.hashCode());

        PositionalAccuracy positionalAccuracy3 = new PositionalAccuracy(semiMajor, semiMinor, orientation);

        assertTrue(positionalAccuracy.hashCode() == positionalAccuracy3.hashCode());
        assertFalse(positionalAccuracy2.hashCode() == positionalAccuracy3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(positionalAccuracy.equals(positionalAccuracy));
        assertFalse(positionalAccuracy.equals(null));
        assertFalse(positionalAccuracy.equals(new String()));

        int semiMajor = positionalAccuracy.getSemiMajor().getValue();
        int semiMinor = positionalAccuracy.getSemiMinor().getValue();
        int orientation = positionalAccuracy.getOrientation().getValue();

        // different
        PositionalAccuracy positionalAccuracy2 = new PositionalAccuracy(semiMajor + 1, semiMinor + 1, orientation + 1);

        assertFalse(positionalAccuracy.equals(positionalAccuracy2));

        // different semi major
        positionalAccuracy2 = new PositionalAccuracy(semiMajor + 1, semiMinor, orientation);

        assertFalse(positionalAccuracy.equals(positionalAccuracy2));

        // different semi minor
        positionalAccuracy2 = new PositionalAccuracy(semiMajor, semiMinor + 1, orientation);

        assertFalse(positionalAccuracy.equals(positionalAccuracy2));

        // different orientation
        positionalAccuracy2 = new PositionalAccuracy(semiMajor, semiMinor, orientation + 1);

        assertFalse(positionalAccuracy.equals(positionalAccuracy2));

        // same
        positionalAccuracy2 = new PositionalAccuracy(semiMajor, semiMinor, orientation);

        assertTrue(positionalAccuracy.equals(positionalAccuracy2));
    }
}
