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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.AdvisorySpeedType;
import org.etexascode.j2735_2016.elements.AdvisorySpeedType.Advisory;
import org.etexascode.j2735_2016.elements.RestrictionClassID;
import org.etexascode.j2735_2016.elements.SpeedAdvice;
import org.etexascode.j2735_2016.elements.SpeedConfidence;
import org.etexascode.j2735_2016.elements.SpeedConfidence.SpeedPrecision;
import org.etexascode.j2735_2016.elements.ZoneLength;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the advisory speed frame.
 * 
 * @author ttevendale
 */
public class AdvisorySpeedTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    AdvisorySpeed advisorySpeed;

    @Before
    public void init() {

        AdvisorySpeedType type = new AdvisorySpeedType(Advisory.GREENWAVE);
        SpeedAdvice speed = new SpeedAdvice(24);
        SpeedConfidence confidence = new SpeedConfidence(SpeedPrecision.PREC1MS);
        ZoneLength distance = new ZoneLength(54);
        RestrictionClassID restrictedClass = new RestrictionClassID(1);

        advisorySpeed = new AdvisorySpeed(type);
        advisorySpeed.setType(type);
        advisorySpeed.setSpeed(speed);
        advisorySpeed.setConfidence(confidence);
        advisorySpeed.setDistance(distance);
        advisorySpeed.setRestrictedClass(restrictedClass);
    }

    @Test
    public void testConstructor() {

        AdvisorySpeedType type = new AdvisorySpeedType(Advisory.GREENWAVE);

        AdvisorySpeed advisorySpeed = new AdvisorySpeed(type);

        assertNull(advisorySpeed.getSpeed());
        assertNull(advisorySpeed.getConfidence());
        assertNull(advisorySpeed.getDistance());
        assertNull(advisorySpeed.getRestrictedClass());
        assertTrue(type.equals(advisorySpeed.getType()));

        thrown.expect(NullPointerException.class);
        new AdvisorySpeed((AdvisorySpeedType)null);
    }

    @Test
    public void testConstructorPrimitive() {

        Advisory type = Advisory.ECODRIVE;

        AdvisorySpeed advisorySpeed = new AdvisorySpeed(type);

        assertTrue(type.equals(advisorySpeed.getType().getEnumeration()));

        thrown.expect(NullPointerException.class);
        new AdvisorySpeed((Advisory)null);
    }

    @Test
    public void testSetType() {

        AdvisorySpeedType type = new AdvisorySpeedType(Advisory.GREENWAVE);

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        advisorySpeed.setType(type);

        assertTrue(type.equals(advisorySpeed.getType()));

        thrown.expect(NullPointerException.class);
        advisorySpeed.setType((AdvisorySpeedType)null);
    }

    @Test
    public void testSetTypePrimitive() {

        Advisory type = Advisory.ECODRIVE;

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        advisorySpeed.setType(type);

        assertTrue(type.equals(advisorySpeed.getType().getEnumeration()));

        thrown.expect(NullPointerException.class);
        advisorySpeed.setType((Advisory)null);
    }

    @Test
    public void testSetSpeedPrimitive() {

        int speed = 84;

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        advisorySpeed.setSpeed(speed);

        assertTrue(speed == advisorySpeed.getSpeed().getValue());

        speed = 12;

        advisorySpeed.setSpeed(speed);

        assertTrue(speed == advisorySpeed.getSpeed().getValue());
    }

    @Test
    public void testSetConfidencePrimitive() {

        SpeedPrecision precision = SpeedPrecision.PREC100MS;

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        advisorySpeed.setConfidence(precision);

        assertTrue(precision.equals(advisorySpeed.getConfidence().getEnumeration()));

        precision = SpeedPrecision.PREC0_01MS;

        advisorySpeed.setConfidence(precision);

        assertTrue(precision.equals(advisorySpeed.getConfidence().getEnumeration()));

        // the SpeedConfidence can be set to null, but the enumeration cannot.
        thrown.expect(NullPointerException.class);
        advisorySpeed.setConfidence((SpeedPrecision)null);
    }

    @Test
    public void testSetDistancePrimitive() {

        int distance = 879;

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        advisorySpeed.setDistance(distance);

        assertTrue(distance == advisorySpeed.getDistance().getValue());

        distance = 182;

        advisorySpeed.setDistance(distance);

        assertTrue(distance == advisorySpeed.getDistance().getValue());
    }

    @Test
    public void testSetRestrictedClassPrimitive() {

        int restrictedClass = 255;

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        advisorySpeed.setRestrictedClass(restrictedClass);

        assertTrue(restrictedClass == advisorySpeed.getRestrictedClass().getValue());

        restrictedClass = 5;

        advisorySpeed.setRestrictedClass(restrictedClass);

        assertTrue(restrictedClass == advisorySpeed.getRestrictedClass().getValue());
    }

    @Test
    public void testEncodeUPERMin() {

        AdvisorySpeedType type = new AdvisorySpeedType(Advisory.TRANSIT);

        AdvisorySpeed advisorySpeed = new AdvisorySpeed(type);

        String advisorySpeedOptionals = "000000";
        String remainingBits = type.encodeUPER();
        assertTrue((advisorySpeedOptionals + remainingBits).equals(advisorySpeed.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        AdvisorySpeedType type = new AdvisorySpeedType(Advisory.TRANSIT);
        SpeedAdvice speed = new SpeedAdvice(51);
        SpeedConfidence confidence = new SpeedConfidence(SpeedPrecision.PREC10MS);
        ZoneLength distance = new ZoneLength(787);
        RestrictionClassID restrictedClass = new RestrictionClassID(240);

        AdvisorySpeed advisorySpeed = new AdvisorySpeed(type);
        advisorySpeed.setType(type);
        advisorySpeed.setSpeed(speed);
        advisorySpeed.setConfidence(confidence);
        advisorySpeed.setDistance(distance);
        advisorySpeed.setRestrictedClass(restrictedClass);

        String advisorySpeedOptionals = "011110";
        String remainingBits = type.encodeUPER() + speed.encodeUPER() + confidence.encodeUPER() + distance.encodeUPER() + restrictedClass.encodeUPER();
        assertTrue((advisorySpeedOptionals + remainingBits).equals(advisorySpeed.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        AdvisorySpeedType type = new AdvisorySpeedType(Advisory.NONE);

        String advisorySpeedOptionals = "000000";

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        String remainingBits = advisorySpeed.decodeUPER(advisorySpeedOptionals + type.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(advisorySpeed.getSpeed());
        assertNull(advisorySpeed.getConfidence());
        assertNull(advisorySpeed.getDistance());
        assertNull(advisorySpeed.getRestrictedClass());
        assertTrue(type.equals(advisorySpeed.getType()));
    }

    @Test
    public void testDecodeUPERMax() {

        AdvisorySpeedType type = new AdvisorySpeedType(Advisory.ECODRIVE);
        SpeedAdvice speed = new SpeedAdvice(500);
        SpeedConfidence confidence = new SpeedConfidence(SpeedPrecision.PREC0_01MS);
        ZoneLength distance = new ZoneLength(7);
        RestrictionClassID restrictedClass = new RestrictionClassID(101);

        String advisorySpeedOptionals = "011110";

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        String remainingBits = advisorySpeed
                .decodeUPER(advisorySpeedOptionals + type.encodeUPER() + speed.encodeUPER() + confidence.encodeUPER() + distance.encodeUPER() + restrictedClass.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(type.equals(advisorySpeed.getType()));
        assertTrue(speed.equals(advisorySpeed.getSpeed()));
        assertTrue(confidence.equals(advisorySpeed.getConfidence()));
        assertTrue(distance.equals(advisorySpeed.getDistance()));
        assertTrue(restrictedClass.equals(advisorySpeed.getRestrictedClass()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String advisorySpeedOptionals = "100000";

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        thrown.expect(IllegalArgumentException.class);
        advisorySpeed.decodeUPER(advisorySpeedOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String advisorySpeedOptionals = "001111";

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        thrown.expect(IllegalArgumentException.class);
        advisorySpeed.decodeUPER(advisorySpeedOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String advisorySpeedOptionals = "01110";

        AdvisorySpeed advisorySpeed = new AdvisorySpeed();
        thrown.expect(IllegalArgumentException.class);
        advisorySpeed.decodeUPER(advisorySpeedOptionals);
    }

    @Test
    public void testHashCode() {

        Advisory type = advisorySpeed.getType().getEnumeration();
        int speed = advisorySpeed.getSpeed().getValue();
        SpeedPrecision confidence = advisorySpeed.getConfidence().getEnumeration();
        int distance = advisorySpeed.getDistance().getValue();
        int restrictedClass = advisorySpeed.getRestrictedClass().getValue();

        AdvisorySpeed advisorySpeed2 = new AdvisorySpeed(Advisory.NONE);
        advisorySpeed2.setSpeed(speed + 1);
        advisorySpeed2.setConfidence(SpeedPrecision.UNAVAILABLE);
        advisorySpeed2.setDistance(distance + 1);
        advisorySpeed2.setRestrictedClass(restrictedClass + 1);

        assertFalse(advisorySpeed.hashCode() == advisorySpeed2.hashCode());
        assertTrue(advisorySpeed.hashCode() == advisorySpeed.hashCode());
        assertTrue(advisorySpeed2.hashCode() == advisorySpeed2.hashCode());

        AdvisorySpeed advisorySpeed3 = new AdvisorySpeed(type);
        advisorySpeed3.setSpeed(speed);
        advisorySpeed3.setConfidence(confidence);
        advisorySpeed3.setDistance(distance);
        advisorySpeed3.setRestrictedClass(restrictedClass);

        assertTrue(advisorySpeed.hashCode() == advisorySpeed3.hashCode());
        assertFalse(advisorySpeed2.hashCode() == advisorySpeed3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(advisorySpeed.equals(advisorySpeed));
        assertFalse(advisorySpeed.equals(null));
        assertFalse(advisorySpeed.equals(new String()));

        Advisory type = advisorySpeed.getType().getEnumeration();
        int speed = advisorySpeed.getSpeed().getValue();
        SpeedPrecision confidence = advisorySpeed.getConfidence().getEnumeration();
        int distance = advisorySpeed.getDistance().getValue();
        int restrictedClass = advisorySpeed.getRestrictedClass().getValue();

        // different
        AdvisorySpeed advisorySpeed2 = new AdvisorySpeed(Advisory.NONE);
        advisorySpeed2.setSpeed(speed + 1);
        advisorySpeed2.setConfidence(SpeedPrecision.UNAVAILABLE);
        advisorySpeed2.setDistance(distance + 1);
        advisorySpeed2.setRestrictedClass(restrictedClass + 1);

        assertFalse(advisorySpeed.equals(advisorySpeed2));

        // different type
        advisorySpeed2 = new AdvisorySpeed(Advisory.NONE);
        advisorySpeed2.setSpeed(speed);
        advisorySpeed2.setConfidence(confidence);
        advisorySpeed2.setDistance(distance);
        advisorySpeed2.setRestrictedClass(restrictedClass);

        assertFalse(advisorySpeed.equals(advisorySpeed2));

        // different speed
        advisorySpeed2 = new AdvisorySpeed(type);
        advisorySpeed2.setSpeed(speed + 1);
        advisorySpeed2.setConfidence(confidence);
        advisorySpeed2.setDistance(distance);
        advisorySpeed2.setRestrictedClass(restrictedClass);

        assertFalse(advisorySpeed.equals(advisorySpeed2));

        // different confidence
        advisorySpeed2 = new AdvisorySpeed(type);
        advisorySpeed2.setSpeed(speed);
        advisorySpeed2.setConfidence(SpeedPrecision.UNAVAILABLE);
        advisorySpeed2.setDistance(distance);
        advisorySpeed2.setRestrictedClass(restrictedClass);

        assertFalse(advisorySpeed.equals(advisorySpeed2));

        // different distance
        advisorySpeed2 = new AdvisorySpeed(type);
        advisorySpeed2.setSpeed(speed);
        advisorySpeed2.setConfidence(confidence);
        advisorySpeed2.setDistance(distance + 1);
        advisorySpeed2.setRestrictedClass(restrictedClass);

        assertFalse(advisorySpeed.equals(advisorySpeed2));

        // different restricted class
        advisorySpeed2 = new AdvisorySpeed(type);
        advisorySpeed2.setSpeed(speed);
        advisorySpeed2.setConfidence(confidence);
        advisorySpeed2.setDistance(distance);
        advisorySpeed2.setRestrictedClass(restrictedClass + 1);

        assertFalse(advisorySpeed.equals(advisorySpeed2));

        // same
        advisorySpeed2 = new AdvisorySpeed(type);
        advisorySpeed2.setSpeed(speed);
        advisorySpeed2.setConfidence(confidence);
        advisorySpeed2.setDistance(distance);
        advisorySpeed2.setRestrictedClass(restrictedClass);

        assertTrue(advisorySpeed.equals(advisorySpeed2));
    }

    @Test
    public void testEqualsNull() {

        Advisory type = advisorySpeed.getType().getEnumeration();
        int speed = advisorySpeed.getSpeed().getValue();
        SpeedPrecision confidence = advisorySpeed.getConfidence().getEnumeration();
        int distance = advisorySpeed.getDistance().getValue();
        int restrictedClass = advisorySpeed.getRestrictedClass().getValue();

        AdvisorySpeed advisorySpeed2 = new AdvisorySpeed(type);

        assertFalse(advisorySpeed.equals(advisorySpeed2));

        advisorySpeed2.setSpeed(speed);

        assertFalse(advisorySpeed.equals(advisorySpeed2));

        advisorySpeed2.setConfidence(confidence);

        assertFalse(advisorySpeed.equals(advisorySpeed2));

        advisorySpeed2.setDistance(distance);

        assertFalse(advisorySpeed.equals(advisorySpeed2));

        advisorySpeed2.setRestrictedClass(restrictedClass);

        assertTrue(advisorySpeed.equals(advisorySpeed2));
    }
}
