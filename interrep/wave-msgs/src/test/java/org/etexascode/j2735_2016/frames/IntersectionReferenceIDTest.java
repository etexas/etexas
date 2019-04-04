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

import org.etexascode.j2735_2016.elements.IntersectionID;
import org.etexascode.j2735_2016.elements.RoadRegulatorID;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the intersection reference ID frame.
 * 
 * @author ttevendale
 */
public class IntersectionReferenceIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    IntersectionReferenceID referenceId;

    @Before
    public void init() {

        RoadRegulatorID region = new RoadRegulatorID(49);
        IntersectionID id = new IntersectionID(1);

        referenceId = new IntersectionReferenceID(id);
        referenceId.setRegion(region);
    }

    @Test
    public void testConstructor() {

        IntersectionID id = new IntersectionID(98);

        IntersectionReferenceID referenceId = new IntersectionReferenceID(id);

        assertNull(referenceId.getRegion());
        assertTrue(id.equals(referenceId.getId()));

        thrown.expect(NullPointerException.class);
        referenceId = new IntersectionReferenceID(null);
    }

    @Test
    public void testConstructorPrimitive() {

        int id = 111;

        IntersectionReferenceID referenceId = new IntersectionReferenceID(id);

        assertTrue(id == referenceId.getId().getValue());
    }

    @Test
    public void testSetRegionPrimitive() {

        int region = 7;

        IntersectionReferenceID referenceId = new IntersectionReferenceID();
        referenceId.setRegion(region);

        assertTrue(region == referenceId.getRegion().getValue());

        region = 42;

        referenceId.setRegion(region);

        assertTrue(region == referenceId.getRegion().getValue());
    }

    @Test
    public void testSetId() {

        IntersectionID id = new IntersectionID(987);

        IntersectionReferenceID referenceId = new IntersectionReferenceID();
        referenceId.setId(id);

        assertTrue(id.equals(referenceId.getId()));

        thrown.expect(NullPointerException.class);
        referenceId.setId(null);
    }

    @Test
    public void testSetIdPrimitive() {

        int id = 999;

        IntersectionReferenceID referenceId = new IntersectionReferenceID();
        referenceId.setId(id);

        assertTrue(id == referenceId.getId().getValue());
    }

    @Test
    public void testEncodeUPERMin() {

        IntersectionID id = new IntersectionID(1);

        IntersectionReferenceID referenceId = new IntersectionReferenceID(id);

        String referenceIdOptionals = "0";
        String remainingBits = id.encodeUPER();
        assertTrue((referenceIdOptionals + remainingBits).equals(referenceId.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        RoadRegulatorID region = new RoadRegulatorID(8797);
        IntersectionID id = new IntersectionID(100);

        IntersectionReferenceID referenceId = new IntersectionReferenceID(id);
        referenceId.setRegion(region);

        String referenceIdOptionals = "1";
        String remainingBits = region.encodeUPER() + id.encodeUPER();
        assertTrue((referenceIdOptionals + remainingBits).equals(referenceId.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        IntersectionID id = new IntersectionID(321);

        String referenceIdOptionals = "0";

        IntersectionReferenceID referenceId = new IntersectionReferenceID();
        String remainingBits = referenceId.decodeUPER(referenceIdOptionals + id.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(referenceId.getRegion());
        assertTrue(id.equals(referenceId.getId()));
    }

    @Test
    public void testDecodeUPERMax() {

        RoadRegulatorID region = new RoadRegulatorID(1111);
        IntersectionID id = new IntersectionID(546);

        String referenceIdOptionals = "1";

        IntersectionReferenceID referenceId = new IntersectionReferenceID();
        String remainingBits = referenceId.decodeUPER(referenceIdOptionals + region.encodeUPER() + id.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(region.equals(referenceId.getRegion()));
        assertTrue(id.equals(referenceId.getId()));
    }

    @Test
    public void testDecodeUPERLessBits() {

        String referenceIdOptionals = "";

        IntersectionReferenceID referenceId = new IntersectionReferenceID();
        thrown.expect(IllegalArgumentException.class);
        referenceId.decodeUPER(referenceIdOptionals);
    }

    @Test
    public void testHashCode() {

        int region = referenceId.getRegion().getValue();
        int id = referenceId.getId().getValue();

        IntersectionReferenceID referenceId2 = new IntersectionReferenceID(id + 1);
        referenceId2.setRegion(region + 1);

        assertFalse(referenceId.hashCode() == referenceId2.hashCode());
        assertTrue(referenceId.hashCode() == referenceId.hashCode());
        assertTrue(referenceId2.hashCode() == referenceId2.hashCode());

        IntersectionReferenceID referenceId3 = new IntersectionReferenceID(id);
        referenceId3.setRegion(region);

        assertTrue(referenceId.hashCode() == referenceId3.hashCode());
        assertFalse(referenceId2.hashCode() == referenceId3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(referenceId.equals(referenceId));
        assertFalse(referenceId.equals(null));
        assertFalse(referenceId.equals(new String()));

        int region = referenceId.getRegion().getValue();
        int id = referenceId.getId().getValue();

        // different
        IntersectionReferenceID referenceId2 = new IntersectionReferenceID(id + 1);
        referenceId2.setRegion(region + 1);

        assertFalse(referenceId.equals(referenceId2));

        // different region
        referenceId2 = new IntersectionReferenceID(id);
        referenceId2.setRegion(region + 1);

        assertFalse(referenceId.equals(referenceId2));

        // different id
        referenceId2 = new IntersectionReferenceID(id + 1);
        referenceId2.setRegion(region);

        assertFalse(referenceId.equals(referenceId2));

        // same
        referenceId2 = new IntersectionReferenceID(id);
        referenceId2.setRegion(region);

        assertTrue(referenceId.equals(referenceId2));
    }

    @Test
    public void testEqualsNull() {

        int region = referenceId.getRegion().getValue();
        int id = referenceId.getId().getValue();

        IntersectionReferenceID referenceId2 = new IntersectionReferenceID(id);

        assertFalse(referenceId.equals(referenceId2));

        referenceId2.setRegion(region);

        assertTrue(referenceId.equals(referenceId2));
    }
}
