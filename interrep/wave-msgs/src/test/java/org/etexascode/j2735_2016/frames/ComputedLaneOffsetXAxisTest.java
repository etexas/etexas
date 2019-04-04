/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
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

import org.etexascode.j2735_2016.elements.DrivenLineOffsetLg;
import org.etexascode.j2735_2016.elements.DrivenLineOffsetSm;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the computed lane offset x axis frame (Choice).
 * 
 * @author ttevendale
 */
public class ComputedLaneOffsetXAxisTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorDrivenLineOffsetSm() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(151);

        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(offset);

        assertTrue(offset.equals(offsetXAxis.getSmall()));
        assertNull(offsetXAxis.getLarge());

        thrown.expect(NullPointerException.class);
        new ComputedLaneOffsetXAxis((DrivenLineOffsetSm)null);
    }

    @Test
    public void testConstructorDrivenLineOffsetLg() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(5141);

        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(offset);

        assertTrue(offset.equals(offsetXAxis.getLarge()));
        assertNull(offsetXAxis.getSmall());

        thrown.expect(NullPointerException.class);
        new ComputedLaneOffsetXAxis((DrivenLineOffsetLg)null);
    }

    @Test
    public void testEncodeUPERDrivenLineOffsetSm() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(-154);
        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(offset);

        String computedLaneOffsetXAxisChoice = "0";
        String remainingBits = offset.encodeUPER();

        assertTrue((computedLaneOffsetXAxisChoice + remainingBits).equals(offsetXAxis.encodeUPER()));
    }

    @Test
    public void testEncodeUPERDrivenLineOffsetLg() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(998);
        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(offset);

        String computedLaneOffsetXAxisChoice = "1";
        String remainingBits = offset.encodeUPER();

        assertTrue((computedLaneOffsetXAxisChoice + remainingBits).equals(offsetXAxis.encodeUPER()));
    }

    @Test
    public void testEncodeUPERAllNull() {

        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis();
        thrown.expect(IllegalStateException.class);
        offsetXAxis.encodeUPER();
    }

    @Test
    public void testDecodeUPERDrivenLineOffsetSm() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(354);

        String computedLaneOffsetXAxisChoice = "0";

        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis();
        offsetXAxis.decodeUPER(computedLaneOffsetXAxisChoice + offset.encodeUPER());

        assertTrue(offset.equals(offsetXAxis.getSmall()));
        assertNull(offsetXAxis.getLarge());
    }

    @Test
    public void testDecodeUPERDrivenLineOffsetLg() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(-4454);

        String computedLaneOffsetXAxisChoice = "1";

        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis();
        offsetXAxis.decodeUPER(computedLaneOffsetXAxisChoice + offset.encodeUPER());

        assertTrue(offset.equals(offsetXAxis.getLarge()));
        assertNull(offsetXAxis.getSmall());
    }

    @Test
    public void testDecodeUPERLessBits() {

        String choice = "";

        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis();
        thrown.expect(IllegalArgumentException.class);
        offsetXAxis.decodeUPER(choice);
    }

    @Test
    public void testHashCode() {

        DrivenLineOffsetSm drivenOffset = new DrivenLineOffsetSm(955);
        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(drivenOffset);

        ComputedLaneOffsetXAxis offsetXAxis2 = new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(drivenOffset.getValue() + 1));

        assertFalse(offsetXAxis.hashCode() == offsetXAxis2.hashCode());
        assertTrue(offsetXAxis.hashCode() == offsetXAxis.hashCode());
        assertTrue(offsetXAxis2.hashCode() == offsetXAxis2.hashCode());

        ComputedLaneOffsetXAxis offsetXAxis3 = new ComputedLaneOffsetXAxis(drivenOffset);

        assertTrue(offsetXAxis.hashCode() == offsetXAxis3.hashCode());
        assertFalse(offsetXAxis2.hashCode() == offsetXAxis3.hashCode());
    }

    @Test
    public void testEquals() {

        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(-654));

        assertTrue(offsetXAxis.equals(offsetXAxis));
        assertFalse(offsetXAxis.equals(null));
        assertFalse(offsetXAxis.equals(new String()));

        // different driven offset small
        ComputedLaneOffsetXAxis offsetXAxis2 = new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(offsetXAxis.getSmall().getValue() + 1));
        assertFalse(offsetXAxis.equals(offsetXAxis2));

        // same driven offset small
        offsetXAxis2 = new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(offsetXAxis.getSmall().getValue()));
        assertTrue(offsetXAxis.equals(offsetXAxis2));

        // different driven offset large
        offsetXAxis = new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(12345));
        offsetXAxis2 = new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(offsetXAxis.getLarge().getValue() + 1));
        assertFalse(offsetXAxis.equals(offsetXAxis2));

        // same driven offset large
        offsetXAxis2 = new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(offsetXAxis.getLarge().getValue()));
        assertTrue(offsetXAxis.equals(offsetXAxis2));
    }
}
