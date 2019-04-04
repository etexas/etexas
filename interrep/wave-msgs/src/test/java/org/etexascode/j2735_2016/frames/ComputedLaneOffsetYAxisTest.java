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
 * Unit tests for the computed lane offset y axis frame (Choice).
 * 
 * @author ttevendale
 */
public class ComputedLaneOffsetYAxisTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorDrivenLineOffsetSm() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(-454);

        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(offset);

        assertTrue(offset.equals(offsetYAxis.getSmall()));
        assertNull(offsetYAxis.getLarge());

        thrown.expect(NullPointerException.class);
        new ComputedLaneOffsetYAxis((DrivenLineOffsetSm)null);
    }

    @Test
    public void testConstructorDrivenLineOffsetLg() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(-5141);

        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(offset);

        assertTrue(offset.equals(offsetYAxis.getLarge()));
        assertNull(offsetYAxis.getSmall());

        thrown.expect(NullPointerException.class);
        new ComputedLaneOffsetYAxis((DrivenLineOffsetLg)null);
    }

    @Test
    public void testEncodeUPERDrivenLineOffsetSm() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(1540);
        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(offset);

        String computedLaneOffsetYAxisChoice = "0";
        String remainingBits = offset.encodeUPER();

        assertTrue((computedLaneOffsetYAxisChoice + remainingBits).equals(offsetYAxis.encodeUPER()));
    }

    @Test
    public void testEncodeUPERDrivenLineOffsetLg() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(11998);
        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(offset);

        String computedLaneOffsetYAxisChoice = "1";
        String remainingBits = offset.encodeUPER();

        assertTrue((computedLaneOffsetYAxisChoice + remainingBits).equals(offsetYAxis.encodeUPER()));
    }

    @Test
    public void testEncodeUPERAllNull() {

        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis();
        thrown.expect(IllegalStateException.class);
        offsetYAxis.encodeUPER();
    }

    @Test
    public void testDecodeUPERDrivenLineOffsetSm() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(-1354);

        String computedLaneOffsetYAxisChoice = "0";

        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis();
        offsetYAxis.decodeUPER(computedLaneOffsetYAxisChoice + offset.encodeUPER());

        assertTrue(offset.equals(offsetYAxis.getSmall()));
        assertNull(offsetYAxis.getLarge());
    }

    @Test
    public void testDecodeUPERDrivenLineOffsetLg() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(14454);

        String computedLaneOffsetYAxisChoice = "1";

        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis();
        offsetYAxis.decodeUPER(computedLaneOffsetYAxisChoice + offset.encodeUPER());

        assertTrue(offset.equals(offsetYAxis.getLarge()));
        assertNull(offsetYAxis.getSmall());
    }

    @Test
    public void testDecodeUPERLessBits() {

        String choice = "";

        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis();
        thrown.expect(IllegalArgumentException.class);
        offsetYAxis.decodeUPER(choice);
    }

    @Test
    public void testHashCode() {

        DrivenLineOffsetSm drivenOffset = new DrivenLineOffsetSm(115);
        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(drivenOffset);

        ComputedLaneOffsetYAxis offsetYAxis2 = new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(drivenOffset.getValue() + 1));

        assertFalse(offsetYAxis.hashCode() == offsetYAxis2.hashCode());
        assertTrue(offsetYAxis.hashCode() == offsetYAxis.hashCode());
        assertTrue(offsetYAxis2.hashCode() == offsetYAxis2.hashCode());

        ComputedLaneOffsetYAxis offsetYAxis3 = new ComputedLaneOffsetYAxis(drivenOffset);

        assertTrue(offsetYAxis.hashCode() == offsetYAxis3.hashCode());
        assertFalse(offsetYAxis2.hashCode() == offsetYAxis3.hashCode());
    }

    @Test
    public void testEquals() {

        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(1654));

        assertTrue(offsetYAxis.equals(offsetYAxis));
        assertFalse(offsetYAxis.equals(null));
        assertFalse(offsetYAxis.equals(new String()));

        // different driven offset small
        ComputedLaneOffsetYAxis offsetYAxis2 = new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(offsetYAxis.getSmall().getValue() + 1));
        assertFalse(offsetYAxis.equals(offsetYAxis2));

        // same driven offset small
        offsetYAxis2 = new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(offsetYAxis.getSmall().getValue()));
        assertTrue(offsetYAxis.equals(offsetYAxis2));

        // different driven offset large
        offsetYAxis = new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(145));
        offsetYAxis2 = new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(offsetYAxis.getLarge().getValue() + 1));
        assertFalse(offsetYAxis.equals(offsetYAxis2));

        // same driven offset large
        offsetYAxis2 = new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(offsetYAxis.getLarge().getValue()));
        assertTrue(offsetYAxis.equals(offsetYAxis2));
    }
}
