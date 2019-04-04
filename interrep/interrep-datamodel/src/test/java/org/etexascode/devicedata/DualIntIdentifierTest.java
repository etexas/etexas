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
package org.etexascode.devicedata;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.junit.Test;

/**
 * @author janway
 */
public class DualIntIdentifierTest {

    private static final DualIntIdentifier id1 = new DualIntIdentifier(1, 2, false);

    private static final DualIntIdentifier id2 = new DualIntIdentifier(3, 4, false);

    private static final DualIntIdentifier id3 = new DualIntIdentifier(0, 0, true);

    @Test
    public void testHashCode() {
        assertTrue(id3.hashCode() == 0);
        assertTrue(id1.hashCode() == new HashCodeBuilder(1729, 831).append(1).append(2).append(false).toHashCode());
    }

    @Test
    public void testEquals() {
        assertFalse(id3.equals(id1));
        assertFalse(id1.equals(id2));
        assertTrue(id1.equals(id1));
        assertFalse(id1.equals(new Object()));
    }
}
