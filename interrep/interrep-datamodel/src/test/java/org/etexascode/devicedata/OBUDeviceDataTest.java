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
import org.etexascode.devicedata.testclasses.TestIDable;
import org.junit.Test;

/**
 * @author janway
 */
public class OBUDeviceDataTest {

    private static OBUDeviceData dev1 = new OBUDeviceData(0, null, "1");

    private static OBUDeviceData dev2 = new OBUDeviceData(0, null, "1");

    private static OBUDeviceData dev3 = new OBUDeviceData(0, null, "2");

    @Test
    public void testEquals() {
        assertTrue(dev1.equals(dev2));
        assertFalse(dev1.equals(dev3));
        assertFalse(dev1.equals(new Object()));
    }

    @Test
    public void testHashCode() {
        int dev1Code = new HashCodeBuilder(995, 387).append(dev1.vehicleId).hashCode();
        assertTrue(dev1.hashCode() == dev1Code);
        assertFalse(dev3.hashCode() == dev1Code);
        assertTrue(dev2.hashCode() == dev1Code);
    }

    @Test
    public void testEqualsId() {
        TestIDable entity = new TestIDable("2");
        assertTrue(dev3.equalsId(entity));
        assertFalse(dev2.equalsId(entity));
    }

    @Test
    public void testGetProperId() {
        assertTrue("1".equals(dev1.getProperId()));
    }

    @Test
    public void testGetApps() {
        assertTrue(dev1.getApps() == null);
    }
}
