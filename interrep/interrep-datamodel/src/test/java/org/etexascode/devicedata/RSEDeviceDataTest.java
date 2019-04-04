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

import static org.junit.Assert.assertTrue;

import org.etexascode.devicedata.testclasses.TestIDable;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.junit.Test;

/**
 * @author janway
 */
public class RSEDeviceDataTest {

    private static RSEDeviceData dev1 = new RSEDeviceData(1L, null, null, new int[] { 0 }, 2.0, 3.0, 4.0);

    @Test
    public void testConstructor() {
        ReferencePoint[] refPts = new ReferencePoint[1];
        refPts[0] = new ReferencePoint(5.0, 6.0);
        RSEDeviceData dev2 = new RSEDeviceData(8L, null, refPts, new int[] { 7 }, 9.0, 10.0, 11.0);

        assertTrue(dev2.getReferencePoints() != null);
        assertTrue(dev2.getReferencePoints() != refPts);
        assertTrue(dev2.getReferencePoints()[0].getLatitude() == 5.0);
        assertTrue(dev2.getReferencePoints()[0].getLongitude() == 6.0);
    }

    @Test
    public void testGetX() {
        assertTrue(dev1.getX() == 2.0);
    }

    @Test
    public void testGetY() {
        assertTrue(dev1.getY() == 3.0);
    }

    @Test
    public void testGetZ() {
        assertTrue(dev1.getZ() == 4.0);
    }

    @Test
    public void testEqualsId() {
        TestIDable entity = new TestIDable("RSEDeviceData:1");
        assertTrue(dev1.equalsId(entity));
    }

    @Test
    public void testGetProperId() {
        assertTrue("RSEDeviceData:1".equals(dev1.getProperId()));
        assertTrue("RSEDeviceData:1".equals(dev1.getProperId()));
    }

}
