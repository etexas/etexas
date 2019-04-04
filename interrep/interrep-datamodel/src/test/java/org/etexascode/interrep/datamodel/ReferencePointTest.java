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

package org.etexascode.interrep.datamodel;

import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test Point with latitude and longitude
 * 
 * @author bmauldon
 */
public class ReferencePointTest {

    double latitude = 37.8657;

    double longitude = 10.6007;

    double delta = 0.0001;

    ReferencePoint rp = new ReferencePoint(latitude, longitude);

    ReferencePoint rptest = new ReferencePoint(latitude, longitude);

    @Before
    public void setUp() throws Exception {

    }

    @After
    public void tearDown() throws Exception {
        rptest = rp;
    }

    @Test
    public void testGetLatitude() {
        assertEquals(latitude, rptest.getLatitude(), delta);
    }

    @Test
    public void testGetLongitude() {
        assertEquals(longitude, rptest.getLongitude(), delta);
    }

    @Test
    public void testSetLatitude() {
        rptest.setLatitude(latitude - 1.00);
        assertEquals(latitude - 1.00, rptest.getLatitude(), delta);

    }

    @Test
    public void testSetLongitude() {
        rptest.setLongitude(longitude - 1.00);
        assertEquals(longitude - 1.00, rptest.getLongitude(), delta);
    }

    @Test
    public void testEquals() {
        assertEquals(rp, rptest);
        rptest.setLatitude(.1);
        assertNotEquals(rp, rptest);
        rptest.setLatitude(latitude);
        rptest.setLongitude(.2);
        assertNotEquals(rp, rptest);
        assertFalse(rp.equals(new String()));
    }

}
