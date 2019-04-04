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

package org.etexascode.webapp.genericadapter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import javax.resource.ResourceException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class SimResourceAdapterImplTest {

    SimResourceAdapterImpl srai = null;

    @Before
    public void setup() {
        srai = new SimResourceAdapterImpl();
    }

    @After
    public void tearDown() {
        srai = null;
    }

    @Test
    public void testConstructor() {
        SimResourceAdapterImpl srai = new SimResourceAdapterImpl();
        assertTrue(srai instanceof SimResourceAdapterImpl);
    }

    @Test
    public void testGetSetRmiPort() {
        int p = 1337;
        srai.setRmiPort(p);
        assertEquals(p, srai.getRmiPort());
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testEndpointActivation() {
        try {
            srai.endpointActivation(null, null);
        }
        catch (ResourceException e) {
            e.printStackTrace();
        }
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testEndpointDeactivation() {
        srai.endpointDeactivation(null, null);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testGetXAResources() {
        try {
            srai.getXAResources(null);
        }
        catch (ResourceException e) {
            e.printStackTrace();
        }
    }
}
