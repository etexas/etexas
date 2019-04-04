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

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import javax.resource.ResourceException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class GenericManagedConnectionFactoryTest {

    SimRequestInfo info = null;

    GenericManagedConnectionFactory gmcf = null;

    @Before
    public void setup() {
        info = new SimRequestInfo("UUID", TestSimInterface.class, new HashMap<String, Object>());
        gmcf = new GenericManagedConnectionFactory();
    }

    @After
    public void tearDown() {
        info = null;
        gmcf = null;
    }

    @Test
    public void testConstructor() {
        GenericManagedConnectionFactory gmcf = new GenericManagedConnectionFactory();
        assertTrue(gmcf instanceof GenericManagedConnectionFactory);
    }

    @Test(expected = ResourceException.class)
    public void testCreateConnectionFactory1() throws ResourceException {
        gmcf.createConnectionFactory();
    }

    @Test
    public void testCreateConnectionFactory2() throws ResourceException {
        GenericFactoryImpl gmc = (GenericFactoryImpl)gmcf.createConnectionFactory(null);
        assertTrue(gmc instanceof GenericFactoryImpl);
    }

    @Test
    public void testCreateManagedConnection1() throws ResourceException {
        GenericManagedConnection gmc = (GenericManagedConnection)gmcf.createManagedConnection(null, info);
    }

    @Test(expected = ResourceException.class)
    public void testCreateManagedConnection2() throws ResourceException {
        GenericManagedConnection gmc = (GenericManagedConnection)gmcf.createManagedConnection(null, null);
    }

    @Test
    public void testGetLogWriter() throws ResourceException {
        assertNull(gmcf.getLogWriter());
    }

    @Test
    public void testMatchManagedConnections1() throws ResourceException {
        assertNull(gmcf.matchManagedConnections(null, null, null));
    }

    @Test
    public void testMatchManagedConnections2() throws ResourceException {
        assertNull(gmcf.matchManagedConnections(new HashSet(), null, info));
    }

    @Test
    public void testMatchManagedConnections3() throws ResourceException {
        Set s = new HashSet();
        s.add("");
        assertNull(gmcf.matchManagedConnections(s, null, info));
    }

    @Test
    public void testMatchManagedConnections4() throws ResourceException {
        Set s = new HashSet();
        GenericManagedConnection gmc = new GenericManagedConnection(gmcf, null, info);
        s.add(gmc);
        assertTrue(gmc == gmcf.matchManagedConnections(s, null, info));
    }

    @Test
    public void testMatchManagedConnections5() throws ResourceException {
        Set s = new HashSet();
        GenericManagedConnection gmc = new GenericManagedConnection(gmcf, null, info);
        s.add(gmc);
        SimRequestInfo info2 = new SimRequestInfo("NOTUUID", TestSimInterface2.class, new HashMap<String, Object>());
        assertNull(gmcf.matchManagedConnections(s, null, info2));
    }

    @Test
    public void testSetWriter() throws ResourceException {
        gmcf.setLogWriter(null);
        assertTrue(true);
    }
}
