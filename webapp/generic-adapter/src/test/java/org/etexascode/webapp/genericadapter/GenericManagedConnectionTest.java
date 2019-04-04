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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;

import javax.resource.NotSupportedException;
import javax.resource.ResourceException;
import javax.resource.spi.ConnectionEventListener;

import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class GenericManagedConnectionTest {

    SimRequestInfo info = null;

    GenericManagedConnection gmc = null;

    String uuid = null;

    @Before
    public void setup() {
        uuid = "here";
        info = new SimRequestInfo(uuid, TestSimInterface.class, new HashMap<String, Object>());
        gmc = new GenericManagedConnection(null, null, info);
    }

    @After
    public void teardown() {
        info = null;
        gmc = null;
        uuid = null;
    }

    @Test
    public void testConstructor() {
        GenericManagedConnection gmc = new GenericManagedConnection(null, null, info);
        assertTrue(gmc instanceof GenericManagedConnection);
    }

    @Test
    public void testAssociateConnection1() throws ResourceException {
        GenericSimulator gs = new GenericSimulator(gmc);
        gs.associateConnection(null);

        gmc.associateConnection(gs);

        assertTrue(gmc == gs.mc);
    }

    @Test(expected = IllegalStateException.class)
    public void testAssociateConnection2() throws ResourceException {
        gmc.destroy();
        gmc.associateConnection(null);
    }

    @Test(expected = IllegalStateException.class)
    public void testAssociateConnection3() throws ResourceException {
        gmc.associateConnection(null);
    }

    @Test(expected = IllegalStateException.class)
    public void testCleanup1() throws ResourceException {
        gmc.destroy();
        gmc.cleanup();
    }

    @Test
    public void testCleanup2() throws ResourceException {
        gmc.getConnection(null, null);
        assertEquals(1, gmc.connectionSet.size());
        gmc.cleanup();
        assertEquals(0, gmc.connectionSet.size());
    }

    @Test
    public void testCloseHandle1() throws ResourceException {
        GenericSimulator gs = (GenericSimulator)gmc.getConnection(null, null);
        gmc.closeHandle(gs);
        assertEquals(0, gmc.connectionSet.size());
    }

    @Test(expected = IllegalStateException.class)
    public void testCloseHandle2() throws ResourceException {
        gmc.destroy();
        gmc.closeHandle(null);
    }

    @Test
    public void testGetSetLogWriter() throws ResourceException {
        gmc.setLogWriter(null);
        assertNull(gmc.getLogWriter());
    }

    @Test(expected = NotSupportedException.class)
    public void testGetXAResource() throws ResourceException {
        gmc.getXAResource();
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testGetMetaData() throws ResourceException {
        gmc.getMetaData();
    }

    @Test(expected = NotSupportedException.class)
    public void testGetLocalTransaction() throws ResourceException {
        gmc.getLocalTransaction();
    }

    @Test
    public void testGetSimDriver() {
        SimulatorInterface si = gmc.getSimDriver();
        assertTrue(si instanceof TestSimInterface);
    }

    @Test
    public void testGetUuid() {
        assertTrue(uuid == gmc.getUuid());
    }

    @Test
    public void testDestroyAutoReturn() throws ResourceException {
        gmc.destroy();
        gmc.destroy();
    }

    @Test
    public void testGetConnection1() throws ResourceException {
        SimulatorInterface si = (SimulatorInterface)gmc.getConnection(null, null);
        assertTrue(si instanceof GenericSimulator);
    }

    @Test(expected = IllegalStateException.class)
    public void testGetConnection2() throws ResourceException {
        gmc.destroy();
        gmc.getConnection(null, null);
    }

    @Test
    public void testAddRemoveListener() {
        ConnectionEventListener cl = new TestConnectionEventListener();
        gmc.addConnectionEventListener(cl);
        gmc.removeConnectionEventListener(cl);
        assertTrue(true);
    }
}
