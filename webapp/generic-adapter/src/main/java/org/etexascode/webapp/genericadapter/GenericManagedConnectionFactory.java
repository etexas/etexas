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

import java.io.PrintWriter;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.ManagedConnection;
import javax.resource.spi.ManagedConnectionFactory;
import javax.security.auth.Subject;

/**
 * Connection Factory for the Generic Adapter. This primarily concerns itself with trying to match
 * requests with pre-existing connections, and creating new connections whenever it fails to find
 * matching connections.
 * 
 * @author ablatt
 */
public class GenericManagedConnectionFactory implements ManagedConnectionFactory {

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(GenericManagedConnectionFactory.class);

    /**
     * Default constructor used by Glassfish.
     */
    public GenericManagedConnectionFactory() {
        LOGGER.info("[Generic RA] Instantiating a GenericManagedConnectionFactory...");
    }

    @Override
    public Object createConnectionFactory() throws ResourceException {
        LOGGER.info("[Generic RA] Creating an GenericManagedConnectionFactory without container connection manager...");

        throw new ResourceException("[Generic RA] Unmanaged connections not supported.");
    }

    @Override
    public Object createConnectionFactory(ConnectionManager arg0) throws ResourceException {
        LOGGER.info("[Generic RA] Creating a GenericManagedConnectionFactory...");

        return new GenericFactoryImpl(this, arg0);
    }

    @Override
    public ManagedConnection createManagedConnection(Subject sbjct, ConnectionRequestInfo reqInfo) throws ResourceException {
        LOGGER.info("[Generic RA] Creating a new GenericManagedConnection...");

        if (reqInfo instanceof SimRequestInfo) {
            return new GenericManagedConnection(this, sbjct, (SimRequestInfo)reqInfo);
        }

        throw new ResourceException("[Generic RA] Could not create Sim Managed Connected.");
    }

    @Override
    public PrintWriter getLogWriter() throws ResourceException {
        return null;
    }

    @SuppressWarnings("rawtypes")
    @Override
    public ManagedConnection matchManagedConnections(Set set, Subject arg1, ConnectionRequestInfo reqInfo) throws ResourceException {
        LOGGER.info("[Generic RA] Matching a GenericManagedConnection...");

        if (reqInfo instanceof SimRequestInfo) {
            SimRequestInfo simReqInfo = (SimRequestInfo)reqInfo;
            for (Object o : set) {
                if (o instanceof GenericManagedConnection) {
                    GenericManagedConnection conn = (GenericManagedConnection)o;
                    if (simReqInfo.getUuid().equals(conn.getUuid())) {
                        return conn;
                    }
                }
            }
        }

        LOGGER.info("[Generic RA] No match found.");
        return null;
    }

    @Override
    public void setLogWriter(PrintWriter arg0) throws ResourceException {}

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof GenericManagedConnectionFactory) {
            GenericManagedConnectionFactory mcf = (GenericManagedConnectionFactory)obj;
            if (mcf == this)
                return true;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 3738451;
        // Random hash code.
    }
}
