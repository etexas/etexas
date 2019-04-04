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
import java.rmi.RemoteException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.resource.NotSupportedException;
import javax.resource.ResourceException;
import javax.resource.spi.ConnectionEvent;
import javax.resource.spi.ConnectionEventListener;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.LocalTransaction;
import javax.resource.spi.ManagedConnection;
import javax.resource.spi.ManagedConnectionMetaData;
import javax.security.auth.Subject;
import javax.transaction.xa.XAResource;

import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Managed connection for the Generic Adapter. This class represents a single Simulation execution.
 * This entire class should be package-protected (ablatt: but not all of it is for some reason
 * having to do with extended classes).
 * 
 * @author bbadillo
 * @author ablatt
 */
class GenericManagedConnection implements ManagedConnection {

    /** Size of the byte buffer to use for stream transfers. */
    private static final int BUFFER = 4096;

    /** Logger for convenience. */
    private static final Logger LOGGER = LoggerFactory.getLogger(GenericManagedConnection.class);

    /** A log writer. */
    private PrintWriter logWriter;

    /**
     * A unique identifier for the managed connection that represents a unique eTEXAS simulation.
     */
    private final String uuid;

    /** JCA managed connection factory for Generic RA. */
    private final GenericManagedConnectionFactory mcf;

    /** Underlying simulator. */
    private SimulatorInterface sim;

    /** The set of logical connections to this managed connection. */
    final Set<SimulatorInterface> connectionSet;

    /**
     * Listeners that need to be notified of events on this managed connection.
     */
    private final List<ConnectionEventListener> eventListeners = new LinkedList<ConnectionEventListener>();

    /** Whether or not this connection has been destroyed. */
    private boolean destroyed = false;

    /**
     * Constructor
     * 
     * @param mcf JCA managed connection factory for Generic RA.
     * @param sbjct The subject requesting the connection.
     * @param reqInfo Additional information needed to make the connection.
     */
    GenericManagedConnection(GenericManagedConnectionFactory mcf, Subject sbjct, SimRequestInfo reqInfo) {
        uuid = reqInfo.getUuid();

        this.mcf = mcf;

        try {
            sim = (SimulatorInterface)reqInfo.getSimClass().newInstance();
            sim.init(reqInfo.getConfs());
        }
        catch (InstantiationException e) {
            LOGGER.error("", e);
        }
        catch (IllegalAccessException e) {
            LOGGER.error("", e);
        }
        catch (RemoteException e) {
            LOGGER.error("", e);
        }

        connectionSet = new HashSet<SimulatorInterface>();
    }

    /**
     * Adds an event listener to the connection factory
     * 
     * @param cl The connection event listener to add.
     */
    @Override
    public void addConnectionEventListener(ConnectionEventListener cl) {
        synchronized (eventListeners) {
            if (!eventListeners.contains(cl)) {
                eventListeners.add(cl);
            }
        }
    }

    /**
     * Associates a connection to the simulator.
     * 
     * @param o The connection object to associate.
     * @throws ResourceException
     */
    @Override
    public void associateConnection(Object o) throws ResourceException {
        LOGGER.info("[Generic RA: {}] Associating a connection", uuid);

        if (destroyed) {
            LOGGER.warn("Connection is destroyed");
            throw new IllegalStateException("Connection is destroyed");
        }

        if (o instanceof GenericSimulator) {
            GenericSimulator connectionImpl = (GenericSimulator)o;
            connectionImpl.associateConnection(this);
        }
        else {
            throw new IllegalStateException("Invalid connection");
        }
    }

    /**
     * Clears out the connection set for a simulator.
     * 
     * @throws ResourceException
     */
    @Override
    public void cleanup() throws ResourceException {

        if (destroyed) {
            LOGGER.warn("[Generic RA: {}] Managed Connection is destroyed", uuid);
            throw new IllegalStateException("Managed Connection is destroyed");
        }

        LOGGER.info("[Generic RA: {}] Cleanup for Generic Managed Connection", uuid);

        Iterator it = connectionSet.iterator();
        while (it.hasNext()) {
            GenericSimulator conn = (GenericSimulator)it.next();
            conn.close();
        }
        connectionSet.clear();
    }

    /**
     * Close a logical connection to this managed connection.
     * 
     * @param handle The logical connection to close.
     */
    void closeHandle(GenericSimulator handle) {
        if (destroyed) {
            LOGGER.warn("Connection is destroyed");
            throw new IllegalStateException("Connection is destroyed");
        }

        connectionSet.remove(handle);

        if (connectionSet.isEmpty()) {
            ConnectionEvent ce = new ConnectionEvent(this, ConnectionEvent.CONNECTION_CLOSED);
            ce.setConnectionHandle(handle);
            sendEvent(ce);
        }
    }

    @Override
    public void destroy() throws ResourceException {

        if (destroyed) {
            return;
        }

        cleanup();

        destroyed = true;
        connectionSet.clear();
    }

    @Override
    public Object getConnection(Subject sbjct, ConnectionRequestInfo cri) throws ResourceException {
        LOGGER.info("[Generic RA: {}] Getting the Connection for Managed Connection", uuid);

        if (destroyed) {
            LOGGER.warn("Connection is destroyed");
            throw new IllegalStateException("Connection is destroyed");
        }

        SimulatorInterface conn = new GenericSimulator(this);
        connectionSet.add(conn);

        return conn;
    }

    @Override
    public LocalTransaction getLocalTransaction() throws ResourceException {
        throw new NotSupportedException("This connector does not support transactions");
    }

    @Override
    public PrintWriter getLogWriter() throws ResourceException {
        return logWriter;
    }

    @Override
    public ManagedConnectionMetaData getMetaData() throws ResourceException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    /**
     * Get the eTEXAS simulation driver for this managed connection.
     * 
     * @return The eTEXAS simulation driver for this managed connection.
     */
    SimulatorInterface getSimDriver() {
        return sim;
    }

    /**
     * Get the unique identifier for this managed connection.
     * 
     * @return The unique identifier for this managed connection.
     */
    String getUuid() {
        return uuid;
    }

    @Override
    public XAResource getXAResource() throws ResourceException {
        throw new NotSupportedException("This connector does not support transactions");
    }

    @Override
    public void removeConnectionEventListener(ConnectionEventListener cl) {
        synchronized (eventListeners) {
            eventListeners.remove(cl);
        }
    }

    /**
     * Send an event to listeners.
     * 
     * @param event The event to broadcast.
     */
    private void sendEvent(ConnectionEvent event) {
        synchronized (eventListeners) {
            for (ConnectionEventListener listener : eventListeners) {

                switch (event.getId()) {
                    case ConnectionEvent.CONNECTION_CLOSED:
                        listener.connectionClosed(event);
                        break;
                    case ConnectionEvent.CONNECTION_ERROR_OCCURRED:
                        listener.connectionErrorOccurred(event);
                        break;
                    case ConnectionEvent.LOCAL_TRANSACTION_COMMITTED:
                        listener.localTransactionCommitted(event);
                        break;
                    case ConnectionEvent.LOCAL_TRANSACTION_ROLLEDBACK:
                        listener.localTransactionRolledback(event);
                        break;
                    case ConnectionEvent.LOCAL_TRANSACTION_STARTED:
                        listener.localTransactionStarted(event);
                        break;
                    default:
                        // Unknown event, skip
                }
            }
        }
    }

    @Override
    public void setLogWriter(PrintWriter writer) throws ResourceException {
        logWriter = writer;
    }
}
