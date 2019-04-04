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

import java.io.Serializable;
import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.BootstrapContext;
import javax.resource.spi.ResourceAdapter;
import javax.resource.spi.ResourceAdapterInternalException;
import javax.resource.spi.endpoint.MessageEndpointFactory;
import javax.transaction.xa.XAResource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Resource Adapter for the Generic Adapter.
 * 
 * @author bbadillo
 * @author ablatt
 */
public class SimResourceAdapterImpl implements ResourceAdapter, Serializable {

    /**
     * Registry for remote RMI objects.
     */
    private Registry registry; // XXX: bbadillo - need to find out if this is serializable.

    /**
     * The port for the RMI registry to use.
     */
    private int rmiPort;

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(SimResourceAdapterImpl.class);

    @Override
    public void start(BootstrapContext bc) throws ResourceAdapterInternalException {
        rmiPort = 1099;

        try {
            registry = LocateRegistry.createRegistry(rmiPort);
        }
        catch (RemoteException ex) {
            throw new ResourceAdapterInternalException(ex);
        }
    }

    @Override
    public void stop() {
        LOGGER.info("Stopping Generic Resource Adapter...");
        String msg = "RMI registry could not be closed.";
        try {
            boolean registryClosed = UnicastRemoteObject.unexportObject(registry, true);
            if (registryClosed) {
                LOGGER.info("Generic RMI registry successfully closed.");
            }
            else {
                LOGGER.debug(msg);
            }
        }
        catch (NoSuchObjectException ex) {
            LOGGER.debug(msg, ex);
        }
    }

    @Override
    public void endpointActivation(MessageEndpointFactory mef, ActivationSpec as) throws ResourceException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void endpointDeactivation(MessageEndpointFactory mef, ActivationSpec as) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public XAResource[] getXAResources(ActivationSpec[] ass) throws ResourceException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    /**
     * Get the port for the RMI registry to use.
     * 
     * @return The port for the RMI registry to use.
     */
    public int getRmiPort() {
        return rmiPort;
    }

    /**
     * Set the port for the RMI registry to use.
     * 
     * @param rmiPort The port for the RMI registry to use.
     */
    public void setRmiPort(int rmiPort) {
        this.rmiPort = rmiPort;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof SimResourceAdapterImpl) {
            SimResourceAdapterImpl rai = (SimResourceAdapterImpl)obj;
            if (rai == this)
                return true;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 3738452;
        // Random hash code.
    }
}
