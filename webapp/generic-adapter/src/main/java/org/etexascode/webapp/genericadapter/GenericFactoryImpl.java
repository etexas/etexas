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
import java.util.Map;

import javax.naming.NamingException;
import javax.naming.Reference;
import javax.resource.Referenceable;
import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;

import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.webapp.ra.api.SimFactory;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException.SimResourceErrorType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Factory for creating simulations.
 * 
 * @author ablatt
 */
public class GenericFactoryImpl implements SimFactory, Serializable, Referenceable {

    /** Serialization number. */
    private static final long serialVersionUID = 7151005166469975536L;

    /** Logger for convenience. */
    private static final Logger LOGGER = LoggerFactory.getLogger(GenericFactoryImpl.class);

    /** JCA managed connection factory for Playback RA. */
    private GenericManagedConnectionFactory managedConnectionFactory;

    /** JCA connection manager. */
    private ConnectionManager connectionManager;

    /**
     * Default constructor
     */
    public GenericFactoryImpl() {}

    /**
     * Constructor used by Glassfish. //TODO:should this be changed to WildFly?
     * 
     * @param mcf The JCA managed connection factory for Plaback RA.
     * @param cm The JCA connection manager.
     */
    public GenericFactoryImpl(GenericManagedConnectionFactory mcf, ConnectionManager cm) {
        LOGGER.info("Constructing an Generic Connection Factory");

        managedConnectionFactory = mcf;
        connectionManager = cm;

    }

    /**
     * Creates a simulator interface.
     * 
     * @param uuid The UUID for the simulator.
     * @param simClass The class for the simulation to run.
     * @param confs The configuration file to establish the simulation
     * @return The simulator interface.
     * @throws SimResourceException If a simulation resource exception occurs.
     */
    @SuppressWarnings("rawtypes")
    @Override
    public SimulatorInterface createSim(String uuid, Class simClass, Map<String, Object> confs) throws SimResourceException {
        SimRequestInfo reqInfo = new SimRequestInfo(uuid, simClass, confs);
        try {
            return (SimulatorInterface)connectionManager.allocateConnection(managedConnectionFactory, reqInfo);
        }
        catch (ResourceException e) {
            LOGGER.error("Resource Exception:", e);
            throw new SimResourceException(e, SimResourceErrorType.ALLOCATE);
        }
    }

    @Override
    public Reference getReference() throws NamingException {
        // THIS METHOD NEEDED FOR JBOSS J2EE DEPLOYMENT.
        return null;
    }

    @Override
    public void setReference(Reference arg0) {
        // THIS METHOD NEEDED FOR JBOSS J2EE DEPLOYMENT.
    }
}
