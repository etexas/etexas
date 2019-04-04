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

import java.rmi.RemoteException;
import java.util.List;
import java.util.Map;

import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.SimulatorMessage;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Simulator wrapper returned by GenericManagedConnection.
 * 
 * @author ablatt
 */
public class GenericSimulator implements SimulatorInterface {

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(GenericSimulator.class);

    /**
     * JCA managed connection for the eTEXAS simulation process.
     */
    GenericManagedConnection mc;

    /**
     * Constructor
     * 
     * @param mc JCA managed connection for the eTEXAS simulation process.
     */
    public GenericSimulator(GenericManagedConnection mc) {

        this.mc = mc;
    }

    /**
     * Remove the underlying managed connection from this object.
     */
    void invalidate() {

        mc = null;
    }

    /**
     * Associate a new underlying managed connection with this object.
     * 
     * @param mc JCA managed connection for the eTEXAS simulation process.
     */
    void associateConnection(GenericManagedConnection mc) {

        this.mc = mc;
    }

    @Override
    public void close() {

        if (mc != null) {

            try {

                mc.getSimDriver().close();
            }
            catch (RemoteException e) {

                e.printStackTrace();
            }
            mc.closeHandle(this);
        }
        mc = null;
    }

    @CoberturaIgnore
    @Override
    public StaticData getStaticData() throws RemoteException {

        return mc.getSimDriver().getStaticData();
    }

    @CoberturaIgnore
    @Override
    public StepData getStepData(long stepNum) throws RemoteException {

        return mc.getSimDriver().getStepData(stepNum);
    }

    @CoberturaIgnore
    @Override
    public void addVehicleCommand(VehicleCommand command) throws RemoteException {

        mc.getSimDriver().addVehicleCommand(command);
    }

    @CoberturaIgnore
    @Override
    public void addSignalCommand(SignalCommand command) throws RemoteException {

        mc.getSimDriver().addSignalCommand(command);
    }

    @CoberturaIgnore
    @Override
    public void addVehicleInjectionRequest(VehicleInjectionRequest request) throws RemoteException {

        mc.getSimDriver().addVehicleInjectionRequest(request);
    }

    @CoberturaIgnore
    @Override
    public void init(Map<String, Object> conf) throws RemoteException {}

    @CoberturaIgnore
    @Override
    public List<SimulatorMessage> checkForErrorOutput(String execId) throws RemoteException {

        return mc.getSimDriver().checkForErrorOutput(execId);
    }

    @CoberturaIgnore
    @Override
    public List<SimulatorMessage> checkForWarningOutput(String execId) throws RemoteException {

        return mc.getSimDriver().checkForWarningOutput(execId);
    }
}
