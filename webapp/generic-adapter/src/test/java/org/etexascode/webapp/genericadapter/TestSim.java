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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.SimulatorMessage;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;

/**
 * @author ablatt
 */
public class TestSim implements SimulatorInterface {

    public boolean isInited = false;

    public boolean gotStatic = false;

    public boolean gotStep = false;

    public boolean isVehDat = false;

    public boolean isSigDat = false;

    public boolean isClosed = false;

    public boolean isVehCom = false;

    public boolean isSigCom = false;

    public boolean isVehReq = false;

    @Override
    public void init(Map<String, Object> conf) throws RemoteException {
        isInited = true;
    }

    @Override
    public StaticData getStaticData() throws RemoteException {
        gotStatic = true;
        return null;
    }

    @Override
    public StepData getStepData(long stepNum) throws RemoteException {
        gotStep = true;
        return null;
    }

    @Override
    public void close() throws RemoteException {
        isClosed = true;
    }

    @Override
    public void addVehicleCommand(VehicleCommand command) throws RemoteException {
        isVehCom = true;
    }

    @Override
    public void addSignalCommand(SignalCommand command) throws RemoteException {
        isSigCom = true;
    }

    @Override
    public void addVehicleInjectionRequest(VehicleInjectionRequest request) throws RemoteException {
        isVehReq = true;
    }

    @Override
    public List<SimulatorMessage> checkForErrorOutput(String execId) throws RemoteException {
        return new ArrayList<SimulatorMessage>();
    }

    @Override
    public List<SimulatorMessage> checkForWarningOutput(String simDirectory) throws RemoteException {
        return new ArrayList<SimulatorMessage>();
    }
}
