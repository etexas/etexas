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
package org.etexascode.datalayer.inmemory;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.datalayer.interfaces.IIntersectionModelsComponent;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

/**
 * A component which manages the intersection model for a single intersection implementation.
 * 
 * @author ablatt
 */
public class SingleIntersectionModelsComponent implements IIntersectionModelsComponent {

    /**
     * The representation of the intersection at this specific time step.
     */
    InterRepInfoModel model = null;

    /**
     * Adds the interrep information model to the intersection model
     * 
     * @param stepNum The current step number
     * @param interRepId The id of the interrep
     * @param model The interrep information model
     */
    @Override
    public void putInterRepInfoModel(int stepNum, int interRepId, InterRepInfoModel model) {
        this.model = model;
    }

    /**
     * Gets the vehicles for the intersection model
     * 
     * @param stepNum The current step number
     * @param interRepId The intersection representation id
     * @return The vehicle manager in the model
     */
    @Override
    public IVehicleManager getVehicles(int stepNum, int interRepId) {
        if (model == null) {
            return new VehicleManager();
        }
        return model.vmi;
    }

    /**
     * Gets vehicle information from the intersection model
     * 
     * @param stepNum The current step number
     * @param id The id of the vehicle to be returned
     * @return model The vehicle information for the vehicle id provided
     */
    @Override
    public IVehicle getVehicleInfoById(int stepNum, String id) {
        return model.vmi.getVehicle(id);
    }

    /**
     * Gets lane information from the vehicle provided
     * 
     * @param stepNum The current step number
     * @param vi The vehicle from which to pull the lane information
     * @return model The lane information for the specified vehicle
     */
    @Override
    public ILane getLaneInfoByVehicle(int stepNum, IVehicle vi) {
        return model.lmi.getLaneById(vi.getLaneID());
    }

    /**
     * Gets signal information from the lane provided
     * 
     * @param stepNum The current step number
     * @param li The lane from which to pull the signal information
     * @return model The signal information for the specified lane
     */
    @Override
    public List<? extends ISignalIndication> getSignalsByLane(int stepNum, ILane li) {
        return model.smi.getSignalsByLaneId(li.getLaneId());
    }

    /**
     * Gets the intersection model information
     * 
     * @param stepNum The current step number
     * @param interRepId The interrep id for the model
     * @return model The intersection model information
     */
    @Override
    public InterRepInfoModel getInfoModel(int stepNum, int interRepId) {
        return model;
    }

    @Override
    public Iterable<? extends IDetector> getDetectors(int stepNum, int interRepId) {
        if ((model == null) || (model.dmi == null)) {
            return new ArrayList<IDetector>(0);
        }
        return model.dmi;
    }
}
