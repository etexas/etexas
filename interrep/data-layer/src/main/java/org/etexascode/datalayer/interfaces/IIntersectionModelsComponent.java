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
package org.etexascode.datalayer.interfaces;

import java.util.List;

import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

/**
 * Interface for accessing a container for managing interRep models
 * 
 * @author ablatt
 */
public interface IIntersectionModelsComponent {

    /**
     * Add an interRep model to the container
     * 
     * @param stepNum The step number to associate the interRep model with
     * @param interRepId The id of the interRep the model is coming from
     * @param model The model to add
     */
    public void putInterRepInfoModel(int stepNum, int interRepId, InterRepInfoModel model);

    /**
     * Get the interRep model associated with step num and interRep id
     * 
     * @param stepNum The step num for the model we are interested in
     * @param interRepId The interRep id we are interested in
     * @return The corresponding model
     */
    public InterRepInfoModel getInfoModel(int stepNum, int interRepId);

    /**
     * Get the vehicle manager associated with step num and interRep id
     * 
     * @param stepNum The step num for the manager we are interested in
     * @param interRepId The interRep id we are interested in
     * @return The corresponding vehicle manager
     */
    public IVehicleManager getVehicles(int stepNum, int interRepId);

    /**
     * Get a specific vehicle from the container
     * 
     * @param stepNum The step number the vehicle is associated with
     * @param id The proper id of the vehicle
     * @return The vehicle associated with id (null if there is no such vehicle)
     */
    public IVehicle getVehicleInfoById(int stepNum, String id);

    /**
     * Get the lane a specific vehicle is in
     * 
     * @param stepNum The step associated with the vehicle
     * @param vi The vehicle we want the lane for
     * @return The lane vi is in
     */
    public ILane getLaneInfoByVehicle(int stepNum, IVehicle vi);

    /**
     * Gets the signal indications associated with a lane on a step num
     * 
     * @param stepNum The step num of the model we are looking at
     * @param li The lane we want the signals for
     * @return The signals governing li on step num
     */
    public List<? extends ISignalIndication> getSignalsByLane(int stepNum, ILane li);

    /**
     * Gets the detectors associated with an interRep on a specific step
     * 
     * @param stepNum The step num of the model we are looking at
     * @param interRepId The id of the interRep we are looking at
     * @return The state of the detectors in the requested interRep on stepNum
     */
    public Iterable<? extends IDetector> getDetectors(int stepNum, int interRepId);
}
