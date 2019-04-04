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

import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;

/**
 * Interface of the Data Layer used by the Apps Layer
 * 
 * @author ablatt
 */
public interface IAppsDataLayer {

    /**
     * Get the inputs to the Apps Layer
     * 
     * @param stepNum The step number which the Data Layer should use
     * @return The apps the app layer should execute (along with the relevant data for those apps)
     */
    public List<AppLayerInput> getAppData(int stepNum);

    /**
     * Add the outputs of the Apps Layer to the Data Layer
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param appOutputs The outputs of the Apps Layer
     */
    public void putAppOutputs(int stepNum, List<AppLayerOutput> appOutputs);

    /**
     * Get the current time of the simulation
     * 
     * @param stepNum The step number which the Data Layer should use
     * @return The time in the simulation (in seconds)
     */
    public double getSimTime(int stepNum);

}
