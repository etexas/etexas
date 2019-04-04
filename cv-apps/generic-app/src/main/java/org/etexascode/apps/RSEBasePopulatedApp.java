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
package org.etexascode.apps;

import java.util.Collection;

import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;

/**
 * InterRepPopulatedApp automatically populates an InterRep for the whomever is over-riding it. It
 * is important to note that the InterRep provided in performUpdate is not the true underlying model
 * but rather an estimation of that model given the messages received from other apps.
 * 
 * @author ablatt
 */
public abstract class RSEBasePopulatedApp implements IRSEBaseApp {

    /**
     * Estimates the state of the intersection.
     */
    EstimatedDataModel model = null;

    /** The ID of the intersection to model. */
    @AppConfigProperty(value = "0", description = "The ID of the intersection model.")
    protected int intersection;

    /**
     * The calculator type you would like to use. Defaults to 1 (or Geodetic2D). Options: Geodetic
     * 2D: 1 Spherical: 2 Geodetic 3D: 3 Cartesian: 4
     * 
     * @return The calculator type.
     */
    public int getGeoCalcType() {
        return UtilsLatLongConversion.GEODETIC2D;
    }

    /**
     * The size of the logout zone for vehicles in centimeters. Defaults to 3000 cm. You must
     * override this method if you want it to be set to something else.
     * 
     * @return The logout zone size in cm.
     */
    public int getLogoutZone() {
        return 3000;
    }

    @Override
    public void performUpdate(RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {
        // TODO: ablatt - deal with vehicle logout appropriately etc. and deal with recycling lane
        // and signal managers...

        IDetectorManager detectorManager = device.getDetectorManager(intersection);

        if (model == null) {

            model = new EstimatedDataModel(messages, detectorManager, getGeoCalcType(), getLogoutZone());
        }
        else {

            model.updateEDM(messages, detectorManager, getGeoCalcType(), getLogoutZone());
        }

        if (model.isReferencePointSet()) {

            performUpdate(model, device, messages, receive, simTime, logger);
        }
    }

    /**
     * The update function for the device. This function is meant to take in all relevant
     * information and return the list of messages the app should emit.
     * 
     * @param model The model constructed from the known data (see the EstimatedDataModel
     *        documentation for more).
     * @param device The device the app is executing on.
     * @param messages The messages which the app is due to receive.
     * @param receive The messages which the app is due to recieve as BasicMessages.
     * @param simTime The time in the simulation in seconds since the start of the simulation.
     * @param logger The logger the user can use to get data out of the app.
     */
    public abstract void performUpdate(EstimatedDataModel model, RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger);

}
