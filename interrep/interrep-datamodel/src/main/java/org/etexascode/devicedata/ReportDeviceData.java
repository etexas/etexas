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
package org.etexascode.devicedata;

import java.util.Arrays;
import java.util.List;

import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;

/**
 * Container detailing the data associated with a specific Report device
 * 
 * @author ablatt
 */
public class ReportDeviceData extends AbstractDeviceData implements IDistanceable {

    /** The intersections for this report device. */
    private int[] intersections;

    /** The proper ID for this report device. */
    private String properId = null;

    /**
     * Constructor
     * 
     * @param apps The apps for this report device.
     * @param intersections The intersections for this report device.
     */
    public ReportDeviceData(List<IConnectedVehicleApp<?>> apps, int[] intersections) {

        // null because a report device shouldn't have a mac address that the user can look up
        super(0, apps);
        this.intersections = Arrays.copyOf(intersections, intersections.length);
    }

    /**
     * Gets the x parameter for the location of the report device
     * 
     * @return 0 for placing report device at origin
     */
    @Override
    public double getX() {
        return 0;
    }

    /**
     * Gets the y parameter for the location of the report device
     * 
     * @return 0 for placing the report device at origin
     */
    @Override
    public double getY() {
        return 0;
    }

    /**
     * Gets the z parameter for the location of the report device
     * 
     * @return 0 for placing the report device at origin
     */
    @Override
    public double getZ() {
        return 0;
    }

    /**
     * Equality check for entity id
     * 
     * @return True/False the entity is equal
     */
    @Override
    public boolean equalsId(IDable entity) {
        return getProperId().equals(entity.getProperId());
    }

    /**
     * Gets the proper id for the report device
     * 
     * @return properId The proper id for the report device
     */
    @Override
    public String getProperId() {

        if (properId == null) {

            properId = "ReportDeviceData";
        }

        return properId;
    }

    /**
     * Returns the IDs of the intersections for this report device. The returned array is a copy of
     * the original intersection IDs. Changes to the returned array will not affect the original
     * IDs.
     * 
     * @return An array of intersection IDs for this report device.
     */
    public int[] getIntersections() {

        return Arrays.copyOf(intersections, intersections.length);
    }
}
