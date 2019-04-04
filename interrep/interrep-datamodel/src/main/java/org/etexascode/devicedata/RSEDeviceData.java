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

import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.interfaces.IDable;

/**
 * Container detailing the data associated with a specific RSE device
 * 
 * @author ablatt
 */
public class RSEDeviceData extends AbstractStandaloneDeviceData {

    /** The intersections for this RSE. */
    private int[] intersections;

    /** The reference points for this RSE. */
    private ReferencePoint[] points;

    /** The proper ID for this RSE. */
    private String properId = null;

    /**
     * Constructor
     * 
     * @param deviceRuleId The device rule ID
     * @param apps The apps on this device
     * @param points The reference points of this simulation
     * @param intersections The intersections for this RSE.
     * @param x The x coordinate of this RSE
     * @param y The y coordinate of this RSE
     * @param z The z coordinate of this RSE
     */
    public RSEDeviceData(long deviceRuleId, List<IConnectedVehicleApp<?>> apps, ReferencePoint[] points, int[] intersections, double x, double y, double z) {

        super(deviceRuleId, apps, x, y, z);
        this.points = points == null ? null : points.clone();
        this.intersections = Arrays.copyOf(intersections, intersections.length);
    }

    /**
     * Verifies the id of the RSE
     * 
     * @return True/False the id of the object
     */
    @Override
    public boolean equalsId(IDable entity) {
        return getProperId().equals(entity.getProperId());
    }

    /**
     * Gets the proper id for the RSE
     * 
     * @return properId The proper id for this RSE
     */
    @Override
    public String getProperId() {
        if (properId == null) {
            StringBuilder sb = new StringBuilder("RSEDeviceData:");
            sb.append(this.getDeviceRuleId());
            properId = sb.toString();
        }
        return properId;
    }

    /**
     * Returns the IDs of the intersections for this RSE. The returned array is a copy of the
     * original intersection IDs. Changes to the returned array will not affect the original IDs.
     * 
     * @return An array of intersection IDs for this RSE.
     */
    public int[] getIntersections() {

        return Arrays.copyOf(intersections, intersections.length);
    }

    /**
     * Returns the reference points for this RSE. The returned array contains only copies of the
     * original reference points. Changes to the returned copies will not affect the original data.
     * 
     * @return An array of reference points for this RSE.
     */
    public ReferencePoint[] getReferencePoints() {

        return points.clone();
    }
}
