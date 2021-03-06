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

package org.etexascode.interrep.datamodel.interfaces;

import java.awt.Polygon;
import java.util.List;

/**
 * Interface for merging the mutable and immutable models.
 * 
 * @author jrutherford
 */
public interface IDetector extends IDistanceable, IDable {

    /**
     * Gets the detector ID.
     * 
     * @return Detector ID.
     */
    public int getDetectorID();

    /**
     * Gets the lane IDs.
     * 
     * @return List of lane ID's.
     */
    public List<Integer> getLaneIDs();

    /**
     * Is this a presence detector.
     * 
     * @return True/False
     */
    public boolean isPresenceDetectCap();

    /**
     * Is this a pulse detector.
     * 
     * @return True/False
     */
    public boolean isPulseDetectCap();

    /**
     * Is this a speed detector.
     * 
     * @return True/False
     */
    public boolean isSpeedDetectCap();

    /**
     * Is this a length detector.
     * 
     * @return True/False
     */
    public boolean isLengthDetectCap();

    /**
     * Gets the detector event.
     * 
     * @return The detector event.
     */
    public IDetectorEvent getDetEvent();

    /**
     * Gets the area of the detector.
     * 
     * @return The area.
     */
    public Polygon getArea();
}