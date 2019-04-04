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

package org.etexascode.nonstd;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Carrier structure for simulation data to be transmitted.
 * 
 * @author bbadillo
 * @author ablatt
 */
@XmlRootElement
public class SimData extends Message implements Serializable {

    /**
     * The current time in simulation in seconds
     */
    private double simTime;

    /**
     * The calculator type to be used in lat,long conversions. (Calculator type is used by
     * UtilsLatLongConversions for calculations)
     */
    private int geoCalculatorType;

    /**
     * Default noarg constructor for serialization
     */
    public SimData() {}

    /**
     * Get the current time in simulation (measured in seconds).
     * 
     * @return The current time in simulation in seconds.
     */
    public double getSimTime() {
        return simTime;
    }

    /**
     * Set the current time in simulation (measured in seconds).
     * 
     * @param simTime The current time in simulation in seconds.
     */
    public void setSimTime(double simTime) {
        this.simTime = simTime;
    }

    /**
     * Get the type of Calculator being used by the simulation.
     * 
     * @return the type of Calculator
     */
    public int getGeoCalculatorType() {
        return geoCalculatorType;
    }

    /**
     * Set the type of Calculator being used by the simulation.
     * 
     * @param geoCalculatorType
     */
    public void setGeoCalculatorType(int geoCalculatorType) {
        this.geoCalculatorType = geoCalculatorType;
    }
}
