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

package org.etexascode.apps.dcs.model;

/**
 * A structure used to count cars and trucks in a specific time interval.
 * 
 * @author jrutherford
 */
public class TimeIntervalData {

    /** The number of cars in a specific time interval. */
    private int numberOfCars = 0;

    /** The number of trucks in a specific time interval. */
    private int numberOfTrucks = 0;

    /** Add a car to the car count. */
    public void addCar() {
        numberOfCars++;
    }

    /** Add a truck to the truck count. */
    public void addTruck() {
        numberOfTrucks++;
    }

    /**
     * Get the number of cars in the time interval.
     * 
     * @return The number of cars in the time interval.
     */
    public int getNumberOfCars() {
        return numberOfCars;
    }

    /**
     * Get the number of trucks in the time interval.
     * 
     * @return The number of trucks in the time interval.
     */
    public int getNumberOfTrucks() {
        return numberOfTrucks;
    }
}