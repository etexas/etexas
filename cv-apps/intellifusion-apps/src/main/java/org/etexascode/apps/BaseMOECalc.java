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

import java.util.HashMap;
import java.util.Map;

/**
 * A base class for MOE calculators to extend so that they can be used in the IntelliFusion UI.
 * 
 * @author bbadillo
 * @author dranker
 * @author ablatt
 */
public abstract class BaseMOECalc {

    /**
     * A map of outputs of this calculator keyed by output name.
     */
    protected Map<String, String> outputs;

    /**
     * An integer value representing the milliseconds of time that the calculator should be
     * configured to calculate.
     */
    private long calcInterval;

    /**
     * Default Constructor
     */
    public BaseMOECalc() {
        super();

        outputs = new HashMap<String, String>();
        calcInterval = 500;
    }

    /**
     * Constructor to configure the calculation interval.
     * 
     * @param inputCalcInterval The calculation interval to set for this calculator.
     */
    public BaseMOECalc(long inputCalcInterval) {
        super();

        outputs = new HashMap<String, String>();
        this.calcInterval = inputCalcInterval;
    }

    /**
     * An abstract method to allow extending classes to define their own identifier.
     * 
     * @return The MOE Calculator identifier.
     */
    abstract public String getCalcID();

    /**
     * Gets the complete map of outputs for this calculator keyed by output name.
     * 
     * @return A map of outputs for this calculator.
     */
    public Map<String, String> getOutputs() {
        return this.outputs;
    }

    /**
     * Gets the value of a specific calculator output by the name of the output.
     * 
     * @param key The name of the output to retrieve.
     * @return The value of the specified output.
     */
    public String getOutput(String key) {
        return outputs.get(key);
    }

    /**
     * Get the calculation interval for this calculator.
     * 
     * @return The calculation interval for this calculator.
     */
    public long getCalcInterval() {
        return calcInterval;
    }

    /**
     * Set the calculation interval for this calculator.
     * 
     * @param calcInterval The calculation interval for this calculator.
     */
    public void setCalcInterval(long calcInterval) {
        this.calcInterval = calcInterval;
    }

    /**
     * Advances the internal sim time.
     * 
     * @param elapsedTime The amount of time to advance (in seconds).
     */
    abstract public void advance(double elapsedTime);
}
