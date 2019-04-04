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
package org.etexascode.wavesim;

/**
 * Provides propagation loss model constants.
 * 
 * @author ttevendale
 */
public enum PropagationLossModel {

    /** Propagation loss model for an open environment */
    OPEN("Open"),

    /** Propagation loss model for an suburban environment */
    SUBURBAN("Suburban"),

    /** Propagation loss model for an urban environment */
    URBAN("Urban");

    /** The name of this propagation loss model. */
    private String name;

    /**
     * Creates a new <code>PropagationLossModel</code> with the specified name.
     * 
     * @param name The string name to set.
     */
    private PropagationLossModel(String name) {

        this.name = name;
    }

    /**
     * Returns the name of this propagation loss model.
     * 
     * @return The string name of this propagation loss model.
     */
    public String getName() {

        return name;
    }

    /**
     * Returns the propagation loss model with the specified name.
     * 
     * @param name The string name to match.
     * @return The propagation loss model with the specified name.
     * @throws IllegalArgumentException If no <code>PropagationLossModel</code> with the specified
     *         name exists.
     */
    public static PropagationLossModel valueOfName(String name) {

        for (PropagationLossModel value : PropagationLossModel.values()) {
            if (value.getName().equals(name)) {
                return value;
            }
        }

        throw new IllegalArgumentException(String.format("The value \"%s\" is not a recognized propagation loss model.", name));
    }
}