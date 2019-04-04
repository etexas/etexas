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
 * Keeps track of the available implementations of IWaveSim.
 * 
 * @author janway
 */
public enum WaveSimType {
    IDEALIZED("Idealized", "A simple model where messages always reach their destination after a single timestep."),
    NS3("NS3", "Uses the NS3 simulator (http://www.nsnam.org/) to model message propagation. Messages only reach nodes within range and can take longer than 1 timestep.");

    /**
     * A user-friendly name for the implementation.
     */
    private final String name;

    /**
     * A description of the implementation.
     */
    private final String description;

    /**
     * Constructor
     * 
     * @param name The name.
     * @param description The description.
     */
    WaveSimType(String name, String description) {
        this.name = name;
        this.description = description;
    }

    /**
     * Getter.
     * 
     * @return The name.
     */
    public String getName() {
        return this.name;
    }

    /**
     * Getter.
     * 
     * @return The description.
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * Returns the <code>WaveSimType</code> with the specified name.
     * 
     * @param name The string name to match.
     * @return The <code>WaveSimType</code> with the specified name.
     * @throws IllegalArgumentException If no <code>WaveSimType</code> with the specified name
     *         exists.
     */
    public static WaveSimType valueOfName(String name) {

        for (WaveSimType value : WaveSimType.values()) {
            if (value.getName().equals(name)) {
                return value;
            }
        }

        throw new IllegalArgumentException();
    }
}
