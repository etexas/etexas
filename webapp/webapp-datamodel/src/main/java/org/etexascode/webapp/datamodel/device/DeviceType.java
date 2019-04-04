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
package org.etexascode.webapp.datamodel.device;

import org.etexascode.apps.ICellularBaseApp;
import org.etexascode.apps.IOBUBaseApp;
import org.etexascode.apps.IRSEBaseApp;
import org.etexascode.apps.IReportBaseApp;

/**
 * Provides connected vehicle device type constants.
 * 
 * @author emyers
 */
public enum DeviceType {

    /** The type for a cellular device profile. */
    CELLULAR("Cellular"),

    /** The type for a fixed cellular device. */
    FIXED_CELLULAR("Fixed Cellular"),

    /** The type for an OBU device profile. */
    OBU("OBU"),

    /** The type for a report device. */
    REPORT("Report"),

    /** The type for a RSE device. */
    RSE("RSE");

    /** The name of this device type. */
    private String name;

    /**
     * Creates a new <code>DeviceType</code> with the specified name.
     * 
     * @param name The string name to set.
     */
    private DeviceType(String name) {

        this.name = name;
    }

    /**
     * Returns the name of this device type.
     * 
     * @return The string name of this device type.
     */
    public String getName() {

        return name;
    }

    /**
     * Returns the device type for the specified application class.
     * 
     * @param classType The application class to match.
     * @return The device type for the specified application class.
     * @throws IllegalArgumentException If no <code>DeviceType</code> for the specified application
     *         class exists.
     */
    public static DeviceType valueOfApplicationClass(Class<?> classType) {

        if (ICellularBaseApp.class.isAssignableFrom(classType)) {

            return DeviceType.CELLULAR;
        }
        else if (IOBUBaseApp.class.isAssignableFrom(classType)) {

            return DeviceType.OBU;
        }
        else if (IReportBaseApp.class.isAssignableFrom(classType)) {

            return DeviceType.REPORT;
        }
        else if (IRSEBaseApp.class.isAssignableFrom(classType)) {

            return DeviceType.RSE;
        }

        throw new IllegalArgumentException();
    }

    /**
     * Returns the device type with the specified name.
     * 
     * @param name The string name to match.
     * @return The device type with the specified name.
     * @throws IllegalArgumentException If no <code>DeviceType</code> with the specified name
     *         exists.
     */
    public static DeviceType valueOfName(String name) {

        for (DeviceType value : DeviceType.values()) {
            if (value.getName().equals(name)) {
                return value;
            }
        }

        throw new IllegalArgumentException(String.format("The value \"%s\" is not a recognized device type.", name));
    }

    /** The discriminator values for the defined connected vehicle device type constants. */
    public static final class Discriminator {

        /** The discriminator value for a cellular device profile. */
        public static final String CELLULAR = "CELLULAR";

        /** The discriminator value for a fixed cellular device. */
        public static final String FIXED_CELLULAR = "FIXED_CELLULAR";

        /** The discriminator value for an OBU device profile. */
        public static final String OBU = "OBU";

        /** The discriminator value for a report device. */
        public static final String REPORT = "REPORT";

        /** The discriminator value for a RSE device. */
        public static final String RSE = "RSE";
    }
}