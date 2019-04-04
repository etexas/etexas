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

package org.etexascode.webapp.ra.api.exceptions.simresource;

/**
 * An exception class for the eTEXAS resource adapter. This checked exception type has a number of
 * sub-exception types. When encountered, it would be wise to check which subclass is actually being
 * thrown. (note: there are some instances when the caught SimResourceException is just a
 * SimResourceException)
 * 
 * @author bbadillo
 * @author ablatt
 * @author ttevendale
 */
public class SimResourceException extends Exception {

    public static enum SimResourceErrorType {
        DEFAULT,
        ALLOCATE,
        FILE_FORMAT,
        FILE_NOT_FOUND,
        IO,
        REMOTE,
        INVALID_SIM_TYPE
    }

    public final SimResourceErrorType errorType;

    /**
     * Constructor which takes in an exception. This constructor is meant to wrap an exception
     * inside the checked exception type.
     * 
     * @param ex The exception to be wrapped by this class.
     * @param errorType The type of error that is happening.
     */
    public SimResourceException(Exception ex, SimResourceErrorType errorType) {
        super(ex);
        this.errorType = errorType;
    }

    /**
     * Constructor which takes in a message.
     * 
     * @param message A message to help determine the root cause of the exception.
     * @param errorType The type of error that is happening.
     */
    public SimResourceException(String message, SimResourceErrorType errorType) {
        super(message);
        this.errorType = errorType;
    }

    public SimResourceErrorType getErrorType() {
        return errorType;
    }
}
