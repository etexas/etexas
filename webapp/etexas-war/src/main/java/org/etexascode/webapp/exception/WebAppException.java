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

package org.etexascode.webapp.exception;

import javax.json.Json;
import javax.json.JsonObject;
import javax.ws.rs.core.Response;

/**
 * A web application exception with a user friendly message.
 * 
 * @author jrutherford
 * @author bbadillo
 * @author emyers
 */
public class WebAppException extends Exception {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The title of this exception. */
    private String title;

    /** The response status for this exception. */
    private Response.Status status;

    /**
     * Creates a new <code>WebAppException</code> with the specified title and message. The response
     * status is set to a 409 Conflict HTTP status code.
     * 
     * @param title The string title for this exception.
     * @param message The string message for this exception.
     */
    public WebAppException(String title, String message) {

        this(title, message, Response.Status.CONFLICT);
    }

    /**
     * Creates a new <code>WebAppException</code> with the specified title, message, and response
     * status.
     * 
     * @param title The string exception title to set.
     * @param message The string exception message to set.
     * @param status The exception response status to set.
     */
    public WebAppException(String title, String message, Response.Status status) {

        super(message);
        this.title = title;
        this.status = status;
    }

    /**
     * Returns the title of this exception.
     * 
     * @return The string title of this exception.
     */
    public String getTitle() {

        return title;
    }

    /**
     * Returns the response status for this exception.
     * 
     * @return The response status for this exception.
     */
    public Response.Status getStatus() {

        return status;
    }

    /**
     * Returns a JSON representation of this exception.
     * 
     * @return A <code>JsonObject</code> for this exception.
     */
    public JsonObject toJson() {

        return Json.createObjectBuilder()
                .add("exceptionType", WebAppException.class.getSimpleName())
                .add("statusCode", status.getStatusCode())
                .add("title", title)
                .add("messages", Json.createArrayBuilder().add(getMessage()).build())
                .build();
    }

    @Override
    public String toString() {

        return String.format("%s;%s", getTitle(), getMessage());
    }
}