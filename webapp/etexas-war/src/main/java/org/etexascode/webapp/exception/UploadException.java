/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.webapp.exception;

import javax.json.Json;
import javax.json.JsonObject;
import javax.servlet.ServletException;

public class UploadException extends ServletException {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The title for this exception. */
    private String title;

    /** The status code for this exception. */
    private int status;

    /**
     * Creates a new <code>UploadException</code> with the specified title, message, and status
     * code.
     * 
     * @param title The string title for this exception.
     * @param message The string message for this exception.
     * @param status The integer status code for this exception.
     */
    public UploadException(String title, String message, int status) {

        super(message);
        this.title = title;
        this.status = status;
    }

    /**
     * Returns the title for this exception.
     * 
     * @return The string title for this exception.
     */
    public String getTitle() {

        return title;
    }

    /**
     * Returns the status code for this exception.
     * 
     * @return The integer status code for this exception.
     */
    public int getStatus() {

        return status;
    }

    /**
     * Returns a JSON representation of this exception.
     * 
     * @return A <code>JsonObject</code> for this exception.
     */
    public JsonObject toJson() {

        return Json.createObjectBuilder()
                .add("exceptionType", UploadException.class.getSimpleName())
                .add("statusCode", status)
                .add("title", title)
                .add("messages", Json.createArrayBuilder().add(getMessage()).build())
                .build();
    }
}
