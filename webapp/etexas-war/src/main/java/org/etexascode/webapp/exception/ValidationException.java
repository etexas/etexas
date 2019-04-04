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

import java.util.ArrayList;
import java.util.List;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;

/**
 * A web application exception for REST validation failures.
 * 
 * @author emyers
 */
public class ValidationException extends WebAppException {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The list of validation failure messages. */
    private List<String> messages;

    /**
     * Creates a new <code>ValidationException</code> with the specified messages.
     * 
     * @param messages The list of validation failure messages for this exception.
     */
    public ValidationException(List<String> messages) {

        super("Validation Failure", "One or more values failed REST parameter validation.");
        this.messages = messages;
    }

    /**
     * Returns the validation failure messages for this exception.
     * 
     * @return A list of the validation failure messages for this exception.
     */
    public List<String> getMessages() {

        return new ArrayList<String>(messages);
    }

    @Override
    public JsonObject toJson() {

        JsonArrayBuilder messageArray = Json.createArrayBuilder();

        for (int i = 0; i < messages.size(); i++) {

            messageArray.add(messages.get(i));
        }

        return Json.createObjectBuilder()
                .add("exceptionType", ValidationException.class.getSimpleName())
                .add("statusCode", getStatus().getStatusCode())
                .add("title", getTitle())
                .add("messages", messageArray.build())
                .build();
    }
}
