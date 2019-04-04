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
package org.etexascode.webapp.rest.validation;

import org.etexascode.webapp.exception.WebAppException;

/**
 * A validator for port number REST parameter values.
 * 
 * @author emyers
 */
public class PortNumberValidator extends AbstractValidator<Integer> {

    /** The minimum port number value. */
    public static final int MIN_VALUE = 1;

    /** The maximum port number value. */
    public static final int MAX_VALUE = 65535;

    /**
     * Creates a new <code>PortNumberValidator</code> with the specified default value.
     * 
     * @param value The default value to validate.
     */
    public PortNumberValidator(Integer value) {

        super(value);
    }

    @Override
    public void validate(Integer value) throws WebAppException {

        String title = "Invalid Port Number";

        try {

            new NullValidator(value).validate();
        }
        catch (WebAppException exception) {

            throw new WebAppException(title, "A valid port number is required.");
        }

        if (value < PortNumberValidator.MIN_VALUE || value > PortNumberValidator.MAX_VALUE) {

            throw new WebAppException(title, String.format("Port numbers must be in the range of %d to %d.", PortNumberValidator.MIN_VALUE, PortNumberValidator.MAX_VALUE));
        }
    }
}
