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
 * A validator for host IP address REST parameter values.
 * 
 * @author emyers
 */
public class HostAddressValidator extends AbstractValidator<String> {

    /** The regular expression to match against IPv4 host address values. */
    public static final String IPV4_PATTERN = "^((\\d{1}|[1-9]\\d|[1]\\d{2}|[2][0-4]\\d|[2][5][0-5])\\.){3}(\\d{1}|[1-9]\\d|[1]\\d{2}|[2][0-4]\\d|[2][5][0-5])$";

    /** The regular expression to match against IPv6 host address values. */
    public static final String IPV6_PATTERN = "^(([0]{1}|[1-9a-fA-F][0-9a-fA-F]{0,3}):){7}([0]{1}|[1-9a-fA-F][0-9a-fA-F]{0,3})$";

    /**
     * Creates a new <code>HostAddressValidator</code> with the specified default value.
     * 
     * @param value The default value to validate.
     */
    public HostAddressValidator(String value) {

        super(value);
    }

    @Override
    public void validate(String value) throws WebAppException {

        String title = "Invalid Host IP Address";

        try {

            new NullValidator(value).validate();
            new EmptyStringValidator(value).validate();
        }
        catch (WebAppException exception) {

            throw new WebAppException(title, "A valid host IP address is required.");
        }

        if (!(value.matches(HostAddressValidator.IPV4_PATTERN) || value.matches(HostAddressValidator.IPV6_PATTERN))) {

            throw new WebAppException(title, String.format("The value \"%s\" is not a recognized IP address.", value));
        }
    }
}
