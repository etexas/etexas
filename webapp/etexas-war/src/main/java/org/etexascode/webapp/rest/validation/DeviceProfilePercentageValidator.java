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
 * A validator for device profile percentage REST parameter values.
 * 
 * @author emyers
 */
public class DeviceProfilePercentageValidator extends AbstractValidator<Double> {

    /** The minimum device profile percentage value. */
    public static final Integer MIN_VALUE = 1;

    /** The maximum device profile percentage value. */
    public static final Integer MAX_VALUE = 100;

    /**
     * Creates a new <code>DeviceProfilePercentageValidator</code> with the specified default value.
     * 
     * @param value The default value to validate.
     */
    public DeviceProfilePercentageValidator(Double value) {

        super(value);
    }

    @Override
    public void validate(Double value) throws WebAppException {

        String title = "Invalid Device Profile Percentage";

        try {

            new NullValidator(value).validate();
        }
        catch (WebAppException exception) {

            throw new WebAppException(title, "A valid device profile percentage is required.");
        }

        if (value < DeviceProfilePercentageValidator.MIN_VALUE || value > DeviceProfilePercentageValidator.MAX_VALUE) {

            throw new WebAppException(title, String.format("Device profile percentages must be in the range of %d percent to %d percent.",
                    DeviceProfilePercentageValidator.MIN_VALUE, DeviceProfilePercentageValidator.MAX_VALUE));
        }
    }
}
