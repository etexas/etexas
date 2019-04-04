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
 * A validator for uplink carrier frequency REST parameter values.
 * 
 * @author emyers
 */
public class UplinkCarrierFrequencyValidator extends AbstractValidator<Integer> {

    /** The minimum uplink carrier frequency value. */
    public static final Integer MIN_VALUE = 18000;

    /** The maximum uplink carrier frequency value. */
    public static final Integer MAX_VALUE = 24599;

    /**
     * Creates a new <code>UplinkCarrierFrequencyValidator</code> with the specified default value.
     * 
     * @param value The default value to validate.
     */
    public UplinkCarrierFrequencyValidator(Integer value) {

        super(value);
    }

    @Override
    public void validate(Integer value) throws WebAppException {

        String title = "Invalid Uplink Carrier Frequency";

        try {

            new NullValidator(value).validate();
        }
        catch (WebAppException exception) {

            throw new WebAppException(title, "A valid uplink carrier frequency is required.");
        }

        if (value < UplinkCarrierFrequencyValidator.MIN_VALUE || value > UplinkCarrierFrequencyValidator.MAX_VALUE) {

            throw new WebAppException(title, String.format("Uplink carrier frequencies must be in the range of %d to %d.",
                    UplinkCarrierFrequencyValidator.MIN_VALUE, UplinkCarrierFrequencyValidator.MAX_VALUE));
        }
    }
}
