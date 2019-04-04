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

import java.util.ArrayList;
import java.util.List;

import org.etexascode.webapp.exception.ValidationException;
import org.etexascode.webapp.exception.WebAppException;

/**
 * An executor for multiple validators for REST parameter values.
 * 
 * @author emyers
 */
public class RestValidator {

    /* prevents instantiation */
    private RestValidator() {}

    /**
     * Executes the provided validators for REST parameter values.
     * 
     * @param validators The validators to execute.
     * @throws ValidationException If any validation fails.
     */
    public static void validate(AbstractValidator<?>... validators) throws ValidationException {

        List<String> messages = new ArrayList<String>();

        for (AbstractValidator<?> validator : validators) {

            try {

                validator.validate();
            }
            catch (WebAppException exception) {

                messages.add(exception.getMessage());
            }
        }

        if (!messages.isEmpty()) {

            throw new ValidationException(messages);
        }
    }

    /**
     * Executes the provided validators for REST parameter values. Unlike <code>validate</code> the
     * method will only execute those validators that do not have <code>null</code> default values.
     * 
     * @param validators The validators to execute.
     * @throws ValidationException If any validation fails.
     */
    public static void validateOptional(AbstractValidator<?>... validators) throws ValidationException {

        ArrayList<AbstractValidator<?>> executable = new ArrayList<AbstractValidator<?>>();

        for (AbstractValidator<?> validator : validators) {

            if (validator.getValue() != null) {

                executable.add(validator);
            }
        }

        RestValidator.validate(executable.toArray(new AbstractValidator<?>[executable.size()]));
    }
}
