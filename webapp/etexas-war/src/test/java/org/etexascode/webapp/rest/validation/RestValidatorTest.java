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

import org.etexascode.webapp.exception.ValidationException;
import org.junit.Test;

/**
 * Tests the executor for multiple validators for REST parameter values.
 * 
 * @author emyers
 */
public class RestValidatorTest {

    /**
     * Tests the executor for multiple validators for REST parameter values when all REST parameter
     * values are illegal.
     * 
     * @throws ValidationException If any REST parameter value is not valid.
     */
    @Test(expected = ValidationException.class)
    public void testValidationFailure() throws ValidationException {

        RestValidator.validate(
                new CompositeNameValidator(null),
                new SimulationNameValidator(null),
                new ExecutionNameValidator(null));
    }

    /**
     * Tests the executor for multiple validators for REST parameter values when all REST parameter
     * values are valid.
     * 
     * @throws ValidationException If any REST parameter value is not valid.
     */
    @Test
    public void testValidationSuccess() throws ValidationException {

        RestValidator.validate(
                new CompositeNameValidator("Composite Alpha"),
                new SimulationNameValidator("Simulation Alpha"),
                new ExecutionNameValidator("Execution Alpha"));
    }

    /**
     * Tests the executor for multiple optional validators for REST parameter values when a REST
     * parameter value is illegal.
     * 
     * @throws ValidationException If any REST parameter value is not valid.
     */
    @Test(expected = ValidationException.class)
    public void testOptionalValidationFailure() throws ValidationException {

        RestValidator.validateOptional(
                new CompositeNameValidator(null),
                new SimulationNameValidator(null),
                new ExecutionNameValidator(null),
                new FirstNameValidator(""));
    }

    /**
     * Tests the executor for multiple optional validators for REST parameter values when all REST
     * parameter values are omitted.
     * 
     * @throws ValidationException If any REST parameter value is not valid.
     */
    @Test
    public void testOptionalValidationSuccess() throws ValidationException {

        RestValidator.validateOptional(
                new CompositeNameValidator(null),
                new SimulationNameValidator(null),
                new ExecutionNameValidator(null));
    }
}
