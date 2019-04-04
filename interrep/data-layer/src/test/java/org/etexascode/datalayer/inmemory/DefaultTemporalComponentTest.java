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
package org.etexascode.datalayer.inmemory;

import static org.junit.Assert.assertTrue;

import org.etexascode.datalayer.interfaces.ITemporalComponent;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for the default temporal component.
 * 
 * @author emyers
 */
public class DefaultTemporalComponentTest {

    /** The temporal component. */
    private ITemporalComponent temporalComponent;

    /**
     * Creates a new temporal component before each test.
     */
    @Before
    public void init() {

        temporalComponent = new DefaultTemporalComponent();
    }

    /**
     * Tests the <code>getSimTime</code> method for the default temporal component.
     */
    @Test
    public void testGetSimTime() {

        int stepNumber = 5;
        double stepSize = 0.5;
        temporalComponent.setStepSize(0.5);
        assertTrue(temporalComponent.getSimTime(stepNumber) == stepNumber * stepSize);
    }

    /**
     * Tests the <code>setStepSize</code> method for the default temporal component.
     */
    @Test
    public void testSetStepSize() {

        double stepSize = 0.5;
        temporalComponent.setStepSize(stepSize);
        assertTrue(temporalComponent.getStepSize() == stepSize);
    }
}
