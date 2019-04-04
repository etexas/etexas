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

package org.etexascode.interrep.datamodel;

import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author bmauldon
 */
public class ProjMetaDataTest {

    /** hard code constants */
    SimMetaData pmd = new SimMetaData();

    double simWidth = 1000.0;

    double simHeight = 1000.0;

    long maxSteps = 20L;

    long firstStep = 1L;

    double stepSize = 5;

    double delta = 0.00001;

    @Before
    public void setUp() throws Exception {
        /** Set values, test using gets */
        pmd.setFirstStep(firstStep);
        pmd.setMaxSteps(maxSteps);
        pmd.setSimHeight(simHeight);
        pmd.setSimWidth(simWidth);
        pmd.setStepSize(stepSize);
    }

    @After
    public void tearDown() throws Exception {

    }

    @Test
    public void testGetFirstStep() {
        assertEquals(firstStep, pmd.getFirstStep());
    }

    @Test
    public void testGetMaxSteps() {
        assertEquals(maxSteps, pmd.getMaxSteps());
    }

    @Test
    public void testGetSimHeight() {
        assertEquals(simHeight, pmd.getSimHeight(), delta);
    }

    @Test
    public void testGetSimWidth() {
        assertEquals(simWidth, pmd.getSimWidth(), delta);
    }

    @Test
    public void testGetStepSize() {
        assertEquals(stepSize, pmd.getStepSize(), delta);
    }

    @Test
    public void testSetFirstStep() {
        assertEquals(firstStep, pmd.getFirstStep());
    }

    @Test
    public void testSetMaxSteps() {
        assertEquals(maxSteps, pmd.getMaxSteps());
    }

    @Test
    public void testSetSimHeight() {
        assertEquals(simHeight, pmd.getSimHeight(), delta);
    }

    @Test
    public void testSetSimWidth() {
        assertEquals(simWidth, pmd.getSimWidth(), delta);
    }

    @Test
    public void testSetStepSize() {
        assertEquals(stepSize, pmd.getStepSize(), delta);
    }

}
