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
package org.etexascode.interrep;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class InterRepExceptionTest {

    @Rule
    public ExpectedException ee = ExpectedException.none();

    @Test
    public void testInterRepException() throws InterRepException {
        ee.expect(InterRepException.class);
        ee.expectMessage("Geographical");
        throw new InterRepException(InterRepException.EX_NO_GEO_CALC);
    }

    @Test
    public void testInterRepException2() throws InterRepException {
        ee.expect(InterRepException.class);
        ee.expectMessage("geometry");
        throw new InterRepException(InterRepException.EX_NO_LANE_GEOMETRY);
    }

    @Test
    public void testInterRepException3() throws InterRepException {
        ee.expect(InterRepException.class);
        ee.expectMessage("MapData");
        throw new InterRepException(InterRepException.EX_NO_MAP_DATA);
    }
}
