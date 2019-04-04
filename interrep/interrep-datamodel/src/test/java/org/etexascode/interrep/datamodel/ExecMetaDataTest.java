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
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author bmauldon
 */
public class ExecMetaDataTest {

    DetectorManager dm = JaxbRead.readJaxManager("/detectormanager.xml", DetectorManager.class);

    /** Add detectors to list from map */
    List<Detector> dlist = new ArrayList<Detector>();

    double latitude = 37.56999;

    double longitude = 10.00008;

    ExecMetaData emd = new ExecMetaData();

    /** Default constructor set to (0,0) */
    ReferencePoint rp = new ReferencePoint();

    ReferencePoint rpnew = new ReferencePoint(latitude, longitude);

    ReferencePoint rpArray[] = { rpnew };

    int type = 3;

    private final double delta = 0.00001;

    @Before
    public void setUp() throws Exception {
        dlist.add(dm.getDetector(1));
        dlist.add(dm.getDetector(7));
    }

    @After
    public void tearDown() throws Exception {
        dlist.clear();
    }

    @Test
    public void testGetGeoCalculatorType() {
        emd.setGeoCalculatorType(type);
        assertEquals(type, emd.getGeoCalculatorType());
    }

    @Test
    public void testGetReferencePoints() {
        emd.setReferencePoints(rpArray);
        assertEquals(rpArray, emd.getReferencePoints());
    }

    @Test
    public void testSetExecDetectors() {
        emd.setExecDetectors(dlist);
        assertEquals(dlist, emd.getExecDetectors());
    }

    @Test
    public void testSetGeoCalculatorType() {
        emd.setGeoCalculatorType(type);
        assertEquals(type, emd.getGeoCalculatorType());
    }

    @Test
    public void testSetReferencePoints() {
        emd.setReferencePoints(null);
        assertNull(emd.getReferencePoints());
    }

}
