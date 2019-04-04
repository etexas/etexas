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

import java.util.HashMap;
import java.util.Map;

import org.etexascode.interrep.datamodel.xmladapters.ManagerAdapterString;
import org.etexascode.interrep.datamodel.xmladapters.ManagerAdapterString.XmlMap;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Uses signal indication map to test Jaxb marshalling and unmarshalling Read in vehiclemanager.xml
 *
 * @author ablatt
 * @author jconnelly
 */
public class ManagerAdapterStringTest {

    /**
     * Read in vehicle manager from xml file, create vehicle list
     */
    VehicleManager vm = JaxbRead.readJaxManager("/vehicleadaptertest.xml", VehicleManager.class);

    ManagerAdapterString mas = new ManagerAdapterString();

    Vehicle vList = new Vehicle();

    Map<String, Vehicle> vMap = new HashMap<String, Vehicle>();

    XmlMap<Vehicle> av = null;

    @Before
    public void setUp() throws Exception {
        /**
         * Read vehicle into map
         */
        vList = vm.getVehicle("Vehicle:2");
        vMap.put("Vehicle:3", vList);
        mas.marshal(null);
        av = mas.marshal(vMap);
    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testMarshalMapOfIntegerT() throws Exception {
        assertEquals(vMap, mas.unmarshal(av));
    }

    @Test
    public void testUnmarshalAdaptedOfT() throws Exception {
        mas.unmarshal(null);
        assertEquals(vMap, mas.unmarshal(av));
    }
}
