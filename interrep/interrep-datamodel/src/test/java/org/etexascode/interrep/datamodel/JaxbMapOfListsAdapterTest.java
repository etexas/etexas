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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.utils.JaxbRead;
import org.etexascode.interrep.datamodel.xmladapters.JaxbMapOfListsAdapter;
import org.etexascode.interrep.datamodel.xmladapters.JaxbMapOfListsAdapter.ListMap;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Uses signal indication map to test Jaxb marshalling and unmarshalling Read in signalmanager.xml
 * 
 * @author bmauldon
 */

public class JaxbMapOfListsAdapterTest {

    /** Read in signal manager from xml file, create signal indication list */
    SignalManager sm = JaxbRead.readJaxManager("/signaladaptertest.xml", SignalManager.class);

    JaxbMapOfListsAdapter<SignalIndication> ma = new JaxbMapOfListsAdapter<SignalIndication>();

    List<SignalIndication> siList = new ArrayList<SignalIndication>();

    Map<Integer, List<SignalIndication>> siMap = new HashMap<Integer, List<SignalIndication>>();

    ListMap<SignalIndication> asi = null;

    @Before
    public void setUp() throws Exception {
        /** Read 2 signals for lane 100 into map */
        siList = sm.getSignalsByLaneId(100);
        siMap.put(100, siList);
        ma.marshal(null);
        asi = ma.marshal(siMap);
    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testMarshalMapOfIntegerT() throws Exception {
        assertEquals(siMap, ma.unmarshal(asi));
    }

    @Test
    public void testUnmarshalAdaptedOfT() throws Exception {
        ma.unmarshal(null);
        assertEquals(siMap, ma.unmarshal(asi));
    }
}
