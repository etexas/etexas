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

package org.etexascode.interrep.datamodel.utils;

import static org.junit.Assert.assertEquals;

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Test for UtilsStringOnModel. Sets up a Map<Integer,Integer>, a Map<Integer, SignalIndication>, a
 * List<Integer>, and a List<SignalIndication> for testing the StringBuilder methods.
 * 
 * @author bmauldon
 */
public class UtilsStringOnModelTest {

    /** Set up test Strings */
    private final static Logger LOGGER = LoggerFactory.getLogger(UtilsStringOnModelTest.class);

    private StringBuilder addTo = null;

    private final StringBuilder complete1 = new StringBuilder("This is a test This is an object  = 46.992\n");

    private final StringBuilder complete2 = new StringBuilder("This is a test This is an object  = 1300\n");

    private final StringBuilder complete3 = new StringBuilder("This is a test This is an object  = add this string\n");

    private final StringBuilder complete4 = new StringBuilder("This is a test This is a map entry  Map: key = 1\n     value = 1\n");

    StringBuilder complete5 = null;

    StringBuilder complete6 = null;

    StringBuilder complete7 = null;

    StringBuilder points = new StringBuilder("(20, 20)\n");

    StringBuilder polyPoints = new StringBuilder("(20, 20)\n(40, 20)\n(40, 40)\n(60, 40)\n(60, 60)\n(40, 20)\n");

    byte[] bytevalid = { (byte)74, (byte)75, (byte)76, (byte)80 };

    private final int[] x1 = { 20, 40, 40, 60, 60, 40 };

    private final int[] y1 = { 20, 20, 40, 40, 60, 20 };

    Polygon poly = new Polygon(x1, y1, 6);

    private List<Integer> list1 = new ArrayList<Integer>();

    private List<SignalIndication> list2 = new ArrayList<SignalIndication>();

    private Map<Integer, Integer> map1 = new HashMap<Integer, Integer>();

    private Map<Integer, SignalIndication> map2 = new HashMap<Integer, SignalIndication>();

    private final SignalIndication signal1 = new SignalIndication();

    private final SignalIndication signal2 = new SignalIndication();

    private final SignalIndication signal3 = new SignalIndication();

    int one = 1;

    int two = 2;

    int three = 3;

    double doubleToAdd = 46.992;

    int intToAdd = 1300;

    String stringToAdd = "add this string";

    String title1 = "This is an object ";

    String title2 = "This is a map entry ";

    String title3 = "This is a map ";

    String title4 = "This is a list ";
    // Setup SignalIndication objects
    // Set-up Maps and Lists

    private final String listToStringRes = "List: Title\nEntry: Element 1\nEntry: Element 2\nEntry: Element 3\n";

    private final String listToStringTitle = "Title";

    private List<String> listToStringIn = new ArrayList<String>(3);

    @Before
    public void setUp() throws Exception {

        signal1.setLaneId(one);
        signal2.setLaneId(two);
        signal3.setLaneId(three);
        signal1.setColorIndication(Color.GREEN);
        signal2.setColorIndication(Color.RED);
        signal3.setColorIndication(Color.YELLOW);
        signal1.setStateIndication(State.STEADY);
        signal2.setStateIndication(State.STEADY);
        signal3.setStateIndication(State.FLASHING);
        signal1.setTypeIndication(Type.BALL);
        signal2.setTypeIndication(Type.BALL);
        signal3.setTypeIndication(Type.LEFT_ARROW);
        complete5 = new StringBuilder("This is a test This is a map  Map: key = 1\n     value = " + signal1.toString() + "\nThis is a map  Map: key = 2\n     value = " + signal2.toString()
                + "\nThis is a map  Map: key = 3\n     value = " + signal3.toString() + "\n");
        complete6 = new StringBuilder("This is a test This is a list  Element = 2\n");
        complete7 = new StringBuilder(
                "This is a list  Element = " + signal1.toString() + "\nThis is a list  Element = " + signal2.toString() + "\nThis is a list  Element = " + signal3.toString() + "\n");

        addTo = new StringBuilder("This is a test ");
        map1.put(one, one);
        map1.put(two, two);
        map1.put(three, three);
        map2.put(one, signal1);
        map2.put(two, signal2);
        map2.put(three, signal3);
        list1.add(one);
        list1.add(two);
        list1.add(three);
        list2.add(signal1);
        list2.add(signal2);
        list2.add(signal3);

        listToStringIn.add("Element 1");
        listToStringIn.add("Element 2");
        listToStringIn.add("Element 3");
    }

    @After
    public void tearDown() throws Exception {
        map1 = null;
        map2 = null;
        list1 = null;
        list2 = null;
        complete5 = null;
        addTo = new StringBuilder("This is a test ");
        listToStringIn = null;
    }

    @Test
    public void testAddDouble() {
        UtilsStringOnModel.addDouble(addTo, doubleToAdd, title1);
        assertEquals(complete1.toString(), addTo.toString());
    }

    @Test
    public void testAddInt() {
        UtilsStringOnModel.addInt(addTo, intToAdd, title1);
        assertEquals(complete2.toString(), addTo.toString());
    }

    @Test
    public void testAddList() {
        addTo = new StringBuilder("");
        UtilsStringOnModel.addList(addTo, list2, title4);
        assertEquals(complete7.toString(), addTo.toString());
    }

    @Test
    public void testAddListElement() {
        UtilsStringOnModel.addListElement(addTo, list1.get(1), title4);
        assertEquals(complete6.toString(), addTo.toString());
    }

    @Test
    public void testAddMap() {
        UtilsStringOnModel.addMap(addTo, map2, title3);
        assertEquals(complete5.toString(), addTo.toString());
    }

    @Test
    public void testAddMapKeyValue() {
        UtilsStringOnModel.addMapKeyValue(addTo, 1, 1, title2);
        assertEquals(complete4.toString(), addTo.toString());
    }

    @Test
    public void testAddString() {
        UtilsStringOnModel.addString(addTo, stringToAdd, title1);
        assertEquals(complete3.toString(), addTo.toString());
    }

    @Test
    public void testBuildPointString() {
        assertEquals(points.toString(), UtilsStringOnModel.buildPointString(20, 20));
    }

    @Test
    public void testBuildPolygonString() {
        assertEquals(polyPoints.toString(), UtilsStringOnModel.buildPolygonString(poly));

    }

    @Test
    public void testDecodeUTF8() {
        assertEquals(new String("JKLP"), UtilsStringOnModel.decodeUTF8(bytevalid));

    }

    @Test
    public void testAddMap2() {
        Map<String, String> map = new HashMap<String, String>();
        map.put("key", "value");
        map.put("foo", "bar");
        String title = "title";
        String result = "Map: title\nfoo: bar\nkey: value\n";
        assertEquals(result, UtilsStringOnModel.addMap(map, title));
    }

    @Test
    public void testConstructor() {
        UtilsStringOnModel obj = new UtilsStringOnModel();
    }

    @Test
    public void testListToString() {
        String tmp = UtilsStringOnModel.listToString(listToStringIn, listToStringTitle);
        assertEquals(listToStringRes, tmp);
    }
}
