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
package org.etexascode.apps;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.codec.binary.Base64;
import org.etexascode.interrep.datamodel.LaneNode;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class MapDataUtilTest {

    private final double TOLERANCE = 0.0005;

    List<String> testNodesShort = null;

    List<String> testNodesMedium = null;

    List<String> testNodesLong = null;

    List<LaneNode> expectedNodesShort = null;

    int defaultLaneWidth = 100;

    @Before
    public void setUp() throws Exception {
        // setup test nodes for conversion
        testNodesShort = new LinkedList<String>();
        testNodesMedium = new LinkedList<String>();
        testNodesLong = new LinkedList<String>();

        expectedNodesShort = new LinkedList<LaneNode>();

        // setup x, y only
        ByteBuffer bb = ByteBuffer.allocate(4);
        bb.putShort((short)-896.112);
        bb.putShort((short)21234.11);
        testNodesShort.add(new String(Base64.encodeBase64(bb.array())));

        LaneNode ln = new LaneNode();
        ln.setX(-896);
        ln.setY(21234);
        ln.setZ(0);
        ln.setWidth(defaultLaneWidth);
        expectedNodesShort.add(ln);

        bb = ByteBuffer.allocate(4);
        bb.putShort((short)-871.728);
        bb.putShort((short)21234.11);
        testNodesShort.add(new String(Base64.encodeBase64(bb.array())));

        ln = new LaneNode();
        ln.setX(-871);
        ln.setY(21234);
        ln.setZ(0);
        ln.setWidth(defaultLaneWidth);
        expectedNodesShort.add(ln);

        bb = ByteBuffer.allocate(4);
        bb.putShort((short)-877.8240000000001);
        bb.putShort((short)21234.11);
        testNodesShort.add(new String(Base64.encodeBase64(bb.array())));

        ln = new LaneNode();
        ln.setX(-877);
        ln.setY(21234);
        ln.setZ(0);
        ln.setWidth(defaultLaneWidth);
        expectedNodesShort.add(ln);

        // setup x, y, z only
        bb = ByteBuffer.allocate(6);
        bb.putShort((short)-896.112);
        bb.putShort((short)21234.11);
        bb.putShort((short)0.0);
        testNodesMedium.add(new String(Base64.encodeBase64(bb.array())));

        bb = ByteBuffer.allocate(6);
        bb.putShort((short)-871.728);
        bb.putShort((short)21234.11);
        bb.putShort((short)6.0);
        testNodesMedium.add(new String(Base64.encodeBase64(bb.array())));

        bb = ByteBuffer.allocate(6);
        bb.putShort((short)-877.8240000000001);
        bb.putShort((short)21234.11);
        bb.putShort((short)12.0);
        testNodesMedium.add(new String(Base64.encodeBase64(bb.array())));

        // setup all
        bb = ByteBuffer.allocate(8);
        bb.putShort((short)-896.112);
        bb.putShort((short)21234.11);
        bb.putShort((short)12.0);
        bb.putShort((short)473.0);
        testNodesLong.add(new String(Base64.encodeBase64(bb.array())));

        bb = ByteBuffer.allocate(8);
        bb.putShort((short)-871.728);
        bb.putShort((short)21234.11);
        bb.putShort((short)12.0);
        bb.putShort((short)600.0);
        testNodesLong.add(new String(Base64.encodeBase64(bb.array())));

        bb = ByteBuffer.allocate(8);
        bb.putShort((short)-877.8240000000001);
        bb.putShort((short)21234.11);
        bb.putShort((short)12.0);
        bb.putShort((short)18.0);
        testNodesLong.add(new String(Base64.encodeBase64(bb.array())));
    }

    @After
    public void tearDown() throws Exception {
        testNodesShort = null;
        testNodesMedium = null;
        testNodesMedium = null;

        expectedNodesShort = null;
    }

    @Test
    public void testConvertMessageNodeListShort() {
        List<LaneNode> res = MapDataUtil.convertMessageNodeList(testNodesShort, defaultLaneWidth);

        for (int i = 0; i < res.size(); i++) {
            if (!expectedNodesShort.get(i).equals(res.get(i))) {
                System.out.println("i = " + i);
            }
            assertEquals(expectedNodesShort.get(i), res.get(i));
        }
    }

    @Test
    public void testConvertMessageNodeListShortLength() {
        assertEquals(expectedNodesShort.size(), testNodesShort.size());
    }

    @Test
    public void testCohenSutherland() {
        double xmin = -10;
        double ymin = -10;
        double xmax = 10;
        double ymax = 10;

        double[] input1 = new double[] { -15, 0, 15, 0 };
        double[] output1 = new double[] { -10, 0, 10, 0 };

        double[] input2 = new double[] { 0, -15, 0, 15 };
        double[] output2 = new double[] { 0, -10, 0, 10 };

        double[] input3 = new double[] { 0, 20, 5, 20 };
        double[] output3 = null;

        assertArrayEquals(output1, MapDataUtil.cohenSutherland(input1[0], input1[1], input1[2], input1[3], xmin, xmax, ymin, ymax), TOLERANCE);
        assertArrayEquals(output2, MapDataUtil.cohenSutherland(input2[0], input2[1], input2[2], input2[3], xmin, xmax, ymin, ymax), TOLERANCE);
        assertArrayEquals(output3, MapDataUtil.cohenSutherland(input3[0], input3[1], input3[2], input3[3], xmin, xmax, ymin, ymax), TOLERANCE);
    }
}
