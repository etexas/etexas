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
package org.etexascode.wavesim.ns3;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.awt.Polygon;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.datalayer.interfaces.IWaveSimDataLayer;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.interrep.topography.DSRCTopography;
import org.etexascode.interrep.topography.ITopographyFeature;
import org.etexascode.interrep.topography.TopographyPolygon;
import org.etexascode.wavesim.CellTower;
import org.etexascode.wavesim.CellularConfig;
import org.etexascode.wavesim.Constants;
import org.etexascode.wavesim.PropagationLossModel;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;
import org.etexascode.wavesim.Tx.MessageType;
import org.etexascode.wavesim.WaveMessage;
import org.etexascode.wavesim.WaveSimLayer;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;

/**
 * @author janway
 * @author ttevendale
 */
public class WaveSimNS3ImplTest {

    Map<String, String> jndiParams;

    TestWaveSimDataLayer twsdl;

    List<CellTower> cellTowers;

    CellularConfig cellConfig;

    org.etexascode.datalayer.tests.TestWaveSimDataLayer dataForTopography;

    int stepNum = 42;

    @Before
    public void setup() {
        twsdl = new TestWaveSimDataLayer();
        jndiParams = new HashMap<String, String>();
        jndiParams.put(Constants.NS3_CYGWIN_HOME, "");
        jndiParams.put(Constants.NS3_BUILD_DIR, "");
        cellTowers = new ArrayList<CellTower>(1);
        cellTowers.add(this.genCellTower());
        cellConfig = this.genCellConfig();

        dataForTopography = new org.etexascode.datalayer.tests.TestWaveSimDataLayer();
        dataForTopography.inputs = genTopographyWaveNodes();
    }

    @After
    public void teardown() {
        twsdl = null;
        jndiParams = null;
        cellTowers = null;
        cellConfig = null;
        dataForTopography = null;
    }

    @Test
    public void testConstructor() {
        PropagationLossModel propagationLossModel = PropagationLossModel.URBAN;
        WaveSimNS3Impl wsWithCell = new WaveSimNS3Impl(null, twsdl, jndiParams, null, cellTowers, cellConfig, propagationLossModel);
        List<String> coms = wsWithCell.lteCommands;

        if (coms.size() == 9) {
            String cellTowerNumStr = coms.get(1);
            CellTower cellTower = cellTowers.get(0);
            assertTrue(cellTowerNumStr.equals(String.format("--cellTowerNum=%d", cellTowers.size())));

            String cellTowersStr = coms.get(2);
            assertTrue(cellTowersStr.equals(String.format("--cellTowers=0,%f,%f,%f;", UtilsUnitConversion.convertCentimetersToMeters(cellTower.getX()),
                    UtilsUnitConversion.convertCentimetersToMeters(cellTower.getY()), UtilsUnitConversion.convertCentimetersToMeters(cellTower.getZ()))));

            String lteConfigNumStr = coms.get(3);
            assertTrue(lteConfigNumStr.equals(String.format("--lteConfigNum=%d", CellularConfig.NUMBER_OF_CELLULAR_CONFIGURATIONS)));

            String lteConfigsStr = coms.get(4);
            assertTrue(lteConfigsStr.equals(String.format(
                    "--lteConfigs=UplinkBandwidth,%d;DownlinkBandwidth,%d;UplinkFrequency,%d;DownlinkFrequency,%d;CellTxPower,%f;CellNoiseFigure,%f;CellTowerTxPower,%f;CellTowerNoiseFigure,%f;",
                    cellConfig.getUplinkBandwidth(), cellConfig.getDownlinkBandwidth(), cellConfig.getUplinkCarrierFrequency(), cellConfig.getDownlinkCarrierFrequency(), cellConfig.getCellTxPower(),
                    cellConfig.getCellNoiseFigure(), cellConfig.getCellTowerTxPower(), cellConfig.getCellTowerNoiseFigure())));

            String environmentStr = coms.get(5);
            assertTrue(environmentStr.contains(propagationLossModel.getName().toLowerCase()));

            // the rest of these shouldn't be filled with actual data yet.
            String placeholder = "Placeholder";

            String txNumStr = coms.get(6);
            assertTrue(txNumStr.contains(placeholder));

            String nodesStr = coms.get(7);
            assertTrue(nodesStr.contains(placeholder));

            String txsStr = coms.get(8);
            assertTrue(txsStr.contains(placeholder));
        }
        else {
            fail();
        }
    }

    @Test
    public void testConstructor2() {
        PropagationLossModel propagationLossModel = PropagationLossModel.URBAN;
        WaveSimNS3Impl wsWithoutCell = new WaveSimNS3Impl(null, twsdl, jndiParams, null, new ArrayList<CellTower>(), cellConfig, propagationLossModel);
        List<String> coms = wsWithoutCell.dsrcCommands;

        if (coms.size() == 5) {

            String environmentStr = coms.get(1);
            assertTrue(environmentStr.contains(propagationLossModel.getName().toLowerCase()));

            // these shouldn't have actual data yet.
            String placeholder = "Placeholder";

            String txNumStr = coms.get(2);
            assertTrue(txNumStr.contains(placeholder));

            String nodesStr = coms.get(3);
            assertTrue(nodesStr.contains(placeholder));

            String txsStr = coms.get(4);
            assertTrue(txsStr.contains(placeholder));
        }
        else {
            fail();
        }
    }

    @Test
    public void testCreateTxList() {
        WaveSimNS3Impl wsWithCell = new WaveSimNS3Impl(null, new TestWaveSimDataLayerWithCell(), jndiParams, null, cellTowers, cellConfig, PropagationLossModel.URBAN);

        Iterable<Tx> nodes = wsWithCell.data.getTxs(0, null);
        Map<Long, Integer> macToNodeListIndex = new HashMap<Long, Integer>();
        Map<Integer, String> txIdToMsgId = new HashMap<Integer, String>();
        Map<Integer, String> testMapTxIdToMsgId = new HashMap<Integer, String>();

        int messageMac = 0;
        int index = 0;
        for (Tx node : nodes) {
            macToNodeListIndex.put(node.mac, index);
            for (WaveMessage message : node.outgoingMessages) {
                testMapTxIdToMsgId.put(messageMac, message.messageId);
                messageMac++;
            }
            index++;
        }

        String result = wsWithCell.createTxList(nodes, macToNodeListIndex, txIdToMsgId);

        assertTrue("-1:500:0;1:1000:1;0:222:2;4:1000:3;3:1000:4".equals(result));
        assertTrue(txIdToMsgId.equals(testMapTxIdToMsgId));
    }

    @Test
    public void testTransmitMessages() {
        WaveSimNS3Impl ws = Mockito.spy(new WaveSimNS3Impl(null, twsdl, jndiParams, null, new ArrayList<CellTower>(), cellConfig, PropagationLossModel.URBAN));

        PowerMockito.doReturn("5,0,1;5,0,2;10,1,0;21,2,1;").when(ws).runNS3(Mockito.anyListOf(String.class), Mockito.anyString());

        ws.transmitMessages(1);
        Collection<Rx> actual = twsdl.getMessages(1);
        List<Rx> expected = new ArrayList<Rx>(3);

        Rx nm1 = new Rx(789);
        nm1.messages.put("id2", 2);
        Rx nm2 = new Rx(456);
        nm2.messages.put("id0", 2);
        nm2.messages.put("id1", 2);
        Rx nm3 = new Rx(123);
        nm3.messages.put("id0", 2);
        expected.add(nm1);
        expected.add(nm2);
        expected.add(nm3);

        List<Rx> actualList = new ArrayList<Rx>(3);
        for (Rx rx : actual) {
            actualList.add(rx);
        }
        assertEquals(expected, actualList);
    }

    @Test
    public void testTransmitMessagesWithDSRCTopography() throws Exception {

        WaveSimNS3Impl ws = Mockito.spy(new WaveSimNS3Impl(null, dataForTopography, jndiParams, new DSRCTopography(this.genTopography()), cellTowers, cellConfig, PropagationLossModel.URBAN));

        PowerMockito.doReturn("14,1,3;24,0,1;").doReturn("14,0,1;").when(ws).runNS3(Mockito.anyListOf(String.class), Mockito.anyString());

        // This test has 2 buildings with 3 messages being sent. The first DSRC message has a
        // building
        // between source and destination, so it should be blocked by the topography and the
        // Cellular message also has a building between the source and the destination however the
        // topography used should ignore cellular messages. The last DSRC message does not have a
        // building between the source and destination, so it should not be dropped.
        ws.transmitMessages(stepNum);

        Collection<Rx> receivedMessages = dataForTopography.outputs.get(stepNum);

        assertEquals(2, dataForTopography.outputs.get(stepNum).size());
        for (Rx rx : receivedMessages) {
            assertTrue(rx.mac == 4 || rx.mac == 6);
        }

    }

    @Test
    public void testTransmitMessagesWithMaxDistance() {

        createTestWithMaxDistance(WaveSimLayer.MAX_DSRC_MESSAGE_DISTANCE_URBAN, PropagationLossModel.URBAN);
        createTestWithMaxDistance(WaveSimLayer.MAX_DSRC_MESSAGE_DISTANCE_URBAN, null);
        createTestWithMaxDistance(WaveSimLayer.MAX_DSRC_MESSAGE_DISTANCE_SUBURBAN, PropagationLossModel.SUBURBAN);
        createTestWithMaxDistance(WaveSimLayer.MAX_DSRC_MESSAGE_DISTANCE_OPEN, PropagationLossModel.OPEN);
    }

    private void createTestWithMaxDistance(int maxDistance, PropagationLossModel propagationLossModel) {
        org.etexascode.datalayer.tests.TestWaveSimDataLayer data = new org.etexascode.datalayer.tests.TestWaveSimDataLayer();

        List<WaveMessage> waveMessages1 = new ArrayList<WaveMessage>(1);
        waveMessages1.add(new WaveMessage("id2", 2, 1000));

        List<WaveMessage> waveMessages2 = new ArrayList<WaveMessage>(1);
        waveMessages2.add(new WaveMessage("id3", 3, 1000));

        List<Tx> txs = new ArrayList<Tx>();
        // the message shouldn't make it to node 2
        txs.add(new Tx(MessageType.DSRC, new DistanceImpl(0.0, 0.0, 0.0), 1, waveMessages1));
        // the message should make it to node 3
        txs.add(new Tx(MessageType.DSRC, new DistanceImpl(maxDistance + 5, 0.0, 0.0), 2, waveMessages2));
        txs.add(new Tx(MessageType.DSRC, new DistanceImpl(maxDistance * 2 - 5, 0.0, 0.0), 3, new ArrayList<WaveMessage>()));

        data.inputs = txs;
        WaveSimNS3Impl waveSim = Mockito.spy(new WaveSimNS3Impl(null, data, jndiParams, new DSRCTopography(this.genTopography()), cellTowers, cellConfig, propagationLossModel));
        PowerMockito.doReturn("14,1,2;24,0,1;").when(waveSim).runNS3(Mockito.anyListOf(String.class), Mockito.anyString());
        waveSim.transmitMessages(0);

        Collection<Rx> receivedMessages = data.outputs.get(0);

        assertTrue(receivedMessages.size() == 1);
        for (Rx rx : receivedMessages) {
            assertTrue(rx.mac == 3);
        }
    }

    private class TestWaveSimDataLayer implements IWaveSimDataLayer {

        public Map<Integer, Collection<Rx>> messages = new HashMap<Integer, Collection<Rx>>();

        @Override
        public List<Tx> getTxs(int stepNum, Polygon segment) {
            WaveMessage mess0 = new WaveMessage("id0", WaveMessage.MACBROADCAST, 500);
            WaveMessage mess1 = new WaveMessage("id1", 456, 1000);
            WaveMessage mess2 = new WaveMessage("id2", 789, 222);

            DistanceImpl node1 = new DistanceImpl(0, 0, 0);
            DistanceImpl node2 = new DistanceImpl(1, 1, 0);
            DistanceImpl node3 = new DistanceImpl(2, 0, 0);

            List<Tx> ret = new ArrayList<Tx>(3);
            ret.add(new Tx(MessageType.DSRC, node1, 789, new ArrayList<WaveMessage>(Arrays.asList(mess0))));
            ret.add(new Tx(MessageType.DSRC, node2, 456, new ArrayList<WaveMessage>(Arrays.asList(mess2))));
            ret.add(new Tx(MessageType.DSRC, node3, 123, new ArrayList<WaveMessage>(Arrays.asList(mess1))));

            return ret;
        }

        @Override
        public void putMessages(int stepNum, Collection<Rx> nodes) {
            messages.put(stepNum, nodes);
        }

        @Override
        public double getSimTime(int stepNum) {
            return 0 + stepNum * getStepSize();
        }

        @Override
        public double getStepSize() {
            return 0.5;
        }

        public Collection<Rx> getMessages(int stepNum) {
            return messages.get(stepNum);
        }
    }

    private class TestWaveSimDataLayerWithCell implements IWaveSimDataLayer {

        public Map<Integer, Collection<Rx>> messages = new HashMap<Integer, Collection<Rx>>();

        @Override
        public List<Tx> getTxs(int stepNum, Polygon segment) {
            WaveMessage mess0 = new WaveMessage("id0", WaveMessage.MACBROADCAST, 500);
            WaveMessage mess1 = new WaveMessage("id1", 456, 1000);
            WaveMessage mess2 = new WaveMessage("id2", 789, 222);
            WaveMessage mess3 = new WaveMessage("id3", 12, 1000);
            WaveMessage mess4 = new WaveMessage("id4", 21, 1000);

            DistanceImpl node1 = new DistanceImpl(0, 0, 0);
            DistanceImpl node2 = new DistanceImpl(1, 1, 0);
            DistanceImpl node3 = new DistanceImpl(2, 0, 0);
            DistanceImpl node4 = new DistanceImpl(30, 2, 5);
            DistanceImpl node5 = new DistanceImpl(25, 3234, 23);

            List<Tx> ret = new ArrayList<Tx>(5);
            ret.add(new Tx(MessageType.DSRC, node1, 789, new ArrayList<WaveMessage>(Arrays.asList(mess0))));
            ret.add(new Tx(MessageType.DSRC, node2, 456, new ArrayList<WaveMessage>(Arrays.asList(mess1))));
            ret.add(new Tx(MessageType.DSRC, node3, 123, new ArrayList<WaveMessage>(Arrays.asList(mess2))));
            ret.add(new Tx(MessageType.CELLULAR, node4, 21, new ArrayList<WaveMessage>(Arrays.asList(mess3))));
            ret.add(new Tx(MessageType.CELLULAR, node5, 12, new ArrayList<WaveMessage>(Arrays.asList(mess4))));

            return ret;
        }

        @Override
        public void putMessages(int stepNum, Collection<Rx> nodes) {
            messages.put(stepNum, nodes);
        }

        @Override
        public double getSimTime(int stepNum) {
            return 0 + stepNum * getStepSize();
        }

        @Override
        public double getStepSize() {
            return 0.5;
        }

        public Collection<Rx> getMessages(int stepNum) {
            return messages.get(stepNum);
        }
    }

    private CellTower genCellTower() {
        CellTower cellTower = new CellTower();
        cellTower.setProvider("provider");
        cellTower.setX(14);
        cellTower.setY(23);
        cellTower.setZ(-29);
        return cellTower;
    }

    private CellularConfig genCellConfig() {
        CellularConfig cellConfig = new CellularConfig();
        cellConfig.setUplinkBandwidth(25);
        cellConfig.setUplinkCarrierFrequency(18100);
        cellConfig.setDownlinkBandwidth(25);
        cellConfig.setDownlinkCarrierFrequency(100);
        cellConfig.setCellTxPower(10);
        cellConfig.setCellNoiseFigure(9);
        cellConfig.setCellTowerTxPower(30);
        cellConfig.setCellTowerNoiseFigure(5);
        return cellConfig;
    }

    private Collection<ITopographyFeature> genTopography() {
        List<ITopographyFeature> features = new ArrayList<ITopographyFeature>(4);

        double height = 500;

        List<Point2D> points = new ArrayList<Point2D>();
        points.add(new Point2D.Double(0, 0));
        points.add(new Point2D.Double(100, 0));
        points.add(new Point2D.Double(100, 100));
        points.add(new Point2D.Double(0, 100));

        features.add(new TopographyPolygon(0, points, height));

        points = new ArrayList<Point2D>();
        points.add(new Point2D.Double(400, 400));
        points.add(new Point2D.Double(500, 400));
        points.add(new Point2D.Double(500, 500));
        points.add(new Point2D.Double(400, 500));

        features.add(new TopographyPolygon(1, points, height));

        return features;
    }

    private List<Tx> genTopographyWaveNodes() {
        List<Tx> inputs = new ArrayList<Tx>(6);

        // This DSRC message should not make it
        List<WaveMessage> messages = new ArrayList<WaveMessage>(1);
        messages.add(new WaveMessage("id1", 2, 1000));
        inputs.add(new Tx(MessageType.DSRC, new DistanceImpl(-50.0, 50.0, 50.0), 1, messages));

        messages = new ArrayList<WaveMessage>(0);
        inputs.add(new Tx(MessageType.DSRC, new DistanceImpl(150.0, 50.0, 50.0), 2, messages));

        // Cellular should be ignored by the DSRC topography
        messages = new ArrayList<WaveMessage>(1);
        messages.add(new WaveMessage("id2", 4, 1000));
        inputs.add(new Tx(MessageType.CELLULAR, new DistanceImpl(-50.0, 50.0, 50.0), 3, messages));

        messages = new ArrayList<WaveMessage>(0);
        inputs.add(new Tx(MessageType.CELLULAR, new DistanceImpl(150.0, 50.0, 50.0), 4, messages));

        // This DSRC message should make it
        messages = new ArrayList<WaveMessage>(1);
        messages.add(new WaveMessage("id3", 6, 1000));
        inputs.add(new Tx(MessageType.DSRC, new DistanceImpl(150.0, -50.0, 50.0), 5, messages));

        messages = new ArrayList<WaveMessage>(0);
        inputs.add(new Tx(MessageType.DSRC, new DistanceImpl(150.0, 500.0, 50.0), 6, messages));

        return inputs;
    }
}
