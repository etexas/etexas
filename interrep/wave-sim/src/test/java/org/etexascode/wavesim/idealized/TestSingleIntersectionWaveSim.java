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
package org.etexascode.wavesim.idealized;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.etexascode.datalayer.tests.TestWaveSimDataLayer;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.topography.DSRCTopography;
import org.etexascode.interrep.topography.ITopographyFeature;
import org.etexascode.interrep.topography.TopographyPolygon;
import org.etexascode.wavesim.PropagationLossModel;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;
import org.etexascode.wavesim.Tx.MessageType;
import org.etexascode.wavesim.WaveMessage;
import org.etexascode.wavesim.WaveSimLayer;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 */
public class TestSingleIntersectionWaveSim {

    TestWaveSimDataLayer data = null;

    TestWaveSimDataLayer dataForTopography = null;

    Map<Integer, Collection<Rx>> expected = null;

    IdealizedWaveSim siws = null;

    IdealizedWaveSim idealizedWithDSRCTopography = null;

    int stepNum = 42;

    @Before
    public void setup() {
        data = new TestWaveSimDataLayer();
        data.inputs = getWaveNodes();
        dataForTopography = new TestWaveSimDataLayer();
        dataForTopography.inputs = genTopographyWaveNodes();
        expected = getExpected();
        siws = new IdealizedWaveSim(null, new PacketAlwaysMakesIt(), new PacketThereNextTimeStep(), data, null, PropagationLossModel.URBAN);
        idealizedWithDSRCTopography = new IdealizedWaveSim(null, new PacketAlwaysMakesIt(), new PacketThereNextTimeStep(), dataForTopography, new DSRCTopography(this.genTopography()),
                PropagationLossModel.URBAN);
    }

    @After
    public void teardown() {
        data = null;
        dataForTopography = null;
        expected = null;
        siws = null;
        idealizedWithDSRCTopography = null;
    }

    @Test
    public void testConstructor() {
        IdealizedWaveSim sim = new IdealizedWaveSim(null, new PacketAlwaysMakesIt(), new PacketThereNextTimeStep(), data, new DSRCTopography(new ArrayList<ITopographyFeature>()),
                PropagationLossModel.URBAN);
        assertTrue(sim.data == data);
    }

    @Test
    public void testTransmitMessages() {
        siws.transmitMessages(stepNum);

        // ensure they are the same size
        assertEquals(data.outputs.size(), expected.size());

        // iterate to ensure the same contents
        for (Integer i : data.outputs.keySet()) {
            assertTrue(expected.containsKey(i));

            Iterator<Rx> it = data.outputs.get(i).iterator();
            while (it.hasNext()) {
                assertTrue(expected.get(i).contains(it.next()));
            }
        }

    }

    @Test
    public void testTransmitMessagesWithDSRCTopography() {
        // This test has 2 buildings with 3 messages being sent. The first DSRC message has a
        // building
        // between source and destination, so it should be blocked by the topography and the
        // Cellular message also has a building between the source and the destination however the
        // topography used should ignore cellular messages. The last DSRC message does not have a
        // building between the source and destination, so it should not be dropped.
        idealizedWithDSRCTopography.transmitMessages(stepNum);

        Collection<Rx> receivedMessages = dataForTopography.outputs.get(stepNum);

        assertEquals(2, receivedMessages.size());
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
        TestWaveSimDataLayer data = new TestWaveSimDataLayer();

        List<WaveMessage> waveMessages1 = new ArrayList<WaveMessage>(1);
        waveMessages1.add(new WaveMessage("id2", 2, 1000));

        List<WaveMessage> waveMessages2 = new ArrayList<WaveMessage>(1);
        waveMessages2.add(new WaveMessage("id3", 3, 1000));

        List<Tx> txs = new ArrayList<Tx>();
        // the message shouldn't make it to node 2
        txs.add(new Tx(MessageType.DSRC, new DistanceImpl(0.0, 0.0, 0.0), 1, waveMessages1));
        // the message should make it to node 3
        txs.add(new Tx(MessageType.DSRC, new DistanceImpl(maxDistance + 1, 0.0, 0.0), 2, waveMessages2));
        txs.add(new Tx(MessageType.DSRC, new DistanceImpl(maxDistance * 2 - 1, 0.0, 0.0), 3, new ArrayList<WaveMessage>()));

        data.inputs = txs;
        IdealizedWaveSim waveSim = new IdealizedWaveSim(null, new PacketAlwaysMakesIt(), new PacketThereNextTimeStep(), data, null, propagationLossModel);
        waveSim.transmitMessages(0);

        Collection<Rx> receivedMessages = data.outputs.get(0);

        assertTrue(receivedMessages.size() == 1);
        for (Rx rx : receivedMessages) {
            assertTrue(rx.mac == 3);
        }
    }

    List<Tx> getWaveNodes() {
        List<Tx> ret = new ArrayList<Tx>(3);

        ret.add(new Tx(MessageType.DSRC, new DistanceImpl(50.0, 50.0, 0.0), 1, getWaveMessages1()));

        ret.add(new Tx(MessageType.DSRC, new DistanceImpl(100.0, 100.0, 0.0), 2, getWaveMessages2()));

        ret.add(new Tx(MessageType.DSRC, new DistanceImpl(150.0, 150.0, 0.0), 3, getWaveMessages3()));

        return ret;
    }

    List<WaveMessage> getWaveMessages1() {
        List<WaveMessage> ret = new ArrayList<WaveMessage>(1);
        ret.add(new WaveMessage("id1", WaveMessage.MACBROADCAST, 1000));
        return ret;
    }

    List<WaveMessage> getWaveMessages2() {
        List<WaveMessage> ret = new ArrayList<WaveMessage>(1);
        ret.add(new WaveMessage("id2", 3, 1000));
        return ret;
    }

    List<WaveMessage> getWaveMessages3() {
        return new ArrayList<WaveMessage>(0);
    }

    Map<Integer, Collection<Rx>> getExpected() {
        Map<Integer, Collection<Rx>> ret = new HashMap<Integer, Collection<Rx>>();

        List<Rx> out = new ArrayList<Rx>(3);

        Rx mess = new Rx(1);
        out.add(mess);

        mess = new Rx(2);
        mess.messages.put("id1", stepNum + 1);
        out.add(mess);

        mess = new Rx(3);
        mess.messages.put("id1", stepNum + 1);
        mess.messages.put("id2", stepNum + 1);
        out.add(mess);

        ret.put(stepNum, out);
        return ret;
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
