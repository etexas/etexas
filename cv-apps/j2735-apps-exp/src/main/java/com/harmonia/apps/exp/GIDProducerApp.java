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
package com.harmonia.apps.exp;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;

import org.etexascode.apps.IRSEBaseApp;
import org.etexascode.apps.RSEDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneMovement;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735.exp.ApproachLaneExp;
import org.etexascode.j2735.exp.ApproachObjectExp;
import org.etexascode.j2735.exp.GIDExp;
import org.etexascode.j2735.exp.IntersectionExp;
import org.etexascode.j2735.util.SizeCalculators;
import org.etexascode.wavesim.WaveMessage;

/**
 * App for producing GIDExp messages Parameters: Standard rse app paramters Return value: a list
 * containing a single GIDExp message.
 * 
 * @author ablatt
 */
public class GIDProducerApp implements IRSEBaseApp, IAppLifecycle {

    /** The last recorded time a transmission was made. */
    private Double lastTxTime;

    /** The frequency at which to produce messages. */
    @AppConfigProperty(value = "1.0")
    private double frequency;

    /**
     * Get the message attributes for the GID message.
     * 
     * @return the message attributes.
     */
    static byte[] getMessageAttributes() {
        return new byte[] { (byte)0x4 };
    }

    /**
     * Format the intersection id for the message.
     * 
     * @param lmi The lane manager.
     * @return Gets the intersection id.
     */
    static byte[] getIntersectId(ILaneManager lmi) {
        ByteBuffer bb = ByteBuffer.allocate(4);
        bb.putInt(lmi.getIntersectionId());
        return bb.array();
    }

    /**
     * Format the lat-lon from the lane manager into a reference point. (TODO: ablatt - will want to
     * use the reference points we are currently taking in once we figure out what they mean)
     * 
     * @param lmi The lane manager.
     * @return Gets the reference point.
     */
    static byte[] getReferencePoint(ILaneManager lmi) {
        ByteBuffer bb = ByteBuffer.allocate(8);
        bb.putInt(UtilsUnitConversion.convertToOneTenthMicrodegree(lmi.getLatitude()));
        bb.putInt(UtilsUnitConversion.convertToOneTenthMicrodegree(lmi.getLongitude()));
        return bb.array();
    }

    /**
     * Get the attributes (formatted in accordance with the GID specification) for the lane li.
     * 
     * @param li The lane to get attributes for.
     * @return Get the lane attributes.
     */
    static byte[] getLaneAttributes(ILane li) {
        int ret = 0;
        boolean noTurnOnRed = true;
        boolean noUTurn = true;

        for (ILaneMovement lmi : li.getLaneMovements().values()) {
            if (lmi.getMovement() == LaneMovement.Movement.STRAIGHT) {
                ret |= 2;
            }
            else if ((lmi.getMovement() == LaneMovement.Movement.LEFT_TURN) || (lmi.getMovement() == LaneMovement.Movement.LEFT_TURN_ON_RED)) {
                ret |= 4;
            }
            else if ((lmi.getMovement() == LaneMovement.Movement.RIGHT_TURN) || (lmi.getMovement() == LaneMovement.Movement.RIGHT_TURN_ON_RED)) {
                ret |= 8;
            }

            if ((lmi.getMovement() == LaneMovement.Movement.LEFT_TURN_ON_RED) || (lmi.getMovement() == LaneMovement.Movement.RIGHT_TURN_ON_RED)) {
                noTurnOnRed = false;
            }
            else if (lmi.getMovement() == LaneMovement.Movement.U_TURN) {
                noUTurn = false;
            }
        }

        if (noTurnOnRed) {
            ret |= 64;
        }

        if (noUTurn) {
            ret |= 32;
        }

        ByteBuffer bb = ByteBuffer.allocate(2);
        bb.putShort((short)ret);
        return bb.array();
    }

    /**
     * Get the node list (formatted in accordance with the GID specification) for the lane li
     * 
     * @param li The lane to get the node list for.
     * @return Gets the node list.
     */
    static byte[] getNodeList(ILane li) {
        ByteBuffer bb = ByteBuffer.allocate(4 * li.getLaneGeomList().size());

        double eastOffset = 0;
        double northOffset = 0;

        for (ILaneNode lni : li) {
            bb.putShort((short)(lni.getX() - eastOffset));
            bb.putShort((short)(lni.getY() - northOffset));

            eastOffset = lni.getX();
            northOffset = lni.getY();
        }

        return bb.array();
    }

    /**
     * Get an approach lane object for an xml from a lane
     * 
     * @param li The lane to get the approach object for.
     * @return Gets the approach lane.
     */
    static ApproachLaneExp getApproachLaneFromLane(ILane li) {
        ApproachLaneExp ret = new ApproachLaneExp();

        ret.getLane().setPayload(new byte[] { (byte)li.getLaneId(), (byte)1 });
        ret.getLaneAttributes().setPayload(getLaneAttributes(li));
        ret.getNodeList().setPayload(getNodeList(li));

        return ret;
    }

    /**
     * Get an approach in accordance with the GID specification
     * 
     * @param type The type of lane (Inbound or Outbound).
     * @return Gets an approach with the GID specs.
     */
    static byte[] getApproach(String type) {
        if (type.equals(Lane.INBOUND)) {
            return new byte[] { (byte)1 };
        }
        else if (type.equals(Lane.OUTBOUND)) {
            return new byte[] { (byte)2 };
        }
        else {
            return new byte[] { (byte)3 };
        }
    }

    /**
     * Get all the lanes of the provided type from the lane manager
     * 
     * @param lmi The lane manager.
     * @param type The type of lane to get (inbound or outbound)
     * @return Gets all the lanes of the provided type from the lane manager.
     */
    static List<ILane> getLanes(ILaneManager lmi, String type) {
        List<ILane> ret = new ArrayList<ILane>();

        for (Integer i : lmi.getLaneIds()) {
            ILane li = lmi.getLaneById(i);
            if (li.getType().equals(type)) {
                ret.add(li);
            }
        }

        return ret;
    }

    /**
     * Get the vehicle lanes of the stated type
     * 
     * @param lmi The lane manager.
     * @param type The type of lane to get (inbound or outbound)
     * @return Gets the vehicle lanes.
     */
    static ApproachObjectExp getVehicleLanes(ILaneManager lmi, String type) {
        ApproachObjectExp ret = new ApproachObjectExp();
        ret.getApproach().setPayload(getApproach(type));
        List<ILane> lanes = getLanes(lmi, type);

        if (type.equals(Lane.INBOUND) || type.equals(Lane.OUTBOUND)) {
            for (ILane li : lanes) {
                ret.getVehicleLanes().getApproachLaneObject().add(getApproachLaneFromLane(li));
            }
        }
        else {
            return null; // we've got nothing on this
        }

        return ret;
    }

    /**
     * Get the information on the intersection represented by the lane manager
     * 
     * @param lmi The lane manager.
     * @return Gets the intersection represented by the lane manager.
     */
    static IntersectionExp getIntersectionFromLaneMan(ILaneManager lmi) {
        IntersectionExp ret = new IntersectionExp();

        ret.getAttributes().setPayload(getMessageAttributes());
        ret.getIntersectionId().setPayload(getIntersectId(lmi));
        ret.getReferencePoint().setPayload(getReferencePoint(lmi));
        ret.getApproaches().getApproachObject().add(getVehicleLanes(lmi, Lane.INBOUND));
        ret.getApproaches().getApproachObject().add(getVehicleLanes(lmi, Lane.OUTBOUND));

        return ret;
    }

    /**
     * Construct a GIDExp message out of the lane manager.
     * 
     * @param lmi The lane manager.
     * @return Gets a GID message from the lane manager.
     */
    static GIDExp getGIDFromLaneMan(ILaneManager lmi) {
        GIDExp ret = new GIDExp();
        ret.setContentVersion((byte)0);
        ret.getIntersections().getIntersection().add(getIntersectionFromLaneMan(lmi));
        return ret;
    }

    @Override
    public void init(String[] appConfigs) {
        frequency = Double.parseDouble(appConfigs[0]);
    }

    @Override
    public void performUpdate(RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {
        if (lastTxTime != null) {
            if (simTime - lastTxTime < frequency) {
                return;
            }
        }

        lastTxTime = simTime;
        List<BasicMessage> ret = new ArrayList<BasicMessage>();

        for (Entry<Integer, ILaneManager> laneManagerEntry : device.getLaneManagers().entrySet()) {

            GIDExp msg = getGIDFromLaneMan(laneManagerEntry.getValue());
            logger.log("GIDExp size", SizeCalculators.getSize(msg) + " bytes.");
            ret.add(new DSRCMessage(msg, DSRCChannel.CH184, WaveMessage.MACBROADCAST, msg.getSize()));
        }

        // code for creating an XML and logging it (good for debugging)
        /*
         * try { ByteArrayOutputStream stream = new ByteArrayOutputStream(); JAXBContext context =
         * JAXBContext.newInstance(SPAT.Intersections.class); Marshaller marsh =
         * context.createMarshaller(); marsh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
         * marsh.marshal((GIDExp)ret.get(0), stream); String temp = new
         * String(stream.toByteArray()); logger.log("xml", temp); } catch (JAXBException e) { throw
         * new RuntimeException(e); }
         */
        device.addAppMessages(ret);
    }

    @Override
    public void appShutdown(AppLogger logger) {}
}