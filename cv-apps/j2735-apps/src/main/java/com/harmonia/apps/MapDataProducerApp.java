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
package com.harmonia.apps;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.codec.binary.Base64;
import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.DEREnumerated;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERTaggedObject;
import org.etexascode.apps.IRSEBaseApp;
import org.etexascode.apps.MapDataUtil;
import org.etexascode.apps.RSEDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.FormattedDSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneMovement;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneMovement;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735.Approach;
import org.etexascode.j2735.ApproachObject;
import org.etexascode.j2735.Intersection;
import org.etexascode.j2735.MapData;
import org.etexascode.j2735.Position3D;
import org.etexascode.j2735.VehicleReferenceLane;
import org.etexascode.j2735.util.DSRCMessageID;
import org.etexascode.j2735.util.VehicleLaneAttribute;
import org.etexascode.wavesim.WaveMessage;

/**
 * A MapData producing Connected Vehicle App.
 * 
 * @author bbadillo
 * @author ablatt
 * @author bmauldon
 * @author ttevendale
 */
public class MapDataProducerApp implements IRSEBaseApp, IAppLifecycle, IAppName {

    /**
     * The application identifier.
     */
    public static final String APP_NAME_MAP_DATA_PRODUCER_APP = "MapDataProducerApp";

    /**
     * SPAT dsrc message id (is always the same).
     */
    public static final ASN1Encodable DSRCmsgID = new DERTaggedObject(false, 0, new DEREnumerated(7)); // permanent
                                                                                                       // MAP
                                                                                                       // Id

    /**
     * Array used to convert a list into an array (dealing with a type/efficiency mismatch)
     */
    static final ASN1Encodable[] CONVERSION_ARRAY = new ASN1Encodable[0];

    /**
     * The last recorded crc
     */
    private byte[] crcBytes = null;

    /**
     * The last recorded time a transmission was made.
     */
    private Double lastTxTime;

    /**
     * An incremented counter for the current number of generated messages (modulo 128).
     */
    private short msgCount = 0;

    @AppConfigProperty(value = "false", description = "The indication of whether or not to include the formatted message data.")
    boolean hasFormattedData;

    /**
     * The frequency at which to produce messages in seconds. No recommended default in SPEC.
     */
    @AppConfigProperty(value = "1.0", description = "The frequency to send messages in seconds.")
    double frequency;

    /**
     * Store the MapData so we don't generate it each time.
     */
    private List<BasicMessage> mapDataRequests = null;

    /**
     * Build an approach for a specific leg of an intersection. Set that approach in the correct
     * spot in the intersection.
     * 
     * @param isEgress The leg of the intersection is moving away from the intersection. (true or
     *        false?)
     * @param leg The leg of the intersection to make an approach out of.
     * @param legID The id of the leg.
     * @return The approach that accurately represents the leg of the intersection.
     */

    private static Approach buildApproach(boolean isEgress, ApproachObject leg, int legID) {
        Approach ret;
        // TODO: ablatt - this could use some rewriting. It is either using some
        // slight of hand to set the egress value in leg early, or checking
        // egress against null is pointless...
        if (isEgress) {
            ret = leg.getEgress();
            if (ret == null) {
                ret = new Approach();
                leg.setEgress(ret);
            }
        }
        else {
            ret = leg.getApproach();
            if (ret == null) {
                ret = new Approach();
                leg.setApproach(ret);
            }
        }

        ret.setId((short)legID);

        return ret;
    }

    /**
     * Clips a lane so that every node can be represented using centimeter offsets from the
     * reference point stored in a short. Also shifts the lane to be relative to a given reference
     * point.
     * 
     * @param lane The lane to clip.
     * @param baseLat The latitude of the old reference point.
     * @param baseLong The longitude of the old reference point.
     * @param curLat The latitude of the new reference point.
     * @param curLong The longitude of the new reference point.
     * @param geoCalcType The type of calculator to use for latitude/longitude operations.
     * @return The clipped + shifted list of nodes.
     */
    static List<ILaneNode> clip(List<ILaneNode> lane, double baseLat, double baseLong, double curLat, double curLong, int geoCalcType) {
        double[] offsets = UtilsLatLongConversion.convertLatLongToCentimeterOffset(baseLat, baseLong, curLat, curLong, geoCalcType);
        offsets[0] = Math.round(offsets[0] * 1000) / 1000.0;
        offsets[1] = Math.round(offsets[1] * 1000) / 1000.0;

        List<ILaneNode> ret = new ArrayList<ILaneNode>();

        for (int k = 0; k < lane.size() - 1; ++k) {
            ILaneNode n1 = lane.get(k);
            ILaneNode n2 = lane.get(k + 1);

            double[] clipPts = MapDataUtil.cohenSutherland(n1.getX(), n1.getY(), n2.getX(), n2.getY(), offsets[0] + Short.MIN_VALUE, offsets[0] + Short.MAX_VALUE, offsets[1] + Short.MIN_VALUE,
                    offsets[1] + Short.MAX_VALUE);

            if (clipPts != null) {
                ILaneNode clipn1 = new LaneNode(clipPts[0] - offsets[0], clipPts[1] - offsets[1], n1.getZ(), n1.getWidth());
                ILaneNode clipn2 = new LaneNode(clipPts[2] - offsets[0], clipPts[3] - offsets[1], n2.getZ(), n2.getWidth());

                if (ret.size() >= 1 && clipn1.equals(ret.get(ret.size() - 1))) {
                    // already added the first node in this pair
                }
                else {
                    ret.add(clipn1);
                }
                ret.add(clipn2);
            }
        }

        return ret;
    }

    /**
     * Convenience method for single intersection, single reference point models.
     * 
     * @param laneManager The LaneManager representing the intersection.
     * @param msgCount The iteration of the message.
     * @param refLat The latitude of the reference point.
     * @param refLong The longitude of the reference point.
     * @return The message size in bytes
     */
    static MapData createFormattedMapDataMessage(ILaneManager laneManager, short msgCount, byte[] crcBytes, double refLat, double refLong) {
        List<ReferencePoint> rps = new LinkedList<ReferencePoint>();
        rps.add(new ReferencePoint(refLat, refLong));

        return createFormattedMapDataMessage(laneManager, msgCount, crcBytes, rps);
    }

    /**
     * Creates a message containing the lane geometry for an intersection.
     * 
     * @param laneManager The LaneManager for the intersection.
     * @param msgCount The current iteration of the message.
     * @param referencePoints The reference points we can express lanes relative to.
     * @return size the message size in bytes.
     */
    static MapData createFormattedMapDataMessage(ILaneManager laneManager, short msgCount, byte[] crcBytes, List<ReferencePoint> referencePoints) {
        // TODO: ablatt - I could not find any code listing lane connections
        // being put into the messages
        // Object factory

        MapData message = new MapData();

        message.setMsgID(DSRCMessageID.MSG_ID_MAPDATA);
        message.setMsgCnt(msgCount);
        message.setCrc(crcBytes);

        MapData.Intersections intersections = new MapData.Intersections();
        message.setIntersections(intersections);
        List<Intersection> intersectionList = intersections.getIntersection();

        // TODO: bbadillo - decide what to put in the Intersection ID field
        // NOTE: janway - must be 32 bits
        byte[] intersectionId = new byte[] { (byte)0, (byte)0, (byte)0, (byte)0 };

        for (ReferencePoint rf : referencePoints) {
            double refLat = rf.getLatitude();
            double refLong = rf.getLongitude();

            Intersection intersection = new Intersection();
            intersection.setId(intersectionId);
            Position3D pos3D = new Position3D();
            int latitude = UtilsUnitConversion.convertToOneTenthMicrodegree(refLat);
            int longitude = UtilsUnitConversion.convertToOneTenthMicrodegree(refLong);
            pos3D.setLat(latitude);
            pos3D.setLong(longitude);

            intersection.setRefPoint(pos3D);

            intersectionList.add(intersection);
            Intersection.Approaches approaches = new Intersection.Approaches();
            intersection.setApproaches(approaches);
            List<ApproachObject> approachList = approaches.getApproachObject();
            // Iterate over all lanes in the model
            for (ILane lane : laneManager) {
                // Set the lane ID to be the index of the lane in the model
                byte laneID = (byte)lane.getLaneId();
                // Get the ID of the leg the the approach comprises
                int legID = lane.getApproachId();

                // Grow the approach list to the required size and obtain the
                // correct approach object
                // TODO: ablatt - generate this list once pre-loop and simply
                // use that...
                int lid = legID - 1;
                while (lid >= approachList.size()) {
                    approachList.add(new ApproachObject());
                }
                ApproachObject leg = approachList.get(lid);

                // Find or create the approach to add the vehicle lane too
                boolean isEgress = Lane.OUTBOUND.equals(lane.getType());
                Approach approach = buildApproach(isEgress, leg, legID);
                Approach.DrivingLanes drivingLanes = populateDrivingLanes(approach);
                List<VehicleReferenceLane> vehicleReferenceLanes = drivingLanes.getVehicleReferenceLane();

                // Generate a reference lane
                VehicleReferenceLane vehicleReferenceLane = new VehicleReferenceLane();

                // Set the lane reference ID
                vehicleReferenceLane.setLaneNumber(new byte[] { laneID });

                // Set the lane turning attributes
                if (isEgress) {
                    vehicleReferenceLane.setLaneAttributes(VehicleLaneAttribute.EGRESS_PATH);

                }
                else {
                    int vehicleLaneAttribute = 0;
                    for (ILaneMovement laneMovement : lane.getLaneMovements().values()) {
                        vehicleLaneAttribute = vehicleLaneAttribute | getVehicleLaneAttributesFromLaneMovement(laneMovement);
                    }
                    vehicleReferenceLane.setLaneAttributes(vehicleLaneAttribute);
                }

                populateNodeList(vehicleReferenceLane, lane, laneManager.getLatitude(), laneManager.getLongitude(), refLat, refLong, laneManager.getGeoCalculatorType());

                vehicleReferenceLanes.add(vehicleReferenceLane);
            }
        }
        return message;
    }

    /**
     * Convert a {@link LaneMovement} value into a {@link VehicleLaneAttribute} value.
     * 
     * @param laneMovement {@link LaneMovement} to convert.
     * @return A {@link VehicleLaneAttribute} value.
     */
    protected static int getVehicleLaneAttributesFromLaneMovement(ILaneMovement laneMovement) {
        switch (laneMovement.getMovement()) {
            case LEFT_TURN:
                return VehicleLaneAttribute.MANEUVER_LEFT_ALLOWED | VehicleLaneAttribute.MANEUVER_NO_TURN_ON_RED;
            case LEFT_TURN_ON_RED:
                return VehicleLaneAttribute.MANEUVER_LEFT_ALLOWED;
            case RIGHT_TURN:
                return VehicleLaneAttribute.MANEUVER_RIGHT_ALLOWED | VehicleLaneAttribute.MANEUVER_NO_TURN_ON_RED;
            case RIGHT_TURN_ON_RED:
                return VehicleLaneAttribute.MANEUVER_RIGHT_ALLOWED;
            case STRAIGHT:
                return VehicleLaneAttribute.MANEUVER_STRAIGHT_ALLOWED;
            case U_TURN:
                // TODO ttevendale - figure out what needs to go here
            default:
                return VehicleLaneAttribute.NO_LANE_DATA;
        }
    }

    /**
     * Populate the driving lanes attribute based on the approach.
     * 
     * @param approach The approach to populate lanes.
     * @return The driving lanes based on the approach.
     */
    private static Approach.DrivingLanes populateDrivingLanes(Approach approach) {
        Approach.DrivingLanes drivingLanes = approach.getDrivingLanes();
        if (drivingLanes == null) {
            drivingLanes = new Approach.DrivingLanes();
            approach.setDrivingLanes(drivingLanes);
        }

        return drivingLanes;
    }

    /**
     * Populate a node list for a specific reference lane.
     * 
     * @param vehicleReferenceLane The reference lane to populate
     * @param lane The info on the lane
     */
    private static void populateNodeList(VehicleReferenceLane vehicleReferenceLane, ILane lane, double baseLat, double baseLong, double curLat, double curLong, int geoCalcType) {

        List<String> nodeList = vehicleReferenceLane.getNodeList();
        @SuppressWarnings("unchecked")
        List<ILaneNode> laneGeomList = (List<ILaneNode>)lane.getLaneGeomList();

        // Clip the lane
        laneGeomList = clip(laneGeomList, baseLat, baseLong, curLat, curLong, geoCalcType);

        for (ILaneNode laneNode : laneGeomList) {
            ByteBuffer baseBuffer = ByteBuffer.allocate(8);
            baseBuffer.putShort(truncateLaneForShortConversion(laneNode.getX()));
            baseBuffer.putShort(truncateLaneForShortConversion(laneNode.getY()));
            baseBuffer.putShort(truncateLaneForShortConversion(laneNode.getZ()));
            baseBuffer.putShort(truncateLaneForShortConversion(laneNode.getWidth()));

            String baseNode = Base64.encodeBase64String(baseBuffer.array());
            nodeList.add(baseNode);
        }
    }

    /**
     * Cast value to a short. If value will not fit into a short, log this issue.
     * 
     * @param value The value we want to cast.
     * @return The short representation of that value.
     */
    private static short truncateLaneForShortConversion(double value) {
        if (value >= Short.MAX_VALUE) {

            return Short.MAX_VALUE;
        }

        if (value <= Short.MIN_VALUE) {

            return Short.MIN_VALUE;
        }

        return (short)value;
    }

    /**
     * Parses out data about a nodelist
     * 
     * @param lane the lane to be parsed
     * @return the nodelist from the lane
     */
    ASN1Encodable parseNodeListToDEREncoding(ILane lane) {
        List<ASN1Encodable> derList = new LinkedList<ASN1Encodable>();

        for (ILaneNode laneNode : lane.getLaneGeomList()) {
            ByteBuffer baseBuffer = ByteBuffer.allocate(8);
            baseBuffer.putShort(truncateLaneForShortConversion(laneNode.getX()));
            baseBuffer.putShort(truncateLaneForShortConversion(laneNode.getY()));
            baseBuffer.putShort(truncateLaneForShortConversion(laneNode.getZ()));
            baseBuffer.putShort(truncateLaneForShortConversion(laneNode.getWidth()));

            byte[] baseNode = baseBuffer.array();
            derList.add(new DEROctetString(baseNode));
        }

        return new DERSequence(derList.toArray(CONVERSION_ARRAY));
    }

    /**
     * Parses out data about a lane
     * 
     * @param lane the lane to be parsed
     * @return the parsed lane data
     */
    DERSequence parseLaneToDEREncoding(ILane lane) {
        int laneAttributes = 0;

        if (lane.getType().equals(Lane.OUTBOUND)) {
            laneAttributes = VehicleLaneAttribute.EGRESS_PATH;

        }
        else {
            for (ILaneMovement laneMovement : lane.getLaneMovements().values()) {
                laneAttributes = laneAttributes | getVehicleLaneAttributesFromLaneMovement(laneMovement);
            }
        }

        return new DERSequence(new ASN1Encodable[] { new DERTaggedObject(false, 0, new DERInteger(lane.getLaneId())), new DERTaggedObject(false, 2, new DERInteger(laneAttributes)),
                new DERTaggedObject(false, 3, parseNodeListToDEREncoding(lane)) });
    }

    /**
     * Parses out data about an approach lane
     * 
     * @param laneList the lane manager to be parsed
     * @return the DERSequence of Approach lanes
     */
    ASN1Encodable parseApproachLaneToDEREncoding(List<ILane> laneList) {
        List<DERSequence> derList = new LinkedList<DERSequence>();

        for (int i = 0; i < laneList.size(); i++) {
            derList.add(parseLaneToDEREncoding(laneList.get(i)));
        }
        DERSequence drivingLanes = new DERSequence(derList.toArray(CONVERSION_ARRAY));

        return new DERSequence(new ASN1Encodable[] { new DERTaggedObject(false, 1, new DERInteger(laneList.get(0).getApproachId())), new DERTaggedObject(false, 2, drivingLanes) });
    }

    /**
     * Parses out the approach objects into a DERSequence
     * 
     * @param laneMan the lane manager to be parsed
     * @return a DERSequence of approach objects
     */
    ASN1Encodable parseApproachLanesToDEREncoding(ILaneManager laneMan) {
        List<ASN1Encodable> derList = new LinkedList<ASN1Encodable>();
        HashMap<Integer, List<ILane>> approachMap = new HashMap<Integer, List<ILane>>();
        Iterator<ILane> lanes = laneMan.getLanes().values().iterator();

        while (lanes.hasNext()) {

            ILane lane = lanes.next();
            List<ILane> laneList;

            if (approachMap.get(lane.getApproachId()) == null) {

                laneList = new LinkedList<ILane>();
                laneList.add(lane);
                approachMap.put(lane.getApproachId(), laneList);
            }
            else {

                approachMap.get(lane.getApproachId()).add(lane);
            }
        }

        for (List<ILane> laneList : approachMap.values()) {

            List<ILane> approacheList = new LinkedList<ILane>();
            List<ILane> egressList = new LinkedList<ILane>();
            DERTaggedObject approaches = null;
            DERTaggedObject egresses = null;
            for (ILane lane : laneList) {

                if (Lane.INBOUND.equals(lane.getType())) {

                    approacheList.add(lane);
                }
                else if (Lane.OUTBOUND.equals(lane.getType())) {

                    egressList.add(lane);
                }
            }
            if (!approacheList.isEmpty()) {

                approaches = new DERTaggedObject(false, 2, parseApproachLaneToDEREncoding(laneList));
            }
            else if (!egressList.isEmpty()) {

                egresses = new DERTaggedObject(false, 3, parseApproachLaneToDEREncoding(laneList));
            }
            if (approaches != null && egresses != null) {

                derList.add(new DERSequence(new ASN1Encodable[] { approaches, egresses }));
            }
            else if (approaches != null) {

                derList.add(new DERSequence(new ASN1Encodable[] { approaches }));
            }
            else if (egresses != null) {

                derList.add(new DERSequence(new ASN1Encodable[] { egresses }));
            }
        }

        // ApproachObject ::= SEQUENCE (list)
        return new DERSequence(derList.toArray(CONVERSION_ARRAY));
    }

    /**
     * Parses the lane manager for the position of the lane manager
     * 
     * @param laneMan the lane manager to be parsed
     * @return a DERSequence of the position of the lane manager
     */
    ASN1Encodable parsePosition3DToDEREncoding(ILaneManager laneMan) {
        // There is also an optional parameter for Elevation
        DERInteger lat = new DERInteger(UtilsUnitConversion.convertToOneTenthMicrodegree(laneMan.getLatitude()));
        DERInteger lon = new DERInteger(UtilsUnitConversion.convertToOneTenthMicrodegree(laneMan.getLongitude()));

        // Position3D ::= SEQUENCE
        return new DERSequence(new ASN1Encodable[] { new DERTaggedObject(false, 0, lat), new DERTaggedObject(false, 1, lon) });
    }

    /**
     * Parses the lane manager for the intersections
     * 
     * @param laneMan the lane manager to get the data from
     * @return a DERSequence of intersections
     */
    ASN1Encodable parseIntersectionsToDEREncoding(ILaneManager laneMan) {
        // There are more optional fields: DescriptiveName, IntersectionID(secondary for computed
        // intersections?),
        // Heading, LaneWidth, IntersectionStatusObject, SignalControlZone(PreemptZones), and
        // SignalControlZone(PriorityZones)

        // TODO ttevendale - This will need to be updated for multi-intersection. Currently this is
        // only getting one intersection

        ByteBuffer intersectionIdBuffer = ByteBuffer.allocate(2);
        intersectionIdBuffer.putShort((short)laneMan.getIntersectionId());
        DEROctetString intersectionId = new DEROctetString(intersectionIdBuffer.array());

        // Intersection ::= SEQUENCE
        return new DERSequence(
                new ASN1Encodable[] { new DERSequence(new ASN1Encodable[] { new DERTaggedObject(false, 1, intersectionId), new DERTaggedObject(false, 2, parsePosition3DToDEREncoding(laneMan)),
                        new DERTaggedObject(false, 7, parseApproachLanesToDEREncoding(laneMan)) }) });
    }

    /**
     * Parses a MAP message out of a LaneManager.
     * 
     * @param laneMan The lane manager to parse.
     * @return The MAP message in DER object format.
     */
    ASN1Encodable convertToRawMapData(ILaneManager laneMan) {
        // TODO: ablatt - "debug mode" is supposed to contain 1 additional field
        // There are three more optional fields that could be included: DescriptiveName, LayerType,
        // LayerID but aren't needed
        // MapData ::= SEQUENCE
        DERTaggedObject msgCnt = new DERTaggedObject(false, 1, new DERInteger(msgCount));
        DERTaggedObject intersections = new DERTaggedObject(false, 5, parseIntersectionsToDEREncoding(laneMan));

        byte[] bytes = new byte[DSRCmsgID.getDEREncoded().length + msgCnt.getDEREncoded().length];
        System.arraycopy(DSRCmsgID.getDEREncoded(), 0, bytes, 0, DSRCmsgID.getDEREncoded().length);
        System.arraycopy(msgCnt.getDEREncoded(), 0, bytes, DSRCmsgID.getDEREncoded().length, msgCnt.getDEREncoded().length);

        crcBytes = this.generateChecksumCRC16(bytes);

        DERTaggedObject crc = new DERTaggedObject(false, 7, new DEROctetString(crcBytes));

        ASN1Encodable[] ret = new ASN1Encodable[] { DSRCmsgID, msgCnt, intersections, crc };

        return new DERSequence(ret);
    }

    /**
     * Generates a checksum for a crc
     * 
     * @param bytes the bytes to make the crc from
     * @return the calculated crc
     */
    public byte[] generateChecksumCRC16(byte[] bytes) {

        int crc = 0xFFFF;
        int temp;
        int crc_byte;

        for (int byte_index = 0; byte_index < bytes.length; byte_index++) {

            crc_byte = bytes[byte_index];

            for (int bit_index = 0; bit_index < 8; bit_index++) {

                temp = ((crc >> 15)) ^ ((crc_byte >> 7));

                crc <<= 1;
                crc &= 0xFFFF;

                if (temp > 0) {
                    crc ^= 0x1021;
                    crc &= 0xFFFF;
                }

                crc_byte <<= 1;
                crc_byte &= 0xFF;

            }
        }

        return new byte[] { (byte)(crc >> 8), (byte)crc };
    }

    /**
     * Build a raw MAP message from a lane manager.
     * 
     * @param laneMan The lane manager to build from.
     * @return The MAP message in byte array form.
     */
    public byte[] buildRawMapData(ILaneManager laneMan) {
        byte[] bytes = convertToRawMapData(laneMan).getDEREncoded();
        return bytes;
    }

    @Override
    public void init(String[] appConfigs) {
        hasFormattedData = Boolean.parseBoolean(appConfigs[0]);
        frequency = Double.parseDouble(appConfigs[1]);
    }

    @Override
    public void performUpdate(RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simtime, AppLogger logger) {

        if (lastTxTime != null) {

            if (simtime - lastTxTime < frequency) {

                return;
            }
        }

        if (mapDataRequests == null) {

            mapDataRequests = new ArrayList<BasicMessage>();

            for (ILaneManager laneManager : device.getLaneManagers().values()) {

                ReferencePoint[] referencePoints = device.getReferencePoints();
                byte[] mapDataBytes = buildRawMapData(laneManager);
                MapData mapData = createFormattedMapDataMessage(laneManager, msgCount, crcBytes, Arrays.asList(referencePoints));

                mapDataRequests.add(hasFormattedData
                        ? new FormattedDSRCMessage(mapDataBytes, mapData, DSRCChannel.CH184, WaveMessage.MACBROADCAST, mapDataBytes.length)
                        : new DSRCMessage(mapDataBytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, mapDataBytes.length));

                msgCount = (short)(++msgCount % 128);
                double coverage = MapDataUtil.calculateMapCoverage(laneManager, Arrays.asList(referencePoints), laneManager.getGeoCalculatorType());

                if (Math.abs(coverage - 100.0) > 0.005) {

                    logger.log("Coverage Warning", String.format("Only %.2f%% of the lane geometry for intersection %d is covered.", coverage, laneManager.getIntersectionId()));
                }
            }
        }

        lastTxTime = simtime;
        device.addAppMessages(mapDataRequests);
    }

    @Override
    public void appShutdown(AppLogger logger) {}

    @Override
    public String getAppName() {
        return APP_NAME_MAP_DATA_PRODUCER_APP;
    }

}