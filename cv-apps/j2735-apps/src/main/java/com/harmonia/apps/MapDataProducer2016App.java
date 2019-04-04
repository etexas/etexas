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

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.CoberturaIgnore;
import org.etexascode.apps.IRSEBaseApp;
import org.etexascode.apps.RSEDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneMovement;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735_2016.elements.AllowedManeuvers;
import org.etexascode.j2735_2016.elements.ApproachID;
import org.etexascode.j2735_2016.elements.DSRCmsgID;
import org.etexascode.j2735_2016.elements.Elevation;
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.etexascode.j2735_2016.elements.LaneDirection;
import org.etexascode.j2735_2016.elements.LaneSharing;
import org.etexascode.j2735_2016.elements.Latitude;
import org.etexascode.j2735_2016.elements.LayerType.Layer;
import org.etexascode.j2735_2016.elements.Longitude;
import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.elements.OffsetB10;
import org.etexascode.j2735_2016.elements.OffsetB11;
import org.etexascode.j2735_2016.elements.OffsetB12;
import org.etexascode.j2735_2016.elements.OffsetB13;
import org.etexascode.j2735_2016.elements.OffsetB14;
import org.etexascode.j2735_2016.elements.OffsetB16;
import org.etexascode.j2735_2016.elements.SpeedLimitType.SpeedLimit;
import org.etexascode.j2735_2016.frames.GenericLane;
import org.etexascode.j2735_2016.frames.IntersectionGeometry;
import org.etexascode.j2735_2016.frames.IntersectionGeometryList;
import org.etexascode.j2735_2016.frames.IntersectionReferenceID;
import org.etexascode.j2735_2016.frames.LaneAttributes;
import org.etexascode.j2735_2016.frames.LaneDataAttribute;
import org.etexascode.j2735_2016.frames.LaneDataAttributeList;
import org.etexascode.j2735_2016.frames.LaneList;
import org.etexascode.j2735_2016.frames.LaneTypeAttributes;
import org.etexascode.j2735_2016.frames.NodeAttributeSetXY;
import org.etexascode.j2735_2016.frames.NodeListXY;
import org.etexascode.j2735_2016.frames.NodeOffsetPointXY;
import org.etexascode.j2735_2016.frames.NodeSetXY;
import org.etexascode.j2735_2016.frames.NodeXY;
import org.etexascode.j2735_2016.frames.NodeXY20B;
import org.etexascode.j2735_2016.frames.NodeXY22B;
import org.etexascode.j2735_2016.frames.NodeXY24B;
import org.etexascode.j2735_2016.frames.NodeXY26B;
import org.etexascode.j2735_2016.frames.NodeXY28B;
import org.etexascode.j2735_2016.frames.NodeXY32B;
import org.etexascode.j2735_2016.frames.Position3D;
import org.etexascode.j2735_2016.frames.RegulatorySpeedLimit;
import org.etexascode.j2735_2016.frames.SpeedLimitList;
import org.etexascode.j2735_2016.messages.MapData;
import org.etexascode.j2735_2016.messages.MessageFrame;
import org.etexascode.wavesim.WaveMessage;

/**
 * A MapData producing Connected Vehicle App.
 * 
 * @author ttevendale
 */
public class MapDataProducer2016App implements IRSEBaseApp, IAppLifecycle, IAppName {

    /**
     * The application identifier.
     */
    public static final String APP_NAME_MAP_DATA_PRODUCER_2016_APP = "MapDataProducer2016App";

    /**
     * The last recorded time a transmission was made.
     */
    private Double lastTxTime;

    /**
     * Store the MapData messages so we don't generate them each time.
     */
    private List<BasicMessage> mapDataMessages = null;

    /**
     * Builds a map data message byte[].
     * 
     * @param laneManagers The lane managers for the execution.
     * @param simTime The current simulation time.
     * @return The MapData as a byte[].
     */
    private byte[] buildMapData(Map<Integer, ILaneManager> laneManagers, double simTime) {

        IntersectionGeometryList intersections = new IntersectionGeometryList(laneManagers.size());
        int index = 0;
        for (Entry<Integer, ILaneManager> laneManager : laneManagers.entrySet()) {

            intersections.getIntersectionGeometryArray()[index] = buildIntersection(laneManager.getKey(), laneManager.getValue());
            index++;
        }

        // setting to zero since this implementation assumes the MapData won't change.
        MapData mapData = new MapData(new MsgCount(0));
        mapData.setTimeStamp(buildTimeStamp(simTime));
        mapData.setLayerType(Layer.INTERSECTION_DATA);
        mapData.setIntersections(intersections);

        MessageFrame frame = new MessageFrame(DSRCmsgID.MAP, mapData.encodeUPER());
        byte[] mapDataBytes = null;

        try {

            mapDataBytes = frame.encodeHexUPER().getBytes("UTF-8");
        }
        catch (UnsupportedEncodingException e) {

            throw new IllegalStateException("Unsupported Encoding Exception was thrown while creating MapData.");
        }

        return mapDataBytes;
    }

    /**
     * Builds the time stamp for a MapData message. NOTE: converts seconds to minutes.
     * 
     * @param simTime The simulation time to convert to a time stamp.
     * @return The time stamp.
     */
    private MinuteOfTheYear buildTimeStamp(double simTime) {

        // converts seconds to minutes
        return new MinuteOfTheYear((int)simTime / 60);
    }

    /**
     * Builds an intersection for the MapData message.
     * 
     * @param intersectionId The intersection ID.
     * @param laneManager The lane manager for the current intersection.
     * @return The intersection.
     */
    private IntersectionGeometry buildIntersection(Integer intersectionId, ILaneManager laneManager) {

        Map<Integer, ILane> laneMap = laneManager.getLanes();

        LaneList lanes = new LaneList(laneMap.size());
        int index = 0;
        for (ILane lane : laneMap.values()) {

            lanes.getLaneArray()[index] = buildLane(lane);
            index++;
        }

        // setting MsgCount to zero since this implementation assumes the MapData won't change.
        return new IntersectionGeometry(new IntersectionReferenceID(intersectionId), new MsgCount(0), buildRefPoint(laneManager), lanes);
    }

    /**
     * Builds the reference point of the intersection.
     * 
     * @param laneManager The lane manager for the current intersection.
     * @return The reference point of the intersection.
     */
    private Position3D buildRefPoint(ILaneManager laneManager) {

        Latitude latitude = new Latitude(UtilsUnitConversion.convertToOneTenthMicrodegree(laneManager.getLatitude()));
        Longitude longitude = new Longitude(UtilsUnitConversion.convertToOneTenthMicrodegree(laneManager.getLongitude()));

        // converts cm to 10cm
        Elevation elevation = new Elevation((int)(laneManager.getElevation() / 10));

        Position3D position3d = new Position3D(latitude, longitude);
        position3d.setElevation(elevation);

        return position3d;
    }

    /**
     * Builds a generic lane for the MapData message.
     * 
     * @param lane The lane to build the generic lane with.
     * @return The generic lane.
     */
    private GenericLane buildLane(ILane lane) {

        GenericLane genericLane = new GenericLane(lane.getLaneId(), buildLaneAttributes(lane.getType()), buildNodeList(lane));

        int approachId = getValidApproachId(lane.getApproachId());
        if (lane.getType().equals(Lane.INBOUND)) {

            genericLane.setIngressApproach(approachId);
        }
        else {

            genericLane.setEgressApproach(approachId);
        }

        genericLane.setManeuvers(buildAllowedManeuvers(lane.getLaneMovements().values()));
        return genericLane;
    }

    /**
     * Gets the approach ID for a generic lane.
     * 
     * @param lane The lane to get the approach ID from.
     * @return The approach ID.
     */
    private int getValidApproachId(int approachId) {

        if (approachId > ApproachID.MAX || approachId < ApproachID.MIN) {

            return ApproachID.UNKNOWN;
        }

        return approachId;
    }

    /**
     * Builds the lane attributes for a generic lane.
     * 
     * @param lane The lane used to build the lane attributes.
     * @return The lane attributes.
     */
    private LaneAttributes buildLaneAttributes(String laneType) {

        return new LaneAttributes(buildLaneDirection(laneType), buildLaneSharing(), buildLaneTypeAttributes());
    }

    /**
     * Builds the lane direction for a generic lane.
     * 
     * @param laneType The lane type used to determine the lane direction.
     * @return The lane direction.
     */
    private LaneDirection buildLaneDirection(String laneType) {

        LaneDirection laneDirection = new LaneDirection();

        if (laneType.equals(Lane.INBOUND)) {

            laneDirection.setIngressPath(true);
        }
        else {

            laneDirection.setEgressPath(true);
        }

        return laneDirection;
    }

    /**
     * Builds the lane sharing for the generic lane.
     * 
     * @return The lane sharing.
     */
    private LaneSharing buildLaneSharing() {

        LaneSharing laneSharing = new LaneSharing();
        laneSharing.setIndividualMotorizedVehicleTraffic(true);
        laneSharing.setBusVehicleTraffic(true);
        laneSharing.setTaxiVehicleTraffic(true);

        return laneSharing;
    }

    /**
     * Builds the lane type attributes for the generic lane.
     * 
     * @return The lane type attributes.
     */
    private LaneTypeAttributes buildLaneTypeAttributes() {

        return new LaneTypeAttributes(new LaneAttributesVehicle());
    }

    /**
     * Builds the allowed maneuvers for a generic lane.
     * 
     * @param laneMovements The lane movements to build the allowed maneuvers from.
     * @return The allowed maneuvers.
     */
    private AllowedManeuvers buildAllowedManeuvers(Collection<? extends ILaneMovement> laneMovements) {

        AllowedManeuvers maneuvers = new AllowedManeuvers();

        for (ILaneMovement movement : laneMovements) {

            switch (movement.getMovement()) {

                case LEFT_TURN:
                    maneuvers.setManeuverLeftAllowed(true);
                    break;
                case LEFT_TURN_ON_RED:
                    maneuvers.setManeuverLeftTurnOnRedAllowed(true);
                    break;
                case RIGHT_TURN:
                    maneuvers.setManeuverRightAllowed(true);
                    break;
                case RIGHT_TURN_ON_RED:
                    maneuvers.setManeuverRightTurnOnRedAllowed(true);
                    break;
                case STRAIGHT:
                    maneuvers.setManeuverStraightAllowed(true);
                    break;
                case U_TURN:
                    maneuvers.setManeuverUTurnAllowed(true);
                    break;
                default:
                    // unknown movement
                    break;
            }
        }
        return maneuvers;
    }

    /**
     * Builds the node list for the generic lane.
     * 
     * @param lane The lane used to build the node list.
     * @return The node list.
     */
    private NodeListXY buildNodeList(ILane lane) {

        List<? extends ILaneNode> laneNodes = lane.getLaneGeomList();
        NodeSetXY nodeSet = new NodeSetXY(laneNodes.size());

        if (lane.getType().equals(Lane.INBOUND)) {

            // reversed the order for inbound lane nodes because in the spec all lanes are described
            // from the stop line outwards while our lane nodes are created based on the direction
            // of travel.
            int index = 0;
            ILaneNode previousNode = null;
            ListIterator<? extends ILaneNode> iterator = laneNodes.listIterator(laneNodes.size());
            while (iterator.hasPrevious()) {

                ILaneNode node = iterator.previous();
                nodeSet.getNodeArray()[index] = buildNode(previousNode, node, lane.getSpeedLimitInMetersPerSecond());
                previousNode = node;
                index++;
            }
        }
        else {

            int index = 0;
            ILaneNode previousNode = null;
            for (ILaneNode node : lane.getLaneGeomList()) {

                nodeSet.getNodeArray()[index] = buildNode(previousNode, node, lane.getSpeedLimitInMetersPerSecond());
                previousNode = node;
                index++;
            }
        }

        return new NodeListXY(nodeSet);
    }

    /**
     * Builds a node xy for the generic lane.
     * 
     * @param previousNode The previous node that was used to create the last node xy.
     * @param node The node used to build the node xy.
     * @param speedLimit The speed limit for the lane.
     * @return The node xy.
     */
    private NodeXY buildNode(ILaneNode previousNode, ILaneNode node, double speedLimit) {

        NodeXY nodeXY = new NodeXY(buildNodeOffset((int)node.getX(), (int)node.getY()));
        NodeAttributeSetXY nodeAttributes = buildNodeAttributes(previousNode, node, speedLimit);

        if (nodeAttributes != null) {

            nodeXY.setAttributes(nodeAttributes);
        }
        return nodeXY;
    }

    /**
     * Builds a node offset point xy based on how much space is needed to express the node. NOTE: if
     * either the x or y value is out of range of the OffsetB16 then the largest value is assumed
     * for that x or y, which will cause a loss of some data. for example if the input is (40000,
     * 2000) then this will be converted to (32767, 2000).
     * 
     * @param x The x value of the node offset.
     * @param y The y value of the node offset.
     * @return The node offset point xy.
     */
    private NodeOffsetPointXY buildNodeOffset(int x, int y) {

        NodeOffsetPointXY nodeOffsetPointXY = null;

        if (x >= OffsetB10.MIN && x <= OffsetB10.MAX && y >= OffsetB10.MIN && y <= OffsetB10.MAX) {

            nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY20B(x, y));
        }
        else if (x >= OffsetB11.MIN && x <= OffsetB11.MAX && y >= OffsetB11.MIN && y <= OffsetB11.MAX) {

            nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY22B(x, y));
        }
        else if (x >= OffsetB12.MIN && x <= OffsetB12.MAX && y >= OffsetB12.MIN && y <= OffsetB12.MAX) {

            nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY24B(x, y));
        }
        else if (x >= OffsetB13.MIN && x <= OffsetB13.MAX && y >= OffsetB13.MIN && y <= OffsetB13.MAX) {

            nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY26B(x, y));
        }
        else if (x >= OffsetB14.MIN && x <= OffsetB14.MAX && y >= OffsetB14.MIN && y <= OffsetB14.MAX) {

            nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY28B(x, y));
        }
        else if (x >= OffsetB16.MIN && x <= OffsetB16.MAX && y >= OffsetB16.MIN && y <= OffsetB16.MAX) {

            nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY32B(x, y));
        }
        else {

            // One or both are outside the range of the spec, so won't be fully covered
            x = x > OffsetB16.MAX ? OffsetB16.MAX : x;
            x = x < OffsetB16.MIN ? OffsetB16.MIN : x;

            y = y > OffsetB16.MAX ? OffsetB16.MAX : y;
            y = y < OffsetB16.MIN ? OffsetB16.MIN : y;

            nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY32B(x, y));
        }
        return nodeOffsetPointXY;
    }

    /**
     * Builds a node attribute set xy if there are any to build, otherwise returns null.
     * 
     * @param previousNode The previous node that was used to create the last node xy.
     * @param node The node used to build the node xy.
     * @param speedLimit The speed limit for the lane.
     * @return The node attribute set xy or <code>null</code> if none were built.
     */
    private NodeAttributeSetXY buildNodeAttributes(ILaneNode previousNode, ILaneNode node, double speedLimit) {

        NodeAttributeSetXY nodeAttributes = null;

        if (previousNode == null) {

            // if there is no previous node then set the width and speedlimit of the lane. from spec
            // "A value added to the current lane width at this node and from this node onwards, in
            // 1cm steps" and since this implementation didn't set a default lane width the first
            // node to be created will need the width value set. also from spec "Reference
            // regulatory speed limits used by all segments" so only needs to set once
            LaneDataAttributeList laneDataAttributeList = new LaneDataAttributeList(1);
            SpeedLimitList speeds = new SpeedLimitList(1);

            int convertedSpeedLimit = (int)UtilsUnitConversion.convertMetersPerSecondToOneFiftiethMetersPerSecond(speedLimit);
            speeds.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.VEHICLE_MAX_SPEED, convertedSpeedLimit);

            laneDataAttributeList.getAttributeArray()[0] = new LaneDataAttribute(speeds);

            nodeAttributes = new NodeAttributeSetXY();
            nodeAttributes.setDWidth((int)node.getWidth());
            nodeAttributes.setData(laneDataAttributeList);
        }
        else {

            int diffWidth = (int)node.getWidth() - (int)previousNode.getWidth();
            if (diffWidth != 0) {

                nodeAttributes = new NodeAttributeSetXY();
                nodeAttributes.setDWidth(diffWidth);
            }
        }

        return nodeAttributes;
    }

    /**
     * The frequency at which to produce messages in seconds. No recommended default in SPEC.
     */
    @AppConfigProperty(value = "1.0", description = "The frequency to send messages in seconds.")
    private double frequency;

    @Override
    public void init(String[] appConfigs) {

        frequency = Double.parseDouble(appConfigs[0]);
    }

    @Override
    public void performUpdate(RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

        if (lastTxTime != null && simTime - lastTxTime < frequency) {

            return;
        }

        if (mapDataMessages == null) {

            mapDataMessages = new ArrayList<BasicMessage>();
            byte[] mapDataBytes = buildMapData(device.getLaneManagers(), simTime);

            mapDataMessages.add(new DSRCMessage(mapDataBytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, mapDataBytes.length));
        }

        lastTxTime = simTime;

        device.addAppMessages(mapDataMessages);
    }

    @Override
    @CoberturaIgnore
    public void appShutdown(AppLogger logger) {

        logger.log(APP_NAME_MAP_DATA_PRODUCER_2016_APP, "The application has shutdown.");
    }

    @Override
    public String getAppName() {

        return APP_NAME_MAP_DATA_PRODUCER_2016_APP;
    }

}