/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.j2735_2016.messages;

import java.util.Objects;

import org.etexascode.j2735_2016.elements.LayerID;
import org.etexascode.j2735_2016.elements.LayerType;
import org.etexascode.j2735_2016.elements.LayerType.Layer;
import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.frames.IntersectionGeometryList;
import org.etexascode.j2735_2016.frames.RestrictionClassList;
import org.etexascode.j2735_2016.frames.RoadSegmentList;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The map data message for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class MapData implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the map data.
     */
    public static final int NUM_BITS = 9;

    /**
     * The minute of the year element. (OPTIONAL)
     */
    private MinuteOfTheYear timeStamp;

    /**
     * The message count element.
     */
    private MsgCount msgIssueRevision;

    /**
     * The layer type element. (OPTIONAL)
     */
    private LayerType layerType;

    /**
     * The layer ID element. (OPTIONAL)
     */
    private LayerID layerId;

    /**
     * The intersection geometry list frame. (OPTIONAL)
     */
    private IntersectionGeometryList intersections;

    /**
     * The road segment list frame. (OPTIONAL)
     */
    private RoadSegmentList roadSegments;

    /**
     * The restriction class list frame. (OPTIONAL)
     */
    private RestrictionClassList restrictionList;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public MapData() {

        msgIssueRevision = new MsgCount();
    }

    /**
     * A constructor for the map data message for all required fields.
     * 
     * @param msgIssueRevision The message count element.
     */
    public MapData(MsgCount msgIssueRevision) {

        this.msgIssueRevision = Objects.requireNonNull(msgIssueRevision);
    }

    /**
     * A constructor for the map data message for all required fields. (primitive)
     * 
     * @param msgIssueRevision The message count value.
     */
    public MapData(int msgIssueRevision) {

        this.msgIssueRevision = new MsgCount(msgIssueRevision);
    }

    /**
     * A getter for the minute of the year element.
     * 
     * @return The minute of the year element.
     */
    public MinuteOfTheYear getTimeStamp() {

        return timeStamp;
    }

    /**
     * A setter for the minute of the year element.
     * 
     * @param timeStamp The minute of the year element to set.
     */
    public void setTimeStamp(MinuteOfTheYear timeStamp) {

        this.timeStamp = timeStamp;
    }

    /**
     * A setter for the minute of the year element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param timeStamp The minute of the year value to be set in the element.
     */
    public void setTimeStamp(int timeStamp) {

        if (this.timeStamp == null) {

            this.timeStamp = new MinuteOfTheYear();
        }
        this.timeStamp.setValue(timeStamp);
    }

    /**
     * A getter for the message count element.
     * 
     * @return The message count element.
     */
    public MsgCount getMsgIssueRevision() {

        return msgIssueRevision;
    }

    /**
     * A setter for the message count element.
     * 
     * @param msgIssueRevision The message count element to set.
     */
    public void setMsgIssueRevision(MsgCount msgIssueRevision) {

        this.msgIssueRevision = Objects.requireNonNull(msgIssueRevision);
    }

    /**
     * A setter for the message count element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param msgIssueRevision The message count value to be set in the element.
     */
    public void setMsgIssueRevision(int msgIssueRevision) {

        this.msgIssueRevision.setValue(msgIssueRevision);
    }

    /**
     * A getter for the layer type element.
     * 
     * @return The layer type element.
     */
    public LayerType getLayerType() {

        return layerType;
    }

    /**
     * A setter for the layer type element.
     * 
     * @param layerType The layer type element to set.
     */
    public void setLayerType(LayerType layerType) {

        this.layerType = layerType;
    }

    /**
     * A setter for the layer type element. Allows enumeration data to be passed to the enumeration
     * element.
     * 
     * @param layerType The layer type value to be set in the element.
     */
    public void setLayerType(Layer layerType) {

        if (this.layerType == null) {

            this.layerType = new LayerType();
        }
        this.layerType.setEnumeration(layerType);
    }

    /**
     * A getter for the layer ID element.
     * 
     * @return The layer ID element.
     */
    public LayerID getLayerId() {

        return layerId;
    }

    /**
     * A setter for the layer ID element.
     * 
     * @param layerId The layer ID element to set.
     */
    public void setLayerId(LayerID layerId) {

        this.layerId = layerId;
    }

    /**
     * A setter for the layer ID element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param layerId The layer ID value to be set in the element.
     */
    public void setLayerId(int layerId) {

        if (this.layerId == null) {

            this.layerId = new LayerID();
        }
        this.layerId.setValue(layerId);
    }

    /**
     * A getter for the intersection geometry list frame.
     * 
     * @return The intersection geometry list frame.
     */
    public IntersectionGeometryList getIntersections() {

        return intersections;
    }

    /**
     * A setter for the intersection geometry list frame.
     * 
     * @param intersections The intersection geometry list frame to set.
     */
    public void setIntersections(IntersectionGeometryList intersections) {

        this.intersections = intersections;
    }

    /**
     * A getter for the road segment list frame.
     * 
     * @return The road segment list frame.
     */
    public RoadSegmentList getRoadSegments() {

        return roadSegments;
    }

    /**
     * A setter for the road segment list frame.
     * 
     * @param roadSegments The road segment list frame to set.
     */
    public void setRoadSegments(RoadSegmentList roadSegments) {

        this.roadSegments = roadSegments;
    }

    /**
     * A getter for the restriction class list frame.
     * 
     * @return The restriction class list frame.
     */
    public RestrictionClassList getRestrictionList() {

        return restrictionList;
    }

    /**
     * A setter for the restriction class list frame.
     * 
     * @param restrictionList The restriction class list frame to set.
     */
    public void setRestrictionList(RestrictionClassList restrictionList) {

        this.restrictionList = restrictionList;
    }

    @Override
    public String encodeUPER() {

        StringBuilder mapDataBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension usable so setting to off.
        optionalBits.append('0');

        if (timeStamp != null) {

            optionalBits.append('1');
            mapDataBits.append(timeStamp.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        mapDataBits.append(msgIssueRevision.encodeUPER());

        if (layerType != null) {

            optionalBits.append('1');
            mapDataBits.append(layerType.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (layerId != null) {

            optionalBits.append('1');
            mapDataBits.append(layerId.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (intersections != null) {

            optionalBits.append('1');
            mapDataBits.append(intersections.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (roadSegments != null) {

            optionalBits.append('1');
            mapDataBits.append(roadSegments.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the data parameters usable so setting to off.
        optionalBits.append('0');

        if (restrictionList != null) {

            optionalBits.append('1');
            mapDataBits.append(restrictionList.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');

        mapDataBits.insert(0, optionalBits);

        return mapDataBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (MapData.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a MapData frame (%d)", MapData.NUM_BITS));
        }

        String mapDataOptionalBits = bits.substring(0, MapData.NUM_BITS);
        bits = bits.substring(MapData.NUM_BITS);

        if (mapDataOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The MapData extension is not supported");
        }

        if (mapDataOptionalBits.charAt(6) != '0') {

            throw new IllegalArgumentException("The MapData data parameters is not supported");
        }

        if (mapDataOptionalBits.charAt(8) != '0') {

            throw new IllegalArgumentException("The MapData regional extension is not supported");
        }

        if (mapDataOptionalBits.charAt(1) == '1') {

            timeStamp = new MinuteOfTheYear();
            bits = timeStamp.decodeUPER(bits);
        }

        bits = msgIssueRevision.decodeUPER(bits);

        if (mapDataOptionalBits.charAt(2) == '1') {

            layerType = new LayerType();
            bits = layerType.decodeUPER(bits);
        }

        if (mapDataOptionalBits.charAt(3) == '1') {

            layerId = new LayerID();
            bits = layerId.decodeUPER(bits);
        }

        if (mapDataOptionalBits.charAt(4) == '1') {

            intersections = new IntersectionGeometryList();
            bits = intersections.decodeUPER(bits);
        }

        if (mapDataOptionalBits.charAt(5) == '1') {

            roadSegments = new RoadSegmentList();
            bits = roadSegments.decodeUPER(bits);
        }

        if (mapDataOptionalBits.charAt(7) == '1') {

            restrictionList = new RestrictionClassList();
            bits = restrictionList.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(timeStamp, msgIssueRevision, layerType, layerId, intersections, roadSegments, restrictionList);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof MapData)) {

            return false;
        }
        MapData message = (MapData)object;
        return Objects.equals(this.timeStamp, message.timeStamp)
                && this.msgIssueRevision.equals(message.msgIssueRevision)
                && Objects.equals(this.layerType, message.layerType)
                && Objects.equals(this.layerId, message.layerId)
                && Objects.equals(this.intersections, message.intersections)
                && Objects.equals(this.roadSegments, message.roadSegments)
                && Objects.equals(this.restrictionList, message.restrictionList);
    }
}
