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
package org.etexascode.j2735_2016.frames;

import java.util.Objects;

import org.etexascode.j2735_2016.elements.LaneWidth;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The road segment frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RoadSegment implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the road segment.
     */
    public static final int NUM_BITS = 5;

    /**
     * The road segment reference ID frame.
     */
    private RoadSegmentReferenceID id;

    /**
     * The message count element.
     */
    private MsgCount revision;

    /**
     * The position 3d frame.
     */
    private Position3D refPoint;

    /**
     * The lane width element. (OPTIONAL)
     */
    private LaneWidth laneWidth;

    /**
     * The speed limit list frame. (OPTIONAL)
     */
    private SpeedLimitList speedLimits;

    /**
     * The lane list frame.
     */
    private RoadLaneSetList roadLaneSet;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public RoadSegment() {

        id = new RoadSegmentReferenceID();
        revision = new MsgCount();
        refPoint = new Position3D();
        roadLaneSet = new RoadLaneSetList();
    }

    /**
     * A constructor for the road segment frame for all required fields.
     * 
     * @param id The road segment reference ID frame.
     * @param revision The message count element.
     * @param refPoint The position 3d frame.
     * @param laneSet The road lane set list frame.
     */
    public RoadSegment(RoadSegmentReferenceID id, MsgCount revision, Position3D refPoint, RoadLaneSetList roadLaneSet) {

        this.id = Objects.requireNonNull(id);
        this.revision = Objects.requireNonNull(revision);
        this.refPoint = Objects.requireNonNull(refPoint);
        this.roadLaneSet = Objects.requireNonNull(roadLaneSet);
    }

    /**
     * A constructor for the intersection geometry frame for all required fields. (primitive)
     * 
     * @param id The road segment reference ID frame.
     * @param revision The message count value.
     * @param refPoint The position 3d frame.
     * @param laneSet The road lane set list frame.
     */
    public RoadSegment(RoadSegmentReferenceID id, int revision, Position3D refPoint, RoadLaneSetList roadLaneSet) {

        this.id = Objects.requireNonNull(id);
        this.revision = new MsgCount(revision);
        this.refPoint = Objects.requireNonNull(refPoint);
        this.roadLaneSet = Objects.requireNonNull(roadLaneSet);
    }

    /**
     * A getter for the road segment reference ID frame.
     * 
     * @return The road segment reference ID frame.
     */
    public RoadSegmentReferenceID getId() {

        return id;
    }

    /**
     * A setter for the road segment reference ID frame.
     * 
     * @param id The road segment reference ID frame.
     */
    public void setId(RoadSegmentReferenceID id) {

        this.id = Objects.requireNonNull(id);
    }

    /**
     * A getter for the message count element.
     * 
     * @return The message count element.
     */
    public MsgCount getRevision() {

        return revision;
    }

    /**
     * A setter for the message count element.
     * 
     * @param revision The message count element to set.
     */
    public void setRevision(MsgCount revision) {

        this.revision = Objects.requireNonNull(revision);
    }

    /**
     * A setter for the message count element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param revision The message count value to be set in the element.
     */
    public void setRevision(int revision) {

        this.revision.setValue(revision);
    }

    /**
     * A getter for the position 3d frame.
     * 
     * @return The position 3d frame.
     */
    public Position3D getRefPoint() {

        return refPoint;
    }

    /**
     * A setter for the position 3d frame.
     * 
     * @param refPoint The position 3d frame.
     */
    public void setRefPoint(Position3D refPoint) {

        this.refPoint = Objects.requireNonNull(refPoint);
    }

    /**
     * A getter for the lane width element.
     * 
     * @return The lane width element.
     */
    public LaneWidth getLaneWidth() {

        return laneWidth;
    }

    /**
     * A setter for the lane width element.
     * 
     * @param laneWidth The lane width element to set.
     */
    public void setLaneWidth(LaneWidth laneWidth) {

        this.laneWidth = laneWidth;
    }

    /**
     * A setter for the lane width element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param laneWidth The lane width value to be set in the element.
     */
    public void setLaneWidth(int laneWidth) {

        if (this.laneWidth == null) {

            this.laneWidth = new LaneWidth();
        }
        this.laneWidth.setValue(laneWidth);
    }

    /**
     * A getter for the speed limit list frame.
     * 
     * @return The speed limit list frame.
     */
    public SpeedLimitList getSpeedLimits() {

        return speedLimits;
    }

    /**
     * A setter for the speed limit list frame.
     * 
     * @param speedLimits The speed limit list frame.
     */
    public void setSpeedLimits(SpeedLimitList speedLimits) {

        this.speedLimits = speedLimits;
    }

    /**
     * A getter for the road lane set list frame.
     * 
     * @return The road lane set list frame.
     */
    public RoadLaneSetList getRoadLaneSet() {

        return roadLaneSet;
    }

    /**
     * A setter for the road lane set list frame.
     * 
     * @param roadLaneSet The road lane set list frame.
     */
    public void setRoadLaneSet(RoadLaneSetList roadLaneSet) {

        this.roadLaneSet = Objects.requireNonNull(roadLaneSet);
    }

    @Override
    public String encodeUPER() {

        StringBuilder roadSegmentBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension or the descriptive name usable so setting both
        // to off.
        optionalBits.append("00");
        roadSegmentBits.append(id.encodeUPER());
        roadSegmentBits.append(revision.encodeUPER());
        roadSegmentBits.append(refPoint.encodeUPER());

        if (laneWidth != null) {

            optionalBits.append('1');
            roadSegmentBits.append(laneWidth.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (speedLimits != null) {

            optionalBits.append('1');
            roadSegmentBits.append(speedLimits.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        roadSegmentBits.append(roadLaneSet.encodeUPER());

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');

        roadSegmentBits.insert(0, optionalBits);

        return roadSegmentBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (RoadSegment.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RoadSegment frame (%d)", RoadSegment.NUM_BITS));
        }

        String roadSegmentOptionalBits = bits.substring(0, RoadSegment.NUM_BITS);
        bits = bits.substring(RoadSegment.NUM_BITS);

        if (roadSegmentOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The RoadSegment extension is not supported");
        }

        if (roadSegmentOptionalBits.charAt(1) != '0') {

            throw new IllegalArgumentException("The RoadSegment descriptive name is not supported");
        }

        if (roadSegmentOptionalBits.charAt(4) != '0') {

            throw new IllegalArgumentException("The RoadSegment regional extension is not supported");
        }

        bits = id.decodeUPER(bits);
        bits = revision.decodeUPER(bits);
        bits = refPoint.decodeUPER(bits);

        if (roadSegmentOptionalBits.charAt(2) == '1') {

            laneWidth = new LaneWidth();
            bits = laneWidth.decodeUPER(bits);
        }

        if (roadSegmentOptionalBits.charAt(3) == '1') {

            speedLimits = new SpeedLimitList();
            bits = speedLimits.decodeUPER(bits);
        }

        bits = roadLaneSet.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(id, revision, refPoint, laneWidth, speedLimits, roadLaneSet);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof RoadSegment)) {

            return false;
        }
        RoadSegment frame = (RoadSegment)object;
        return this.id.equals(frame.id)
                && this.revision.equals(frame.revision)
                && this.refPoint.equals(frame.refPoint)
                && Objects.equals(this.laneWidth, frame.laneWidth)
                && Objects.equals(this.speedLimits, frame.speedLimits)
                && this.roadLaneSet.equals(frame.roadLaneSet);
    }
}
