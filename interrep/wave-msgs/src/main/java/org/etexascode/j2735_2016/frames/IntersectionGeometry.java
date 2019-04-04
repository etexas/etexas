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
 * The intersection geometry frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class IntersectionGeometry implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the intersection geometry.
     */
    public static final int NUM_BITS = 6;

    /**
     * The intersection reference ID frame.
     */
    private IntersectionReferenceID id;

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
    private LaneList laneSet;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public IntersectionGeometry() {

        id = new IntersectionReferenceID();
        revision = new MsgCount();
        refPoint = new Position3D();
        laneSet = new LaneList();
    }

    /**
     * A constructor for the intersection geometry frame for all required fields.
     * 
     * @param id The intersection reference ID frame.
     * @param revision The message count element.
     * @param refPoint The position 3d frame.
     * @param laneSet The lane list frame.
     */
    public IntersectionGeometry(IntersectionReferenceID id, MsgCount revision, Position3D refPoint, LaneList laneSet) {

        this.id = Objects.requireNonNull(id);
        this.revision = Objects.requireNonNull(revision);
        this.refPoint = Objects.requireNonNull(refPoint);
        this.laneSet = Objects.requireNonNull(laneSet);
    }

    /**
     * A constructor for the intersection geometry frame for all required fields. (primitive)
     * 
     * @param id The intersection reference ID frame.
     * @param revision The message count value.
     * @param refPoint The position 3d frame.
     * @param laneSet The lane list frame.
     */
    public IntersectionGeometry(IntersectionReferenceID id, int revision, Position3D refPoint, LaneList laneSet) {

        this.id = Objects.requireNonNull(id);
        this.revision = new MsgCount(revision);
        this.refPoint = Objects.requireNonNull(refPoint);
        this.laneSet = Objects.requireNonNull(laneSet);
    }

    /**
     * A getter for the intersection reference ID frame.
     * 
     * @return The intersection reference ID frame.
     */
    public IntersectionReferenceID getId() {

        return id;
    }

    /**
     * A setter for the intersection reference ID frame.
     * 
     * @param id The intersection reference ID frame.
     */
    public void setId(IntersectionReferenceID id) {

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
     * A getter for the lane list frame.
     * 
     * @return The lane list frame.
     */
    public LaneList getLaneSet() {

        return laneSet;
    }

    /**
     * A setter for the lane list frame.
     * 
     * @param laneSet The lane list frame.
     */
    public void setLaneSet(LaneList laneSet) {

        this.laneSet = Objects.requireNonNull(laneSet);
    }

    @Override
    public String encodeUPER() {

        StringBuilder intersectionGeometryBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension or the descriptive name usable so setting both
        // to off.
        optionalBits.append("00");
        intersectionGeometryBits.append(id.encodeUPER());
        intersectionGeometryBits.append(revision.encodeUPER());
        intersectionGeometryBits.append(refPoint.encodeUPER());

        if (laneWidth != null) {

            optionalBits.append('1');
            intersectionGeometryBits.append(laneWidth.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (speedLimits != null) {

            optionalBits.append('1');
            intersectionGeometryBits.append(speedLimits.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        intersectionGeometryBits.append(laneSet.encodeUPER());

        // we aren't planning on having the regional extension or the preempt priority list usable
        // so setting both to off.
        optionalBits.append("00");

        intersectionGeometryBits.insert(0, optionalBits);

        return intersectionGeometryBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (IntersectionGeometry.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an IntersectionGeometry frame (%d)", IntersectionGeometry.NUM_BITS));
        }

        String intersectionGeometryOptionalBits = bits.substring(0, IntersectionGeometry.NUM_BITS);
        bits = bits.substring(IntersectionGeometry.NUM_BITS);

        if (intersectionGeometryOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The IntersectionGeometry extension is not supported");
        }

        if (intersectionGeometryOptionalBits.charAt(1) != '0') {

            throw new IllegalArgumentException("The IntersectionGeometry descriptive name is not supported");
        }

        if (intersectionGeometryOptionalBits.charAt(4) != '0') {

            throw new IllegalArgumentException("The IntersectionGeometry PreemptPriorityList is not supported");
        }

        if (intersectionGeometryOptionalBits.charAt(5) != '0') {

            throw new IllegalArgumentException("The IntersectionGeometry regional extension is not supported");
        }

        bits = id.decodeUPER(bits);
        bits = revision.decodeUPER(bits);
        bits = refPoint.decodeUPER(bits);

        if (intersectionGeometryOptionalBits.charAt(2) == '1') {

            laneWidth = new LaneWidth();
            bits = laneWidth.decodeUPER(bits);
        }

        if (intersectionGeometryOptionalBits.charAt(3) == '1') {

            speedLimits = new SpeedLimitList();
            bits = speedLimits.decodeUPER(bits);
        }

        bits = laneSet.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(id, revision, refPoint, laneWidth, speedLimits, laneSet);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof IntersectionGeometry)) {

            return false;
        }
        IntersectionGeometry frame = (IntersectionGeometry)object;
        return this.id.equals(frame.id)
                && this.revision.equals(frame.revision)
                && this.refPoint.equals(frame.refPoint)
                && Objects.equals(this.laneWidth, frame.laneWidth)
                && Objects.equals(this.speedLimits, frame.speedLimits)
                && this.laneSet.equals(frame.laneSet);
    }
}
