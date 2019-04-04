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

import org.etexascode.j2735_2016.elements.TimeIntervalConfidence;
import org.etexascode.j2735_2016.elements.TimeMark;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The time change details frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class TimeChangeDetails implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the time change details.
     */
    public static final int NUM_BITS = 5;

    /**
     * The start time (time mark element). (OPTIONAL)
     */
    private TimeMark startTime;

    /**
     * The minimum end time (time mark element).
     */
    private TimeMark minEndTime;

    /**
     * The maximum end time (time mark element). (OPTIONAL)
     */
    private TimeMark maxEndTime;

    /**
     * The likely time (time mark element). (OPTIONAL)
     */
    private TimeMark likelyTime;

    /**
     * The time interval confidence element. (OPTIONAL)
     */
    private TimeIntervalConfidence confidence;

    /**
     * The next time (time mark element). (OPTIONAL)
     */
    private TimeMark nextTime;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public TimeChangeDetails() {

        minEndTime = new TimeMark();
    }

    /**
     * A constructor for the time change details frame for all required fields.
     * 
     * @param minEndTime The minimum end time (time mark element).
     */
    public TimeChangeDetails(TimeMark minEndTime) {

        this.minEndTime = Objects.requireNonNull(minEndTime);
    }

    /**
     * A constructor for the time change details frame for all required fields (primitive).
     * 
     * @param minEndTime The minimum end time value.
     */
    public TimeChangeDetails(int minEndTime) {

        this.minEndTime = new TimeMark(minEndTime);
    }

    /**
     * A getter for the start time (time mark element).
     * 
     * @return The start time (time mark element).
     */
    public TimeMark getStartTime() {

        return startTime;
    }

    /**
     * A setter for the start time (time mark element).
     * 
     * @param startTime The start time (time mark element) to set.
     */
    public void setStartTime(TimeMark startTime) {

        this.startTime = startTime;
    }

    /**
     * A setter for the start time (time mark element). Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param startTime The start time value to be set in the element.
     */
    public void setStartTime(int startTime) {

        if (this.startTime == null) {

            this.startTime = new TimeMark();
        }
        this.startTime.setValue(startTime);
    }

    /**
     * A getter for the minimum end time (time mark element).
     * 
     * @return The minimum end time (time mark element).
     */
    public TimeMark getMinEndTime() {

        return minEndTime;
    }

    /**
     * A setter for the minimum end time (time mark element).
     * 
     * @param minEndTime The minimum end time (time mark element) to set.
     */
    public void setMinEndTime(TimeMark minEndTime) {

        this.minEndTime = Objects.requireNonNull(minEndTime);
    }

    /**
     * A setter for the minimum end time (time mark element). Allows primitive data to be passed to
     * the primitive element.
     * 
     * @param minEndTime The minimum end time value to be set in the element.
     */
    public void setMinEndTime(int minEndTime) {

        this.minEndTime.setValue(minEndTime);
    }

    /**
     * A getter for the maximum end time (time mark element).
     * 
     * @return The maximum end time (time mark element).
     */
    public TimeMark getMaxEndTime() {

        return maxEndTime;
    }

    /**
     * A setter for the maximum end time (time mark element).
     * 
     * @param maxEndTime The maximum end time (time mark element) to set.
     */
    public void setMaxEndTime(TimeMark maxEndTime) {

        this.maxEndTime = maxEndTime;
    }

    /**
     * A setter for the maximum end time (time mark element). Allows primitive data to be passed to
     * the primitive element.
     * 
     * @param maxEndTime The maximum end time value to be set in the element.
     */
    public void setMaxEndTime(int maxEndTime) {

        if (this.maxEndTime == null) {

            this.maxEndTime = new TimeMark();
        }
        this.maxEndTime.setValue(maxEndTime);
    }

    /**
     * A getter for the likely time (time mark element).
     * 
     * @return The likely time (time mark element).
     */
    public TimeMark getLikelyTime() {

        return likelyTime;
    }

    /**
     * A setter for the likely time (time mark element).
     * 
     * @param likelyTime The likely time (time mark element) to set.
     */
    public void setLikelyTime(TimeMark likelyTime) {

        this.likelyTime = likelyTime;
    }

    /**
     * A setter for the likely time (time mark element). Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param likelyTime The likely time value to be set in the element.
     */
    public void setLikelyTime(int likelyTime) {

        if (this.likelyTime == null) {

            this.likelyTime = new TimeMark();
        }
        this.likelyTime.setValue(likelyTime);
    }

    /**
     * A getter for the time interval confidence element.
     * 
     * @return The time interval confidence element.
     */
    public TimeIntervalConfidence getConfidence() {

        return confidence;
    }

    /**
     * A setter for the time interval confidence element.
     * 
     * @param confidence The time interval confidence element to set.
     */
    public void setConfidence(TimeIntervalConfidence confidence) {

        this.confidence = confidence;
    }

    /**
     * A setter for the time interval confidence element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param confidence The time interval confidence value to be set in the element.
     */
    public void setConfidence(int confidence) {

        if (this.confidence == null) {

            this.confidence = new TimeIntervalConfidence();
        }
        this.confidence.setValue(confidence);
    }

    /**
     * A getter for the next time (time mark element).
     * 
     * @return The next time (time mark element).
     */
    public TimeMark getNextTime() {

        return nextTime;
    }

    /**
     * A setter for the next time (time mark element).
     * 
     * @param nextTime The next time (time mark element) to set.
     */
    public void setNextTime(TimeMark nextTime) {

        this.nextTime = nextTime;
    }

    /**
     * A setter for the next time (time mark element). Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param nextTime The next time value to be set in the element.
     */
    public void setNextTime(int nextTime) {

        if (this.nextTime == null) {

            this.nextTime = new TimeMark();
        }
        this.nextTime.setValue(nextTime);
    }

    @Override
    public String encodeUPER() {

        StringBuilder timeChangeDetailsBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        if (startTime != null) {

            optionalBits.append('1');
            timeChangeDetailsBits.append(startTime.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        timeChangeDetailsBits.append(minEndTime.encodeUPER());

        if (maxEndTime != null) {

            optionalBits.append('1');
            timeChangeDetailsBits.append(maxEndTime.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (likelyTime != null) {

            optionalBits.append('1');
            timeChangeDetailsBits.append(likelyTime.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (confidence != null) {

            optionalBits.append('1');
            timeChangeDetailsBits.append(confidence.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (nextTime != null) {

            optionalBits.append('1');
            timeChangeDetailsBits.append(nextTime.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        timeChangeDetailsBits.insert(0, optionalBits);

        return timeChangeDetailsBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (TimeChangeDetails.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a TimeChangeDetails frame (%d)", TimeChangeDetails.NUM_BITS));
        }

        String timeChangeDetailsOptionalBits = bits.substring(0, TimeChangeDetails.NUM_BITS);
        bits = bits.substring(TimeChangeDetails.NUM_BITS);

        if (timeChangeDetailsOptionalBits.charAt(0) == '1') {

            startTime = new TimeMark();
            bits = startTime.decodeUPER(bits);
        }

        bits = minEndTime.decodeUPER(bits);

        if (timeChangeDetailsOptionalBits.charAt(1) == '1') {

            maxEndTime = new TimeMark();
            bits = maxEndTime.decodeUPER(bits);
        }

        if (timeChangeDetailsOptionalBits.charAt(2) == '1') {

            likelyTime = new TimeMark();
            bits = likelyTime.decodeUPER(bits);
        }

        if (timeChangeDetailsOptionalBits.charAt(3) == '1') {

            confidence = new TimeIntervalConfidence();
            bits = confidence.decodeUPER(bits);
        }

        if (timeChangeDetailsOptionalBits.charAt(4) == '1') {

            nextTime = new TimeMark();
            bits = nextTime.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(startTime, minEndTime, maxEndTime, likelyTime, confidence, nextTime);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof TimeChangeDetails)) {

            return false;
        }
        TimeChangeDetails frame = (TimeChangeDetails)object;
        return Objects.equals(this.startTime, frame.startTime)
                && this.minEndTime.equals(frame.minEndTime)
                && Objects.equals(this.maxEndTime, frame.maxEndTime)
                && Objects.equals(this.likelyTime, frame.likelyTime)
                && Objects.equals(this.confidence, frame.confidence)
                && Objects.equals(this.nextTime, frame.nextTime);
    }
}
