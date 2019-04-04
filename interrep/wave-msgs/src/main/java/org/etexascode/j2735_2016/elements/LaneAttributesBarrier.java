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
package org.etexascode.j2735_2016.elements;

import java.util.Objects;

import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The lane attributes barrier element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneAttributesBarrier implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a lane attributes barrier.
     */
    public static final int NUM_BITS = 16;

    /**
     * The median revocable lane status.
     */
    private boolean medianRevocableLane = false;

    /**
     * The median status.
     */
    private boolean median = false;

    /**
     * The white line hashing status.
     */
    private boolean whiteLineHashing = false;

    /**
     * The striped lines status.
     */
    private boolean stripedLines = false;

    /**
     * The double striped lines status.
     */
    private boolean doubleStripedLines = false;

    /**
     * The traffic cones status.
     */
    private boolean trafficCones = false;

    /**
     * The construction barrier status.
     */
    private boolean constructionBarrier = false;

    /**
     * The traffic channels status.
     */
    private boolean trafficChannels = false;

    /**
     * The low curbs status.
     */
    private boolean lowCurbs = false;

    /**
     * The high curbs status.
     */
    private boolean highCurbs = false;

    /**
     * A getter for the median revocable lane status.
     * 
     * @return The median revocable lane status.
     */
    public boolean isMedianRevocableLane() {

        return medianRevocableLane;
    }

    /**
     * A setter for the median revocable lane status.
     * 
     * @param medianRevocableLane The median revocable lane status to set.
     */
    public void setMedianRevocableLane(boolean medianRevocableLane) {

        this.medianRevocableLane = medianRevocableLane;
    }

    /**
     * A getter for the median status.
     * 
     * @return The median status.
     */
    public boolean isMedian() {

        return median;
    }

    /**
     * A setter for the median status.
     * 
     * @param median The median status to set.
     */
    public void setMedian(boolean median) {

        this.median = median;
    }

    /**
     * A getter for the white line hashing status.
     * 
     * @return The white line hashing status.
     */
    public boolean isWhiteLineHashing() {

        return whiteLineHashing;
    }

    /**
     * A setter for the white line hashing status.
     * 
     * @param whiteLineHashing The white line hashing status to set.
     */
    public void setWhiteLineHashing(boolean whiteLineHashing) {

        this.whiteLineHashing = whiteLineHashing;
    }

    /**
     * A getter for the striped lines status.
     * 
     * @return The striped lines status.
     */
    public boolean isStripedLines() {

        return stripedLines;
    }

    /**
     * A setter for the striped lines status.
     * 
     * @param stripedLines The striped lines status to set.
     */
    public void setStripedLines(boolean stripedLines) {

        this.stripedLines = stripedLines;
    }

    /**
     * A getter for the double striped lines status.
     * 
     * @return The double striped lines status.
     */
    public boolean isDoubleStripedLines() {

        return doubleStripedLines;
    }

    /**
     * A setter for the double striped lines status.
     * 
     * @param doubleStripedLines The double striped lines status to set.
     */
    public void setDoubleStripedLines(boolean doubleStripedLines) {

        this.doubleStripedLines = doubleStripedLines;
    }

    /**
     * A getter for the traffic cones status.
     * 
     * @return The traffic cones status.
     */
    public boolean isTrafficCones() {

        return trafficCones;
    }

    /**
     * A setter for the traffic cones status.
     * 
     * @param trafficCones The traffic cones status to set.
     */
    public void setTrafficCones(boolean trafficCones) {

        this.trafficCones = trafficCones;
    }

    /**
     * A getter for the construction barrier status.
     * 
     * @return The construction barrier status.
     */
    public boolean isConstructionBarrier() {

        return constructionBarrier;
    }

    /**
     * A setter for the construction barrier status.
     * 
     * @param constructionBarrier The construction barrier status to set.
     */
    public void setConstructionBarrier(boolean constructionBarrier) {

        this.constructionBarrier = constructionBarrier;
    }

    /**
     * A getter for the traffic channels status.
     * 
     * @return The traffic channels status.
     */
    public boolean isTrafficChannels() {

        return trafficChannels;
    }

    /**
     * A setter for the traffic channels status.
     * 
     * @param trafficChannels The traffic channels status to set.
     */
    public void setTrafficChannels(boolean trafficChannels) {

        this.trafficChannels = trafficChannels;
    }

    /**
     * A getter for the low curbs status.
     * 
     * @return The low curbs status.
     */
    public boolean isLowCurbs() {

        return lowCurbs;
    }

    /**
     * A setter for the low curbs status.
     * 
     * @param lowCurbs The low curbs status to set.
     */
    public void setLowCurbs(boolean lowCurbs) {

        this.lowCurbs = lowCurbs;
    }

    /**
     * A getter for the high curbs status.
     * 
     * @return The high curbs status.
     */
    public boolean isHighCurbs() {

        return highCurbs;
    }

    /**
     * A setter for the high curbs status.
     * 
     * @param highCurbs The high curbs status to set.
     */
    public void setHighCurbs(boolean highCurbs) {

        this.highCurbs = highCurbs;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneAttributesBarrier = new StringBuilder(16);
        laneAttributesBarrier.append(medianRevocableLane ? '1' : '0');
        laneAttributesBarrier.append(median ? '1' : '0');
        laneAttributesBarrier.append(whiteLineHashing ? '1' : '0');
        laneAttributesBarrier.append(stripedLines ? '1' : '0');
        laneAttributesBarrier.append(doubleStripedLines ? '1' : '0');
        laneAttributesBarrier.append(trafficCones ? '1' : '0');
        laneAttributesBarrier.append(constructionBarrier ? '1' : '0');
        laneAttributesBarrier.append(trafficChannels ? '1' : '0');
        laneAttributesBarrier.append(lowCurbs ? '1' : '0');
        laneAttributesBarrier.append(highCurbs ? '1' : '0');
        laneAttributesBarrier.append("000000");

        return laneAttributesBarrier.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneAttributesBarrier.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneAttributesBarrier element (%d)", LaneAttributesBarrier.NUM_BITS));
        }

        String laneAttributesBarrier = bits.substring(0, LaneAttributesBarrier.NUM_BITS);

        if (!"000000".equals(laneAttributesBarrier.substring(10, 16))) {

            throw new IllegalArgumentException("The bits 10 - 15 must be set to zero, because the reserved bits are not currently in use.");
        }

        medianRevocableLane = laneAttributesBarrier.charAt(0) == '1';
        median = laneAttributesBarrier.charAt(1) == '1';
        whiteLineHashing = laneAttributesBarrier.charAt(2) == '1';
        stripedLines = laneAttributesBarrier.charAt(3) == '1';
        doubleStripedLines = laneAttributesBarrier.charAt(4) == '1';
        trafficCones = laneAttributesBarrier.charAt(5) == '1';
        constructionBarrier = laneAttributesBarrier.charAt(6) == '1';
        trafficChannels = laneAttributesBarrier.charAt(7) == '1';
        lowCurbs = laneAttributesBarrier.charAt(8) == '1';
        highCurbs = laneAttributesBarrier.charAt(9) == '1';

        return bits.substring(LaneAttributesBarrier.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(medianRevocableLane, median, whiteLineHashing, stripedLines, doubleStripedLines, trafficCones, constructionBarrier, trafficChannels, lowCurbs, highCurbs);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneAttributesBarrier)) {

            return false;
        }
        LaneAttributesBarrier element = (LaneAttributesBarrier)object;
        return this.medianRevocableLane == element.medianRevocableLane
                && this.median == element.median
                && this.whiteLineHashing == element.whiteLineHashing
                && this.stripedLines == element.stripedLines
                && this.doubleStripedLines == element.doubleStripedLines
                && this.trafficCones == element.trafficCones
                && this.constructionBarrier == element.constructionBarrier
                && this.trafficChannels == element.trafficChannels
                && this.lowCurbs == element.lowCurbs
                && this.highCurbs == element.highCurbs;
    }
}
