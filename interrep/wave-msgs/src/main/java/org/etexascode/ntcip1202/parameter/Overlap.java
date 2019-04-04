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

package org.etexascode.ntcip1202.parameter;

import java.io.Serializable;

/**
 * This node contains objects that configure, monitor, and control overlap functions.
 * 
 * @author dranker
 */
public class Overlap implements Serializable {

    /**
     * The Maximum Number of Overlaps this Actuated Controller Unit supports. This object indicates
     * the maximum number of rows which shall appear in the overlapTable object.
     */
    private int maxOverlaps;

    /**
     * The Maximum Number of Overlap Status Groups (8 overlaps per group) this Actuated Controller
     * Unit supports. This value is equal to TRUNCATE [(maxOverlaps + 7) / 8]. This object indicates
     * the maximum rows which shall appear in the overlapStatusGroupTable object.
     */
    private int maxOverlapStatusGroups;

    private OverlapTable[] overlaps;

    private OverlapStatusGroupTable[] overlapStatuses;

    public Overlap() {
        overlaps = new OverlapTable[maxOverlaps];
        overlapStatuses = new OverlapStatusGroupTable[maxOverlapStatusGroups];
    }

    /**
     * A table containing Actuated Controller Unit overlap parameters. The number of rows in this
     * table is equal to the maxOverlaps object.
     */
    public class OverlapTable {

        /**
         * The overlap number for objects in this row. The value shall not exceed the maxOverlaps
         * object. The value maps to the Overlap as follows: 1 = Overlap A, 2 = Overlap B etc.
         */
        private int overlapNumber;

        /**
         * The type of overlap operation for this row. The types are as follows: other: The overlap
         * operates in another mode than those described herein. normal: The overlap output shall be
         * controlled by the overlapIncludedPhases when this type is indicated. The overlap output
         * shall be green in the following situations: (1) when an overlap included phase is green.
         * (2) when an overlap included phase is yellow (or red clearance) and an overlap included
         * phase is next. The overlap output shall be yellow when an included phase is yellow and an
         * overlap included phase is not next. The overlap output shall be red whenever the overlap
         * green and yellow are not ON. minusGreenYellow: The overlap output shall be controlled by
         * the overlapIncludedPhases and the overlapModifierPhases if this type is indicated. The
         * overlap output shall be green in the following situations: (1) when an overlap included
         * phase is green and an overlap modifier phase is NOT green. (2) when an overlap included
         * phase is yellow (or red clearance) and an overlap included phase is next and an overlap
         * modifier phase is NOT green. The overlap output shall be yellow when an overlap included
         * phase is yellow and an overlap modifier phase is NOT yellow and an overlap included phase
         * is not next. The overlap output shall be red whenever the overlap green and yellow are
         * not ON.
         */
        private int overlapType;

        /**
         * Each octet is a Phase (number) that shall be an included phase for the overlap. The phase
         * number value shall not exceed the maxPhases object value. When an included phase output
         * is green or when the CU is cycling between included phases, the overlap output shall be
         * green.
         */
        private String overlapIncludedPhases;

        /**
         * Each octet is a Phase (number) that shall be a modifier phase for the overlap. The phase
         * number value shall not exceed the maxPhases object value. A null value provides a normal
         * overlap type. A non-null value provides a minusGreenYellow overlap type.
         */
        private String overlapModifierPhases;

        /**
         * Overlap Trailing Green Parameter in seconds (0-255 sec). When this value is greater than
         * zero and the overlap green would normally terminate, the overlap green shall be extended
         * by this additional time.
         */
        private int overlapTrailGreen;

        /**
         * Overlap Trailing Yellow Change Parameter in tenth seconds (NEMA range: 3.0-25.5 sec).
         * When the overlap green has been extended (Trailing Green), this value shall determine the
         * current length of the Yellow Change interval for the overlap.
         */
        private int overlapTrailYellow;

        /**
         * Overlap Trailing Red Clear Parameter in tenth seconds (0-25.5 sec). When the overlap
         * green has been extended (Trailing Green), this value shall determine the current length
         * of the Red Clearance interval for the overlap.
         */
        private int overlapTrailRed;

        public String getOverlapIncludedPhases() {
            return overlapIncludedPhases;
        }

        public void setOverlapIncludedPhases(String overlapIncludedPhases) {
            this.overlapIncludedPhases = overlapIncludedPhases;
        }

        public String getOverlapModifierPhases() {
            return overlapModifierPhases;
        }

        public void setOverlapModifierPhases(String overlapModifierPhases) {
            this.overlapModifierPhases = overlapModifierPhases;
        }

        public int getOverlapNumber() {
            return overlapNumber;
        }

        public void setOverlapNumber(int overlapNumber) {
            this.overlapNumber = overlapNumber;
        }

        public int getOverlapTrailGreen() {
            return overlapTrailGreen;
        }

        public void setOverlapTrailGreen(int overlapTrailGreen) {
            this.overlapTrailGreen = overlapTrailGreen;
        }

        public int getOverlapTrailRed() {
            return overlapTrailRed;
        }

        public void setOverlapTrailRed(int overlapTrailRed) {
            this.overlapTrailRed = overlapTrailRed;
        }

        public int getOverlapTrailYellow() {
            return overlapTrailYellow;
        }

        public void setOverlapTrailYellow(int overlapTrailYellow) {
            this.overlapTrailYellow = overlapTrailYellow;
        }

        public int getOverlapType() {
            return overlapType;
        }

        public void setOverlapType(int overlapType) {
            this.overlapType = overlapType;
        }
    }

    /**
     * A table containing Actuated Controller Unit overlap output (Red, Yellow, & Green) status in
     * groups of eight overlaps. The number of rows in this table is equal to the
     * maxOverlapStatusGroups object.
     */
    public class OverlapStatusGroupTable {

        /**
         * The overlap StatusGroup number for objects in this row. This value shall not exceed the
         * maxOverlapStatusGroups object value.
         */
        private int overlapStatusGroupNumber;

        /**
         * Overlap Red Output Status Mask, when a bit=1, the Overlap Red is currently active. When a
         * bit=0, the Overlap Red is NOT currently active. Bit 7: Overlap # =
         * (overlapStatusGroupNumber * 8) Bit 6: Overlap # = (overlapStatusGroupNumber * 8) - 1 Bit
         * 5: Overlap # = (overlapStatusGroupNumber * 8) - 2 Bit 4: Overlap # =
         * (overlapStatusGroupNumber * 8) - 3 Bit 3: Overlap # = (overlapStatusGroupNumber * 8) - 4
         * Bit 2: Overlap # = (overlapStatusGroupNumber * 8) - 5 Bit 1: Overlap # =
         * (overlapStatusGroupNumber * 8) - 6 Bit 0: Overlap # = (overlapStatusGroupNumber * 8) - 7
         */
        private int overlapStatusGroupReds;

        /**
         * Overlap Yellow Output Status Mask, when a bit=1, the Overlap Yellow is currently active.
         * When a bit=0, the Overlap Yellow is NOT currently active. Bit 7: Overlap # =
         * (overlapStatusGroupNumber * 8) Bit 6: Overlap # = (overlapStatusGroupNumber * 8) - 1 Bit
         * 5: Overlap # = (overlapStatusGroupNumber * 8) - 2 Bit 4: Overlap # =
         * (overlapStatusGroupNumber * 8) - 3 Bit 3: Overlap # = (overlapStatusGroupNumber * 8) - 4
         * Bit 2: Overlap # = (overlapStatusGroupNumber * 8) - 5 Bit 1: Overlap # =
         * (overlapStatusGroupNumber * 8) - 6 Bit 0: Overlap # = (overlapStatusGroupNumber * 8) - 7
         */
        private int overlapStatusGroupYellows;

        /**
         * Overlap Green Output Status Mask, when a bit=1, the Overlap Green is currently active.
         * When a bit=0, the Overlap Green is NOT currently active. Bit 7: Overlap # =
         * (overlapStatusGroupNumber * 8) Bit 6: Overlap # = (overlapStatusGroupNumber * 8) - 1 Bit
         * 5: Overlap # = (overlapStatusGroupNumber * 8) - 2 Bit 4: Overlap # =
         * (overlapStatusGroupNumber * 8) - 3 Bit 3: Overlap # = (overlapStatusGroupNumber * 8) - 4
         * Bit 2: Overlap # = (overlapStatusGroupNumber * 8) - 5 Bit 1: Overlap # =
         * (overlapStatusGroupNumber * 8) - 6 Bit 0: Overlap # = (overlapStatusGroupNumber * 8) - 7
         */
        private int overlapStatusGroupGreens;

        public int getOverlapStatusGroupGreens() {
            return overlapStatusGroupGreens;
        }

        public void setOverlapStatusGroupGreens(int overlapStatusGroupGreens) {
            this.overlapStatusGroupGreens = overlapStatusGroupGreens;
        }

        public int getOverlapStatusGroupNumber() {
            return overlapStatusGroupNumber;
        }

        public void setOverlapStatusGroupNumber(int overlapStatusGroupNumber) {
            this.overlapStatusGroupNumber = overlapStatusGroupNumber;
        }

        public int getOverlapStatusGroupReds() {
            return overlapStatusGroupReds;
        }

        public void setOverlapStatusGroupReds(int overlapStatusGroupReds) {
            this.overlapStatusGroupReds = overlapStatusGroupReds;
        }

        public int getOverlapStatusGroupYellows() {
            return overlapStatusGroupYellows;
        }

        public void setOverlapStatusGroupYellows(int overlapStatusGroupYellows) {
            this.overlapStatusGroupYellows = overlapStatusGroupYellows;
        }

    }

    public int getMaxOverlapStatusGroups() {
        return maxOverlapStatusGroups;
    }

    public void setMaxOverlapStatusGroups(int maxOverlapStatusGroups) {
        this.maxOverlapStatusGroups = maxOverlapStatusGroups;
    }

    public int getMaxOverlaps() {
        return maxOverlaps;
    }

    public void setMaxOverlaps(int maxOverlaps) {
        this.maxOverlaps = maxOverlaps;
    }

    public OverlapStatusGroupTable[] getOverlapStatuses() {
        return overlapStatuses.clone();
    }

    public void setOverlapStatuses(OverlapStatusGroupTable[] overlapStatuses) {
        this.overlapStatuses = overlapStatuses.clone();
    }

    public OverlapTable[] getOverlaps() {
        return overlaps.clone();
    }

    public void setOverlaps(OverlapTable[] overlaps) {
        this.overlaps = overlaps.clone();
    }
}
