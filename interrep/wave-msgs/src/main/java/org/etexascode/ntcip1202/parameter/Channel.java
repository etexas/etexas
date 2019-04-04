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
 * This defines a node for supporting channel objetcts.
 * 
 * @author dranker
 */
public class Channel implements Serializable {

    // NOT MAPPED

    /**
     * The Maximum Number of Channels this Actuated Controller Unit supports. This object indicates
     * the maximum rows which shall appear in the channelTable object.
     */
    private int maxChannels;

    // NOT MAPPED
    /**
     * The maximum number of Channel Status Groups (8 channels per group) this Actuated Controller
     * Unit supports. This value is equal to TRUNCATE [(maxChannels + 7) / 8]. This object indicates
     * the maximum rows which shall appear in the channelStatusGroupTable object.
     */
    private int maxChannelStatusGroups;

    private ChannelTable[] channels;

    private ChannelStatusGroupTable[] statusGroups;

    public Channel() {
        channels = new ChannelTable[maxChannels];
        statusGroups = new ChannelStatusGroupTable[maxChannelStatusGroups];
    }

    /**
     * A table containing Actuated Controller Unit channel parameters. The number of rows in this
     * table is equal to the maxChannels object.
     */
    public class ChannelTable implements Serializable {

        // NOT MAPPED

        /** Serial ID. */
        private static final long serialVersionUID = -3543552305326531042L;

        /**
         * The channel number for objects in this row. This value shall not exceed the maxChannels
         * object value.
         */
        private int channelNumber;

        // NOT MAPPED
        /**
         * This object defines the channel control source (which Phase or Overlap). The value shall
         * not exceed maxPhases or maxOverlaps as determined by channelControlType object: Value 00
         * = No Control (Not In Use) Value 01 = Phase 01 or Overlap A Value 02 = Phase 02 or Overlap
         * B || Value 15 = Phase 15 or Overlap O Value 16 = Phase 16 or Overlap P etc.
         */
        private int channelControlSource;

        // NOT MAPPED
        /**
         * This object defines the channel control type (Vehicle Phase, Pedestrian Phase, or
         * Overlap): other: The channel controls an other type of display. phaseVehicle: The channel
         * controls a vehicle phase display. phasePedestrian: The channel controls a pedestrian
         * phase display. overlap: The channel controls an overlap display.
         */
        private int channelControlType;

        // NOT MAPPED
        /**
         * This object defines the channel state during Automatic Flash. Bit 7: Reserved Bit 6:
         * Reserved Bit 5: Reserved Bit 4: Reserved Bit 3: Flash Alternate Half Hertz Bit=0:
         * Off/Disabled & Bit=1: On/Enabled Bit 2: Flash Red Bit=0: Off/Red Dark & Bit=1: On/Flash
         * Red Bit 1: Flash Yellow Bit=0: Off/Yellow Dark & Bit=1: On/Flash Yellow Bit 0: Reserved A
         * SET of both bits 1 & 2 shall result in bit 1=0 and bit 2=1. A SET of a 'reserved' bit to
         * a value other than zero (0) shall return a badValue(3) error.
         */
        private int channelFlash;

        // NOT MAPPED
        /**
         * This object defines the channel state during Dimming. Dimming shall be accomplished by
         * the elimination of alternate one-half segments from the AC sinusoid applied to the field
         * terminals. Bit 7: Reserved Bit 6: Reserved Bit 5: Reserved Bit 4: Reserved Bit 3: Dim
         * Alternate Half Line Cycle Bit=0: Off/+ half cycle & Bit=1: On/- half cycle Bit 2: Dim Red
         * Bit=0: Off/Red Not Dimmed & Bit=1: On/Dimmed Red Bit 1: Dim Yellow Bit=0: Off / Yellow
         * Not Dimmed & Bit=1: On / Dimmed Yellow Bit 0: Dim Green Bit=0: Off / Green Not Dimmed &
         * Bit=1: On / Dimmed Green A SET of a 'reserved' bit to a value other than zero (0) shall
         * return a badValue(3) error.
         */
        private int channelDim;

        public int getChannelControlSource() {
            return channelControlSource;
        }

        public void setChannelControlSource(int channelControlSource) {
            this.channelControlSource = channelControlSource;
        }

        public int getChannelControlType() {
            return channelControlType;
        }

        public void setChannelControlType(int channelControlType) {
            this.channelControlType = channelControlType;
        }

        public int getChannelDim() {
            return channelDim;
        }

        public void setChannelDim(int channelDim) {
            this.channelDim = channelDim;
        }

        public int getChannelFlash() {
            return channelFlash;
        }

        public void setChannelFlash(int channelFlash) {
            this.channelFlash = channelFlash;
        }

        public int getChannelNumber() {
            return channelNumber;
        }

        public void setChannelNumber(int channelNumber) {
            this.channelNumber = channelNumber;
        }
    }

    /**
     * A table containing Actuated Controller Unit channel output (Red, Yellow, & Green) status in
     * groups of eight channels. The number of rows in this table is equal to the
     * maxChannelStatusGroups object.
     */
    public class ChannelStatusGroupTable implements Serializable {

        // NOT MAPPED

        /** Serial ID. */
        private static final long serialVersionUID = 7212847944240693087L;

        /**
         * The channelStatusGroup number for objects in this row. This value shall not exceed the
         * maxChannelStatusGroups object value.
         */
        private int channelStatusGroupNumber;

        // NOT MAPPED
        /**
         * Channel Red Output Status Mask, when a bit=1, the Channel Red is currently active. When a
         * bit=0, the Channel Red is NOT currently active. Bit 7: Channel # =
         * (channelStatusGroupNumber * 8) Bit 6: Channel # = (channelStatusGroupNumber * 8) - 1 Bit
         * 5: Channel # = (channelStatusGroupNumber * 8) - 2 Bit 4: Channel # =
         * (channelStatusGroupNumber * 8) - 3 Bit 3: Channel # = (channelStatusGroupNumber * 8) - 4
         * Bit 2: Channel # = (channelStatusGroupNumber * 8) - 5 Bit 1: Channel # =
         * (channelStatusGroupNumber * 8) - 6 Bit 0: Channel # = (channelStatusGroupNumber * 8) - 7
         */
        private int channelStatusGroupReds;

        // NOT MAPPED
        /**
         * Channel Yellow Output Status Mask, when a bit=1, the Channel Yellow is currently active.
         * When a bit=0, the Channel Yellow is NOT currently active. Bit 7: Channel # =
         * (channelStatusGroupNumber * 8) Bit 6: Channel # = (channelStatusGroupNumber * 8) - 1 Bit
         * 5: Channel # = (channelStatusGroupNumber * 8) - 2 Bit 4: Channel # =
         * (channelStatusGroupNumber * 8) - 3 Bit 3: Channel # = (channelStatusGroupNumber * 8) - 4
         * Bit 2: Channel # = (channelStatusGroupNumber * 8) - 5 Bit 1: Channel # =
         * (channelStatusGroupNumber * 8) - 6 Bit 0: Channel # = (channelStatusGroupNumber * 8) - 7
         */
        private int channelStatusGroupYellows;

        // NOT MAPPED
        /**
         * Channel Green Output Status Mask, when a bit=1, the Channel Green is currently active.
         * When a bit=0, the Channel Green is NOT currently active. Bit 7: Channel # =
         * (channelStatusGroupNumber * 8) Bit 6: Channel # = (channelStatusGroupNumber * 8) - 1 Bit
         * 5: Channel # = (channelStatusGroupNumber * 8) - 2 Bit 4: Channel # =
         * (channelStatusGroupNumber * 8) - 3 Bit 3: Channel # = (channelStatusGroupNumber * 8) - 4
         * Bit 2: Channel # = (channelStatusGroupNumber * 8) - 5 Bit 1: Channel # =
         * (channelStatusGroupNumber * 8) - 6 Bit 0: Channel # = (channelStatusGroupNumber * 8) - 7
         */
        private int channelStatusGroupGreens;

        public int getChannelStatusGroupGreens() {
            return channelStatusGroupGreens;
        }

        public void setChannelStatusGroupGreens(int channelStatusGroupGreens) {
            this.channelStatusGroupGreens = channelStatusGroupGreens;
        }

        public int getChannelStatusGroupNumber() {
            return channelStatusGroupNumber;
        }

        public void setChannelStatusGroupNumber(int channelStatusGroupNumber) {
            this.channelStatusGroupNumber = channelStatusGroupNumber;
        }

        public int getChannelStatusGroupReds() {
            return channelStatusGroupReds;
        }

        public void setChannelStatusGroupReds(int channelStatusGroupReds) {
            this.channelStatusGroupReds = channelStatusGroupReds;
        }

        public int getChannelStatusGroupYellows() {
            return channelStatusGroupYellows;
        }

        public void setChannelStatusGroupYellows(int channelStatusGroupYellows) {
            this.channelStatusGroupYellows = channelStatusGroupYellows;
        }
    }
}
