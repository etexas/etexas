/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** *  COPYRIGHT (C) 2003 by The University of Texas at Austin   * ** *
 * * ** *                                                            * ** *
 * * ** * Permission is hereby granted to use, modify, copy, and     * ** *
 * * ** * distribute this software and its documentation for any     * ** *
 * * ** * purpose only without profit, provided that the above       * ** *
 * * ** * Copyright Notice appears in all copies and that both the   * ** *
 * * ** * Copyright Notice and this Permission Notice appears in     * ** *
 * * ** * every copy of supporting documentation.  No title to nor   * ** *
 * * ** * ownership of the software is transferred hereby.  The name * ** *
 * * ** * of The University of Texas at Austin shall not be used in  * ** *
 * * ** * advertising or publicity related to the distribution of    * ** *
 * * ** * the software without specific, written, prior permission.  * ** *
 * * ** * This software is provided as-delivered without expressed   * ** *
 * * ** * or implied warranty.  The University of Texas at Austin    * ** *
 * * ** * makes no representation about the suitability of this      * ** *
 * * ** * software for any purpose and accepts no responsibility for * ** *
 * * ** * its use.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** * This program is free software; you can redistribute it     * ** *
 * * ** * and/or modify it under the terms of the GNU General Public * ** *
 * * ** * License as published by the Free Software Foundation;      * ** *
 * * ** * either version 2 of the License, or (at your option) any   * ** *
 * * ** * later version.                                             * ** *
 * * ** *                                                            * ** *
 * * ** * This program is distributed in the hope that it will be    * ** *
 * * ** * useful, but WITHOUT ANY WARRANTY; without even the implied * ** *
 * * ** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ** *
 * * ** * PURPOSE.  See the GNU General Public License for more      * ** *
 * * ** * details.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** * You should have received a copy of the GNU General Public  * ** *
 * * ** * License along with this program; if not, write to the Free * ** *
 * * ** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ** *
 * * ** * Floor, Boston, MA 02110-1301, USA.                         * ** *
 * * ** *                                                            * ** *
 * * ** * For more information: http://www.gnu.org/licenses/gpl.html * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * #L%
 */

package org.etexascode.api;

/**
 * Model class for the VMS message.
 * 
 * @author jrutherford
 */
public class VMSMessage {

    /** A signal tells the driver this message. */
    public static final int DRIVER_SIGNAL = 1;

    /** The driver receives this message through a headset. */
    public static final int DRIVER_IVMS = 2;

    /** The message is sent to the onboard computer. */
    public static final int COMPUTER_IVMS = 3;

    /** The ETexas API message injection constants. */
    public static final int NONE = 0;

    public static final int ETEXAS_API_NORMAL_ACCELERATE = 1;

    public static final int ETEXAS_API_NORMAL_DECELERATE = 1;

    public static final int ETEXAS_API_MAX_ACCELERATE = 2;

    public static final int ETEXAS_API_MAX_DECELERATE = 2;

    public static final int ETEXAS_API_LEFT_LANE = 7;

    public static final int ETEXAS_API_RIGHT_LANE = 8;

    /** The type of message. */
    private int type;

    /** The message action. */
    private int message;

    /** The message parameter. */
    private double parameter;

    /** The start time of the message. */
    private double startTime;

    /** The end time of the message. */
    private double activeTime;

    /** The type of approach. */
    private int approach;

    /** The starting lane. */
    private int startLane;

    /** The ending lane. */
    private int endLane;

    /** The starting distance. */
    private double distStart;

    /** The ending distance. */
    private double distEnd;

    /** The vehicle ID. */
    private int vehicleID;

    /** The distribution type. Must be > 0. */
    private int distType;

    /** The distribution mean. */
    private double distMean;

    /** The distribution parameter. */
    private double distParam;

    /** Constructor. */
    public VMSMessage() {
        this.type = 3;
        this.message = VMSMessage.NONE;
        this.parameter = 0.0;
        this.startTime = 0.0;
        this.activeTime = 90.0;
        this.approach = 1;
        this.startLane = 1;
        this.endLane = 2;
        this.distStart = 0.0;
        this.distEnd = 50.0;
        this.vehicleID = 0;
        this.distType = 1;
        this.distMean = 0.0;
        this.distParam = 0.0;
    }

    /**
     * Gets the type of message.
     * 
     * @return The message type.
     */
    public int getType() {
        return type;
    }

    /**
     * Sets the message type.
     * 
     * @param type The new message type.
     */
    public void setType(int type) {
        this.type = type;
    }

    /**
     * Gets the message.
     * 
     * @return The message.
     */
    public int getMessage() {
        return message;
    }

    /**
     * Sets the message.
     * 
     * @param message The new message.
     */
    public void setMessage(int message) {
        this.message = message;
    }

    /**
     * Gets the message parameter.
     * 
     * @return The message parameter.
     */
    public double getParameter() {
        return parameter;
    }

    /**
     * Sets the message parameter.
     * 
     * @param parameter The new message parameter.
     */
    public void setParameter(double parameter) {
        this.parameter = parameter;
    }

    /**
     * Gets the message start time.
     * 
     * @return The message start time.
     */
    public double getStartTime() {
        return startTime;
    }

    /**
     * Sets the message start time.
     * 
     * @param startTime The new message start time.
     */
    public void setStartTime(double startTime) {
        this.startTime = startTime;
    }

    /**
     * Gets the message end time.
     * 
     * @return The end time.
     */
    public double getActiveTime() {
        return activeTime;
    }

    /**
     * Sets the message end time.
     * 
     * @param activeTime The new end time.
     */
    public void setActiveTime(double activeTime) {
        this.activeTime = activeTime;
    }

    /**
     * Gets the vehicle approach.
     * 
     * @return The approach.
     */
    public int getApproach() {
        return approach;
    }

    /**
     * Sets the vehicle approach.
     * 
     * @param approach The new vehicle approach.
     */
    public void setApproach(int approach) {
        this.approach = approach;
    }

    /**
     * Gets the start lane.
     * 
     * @return The start lane.
     */
    public int getStartLane() {
        return startLane;
    }

    /**
     * Sets the start lane.
     * 
     * @param startLane The new start lane.
     */
    public void setStartLane(int startLane) {
        this.startLane = startLane;
    }

    /**
     * Gets the end lane.
     * 
     * @return The end lane.
     */
    public int getEndLane() {
        return endLane;
    }

    /**
     * Sets the end lane.
     * 
     * @param endLane The new end lane.
     */
    public void setEndLane(int endLane) {
        this.endLane = endLane;
    }

    /**
     * Gets the distance start.
     * 
     * @return The distance start.
     */
    public double getDistStart() {
        return distStart;
    }

    /**
     * Sets the distance start.
     * 
     * @param distStart The new distance start.
     */
    public void setDistStart(double distStart) {
        this.distStart = distStart;
    }

    /**
     * Gets the distance end.
     * 
     * @return The distance end.
     */
    public double getDistEnd() {
        return distEnd;
    }

    /**
     * Sets the distance end.
     * 
     * @param distEnd The new distance end.
     */
    public void setDistEnd(double distEnd) {
        this.distEnd = distEnd;
    }

    /**
     * Gets the vehicle ID.
     * 
     * @return The vehicle ID.
     */
    public int getVehicleID() {
        return vehicleID;
    }

    /**
     * Sets the vehicle ID.
     * 
     * @param vehicleID The new vehicle ID.
     */
    public void setVehicleID(int vehicleID) {
        this.vehicleID = vehicleID;
    }

    /**
     * Gets the distribution type.
     * 
     * @return The distribution type.
     */
    public int getDistType() {
        return distType;
    }

    /**
     * Sets the distribution type.
     * 
     * @param distType The new distribution type.
     */
    public void setDistType(int distType) {
        this.distType = distType;
    }

    /**
     * Gets the distribution mean.
     * 
     * @return The distribution mean.
     */
    public double getDistMean() {
        return distMean;
    }

    /**
     * Sets the distribution mean.
     * 
     * @param distMean The new distribution mean.
     */
    public void setDistMean(double distMean) {
        this.distMean = distMean;
    }

    /**
     * Gets the distribution parameter.
     * 
     * @return The distribution parameter.
     */
    public double getDistParam() {
        return distParam;
    }

    /**
     * Sets the distribution parameter.
     * 
     * @param distParam The new parameter.
     */
    public void setDistParam(double distParam) {
        this.distParam = distParam;
    }

    /**
     * Injects a message.
     * 
     * @return True if injection was successful.
     */
    public boolean inject() {
        SimproJNA lib = SimproJNA.INSTANCE;
        if (lib == null)
            return false;
        lib.injmsg(type, message, parameter, startTime, activeTime, approach, startLane, endLane, distStart, distEnd, vehicleID, distType, distMean, distParam);
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("[ ");
        builder.append("Type: ");
        builder.append(type);
        builder.append(", Message: ");
        builder.append(message);
        builder.append(", Parameter: ");
        builder.append(parameter);
        builder.append(", Start Time: ");
        builder.append(startTime);
        builder.append(", Active Time: ");
        builder.append(activeTime);
        builder.append(", Approach: ");
        builder.append(approach);
        builder.append(", Start Lane: ");
        builder.append(startLane);
        builder.append(", End Lane: ");
        builder.append(endLane);
        builder.append(", Distance Start: ");
        builder.append(distStart);
        builder.append(", Distance End: ");
        builder.append(distEnd);
        builder.append(", Vehicle ID: ");
        builder.append(vehicleID);
        builder.append(", Dist Type: ");
        builder.append(distType);
        builder.append(", Dist Mean: ");
        builder.append(distMean);
        builder.append(", Dist Param: ");
        builder.append(distParam);
        builder.append(" ]");
        return builder.toString();
    }
}