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
package org.etexascode.apps;

/**
 * Enum for the byte counts of fields in the J2735 spec. The enum type names are the J2735 data
 * types. The PATHHISTORY byte count is set to the minimum value of 5.
 * 
 * @author bmauldon
 */
public enum MsgByteCount {
    ACCELERATIONCONFIDENCE(2),
    ACCELERATIONSET4WAY(7),
    AMBIENTAIRTEMPERATURE(1),
    AMBIENTAIRPRESSURE(1),
    APPROACHNUMBER(1),
    BARRIERATRRIBUTES(2),
    BSMBLOB(38),
    BRAKESYSTEMSTATUS(2),
    BRAKEAPPLIEDPRESSURE(0.5),
    BUMPERHEIGHTS(2),
    COEFFICIENTOFFRICTION(1),
    CONFIDENCESET(10.75),
    DDATETIME(4),
    DRIVENLINEOFFSET(2),
    DRIVINGWHEELANGLE(2),
    DSRCMSGID(1),
    ELEVATION(2),
    EVENTFLAGS(2),
    EXTERIORLIGHTS(2),
    FULLPOSITIONVECTOR(24),
    GPSSTATUS(4),
    HEADING(2),
    INTERSECTIONID(2),
    INTERSECTIONSTATUSOBJECT(1),
    ITISEXTERNAL(4),
    J1939DATA(27),
    LANECOUNT(1),
    LANENUMBER(1),
    LANESET(1),
    LANEWIDTH(2),
    LATITUDE(4),
    LAYERTYPE(1),
    LAYERID(1),
    LIGHTBARINUSE(1),
    LONGITUDE(4),
    MSGCOUNT(1),
    MSGCRC(2),
    NODE(8),
    NTCIPPRECIPFRICTION(1),
    NTCIPPRECIPRADIATION(2),
    NTCIPPRECIPRATE(2),
    NTCIPPRECIPSITUATION(4),
    NTCIPPRECIPYESYNO(4),
    OBJECTCOUNT(1),
    OBSTACLEDIRECTION(2),
    OBSTACLEDISTANCE(2),
    PATHHISTORY(5),
    PATHPREDICTION(3),
    PEDESTRIANDETECT(1),
    PEDESTRIANSIGNALSTATE(1),
    POSITION3D(10),
    POSITIONALACCURACY(4),
    POSITIONCONFIDENCESET(10.75),
    RAINSENSOR(1),
    REQUESTEDITEM(4),
    RTCMREVISION(1),
    RTCMID(2),
    SIGNALSTATE(1),
    SPECIALSIGNALSTATE(1),
    SPEEDCONFIDENCE(0.375),
    STATUS(1),
    STEERINGWHEELANGLE(1),
    STEERINGWHEELANGLECONFIDENCE(4),
    STEERINGWHEELRATEOFCHANGE(1),
    SUNSENSOR(2),
    TEMPORARYID(4),
    THROTTLEPOSITION(.25),
    TIMECONFIDENCE(2),
    TIMEMARK(2),
    TIRELEAKAGERATE(2),
    TIRELOCATION(1),
    TIREPRESSURE(2),
    TIREPRESSURETHRESHOLDDETECTION(1),
    TIRETEMP(2),
    TRAILERWEIGHT(2),
    VEHICLECLASS(1),
    VEHCILEHEIGHT(1),
    VEHICLELANEATTRIBUTES(2),
    VEHICLEMASS(1),
    VEHICLETYPE(1),
    VERTICALACCELERATIONTHRESHOLD(0.5),
    WEATHER(7),
    WIPERRATE(1),
    WIPERSTATUSFRONT(1),
    WIPERSTATUSREAR(1),
    YAWRATECONFIDENCE(2);

    /**
     * byte count for data types
     */
    private final double byteCount;

    MsgByteCount(double byteCount) {
        this.byteCount = byteCount;
    }

    public double getByteCount() {
        return this.byteCount;
    }

}
