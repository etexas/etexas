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

package org.etexascode.interrep.datamodel;

/**
 * Class to hold the information received from the j2735 basic safety messages.
 * 
 * @author dranker
 * @author ablatt
 */
public class BSM {

    /**
     * Possible values for the Transmission properties in this BSM class. See also
     * getTransmission(), getTransmissionType(int) and getTransmissionText(int)
     */
    public enum Transmission {
        NEUTRAL,
        PARK,
        FORWARDGEARS,
        REVERSEGEARS,
        RESERVED1,
        RESERVED2,
        RESERVED3,
        UNAVAILABLE
    }

    // Note: ablatt - BSMDataUtil (which builds BSM from the BasicSafetyMessage) is in
    // org.etexascode.interrep.messages in the interrep-model project

    /**
     * Converts speeds to a usable form.
     * 
     * @param speed The speed in 0.02 m/s.
     * @return The speed in m/s.
     */
    public static double convertSpeed(int speed) {
        return speed * 0.02;
    }

    /**
     * Converts the transmission text into a human readable string
     * 
     * @param t The transmission value to convert to a human readable string
     * @return The resulting human readable string
     */
    public static String getTransmissionText(Transmission t) {
        switch (t) {
            case NEUTRAL:
                return "Neutral";
            case PARK:
                return "Park";
            case FORWARDGEARS:
                return "Forward Gears";
            case REVERSEGEARS:
                return "Reverse Gears";
            case RESERVED1:
                return "Reserved 1";
            case RESERVED2:
                return "Reserved 2";
            case RESERVED3:
                return "Reserved 3";
            default:
                // case UNAVAILABLE
                return "Unavailable";
        }
    }

    /**
     * Translates a transmission code into a transmission enum type
     * 
     * @param code The transmission code
     * @return The enum value associated with the transmission code
     */
    public static Transmission getTransmissionType(int code) {
        switch (code) {
            case 0:
                return Transmission.NEUTRAL;
            case 1:
                return Transmission.PARK;
            case 2:
                return Transmission.FORWARDGEARS;
            case 3:
                return Transmission.REVERSEGEARS;
            case 4:
                return Transmission.RESERVED1;
            case 5:
                return Transmission.RESERVED2;
            case 6:
                return Transmission.RESERVED3;
            default:
                // case 7:
                return Transmission.UNAVAILABLE;
        }
    }

    /**
     * Converts a compressed speed and transmission into its usable parts. Returns {transmission
     * (int), 0.02 * speed (int)} Call getTransmissionType to convert transmission into something
     * usable. Call convertSpeed to convert the speed to m/s
     * 
     * @param originalBytes The original byte[]
     * @return {transmission, speed}
     */
    public static int[] parseSpeedAndTransmission(byte[] originalBytes) {
        short transmissionAndSpeed = originalBytes[0];
        transmissionAndSpeed <<= 8;
        transmissionAndSpeed |= (originalBytes[1] & 0xff);

        // double speed = (double) (transmissionAndSpeed & 8191); //0d8191 == 0b1111111111111
        // speed *= 0.02; // convert speed from (0.02) meters per second to meters per second
        int transmission = transmissionAndSpeed >> 13;
        transmission &= 7; // 0d7 == 0b111

        return new int[] { transmission, transmissionAndSpeed & 8191 };
    }

    /**
     * ID of the DSRC that is transmitting this message
     */
    private String DSRCmsgID;

    /**
     * Total blob byte size = 38 Byte 1 from blob
     */
    private int msgCount;

    /**
     * Bytes 2-5: BSM Temporary ID.
     */
    private int tempID;

    /**
     * Bytes 6-7: Second of message
     */
    private short second;

    /**
     * PositionLocal3D Block Bytes 8-11: Latitude
     */
    private double latitude;

    /**
     * Bytes 12-15: Longitude
     */
    private double longitude;

    /**
     * Bytes 16-17: Elevation
     */
    private short elevation;

    /**
     * Bytes 18-21: Accuracy
     */
    private int accuracy;

    /**
     * The transmission state of from within the BSM message
     */
    private Transmission transmission;

    /**
     * Motion Block Bytes 22-23: Speed speed in meters per second
     */
    private double speed;

    /**
     * Bytes 24-25: Heading
     */
    private short heading;

    /**
     * Byte 26: Angle (of steering wheel)
     */
    private int steeringAngle;

    /**
     * Bytes 27-33: AccelerationSet4Way (Octet String)
     */
    private short lateralAcceleration;

    private short longitudeAcceleration;

    private short verticalAcceleration;

    private int yaw;

    /**
     * Bytes 34-35: Brake Status
     */
    private short brakeStatus;

    /**
     * Byte 36+ 1st 2 bits of 37 the width of the vehicle in cm
     */
    private int vehicleWidth;

    /**
     * last 6 bits of Byte 37, Byte 38: the length of the vehicle in cm
     */
    private int vehicleLength;

    /**
     * Getter {@link BSM#accuracy}
     * 
     * @return The accuracy.
     */
    public int getAccuracy() {
        return accuracy;
    }

    /**
     * Getter {@link BSM#brakeStatus}
     * 
     * @return The brake status.
     */
    public short getBrakeStatus() {
        return brakeStatus;
    }

    /**
     * Getter {@link BSM#DSRCmsgID}
     * 
     * @return The DSRCMsgID.
     */
    public String getDSRCmsgID() {
        return DSRCmsgID;
    }

    /**
     * Getter {@link BSM#elevation}
     * 
     * @return The elevation.
     */
    public short getElevation() {
        return elevation;
    }

    /**
     * Getter {@link BSM#heading}
     * 
     * @return The heading.
     */
    public short getHeading() {
        return heading;
    }

    /**
     * Getter {@link BSM#latitude}
     * 
     * @return The latitude.
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * Getter {@link BSM#lateralAcceleration}
     * 
     * @return The lateral acceleration.
     */

    public short getLateralAcceleration() {

        return lateralAcceleration;
    }

    /**
     * Getter {@link BSM#longitude}
     * 
     * @return The longitude.
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * Getter {@link BSM#longitudeAcceleration}
     * 
     * @return The longitude acceleration.
     */
    public short getLongitudeAcceleration() {
        return longitudeAcceleration;
    }

    /**
     * Getter {@link BSM#msgCount}
     * 
     * @return The message count.
     */
    public int getMsgCount() {
        return msgCount;
    }

    /**
     * Getter {@link BSM#second}
     * 
     * @return The second.
     */
    public short getSecond() {
        return second;
    }

    /**
     * Getter {@link BSM#speed}
     * 
     * @return The speed.
     */
    public double getSpeed() {
        return speed;
    }

    /**
     * Getter {@link BSM#steeringAngle}
     * 
     * @return The steering angle.
     */
    public int getSteeringAngle() {
        return steeringAngle;
    }

    /**
     * Getter {@link BSM#tempID}
     * 
     * @return The temporary ID.
     */
    public int getTempID() {
        return tempID;
    }

    /**
     * Getter {@link BSM#transmission}
     * 
     * @return The transmission.
     */
    public Transmission getTransmission() {
        return transmission;
    }

    /**
     * Getter {@link BSM#vehicleLength}
     * 
     * @return The vehicle length.
     */
    public int getVehicleLength() {
        return vehicleLength;
    }

    /**
     * Getter {@link BSM#vehicleWidth}
     * 
     * @return The vehicle width.
     */
    public int getVehicleWidth() {
        return vehicleWidth;
    }

    /**
     * Getter {@link BSM#verticalAcceleration}
     * 
     * @return The vertical acceleration.
     */
    public short getVerticalAcceleration() {
        return verticalAcceleration;
    }

    /**
     * Getter {@link BSM#yaw}
     * 
     * @return The yaw.
     */
    public int getYaw() {
        return yaw;
    }

    /**
     * Setter {@link BSM#accuracy}
     * 
     * @param accuracy The new accuracy.
     */
    public void setAccuracy(int accuracy) {
        this.accuracy = accuracy;
    }

    /**
     * Setter {@link BSM#brakeStatus}
     * 
     * @param brakeStatus The new brake status.
     */
    public void setBrakeStatus(short brakeStatus) {
        this.brakeStatus = brakeStatus;
    }

    /**
     * Setter {@link BSM#DSRCmsgID}
     * 
     * @param DSRCmsgID The new DSRC message ID.
     */
    public void setDSRCmsgID(String DSRCmsgID) {
        this.DSRCmsgID = DSRCmsgID;
    }

    /**
     * Setter {@link BSM#elevation}
     * 
     * @param elevation The new elevation.
     */
    public void setElevation(short elevation) {
        this.elevation = elevation;
    }

    /**
     * Setter {@link BSM#heading}
     * 
     * @param heading The new heading.
     */
    public void setHeading(short heading) {
        this.heading = heading;
    }

    /**
     * Setter {@link BSM#latitude}
     * 
     * @param latitude The new latitude.
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Setter {@link BSM#lateralAcceleration}
     * 
     * @param latitudeAcceleration The new latitude acceleration.
     */

    public void setLateralAcceleration(short latitudeAcceleration) {
        this.lateralAcceleration = latitudeAcceleration;

    }

    /**
     * Setter {@link BSM#longitude}
     * 
     * @param longitude The new longitude.
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Setter {@link BSM#longitudeAcceleration}
     * 
     * @param longitudeAcceleration The new longitude acceleration.
     */
    public void setLongitudeAcceleration(short longitudeAcceleration) {
        this.longitudeAcceleration = longitudeAcceleration;
    }

    /**
     * Setter {@link BSM#msgCount}
     * 
     * @param msgCount The new message count.
     */
    public void setMsgCount(int msgCount) {
        this.msgCount = msgCount;
    }

    /**
     * Setter {@link BSM#second}
     * 
     * @param second The new seconds.
     */
    public void setSecond(short second) {
        this.second = second;
    }

    /**
     * Setter {@link BSM#speed}
     * 
     * @param speed The new speed.
     */
    public void setSpeed(double speed) {
        this.speed = speed;
    }

    /**
     * Setter {@link BSM#steeringAngle}
     * 
     * @param steeringAngle The new steering angle.
     */
    public void setSteeringAngle(int steeringAngle) {
        this.steeringAngle = steeringAngle;
    }

    /**
     * Setter {@link BSM#tempID}
     * 
     * @param tempID The new temporary ID.
     */
    public void setTempID(int tempID) {
        this.tempID = tempID;
    }

    /**
     * Setter {@link BSM#transmission}
     * 
     * @param transmission The new transmission.
     */
    public void setTransmission(Transmission transmission) {
        this.transmission = transmission;
    }

    /**
     * Setter {@link BSM#vehicleLength}
     * 
     * @param vehicleLength The new vehicle length.
     */
    public void setVehicleLength(int vehicleLength) {
        this.vehicleLength = vehicleLength;
    }

    /**
     * Setter {@link BSM#vehicleWidth}
     * 
     * @param vehicleWidth The new vehicle width.
     */
    public void setVehicleWidth(int vehicleWidth) {
        this.vehicleWidth = vehicleWidth;
    }

    /**
     * Setter {@link BSM#verticalAcceleration}
     * 
     * @param verticalAcceleration The new vertical acceleration.
     */
    public void setVerticalAcceleration(short verticalAcceleration) {
        this.verticalAcceleration = verticalAcceleration;
    }

    /**
     * Setter {@link BSM#yaw}
     * 
     * @param yaw The new yaw.
     */
    public void setYaw(int yaw) {
        this.yaw = yaw;
    }
}
